package syndie.db;

import java.util.*;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import net.i2p.data.Base64;
import net.i2p.data.DataHelper;
import net.i2p.data.Hash;
import net.i2p.data.SigningPublicKey;
import syndie.Constants;
import syndie.data.*;

/**
 * revamped thread gathering/filtering, using the ThreadBuilder to wrap them
 * up by threads, jwz-style
 */
public class ThreadAccumulatorJWZ extends ThreadAccumulator {
    private DBClient _client;
    private UI _ui;

    private List _rootURIs;
    /** fully populated threads, in ReferenceNode form */
    private List _roots;
    /** msgId (Long) to Set of tags on that message */
    private Map _msgTags;
    /** one List of tags for each root URI, duplicates allowed */
    private List _threadTags;
    /** Integer for each thread specifying how many messages are in the thread */
    private List _threadMessages;
    /** String describing the subject of the thread */
    private List _threadSubject;
    /** internal channel id of the thread root's author */
    private List _threadRootAuthorId;
    /** internal channel id of the most recent post's author */
    private List _threadLatestAuthorId;
    /** when (Long) the most recent post was made */
    private List _threadLatestPostDate;

    
    // parsed search critera
    private boolean _showThreaded;
    private boolean _includeOwners;
    private boolean _includeManagers;
    private boolean _includeAuthorizedPosters;
    private boolean _includeAuthorizedReplies;
    private boolean _includeUnauthorizedPosts;
    private long _earliestReceiveDate;
    private long _earliestPostDate;
    private boolean _applyTagFilterToMessages;
    private int _minPages;
    private int _maxPages;
    private int _minAttachments;
    private int _maxAttachments;
    private int _minReferences;
    private int _maxReferences;
    private int _minKeys;
    private int _maxKeys;
    private boolean _alreadyDecrypted;
    private boolean _pbe;
    private boolean _publicMessage;
    private boolean _privateMessage;
    private boolean _authorizedMessage;
    private boolean _unreadOnly;
    private String _keyword;
    private Set _channelHashes;
    private Set _requiredTags;
    private Set _wantedTags;
    private Set _rejectedTags;
    
    private int _sortField;
    private boolean _sortOrderAscending;
        
    public ThreadAccumulatorJWZ(DBClient client, UI ui) {
        super(client, ui);
        _client = client;
        _ui = ui;
        _sortField = SORT_DEFAULT;
        _sortOrderAscending = false;
        //_ui = new NullUI(true);
    }
    
    public void setSort(int sortField, boolean ascending) {
        _sortField = sortField;
        _sortOrderAscending = ascending;
    }
    
    public void setFilter(SyndieURI criteria) {
        // split up the individual attributes. see doc/web/spec.html#uri_search
        String scope[] = criteria.getStringArray("scope");
        if ( (scope == null) || (scope.length == 0) || ( (scope.length == 1) && ("all".equals(scope[0]))) ) {
            _channelHashes = null;
        } else {
            Set chans = new HashSet();
            for (int i = 0; i < scope.length; i++) {
                byte b[] = Base64.decode(scope[i]);
                if ( (b != null) && (b.length == Hash.HASH_LENGTH) )
                    chans.add(new Hash(b));
            }
            _channelHashes = chans;
        }
        
        String author = criteria.getString("author");
        if ( (author != null) && ("any".equals(author)) ) {
            _includeOwners = true;
            _includeManagers = true;
            _includeAuthorizedPosters = true;
            _includeAuthorizedReplies = true;
            _includeUnauthorizedPosts = true;
        } else if ( (author != null) && ("manager".equals(author)) ) {
            _includeOwners = true;
            _includeManagers = true;
            _includeAuthorizedPosters = false;
            _includeAuthorizedReplies = false;
            _includeUnauthorizedPosts = false;
        } else if ( (author != null) && ("owner".equals(author)) ) {
            _includeOwners = true;
            _includeManagers = false;
            _includeAuthorizedPosters = false;
            _includeAuthorizedReplies = false;
            _includeUnauthorizedPosts = false;
        } else {
            _includeOwners = true;
            _includeManagers = true;
            _includeAuthorizedPosters = true;
            _includeAuthorizedReplies = true;
            _includeUnauthorizedPosts = false;
        }
        
        _earliestPostDate = getStartDate(criteria.getLong("age"));
        _earliestReceiveDate = getStartDate(criteria.getLong("agelocal"));
        
        _requiredTags = getTags(criteria.getStringArray("tagrequire"));
        _rejectedTags = getTags(criteria.getStringArray("tagexclude"));
        _wantedTags = getTags(criteria.getStringArray("taginclude"));
        _applyTagFilterToMessages = criteria.getBoolean("tagmessages", false);
    
        _minPages = getInt(criteria.getLong("pagemin"));
        _maxPages = getInt(criteria.getLong("pagemax"));
        _minAttachments = getInt(criteria.getLong("attachmin"));
        _maxAttachments = getInt(criteria.getLong("attachmax"));
        _minReferences = getInt(criteria.getLong("refmin"));
        _maxReferences = getInt(criteria.getLong("refmax"));
        _minKeys = getInt(criteria.getLong("keymin"));
        _maxKeys = getInt(criteria.getLong("keymax"));
    
        _alreadyDecrypted = !criteria.getBoolean("encrypted", false);
        _pbe = criteria.getBoolean("pbe", true);
        _publicMessage = criteria.getBoolean("public", true);
        _privateMessage = criteria.getBoolean("private", true);
        _authorizedMessage = criteria.getBoolean("authorized", true);
        _showThreaded = criteria.getBoolean("threaded", true);
        _unreadOnly = criteria.getBoolean("unreadonly", false);
        
        _keyword = criteria.getString("keyword");
    }
    
    private static final Set getTags(String tags[]) {
        Set rv = new HashSet();
        if (tags != null) {
            for (int i = 0; i < tags.length; i++) {
                String s = tags[i].trim();
                if (s.length() > 0)
                    rv.add(s);
            }
        }
        return rv;
    }
    
    private static final long getStartDate(Long numDaysAgo) {
        if (numDaysAgo == null) return -1;
        long now = System.currentTimeMillis();
        long dayBegin = now - (now % 24*60*60*1000L);
        dayBegin -= numDaysAgo.longValue()*24*60*60*1000L;
        return dayBegin;
    }
    private static final int getInt(Long val) { 
        if (val == null) 
            return -1; 
        else 
            return val.intValue();
    }

    /**
     * @param owners include posts by the channel owner
     * @param managers include posts by those authorized to manage the channel 
     * @param posters include posts by those authorized to create new threads
     * @param authReplies include authorized messages by those allowed to reply to authorized posts
     * @param unauthorizedPosts include authentic yet unauthorized posts
     */
    public void setAuthorFilter(boolean owners, boolean managers, boolean posters, boolean authReplies, boolean unauthorizedPosts) {
        _includeOwners = owners;
        _includeManagers = managers;
        _includeAuthorizedPosters = posters;
        _includeAuthorizedReplies = authReplies;
        _includeUnauthorizedPosts = unauthorizedPosts;
    }
    /** the post was received locally on or after the given date */
    public void setReceivedSince(long date) { _earliestReceiveDate = date; }
    /** the post was created on or after the given date */
    public void setPostSince(long date) { _earliestPostDate = date; }
    /** apply the tag filters to individual messages, not threads as a whole */
    public void applyTagFilterToMessages(boolean apply) { _applyTagFilterToMessages = apply; }
    /**
     * minimum and maximum values (inclusive) for various post attributes, or -1 if
     * the value is not relevent
     */
    public void setContentFilter(int minPages, int maxPages, int minAttachments, int maxAttachments,
                                 int minReferences, int maxReferences, int minKeys, int maxKeys) {
        _minPages = minPages;
        _maxPages = maxPages;
        _minAttachments = minAttachments;
        _maxAttachments = maxAttachments;
        _minReferences = minReferences;
        _maxReferences = maxReferences;
        _minKeys = minKeys;
        _maxKeys = maxKeys;
    }
    /**
     * @param alreadyDecrypted included posts must already be readable (false means they must not be readable)
     * @param pbe the post was or is encrypted with a passphrase
     * @param privateMessage the post was or is encrypted to the channel reply key
     */
    public void setStatus(boolean alreadyDecrypted, boolean pbe, boolean privateMessage) {
        _alreadyDecrypted = alreadyDecrypted;
        _pbe = pbe;
        _privateMessage = privateMessage;
    }

    public void setScope(Set channelHashes) { _channelHashes = channelHashes; }
    public void setTags(Set required, Set wanted, Set rejected) {
        _requiredTags = required;
        _wantedTags = wanted;
        _rejectedTags = rejected;
    }
    public void setKeyword(String keyword) { _keyword = keyword; }
        
    public int getThreadCount() { return _rootURIs.size(); }
    public SyndieURI getRootURI(int index) { return (SyndieURI)_rootURIs.get(index); }
    public ReferenceNode getRootThread(int index) { return (ReferenceNode)_roots.get(index); }
    /** sorted set of tags in the given thread */
    public Set getTags(int index) { return new TreeSet((List)_threadTags.get(index)); }
    public int getTagCount(int index, String tag) {
        int rv = 0;
        if (tag == null) return 0;
        List tags = (List)_threadTags.get(index);
        if (tags == null) return 0;
        for (int i = 0; i < tags.size(); i++)
            if (tag.equals((String)tags.get(i)))
                rv++;
        return rv;
    }
    public int getMessages(int index) { return ((Integer)_threadMessages.get(index)).intValue(); }
    public String getSubject(int index) { return (String)_threadSubject.get(index); }
    public long getRootAuthor(int index) { return ((Long)_threadRootAuthorId.get(index)).longValue(); }
    public long getMostRecentAuthor(int index) { return ((Long)_threadLatestAuthorId.get(index)).longValue(); }
    public long getMostRecentDate(int index) { return ((Long)_threadLatestPostDate.get(index)).longValue(); }
    
    /**
     * actually gather the matching threads according to the search criteria
     */
    public void gatherThreads() {
        init();
        _ui.debugMessage("beginning gather threads w/ state: \n" + toString());
        
        _client.beginTrace();
    
        if (!_alreadyDecrypted && _pbe) {
            gatherPBEPendingDecryption();
            return;
        }
        
        // filter by date and scope only
        Set matchingThreadMsgIds = getMatchingThreadMsgIds();
        _ui.debugMessage("matching msgIds: " + matchingThreadMsgIds);
        long beforeFilterStatus = System.currentTimeMillis();
        
        if (_unreadOnly && matchingThreadMsgIds.size() > 0) {
            long beforePrep = System.currentTimeMillis();
            long msgIds[] = new long[matchingThreadMsgIds.size()];
            int i = 0;
            for (Iterator iter = matchingThreadMsgIds.iterator(); iter.hasNext(); i++) {
                ThreadMsgId tmi = (ThreadMsgId)iter.next();
                msgIds[i] = tmi.msgId;
            }
            long afterPrep = System.currentTimeMillis();
            List read = _client.getRead(msgIds);
            long beforeStrip = System.currentTimeMillis();
            for (Iterator iter = matchingThreadMsgIds.iterator(); iter.hasNext(); ) {
                ThreadMsgId tmi = (ThreadMsgId)iter.next();
                if (read.contains(new Long(tmi.msgId))) {
                    _ui.debugMessage("reject " + tmi + " because it was already read");
                    iter.remove();
                }
            }
            long afterStrip = System.currentTimeMillis();
            _ui.debugMessage("filtering unread: prep: " + (afterPrep-beforePrep) +
                             " getRead: " + (beforeStrip-afterPrep) + " strip: " + (afterStrip-beforeStrip));
            /* 
            
            for (Iterator iter = matchingThreadMsgIds.iterator(); iter.hasNext(); ) {
                ThreadMsgId tmi = (ThreadMsgId)iter.next();
                //Long msgId = (Long)iter.next();
                int status = _client.getMessageStatus(tmi.msgId); //msgId.longValue());
                if (DBClient.MSG_STATUS_NEW_UNREAD != status) {
                    _ui.debugMessage("reject " + tmi + " because status=" + status);
                    iter.remove();
                }
            }
            */
        }
        long afterFilterStatus = System.currentTimeMillis();
        _ui.debugMessage("filter messages by message status took " + (afterFilterStatus-beforeFilterStatus));

        boolean tagFilter = true;
        if ( ( (_rejectedTags == null) || (_rejectedTags.size() <= 0) ) &&
             ( (_requiredTags == null) || (_requiredTags.size() <= 0) ) &&
             ( (_wantedTags == null) || (_wantedTags.size() <= 0) ) )
            tagFilter = false;
        
        _msgTags = new HashMap();
        
        if (tagFilter) {
            for (Iterator iter = matchingThreadMsgIds.iterator(); iter.hasNext(); ) {
                ThreadMsgId tmi = (ThreadMsgId)iter.next();
                //Long msgId = (Long)iter.next();
                Set tags = _client.getMessageTags(tmi.msgId, true, true);
                if (_applyTagFilterToMessages) {
                    if (!tagFilterPassed(tags)) {
                        _ui.debugMessage("reject " + tmi + " because msg tag filters failed: " + tags);
                        iter.remove();
                    } else {
                        _msgTags.put(new Long(tmi.msgId), tags);
                    }
                } else {
                    _msgTags.put(new Long(tmi.msgId), tags);
                    //_ui.debugMessage("tags for msg " + msgId + ": " + tags);
                }
            }
        }
        // now we gather threads out of the remaining (inserting stubs between them as necessary)
        long beforeGather = System.currentTimeMillis();
        _ui.debugMessage("filter individual messages by thread took " + (beforeGather-afterFilterStatus));
        ThreadReferenceNode threads[] = buildThreads(matchingThreadMsgIds);
        long afterGather = System.currentTimeMillis();
        _ui.debugMessage("Build threads took " + (afterGather-beforeGather) + "ms to gather " + threads.length + " threads");
        
        // then drop the threads who do not match the tags (if !_applyTagFilterToMessages)
        if (tagFilter) {
            if (!_applyTagFilterToMessages) {
                List tagBuf = new ArrayList();
                for (int i = 0; i < threads.length; i++) {
                    threads[i].getThreadTags(tagBuf, _msgTags);
                    if (!tagFilterPassed(tagBuf)) {
                        _ui.debugMessage("reject thread because tag filters failed: " + tagBuf + ":" + threads[i]);
                        threads[i] = null;
                    }
                    tagBuf.clear();
                }
            }
        }
        long afterThreadTagFilter = System.currentTimeMillis();
        // now filter the remaining threads by authorization status (owner/manager/authPoster/authReply/unauth)
        // (done against the thread so as to allow simple authReply)
        for (int i = 0; i < threads.length; i++) {
            if (threads[i] != null) {
                boolean empty = filterAuthorizationStatus(threads[i]);
                if (empty) {
                    _ui.debugMessage("reject because authorization status failed: " + threads[i]);
                    threads[i] = null;
                }
            }
        }
        long afterAuthorizationFilter = System.currentTimeMillis();

        // filter the messages in the threads by type (pbe/private/public/authorized)
        if ( !_pbe || !_privateMessage || !_publicMessage || !_authorizedMessage) {
            for (int i = 0; i < threads.length; i++) {    
                if (threads[i] != null) {
                    boolean empty = filterPrivacy(threads[i]);
                    if (empty) {
                        _ui.debugMessage("reject because privacy failed: " + threads[i]);
                        threads[i] = null;
                    }
                }
            }
        }
        long afterThreadPrivacyFilter = System.currentTimeMillis();
        
        // filter the messages in the threads by keyword (we do this so late in the game in the
        // hopes that the above will minimize how much we have to filter w/ fulltext searches..)
        if ( (_keyword != null) && (_keyword.length() > 0) ) {
            for (int i = 0; i < threads.length; i++) {    
                if (threads[i] != null) {
                    boolean empty = filterKeyword(threads[i]);
                    if (empty) {
                        _ui.debugMessage("reject because keyword search failed: " + threads[i]);
                        threads[i] = null;
                    }
                }
            }
        }
        long afterThreadKeywordFilter = System.currentTimeMillis();
        
        // prune like a motherfucker,
        // and store the results in the accumulator's vars
        ThreadReferenceNode pruned[] = prune(threads, matchingThreadMsgIds);
        long afterPrune = System.currentTimeMillis();
        _ui.debugMessage("threads pruned: " + (pruned != null ? pruned.length +"" : "none"));
        ThreadReferenceNode sorted[] = sort(pruned);
        long afterSort = System.currentTimeMillis();
        _ui.debugMessage("threads sorted: " + (pruned != null ? pruned.length +"" : "none"));
        storePruned(sorted);
        long afterStore = System.currentTimeMillis();
           
        _ui.debugMessage("gather threads trace: " + _client.completeTrace());
        _ui.debugMessage("gather: " + (afterGather-beforeGather));
        _ui.debugMessage("threadTagFilter: " + (afterThreadTagFilter-afterGather));
        _ui.debugMessage("authorizationFilter: " + (afterAuthorizationFilter-afterThreadTagFilter));
        _ui.debugMessage("privacyFilter: " + (afterThreadPrivacyFilter-afterAuthorizationFilter));
        _ui.debugMessage("keywordFilter: " + (afterThreadKeywordFilter-afterThreadPrivacyFilter));
        _ui.debugMessage("prune: " + (afterPrune-afterThreadKeywordFilter));
        _ui.debugMessage("sort: " + (afterSort-afterPrune));
        _ui.debugMessage("store: " + (afterStore-afterSort));
        //_ui.debugMessage("threads: " + _roots);
    }
    
    private static final String SQL_GET_BASE_MSGS_BY_TARGET = "SELECT msgId, cs.channelHash, messageId FROM channelMessage m " +
                "JOIN channel c ON targetChannelId = c.channelId " +
                "JOIN channel cs ON scopeChannelId = cs.channelId " +
                "WHERE c.channelHash = ? AND m.importDate > ? AND messageId > ? " +
                "AND m.isCancelled = FALSE AND m.readKeyMissing = false " +
                "AND m.pbePrompt IS NULL AND m.replyKeyMissing = false";
    private static final String SQL_GET_BASE_MSGS_ALLCHANS = "SELECT msgId, channelHash, messageId FROM channelMessage " +
                "JOIN channel ON scopeChannelId = channelId " +
                "WHERE importDate > ? AND messageId > ? " +
                "AND isCancelled = FALSE AND readKeyMissing = false " +
                "AND pbePrompt IS NULL AND replyKeyMissing = false";
    
    private static final String SQL_GET_BASE_MSGS_BY_TARGET_PBE = "SELECT msgId, cs.channelHash, messageId FROM channelMessage m " +
                "JOIN channel c ON targetChannelId = c.channelId " +
                "JOIN channel cs ON scopeChannelId = cs.channelId " +
                "WHERE c.channelHash = ? AND m.importDate > ? AND messageId > ? " +
                "AND m.pbePrompt IS NOT NULL";
    private static final String SQL_GET_BASE_MSGS_ALLCHANS_PBE = "SELECT msgId, channelHash, messageId FROM channelMessage m " +
                "JOIN channel ON scopeChannelId = channelId " +
                "WHERE m.importDate > ? AND messageId > ? " +
                "AND m.pbePrompt IS NOT NULL";
    private Set getMatchingThreadMsgIds() { return getMatchingThreadMsgIds(false); }
    private Set getMatchingThreadMsgIds(boolean pbePending) {
        long minImportDate = _earliestReceiveDate;
        long minMsgId = _earliestPostDate;
        
        Set matchingThreadMsgIds = new HashSet();
        
        // do gather threads
        PreparedStatement stmt = null;
        ResultSet rs = null;
        
        try {
            if (_channelHashes != null) {
                String query = SQL_GET_BASE_MSGS_BY_TARGET;
                if (pbePending)
                    query = SQL_GET_BASE_MSGS_BY_TARGET_PBE;
                stmt = _client.con().prepareStatement(query);
                
                for (Iterator iter = _channelHashes.iterator(); iter.hasNext(); ) {
                    Hash chan = (Hash)iter.next();
                    stmt.setBytes(1, chan.getData());
                    stmt.setDate(2, new java.sql.Date(minImportDate));
                    stmt.setLong(3, minMsgId);
                    
                    _ui.debugMessage("query for msgs: " + query + " [" + chan.toBase64() + ", " + DataHelper.formatDuration(System.currentTimeMillis()-minImportDate) + ", " + minMsgId + ")");
                    rs = stmt.executeQuery();
                    while (rs.next()) {
                        long msgId = rs.getLong(1);
                        if (rs.wasNull()) continue;
                        byte scope[] = rs.getBytes(2);
                        if ( (scope == null) || (scope.length != Hash.HASH_LENGTH) )
                            continue;
                        long messageId = rs.getLong(3);
                        if (rs.wasNull()) continue;
                        
                        ThreadMsgId tmi = new ThreadMsgId(msgId);
                        tmi.scope = new Hash(scope);
                        tmi.messageId = messageId;
                        matchingThreadMsgIds.add(tmi);
                    }
                    rs.close();
                    rs = null;
                }
                stmt.close();
                stmt = null;
            } else {
                String query = SQL_GET_BASE_MSGS_ALLCHANS;
                if (pbePending)
                    query = SQL_GET_BASE_MSGS_ALLCHANS_PBE;
                stmt = _client.con().prepareStatement(query);
                stmt.setDate(1, new java.sql.Date(minImportDate));
                stmt.setLong(2, minMsgId);

                _ui.debugMessage("query for msgs: " + query + " [" + DataHelper.formatDuration(System.currentTimeMillis()-minImportDate) + ", " + minMsgId + ")");

                rs = stmt.executeQuery();
                while (rs.next()) {
                    long msgId = rs.getLong(1);
                    if (rs.wasNull()) continue;
                    byte scope[] = rs.getBytes(2);
                    if ( (scope == null) || (scope.length != Hash.HASH_LENGTH) ) continue;
                    long messageId = rs.getLong(3);
                    if (rs.wasNull()) continue;

                    ThreadMsgId tmi = new ThreadMsgId(msgId);
                    tmi.scope = new Hash(scope);
                    tmi.messageId = messageId;
                    matchingThreadMsgIds.add(tmi);
                }
                rs.close();
                rs = null;
                stmt.close();
                stmt = null;
            }
        } catch (SQLException se) {
            _ui.errorMessage("Internal error gathering threads", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return matchingThreadMsgIds;
    }
    
    private void gatherPBEPendingDecryption() {
        Set matchingThreadMsgIds = getMatchingThreadMsgIds(true);
        _ui.debugMessage("PBE pending matching msgIds: " + matchingThreadMsgIds);
        
        // the messages are still encrypted, so we dont know too much.  fake
        // what we do know though
        for (Iterator iter = matchingThreadMsgIds.iterator(); iter.hasNext(); ) {
            ThreadMsgId tmi = (ThreadMsgId)iter.next();
            long chanId = _client.getChannelId(tmi.scope);

            ThreadReferenceNode node = new ThreadReferenceNode(tmi);
            node.setURI(SyndieURI.createMessage(tmi.scope, tmi.messageId));
            node.setAuthorId(chanId);
            node.setName("");
            node.setDescription("");
            node.setThreadTarget(chanId);

            _roots.add(node);
            _rootURIs.add(node.getURI());
            _threadLatestAuthorId.add(new Long(chanId));
            _threadLatestPostDate.add(new Long(tmi.messageId));
            _threadMessages.add(new Integer(1));
            _threadRootAuthorId.add(new Long(chanId));
            _threadSubject.add("");
            _threadTags.add(new ArrayList(0));
        }
    }
    
    private ThreadReferenceNode[] buildThreads(Set matchingThreadMsgIds) {
        _ui.debugMessage("building threads w/ matching msgIds: " + matchingThreadMsgIds);
        ThreadBuilder b = new ThreadBuilder(_client, _ui);
        long before = System.currentTimeMillis();
        List rv = b.buildThread(matchingThreadMsgIds);
        long after = System.currentTimeMillis();
        _ui.debugMessage("build threads took " + (after-before) + " to build: \n" + rv);
        return (ThreadReferenceNode[])rv.toArray(new ThreadReferenceNode[0]);
    }
    
    private static final String SQL_BUILD_ANCESTORS = 
            "SELECT referencedChannelHash, referencedMessageId, referencedCloseness, cm.msgId, cm.readKeyMissing, cm.pbePrompt, cm.replyKeyMissing " +
            "FROM messageHierarchy mh " +
            "LEFT OUTER JOIN channel c ON channelHash = referencedChannelHash " +
            "LEFT OUTER JOIN channelMessage cm ON messageId = referencedMessageId AND cm.scopeChannelId = c.channelId " +
            "WHERE mh.msgId = ? " +
            "ORDER BY referencedCloseness ASC";
    public static int buildAncestors(DBClient client, UI ui, ThreadMsgId tmi, Map existingAncestors) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        
        List pendingThreadMsgIds = new ArrayList();
        pendingThreadMsgIds.add(tmi);
        
        int queryRuns = 0;
        long queryTime = 0;
        int queryMatches = 0;
        try {
            stmt = client.con().prepareStatement(SQL_BUILD_ANCESTORS);
            while (pendingThreadMsgIds.size() > 0) {
                tmi = (ThreadMsgId)pendingThreadMsgIds.remove(0);
                List rv = (List)existingAncestors.get(tmi);
                if (rv == null) {
                    rv = new ArrayList();
                    existingAncestors.put(tmi, rv);
                }
                stmt.setLong(1, tmi.msgId);
                queryRuns++;
                long before = System.currentTimeMillis();
                rs = stmt.executeQuery();
                while (rs.next()) {
                    queryMatches++;
                    byte chanHash[] = rs.getBytes(1);
                    long messageId = rs.getLong(2);
                    int closeness = rs.getInt(3);
                    long ancestorMsgId = rs.getLong(4);
                    if (rs.wasNull()) ancestorMsgId = -1;
                    boolean readKeyMissing = rs.getBoolean(5);
                    if (rs.wasNull()) readKeyMissing = false;
                    String pbePrompt = rs.getString(6);
                    boolean replyKeyMissing = rs.getBoolean(7);
                    if (rs.wasNull()) replyKeyMissing = false;
                    
                    ThreadMsgId ancestor = new ThreadMsgId(ancestorMsgId);
                    ancestor.messageId = messageId;
                    if ( (chanHash != null) && (chanHash.length == Hash.HASH_LENGTH) )
                        ancestor.scope = new Hash(chanHash);
                    
                    // if we don't have the actual data, just use a dummy
                    if ( (pbePrompt != null) || (replyKeyMissing) || (readKeyMissing) )
                        ancestor.unreadable = true;
                    
                    if (!rv.contains(ancestor)) {
                        rv.add(ancestor);
                        if (ancestorMsgId >= 0) {
                            Long aMsgId = new Long(ancestorMsgId);
                            if (!existingAncestors.containsKey(aMsgId) && !pendingThreadMsgIds.contains(ancestor))
                                pendingThreadMsgIds.add(ancestor);
                        }
                    }
                }
                
                long after = System.currentTimeMillis();
                queryTime += (after-before);
                rs.close();
                rs = null;
            }
            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            ui.errorMessage("Internal error building ancestors", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        //_ui.debugMessage("building ancestors, query " + queryRuns + " in " + queryTime + " w/ " + queryMatches + " matches");
        return queryMatches;
    }
    
    /**
     * null out any messages in the thread who do not meet the authorization criteria,
     * returning true if the entire thread was nulled out
     */
    private boolean filterAuthorizationStatus(ThreadReferenceNode root) {
        long targetChannelId = root.getThreadTarget();
        if (targetChannelId == -1)
            return true;
        Set allowedAuthorIds = new HashSet();
        if (_includeOwners)
            allowedAuthorIds.add(new Long(targetChannelId));
        List authKeys = _client.getAuthorizedPosters(targetChannelId, false, true, true);
        for (int i = 0; i < authKeys.size(); i++) {
            Hash chan = ((SigningPublicKey)authKeys.get(i)).calculateHash();
            long chanId = _client.getChannelId(chan);
            // we don't need to worry about authors that can post but that we don't have a channelId for,
            // since we only care about the ones we have messages from (and we always have a channelId for
            // things we have messages from)
            if (chanId >= 0)
                allowedAuthorIds.add(new Long(chanId));
        }
        boolean allowAnyone = _client.getChannelAllowPublicPosts(targetChannelId);
        boolean allowPublicReplies = _client.getChannelAllowPublicReplies(targetChannelId);
        boolean rv = filterAuthorizationStatus(root, allowedAuthorIds, _includeAuthorizedReplies && allowPublicReplies, allowAnyone, false);
        if (rv)
            _ui.debugMessage("filter auth status rejects w/ target=" + targetChannelId + ", allowedAuthorIds=" + allowedAuthorIds + ", allowAnyone=" + allowAnyone + ", allowPubReply=" + allowPublicReplies + ": " + root);
        return rv;
    }
    
    /** flag any unauthorized posts as dummy nodes, returning true if the entire tree rooted at the node is made of dummies  */
    private boolean filterAuthorizationStatus(ThreadReferenceNode node, Set authorIds, boolean authorizeReplies, boolean allowAnyone, boolean parentIsAuthorized) {
        boolean rv = true;
        boolean nodeIsAuthorized = allowAnyone;
        if (!node.isDummy()) {
            long authorId = node.getAuthorId();
            if (authorIds.contains(new Long(authorId))) {
                nodeIsAuthorized = true;
            } else if (authorizeReplies && parentIsAuthorized) {
                nodeIsAuthorized = true;
            }
            if (!nodeIsAuthorized) {
                _ui.debugMessage("node wasn't a dummy, but they're not sufficiently authorized: " + node.getAuthorId() + "/" + node.getURI().toString() + " parentAuth?" + parentIsAuthorized);
                _ui.debugMessage("parent: " + node.getParent());                
                node.setIsDummy(true);
            }
        } else {
            _ui.debugMessage("node is a dummy: " + node.getMsgId());
        }
        if (!node.isDummy())
            rv = false;
        for (int i = 0; i < node.getChildCount(); i++) {
            boolean childIsEmpty = filterAuthorizationStatus((ThreadReferenceNode)node.getChild(i), authorIds, authorizeReplies, allowAnyone, nodeIsAuthorized || parentIsAuthorized);
            rv = rv && childIsEmpty;
        }
        return rv;
    }
    
    /**
     * null out any messages in the thread who do not meet the privacy requirements,
     * returning true if the entire thread was nulled out
     */
    private boolean filterPrivacy(ThreadReferenceNode node) {
        boolean rv = true;
        if (!node.isDummy()) {
            ThreadMsgId id = node.getMsgId();
            if (id != null) {
                int privacy = _client.getMessagePrivacy(id.msgId);
                switch (privacy) {
                    case DBClient.PRIVACY_AUTHORIZEDONLY:
                        if (!_authorizedMessage)
                            node.setIsDummy(true);
                        break;
                    case DBClient.PRIVACY_PBE:
                        if (!_pbe)
                            node.setIsDummy(true);
                        break;
                    case DBClient.PRIVACY_PRIVREPLY:
                        if (!_privateMessage)
                            node.setIsDummy(true);
                        break;
                    case DBClient.PRIVACY_PUBLIC:
                        if (!_publicMessage)
                            node.setIsDummy(true);
                        break;
                }
                if (node.isDummy())
                    _ui.debugMessage("rejecting a node because of the privacy needs: " + privacy + ": " + id.msgId);
            }
        } else {
            //_ui.debugMessage("node is a dummy: " + node.getMsgId());
        }
        if (!node.isDummy())
            rv = false;
        for (int i = 0; i < node.getChildCount(); i++) {
            boolean childIsEmpty = filterPrivacy((ThreadReferenceNode)node.getChild(i));
            rv = rv && childIsEmpty;
        }
        _ui.debugMessage("filter privacy rv for " + node.getAuthorId() + ": " + rv + " - " + node.getURI().toString());
        return rv;
    }
    
    /**
     * null out any messages in the thread who do not have the keyword,
     * returning true if the entire thread was nulled out
     */
    private boolean filterKeyword(ThreadReferenceNode node) {
        boolean rv = true;
        if (!node.isDummy()) {
            ThreadMsgId id = node.getMsgId();
            if (id != null) {
                boolean match = _client.messageKeywordMatch(id.msgId, _keyword);
                if (!match) {
                    _ui.debugMessage("reject " + id + " because it didn't match the keyword");
                    node.setIsDummy(true);
                }
            }
        } else {
            //_ui.debugMessage("node is a dummy: " + node.getMsgId());
        }
        if (!node.isDummy())
            rv = false;
        for (int i = 0; i < node.getChildCount(); i++) {
            boolean childIsEmpty = filterKeyword((ThreadReferenceNode)node.getChild(i));
            rv = rv && childIsEmpty;
        }
        _ui.debugMessage("filter keyword rv for " + node.getAuthorId() + ": " + rv + " - " + node.getURI().toString());
        return rv;
    }
    
    private ThreadReferenceNode[] sort(ThreadReferenceNode roots[]) {
        return sort(roots, null);
    }
    private ThreadReferenceNode[] sort(ThreadReferenceNode peers[], ThreadReferenceNode parent) {
        ThreadReferenceNode sorted[] = sortSiblings(peers);
        if (parent != null)
            parent.setChildren(sorted);
        for (int i = 0; i < sorted.length; i++)
            sort(sorted[i].getChildren(), sorted[i]);
        return sorted;
    }
    private ThreadReferenceNode[] sortSiblings(ThreadReferenceNode peers[]) {
        if ( (peers == null) || (peers.length <= 1) ) return peers;
        switch (_sortField) {
            case SORT_AUTHOR: return sortAuthor(peers);
            case SORT_FORUM: return sortForum(peers);
            case SORT_SUBJECT: return sortSubject(peers);
            case SORT_DATE: 
            default:
                return sortDate(peers);
        }
    }
    private ThreadReferenceNode[] sortAuthor(ThreadReferenceNode peers[]) {
        TreeSet sorted = new TreeSet(_sortOrderAscending ? ASCENDING_COMPARATOR : DESCENDING_COMPARATOR);
        HashMap keyToNode = new HashMap();
        for (int i = 0; i < peers.length; i++) {
            String author = peers[i].getName();
            if (author == null) author = "";
            author = author.toLowerCase();
            int dup = 0;
            String key = author;
            while (keyToNode.containsKey(key)) {
                key = author + " " + dup;
                dup++;
            }
            keyToNode.put(key, peers[i]);
            sorted.add(key);
        }
        _ui.debugMessage("sorting by author/" + _sortOrderAscending + " among " + peers.length + " peers");
        ThreadReferenceNode rv[] = new ThreadReferenceNode[peers.length];
        int i = 0;
        for (Iterator iter = sorted.iterator(); iter.hasNext(); i++)
            rv[i] = (ThreadReferenceNode)keyToNode.get(iter.next());
        return rv;
    }
    private ThreadReferenceNode[] sortForum(ThreadReferenceNode peers[]) { 
        TreeSet sorted = new TreeSet(_sortOrderAscending ? ASCENDING_COMPARATOR : DESCENDING_COMPARATOR);
        HashMap keyToNode = new HashMap();
        for (int i = 0; i < peers.length; i++) {
            // todo: make this sort on the forum name, not its local internal channelId
            String target = peers[i].getThreadTarget() + "";
            // sorting with spaces at the end instead of numerically keeps all threads in the same channel together
            int dup = 0;
            String key = target;
            while (keyToNode.containsKey(key)) {
                key = target + " " + dup;
                dup++;
            }
            keyToNode.put(key, peers[i]);
            sorted.add(key);
        }
        _ui.debugMessage("sorting by forum/" + _sortOrderAscending + " among " + peers.length + " peers");
        ThreadReferenceNode rv[] = new ThreadReferenceNode[peers.length];
        int i = 0;
        for (Iterator iter = sorted.iterator(); iter.hasNext(); i++)
            rv[i] = (ThreadReferenceNode)keyToNode.get(iter.next());
        return rv;
    }
    private ThreadReferenceNode[] sortSubject(ThreadReferenceNode peers[]) { 
        TreeSet sorted = new TreeSet(_sortOrderAscending ? ASCENDING_COMPARATOR : DESCENDING_COMPARATOR);
        HashMap keyToNode = new HashMap();
        for (int i = 0; i < peers.length; i++) {
            String subject = peers[i].getSubject();
            if (subject == null) subject = "";
            subject = subject.toLowerCase();
            //_ui.debugMessage("sorting subject [" + subject + "], for message " + peers[i].getMsgId() + "/" + i);
            String key = subject;
            int dup = 0;
            while (keyToNode.containsKey(key)) {
                key = subject + " " + dup;
                dup++;
            }
            keyToNode.put(key, peers[i]);
            sorted.add(key);
        }
        _ui.debugMessage("sorting by subject/" + _sortOrderAscending + " among " + peers.length + " peers");
        ThreadReferenceNode rv[] = new ThreadReferenceNode[peers.length];
        int i = 0;
        for (Iterator iter = sorted.iterator(); iter.hasNext(); i++)
            rv[i] = (ThreadReferenceNode)keyToNode.get(iter.next());
        return rv;
    }
    /** sort by *(sub)thread* date, not by message date */
    private ThreadReferenceNode[] sortDate(ThreadReferenceNode peers[]) {
        TreeSet sorted = new TreeSet(_sortOrderAscending ? ASCENDING_COMPARATOR : DESCENDING_COMPARATOR);
        HashMap keyToNode = new HashMap();
        for (int i = 0; i < peers.length; i++) {
            long when = -1;
            if (_earliestReceiveDate > 0) {
                // we are filtering by import date, not post date
                when = peers[i].getLatestImportDate(_client);
            } else {
                // filtering by post date, not import date
                when = peers[i].getLatestMessageId();
            }
            while (keyToNode.containsKey(new Long(when)))
                when++;
            keyToNode.put(new Long(when), peers[i]);
            sorted.add(new Long(when));
        }
        _ui.debugMessage("sorting by date/" + _sortOrderAscending + " among " + peers.length + " peers");
        ThreadReferenceNode rv[] = new ThreadReferenceNode[peers.length];
        int i = 0;
        for (Iterator iter = sorted.iterator(); iter.hasNext(); i++)
            rv[i] = (ThreadReferenceNode)keyToNode.get(iter.next());
        return rv;
    }
    
    private static final Comparator ASCENDING_COMPARATOR = new Comparator() {
        public int compare(Object lhs, Object rhs) { return compareObj(lhs, rhs); }
        public boolean equals(Object obj) { return obj == this; }
    };
    private static final Comparator DESCENDING_COMPARATOR = new Comparator() {
        public int compare(Object lhs, Object rhs) { return compareObj(rhs, lhs); } // note order
        public boolean equals(Object obj) { return obj == this; }
    };
    private static final int compareObj(Object lhs, Object rhs) {
        if ( (lhs == null) && (rhs == null) ) return 0;
        if (lhs == null) return 1;
        if (rhs == null) return -1;
        if (lhs instanceof Comparable)
            return ((Comparable)lhs).compareTo(rhs);
        else
            return lhs.toString().compareTo(rhs.toString());
    }
    
    private ThreadReferenceNode[] prune(ThreadReferenceNode roots[], Set matchingThreadMsgIds) {
        List remaining = new ArrayList(roots.length);
        for (int i = 0; i < roots.length; i++) {
            if (roots[i] == null) {
                continue;
            } else {
                // go in deeper to prune out children as necessary
                Set threadMatches = new HashSet();
                ThreadReferenceNode newRoot = prune(roots[i], null, matchingThreadMsgIds, threadMatches);
                if ( (newRoot != null) && (threadMatches.size() > 0) )
                    remaining.add(newRoot);
            }
        }
        return (ThreadReferenceNode[])remaining.toArray(new ThreadReferenceNode[0]);
    }
    private ThreadReferenceNode prune(ThreadReferenceNode cur, ThreadReferenceNode parent, Set matchingThreadMsgIds, Set threadMatches) {
        // add the threadMsgId to threadMatches for every thread element also in matchingThreadMsgIds
        // if the node is a dummy and has no children, drop the node by returning null
        // if the node is a dummy and has 1 child, return the pruned child
        // if the node is a dummy and has more than 1 child after pruning, return the dummy
        // if the node is not a dummy, return the node after pruning the children
        
        // the above leaves the following untouched ([dummy])
        // a<--[b]<--[c]<--d
        //       \     \---e
        //        \--[f]<--g
        //             \---h
        
        // but it turns:
        // a<--[b]<--[c]<--d
        //       \     \---e
        //        \--[f]<--g
        // into:
        // a<--[b]<--[c]<--d
        //       \     \---e
        //        \---g
        
        // and it turns:
        // a<--[b]<--[c]<--d
        //       \    
        //        \--[f]<--g
        // into:
        // a<--[b]<---d
        //       \    
        //        \---g
        
        // and it turns:
        // a<--[b]<--[c]<--[d]<--e
        //       \    
        //        \--[f]<--[g]<--h
        // into:
        // a<--[b]<---e
        //       \    
        //        \---h
        
        // prune the kids recursively first
        ThreadReferenceNode children[] = new ThreadReferenceNode[cur.getChildCount()];
        for (int i = 0; i < children.length; i++) {
            ThreadReferenceNode child = (ThreadReferenceNode)cur.getChild(0);
            cur.removeChild(child);
            children[i] = prune(child, cur, matchingThreadMsgIds, threadMatches);
        }
        for (int i = 0; i < children.length; i++) {
            if (children[i] != null)
                cur.addChild(children[i]);
        }
        
        // now compress the current node if necessary
        if (cur.isDummy() && cur.getChildCount() == 0) {
            if (parent != null)
                parent.removeChild(cur);
            return null;
        } else if (cur.isDummy() && cur.getChildCount() == 1) {
            if (parent != null)
                parent.removeChild(cur);
            ThreadReferenceNode child = (ThreadReferenceNode)cur.getChild(0);
            if (parent != null)
                parent.addChild(child);
            return child;
        } else if (cur.isDummy()) {
            // dummy with more than one pruned child.. gotta keep 'er
            return cur;
        } else {
            if (matchingThreadMsgIds.contains(cur.getMsgId()))
                threadMatches.add(cur.getMsgId());
            return cur;
        }
    }
    
    private void storePruned(ThreadReferenceNode roots[]) {
        for (int i = 0; i < roots.length; i++) {
            if (roots[i] == null) {
                continue;
            } else {
                _roots.add(roots[i]);
                _rootURIs.add(roots[i].getURI());
                _threadLatestAuthorId.add(new Long(roots[i].getLatestAuthorId()));
                _threadLatestPostDate.add(new Long(roots[i].getLatestPostDate()));
                _threadMessages.add(new Integer(roots[i].getMessageCount()));
                _threadRootAuthorId.add(new Long(roots[i].getAuthorId()));
                _threadSubject.add(roots[i].getThreadSubject());
                List tags = new ArrayList();
                roots[i].getThreadTags(tags, _msgTags);
                _threadTags.add(tags);
            }
        }
    }
    
    /** return true if the tags for the message meet our search criteria */
    private boolean tagFilterPassed(Collection tags) {
        if (_rejectedTags != null) {
            for (Iterator iter = _rejectedTags.iterator(); iter.hasNext(); ) {
                String tag = (String)iter.next();
                if (tags.contains(tag)) {
                    _ui.debugMessage("Rejecting thread tagged with " + tag);
                    return false;
                } else {
                    if (tag.endsWith("*") && (tag.length() > 0)) {
                        // substring match
                        String prefix = tag.substring(0, tag.length()-1);
                        boolean substringMatch = false;
                        for (Iterator msgTagIter = tags.iterator(); msgTagIter.hasNext(); ) {
                            String cur = (String)msgTagIter.next();
                            if (cur.startsWith(prefix)) {
                                _ui.debugMessage("Rejecting thread prefix tagged with " + tag);
                                return false;
                            }
                        }
                    }
                }
            }
        }
        if ( (_requiredTags != null) && (_requiredTags.size() > 0) ) {
            for (Iterator iter = _requiredTags.iterator(); iter.hasNext(); ) {
                String tag = (String)iter.next();
                if (!tags.contains(tag)) {
                    if (tag.endsWith("*") && (tag.length() > 0)) {
                        // substring match
                        String prefix = tag.substring(0, tag.length()-1);
                        boolean substringMatch = false;
                        for (Iterator msgTagIter = tags.iterator(); msgTagIter.hasNext(); ) {
                            String cur = (String)msgTagIter.next();
                            if (cur.startsWith(prefix)) {
                                substringMatch = true;
                                break;
                            }
                        }
                        if (substringMatch) {
                            //_ui.debugMessage("Substring tagged with " + tag);
                        } else {
                            _ui.debugMessage("Rejecting thread not substring tagged with " + tag);
                            return false;
                        }
                    } else {
                        _ui.debugMessage("Rejecting thread not tagged with " + tag);
                        return false;
                    }
                }
            }
        }
        if ( (_wantedTags != null) && (_wantedTags.size() > 0) ) {
            boolean found = false;
            for (Iterator iter = _wantedTags.iterator(); iter.hasNext(); ) {
                String tag = (String)iter.next();
                if (tags.contains(tag)) {
                    found = true;
                    break;
                } else {
                    if (tag.endsWith("*") && (tag.length() > 0)) {
                        // substring match
                        String prefix = tag.substring(0, tag.length()-1);
                        for (Iterator msgTagIter = tags.iterator(); msgTagIter.hasNext(); ) {
                            String cur = (String)msgTagIter.next();
                            if (cur.startsWith(prefix)) {
                                found = true;
                                break;
                            }
                        }
                        if (found)
                            break;
                    }
                }
            }
            if (!found) {
                _ui.debugMessage("Rejecting thread not tagged with any of the wanted tags (" + _wantedTags + ")");
                return false;
            }
        }
        return true;
    }
    
    private void init() {
        _roots = new ArrayList();
        _rootURIs = new ArrayList();
        _threadTags = new ArrayList();
        _threadMessages = new ArrayList();
        _threadSubject = new ArrayList();
        _threadRootAuthorId = new ArrayList();
        _threadLatestAuthorId = new ArrayList();
        _threadLatestPostDate = new ArrayList();
    }

    public String toString() {
        StringBuffer buf = new StringBuffer();
        buf.append(" threaded? ").append(_showThreaded);
        buf.append(" unreadOnly? ").append(_unreadOnly);
        buf.append(" owners? ").append(_includeOwners);
        buf.append(" managers? ").append(_includeManagers);
        buf.append(" authPosters? ").append(_includeAuthorizedPosters);
        buf.append(" authReplies? ").append(_includeAuthorizedReplies);
        buf.append(" unauthPosts? ").append(_includeUnauthorizedPosts);
        if (_earliestReceiveDate > 0)
            buf.append(" _earliestReceiveDate? ").append(Constants.getDate(_earliestReceiveDate));
        if (_earliestPostDate > 0)
            buf.append(" _earliestPostDate? ").append(Constants.getDate(_earliestPostDate));
        buf.append(" applyTagFilterToMessages? ").append(_applyTagFilterToMessages);
        buf.append(" pagesRequired? ").append(_minPages > 0);
        buf.append(" attachmentsRequired? ").append(_minAttachments > 0);
        buf.append(" refsRequired? ").append(_minReferences > 0);
        buf.append(" keysRequired? ").append(_minKeys > 0);
        buf.append(" decrypted? ").append(_alreadyDecrypted);
        buf.append(" PBE? ").append(_pbe);
        buf.append(" privateMessages? ").append(_privateMessage);
        if (_channelHashes != null)
            buf.append(" channels: ").append(_channelHashes);
        else
            buf.append(" channels: all");
        if ( (_requiredTags != null) && (_requiredTags.size() > 0) )
            buf.append(" requiredTags: [").append(_requiredTags).append("]");
        if ( (_wantedTags != null) && (_wantedTags.size() > 0) )
            buf.append(" wantedTags: [").append(_wantedTags).append("]");
        if ( (_rejectedTags != null) && (_rejectedTags.size() > 0) )
            buf.append(" rejectedTags: [").append(_rejectedTags).append("]");
        return buf.toString();
    }
}
