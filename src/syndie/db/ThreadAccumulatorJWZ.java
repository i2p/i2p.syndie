package syndie.db;

import java.util.*;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import net.i2p.data.Base64;
import net.i2p.data.Hash;
import net.i2p.data.SigningPublicKey;
import syndie.Constants;
import syndie.data.*;

/**
 * revamped thread gathering/filtering, based off the jwz threading alogrithm
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
        Set matchingMsgIds = getMatchingMsgIds();
        _ui.debugMessage("matching msgIds: " + matchingMsgIds);
        
        if (_unreadOnly) {
            for (Iterator iter = matchingMsgIds.iterator(); iter.hasNext(); ) {
                Long msgId = (Long)iter.next();
                int status = _client.getMessageStatus(msgId.longValue());
                if (DBClient.MSG_STATUS_NEW_UNREAD != status) {
                    _ui.debugMessage("reject " + msgId + " because status=" + status);
                    iter.remove();
                }
            }
        }
        
        _msgTags = new HashMap();
        // todo: skip this if there isn't a tag filter
        for (Iterator iter = matchingMsgIds.iterator(); iter.hasNext(); ) {
            Long msgId = (Long)iter.next();
            Set tags = _client.getMessageTags(msgId.longValue(), true, true);
            if (_applyTagFilterToMessages) {
                if (!tagFilterPassed(tags)) {
                    _ui.debugMessage("reject " + msgId + " because msg tag filters failed: " + tags);
                    iter.remove();
                } else {
                    _msgTags.put(msgId, tags);
                }
            } else {
                _msgTags.put(msgId, tags);
                //_ui.debugMessage("tags for msg " + msgId + ": " + tags);
            }
        }
        // now we gather threads out of the remaining (inserting stubs between them as necessary)
        long beforeGather = System.currentTimeMillis();
        ThreadReferenceNode threads[] = buildThreads(matchingMsgIds);
        long afterGather = System.currentTimeMillis();
        _ui.debugMessage("Build threads took " + (afterGather-beforeGather) + "ms to gather " + threads.length + " threads");
        
        // then drop the threads who do not match the tags (if !_applyTagFilterToMessages)
        if (!_applyTagFilterToMessages) {
            List tagBuf = new ArrayList();
            for (int i = 0; i < threads.length; i++) {
                threads[i].getThreadTags(tagBuf);
                if (!tagFilterPassed(tagBuf)) {
                    _ui.debugMessage("reject thread because tag filters failed: " + tagBuf + ":" + threads[i]);
                    threads[i] = null;
                }
                tagBuf.clear();
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
        ThreadReferenceNode pruned[] = prune(threads);
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
    
    private static final String SQL_GET_BASE_MSGS_BY_TARGET = "SELECT msgId FROM channelMessage " +
                "JOIN channel ON targetChannelId = channelId " +
                "WHERE channelHash = ? AND importDate > ? AND messageId > ? " +
                "AND isCancelled = FALSE AND readKeyMissing = false " +
                "AND pbePrompt IS NULL AND replyKeyMissing = false";
    private static final String SQL_GET_BASE_MSGS_ALLCHANS = "SELECT msgId FROM channelMessage " +
                "WHERE importDate > ? AND messageId > ? " +
                "AND isCancelled = FALSE AND readKeyMissing = false " +
                "AND pbePrompt IS NULL AND replyKeyMissing = false";
    
    private static final String SQL_GET_BASE_MSGS_BY_TARGET_PBE = "SELECT msgId FROM channelMessage " +
                "JOIN channel ON targetChannelId = channelId " +
                "WHERE channelHash = ? AND importDate > ? AND messageId > ? " +
                "AND pbePrompt IS NOT NULL";
    private static final String SQL_GET_BASE_MSGS_ALLCHANS_PBE = "SELECT msgId FROM channelMessage " +
                "WHERE importDate > ? AND messageId > ? " +
                "AND pbePrompt IS NOT NULL";
    private Set getMatchingMsgIds() { return getMatchingMsgIds(false); }
    private Set getMatchingMsgIds(boolean pbePending) {
        long minImportDate = _earliestReceiveDate;
        long minMsgId = _earliestPostDate;
        
        Set matchingMsgIds = new HashSet();
        
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
                    
                    rs = stmt.executeQuery();
                    while (rs.next()) {
                        long msgId = rs.getLong(1);
                        if (!rs.wasNull())
                            matchingMsgIds.add(new Long(msgId));
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
                    
                rs = stmt.executeQuery();
                while (rs.next()) {
                    long msgId = rs.getLong(1);
                    if (!rs.wasNull())
                        matchingMsgIds.add(new Long(msgId));
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
        return matchingMsgIds;
    }
    
    private void gatherPBEPendingDecryption() {
        Set matchingMsgIds = getMatchingMsgIds(true);
        _ui.debugMessage("PBE pending matching msgIds: " + matchingMsgIds);
        
        // the messages are still encrypted, so we dont know too much.  fake
        // what we do know though
        for (Iterator iter = matchingMsgIds.iterator(); iter.hasNext(); ) {
            Long msgId = (Long)iter.next();
            ThreadMsgId tmi = new ThreadMsgId(msgId.longValue());
            tmi.scope = _client.getMessageScope(tmi.msgId);
            tmi.messageId = _client.getMessageId(tmi.msgId);

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
    
    private ThreadReferenceNode[] buildThreads(Set matchingMsgIds) {
        _ui.debugMessage("building threads w/ matching msgIds: " + matchingMsgIds);
        long beforeAncestors = System.currentTimeMillis();
        Map ancestors = buildAncestors(matchingMsgIds);
        long afterAncestors = System.currentTimeMillis();
        _ui.debugMessage("finding ancestors for " + matchingMsgIds.size() + " took " + (afterAncestors-beforeAncestors));
        List rootMsgs = new ArrayList(ancestors.size());
        Map msgContainers = new HashMap();
        for (Iterator iter = ancestors.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry cur = (Map.Entry)iter.next();
            ThreadMsgId msg = (ThreadMsgId)cur.getKey();
            List msgAncestors = (List)cur.getValue();
            
            boolean foundRoot = false;
            for (int i = msgAncestors.size()-1; i >= 0; i--) {
                ThreadMsgId ancestorId = (ThreadMsgId)msgAncestors.get(i);
                if (rootMsgs.contains(ancestorId)) {
                    foundRoot = true;
                    break;
                }
            }
            if (!foundRoot) {
                if (msgAncestors.size() > 0) {
                    ThreadMsgId ancestor = (ThreadMsgId)msgAncestors.get(msgAncestors.size()-1);
                    if (!rootMsgs.contains(ancestor))
                        rootMsgs.add(ancestor);
                } else {
                    if (!rootMsgs.contains(msg))
                        rootMsgs.add(msg);
                }
            }
            
            // build up hierarchy relationships, jwz-threading style (though without
            // the rough missing-references/etc stuff, since syndie doesn't have backwards compatability)
            ThreadMsgId child = msg;
            ThreadContainer childContainer = (ThreadContainer)msgContainers.get(child);
            if (childContainer == null) {
                //_ui.debugMessage("building new container for current node " + msg);
                childContainer = new ThreadContainer();
                childContainer.msg = msg;
                childContainer.parent = null;
                childContainer.child = null;
                msgContainers.put(msg, childContainer);
            } else {
                //_ui.debugMessage("container exists for current node " + msg);
            }

            if (msgAncestors.size() > 0)
                _ui.debugMessage("building ancestor containers for " + msgAncestors + " above " + msg);
            for (int i = 0; i < msgAncestors.size(); i++) {
                ThreadMsgId ancestorId = (ThreadMsgId)msgAncestors.get(i);
                ThreadContainer container = (ThreadContainer)msgContainers.get(ancestorId);
                //_ui.debugMessage("ancestor: " + ancestorId + " container: " + container);
                
                if (childContainer.contains(ancestorId)) {
                    _ui.debugMessage("loop detected under child " + childContainer + " --> parent would be " + ancestorId);
                    continue;
                }
                
                if (container == null) {
                    //_ui.debugMessage("building new container for " + ancestorId + " w/ child " + child);
                    container = new ThreadContainer();
                    container.msg = ancestorId;
                    if ( (childContainer != null) && (childContainer.parent == null) )
                        childContainer.parent = container;
                    container.child = childContainer;
                    msgContainers.put(ancestorId, container);
                } else if (container.child != null) {
                    ThreadContainer curChild = container.child;
                    //_ui.debugMessage("building siblings for " + curChild + " under " + container.msg);
                    boolean alreadyContained = false;
                    while (curChild.nextSibling != null) {
                        //_ui.debugMessage("building siblings... container is " + container + ", curChild is " + curChild + ": next sibling is " + curChild.nextSibling);
                        if (curChild.contains(childContainer.msg)) {
                            _ui.debugMessage("loop avoided: child already contains the new branch");
                            alreadyContained = true;
                            break;
                        }
                        curChild = curChild.nextSibling;
                    }
                    //_ui.debugMessage("done building siblings for " + curChild + " under " + container.msg);
                    if (alreadyContained)
                        continue;
                    //??
                    if (!curChild.contains(childContainer.msg)) {
                        curChild.nextSibling = childContainer;
                        _ui.debugMessage("set the last sibling to the new child: " + child);
                    } else {
                        _ui.debugMessage("loop found and avoided, but should have been detected");
                        continue;
                    }
                    //    curChild.nextSibling = (ThreadContainer)msgContainers.get(child);
                    //} else {
                    //    _ui.debugMessage("building siblings, but the child " + child + " is already in the tree: " + container);
                    //}
                } else {
                    //_ui.debugMessage("existing container has no children, setting their child to " + child);
                    container.child = childContainer;
                }
                child = ancestorId;
                childContainer = container;
            }
        }

        // now we can build the ThreadReferenceNode instances out of these
        List roots = new ArrayList(rootMsgs.size());
        Set fakeRoots = new HashSet();
        //_ui.debugMessage("roots: " + rootMsgs);
        for (int i = 0; i < rootMsgs.size(); i++) {
            ThreadMsgId root = (ThreadMsgId)rootMsgs.get(i);
            //_ui.debugMessage("building thread root " + i + ": " + root + " --> " + ancestors.get(root));
            ThreadReferenceNode thread = buildThread(root, msgContainers, rootMsgs, fakeRoots);
            //_ui.debugMessage("done building thread root " + i + ": " + thread);
            if (thread != null)
                roots.add(thread);
        }
        for (Iterator iter = fakeRoots.iterator(); iter.hasNext(); ) {
            ThreadMsgId fakeRootId = (ThreadMsgId)iter.next();
            for (int j = 0; j < roots.size(); j++) {
                ThreadReferenceNode thread = (ThreadReferenceNode)roots.get(j);
                if (fakeRootId.equals(thread.getMsgId())) {
                    roots.remove(j);
                    break;
                }
            }
        }
        return (ThreadReferenceNode[])roots.toArray(new ThreadReferenceNode[0]);
    }
    
    private ThreadReferenceNode buildThread(ThreadMsgId rootMsg, Map msgContainers, List rootMsgs, Set fakeRoots) {
        ThreadReferenceNode node = new ThreadReferenceNode(rootMsg);
        node.setURI(SyndieURI.createMessage(rootMsg.scope, rootMsg.messageId));
        if ( (rootMsg.msgId >= 0) && (!rootMsg.unreadable) ) {
            node.setIsDummy(false);
            long authorId = _client.getMessageAuthor(rootMsg.msgId);
            String subject = _client.getMessageSubject(rootMsg.msgId);
            long target = _client.getMessageTarget(rootMsg.msgId);
            String authorName = _client.getChannelName(authorId);
            node.setAuthorId(authorId);
            node.setSubject(subject);
            node.setThreadTarget(target);
            
            List tags = new ArrayList(); node.getThreadTags(tags);
            //_ui.debugMessage("buildThread: msg: " + rootMsg + " authorId: " + authorId + " target: " + target + " authorName: " + authorName + " tags: " + tags);
           
            // to mirror the MessageThreadBuilder, fill the node in per:
            //
            // * each node has the author's preferred name stored in node.getName()
            // * and the message subject in node.getDescription(), with the message URI in
            // * node.getURI().
            //
            node.setName(authorName);
            node.setDescription(node.getThreadSubject());
        } else {
            //_ui.debugMessage("node is a dummy: " + rootMsg);
            node.setIsDummy(true);
        }
        ThreadContainer container = (ThreadContainer)msgContainers.get(rootMsg);
        if ( (container != null) && (container.child != null) ) {
            ThreadContainer child = container.child;
            while (child != null) {
                if (child.parent == null) {
                    //_ui.debugMessage("child of " + container.msg + " did not have a parent link from " + child.msg);
                    child.parent = container;
                }
                // fake roots happen w/ incomplete ancestry is listed in a message, so
                // simply make sure all children in a thread are not marked as roots for
                // some other thread
                if (rootMsgs.contains(child.msg)) {
                    //_ui.debugMessage("fake root under container " + rootMsg + "/" + child.msg);
                    fakeRoots.add(child.msg);
                } else {
                    //_ui.debugMessage("no fake root under container " + rootMsg + "/" + child.msg + ", building their subthread (parent: " + child.parent + ")");
                }
                // now build that child's thread
                //_ui.debugMessage("building child of " + rootMsg + ": " + child.msg);
                ThreadReferenceNode childNode = buildThread(child.msg, msgContainers, rootMsgs, fakeRoots);
                //_ui.debugMessage("child of " + rootMsg + ": " + child.msg + " built");
                node.addChild(childNode);
                child = child.nextSibling;
            }
        }

        //_ui.debugMessage("buildThread: done with msg: " + rootMsg);
        return node;
    }
    
    /** build a map of ThreadMsgId to a List of ThreadMsgId instances, most recent first */
    private Map buildAncestors(Set msgIds) {
        Map rv = new HashMap();
        for (Iterator iter = msgIds.iterator(); iter.hasNext(); ) {
            Long msgId = (Long)iter.next();
            ThreadMsgId tmi = new ThreadMsgId(msgId.longValue());
            tmi.scope = _client.getMessageScope(tmi.msgId);
            tmi.messageId = _client.getMessageId(tmi.msgId);
            List ancestors = (List)rv.get(tmi);
            if (ancestors == null) {
                ancestors = new ArrayList();
                rv.put(tmi, ancestors);
                // this effectively runs recursively to populate entries in rv for
                // the ancestors of the message and any of its ancestors, that we
                // know of
                if (_showThreaded)
                    buildAncestors(_client, _ui, tmi, rv);
            }
        }
        return rv;
    }
    
    /**
     * list of ancestors (SyndieURI) that are parents (or parents of parents, etc) of the msgId given 
     */
    public static List getAncestorURIs(DBClient client, UI ui, long msgId) {
        ThreadMsgId id = new ThreadMsgId(msgId);
        Map tmiToAncestorIds = new HashMap();
        int total = buildAncestors(client, ui, id, tmiToAncestorIds);
        //ui.debugMessage("ancestors for " + msgId + ": " + tmiToAncestorIds);
        List orderedAncestors = new ArrayList(tmiToAncestorIds.size());
        if (total > 0) // fetch this so the ThreadMsgId.hashCode will match
            id.messageId = client.getMessageId(msgId);
        walkAncestors(ui, id, orderedAncestors, tmiToAncestorIds);
        //ui.debugMessage("ordered ancestors: " + orderedAncestors);
        List rv = new ArrayList(orderedAncestors.size());
        for (int i = 0; i < orderedAncestors.size(); i++) {
            ThreadMsgId cur = (ThreadMsgId)orderedAncestors.get(i);
            rv.add(SyndieURI.createMessage(cur.scope, cur.messageId));
        }
        return rv;
    }
    private static void walkAncestors(UI ui, ThreadMsgId id, List rv, Map tmiToAncestorIds) {
        ThreadMsgId cur = id;
        List ancestors = (List)tmiToAncestorIds.get(cur);
        if (ancestors != null) {
            for (int i = 0; i < ancestors.size(); i++) {
                ThreadMsgId ancestor = (ThreadMsgId)ancestors.get(i);
                if (!rv.contains(ancestor))
                    rv.add(ancestor);
                walkAncestors(ui, ancestor, rv, tmiToAncestorIds);
            }
        }
    }
    
    private static final String SQL_BUILD_ANCESTORS = 
            "SELECT referencedChannelHash, referencedMessageId, referencedCloseness, cm.msgId, cm.readKeyMissing, cm.pbePrompt, cm.replyKeyMissing " +
            "FROM messageHierarchy mh " +
            "LEFT OUTER JOIN channel c ON channelHash = referencedChannelHash " +
            "LEFT OUTER JOIN channelMessage cm ON messageId = referencedMessageId AND cm.scopeChannelId = c.channelId " +
            "WHERE mh.msgId = ? " +
            "ORDER BY referencedCloseness ASC";
    private static int buildAncestors(DBClient client, UI ui, ThreadMsgId tmi, Map existingAncestors) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        
        List pendingMsgIds = new ArrayList();
        pendingMsgIds.add(new Long(tmi.msgId));
        
        int queryRuns = 0;
        long queryTime = 0;
        int queryMatches = 0;
        try {
            stmt = client.con().prepareStatement(SQL_BUILD_ANCESTORS);
            while (pendingMsgIds.size() > 0) {
                Long msgId = (Long)pendingMsgIds.remove(0);
                List rv = (List)existingAncestors.get(msgId);
                if (rv == null) {
                    rv = new ArrayList();
                    tmi = new ThreadMsgId(msgId.longValue());
                    tmi.scope = client.getMessageScope(tmi.msgId);
                    tmi.messageId = client.getMessageId(tmi.msgId);
                    existingAncestors.put(tmi, rv);
                }
                stmt.setLong(1, msgId.longValue());
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
                    
                    
                    rv.add(ancestor);
                    if (ancestorMsgId >= 0) {
                        Long aMsgId = new Long(ancestorMsgId);
                        if (!existingAncestors.containsKey(aMsgId) && !pendingMsgIds.contains(aMsgId))
                            pendingMsgIds.add(aMsgId);
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
    
    private static final class ThreadMsgId {
        public long msgId;
        public long messageId;
        public Hash scope;
        public boolean unreadable;
        public ThreadMsgId(long id) {
            msgId = id;
            messageId = -1;
            scope = null;
            unreadable = false;
        }
        public int hashCode() { return messageId >= 0 ? (int)messageId : (int)msgId; }
        public boolean equals(Object obj) throws ClassCastException {
            ThreadMsgId tmi = (ThreadMsgId)obj;
            return ( ( (tmi.msgId == msgId) && (tmi.msgId >= 0) ) || 
                     ( (tmi.messageId == messageId) && (tmi.scope != null) && (tmi.scope.equals(scope))));
        }
        public String toString() {
            return msgId + "/" + (scope != null ? scope.toBase64().substring(0,6) + ":" + messageId : "");
        }
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
        //_ui.debugMessage("filter thread status: allowedAuthorIds: " + allowedAuthorIds + " allowPubReplues to " + targetChannelId + ": " + allowPublicReplies + " root: " + root.getURI().toString());
        return filterAuthorizationStatus(root, allowedAuthorIds, _includeAuthorizedReplies && allowPublicReplies, allowAnyone, false);
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
                //_ui.debugMessage("parent: " + node.getParent());                
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
        //_ui.debugMessage("filter rv for " + node.getAuthorId() + ": " + rv + " - " + node.getURI().toString());
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
            while (keyToNode.containsKey(author))
                author = author + " ";
            keyToNode.put(author, peers[i]);
            sorted.add(author);
        }
        _ui.debugMessage("sorting by author/" + _sortOrderAscending + " among " + peers.length + " peers: sorted=" + sorted);        ThreadReferenceNode rv[] = new ThreadReferenceNode[peers.length];
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
            while (keyToNode.containsKey(target)) 
                target = target + " ";
            keyToNode.put(target, peers[i]);
            sorted.add(target);
        }
        _ui.debugMessage("sorting by forum/" + _sortOrderAscending + " among " + peers.length + " peers: sorted=" + sorted);
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
            _ui.debugMessage("sorting subject [" + subject + "], for message " + peers[i].getMsgId() + "/" + i);
            while (keyToNode.containsKey(subject))
                subject = subject + " ";
            keyToNode.put(subject, peers[i]);
            sorted.add(subject);
        }
        _ui.debugMessage("sorting by subject/" + _sortOrderAscending + " among " + peers.length + " peers: sorted=" + sorted);
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
            long when = peers[i].getLatestMessageId();
            while (keyToNode.containsKey(new Long(when)))
                when++;
            keyToNode.put(new Long(when), peers[i]);
            sorted.add(new Long(when));
        }
        _ui.debugMessage("sorting by date/" + _sortOrderAscending + " among " + peers.length + " peers: sorted=" + sorted);
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
    
    private ThreadReferenceNode[] prune(ThreadReferenceNode roots[]) {
        List remaining = new ArrayList(roots.length);
        for (int i = 0; i < roots.length; i++) {
            if (roots[i] == null) {
                continue;
            } else {
                // go in deeper to prune out children as necessary
                ThreadReferenceNode newRoot = prune(roots[i], null);
                if (newRoot != null)
                    remaining.add(newRoot);
            }
        }
        return (ThreadReferenceNode[])remaining.toArray(new ThreadReferenceNode[0]);
    }
    private ThreadReferenceNode prune(ThreadReferenceNode cur, ThreadReferenceNode parent) {
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
            children[i] = prune(child, cur);
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
                roots[i].getThreadTags(tags);
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

    /** jwz-esque */
    private static class ThreadContainer {
        ThreadMsgId msg;
        ThreadContainer parent;
        ThreadContainer child;
        ThreadContainer nextSibling;
        
        public String toString() { 
            StringBuffer buf = new StringBuffer();
            buf.append("C:").append(msg);
            buf.append("_");
            if (nextSibling == this)
                buf.append("**SELF**");
            buf.append("-");
            if (child == this)
                buf.append("**SELF**");
            return "C:" + msg + "_" + nextSibling + "-" + child; 
        }
        
        public boolean contains(ThreadMsgId query) {
            ThreadContainer cur = this;
            while (cur != null) {
                if ( (cur.msg != null) && (cur.msg.equals(query)) )
                    return true;
                if (cur.nextSibling != null)
                    cur = cur.nextSibling;
                else
                    cur = cur.child;
            }
            return false;
        }
    }
    
    private class ThreadReferenceNode extends ReferenceNode {
        private long _authorId;
        private String _subject;
        private long _targetChannelId;
        private boolean _dummy;
        private ThreadMsgId _msg;
        public ThreadReferenceNode(ThreadMsgId id) {
            super(null, null, null, null);
            _msg = id;
            _authorId = -1;
            _subject = null;
            _targetChannelId = -1;
            _dummy = false;
        }
        public ThreadMsgId getMsgId() { return _msg; }
        public void setAuthorId(long authorId) { _authorId = authorId; }
        public void setSubject(String subject) { _subject = subject; }
        public String getSubject() { return _subject; }
        public void setThreadTarget(long channelId) { _targetChannelId = channelId; }
        /** this node represents something we do not have locally, or is filtered */
        public boolean isDummy() { return _dummy || getUniqueId() < 0; }
        public void setIsDummy(boolean dummy) { _dummy = dummy; }
        public long getThreadTarget() {
            if (_targetChannelId >= 0)
                return _targetChannelId;
            for (int i = 0; i < getChildCount(); i++) {
                long id = ((ThreadReferenceNode)getChild(i)).getThreadTarget();
                if (id >= 0)
                    return id;
            }
            return -1;
        }
        public void getThreadTags(List rv) { 
            if (_msg != null) {
                Set tags = (Set)_msgTags.get(new Long(_msg.msgId));
                if (tags != null)
                    rv.addAll(tags);
            }
            for (int i = 0; i < getChildCount(); i++)
                ((ThreadReferenceNode)getChild(i)).getThreadTags(rv);
        }
        public long getLatestMessageId() {
            long latestMessageId = -1;
            SyndieURI uri = getURI();
            if ( !isDummy() && (uri != null) && (uri.getMessageId() != null) )
                latestMessageId = uri.getMessageId().longValue();
            for (int i = 0; i < getChildCount(); i++)
                latestMessageId = Math.max(latestMessageId, ((ThreadReferenceNode)getChild(i)).getLatestMessageId());
            return latestMessageId;
        }
        public long getLatestAuthorId() { return getLatestAuthorId(getLatestMessageId()); }
        private long getLatestAuthorId(long latestMessageId) {
            SyndieURI uri = getURI();
            if ( !isDummy() && (uri != null) && (uri.getMessageId() != null) )
                if (latestMessageId == uri.getMessageId().longValue())
                    return getAuthorId();
            for (int i = 0; i < getChildCount(); i++) {
                long authorId = ((ThreadReferenceNode)getChild(i)).getLatestAuthorId(latestMessageId);
                if (authorId >= 0)
                    return authorId;
            }
            return -1;
        }
        public long getLatestPostDate() { return getLatestMessageId(); }
        /** count of actual messages, not including any dummy nodes */
        public int getMessageCount() {
            int rv = isDummy() ? 1 : 0;
            for (int i = 0; i < getChildCount(); i++)
                rv += ((ThreadReferenceNode)getChild(i)).getMessageCount();
            return rv;
        }
        public long getAuthorId() { return isDummy() ? -1 : _authorId; }
        public String getThreadSubject() {
            if ( !isDummy() && (_subject != null) && (_subject.length() > 0) )
                return _subject;
            for (int i = 0; i < getChildCount(); i++) {
                String subject = ((ThreadReferenceNode)getChild(i)).getThreadSubject();
                if ( (subject != null) && (subject.length() > 0) )
                    return subject;
            }
            return "";
        }
        
        public void setChildren(ThreadReferenceNode children[]) {
            clearChildren();
            if (children != null)
                for (int i = 0; i < children.length; i++)
                    addChild(children[i]);
        }
        public ThreadReferenceNode[] getChildren() {
            ThreadReferenceNode rv[] = new ThreadReferenceNode[getChildCount()];
            for (int i = 0; i < rv.length; i++)
                rv[i] = (ThreadReferenceNode)getChild(i);
            return rv;
        }
    }
}
