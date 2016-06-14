package syndie.thread;

import java.util.*;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import net.i2p.data.Base64;
import net.i2p.data.Hash;

import syndie.data.*;
import syndie.db.DBClient;
import syndie.db.UI;
import syndie.util.DateTime;

/**
 *
 */
public class ThreadAccumulator {
    private DBClient _client;
    private UI _ui;
    
    private List _rootURIs;
    /** fully populated threads, in ReferenceNode form */
    private List _roots;
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
    private boolean _privateMessage;
    private boolean _unreadOnly;
    private Set _channelHashes;
    private Set _requiredTags;
    private Set _wantedTags;
    private Set _rejectedTags;
    
    public ThreadAccumulator(DBClient client, UI ui) {
        _client = client;
        _ui = ui;
    }

    public static final int SORT_SUBJECT = 1;
    public static final int SORT_FORUM = 2;
    public static final int SORT_DATE = 3;
    public static final int SORT_AUTHOR = 4;
    public static final int SORT_DEFAULT = SORT_DATE;
    
    public void setSort(int sortField, boolean ascending) {}
    
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
                    chans.add(Hash.create(b));
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
        _pbe = criteria.getBoolean("pbe", false);
        _privateMessage = criteria.getBoolean("private", false);
        _showThreaded = criteria.getBoolean("threaded", true);
        _unreadOnly = criteria.getBoolean("unreadonly", false);
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
    
    private static final String SQL_LIST_THREADS_ALL = "SELECT msgId, scopeChannelId, authorChannelId, targetChannelId FROM channelMessage WHERE forceNewThread = TRUE OR msgId NOT IN (SELECT DISTINCT msgId FROM messageHierarchy)";
    // this does not deal with messages that have parents who are not locally known
    //private static final String SQL_LIST_THREADS_CHAN = "SELECT msgId, scopeChannelId, authorChannelId, targetChannelId FROM channelMessage WHERE (targetChannelId = ? OR scopeChannelId = ?) AND (forceNewThread = TRUE OR msgId NOT IN (SELECT DISTINCT msgId FROM messageHierarchy) )";
    // fix w/ subquery looking for known parents (surely this can be simplified)
    /*
SELECT msgId, scopeChannelId, authorChannelId, targetChannelId 
FROM channelMessage WHERE 
(targetChannelId = ? OR scopeChannelId = ?)
AND 
(forceNewThread = TRUE OR 
 msgId NOT IN (
	SELECT DISTINCT mh.msgId FROM messageHierarchy mh 
		JOIN channelMessage cm ON mh.referencedMessageId = cm.messageId 
		JOIN channel c ON c.channelHash = mh.referencedChannelHash AND c.channelId = cm.scopeChannelId
	) 
)
     */
    private static final String SQL_LIST_THREADS_CHAN = "SELECT msgId, scopeChannelId, authorChannelId, targetChannelId FROM channelMessage WHERE (targetChannelId = ? OR scopeChannelId = ?) AND (forceNewThread = TRUE OR  msgId NOT IN ( SELECT DISTINCT mh.msgId FROM messageHierarchy mh JOIN channelMessage cm ON mh.referencedMessageId = cm.messageId JOIN channel c ON c.channelHash = mh.referencedChannelHash AND c.channelId = cm.scopeChannelId ) )";
    /* OUTDATED JAVADOC
     * @param channelHashes set of Hash for each channel to pull threads out of (null means all channels!)
     * @param tagsRequired threads must have all of the tags in this set
     * @param tagsWanted threads must have at least one of the tags in this set
     * @param tagsRejected threads must not have any of the tags in this set
     */
    public void gatherThreads() {
        init();
        _ui.debugMessage("beginning gather threads w/ state: \n" + toString());
        
        _client.beginTrace();
        
        // - iterate across all matching channels
        //  - list all threads in the channel
        //  - list all tags for each thread
        //  - filter threads per tags
        
        List rootMsgIds = new ArrayList();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            if (_channelHashes == null) {
                _ui.debugMessage("gather across all channels");
                stmt = _client.con().prepareStatement(SQL_LIST_THREADS_ALL);
                rs = stmt.executeQuery();
                while (rs.next()) {
                    // msgId, scopeChannelId, authorChannelId, targetChannelId
                    long msgId = rs.getLong(1);
                    if (rs.wasNull()) msgId = -1;
                    long scopeId = rs.getLong(2);
                    if (rs.wasNull()) scopeId = -1;
                    long authorId = rs.getLong(3);
                    if (rs.wasNull()) authorId = -1;
                    long targetId = rs.getLong(4);
                    if (rs.wasNull()) targetId = -1;
                    
                    //if (authorId >= 0)
                    //    _threadRootAuthorId.add(Long.valueOf(authorId));
                    //else
                    //    _threadRootAuthorId.add(Long.valueOf(scopeId));
                    rootMsgIds.add(Long.valueOf(msgId));
                }
                _ui.debugMessage("Found root messageIds for all channels: " + rootMsgIds);
                rs.close();
                rs = null;
                stmt.close();
                stmt = null;
            } else {
                for (Iterator iter = _channelHashes.iterator(); iter.hasNext(); ) {
                    Hash chan = (Hash)iter.next();
                    _ui.debugMessage("gather threads across " + chan.toBase64());
                    long chanId = _client.getChannelId(chan);
                    stmt = _client.con().prepareStatement(SQL_LIST_THREADS_CHAN);
                    stmt.setLong(1, chanId);
                    stmt.setLong(2, chanId);
                    rs = stmt.executeQuery();
                    while (rs.next()) {
                        // msgId, scopeChannelId, authorChannelId, targetChannelId
                        long msgId = rs.getLong(1);
                        if (rs.wasNull()) msgId = -1;
                        long scopeId = rs.getLong(2);
                        if (rs.wasNull()) scopeId = -1;
                        long authorId = rs.getLong(3);
                        if (rs.wasNull()) authorId = -1;
                        long targetId = rs.getLong(4);
                        if (rs.wasNull()) targetId = -1;

                        //if (authorId >= 0)
                        //    _threadRootAuthorId.add(Long.valueOf(authorId));
                        //else
                        //    _threadRootAuthorId.add(Long.valueOf(scopeId));
                        rootMsgIds.add(Long.valueOf(msgId));        
                        //_ui.debugMessage("accumulate root msgId: " + msgId);
                    }
                    rs.close();
                    rs = null;
                    stmt.close();
                    stmt = null;
                    
                    _ui.debugMessage("Found root messageIds including those for channel " + chan.toBase64() + ": " + rootMsgIds);
                } // end iterating over channels
            } // if (all channels) {} else {}
            
            // now find the relevent details for each thread
            for (int i = 0; i < rootMsgIds.size(); i++) {
                Long msgId = (Long)rootMsgIds.get(i);
                MessageThreadBuilder builder = new MessageThreadBuilder(_client, _ui);
                MessageInfo info = _client.getMessage(msgId.longValue());
                if (info == null) // startup maybe?
                    continue;
                //_ui.debugMessage("building thread for root msgId: " + msgId);
                ReferenceNode root = builder.buildThread(info);
                _ui.debugMessage("thread built for root msgId: " + msgId + " - " + (root != null ? root.getURI() : null));
                // loads up the details (tags, etc), and if the thread matches the
                // criteria, the details are added to _rootURIs, _threadMessages, etc
                if (root != null)
                    loadInfo(root);
                //_ui.debugMessage("thread loaded for root msgId: " + msgId);// + ": " + root);
            }
        } catch (SQLException se) {
            _ui.errorMessage("Internal error accumulating threads", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        _ui.debugMessage("gather threads trace: " + _client.completeTrace());
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

    private class Harvester implements ReferenceNode.Visitor {
        private int _messages;
        private ReferenceNode _latest;
        private List _tags;
        public Harvester() {
            _tags = new ArrayList();
            _messages = 0;
        }
        public int getMessageCount() { return _messages; }
        public ReferenceNode getLatestPost() { return _latest; }
        public List getTags() { return _tags; }
        public void visit(ReferenceNode node, int depth, int siblingOrder) {
            _messages++;
            if ( (_latest == null) || (_latest.getURI().getMessageId().longValue() < node.getURI().getMessageId().longValue()) )
                _latest = node;
            long chanId = _client.getChannelId(node.getURI().getScope());
            Set tags = _client.getMessageTags(chanId, node.getURI().getMessageId().longValue(), true, true);
            if (tags != null)
                _tags.addAll(tags);
        }        
    }
    
    /**
     *  @param threadRoot non-null
     */
    private void loadInfo(ReferenceNode threadRoot) {
        // walk the thread to find the latest post / message count / tags
        Harvester visitor = new Harvester();
        List roots = new ArrayList();
        roots.add(threadRoot);
        ReferenceNode.walk(roots, visitor);
        
        int messageCount = visitor.getMessageCount();
        ReferenceNode latestPost = visitor.getLatestPost();
        long latestPostDate = latestPost.getURI().getMessageId().longValue();
        long latestAuthorId = _client.getChannelId(latestPost.getURI().getScope());
        List tags = visitor.getTags();
    
        long rootAuthorId = -1;
        List newRoots = filterRoots(tags, threadRoot.getURI(), threadRoot, visitor, true);
        for (int i = 0; i < newRoots.size(); i++) {
            ReferenceNode newRoot = (ReferenceNode)newRoots.get(i);
            long newRootAuthorId = _client.getChannelId(newRoot.getURI().getScope());
            _ui.debugMessage("filter passed for root " + newRoot.getURI().toString());
            _rootURIs.add(newRoot.getURI());
            _threadSubject.add(newRoot.getDescription());
            _threadLatestAuthorId.add(Long.valueOf(latestAuthorId));
            _threadLatestPostDate.add(Long.valueOf(latestPostDate));
            _threadMessages.add(Integer.valueOf(messageCount));
            _threadRootAuthorId.add(Long.valueOf(newRootAuthorId));
            _threadTags.add(tags);
            _roots.add(newRoot);
            if (rootAuthorId == -1)
                rootAuthorId = newRootAuthorId;
        }
        /*
        // now filter
        if (filterPassed(tags, threadRoot.getURI(), threadRoot, visitor, true)) {
            _ui.debugMessage("filter passed for root " + threadRoot.getURI().toString());
            _rootURIs.add(threadRoot.getURI());
            _threadSubject.add(threadRoot.getDescription());
            _threadLatestAuthorId.add(Long.valueOf(latestAuthorId));
            _threadLatestPostDate.add(Long.valueOf(latestPostDate));
            _threadMessages.add(Integer.valueOf(messageCount));
            _threadRootAuthorId.add(Long.valueOf(rootAuthorId));
            _threadTags.add(tags);
            _roots.add(threadRoot);
        } else {
            // the root didn't pass, but maybe its children will
            _ui.debugMessage("filter did not pass for root " + threadRoot.getURI().toString());
            if (threadRoot.getChildCount() > 0) {
                _ui.debugMessage("thread children: " + threadRoot.toString());
            }
        }
         */
        
        removeFilteredChildren(threadRoot, visitor);
        
        // passed the filter.  add to the accumulator
        if (!_showThreaded) {
            // add the depth-first traversal of the thread, and update the threadRoot
            // to remove its children
            
            //add(threadRoot, rootAuthorId, visitor);
            while (threadRoot.getChildCount() > 0) {
                ReferenceNode child = threadRoot.getChild(0);
                threadRoot.removeChild(child);
                add(child, rootAuthorId, visitor);
            }
        }
    }
    
    private List filterRoots(List tags, SyndieURI rootURI, ReferenceNode root, Harvester visitor, boolean isRoot) {
        List rv = new ArrayList();
        filterRoots(tags, rootURI, root, visitor, isRoot, rv);
        return rv;
    }
    private void filterRoots(List tags, SyndieURI rootURI, ReferenceNode root, Harvester visitor, boolean isRoot, List rv) {
        if (filterPassed(tags, rootURI, root, visitor, isRoot)) {
            _ui.debugMessage("filter passed for root " + rootURI.toString());
            rv.add(root);
        } else {
            // the root didn't pass, but maybe its children will, so recurse
            _ui.debugMessage("filter did not pass for root " + rootURI.toString());
            if (root.getChildCount() > 0) {
                _ui.debugMessage("thread children: " + root.toString());
                for (int i = 0; i < root.getChildCount(); i++) {
                    ReferenceNode child = root.getChild(i);
                    filterRoots(tags, child.getURI(), child, visitor, false, rv);
                }
            }
        }
    }
    
    private boolean allRead(ReferenceNode node) {
        SyndieURI uri = node.getURI();
        if ( (uri == null) || (uri.getScope() == null) || (uri.getMessageId() == null) )
            return true;
        long chanId = _client.getChannelId(uri.getScope());
        long msgId = _client.getMessageId(chanId, uri.getMessageId().longValue());
        int status = _client.getMessageStatus(_client.getLoggedInNymId(), msgId, chanId);
        if (status == DBClient.MSG_STATUS_UNREAD)
            return false;
        for (int i = 0; i < node.getChildCount(); i++) {
            if (!allRead(node.getChild(i)))
                return false;
        }
        return true;
    }
    
    /**
     * currently, this requires the entire thread to pass the filter, trimming 
     * it off at each leaf that does not pass.  todo: this should instead remove
     * filtered elements and reparent the thread for orphaned messages that do
     * pass the filter
     */
    private void removeFilteredChildren(ReferenceNode cur, Harvester harvester) {
        _ui.debugMessage("removeFilteredChildren from " + cur.getURI());
        for (int i = 0; i < cur.getChildCount(); i++) {
            ReferenceNode child = cur.getChild(i);
            
            long chanId = _client.getChannelId(child.getURI().getScope());
            Set tags = null;
            if (child.getURI().getMessageId() != null)
                tags = _client.getMessageTags(chanId, child.getURI().getMessageId().longValue(), true, true);
            
            _ui.debugMessage("removeFilteredChildren: child: " + child.getURI() + " msg known? " + (tags != null));
            if (tags != null) {
                if (!filterPassed(tags, child.getURI(), child, harvester, false)) {
                    cur.removeChild(child);
                    i--;
                } else {
                    removeFilteredChildren(child, harvester);
                }
            } else {
                cur.removeChild(child);
                i--;
                // todo: keep a stub in here rather than trim the whole tree
            }
            /*
            MessageInfo msg = _client.getMessage(chanId, child.getURI().getMessageId());
            if (msg != null) {
                _ui.debugMessage("removeFilteredChildren: child: " + child.getURI() + " msg known? " + (msg != null));
                List tags = new ArrayList();
                tags.addAll(msg.getPublicTags());
                tags.addAll(msg.getPrivateTags());
                if (!filterPassed(tags, child.getURI(), child, harvester, false)) {
                    cur.removeChild(child);
                    i--;
                } else {
                    removeFilteredChildren(child, harvester);
                }
            } else {
                cur.removeChild(child);
                i--;
                // todo: keep a stub in here rather than trim the whole tree
            }
             */
        }
        _ui.debugMessage("removeFilteredChildren complete");
    }
    
    private void add(ReferenceNode cur, long rootAuthorId, Harvester harvester) {
        if (cur.getURI().getMessageId() == null) return;
        long chanId = _client.getChannelId(cur.getURI().getScope());
        //MessageInfo msg = _client.getMessage(chanId, cur.getURI().getMessageId());
        Set msgTags = _client.getMessageTags(chanId, cur.getURI().getMessageId().longValue(), true, true);
        List tags = new ArrayList(msgTags);
        long authorChanId = _client.getMessageAuthor(chanId, cur.getURI().getMessageId().longValue());
        
        // all filtered messages were removed above in removeFilteredChildren
        _rootURIs.add(cur.getURI());
        _threadSubject.add(cur.getDescription());
        _threadLatestAuthorId.add(Long.valueOf(authorChanId));
        _threadLatestPostDate.add(cur.getURI().getMessageId());
        _threadMessages.add(Integer.valueOf(1));
        _threadRootAuthorId.add(Long.valueOf(rootAuthorId));
        _threadTags.add(tags);
        _roots.add(cur);
        while (cur.getChildCount() > 0) {
            ReferenceNode child = cur.getChild(0);
            cur.removeChild(child);
            add(child, rootAuthorId, harvester);
        }
    }
    
    private boolean filterPassed(Collection tags, SyndieURI uri, ReferenceNode node, Harvester harvester, boolean isRoot) {
        _ui.debugMessage("attempting filter pass for " + uri);
        boolean ok = true;
                
        if (ok) {
            long when = -1;
            if (isRoot)
                when = harvester.getLatestPost().getURI().getMessageId().longValue();
            else
                when = uri.getMessageId().longValue();
            if ( ( (_earliestReceiveDate >= 0) && (when < _earliestReceiveDate) ) ||
                 ( (_earliestPostDate >= 0) && (when < _earliestPostDate) ) ) {
                ok = false;
                _ui.debugMessage("filter fail cause: too early");
            }
        }
        
        if (isRoot || _applyTagFilterToMessages) {
            if (!tagFilterPassed(tags, uri)) {
                _ui.debugMessage("filter fail cause: tags");
                ok = false;
            }
        }
        
        long chanId = -1;
        if (ok) {
            chanId = _client.getChannelId(uri.getScope());
            if (chanId == -1) {
                ok = false;
                _ui.debugMessage("filter fail cause: scope not known");
            }
        }
        ChannelInfo chan = null;
        MessageInfo msg = null;
        if (ok) {
            chan = _client.getChannel(chanId);
            msg = _client.getMessage(chanId, uri.getMessageId());
            if ( (chan == null) || (msg == null) ) {
                ok = false;
                _ui.debugMessage("filter fail cause: " + (chan == null ? "chan" : "") + "/" + (msg == null ? "msg" : "") + " not known");
            }
        }
        if (ok) {
            if (!authorFilterPassed(msg, chan, node)) {
                ok = false;
                _ui.debugMessage("filter fail cause: author not passed");
            }
        }
        if (ok) {
            if (_unreadOnly) {
                int status = _client.getMessageStatus(_client.getLoggedInNymId(), msg.getInternalId(), chan.getChannelId());
                if (status != DBClient.MSG_STATUS_UNREAD) {
                    if (!_showThreaded) {
                        ok = false;
                        _ui.debugMessage("filter fail cause: message is already read (" + status + ")");
                    } else if (isRoot) {
                        boolean unreadFound = false;
                        for (int i = 0; i < node.getChildCount(); i++) {
                            if (!allRead(node.getChild(i))) {
                                unreadFound = true;
                                break;
                            }
                        }
                        if (!unreadFound) {
                            _ui.debugMessage("entire thread is read: " + node.getURI());
                            ok = false;
                        }
                    }
                }
            }
        }

        if ( ok && ( (_pbe && !msg.getWasPassphraseProtected()) || (!_pbe && msg.getWasPassphraseProtected()) ) ) {
            ok = false;
            _ui.debugMessage("filter fail cause: pbe");
        }
        if ( ok && ( (_privateMessage && !msg.getWasPrivate()) || (!_privateMessage && msg.getWasPrivate()) ) ) {
            ok = false;
            _ui.debugMessage("filter fail cause: privMsg");
        }
        if ( ok && ( (_alreadyDecrypted && (msg.getReadKeyUnknown() || msg.getPassphrasePrompt() != null) ) ||
                     (!_alreadyDecrypted && !msg.getReadKeyUnknown() && (msg.getPassphrasePrompt() == null) ) ) ) {
            ok = false;
            _ui.debugMessage("filter fail cause: decryption status");
        }
        if ( ok && ((_minPages >= 0) && (msg.getPageCount() < _minPages)) ) {
            ok = false;
            _ui.debugMessage("filter fail cause: minPages");
        }
        if ( ok && ((_maxPages >= 0) && (msg.getPageCount() > _maxPages)) ) {
            ok = false;
            _ui.debugMessage("filter fail cause: maxPages");
        }
        if ( ok && ((_minAttachments >= 0) && (msg.getAttachmentCount() < _minAttachments)) ) {
            ok = false;
            _ui.debugMessage("filter fail cause: minAttachments");
        }
        if ( ok && ((_maxAttachments >= 0) && (msg.getAttachmentCount() > _maxAttachments)) ) {
            ok = false;
            _ui.debugMessage("filter fail cause: maxAttachments");
        }
        if ( ok && ((_minReferences >= 0) && ( (msg.getReferences() == null) || (msg.getReferences().size() < _minReferences) ) )) {
            ok = false;
            _ui.debugMessage("filter fail cause: minRefs");
        }
        if ( ok && ((_maxReferences >= 0) && ( (msg.getReferences() != null) && (msg.getReferences().size() > _maxReferences) ) )) {
            ok = false;
            _ui.debugMessage("filter fail cause: maxRefs");
        }
        // todo: honor minKeys and maxKeys
        if (ok)
            _ui.debugMessage("filter pass for " + uri);
        return ok;
    }
    private boolean authorFilterPassed(MessageInfo msg, ChannelInfo chan, ReferenceNode node) {
        if (_includeUnauthorizedPosts || chan.getAllowPublicPosts()) return true;
        Hash author = msg.getURI().getScope();
        if (author == null)
            _ui.debugMessage("author is null for msg? " + msg.getURI());
        long authorId = msg.getAuthorChannelId();
        if (authorId != msg.getScopeChannelId())
            author = _client.getChannelHash(authorId);
        if (allowedToPost(author, chan))
            return true;
        boolean allowReply = chan.getAllowPublicReplies();
        _ui.debugMessage("author not explicitly allowed to post: " + author + "/" + chan.getChannelHash() + " allowPublicReplies? " + allowReply);
        if (allowReply) {
            // not explicitly authorized, so check its parents
            ReferenceNode cur = node.getParent();
            while (cur != null) {
                long curChanId = _client.getChannelId(cur.getURI().getScope());
                MessageInfo curMsg = _client.getMessage(curChanId, cur.getURI().getMessageId());
                if (curMsg != null) {
                    Hash curAuthor = cur.getURI().getScope();
                    if (curMsg.getAuthorChannelId() != curMsg.getScopeChannelId())
                        curAuthor = _client.getChannelHash(curMsg.getAuthorChannelId());
                    if (allowedToPost(curAuthor, chan))
                        return true;
                }
                cur = cur.getParent();
            }
            return false;
        } else {
            return false;
        }
    }
    private boolean allowedToPost(Hash author, ChannelInfo chan) {
        if ( (author == null) || (chan == null) )
            return false;
        //_ui.debugMessage("allowedToPost: author=" + author.toBase64().substring(0,6) + " chan=" + chan.getChannelHash().toBase64().substring(0,6));
        //_ui.debugMessage("allowedToPost: mgrs=" + chan.getAuthorizedManagerHashes());
        //_ui.debugMessage("allowedToPost: posters=" + chan.getAuthorizedPosterHashes());
        if (chan.getChannelHash().equals(author)) {
            //_ui.debugMessage("allowed to post: author");
            if (_includeOwners || _includeManagers || _includeAuthorizedPosters || _includeAuthorizedReplies)
                return true;
        }
        if (chan.getAuthorizedManagerHashes().contains(author)) {
            //_ui.debugMessage("allowed to post: manager");
            if (_includeManagers || _includeAuthorizedPosters || _includeAuthorizedReplies)
                return true;
        }
        if (chan.getAuthorizedPosterHashes().contains(author)) {
            //_ui.debugMessage("allowed to post: authPoster");
            if (_includeAuthorizedPosters || _includeAuthorizedReplies)
                return true;
        }
        //_ui.debugMessage("allowed to post: !auth");
        return false;
    }
    /** return true if the tags for the message meet our search criteria */
    private boolean tagFilterPassed(Collection tags, SyndieURI msg) {
        if (_rejectedTags != null) {
            for (Iterator iter = _rejectedTags.iterator(); iter.hasNext(); ) {
                String tag = (String)iter.next();
                if (tags.contains(tag)) {
                    _ui.debugMessage("Rejecting thread tagged with " + tag + ": " + msg.toString());
                    return false;
                } else {
                    if (tag.endsWith("*") && (tag.length() > 0)) {
                        // substring match
                        String prefix = tag.substring(0, tag.length()-1);
                        boolean substringMatch = false;
                        for (Iterator msgTagIter = tags.iterator(); msgTagIter.hasNext(); ) {
                            String cur = (String)msgTagIter.next();
                            if (cur.startsWith(prefix)) {
                                _ui.debugMessage("Rejecting thread prefix tagged with " + tag + ": " + msg.toString());
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
                            _ui.debugMessage("Substring tagged with " + tag + ": " + msg.toString());
                        } else {
                            _ui.debugMessage("Rejecting thread not substring tagged with " + tag + ": " + msg.toString());
                            return false;
                        }
                    } else {
                        _ui.debugMessage("Rejecting thread not tagged with " + tag + ": " + msg.toString());
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
                _ui.debugMessage("Rejecting thread not tagged with any of the wanted tags (" + _wantedTags + ") : " + msg.toString());
                return false;
            }
        }
        return true;
    }
    
    public String toString() {
        StringBuilder buf = new StringBuilder();
        buf.append(" threaded? ").append(_showThreaded);
        buf.append(" owners? ").append(_includeOwners);
        buf.append(" managers? ").append(_includeManagers);
        buf.append(" authPosters? ").append(_includeAuthorizedPosters);
        buf.append(" authReplies? ").append(_includeAuthorizedReplies);
        buf.append(" unauthPosts? ").append(_includeUnauthorizedPosts);
        if (_earliestReceiveDate > 0)
            buf.append(" _earliestReceiveDate? ").append(DateTime.getDate(_earliestReceiveDate));
        if (_earliestPostDate > 0)
            buf.append(" _earliestPostDate? ").append(DateTime.getDate(_earliestPostDate));
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
