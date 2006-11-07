package syndie.db;

import java.util.*;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import net.i2p.data.Base64;
import net.i2p.data.Hash;
import syndie.data.*;

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
    
    public ThreadAccumulator(DBClient client, UI ui) {
        _client = client;
        _ui = ui;
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
        
        _requiredTags = getTags(criteria.getStringArray("tagrequired"));
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

    private boolean _includeOwners;
    private boolean _includeManagers;
    private boolean _includeAuthorizedPosters;
    private boolean _includeAuthorizedReplies;
    private boolean _includeUnauthorizedPosts;
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
    private long _earliestReceiveDate;
    /** the post was received locally on or after the given date */
    public void setReceivedSince(long date) { _earliestReceiveDate = date; }
    private long _earliestPostDate;
    /** the post was created on or after the given date */
    public void setPostSince(long date) { _earliestPostDate = date; }
    private boolean _applyTagFilterToMessages;
    /** apply the tag filters to individual messages, not threads as a whole */
    public void applyTagFilterToMessages(boolean apply) { _applyTagFilterToMessages = apply; }
    private int _minPages;
    private int _maxPages;
    private int _minAttachments;
    private int _maxAttachments;
    private int _minReferences;
    private int _maxReferences;
    private int _minKeys;
    private int _maxKeys;
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
    private boolean _alreadyDecrypted;
    private boolean _pbe;
    private boolean _privateMessage;
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

    private Set _channelHashes;
    private Set _requiredTags;
    private Set _wantedTags;
    private Set _rejectedTags;
    public void setScope(Set channelHashes) { _channelHashes = channelHashes; }
    public void setTags(Set required, Set wanted, Set rejected) {
        _requiredTags = required;
        _wantedTags = wanted;
        _rejectedTags = rejected;
    }
    
    private static final String SQL_LIST_THREADS_ALL = "SELECT msgId, scopeChannelId, authorChannelId, targetChannelId FROM channelMessage WHERE forceNewThread = TRUE OR msgId NOT IN (SELECT DISTINCT msgId FROM messageHierarchy)";
    private static final String SQL_LIST_THREADS_CHAN = "SELECT msgId, scopeChannelId, authorChannelId, targetChannelId FROM channelMessage WHERE (targetChannelId = ? OR scopeChannelId = ?) AND (forceNewThread = TRUE OR msgId NOT IN (SELECT DISTINCT msgId FROM messageHierarchy) )";
    /**
     * @param channelHashes set of Hash for each channel to pull threads out of (null means all channels!)
     * @param tagsRequired threads must have all of the tags in this set
     * @param tagsWanted threads must have at least one of the tags in this set
     * @param tagsRejected threads must not have any of the tags in this set
     */
    public void gatherThreads() {
        init();
        
        // - iterate across all matching channels
        //  - list all threads in the channel
        //  - list all tags for each thread
        //  - filter threads per tags
        
        List rootMsgIds = new ArrayList();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            if (_channelHashes == null) {
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
                    //    _threadRootAuthorId.add(new Long(authorId));
                    //else
                    //    _threadRootAuthorId.add(new Long(scopeId));
                    rootMsgIds.add(new Long(msgId));
                }
                _ui.debugMessage("Found root messageIds for all channels: " + rootMsgIds);
                rs.close();
                rs = null;
                stmt.close();
                stmt = null;
            } else {
                for (Iterator iter = _channelHashes.iterator(); iter.hasNext(); ) {
                    Hash chan = (Hash)iter.next();
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
                        //    _threadRootAuthorId.add(new Long(authorId));
                        //else
                        //    _threadRootAuthorId.add(new Long(scopeId));
                        rootMsgIds.add(new Long(msgId));        
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
                ReferenceNode root = builder.buildThread(_client.getMessage(msgId.longValue()));
                // loads up the details (tags, etc), and if the thread matches the
                // criteria, the details are added to _rootURIs, _threadMessages, etc
                loadInfo(root);
            }
        } catch (SQLException se) {
            _ui.errorMessage("Internal error accumulating threads", se);
        }
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
            MessageInfo msg = _client.getMessage(chanId, node.getURI().getMessageId());
            if (msg == null) {
                _ui.debugMessage("Visiting node " + node.getURI() + " is in channelId " + chanId + " but the message isn't in the database?");
                return;
            }
            _tags.addAll(msg.getPublicTags());
            _tags.addAll(msg.getPrivateTags());
        }        
    }
    
    private void loadInfo(ReferenceNode threadRoot) {
        // walk the thread to find the latest post / message count / tags
        Harvester visitor = new Harvester();
        List roots = new ArrayList();
        roots.add(threadRoot);
        ReferenceNode.walk(roots, visitor);
        
        long rootAuthorId = _client.getChannelId(threadRoot.getURI().getScope());
        int messageCount = visitor.getMessageCount();
        ReferenceNode latestPost = visitor.getLatestPost();
        long latestPostDate = latestPost.getURI().getMessageId().longValue();
        long latestAuthorId = _client.getChannelId(latestPost.getURI().getScope());
        List tags = visitor.getTags();
        
        // now filter
        if (_rejectedTags != null) {
            for (Iterator iter = _rejectedTags.iterator(); iter.hasNext(); ) {
                String tag = (String)iter.next();
                if (tags.contains(tag)) {
                    _ui.debugMessage("Rejecting thread tagged with " + tag + ": " + threadRoot.getURI().toString());
                    return;
                }
            }
        }
        if ( (_requiredTags != null) && (_requiredTags.size() > 0) ) {
            for (Iterator iter = _requiredTags.iterator(); iter.hasNext(); ) {
                String tag = (String)iter.next();
                if (!tags.contains(tag)) {
                    _ui.debugMessage("Rejecting thread not tagged with " + tag + ": " + threadRoot.getURI().toString());
                    return;
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
                }
            }
            if (!found) {
                _ui.debugMessage("Rejecting thread not tagged with any of the wanted tags (" + _wantedTags + ") : " + threadRoot.getURI().toString());
                return;
            }
        }
        
        // passed the filter.  add to the accumulator
        _rootURIs.add(threadRoot.getURI());
        _threadSubject.add(threadRoot.getDescription());
        _threadLatestAuthorId.add(new Long(latestAuthorId));
        _threadLatestPostDate.add(new Long(latestPostDate));
        _threadMessages.add(new Integer(messageCount));
        _threadRootAuthorId.add(new Long(rootAuthorId));
        _threadTags.add(tags);
        _roots.add(threadRoot);
    }
}
