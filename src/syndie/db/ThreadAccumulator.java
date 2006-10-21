package syndie.db;

import java.util.*;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import net.i2p.data.Hash;
import syndie.data.*;

/**
 *
 */
public class ThreadAccumulator {
    private DBClient _client;
    private UI _ui;
    
    private List _rootURIs;
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
    
    private static final String SQL_LIST_THREADS_ALL = "SELECT msgId, scopeChannelId, authorChannelId, targetChannelId FROM channelMessage WHERE forceNewThread = TRUE OR msgId NOT IN (SELECT DISTINCT msgId FROM messageHierarchy)";
    private static final String SQL_LIST_THREADS_CHAN = "SELECT msgId, scopeChannelId, authorChannelId, targetChannelId FROM channelMessage WHERE (targetChannelId = ? OR scopeChannelId = ?) AND (forceNewThread = TRUE OR msgId NOT IN (SELECT DISTINCT msgId FROM messageHierarchy) )";
    public void gatherThreads(Set channelHashes, Set tagsRequired, Set tagsRejected) {
        init();
        
        // - iterate across all matching channels
        //  - list all threads in the channel
        //  - list all tags for each thread
        //  - filter threads per tags
        
        List rootMsgIds = new ArrayList();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            if (channelHashes == null) {
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
                for (Iterator iter = channelHashes.iterator(); iter.hasNext(); ) {
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
                loadInfo(root, tagsRequired, tagsRejected);
            }
        } catch (SQLException se) {
            _ui.errorMessage("Internal error accumulating threads", se);
        }
    }
    
    private void init() {
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
    
    private void loadInfo(ReferenceNode threadRoot, Set tagsRequired, Set tagsRejected) {
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
        if (tagsRejected != null) {
            for (Iterator iter = tagsRejected.iterator(); iter.hasNext(); ) {
                String tag = (String)iter.next();
                if (tags.contains(tag)) {
                    _ui.debugMessage("Rejecting thread tagged with " + tag + ": " + threadRoot.getURI().toString());
                    return;
                }
            }
        }
        if ( (tagsRequired != null) && (tagsRequired.size() > 0) ) {
            for (Iterator iter = tagsRequired.iterator(); iter.hasNext(); ) {
                String tag = (String)iter.next();
                if (!tags.contains(tag)) {
                    _ui.debugMessage("Rejecting thread not tagged with " + tag + ": " + threadRoot.getURI().toString());
                    return;
                }
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
    }
}
