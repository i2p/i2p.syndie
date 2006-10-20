package syndie.db;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;
import net.i2p.I2PAppContext;
import net.i2p.data.Hash;
import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;

/**
 * Walk through the message database and build a tree of messages.  note that
 * there is currently a bug where a single thread built from different points
 * can result in a tree with branches in a different order.  so, to keep it
 * consistent, either don't always rebuild the tree, or always build it from
 * the root.  or, of course, improve the algorithm so that it has a single
 * canonical form.
 */
class MessageThreadBuilder {
    private DBClient _client;
    private UI _ui;
    private Map _uriToReferenceNode;
    private List _pendingURI;
    private ThreadedReferenceNode _root;
    
    public MessageThreadBuilder(DBClient client, UI ui) {
        _client = client;
        _ui = ui;
        _uriToReferenceNode = new HashMap();
        _pendingURI = new ArrayList();
    }
    
    /**
     * build the full tree that the given message is a part of, returning
     * the root.  each node has the author's preferred name stored in node.getName()
     * and the message subject in node.getDescription(), with the message URI in
     * node.getURI().
     */
    public ReferenceNode buildThread(MessageInfo msg) {
        long chanId = msg.getScopeChannelId();
        ChannelInfo chan = _client.getChannel(chanId);
        long msgId = msg.getMessageId();
        if ( (chan != null) && (msgId >= 0) )
            _pendingURI.add(SyndieURI.createMessage(chan.getChannelHash(), msgId));
        while (_pendingURI.size() > 0)
            processNextMessage();
        buildTree();
        return _root;
    }
    private void processNextMessage() {
        SyndieURI uri = (SyndieURI)_pendingURI.remove(0);
        if ( (uri.getScope() == null) || (uri.getMessageId() == null) )
            return;
        Hash chan = uri.getScope();
        String subject = null;
        String authorName = null;
        List parentURIs = null;
        List childURIs = null;
        long chanId = _client.getChannelId(uri.getScope());
        if (chanId >= 0) {
            ChannelInfo chanInfo = _client.getChannel(chanId);
            if (chanInfo != null)
                authorName = chanInfo.getName();
            else
                authorName = uri.getScope().toBase64().substring(0,6);
            
            MessageInfo msg = _client.getMessage(chanId, uri.getMessageId());
            if (msg != null) {
                subject = msg.getSubject();
                if (subject == null) subject = "";
                if (!msg.getForceNewThread()) {
                    parentURIs = getParentURIs(msg.getInternalId());
                    enqueue(parentURIs);
                }
                if (!msg.getRefuseReplies()) {
                    childURIs = getChildURIs(chan, msg.getMessageId());
                    enqueue(childURIs);
                }
            } else {
                _ui.debugMessage("message " + uri + " (chanId=" + chanId + ") was not found");
            }
        } else {
            authorName = uri.getScope().toBase64().substring(0,6);
            _ui.debugMessage("channel for message " + uri + " was not found");
        }
        
        ThreadedReferenceNode node = new ThreadedReferenceNode(authorName, uri, subject);
        node.setHistory(parentURIs, childURIs);
        _uriToReferenceNode.put(uri, node);
    }
    
    private void enqueue(List uris) {
        for (int i = 0; i < uris.size(); i++) {
            SyndieURI uri = (SyndieURI)uris.get(i);
            if (_pendingURI.contains(uri)) {
                // already pending, noop
            } else if (!_uriToReferenceNode.containsKey(uri)) {
                _pendingURI.add(uri);
            } else {
                ReferenceNode ref = (ReferenceNode)_uriToReferenceNode.get(uri);
                if (ref.getURI() == null) // only known by reference, not yet pending
                    _pendingURI.add(uri);
            }
        }
    }
    
    private static final String SQL_GET_PARENT_URIS = "SELECT referencedChannelHash, referencedMessageId FROM messageHierarchy WHERE msgId = ? ORDER BY referencedCloseness ASC, msgId DESC";
    /* CREATE CACHED TABLE messageHierarchy (
     *         msgId                   BIGINT
     *         -- refers to a targetChannelId
     *         , referencedChannelHash VARBINARY(32)
     *         , referencedMessageId   BIGINT
     *         -- how far up the tree is the referenced message?  parent has a closeness of 1,
     *         -- grandparent has a closeness of 2, etc.  does not necessarily have to be exact,
     *         -- merely relative
     *         , referencedCloseness   INTEGER DEFAULT 1
     *         , PRIMARY KEY (msgId, referencedCloseness)
     * );
     *
     */
    private List getParentURIs(long msgId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _client.con().prepareStatement(SQL_GET_PARENT_URIS);
            stmt.setLong(1, msgId);
            rs = stmt.executeQuery();
            List rv = new ArrayList();
            while (rs.next()) {
                byte chan[] = rs.getBytes(1);
                long chanMsg = rs.getLong(2);
                if (rs.wasNull())
                    chanMsg = -1;
                if ( (chan != null) && (chan.length == Hash.HASH_LENGTH) && (chanMsg >= 0) )
                    rv.add(SyndieURI.createMessage(new Hash(chan), chanMsg));
            }
            return rv;
        } catch (SQLException se) {
            _ui.errorMessage("Error retrieving parent URIs", se);
            return Collections.EMPTY_LIST;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    private static final String SQL_GET_CHILD_URIS = "SELECT msgId FROM messageHierarchy WHERE referencedChannelHash = ? AND referencedMessageId = ? ORDER BY referencedCloseness ASC, msgId DESC";
    /* CREATE CACHED TABLE messageHierarchy (
     *         msgId                   BIGINT
     *         -- refers to a targetChannelId
     *         , referencedChannelHash VARBINARY(32)
     *         , referencedMessageId   BIGINT
     *         -- how far up the tree is the referenced message?  parent has a closeness of 1,
     *         -- grandparent has a closeness of 2, etc.  does not necessarily have to be exact,
     *         -- merely relative
     *         , referencedCloseness   INTEGER DEFAULT 1
     *         , PRIMARY KEY (msgId, referencedCloseness)
     * );
     *
     */
    private List getChildURIs(Hash channel, long messageId) {
        PreparedStatement stmt = null;
        _client.getMessageIdsAuthenticated(channel);
        ResultSet rs = null;
        try {
            stmt = _client.con().prepareStatement(SQL_GET_CHILD_URIS);
            stmt.setBytes(1, channel.getData());
            stmt.setLong(2, messageId);
            rs = stmt.executeQuery();
            List rv = new ArrayList();
            while (rs.next()) {
                long internalMsgId = rs.getLong(1);
                if (!rs.wasNull()) {
                    MessageInfo msg = _client.getMessage(internalMsgId);
                    if (msg != null)
                        rv.add(msg.getURI());
                }
            }
            return rv;
        } catch (SQLException se) {
            _ui.errorMessage("Error retrieving child URIs", se);
            return Collections.EMPTY_LIST;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    private void buildTree() {
        for (Iterator iter = _uriToReferenceNode.values().iterator(); iter.hasNext(); ) {
            ThreadedReferenceNode node = (ThreadedReferenceNode)iter.next();
            buildTree(node);
        }
        pruneEmpty();
        reindexTree(); // walk through the
    }
    private void buildTree(ThreadedReferenceNode node) {
        ThreadedReferenceNode cur = node;
        List parents = node.getParentURIs();
        if (parents != null) {
            //_ui.debugMessage("building tree for " + node.getURI() + ": parents: " + parents);
            for (int i = 0; i < parents.size(); i++) {
                SyndieURI uri = (SyndieURI)parents.get(i);
                ThreadedReferenceNode parent = (ThreadedReferenceNode)_uriToReferenceNode.get(uri);
                if (parent == null) {
                    parent = new ThreadedReferenceNode(null, uri, null);
                    _uriToReferenceNode.put(uri, parent);
                }
                if (cur.getParent() == null)
                    parent.addChild(cur);
                cur = parent;
            }
        }
    }
    private void pruneEmpty() {
        for (Iterator iter = _uriToReferenceNode.keySet().iterator(); iter.hasNext(); ) {
            SyndieURI uri = (SyndieURI)iter.next();
            ThreadedReferenceNode node = (ThreadedReferenceNode)_uriToReferenceNode.get(uri);
            if (node.getName() == null) {
                _ui.debugMessage("dummy node, parent=" + node.getParent() + " kids: " + node.getChildCount());
                // dummy node
                if (node.getParent() == null) {
                    // we are at the root, so don't pull up any kids (unless there's just one)
                    if (node.getChildCount() == 1) {
                        ThreadedReferenceNode child = (ThreadedReferenceNode)node.getChild(0);
                        child.setParent(null);
                        iter.remove();
                    } else {
                        if (_root != null) {
                            _ui.debugMessage("Corrupt threading, multiple roots");
                            _ui.debugMessage("Current root: " + _root.getURI());
                            _ui.debugMessage("New root: " + node.getURI());
                        }
                        _root = node;
                    }
                } else {
                    // pull up the children
                    _ui.debugMessage("Pulling up the " + node.getChildCount() + " children");
                    ThreadedReferenceNode parent = (ThreadedReferenceNode)node.getParent();
                    for (int i = 0; i < node.getChildCount(); i++) {
                        ThreadedReferenceNode child = (ThreadedReferenceNode)node.getChild(i);
                        parent.addChild(child);
                    }
                    iter.remove();
                }
            } else {
                if (node.getParent() == null) {
                    if (_root != null) {
                        _ui.debugMessage("Corrupt threading, multiple roots");
                        _ui.debugMessage("Current root: " + _root.getURI());
                        _ui.debugMessage("New root: " + node.getURI());
                    }
                    _root = node;
                }
            }
        }
    }
    private void reindexTree() {
        List roots = new ArrayList(1);
        roots.add(_root);
        ThreadWalker walker = new ThreadWalker(_ui);
        ReferenceNode.walk(roots, walker);
    }
    
    private class ThreadWalker implements ReferenceNode.Visitor {
        private UI _ui;
        public ThreadWalker(UI ui) { _ui = ui; }
        public void visit(ReferenceNode node, int indent, int siblingOrder) {
            SyndieURI uri = node.getURI();
            if (uri == null) return;
            Hash channel = uri.getScope();
            Long msgId = uri.getMessageId();
            if ( (channel == null) || (msgId == null) ) return;
            ThreadedReferenceNode tnode = (ThreadedReferenceNode)node;
            String oldIndex = tnode.getTreeIndex();
            if (tnode.getParent() == null)
                tnode.setTreeIndex("" + (siblingOrder+1));
            else
                tnode.setTreeIndex(tnode.getParent().getTreeIndex() + "." + (siblingOrder+1));
            String newIndex = tnode.getTreeIndex();
            if (!newIndex.equals(oldIndex))
                _ui.debugMessage("Reindexing " + oldIndex + " to " + newIndex);
        }
    }
    
    private class ThreadedReferenceNode extends ReferenceNode {
        private List _parentURIs;
        private List _childURIs;
        public ThreadedReferenceNode(String name, SyndieURI uri, String description) {
            super(name, uri, description, null);
            _parentURIs = new ArrayList();
            _childURIs = new ArrayList();
        }
        public void setHistory(List parentURIs, List childURIs) {
            if (parentURIs != null) {
                _parentURIs = parentURIs;
            } else {
                if ( (_parentURIs == null) || (_parentURIs.size() > 0) )
                    _parentURIs = new ArrayList();
            }
            if (childURIs != null) {
                _childURIs = childURIs;
            } else {
                if ( (_childURIs == null) || (_childURIs.size() > 0) )
                    _childURIs = new ArrayList();
            }
        }
        public List getParentURIs() { return _parentURIs; }
        public List getChildURIs() { return _childURIs; }
        public void setParent(ThreadedReferenceNode node) { _parent = node; }
        public void setTreeIndex(String index) { _treeIndex = index; }
    }
    
    public static void main(String args[]) {
        TextUI ui = new TextUI(true);
        final TextEngine te = new TextEngine("/tmp/cleandb", ui);
        ui.insertCommand("login");
        te.runStep();
        MessageThreadBuilder mtb = new MessageThreadBuilder(te.getClient(), ui);
        MessageInfo onetwotwo = te.getClient().getMessage(4);
        ReferenceNode onetwotwoThread = mtb.buildThread(onetwotwo);
        walk(onetwotwoThread, ui);
        ui.debugMessage("built from " + onetwotwo.getScopeChannel().toBase64().substring(0,6) + ":" + onetwotwo.getMessageId());

        mtb = new MessageThreadBuilder(te.getClient(), ui);
        MessageInfo onetwo = te.getClient().getMessage(10);
        ReferenceNode onetwoThread = mtb.buildThread(onetwo);
        walk(onetwoThread, ui);
        ui.debugMessage("built from " + onetwo.getScopeChannel().toBase64().substring(0,6) + ":" + onetwo.getMessageId());
        
        ui.insertCommand("exit");
        te.runStep();
    }

    private static void walk(ReferenceNode root, UI ui) {
        List roots = new ArrayList(1);
        roots.add(root);
        ThreadView walker = new ThreadView(ui);
        ui.statusMessage("Thread: ");
        ReferenceNode.walk(roots, walker);
    }
    
    private static class ThreadView implements ReferenceNode.Visitor {
        private UI _ui;
        public ThreadView(UI ui) { _ui = ui; }
        public void visit(ReferenceNode node, int indent, int siblingOrder) {
            SyndieURI uri = node.getURI();
            if (uri == null) return;
            Hash channel = uri.getScope();
            Long msgId = uri.getMessageId();
            if ( (channel == null) || (msgId == null) ) return;
            _ui.debugMessage("Visiting " + node.getTreeIndex() + ": " + channel.toBase64().substring(0,6) + ":" + msgId);
        }
    }
}
