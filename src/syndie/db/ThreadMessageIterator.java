package syndie.db;

import java.util.ArrayList;
import java.util.List;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.MessageIterator;

/**
 * Iterate in a single thread
 */
public class ThreadMessageIterator implements MessageIterator {
    private ThreadReferenceNode _root;
    
    private SyndieURI _treeURI;
    private SyndieURI _nextNew;
    private SyndieURI _prevNew;
    private SyndieURI _nextInThread;
    private SyndieURI _prevInThread;
    
    public ThreadMessageIterator(ThreadReferenceNode root, SyndieURI treeURI) {
        _root = root;
        _treeURI = treeURI;
    }

    public void recenter(final SyndieURI uri) {
        final List traversal = new ArrayList();
        final ReferenceNode cur[] = new ReferenceNode[1];
        
        List roots = new ArrayList(1);
        roots.add(_root);
        ReferenceNode.walk(roots, new ReferenceNode.Visitor() {
            public void visit(ReferenceNode node, int depth, int siblingOrder) {
                SyndieURI curi = node.getURI();
                if (curi != null) {
                    traversal.add(node);
                    if ( (cur[0] == null) && (curi.equals(uri)) )
                        cur[0] = node;
                }
            }
        });   
        
        recenter(traversal, cur[0]);
    }
    public void recenter(long msgId) {
        List traversal = traverse();
        ReferenceNode cur = _root.getByUniqueId(msgId);
        recenter(traversal, cur);
    }
    private void recenter(List traversal, ReferenceNode cur) {
        _nextNew = null;
        _prevNew = null;
        _nextInThread = null;
        _prevInThread = null;
        
        
        if (cur != null) {
            int idx = traversal.indexOf(cur);
            for (int i = idx-1; i >= 0; i--) {
                ThreadReferenceNode prev = (ThreadReferenceNode)traversal.get(i);
                if (_prevInThread == null)
                    _prevInThread = prev.getURI();
                if (_prevNew == null) {
                    if (prev.getMessageStatus() == DBClient.MSG_STATUS_UNREAD) {
                        _prevNew = prev.getURI();
                        break;
                    }
                }
            }
            
            for (int i = idx+1; i < traversal.size(); i++) {
                ThreadReferenceNode nxt = (ThreadReferenceNode)traversal.get(i);
                if (_nextInThread == null)
                    _nextInThread = nxt.getURI();
                if (_nextNew == null) {
                    if (nxt.getMessageStatus() == DBClient.MSG_STATUS_UNREAD) {
                        _nextNew = nxt.getURI();
                        break;
                    }
                }
            }
        }
    }
    
    public ThreadReferenceNode getThreadRoot() { return (ThreadReferenceNode)_root; }
    
    private List traverse() {
        List roots = new ArrayList(1);
        roots.add(_root);
        final List rv = new ArrayList();
        ReferenceNode.walk(roots, new ReferenceNode.Visitor() {
            public void visit(ReferenceNode node, int depth, int siblingOrder) {
                SyndieURI uri = node.getURI();
                if (uri != null)
                    rv.add(node);
            }
        });
        return rv;
    }
    

    public SyndieURI getNextNew() { return _nextNew; }
    public SyndieURI getPreviousNew() { return _prevNew; }
    public SyndieURI getNextInThread() { return _nextInThread; }
    public SyndieURI getPreviousInThread() { return _prevInThread; }
    public SyndieURI getMessageTreeURI() { return _treeURI; }
    // single thread only
    public SyndieURI getNextThread() { return null; }
    public SyndieURI getPreviousThread() { return null; }
    public SyndieURI getNextViaThread() { return _nextInThread; }
    public SyndieURI getPreviousViaThread() { return _prevInThread; }
}
