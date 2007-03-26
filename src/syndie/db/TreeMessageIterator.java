package syndie.db;

import java.util.ArrayList;
import java.util.List;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.MessageIterator;

/**
 * Iterate across a full tree of messages (and threads)
 *
 */
public class TreeMessageIterator implements MessageIterator {
    private UI _ui;
    private List _roots;
    private SyndieURI _treeURI;
    private SyndieURI _currentURI;
    private int _currentIndex;
    private ReferenceNode _currentThreadRoot;
    private SyndieURI _nextNew;
    private SyndieURI _prevNew;
    private SyndieURI _nextInThread;
    private SyndieURI _prevInThread;
    private SyndieURI _nextThread;
    private SyndieURI _prevThread;
    
    public TreeMessageIterator(UI ui, List threadReferenceNodeRoots, SyndieURI treeURI) {
        _ui = ui;
        _roots = threadReferenceNodeRoots;
        _treeURI = treeURI;
    }

    public void recenter(SyndieURI uri) {
        _currentURI = uri;
        _currentIndex = -1;
        _currentThreadRoot = null;
        _nextNew = null;
        _prevNew = null;
        _nextInThread = null;
        _prevInThread = null;
        _nextThread = null;
        _prevThread = null;
    
        List traversal = traverse(uri);
        boolean noPrevInThread = false;
        for (int i = _currentIndex-1; i >= 0; i--) {
            ThreadReferenceNode prev = (ThreadReferenceNode)traversal.get(i);
            if ( (_prevInThread == null) && (!noPrevInThread) ) {
                ReferenceNode root = prev;
                while (root.getParent() != null)
                    root = root.getParent();
                if (root == _currentThreadRoot)
                    _prevInThread = prev.getURI();
                else
                    noPrevInThread = true;
            }
            if (_prevNew == null) {
                if (prev.getMessageStatus() == DBClient.MSG_STATUS_UNREAD) {
                    _prevNew = prev.getURI();
                    break;
                }
            }
        }

        boolean noNextInThread = false;
        for (int i = _currentIndex+1; i < traversal.size(); i++) {
            ThreadReferenceNode nxt = (ThreadReferenceNode)traversal.get(i);
            if ( (_nextInThread == null) && (!noNextInThread) ) {
                ReferenceNode root = nxt;
                while (root.getParent() != null)
                    root = root.getParent();
                
                if (root == _currentThreadRoot) {
                    _nextInThread = nxt.getURI();
                    _ui.debugMessage("next message is in the same thread");
                } else {
                    noNextInThread = true;
                    _ui.debugMessage("next message is NOT in the same thread: \n" + nxt + "\nnext root: " + root + "\n current root: " + _currentThreadRoot);
                }
            }
            if (_nextNew == null) {
                if (nxt.getMessageStatus() == DBClient.MSG_STATUS_UNREAD) {
                    _nextNew = nxt.getURI();
                    break;
                }
            }
        }
        
        int threadIndex = _roots.indexOf(_currentThreadRoot);
        if (threadIndex > 0)
            _prevThread = ((ReferenceNode)_roots.get(threadIndex-1)).getURI();
        
        if (threadIndex + 1 < _roots.size())
            _nextThread = ((ReferenceNode)_roots.get(threadIndex+1)).getURI();
    }
    
    public ThreadReferenceNode getThreadRoot() { return (ThreadReferenceNode)_currentThreadRoot; }
    
    private List traverse(final SyndieURI target) {
        Walker walker = new Walker(target);
        ReferenceNode.walk(_roots, walker);
        List rv = walker.getNodes();
        _ui.debugMessage("traversal (selected=" + _currentIndex + " root=" + _currentThreadRoot.getURI() + ")");
        for (int i = 0; i < rv.size(); i++)
            _ui.debugMessage(i + ": " + ((ReferenceNode)rv.get(i)).getURI().toString());
        return rv;
    }
    private class Walker implements ReferenceNode.Visitor {
        private SyndieURI _target;
        private List _rv;
        public Walker(SyndieURI target) {
            _target = target;
            _rv = new ArrayList();
        }
        public List getNodes() { return _rv; }
        public void visit(ReferenceNode node, int depth, int siblingOrder) {
            SyndieURI uri = node.getURI();
            if (uri != null) {
                _rv.add(node);
                if (uri.equals(_target)) {
                    _currentIndex = _rv.size()-1;
                    ReferenceNode cur = node;
                    while (cur.getParent() != null)
                        cur = cur.getParent();
                    _currentThreadRoot = cur;
                }
            }
        }
    }
    
    public SyndieURI getNextNew() { return _nextNew; }
    public SyndieURI getPreviousNew() { return _prevNew; }
    public SyndieURI getNextInThread() { return _nextInThread; }
    public SyndieURI getPreviousInThread() { return _prevInThread; }
    public SyndieURI getNextThread() { return _nextThread; }
    public SyndieURI getPreviousThread() { return _prevThread; }
    public SyndieURI getMessageTreeURI() { return _treeURI; }
}
