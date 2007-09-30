package syndie.gui;

import java.net.URISyntaxException;
import net.i2p.data.Base64;
import net.i2p.data.Hash;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.DropTargetListener;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.ScrollBar;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;

class NymChannelTreeDnD {
    private DBClient _client;
    private UI _ui;
    private BookmarkControl _bookmarkControl;
    private NymChannelTree _tree;
    private boolean _active;
    
    private DragSource _source;
    private DropTarget _target;

    private boolean _isDragging;
    private NymReferenceNode _dragging;
    
    public NymChannelTreeDnD(DBClient client, UI ui, BookmarkControl bookmarkControl, NymChannelTree tree) {
        _client = client;
        _ui = ui;
        _bookmarkControl = bookmarkControl;
        _tree = tree;
        _active = false;
        _isDragging = false;
    }
    
    public void enable() {
        if (_active) return;
        _active = true;
        initDnD();
    }
    public void disable() {
        if (!_active) return;
        _active = false;
        disposeDnD();
    }
    
    private void initDnD() {
        initSource();
        initTarget();
    }
    private void disposeDnD() {
        if (_source != null)
            _source.dispose();
        _source = null;
        if (_target != null)
            _target.dispose();
        _target = null;
    }
    
    private void initSource() {
        Transfer transfer[] = new Transfer[] { TextTransfer.getInstance() };
        int ops = DND.DROP_COPY;
        DragSource source = new DragSource(_tree.getTree(), ops);
        source.setTransfer(transfer);
        source.addDragListener(new DragSourceListener() {
            public void dragFinished(DragSourceEvent evt) {
                _isDragging = false;
            }
            public void dragSetData(DragSourceEvent evt) {
                TreeItem item = getSelectedItem();
                //System.out.println("dragSetData: " + item);
                if (item == null) {
                    // maybe we should find a way to transfer trees instead of just individual bookmarks?
                    /*
                    if (_allowSelectTrees) {
                        evt.data = new BookmarkDnD().toString();
                        evt.doit = true;
                        _ui.debugMessage("dragSetData to a dummy bookmarkdnd");
                        return;
                    }
                    */
                    evt.doit = false;
                    _ui.debugMessage("dragSetData to null");
                    return;
                }

                BookmarkDnD src = null;
                
                ReferenceNode bookmark = _tree.getNode(item);
                if (bookmark != null) {
                    src = new BookmarkDnD();
                    src.desc = bookmark.getDescription();
                    String name = bookmark.getName();
                    src.name = name;
                    src.uri = bookmark.getURI();
                }
                
                //BookmarkDnD bookmark = getBookmark(sel[0], (ReferenceNode)_itemToNode.get(sel[0]));
                if (src != null) {
                    evt.data = src.toString();
                    evt.doit = true;
                } else {
                    evt.doit = false;
                }
                //System.out.println("dragSetData: " + evt.data);
            }
            public void dragStart(DragSourceEvent evt) {
                ReferenceNode draggable = getDraggable();
                if ( (draggable != null) && (draggable.getURI() != null) ) {
                    evt.doit = true;
                    _isDragging = true;
                    _dragging = (NymReferenceNode)draggable;
                } else {
                    evt.doit = false;
                    _dragging = null;
                }
            }
        });
        
        _source = source;
    }
    
    private TreeItem getSelectedItem() {
        TreeItem items[] = _tree.getTree().getSelection();
        if ( (items != null) && (items.length == 1) )
            return items[0];
        else
            return null;
    }
    
    private ReferenceNode getDraggable() {
        TreeItem item = getSelectedItem();
        if (item == null)
            return null;
        
        return _tree.getNode(item);
    }
    
    /// target stuff
    
    // we expand a node if we are hovering over it for a half second or more
    private long _dndHoverBegin;
    private TreeItem _dndHoverCurrent;
    
    /** currently the insert marks in SWT3.3M4 don't work on linux */
    private static final boolean USE_INSERT_MARKS = (System.getProperty("os.name").indexOf("nix") == -1);

    private void initTarget() {
        if (USE_INSERT_MARKS) {
            _tree.getTree().addMouseTrackListener(new MouseTrackListener() {
                public void mouseEnter(MouseEvent mouseEvent) { 
                    _tree.getTree().setInsertMark(null, true);
                }
                public void mouseExit(MouseEvent mouseEvent) { 
                    _tree.getTree().setInsertMark(null, true); 
                }
                public void mouseHover(MouseEvent mouseEvent) {}
            });
        }
        
        int ops = DND.DROP_COPY | DND.DROP_LINK; // move doesn't seem to work properly...
        Transfer transfer[] = new Transfer[] { TextTransfer.getInstance() };
        DropTarget target = new DropTarget(_tree.getTree(), ops);
        target.setTransfer(transfer);
        target.addDropListener(new DropTargetListener() {
            public void dragEnter(DropTargetEvent evt) {
                evt.detail = DND.DROP_COPY;
                evt.feedback = DND.FEEDBACK_SELECT | DND.FEEDBACK_EXPAND | DND.FEEDBACK_SCROLL;
            }
            public void dragLeave(DropTargetEvent evt) {
                //System.out.println("dragLeave: " + evt + "/" + evt.feedback + "/" + evt.operations + "/" + evt.detail);
                if (USE_INSERT_MARKS)
                    _tree.getTree().setInsertMark(null, true);
            }
            public void dragOperationChanged(DropTargetEvent evt) {}
            public void dragOver(DropTargetEvent evt) {
                Tree tree = _tree.getTree();
                Point pt = null;
                // swt3.3M4/gtk/linux seems to put the original drag-from location in evt.x/evt.y
                // when dragging and dropping from the same tree, which is completely useless
                //pt = tree.toControl(evt.x, evt.y);
                pt = tree.toControl(tree.getDisplay().getCursorLocation());
                
                TreeItem item = tree.getItem(pt);
                //System.out.println("dragOver: " + item + " pt:" + pt + " evt: " + evt.x + "/" + evt.y);
                setFeedback(item, evt, pt);
                expand(item, evt, pt);
                scroll(tree, item, evt, pt);
            }
            private void setFeedback(TreeItem item, DropTargetEvent evt, Point pt) {
                if (item != null) {
                    TreeItem root = item;
                    while (root.getParentItem() != null)
                        root = root.getParentItem();

                    ReferenceNode node = _tree.getNode(item);
                    if (root == item) { // bookmark root, (node==null)
                        evt.detail = DND.DROP_COPY;
                        evt.feedback = DND.FEEDBACK_SELECT | DND.FEEDBACK_EXPAND | DND.FEEDBACK_SCROLL;
                    } else if (node.getURI() == null) {
                        evt.detail = DND.DROP_COPY;
                        evt.feedback = DND.FEEDBACK_SELECT | DND.FEEDBACK_EXPAND | DND.FEEDBACK_SCROLL;
                    } else {
                        evt.detail = DND.DROP_COPY;
                        evt.feedback = DND.FEEDBACK_INSERT_AFTER;
                    }
                } else {
                    evt.detail = DND.DROP_NONE;
                    evt.feedback = DND.FEEDBACK_NONE;
                    //System.out.println("no item selected, setting detail/feedback to none");
                }
            }
            private void expand(TreeItem item, DropTargetEvent evt, Point pt) {
                if (item != null) {
                    if (item.getItemCount() > 0) {
                        if (!item.getExpanded()) {
                            if ( (_dndHoverBegin > 0) && (_dndHoverBegin + 500 < System.currentTimeMillis()) && (_dndHoverCurrent == item) ) {
                                item.setExpanded(true);
                                _dndHoverBegin = -1;
                                _dndHoverCurrent = null;
                            } else if (_dndHoverCurrent != item) {
                                _dndHoverBegin = System.currentTimeMillis();
                                _dndHoverCurrent = item;
                            }
                        }
                        if (USE_INSERT_MARKS)
                            _tree.getTree().setInsertMark(null, true);
                    } else {
                        _dndHoverBegin = -1;
                        _dndHoverCurrent = null;
                    }
                } else {
                    _dndHoverBegin = -1;
                    _dndHoverCurrent = null;
                    if (USE_INSERT_MARKS)
                        _tree.getTree().setInsertMark(null, true);
                }
            }
            private void scroll(Tree tree, TreeItem item, DropTargetEvent evt, Point pt) {
                int height = tree.getClientArea().height;
                // scroll up/down when over items at the top/bottom 5% of the height
                int margin = height/20;
                if (pt.y <= margin)
                    scrollUp(tree);
                else if (pt.y >= height-margin)
                    scrollDown(tree);
            }
            private void scrollUp(Tree tree) {
                ScrollBar bar = tree.getVerticalBar();
                if ( (bar != null) && (bar.getSelection() > bar.getMinimum()) )
                    bar.setSelection(bar.getSelection() - bar.getIncrement());
            }
            private void scrollDown(Tree tree) {
                ScrollBar bar = tree.getVerticalBar();
                if ( (bar != null) && (bar.getSelection() < bar.getMaximum()) )
                    bar.setSelection(bar.getSelection() + bar.getIncrement());
            }
            private boolean isPointFirstHalf(Tree tree, Point point, TreeItem item) {
                Rectangle loc = item.getBounds();
                int margin = loc.height / 2;
                return (point.y < (loc.y + margin));
            }
            public void drop(DropTargetEvent evt) {
                _ui.errorMessage("node dropped: " + evt);
                //System.out.println("drop: " + evt);
                if (USE_INSERT_MARKS)
                    _tree.getTree().setInsertMark(null, true);
                
                Tree tree = _tree.getTree();
                Point pt = tree.toControl(evt.x, evt.y);
                TreeItem item = tree.getItem(pt);
                boolean before = isPointFirstHalf(tree, pt, item);
                
                TreeItem root = item;
                while (root.getParentItem() != null)
                    root = root.getParentItem();
                
                long parentGroupId = -1;
                int siblingOrder = 0;
                
                ReferenceNode node = _tree.getNode(item);
                if (!(node instanceof NymReferenceNode)) {
                    evt.detail = DND.DROP_NONE;
                    _ui.errorMessage("node dropped is not a nymRefNode: " + node);
                    return;
                }
                NymReferenceNode cur = (NymReferenceNode)node;
                if (cur != null) {
                    if (cur.getURI() == null) {
                        parentGroupId = cur.getGroupId();
                        siblingOrder = 0;
                    } else {
                        parentGroupId = cur.getParentGroupId();
                        TreeItem parent = item.getParentItem();
                        if (parent != null) {
                            TreeItem siblings[] = parent.getItems();
                            for (int i = 0; i < siblings.length; i++) {
                                if (item == siblings[i]) {
                                    if (before)
                                        siblingOrder = i;
                                    else
                                        siblingOrder = i+1;
                                    break;
                                }
                            }
                        }
                    }
                }
                
                if (evt.data == null) {
                    evt.detail = DND.DROP_NONE;
                    _ui.errorMessage("drop event data is null");
                    return;
                } else {
                    _ui.debugMessage("dropping bookmark under " + parentGroupId + " order " + siblingOrder + " (before? " + before + " target: " + item + ")");
                    BookmarkDnD bookmark = new BookmarkDnD();
                    if (_isDragging) {
                        bookmark.uri = _dragging.getURI();
                        bookmark.name = _dragging.getName();
                        bookmark.desc = _dragging.getDescription();
                    } else {
                        bookmark.fromString(evt.data.toString());
                        if (bookmark.uri == null) { // wasn't in bookmark syntax, try as a uri
                            String str = evt.data.toString();
                            try {
                                SyndieURI uri = new SyndieURI(str);
                                bookmark.uri = uri;
                                bookmark.name = uri.toString();
                                bookmark.desc = "";
                            } catch (URISyntaxException use) {
                                _ui.debugMessage("invalid uri: " + str, use);
                                byte val[] = Base64.decode(str);
                                if ( (val != null) && (val.length == Hash.HASH_LENGTH) ) {
                                    SyndieURI uri = SyndieURI.createScope(new Hash(val));
                                    bookmark.uri = uri;
                                    bookmark.name = uri.toString();
                                }
                            }
                        }
                    }

                    NymReferenceNode newNode = new NymReferenceNode(bookmark.name, bookmark.uri, bookmark.desc, -1, -1, parentGroupId, siblingOrder, false, false, false);
                    if (_isDragging) {
                        _ui.debugMessage("removing old node: " + _dragging.getGroupId() + "/" + _dragging.getParentGroupId() + ": " + _dragging);
                        _bookmarkControl.deleteBookmark(_dragging.getGroupId());
                    }
                    _bookmarkControl.bookmark(newNode, true);
                    _ui.debugMessage("new node: " + newNode.getGroupId() + "/" + newNode.getParentGroupId() + ": " + newNode);
                    _tree.bookmarksUpdated();
                    _tree.recalcTree();
                    _tree.forceFocus();
                }
            }
            public void dropAccept(DropTargetEvent evt) {
                //System.out.println("dropAccept: " + evt);
            }
        });
        _target = target;
    }
}
