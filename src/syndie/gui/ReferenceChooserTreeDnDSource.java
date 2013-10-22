package syndie.gui;

import net.i2p.data.Hash;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.ChannelInfo;
import syndie.data.NymReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.WatchedChannel;
import syndie.db.DBClient;
import syndie.db.UI;

class ReferenceChooserTreeDnDSource {
    private DBClient _client;
    private UI _ui;
    private ReferenceChooserTree _tree;
    private boolean _isDragging;
    private boolean _allowSelectTrees;
    
    public ReferenceChooserTreeDnDSource(DBClient client, UI ui, ReferenceChooserTree tree) { this(client, ui, tree, false); }
    public ReferenceChooserTreeDnDSource(DBClient client, UI ui, ReferenceChooserTree tree, boolean selTrees) {
        _client = client;
        _ui = ui;
        _tree = tree;
        _isDragging = false;
        _allowSelectTrees = selTrees;
        init();
    }
    
    public boolean isDragging() { return _isDragging; }
    
    private Tree getTree() { return _tree.getTree(); }
    
    private void init() {
        Transfer transfer[] = new Transfer[] { TextTransfer.getInstance() };
        int ops = DND.DROP_COPY;
        DragSource source = new DragSource(getTree(), ops);
        source.setTransfer(transfer);
        source.addDragListener(new DragSourceListener() {
            public void dragFinished(DragSourceEvent evt) {
                _isDragging = false;
            }
            public void dragSetData(DragSourceEvent evt) {
                TreeItem item = _tree.getSelectedItem();
                //System.out.println("dragSetData: " + item);
                if (item == null) {
                    // maybe we should find a way to transfer trees instead of just individual bookmarks?
                    if (_allowSelectTrees) {
                        evt.data = new BookmarkDnD().toString();
                        evt.doit = true;
                        _ui.debugMessage("dragSetData to a dummy bookmarkdnd");
                        return;
                    }
                    evt.doit = false;
                    _ui.debugMessage("dragSetData to null");
                    return;
                }

                BookmarkDnD src = null;
                
                NymReferenceNode bookmark = _tree.getBookmark(item);
                if (bookmark != null) {
                    src = new BookmarkDnD();
                    src.desc = bookmark.getDescription();
                    String name = bookmark.getName();
                    src.name = name;
                    src.uri = bookmark.getURI();
                }
                if (src == null) {
                    ChannelInfo chan = _tree.getPostChannel(item);
                    if (chan != null) {
                        src = new BookmarkDnD();
                        src.desc = chan.getDescription();
                        String name = chan.getName();
                        if (name == null)
                            name = chan.getChannelHash().toBase64();
                        src.name = name;
                        src.uri = SyndieURI.createScope(chan.getChannelHash());
                    }
                }
                if (src == null) {
                    ChannelInfo chan = _tree.getManageChannel(item);
                    if (chan != null) {
                        src = new BookmarkDnD();
                        src.desc = chan.getDescription();
                        String name = chan.getName();
                        if (name == null)
                            name = chan.getChannelHash().toBase64();
                        src.name = name;
                        src.uri = SyndieURI.createScope(chan.getChannelHash());
                    }
                }
                if (src == null) {
                    WatchedChannel watched = _tree.getWatchedChannel(item);
                    if (watched != null) {
                        Hash scope = _client.getChannelHash(watched.getChannelId());
                        src = new BookmarkDnD();
                        src.desc = "";
                        String name = _client.getChannelName(watched.getChannelId());
                        if (name == null)
                            name = scope.toBase64();
                        src.name = name;
                        src.uri = SyndieURI.createScope(scope);
                    }
                }
                
                if (_allowSelectTrees) {
                    if ( (item == _tree.getWatchedRoot()) ||
                         (item == _tree.getBookmarkRoot()) ||
                         (item == _tree.getManageRoot()) ||
                         (item == _tree.getPostRoot()) ) {
                        evt.data = new BookmarkDnD().toString();
                        evt.doit = true;
                        _ui.debugMessage("dragSetData to a rooted dummy bookmarkdnd");
                        return;
                    }
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
                evt.doit = isDraggable(); // don't drag when nothing is selected
                //System.out.println("dragStart: " + evt.doit);
                _isDragging = true;
            }
        });
    }
    
    private boolean isDraggable() {
        TreeItem item = _tree.getSelectedItem();
        if (item == null)
            return false;
        
        NymReferenceNode bookmark = _tree.getBookmark(item);
        if ( (bookmark != null) || (item.equals(_tree.getBookmarkRoot())) ) {
            if (_allowSelectTrees)
                return true;
            else if (bookmark != null)
                return bookmark.getURI() != null;
        }
        ChannelInfo chan = _tree.getPostChannel(item);
        if ( (chan != null) || (item.equals(_tree.getPostRoot())) ) {
            return true;
        }
        chan = _tree.getManageChannel(item);
        if ( (chan != null) || (item.equals(_tree.getManageRoot())) ) {
            return true;
        }
        WatchedChannel watched = _tree.getWatchedChannel(item);
        if (watched != null) {
            return true;
        } else if ( (_allowSelectTrees) && (item == _tree.getWatchedRoot()) ) {
            return true;
        }
        return false;
    }    
}
