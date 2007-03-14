package syndie.gui;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import net.i2p.data.Base64;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.TreeEditor;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.DropTargetListener;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.MenuListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.ScrollBar;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.WatchedChannel;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 * based off the ManageForumReferences code, though semantically dealing with
 * different issues
 */
class MessageReferencesEditor extends BaseComponent implements Themeable, Translatable {
    private NavigationControl _navControl;
    private Composite _parent;
    private Tree _targetTree;
    private TreeColumn _colName;
    private TreeColumn _colDesc;
    private TreeColumn _colType;
    private TreeColumn _colTarget;
    private Menu _targetMenu;
    private MenuItem _targetMenuView;
    private MenuItem _targetMenuAdd;
    private MenuItem _targetMenuRemove;
    private List _targetReferenceNodes;
    private Map _targetItemToNode;
    private LinkBuilderPopup _editPopup;
    
    public MessageReferencesEditor(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite parent, NavigationControl navControl) {
        super(client, ui, themes, trans);
        _parent = parent;
        _navControl = navControl;
        initComponents();
    }
    
    public List getReferenceNodes() { return _targetReferenceNodes; }
    public void setReferenceNodes(List nodes) {
        TreeItem items[] = _targetTree.getItems();
        for (int i = 0; i < items.length; i++) items[i].dispose();
        _targetItemToNode.clear();
        _targetReferenceNodes.clear();
        for (int i = 0; i < nodes.size(); i++)
            add((ReferenceNode)nodes.get(i), null, null);
    }
    
    private void initComponents() {
        _targetTree = new Tree(_parent, SWT.MULTI | SWT.BORDER);
        _colName = new TreeColumn(_targetTree, SWT.LEFT);
        _colDesc = new TreeColumn(_targetTree, SWT.LEFT);
        _colType = new TreeColumn(_targetTree, SWT.CENTER);
        _colTarget = new TreeColumn(_targetTree, SWT.LEFT);
        _targetTree.setHeaderVisible(true);
        _targetTree.setLinesVisible(true);
        _targetReferenceNodes = new ArrayList();
        _targetItemToNode = new HashMap();
        
        _targetTree.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) {
                editTarget((TreeItem)evt.item);
            }
            public void widgetSelected(SelectionEvent evt) {
                editTarget((TreeItem)evt.item);
            }
        });
        _targetMenu = new Menu(_targetTree);
        _targetTree.setMenu(_targetMenu);
        _targetMenu.addMenuListener(new MenuListener() {
            public void menuHidden(MenuEvent menuEvent) {}
            public void menuShown(MenuEvent menuEvent) {
                int sel = _targetTree.getSelectionCount();
                _targetMenuRemove.setEnabled(sel > 0);
                _targetMenuAdd.setEnabled(sel == 0);
                _targetMenuView.setEnabled(sel > 0);
            }
        });
        
        _targetMenuView = new MenuItem(_targetMenu, SWT.PUSH);
        _targetMenuView.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                TreeItem items[] = _targetTree.getSelection();
                for (int i = 0; i < items.length; i++) {
                    ReferenceNode node = (ReferenceNode)_targetItemToNode.get(items[i]);
                    if (node != null) {
                        SyndieURI uri = node.getURI();
                        if (uri != null)
                            _navControl.view(uri);
                    }
                }
            }
        });
        
        _targetMenuAdd = new MenuItem(_targetMenu, SWT.PUSH);
        _targetMenuAdd.addSelectionListener(new FireSelectionListener() { public void fire() { add(); } });
        
        _targetMenuRemove = new MenuItem(_targetMenu, SWT.PUSH);
        _targetMenuRemove.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                TreeItem items[] = _targetTree.getSelection();
                for (int i = 0; i < items.length; i++) {
                    ReferenceNode node = (ReferenceNode)_targetItemToNode.get(items[i]);
                    if (node != null) {
                        if (node.getParent() != null)
                            node.getParent().removeChild(node);
                        else
                            _targetReferenceNodes.remove(node);
                    }
                    items[i].dispose();
                }
            }
        });
        
        initDnDTarget();
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
        
        _colName.pack();
        _colDesc.pack();
        _colType.pack();
        _colTarget.pack();
        
        loadData();
    }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        _editPopup.dispose();
    }
    
    private void loadData() {
        //List refs = _manage.getRefs();
        //if (refs != null)
        //    _targetReferenceNodes.addAll(refs);
        redrawTarget();
    }
    
    private void save() {
        //_manage.setReferences(_targetReferenceNodes);
        dispose();
    }
    
    private void add() {
        if (_editPopup == null)
            _editPopup = new LinkBuilderPopup(_client, _ui, _themeRegistry, _translationRegistry, _parent.getShell(), new EditListener());
        _parentNode = null;
        _currentNode = null;
        _editPopup.showPopup();
    }
    
    private void editTarget(final TreeItem item) {
        final TreeEditor ed = new TreeEditor(_targetTree);
        ed.grabHorizontal = true;
        ed.horizontalAlignment = SWT.LEFT;
        
        Point pt = _targetTree.toControl(_targetTree.getDisplay().getCursorLocation());
        int x = pt.x;
        int col = -1;
        if (x > _colName.getWidth()) {
            if (x > _colName.getWidth() + _colDesc.getWidth() + 2*_targetTree.getGridLineWidth())
                col = 2;
            else
                col = 1;
        } else {
            col = 0;
        }
        
        if (col == 2)
            return; // don't allow editing the uris atm
        
        final int column = col;
        
        final ReferenceNode node = (ReferenceNode)_targetItemToNode.get(item);
        if (node == null) return;
        
        final Text field = new Text(_targetTree, SWT.SINGLE);
        field.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        if (col == 0)
            field.setText((node.getName() != null ? node.getName() : ""));
        else if (col == 1)
            field.setText(node.getDescription() != null ? node.getDescription() : "");
        
        field.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                switch (evt.detail) {
                    case SWT.TRAVERSE_RETURN:
                    case SWT.TRAVERSE_TAB_NEXT:
                    case SWT.TRAVERSE_TAB_PREVIOUS:
                        String str = field.getText().trim();
                        if (column == 0)
                            node.setName(str);
                        else if (column == 1)
                            node.setDescription(str);
                        item.setText(column, str);
                        field.dispose();
                        ed.dispose();
                        return;
                    case SWT.TRAVERSE_ESCAPE:
                        field.dispose();
                        ed.dispose();
                        return;
                }
            }
        });
        field.addFocusListener(new FocusListener() {
            public void focusGained(FocusEvent focusEvent) {}
            public void focusLost(FocusEvent focusEvent) {
                if (!field.isDisposed()) {
                    String str = field.getText().trim();
                    if (column == 0)
                        node.setName(str);
                    else if (column == 1)
                        node.setDescription(str);
                    item.setText(column, str);
                    field.dispose();
                }
                ed.dispose();
            }
        });
        field.selectAll();
        field.setFocus();
        ed.setEditor(field, item, col);
    }
    
    private void redrawTarget() {
        _targetTree.setRedraw(false);
        _targetTree.removeAll();
        for (int i = 0; i < _targetReferenceNodes.size(); i++) {
            ReferenceNode node = (ReferenceNode)_targetReferenceNodes.get(i);
            String type = node.getReferenceType();
            if ( (type != null) && (type.equals(Constants.REF_TYPE_BANNED)) )
                continue; // keep the banned refs around, but don't edit them in this view
            addTarget(node, null);
        }
        _targetTree.setRedraw(true);
    }
    private void addTarget(ReferenceNode node, TreeItem parent) {
        if (node == null) return;
        TreeItem item = null;
        if (parent == null)
            item = new TreeItem(_targetTree, SWT.NONE);
        else
            item = new TreeItem(parent, SWT.NONE);
        render(node, item);
        _targetItemToNode.put(item, node);
        for (int i = 0; i < node.getChildCount(); i++)
            addTarget(node.getChild(i), item);
    }
    private void render(ReferenceNode node, TreeItem item) {
        if (node.getName() != null)
            item.setText(0, node.getName());
        else
            item.setText(0, "");
        if (node.getDescription() != null)
            item.setText(1, node.getDescription());
        else
            item.setText(1, "");
        if (node.getURI() != null) {
            item.setImage(2, ImageUtil.getTypeIcon(node.getURI()));
            if (node.getURI().getURL() != null)
                item.setText(3, node.getURI().getURL());
            else
                item.setText(3, node.getURI().toString());
        }
        
        setMinWidth(_colName, item.getText(0));
        setMinWidth(_colDesc, item.getText(1));
        _colType.pack();
        setMinWidth(_colTarget, item.getText(3));
    }
    
    private void setMinWidth(TreeColumn col, String text) {
        int width = ImageUtil.getWidth(text, _targetTree) + _targetTree.getGridLineWidth()*2 + 10;
        int existing = col.getWidth();
        if (width > existing) {
            _ui.debugMessage("Increasing the width on " + col.getText() + " from " + existing + " to " + width);
            col.setWidth(width);
        } else {
            //_browser.getUI().debugMessage("Keeping the width on " + col.getText() + " at " + existing + " (new width would be " + width + ")");
        }
    }
    
    // we expand a node if we are hovering over it for a half second or more
    private long _dndHoverBegin;
    private TreeItem _dndHoverCurrent;
        
    /** currently the insert marks in SWT3.3M4 don't work on linux */
    private static final boolean USE_INSERT_MARKS = (System.getProperty("os.name").indexOf("nix") == -1);
    
    private void initDnDTarget() {
        // if we are pulling from the refTree, do the full transfer direct
        if (USE_INSERT_MARKS) {
            _targetTree.addMouseTrackListener(new MouseTrackListener() {
                public void mouseEnter(MouseEvent mouseEvent) { 
                    _targetTree.setInsertMark(null, true);
                }
                public void mouseExit(MouseEvent mouseEvent) { 
                    _targetTree.setInsertMark(null, true); 
                }
                public void mouseHover(MouseEvent mouseEvent) {}
            });
        }
        
        int ops = DND.DROP_COPY | DND.DROP_LINK; // move doesn't seem to work properly...
        Transfer transfer[] = new Transfer[] { TextTransfer.getInstance() };
        DropTarget target = new DropTarget(_targetTree, ops);
        target.setTransfer(transfer);
        target.addDropListener(new DropTargetListener() {
            public void dragEnter(DropTargetEvent evt) {
                evt.detail = DND.DROP_COPY;
                evt.feedback = DND.FEEDBACK_SELECT | DND.FEEDBACK_EXPAND | DND.FEEDBACK_SCROLL;
            }
            public void dragLeave(DropTargetEvent evt) {
                //System.out.println("dragLeave: " + evt + "/" + evt.feedback + "/" + evt.operations + "/" + evt.detail);
                if (USE_INSERT_MARKS)
                    _targetTree.setInsertMark(null, true);
            }
            public void dragOperationChanged(DropTargetEvent evt) {}
            public void dragOver(DropTargetEvent evt) {
                Tree tree = _targetTree;
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

                    ReferenceNode node = (ReferenceNode)_targetItemToNode.get(item);
                    if (node != null) {
                        if (node.getURI() != null) {
                            evt.detail = DND.DROP_COPY;
                            evt.feedback = DND.FEEDBACK_INSERT_AFTER;
                        } else {
                            evt.detail = DND.DROP_COPY;
                            evt.feedback = DND.FEEDBACK_SELECT | DND.FEEDBACK_EXPAND | DND.FEEDBACK_SCROLL;
                        }
                    } else {
                        evt.detail = DND.DROP_COPY;
                        evt.feedback = DND.FEEDBACK_SELECT | DND.FEEDBACK_EXPAND | DND.FEEDBACK_SCROLL;
                    }
                } else {
                    evt.detail = DND.DROP_COPY;
                    evt.feedback = DND.FEEDBACK_SELECT | DND.FEEDBACK_EXPAND | DND.FEEDBACK_SCROLL;
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
                            _targetTree.setInsertMark(null, true);
                    } else {
                        _dndHoverBegin = -1;
                        _dndHoverCurrent = null;
                    }
                } else {
                    _dndHoverBegin = -1;
                    _dndHoverCurrent = null;
                    if (USE_INSERT_MARKS)
                        _targetTree.setInsertMark(null, true);
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
                if (item == null) return true;
                Rectangle loc = item.getBounds();
                int margin = loc.height / 2;
                return (point.y < (loc.y + margin));
            }
            public void drop(DropTargetEvent evt) {
                //System.out.println("drop: " + evt);
                if (USE_INSERT_MARKS)
                    _targetTree.setInsertMark(null, true);
                
                if (evt.data == null) {
                    evt.detail = DND.DROP_NONE;
                    return;
                }
                
                _ui.debugMessage("drop: " + evt);
                
                Tree tree = _targetTree;
                Point pt = tree.toControl(evt.x, evt.y);
                TreeItem item = tree.getItem(pt);
                boolean before = isPointFirstHalf(tree, pt, item);
                
                ReferenceNode toAdd = getToAdd(evt.data.toString());
                ReferenceNode node = (ReferenceNode)_targetItemToNode.get(item);
                if (node == null)
                    _targetReferenceNodes.add(toAdd);
                else if (node.getURI() == null) // its a folder
                    node.addChild(toAdd);
                else if (node.getParent() == null) // not a folder, but top level
                    _targetReferenceNodes.add(toAdd);
                else // not a folder and not top level
                    node.getParent().addChild(toAdd);
                    
                redrawTarget();
            }
            public void dropAccept(DropTargetEvent evt) {}
        });
    }
    
    private ReferenceNode getToAdd(String data) {
        BookmarkDnD bookmark = null;
        SyndieURI uri = null;

        bookmark = new BookmarkDnD();
        bookmark.fromString(data);
        if (bookmark.uri == null) { // parse fail
            String str = data;
            try {
                uri = new SyndieURI(str);
            } catch (URISyntaxException use) {
                _ui.debugMessage("invalid uri: " + str, use);
                byte val[] = Base64.decode(str);
                if ( (val != null) && (val.length == Hash.HASH_LENGTH) ) {
                    uri = SyndieURI.createScope(new Hash(val));
                }
            }
        }

        if ( (uri == null) && (bookmark == null) ) {
            return null;
        } else if (bookmark != null) {
            return new ReferenceNode(bookmark.name, bookmark.uri, bookmark.desc, null);
        } else {
            return new ReferenceNode(System.currentTimeMillis()+"", uri, "", null);
        }
    }
    
    public void applyTheme(Theme theme) {
        _targetTree.setFont(theme.TREE_FONT);
    }
    
    private static final String T_COLDESC = "syndie.gui.messagereferenceseditor.coldesc";
    private static final String T_COLNAME = "syndie.gui.messagereferenceseditor.colname";
    private static final String T_COLTYPE = "syndie.gui.messagereferenceseditor.coltype";
    private static final String T_COLTARGET = "syndie.gui.messagereferenceseditor.coltarget";
    private static final String T_TARGET_REMOVE = "syndie.gui.messagereferenceseditor.target.remove";
    private static final String T_TARGET_ADD = "syndie.gui.messagereferenceseditor.target.add";
    private static final String T_TARGET_VIEW = "syndie.gui.messagereferenceseditor.target.view";
    
    public void translate(TranslationRegistry registry) {
        _colDesc.setText(registry.getText(T_COLDESC, "Description"));
        _colName.setText(registry.getText(T_COLNAME, "Name"));
        _colType.setText(registry.getText(T_COLTYPE, "Type"));
        _colTarget.setText(registry.getText(T_COLTARGET, "Target"));
        _targetMenuRemove.setText(registry.getText(T_TARGET_REMOVE, "Remove reference"));
        _targetMenuAdd.setText(registry.getText(T_TARGET_ADD, "Add reference"));
        _targetMenuView.setText(registry.getText(T_TARGET_VIEW, "View reference"));
    }
    
    ReferenceNode _parentNode;
    ReferenceNode _currentNode;
    
    private class EditListener implements LinkBuilderPopup.LinkBuilderSource {
        public int getPageCount() { return 0; }
        public List getAttachmentDescriptions() { return new ArrayList(); }
        public void uriBuilt(SyndieURI uri, String text) {
            if (_currentNode == null) {
                add(uri, _parentNode, text);
            } else {
                _currentNode.setURI(uri);
                for (Iterator iter = _targetItemToNode.keySet().iterator(); iter.hasNext(); ) {
                    TreeItem item = (TreeItem)iter.next();
                    ReferenceNode node = (ReferenceNode)_targetItemToNode.get(item);
                    if (node == _currentNode) {
                        update(item, _currentNode, text);
                        break;
                    }
                }
            }
            _currentNode = null;
            _parentNode = null;
        }
    }

    private void add(ReferenceNode node, TreeItem parent, List includedURIs) {
        TreeItem item = parent;
        
        if ( (node.getURI() != null) && ( (includedURIs == null) || (!includedURIs.contains(node.getURI())) ) ) {
            if (includedURIs != null)
                includedURIs.add(node.getURI());
            
            _ui.debugMessage("adding: " + node);
            if (parent != null)
                item = new TreeItem(parent, SWT.NONE);
            else
                item = new TreeItem(_targetTree, SWT.NONE);

            if (parent == null)
                _targetReferenceNodes.add(node);
            _targetItemToNode.put(item, node);

            update(item, node, node.getName());
        }
        for (int i = 0; i < node.getChildCount(); i++)
            add(node.getChild(i), item, includedURIs);
    }
    
    private void update(TreeItem item, ReferenceNode node, String name) {
        if ( (name != null) && (name.trim().length() > 0) )
            node.setName(name);
        render(node, item);
        item.setExpanded(true);
        _targetTree.showItem(item);
    }
    
    private void add(SyndieURI uri, ReferenceNode parent, String name) {
        if ( (name == null) || (name.trim().length() <= 0) )
            name = uri.getType();
        
        ReferenceNode child = null;
        if (parent == null)
            child = new ReferenceNode(name, uri, "", "");
        else
            child = parent.addChild(name, uri, "", "");
        _ui.debugMessage("child added: " + child);
        TreeItem parentItem = null;
        if (parent != null) {
            for (Iterator iter = _targetItemToNode.keySet().iterator(); iter.hasNext(); ) {
                TreeItem item = (TreeItem)iter.next();
                if (parent == _targetItemToNode.get(item)) {
                    parentItem = item;
                    parentItem.setExpanded(true);
                    break;
                }
            }
        }
        add(child, parentItem, null);
    }
}
