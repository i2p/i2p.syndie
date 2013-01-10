package syndie.gui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TreeEditor;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.MenuListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.ArchiveInfo;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 *
 */
public class ManageReferenceChooser extends BaseComponent implements Translatable, Themeable {
    private NavigationControl _navControl;
    private BookmarkControl _bookmarkControl;
    private BanControl _banControl;
    private Composite _parent;
    private Tree _tree;
    private TreeColumn _colName;
    private TreeColumn _colTarget;
    private TreeColumn _colType;
    private TreeColumn _colDescription;
    private TreeEditor _treeEditor;
    private MenuItem _add;
    private MenuItem _addChild;
    private MenuItem _edit;
    private MenuItem _view;
    private MenuItem _remove;
    private MenuItem _importToForum;
    private MenuItem _importToLocal;
    private MenuItem _importAllToLocal;
    private ArrayList _refs;
    private EditPopup _editPopup;
    //private MessageEditor _editor;
    
    /** TreeItem to ReferenceNode */
    private Map _itemToRefNode;
    
    private boolean _editable;
    
    public ManageReferenceChooser(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite parent, NavigationControl navControl, BanControl banControl, BookmarkControl bookmarkControl, boolean editable) {
        super(client, ui, themes, trans);
        _navControl = navControl;
        _banControl = banControl;
        _bookmarkControl = bookmarkControl;
        _parent = parent;
        _editable = editable;
        //_editor = editor;
        _refs = new ArrayList();
        _itemToRefNode = new HashMap();
        initComponents();
    }
    
    public Control getControl() { return _tree; }
    
    private void initComponents() {
        _tree = new Tree(_parent, SWT.SINGLE | SWT.BORDER | SWT.FULL_SELECTION);
        _colName = new TreeColumn(_tree, SWT.LEFT);
        _colTarget = new TreeColumn(_tree, SWT.CENTER);
        _colType = new TreeColumn(_tree, SWT.LEFT);
        _colDescription = new TreeColumn(_tree, SWT.LEFT);
        _tree.setLinesVisible(true);
        _tree.setHeaderVisible(true);
        
        SyndieTreeListener lsnr = new SyndieTreeListener(_tree) {
            public void mouseUp(MouseEvent evt) {
                super.mouseUp(evt);
                if (!_editable) return;
                TreeItem items[] = _tree.getSelection();
                if ( (items == null) || (items.length != 1) ) return;
                
                int col = getColumn(evt.x);
                switch (col) {
                    case 3: 
                        editDescription();
                        return;
                    case 2:
                        editType();
                        return;
                    case 1:
                        edit();
                        return;
                    case 0:
                        editName();
                        return;
                }
            }
        };
        _tree.addSelectionListener(lsnr);
        _tree.addTraverseListener(lsnr);
        _tree.addKeyListener(lsnr);
        _tree.addMouseListener(lsnr);
        _tree.addControlListener(lsnr);
        
        _treeEditor = new TreeEditor(_tree);
        _treeEditor.grabHorizontal = true;
        _treeEditor.horizontalAlignment = SWT.LEFT;
        
        _colTarget.setWidth(20);
        
        Menu menu = new Menu(_tree);
        _tree.setMenu(menu);
        _add = new MenuItem(menu, SWT.PUSH);
        _add.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { add(); }
            public void widgetSelected(SelectionEvent selectionEvent) { add(); }
        });
        _add.setEnabled(_editable);
        _addChild = new MenuItem(menu, SWT.PUSH);
        _addChild.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addChild(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addChild(); }
        });
        _addChild.setEnabled(_editable);
        _edit = new MenuItem(menu, SWT.PUSH);
        _edit.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { edit(); }
            public void widgetSelected(SelectionEvent selectionEvent) { edit(); }
        });
        _edit.setEnabled(_editable);
        _view = new MenuItem(menu, SWT.PUSH);
        _view.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(); }
        });
        _remove = new MenuItem(menu, SWT.PUSH);
        _remove.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { remove(); }
            public void widgetSelected(SelectionEvent selectionEvent) { remove(); }
        });
        _remove.setEnabled(_editable);
        _importToForum = new MenuItem(menu, SWT.PUSH);
        _importToForum.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { importBookmarksToForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { importBookmarksToForum(); }
        });
        _importToForum.setEnabled(_editable);
        _importToLocal = new MenuItem(menu, SWT.PUSH);
        _importToLocal.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { importBookmarksToLocal(false); }
            public void widgetSelected(SelectionEvent selectionEvent) { importBookmarksToLocal(false); }
        });
        _importAllToLocal = new MenuItem(menu, SWT.PUSH);
        _importAllToLocal.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { importBookmarksToLocal(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { importBookmarksToLocal(true); }
        });
        menu.addMenuListener(new MenuListener() {
            public void menuHidden(MenuEvent menuEvent) {}
            public void menuShown(MenuEvent evt) { configMenu(); }
        });

        //lazy init
        //_editPopup = new EditPopup();
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    private int getColumn(int x) {
        int start = _tree.getClientArea().width - _colDescription.getWidth();
        if (x >= start)
            return 3;
        start -= _colType.getWidth() + _tree.getGridLineWidth();
        if (x >= start)
            return 2;
        start -= _colTarget.getWidth() + _tree.getGridLineWidth();
        if (x >= start)
            return 1;
        
        // column width of the first column includes the tree indentation and checkbox,
        // while item bounds do not.  we don't want to edit the name if we click on the
        // expand/collapse
        TreeItem item = null;
        if (_tree.getItemCount() > 0)
            item = _tree.getItem(0);
        if (item != null)
            start -= item.getBounds(0).width;
        else
            start -= _colName.getWidth() + _tree.getGridLineWidth();
        if (x >= start)
            return 0;
        else
            return -1;
    }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        if (_editPopup != null)
            _editPopup.dispose();
    }
    private void add() {
        if (_editPopup == null)
            _editPopup = new EditPopup(_client, _ui, _themeRegistry, _translationRegistry, _navControl, _banControl, _bookmarkControl);
        _editPopup.setParent(null);
        _editPopup.setCurrentNode(null);
        _editPopup.showPopup();
    }
    private void addChild() {
        if (_editPopup == null)
            _editPopup = new EditPopup(_client, _ui, _themeRegistry, _translationRegistry, _navControl, _banControl, _bookmarkControl);
        _editPopup.setParent(getSelectedNode());
        _editPopup.setCurrentNode(null);
        _editPopup.showPopup();
    }
    private void edit() {
        ReferenceNode node = getSelectedNode();
        if (node != null) {
            if (_editPopup == null)
                _editPopup = new EditPopup(_client, _ui, _themeRegistry, _translationRegistry, _navControl, _banControl, _bookmarkControl);
            _editPopup.setCurrentNode(node);
            _editPopup.showPopup(node.getURI());
        }
    }
    private void view() {
        SyndieURI uri = getSelectedURI();
        if (uri != null)
            _navControl.view(uri);
    }
    private void remove() {
        _tree.setRedraw(false);
        TreeItem items[] = _tree.getSelection();
        if ( (items != null) && (items.length == 1) ) {
            remove(items[0]);
        }
        _tree.setRedraw(true);
    }
    private void remove(TreeItem item) {
        ReferenceNode node = (ReferenceNode)_itemToRefNode.remove(item);
        if (node.getParent() != null)
            node.getParent().removeChild(node);
        while (item.getItemCount() > 0)
            remove(item.getItem(0));
        item.dispose();
    }
    private void importBookmarksToForum() {
        // import FROM our bookmarks TO the forum
        _tree.setRedraw(false);
        List includedURIs = new ArrayList();
        for (Iterator iter = _itemToRefNode.values().iterator(); iter.hasNext(); ) {
            ReferenceNode node = (ReferenceNode)iter.next();
            if (!includedURIs.contains(node.getURI()))
                includedURIs.add(node.getURI());
        }
        add(ReferenceNode.deepCopy(_client.getNymReferences(_client.getLoggedInNymId())), false, includedURIs);
        _tree.setRedraw(true);
    }
    private void importBookmarksToLocal(boolean all) {
        // import FROM the forum TO our bookmarks
        if (all) {
            for (int i = 0; i < _refs.size(); i++) {
                boolean last = (i + 1 == _refs.size());
                _bookmarkControl.bookmark(NymReferenceNode.deepCopyNym((ReferenceNode)_refs.get(i)), last);
            }
        } else {
            ReferenceNode node = getSelectedNode();
            if (node != null)
                _bookmarkControl.bookmark(NymReferenceNode.deepCopyNym(node), true);
        }
    }
    
    private SyndieURI getSelectedURI() {
        ReferenceNode node = getSelectedNode();
        if (node != null)
            return node.getURI();
        else
            return null;
    }
    private ReferenceNode getSelectedNode() {
        TreeItem items[] = _tree.getSelection();
        if ( (items != null) && (items.length == 1) ) {
            ReferenceNode node = (ReferenceNode)_itemToRefNode.get(items[0]);
            return node;
        }
        return null;
    }

    private void editName() {
        Control old = _treeEditor.getEditor();
        if (old != null) old.dispose();
        
        TreeItem items[] = _tree.getSelection();
        if ( (items != null) && (items.length == 1) ) {
            final TreeItem item = items[0];
            final ReferenceNode node = (ReferenceNode)_itemToRefNode.get(item);
            
            final Text edit = new Text(_tree, SWT.NONE);
            if (node.getName() != null)
                edit.setText(node.getName());
            else
                edit.setText("");
            
            edit.addModifyListener(new ModifyListener() {
                public void modifyText(ModifyEvent evt) {
                    String txt = edit.getText();
                    item.setText(0, txt);
                    node.setName(txt);
                    setMinWidth(_colName, txt, _colName.getWidth()-item.getBounds(0).width);
                }
            });
            edit.addTraverseListener(new TraverseListener() {
                public void keyTraversed(TraverseEvent evt) {
                    String txt = edit.getText();
                    item.setText(0, edit.getText());
                    node.setName(edit.getText());
                    setMinWidth(_colName, txt, _colName.getWidth()-item.getBounds(0).width);
                    evt.display.asyncExec(new Runnable() { public void run() { _treeEditor.getEditor().dispose(); _tree.setFocus(); } });
                }
            });
            edit.addFocusListener(new FocusListener() {
                public void focusGained(FocusEvent focusEvent) {}
                public void focusLost(FocusEvent evt) {
                    String txt = edit.getText();
                    item.setText(0, edit.getText());
                    node.setName(edit.getText());
                    setMinWidth(_colName, txt, _colName.getWidth()-item.getBounds(0).width);
                    evt.display.asyncExec(new Runnable() { public void run() { _treeEditor.getEditor().dispose(); _tree.setFocus(); } });
                }
            });
            edit.selectAll();
            edit.setFocus();
            _treeEditor.setEditor(edit, item, 0);
        }
    }
    private void editType() {
        Control old = _treeEditor.getEditor();
        if (old != null) old.dispose();
        
        TreeItem items[] = _tree.getSelection();
        if ( (items != null) && (items.length == 1) ) {
            final TreeItem item = items[0];
            final ReferenceNode node = (ReferenceNode)_itemToRefNode.get(item);
            
            final Text edit = new Text(_tree, SWT.NONE);
            if (node.getReferenceType() != null)
                edit.setText(node.getReferenceType());
            else
                edit.setText("");
            
            edit.addModifyListener(new ModifyListener() {
                public void modifyText(ModifyEvent evt) {
                    String txt = edit.getText();
                    item.setText(2, edit.getText());
                    node.setReferenceType(edit.getText());
                    setMinWidth(_colType, txt);
                }
            });
            edit.addTraverseListener(new TraverseListener() {
                public void keyTraversed(TraverseEvent evt) {
                    String txt = edit.getText();
                    item.setText(2, edit.getText());
                    node.setReferenceType(edit.getText());
                    setMinWidth(_colType, txt);
                    evt.display.asyncExec(new Runnable() { public void run() { _treeEditor.getEditor().dispose(); _tree.setFocus();} });
                }
            });
            edit.addFocusListener(new FocusListener() {
                public void focusGained(FocusEvent focusEvent) {}
                public void focusLost(FocusEvent evt) {
                    String txt = edit.getText();
                    item.setText(2, edit.getText());
                    node.setReferenceType(edit.getText());
                    setMinWidth(_colType, txt);
                    evt.display.asyncExec(new Runnable() { public void run() { _treeEditor.getEditor().dispose(); _tree.setFocus();} });
                }
            });
            edit.selectAll();
            edit.setFocus();
            _treeEditor.setEditor(edit, item, 2);
        }
    }
    private void editDescription() {
        Control old = _treeEditor.getEditor();
        if (old != null) old.dispose();
        
        TreeItem items[] = _tree.getSelection();
        if ( (items != null) && (items.length == 1) ) {
            final TreeItem item = items[0];
            final ReferenceNode node = (ReferenceNode)_itemToRefNode.get(item);
            
            final Text edit = new Text(_tree, SWT.NONE);
            if (node.getDescription() != null)
                edit.setText(node.getDescription());
            else
                edit.setText("");
            
            edit.addModifyListener(new ModifyListener() {
                public void modifyText(ModifyEvent evt) {
                    item.setText(3, edit.getText());
                    node.setDescription(edit.getText());
                }
            });
            edit.addTraverseListener(new TraverseListener() {
                public void keyTraversed(TraverseEvent evt) {
                    item.setText(3, edit.getText());
                    node.setDescription(edit.getText());
                    evt.display.asyncExec(new Runnable() { public void run() { _treeEditor.getEditor().dispose(); _tree.setFocus();} });
                }
            });
            edit.addFocusListener(new FocusListener() {
                public void focusGained(FocusEvent focusEvent) {}
                public void focusLost(FocusEvent evt) {
                    item.setText(3, edit.getText());
                    node.setDescription(edit.getText());
                    evt.display.asyncExec(new Runnable() { public void run() { _treeEditor.getEditor().dispose(); _tree.setFocus();} });
                }
            });
            edit.selectAll();
            edit.setFocus();
            _treeEditor.setEditor(edit, item, 3);
        }
    }

    private void configMenu() {
        TreeItem items[] = _tree.getSelection();
        if ( (items != null) && (items.length == 1) ) {
            _view.setEnabled(true);
            _addChild.setEnabled(_editable);
        } else {
            _view.setEnabled(false);
            _edit.setEnabled(false);
            _addChild.setEnabled(_editable);
        }
    }
    
    public String getReferences() { 
        String refs = ReferenceNode.walk(getReferenceNodes());
        //_browser.getUI().debugMessage("refs: \n" + refs + "\n");
        return refs;
    }
    public List getReferenceNodes() { 
        ArrayList rv = new ArrayList();
        int items = _tree.getItemCount();
        for (int i = 0; i < items; i++) {
            TreeItem item = _tree.getItem(i);
            walkChecked(item, rv, new HashSet());
        }
        return rv;
    }
    private void walkChecked(TreeItem item, ArrayList roots, Set checkedNodes) {
        ReferenceNode node = (ReferenceNode)_itemToRefNode.get(item);
        // removes the children only, but doesn't remove the kids' parent refs, so
        // the ancestor checking below will still work
        node.clearChildren();
        if (true) { //item.getChecked()) {
            checkedNodes.add(node);
            // might need to reparent
            boolean checkedAncestor = false;
            ReferenceNode ancestor = node;
            while (!checkedAncestor) {
                ancestor = ancestor.getParent();
                if (ancestor == null) break;
                if (checkedNodes.contains(ancestor))
                    checkedAncestor = true;
            }
            if (checkedAncestor && (ancestor != node))
                ancestor.addChild(node);
            else
                roots.add(node);
        }
        int kids = item.getItemCount();
        for (int i = 0; i < kids; i++)
            walkChecked(item.getItem(i), roots, checkedNodes);
    }
    
    public void setReferences(List refs) {
        _ui.debugMessage("setting refs:\n" + refs);
        _tree.setRedraw(false);
        _tree.removeAll();
        _refs.clear();
        ArrayList includedURIs = new ArrayList();
        add(ReferenceNode.deepCopy(refs), true, includedURIs);
        _tree.setRedraw(true);
    }
    
    private void add(List refRoots, boolean checked, List includedURIs) {
        if (refRoots != null) {
            for (int i = 0; i < refRoots.size(); i++) {
                ReferenceNode node = (ReferenceNode)refRoots.get(i);
                add(node, null, checked, includedURIs);
            }
        }
    }
    
    private void add(ReferenceNode node, TreeItem parent, boolean checked, List includedURIs) {
        TreeItem item = parent;
        
        if ( (node.getURI() != null) && ( (includedURIs == null) || (!includedURIs.contains(node.getURI())) ) ) {
            if (includedURIs != null)
                includedURIs.add(node.getURI());
            
            _ui.debugMessage("adding: " + node);
            if (parent != null)
                item = new TreeItem(parent, SWT.NONE);
            else
                item = new TreeItem(_tree, SWT.NONE);

            if (parent == null)
                _refs.add(node);
            _itemToRefNode.put(item, node);

            update(item, node, checked);
        }
        for (int i = 0; i < node.getChildCount(); i++)
            add(node.getChild(i), item, checked, includedURIs);
    }
    
    private void update(TreeItem item, ReferenceNode node, boolean checked) {
        item.setChecked(checked);
        if (node.getName() != null)
            item.setText(0, node.getName());
        else
            item.setText(0, "");

        item.setImage(1, ImageUtil.getTypeIcon(node.getURI()));

        if (node.getReferenceType() != null)
            item.setText(2, node.getReferenceType());
        else
            item.setText(2, "");

        if (node.getDescription() != null)
            item.setText(3, node.getDescription());
        else
            item.setText(3, "");

        setMinWidth(_colName, item.getText(0), 20); // checkbox
        //setMinWidth(_colType, item.getText(2));
        //setMinWidth(_colDescription, item.getText(3));

        item.setExpanded(true);
        _tree.showItem(item);
    }
    
    private void setMinWidth(TreeColumn col, String text) { setMinWidth(col, text, 0); }
    private void setMinWidth(TreeColumn col, String text, int extraWidth) {
        int width = ImageUtil.getWidth(text, _tree) + _tree.getGridLineWidth()*2 + extraWidth;
        int existing = col.getWidth();
        if (width > existing) {
            _ui.debugMessage("Increasing the width on " + col.getText() + " from " + existing + " to " + width);
            col.setWidth(width);
        } else {
            //_browser.getUI().debugMessage("Keeping the width on " + col.getText() + " at " + existing + " (new width would be " + width + ")");
        }
    }
    
    private void add(SyndieURI uri, ReferenceNode parent) {
        ReferenceNode child = null;
        if (parent == null)
            child = new ReferenceNode(_refs.size() + "", uri, "", "");
        else
            child = parent.addChild(parent.getChildCount() + "", uri, "", "");
        _ui.debugMessage("child added: " + child);
        TreeItem parentItem = null;
        if (parent != null) {
            for (Iterator iter = _itemToRefNode.keySet().iterator(); iter.hasNext(); ) {
                TreeItem item = (TreeItem)iter.next();
                if (parent == _itemToRefNode.get(item)) {
                    parentItem = item;
                    parentItem.setExpanded(true);
                    break;
                }
            }
        }
        add(child, parentItem, true, null);
    }
    
    
    public void translate(TranslationRegistry registry) {
        _colDescription.setText(registry.getText("Description"));
        _colType.setText(registry.getText("Type"));
        _colTarget.setText(registry.getText(""));
        _colName.setText(registry.getText("Name"));
        _add.setText(registry.getText("Add"));
        _addChild.setText(registry.getText("Add child"));
        _edit.setText(registry.getText("Edit"));
        _view.setText(registry.getText("View"));
        _remove.setText(registry.getText("Remove"));
        _importToForum.setText(registry.getText("Add local bookmarks"));
        _importToLocal.setText(registry.getText("Import to local bookmarks"));
        _importAllToLocal.setText(registry.getText("Import all to local bookmarks"));
        
        _colType.pack();
        _colDescription.pack();
        //_colName.setWidth(_tree.getClientArea().width - _colTarget.getWidth() - _colType.getWidth() - _colDescription.getWidth());
        _colName.pack();
    }
    
    public void applyTheme(Theme theme) { _tree.setFont(theme.TREE_FONT); }

    private class EditPopup extends LinkBuilderPopup {
        private ReferenceNode _parentNode;
        private ReferenceNode _currentNode;
        public EditPopup(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl navControl, BanControl banControl, BookmarkControl bookmarkControl) { 
            super(client, ui, themes, trans, navControl, banControl, bookmarkControl, _parent.getShell(), null); //_editor); }
        }
        public void uriBuildingCancelled() {}
        protected void uriBuilt(SyndieURI uri) {
            if (_currentNode == null) {
                add(uri, _parentNode);
            } else {
                _currentNode.setURI(uri);
                for (Iterator iter = _itemToRefNode.keySet().iterator(); iter.hasNext(); ) {
                    TreeItem item = (TreeItem)iter.next();
                    ReferenceNode node = (ReferenceNode)_itemToRefNode.get(item);
                    if (node == _currentNode) {
                        update(item, _currentNode, true);
                        break;
                    }
                }
            }
            _currentNode = null;
            _parentNode = null;
        }
        public void setCurrentNode(ReferenceNode node) { _currentNode = node; }
        public void setParent(ReferenceNode node) { _parentNode = node; }
    }
}
