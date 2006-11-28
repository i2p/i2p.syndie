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
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.MenuListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.ArchiveInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;

/**
 *
 */
public class ManageForumReferenceChooser implements Translatable {
    private BrowserControl _browser;
    private ManageForum _forum;
    private Composite _parent;
    private Tree _tree;
    private TreeColumn _colName;
    private TreeColumn _colTarget;
    private TreeColumn _colType;
    private TreeColumn _colDescription;
    private MenuItem _add;
    private MenuItem _addChild;
    private MenuItem _edit;
    private MenuItem _view;
    private ArrayList _refs;
    private EditPopup _editPopup;
    
    /** TreeItem to ReferenceNode */
    private Map _itemToRefNode;
    
    public ManageForumReferenceChooser(Composite parent, BrowserControl browser, ManageForum forum) {
        _browser = browser;
        _forum = forum;
        _parent = parent;
        _refs = new ArrayList();
        _itemToRefNode = new HashMap();
        initComponents();
    }
    
    private void initComponents() {
        _tree = new Tree(_parent, SWT.SINGLE | SWT.CHECK | SWT.BORDER);
        _colName = new TreeColumn(_tree, SWT.LEFT);
        _colTarget = new TreeColumn(_tree, SWT.CENTER);
        _colType = new TreeColumn(_tree, SWT.LEFT);
        _colDescription = new TreeColumn(_tree, SWT.LEFT);
        _tree.setLinesVisible(true);
        _tree.setHeaderVisible(true);
        
        _colTarget.setWidth(20);
        
        Menu menu = new Menu(_tree);
        _tree.setMenu(menu);
        _add = new MenuItem(menu, SWT.PUSH);
        _add.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { add(); }
            public void widgetSelected(SelectionEvent selectionEvent) { add(); }
        });
        _addChild = new MenuItem(menu, SWT.PUSH);
        _addChild.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addChild(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addChild(); }
        });
        _edit = new MenuItem(menu, SWT.PUSH);
        _edit.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { edit(); }
            public void widgetSelected(SelectionEvent selectionEvent) { edit(); }
        });
        _view = new MenuItem(menu, SWT.PUSH);
        _view.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(); }
        });
        menu.addMenuListener(new MenuListener() {
            public void menuHidden(MenuEvent menuEvent) {}
            public void menuShown(MenuEvent evt) { configMenu(); }
        });

        _editPopup = new EditPopup();
        
        _browser.getTranslationRegistry().register(this);
    }
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
        _editPopup.dispose();
    }
    private void add() {
        _editPopup.setParent(null);
        _editPopup.setCurrentNode(null);
        _editPopup.showPopup();
    }
    private void addChild() {
        _editPopup.setParent(getSelectedNode());
        _editPopup.setCurrentNode(null);
        _editPopup.showPopup();
    }
    private void edit() {
        ReferenceNode node = getSelectedNode();
        if (node != null) {
            _editPopup.setCurrentNode(node);
            _editPopup.showPopup(node.getURI());
        }
    }
    private void view() {
        SyndieURI uri = getSelectedURI();
        if (uri != null)
            _browser.view(uri);
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
    
    private void configMenu() {
        TreeItem items[] = _tree.getSelection();
        if ( (items != null) && (items.length == 1) ) {
            if (items[0].getItemCount() > 0)
                _view.setEnabled(false);
            else
                _view.setEnabled(true);
            _addChild.setEnabled(true);
        } else {
            _view.setEnabled(false);
            _edit.setEnabled(false);
            _addChild.setEnabled(false);
        }
    }
    
    public String getReferences() { 
        String refs = ReferenceNode.walk(getChecked());
        _browser.getUI().debugMessage("refs: \n" + refs + "\n");
        return refs;
    }
    
    private ArrayList getChecked() { 
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
        if (item.getChecked()) {
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
        _browser.getUI().debugMessage("setting refs:\n\n\n" + refs + "\n\n\n");
        _tree.setRedraw(false);
        _refs.clear();
        ArrayList includedURIs = new ArrayList();
        add(ReferenceNode.deepCopy(refs), true, includedURIs);
        add(ReferenceNode.deepCopy(_browser.getClient().getNymReferences(_browser.getClient().getLoggedInNymId())), false, includedURIs);
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
            
            _browser.getUI().debugMessage("adding: " + node);
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

        int type = getType(node.getURI());
        if (type == TYPE_MSG) item.setImage(1, ImageUtil.ICON_REF_MSG);
        else if (type == TYPE_FORUM) item.setImage(1, ImageUtil.ICON_REF_FORUM);
        else if (type == TYPE_ARCHIVE) item.setImage(1, ImageUtil.ICON_REF_ARCHIVE);
        else if (type == TYPE_URL) item.setImage(1, ImageUtil.ICON_REF_URL);
        else if (type == TYPE_SYNDIE) item.setImage(1, ImageUtil.ICON_REF_SYNDIE);
        else if (type == TYPE_FREENET) item.setImage(1, ImageUtil.ICON_REF_FREENET);
        else item.setImage(1, null);

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
    }
    
    private void setMinWidth(TreeColumn col, String text) { setMinWidth(col, text, 0); }
    private void setMinWidth(TreeColumn col, String text, int extraWidth) {
        int width = ImageUtil.getWidth(text, _tree) + _tree.getGridLineWidth()*2 + extraWidth;
        int existing = col.getWidth();
        if (width > existing) {
            _browser.getUI().debugMessage("Increasing the width on " + col.getText() + " from " + existing + " to " + width);
            col.setWidth(width);
        } else {
            _browser.getUI().debugMessage("Keeping the width on " + col.getText() + " at " + existing + " (new width would be " + width + ")");
        }
    }
    
    private void add(SyndieURI uri, ReferenceNode parent) {
        ReferenceNode child = null;
        if (parent == null)
            child = new ReferenceNode(_refs.size() + "", uri, "", "");
        else
            child = parent.addChild(parent.getChildCount() + "", uri, "", "");
        _browser.getUI().debugMessage("child added: " + child);
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
    
    private static final int TYPE_MSG = 0;
    private static final int TYPE_FORUM = 1;
    private static final int TYPE_ARCHIVE = 2;
    private static final int TYPE_URL = 3;
    private static final int TYPE_SYNDIE = 4;
    private static final int TYPE_FREENET = 5;
    private static final int TYPE_OTHER = -1;
    
    private int getType(SyndieURI uri) {
        if (uri == null) {
            return TYPE_OTHER;
        } else if (uri.isChannel()) {
            if (uri.getScope() != null) {
                if (uri.getMessageId() != null) 
                    return TYPE_MSG;
                else
                    return TYPE_FORUM;
            } else {
                return TYPE_SYNDIE;
            }
        } else if (uri.isArchive()) {
            return TYPE_ARCHIVE;
        } else if (uri.isURL()) {
            String url = uri.getURL();
            if ( (url != null) && (url.startsWith("SSK@") || url.startsWith("CHK@") || url.startsWith("USK@")) )
                return TYPE_FREENET;
            else
                return TYPE_URL;
        } else if (uri.isSearch()) {
            Hash scope = uri.getHash("scope");
            if (scope == null)
                return TYPE_SYNDIE;
            else if (uri.getMessageId() == null)
                return TYPE_FORUM;
            else
                return TYPE_MSG;
        } else {
            return TYPE_SYNDIE;
        }
    }
    
    private static final String T_COL_DESC = "syndie.gui.manageforumreferencechooser.desc";
    private static final String T_COL_TYPE = "syndie.gui.manageforumreferencechooser.type";
    private static final String T_COL_TARGET = "syndie.gui.manageforumreferencechooser.target";
    private static final String T_COL_NAME = "syndie.gui.manageforumreferencechooser.name";
    private static final String T_ADD = "syndie.gui.manageforumreferencechooser.add";
    private static final String T_ADDCHILD = "syndie.gui.manageforumreferencechooser.add.child";
    private static final String T_EDIT = "syndie.gui.manageforumreferencechooser.edit";
    private static final String T_VIEW = "syndie.gui.manageforumreferencechooser.view";
    
    public void translate(TranslationRegistry registry) {
        _colDescription.setText(registry.getText(T_COL_DESC, "Description"));
        _colType.setText(registry.getText(T_COL_TYPE, "Type"));
        _colTarget.setText(registry.getText(T_COL_TARGET, ""));
        _colName.setText(registry.getText(T_COL_NAME, "Name"));
        _add.setText(registry.getText(T_ADD, "Add"));
        _addChild.setText(registry.getText(T_ADDCHILD, "Add child"));
        _edit.setText(registry.getText(T_EDIT, "Edit"));
        _view.setText(registry.getText(T_VIEW, "View"));
        
        _colType.pack();
        _colDescription.pack();
        //_colName.setWidth(_tree.getClientArea().width - _colTarget.getWidth() - _colType.getWidth() - _colDescription.getWidth());
        _colName.pack();
    }
    
    private class EditPopup extends LinkBuilderPopup {
        private ReferenceNode _parentNode;
        private ReferenceNode _currentNode;
        public EditPopup() { super(_browser, _parent.getShell(), null); }
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
