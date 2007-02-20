package syndie.gui;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import syndie.data.NymReferenceNode;
import syndie.data.SyndieURI;

/**
 *
 */
class BookmarkEditor implements Translatable {
    private BrowserControl _browser;
    private NymReferenceNode _node;
    private Composite _parent;
    private Composite _root;
    
    private Label _nameLabel;
    private Text _name;
    private Label _descriptionLabel;
    private Text _description;
    private Label _parentGroupLabel;
    private Combo _parentGroup;
    private ArrayList _parentNodes;
    private List _roots;
    private Label _siblingOrderLabel;
    private Combo _siblingOrder;
    private Label _uriLabel;
    private Text _uri;
    private Button _uriBrowse;
    private Button _save;
    private Button _cancel;
    
    private LinkBuilderPopup _refPopup;
    private BookmarkEditorListener _lsnr;
    
    private boolean _pickParent;
    private boolean _pickOrder;
    private boolean _pickTarget;
    
    public BookmarkEditor(BrowserControl control, Composite parent, BookmarkEditorListener lsnr) {
        _browser = control;
        _parent = parent;
        _lsnr = lsnr;
        _pickParent = true;
        _pickOrder = true;
        _pickTarget = true;
        initComponents();
    }
    
    public void pickParent(boolean pick) { 
        _pickParent = pick;
        ((GridData)_parentGroup.getLayoutData()).exclude = !pick;
        ((GridData)_parentGroupLabel.getLayoutData()).exclude = !pick;
        _parentGroup.setVisible(pick);
        _parentGroupLabel.setVisible(pick);
    }
    public void pickOrder(boolean pick) { 
        _pickOrder = pick;
        ((GridData)_siblingOrder.getLayoutData()).exclude = !pick;
        ((GridData)_siblingOrderLabel.getLayoutData()).exclude = !pick;
        _siblingOrder.setVisible(pick);
        _siblingOrderLabel.setVisible(pick);
    }
    public void pickTarget(boolean pick) { 
        _pickTarget = pick;
        ((GridData)_uri.getLayoutData()).exclude = !pick;
        ((GridData)_uriLabel.getLayoutData()).exclude = !pick;
        ((GridData)_uriBrowse.getLayoutData()).exclude = !pick;
        _uri.setVisible(pick);
        _uriLabel.setVisible(pick);
        _uriBrowse.setVisible(pick);
    }
    
    public interface BookmarkEditorListener {
        public void updateBookmark(BookmarkEditor editor, NymReferenceNode bookmark, boolean delete);
        public void cancelEditor(BookmarkEditor editor);
    }
    
    public void setBookmark(NymReferenceNode node) {
        _node = node;
        updateUI();
    }
    
    public void dispose() {
        if (_refPopup != null)
            _refPopup.dispose();
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(3, false));
        
        _nameLabel = new Label(_root, SWT.NONE);
        _nameLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _name = new Text(_root, SWT.SINGLE | SWT.BORDER);
        _name.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _descriptionLabel = new Label(_root, SWT.NONE);
        _descriptionLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _description = new Text(_root, SWT.MULTI | SWT.BORDER | SWT.WRAP);
        _description.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
        
        _parentGroupLabel = new Label(_root, SWT.NONE);
        _parentGroupLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _parentGroup = new Combo(_root, SWT.DROP_DOWN | SWT.READ_ONLY);
        _parentGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        _parentNodes = new ArrayList();
        _parentGroup.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { updateSiblingOrder(); }
            public void widgetSelected(SelectionEvent selectionEvent) { updateSiblingOrder(); }
        });
        
        _siblingOrderLabel = new Label(_root, SWT.NONE);
        _siblingOrderLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _siblingOrder = new Combo(_root, SWT.DROP_DOWN | SWT.READ_ONLY);
        _siblingOrder.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _uriLabel = new Label(_root, SWT.NONE);
        _uriLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _uri = new Text(_root, SWT.SINGLE | SWT.BORDER);
        _uri.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _uriBrowse = new Button(_root, SWT.PUSH);
        _uriBrowse.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _uriBrowse.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { browse(); }
            public void widgetSelected(SelectionEvent selectionEvent) { browse(); }
        });
        
        Composite actions = new Composite(_root, SWT.NONE);
        actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        actions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        _save = new Button(actions, SWT.PUSH);
        _cancel = new Button(actions, SWT.PUSH);
        
        _save.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { 
                _lsnr.updateBookmark(BookmarkEditor.this, getState(), false);
            }
            public void widgetSelected(SelectionEvent selectionEvent) { 
                _lsnr.updateBookmark(BookmarkEditor.this, getState(), false);
            }
        });
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                _lsnr.cancelEditor(BookmarkEditor.this);
            }
            public void widgetSelected(SelectionEvent selectionEvent) {
                _lsnr.cancelEditor(BookmarkEditor.this);
            }
        });
        
        _browser.getTranslationRegistry().register(this);
    }
    
    private void browse() {
        if (_refPopup == null) {
            _refPopup = new LinkBuilderPopup(_browser, _root.getShell(), new LinkBuilderPopup.LinkBuilderSource() {
                public void uriBuilt(SyndieURI uri, String text) {
                    if (uri != null) {
                        if ( (text != null) && (text.trim().length() > 0) )
                            _name.setText(text.trim());
                        _uri.setText(uri.toString());
                    }
                }
                public int getPageCount() { return 0; }
                public List getAttachmentDescriptions() { return Collections.EMPTY_LIST; }
            });
        }
        String name = _name.getText().trim();
        SyndieURI uri = getURI();
        if (uri != null)
            _refPopup.showPopup(uri, name);
        else
            _refPopup.showPopup();
    }
    
    private SyndieURI getURI() {
        if (!_pickTarget) {
            if (_node != null)
                return _node.getURI();
            else
                return null;
        } else {
            String uriStr = _uri.getText().trim();
            if (uriStr.length() > 0) {
                try {
                    return new SyndieURI(uriStr);
                } catch (URISyntaxException use) {}
            }
            return null;
        }
    }
    
    private NymReferenceNode getState() {
        String name = null;
        SyndieURI uri = null;
        String desc = null;
        long uriId = -1;
        long groupId = -1;
        long parentGroupId = -1;
        int order = 0;
        boolean ignored = false;
        boolean banned = false;
        
        _browser.getUI().debugMessage("getState for node " + _node);
        
        name = _name.getText().trim();
        desc = _description.getText().trim();
        
        if (_pickTarget) {
            String uriStr = _uri.getText().trim();
            if (_node != null) {
                SyndieURI orig = _node.getURI();
                SyndieURI newURI = null;
                if (uriStr.length() > 0) {
                    try {
                        newURI = new SyndieURI(uriStr);
                    } catch (URISyntaxException use) {
                        _browser.getUI().errorMessage("Invalid URI [" + _uri.getText() + "]", use);
                        newURI = null;
                    }
                }
                if ( (orig != null) && (orig.equals(newURI)) ) {
                    uriId = _node.getURIId();
                } else {
                    uriId = -1;
                }
                uri = newURI;
            } else {
                uriId = -1;
                if (uriStr.length() > 0) {
                    try {
                        uri = new SyndieURI(uriStr);
                    } catch (URISyntaxException use) {
                        _browser.getUI().errorMessage("Invalid URI [" + _uri.getText() + "]", use);
                        uri = null;
                    }
                }
            }
        } else if (_node != null) {
            uri = _node.getURI();
            uriId = _node.getURIId();
        }
        if (_node != null)
            groupId = _node.getGroupId();
        if (_pickParent) {
            int idx = _parentGroup.getSelectionIndex() - 1;
            if (idx >= 0) {
                NymReferenceNode parent = (NymReferenceNode)_parentNodes.get(idx);
                parentGroupId = parent.getGroupId();
            } else {
                parentGroupId = -1;
            }
        } else if (_node != null) {
            parentGroupId = _node.getParentGroupId();
        } else{
            parentGroupId = -1;
        }
        if (_pickOrder)
            order = _siblingOrder.getSelectionIndex();
        else
            order = _node.getSiblingOrder();
        
        _browser.getUI().debugMessage("uri: " + uri + " uriId: " + uriId + " groupId: " + groupId + " order: " + order);
        
        NymReferenceNode rv = _node;
        if (rv == null) {
            rv = new NymReferenceNode(name, uri, desc, uriId, groupId, parentGroupId, order, ignored, banned, false);
        } else {
            rv.setName(name);
            rv.setDescription(desc);
            rv.setURI(uri);
            rv.setURIId(uriId);
            rv.setParentGroupId(parentGroupId);
            rv.setSiblingOrder(order);
            rv.setIsIgnored(ignored);
            rv.setIsBanned(banned);
            rv.setLoadOnStart(false);
        }
        return rv;
    }
    
    /** _node to ui elements */
    private void updateUI() {
        if (_node == null) {
            _name.setText("");
            _description.setText("");

            populateParent(-1);
            updateSiblingOrder();
            
            _uri.setText("");
        } else {
            if (_node.getName() != null)
                _name.setText(_node.getName());
            else
                _name.setText("");
            if (_node.getDescription() != null)
                _description.setText(_node.getDescription());
            else
                _description.setText("");

            populateParent(-1);            
            updateSiblingOrder();
            
            if (_node.getURI() != null)
                _uri.setText(_node.getURI().toString());
            else
                _uri.setText("");
        }
    }
    
    private void updateSiblingOrder() {
        if (!_pickOrder) return;
        // populate the _siblingOrder w/ the children of the currently selected _parentGroup
        _siblingOrder.setRedraw(false);
        _siblingOrder.removeAll();
        
        int order = 0;
        if (_node != null)
            order = _node.getSiblingOrder();
        
        _siblingOrder.add(_browser.getTranslationRegistry().getText(T_SIBLINGORDER_BEGINNING, "At the beginning"));
        if (order == 0)
            _siblingOrder.select(0);
        
        NymReferenceNode parent = getParent();
        if (parent != null) {
            for (int i = 0; i < parent.getChildCount(); i++) {
                NymReferenceNode child = (NymReferenceNode)parent.getChild(i);
                if ( (_node != null) && (child.getGroupId() == _node.getGroupId()) ) {
                    _siblingOrder.add(_browser.getTranslationRegistry().getText(T_SIBLINGORDER_SAME, "Same as before"));
                    _siblingOrder.select(_siblingOrder.getItemCount()-1);
                } else {
                    _siblingOrder.add(_browser.getTranslationRegistry().getText(T_SIBLINGORDER_SAME, "After: ") + getElementName(child));
                }
            }
        } else {
            for (int i = 0; i < _roots.size(); i++) {
                NymReferenceNode child = (NymReferenceNode)_roots.get(i);
                if ( (_node != null) && (child.getGroupId() == _node.getGroupId()) ) {
                    _siblingOrder.add(_browser.getTranslationRegistry().getText(T_SIBLINGORDER_SAME, "Same as before"));
                    _siblingOrder.select(_siblingOrder.getItemCount()-1);
                } else {
                    _siblingOrder.add(_browser.getTranslationRegistry().getText(T_SIBLINGORDER_SAME, "After: ") + getElementName(child));
                }
            }
        }
        
        _siblingOrder.setRedraw(true);
    }
    
    private NymReferenceNode getParent() {
        int index = _parentGroup.getSelectionIndex();
        index--; // _parentNodes does not contain the root
        if (index < 0) {
            return null;
        } else if (index < _parentNodes.size()) {
            return (NymReferenceNode)_parentNodes.get(index);
        } else { // wtf
            return null;
        }
    }
    
    private void populateParent(long parentId) {
        _parentGroup.setRedraw(false);
        _parentGroup.removeAll();
        _parentNodes.clear();
        _parentGroup.add(_browser.getTranslationRegistry().getText(T_TOPLEVELBOOKMARK, "Top level bookmark"));
        _browser.getUI().debugMessage("bookmarkEditor: populating parent(" + parentId + ")");
        if (parentId == -1)
            _parentGroup.select(0);
        
        _roots = _browser.getClient().getNymReferences(_browser.getClient().getLoggedInNymId());
        for (int i = 0; i < _roots.size(); i++) {
            NymReferenceNode node = (NymReferenceNode)_roots.get(i);
            addParentElement(node, "", parentId);
        }
        _parentGroup.setRedraw(true);
    }
    
    private void addParentElement(NymReferenceNode cur, String parentString, long targetParentId) {
        if (cur == null) return;
        if ( (_node != null) && (cur.getGroupId() == _node.getGroupId()) ) return;
        String name = parentString + getElementName(cur);
        _parentGroup.add(name);
        _parentNodes.add(cur);
        if (cur.getGroupId() == targetParentId) {
            _parentGroup.select(_parentGroup.getItemCount()-1);
            _browser.getUI().debugMessage("bookmarkEditor: found parent: " + cur.getName());
        }
        
        String childPrefix = name + " > ";
        for (int i = 0; i < cur.getChildCount(); i++) {
            NymReferenceNode node = (NymReferenceNode)cur.getChild(i);
            addParentElement(node, childPrefix, targetParentId);
        }
    }
    
    private String getElementName(NymReferenceNode cur) {
        if (cur.getName() != null)
            return cur.getName();
        else if ( (cur.getURI() != null) && (cur.getURI().getScope() != null) )
            return cur.getURI().getScope().toBase64().substring(0,6);
        else
            return _browser.getTranslationRegistry().getText(T_UNNAMED, "unnamed");
    }
    
    private static final String T_NAME = "syndie.gui.bookmarkeditor.name";
    private static final String T_DESC = "syndie.gui.bookmarkeditor.desc";
    private static final String T_PARENT = "syndie.gui.bookmarkeditor.parent";
    private static final String T_SIBLINGORDER = "syndie.gui.bookmarkeditor.siblingorder";
    private static final String T_URILABEL = "syndie.gui.bookmarkeditor.urilabel";
    private static final String T_URIBROWSE = "syndie.gui.bookmarkeditor.uribrowse";
    private static final String T_OK = "syndie.gui.bookmarkeditor.ok";
    private static final String T_CANCEL = "syndie.gui.bookmarkeditor.cancel";
    private static final String T_SIBLINGORDER_BEGINNING = "syndie.gui.bookmarkeditor.siblingorder.beginning";
    private static final String T_SIBLINGORDER_SAME = "syndie.gui.bookmarkeditor.siblingorder.same";
    private static final String T_SIBLINGORDER_AFTER = "syndie.gui.bookmarkeditor.siblingorder.after";
    private static final String T_TOPLEVELBOOKMARK = "syndie.gui.bookmarkeditor.toplevelbookmark";
    private static final String T_UNNAMED = "syndie.gui.bookmarkeditor.unnamed";
    
    public void translate(TranslationRegistry registry) {
        _nameLabel.setText(registry.getText(T_NAME, "Name:"));
        _descriptionLabel.setText(registry.getText(T_DESC, "Description:"));
        _parentGroupLabel.setText(registry.getText(T_PARENT, "Parent:"));
        _siblingOrderLabel.setText(registry.getText(T_SIBLINGORDER, "After:"));
        _uriLabel.setText(registry.getText(T_URILABEL, "Target:"));
        _uriBrowse.setText(registry.getText(T_URIBROWSE, "Browse..."));
        _save.setText(registry.getText(T_OK, "OK"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel"));
        updateUI();
    }
}
