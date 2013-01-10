package syndie.gui;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import syndie.Constants;
import syndie.data.NymReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.CommandImpl;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.UI;

/**
 *
 */
public class ReferenceEditor extends BaseComponent implements Themeable, Translatable {
    private BookmarkControl _bookmarkControl;
    private NavigationControl _navControl;
    private BanControl _banControl;
    private Composite _parent;
    private Composite _root;
    private Label _locationLabel;
    private Text _location;
    private Button _locationSelect;
    private ImageCanvas _icon;
    private Label _summary;
    private Button _watched;
    private Button _refs;
    private Tree _refCategory;
    private Label _refNewCatLabel;
    private Text _refNewCat;
    private Button _refNewCatCreate;
    private Button _customName;
    private Text _customNameText;
    private Button _customDesc;
    private Text _customDescText;
    private Button _customIcon;
    private Button _customIconBrowse;
    private String _customIconFile;
    private Button _ok;
    private Button _cancel;
    
    private FileDialog _customIconDialog;
    
    private NymReferenceNode _origRef;
    
    private ReferenceEditorListener _lsnr;
    
    public interface ReferenceEditorListener {
        public void cancelled(ReferenceEditor editor);
        public void saved(ReferenceEditor editor);
    }
    
    public ReferenceEditor(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite parent, NavigationControl navControl, BanControl banControl, BookmarkControl bookmarkControl, ReferenceEditor.ReferenceEditorListener lsnr) {
        super(client, ui, themes, trans);
        _parent = parent;
        _navControl = navControl;
        _banControl = banControl;
        _bookmarkControl = bookmarkControl;
        _lsnr = lsnr;
        initComponents();
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        _root.setLayout(gl);
        
        // summary row
        Composite row = new Composite(_root, SWT.NONE);
        gl = new GridLayout(2, false);
        //gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        row.setLayout(gl);
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
    
        _icon = new ImageCanvas(row, false, true, false);
        _icon.forceSize(64, 64);
        _icon.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _summary = new Label(row, SWT.NONE);
        _summary.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));
        
        // location row
        row = new Composite(_root, SWT.NONE);
        gl = new GridLayout(3, false);
        //gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        row.setLayout(gl);
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
    
        _locationLabel = new Label(row, SWT.NONE);
        _locationLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _location = new Text(row, SWT.SINGLE | SWT.BORDER);
        _location.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _locationSelect = new Button(row, SWT.PUSH);
        _locationSelect.setLayoutData(new GridData(GridData.END, GridData.FILL, false, false));
        _locationSelect.addSelectionListener(new FireSelectionListener() {
            public void fire() { selectURI(); }
        });
        
        // watched row
        _watched = new Button(_root, SWT.CHECK);
        _watched.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        // refs row
        _refs = new Button(_root, SWT.CHECK);
        _refs.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        // refs tree
        _refCategory = new Tree(_root, SWT.SINGLE | SWT.BORDER);
        _refCategory.setHeaderVisible(false);
        _refCategory.setLinesVisible(true);
        _refCategory.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        SyndieTreeListener lsnr = new SyndieTreeListener(_refCategory) {
            /** the tree's selection was updated */
            public void selected() { _refs.setSelection(true); }
        };
        _refCategory.addSelectionListener(lsnr);
        _refCategory.addKeyListener(lsnr);
        _refCategory.addMouseListener(lsnr);
        _refCategory.addTraverseListener(lsnr);
        
        // new cat row
        row = new Composite(_root, SWT.NONE);
        gl = new GridLayout(3, false);
        //gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        row.setLayout(gl);
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
    
        _refNewCatLabel = new Label(row, SWT.NONE);
        _refNewCatLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, true, false));
        _refNewCat = new Text(row, SWT.SINGLE | SWT.BORDER);
        _refNewCat.addFocusListener(new FocusListener() {
            public void focusGained(FocusEvent focusEvent) {}
            public void focusLost(FocusEvent focusEvent) {
                String val = _refNewCat.getText();
                _refNewCatCreate.setEnabled(val.trim().length() > 0);
            }
        });
        _refNewCat.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent modifyEvent) {
                String val = _refNewCat.getText();
                _refNewCatCreate.setEnabled(val.trim().length() > 0);
            }
        });
        GridData gd = new GridData(GridData.FILL, GridData.FILL, false, true);
        gd.widthHint = 100;
        _refNewCat.setLayoutData(gd);
        _refNewCatCreate = new Button(row, SWT.PUSH);
        _refNewCatCreate.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _refNewCatCreate.addSelectionListener(new FireSelectionListener() { public void fire() { createCategory(); } });
        
        // custom name row
        row = new Composite(_root, SWT.NONE);
        gl = new GridLayout(2, false);
        //gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        row.setLayout(gl);
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
    
        _customName = new Button(row, SWT.CHECK);
        _customName.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        _customNameText = new Text(row, SWT.SINGLE | SWT.BORDER);
        _customNameText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _customNameText.addFocusListener(new FocusListener() {
            public void focusGained(FocusEvent focusEvent) {}
            public void focusLost(FocusEvent focusEvent) {
                _customName.setSelection(_customNameText.getText().trim().length() > 0);
            }
        });
        _customNameText.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent modifyEvent) {
                _customName.setSelection(_customNameText.getText().trim().length() > 0);
            }
        });
        
        // custom desc row
        row = new Composite(_root, SWT.NONE);
        gl = new GridLayout(2, false);
        //gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        row.setLayout(gl);
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
    
        _customDesc = new Button(row, SWT.CHECK);
        _customDesc.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        _customDescText = new Text(row, SWT.SINGLE | SWT.BORDER);
        _customDescText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _customDescText.addFocusListener(new FocusListener() {
            public void focusGained(FocusEvent focusEvent) {}
            public void focusLost(FocusEvent focusEvent) {
                _customDesc.setSelection(_customDescText.getText().trim().length() > 0);
            }
        });
        _customDescText.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent modifyEvent) {
                _customDesc.setSelection(_customDescText.getText().trim().length() > 0);
            }
        });
        
        // custom icon row
        row = new Composite(_root, SWT.NONE);
        gl = new GridLayout(2, false);
        //gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        row.setLayout(gl);
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
    
        _customIcon = new Button(row, SWT.CHECK);
        _customIcon.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));
        _customIconBrowse = new Button(row, SWT.PUSH);
        _customIconBrowse.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _customIconBrowse.addSelectionListener(new FireSelectionListener() {
            public void fire() { pickCustomIcon(); }
        });
        
        // action row
        row = new Composite(_root, SWT.NONE);
        gl = new GridLayout(2, true);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        row.setLayout(gl);
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
    
        _ok = new Button(row, SWT.PUSH);
        _ok.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _ok.addSelectionListener(new FireSelectionListener() { public void fire() { ok(); } });
        _cancel = new Button(row, SWT.PUSH);
        _cancel.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _cancel.addSelectionListener(new FireSelectionListener() { public void fire() { cancel(); } });
        
        loadCategories();
        uriSelected(null);
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    public Control getControl() { return _root; }
    
    public void setReference(NymReferenceNode node) {
        _origRef = node;
        if (node != null) {
            SyndieURI uri = node.getURI();
            uriSelected(uri);
            if (node.getName() != null) {
                _customNameText.setText(node.getName());
            } else {
                _customNameText.setText("");
            }
            if (node.getDescription() != null) {
                _customDescText.setText(node.getDescription());
            } else {
                _customDescText.setText("");
            }
            
            if ( (uri != null) && (uri.isChannel()) && (uri.getScope() != null) && (uri.getMessageId() == null) ) {
                long chanId = _client.getChannelId(uri.getScope());
                boolean isPetName = _client.getNymChannelPetNameDefined(chanId);
                boolean isPetDesc = _client.getNymChannelPetDescriptionDefined(chanId);
                boolean avatarDefined = _client.getNymChannelAvatarDefined(chanId);
                boolean watched = _client.isWatched(chanId);
                
                _customName.setSelection(isPetName);
                _customDesc.setSelection(isPetDesc);
                _customIcon.setSelection(avatarDefined);
                _watched.setSelection(watched);
            }
            
            
            long parentId = node.getParentGroupId();
            TreeItem parentItem = getItemByGroupId(parentId);
            _ui.debugMessage("editing existing w/ parentId = " + parentId + " parent found? " + parentItem);
            _refCategory.setSelection(parentItem != null ? new TreeItem[] { parentItem } : new TreeItem[0]);
        } else {
            uriSelected(null);
        }
    }
    private TreeItem getItemByGroupId(long groupId) {
        TreeItem items[] = _refCategory.getItems();
        for (int i = 0; i < items.length; i++) {
            TreeItem item = getItemByGroupId(groupId, items[i]);
            if (item != null)
                return item;
        }
        return null;
    }
    private TreeItem getItemByGroupId(long groupId, TreeItem item) {
        NymReferenceNode node = (NymReferenceNode)item.getData("node");
        if ( (node == null) && (groupId < 0) ) {
            return item;
        } else if ( (node != null) && (node.getGroupId() == groupId) ) {
            return item;
        } else {
            TreeItem items[] = item.getItems();
            for (int i = 0; i < items.length; i++) {
                TreeItem cur = getItemByGroupId(groupId, items[i]);
                if (cur != null)
                    return cur;
            }
        }
        return null;
    }
    
    private void ok() {
        SyndieURI uri = null;
        String customName = null;
        String customDesc = null;
        byte customIcon[] = null;
        NymReferenceNode parent = null;
        long parentCatId = -1;
        boolean addRef = false;
        boolean addWatch = false;
        
        String loc = _location.getText();
        if (loc != null) {
            try {
                uri = new SyndieURI(loc);
            } catch (URISyntaxException use) {
                _ui.errorMessage("Invalid uri: " + loc, use);
            }
        }
        if (_customName.isEnabled() && _customName.getSelection()) {
            String val = _customNameText.getText();
            customName = val.trim();
        }
        if (_customDesc.isEnabled() && _customDesc.getSelection()) {
            String val = _customDescText.getText();
            customDesc = val.trim();
        }
        if (_refs.getEnabled() && _refs.getSelection()) {
            addRef = true;
            TreeItem parents[] = _refCategory.getSelection();
            if ( (parents != null) && (parents.length == 1) ) {
                parent = (NymReferenceNode)parents[0].getData("node");
                if (parent != null)
                    parentCatId = parent.getGroupId();
            }
        }
        if (_watched.getEnabled() && _watched.getSelection())
            addWatch = true;
        if (_customIcon.isEnabled() && _customIcon.getSelection() && _customIconFile != null) {
            customIcon = CommandImpl.read(_ui, _customIconFile, Constants.MAX_AVATAR_SIZE);
        }
        
        _ui.debugMessage("refEditor ok: watch? " + addWatch + " ref? " + addRef + " uri: " + uri + " parentCatId: " + parentCatId);
        
        if (uri != null) {
            if (uri.getScope() != null) {
                if (addWatch) {
                    _client.watchChannel(uri.getScope(), true, true, false, false, false);
                    long chanId = _client.getChannelId(uri.getScope());
                    if (customIcon != null)
                        _client.setNymChannelAvatar(chanId, customIcon);
                    // 0 length name clears the custom name
                    if ( (customName != null) || (customDesc != null) )
                        _client.setNymChannelPetName(chanId, customName, customDesc);
                } else {
                    _client.unban(uri.getScope());
                }
            }
            if (addRef) {
                String name = customName;
                String desc = customDesc;
                if ( (name == null) || (name.trim().length() <= 0) )
                    name = _summary.getText();
                if ( (desc == null) || (desc.trim().length() <= 0) )
                    desc = "";
                NymReferenceNode bookmark = null;
                if (_origRef == null) {
                    bookmark = new NymReferenceNode(name, uri, desc, -1, -1, parentCatId, -1, false, false, false);
                } else {
                    bookmark = _origRef;
                    bookmark.setName(name);
                    bookmark.setURI(uri);
                    bookmark.setDescription(desc);
                    bookmark.setParentGroupId(parentCatId);
                }
                if (bookmark.getGroupId() == -1) {
                    _bookmarkControl.bookmark(bookmark, true);
                    if (parent != null)
                        parent.addChild(bookmark);
                } else {
                    boolean sameCat = false;
                    if (bookmark.getParent() != null) {
                        if ( (parent != null) && (bookmark.getParentGroupId() == parent.getGroupId()) ) 
                            sameCat = true;
                    }
                    if (!sameCat && (parent != null))
                        parent.addChild(bookmark);
                    _bookmarkControl.updateBookmark(bookmark);
                }
                
                if (customIcon != null) {
                    long groupId = bookmark.getGroupId();
                    _client.setNymReferenceIcon(groupId, customIcon);
                }
            }
        }
        _lsnr.saved(this);
    }
    private void cancel() {
        _lsnr.cancelled(this);
    }
    
    private void loadCategories() {
        List roots = _client.getNymReferences();
        _refCategory.setRedraw(false);
        _refCategory.removeAll();
        
        
        TreeItem root = null;
        root = new TreeItem(_refCategory, SWT.NONE);
        root.setText(_translationRegistry.getText("Top level"));
        
        for (int i = 0; i < roots.size(); i++) {
            NymReferenceNode node = (NymReferenceNode)roots.get(i);
            addCategory(node, root);
        }
        root.setExpanded(true);
        _refCategory.setSelection(new TreeItem[] { root });
        _refCategory.setRedraw(true);
    }
    
    private void addCategory(NymReferenceNode node, TreeItem parent) {
        if ( (node.getURI() != null) && (node.getChildCount() <= 0) )
            return;
        
        TreeItem item = new TreeItem(parent, SWT.NONE);
        
        String name = node.getName();
        if ( (name == null) || (name.trim().length() <= 0) )
            name = node.getDescription();
        if ( (name == null) || (name.trim().length() <= 0) )
            name = "?";
        item.setText(name);
        item.setData("node", node);
        
        for (int i = 0; i < node.getChildCount(); i++)
            addCategory((NymReferenceNode)node.getChild(i), item);
        item.setExpanded(true);
    }
    
    private void pickCustomIcon() {
        if (_customIconDialog == null) {
            _customIconDialog = new FileDialog(_root.getShell(), SWT.OPEN | SWT.SINGLE);
            _customIconDialog.setFilterExtensions(new String[] { "*.png; *.jpeg; *.jpg; *.gif; *.ico", "*.*" });
            _customIconDialog.setFilterNames(new String[] { "Images", "All files" });
        }
        final String selected = _customIconDialog.open();
        if (selected != null) {
            try {
                Image img = ImageUtil.createImageFromFile(selected);
                Rectangle bounds = img.getBounds();
                if ( (bounds.height > Constants.MAX_AVATAR_HEIGHT) || (bounds.width > Constants.MAX_AVATAR_WIDTH) )
                    img = ImageUtil.resize(img, Constants.MAX_AVATAR_WIDTH, Constants.MAX_AVATAR_HEIGHT, true);
                _icon.disposeImage();
                _icon.setImage(img);
                _icon.redraw();
                _customIconFile = selected;
                _customIcon.setSelection(true);
                _icon.getParent().setVisible(true);
                ((GridData)_icon.getParent().getLayoutData()).exclude = false;
                _root.layout(true, true);
            } catch (SWTException se) {
                _ui.errorMessage("Error creating the image from " + selected, se);
                MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_ERROR | SWT.OK);
                box.setText(_translationRegistry.getText("Invalid image"));
                box.setMessage(_translationRegistry.getText("Unable to load the image: ") + se.getMessage());
                box.open();
            }
        }
    }
    
    
    private void createCategory() {
        TreeItem items[] = _refCategory.getSelection();
        if ( (items == null) || (items.length != 1) )
            return;
        NymReferenceNode parent = (NymReferenceNode)items[0].getData("node");
        
        String name = _refNewCat.getText().trim();
        if (name.length() <= 0)
            return;
        
        long parentGroupId = (parent == null ? -1 : parent.getGroupId());
        NymReferenceNode newCat = new NymReferenceNode(name, null, null, -1, -1, parentGroupId, -1, false, false, false);
        
        TreeItem child = new TreeItem(items[0], SWT.NONE);
        child.setText(name);
        child.setData("node", newCat); // newCat's fields are updated by the bookmark call
        items[0].setExpanded(true);
        _refCategory.setSelection(new TreeItem[] { child });
        _refCategory.showItem(child);
        _bookmarkControl.bookmark(newCat, true);
        _refNewCat.setText("");
    }
    
    private void uriSelected(SyndieURI uri) {
        _icon.disposeImage();
        _ui.debugMessage("uri selected: " + uri);
        if (uri == null) {
            _watched.setEnabled(false);
            _refs.setEnabled(false);
            _refCategory.setEnabled(false);
            _refNewCat.setEnabled(false);
            _refNewCatCreate.setEnabled(false);
            _refNewCatLabel.setEnabled(false);
            _customDesc.setEnabled(false);
            _customDescText.setEnabled(false);
            _customIcon.setEnabled(false);
            _customIconBrowse.setEnabled(false);
            _customName.setEnabled(false);
            _customNameText.setEnabled(false);
            _ok.setEnabled(false);
            _location.setText("");
            _summary.getParent().setVisible(false);
            ((GridData)_summary.getParent().getLayoutData()).exclude = true;
        } else {
            _refs.setEnabled(true);
            _refCategory.setEnabled(true);
            _refNewCat.setEnabled(true);
            _refNewCatLabel.setEnabled(true);
            _customDesc.setEnabled(true);
            _customDescText.setEnabled(true);
            _customIcon.setEnabled(true);
            _customIconBrowse.setEnabled(true);
            _customName.setEnabled(true);
            _customNameText.setEnabled(true);
            _ok.setEnabled(true);
            
            String val = _refNewCat.getText();
            if (val.trim().length() > 0)
                _refNewCatCreate.setEnabled(_location.getText().trim().length() > 0);
            
            Hash scope = null;
            if ( (uri.isChannel()) && (uri.getScope() != null) && (uri.getMessageId() == null) ) {
                scope = uri.getScope();
            } else if (uri.isSearch()) {
                Hash scopes[] = uri.getSearchScopes();
                if ( (scopes != null) && (scopes.length == 1) )
                    scope = scopes[0];
            }
            
            String name = null;
            if (scope != null) {
                _watched.setEnabled(true);
                _watched.setSelection(true);
                long channelId = _client.getChannelId(scope);
                Image img = null;
                byte avatar[] = _client.getChannelAvatar(channelId);
                if (avatar != null)
                    img = ImageUtil.createImage(avatar);
                if ( (img == null) && (_client.isWatched(channelId)) )
                    img = ImageUtil.ICON_EDITOR_BOOKMARKED_NOAVATAR;
                else if (img == null)
                    img = ImageUtil.ICON_EDITOR_NOT_BOOKMARKED;
                _icon.setImage(img);
                name = _client.getChannelName(channelId);
                if (name == null) name = "";
                name = name + " [" + scope.toBase64().substring(0,6) + "]";
                
                if (_client.getNymChannelAvatarDefined(channelId))
                    _customIcon.setSelection(true);
            } else {
                _watched.setEnabled(false);
                _watched.setSelection(false);
                if (uri.isURL())
                    name = uri.getURL();
                else
                    name = uri.getType();
                _icon.setImage(ImageUtil.getTypeIcon(uri));
            }
            
            _refs.setSelection(true);
            
            _icon.redraw();
            
            _location.setText(uri.toString());            
            _summary.setText(name);
            
            _summary.getParent().setVisible(true);
            ((GridData)_summary.getParent().getLayoutData()).exclude = false;
        }
        
        _root.layout(true, true);
    }
    
    private void selectURI() {
        LinkBuilderPopup popup = new LinkBuilderPopup(_client, _ui, _themeRegistry, _translationRegistry, _navControl, _banControl, _bookmarkControl, _root.getShell(), new LinkBuilderPopup.LinkBuilderSource() {
            public void uriBuildingCancelled() {}
            public void uriBuilt(SyndieURI uri, String text) {
                _ui.debugMessage("uri built: " + uri);
                uriSelected(uri);
            }

            public int getPageCount() { return 0; }
            public List getAttachmentDescriptions() { return new ArrayList(); }
        });
        popup.setShowText(false);
        SyndieURI uri = null;
        try { 
            uri = new SyndieURI(_location.getText());
        } catch (URISyntaxException use) {
            uri = null;
        }
        popup.showPopup(_translationRegistry.getText("Select location"), uri, null);
    }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
    }
    
    public void applyTheme(Theme theme) {
        _locationLabel.setFont(theme.DEFAULT_FONT);
        _location.setFont(theme.DEFAULT_FONT);
        _locationSelect.setFont(theme.BUTTON_FONT);
        _summary.setFont(theme.DEFAULT_FONT);
        _watched.setFont(theme.DEFAULT_FONT);
        _refs.setFont(theme.DEFAULT_FONT);
        _refCategory.setFont(theme.TREE_FONT);
        _refNewCatLabel.setFont(theme.DEFAULT_FONT);
        _refNewCat.setFont(theme.DEFAULT_FONT);
        _refNewCatCreate.setFont(theme.BUTTON_FONT);
        _customName.setFont(theme.DEFAULT_FONT);
        _customNameText.setFont(theme.DEFAULT_FONT);
        _customDesc.setFont(theme.DEFAULT_FONT);
        _customDescText.setFont(theme.DEFAULT_FONT);
        _customIcon.setFont(theme.DEFAULT_FONT);
        _customIconBrowse.setFont(theme.BUTTON_FONT);
        _ok.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
    }
    
    public void translate(TranslationRegistry registry) {
        _locationLabel.setText(registry.getText("Location") + ':');
        _locationSelect.setText(registry.getText("Select") + "...");
        _watched.setText(registry.getText("Include in the watched list"));
        _refs.setText(registry.getText("Add to my references in") + ':');
        _refNewCatLabel.setText(registry.getText("New category name") + ':');
        _refNewCatCreate.setText(registry.getText("Create"));
        _customName.setText(registry.getText("Custom nickname") + ':');
        _customDesc.setText(registry.getText("Custom description") + ':');
        _customIcon.setText(registry.getText("Custom icon"));
        _customIconBrowse.setText(registry.getText("Browse") + "...");
        _ok.setText(registry.getText("OK"));
        _cancel.setText(registry.getText("Cancel"));
    }

}
