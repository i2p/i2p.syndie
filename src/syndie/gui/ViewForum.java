package syndie.gui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.ArchiveInfo;
import syndie.data.ChannelInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;

/**
 *
 */
class ViewForum implements Translatable, Themeable {
    private BrowserControl _browser;
    private Composite _parent;
    private SyndieURI _uri;
    private Hash _scope;
    private long _scopeId;
    
    private ScrolledComposite _scroll;
    private Composite _root;
    private ImageCanvas _avatar;
    private Label _nameLabel;
    private Text _name;
    private Label _tagsLabel;
    private Text _tags;
    private Label _descriptionLabel;
    private Text _description;
    private Button _avatarSelect;
    private Menu _avatarMenu;
    private Label _authorizationLabel;
    private Combo _authorization;
    private Label _expirationLabel;
    private Text _expiration;
    private Label _referencesLabel;
    private Combo _references;
    /** ReferenceNode instances correlating with the entries in _references (may be null) */
    private List _referenceNodes;
    private Button _referencesAdd;
    private Button _referencesEdit;
    private Button _referencesDelete;
    private Group _userGroup;
    private Table _users;
    private TableColumn _userName;
    private TableColumn _userPriv;
    private Map _userItemToHash;
    private Button _userAdd;
    private Group _archiveGroup;
    private Table _archives;
    private TableColumn _archiveURL;
    private TableColumn _archiveIsPub;
    private Map _archiveItemToURI;
    private Button _archiveAdd;
    private Composite _actions;
    private Button _save;
    private Button _cancel;
    private Group _keyManagementGroup;
    private Button _keyManagementOpen;
    private Label _keyManagementOpenInfo;
    private Button _keyManagementKeep;
    private Label _keyManagementKeepInfo;
    private Button _keyManagementRotate;
    private Label _keyManagementRotateInfo;
    private Button _keyManagementReset;
    private Label _keyManagementResetInfo;
    private Button _keyManagementPBE;
    private Label _keyManagementPBEInfo;
    private Button _keyManagementNewReply;
    
    private boolean _editable;
    private boolean _modified;
    
    private List _managerHashes;
    private List _posterHashes;
    private List _pubArchiveURIs;
    private List _privArchiveURIs;
    
    public ViewForum(BrowserControl browser, Composite parent, SyndieURI uri) {
        _browser = browser;
        _parent = parent;
        _uri = uri;
        _editable = false;
        _scope = null;
        _scopeId = -1;
        _archiveItemToURI = new HashMap();
        _userItemToHash = new HashMap();
        Hash scope = uri.getScope();
        if (scope != null) {
            List keys = browser.getClient().getNymKeys(scope, Constants.KEY_FUNCTION_MANAGE);
            if ( (keys != null) && (keys.size() > 0) )
                _editable = true;
            _browser.getUI().debugMessage("management nym keys for " + scope.toBase64() + ": " + keys);
            _scope = scope;
            _scopeId = browser.getClient().getChannelId(scope);
        } else {
            _browser.getUI().debugMessage("no scope!");
        }
        initComponents();
    }
    
    private void initComponents() {
        _scroll = new ScrolledComposite(_parent, SWT.V_SCROLL | SWT.H_SCROLL);
        _scroll.setAlwaysShowScrollBars(false);
        _scroll.setExpandHorizontal(true);
        _scroll.setExpandVertical(true);
        _root = new Composite(_scroll, SWT.NONE);
        _root.setLayout(new GridLayout(7, false));
        _scroll.setContent(_root);
        
        _avatar = new ImageCanvas(_root, false);
        _avatar.setLayoutData(new GridData(GridData.CENTER, GridData.END, false, false, 1, 2));
        
        _nameLabel = new Label(_root, SWT.NONE);
        _nameLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _name = new Text(_root, SWT.BORDER | SWT.SINGLE | (!_editable ? SWT.READ_ONLY : 0));
        GridData gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        //gd.widthHint = 100;
        _name.setLayoutData(gd);
        
        _tagsLabel = new Label(_root, SWT.NONE);
        _tagsLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _tags = new Text(_root, SWT.BORDER | SWT.SINGLE | (!_editable ? SWT.READ_ONLY : 0));
        _tags.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        
        _descriptionLabel = new Label(_root, SWT.NONE);
        _descriptionLabel.setLayoutData(new GridData(GridData.END, GridData.BEGINNING, false, false));
        
        _description = new Text(_root, SWT.BORDER | SWT.SINGLE | (!_editable ? SWT.READ_ONLY : 0));
        _description.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 5, 1));
        
        _avatarSelect = new Button(_root, SWT.PUSH);
        _avatarSelect.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        if (!_editable) _avatarSelect.setVisible(false);
        
        if (_editable) {
            _avatarMenu = new Menu(_avatar);
            _avatar.setMenu(_avatarMenu);
            _avatarSelect.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { _avatarMenu.setVisible(true); }
                public void widgetSelected(SelectionEvent selectionEvent) { _avatarMenu.setVisible(true); }
            });
        }
        
        _authorizationLabel = new Label(_root, SWT.NONE);
        _authorizationLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _authorization = new Combo(_root, SWT.READ_ONLY | SWT.DROP_DOWN);
        _authorization.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        if (!_editable) _authorization.setEnabled(false);
        
        _expirationLabel = new Label(_root, SWT.NONE);
        _expirationLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _expiration = new Text(_root, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        gd.widthHint = 50;
        _expiration.setLayoutData(gd);
        
        _referencesLabel = new Label(_root, SWT.NONE);
        _referencesLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _references = new Combo(_root, SWT.DROP_DOWN | SWT.READ_ONLY);
        _references.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        
        Composite refActions = new Composite(_root, SWT.NONE);
        refActions.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false, 2, 1));
        refActions.setLayout(new FillLayout(SWT.HORIZONTAL));
        
        _referencesAdd = new Button(refActions, SWT.PUSH);
        _referencesEdit = new Button(refActions, SWT.PUSH);
        _referencesDelete = new Button(refActions, SWT.PUSH);
        
        _userGroup = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _userGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 7, 1));
        _userGroup.setLayout(new GridLayout(2, false));
        
        _users = new Table(_userGroup, SWT.SINGLE | SWT.FULL_SELECTION | SWT.BORDER);
        _users.setHeaderVisible(false);
        _users.setLinesVisible(true);
        gd = new GridData(GridData.FILL, GridData.FILL, true, true);
        gd.heightHint = 50;
        _users.setLayoutData(gd);
        
        _userName = new TableColumn(_users, SWT.LEFT);
        _userPriv = new TableColumn(_users, SWT.RIGHT);
        
        final Menu userMenu = new Menu(_users);
        _users.setMenu(userMenu);
        MenuItem viewForum = new MenuItem(userMenu, SWT.PUSH);
        viewForum.setText(_browser.getTranslationRegistry().getText(T_USER_VIEWFORUM, "View forum"));
        viewForum.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewUser(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewUser(); }
            private void viewUser() {
                TableItem items[] = _users.getSelection();
                for (int i = 0; i < items.length; i++) {
                    Hash scope = (Hash)_userItemToHash.get(items[i]);
                    _browser.view(SyndieURI.createScope(scope));
                }
            }
        });
        MenuItem viewMeta = new MenuItem(userMenu, SWT.PUSH);
        viewMeta.setText(_browser.getTranslationRegistry().getText(T_USER_VIEWFORUMMETA, "View forum metadata"));
        viewMeta.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewMeta(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewMeta(); }
            private void viewMeta() {
                TableItem items[] = _users.getSelection();
                for (int i = 0; i < items.length; i++) {
                    Hash scope = (Hash)_userItemToHash.get(items[i]);
                    _browser.view(_browser.createMetaURI(scope));
                }
            }
        });

        if (_editable) {
            final MenuItem manager = new MenuItem(userMenu, SWT.PUSH);
            manager.setText(_browser.getTranslationRegistry().getText(T_USER_MANAGE, "Toggle manager/poster"));
            manager.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent evt) { toggleManager(); }
                public void widgetSelected(SelectionEvent evt) { toggleManager(); }
                private void toggleManager() {
                    TableItem items[] = _users.getSelection();
                    for (int i = 0; i < items.length; i++) {
                        Hash scope = (Hash)_userItemToHash.get(items[i]);
                        toggleUserManagement(scope);
                    }
                }
            });
            
            MenuItem delete = new MenuItem(userMenu, SWT.PUSH);
            delete.setText(_browser.getTranslationRegistry().getText(T_USER_DELETE, "Delete"));
            delete.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { delete(); }
                public void widgetSelected(SelectionEvent selectionEvent) { delete(); }
                private void delete() {
                    TableItem items[] = _users.getSelection();
                    for (int i = 0; i < items.length; i++) {
                        Hash scope = (Hash)_userItemToHash.get(items[i]);
                        deleteUser(scope);
                    }
                }
            });
            
            _userAdd = new Button(_userGroup, SWT.PUSH);
            _userAdd.setFont(_browser.getThemeRegistry().getTheme().BUTTON_FONT);
            _userAdd.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
            _userAdd.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { addUser(); }
                public void widgetSelected(SelectionEvent selectionEvent) { addUser(); }
            });
        }
        
        _archiveGroup = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _archiveGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 7, 1));
        _archiveGroup.setLayout(new GridLayout(2, false));
        
        _archives = new Table(_archiveGroup, SWT.SINGLE | SWT.FULL_SELECTION | SWT.BORDER);
        _archives.setLinesVisible(true);
        _archives.setHeaderVisible(false);
        gd = new GridData(GridData.FILL, GridData.FILL, true, true);
        gd.heightHint = 50;
        _archives.setLayoutData(gd);
        
        _archiveURL = new TableColumn(_archives, SWT.LEFT);
        _archiveIsPub = new TableColumn(_archives, SWT.RIGHT);

        final Menu archiveMenu = new Menu(_archives);
        _archives.setMenu(archiveMenu);
        MenuItem view = new MenuItem(archiveMenu, SWT.PUSH);
        view.setText(_browser.getTranslationRegistry().getText(T_ARCHIVE_VIEW, "View"));
        view.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewArchive(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewArchive(); }
            private void viewArchive() {
                TableItem items[] = _archives.getSelection();
                for (int i = 0; i < items.length; i++) {
                    SyndieURI uri = (SyndieURI)_archiveItemToURI.get(items[i]);
                    if (uri != null) {
                        if (uri.isArchive())
                            _browser.view(uri);
                        else
                            _browser.view(SyndieURI.createArchive(uri.getURL(), null));
                    }
                }
            }
        });
        if (_editable) {
            MenuItem delete = new MenuItem(archiveMenu, SWT.PUSH);
            delete.setText(_browser.getTranslationRegistry().getText(T_ARCHIVE_DELETE, "Delete"));
            delete.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { deleteArchive(); }
                public void widgetSelected(SelectionEvent selectionEvent) { deleteArchive(); }
                private void deleteArchive() {
                    TableItem items[] = _archives.getSelection();
                    for (int i = 0; i < items.length; i++) {
                        SyndieURI uri = (SyndieURI)_archiveItemToURI.get(items[i]);
                        _pubArchiveURIs.remove(uri);
                        _privArchiveURIs.remove(uri);
                    }
                    redrawArchives();
                    _root.layout(true, true);
                }
            });
            MenuItem isPub = new MenuItem(archiveMenu, SWT.PUSH);
            isPub.setText(_browser.getTranslationRegistry().getText(T_ARCHIVE_ISPUB, "Toggle share publicly"));
            isPub.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { toggleArchivePub(); }
                public void widgetSelected(SelectionEvent selectionEvent) { toggleArchivePub(); }
                private void toggleArchivePub() {
                    TableItem items[] = _archives.getSelection();
                    for (int i = 0; i < items.length; i++) {
                        SyndieURI uri = (SyndieURI)_archiveItemToURI.get(items[i]);
    
                        if (_pubArchiveURIs.contains(uri)) {
                            _pubArchiveURIs.remove(uri);
                            if (!_privArchiveURIs.contains(uri))
                                _privArchiveURIs.add(uri);
                        } else {
                            _privArchiveURIs.remove(uri);
                            if (!_pubArchiveURIs.contains(uri))
                                _pubArchiveURIs.add(uri);
                        }
                    }
                    redrawArchives();
                    _root.layout(true, true);
                }
            });
        }

        
        
        if (_editable) {
            _archiveAdd = new Button(_archiveGroup, SWT.PUSH);
            _archiveAdd.setFont(_browser.getThemeRegistry().getTheme().BUTTON_FONT);
            _archiveAdd.setLayoutData(new GridData(GridData.BEGINNING, GridData.BEGINNING, false, false));
            _archiveAdd.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { addArchive(); }
                public void widgetSelected(SelectionEvent selectionEvent) { addArchive(); }
            });
        }
        
        if (!_editable) refActions.setVisible(false);
        
        _actions = new Composite(_root, SWT.NONE);
        _actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        _actions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 7, 1));
        _save = new Button(_actions, SWT.PUSH);
        _cancel = new Button(_actions, SWT.PUSH);
        
        if (!_editable) {
            _actions.setVisible(false);
            ((GridData)_actions.getLayoutData()).exclude = true;
        }
        
        _keyManagementGroup = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _keyManagementGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 7, 1));
        _keyManagementGroup.setLayout(new GridLayout(1, true));
        
        _keyManagementOpen = new Button(_keyManagementGroup, SWT.RADIO);
        _keyManagementOpen.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _keyManagementOpenInfo = new Label(_keyManagementGroup, SWT.WRAP);
        _keyManagementOpenInfo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _keyManagementKeep = new Button(_keyManagementGroup, SWT.RADIO);
        _keyManagementKeep.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _keyManagementKeepInfo = new Label(_keyManagementGroup, SWT.WRAP);
        _keyManagementKeepInfo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _keyManagementRotate = new Button(_keyManagementGroup, SWT.RADIO);
        _keyManagementRotate.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _keyManagementRotateInfo = new Label(_keyManagementGroup, SWT.WRAP);
        _keyManagementRotateInfo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _keyManagementReset = new Button(_keyManagementGroup, SWT.RADIO);
        _keyManagementReset.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _keyManagementResetInfo = new Label(_keyManagementGroup, SWT.WRAP);
        _keyManagementResetInfo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _keyManagementPBE = new Button(_keyManagementGroup, SWT.CHECK);
        _keyManagementPBE.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _keyManagementPBEInfo = new Label(_keyManagementGroup, SWT.WRAP);
        _keyManagementPBEInfo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _keyManagementNewReply = new Button(_keyManagementGroup, SWT.CHECK);
        _keyManagementNewReply.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _keyManagementKeep.setSelection(true);
        
        if (!_editable) {
            _keyManagementGroup.setVisible(false);
            ((GridData)_keyManagementGroup.getLayoutData()).exclude = true;
        }
    
        loadData();
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
    }
    
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
    }
    
    private void loadData() {
        ChannelInfo info = _browser.getClient().getChannel(_scopeId);
        if (info != null) {
            if (info.getAllowPublicPosts())
                _authorization.select(AUTH_PUBLIC);
            else if (info.getAllowPublicReplies())
                _authorization.select(AUTH_PUBREPLY);
            else
                _authorization.select(AUTH_AUTH);
            
            byte avatar[] = _browser.getClient().getChannelAvatar(_scopeId);
            if (avatar != null) {
                Image img = ImageUtil.createImage(avatar);
                if (img != null) {
                    Rectangle rect = img.getBounds();
                    int width = rect.width;
                    int height = rect.height;
                    boolean mod = false;
                    if (width > Constants.MAX_AVATAR_WIDTH) {
                        width = Constants.MAX_AVATAR_WIDTH;
                        mod = true;
                    }
                    if (height > Constants.MAX_AVATAR_HEIGHT) {
                        height = Constants.MAX_AVATAR_HEIGHT;
                        mod = true;
                    }
                    if (mod)
                        img = ImageUtil.resize(img, width, height, true);
                    _avatar.setImage(img);
                } else {
                    _avatar.disposeImage();
                    _avatar.setImage(null);
                }
            }
            
            _description.setText(str(info.getDescription()));
            
            if (info.getExpiration() > 0)
                _expiration.setText(Constants.getDate(info.getExpiration()));
            
            _name.setText(str(info.getName()));
            
            loadReferences(info);
            
            StringBuffer buf = new StringBuffer();
            for (Iterator iter = info.getPublicTags().iterator(); iter.hasNext(); )
                buf.append((String)iter.next()).append(" ");
            for (Iterator iter = info.getPrivateTags().iterator(); iter.hasNext(); )
                buf.append((String)iter.next()).append(" ");
            _tags.setText(buf.toString());
            
            loadArchives(info);
            loadUsers(info);
            _root.layout(true, true);
        }
        _modified = false;
    }
    private static final String str(String orig) { return (orig != null ? orig : ""); }
    private void loadReferences(ChannelInfo info) {
        // place a depth first walk of the references into _references
        List refs = info.getReferences();
        final List dfsNodes = new ArrayList();
        ReferenceNode.Visitor walker = new ReferenceNode.Visitor() {
            public void visit(ReferenceNode node, int depth, int siblingOrder) {
                dfsNodes.add(node);
                ReferenceNode parent = node.getParent();
                String parentString = null;
                if (parent != null)
                    parentString = _references.getItem(dfsNodes.indexOf(parent));
                String name = node.getName();
                String desc = node.getDescription();
                SyndieURI uri = node.getURI();
                StringBuffer buf = new StringBuffer();
                if (parentString != null) 
                    buf.append(parentString).append(" > ");
                if (name != null)
                    buf.append(name);
                else if (desc != null)
                    buf.append(desc);
                else if (uri != null)
                    buf.append(uri.toString());
                /*
                if (desc != null)
                    buf.append(desc).append(' ');
                if (uri != null) {
                    if (uri.isArchive())
                        buf.append(uri.getURL());
                    else if (uri.isChannel())
                        buf.append(uri.getScope().toBase64().substring(0,6));
                    else if (uri.isSearch())
                        buf.append(uri.toString());
                    else if (uri.isURL())
                        buf.append(uri.getURL());
                    else
                        buf.append(uri.toString());
                }
                 */
                _references.add(buf.toString());
            }
        };
        ReferenceNode.walk(refs, walker);
        _referenceNodes = dfsNodes;
        if ( (!_editable) && (_referenceNodes.size() <= 0) ) {
            ((GridData)_references.getLayoutData()).exclude = true;
            ((GridData)_referencesLabel.getLayoutData()).exclude = true;
            ((GridData)_referencesAdd.getParent().getLayoutData()).exclude = true;
            _references.setVisible(false);
            _referencesLabel.setVisible(false);
            _referencesAdd.getParent().setVisible(false);
        }
    }
    private void loadArchives(ChannelInfo info) {
        // add buttons w/ menus for the archives in _archiveGroup
        _privArchiveURIs = new ArrayList();
        for (Iterator iter = info.getPrivateArchives().iterator(); iter.hasNext(); ) {
            ArchiveInfo archive = (ArchiveInfo)iter.next();
            _privArchiveURIs.add(archive.getURI());
        }
        _pubArchiveURIs = new ArrayList();
        for (Iterator iter = info.getPublicArchives().iterator(); iter.hasNext(); ) {
            ArchiveInfo archive = (ArchiveInfo)iter.next();
            _pubArchiveURIs.add(archive.getURI());
        }
        
        redrawArchives();
    }
    private void redrawArchives() {
        _archives.setRedraw(false);
        _archives.removeAll();
        _archiveItemToURI.clear();
        
        List all = new ArrayList();
        all.addAll(_pubArchiveURIs);
        all.addAll(_privArchiveURIs);
        for (int i = 0; i < all.size(); i++) {
            final SyndieURI uri = (SyndieURI)all.get(i);
            if (uri == null) continue;
            String url = uri.getURL();
            if ( (url == null) || (url.trim().length() <= 0) ) continue;
            TableItem item = new TableItem(_archives, SWT.NONE);
            item.setText(0, url.trim());
            if (_pubArchiveURIs.contains(uri))
                item.setText(1, _browser.getTranslationRegistry().getText(T_ARCHIVE_PUBLIC, "Public"));
            else
                item.setText(1, _browser.getTranslationRegistry().getText(T_ARCHIVE_PRIVATE, "Authorized readers only"));
            
            _archiveItemToURI.put(item, uri);
        }
        
        if ( (all.size() <= 0) && (!_editable) ) {
            _archiveGroup.setVisible(false);
            ((GridData)_archiveGroup.getLayoutData()).exclude = true;
        }
        
        _archiveURL.pack();
        _archiveIsPub.pack();
    
        _archives.setRedraw(true);
    }

    private static final String T_ARCHIVE_VIEW = "syndie.gui.viewforum.archive.view";
    private static final String T_ARCHIVE_DELETE = "syndie.gui.viewforum.archive.delete";
    private static final String T_ARCHIVE_ISPUB = "syndie.gui.viewforum.archive.ispub";
    private static final String T_ARCHIVE_ADD = "syndie.gui.viewforum.archive.add";
    private static final String T_ARCHIVE_PUBLIC = "syndie.gui.viewforum.archive.public";
    private static final String T_ARCHIVE_PRIVATE = "syndie.gui.viewforum.archive.private";
    
    private void addArchive() {
        LinkBuilderPopup popup = new LinkBuilderPopup(_browser, _parent.getShell(), new LinkBuilderPopup.LinkBuilderSource () {
                public void uriBuilt(SyndieURI uri, String text) {
                    if (uri != null) {
                        _privArchiveURIs.add(uri);
                        redrawArchives();
                    }
                }
                public int getPageCount() { return 0; }
                public List getAttachmentDescriptions() { return Collections.EMPTY_LIST; }
            });
        popup.limitOptions(false, false, false, false, false, false, false, false, false, true);
        popup.setShowText(false);
        popup.showPopup();
    }
    
    private void loadUsers(ChannelInfo info) {
        // add buttons w/ menus for the authorized managers and posters in _userGroup
        _managerHashes = new ArrayList(info.getAuthorizedManagerHashes());
        _posterHashes = new ArrayList(info.getAuthorizedPosterHashes());
        
        redrawUsers();
    }
    
    private void redrawUsers() {
        _users.setRedraw(false);
        _users.removeAll();
        _userItemToHash.clear();
        
        List all = new ArrayList();
        all.addAll(_managerHashes);
        all.addAll(_posterHashes);
        
        for (int i = 0; i < all.size(); i++) {
            final Hash scope = (Hash)all.get(i);
            String name = _browser.getClient().getChannelName(scope);
            
            TableItem item = new TableItem(_users, SWT.NONE);
            if (name != null)
                item.setText(0, scope.toBase64().substring(0,6) + ": " + name);
            else
                item.setText(0, scope.toBase64().substring(0,6));
            
            if (_managerHashes.contains(scope))
                item.setText(1, _browser.getTranslationRegistry().getText(T_USER_PRIV_MANAGE, "Manager"));
            else
                item.setText(1, _browser.getTranslationRegistry().getText(T_USER_PRIV_POST, "Authorized poster"));
            
            _userItemToHash.put(item, scope);
        }
        
        if ( (all.size() <= 0) && (!_editable) ) {
            _userGroup.setVisible(false);
            ((GridData)_userGroup.getLayoutData()).exclude = true;
        }
        
        _userName.pack();
        _userPriv.pack();
    
        _users.setRedraw(true);
    }
    private static final String T_USER_PRIV_MANAGE = "syndie.gui.viewforum.users.priv.manage";
    private static final String T_USER_PRIV_POST = "syndie.gui.viewforum.users.priv.post";
    private static final String T_USER_ADD = "syndie.gui.viewforum.user.add";
    private static final String T_USER_DELETE = "syndie.gui.viewforum.user.delete";
    private static final String T_USER_MANAGE = "syndie.gui.viewforum.user.manage";
    private static final String T_USER_VIEWFORUMMETA = "syndie.gui.viewforum.user.viewforummeta";
    private static final String T_USER_VIEWFORUM = "syndie.gui.viewforum.user.viewforum";
    private void toggleUserManagement(Hash scope) {
        if (_managerHashes.contains(scope)) {
            _managerHashes.remove(scope);
            if (!_posterHashes.contains(scope))
                _posterHashes.add(scope);
        } else {
            _posterHashes.remove(scope);
            if (!_managerHashes.contains(scope))
                _managerHashes.add(scope);
        }
        redrawUsers();
        _root.layout(true, true);
    }
    private void addUser() {}
    private void deleteUser(Hash scope) {
        _managerHashes.remove(scope);
        _posterHashes.remove(scope);
        redrawUsers();
        _root.layout(true, true);
    }
    
    public void applyTheme(Theme theme) {
        _nameLabel.setFont(theme.DEFAULT_FONT);
        _name.setFont(theme.DEFAULT_FONT);
        _tagsLabel.setFont(theme.DEFAULT_FONT);
        _tags.setFont(theme.DEFAULT_FONT);
        _descriptionLabel.setFont(theme.DEFAULT_FONT);
        _description.setFont(theme.DEFAULT_FONT);
        _authorizationLabel.setFont(theme.DEFAULT_FONT);
        _authorization.setFont(theme.DEFAULT_FONT);
        _expirationLabel.setFont(theme.DEFAULT_FONT);
        _expiration.setFont(theme.DEFAULT_FONT);
        _referencesLabel.setFont(theme.DEFAULT_FONT);
        _references.setFont(theme.DEFAULT_FONT);
        _userGroup.setFont(theme.DEFAULT_FONT);
        _archiveGroup.setFont(theme.DEFAULT_FONT);
        _save.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
        _keyManagementGroup.setFont(theme.DEFAULT_FONT);
        _keyManagementOpenInfo.setFont(theme.DEFAULT_FONT);
        _keyManagementRotateInfo.setFont(theme.DEFAULT_FONT);
        _keyManagementKeepInfo.setFont(theme.DEFAULT_FONT);
        _keyManagementResetInfo.setFont(theme.DEFAULT_FONT);
        _keyManagementPBEInfo.setFont(theme.DEFAULT_FONT);
        _keyManagementOpen.setFont(theme.DEFAULT_FONT);
        _keyManagementRotate.setFont(theme.DEFAULT_FONT);
        _keyManagementKeep.setFont(theme.DEFAULT_FONT);
        _keyManagementReset.setFont(theme.DEFAULT_FONT);
        _keyManagementPBE.setFont(theme.DEFAULT_FONT);
        _keyManagementNewReply.setFont(theme.DEFAULT_FONT);
        _users.setFont(theme.TABLE_FONT);
        _archives.setFont(theme.TABLE_FONT);
        
        _avatarSelect.setFont(theme.BUTTON_FONT);
        _referencesAdd.setFont(theme.BUTTON_FONT);
        _referencesEdit.setFont(theme.BUTTON_FONT);
        _referencesDelete.setFont(theme.BUTTON_FONT);
        
        if (_userAdd != null) _userAdd.setFont(theme.BUTTON_FONT);
        if (_archiveAdd != null) _archiveAdd.setFont(theme.BUTTON_FONT);
        
        redrawArchives();
        redrawUsers();

        _root.layout(true, true);
        _scroll.setMinSize(_root.computeSize(_userGroup.computeSize(SWT.DEFAULT, SWT.DEFAULT).x, SWT.DEFAULT));
    }

    private static final String T_NAME = "syndie.gui.viewforum.name";
    private static final String T_TAGS = "syndie.gui.viewforum.tags";
    private static final String T_DESC = "syndie.gui.viewforum.desc";
    private static final String T_AVATAR_SELECT = "syndie.gui.viewforum.avatar.select";
    private static final String T_AUTH = "syndie.gui.viewforum.auth";
    private static final String T_EXPIRATION = "syndie.gui.viewforum.expiration";
    private static final String T_REFERENCES = "syndie.gui.viewforum.references";
    private static final String T_REFERENCES_ADD = "syndie.gui.viewforum.references.add";
    private static final String T_REFERENCES_EDIT = "syndie.gui.viewforum.references.edit";
    private static final String T_REFERENCES_DELETE = "syndie.gui.viewforum.references.delete";
    private static final String T_USERS = "syndie.gui.viewforum.users";
    private static final String T_PUBARCHIVE = "syndie.gui.viewforum.pubarchive";
    private static final String T_PRIVARCHIVE = "syndie.gui.viewforum.privarchive";
    private static final String T_SAVE = "syndie.gui.viewforum.save";
    private static final String T_CANCEL = "syndie.gui.viewforum.cancel";
    private static final String T_KEYMGMT = "syndie.gui.viewforum.keymgmt";
    private static final String T_KEYMGMT_OPEN = "syndie.gui.viewforum.keymgmt.open";
    private static final String T_KEYMGMT_OPEN_INFO = "syndie.gui.viewforum.keymgmt.open.info";
    private static final String T_KEYMGMT_KEEP = "syndie.gui.viewforum.keymgmt.keep";
    private static final String T_KEYMGMT_KEEP_INFO = "syndie.gui.viewforum.keymgmt.keep.info";
    private static final String T_KEYMGMT_ROTATE = "syndie.gui.viewforum.keymgmt.rotate";
    private static final String T_KEYMGMT_ROTATE_INFO = "syndie.gui.viewforum.keymgmt.rotate.info";
    private static final String T_KEYMGMT_RESET = "syndie.gui.viewforum.keymgmt.reset";
    private static final String T_KEYMGMT_RESET_INFO = "syndie.gui.viewforum.keymgmt.reset.info";
    private static final String T_KEYMGMT_PBE = "syndie.gui.viewforum.keymgmt.pbe";
    private static final String T_KEYMGMT_PBE_INFO = "syndie.gui.viewforum.keymgmt.pbe.info";
    private static final String T_KEYMGMT_NEWREPLY = "syndie.gui.viewforum.keymgmt.newreply";

    private static final String T_AUTH_PUBLIC = "syndie.gui.viewforum.auth.public";
    private static final String T_AUTH_PUBREPLY = "syndie.gui.viewforum.auth.pubreply";
    private static final String T_AUTH_AUTH = "syndie.gui.viewforum.auth.auth";
        
    public void translate(TranslationRegistry registry) {
        _nameLabel.setText(registry.getText(T_NAME, "Name:"));
        _tagsLabel.setText(registry.getText(T_TAGS, "Tags:"));
        _descriptionLabel.setText(registry.getText(T_DESC, "Description:"));
        _avatarSelect.setText(registry.getText(T_AVATAR_SELECT, "Select..."));
        _authorizationLabel.setText(registry.getText(T_AUTH, "Authorization:"));
        _expirationLabel.setText(registry.getText(T_EXPIRATION, "Expiration:"));
        _referencesLabel.setText(registry.getText(T_REFERENCES, "References:"));
        _referencesAdd.setText(registry.getText(T_REFERENCES_ADD, "Add"));
        _referencesEdit.setText(registry.getText(T_REFERENCES_EDIT, "Edit"));
        _referencesDelete.setText(registry.getText(T_REFERENCES_DELETE, "Delete"));
        _userGroup.setText(registry.getText(T_USERS, "Authorized managers and posters:"));
        _archiveGroup.setText(registry.getText(T_PUBARCHIVE, "Advertized archives:"));
        _save.setText(registry.getText(T_SAVE, "Save changes"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel changes"));
        _keyManagementGroup.setText(registry.getText(T_KEYMGMT, "Advanced: forum key management:"));
        _keyManagementOpen.setText(registry.getText(T_KEYMGMT_OPEN, "Open access"));
        _keyManagementOpenInfo.setText(registry.getText(T_KEYMGMT_OPEN_INFO, "This creates a new key for reading messages and publicizes it so that anyone can read with it"));
        _keyManagementKeep.setText(registry.getText(T_KEYMGMT_KEEP, "Keep existing keys"));
        _keyManagementKeepInfo.setText(registry.getText(T_KEYMGMT_KEEP_INFO, "This uses existing keys, publicizing them if they used to be public.  If the keys were passphrase protected before however, new users will not be able to enter the passphrase and gain access to these existing keys.  To do so, require a passphrase again here as well."));
        _keyManagementRotate.setText(registry.getText(T_KEYMGMT_ROTATE, "Rotate keys for authorized readers"));
        _keyManagementRotateInfo.setText(registry.getText(T_KEYMGMT_ROTATE_INFO, "This creates a new key, encrypted so that only already authorized readers can access the new key"));
        _keyManagementReset.setText(registry.getText(T_KEYMGMT_RESET, "Reset all keys"));
        _keyManagementResetInfo.setText(registry.getText(T_KEYMGMT_RESET_INFO, "This creates a new key encrypted with another new key.  The key used can either be distributed to authorized readers manually (with links in private messages, keyfiles, etc) or shared in a passphrase encrypted post."));
        _keyManagementPBE.setText(registry.getText(T_KEYMGMT_PBE, "Require a passphrase to access the keys"));
        _keyManagementPBEInfo.setText(registry.getText(T_KEYMGMT_PBE_INFO, "If a passphrase is required, even already authorized users will need to know the passphrase, so consider posting that prior to a key rotation."));
        _keyManagementNewReply.setText(registry.getText(T_KEYMGMT_NEWREPLY, "Create a new forum reply key"));

        if (_archiveAdd != null)
            _archiveAdd.setText(_browser.getTranslationRegistry().getText(T_ARCHIVE_ADD, "Add"));
        if (_userAdd != null)
            _userAdd.setText(_browser.getTranslationRegistry().getText(T_USER_ADD, "Add"));
        
        int auth = -1;
        if (_authorization.getItemCount() > 0)
            auth = _authorization.getSelectionIndex();
        else
            auth = 1;
        _authorization.setRedraw(false);
        _authorization.removeAll();
        // order correlates w/ AUTH_*
        _authorization.add(registry.getText(T_AUTH_PUBLIC, "Allow anyone to post to the forum"));
        _authorization.add(registry.getText(T_AUTH_PUBREPLY, "Allow anyone to reply to authorized posts"));
        _authorization.add(registry.getText(T_AUTH_AUTH, "Only allow authorized posters to post"));
        _authorization.select(auth);
        _authorization.setRedraw(true);
    }
    
    private static final int AUTH_PUBLIC = 0;
    private static final int AUTH_PUBREPLY = 1;
    private static final int AUTH_AUTH = 2;
}
