package syndie.gui;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.i2p.data.Hash;
import net.i2p.data.PrivateKey;
import net.i2p.data.SessionKey;
import net.i2p.data.SigningPrivateKey;
import net.i2p.data.SigningPublicKey;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.ArchiveInfo;
import syndie.data.ChannelInfo;
import syndie.data.NymKey;
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
    
    private Composite _root;
    private Label _avatar;
    private Image _avatarImgOrig;
    private Image _avatarImg;
    private List _avatarImgStandard;
    private Label _nameLabel;
    private Text _name;
    private Label _tagsLabel;
    private Text _tags;
    private Label _descriptionLabel;
    private Text _description;
    private Menu _avatarMenu;
    private Label _authorizationLabel;
    private Label _authorization;
    private Label _expirationLabel;
    private Text _expiration;
    private Label _referencesLabel;
    private Combo _references;
    /** ReferenceNode instances correlating with the entries in _references (may be null) */
    private List _referenceNodes;
    /** just the roots of the _references */
    private List _referenceNodeRoots;
    private Group _userGroup;
    private Table _users;
    private TableColumn _userName;
    private TableColumn _userPriv;
    private Map _userItemToHash;
    private Group _archiveGroup;
    private Table _archives;
    private TableColumn _archiveURL;
    private TableColumn _archiveIsPub;
    private Map _archiveItemToURI;

    private boolean _initialized;

    private ManageReferenceChooserPopup _refPopup;
    
    private List _managerHashes;
    private List _posterHashes;
    private List _pubArchiveURIs;
    private List _privArchiveURIs;
    
    private int _auth = 1;
    
    public ViewForum(BrowserControl browser, Composite parent, SyndieURI uri) {
        _browser = browser;
        _parent = parent;
        _uri = uri;
        _scope = null;
        _scopeId = -1;
        _initialized = false;
        _avatarImgStandard = new ArrayList();
        _archiveItemToURI = new HashMap();
        _userItemToHash = new HashMap();
        _privArchiveURIs = new ArrayList();
        _pubArchiveURIs = new ArrayList();
        _managerHashes = new ArrayList();
        _posterHashes = new ArrayList();
        Hash scope = uri.getScope();
        if (scope == null)
            scope = uri.getHash("scope");
        if (scope != null) {
            _scope = scope;
            _scopeId = browser.getClient().getChannelId(scope);
        } else {
            _browser.getUI().debugMessage("no scope!  creating a new one");
        }
        initComponents();
    }
    public boolean getEditable() { return false; }
    public boolean canShow(Hash scope) {
        return (scope != null) && (_scope != null) && _scope.equals(scope);
    }
    
    public void resized() {
        _browser.getUI().debugMessage("resizing forum parent");
        applyTheme(_browser.getThemeRegistry().getTheme()); 
        _parent.layout(true, true);
    }
    
    Composite getRoot() { return _root; }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(7, false));
        
        loadOrigAvatar();
        
        _avatar = new Label(_root, SWT.NONE);
        _avatar.setLayoutData(new GridData(GridData.CENTER, GridData.CENTER, false, false, 1, 3));
        
        _nameLabel = new Label(_root, SWT.NONE);
        _nameLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _name = new Text(_root, SWT.BORDER | SWT.SINGLE | SWT.READ_ONLY);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        //gd.widthHint = 100;
        _name.setLayoutData(gd);
        
        _tagsLabel = new Label(_root, SWT.NONE);
        _tagsLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _tags = new Text(_root, SWT.BORDER | SWT.SINGLE | SWT.READ_ONLY);
        _tags.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        
        _descriptionLabel = new Label(_root, SWT.NONE);
        _descriptionLabel.setLayoutData(new GridData(GridData.END, GridData.BEGINNING, false, false));
        
        int descWidth = 3;
        _description = new Text(_root, SWT.BORDER | SWT.SINGLE | SWT.READ_ONLY);
        _description.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, descWidth, 1));
        
        _expirationLabel = new Label(_root, SWT.NONE);
        _expirationLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _expiration = new Text(_root, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        gd.widthHint = 50;
        _expiration.setLayoutData(gd);
        
        _authorizationLabel = new Label(_root, SWT.NONE);
        _authorizationLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));

        _authorization = new Label(_root, SWT.NONE);
        _authorization.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        
        _referencesLabel = new Label(_root, SWT.NONE);
        _referencesLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _references = new Combo(_root, SWT.DROP_DOWN | SWT.READ_ONLY);
        int colspan = 6;
        _references.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, colspan, 1));
        
        _references.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewRef(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewRef(); }
            private void viewRef() {
                if (_refPopup == null)
                    _refPopup = new ManageReferenceChooserPopup(_browser, _root.getShell(), false);
                _refPopup.setReferences(_referenceNodeRoots);
                _refPopup.show();
            }
        });
        
        _userGroup = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _userGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 7, 1));
        _userGroup.setLayout(new GridLayout(1, false));

        _users = new Table(_userGroup, SWT.SINGLE | SWT.FULL_SELECTION | SWT.BORDER);
        _users.setHeaderVisible(false);
        _users.setLinesVisible(true);
        gd = new GridData(GridData.FILL, GridData.FILL, true, true, 1, 2);
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
        viewMeta.setText(_browser.getTranslationRegistry().getText(T_USER_VIEWFORUMMETA, "View forum profile"));
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

        _archiveGroup = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _archiveGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 7, 1));
        _archiveGroup.setLayout(new GridLayout(1, false));
        
        _archives = new Table(_archiveGroup, SWT.SINGLE | SWT.FULL_SELECTION | SWT.BORDER);
        _archives.setLinesVisible(true);
        _archives.setHeaderVisible(false);
        gd = new GridData(GridData.FILL, GridData.FILL, true, true);
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
        
        ChannelInfo info = _browser.getClient().getChannel(_scopeId);
        loadData();
        
        if (info != null) {
            if (info.getAllowPublicPosts())
                _auth = 1;
            else if (info.getAllowPublicReplies())
                _auth = 2;
            else
                _auth = 3;
        }
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
        
    }
    
    ChannelInfo getChannelInfo() { return _browser.getClient().getChannel(_scopeId); }
    
    public void dispose() {
        ImageUtil.dispose(_avatarImgOrig);
        for (int i = 0; i < _avatarImgStandard.size(); i++)
            ImageUtil.dispose((Image)_avatarImgStandard.get(i));
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
        if (_refPopup != null) _refPopup.dispose();
    }
    
    /* called when the tab is closed */
    public boolean confirmClose() { return true; }
    
    private void loadOrigAvatar() {
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
                _avatarImgOrig = img;
            } else {
                _avatarImgOrig = null;
            }
        }
    }
    
    private void pickAvatar(Image img) {
        Image old = _avatarImg;
        if (!_avatarImgStandard.contains(old) && (old != _avatarImgOrig) ) {
            ImageUtil.dispose(old);
        }
        _avatarImg = img;
        _avatar.setImage(img);
        _avatar.setVisible(true);
        _avatar.redraw();
        _avatar.getParent().layout(new Control[] { _avatar });
    }
    
    private void loadData() { loadData(_browser.getClient().getChannel(_scopeId)); }
    private void loadData(ChannelInfo info) {
        if (info != null) {
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
            
            if (_avatarImgOrig != null)
                pickAvatar(_avatarImgOrig);
            
            loadArchives(info);
            loadUsers(info);
            _root.layout(true, true);
        }
        _initialized = true;
    }
    private static final String str(String orig) { return (orig != null ? orig : ""); }
    private void loadReferences(ChannelInfo info) {
        _referenceNodeRoots = info.getReferences();
        redrawReferences();
    }
    private void redrawReferences() {
        _references.setRedraw(false);
        _references.removeAll();
        // place a depth first walk of the references into _references
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
                _references.add(buf.toString());
            }
        };
        ReferenceNode.walk(_referenceNodeRoots, walker);
        _referenceNodes = dfsNodes;
        if (_referenceNodes.size() <= 0) {
            ((GridData)_references.getLayoutData()).exclude = true;
            ((GridData)_referencesLabel.getLayoutData()).exclude = true;
            //((GridData)_referencesAdd.getParent().getLayoutData()).exclude = true;
            //_referencesAdd.getParent().setVisible(false);
            _references.setVisible(false);
            _referencesLabel.setVisible(false);
        }
        _references.setRedraw(true);
    }
    private void loadArchives(ChannelInfo info) {
        // add buttons w/ menus for the archives in _archiveGroup
        _privArchiveURIs.clear();
        for (Iterator iter = info.getPrivateArchives().iterator(); iter.hasNext(); ) {
            ArchiveInfo archive = (ArchiveInfo)iter.next();
            _privArchiveURIs.add(archive.getURI());
        }
        _pubArchiveURIs.clear();
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
        _browser.getUI().debugMessage("redrawArchives: all=" + all.size() + " pub=" + _pubArchiveURIs.size() + " priv=" + _privArchiveURIs.size());
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
        
        if (all.size() <= 0) {
            _archiveGroup.setVisible(false);
            ((GridData)_archiveGroup.getLayoutData()).exclude = true;
        }
        
        _archiveURL.pack();
        _archiveIsPub.pack();
    
        _archives.setRedraw(true);
    }

    private static final String T_ARCHIVE_VIEW = "syndie.gui.viewforum.archive.view";
    private static final String T_ARCHIVE_PUBLIC = "syndie.gui.viewforum.archive.public";
    private static final String T_ARCHIVE_PRIVATE = "syndie.gui.viewforum.archive.private";
    
    private void loadUsers(ChannelInfo info) {
        // add buttons w/ menus for the authorized managers and posters in _userGroup
        _managerHashes.clear();
        _posterHashes.clear();
        _managerHashes.addAll(info.getAuthorizedManagerHashes());
        _posterHashes.addAll(info.getAuthorizedPosterHashes());
        
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
        
        if (all.size() <= 0) {
            _userGroup.setVisible(false);
            ((GridData)_userGroup.getLayoutData()).exclude = true;
        }
        
        _userName.pack();
        _userPriv.pack();
    
        _users.setRedraw(true);
    }
    private static final String T_USER_PRIV_MANAGE = "syndie.gui.viewforum.users.priv.manage";
    private static final String T_USER_PRIV_POST = "syndie.gui.viewforum.users.priv.post";
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
    
    public void applyTheme(Theme theme) {
        _nameLabel.setFont(theme.DEFAULT_FONT);
        _name.setFont(theme.DEFAULT_FONT);
        _tagsLabel.setFont(theme.DEFAULT_FONT);
        _tags.setFont(theme.DEFAULT_FONT);
        _descriptionLabel.setFont(theme.DEFAULT_FONT);
        _description.setFont(theme.DEFAULT_FONT);
        _expirationLabel.setFont(theme.DEFAULT_FONT);
        _expiration.setFont(theme.DEFAULT_FONT);
        _referencesLabel.setFont(theme.DEFAULT_FONT);
        _references.setFont(theme.DEFAULT_FONT);
        _archiveGroup.setFont(theme.DEFAULT_FONT);
        _archives.setFont(theme.TABLE_FONT);
        
        _userGroup.setFont(theme.DEFAULT_FONT);
        _users.setFont(theme.TABLE_FONT);
        _authorizationLabel.setFont(theme.DEFAULT_FONT);
        _authorization.setFont(theme.DEFAULT_FONT);
        
        redrawArchives();
        redrawUsers();

        _root.pack(true);
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
    private static final String T_USERS = "syndie.gui.viewforum.users";
    private static final String T_PUBARCHIVE = "syndie.gui.viewforum.pubarchive";
    private static final String T_PRIVARCHIVE = "syndie.gui.viewforum.privarchive";
    private static final String T_SAVE = "syndie.gui.viewforum.save";
    private static final String T_CANCEL = "syndie.gui.viewforum.cancel";
    
    private static final String T_AUTHGROUP = "syndie.gui.viewforum.authgroup";
    private static final String T_AUTHGROUP_LABEL = "syndie.gui.viewforum.authgroup.label";
    private static final String T_AUTHGROUP_READ = "syndie.gui.viewforum.authgroup.read";
    private static final String T_AUTHGROUP_POST = "syndie.gui.viewforum.authgroup.post";
    private static final String T_AUTHGROUP_MANAGE = "syndie.gui.viewforum.authgroup.manage";
    private static final String T_AUTHGROUP_REPLY = "syndie.gui.viewforum.authgroup.reply";

    private static final String T_AUTH_PUBLIC = "syndie.gui.viewforum.auth.public";
    private static final String T_AUTH_PUBREPLY = "syndie.gui.viewforum.auth.pubreply";
    private static final String T_AUTH_AUTH = "syndie.gui.viewforum.auth.auth";
    
    private static final String T_AVATAR_OTHER = "syndie.gui.viewforum.avatar.other";

    public void translate(TranslationRegistry registry) {
        _nameLabel.setText(registry.getText(T_NAME, "Name:"));
        _tagsLabel.setText(registry.getText(T_TAGS, "Tags:"));
        _descriptionLabel.setText(registry.getText(T_DESC, "Description:"));
        _expirationLabel.setText(registry.getText(T_EXPIRATION, "Expiration:"));
        _referencesLabel.setText(registry.getText(T_REFERENCES, "References:"));
        _archiveGroup.setText(registry.getText(T_PUBARCHIVE, "Advertized archives:"));
        _userGroup.setText(registry.getText(T_USERS, "Authorized managers and posters:"));
        _authorizationLabel.setText(registry.getText(T_AUTH, "Authorization:"));

        // order correlates w/ AUTH_*
        if (_auth == 1)
            _authorization.setText(registry.getText(T_AUTH_PUBLIC, "Allow anyone to post to the forum"));
        else if (_auth == 2)
            _authorization.setText(registry.getText(T_AUTH_PUBREPLY, "Allow anyone to reply to authorized posts"));
        else if (_auth == 3)
            _authorization.setText(registry.getText(T_AUTH_AUTH, "Only allow authorized posters to post"));
    }
    
    private static final int AUTH_PUBLIC = 0;
    private static final int AUTH_PUBREPLY = 1;
    private static final int AUTH_AUTH = 2;
}
