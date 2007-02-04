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
class ManageForum implements Translatable, Themeable {
    private BrowserControl _browser;
    private Composite _parent;
    private SyndieURI _uri;
    private Hash _scope;
    private long _scopeId;
    
    private Composite _root;
    private Button _avatar;
    private Image _avatarImgOrig;
    private Image _avatarImg;
    private List _avatarImgStandard;
    private MenuItem _avatarOther;
    private Menu _avatarMenu;
    private Label _nameLabel;
    private Text _name;
    private Label _tagsLabel;
    private Text _tags;
    private Label _descriptionLabel;
    private Text _description;
    private Label _expirationLabel;
    private Text _expiration;
    private Label _referencesLabel;
    private Combo _references;
    /** ReferenceNode instances correlating with the entries in _references (may be null) */
    private List _referenceNodes;
    /** just the roots of the _references */
    private List _referenceNodeRoots;
    private Group _archiveGroup;
    private Table _archives;
    private TableColumn _archiveURL;
    private TableColumn _archiveIsPub;
    private Map _archiveItemToURI;
    private Button _archiveAdd;
    private Group _authGroup;
    private Label _authLabel;
    private Button _authRead;
    private Button _authPost;
    private Button _authManage;
    private Button _authReply;
    private Composite _actions;
    private Button _save;
    private Button _cancel;

    private boolean _initialized;
    private boolean _modified;
    
    private ManageReferenceChooserPopup _refPopup;
    private ManageForumAuthRead _manageForumAuthRead;
    private ManageForumAuthPost _manageForumAuthPost;
    private ManageForumAuthManage _manageForumAuthManage;
    private ManageForumAuthReply _manageForumAuthReply;
    
    private List _managerHashes;
    private List _posterHashes;
    private List _pubArchiveURIs;
    private List _privArchiveURIs;
    private String _passphrase;
    private String _prompt;
    
    public ManageForum(BrowserControl browser, Composite parent, SyndieURI uri) {
        _browser = browser;
        _parent = parent;
        _uri = uri;
        _scope = null;
        _scopeId = -1;
        _initialized = false;
        _avatarImgStandard = new ArrayList();
        _archiveItemToURI = new HashMap();
        _privArchiveURIs = new ArrayList();
        _pubArchiveURIs = new ArrayList();
        _managerHashes = new ArrayList();
        _posterHashes = new ArrayList();
        Hash scope = uri.getScope();
        if (scope == null)
            scope = uri.getHash("scope");
        if (scope != null) {
            List keys = browser.getClient().getNymKeys(scope, Constants.KEY_FUNCTION_MANAGE);
            _scope = scope;
            _scopeId = browser.getClient().getChannelId(scope);
        } else {
            _browser.getUI().debugMessage("no scope!  creating a new one");
        }
        initComponents();
    }
    public boolean getEditable() { return true; }
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
        
        _avatar = new Button(_root, SWT.PUSH);
        _avatar.setLayoutData(new GridData(GridData.CENTER, GridData.CENTER, false, false, 1, 3));
        
        _avatarMenu = new Menu(_avatar);
        _avatar.setMenu(_avatarMenu);

        populateAvatarMenu();

        _avatar.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _avatarMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _avatarMenu.setVisible(true); }
        });
        
        _nameLabel = new Label(_root, SWT.NONE);
        _nameLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _name = new Text(_root, SWT.BORDER | SWT.SINGLE);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        //gd.widthHint = 100;
        _name.setLayoutData(gd);
        _name.addModifyListener(new ModifyListener() { public void modifyText(ModifyEvent evt) { modified(); } });
        
        _tagsLabel = new Label(_root, SWT.NONE);
        _tagsLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _tags = new Text(_root, SWT.BORDER | SWT.SINGLE);
        _tags.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        _tags.addModifyListener(new ModifyListener() { public void modifyText(ModifyEvent evt) { modified(); } });
        
        _descriptionLabel = new Label(_root, SWT.NONE);
        _descriptionLabel.setLayoutData(new GridData(GridData.END, GridData.BEGINNING, false, false));
        
        int descWidth = 3;
        _description = new Text(_root, SWT.BORDER | SWT.SINGLE);
        _description.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, descWidth, 1));
        _description.addModifyListener(new ModifyListener() { public void modifyText(ModifyEvent evt) { modified(); } });
        
        _expirationLabel = new Label(_root, SWT.NONE);
        _expirationLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _expiration = new Text(_root, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        gd.widthHint = 50;
        _expiration.setLayoutData(gd);
        _expiration.addModifyListener(new ModifyListener() { public void modifyText(ModifyEvent evt) { modified(); } });
        
        _referencesLabel = new Label(_root, SWT.NONE);
        _referencesLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _references = new Combo(_root, SWT.DROP_DOWN | SWT.READ_ONLY);
        int colspan = 5;
        _references.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, colspan, 1));
        
        _references.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewRef(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewRef(); }
            private void viewRef() {
                if (_refPopup == null)
                    _refPopup = new ManageReferenceChooserPopup(_browser, _root.getShell(), true);
                _refPopup.setReferences(_referenceNodeRoots);
                _refPopup.addCloseListener(new ManageReferenceChooserPopup.CloseListener() {
                    public void closed(List refRoots) {
                        _referenceNodeRoots = refRoots;
                        modified();
                        redrawReferences();
                    }
                });
                _refPopup.show();
            }
        });
        
        _archiveGroup = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _archiveGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 7, 1));
        _archiveGroup.setLayout(new GridLayout(2, false));
        
        _archives = new Table(_archiveGroup, SWT.SINGLE | SWT.FULL_SELECTION | SWT.BORDER);
        _archives.setLinesVisible(true);
        _archives.setHeaderVisible(false);
        gd = new GridData(GridData.FILL, GridData.FILL, true, true);
        _archives.setLayoutData(gd);
        
        _archiveURL = new TableColumn(_archives, SWT.LEFT);
        _archiveIsPub = new TableColumn(_archives, SWT.RIGHT);

        final Menu archiveMenu = new Menu(_archives);
        _archives.setMenu(archiveMenu);
        MenuItem add = new MenuItem(archiveMenu, SWT.PUSH);
        add.setText(_browser.getTranslationRegistry().getText(T_ARCHIVE_ADD, "Add"));
        add.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addArchive(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addArchive(); }
        });
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
                    modified();
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
                modified();
                redrawArchives();
                _root.layout(true, true);
            }
        });

        _archiveAdd = new Button(_archiveGroup, SWT.PUSH);
        _archiveAdd.setFont(_browser.getThemeRegistry().getTheme().BUTTON_FONT);
        _archiveAdd.setLayoutData(new GridData(GridData.BEGINNING, GridData.BEGINNING, false, false));
        _archiveAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addArchive(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addArchive(); }
        });
        
        _authGroup = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _authGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 7, 1));
        _authGroup.setLayout(new GridLayout(4, true));

        _authLabel = new Label(_authGroup, SWT.WRAP);
        _authLabel.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 4, 1));

        _authRead = new Button(_authGroup, SWT.PUSH);
        _authRead.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _authRead.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
            public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
            private void fire() { 
                if (_manageForumAuthRead == null)
                    _manageForumAuthRead = new ManageForumAuthRead(_browser, ManageForum.this);
                _manageForumAuthRead.show();
            }
        });
        _authPost = new Button(_authGroup, SWT.PUSH);
        _authPost.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _authPost.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
            public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
            private void fire() { 
                if (_manageForumAuthPost == null)
                    _manageForumAuthPost = new ManageForumAuthPost(_browser, ManageForum.this);
                _manageForumAuthPost.show();
            }
        });
        _authManage = new Button(_authGroup, SWT.PUSH);
        _authManage.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _authManage.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
            public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
            private void fire() { 
                if (_manageForumAuthManage == null)
                    _manageForumAuthManage = new ManageForumAuthManage(_browser, ManageForum.this);
                _manageForumAuthManage.show();
            }
        });
        _authReply = new Button(_authGroup, SWT.PUSH);
        _authReply.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _authReply.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
            public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
            private void fire() { 
                if (_manageForumAuthReply == null)
                    _manageForumAuthReply = new ManageForumAuthReply(_browser, ManageForum.this);
                _manageForumAuthReply.show();
            }
        });
        
        _actions = new Composite(_root, SWT.NONE);
        _actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        _actions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 7, 1));
        _save = new Button(_actions, SWT.PUSH);
        _cancel = new Button(_actions, SWT.PUSH);
        _save.setEnabled(false);
        _cancel.setEnabled(false);
        
        _save.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { saveChanges(); }
            public void widgetSelected(SelectionEvent selectionEvent) { saveChanges(); }
        });
        
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { loadData(); }
            public void widgetSelected(SelectionEvent selectionEvent) { loadData(); }
        });
        
        ChannelInfo info = _browser.getClient().getChannel(_scopeId);
        loadData();
        
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
        if (_manageForumAuthRead != null) _manageForumAuthRead.dispose();
        if (_manageForumAuthPost != null) _manageForumAuthPost.dispose();
        if (_manageForumAuthManage != null) _manageForumAuthManage.dispose();
        if (_manageForumAuthReply != null) _manageForumAuthReply.dispose();
    }
    
    /* called when the tab is closed */
    public boolean confirmClose() {
        if (!_modified) return true;
        MessageBox confirm = new MessageBox(_parent.getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO);
        confirm.setText(_browser.getTranslationRegistry().getText(T_CONFIRM_CLOSE_TITLE, "Confirm"));
        confirm.setMessage(_browser.getTranslationRegistry().getText(T_CONFIRM_CLOSE_MSG, "Do you want to discard these changes to the forum?"));
        int rc = confirm.open();
        if (rc == SWT.YES) {
            return true;
        } else if (rc == SWT.NO) {
            return false;
        } else {
            return false;
        }
    }    
    private static final String T_CONFIRM_CLOSE_TITLE = "syndie.gui.manageforum.close.title";
    private static final String T_CONFIRM_CLOSE_MSG = "syndie.gui.manageforum.close.msg";
    
    private void promptForPBE() {
        PassphrasePrompt prompt = new PassphrasePrompt(_browser, _root.getShell(), true);
        prompt.setPassphraseListener(new PassphrasePrompt.PassphraseListener() {
            public void promptComplete(String passphraseEntered, String promptEntered) {
                _passphrase = passphraseEntered;
                _prompt = promptEntered;
            }
            public void promptAborted() {}
        });
        prompt.open();
    }
    
    private void saveChanges() {
        ManageForumExecutor exec = new ManageForumExecutor(_browser.getClient(), _browser.getUI(), new ManageForumExecutor.ManageForumState() {
            public boolean getCreateReadKey() {
                if (_manageForumAuthRead != null) {
                    return _manageForumAuthRead.getNewKey();
                } else {
                    return false;
                }
            }
            public boolean getCreatePostIdentity() {
                if (_manageForumAuthPost != null) {
                    return _manageForumAuthPost.getNewIdentity();
                } else {
                    return false;
                }
            }
            public boolean getCreateManageIdentity() {
                if (_manageForumAuthManage != null) {
                    return _manageForumAuthManage.getNewIdentity();
                } else {
                    return false;
                }
            }
            public boolean getCreateReplyKey() {
                if (_manageForumAuthReply != null) {
                    return _manageForumAuthReply.getRotate();
                } else {
                    return false;
                }
            }
            
            public Image getAvatar() { return _avatarImg; }
            public String getName() { return _name.getText(); }
            public String getDescription() { return _description.getText(); }
            public long getLastEdition() {
                if (_scopeId >= 0) 
                    return _browser.getClient().getChannelVersion(_scopeId);
                else
                    return -1;
            }

            public boolean getAllowPublicPosts() { 
                if (_manageForumAuthPost != null) {
                    return _manageForumAuthPost.getAllowPublicPosts();
                } else {
                    ChannelInfo info = getChannelInfo();
                    if (info != null)
                        return info.getAllowPublicPosts();
                    else
                        return false;
                }
            }
            public boolean getAllowPublicReplies() { 
                if (_manageForumAuthPost != null) {
                    return _manageForumAuthPost.getAllowPublicReplies();
                } else {
                    ChannelInfo info = getChannelInfo();
                    if (info != null)
                        return info.getAllowPublicReplies();
                    else
                        return false;
                }
            }
            public Set getPublicTags() { return Collections.EMPTY_SET; }
            public Set getPrivateTags() { 
                String tags[] = Constants.split(" \t\r\n,", _tags.getText(), false);
                Set rv = new HashSet(tags.length);
                for (int i = 0; i < tags.length; i++)
                    rv.add(tags[i]);
                return rv;
            }
            public Set getAuthorizedPosters() { 
                if (_manageForumAuthPost != null) {
                    return getPubKeys(_manageForumAuthPost.getAuthorizedPosters());
                } else {
                    ChannelInfo info = getChannelInfo();
                    if (info != null)
                        return info.getAuthorizedPosters();
                    else
                        return new HashSet();
                }
            }
            public Set getAuthorizedManagers() {
                if (_manageForumAuthManage != null) {
                    return getPubKeys(_manageForumAuthManage.getAuthorizedManagers());
                } else {
                    ChannelInfo info = getChannelInfo();
                    if (info != null)
                        return info.getAuthorizedManagers();
                    else
                        return new HashSet();
                }
            }
            private Set getPubKeys(List scopes) {
                Set rv = new HashSet();
                for (int i = 0; i < scopes.size(); i++) {
                    Hash scope = (Hash)scopes.get(i);
                    SigningPublicKey key = _browser.getClient().getChannelIdentKey(scope);
                    if (key != null)
                        rv.add(key);
                }
                return rv;
            }
            public String getReferences() { return (_referenceNodeRoots != null ? ReferenceNode.walk(_referenceNodeRoots) : ""); }
            public Set getPublicArchives() { return getArchives(_pubArchiveURIs); }
            public Set getPrivateArchives() { return getArchives(_privArchiveURIs); }
            private Set getArchives(List uris) {
                Set archives = new HashSet();
                for (Iterator iter = uris.iterator(); iter.hasNext(); ) {
                    SyndieURI uri = (SyndieURI)iter.next();
                    ArchiveInfo info = new ArchiveInfo(uri);
                    archives.add(info);
                    _browser.getUI().debugMessage("getArchives: sz=" + archives.size() + " uri=" + uri);
                }
                _browser.getUI().debugMessage("getArchives(" + uris + "): found " + archives.size() + " archives");
                return archives;
            }

            public long getChannelId() { return _scopeId; }
            
            public boolean getEncryptContent() { 
                return ((_manageForumAuthRead != null) && (_manageForumAuthRead.getEncryptMetadata()));
            }
            public boolean getPBE() { return false; }
            public String getPassphrase() { return _passphrase; }
            public String getPassphrasePrompt() { return _prompt; }
            /** return the read keys we explicitly want to deliver in the metadata, or null/empty if we don't care */
            public List getCurrentReadKeys() { return null; }
        });
        exec.execute();
        String errs = exec.getErrors();
        if ( (errs != null) && (errs.trim().length() > 0) ) {
            MessageBox box = new MessageBox(_parent.getShell(), SWT.ICON_ERROR | SWT.OK);
            box.setText(_browser.getTranslationRegistry().getText(T_ERROR_TITLE, "Error"));
            box.setMessage(_browser.getTranslationRegistry().getText(T_ERROR_MSG, "Internal error saving the forum:") + errs);
            box.open();
        } else {
            // ok, now create any of the posts we need to send keys to the right people, 
            // as defined by viewForumAuth*
            
            SyndieURI uri = exec.getForum();
            Hash scope = uri.getScope();
            SessionKey readKey = exec.getCreatedReadKey();
            Hash postIdentity = exec.getCreatedPostIdentity();
            Hash manageIdentity = exec.getCreatedManageIdentity();
            File postFile = null;
            if (postIdentity != null)
                postFile = new File(new File(_browser.getClient().getOutboundDir(), postIdentity.toBase64()), "meta" + Constants.FILENAME_SUFFIX);
            File manageFile = null;
            if (manageIdentity != null)
                manageFile = new File(new File(_browser.getClient().getOutboundDir(), manageIdentity.toBase64()), "meta" + Constants.FILENAME_SUFFIX);
            
            _browser.getUI().debugMessage("done updating, scope=" + scope + " readKey=" + readKey + " postIdent=" + postIdentity + " manageIdent=" + manageIdentity);
            
            if (_manageForumAuthRead != null) {
                // open a new post to the appropriate locations containing the read key
                if (readKey == null) {
                    // use the existing readkey to send
                    List nks = _browser.getClient().getNymKeys(uri.getScope(), Constants.KEY_FUNCTION_READ);
                    for (int i = 0; i < nks.size(); i++) {
                        NymKey nk = (NymKey)nks.get(i);
                        if (!nk.getIsExpired()) {
                            readKey = new SessionKey(nk.getData());
                            break;
                        }
                    }
                    if (readKey == null) {
                        // could be attached to the channel and not the nym, so try it there too
                        List rks = _browser.getClient().getReadKeys(uri.getScope(), true);
                        for (int i = 0; i < rks.size(); i++) {
                            SessionKey rk = (SessionKey)rks.get(i);
                            if (!_browser.getClient().getChannelReadKeyIsPublic(uri.getScope(), rk)) {
                                readKey = rk;
                                break;
                            }
                        }
                    }
                }
                List scopes = _manageForumAuthRead.getSendExplicit();
                for (int i = 0; i < scopes.size(); i++) {
                    Hash to = (Hash)scopes.get(i);
                    _browser.getUI().debugMessage("pop up a window to post the read key to " + to.toBase64());
                    _browser.view(_browser.createPostURI(to, null, true, createReferences(uri.getScope(), readKey), null));
                    //_browser.createPostURI(to, null, true, readKey);
                } 
                if (_manageForumAuthRead.getPostPBE()) {
                    _browser.getUI().debugMessage("pop up a window to post to " + _scope.toBase64() + " with pbe prompt [" + _manageForumAuthRead.getSendPassphrasePrompt() + "] for pass [" + _manageForumAuthRead.getSendPassphrase() + "]");
                    _browser.view(_browser.createPostURI(uri.getScope(), null, _manageForumAuthRead.getSendPassphrase(), _manageForumAuthRead.getSendPassphrasePrompt(), createReferences(uri.getScope(), readKey), null));
                    //_browser.createPostURI(_scope, null, false, readKey, _viewForumAuthRead.getSendPassphrase(), _viewForumAuthRead.getSendPassphrasePrompt());
                }
            }
            if ( (postIdentity != null) && (_manageForumAuthPost != null) ) {
                // open a new post to the appropriate locations containing the post identity's keys
                List scopes = _manageForumAuthPost.getSendNewExplicit();
                for (int i = 0; i < scopes.size(); i++) {
                    Hash to = (Hash)scopes.get(i);
                    _browser.getUI().debugMessage("pop up a window to post the post identity key to " + to.toBase64());
                    _browser.view(_browser.createPostURI(to, null, true, createReferences(uri.getScope(), postIdentity, Constants.KEY_FUNCTION_POST), new File[] { postFile }));
                    //_browser.createPostURI(to, null, true, readKey);
                } 
                if (_manageForumAuthPost.getPostPBE()) {
                    _browser.getUI().debugMessage("pop up a window to post the post identity keys to " + _scope.toBase64() + " with pbe prompt [" + _manageForumAuthPost.getSendPassphrasePrompt() + "] for pass [" + _manageForumAuthPost.getSendPassphrase() + "]");
                    _browser.view(_browser.createPostURI(uri.getScope(), null, _manageForumAuthPost.getSendPassphrase(), _manageForumAuthPost.getSendPassphrasePrompt(), createReferences(uri.getScope(), postIdentity, Constants.KEY_FUNCTION_POST), new File[] { postFile }));
                    //_browser.createPostURI(_scope, null, false, readKey, _viewForumAuthRead.getSendPassphrase(), _viewForumAuthRead.getSendPassphrasePrompt());
                }
            }
            if ( (manageIdentity != null) && (_manageForumAuthManage != null) ) {
                // open a new post to the appropriate locations containing the manage identity's keys
                List scopes = _manageForumAuthManage.getSendNewExplicit();
                for (int i = 0; i < scopes.size(); i++) {
                    Hash to = (Hash)scopes.get(i);
                    _browser.getUI().debugMessage("pop up a window to post the manage identity key to " + to.toBase64());
                    _browser.view(_browser.createPostURI(to, null, true, createReferences(uri.getScope(), manageIdentity, Constants.KEY_FUNCTION_MANAGE), new File[] { manageFile }));
                    //_browser.createPostURI(to, null, true, readKey);
                } 
                if (_manageForumAuthManage.getPostPBE()) {
                    _browser.getUI().debugMessage("pop up a window to post the manage identity keys to " + _scope + " with pbe prompt [" + _manageForumAuthManage.getSendPassphrasePrompt() + "] for pass [" + _manageForumAuthManage.getSendPassphrase() + "]");
                    _browser.view(_browser.createPostURI(uri.getScope(), null, _manageForumAuthManage.getSendPassphrase(), _manageForumAuthManage.getSendPassphrasePrompt(), createReferences(uri.getScope(), manageIdentity, Constants.KEY_FUNCTION_MANAGE), new File[] { manageFile }));
                    //_browser.createPostURI(_scope, null, false, readKey, _viewForumAuthRead.getSendPassphrase(), _viewForumAuthRead.getSendPassphrasePrompt());
                }
            }
            if ( (_manageForumAuthReply != null) && (_manageForumAuthReply.getRotate()) ) {
                // open a new post to the appropriate locations containing the reply keys
                List scopes = _manageForumAuthReply.getSendNewExplicit();
                for (int i = 0; i < scopes.size(); i++) {
                    Hash to = (Hash)scopes.get(i);
                    _browser.getUI().debugMessage("pop up a window to post the reply key to " + to.toBase64());
                    _browser.view(_browser.createPostURI(to, null, true, createReplyReferences(uri.getScope()), null));
                    //_browser.createPostURI(to, null, true, readKey);
                } 
                if (_manageForumAuthReply.getPostPBE()) {
                    _browser.getUI().debugMessage("pop up a window to post the reply key to " + _scope.toBase64() + " with pbe prompt [" + _manageForumAuthReply.getSendPassphrasePrompt() + "] for pass [" + _manageForumAuthReply.getSendPassphrase() + "]");
                    _browser.view(_browser.createPostURI(uri.getScope(), null, _manageForumAuthReply.getSendPassphrase(), _manageForumAuthReply.getSendPassphrasePrompt(), createReplyReferences(uri.getScope()), null));
                    //_browser.createPostURI(_scope, null, false, readKey, _viewForumAuthRead.getSendPassphrase(), _viewForumAuthRead.getSendPassphrasePrompt());
                }
            }
            
            // done
            _browser.unview(_uri);
            if (_scopeId < 0)
                _browser.view(uri);
            _browser.forumCreated();
        }
    }
    private static final String T_ERROR_TITLE = "syndie.gui.manageforum.error.title";
    private static final String T_ERROR_MSG = "syndie.gui.manageforum.error.msg";
    
    private List createReferences(Hash scope, SessionKey readKey) { 
        //_browser.getUI().debugMessage("todo: create references for the read key in " + scope + ": " + readKey);
        SyndieURI uri = SyndieURI.createScope(scope);
        Map attributes = uri.getAttributes();
        attributes.put("readKey", readKey.toBase64());
        ArrayList rv = new ArrayList();
        ReferenceNode node = new ReferenceNode("key", uri, "", "key");
        rv.add(node);
        return rv;
    }
    private List createReferences(Hash scope, Hash postIdentity, String keyFunction) {
        //_browser.getUI().debugMessage("todo: create references for the " + keyFunction + " key in " + scope + ": " + postIdentity);
        SyndieURI uri = SyndieURI.createScope(scope);
        Map attributes = uri.getAttributes();
        List nymKeys = _browser.getClient().getNymKeys(postIdentity, Constants.KEY_FUNCTION_MANAGE);
        SigningPrivateKey privKey = null;
        for (int i = 0; i < nymKeys.size(); i++) {
            NymKey k = (NymKey)nymKeys.get(i);
            if (k.getData() != null)
                privKey = new SigningPrivateKey(k.getData());
        }
        if (privKey == null) return new ArrayList();
        
        if (Constants.KEY_FUNCTION_POST.equals(keyFunction)) {
            attributes.put("postKey", privKey.toBase64());
        } else {
            attributes.put("manageKey", privKey.toBase64());
        }
        ArrayList rv = new ArrayList();
        ReferenceNode node = new ReferenceNode("key", uri, "", "key");
        rv.add(node);
        return rv;
    }
    private List createReplyReferences(Hash scope) {
        //_browser.getUI().debugMessage("todo: create references for the reply key in " + scope);
        SyndieURI uri = SyndieURI.createScope(scope);
        Map attributes = uri.getAttributes();
        List nymKeys = _browser.getClient().getNymKeys(scope, Constants.KEY_FUNCTION_REPLY);
        PrivateKey privKey = null;
        for (int i = 0; i < nymKeys.size(); i++) {
            NymKey k = (NymKey)nymKeys.get(i);
            if (k.getData() != null)
                privKey = new PrivateKey(k.getData());
        }
        if (privKey == null) return new ArrayList();
        
        attributes.put("replyKey", privKey.toBase64());
        ArrayList rv = new ArrayList();
        ReferenceNode node = new ReferenceNode("key", uri, "", "key");
        rv.add(node);
        return rv;
    }
    
    void modified() {
        if (!_initialized) return;
        if (!_modified) {
            _save.setEnabled(true);
            _cancel.setEnabled(true);
        }
        _modified = true;
    }

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
    private void populateAvatarMenu() {
        if (_avatarImgOrig != null) { // populated earlier in the initialization
            MenuItem origItem = new MenuItem(_avatarMenu, SWT.PUSH);
            origItem.setImage(_avatarImgOrig);
            origItem.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickAvatar(_avatarImgOrig); }
                public void widgetSelected(SelectionEvent selectionEvent) { pickAvatar(_avatarImgOrig); }
            });
        }
        int i = 0;
        while (true) {
            final Image img = ImageUtil.createImageFromResource("iconAvatar" + i + ".png");
            if (img != null) {
                _avatarImgStandard.add(img);
                MenuItem item = new MenuItem(_avatarMenu, SWT.PUSH);
                item.setImage(img);
                item.addSelectionListener(new SelectionListener() {
                    public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickAvatar(img); }
                    public void widgetSelected(SelectionEvent selectionEvent) { pickAvatar(img); }
                });
                i++;
            } else {
                break;
            }
        }
        
        _avatarOther = new MenuItem(_avatarMenu, SWT.PUSH);
        _avatarOther.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickAvatar(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickAvatar(); }
        });
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
        if (img != _avatarImgOrig)
            modified();
    }
    private void pickAvatar() {
        FileDialog dialog = new FileDialog(_root.getShell(), SWT.SINGLE | SWT.OPEN);
        dialog.setText(_browser.getTranslationRegistry().getText(T_AVATAR_OPEN_NAME, "Select a 48x48 pixel PNG image"));
        dialog.setFilterExtensions(new String[] { "*.png" });
        dialog.setFilterNames(new String[] { _browser.getTranslationRegistry().getText(T_AVATAR_OPEN_TYPE, "PNG image") });
        String filename = dialog.open();
        if (filename != null) {
            Image img = ImageUtil.createImageFromFile(filename);
            if (img != null) {
                Rectangle bounds = img.getBounds();
                int width = bounds.width;
                int height = bounds.height;
                if (width > Constants.MAX_AVATAR_WIDTH)
                    width = Constants.MAX_AVATAR_WIDTH;
                if (height > Constants.MAX_AVATAR_HEIGHT)
                    height = Constants.MAX_AVATAR_HEIGHT;
                if ( (height != bounds.height) || (width != bounds.width) ) {
                    img = ImageUtil.resize(img, width, height, true);
                }
                final Image revamped = img;
                int idx = _avatarMenu.indexOf(_avatarOther);
                MenuItem item = new MenuItem(_avatarMenu, SWT.PUSH, idx);
                item.setImage(img);
                item.addSelectionListener(new SelectionListener() {
                    public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickAvatar(revamped); }
                    public void widgetSelected(SelectionEvent selectionEvent) { pickAvatar(revamped); }
                });
                _avatarImgStandard.add(img);
                pickAvatar(img);
            }
        }
    }
    
    private static final String T_AVATAR_OPEN_NAME = "syndie.gui.manageforum.avatar.name";
    private static final String T_AVATAR_OPEN_TYPE = "syndie.gui.manageforum.avatar.type";
    
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
        _modified = false;
        _save.setEnabled(false);
        _cancel.setEnabled(false);
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
            _references.add(_browser.getTranslationRegistry().getText(T_REFERENCES_ADD, "Add new references"));
        } else {
            _references.add(_browser.getTranslationRegistry().getText(T_REFERENCES_EDIT, "Manage references"));
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
        
        _archiveURL.pack();
        _archiveIsPub.pack();
    
        _archives.setRedraw(true);
    }

    private void loadUsers(ChannelInfo info) {
        _managerHashes.clear();
        _posterHashes.clear();
        _managerHashes.addAll(info.getAuthorizedManagerHashes());
        _posterHashes.addAll(info.getAuthorizedPosterHashes());
    }
    
    private static final String T_ARCHIVE_VIEW = "syndie.gui.manageforum.archive.view";
    private static final String T_ARCHIVE_DELETE = "syndie.gui.manageforum.archive.delete";
    private static final String T_ARCHIVE_ISPUB = "syndie.gui.manageforum.archive.ispub";
    private static final String T_ARCHIVE_ADD = "syndie.gui.manageforum.archive.add";
    private static final String T_ARCHIVE_PUBLIC = "syndie.gui.manageforum.archive.public";
    private static final String T_ARCHIVE_PRIVATE = "syndie.gui.manageforum.archive.private";
    
    private void addArchive() {
        LinkBuilderPopup popup = new LinkBuilderPopup(_browser, _parent.getShell(), new LinkBuilderPopup.LinkBuilderSource () {
                public void uriBuilt(SyndieURI uri, String text) {
                    if (uri != null) {
                        _privArchiveURIs.add(uri);
                        redrawArchives();
                        modified();
                    }
                }
                public int getPageCount() { return 0; }
                public List getAttachmentDescriptions() { return Collections.EMPTY_LIST; }
            });
        popup.limitOptions(false, false, false, false, false, false, false, false, false, true, false);
        popup.setShowText(false);
        popup.showPopup();
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
        
        _save.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);

        _authGroup.setFont(theme.DEFAULT_FONT);
        _authLabel.setFont(theme.DEFAULT_FONT);
        _authRead.setFont(theme.BUTTON_FONT);
        _authPost.setFont(theme.BUTTON_FONT);
        _authManage.setFont(theme.BUTTON_FONT);
        _authReply.setFont(theme.BUTTON_FONT);
        _archiveAdd.setFont(theme.BUTTON_FONT);
        
        redrawArchives();

        _root.pack(true);
    }

    private static final String T_NAME = "syndie.gui.manageforum.name";
    private static final String T_TAGS = "syndie.gui.manageforum.tags";
    private static final String T_DESC = "syndie.gui.manageforum.desc";
    private static final String T_AUTH = "syndie.gui.manageforum.auth";
    private static final String T_EXPIRATION = "syndie.gui.manageforum.expiration";
    private static final String T_REFERENCES = "syndie.gui.manageforum.references";
    private static final String T_REFERENCES_ADD = "syndie.gui.manageforum.references.add";
    private static final String T_REFERENCES_EDIT = "syndie.gui.manageforum.references.edit";
    private static final String T_USERS = "syndie.gui.manageforum.users";
    private static final String T_PUBARCHIVE = "syndie.gui.manageforum.pubarchive";
    private static final String T_PRIVARCHIVE = "syndie.gui.manageforum.privarchive";
    private static final String T_SAVE = "syndie.gui.manageforum.save";
    private static final String T_CANCEL = "syndie.gui.manageforum.cancel";
    
    private static final String T_AUTHGROUP = "syndie.gui.manageforum.authgroup";
    private static final String T_AUTHGROUP_LABEL = "syndie.gui.manageforum.authgroup.label";
    private static final String T_AUTHGROUP_READ = "syndie.gui.manageforum.authgroup.read";
    private static final String T_AUTHGROUP_POST = "syndie.gui.manageforum.authgroup.post";
    private static final String T_AUTHGROUP_MANAGE = "syndie.gui.manageforum.authgroup.manage";
    private static final String T_AUTHGROUP_REPLY = "syndie.gui.manageforum.authgroup.reply";

    private static final String T_AUTH_PUBLIC = "syndie.gui.manageforum.auth.public";
    private static final String T_AUTH_PUBREPLY = "syndie.gui.manageforum.auth.pubreply";
    private static final String T_AUTH_AUTH = "syndie.gui.manageforum.auth.auth";
    
    private static final String T_AVATAR_OTHER = "syndie.gui.manageforum.avatar.other";

    public void translate(TranslationRegistry registry) {
        _nameLabel.setText(registry.getText(T_NAME, "Name:"));
        _tagsLabel.setText(registry.getText(T_TAGS, "Tags:"));
        _descriptionLabel.setText(registry.getText(T_DESC, "Description:"));
        _expirationLabel.setText(registry.getText(T_EXPIRATION, "Expiration:"));
        _referencesLabel.setText(registry.getText(T_REFERENCES, "References:"));
        _archiveGroup.setText(registry.getText(T_PUBARCHIVE, "Advertized archives:"));
        _save.setText(registry.getText(T_SAVE, "Save changes"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel changes"));

        _authGroup.setText(registry.getText(T_AUTHGROUP, "Authorization and authentication"));
        _authLabel.setText(registry.getText(T_AUTHGROUP_LABEL, "Forum authorization and authentication takes four forms - those allowed to read a forum's posts, those allowed to post to a forum, those allowed to manage a forum, and those allowed to read the private replies to forum administrators"));
        _authRead.setText(registry.getText(T_AUTHGROUP_READ, "Read posts"));
        _authPost.setText(registry.getText(T_AUTHGROUP_POST, "Create posts"));
        _authManage.setText(registry.getText(T_AUTHGROUP_MANAGE, "Manage"));
        _authReply.setText(registry.getText(T_AUTHGROUP_REPLY, "Read forum feedback"));
        _archiveAdd.setText(_browser.getTranslationRegistry().getText(T_ARCHIVE_ADD, "Add"));

        _avatarOther.setText(_browser.getTranslationRegistry().getText(T_AVATAR_OTHER, "Other..."));
    }
}
