package syndie.gui;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import net.i2p.data.Hash;
import net.i2p.data.SigningPrivateKey;
import net.i2p.data.SigningPublicKey;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
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
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.ArchiveInfo;
import syndie.data.ChannelInfo;
import syndie.data.NymKey;
import syndie.data.SyndieURI;
import syndie.db.CommandImpl;
import syndie.db.JobRunner;

/**
 *
 */
class ManageForum implements ReferenceChooserTree.AcceptanceListener, Translatable, Themeable {
    private Composite _parent;
    private Composite _root;
    private BrowserControl _browser;
    
    private Image _avatarImage;
    private ImageCanvas _avatar;
    private Label _nameLabel;
    private Text _name;
    private Label _tagsLabel;
    private Text _tags;
    private Label _expireLabel;
    private Text _expire;
    private Label _descLabel;
    private Text _desc;
    /*
    private Label _privacyLabel;
    private Combo _privacy;
     */
    private Label _authLabel;
    private Combo _auth;

    private CTabFolder _detailTabs;
    private CTabItem _managerItem;
    private CTabItem _posterItem;
    private CTabItem _archiveItem;
    private CTabItem _referencesItem;
    private CTabItem _readKeysItem;
    
    private ScrolledComposite _readKeysScroll;
    private Composite _readKeys;
    private Label _readKeySummary;

    private Group _readKeysCurrent;
    private Table _readKeysCurrentTable;
    private TableColumn _readKeysCurrentHash;
    private TableColumn _readKeysCurrentPublic;
    private TableColumn _readKeysCurrentCreatedOn;
    private Button _readKeysAdd;
    private Button _readKeysDrop;
    
    private Group _readKeysOld;
    private Table _readKeysOldTable;
    private TableColumn _readKeysOldHash;
    private TableColumn _readKeysOldPublic;
    private TableColumn _readKeysOldCreatedOn;
    private TableColumn _readKeysOldDroppedOn;

    private Label _readKeyPrivacySummary;
    private Label _readKeyPrivacyPrompt;
    private Combo _readKeyPrivacyCombo;
    
    private Composite _managers;
    private List _managerList;
    private MenuItem _managerAdd;
    private MenuItem _managerView;
    private MenuItem _managerRemove;
    
    private Composite _posters;
    private List _posterList;
    private MenuItem _posterAdd;
    private MenuItem _posterView;
    private MenuItem _posterRemove;
    
    private Composite _archives;
    private ManageForumArchiveChooser _archiveChooser;
    
    private Composite _references;
    private ManageReferenceChooser _referencesChooser;

    private Button _save;
    private Button _cancel;
    
    private ManageForumListener _listener;
    
    private long _channelId;
    private ChannelInfo _origInfo;
    private ArrayList _managerKeys;
    private ArrayList _posterKeys;
    private String _passphrase;
    private String _passphrasePrompt;
    
    private ReferenceChooserPopup _refChooser;
    private boolean _addingPoster;
    
    private boolean _editable;
    private boolean _modified;
    
    public ManageForum(BrowserControl browser, Composite parent, ManageForumListener lsnr) {
        this(browser, parent, lsnr, true);
    }
    public ManageForum(BrowserControl browser, Composite parent, ManageForumListener lsnr, boolean editable) {
        _parent = parent;
        _browser = browser;
        _listener = lsnr;
        _managerKeys = new ArrayList();
        _posterKeys = new ArrayList();
        _editable = editable;
        _modified = false;
        initComponents();
    }
    
    public interface ManageForumListener {
        public void manageComplete(ManageForum manage);
    }
    
    public static final String DETAIL = "detail";
    public static final String DETAIL_ARCHIVES = "archives";
    public static final String DETAIL_POSTERS = "posters";
    public static final String DETAIL_REFS = "refs";
    public static final String DETAIL_MANAGER = "manager";
    
    public void pickDetail(String detail) {
        if (DETAIL_ARCHIVES.equals(detail)) {
            _detailTabs.setSelection(CHOICE_ARCHIVES);
        } else if (DETAIL_POSTERS.equals(detail)) {
            _detailTabs.setSelection(CHOICE_POSTER);
        } else if (DETAIL_REFS.equals(detail)) {
            _detailTabs.setSelection(CHOICE_REFS);
        } else if (DETAIL_MANAGER.equals(detail)) {
            _detailTabs.setSelection(CHOICE_MANAGER);
        } else {
            _detailTabs.setSelection(CHOICE_REFS);
        }
    }
    
    public void setForum(final SyndieURI uri) {
        _managerKeys.clear();
        _posterKeys.clear();
        _origInfo = null;
        _channelId = -1;
        
        _managerList.setRedraw(false);
        _posterList.setRedraw(false);
        
        _managerList.removeAll();
        _posterList.removeAll();
        
        Hash scope = null;
        ChannelInfo info = null;
        if (uri != null)
            scope = uri.getScope();
        if (scope != null) {
            long id = _browser.getClient().getChannelId(scope);
            if (id >= 0) {
                info = _browser.getClient().getChannel(id);
            }
        }
        if (info.getPassphrasePrompt() != null) {
            _browser.getUI().debugMessage("the channel info is still encrypted");
            PassphrasePrompt prompt = new PassphrasePrompt(_browser, _root.getShell(), false);
            prompt.setPassphrasePrompt(info.getPassphrasePrompt());
            prompt.setPassphraseListener(new PassphrasePrompt.PassphraseListener() {
                public void promptComplete(String passphraseEntered, String promptEntered) {
                    reimport(passphraseEntered, uri);
                }
                public void promptAborted() { _browser.unview(uri); }
            });
            prompt.open();
            return;
        }
        _browser.getUI().debugMessage("config forum: " + scope);
        if ( (scope != null) && (_browser.getClient().getNymKeys(info.getChannelHash(), Constants.KEY_FUNCTION_MANAGE).size() <= 0) ) {
            _browser.getUI().debugMessage("we don't have the forum's identity key, try for an explicit key");
            // we don't have a key that we use for this channel, but we may have a key that the
            // channel lets manage it
            boolean found = false;
            if (info != null) {
                for (Iterator iter = info.getAuthorizedManagers().iterator(); iter.hasNext() && !found; ) {
                    SigningPublicKey pub = (SigningPublicKey)iter.next();
                    for (Iterator nymIter = _browser.getClient().getNymKeys(pub.calculateHash(), Constants.KEY_FUNCTION_MANAGE).iterator(); nymIter.hasNext(); ) {
                        NymKey key = (NymKey)nymIter.next();
                        SigningPrivateKey priv = new SigningPrivateKey(key.getData());
                        if (priv.toPublic().equals(pub)) {
                            // we have one of the explicitly specified management keys
                            found = true;
                            break;
                        }
                    }
                }
            }
            if (!found) {
                _editable = false;
                //_browser.getUI().errorMessage("You are not authorized to manage this channel");
                //info = null;
            } else {
                _readKeys.setEnabled(true);
                _browser.getUI().debugMessage("explicit forum key found");
            }
            
            _readKeys.setEnabled(found);
            _readKeyPrivacyCombo.setEnabled(found);
            _readKeyPrivacyPrompt.setEnabled(found);
            _readKeyPrivacySummary.setEnabled(found);
            _readKeySummary.setEnabled(found);
            _readKeysAdd.setEnabled(found);
            _readKeysCurrent.setEnabled(found);
            _readKeysCurrentTable.setEnabled(found);
            _readKeysDrop.setEnabled(found);
            _readKeysOld.setEnabled(found);
            _readKeysOldTable.setEnabled(found);
        }
        if (info != null) {
            _browser.getUI().debugMessage("forum loaded: " + info.getChannelHash().toBase64());
            _channelId = info.getChannelId();
            if (info.getAllowPublicPosts()) {
                _browser.getUI().debugMessage("allow public posts");
                _auth.select(AUTH_UNAUTHPOST);
            } else if (info.getAllowPublicReplies()) {
                _browser.getUI().debugMessage("allow public replies");
                _auth.select(AUTH_UNAUTHREPLY);
            } else {
                _browser.getUI().debugMessage("allow neither public posts nor public replies");
                _auth.select(AUTH_AUTHORIZEDONLY);
            }
            _browser.getUI().debugMessage("auth index: " + _auth.getSelectionIndex() + " out of " + _auth.getItemCount());
            //todo: pick the right value here.
            //_privacy.select(PRIV_PUBLIC);
            byte avatar[] = _browser.getClient().getChannelAvatar(info.getChannelId());
            if (avatar != null)
                _avatarImage = ImageUtil.createImage(avatar);
            else
                _avatarImage = ImageUtil.ICON_QUESTION;
            _avatar.setImage(_avatarImage);
            if (info.getDescription() != null)
                _desc.setText(info.getDescription());
            else
                _desc.setText("");
            if (info.getName() != null)
                _name.setText(info.getName());
            else
                _name.setText("");
            if (info.getExpiration() <= 0)
                _expire.setText(_browser.getTranslationRegistry().getText(T_EXPIRE_NEVER, "never"));
            else
                _expire.setText(Constants.getDate(info.getExpiration()));
            StringBuffer tags = new StringBuffer();
            for (Iterator iter = info.getPublicTags().iterator(); iter.hasNext(); ) {
                tags.append(((String)iter.next()).trim()).append(" ");
            }
            tags.append(" ");
            for (Iterator iter = info.getPrivateTags().iterator(); iter.hasNext(); ) {
                tags.append(((String)iter.next()).trim()).append(" ");
            }
            _tags.setText(tags.toString().trim());

            _archiveChooser.setArchives(info.getPublicArchives(), info.getPrivateArchives());
            
            _referencesChooser.setReferences(info.getReferences());
            
            for (Iterator iter = info.getAuthorizedManagers().iterator(); iter.hasNext(); ) {
                SigningPublicKey pub = (SigningPublicKey)iter.next();
                Hash hash = pub.calculateHash();
                long id = _browser.getClient().getChannelId(hash);
                ChannelInfo managerInfo = _browser.getClient().getChannel(id);
                if ( (managerInfo != null) && (managerInfo.getName() != null) )
                    _managerList.add(managerInfo.getName());
                else
                    _managerList.add(hash.toBase64().substring(0,6));
                _managerKeys.add(pub);
            }
            
            for (Iterator iter = info.getAuthorizedPosters().iterator(); iter.hasNext(); ) {
                SigningPublicKey pub = (SigningPublicKey)iter.next();
                final Hash hash = pub.calculateHash();
                long id = _browser.getClient().getChannelId(hash);
                ChannelInfo posterInfo = _browser.getClient().getChannel(id);
                if ( (posterInfo != null) && (posterInfo.getName() != null) )
                    _posterList.add(posterInfo.getName());
                else
                    _posterList.add(hash.toBase64().substring(0,6));
                _posterKeys.add(pub);
            }
            
            _origInfo = info;
        } else {
            // new forum
            _browser.getUI().debugMessage("forum NOT loaded");
            _channelId = -1;
            _auth.select(AUTH_AUTHORIZEDONLY);
            //_privacy.select(PRIV_PUBLIC);
            _avatar.setImage(null);
            _desc.setText("");
            _name.setText("");
            _expire.setText(_browser.getTranslationRegistry().getText(T_EXPIRE_NEVER, "never"));
            _tags.setText("");
            _archiveChooser.setArchives(null, null);
            _referencesChooser.setReferences(null);
        }
        
        _managerList.setRedraw(true);
        _posterList.setRedraw(true);
        _modified = false;
    }
    
    private static final String T_REIMPORT_ERR_TITLE = "syndie.gui.manageforum.reimporterrtitle";
    private static final String T_REIMPORT_ERR_MSG = "syndie.gui.manageforum.reimporterrmsg";
    private void reimport(final String passphrase, final SyndieURI uri) {
        JobRunner.instance().enqueue(new Runnable() {
            public void run() {
                final boolean ok = _browser.reimport(uri, passphrase);
                Display.getDefault().asyncExec(new Runnable() { 
                   public void run() {
                       MessageBox box = null;
                       if (!ok) {
                           box = new MessageBox(_root.getShell(), SWT.ICON_ERROR | SWT.YES | SWT.NO);
                           box.setText(_browser.getTranslationRegistry().getText(T_REIMPORT_ERR_TITLE, "Passphrase incorrect"));
                           box.setMessage(_browser.getTranslationRegistry().getText(T_REIMPORT_ERR_MSG, "The forum metadata could not be reimported - the passphrase was not correct.  Would you like to try again?"));
                           int rc = box.open();
                           if (rc == SWT.YES)
                               setForum(uri);
                           else
                               _browser.unview(uri);
                           return;
                       } else {
                           setForum(uri);
                       }
                   }
                });
            }
        });
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(7, false));
    
        _avatar = new ImageCanvas(_root, false);
        _avatar.setLayoutData(new GridData(GridData.CENTER, GridData.CENTER, false, false, 1, 3));
        _avatar.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) {}
            public void mouseDown(MouseEvent mouseEvent) {}
            public void mouseUp(MouseEvent mouseEvent) { pickAvatar(); }
        });
        _avatar.addKeyListener(new KeyListener() {
            public void keyPressed(KeyEvent keyEvent) {}
            public void keyReleased(KeyEvent evt) {
                // escape seems to be sent to the component when hit to close the popup, at least
                // on swt-I20060602-1317-gtk-linux-x86
                if (evt.character == ' ')
                    pickAvatar();
            }
        });
        
        _nameLabel = new Label(_root, SWT.NONE);
        _nameLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _name = new Text(_root, SWT.BORDER | SWT.SINGLE | (_editable ? 0 : SWT.READ_ONLY));
        _name.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _name.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent modifyEvent) { modified(); }
        });
        
        _tagsLabel = new Label(_root, SWT.NONE);
        _tagsLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _tags = new Text(_root, SWT.BORDER | SWT.SINGLE | (_editable ? 0 : SWT.READ_ONLY));
        _tags.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _tags.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent modifyEvent) { modified(); }
        });
        
        _expireLabel = new Label(_root, SWT.NONE);
        _expireLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _expire = new Text(_root, SWT.BORDER | SWT.SINGLE | (_editable ? 0 : SWT.READ_ONLY));
        GridData gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        gd.widthHint = 50;
        _expire.setLayoutData(gd);
        _expire.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent modifyEvent) { modified(); }
        });
        
        _descLabel = new Label(_root, SWT.NONE);
        _descLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _desc = new Text(_root, SWT.BORDER | SWT.SINGLE | SWT.WRAP | (_editable ? 0 : SWT.READ_ONLY));
        _desc.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 5, 1));
        _desc.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent modifyEvent) { modified(); }
        });
        
        /*
        _privacyLabel = new Label(_root, SWT.NONE);
        _privacyLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _privacy = new Combo(_root, SWT.DROP_DOWN | (_editable ? 0 : SWT.READ_ONLY));
        _privacy.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _privacy.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { privacyUpdated(); modified(); }
            public void widgetSelected(SelectionEvent selectionEvent) { privacyUpdated(); modified(); }
        });
         */
        
        _authLabel = new Label(_root, SWT.NONE);
        _authLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _auth = new Combo(_root, SWT.DROP_DOWN | (_editable ? 0 : SWT.READ_ONLY));
        _auth.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 5, 1));
        _auth.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { modified(); }
            public void widgetSelected(SelectionEvent selectionEvent) { modified(); }
        });
    
        _detailTabs = new CTabFolder(_root, SWT.MULTI | SWT.TOP | SWT.BORDER);
        _detailTabs.setMaximizeVisible(false);
        _detailTabs.setMinimizeVisible(false);
        _detailTabs.setSimple(false);
        _detailTabs.setUnselectedImageVisible(true);
        _detailTabs.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 7, 1));
        
        _readKeysItem = new CTabItem(_detailTabs, SWT.NONE);
        _readKeysScroll = new ScrolledComposite(_detailTabs, SWT.H_SCROLL | SWT.V_SCROLL);
        _readKeys = new Composite(_readKeysScroll, SWT.NONE);
        _readKeysScroll.setContent(_readKeys);
        _readKeysScroll.setExpandHorizontal(true);
        _readKeysScroll.setExpandVertical(true);
        
        _readKeysItem.setControl(_readKeysScroll);
        _readKeys.setLayout(new GridLayout(2, false));
        
        _readKeySummary = new Label(_readKeys, SWT.WRAP | SWT.LEFT);
        _readKeySummary.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 2, 1));
        
        _readKeysCurrent = new Group(_readKeys, SWT.SHADOW_ETCHED_IN);
        _readKeysCurrent.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _readKeysCurrent.setLayout(new GridLayout(2, false));
        
        _readKeysCurrentTable = new Table(_readKeysCurrent, SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        _readKeysCurrentTable.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
        _readKeysCurrentTable.setHeaderVisible(true);
        _readKeysCurrentTable.setLinesVisible(true);
        
        _readKeysCurrentHash = new TableColumn(_readKeysCurrentTable, SWT.LEFT);
        _readKeysCurrentPublic = new TableColumn(_readKeysCurrentTable, SWT.CENTER);
        _readKeysCurrentCreatedOn = new TableColumn(_readKeysCurrentTable, SWT.LEFT);
        
        _readKeysAdd = new Button(_readKeysCurrent, SWT.PUSH);
        _readKeysAdd.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _readKeysDrop = new Button(_readKeysCurrent, SWT.PUSH);
        _readKeysDrop.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _readKeysOld = new Group(_readKeys, SWT.SHADOW_ETCHED_IN);
        _readKeysOld.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _readKeysOld.setLayout(new GridLayout(2, false));
        
        _readKeysOldTable = new Table(_readKeysOld, SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        _readKeysOldTable.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
        _readKeysOldTable.setHeaderVisible(true);
        _readKeysOldTable.setLinesVisible(true);
        
        _readKeysOldHash = new TableColumn(_readKeysOldTable, SWT.LEFT);
        _readKeysOldPublic = new TableColumn(_readKeysOldTable, SWT.CENTER);
        _readKeysOldCreatedOn = new TableColumn(_readKeysOldTable, SWT.LEFT);
        _readKeysOldDroppedOn = new TableColumn(_readKeysOldTable, SWT.LEFT);
        
        _readKeyPrivacySummary = new Label(_readKeys, SWT.WRAP | SWT.LEFT);
        _readKeyPrivacySummary.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 2, 1));
        
        _readKeyPrivacyPrompt = new Label(_readKeys, SWT.WRAP | SWT.LEFT);
        _readKeyPrivacyPrompt.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 2, 1));
        
        _readKeyPrivacyCombo = new Combo(_readKeys, SWT.DROP_DOWN | SWT.READ_ONLY);
        _readKeyPrivacyCombo.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, true, 2, 1));
        
        _managerItem = new CTabItem(_detailTabs, SWT.NONE);
        _managers = new Composite(_detailTabs, SWT.NONE);
        _managerItem.setControl(_managers);
        _managers.setLayout(new GridLayout(1, true));
        
        _managerList = new List(_managers, SWT.SINGLE | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        gd = new GridData(GridData.FILL, GridData.FILL, true, true);
        gd.heightHint = 30;
        _managerList.setLayoutData(gd);
        _managerList.getVerticalBar().setVisible(true);
        
        Menu menu = new Menu(_managerList);
        _managerList.setMenu(menu);
        
        _managerAdd = new MenuItem(menu, SWT.PUSH);
        _managerAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addManager(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addManager(); }
        });
        _managerAdd.setEnabled(_editable);
        
        _managerView = new MenuItem(menu, SWT.PUSH);
        _managerView.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewManager(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewManager(); }
        });
        
        _managerRemove = new MenuItem(menu, SWT.PUSH);
        _managerRemove.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { removeManager(); }
            public void widgetSelected(SelectionEvent selectionEvent) { removeManager(); }
        });
        _managerRemove.setEnabled(_editable);
        
        _posterItem = new CTabItem(_detailTabs, SWT.NONE);
        _posters = new Composite(_detailTabs, SWT.NONE);
        _posterItem.setControl(_posters);
        _posters.setLayout(new GridLayout(1, true));
        
        _posterList = new List(_posters, SWT.SINGLE | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        gd = new GridData(GridData.FILL, GridData.FILL, true, true);
        gd.heightHint = 30;
        _posterList.setLayoutData(gd);
        _posterList.getVerticalBar().setVisible(true);
        
        menu = new Menu(_posterList);
        _posterList.setMenu(menu);
        
        _posterAdd = new MenuItem(menu, SWT.PUSH);
        _posterAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addPoster(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addPoster(); }
        });
        _posterAdd.setEnabled(_editable);
        
        _posterView = new MenuItem(menu, SWT.PUSH);
        _posterView.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewPoster(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewPoster(); }
        });
        
        _posterRemove = new MenuItem(menu, SWT.PUSH);
        _posterRemove.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { removePoster(); }
            public void widgetSelected(SelectionEvent selectionEvent) { removePoster(); }
        });
        _posterRemove.setEnabled(_editable);
        
        _archiveItem = new CTabItem(_detailTabs, SWT.NONE);
        _archives = new Composite(_detailTabs, SWT.NONE);
        _archiveItem.setControl(_archives);
        _archives.setLayout(new FillLayout());
        //_archives.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, true, 7, 1));
        
        _archiveChooser = new ManageForumArchiveChooser(_archives, _browser, this, _editable);
        
        _referencesItem = new CTabItem(_detailTabs, SWT.NONE);
        _references = new Composite(_detailTabs, SWT.NONE);
        _referencesItem.setControl(_references);
        _references.setLayout(new FillLayout());
        //_references.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, true, 7, 1));
        
        _referencesChooser = new ManageReferenceChooser(_references, _browser, _editable);
        
        Composite actions = new Composite(_root, SWT.NONE);
        actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        gd = new GridData(GridData.FILL, GridData.BEGINNING, false, false, 7, 1);
        if (!_editable)
            gd.exclude = true;
        actions.setLayoutData(gd);

        populateCombos();
        
        _save = new Button(actions, SWT.PUSH);
        _save.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { save(); }
            public void widgetSelected(SelectionEvent selectionEvent) { save(); }
        });
        _cancel = new Button(actions, SWT.PUSH);
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { cancel(); }
            public void widgetSelected(SelectionEvent selectionEvent) { cancel(); }
        });
        
        _refChooser = new ReferenceChooserPopup(_parent.getShell(), _browser, this);
        
        pickDetail(null);
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
    }
    
    /*
    private void privacyUpdated() {
        int idx = _privacy.getSelectionIndex();
        if (idx == PRIV_PASSPHRASE) {
            PassphrasePrompt prompt = new PassphrasePrompt(_browser, _root.getShell(), true);
            if (_passphrase != null)
                prompt.setPassphrase(_passphrase);
            else
                prompt.setPassphrase("");
            if (_passphrasePrompt != null)
                prompt.setPassphrasePrompt(_passphrasePrompt);
            else
                prompt.setPassphrasePrompt("");
            prompt.setPassphraseListener(new PassphrasePrompt.PassphraseListener() {
                public void promptComplete(String passphraseEntered, String promptEntered) {
                    _passphrase = passphraseEntered;
                    _passphrasePrompt = promptEntered;
                }
                public void promptAborted() {}
            });
            prompt.open();
        }
    }
     */
    
    private static final int PRIV_PUBLIC = 0;
    private static final int PRIV_AUTHORIZED = 1;
    private static final int PRIV_PASSPHRASE = 2;
    
    private static final int AUTH_AUTHORIZEDONLY = 0;
    private static final int AUTH_UNAUTHREPLY = 1;
    private static final int AUTH_UNAUTHPOST = 2;
    
    private void populateCombos() {
        /*
        _privacy.setRedraw(false);
        int idx = PRIV_PUBLIC;
        if (_privacy.getItemCount() != 0)
            idx = _privacy.getSelectionIndex();
        _privacy.removeAll();
        
        _privacy.add(_browser.getTranslationRegistry().getText(T_PRIV_PUBLIC, "Publicly readable"));
        _privacy.add(_browser.getTranslationRegistry().getText(T_PRIV_AUTHORIZED, "Authorized readers only"));
        _privacy.add(_browser.getTranslationRegistry().getText(T_PRIV_PASSPHRASE, "Passphrase protected..."));
        _privacy.select(idx);
        _privacy.setEnabled(_editable);
        _privacy.setRedraw(true);
         */
        
        _auth.setRedraw(false);
        int idx = AUTH_AUTHORIZEDONLY;
        if (_auth.getItemCount() != 0)
            idx = _auth.getSelectionIndex();
        _auth.removeAll();
        _auth.add(_browser.getTranslationRegistry().getText(T_AUTH_AUTHONLY, "Only authorized users can post"));
        _auth.add(_browser.getTranslationRegistry().getText(T_AUTH_PUBREPLY, "Anyone can reply to authorized posts"));
        _auth.add(_browser.getTranslationRegistry().getText(T_AUTH_PUBPOST, "Anyone can post"));
        _auth.select(idx);
        _auth.setEnabled(_editable);
        _auth.setRedraw(true);
    }
    
    private static final int CHOICE_READKEYS = 0;
    private static final int CHOICE_MANAGER = 1;
    private static final int CHOICE_POSTER = 2;
    private static final int CHOICE_ARCHIVES = 3;
    private static final int CHOICE_REFS = 4;
    private static final String T_CHOICE_READKEYS = "syndie.gui.manageforum.choice.readkeys";
    private static final String T_CHOICE_MANAGER = "syndie.gui.manageforum.choice.manager";
    private static final String T_CHOICE_POSTER = "syndie.gui.manageforum.choice.poster";
    private static final String T_CHOICE_ARCHIVES = "syndie.gui.manageforum.choice.archives";
    private static final String T_CHOICE_REFS = "syndie.gui.manageforum.choice.refs";
    
    String getName() { return _name.getText().trim(); }
    String getDescription() { return _desc.getText().trim(); }
    boolean getAllowPublicPosts() { return _auth.getSelectionIndex() == AUTH_UNAUTHPOST; }
    boolean getAllowPublicReplies() { return _auth.getSelectionIndex() == AUTH_UNAUTHREPLY; }
    Set getPublicTags() {
        String tags[] = Constants.split(", \t\r\n", _tags.getText().trim(), false);
        Set rv = new HashSet();
        for (int i = 0; i < tags.length; i++)
            rv.add(tags[i]);
        return rv;
    }
    Set getAuthorizedPosters() { return new HashSet(_posterKeys); }
    Set getAuthorizedManagers() { return new HashSet(_managerKeys); }
    Set getPublicArchives() { return _archiveChooser.getPublicArchives(); }
    Set getPrivateArchives() { return _archiveChooser.getPrivateArchives(); }
    String getReferences() { return _referencesChooser.getReferences(); }
    boolean getEncryptContent() {
        return false;
        //return _privacy.getSelectionIndex() == PRIV_PASSPHRASE || 
        //       _privacy.getSelectionIndex() == PRIV_AUTHORIZED; 
    }
    long getChannelId() { return _channelId; }
    boolean getPBE() { return false; } //_privacy.getSelectionIndex() == PRIV_PASSPHRASE; }
    String getPassphrase() { return _passphrase; }
    String getPassphrasePrompt() { return _passphrasePrompt; }
    long getLastEdition() { if (_origInfo != null) return _origInfo.getEdition(); else return -1; }
    Image getAvatar() { return (_avatarImage == ImageUtil.ICON_QUESTION ? null : _avatarImage); }
    /** return the read keys we explicitly want to deliver in the metadata, or null/empty if we don't care */
    ArrayList getCurrentReadKeys() { return null; }
    
    private void save() {
        ManageForumExecutor exec = new ManageForumExecutor(_browser.getClient(), _browser.getUI(), this);
        _browser.showWaitCursor(true);
        exec.execute();
        _browser.showWaitCursor(false);
        if (exec.getErrors().length() == 0) {
            if (_listener != null)
                _listener.manageComplete(this);
            _browser.view(exec.getForum());
            _modified = false;
        }
    }
    private void cancel() {
        if (confirmClose()) {
            if (_listener != null)
                _listener.manageComplete(this);
        }
    }
    
    private static final String T_PICKAVATAR = "syndie.gui.manageforum.pickavatar";
    private static final String T_PICKAVATAR_IMAGES = "syndie.gui.manageforum.pickavatarimages";
    private static final String T_PICKAVATAR_ALL = "syndie.gui.manageforum.pickavatarall";
    
    private void pickAvatar() {
        //_browser.getUI().debugMessage("pickAvatar", new Exception("source"));
        if (!_editable) return;
        modified();
        
        FileDialog dialog = new FileDialog(_root.getShell(), SWT.OPEN);
        dialog.setText(_browser.getTranslationRegistry().getText(T_PICKAVATAR, "Select an avatar"));
        dialog.setFilterExtensions(new String[] { "*.png; *.jpeg; *.jpg; *.gif; *.ico", "*.*" });
        dialog.setFilterNames(new String[] { _browser.getTranslationRegistry().getText(T_PICKAVATAR_IMAGES, "Images"), 
                                             _browser.getTranslationRegistry().getText(T_PICKAVATAR_ALL, "All") });
        
        String file = dialog.open();
        if (file != null) {
            //ignoring it here, since we scale it down later
            //if (f.length() > Constants.MAX_AVATAR_SIZE)
            //    return;
            _avatar.setRedraw(false);
            try {
                Image img = ImageUtil.createImageFromFile(file);
                Rectangle bounds = img.getBounds();
                if ( (bounds.width != Constants.MAX_AVATAR_WIDTH) || (bounds.height != Constants.MAX_AVATAR_HEIGHT) ) {
                    img = ImageUtil.resize(img, Constants.MAX_AVATAR_WIDTH, Constants.MAX_AVATAR_HEIGHT, true);
                }
                // we could verify the size is < MAX_AVATAR_SIZE here, but no 64x64 pixel PNG should exceed it
                ImageUtil.dispose(_avatarImage);
                _avatarImage = img;
                _avatar.setImage(_avatarImage);
            } catch (IllegalArgumentException iae) {
                // invalid.  so don't touch the current avatar
            } catch (SWTException se) {
                // again, invalid.  so don't touch the current avatar
            }
            _avatar.setRedraw(true);
        } else {
            if ( (_avatarImage != null) && (_avatarImage != ImageUtil.ICON_QUESTION) ) {
                _avatar.setRedraw(false);
                ImageUtil.dispose(_avatarImage);
                _avatarImage = ImageUtil.ICON_QUESTION;
                _avatar.setImage(_avatarImage);
                _avatar.setRedraw(true);
            }
        }
    }
    
    private void addManager() {
        modified();
        _addingPoster = false;
        _refChooser.show();
    }
    private void addPoster() {
        modified();
        _addingPoster = true;
        _refChooser.show();
    }

    private void viewManager() {
        int idx = _managerList.getSelectionIndex();
        if (idx < 0) return;
        _browser.view(SyndieURI.createScope(((SigningPublicKey)_managerKeys.get(idx)).calculateHash()));
    }
    private void viewPoster() {
        int idx = _posterList.getSelectionIndex();
        if (idx < 0) return;
        _browser.view(SyndieURI.createScope(((SigningPublicKey)_posterKeys.get(idx)).calculateHash()));
    }
    private void removeManager() {
        modified();
        int idx = _managerList.getSelectionIndex();
        if (idx < 0) return;
        _managerKeys.remove(idx);
        _managerList.remove(idx);
    }
    private void removePoster() {
        modified();
        int idx = _posterList.getSelectionIndex();
        if (idx < 0) return;
        _posterKeys.remove(idx);
        _posterList.remove(idx);
    }
    
    public void referenceAccepted(SyndieURI uri) {
        modified();
        _browser.getUI().debugMessage("ref selected: " + uri);
        long id = _browser.getClient().getChannelId(uri.getScope());
        ChannelInfo info = _browser.getClient().getChannel(id);
        if (info != null) {
            if (_addingPoster) {
                if (!_posterKeys.contains(info.getIdentKey())) {
                    _browser.getUI().debugMessage("adding a new poster: " + info.getName());
                    
                    if ( (info != null) && (info.getName() != null) )
                        _posterList.add(info.getName());
                    else
                        _posterList.add(info.getChannelHash().toBase64().substring(0,6));
                    
                    _posterKeys.add(info.getIdentKey());
                } else {
                    _browser.getUI().debugMessage("poster already added: " + info.getName());
                }
            } else {
                if (!_managerKeys.contains(info.getIdentKey())) {
                    _browser.getUI().debugMessage("adding a new manager: " + info.getName());

                    if ( (info != null) && (info.getName() != null) )
                        _managerList.add(info.getName());
                    else
                        _managerList.add(info.getChannelHash().toBase64().substring(0,6));
                    
                    
                    _managerKeys.add(info.getIdentKey());

                } else {
                    _browser.getUI().debugMessage("manager already added: " + info.getName());
                }
            }
        }
        _refChooser.hide();
    }

    public void referenceChoiceAborted() { _refChooser.hide(); }
    
    private void modified() { 
        if (!_modified) {
            _modified = true;
            //_browser.getUI().debugMessage("modified", new Exception("modified by"));
        }
    }
    
    public boolean confirmClose() {
        if (!_editable || !_modified) return true;
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
    
    public void dispose() { 
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
        _archiveChooser.dispose();
        _referencesChooser.dispose();
        ImageUtil.dispose(_avatarImage);
    }

    private static final String T_PRIV_PUBLIC = "syndie.gui.manageforum.priv.public";
    private static final String T_PRIV_AUTHORIZED = "syndie.gui.manageforum.priv.authorized";
    private static final String T_PRIV_PASSPHRASE = "syndie.gui.manageforum.priv.pbe";
    
    private static final String T_AUTH_AUTHONLY = "syndie.gui.manageforum.auth.authonly";
    private static final String T_AUTH_PUBREPLY = "syndie.gui.manageforum.auth.pubreply";
    private static final String T_AUTH_PUBPOST = "syndie.gui.manageforum.auth.pubpost";
    
    private static final String T_CONFIRM_CLOSE_TITLE = "syndie.gui.manageforum.close.title";
    private static final String T_CONFIRM_CLOSE_MSG = "syndie.gui.manageforum.close.msg";
    
    private static final String T_EXPIRE_NEVER = "syndie.gui.manageforum.expire.never";
    private static final String T_NAME = "syndie.gui.manageforum.name";
    private static final String T_TAGS = "syndie.gui.manageforum.tags";
    private static final String T_EXPIRATION = "syndie.gui.manageforum.expiration";
    private static final String T_DESC = "syndie.gui.manageforum.desc";
    //private static final String T_PRIVACY = "syndie.gui.manageforum.privacy";
    private static final String T_AUTHORIZATION = "syndie.gui.manageforum.authorization";
    private static final String T_CHOOSER = "syndie.gui.manageforum.chooserlabel";
    private static final String T_MANAGERS = "syndie.gui.manageforum.managers";
    private static final String T_MANAGERS_ADD = "syndie.gui.manageforum.managers.add";
    private static final String T_MANAGERS_VIEW = "syndie.gui.manageforum.managers.view";
    private static final String T_MANAGERS_REMOVE = "syndie.gui.manageforum.managers.remove";
    private static final String T_POSTERS = "syndie.gui.manageforum.posters";
    private static final String T_POSTERS_ADD = "syndie.gui.manageforum.posters.add";
    private static final String T_POSTERS_VIEW = "syndie.gui.manageforum.posters.view";
    private static final String T_POSTERS_REMOVE = "syndie.gui.manageforum.posters.remove";
    private static final String T_ARCHIVES = "syndie.gui.manageforum.archives";
    private static final String T_REFERENCES = "syndie.gui.manageforum.references";
    private static final String T_SAVE = "syndie.gui.manageforum.save";
    private static final String T_CANCEL = "syndie.gui.manageforum.cancel";
    
    private static final String T_READKEYSUMMARY = "syndie.gui.manageforum.readkeysummary";
    private static final String T_READKEYPRIVSUMMARY = "syndie.gui.manageforum.readkeyprivsummary";
    private static final String T_READKEYPRIVPROMPT = "syndie.gui.manageforum.readkeyprivprompt";
    private static final String T_READKEYPRIV_EXISTING = "syndie.gui.manageforum.readkeypriv.existing";
    private static final String T_READKEYPRIV_PUBLIC = "syndie.gui.manageforum.readkeypriv.public";
    private static final String T_READKEYPRIV_PBE = "syndie.gui.manageforum.readkeypriv.pbe";
    
    private static final String T_READKEYSCURRENT = "syndie.gui.manageforum.readkeyscurrent";
    private static final String T_READKEYSOLD = "syndie.gui.manageforum.readkeysold";
    private static final String T_READKEYSADD = "syndie.gui.manageforum.readkeysadd";
    private static final String T_READKEYSDROP = "syndie.gui.manageforum.readkeysdrop";

    private static final String T_READKEYSCURRENTHASH = "syndie.gui.manageforum.readkeyscurrenthash";
    private static final String T_READKEYSCURRENTCREATEDON = "syndie.gui.manageforum.readkeyscurrentcreatedon";
    private static final String T_READKEYSCURRENTPUBLIC = "syndie.gui.manageforum.readkeyscurrentpublic";

    private static final String T_READKEYSOLDHASH = "syndie.gui.manageforum.readkeysoldhash";
    private static final String T_READKEYSOLDCREATEDON = "syndie.gui.manageforum.readkeysoldcreatedon";
    private static final String T_READKEYSOLDDROPPEDON = "syndie.gui.manageforum.readkeysolddroppedon";
    private static final String T_READKEYSOLDPUBLIC = "syndie.gui.manageforum.readkeysoldpublic";

    public void translate(TranslationRegistry registry) {
        _nameLabel.setText(registry.getText(T_NAME, "Name:"));
        _tagsLabel.setText(registry.getText(T_TAGS, "Tags:"));
        _expireLabel.setText(registry.getText(T_EXPIRATION, "Expiration:"));
        _descLabel.setText(registry.getText(T_DESC, "Description:"));
        //_privacyLabel.setText(registry.getText(T_PRIVACY, "Privacy:"));
        _authLabel.setText(registry.getText(T_AUTHORIZATION, "Authorization:"));
        //_managers.setText(registry.getText(T_MANAGERS, "Managers"));
        _managerAdd.setText(registry.getText(T_MANAGERS_ADD, "add"));
        _managerView.setText(registry.getText(T_MANAGERS_VIEW, "view"));
        _managerRemove.setText(registry.getText(T_MANAGERS_REMOVE, "remove"));
        //_posters.setText(registry.getText(T_POSTERS, "Posters"));
        _posterAdd.setText(registry.getText(T_POSTERS_ADD, "add"));
        _posterView.setText(registry.getText(T_POSTERS_VIEW, "view"));
        _posterRemove.setText(registry.getText(T_POSTERS_REMOVE, "remove"));
        //_archives.setText(registry.getText(T_ARCHIVES, "Archives"));
        //_references.setText(registry.getText(T_REFERENCES, "References"));
        _save.setText(registry.getText(T_SAVE, "Save"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel"));

        _readKeySummary.setText(registry.getText(T_READKEYSUMMARY, "Anyone with the \"read key\" that a message used to secure it " +
                "wil be able to view the message.  Some messages publish that \"read key\" so anyone can read it, others derive the" +
                "\"read key\" from a passphrase, and others use one of the forum's predefined \"read keys\".  The later are managed here:"));
        _readKeyPrivacySummary.setText(registry.getText(T_READKEYPRIVSUMMARY, "The current read keys will be published as part of " +
                "the updated forum metadata, but hidden inside the secured area.  That secured area can be publicly readable, " +
                "thereby giving everyone access to these keys, or it can be protected - either by a passphrase or by one of the existing " +
                "read keys (so only those already authorized to read the forum will know these new keys, or be able to view any of the published " +
                "forum details)"));
        _readKeyPrivacyPrompt.setText(registry.getText(T_READKEYPRIVPROMPT, "How would you like to protect the secured area?"));
        
        int idx = -1;
        if (_readKeyPrivacyCombo.getItemCount() > 0)
            idx = _readKeyPrivacyCombo.getSelectionIndex();
        _readKeyPrivacyCombo.removeAll();
        _readKeyPrivacyCombo.add(registry.getText(T_READKEYPRIV_EXISTING, "Use one of the existing keys"));
        _readKeyPrivacyCombo.add(registry.getText(T_READKEYPRIV_PUBLIC, "Make the keys and metadata publicly readable"));
        _readKeyPrivacyCombo.add(registry.getText(T_READKEYPRIV_PBE, "Require a passphrase to read the keys and metadata"));
        if (idx >= 0)
            _readKeyPrivacyCombo.select(idx);

        _readKeysCurrent.setText(registry.getText(T_READKEYSCURRENT, "Current read keys:"));
        _readKeysOld.setText(registry.getText(T_READKEYSOLD, "Old read keys:"));
        _readKeysAdd.setText(registry.getText(T_READKEYSADD, "Add new"));
        _readKeysDrop.setText(registry.getText(T_READKEYSDROP, "Drop selected"));

        _readKeysCurrentHash.setText(registry.getText(T_READKEYSCURRENTHASH, "Key"));
        _readKeysCurrentPublic.setText(registry.getText(T_READKEYSCURRENTPUBLIC, "Was public?"));
        _readKeysCurrentCreatedOn.setText(registry.getText(T_READKEYSCURRENTCREATEDON, "Created on"));
        
        _readKeysOldHash.setText(registry.getText(T_READKEYSOLDHASH, "Key"));
        _readKeysOldPublic.setText(registry.getText(T_READKEYSOLDPUBLIC, "Was public?"));
        _readKeysOldCreatedOn.setText(registry.getText(T_READKEYSOLDCREATEDON, "Created on"));
        _readKeysOldDroppedOn.setText(registry.getText(T_READKEYSOLDDROPPEDON, "Dropped on"));
        
        _readKeysItem.setText(registry.getText(T_CHOICE_READKEYS, "Read keys"));
        _managerItem.setText(registry.getText(T_CHOICE_MANAGER, "Authorized managers"));
        _posterItem.setText(registry.getText(T_CHOICE_POSTER, "Authorized posters"));
        _archiveItem.setText(registry.getText(T_CHOICE_ARCHIVES, "Archives"));
        _referencesItem.setText(registry.getText(T_CHOICE_REFS, "References"));
        
        populateCombos();
    }
    
    public void applyTheme(Theme theme) {
        _auth.setFont(theme.DEFAULT_FONT);
        _authLabel.setFont(theme.DEFAULT_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
        _desc.setFont(theme.DEFAULT_FONT);
        _descLabel.setFont(theme.DEFAULT_FONT);
        _detailTabs.setFont(theme.TAB_FONT);
        _expire.setFont(theme.DEFAULT_FONT);
        _expireLabel.setFont(theme.DEFAULT_FONT);
        _name.setFont(theme.DEFAULT_FONT);
        _nameLabel.setFont(theme.DEFAULT_FONT);
        //_privacy.setFont(theme.DEFAULT_FONT);
        //_privacyLabel.setFont(theme.DEFAULT_FONT);
        _save.setFont(theme.BUTTON_FONT);
        _tags.setFont(theme.DEFAULT_FONT);
        _tagsLabel.setFont(theme.DEFAULT_FONT);
        _managerList.setFont(theme.DEFAULT_FONT);
        _posterList.setFont(theme.DEFAULT_FONT);
        
        _readKeysCurrentTable.setFont(theme.TABLE_FONT);
        _readKeysOldTable.setFont(theme.TABLE_FONT);
        
        _readKeysCurrent.setFont(theme.DEFAULT_FONT);
        _readKeysOld.setFont(theme.DEFAULT_FONT);
        _readKeysAdd.setFont(theme.BUTTON_FONT);
        _readKeysDrop.setFont(theme.BUTTON_FONT);
        
        _readKeyPrivacyCombo.setFont(theme.DEFAULT_FONT);
        _readKeyPrivacyPrompt.setFont(theme.DEFAULT_FONT);
        _readKeyPrivacySummary.setFont(theme.DEFAULT_FONT);
        _readKeySummary.setFont(theme.DEFAULT_FONT);
                
        _readKeysCurrentCreatedOn.pack();
        _readKeysCurrentHash.pack();
        _readKeysCurrentPublic.pack();
        
        _readKeysOldCreatedOn.pack();
        _readKeysOldDroppedOn.pack();
        _readKeysOldHash.pack();
        _readKeysOldPublic.pack();

        //_readKeys.layout(true, true);
        _readKeys.pack(true);
        _readKeysScroll.setMinSize(_readKeys.computeSize(SWT.DEFAULT, SWT.DEFAULT));
    }
}
