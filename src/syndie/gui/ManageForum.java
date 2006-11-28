package syndie.gui;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import net.i2p.data.Hash;
import net.i2p.data.SigningPrivateKey;
import net.i2p.data.SigningPublicKey;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
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
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.ArchiveInfo;
import syndie.data.ChannelInfo;
import syndie.data.NymKey;
import syndie.data.SyndieURI;
import syndie.db.CommandImpl;

/**
 *
 */
class ManageForum implements ReferenceChooserTree.AcceptanceListener, Translatable {
    private Composite _parent;
    private Composite _root;
    private BrowserControl _browser;
    
    private ImageCanvas _avatar;
    private Label _nameLabel;
    private Text _name;
    private Label _tagsLabel;
    private Text _tags;
    private Label _expireLabel;
    private Text _expire;
    private Label _descLabel;
    private Text _desc;
    private Label _privacyLabel;
    private Combo _privacy;
    private Label _authLabel;
    private Combo _auth;

    private Label _chooserLabel;
    private Combo _chooserChoice;
    
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
    private ManageForumReferenceChooser _referencesChooser;

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
    
    public ManageForum(BrowserControl browser, Composite parent, ManageForumListener lsnr) {
        _parent = parent;
        _browser = browser;
        _listener = lsnr;
        _managerKeys = new ArrayList();
        _posterKeys = new ArrayList();
        initComponents();
    }
    
    public interface ManageForumListener {
        public void manageComplete(ManageForum manage);
    }
    
    public void setForum(SyndieURI uri) {
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
                _browser.getUI().errorMessage("You are not authorized to manage this channel");
                info = null;
            } else {
                _browser.getUI().debugMessage("explicit forum key found");
            }
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
            _privacy.select(PRIV_PUBLIC);
            _avatar.setImage(null);
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
            _browser.getUI().debugMessage("forum NOT loaded");
            _channelId = -1;
            _auth.select(AUTH_AUTHORIZEDONLY);
            _privacy.select(PRIV_PUBLIC);
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
    }
    
    private StackLayout _stack;
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(7, false));
    
        _avatar = new ImageCanvas(_root, false);
        _avatar.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false, 1, 3));
        
        _nameLabel = new Label(_root, SWT.NONE);
        _nameLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _name = new Text(_root, SWT.BORDER | SWT.SINGLE);
        _name.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _tagsLabel = new Label(_root, SWT.NONE);
        _tagsLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _tags = new Text(_root, SWT.BORDER | SWT.SINGLE);
        _tags.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _expireLabel = new Label(_root, SWT.NONE);
        _expireLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _expire = new Text(_root, SWT.BORDER | SWT.SINGLE);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        gd.widthHint = 50;
        _expire.setLayoutData(gd);
        
        _descLabel = new Label(_root, SWT.NONE);
        _descLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _desc = new Text(_root, SWT.BORDER | SWT.SINGLE | SWT.WRAP);
        _desc.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 5, 1));
        
        _privacyLabel = new Label(_root, SWT.NONE);
        _privacyLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _privacy = new Combo(_root, SWT.DROP_DOWN);
        _privacy.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _authLabel = new Label(_root, SWT.NONE);
        _authLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _auth = new Combo(_root, SWT.DROP_DOWN);
        _auth.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        
        _chooserLabel = new Label(_root, SWT.NONE);
        _chooserLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false, 2, 1));
        
        _chooserChoice = new Combo(_root, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.BORDER);
        _chooserChoice.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 5, 1));
        _chooserChoice.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { choose(); }
            public void widgetSelected(SelectionEvent selectionEvent) { choose(); }
        });
        
        _stack = new StackLayout();
        Composite selected = new Composite(_root, SWT.NONE);
        selected.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 7, 1));
        selected.setLayout(_stack);
        
        _managers = new Composite(selected, SWT.NONE);
        _managers.setLayout(new GridLayout(1, true));
        _managers.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
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
        
        _posters = new Composite(selected, SWT.NONE);
        _posters.setLayout(new GridLayout(1, true));
        _posters.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
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
        
        _archives = new Composite(selected, SWT.NONE);
        _archives.setLayout(new FillLayout());
        //_archives.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, true, 7, 1));
        _archives.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _archiveChooser = new ManageForumArchiveChooser(_archives, _browser, this);
        
        _references = new Composite(selected, SWT.NONE);
        _references.setLayout(new FillLayout());
        //_references.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, true, 7, 1));
        _references.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _referencesChooser = new ManageForumReferenceChooser(_references, _browser, this);
        
        Composite actions = new Composite(_root, SWT.NONE);
        actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        actions.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false, 7, 1));

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
        
        choose();
        
        _browser.getTranslationRegistry().register(this);
    }
    
    private void choose() {
        // update _stack.topLayout
        switch (_chooserChoice.getSelectionIndex()) {
            case CHOICE_ARCHIVES:
                _stack.topControl = _archives;
                break;
            case CHOICE_POSTER:
                _stack.topControl = _posters;
                break;
            case CHOICE_REFS:
                _stack.topControl = _references;
                break;
            case CHOICE_MANAGER:
            default:
                _stack.topControl = _managers;
                break;
        }
        _archives.getParent().layout();
    }
    
    private static final int PRIV_PUBLIC = 0;
    private static final int PRIV_AUTHORIZED = 1;
    private static final int PRIV_PASSPHRASE = 2;
    
    private static final int AUTH_AUTHORIZEDONLY = 0;
    private static final int AUTH_UNAUTHREPLY = 1;
    private static final int AUTH_UNAUTHPOST = 2;
    
    private void populateCombos() {
        _privacy.setRedraw(false);
        int idx = PRIV_PUBLIC;
        if (_privacy.getItemCount() != 0)
            idx = _privacy.getSelectionIndex();
        _privacy.removeAll();
        
        _privacy.add(_browser.getTranslationRegistry().getText(T_PRIV_PUBLIC, "Publicly readable"));
        _privacy.add(_browser.getTranslationRegistry().getText(T_PRIV_AUTHORIZED, "Authorized readers only"));
        _privacy.add(_browser.getTranslationRegistry().getText(T_PRIV_PASSPHRASE, "Passphrase protected..."));
        _privacy.select(idx);
        _privacy.setRedraw(true);
        
        _auth.setRedraw(false);
        idx = AUTH_AUTHORIZEDONLY;
        if (_auth.getItemCount() != 0)
            idx = _auth.getSelectionIndex();
        _auth.removeAll();
        _auth.add(_browser.getTranslationRegistry().getText(T_AUTH_AUTHONLY, "Only authorized users can post"));
        _auth.add(_browser.getTranslationRegistry().getText(T_AUTH_PUBREPLY, "Anyone can reply to authorized posts"));
        _auth.add(_browser.getTranslationRegistry().getText(T_AUTH_PUBPOST, "Anyone can post"));
        _auth.select(idx);
        _auth.setRedraw(true);
        
        _chooserChoice.setRedraw(false);
        idx = CHOICE_MANAGER;
        if (_chooserChoice.getItemCount() != 0)
            idx = _chooserChoice.getSelectionIndex();
        _chooserChoice.removeAll();
        _chooserChoice.add(_browser.getTranslationRegistry().getText(T_CHOICE_MANAGER, "Authorized managers"));
        _chooserChoice.add(_browser.getTranslationRegistry().getText(T_CHOICE_POSTER, "Authorized posters"));
        _chooserChoice.add(_browser.getTranslationRegistry().getText(T_CHOICE_ARCHIVES, "Archives"));
        _chooserChoice.add(_browser.getTranslationRegistry().getText(T_CHOICE_REFS, "References"));
        _chooserChoice.select(idx);
        _chooserChoice.setRedraw(true);        
    }
    
    private static final int CHOICE_MANAGER = 0;
    private static final int CHOICE_POSTER = 1;
    private static final int CHOICE_ARCHIVES = 2;
    private static final int CHOICE_REFS = 3;
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
        return _privacy.getSelectionIndex() == PRIV_PASSPHRASE || 
               _privacy.getSelectionIndex() == PRIV_AUTHORIZED; 
    }
    long getChannelId() { return _channelId; }
    boolean getPBE() { return _privacy.getSelectionIndex() == PRIV_PASSPHRASE; }
    String getPassphrase() { return _passphrase; }
    String getPassphrasePrompt() { return _passphrasePrompt; }
    long getLastEdition() { if (_origInfo != null) return _origInfo.getEdition(); else return -1; }
    
    private void save() {
        ManageForumExecutor exec = new ManageForumExecutor(_browser.getClient(), _browser.getUI(), this);
        _browser.showWaitCursor(true);
        exec.execute();
        _browser.showWaitCursor(false);
        if (exec.getErrors().length() == 0) {
            if (_listener != null)
                _listener.manageComplete(this);
            _browser.view(exec.getForum());
        }
    }
    private void cancel() {
        if (confirmClose()) {
            if (_listener != null)
                _listener.manageComplete(this);
        }
    }
    
    private void addManager() {
        _addingPoster = false;
        _refChooser.show();
    }
    private void addPoster() {
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
        int idx = _managerList.getSelectionIndex();
        if (idx < 0) return;
        _managerKeys.remove(idx);
        _managerList.remove(idx);
    }
    private void removePoster() {
        int idx = _posterList.getSelectionIndex();
        if (idx < 0) return;
        _posterKeys.remove(idx);
        _posterList.remove(idx);
    }
    
    public void referenceAccepted(SyndieURI uri) {
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
    
    public boolean confirmClose() {
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
        _archiveChooser.dispose();
        _referencesChooser.dispose();
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
    private static final String T_PRIVACY = "syndie.gui.manageforum.privacy";
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
    
    public void translate(TranslationRegistry registry) {
        _nameLabel.setText(registry.getText(T_NAME, "Name:"));
        _tagsLabel.setText(registry.getText(T_TAGS, "Tags:"));
        _expireLabel.setText(registry.getText(T_EXPIRATION, "Expiration:"));
        _descLabel.setText(registry.getText(T_DESC, "Description:"));
        _privacyLabel.setText(registry.getText(T_PRIVACY, "Privacy:"));
        _authLabel.setText(registry.getText(T_AUTHORIZATION, "Authorization:"));
        _chooserLabel.setText(registry.getText(T_CHOOSER, "Manage attributes:"));
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
        
        populateCombos();
    }
}
