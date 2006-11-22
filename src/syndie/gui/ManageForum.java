package syndie.gui;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import net.i2p.data.Hash;
import net.i2p.data.SigningPrivateKey;
import net.i2p.data.SigningPublicKey;
import org.eclipse.swt.SWT;
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
class ManageForum implements ReferenceChooserTree.AcceptanceListener {
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
    
    private Group _managers;
    private List _managerList;
    private Button _managerAdd;
    
    private Group _posters;
    private List _posterList;
    private Button _posterAdd;
    
    private Group _archives;
    private Composite _archiveElements;
    private Button _archiveAdd;
    
    private Group _references;

    private Button _save;
    private Button _cancel;
    
    private ManageForumListener _listener;
    
    private long _channelId;
    private ChannelInfo _origInfo;
    private ArrayList _archiveInfos;
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
        _archiveInfos = new ArrayList();
        _managerKeys = new ArrayList();
        _posterKeys = new ArrayList();
        initComponents();
    }
    
    public interface ManageForumListener {
        public void manageComplete(ManageForum manage);
    }
    
    private void clearChildren(Composite container) {
        Control children[] = container.getChildren();
        for (int i = 0; i < children.length; i++)
            children[i].dispose();
    }
    
    public void setForum(SyndieURI uri) {
        _archiveInfos.clear();
        _managerKeys.clear();
        _posterKeys.clear();
        _origInfo = null;
        _channelId = -1;
        
        _archiveElements.setRedraw(false);
        _managerList.setRedraw(false);
        _posterList.setRedraw(false);
        
        clearChildren(_archiveElements);
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
                _expire.setText("never");
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
            
            for (Iterator iter = info.getPublicArchives().iterator(); iter.hasNext(); ) {
                final ArchiveInfo archive = (ArchiveInfo)iter.next();
                Button b = new Button(_archiveElements, SWT.PUSH);
                if (archive.getURI().isURL()) {
                    String url = archive.getURI().getURL();
                    if (url.startsWith("http")) {
                        // format http/https urls one way...
                        b.setText(CommandImpl.strip(url, " \r\n\t<>", '_'));
                        b.setToolTipText("http archive");
                    } else if (url.startsWith("USK@") || url.startsWith("SSK@") || url.startsWith("CHK@")) {
                        // perhaps some special treatment for freenet keys?
                        b.setText(CommandImpl.strip(url, " \r\n\t<>", '_'));
                        b.setToolTipText("freenet archive");
                    } else {
                        // everything else...
                        b.setText(CommandImpl.strip(url, " \r\n\t<>", '_'));
                    }
                    _archiveInfos.add(archive);
                } else {
                    // unknown
                    b.setText("unknown archive uri");
                    b.setToolTipText(archive.getURI().toString());
                }
                b.addSelectionListener(new SelectionListener() {
                    public void widgetDefaultSelected(SelectionEvent selectionEvent) { _browser.view(archive.getURI()); }
                    public void widgetSelected(SelectionEvent selectionEvent) { _browser.view(archive.getURI()); }
                });
            }
            
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
            _expire.setText("never");
            _tags.setText("");
        }
        
        _archiveElements.setRedraw(true);
        _managerList.setRedraw(true);
        _posterList.setRedraw(true);
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(7, false));
    
        _avatar = new ImageCanvas(_root, false);
        _avatar.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false, 1, 3));
        
        _nameLabel = new Label(_root, SWT.NONE);
        _nameLabel.setText("Name:");
        _nameLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _name = new Text(_root, SWT.BORDER | SWT.SINGLE);
        _name.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _tagsLabel = new Label(_root, SWT.NONE);
        _tagsLabel.setText("Tags:");
        _tagsLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _tags = new Text(_root, SWT.BORDER | SWT.SINGLE);
        _tags.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _expireLabel = new Label(_root, SWT.NONE);
        _expireLabel.setText("Expiration:");
        _expireLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _expire = new Text(_root, SWT.BORDER | SWT.SINGLE);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        gd.widthHint = 50;
        _expire.setLayoutData(gd);
        
        _descLabel = new Label(_root, SWT.NONE);
        _descLabel.setText("Description:");
        _descLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _desc = new Text(_root, SWT.BORDER | SWT.SINGLE | SWT.WRAP);
        _desc.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 5, 1));
        
        _privacyLabel = new Label(_root, SWT.NONE);
        _privacyLabel.setText("Privacy:");
        _privacyLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _privacy = new Combo(_root, SWT.DROP_DOWN);
        _privacy.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _authLabel = new Label(_root, SWT.NONE);
        _authLabel.setText("Authorization:");
        _authLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _auth = new Combo(_root, SWT.DROP_DOWN);
        _auth.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));

        Composite authRow = new Composite(_root, SWT.NONE);
        authRow.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 7, 1));
        authRow.setLayout(new GridLayout(2, true));
        
        _managers = new Group(authRow, SWT.SHADOW_ETCHED_IN);
        _managers.setText("Managers");
        _managers.setLayout(new GridLayout(1, true));
        _managers.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _managerList = new List(_managers, SWT.SINGLE | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        gd = new GridData(GridData.FILL, GridData.FILL, true, true);
        gd.heightHint = 50;
        _managerList.setLayoutData(gd);
        _managerList.getVerticalBar().setVisible(true);
        
        Menu menu = new Menu(_managerList);
        _managerList.setMenu(menu);
        MenuItem view = new MenuItem(menu, SWT.PUSH);
        view.setText("view");
        view.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewManager(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewManager(); }
        });
        
        MenuItem remove = new MenuItem(menu, SWT.PUSH);
        remove.setText("remove");
        remove.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { removeManager(); }
            public void widgetSelected(SelectionEvent selectionEvent) { removeManager(); }
        });
        
        _managerAdd = new Button(_managers, SWT.PUSH);
        _managerAdd.setText("Add");
        _managerAdd.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _managerAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addManager(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addManager(); }
        });
        
        _posters = new Group(authRow, SWT.SHADOW_ETCHED_IN);
        _posters.setText("Posters");
        _posters.setLayout(new GridLayout(1, true));
        _posters.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _posterList = new List(_posters, SWT.SINGLE | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        gd = new GridData(GridData.FILL, GridData.FILL, true, true);
        gd.heightHint = 50;
        _posterList.setLayoutData(gd);
        _posterList.getVerticalBar().setVisible(true);
        
        menu = new Menu(_posterList);
        _posterList.setMenu(menu);
        view = new MenuItem(menu, SWT.PUSH);
        view.setText("view");
        view.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewPoster(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewPoster(); }
        });
        
        remove = new MenuItem(menu, SWT.PUSH);
        remove.setText("remove");
        remove.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { removePoster(); }
            public void widgetSelected(SelectionEvent selectionEvent) { removePoster(); }
        });
        
        _posterAdd = new Button(_posters, SWT.PUSH);
        _posterAdd.setText("Add");
        _posterAdd.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _posterAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addPoster(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addPoster(); }
        });
        
        _archives = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _archives.setText("Archives");
        _archives.setLayout(new GridLayout(2, false));
        _archives.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false, 7, 1));
        
        _archiveElements = new Composite(_archives, SWT.NONE);
        RowLayout rl = new RowLayout(SWT.HORIZONTAL);
        rl.wrap = true;
        _archiveElements.setLayout(rl);
        _archiveElements.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _archiveAdd = new Button(_archives, SWT.PUSH);
        _archiveAdd.setText("Add");
        _archiveAdd.setLayoutData(new GridData(GridData.END, GridData.FILL, false, false));
        
        _references = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _references.setText("References");
        _references.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false, 7, 1));
        
        Composite actions = new Composite(_root, SWT.NONE);
        actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        actions.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, true, 7, 1));

        populateCombos();
        
        _save = new Button(actions, SWT.PUSH);
        _save.setText("Save");
        _save.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { save(); }
            public void widgetSelected(SelectionEvent selectionEvent) { save(); }
        });
        _cancel = new Button(actions, SWT.PUSH);
        _cancel.setText("Cancel");
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { cancel(); }
            public void widgetSelected(SelectionEvent selectionEvent) { cancel(); }
        });
        
        _refChooser = new ReferenceChooserPopup(_parent.getShell(), _browser.getUI(), _browser.getClient(), this);
    }
    
    private static final int PRIV_PUBLIC = 0;
    private static final int PRIV_AUTHORIZED = 1;
    private static final int PRIV_PASSPHRASE = 2;
    
    private static final int AUTH_AUTHORIZEDONLY = 0;
    private static final int AUTH_UNAUTHREPLY = 1;
    private static final int AUTH_UNAUTHPOST = 2;
    
    private void populateCombos() {
        _privacy.setRedraw(false);
        _privacy.removeAll();
        _privacy.add("Publicly readable");
        _privacy.add("Authorized readers only");
        _privacy.add("Passphrase protected...");
        _privacy.select(PRIV_PUBLIC);
        _privacy.setRedraw(true);
        
        _auth.setRedraw(false);
        _auth.removeAll();
        _auth.add("Only authorized users can post");
        _auth.add("Anyone can reply to authorized posts");
        _auth.add("Anyone can post");
        _auth.select(AUTH_AUTHORIZEDONLY);
        _auth.setRedraw(true);
    }
    
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
    Set getPublicArchives() { return new HashSet(_archiveInfos); }
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
        confirm.setText("Confirm");
        confirm.setMessage("Do you want to discard these changes to the forum?");
        int rc = confirm.open();
        if (rc == SWT.YES) {
            return true;
        } else if (rc == SWT.NO) {
            return false;
        } else {
            return false;
        }
    }
}
