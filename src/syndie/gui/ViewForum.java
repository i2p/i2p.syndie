package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Text;
import syndie.data.SyndieURI;

/**
 *
 */
class ViewForum implements Translatable, Themeable {
    private BrowserControl _browser;
    private Composite _parent;
    private SyndieURI _uri;
    
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
    private Button _referencesAdd;
    private Button _referencesEdit;
    private Button _referencesDelete;
    private Group _userGroup;
    private Group _archiveGroup;
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
    
    public ViewForum(BrowserControl browser, Composite parent, SyndieURI uri) {
        _browser = browser;
        _parent = parent;
        _uri = uri;
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
        
        _name = new Text(_root, SWT.BORDER | SWT.SINGLE);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        gd.widthHint = 100;
        _name.setLayoutData(gd);
        
        _tagsLabel = new Label(_root, SWT.NONE);
        _tagsLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _tags = new Text(_root, SWT.BORDER | SWT.SINGLE);
        _tags.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        
        _descriptionLabel = new Label(_root, SWT.NONE);
        _descriptionLabel.setLayoutData(new GridData(GridData.END, GridData.BEGINNING, false, false));
        
        _description = new Text(_root, SWT.BORDER | SWT.SINGLE);
        _description.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 5, 1));
        
        _avatarSelect = new Button(_root, SWT.PUSH);
        _avatarSelect.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _avatarMenu = new Menu(_avatar);
        _avatar.setMenu(_avatarMenu);
        _avatarSelect.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _avatarMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _avatarMenu.setVisible(true); }
        });
        
        _authorizationLabel = new Label(_root, SWT.NONE);
        _authorizationLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _authorization = new Combo(_root, SWT.READ_ONLY | SWT.DROP_DOWN);
        _authorization.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        
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
        _userGroup.setLayout(new GridLayout(1, false));
        
        _archiveGroup = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _archiveGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 7, 1));
        _archiveGroup.setLayout(new GridLayout(1, false));
        
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
    
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
    }
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
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
        
        _avatarSelect.setFont(theme.BUTTON_FONT);
        _referencesAdd.setFont(theme.BUTTON_FONT);
        _referencesEdit.setFont(theme.BUTTON_FONT);
        _referencesDelete.setFont(theme.BUTTON_FONT);

        _root.layout(true, true);
        _scroll.setMinSize(_root.computeSize(SWT.DEFAULT, SWT.DEFAULT));
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
        _keyManagementGroup.setText(registry.getText(T_KEYMGMT, "Key management:"));
        _keyManagementOpen.setText(registry.getText(T_KEYMGMT_OPEN, "Open access"));
        _keyManagementOpenInfo.setText(registry.getText(T_KEYMGMT_OPEN_INFO, "This creates a new key for reading messages and publicizes it so that anyone can read with it"));
        _keyManagementKeep.setText(registry.getText(T_KEYMGMT_KEEP, "Keep existing keys"));
        _keyManagementKeepInfo.setText(registry.getText(T_KEYMGMT_KEEP_INFO, "This uses existing keys, publicizing them if they used to be public.  If the keys were passphrase protected before however, new users will not be able to enter the passphrase and gain access to these existing keys.  To do so, require a passphrase again here instead."));
        _keyManagementRotate.setText(registry.getText(T_KEYMGMT_ROTATE, "Rotate keys for authorized readers"));
        _keyManagementRotateInfo.setText(registry.getText(T_KEYMGMT_ROTATE_INFO, "This creates a new key, encrypted so that only already authorized readers can access the new key"));
        _keyManagementReset.setText(registry.getText(T_KEYMGMT_RESET, "Reset all keys"));
        _keyManagementResetInfo.setText(registry.getText(T_KEYMGMT_RESET_INFO, "This creates a new key encrypted with another new key.  The key used can either be distributed to authorized readers manually (with links in private messages, keyfiles, etc) or shared in a passphrase encrypted post."));
        _keyManagementPBE.setText(registry.getText(T_KEYMGMT_PBE, "Require a passphrase to access the keys"));
        _keyManagementPBEInfo.setText(registry.getText(T_KEYMGMT_PBE_INFO, "If a passphrase is required, even already authorized users will need to know the passphrase, so consider posting that prior to a key rotation."));
        _keyManagementNewReply.setText(registry.getText(T_KEYMGMT_NEWREPLY, "Create a new forum reply key"));
        
        int auth = -1;
        if (_authorization.getItemCount() > 0)
            auth = _authorization.getSelectionIndex();
        else
            auth = 1;
        _authorization.setRedraw(false);
        _authorization.removeAll();
        _authorization.add(registry.getText(T_AUTH_PUBLIC, "Allow anyone to post to the forum"));
        _authorization.add(registry.getText(T_AUTH_PUBREPLY, "Allow anyone to reply to authorized posts"));
        _authorization.add(registry.getText(T_AUTH_AUTH, "Only allow authorized posters to post"));
        _authorization.select(auth);
        _authorization.setRedraw(true);
        populateReferences();
    }
    
    private void populateReferences() {}
}
