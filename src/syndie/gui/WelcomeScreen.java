package syndie.gui;

import java.text.Collator;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import net.i2p.util.RandomSource;

import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Text;

import syndie.Constants;
import syndie.util.Timer;
import syndie.db.ManageForumExecutor;
import syndie.gui.Wizard.Page;

/**
 *
 */
class WelcomeScreen extends Wizard {
    private final Browser _browser;
    private final CompleteListener _lsnr;
    
    private Label _langMessage;
    private Combo _langCombo;
    private final List<String> _langCodes;

    private Page _welcomePage;
    private Label _welcomeMessage;
    
    private Page _identityPage;
    private Label _description;
    private Label _nameLabel;
    private Text _name;
    private Label _avatarLabel;
    private Button _avatar;
    private Label _authenticationLabel;
    private Button _authenticatePublic;
    private Button _authenticateReplies;
    private Button _authenticateAuth;
    
    private Menu _avatarMenu;
    private MenuItem _avatarItems[];
    private List _avatarImages;
    
    private Image _avatarImage;
    
    private Page _archiveExplanationPage;
    private Label _archiveExplanationMessage;
    
    private Page _archiveDefaultsPage;
    private ArchiveDefaults _archiveDefaults;
    private Label _archiveInstructions;
    
    private Page _finishPage;
    private Label _finishMessage;
    
    public WelcomeScreen(Display display, Browser browser, CompleteListener lsnr, Timer timer) {
        super(display);
        _browser = browser;
        _lsnr = lsnr;
        _avatarImages = new ArrayList();
        ImageUtil.init(browser.getClient().getTempDir(), timer);
        _langCodes = new ArrayList(16);
        initComponents();
    }
    
    public void open() {
        super.open();
        Splash.dispose();
    }
    
    @Override
    void close(boolean success) {
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
        _archiveDefaults.dispose();
        super.close(success);
        
        _lsnr.complete(success);
    }
    
    void save() {
        ManageForumExecutor exec = new ManageForumExecutor(_browser.getClient(), _browser.getUI(), new ManageForumExecutor.ManageForumState() {
            public byte[] getAvatarData() {
                Image avatar = _avatarImage;
                if (avatar != null) {
                    try {
                        return ImageUtil.serializeImage(avatar);
                    } catch (SWTException se) {
                        _browser.getUI().errorMessage("Internal error serializing image", se);
                        return null;
                    }
                } else {
                    return null;
                }
            }
        
            public String getName() { return _name.getText().trim(); }
            public String getDescription() { return ""; }
            /** 
             * note the assumption that the newly created forum is version 0.  not sure when this
             * wouldn't be the case, since this is only run on install
             */
            public long getChannelId() { return 0; }
            public long getLastEdition() { return _browser.getClient().getChannelVersion(getChannelId()); }
            public boolean getAllowPublicPosts() { return _authenticatePublic.getSelection(); }
            public boolean getAllowPublicReplies() { return _authenticateReplies.getSelection() || _authenticatePublic.getSelection(); }
            public Set getPublicTags() { return Collections.EMPTY_SET; }
            public Set getPrivateTags() { return Collections.EMPTY_SET; }
            public Set getAuthorizedPosters() { return Collections.EMPTY_SET; }
            public Set getAuthorizedManagers() { return Collections.EMPTY_SET; }
            public String getReferences() { return ""; }
            public Set getPublicArchives() { return Collections.EMPTY_SET; }
            public Set getPrivateArchives() { return Collections.EMPTY_SET; }
            public boolean getEncryptContent() { return false; }
            public boolean getPBE() { return false; }
            public String getPassphrase() { return null; }
            public String getPassphrasePrompt() { return null; }
            public List getCurrentReadKeys() { return Collections.EMPTY_LIST; }
            public boolean getCreateReadKey() { return false; }
            public boolean getCreatePostIdentity() { return false; }
            public boolean getCreateManageIdentity() { return false; }
            public boolean getCreateReplyKey() { return false; }
            public List getCancelledURIs() { return new ArrayList(); }
        });
        exec.execute();
        String errs = exec.getErrors();
        if ( (errs != null) && (errs.length() > 0) )
            _browser.getUI().errorMessage("Error updating the forum: " + errs);
        
        _archiveDefaults.save();
    }
    
    private void initComponents() {
        _shell.setImage(ImageUtil.ICON_SHELL);
        // Create language page
        Page langPage = new Page();
        langPage.setLayout(new GridLayout(1, false));
        Composite center = new Composite(langPage, SWT.NONE);
        center.setLayout(new GridLayout(1, false));
        center.setLayoutData(new GridData(GridData.CENTER, GridData.CENTER, true, true));
        _langMessage = new Label(center, SWT.WRAP);
        _langMessage.setLayoutData(new GridData(GridData.CENTER, GridData.END, false, false));
        _langCombo = new Combo(center, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.BORDER);
        _langCombo.setLayoutData(new GridData(GridData.CENTER, GridData.BEGINNING, false, false));
        _langCombo.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                int idx = _langCombo.getSelectionIndex();
                if (idx >= 0)
                    _browser.getTranslationRegistry().switchTranslation(_langCodes.get(idx));
            }
        });

        // Create welcome page
        _welcomePage = new Page();
        _welcomeMessage = new Label(_welcomePage, SWT.WRAP);
        
        // Create identity page
        _identityPage = new Page();
        _identityPage.setLayout(new GridLayout(2, false));
        
        _description = new Label(_identityPage, SWT.WRAP);
        _description.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 2, 1));
        
        _nameLabel = new Label(_identityPage, SWT.NONE);
        _nameLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _name = new Text(_identityPage, SWT.SINGLE | SWT.BORDER);
        _name.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _name.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) {
                    next();
                }
            }
        });
        
        _avatarLabel = new Label(_identityPage, SWT.NONE);
        _avatarLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _avatar = new Button(_identityPage, SWT.PUSH);
        GridData avatarGD = new GridData(GridData.BEGINNING, GridData.CENTER, true, false);
        avatarGD.widthHint = 52;
        avatarGD.heightHint = 52;
        _avatar.setLayoutData(avatarGD);
        _avatarMenu = new Menu(_avatar);
        _avatar.setMenu(_avatarMenu);
        _avatar.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _avatarMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _avatarMenu.setVisible(true); }
        });
        
        _authenticationLabel = new Label(_identityPage, SWT.WRAP);
        _authenticationLabel.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 2, 1));
        
        _authenticatePublic = new Button(_identityPage, SWT.RADIO);
        _authenticatePublic.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _authenticateReplies = new Button(_identityPage, SWT.RADIO);
        _authenticateReplies.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _authenticateAuth = new Button(_identityPage, SWT.RADIO);
        _authenticateAuth.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));

        _authenticateReplies.setSelection(true);
        
        _identityPage.addKeyListener(new KeyListener() {
            public void keyPressed(KeyEvent evt) {
                if (evt.character == '=')
                    _browser.getThemeRegistry().increaseFont();
                if (evt.character == '-')
                    _browser.getThemeRegistry().decreaseFont();
            }
            public void keyReleased(KeyEvent keyEvent) {}
        });
        
        populateAvatarMenu();
        
        // Create archive explanation page
        _archiveExplanationPage = new Page();
        _archiveExplanationMessage = new Label(_archiveExplanationPage, SWT.WRAP);
        
        // Create archive page
        _archiveDefaultsPage = new Page(0, 0);
        _archiveDefaultsPage.setLayout(new GridLayout(1, false));
        
        _archiveDefaults = new ArchiveDefaults(_archiveDefaultsPage, _browser.getClient(), _browser.getUI(), _browser.getThemeRegistry(), _browser.getTranslationRegistry());
        _archiveDefaults.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        
        _archiveInstructions = new Label(_archiveDefaultsPage, SWT.WRAP);
        _archiveInstructions.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        
        // Create finish page
        _finishPage = new Page();
        _finishMessage = new Label(_finishPage, SWT.WRAP);
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
        
        update();
    }
    
    /**
     *  Modified from Browser
     *  @since 1.102b-13
     */
    private void populateTranslations() {
        final TranslationRegistry reg = _browser.getTranslationRegistry();
        List<String> translations = reg.getTranslations();
        String curLang = reg.getTranslation();
        String selected = curLang;
        Locale curLocale = new Locale(curLang);
        // sort by display name
        Map<String, String> tmap = new TreeMap(Collator.getInstance());
        for (String translation : translations) {
            String langName = (new Locale(translation)).getDisplayLanguage(curLocale);
            if (langName.length() <= 0)
                langName = translation;
            tmap.put(langName, translation);
        }
        _langCombo.removeAll();
        _langCodes.clear();
        int i = 0;
        for (Map.Entry<String, String> e : tmap.entrySet()) {
            _langCombo.add(e.getKey());
            String translation = e.getValue();
            _langCodes.add(translation);
            if (translation.equals(selected))
                _langCombo.select(i);
            i++;
        }
    }

    private void populateAvatarMenu() {
        int i = 0;
        while (true) {
            Image img = ImageUtil.createImageFromResource("iconAvatar" + i + ".png");
            if (img != null) {
                _avatarImages.add(img);
                i++;
            } else {
                break;
            }
        }
        
        _avatarItems = new MenuItem[_avatarImages.size() + 1];
        for (i = 0; i < _avatarItems.length-1; i++) {
            final Image img = (Image)_avatarImages.get(i);
            _avatarItems[i] = new MenuItem(_avatarMenu, SWT.PUSH);
            // image not displayed on all platforms - TODO pupup?
            _avatarItems[i].setImage(img);
            _avatarItems[i].setText(_browser.getTranslationRegistry().getText("Default") + ' ' + (i+1));
            _avatarItems[i].addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { 
                    _avatarImage = img;
                    _avatar.setImage(img);
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    _avatarImage = img;
                    _avatar.setImage(img);
                }
            });
        }
        _avatarItems[_avatarItems.length-1] = new MenuItem(_avatarMenu, SWT.PUSH);
        _avatarItems[_avatarItems.length-1].setText(_browser.getTranslationRegistry().getText("Load from file"));
        _avatarItems[_avatarItems.length-1].addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickAvatar(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickAvatar(); }
        });
        
        // pick one for them
        if (_avatarImages.size() > 0) {
            int rand = RandomSource.getInstance().nextInt(_avatarImages.size());
            _avatarImage = (Image)_avatarImages.get(rand);
            _avatar.setImage(_avatarImage);
        }
    }
    
    private void pickAvatar() {
        FileDialog dialog = new FileDialog(getShell(), SWT.SINGLE | SWT.OPEN);
        dialog.setText(_browser.getTranslationRegistry().getText("Select a 48x48 pixel PNG image"));
        dialog.setFilterExtensions(new String[] { "*.png" });
        dialog.setFilterNames(new String[] { _browser.getTranslationRegistry().getText("PNG image") });
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
                    System.out.println("resizing avatar from " + bounds + " to " + width +"/"+ height);
                } else {
                    System.out.println("keeping avatar at " + bounds);
                }
                ImageUtil.dispose(_avatarImage);
                _avatarImage = img;
                _avatar.setImage(_avatarImage);
            }
        }
    }
    
    
    public void applyTheme(Theme theme) {
        super.applyTheme(theme);
        
        _welcomeMessage.setFont(theme.DEFAULT_FONT);
        
        _description.setFont(theme.DEFAULT_FONT);
        _nameLabel.setFont(theme.DEFAULT_FONT);
        _name.setFont(theme.DEFAULT_FONT);
        _avatarLabel.setFont(theme.DEFAULT_FONT);
        _authenticationLabel.setFont(theme.DEFAULT_FONT);
        _authenticatePublic.setFont(theme.DEFAULT_FONT);
        _authenticateReplies.setFont(theme.DEFAULT_FONT);
        _authenticateAuth.setFont(theme.DEFAULT_FONT);
    }
    
    
    public void translate(TranslationRegistry registry) {
        super.translate(registry);

        _langMessage.setText(registry.getText("Language"));
        populateTranslations();
        
        _welcomeMessage.setText(reflow(registry, new String [] {
                _x("Welcome to Syndie!"), "\n\n",
                _x("This wizard will help you set up your new Syndie installation. First we'll set up your identity, " +
                "and then we'll configure some archives for you to syndicate with."), "\n\n",
                _x("It is strongly recommended that you have I2P running before you start Syndie."),
                _x("If I2P is not running, please start it and wait 5 minutes before proceeding.")
        }));
        
        _description.setText(reflow(registry, new String [] {
                _x("Syndie will create a new identity for you to use with which to post messages in other forums and to run" +
                "your own blog/forum.")}));

        _nameLabel.setText(registry.getText("What name would you like to use for your new identity?"));
        _name.setText(registry.getText("Syndie user") + ' ' + (1001 + RandomSource.getInstance().nextInt(98888)));
        _avatarLabel.setText(registry.getText("Click the image to select the avatar would you like to use") + ':');
        _authenticationLabel.setText(registry.getText("In your new identity's blog/forum, would  you like to allow other people to post?"));
        _authenticatePublic.setText(registry.getText("Yes, let anyone reply to existing posts and post new topics"));
        _authenticateReplies.setText(registry.getText("Yes, let anyone reply to existing posts"));
        _authenticateAuth.setText(registry.getText("No"));
        
        _archiveExplanationMessage.setText(reflow(registry, new String [] {
                _x("Next it's time to select some archives to syndicate with."), "\n\n",
                _x("Syndie messages are propagated from one Syndie instance to another by a process called 'syndication'."),
                _x("Each client connects to one or more archives and uploads any messages which the client has, but the " +
                "archive does not, and downloads any messages which the archive has, but the client does not. In this " +
                "way messages are propagated from client to client and archive to archive within a Syndie community."), "\n\n",
                _x("To join a Syndie community, you need to syndicate with one or more archives of that community.")}));
        
        _archiveInstructions.setText(reflow(registry, new String [] {
                _x("The default archives shipped with your Syndie install are listed above. Double-click a field to edit it."),
                _x("Please make any necessary changes and uncheck any archives that you don't want.")}));
        
        _finishMessage.setText(reflow(registry, new String [] {
                _x("Congratulations! Your Syndie installation is configured!"), "\n\n",
                _x("Click Finish to start exploring Syndie.")}));
    }
    
    /**
     *  Tagging for static initializers. Does not translate!
     *  @return s
     */
    private static final String _x(String s) {
        return s;
    }

    private String reflow(TranslationRegistry registry, String [] str) {
        String sep = SWT.getPlatform().equals("win32") ? "\n" : " ";
        StringBuilder buf = new StringBuilder();
        
        for (int c = 0; c < str.length;c ++) {
            String line = registry.getText(str[c]);
            if (buf.length() == 0)
                buf.append(line);
            else if (line.endsWith("\n")) // manual break
                buf.append(line);
            else
                buf.append(sep).append(line);
        }
        
        return buf.toString();
    }
    
    public static interface CompleteListener { public void complete(boolean success); }
}
