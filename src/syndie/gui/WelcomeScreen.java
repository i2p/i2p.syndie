package syndie.gui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.Timer;
import syndie.db.ManageForumExecutor;

/**
 *
 */
public class WelcomeScreen implements Themeable, Translatable {
    private Display _display;
    private Browser _browser;
    private CompleteListener _lsnr;
    private Shell _shell;
    private Button _ok;
    
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
    
    public WelcomeScreen(Display display, Browser browser, CompleteListener lsnr, Timer timer) {
        _display = display;
        _browser = browser;
        _lsnr = lsnr;
        _avatarImages = new ArrayList();
        ImageUtil.init(browser.getClient().getTempDir(), timer);
        initComponents();
    }
    
    public void open() {
        _shell.pack();
        
        Rectangle shellSize = _shell.getBounds();
        Rectangle screenSize = Splash.getScreenSize(_shell);
        int width = Math.max(shellSize.width, 400);
        int x = screenSize.width/2-width/2;
        int y = screenSize.height/2-shellSize.height/2;
        _shell.setBounds(x, y, width, shellSize.height);
        
        _shell.open(); 
        Splash.dispose(); 
    }
    
    private void close() {
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
        _shell.dispose();
        _lsnr.complete();
    }
    
    private void save() {
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
    }
    
    private void initComponents() {
        _shell = new Shell(_display, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; close(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        _shell.setLayout(new GridLayout(2, false));
        
        _description = new Label(_shell, SWT.WRAP);
        _description.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 2, 1));
        
        _nameLabel = new Label(_shell, SWT.NONE);
        _nameLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _name = new Text(_shell, SWT.SINGLE | SWT.BORDER);
        _name.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _name.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) {
                    save();
                    close();
                }
            }
        });
        
        _avatarLabel = new Label(_shell, SWT.NONE);
        _avatarLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _avatar = new Button(_shell, SWT.PUSH);
        GridData gd = new GridData(GridData.BEGINNING, GridData.CENTER, true, false);
        gd.widthHint = 52;
        gd.heightHint = 52;
        _avatar.setLayoutData(gd);
        _avatarMenu = new Menu(_avatar);
        _avatar.setMenu(_avatarMenu);
        _avatar.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _avatarMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _avatarMenu.setVisible(true); }
        });
        
        _authenticationLabel = new Label(_shell, SWT.WRAP);
        _authenticationLabel.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 2, 1));
        
        _authenticatePublic = new Button(_shell, SWT.RADIO);
        _authenticatePublic.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _authenticateReplies = new Button(_shell, SWT.RADIO);
        _authenticateReplies.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _authenticateAuth = new Button(_shell, SWT.RADIO);
        _authenticateAuth.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));

        _authenticateReplies.setSelection(true);
        
        _ok = new Button(_shell, SWT.PUSH);
        _ok.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
        _ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { save(); close(); }
            public void widgetSelected(SelectionEvent selectionEvent) { save(); close(); }
        });
        
        _shell.addKeyListener(new KeyListener() {
            public void keyPressed(KeyEvent evt) {
                if (evt.character == '=')
                    _browser.getThemeRegistry().increaseFont();
                if (evt.character == '-')
                    _browser.getThemeRegistry().decreaseFont();
            }
            public void keyReleased(KeyEvent keyEvent) {}
        });
        
        _shell.setImage(ImageUtil.ICON_SHELL);
        
        populateAvatarMenu();
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
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
            _avatarItems[i].setImage(img);
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
        _avatarItems[_avatarItems.length-1].setText(_browser.getTranslationRegistry().getText(T_AVATAR_OTHER, "Other..."));
        _avatarItems[_avatarItems.length-1].addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickAvatar(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickAvatar(); }
        });
        
        _avatarImage = (Image)_avatarImages.get(0);
        _avatar.setImage(_avatarImage);
    }
    private static final String T_AVATAR_OTHER = "syndie.gui.welcomescreen.avatar.other";
    
    private void pickAvatar() {
        FileDialog dialog = new FileDialog(_shell, SWT.SINGLE | SWT.OPEN);
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
    
    private static final String T_AVATAR_OPEN_NAME = "syndie.gui.welcomescreen.avatar.name";
    private static final String T_AVATAR_OPEN_TYPE = "syndie.gui.welcomescreen.avatar.type";
    
    public void applyTheme(Theme theme) {
        /*
        _ok.setFont(theme.BUTTON_FONT);
        _description.setFont(theme.DEFAULT_FONT);
        _nameLabel.setFont(theme.DEFAULT_FONT);
        _name.setFont(theme.DEFAULT_FONT);
        _avatarLabel.setFont(theme.DEFAULT_FONT);
        _authenticationLabel.setFont(theme.DEFAULT_FONT);
        _authenticatePublic.setFont(theme.DEFAULT_FONT);
        _authenticateReplies.setFont(theme.DEFAULT_FONT);
        _authenticateAuth.setFont(theme.DEFAULT_FONT);
         */
    }
    
    public void translate(TranslationRegistry registry) {
        _ok.setText(registry.getText(T_OK, "Continue"));
        _shell.setText(registry.getText(T_TITLE, "Welcome to Syndie!"));
        _description.setText(registry.getText(T_DESC, "Syndie will create a new identity for you to use with which to post messages in other forums and to run your own blog/forum"));
        _nameLabel.setText(registry.getText(T_NAME, "What name would you like to use for your new identity?"));
        _name.setText(registry.getText(T_NAME_DEFAULT, "Syndie user"));
        _avatarLabel.setText(registry.getText(T_AVATAR_LABEL, "What avatar would you like to use?"));
        _authenticationLabel.setText(registry.getText(T_AUTH_LABEL, "In your new identity's blog/forum, would  you like to allow other people to post?"));
        _authenticatePublic.setText(registry.getText(T_AUTH_PUBLIC, "Yes, let anyone reply to existing posts and post new topics"));
        _authenticateReplies.setText(registry.getText(T_AUTH_REPLY, "Yes, let anyone reply to existing posts"));
        _authenticateAuth.setText(registry.getText(T_AUTH_AUTH, "No"));
    }
    private static final String T_OK = "syndie.gui.welcomescreen.ok";
    private static final String T_TITLE = "syndie.gui.welcomescreen.title";
    private static final String T_DESC = "syndie.gui.welcomescreen.desc";
    private static final String T_NAME = "syndie.gui.welcomescreen.name";
    private static final String T_NAME_DEFAULT = "syndie.gui.welcomescreen.name.default";
    private static final String T_AVATAR_LABEL = "syndie.gui.welcomescreen.avatar.label";
    private static final String T_AUTH_LABEL = "syndie.gui.welcomescreen.auth.label";
    private static final String T_AUTH_PUBLIC = "syndie.gui.welcomescreen.auth.public";
    private static final String T_AUTH_REPLY = "syndie.gui.welcomescreen.auth.reply";
    private static final String T_AUTH_AUTH = "syndie.gui.welcomescreen.auth.auth";
    
    public static interface CompleteListener { public void complete(); }
}
