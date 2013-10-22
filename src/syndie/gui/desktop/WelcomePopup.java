package syndie.gui.desktop;

import net.i2p.data.Base64;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import syndie.Constants;
import syndie.data.MessageInfo;
import syndie.data.SyndieURI;
import syndie.db.UI;
import syndie.gui.FireSelectionListener;
import syndie.gui.Theme;
import syndie.gui.ThemeRegistry;
import syndie.gui.Themeable;
import syndie.gui.Translatable;
import syndie.gui.TranslationRegistry;
import syndie.gui.URIHelper;

/**
 *
 */
class WelcomePopup implements Themeable, Translatable {
    private Desktop _desktop;
    private UI _ui;
    private ThemeRegistry _themeRegistry;
    private TranslationRegistry _translationRegistry;
    private Shell _parent;
    
    private Shell _shell;
    private Label _greeting;
    private Label _question;
    private Button _tour;
    private Button _configSharing;
    private Button _configIdent;
    private Button _configPass;
    private Button _continueToSyndie;
    
    public WelcomePopup(Desktop desktop, UI ui, Shell parent, ThemeRegistry themes, TranslationRegistry trans) {
        _desktop = desktop;
        _ui = ui;
        _parent = parent;
        _translationRegistry = trans;
        _themeRegistry = themes;
        initComponents();
    }
    
    
    private void initComponents() {
        _shell = new Shell(_parent, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent shellEvent) {
                continueToSyndie();
            }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 0;
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        gl.verticalSpacing = 0;
        _shell.setLayout(gl);
        
        _greeting = new Label(_shell, SWT.WRAP);
        _greeting.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        
        _question = new Label(_shell, SWT.WRAP);
        _question.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        
        _tour = new Button(_shell, SWT.PUSH);
        _tour.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _tour.addSelectionListener(new FireSelectionListener() {
            public void fire() { beginTour(); }
        });
        
        _configSharing = new Button(_shell, SWT.PUSH);
        _configSharing.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _configSharing.addSelectionListener(new FireSelectionListener() {
            public void fire() { configSharing(); }
        });
        
        _configIdent = new Button(_shell, SWT.PUSH);
        _configIdent.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _configIdent.addSelectionListener(new FireSelectionListener() {
            public void fire() { configIdent(); }
        });
        
        _configPass = new Button(_shell, SWT.PUSH);
        _configPass.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _configPass.addSelectionListener(new FireSelectionListener() {
            public void fire() { configPass(); }
        });
        
        _continueToSyndie = new Button(_shell, SWT.PUSH);
        _continueToSyndie.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _continueToSyndie.addSelectionListener(new FireSelectionListener() {
            public void fire() { continueToSyndie(); }
        });
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
        
        _shell.pack();
        _shell.open();
    }
    
    private void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        _shell.dispose();
    }
    
    private void continueToSyndie() { dispose(); }
    
    private void beginTour() { 
        // show tour popup...
        dispose();
        long msgId = _desktop.getDBClient().getMessageId(Constants.TOUR_MSG.getScope(), Constants.TOUR_MSG.getMessageId());
        MessageInfo msg = _desktop.getDBClient().getMessage(msgId);
        int page = 0;
        new StandaloneMessageViewer(_desktop.getDBClient(), _desktop.getUI(), _parent, msg, page, _desktop.getNavControl(), _themeRegistry, _translationRegistry, _desktop.getBookmarkControl(), _desktop.getBanControl(), _desktop.getDataCallback());
        //_desktop.hide();
    } 
    private void configSharing() {
        dispose();
        _desktop.getNavControl().view(URIHelper.instance().createSyndicationArchiveURI());
    }
    private void configIdent() {
        Hash ident = _desktop.getDefaultIdent();
        dispose();
        _desktop.getNavControl().view(URIHelper.instance().createManageURI(ident));
    }
    private void configPass() {
        new ChangePassPopup(_desktop, _ui, _shell, _themeRegistry, _translationRegistry);
    }
    
    public void applyTheme(Theme theme) {
        _shell.setFont(theme.SHELL_FONT);
        _greeting.setFont(theme.SHELL_FONT);
        _question.setFont(theme.SHELL_FONT);
        _tour.setFont(theme.BUTTON_FONT);
        _configSharing.setFont(theme.BUTTON_FONT);
        _configIdent.setFont(theme.BUTTON_FONT);
        _configPass.setFont(theme.BUTTON_FONT);
        _continueToSyndie.setFont(theme.BUTTON_FONT);
        _shell.pack(true);
        //_shell.layout(true, true);
    }
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText("Welcome"));
        _greeting.setText(registry.getText("Welcome to Syndie!"));
        _question.setText(registry.getText("Do you want to") + "...");
        _tour.setText(registry.getText("Take the tour?"));
        _configSharing.setText(registry.getText("Configure your sharing policy?"));
        _configIdent.setText(registry.getText("Configure your default identity?"));
        _configPass.setText(registry.getText("Configure your instance passphrase?"));
        _continueToSyndie.setText(registry.getText("Continue on to Syndie?"));
    }
}
