package syndie.gui.desktop;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTError;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.Version;
import syndie.data.MessageInfo;
import syndie.data.NymKey;
import syndie.data.NymReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.Timer;
import syndie.db.DBClient;
import syndie.db.HTTPServ;
import syndie.db.Importer;
import syndie.db.JobRunner;
import syndie.db.SyncManager;
import syndie.db.TextEngine;
import syndie.db.UI;
import syndie.gui.*;

/**
 *
 */
class Desktop {
    private File _rootFile;
    private String _login;
    private String _passphrase;
    private TextEngine _engine;
    private DesktopUI _ui;
    private Display _display;
    private Shell _shell;
    private List _listeners;
    private Composite _edgeNorthWest;
    private Composite _edgeNorthEast;
    private Composite _edgeSouthEast;
    private Composite _edgeSouthWest;
    
    private DesktopCorner _cornerNorthWest;
    private DesktopCorner _cornerNorthEast;
    private DesktopCorner _cornerSouthEast;
    private DesktopCorner _cornerSouthWest;

    private Composite _edgeNorth;
    private Composite _edgeEast;
    private Composite _edgeSouth;
    private Composite _edgeWest;
    private Composite _centerInfo;
    private Composite _center;

    private CommandBar _commandBar;
    //private DesktopEdge _edgeNorthDefault;
    private DesktopEdge _edgeEastDefault;
    private DesktopEdge _edgeSouthDefault;
    private LinkEdge _edgeWestDefault;
    
    private Composite _centerDefault;
    
    private StackLayout _centerInfoStack;
    private StackLayout _centerStack;
    //private StackLayout _edgeNorthStack;
    private StackLayout _edgeEastStack;
    private StackLayout _edgeSouthStack;
    private StackLayout _edgeWestStack;
    
    private StartupPanel _startupPanel;
    private ForumSelectionPanel _forumSelectionPanel;
    private ControlMenuPanel _controlPanel;
    private TabPanel _tabs;
    private TaskTreeShell _taskTree;
    
    private DBClient _client;
    private TranslationRegistry _translationRegistry;
    private ThemeRegistry _themeRegistry;
    
    private List _loadedPanels;
    private int _curPanelIndex;
    
    private NavigationControl _navControl;
    private BanControl _banControl;
    private BookmarkControl _bookmarkControl;
    private DataCallback _dataCallback;
    private LocalMessageCallback _localMessageCallback;
    
    /** true when syndie creates a new instance on startup, and we then fire up a welcomepopup when ready */
    private boolean _shouldShowWelcome;
    
    public Desktop(File rootFile, DesktopUI ui, Display display, Timer timer) {
        _rootFile = rootFile;
        _ui = ui;
        _display = display;
        _listeners = new ArrayList();
        _loadedPanels = new ArrayList();
        _curPanelIndex = -1;
        _shouldShowWelcome = false;
        _navControl = new DesktopNavigationControl(this);
        _banControl = new DesktopBan();
        _bookmarkControl = new DesktopBookmark();
        _dataCallback = new DesktopDataCallback();
        _localMessageCallback = new DesktopLocalMessageCallback();
        
        ComponentBuilder.instance().setNavigationControl(_navControl);
        ComponentBuilder.instance().setBanControl(_banControl);
        ComponentBuilder.instance().setBookmarkControl(_bookmarkControl);
        ComponentBuilder.instance().setDataCallback(_dataCallback);
        ComponentBuilder.instance().setLocalMessageCallback(_localMessageCallback);
        ComponentBuilder.instance().setUI(_ui);
        initComponents(timer);
    }
    
    public interface DesktopListener {
        public void panelShown(DesktopPanel panel);
        public void panelHidden(DesktopPanel panel);
        public void destroyed(DesktopPanel panel);
    }
    public void addListener(DesktopListener lsnr) { synchronized (_listeners) { _listeners.add(lsnr); } }
    public void removeListener(DesktopListener lsnr) { synchronized (_listeners) { _listeners.add(lsnr); } }
    
    private boolean TRIM = true;
    
    private void initComponents(Timer timer) {
        timer.addEvent("init desktop components begin");
        prepareShell(timer);
        timer.addEvent("init desktop components: shell prepared");
        _startupPanel = new StartupPanel(this, _center, _ui, timer);
        show(_startupPanel, null, null, null);
        timer.addEvent("init desktop components: startup panel shown");
        
        initKeyFilters();
        timer.addEvent("init desktop components: key filters installed");
        
        show();
        timer.addEvent("init desktop components: desktop shown");
    }
    private void prepareShell(Timer timer) {
        if (TRIM)
            _shell = new Shell(_display, SWT.SHELL_TRIM);
        else
            _shell = new Shell(_display, SWT.NO_TRIM);
        _shell.setText("Syndie " + Version.VERSION);
        timer.addEvent("shell created");
        prepareGrid();
        timer.addEvent("grid prepared");
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {
                DesktopPanel panel = getCurrentPanel();
                if (panel != null) {
                    panel.forceFocus(); // when the key filters are triggered, swt seems to lose track of the focus
                }
            }
            public void shellClosed(ShellEvent evt) {
                evt.doit = false;
                close();
            }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
    }
    
    public void setEngine(TextEngine engine) { 
        synchronized (this) {
            _engine = engine;
            notifyAll();
        }
    }
    
    public Hash getDefaultIdent() {
        List keys = _client.getNymKeys(null, Constants.KEY_FUNCTION_MANAGE);
        for (int i = 0; i < keys.size(); i++) {
            NymKey key = (NymKey)keys.get(i);
            if (key.isIdentity())
                return key.getChannel();
        }
        return null;
    }
    
    void restart(final String rootDir) {
        boolean startupClosed = false;
        HTTPServ.killAll();
        while (_loadedPanels.size() > (startupClosed ? 1 : 0)) {
            DesktopPanel panel = (DesktopPanel)_loadedPanels.get((startupClosed ? 1 : 0));
            _ui.debugMessage("closing " + panel);
            if (panel instanceof StartupPanel) {
                _ui.debugMessage("never closing the startup panel");
                startupClosed = true;
            } else if (!panel.close()) {
                _ui.debugMessage("could not close " + panel);
                return;
            }
        }
        
        SyncManager.unloadAll();
        _ui.debugMessage("sync manager unloaded for restart");
        
        if (_engine != null) {
            _ui.debugMessage("inserting exit...");
            _ui.insertCommand("exit");
        } else {
            _ui.debugMessage("engine already gone (?)");
        }
        JobRunner.instance().enqueue(new Runnable() {
            public void run() {
                while (_engine != null) {
                    synchronized (this) {
                        try { wait(1000); } catch (InterruptedException ie) {}
                    }
                    _ui.debugMessage("waiting for engine to disappear...");
                }
                _rootFile = new File(rootDir);
                _client.restart(rootDir);
                _ui.debugMessage("client restarted");

                boolean ok = startEngine();
                if (!ok) {
                    _ui.errorMessage("Error - engine startup failed!  die die die");
                    _display.syncExec(new Runnable() {
                        public void run() { close(); }
                    });
                    System.exit(0); // reached if the user says they don't want to exit
                    return;
                }
                _ui.debugMessage("engine restarted");

                _display.asyncExec(new Runnable() { 
                    public void run() { 
                        refreshPrimaryAvatar(); 
                        _themeRegistry.loadTheme();
                        _translationRegistry.loadTranslations();
                        
                        showForumSelectionPanel();
                        if (_shouldShowWelcome)
                            new WelcomePopup(Desktop.this, _ui, _shell, _themeRegistry, _translationRegistry);
                    }
                });
                
                _ui.debugMessage("restart complete");
            }
        });
    }
    
    /**
     * If true, remove the 30 second timeout when connecting to the database,
     * as slow computers w/ lots in their redo logs could take that long.  The
     * reason the timeout is here in the first place is to deal with those 
     * running Syndie off pre-1.0 installs that didn't automatically log in
     */
    private static final boolean ALLOW_SLOW_STARTUP = true;
    
    private static final String T_ALREADY_RUNNING_TITLE = "syndie.gui.desktop.alreadyrunning.title";
    private static final String T_ALREADY_RUNNING = "syndie.gui.desktop.alreadyrunning";
    private static final String T_ALREADY_RUNNING_EXIT = "syndie.gui.desktop.alreadyrunning.exit";
    
    private static final String T_LOGIN_FAILED_TITLE = "syndie.gui.desktop.loginfailed.title";
    private static final String T_LOGIN_FAILED = "syndie.gui.desktop.loginfailed";
    private static final String T_LOGIN_FAILED_EXIT = "syndie.gui.desktop.loginfailed.exit";
    
    boolean startEngine() {
        final Timer startupTimer = new Timer("restart engine", _ui);
        final StartupListener lsnr = new StartupListener(startupTimer);
        final TextEngine engine = new TextEngine(_client, _ui, lsnr);
        startupTimer.addEvent("text engine instantiated");
        
        Thread t = new Thread(new Runnable() {
            public void run() {
                _ui.debugMessage("starting the engine");
                try {
                    setEngine(engine);
                    engine.run();
                } catch (Exception e) {
                    _ui.errorMessage("error running the engine", e);
                }
                _ui.debugMessage("engine stopped");
                setEngine(null);
            }
        }, "text ui");
        t.setPriority(Thread.MIN_PRIORITY);
        t.start();
        startupTimer.addEvent("text engine started");
        
        _ui.debugMessage("waiting for login completion...");
        
        while (true) {
            int rc = proceedAfterLogin(engine, startupTimer, lsnr);
            _ui.debugMessage("proceedAfterLogin returned " + rc);
            if (rc == 1) {
                while ( !(_client.isLoggedIn() && _client.verifyNymKeyEncryption()) ) {
                    _ui.debugMessage("invalid pass, try again");
                    promptForPass();
                    // invalid pasphrase, so loop
                }
                
                // reached both on initial startup and on restarts
                JobRunner.instance().enqueue(new Runnable() { 
                    public void run() { 
                        String onStartup = _client.getNymPrefs().getProperty("httpserv.runOnStartup");
                        if ( (onStartup != null) && ("true".equalsIgnoreCase(onStartup)) )
                            new HTTPServ(_client, _ui); // starts it up in its own threads
                    }
                });
                
                return true;
            } else if (rc == 2) { 
                return false; 
            } else if (rc == 0) {
                _ui.debugMessage("clearing login state");
                lsnr.clearLoginState();
                _ui.debugMessage("trying to proceed again");
                continue; // redundant, yes, but just clarifying the value that is returned
            }
        }
    }
    
    
    private static final String T_PASSPHRASE_REQ = "syndie.gui.desktop.passphrase.req";
    private static final String T_LOGIN_PROCEED = "syndie.gui.desktop.login.proceed";
    private static final String T_LOGIN_EXIT = "syndie.gui.desktop.login.exit";
    private static final String T_LOGIN = "syndie.gui.desktop.login";
    
    private int proceedAfterLogin(final TextEngine engine, final Timer timer, final StartupListener lsnr) {
        // to allow the startup scripts to run, which may include 'login',
        // so we dont have to show a login prompt.  perhaps toss up a splash screen
        boolean ok = lsnr.waitFor("login", ALLOW_SLOW_STARTUP ? -1 : 30*1000);
        _ui.debugMessage("after login: listener found ok? " + ok);
        if (lsnr.getAlreadyRunning()) {
            // show a special warning/error screen
            
            _display.asyncExec(new Runnable() { 
                public void run() {
                    final Shell s = new Shell(_display, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
                    s.setText(strans(T_ALREADY_RUNNING_TITLE, "Already running"));
                    s.setFont(_themeRegistry.getTheme().SHELL_FONT);
                    s.setLayout(new GridLayout(1, true));
                    Label l = new Label(s, SWT.SINGLE | SWT.WRAP);
                    l.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
                    l.setText(strans(T_ALREADY_RUNNING, "Syndie is already running - please use the existing Syndie window"));
                    l.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
                    Button b = new Button(s, SWT.PUSH);
                    b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
                    b.setText(strans(T_ALREADY_RUNNING_EXIT, "Exit"));
                    b.setFont(_themeRegistry.getTheme().BUTTON_FONT);
                    b.addSelectionListener(new FireSelectionListener() { 
                        public void fire() {
                            s.dispose();
                            exit();
                        }
                    });
                    s.addShellListener(new ShellListener() {
                        public void shellActivated(ShellEvent shellEvent) {}
                        public void shellClosed(ShellEvent shellEvent) {
                            s.dispose();
                            exit();
                        }
                        public void shellDeactivated(ShellEvent shellEvent) {}
                        public void shellDeiconified(ShellEvent shellEvent) {}
                        public void shellIconified(ShellEvent shellEvent) {}
                    });
                    s.pack();
                    Rectangle sSize = s.getBounds();
                    Rectangle screenSize = Splash.getScreenSize(s);
                    int x = screenSize.width/2-sSize.width/2;
                    int y = screenSize.height/2-sSize.height/2;
                    s.setBounds(x, y, sSize.width, sSize.height);
                    s.open();
                }
            });
            return 2;
        } else if (lsnr.getLoginFailedCause() != null) {
            return 1;
        } else {
            if (!ok) {
                _ui.errorMessage("Timed out trying to start syndie up.  Please review the logs");
                close(false);
                return 2;
            }
            timer.addEvent("login complete");
            if (engine.newNymCreated())
                _shouldShowWelcome = true;
            timer.addEvent("startupClient complete");
            timer.complete();
            
            return 1;
        }
    }
    
    /** safe translate for startup before the translation registry has been created */
    private String strans(String key, String defValue) {
        if (_translationRegistry != null)
            return _translationRegistry.getText(key, defValue);
        else
            return defValue;
    }
    
    private int promptForPass() {
        final ArrayList loginMutex = new ArrayList();
        _ui.debugMessage("Prompting for the passphrase...");
        _display.asyncExec(new Runnable() { 
            public void run() {
                // show a special warning/error screen
                final Shell s = new Shell(_display, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
                s.setText(strans(T_PASSPHRASE_REQ, "Passphrase..."));
                if (_themeRegistry != null)
                    s.setFont(_themeRegistry.getTheme().SHELL_FONT);
                s.setLayout(new GridLayout(2, false));

                Label l = new Label(s, SWT.SINGLE | SWT.WRAP);
                l.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
                l.setText(strans(T_LOGIN, "Passphrase:"));
                if (_themeRegistry != null)
                    l.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
                final Text pass = new Text(s, SWT.SINGLE | SWT.WRAP);
                pass.setText(TextEngine.DEFAULT_PASS);
                pass.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
                pass.addTraverseListener(new TraverseListener() {
                    public void keyTraversed(TraverseEvent evt) {
                        if (evt.detail == SWT.TRAVERSE_RETURN)
                            recon(pass, s, loginMutex);
                    }
                });
                if (_themeRegistry != null)
                    pass.setFont(_themeRegistry.getTheme().DEFAULT_FONT);

                Button b = new Button(s, SWT.PUSH);
                b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
                b.setText(strans(T_LOGIN_PROCEED, "Login"));
                if (_themeRegistry != null)
                    b.setFont(_themeRegistry.getTheme().BUTTON_FONT);
                b.addSelectionListener(new FireSelectionListener() { 
                    public void fire() {
                        recon(pass, s, loginMutex);
                    }
                });

                b = new Button(s, SWT.PUSH);
                b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
                b.setText(strans(T_LOGIN_EXIT, "Exit"));
                if (_themeRegistry != null)
                    b.setFont(_themeRegistry.getTheme().BUTTON_FONT);
                b.addSelectionListener(new FireSelectionListener() { 
                    public void fire() {
                        s.dispose();
                        close(false);
                    }
                });

                s.addShellListener(new ShellListener() {
                    public void shellActivated(ShellEvent shellEvent) {}
                    public void shellClosed(ShellEvent shellEvent) {
                        s.dispose();
                        close(false);
                    }
                    public void shellDeactivated(ShellEvent shellEvent) {}
                    public void shellDeiconified(ShellEvent shellEvent) {}
                    public void shellIconified(ShellEvent shellEvent) {}
                });
                s.pack();
                Rectangle sSize = s.getBounds();
                Rectangle screenSize = Splash.getScreenSize(s);
                int x = screenSize.width/2-sSize.width/2;
                int y = screenSize.height/2-sSize.height/2;
                s.setBounds(x, y, sSize.width, sSize.height);
                s.open();
            }
        });

        while (true) {
            synchronized (loginMutex) {
                if (loginMutex.size() > 0) {
                    Integer rv = (Integer)loginMutex.remove(0);
                    loginMutex.clear();
                    return rv.intValue();
                } else {
                    try {
                        loginMutex.wait(1000);
                    } catch (InterruptedException ie) {}
                }
            }
        }
    }
    
    private void recon(final Text pass, final Shell s, final ArrayList loginMutex) {
        _login = TextEngine.DEFAULT_LOGIN; //login.getText();
        _passphrase = pass.getText();
        boolean ok = false;
        ok = _client.reconnect(_passphrase);
        if (!ok) {
            _ui.errorMessage("Database password is incorrect");
        } else if (_client.verifyNymKeyEncryption()) {
            _ui.debugMessage("nym keys are valid");
            _client.setPass(_passphrase);
            ok = true;
        } else {
            _ui.debugMessage("database password is correct, but the nym keys are not encrypted with that pass.  b0rked ident!");
            _client.disconnect();
        }
        s.dispose();
        synchronized (loginMutex) {
            loginMutex.clear();
            if (ok)
                loginMutex.add(new Integer(1));
            else
                loginMutex.add(new Integer(0));
            loginMutex.notifyAll();
        }
    }
    
    public String getLogin() { return _login == null ? _login : TextEngine.DEFAULT_LOGIN; }
    public String getPassphrase() { return _passphrase; }
    public void changePassphrase(String pass) {
        _client.changePassphrase(pass);
        _passphrase = pass;
    }
    
    private void show() {
        Monitor mon[] = _display.getMonitors();
        Rectangle rect = null;
        if ( (mon != null) && (mon.length > 1) )
            rect = mon[0].getClientArea();
        else
            rect = _display.getClientArea();
        _shell.setSize(rect.width, rect.height);
        _shell.setMaximized(true);
        _shell.open();
        _shell.forceActive();
        _shell.forceFocus();
    }
    
    void hide() {
        _shell.setMinimized(true);
    }
    
    void showHelp() {        
        long msgId = _client.getMessageId(Constants.HELP_MSG.getScope(), Constants.HELP_MSG.getMessageId());
        MessageInfo msg = _client.getMessage(msgId);
        int page = getHelpPage();
        _ui.debugMessage("view help page " + page);
        new StandaloneMessageViewer(_client, _ui, _shell, msg, page, _navControl, _themeRegistry, _translationRegistry, _bookmarkControl, _banControl, _dataCallback);
    }
    private int getHelpPage() {
        // this will obviously need to be updated as the help text is.  fooey.
        DesktopPanel panel = getCurrentPanel();
        if (panel == null) {
            return 0;
        } else if (panel instanceof ForumSelectionPanel) {
            return 3;
        } else if (panel instanceof MessageTreePanel) {
            return 4;
        } else if (panel instanceof MessagePanel) {
            return 5;
        } else if (panel instanceof MessageEditorPanel) {
            return 6;
        } else if (panel instanceof ProfilePanel) {
            return 7;
        } else if (panel instanceof ResumeablePanel) {
            return 8;
        } else if (panel instanceof SQLPanel) {
            return 9;
        } else if (panel instanceof SyndicatorPanel) {
            return 10;
        } else if (panel instanceof TabPanel) {
            return 11;
        } else if (panel instanceof StartupPanel) {
            return 12;
        } else {
            return 0;
        }
    }
    
    private void initKeyFilters() {
        _display.addFilter(SWT.KeyDown, new Listener() {
            public void handleEvent(Event evt) {
                if (evt.character == SWT.ESC) {
                    DesktopPanel panel = getCurrentPanel();
                    if (panel instanceof ForumSelectionPanel) {
                        ((ForumSelectionPanel)panel).forumSelectorCancelled();
                        evt.type = SWT.None;
                    }
                } else if ( (evt.character == 'l') && ((evt.stateMask & SWT.MOD3) == SWT.MOD3) ) { // ALT+L to show/hide forum selector
                    DesktopPanel panel = getCurrentPanel();
                    if (panel instanceof ForumSelectionPanel)
                        ((ForumSelectionPanel)panel).forumSelectorCancelled();
                    else
                        toggleForumSelectionPanel();
                    evt.type = SWT.None;
                } else if ( (evt.character == 'u') && ((evt.stateMask & SWT.MOD3) == SWT.MOD3) ) { // ALT+u to copy the current URI to the clipboard
                    DesktopPanel panel = getCurrentPanel();
                    SyndieURI uri = panel.getSelectedURI();
                    if (uri != null) {
                        addToClipboard(uri.toString());
                    } else {
                        addToClipboard("");
                        _ui.debugMessage("no uri to add to the clipboard: " + panel + ", setting as blank");
                    }
                    evt.type = SWT.None;
                } else if ( (evt.character == 'f') && ((evt.stateMask & SWT.MOD3) != 0) ) { // ALT-f
                    evt.type = SWT.None;
                    showTaskTree();
                } else if ( (evt.keyCode == SWT.ARROW_DOWN) && ((evt.stateMask & SWT.MOD3) != 0) ) { // ALT-down
                    //_ui.debugMessage("showNext: evt.keyCode=" + evt.keyCode + " state=" + evt.stateMask + " detail=" + evt.detail + " type=" + evt.type + " end=" + evt.end + " doit=" + evt.doit + " start=" + evt.start + " time=" + evt.time + " hashcode=" + evt.hashCode());
                    evt.type = SWT.None;
                    evt.doit = false; // not sure why this is necessary for pressing alt+up/down...
                    showNextPanel();
                } else if ( (evt.keyCode == SWT.ARROW_UP) && ((evt.stateMask & SWT.MOD3) != 0) ) { // ALT-up
                    //_ui.debugMessage("showPrev: evt.keyCode=" + evt.keyCode + " state=" + evt.stateMask + " detail=" + evt.detail + " hashcode=" + evt.hashCode());
                    evt.type = SWT.None;
                    evt.doit = false;
                    showPreviousPanel();
                } else if ( (evt.character == 0x17) && ((evt.stateMask & SWT.MOD1) != 0) ) { // ^W
                    evt.type = SWT.None;
                    DesktopPanel panel = getCurrentPanel();
                    if (panel != null)
                        panel.close();
                    panel = getCurrentPanel();
                    if (panel != null) {
                        _ui.debugMessage("forcing focus onto panel: " + panel);
                        panel.forceFocus();
                    }
                } else if ( (evt.character == '=') && ((evt.stateMask & SWT.MOD1) != 0) ) { // ^= (aka ^+)
                    evt.type = SWT.None;
                    _shell.setRedraw(false);
                    _themeRegistry.increaseFont();
                    _center.layout(true, true);
                    _shell.setRedraw(true);
                } else if ( (evt.character == '-') && ((evt.stateMask & SWT.MOD1) != 0) ) { // ^-
                    evt.type = SWT.None;
                    _shell.setRedraw(false);
                    _themeRegistry.decreaseFont();
                    _center.layout(true, true);
                    _shell.setRedraw(true);
                }
            }
        });
    }
    
    void addToClipboard(final String value) {
        Clipboard clipboard = new Clipboard(_display);
        Object data[] = new Object[] { value };
        TextTransfer transfer = TextTransfer.getInstance();
        TextTransfer types[] = new TextTransfer[] { transfer };
        try {
            clipboard.setContents(data, types);
        } catch (SWTException se) {
            _ui.debugMessage("unable to add to the clipboard: [" + value + "] - " + se.getMessage());
        } catch (SWTError se) {
            _ui.debugMessage("error: unable to add to the clipboard: [" + value + "] - " + se.getMessage());
        }
        clipboard.dispose();
    }
    
    void show(DesktopPanel panel) { show(panel, null, null, null, true); }
    void show(DesktopPanel panel, SyndieURI uri, String name, String desc) { show(panel, uri, name, desc, true); }
    void show(DesktopPanel panel, SyndieURI uri, String name, String desc, boolean notifyPrev) {
        if (panel == null) return;
        
        DesktopEdge infoEdge = panel.getEdgeNorth();
        if (infoEdge != null) {
            _centerInfoStack.topControl = infoEdge.getEdgeRoot();
            _centerInfo.layout();
            setSize(_centerInfo, -1, BORDER_SIZE);
        } else {
            setSize(_centerInfo, -1, 0);
        }
        _centerStack.topControl = panel.getRoot();
        _center.layout();
        _shell.layout(new Control[] { _centerInfo, _center });
        
        //setEdge(_edgeNorth, _edgeNorthStack, panel.getEdgeNorth(), _edgeNorthDefault);
        setEdge(_edgeEast, _edgeEastStack, panel.getEdgeEast(), _edgeEastDefault);
        setEdge(_edgeSouth, _edgeSouthStack, panel.getEdgeSouth(), _edgeSouthDefault);
        setEdge(_edgeWest, _edgeWestStack, panel.getEdgeWest(), _edgeWestDefault);
        panel.shown(this, uri, name, desc);
        if (notifyPrev) {
            if (_curPanelIndex >= 0) {
                if (_curPanelIndex >= _loadedPanels.size())
                    _curPanelIndex = _loadedPanels.size() - 1;
                DesktopPanel prev = (DesktopPanel)_loadedPanels.get(_curPanelIndex);
                if (prev != panel) {
                    prev.hidden();
                    synchronized (_listeners) {
                        for (int i = 0; i < _listeners.size(); i++)
                            ((DesktopListener)_listeners.get(i)).panelHidden(prev);
                    }
                }
            }
        }
        int idx = _loadedPanels.indexOf(panel);
        if (idx >= 0) {
            _curPanelIndex = idx;
        } else {
            _loadedPanels.add(panel);
            _curPanelIndex = _loadedPanels.size()-1;
        }
        synchronized (_listeners) {
            for (int i = 0; i < _listeners.size(); i++)
                ((DesktopListener)_listeners.get(i)).panelShown(panel);
        }
    }
    
    void uriUnhandled(SyndieURI uri) {
        if ( (uri != null) && (uri.isURL()) ) {
            final Shell shell = new Shell(_shell, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
            GridLayout gl = new GridLayout(1, true);
            shell.setLayout(gl);
            shell.setText(_translationRegistry.getText(T_EXTERNAL_TITLE, "External URL selected"));
            
            Text msg = new Text(shell, SWT.WRAP | SWT.MULTI | SWT.READ_ONLY);
            msg.setText(_translationRegistry.getText(T_EXTERNAL_MSG, "The URL selected refers to a resource outside of Syndie.  You may load this in the browser of your choice, but doing so may be risky, as Syndie cannot protect your browser, and even following this link may compromise your identity or security."));
            GridData gd = new GridData(GridData.FILL, GridData.FILL, true, false);
            gd.widthHint = 400;
            msg.setLayoutData(gd);
            
            String urlStr = uri.getURL();
            if (urlStr == null) urlStr = "";
            
            Text url = new Text(shell, SWT.BORDER | SWT.SINGLE);
            url.setText(urlStr);
            gd = new GridData(GridData.FILL, GridData.FILL, true, false);
            gd.widthHint = 400;
            url.setLayoutData(gd);
            
            Button b = new Button(shell, SWT.PUSH);
            b.setText(_translationRegistry.getText(T_EXTERNAL_OK, "Close"));
            gd = new GridData(GridData.FILL, GridData.FILL, true, false);
            gd.widthHint = 400;
            b.setLayoutData(gd);
            b.addSelectionListener(new FireSelectionListener() { public void fire() { shell.dispose(); } });
            
            //shell.setSize(shell.computeSize(400, SWT.DEFAULT));
            shell.pack(true);
            shell.open();
            //url.selectAll()
            url.forceFocus();
        } else {
            MessageBox box = new MessageBox(_shell, SWT.ICON_ERROR | SWT.OK);
            box.setText(_translationRegistry.getText(T_BADURI_TITLE, "Invalid URI"));
            box.setMessage(_translationRegistry.getText(T_BADURI_MSG, "The URI visited is not understood by Syndie: ") + uri);
            box.open();
        }
    }
    
    private static final String T_EXTERNAL_TITLE = "syndie.gui.desktop.desktop.external.title";
    private static final String T_EXTERNAL_MSG = "syndie.gui.desktop.desktop.external.msg";
    private static final String T_EXTERNAL_OK = "syndie.gui.desktop.desktop.external.ok";
    private static final String T_BADURI_TITLE = "syndie.gui.desktop.desktop.baduri.title";
    private static final String T_BADURI_MSG = "syndie.gui.desktop.desktop.baduri.msg";
    
    
    DesktopPanel getCurrentPanel() { 
        if ( (_curPanelIndex >= 0) && (_curPanelIndex < _loadedPanels.size()) )
            return (DesktopPanel)_loadedPanels.get(_curPanelIndex);
        else
            return null;
    }
    List getPanels() { return new ArrayList(_loadedPanels); }
    NavigationControl getNavControl() { return _navControl; }
    void setNavControl(NavigationControl ctl) { _navControl = ctl; }
    BanControl getBanControl() { return _banControl; }
    BookmarkControl getBookmarkControl() { return _bookmarkControl; }
    DataCallback getDataCallback() { return _dataCallback; }
    LocalMessageCallback getLocalMessageCallback() { return _localMessageCallback; }
    Composite getCenter() { return _center; }
    Composite getNorth() { return _centerInfo; }
    Composite getEast() { return _edgeEast; }
    Composite getSouth() { return _edgeSouth; }
    Composite getWest() { return _edgeWest; }
    
    boolean isShowing(DesktopPanel panel) { return getCurrentPanel() == panel; }
    
    void showTaskTree() { _taskTree.show(); }
    void showPreviousPanel() { showPreviousPanel(true); }
    void showPreviousPanel(boolean notifyPrev) {
        if (_loadedPanels.size() > 0) {
            int idx = _curPanelIndex - 1;
            if (idx < 0) idx = _loadedPanels.size()-1;
            DesktopPanel panel = (DesktopPanel)_loadedPanels.get(idx);
            show(panel, null, null, null, notifyPrev);
        }
    }
    void showNextPanel() {
        if (_loadedPanels.size() > 0) {
            int idx = _curPanelIndex + 1;
            if (idx >= _loadedPanels.size()) idx = 0;
            DesktopPanel panel = (DesktopPanel)_loadedPanels.get(idx);
            show(panel, null, null, null);
        }
    }
    void panelDisposed(DesktopPanel panel, boolean showAnother) {
        int idx = _loadedPanels.indexOf(panel);
        if (idx >= 0) {
            _loadedPanels.remove(idx);
            synchronized (_listeners) {
                for (int i = 0; i < _listeners.size(); i++)
                    ((DesktopListener)_listeners.get(i)).destroyed(panel);
            }
            if (_curPanelIndex == idx) {
                if (showAnother)
                    showPreviousPanel(false);
            } else if (_curPanelIndex > idx)
                _curPanelIndex--;
        }
        _ui.debugMessage("panel disposed: " + panel.getClass().getName() + ": idx=" + idx + " curIndex=" + _curPanelIndex + " loaded=" + _loadedPanels.size());
    }
    
    private void setEdge(Composite edge, StackLayout stack, DesktopEdge specificEdge, DesktopEdge defEdge) {
        if (specificEdge != null)
            stack.topControl = specificEdge.getEdgeRoot();
        else
            stack.topControl = defEdge.getEdgeRoot();
        edge.layout();
    }
    
    void startupComplete(boolean ok) {
        if (ok) {        
            _display.asyncExec(new Runnable() { 
                public void run() { 
                    _taskTree = new TaskTreeShell(Desktop.this);
                    ((HideDesktopCorner)_cornerNorthEast).startupComplete();
                    ((SyndicateDesktopCorner)_cornerSouthWest).startupComplete();
                    ((ControlDesktopCorner)_cornerNorthWest).startupComplete();
                    ((HelpDesktopCorner)_cornerSouthEast).startupComplete();
                    _edgeWestDefault.startupComplete();
                    toggleForumSelectionPanel();
                } 
            });
            JobRunner.instance().enqueue(new Runnable() { public void run() { importMsgs(); } });
        }
        //if (ok)
        //    _display.asyncExec(new Runnable() { public void run() { showDesktopTabs(); } });
    }
    
    private void importMsgs() {
        int index = 1;
        while (importMsgs("/import_meta" + index + ".syndie"))
            index++;
        index = 1;
        while (importMsgs("/import_post" + index + ".syndie"))
            index++;
    }
    private boolean importMsgs(String resourceName) {
        try {
            InputStream in = getClass().getResourceAsStream(resourceName);
            if (in == null) {
                return false;
            }
            
            Importer imp = new Importer(_client);
            boolean ok = imp.processMessage(_ui, in, _client.getLoggedInNymId(), _client.getPass(), null, false, null, null);
            return true;
        } catch (IOException ioe) {
            _ui.errorMessage("Error importing packaged message " + resourceName);
            return false;
        }
    }

    TabPanel getTabPanel(boolean create) {
        if ((_tabs == null) && create) {
            _tabs = new TabPanel(_center, this);

            // when switching to the tab panel, close all of the loaded panels so
            // they can be opened in the tabs (if necessary)
            int unclosed = 0;
            while (_loadedPanels.size() > unclosed) {
                DesktopPanel panel = (DesktopPanel)_loadedPanels.get(unclosed);
                _ui.debugMessage("closing " + panel);
                if (panel instanceof StartupPanel) {
                    _ui.debugMessage("never closing the startup panel");
                    unclosed++;
                } else if (!panel.close()) {
                    _ui.debugMessage("could not close " + panel);
                    unclosed++;
                }
            }
        }
        return _tabs;
    }
    void showDesktopTabs() { show(getTabPanel(true), null, null, null); }
    
    void exit() { close(); }
    
    File getRootFile() { return _rootFile; }
    void setDBClient(DBClient client) { _client = client; }
    DBClient getDBClient() { return _client; }
    UI getUI() { return _ui; }
    ThemeRegistry getThemeRegistry() { return _themeRegistry; }
    TranslationRegistry getTranslationRegistry() { return _translationRegistry; }
    
    void setTranslationRegistry(TranslationRegistry trans) { 
        _translationRegistry = trans; 
        _commandBar.setTranslationRegistry(trans);
    }
    void setThemeRegistry(ThemeRegistry themes) { 
        _themeRegistry = themes; 
        _commandBar.setThemeRegistry(themes);
    }
    
    StartupPanel getStartupPanel() { return _startupPanel; }
    //void setThemeRegistry(ThemeRegistry registry) { _themes = registry; }
    //void setTranslationRegistry(TranslationRegistry registry) { _translations = registry; }
    void viewControlMenu() {
        if (_controlPanel == null)
            _controlPanel = new ControlMenuPanel(this, _client, _themeRegistry, _translationRegistry, _center, _ui);
        show(_controlPanel);
    }
    
    void close() { close(true); }
    void close(boolean prompt) {
        if (prompt) {
            MessageBox box = new MessageBox(_shell, SWT.ICON_QUESTION | SWT.YES | SWT.NO);
            box.setMessage("exit?");
            int rc = box.open();
            if (rc != SWT.YES)
                return;
        }
        
        _shell.setVisible(false);
        if (_taskTree != null)
            _taskTree.dispose();
        new Thread(new Runnable() { 
            public void run() {
                if (HTTPServ.isAlive())
                    HTTPServ.killAll();
                System.exit(0); 
            } 
        }).start();
    }
    
    private void prepareGrid() {
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 0;
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        gl.verticalSpacing = 0;
        _shell.setLayout(gl);
        
        _edgeNorthWest = new Composite(_shell, SWT.NONE);
        _edgeNorth = new Composite(_shell, SWT.NONE);
        _edgeNorthEast = new Composite(_shell, SWT.NONE);
        _edgeWest = new Composite(_shell, SWT.NONE);
        _centerInfo = new Composite(_shell, SWT.NONE);
        _edgeEast = new Composite(_shell, SWT.NONE);
        _center = new Composite(_shell, SWT.NONE);
        _edgeSouthWest = new Composite(_shell, SWT.NONE);
        _edgeSouth = new Composite(_shell, SWT.NONE);
        _edgeSouthEast = new Composite(_shell, SWT.NONE);
        
        _edgeNorthWest.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _edgeNorth.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _edgeNorthEast.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        GridData gd = new GridData(GridData.FILL, GridData.FILL, false, true);
        gd.verticalSpan = 2;
        _edgeWest.setLayoutData(gd);
        _centerInfo.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false));
        _edgeEast.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, true));
        gd = new GridData(GridData.FILL, GridData.FILL, false, true);
        gd.verticalSpan = 2;
        _edgeEast.setLayoutData(gd);
        _center.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _edgeSouthWest.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _edgeSouth.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _edgeSouthEast.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _edgeNorthWest.setLayout(new FillLayout());
        _edgeNorthEast.setLayout(new FillLayout());
        _edgeSouthWest.setLayout(new FillLayout());
        _edgeSouthEast.setLayout(new FillLayout());
        
        setSize(_edgeNorthWest, BORDER_SIZE, BORDER_SIZE);
        setSize(_edgeNorthEast, BORDER_SIZE, BORDER_SIZE);
        setSize(_edgeSouthWest, BORDER_SIZE, BORDER_SIZE);
        setSize(_edgeSouthEast, BORDER_SIZE, BORDER_SIZE);
        
        setSize(_edgeNorth, -1, BORDER_SIZE);
        setSize(_centerInfo, -1, BORDER_SIZE);
        setSize(_edgeSouth, -1, BORDER_SIZE);
        setSize(_edgeEast, BORDER_SIZE, -1);
        setSize(_edgeWest, BORDER_SIZE, -1);
        
        _cornerNorthWest = new ControlDesktopCorner(this, SWT.COLOR_BLUE, _edgeNorthWest, _ui);
        _cornerNorthEast = new HideDesktopCorner(this, SWT.COLOR_BLUE, _edgeNorthEast, _ui);
        _cornerSouthEast = new HelpDesktopCorner(this, SWT.COLOR_MAGENTA, _edgeSouthEast, _ui);
        _cornerSouthWest = new SyndicateDesktopCorner(this, SWT.COLOR_BLUE, _edgeSouthWest, _ui);
        
        _commandBar = new CommandBar(this, _edgeNorth, _ui, _translationRegistry, _themeRegistry);
        _edgeEastDefault = new DesktopEdgeDummy(SWT.COLOR_GREEN, _edgeEast, _ui);
        _edgeSouthDefault = new DesktopEdgeDummy(SWT.COLOR_GREEN, _edgeSouth, _ui);
        _edgeWestDefault = new LinkEdge(_edgeWest, _ui, this); //new DesktopEdgeDummy(SWT.COLOR_GREEN, _edgeWest, _ui);
        
        _centerInfoStack = new StackLayout();
        _centerInfo.setLayout(_centerInfoStack);
        
        _centerStack = new StackLayout();
        _center.setLayout(_centerStack);

        _edgeNorth.setLayout(new FillLayout(SWT.HORIZONTAL));
        //_edgeNorthStack = new StackLayout();
        //_edgeNorth.setLayout(_edgeNorthStack);
        _edgeEastStack = new StackLayout();
        _edgeEast.setLayout(_edgeEastStack);
        _edgeSouthStack = new StackLayout();
        _edgeSouth.setLayout(_edgeSouthStack);
        _edgeWestStack = new StackLayout();
        _edgeWest.setLayout(_edgeWestStack);
    }
    
    private static final int BORDER_SIZE = 64;
    
    private void setSize(Composite c, int width, int height) {
        GridData gd = (GridData)c.getLayoutData();
        gd.heightHint = height;
        gd.widthHint = width;
    }

    public void showForumSelectionPanel() { toggleForumSelectionPanel(false, true); }
    public void toggleForumSelectionPanel() { toggleForumSelectionPanel(false, false); }
    public void toggleForumSelectionPanel(boolean startWithRefs) { toggleForumSelectionPanel(startWithRefs, false); }
    private void toggleForumSelectionPanel(boolean startWithRefs, boolean alwaysShow) { 
        if (_forumSelectionPanel == null)
            _forumSelectionPanel = new ForumSelectionPanel(this, _client, _themeRegistry, _translationRegistry, _center, _ui, _navControl, _banControl, _bookmarkControl);
        if (getCurrentPanel() == _forumSelectionPanel) {
            if (alwaysShow) {
                // noop - already showing
            } else {
                _forumSelectionPanel.forumSelectorCancelled();
                //showPreviousPanel();
            }
        } else {
            _forumSelectionPanel.preferRefs(startWithRefs);
            show(_forumSelectionPanel, null, null, null);
        }
    }
    public void showForumManagementSelectionPanel() {
        if (_forumSelectionPanel == null)
            _forumSelectionPanel = new ForumSelectionPanel(this, _client, _themeRegistry, _translationRegistry, _center, _ui, _navControl, _banControl, _bookmarkControl);
        //_forumSelectionPanel.showManageable(this, null, null, null);
        if (getCurrentPanel() != _forumSelectionPanel)
            show(_forumSelectionPanel, null, null, null);
    }
    
    public void refreshPrimaryAvatar() { ((ControlDesktopCorner)_cornerNorthWest).refreshPrimaryAvatar(); }
    
    private static final String T_CONFIRMBAN = "syndie.gui.desktop.desktop.confirmban";
    private static final String T_CONFIRMBAN_NAME = "syndie.gui.desktop.desktop.confirmbanname";
    
    private static final String T_CONFIRMCANCEL = "syndie.gui.desktop.confirmcancel";
    private static final String T_CANCEL_TITLE = "syndie.gui.desktop.cancel.title";
    private static final String T_CANCEL_MSG = "syndie.gui.desktop.cancel.msg";

    private static final String T_CONFIRMDELETE = "syndie.gui.desktop.confirmdelete";
    private static final String T_CONFIRMDELETE_NAME = "syndie.gui.desktop.confirmdelete.name";
    private static final String T_DELETE_TITLE = "syndie.gui.desktop.delete.title";
    private static final String T_DELETE_MSG = "syndie.gui.desktop.delete.msg";
    
    private class DesktopBan implements BanControl {
        public boolean ban(final Hash scope) { 
            String scopeName = _client.getChannelName(scope);
            if (scopeName == null)
                scopeName = "";
            scopeName = scopeName + " [" + scope.toBase64().substring(0,6) + "]";

            MessageBox box = new MessageBox(_shell, SWT.ICON_QUESTION | SWT.YES | SWT.NO);
            box.setMessage(_translationRegistry.getText(T_CONFIRMBAN, 
                    "All of the messages in it will be removed and you will never receive " +
                    "any messages in it again, or posts written by the forum's owner.  Do you want to ban: ") 
                    + scopeName);
            box.setText(_translationRegistry.getText(T_CONFIRMBAN_NAME, "Confirm ban"));
            int rc = box.open();
            if (rc == SWT.YES) {
                JobRunner.instance().enqueue(new Runnable() {
                    public void run() { _client.ban(scope, _ui, true, true); }
                });
                return true;
            } else {
                return false;
            }
        }
            
        public boolean cancelMessage(SyndieURI uri) {
            if ( (uri == null) || (uri.getScope() == null) )
                return false;
            String scopeName = _client.getChannelName(uri.getScope());
            if (scopeName == null)
                scopeName = "";
            scopeName = scopeName + " [" + uri.getScope().toBase64().substring(0,6) + "]";

            long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
            if (msgId >= 0) {
                String subject = _client.getMessageSubject(msgId);
                if (subject != null)
                    scopeName = scopeName + ": " + subject;
            }

            MessageBox box = new MessageBox(_shell, SWT.ICON_QUESTION | SWT.YES | SWT.NO);
            box.setMessage(getTranslationRegistry().getText(T_CONFIRMCANCEL, 
                    "Do you really want to tell everyone to ignore this message: " + scopeName));
            box.setText(getTranslationRegistry().getText(T_CONFIRMBAN_NAME, "Confirm cancel"));
            int rc = box.open();
            if (rc == SWT.YES) {
                doCancel(uri);
                return true;
            } else {
                return false;
            }
        }
        private void doCancel(SyndieURI uri) {
            _client.cancelMessage(uri, getUI());
            //_client.ban(scope, getUI(), true, false); 
            MessageBox box = new MessageBox(_shell, SWT.ICON_INFORMATION | SWT.OK);
            box.setText(getTranslationRegistry().getText(T_CANCEL_TITLE, "Cancelled"));
            box.setMessage(getTranslationRegistry().getText(T_CANCEL_MSG, "Selected message cancelled"));
            box.open();
        }

        public boolean deleteMessage(SyndieURI uri) {
            if ( (uri == null) || (uri.getScope() == null) )
                return false;
            String scopeName = _client.getChannelName(uri.getScope());
            if (scopeName == null)
                scopeName = "";
            scopeName = scopeName + " [" + uri.getScope().toBase64().substring(0,6) + "]";

            long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
            if (msgId >= 0) {
                String subject = _client.getMessageSubject(msgId);
                if (subject != null)
                    scopeName = scopeName + ": " + subject;
            }

            MessageBox box = new MessageBox(_shell, SWT.ICON_QUESTION | SWT.YES | SWT.NO);
            box.setMessage(getTranslationRegistry().getText(T_CONFIRMDELETE, 
                    "Do you really want to locally delete this message: " + scopeName));
            box.setText(getTranslationRegistry().getText(T_CONFIRMDELETE_NAME, "Confirm delete"));
            int rc = box.open();
            if (rc == SWT.YES) {
                doDelete(uri);
                return true;
            } else {
                return false;
            }
        }
        private void doDelete(SyndieURI uri) {
            _client.deleteMessage(uri, getUI(), true);
            //_client.ban(scope, getUI(), true, false); 
            MessageBox box = new MessageBox(_shell, SWT.ICON_INFORMATION | SWT.OK);
            box.setText(getTranslationRegistry().getText(T_DELETE_TITLE, "Deleted"));
            box.setMessage(getTranslationRegistry().getText(T_DELETE_MSG, "Selected message deleted"));
            box.open();
        }
    }
    
    private class DesktopBookmark implements BookmarkControl {
        /** show a popup to bookmark the given uri in the user's set of bookmarked references */
        public void bookmark(SyndieURI uri) {}
        /** show a popup to bookmark the given uri in the user's set of bookmarked references */
        public void bookmark(SyndieURI uri, long parentGroupId) {}
        /** just add the given bookmark.  the node's groupId, siblingOrder, and uriId will be populated */
        public void bookmark(NymReferenceNode node, boolean doneBookmarking) {}
        public void deleteBookmark(long bookmarkGroupId) {}
        public void deleteBookmarks(List bookmarkGroupIds) {}
        public void updateBookmark(NymReferenceNode bookmark) {}
        public void bookmarkCurrentTab() {}
        /** get the bookmarks (NymReferenceNode) currently loaded */
        public List getBookmarks() { return null; }
        public boolean isBookmarked(SyndieURI syndieURI) { return false; }
    }
    
    private class DesktopDataCallback implements DataCallback {
        public void messageImported() {}
        public void metaImported() {}
        public void readStatusUpdated() {}
        public void forumCreated() {}
    }
    
    private class DesktopLocalMessageCallback implements LocalMessageCallback {
        public void messageCreated(SyndieURI postedURI) {}
        public void messagePostponed(long postponementId) {}
        public void messageCancelled() {}
    }
}

class HideDesktopCorner extends DesktopCorner {
    private Desktop _desktop;
    private Button _button;
    public HideDesktopCorner(Desktop desktop, int color, Composite parent, UI ui) {
        super(parent, ui);
        _desktop = desktop;
        initComponents(color);
    }
    public void startupComplete() {
        _button.setImage(ImageUtil.ICON_HIDE);
    }
    private void initComponents(int color) {
        _button = new Button(getRoot(), SWT.PUSH);
        //_button.setBackground(_button.getDisplay().getSystemColor(color));
        _button.addSelectionListener(new FireSelectionListener() { public void fire() { _desktop.hide(); } });
        getRoot().layout(true, true);
    }
}

class HelpDesktopCorner extends DesktopCorner {
    private Desktop _desktop;
    private Button _button;
    public HelpDesktopCorner(Desktop desktop, int color, Composite parent, UI ui) {
        super(parent, ui);
        _desktop = desktop;
        initComponents(color);
    }
    public void startupComplete() {
        _button.setImage(ImageUtil.ICON_HELP);
    }
    private void initComponents(int color) {
        _button = new Button(getRoot(), SWT.PUSH);
        //_button.setBackground(_button.getDisplay().getSystemColor(color));
        _button.addSelectionListener(new FireSelectionListener() { public void fire() { _desktop.showHelp(); } });
        getRoot().layout(true, true);
    }
}

class SyndicateDesktopCorner extends DesktopCorner {
    private Desktop _desktop;
    private Button _button;
    public SyndicateDesktopCorner(Desktop desktop, int color, Composite parent, UI ui) {
        super(parent, ui);
        _desktop = desktop;
        initComponents(color);
    }
    public void startupComplete() {
        _button.setImage(ImageUtil.ICON_TAB_ARCHIVE);
    }
    private void initComponents(int color) {
        _button = new Button(getRoot(), SWT.PUSH);
        //_button.setBackground(_button.getDisplay().getSystemColor(color));
        _button.addSelectionListener(new FireSelectionListener() { 
            public void fire() { 
                _desktop.getNavControl().view(URIHelper.instance().createSyndicationStatusURI());
            } 
        });
        getRoot().layout(true, true);
    }
}

class ControlDesktopCorner extends DesktopCorner {
    private Desktop _desktop;
    private Button _button;
    public ControlDesktopCorner(Desktop desktop, int color, Composite parent, UI ui) {
        super(parent, ui);
        _desktop = desktop;
        initComponents(color);
    }
    public void startupComplete() {
        refreshPrimaryAvatar();
    }
    public void refreshPrimaryAvatar() {
        byte avatar[] = _desktop.getDBClient().getChannelAvatar(0);
        Image img = null;
        if (avatar != null) {
            img = ImageUtil.createImage(avatar);
        }
        if (img != null) {
            ImageUtil.dispose(_button.getImage());
            _button.setImage(img);
        } else {
            _button.setImage(ImageUtil.ICON_TAB_SQL);
        }
    }
    private void initComponents(int color) {
        _button = new Button(getRoot(), SWT.PUSH);
        //_button.setBackground(_button.getDisplay().getSystemColor(color));
        _button.addSelectionListener(new FireSelectionListener() { public void fire() { _desktop.viewControlMenu(); } });
        getRoot().layout(true, true);
    }
}

class CommandBar implements Themeable, Translatable {
    private UI _ui;
    private Composite _parent;
    private Composite _root;
    private Desktop _desktop;
    private Button _syndicate;
    private Button _read;
    private Button _post;
    private Button _manageForum;
    private Button _closePanel;
    private Button _switchPanel;
    private TranslationRegistry _translationRegistry;
    private ThemeRegistry _themeRegistry;

    public CommandBar(Desktop desktop, Composite parent, UI ui, TranslationRegistry trans, ThemeRegistry themes) { 
        _parent = parent;
        _ui = ui;
        _desktop = desktop;
        _translationRegistry = trans;
        _themeRegistry = themes;
        initComponents();
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new FillLayout());
        Composite bar = _root;
        FillLayout fl = new FillLayout(SWT.HORIZONTAL);
        bar.setLayout(fl);
                
        _read = new Button(bar, SWT.PUSH);
        _read.setText("read");
        _read.addSelectionListener(new FireSelectionListener() { public void fire() { _desktop.showForumSelectionPanel(); } }); 
        
        _post = new Button(bar, SWT.PUSH);
        _post.setText("write");
        _post.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                TreeMap resumeable = _desktop.getDBClient().getResumeable();
                if ( (resumeable == null) || (resumeable.size() == 0) ) {
                    // nothing to resume, so jump straight to posting
                    _desktop.getNavControl().view(URIHelper.instance().createPostURI(null, null));
                } else {
                    // lets check to see if they want to resume a postponed message
                    _desktop.getNavControl().view(URIHelper.instance().createResumeableURI());
                }
                if (_desktop.getTabPanel(false) != null)
                    _desktop.showDesktopTabs();
            }
        });

        _syndicate = new Button(bar, SWT.PUSH);
        _syndicate.setText("share");
        _syndicate.addSelectionListener(new FireSelectionListener() { 
            public void fire() { 
                _desktop.getNavControl().view(URIHelper.instance().createSyndicationStatusURI());
                if (_desktop.getTabPanel(false) != null)
                    _desktop.showDesktopTabs();
            } 
        });

        _manageForum = new Button(bar, SWT.PUSH);
        _manageForum.setText("manage");
        _manageForum.addSelectionListener(new FireSelectionListener() { 
            public void fire() { 
                _desktop.showForumManagementSelectionPanel(); 
                if (_desktop.getTabPanel(false) != null)
                    _desktop.showDesktopTabs();
            } 
        }); 
        
        _switchPanel = new Button(bar, SWT.PUSH);
        _switchPanel.setText("switch");
        _switchPanel.addSelectionListener(new FireSelectionListener() { public void fire() { _desktop.showTaskTree(); } });
        
        bar.layout(true, true);
        
        if (_translationRegistry != null)
            _translationRegistry.register(this);
        if (_themeRegistry != null)
            _themeRegistry.register(this);
    }
    
    public void setTranslationRegistry(TranslationRegistry registry) { 
        if (_translationRegistry != null) return;
        if (registry == null) return;
        
        _translationRegistry = registry;
        Display.getDefault().asyncExec(new Runnable() {
            public void run() { _translationRegistry.register(CommandBar.this); }
        });
    }
    public void setThemeRegistry(ThemeRegistry registry) { 
        if (_themeRegistry != null) return;
        if (registry == null) return;
        
        _themeRegistry = registry;
        Display.getDefault().asyncExec(new Runnable() {
            public void run() { _themeRegistry.register(CommandBar.this); }
        });
    }
    
    public void dispose() {
        if (_translationRegistry != null)
            _translationRegistry.unregister(this);
        if (_themeRegistry != null)
            _themeRegistry.unregister(this);
        _root.dispose(); 
    }

    public void applyTheme(Theme theme) {
        _syndicate.setFont(theme.BUTTON_FONT);
        _read.setFont(theme.BUTTON_FONT);
        _post.setFont(theme.BUTTON_FONT);
        _manageForum.setFont(theme.BUTTON_FONT);
        _switchPanel.setFont(theme.BUTTON_FONT);
    }

    public void translate(TranslationRegistry registry) {
        _syndicate.setText(registry.getText(T_SYNDICATE, "Share"));
        _read.setText(registry.getText(T_READ, "Read"));
        _post.setText(registry.getText(T_POST, "Write"));
        _manageForum.setText(registry.getText(T_MANAGE, "Manage"));
        _switchPanel.setText(registry.getText(T_SWITCH, "Switch task"));
    }
    
    private static final String T_SYNDICATE = "syndie.gui.desktop.CommandBar.syndicate";
    private static final String T_READ = "syndie.gui.desktop.CommandBar.read";
    private static final String T_POST = "syndie.gui.desktop.CommandBar.post";
    private static final String T_MANAGE = "syndie.gui.desktop.CommandBar.manage";
    private static final String T_SWITCH = "syndie.gui.desktop.CommandBar.switch";
}
