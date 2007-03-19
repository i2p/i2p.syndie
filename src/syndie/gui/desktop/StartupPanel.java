package syndie.gui.desktop;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.i2p.I2PAppContext;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;
import syndie.data.Timer;
import syndie.db.DBClient;
import syndie.db.NullUI;
import syndie.db.TextEngine;
import syndie.db.UI;
import syndie.gui.*;

class StartupPanel extends DesktopPanel {
    private Text _text;
    private Display _display;
    private Desktop _desktop;
    private Timer _startupTimer;
    private List _runAfterStartup;
    
    public StartupPanel(Composite parent, DesktopUI ui, Timer timer) {
        super(parent, ui);
        _startupTimer = timer;
        _runAfterStartup = new ArrayList();
        initComponents();
        _display = parent.getDisplay();
        timer.addEvent("startup panel constructed");
        ui.addUI(new NullUI() {
            public void errorMessage(String msg) { errorMessage(msg, null); }
            public void debugMessage(String msg) { debugMessage(msg, null); }
            public void errorMessage(String msg, Exception cause) { append(msg); }
            public void statusMessage(String msg) { append(msg); }
            public void debugMessage(String msg, Exception cause) { append(msg); }
        });
    }
    
    private static final int MAX_SIZE = 100*500;
    private static final int TRIM_SIZE = 100*50;
    private void append(final String msg) {
        _display.asyncExec(new Runnable() { 
            public void run() { 
                int count = _text.getCharCount();
                if (count > MAX_SIZE) {
                    _text.setSelection(0, TRIM_SIZE);
                    _text.insert("");
                }
                _text.append(msg + "\n"); 
            } 
        });
    }
    
    public String getPanelName() { return "console"; }
    public String getPanelDescription() { return "Serves as a window into Syndie's internals"; }
    
    private void initComponents() {
        Composite root = getRoot();
        _text = new Text(root, SWT.BORDER | SWT.WRAP | SWT.READ_ONLY);
        _text.setText("Starting Syndie...\n");
    }
    
    public void shown(Desktop desktop) {
        if (_desktop == null) {
            _startupTimer.addEvent("startup panel shown");
            _desktop = desktop;
            Thread t = new Thread(new Runnable() { public void run() { blockingStartup(); } }, "Desktop startup");
            t.setDaemon(true);
            t.start();
        } else {
            // already shown, meaning there's no more work to do
        }
    }

    /**
     * run outside the SWT thread so that we can do heavyweight things without freezing the UI
     */
    private void blockingStartup() {
        _startupTimer.addEvent("blocking events started");
        boolean ok = execStartupProcess();   
        _startupTimer.addEvent("blocking events complete");
        while (_runAfterStartup.size() > 0) {
            ((Runnable)_runAfterStartup.remove(0)).run();
            _startupTimer.addEvent("doStartup: run deferred");
        }
        _startupTimer.addEvent("runAfterStartup events complete");
        _startupTimer.complete();
        _desktop.startupComplete(ok);
        //try { Thread.sleep(120*1000); } catch (InterruptedException ie) {}
        //_display.asyncExec(new Runnable() { public void run() { _desktop.exit(); } });
    }
    
    private boolean execStartupProcess() {
        if (!startClient()) return false;
 
        final DBClient client = _desktop.getDBClient();
        _ui.debugMessage("startup: loggedIn? " + client.isLoggedIn());
        long beforeInit = System.currentTimeMillis();
        if (client.isLoggedIn()) {
            
            _startupTimer.addEvent("begin initComponents");
            _display.syncExec(new Runnable() { public void run() { ImageUtil.init(client.getTempDir()); } });
            _startupTimer.addEvent("image init");
            SpellUtil.init();
            _startupTimer.addEvent("spell init");
            
            // doing this at the start (if we are logged in) means we don't need
            // to retheme the components later
            _display.syncExec(new Runnable() { public void run() {
                ThemeRegistry themes = new ThemeRegistry(client, _ui, null);
                themes.loadTheme();
                _desktop.setThemeRegistry(themes);
            }});
            TranslationRegistry trans = new TranslationRegistry(_ui, client.getRootDir());
            _desktop.setTranslationRegistry(trans);
            //_startupTimer.addEvent("doStartup themes loaded");

            // doStartup stuff
            
            //enableKeyFilters();
            _startupTimer.addEvent("doStartup key filters loaded");
            return true;
        } else {
            _ui.errorMessage("Not logged in... do you have an old instance of Syndie (pre-1.0)?  If so, try doing a clean install after backing up your keys");
            return false;
        }
    }
    
    /**
     * If true, remove the 30 second timeout when connecting to the database,
     * as slow computers w/ lots in their redo logs could take that long.  The
     * reason the timeout is here in the first place is to deal with those 
     * running Syndie off pre-1.0 installs that didn't automatically log in
     */
    private static final boolean ALLOW_SLOW_STARTUP = true;
    
    private boolean startClient() {
        StartupListener lsnr = new StartupListener();
        DBClient client = new DBClient(I2PAppContext.getGlobalContext(), _desktop.getRootFile());
        //final Browser browser = new Browser(client);
        //final Timer timer = new Timer("swtUI startup", browser.getUI());
        final UI ui = _ui;
        final TextEngine engine = new TextEngine(client, ui, lsnr);
        _startupTimer.addEvent("text engine instantiated");
        client.setDefaultUI(ui);
        
        Thread t = new Thread(new Runnable() {
            public void run() {
                ui.debugMessage("starting the engine");
                try {
                    engine.run();
                } catch (Exception e) {
                    ui.errorMessage("error running the engine", e);
                }
                ui.debugMessage("engine stopped");
            }
        }, "text ui");
        t.setPriority(Thread.MIN_PRIORITY);
        t.start();
        _startupTimer.addEvent("text engine started");
        
        ui.debugMessage("waiting for login completion...");
        
        // to allow the startup scripts to run, which may include 'login',
        // so we dont have to show a login prompt.  perhaps toss up a splash screen
        boolean ok = lsnr.waitFor("login", ALLOW_SLOW_STARTUP ? -1 : 30*1000);
        if (lsnr.getAlreadyRunning()) {
            // show a special warning/error screen
            /*
            final Shell s = new Shell(d, SWT.DIALOG_TRIM);
            s.setText(browser.getTranslationRegistry().getText(T_ALREADY_RUNNING_TITLE, "Already running"));
            s.setLayout(new GridLayout(1, true));
            Label l = new Label(s, SWT.SINGLE | SWT.WRAP);
            l.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            l.setText(browser.getTranslationRegistry().getText(T_ALREADY_RUNNING, "Syndie is already running - please use the existing Syndie window"));
            Button b = new Button(s, SWT.PUSH);
            b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            b.setText(browser.getTranslationRegistry().getText(T_ALREADY_RUNNING_EXIT, "Exit"));
            b.addSelectionListener(new FireSelectionListener() { 
                public void fire() {
                    s.dispose();
                    System.exit(-1);
                }
            });
            s.addShellListener(new ShellListener() {
                public void shellActivated(ShellEvent shellEvent) {}
                public void shellClosed(ShellEvent shellEvent) {
                    s.dispose();
                    System.exit(-1);
                }
                public void shellDeactivated(ShellEvent shellEvent) {}
                public void shellDeiconified(ShellEvent shellEvent) {}
                public void shellIconified(ShellEvent shellEvent) {}
            });
            Splash.dispose();
            s.pack();
            Rectangle sSize = s.getBounds();
            Rectangle screenSize = Splash.getScreenSize(s);
            int x = screenSize.width/2-sSize.width/2;
            int y = screenSize.height/2-sSize.height/2;
            s.setBounds(x, y, sSize.width, sSize.height);
            s.open();
             */
            return false;
        } else if (lsnr.getLoginFailedCause() != null) {
            // show a special warning/error screen
            /*
            final Shell s = new Shell(d, SWT.DIALOG_TRIM);
            s.setText(browser.getTranslationRegistry().getText(T_LOGIN_FAILED_TITLE, "Internal error"));
            s.setLayout(new GridLayout(1, true));
            Label l = new Label(s, SWT.SINGLE | SWT.WRAP);
            l.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            l.setText(browser.getTranslationRegistry().getText(T_LOGIN_FAILED, "Syndie ran into an internal error trying to start up - please see the logs: ") + lsnr.getLoginFailedCause().getMessage());
            Button b = new Button(s, SWT.PUSH);
            b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            b.setText(browser.getTranslationRegistry().getText(T_LOGIN_FAILED_EXIT, "Exit"));
            b.addSelectionListener(new FireSelectionListener() { 
                public void fire() {
                    s.dispose();
                    System.exit(-1);
                }
            });
            s.addShellListener(new ShellListener() {
                public void shellActivated(ShellEvent shellEvent) {}
                public void shellClosed(ShellEvent shellEvent) {
                    s.dispose();
                    System.exit(-1);
                }
                public void shellDeactivated(ShellEvent shellEvent) {}
                public void shellDeiconified(ShellEvent shellEvent) {}
                public void shellIconified(ShellEvent shellEvent) {}
            });
            Splash.dispose();
            s.pack();
            Rectangle sSize = s.getBounds();
            Rectangle screenSize = Splash.getScreenSize(s);
            int x = screenSize.width/2-sSize.width/2;
            int y = screenSize.height/2-sSize.height/2;
            s.setBounds(x, y, sSize.width, sSize.height);
            s.open();
             */
            return false;
        } else {
            if (!ok) {
                ui.errorMessage("Timed out trying to start syndie up.  Please review the logs");
                _desktop.exit();
                return false;
            }
            _startupTimer.addEvent("login complete");
            if (engine.newNymCreated()) {
                /*
                WelcomeScreen screen = new WelcomeScreen(d, browser, new WelcomeScreen.CompleteListener() {
                    public void complete() {
                        browser.startup(timer);
                    }
                });
                screen.open();
                 */
            } else {
                ui.debugMessage("db login complete, starting browser...");
                //browser.startup(timer);
                ui.debugMessage("browser started");
            }
            _startupTimer.addEvent("startupClient complete");
            _desktop.setDBClient(client);
            return true;
        }
    }
    
    private void addMsg(final String msg) {
        _display.asyncExec(new Runnable() { public void run() { _text.append(msg + "\n"); } });
    }
}

class StartupListener implements TextEngine.ScriptListener {
    private Set _complete;
    private boolean _alreadyRunning;
    private Exception _loginFailedCause;

    public StartupListener() { 
        _complete = new HashSet(); 
        _alreadyRunning = false;
    }
    public void scriptComplete(String script) {
        synchronized (_complete) { _complete.add(script); _complete.notifyAll(); }
    }
    public void alreadyRunning() { 
        _alreadyRunning = true; 
        synchronized (_complete) { _complete.notifyAll(); } 
    }
    public void loginFailed(Exception cause) {
        _loginFailedCause = cause;
        synchronized (_complete) { _complete.notifyAll(); }
    }
    public boolean getAlreadyRunning() { return _alreadyRunning; }
    public Exception getLoginFailedCause() { return _loginFailedCause; }
    public boolean waitFor(String scriptName, long maxPeriod) {
        long endAt = System.currentTimeMillis() + maxPeriod;
        for (;;) {
            if (_alreadyRunning) return false;
            if (_loginFailedCause != null) return false;
            long remaining = -1;
            if (maxPeriod > 0) {
                remaining = endAt - System.currentTimeMillis();
                if (remaining <= 0) return false;
            }
            try {
                synchronized (_complete) {
                    if (_complete.contains(scriptName))
                        return true;
                    else if (maxPeriod > 0)
                        _complete.wait(remaining);
                    else
                        _complete.wait();
                }
            } catch (InterruptedException ie) {}
        }
    }
}
