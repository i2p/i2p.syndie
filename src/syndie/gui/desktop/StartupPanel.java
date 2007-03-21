package syndie.gui.desktop;

import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.i2p.I2PAppContext;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;
import syndie.data.SyndieURI;
import syndie.data.Timer;
import syndie.db.DBClient;
import syndie.db.NullUI;
import syndie.db.TextEngine;
import syndie.db.UI;
import syndie.gui.*;

class StartupPanel extends DesktopPanel implements Themeable {
    private Text _text;
    private Display _display;
    private Timer _startupTimer;
    private List _runAfterStartup;
    private boolean _initialized;
    
    private boolean _debug;
    private boolean _status;
    private boolean _error;
    private List _pendingMessages;
    
    public StartupPanel(Desktop desktop, Composite parent, DesktopUI ui, Timer timer) {
        super(desktop, parent, ui, null);
        _startupTimer = timer;
        _runAfterStartup = new ArrayList();
        _initialized = false;
        _debug = true;
        _status = true;
        _error = true;
        _pendingMessages = new ArrayList();
        initComponents();
        _display = parent.getDisplay();
        timer.addEvent("startup panel constructed");
        ui.addUI(new NullUI() {
            public void errorMessage(String msg) { append(ERROR, msg); }
            public void errorMessage(String msg, Exception cause) { append(ERROR, msg, cause); }
            public void statusMessage(String msg) { append(STATUS, msg); }
            public void debugMessage(String msg) { append(DEBUG, msg); }
            public void debugMessage(String msg, Exception cause) { append(DEBUG, msg, cause); }
            public void commandComplete(int status, List location) {}
        });
        
        Thread t = new Thread(new Runnable() {
            public void run() {
                List records = new ArrayList();
                while (true) {
                    synchronized (_pendingMessages) {
                        if (_pendingMessages.size() > 0) {
                            records.addAll(_pendingMessages);
                            _pendingMessages.clear();
                        } else {
                            try {
                                _pendingMessages.wait();
                            } catch (InterruptedException ie) {
                                if (_pendingMessages.size() > 0) {
                                    records.addAll(_pendingMessages);
                                    _pendingMessages.clear();
                                }
                            }
                        }
                    }
                    if (records.size() > 0) {
                        append(records);
                        try { Thread.sleep(1000); } catch (InterruptedException ie) {}
                    }
                }
            }
        }, "StartupRenderer");
        t.setDaemon(true);
        t.start();
    }
    
    
    private void append(int type, String msg) { append(type, msg, null); }
    private void append(final int type, final String msg, final Exception e) {
        if ( (DEBUG == type) && (!_debug) ) return;
        if ( (STATUS == type) && (!_status) ) return;
        if ( (ERROR == type) && (!_error) ) return;
        synchronized (_pendingMessages) {
            _pendingMessages.add(new Record(type, msg, e));
            _pendingMessages.notifyAll();
        }
    }
    
    private static final SimpleDateFormat _fmt = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss.SSS");
    private static final String ts(long when) { 
        synchronized (_fmt) {
            return _fmt.format(new Date(when));
        }
    }
    
    private static final int MAX_SIZE = 100*500;
    private static final int TRIM_SIZE = 100*50;
    
    /** called by the log thread */
    private void append(final List records) {
        if (records.size() <= 0) return;
        try {
            StringBuffer buf = new StringBuffer();
            while (records.size() > 0) {
                Record r = (Record)records.remove(0);
                if (r.msg != null)
                    buf.append(ts(r.when)).append(": ").append(r.msg).append("\n");
                if (r.e != null) {
                    StringWriter out = new StringWriter();
                    r.e.printStackTrace(new PrintWriter(out));
                    buf.append(ts(r.when)).append("\n").append(out.getBuffer().toString()).append("\n");
                }
            }
            
            final String str = buf.toString();
            _display.asyncExec(new Runnable() { 
                public void run() { 
                    int count = _text.getCharCount();
                    int strlen = str.length();
                    int extra = count + strlen - MAX_SIZE;
                    if (extra < 0) {
                        _text.append(str);
                    } else {
                        if (extra >= count) {
                            _text.selectAll();
                            _text.insert(str);
                        } else {
                            _text.setSelection(0, extra);
                            _text.insert("");
                            _text.append(str);
                        }
                    }
                } 
            });
            
        } catch (OutOfMemoryError oom) {
            System.err.println("OOM appending to the log tab");
        }
    }
    
    private static final int DEBUG = 1;
    private static final int STATUS = 2;
    private static final int ERROR = 3;

    private static class Record {
        long when;
        int type;
        String msg;
        Exception e;
        public Record(int stat, String newMsg) { this(stat, newMsg, null); }
        public Record(int stat, String newMsg, Exception cause) { when = System.currentTimeMillis(); type = stat; msg = newMsg; e = cause; }
    }
    
    public String getPanelName() { return "console"; }
    public String getPanelDescription() { return "Serves as a window into Syndie's internals"; }
    
    private void initComponents() {
        Composite root = getRoot();
        _text = new Text(root, SWT.BORDER | SWT.READ_ONLY | SWT.V_SCROLL | SWT.H_SCROLL);
        _text.setText("Starting Syndie...\n");
        if (_themeRegistry != null)
            _themeRegistry.register(this);
    }
    
    public void dispose() {
        if (_themeRegistry != null)
            _themeRegistry.unregister(this);
        super.dispose();
    }
    
    public void shown(Desktop desktop, SyndieURI uri, String suggestedName, String suggestedDescription) {
        if (!_initialized) {
            _initialized = true;
            _startupTimer.addEvent("startup panel shown");
            _desktop = desktop;
            Thread t = new Thread(new Runnable() { public void run() { blockingStartup(); } }, "Desktop startup");
            t.setDaemon(true);
            t.start();
        } else {
            // already shown, meaning there's no more work to do
        }
        super.shown(desktop, uri, suggestedName, suggestedDescription);
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
                themes.register(StartupPanel.this);
                themes.register((SouthEdge)_edgeSouth);
                ComponentBuilder.instance().setThemeRegistry(themes);
            }});
            final TranslationRegistry trans = new TranslationRegistry(_ui, client.getRootDir());
            _desktop.setTranslationRegistry(trans);
            if (_edgeSouth != null) {
                _display.asyncExec(new Runnable() { 
                    public void run() { trans.register((SouthEdge)_edgeSouth); }
                });
            }
            ComponentBuilder.instance().setTranslationRegistry(trans);
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
        StartupListener lsnr = new StartupListener(_startupTimer);
        I2PAppContext ctx = I2PAppContext.getGlobalContext();
        _startupTimer.addEvent("i2pappcontext fetched");
        Object o = ctx.logManager();
        _startupTimer.addEvent("logManager warmed up");
        o = ctx.keyGenerator();
        _startupTimer.addEvent("keyGenerator warmed up");
        DBClient client = new DBClient(ctx, _desktop.getRootFile());
        ComponentBuilder.instance().setDBClient(client);
        _startupTimer.addEvent("db client instantiated");
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
    
    public void applyTheme(Theme theme) { _text.setFont(theme.LOG_FONT); }
    
    public void buildSouth(Composite edge) { if (_edgeSouth == null) _edgeSouth = new SouthEdge(edge, _ui); }
    
    private static final String T_CLEAR = "syndie.gui.desktop.startuppanel.clear";
    private class SouthEdge extends DesktopEdge implements Themeable, Translatable {
        private Button _clear;
        public SouthEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        private void initComponents() {
            Composite edge = getEdgeRoot();
            edge.setLayout(new FillLayout());
            _clear = new Button(edge, SWT.PUSH);
            _clear.addSelectionListener(new FireSelectionListener() {
                public void fire() { _text.selectAll(); _text.insert(""); }
            });
            if (_translationRegistry != null)
                _translationRegistry.register(SouthEdge.this);
            if (_themeRegistry != null)
                _themeRegistry.register(SouthEdge.this);
        }
        public void dispose() {
            _translationRegistry.unregister(SouthEdge.this);
            _themeRegistry.unregister(SouthEdge.this);
        }
        public void translate(TranslationRegistry registry) {
            _clear.setText(registry.getText(T_CLEAR, "Clear messages"));
        }
        public void applyTheme(Theme theme) { _clear.setFont(theme.BUTTON_FONT); }
    }
}

class StartupListener implements TextEngine.ScriptListener {
    private Set _complete;
    private boolean _alreadyRunning;
    private Exception _loginFailedCause;
    private Timer _timer;

    public StartupListener(Timer timer) { 
        _complete = new HashSet(); 
        _alreadyRunning = false;
        _timer = timer;
    }
    public void scriptComplete(String script) {
        _timer.addEvent("script complete: " + script);
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
