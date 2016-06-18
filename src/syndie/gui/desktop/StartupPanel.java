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
import syndie.util.Timer;
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
    
    private static final boolean MSGS_TO_STDERR = false;
    
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
    
    protected boolean canClose() { return false; }
    
    public void forceFocus() { _text.forceFocus(); }
    
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
            StringBuilder buf = new StringBuilder();
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
            if (MSGS_TO_STDERR)
                System.err.print(str);
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
        //int type;
        String msg;
        Exception e;
        public Record(int stat, String newMsg) { this(stat, newMsg, null); }
        public Record(int stat, String newMsg, Exception cause) { when = System.currentTimeMillis(); /* type = stat; */ msg = newMsg; e = cause; }
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
        //long beforeInit = System.currentTimeMillis();
        if (client.isLoggedIn()) {
            
            _startupTimer.addEvent("begin initComponents");
            _display.syncExec(new Runnable() { public void run() { ImageUtil.init(client.getTempDir(), _startupTimer); } });
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
            final TranslationRegistry trans = new TranslationRegistry(client, _ui, client.getRootDir());
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
    
    private boolean startClient() {
        I2PAppContext ctx = I2PAppContext.getGlobalContext();
        _startupTimer.addEvent("i2pappcontext fetched");
        ctx.logManager();
        _startupTimer.addEvent("logManager warmed up");
        ctx.keyGenerator();
        _startupTimer.addEvent("keyGenerator warmed up");
        DBClient client = new DBClient(ctx, _desktop.getRootFile());
        client.setDefaultUI(_ui);
        _desktop.setDBClient(client);
        ComponentBuilder.instance().setDBClient(client);
        _startupTimer.addEvent("db client instantiated");
        final UI ui = _ui;
        return _desktop.startEngine();
    }
    
    public void applyTheme(Theme theme) { _text.setFont(theme.LOG_FONT); }
    
    public void buildSouth(Composite edge) { if (_edgeSouth == null) _edgeSouth = new SouthEdge(edge, _ui); }
    
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
            _clear.setText(registry.getText("Clear messages"));
        }
        public void applyTheme(Theme theme) { _clear.setFont(theme.BUTTON_FONT); }
    }
}
