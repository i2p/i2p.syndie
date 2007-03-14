package syndie.gui;

import java.io.StringWriter;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Text;
import syndie.db.DBClient;
import syndie.db.Opts;
import syndie.db.TextEngine;
import syndie.db.UI;
import syndie.data.SyndieURI;

/**
 *
 */
class LogTab extends BrowserTab implements UI, Themeable, Translatable {
    private Text _out;
    private StringBuffer _outBuf;
    private MenuItem _menuClear;
    private Group _levels;
    private Button _levelError;
    private Button _levelStatus;
    private Button _levelDebug;
    private boolean _error;
    private boolean _status;
    private boolean _debug;
    private boolean _closed;
    private String _name;
    private String _desc;
    
    private int _sizeModifier;
    
    private List _pendingMessages;
    
    private static int MAX_CHARS = 1000*40;
    
    public LogTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri);
        _debug = false;
        _error = true;
        _status = true;
        _sizeModifier = 0;
        Thread t = new Thread(new Runnable() {
            public void run() {
                List records = new ArrayList();
                while (!_closed) {
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
                        try { Thread.sleep(500); } catch (InterruptedException ie) {}
                    }
                }
            }
        }, "LogTabRenderer");
        t.setDaemon(true);
        t.start();
    }
    
    public boolean canShow(SyndieURI uri) {
        boolean rv = false;
        if (super.canShow(uri)) {
            rv = true;
        } else {
            if (BrowserTab.TYPE_LOGS.equals(uri.getType()))
                rv = true;
            else
                rv = false;
        }
        return rv;
    }
    
    public void show(SyndieURI uri) { updateFlags(uri); }
    
    private void updateFlags(SyndieURI uri) {
        _debug = uri.getBoolean("debug", _debug);
        _error = uri.getBoolean("error", _error);
        _status = uri.getBoolean("status", _status);
        
        _levelError.setSelection(_error);
        _levelStatus.setSelection(_status);
        _levelDebug.setSelection(_debug);
    }
    
    protected void initComponents() {
        _pendingMessages = new ArrayList();
        getRoot().setLayout(new GridLayout(1, true));
        
        _outBuf = new StringBuffer();
        _out = new Text(getRoot(), SWT.MULTI | SWT.BORDER | SWT.READ_ONLY | SWT.V_SCROLL | SWT.H_SCROLL);
        _out.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        Menu menu = new Menu(_out);
        _out.setMenu(menu);
        _menuClear = new MenuItem(menu, SWT.PUSH);
        _menuClear.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _outBuf.setLength(0); redrawOut(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _outBuf.setLength(0); redrawOut(); }
        });
        
        _levels = new Group(getRoot(), SWT.NONE);
        _levels.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _levels.setLayout(new FillLayout(SWT.HORIZONTAL));
        _levelError = new Button(_levels, SWT.CHECK);
        _levelError.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _error = _levelError.getSelection(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _error = _levelError.getSelection(); }
        });
        _levelStatus = new Button(_levels, SWT.CHECK);
        _levelStatus.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _status = _levelStatus.getSelection(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _status = _levelStatus.getSelection(); }
        });
        _levelDebug = new Button(_levels, SWT.CHECK);
        _levelDebug.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _debug = _levelDebug.getSelection(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _debug = _levelDebug.getSelection(); }
        });
        
        updateFlags(super.getURI());
        
        _themeRegistry.register(this);
        _translationRegistry.register(this);
        
        _ui.addUI(this);
        //getBrowser().addUIListener(this);
    }
    
    public SyndieURI getURI() {
        SyndieURI old = super.getURI();
        Map attributes = old.getAttributes();
        attributes.put("debug", _levelDebug.getSelection() ? Boolean.TRUE : Boolean.FALSE);
        attributes.put("status", _levelStatus.getSelection() ? Boolean.TRUE : Boolean.FALSE);
        attributes.put("error", _levelError.getSelection() ? Boolean.TRUE : Boolean.FALSE);
        return new SyndieURI(old.getType(), attributes);
    }

    protected void disposeDetails() { 
        _ui.removeUI(this);
        _themeRegistry.unregister(this);
        _translationRegistry.unregister(this);
        _closed = true; 
        synchronized (_pendingMessages) { 
            _pendingMessages.notifyAll();
        }
    }
    
    private static final SimpleDateFormat _fmt = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss.SSS");
    private static final String ts(long when) { 
        synchronized (_fmt) {
            return _fmt.format(new Date(when));
        }
    }
    
    private Color _tsBGColor = ColorUtil.getColor("gray", null);
    private Color _tsFGColor = ColorUtil.getColor("black", null);
    private Color _statusColor = ColorUtil.getColor("yellow", null);
    private Color _debugColor = ColorUtil.getColor("cyan", null);
    private Color _errorColor = ColorUtil.getColor("red", null);
    
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
    
    /** called by the log thread */
    private void append(final List records) {
        if (records.size() <= 0) return;
        try {
            while (records.size() > 0) {
                Record r = (Record)records.remove(0);
                if (r.msg != null) {
                    _outBuf.insert(0, ts(r.when) + ": " + r.msg + "\n");
                }
                if (r.e != null) {
                    StringWriter out = new StringWriter();
                    r.e.printStackTrace(new PrintWriter(out));
                    _outBuf.insert(0, ts(r.when) + "\n" + out.getBuffer().toString() + "\n");
                }
            }
            redrawOut();
        } catch (OutOfMemoryError oom) {
            System.err.println("OOM appending to the log tab");
        }
    }
    private void redrawOut() {
        int chars = _outBuf.length();
        if (chars > MAX_CHARS)
            _outBuf.delete(MAX_CHARS, chars);

        final String str = _outBuf.toString();
        final int strlen = str.length();
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                if ( (_out == null) || (_out.isDisposed()) ) return;
                _out.setText(str);
            }
        });
    }
    
    private static final int DEBUG = 1;
    private static final int STATUS = 2;
    private static final int ERROR = 3;

    public void errorMessage(String msg) { append(ERROR, msg); }
    public void errorMessage(String msg, Exception cause) { append(ERROR, msg, cause); }
    public void statusMessage(String msg) { append(STATUS, msg); }
    public void debugMessage(String msg) { append(DEBUG, msg); }
    public void debugMessage(String msg, Exception cause) { append(DEBUG, msg, cause); }
    public void commandComplete(final int status, final List location) {}
    
    public Image getIcon() { return ImageUtil.ICON_TAB_LOGS; }
    public String getName() { return _name; }
    public String getDescription() { return _desc; }
    
    private static class Record {
        long when;
        int type;
        String msg;
        Exception e;
        public Record(int stat, String newMsg) { this(stat, newMsg, null); }
        public Record(int stat, String newMsg, Exception cause) { when = System.currentTimeMillis(); type = stat; msg = newMsg; e = cause; }
    }
    
    public void applyTheme(Theme theme) {
        _out.setFont(theme.LOG_FONT);
    }
    
    private static final String T_MENU_CLEAR = "syndie.gui.logtab.menuclear";
    private static final String T_NAME = "syndie.gui.logtab.name";
    private static final String T_DESC = "syndie.gui.logtab.desc";
    private static final String T_DEBUG = "syndie.gui.logtab.debug";
    private static final String T_STATUS = "syndie.gui.logtab.status";
    private static final String T_ERROR = "syndie.gui.logtab.error";
    private static final String T_LEVELS = "syndie.gui.logtab.levels";
    
    public void translate(TranslationRegistry registry) {
        _menuClear.setText(registry.getText(T_MENU_CLEAR, "Clear records"));
        _name = registry.getText(T_NAME, "Logs");
        _desc = registry.getText(T_DESC, "Log messages");
        _levelDebug.setText(registry.getText(T_DEBUG, "Debug"));
        _levelStatus.setText(registry.getText(T_STATUS, "Status"));
        _levelError.setText(registry.getText(T_ERROR, "Error"));
        _levels.setText(registry.getText(T_LEVELS, "Log levels"));
        
        reconfigItem();
    }

    // unused UI methods
    public Opts readCommand() { return null; }
    public Opts readCommand(boolean displayPrompt) { return null; }
    public boolean toggleDebug() { return false; }
    public boolean togglePaginate() { return false; }
    public void insertCommand(String commandline) {}
    public String readStdIn() { return null; }
    public void addUI(UI ui) {}
    public void removeUI(UI ui) {}
}
