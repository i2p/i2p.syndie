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
import org.eclipse.swt.widgets.Text;
import syndie.db.DBClient;
import syndie.db.Opts;
import syndie.db.TextEngine;
import syndie.db.UI;
import syndie.data.SyndieURI;

/**
 *
 */
class LogTab extends BrowserTab implements Browser.UIListener, Themeable {
    private Text _out;
    private Text _in;
    private Button _levelError;
    private Button _levelStatus;
    private Button _levelDebug;
    private boolean _error;
    private boolean _status;
    private boolean _debug;
    private boolean _closed;
    
    private int _sizeModifier;
    
    private List _pendingMessages;
    
    private static int MAX_CHARS = 300*120;
    
    public LogTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri);
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
                    }
                }
            }
        }, "LogTabRenderer");
        t.setDaemon(true);
        t.start();
    }
    
    protected void initComponents() {
        _pendingMessages = new ArrayList();
        getRoot().setLayout(new GridLayout(1, true));
        
        _out = new Text(getRoot(), SWT.MULTI | SWT.BORDER | SWT.READ_ONLY | SWT.V_SCROLL | SWT.H_SCROLL);
        _out.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        Group levels = new Group(getRoot(), SWT.NONE);
        levels.setText("Log levels");
        levels.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        levels.setLayout(new FillLayout(SWT.HORIZONTAL));
        _levelError = new Button(levels, SWT.CHECK);
        _levelError.setText("errors");
        _levelError.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _error = _levelError.getSelection(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _error = _levelError.getSelection(); }
        });
        _levelStatus = new Button(levels, SWT.CHECK);
        _levelStatus.setText("status");
        _levelStatus.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _status = _levelStatus.getSelection(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _status = _levelStatus.getSelection(); }
        });
        _levelDebug = new Button(levels, SWT.CHECK);
        _levelDebug.setText("debug");
        _levelDebug.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _debug = _levelDebug.getSelection(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _debug = _levelDebug.getSelection(); }
        });
        
        _debug = super.getURI().getBoolean("debug", false);
        _error = super.getURI().getBoolean("error", true);
        _status = super.getURI().getBoolean("status", true);
        
        _levelError.setSelection(_error);
        _levelStatus.setSelection(_status);
        _levelDebug.setSelection(_debug);
        
        getBrowser().getThemeRegistry().register(this);
        
        getBrowser().addUIListener(this);
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
        getBrowser().removeUIListener(this);
        getBrowser().getThemeRegistry().unregister(this);
        _closed = true; 
        synchronized (_pendingMessages) { 
            _pendingMessages.notify(); 
        }
    }
    
    private static final SimpleDateFormat _fmt = new SimpleDateFormat("yyyy/MM/dd hh:mm:ss.SSS");
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
            _pendingMessages.notify();
        }
    }
    
    /** called by the log thread */
    private void append(final List records) {
        // maybe stylize STATUS/DEBUG/ERROR w/ colors in the out buffer?
        Display.getDefault().syncExec(new Runnable() {
            public void run() {
                if ( (_out == null) || (_out.isDisposed()) ) return;
                _out.setRedraw(false);
                
                while (records.size() > 0) {
                    Record r = (Record)records.remove(0);
                    if (r.msg != null) {
                        _out.append(ts(r.when) + ":");
                        _out.append(" " + r.msg + "\n");
                    }
                    if (r.e != null) {
                        StringWriter out = new StringWriter();
                        r.e.printStackTrace(new PrintWriter(out));
                        _out.append(ts(r.when));
                        _out.append("\n" + out.getBuffer().toString() + "\n");
                    }
                }

                int chars = _out.getCharCount();
                if (chars > MAX_CHARS) {
                    String str = _out.getText(chars-MAX_CHARS, chars);
                    _out.setText(""); // blank then append to use the caret
                    _out.append(str);
                }

                // unnnecessary - append scrolls for us
                /*
                // scroll to the end
                int lines = _out.getLineCount();
                if (lines > 0) {
                    System.out.println("Scroll to line " + lines);
                    _out.setTopIndex(lines-1);
                }
                */
                
                _out.setRedraw(true);
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
    public String getName() { return "Logs"; }
    public String getDescription() { return "Log messages"; }
    
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
}
