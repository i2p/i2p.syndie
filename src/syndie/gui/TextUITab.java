package syndie.gui;

import java.io.StringWriter;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;

import syndie.db.DBClient;
import syndie.db.Opts;
import syndie.db.TextEngine;
import syndie.db.UI;
import syndie.data.SyndieURI;

/**
 *
 */
class TextUITab extends BrowserTab implements UI {
    private StyledText _out;
    private Text _in;
    private Button _exec;
    private boolean _debug;
    
    private volatile boolean _showedHelp;
    private volatile boolean _closed;
    private final List<Record> _pendingMessages;
    
    public TextUITab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri);
        _pendingMessages = new ArrayList();
        
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
        }, "TextUITabRenderer");
        t.setDaemon(true);
        t.start();
    }
    
    protected void initComponents() {
        getRoot().setLayout(new GridLayout(2, false));
        
        _out = new StyledText(getRoot(), SWT.MULTI | SWT.BORDER | SWT.READ_ONLY | SWT.V_SCROLL | SWT.H_SCROLL);
        _out.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
        
        _in = new Text(getRoot(), SWT.SINGLE | SWT.BORDER);
        _in.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _in.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN)
                    runCommand();
            }
        });
        
        _exec = new Button(getRoot(), SWT.PUSH);
        _exec.setText("execute");
        _exec.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { runCommand(); }
            public void widgetSelected(SelectionEvent selectionEvent) { runCommand(); }
        });
        
        //getRoot().setTabList(new Control[] { _in, _exec, _out });
        
        _ui.addUI(this);
    }

    @Override
    public void show(SyndieURI uri) {
        super.show(uri);
        if (_showedHelp)
            return;
        _showedHelp = true;
        statusMessage("Enter commands in the box below.");
        statusMessage("Type 'help' for help.");
        statusMessage("Type 'exit' or 'quit' to close the tab.");
    }

    @Override
    public void tabShown() {
        super.tabShown();
        _in.forceFocus();
    }

    protected void disposeDetails() { 
        _ui.removeUI(this);
        _closed = true; 
        synchronized (_pendingMessages) { 
            _pendingMessages.notifyAll(); 
        }
    }
    
    private void runCommand() {
        _out.setRedraw(false);
        String cmd = _in.getText().trim();
        // parse it here too, just to catch exit/quit
        Opts opts = new Opts(cmd);
        String cmdStr = opts.getCommand();
        if (cmdStr != null &&
           ("exit".equalsIgnoreCase(cmd) || "quit".equalsIgnoreCase(cmd))) {
           dispose();
           return;
        }
        if (cmd.length() > 0)
            getBrowser().getUI().insertCommand(cmd);
        _in.setText("");
    }
    
    private static final SimpleDateFormat _fmt = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss.SSS");
    private static final String now() { 
        synchronized (_fmt) { 
            return _fmt.format(new Date(System.currentTimeMillis()));
        }
    }
    
    private static int MAX_LINES = 100;
    
    private Color _tsBGColor = ColorUtil.getColor("gray", null);
    private Color _tsFGColor = ColorUtil.getColor("black", null);
    private Color _statusColor = ColorUtil.getColor("yellow", null);
    private Color _debugColor = ColorUtil.getColor("cyan", null);
    private Color _errorColor = ColorUtil.getColor("red", null);
    
    private void append(int type, String msg) { append(type, msg, null); }
    private void append(final int type, final String msg, final Exception e) {
        if ( (DEBUG == type) && (!_debug) ) return;
        synchronized (_pendingMessages) {
            _pendingMessages.add(new Record(type, msg, e));
            _pendingMessages.notifyAll();
        }
    }
    
    /** called by the ui display thread */
    private void append(final List records) {
        // maybe stylize STATUS/DEBUG/ERROR w/ colors in the out buffer?
        Display.getDefault().syncExec(new Runnable() {
            public void run() {
                if ( (_out == null) || (_out.isDisposed()) ) return;
                _out.setRedraw(false);
                int overallStart = _out.getCharCount();
                
                while (records.size() > 0) {
                    Record r = (Record)records.remove(0);
                    int start = _out.getCharCount();
                    int end = -1;
                    if (r.msg != null) {
                        _out.append(now() + ":");
                        end = _out.getCharCount();
                        StyleRange range = new StyleRange(start, end-start, _tsFGColor, _tsBGColor);
                        _out.setStyleRange(range);
                        start = end;
                        _out.append(" " + r.msg + "\n");
                        end = _out.getCharCount();
                    }
                    if (r.e != null) {
                        StringWriter out = new StringWriter();
                        r.e.printStackTrace(new PrintWriter(out));
                        start = _out.getCharCount();
                        _out.append(now());
                        end = _out.getCharCount();
                        _out.setStyleRange(new StyleRange(start, end-start, _tsFGColor, _tsBGColor));
                        start = end;
                        _out.append("\n" + out.getBuffer().toString() + "\n");
                    }
                    end = _out.getCharCount();
                    if (end > overallStart) {
                        int startLine = _out.getLineAtOffset(overallStart);
                        int curLine = _out.getLineCount()-1;
                        if (r.type == STATUS)
                            _out.setLineBackground(startLine, curLine-startLine, _statusColor);
                        else if (r.type == DEBUG)
                            _out.setLineBackground(startLine, curLine-startLine, _debugColor);
                        else
                            _out.setLineBackground(startLine, curLine-startLine, _errorColor);
                    }
                }

                int lines = _out.getLineCount();
                if (lines > MAX_LINES) {
                    int off = _out.getOffsetAtLine(lines-MAX_LINES);
                    _out.replaceTextRange(0, off, "");
                }

                // scroll to the end
                if (_out.getLineCount() > 0)
                    _out.setTopIndex(_out.getLineCount()-1);
                
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
    public void commandComplete(final int status, final List location) {
        append(STATUS, "* Command execution complete. ");
        append(STATUS, "* Status: " + status);
        StringBuilder buf = new StringBuilder();
        if (location != null) {
            for (int i = 0; i < location.size(); i++) {
                buf.append(location.get(i).toString()).append("> ");
            }
        }
        append(STATUS, "* Location: " + buf.toString());
        Display.getDefault().syncExec(new Runnable() {
            public void run() { 
                _out.setRedraw(true);
            }
        });
    }
        
    public Image getIcon() { return ImageUtil.ICON_TAB_TEXTUI; }
    public String getName() { return "Text UI"; }
    public String getDescription() { return "Advanced text interface"; }

    // unused UI methods
    public Opts readCommand() { return null; }
    public Opts readCommand(boolean displayPrompt) { return null; }
    public boolean toggleDebug() { return false; }
    public boolean togglePaginate() { return false; }
    public void insertCommand(String commandline) {}
    public String readStdIn() { return null; }
    public void addUI(UI ui) {}
    public void removeUI(UI ui) {}
    
    private static class Record {
        int type;
        String msg;
        Exception e;
        public Record(int stat, String newMsg) { type = stat; msg = newMsg; }
        public Record(int stat, String newMsg, Exception cause) { type = stat; msg = newMsg; e = cause; }
    }
}
