package syndie.gui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolTip;
import org.eclipse.swt.widgets.Tray;
import org.eclipse.swt.widgets.TrayItem;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.ChannelInfo;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.Opts;
import syndie.db.TextEngine;
import syndie.db.UI;

/**
 * main gui wrapper
 */
public class Browser implements UI, BrowserControl {
    private DBClient _client;
    private TextEngine _engine;
    private Shell _shell;
    private Menu _mainMenu;
    private ReferenceChooserTree _bookmarks;
    private CTabFolder _tabs;
    private Composite _statusRow;
    
    private Map _openTabs;
    
    private List _uiListeners;
    private List _commands;
    private volatile boolean _initialized;
    
    public Browser(DBClient client) {
        _client = client;
        _openTabs = new HashMap();
        _uiListeners = new ArrayList();
        _commands = new ArrayList();
        _initialized = false;
        debugMessage("browser construction.  isLoggedIn? " + client.isLoggedIn());
        if (client.isLoggedIn())
            Display.getDefault().syncExec(new Runnable() { public void run() { initComponents(); } });
    }

    private void initComponents() {
        debugMessage("browser initComponents");
        _initialized = true;
        _shell = new Shell(Display.getDefault(), SWT.SHELL_TRIM);
        _shell.setText("Syndie");
        _shell.setLayout(new GridLayout(2, false));
        
        debugMessage("before creating the menu");
        initMenu();
        debugMessage("before creating the systray");
        initSystray();
        
        _bookmarks = new ReferenceChooserTree(_client, _shell, new BookmarkChoiceListener(), new BookmarkAcceptListener());
        _bookmarks.getControl().setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, true));

        _tabs = new CTabFolder(_shell, SWT.MULTI | SWT.TOP | SWT.CLOSE);
        _tabs.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _tabs.setMinimizeVisible(false);
        _tabs.setMinimumCharacters(8);
        _tabs.setUnselectedImageVisible(true);
        _tabs.setBorderVisible(true);
        
        _statusRow = new Composite(_shell, SWT.BORDER);
        _statusRow.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        _statusRow.setLayout(new FillLayout(SWT.HORIZONTAL));
        new Text(_statusRow, SWT.SINGLE|SWT.READ_ONLY|SWT.BORDER).setText("this is the status bar");
        
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; exit(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        _shell.pack();
        _shell.setSize(_shell.computeSize(750, 600));
        _shell.setVisible(false);
    }
    
    public void setEngine(TextEngine engine) { _engine = engine; }
    public void startup() {
        debugMessage("startup: loggedIn? " + _client.isLoggedIn() + " initialized? " + _initialized);
        if (_client.isLoggedIn() && !_initialized) {
            _initialized = true;
            Display.getDefault().syncExec(new Runnable() { public void run() { initComponents(); } });
        }
        Display.getDefault().syncExec(new Runnable() { public void run() { doStartup(); } });
    }
    private void doStartup() {
        debugMessage("doStartup: loggedIn? " + _client.isLoggedIn() + " initialized? " + _initialized + " nymId? " + _client.getLoggedInNymId());
        if (!_initialized || (_client.getLoggedInNymId() < 0)) {
            // show a login prompt
            LoginPrompt prompt = new LoginPrompt(_client, this);
            prompt.login();
        } else if (!_shell.isVisible()) {
            _shell.open();
        }
    }
    
    private void initMenu() {
        _mainMenu = new Menu(_shell, SWT.BAR);
        
        MenuItem file = new MenuItem(_mainMenu, SWT.CASCADE);
        file.setText("File");
        Menu fileMenu = new Menu(file);
        file.setMenu(fileMenu);
        new MenuItem(fileMenu, SWT.PUSH).setText("Open Syndie URI");
        new MenuItem(fileMenu, SWT.PUSH).setText("Import");
        new MenuItem(fileMenu, SWT.PUSH).setText("Export");
        MenuItem exit = new MenuItem(fileMenu, SWT.PUSH);
        exit.setText("Exit");
        exit.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { exit(); }
            public void widgetSelected(SelectionEvent selectionEvent) { exit(); }
        });
        
        MenuItem post = new MenuItem(_mainMenu, SWT.CASCADE);
        post.setText("Post");
        Menu postMenu = new Menu(post);
        post.setMenu(postMenu);
        MenuItem postNew = new MenuItem(postMenu, SWT.PUSH);
        postNew.setText("Post new");
        postNew.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { postNew(); }
            public void widgetSelected(SelectionEvent selectionEvent) { postNew(); }
        });
        new MenuItem(postMenu, SWT.PUSH).setText("Resume existing...");
        
        new MenuItem(_mainMenu, SWT.CASCADE).setText("Syndicate");
        
        MenuItem advanced = new MenuItem(_mainMenu, SWT.CASCADE);
        advanced.setText("Advanced");
        Menu advancedMenu = new Menu(advanced);
        advanced.setMenu(advancedMenu);
        MenuItem advancedText = new MenuItem(advancedMenu, SWT.PUSH);
        advancedText.setText("Text interface");
        advancedText.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showTextUI(); }
            public void widgetSelected(SelectionEvent selectionEvent) { showTextUI(); }
        });
        MenuItem advancedLogs = new MenuItem(advancedMenu, SWT.PUSH);
        advancedLogs.setText("Logs");
        advancedLogs.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showLogs(); }
            public void widgetSelected(SelectionEvent selectionEvent) { showLogs(); }
        });
        
        new MenuItem(_mainMenu, SWT.SEPARATOR);
        
        MenuItem help = new MenuItem(_mainMenu, SWT.CASCADE);
        help.setText("Help");
        Menu helpMenu = new Menu(help);
        help.setMenu(helpMenu);
        new MenuItem(helpMenu, SWT.PUSH).setText("About");
        new MenuItem(helpMenu, SWT.PUSH).setText("FAQ");
        new MenuItem(helpMenu, SWT.PUSH).setText("GUI manual");
        new MenuItem(helpMenu, SWT.PUSH).setText("Text interface manual");
        
        _shell.setMenuBar(_mainMenu);
    }
    
    private void initSystray() {
        Tray tray = _shell.getDisplay().getSystemTray();
        TrayItem root = new TrayItem(tray, SWT.NONE);
        ToolTip tip = new ToolTip(_shell, SWT.BALLOON);
        tip.setText("Syndie");
        tip.setMessage("Syndie is running");
        tip.setAutoHide(true);
        root.setToolTip(tip);
        root.setImage(createSystrayIcon());
        root.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                _shell.setVisible(!_shell.isVisible());
            }
            public void widgetSelected(SelectionEvent selectionEvent) {
                _shell.setVisible(!_shell.isVisible());
            }
        });
    }
    
    private void exit() {
        MessageBox confirm = new MessageBox(_shell, SWT.ICON_QUESTION | SWT.YES | SWT.NO);
        confirm.setText("Confirm exit");
        confirm.setMessage("Are you sure you want to exit Syndie?");
        int rv = confirm.open();
        if (rv == SWT.YES) {
            _shell.setVisible(false);
            System.exit(0);
        }
    }
    
    public void view(SyndieURI uri) {
        debugMessage("Viewing [" + uri.toString() + "]");
        _shell.setCursor(ImageUtil.CURSOR_WAIT);
        BrowserTab tab = null;
        synchronized (_openTabs) {
            tab = (BrowserTab)_openTabs.get(uri);
            if (tab == null) {
                debugMessage("building tab");
                tab = BrowserTab.build(this, uri);
                debugMessage("tab built");
                if (tab != null) {
                    _openTabs.put(uri, tab);
                }
            }
        }
        if (tab != null) {
            debugMessage("showing tab");
            _tabs.showItem(tab.getTabItem());
            debugMessage("tab shown");
            _tabs.setSelection(tab.getTabItem());
            debugMessage("tab selected");
        }
        _shell.setCursor(null);
    }
    public void unview(SyndieURI uri) {
        BrowserTab tab = null;
        synchronized (_openTabs) {
            tab = (BrowserTab)_openTabs.remove(uri);
        }
        if (tab != null) {
            if (_tabs.getSelection() == tab.getTabItem()) {
                // view the previous
            }
            tab.dispose();
        }
    }
    public UI getUI() { return this; }
    
    private void postNew() { view(createPostURI(null, null)); }
    private void showTextUI() { view(createTextUIURI()); }
    private void showLogs() { view(createLogsURI()); }
    
    public SyndieURI createPostURI(Hash forum, SyndieURI parent) {
        return createPostURI(forum, parent, false);
    }
    public SyndieURI createPostURI(Hash forum, SyndieURI parent, boolean asPrivateReply) {
        Map attributes = new HashMap();
        if (forum != null)
            attributes.put("scope", forum.toBase64());
        if (parent != null)
            attributes.put("parent", parent.toString());
        attributes.put("reply", ""+asPrivateReply);
        attributes.put("uniq", "" + System.currentTimeMillis()); // just a local uniq
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_POST, attributes);
        return uri;
    }
    public SyndieURI createMetaURI(Hash forum) {
        Map attributes = new HashMap();
        if (forum != null)
            attributes.put("scope", forum.toBase64());
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_META, attributes);
        return uri;
    }
    
    public SyndieURI createTextUIURI() { return new SyndieURI(BrowserTab.TYPE_TEXTUI, new HashMap()); }
    public SyndieURI createLogsURI() { return new SyndieURI(BrowserTab.TYPE_LOGS, new HashMap()); }
    
    public CTabFolder getTabFolder() { return _tabs; }
    public DBClient getClient() { return _client; }
    
    private class BookmarkChoiceListener implements ReferenceChooserTree.ChoiceListener {
        public void bookmarkSelected(TreeItem item, NymReferenceNode node) { view(node.getURI()); }
        public void manageChannelSelected(TreeItem item, ChannelInfo channel) { view(SyndieURI.createScope(channel.getChannelHash())); }
        public void postChannelSelected(TreeItem item, ChannelInfo channel) { view(SyndieURI.createScope(channel.getChannelHash())); }
        public void searchResultSelected(TreeItem item, ReferenceNode node) { view(node.getURI()); }
        public void otherSelected(TreeItem item) {}
    }
    
    private class BookmarkAcceptListener implements ReferenceChooserTree.AcceptanceListener {
        public void referenceAccepted(SyndieURI uri) { debugMessage("accepted"); view(uri); }
        public void referenceChoiceAborted() {}        
    }

    
    public void insertCommand(String cmd) { 
        synchronized (_commands) { _commands.add(cmd); _commands.notifyAll(); }
    }
    public Opts readCommand() {
        while (true) {
            synchronized (_commands) {
                try {
                    if (_commands.size() <= 0)
                        _commands.wait();
                } catch (InterruptedException ie) {}
                if (_commands.size() > 0)
                    return new Opts((String)_commands.remove(0));
            }
        }
    }
    public Opts readCommand(boolean displayPrompt) { return readCommand(); }
    public void errorMessage(String msg) { errorMessage(msg, null); }
    public void errorMessage(String msg, Exception cause) {
        synchronized (_uiListeners) {
            for (int i = 0; i < _uiListeners.size(); i++)
                ((UIListener)_uiListeners.get(i)).errorMessage(msg, cause);
        }
        //if (msg != null)
        //    System.err.println(msg);
        //if (cause != null)
        //    cause.printStackTrace();
    }

    public void statusMessage(String msg) {
        synchronized (_uiListeners) {
            for (int i = 0; i < _uiListeners.size(); i++)
                ((UIListener)_uiListeners.get(i)).statusMessage(msg);
        }
        //System.out.println(msg);
    }
    public void debugMessage(String msg) { debugMessage(msg, null); }
    public void debugMessage(String msg, Exception cause) {
        synchronized (_uiListeners) {
            for (int i = 0; i < _uiListeners.size(); i++)
                ((UIListener)_uiListeners.get(i)).debugMessage(msg, cause);
        }
        //if (msg != null)
        //    System.out.println(msg);
        //if (cause != null)
        //    cause.printStackTrace();
    }

    public void commandComplete(int status, List location) {
        synchronized (_uiListeners) {
            for (int i = 0; i < _uiListeners.size(); i++)
                ((UIListener)_uiListeners.get(i)).commandComplete(status, location);
        }
    }
    public boolean toggleDebug() { return true; }
    public boolean togglePaginate() { return false; }
    public String readStdIn() { 
        debugMessage("readStdIn()");
        return null;
    }
    
    private Image createSystrayIcon() {
        return ImageUtil.resize(ImageUtil.ICON_INFORMATION, 16, 16, false);
    }

    public void addUIListener(UIListener lsnr) { synchronized (_uiListeners) { _uiListeners.add(lsnr); } }
    public void removeUIListener(UIListener lsnr) { synchronized (_uiListeners) { _uiListeners.remove(lsnr); } }
    
    public interface UIListener {
        public void errorMessage(String msg);
        public void errorMessage(String msg, Exception cause);
        public void statusMessage(String msg);
        public void debugMessage(String msg);
        public void debugMessage(String msg, Exception cause);
        public void commandComplete(int status, List location);
    }
}
