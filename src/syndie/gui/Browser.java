package syndie.gui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabFolder2Listener;
import org.eclipse.swt.custom.CTabFolderEvent;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
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
import syndie.db.JobRunner;
import syndie.db.SyndicationManager;
import syndie.gui.TranslationRegistry;
import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
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
public class Browser implements UI, BrowserControl, Translatable {
    private DBClient _client;
    private TextEngine _engine;
    private TranslationRegistry _translation;
    private SyndicationManager _syndicationManager;
    private Shell _shell;
    private Menu _mainMenu;
    private SashForm _sash;
    private BrowserTree _bookmarks;
    private CTabFolder _tabs;
    private Menu _tabMenu;
    private MenuItem _copyTabLocation;
    private MenuItem _bookmarkTab;
    private MenuItem _fileMenuRoot;
    private MenuItem _fileMenuOpen;
    private MenuItem _fileMenuImport;
    private MenuItem _fileMenuExport;
    private MenuItem _fileMenuExit;
    private MenuItem _postMenuRoot;
    private MenuItem _postMenuNew;
    private MenuItem _postMenuResume;
    private MenuItem _syndicateMenuRoot;
    private MenuItem _syndicateMenuItem;
    private MenuItem _advancedMenuRoot;
    private MenuItem _advancedMenuTextUI;
    private MenuItem _advancedMenuSQL;
    private MenuItem _advancedMenuLogs;
    private MenuItem _helpMenuRoot;
    private MenuItem _helpMenuAbout;
    private MenuItem _helpMenuFAQ;
    private MenuItem _helpMenuGUIManual;
    private MenuItem _helpMenuTextManual;
    private ToolTip _systrayTip;
    
    private Composite _statusRow;
    private BookmarkEditorPopup _bookmarkEditor;
    /** uri to BrowserTab */
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
        _translation = new TranslationRegistry();
        _syndicationManager = new SyndicationManager(_client, this);
        JobRunner.instance().setUI(getUI());
        debugMessage("browser construction.  isLoggedIn? " + client.isLoggedIn());
        if (client.isLoggedIn())
            Display.getDefault().syncExec(new Runnable() { public void run() { initComponents(); } });
    }

    private void initComponents() {
        debugMessage("browser initComponents");
        _initialized = true;
        _shell = new Shell(Display.getDefault(), SWT.SHELL_TRIM);
        _shell.setLayout(new GridLayout(1, true));
        
        debugMessage("before creating the menu");
        initMenu();
        debugMessage("before creating the systray");
        initSystray();
        
        _sash = new SashForm(_shell, SWT.HORIZONTAL);
        _sash.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, true));
        
        _bookmarks = new BrowserTree(this, _sash, new BookmarkChoiceListener(), new BookmarkAcceptListener());
        
        _tabs = new CTabFolder(_sash, SWT.MULTI | SWT.TOP | SWT.CLOSE);
        _tabs.setSimple(false);
        _tabs.setMinimizeVisible(false);
        _tabs.setMinimumCharacters(8);
        _tabs.setUnselectedImageVisible(true);
        _tabs.setBorderVisible(true);
        
        _tabMenu = new Menu(_tabs);
        _tabs.setMenu(_tabMenu);
        
        _copyTabLocation = new MenuItem(_tabMenu, SWT.PUSH);
        _copyTabLocation.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { copyTabLocation(); }
            public void widgetSelected(SelectionEvent selectionEvent) { copyTabLocation(); }
        });
        _bookmarkTab = new MenuItem(_tabMenu, SWT.PUSH);
        _bookmarkTab.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { bookmarkTab(); }
            public void widgetSelected(SelectionEvent selectionEvent) { bookmarkTab(); }
        });
        
        _statusRow = new Composite(_shell, SWT.BORDER);
        _statusRow.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _statusRow.setLayout(new FillLayout(SWT.HORIZONTAL));
        new Text(_statusRow, SWT.SINGLE|SWT.READ_ONLY|SWT.BORDER).setText("this is the status bar");
        
        _bookmarkEditor = new BookmarkEditorPopup(this, _shell);
        
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; exit(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        _translation.register(this);
        
        debugMessage("=tabs: " +_tabs.getClientArea() + "/" + _tabs.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        _sash.setWeights(new int[] { 20, 80 });
        
        debugMessage("=tabs weghted: " +_tabs.getClientArea() + "/" + _tabs.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        _shell.pack();
        debugMessage("=tabs packed: " +_tabs.getClientArea() + "/" + _tabs.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        final Point min = _shell.computeSize(SWT.DEFAULT, 500);
        _shell.setSize(min);//800, 500));
        
        debugMessage("=tabs sized: " +_tabs.getClientArea() + "/" + _tabs.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        _bookmarks.viewStartupItems();
        debugMessage("=tabs w items: " +_tabs.getClientArea() + "/" + _tabs.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        
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
        } else {
            //_syndicationManager.loadArchives();
            if (!_shell.isVisible())
                _shell.open();
        }
    }
    
    private void initMenu() {
        _mainMenu = new Menu(_shell, SWT.BAR);
        
        _fileMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        
        Menu fileMenu = new Menu(_fileMenuRoot);
        _fileMenuRoot.setMenu(fileMenu);
        _fileMenuOpen = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuImport = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuExport = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuExit = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuExit.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { exit(); }
            public void widgetSelected(SelectionEvent selectionEvent) { exit(); }
        });
        
        _postMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        Menu postMenu = new Menu(_postMenuRoot);
        _postMenuRoot.setMenu(postMenu);
        _postMenuNew = new MenuItem(postMenu, SWT.PUSH);
        _postMenuNew.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { postNew(); }
            public void widgetSelected(SelectionEvent selectionEvent) { postNew(); }
        });
        _postMenuResume = new MenuItem(postMenu, SWT.PUSH);
        
        _syndicateMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        Menu syndicateMenu = new Menu(_syndicateMenuRoot);
        _syndicateMenuRoot.setMenu(syndicateMenu);
        _syndicateMenuItem = new MenuItem(syndicateMenu, SWT.PUSH);
        _syndicateMenuItem.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showSyndicate(); }
            public void widgetSelected(SelectionEvent selectionEvent) { showSyndicate(); }
        });
        
        _advancedMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        Menu advancedMenu = new Menu(_advancedMenuRoot);
        _advancedMenuRoot.setMenu(advancedMenu);
        _advancedMenuTextUI = new MenuItem(advancedMenu, SWT.PUSH);
        _advancedMenuTextUI.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showTextUI(); }
            public void widgetSelected(SelectionEvent selectionEvent) { showTextUI(); }
        });
        _advancedMenuLogs = new MenuItem(advancedMenu, SWT.PUSH);
        _advancedMenuLogs.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showLogs(); }
            public void widgetSelected(SelectionEvent selectionEvent) { showLogs(); }
        });
        _advancedMenuSQL = new MenuItem(advancedMenu, SWT.PUSH);
        _advancedMenuSQL.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(createSQLURI()); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(createSQLURI()); }
        });
        
        new MenuItem(_mainMenu, SWT.SEPARATOR);
        
        _helpMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        Menu helpMenu = new Menu(_helpMenuRoot);
        _helpMenuRoot.setMenu(helpMenu);
        _helpMenuAbout = new MenuItem(helpMenu, SWT.PUSH);
        _helpMenuFAQ = new MenuItem(helpMenu, SWT.PUSH);
        _helpMenuGUIManual = new MenuItem(helpMenu, SWT.PUSH);
        _helpMenuTextManual = new MenuItem(helpMenu, SWT.PUSH);
        
        _shell.setMenuBar(_mainMenu);
    }
    
    private void initSystray() {
        Tray tray = _shell.getDisplay().getSystemTray();
        TrayItem root = new TrayItem(tray, SWT.NONE);
        _systrayTip = new ToolTip(_shell, SWT.BALLOON);
        _systrayTip.setAutoHide(true);
        root.setToolTip(_systrayTip);
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
        confirm.setText(_translation.getText(T_CONFIRM_EXIT_TITLE, "Confirm exit"));
        confirm.setMessage(_translation.getText(T_CONFIRM_EXIT_MESSAGE, "Are you sure you want to exit Syndie?"));
        int rv = confirm.open();
        if (rv == SWT.YES) {
            _shell.setVisible(false);
            JobRunner.instance().stop();
            System.exit(0);
        }
    }
    
    public void showWaitCursor(boolean show) {
        if (show)
            _shell.setCursor(ImageUtil.CURSOR_WAIT);
        else
            _shell.setCursor(null);
    }
    
    public void view(SyndieURI uri) { 
        debugMessage("Viewing [" + uri + "]");
        if (uri == null) return;
        showWaitCursor(true);
        BrowserTab tab = null;
        Hash scope = uri.getHash("scope");
        Long msgId = uri.getMessageId();
        SyndieURI browseURI = null;
        if (uri.isSearch()) {
            if ( (scope != null) && (msgId != null) )
                browseURI = SyndieURI.createMessage(scope, msgId.longValue());
            else if (scope != null)
                browseURI = SyndieURI.createScope(scope);
        }
        synchronized (_openTabs) {
            tab = (BrowserTab)_openTabs.get(uri);
            if ( (tab == null) && (browseURI != null) )
                tab = (BrowserTab)_openTabs.get(browseURI);
            if (tab == null) {
                debugMessage("building tab");
                if (browseURI == null) {
                    debugMessage("building normal URI: " + uri);
                    tab = BrowserTab.build(this, uri);
                    if (tab != null)
                        _openTabs.put(uri, tab);
                } else {
                    debugMessage("building browseURI: " + browseURI);
                    tab = BrowserTab.build(this, browseURI);
                    if (tab != null)
                        _openTabs.put(browseURI, tab);
                }
                debugMessage("tab built: " + tab);
            }
        }
        if (tab != null) {
            if ( (browseURI != null) && (tab instanceof BrowseForumTab) )
                ((BrowseForumTab)tab).setFilter(uri);
            debugMessage("showing tab");
            _tabs.showItem(tab.getTabItem());
            debugMessage("tab shown");
            _tabs.setSelection(tab.getTabItem());
            debugMessage("tab selected");
            tab.tabShown();
        } 
        showWaitCursor(false);
        if (tab == null) {
            MessageBox box = new MessageBox(_shell, SWT.ICON_ERROR | SWT.OK);
            box.setText(getTranslationRegistry().getText(T_BADURI_TITLE, "Invalid URI"));
            box.setMessage(getTranslationRegistry().getText(T_BADURI_MSG, "The URI visited is not understood by Syndie: ") + uri.toString());
            box.open();
        }
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
    /** show a popup to bookmark the given uri in the user's set of bookmarked references */
    public void bookmark(SyndieURI uri) {
        debugMessage("bookmarking "+uri);
        String name = "bookmark name";
        String desc = "";
        int siblingOrder = -1;
        long parentGroupId = -1;
        boolean loadOnStart = false;
        
        if (uri.isChannel() && (uri.getScope() != null)) {
            long chanId = _client.getChannelId(uri.getScope());
            if (uri.getMessageId() != null) {
                MessageInfo msg = _client.getMessage(chanId, uri.getMessageId());
                if (msg != null) {
                    name = msg.getSubject();
                    desc = "";
                } else {
                    name = "";
                    desc = uri.getScope().toBase64().substring(0,6) + ":" + uri.getMessageId();
                }
            } else {
                ChannelInfo chan = _client.getChannel(chanId);
                if (chan != null) {
                    name = chan.getName();
                    desc = chan.getDescription();
                } else {
                    name = "";
                    desc = uri.getScope().toBase64().substring(0,6) + ":" + uri.getMessageId();
                }
            }
        } else if (BrowserTab.TYPE_LOGS.equals(uri.getType())) {
            name = _translation.getText(T_BOOKMARK_LOGS_NAME, "logs");
            desc = _translation.getText(T_BOOKMARK_LOGS_DESC, "watch log messages");
        } else if (BrowserTab.TYPE_POST.equals(uri.getType())) {
            name = _translation.getText(T_BOOKMARK_POST_NAME, "post");
            desc = _translation.getText(T_BOOKMARK_POST_DESC, "post a new message");
        } else if (BrowserTab.TYPE_TEXTUI.equals(uri.getType())) {
            name = _translation.getText(T_BOOKMARK_TEXTUI_NAME, "text UI");
            desc = _translation.getText(T_BOOKMARK_TEXTUI_DESC, "text based interface");
        }
        
        // bookmark should always set these to false (ban/ignore would set them to true)
        boolean ignored = false;
        boolean banned = false;
        
        // the following is set by addNymReference
        long uriId = -1;
        long groupId = -1;
        
        NymReferenceNode node = new NymReferenceNode(name, uri, desc, uriId, groupId, parentGroupId, siblingOrder, ignored, banned, loadOnStart);
        
        _bookmarkEditor.setBookmark(node);
        _bookmarkEditor.open();
    }
    /** called by the bookmark editor, or other things that can populate the fields properly */
    public void bookmark(NymReferenceNode node) {
        _client.addNymReference(_client.getLoggedInNymId(), node);
        _bookmarks.refreshBookmarks();
        debugMessage("bookmarks refreshed");
    }
    public void deleteBookmark(long bookmarkGroupId) {
        _client.deleteNymReference(_client.getLoggedInNymId(), bookmarkGroupId);
        _bookmarks.refreshBookmarks();
    }
    public void updateBookmark(NymReferenceNode bookmark) {
        _client.updateNymReference(_client.getLoggedInNymId(), bookmark);
        _bookmarks.refreshBookmarks();
    }
    
    public UI getUI() { return this; }
    public TranslationRegistry getTranslationRegistry() { return _translation; }
    
    private void postNew() { view(createPostURI(null, null)); }
    private void showTextUI() { view(createTextUIURI()); }
    private void showLogs() { view(createLogsURI()); }
    private void showSyndicate() { view(createSyndicationURI()); }
    
    public SyndieURI createPostURI(Hash forum, SyndieURI parent) {
        return createPostURI(forum, parent, false);
    }
    public SyndieURI createPostURI(Hash forum, SyndieURI parent, boolean asPrivateReply) {
        Map attributes = new HashMap();
        if (forum != null)
            attributes.put("channel", forum.toBase64());
        else if (parent != null)
            attributes.put("channel", parent.getScope());
        if (parent != null)
            attributes.put("parent", parent.toString());
        attributes.put("reply", ""+asPrivateReply);
        attributes.put("uniq", "" + System.currentTimeMillis()); // just a local uniq
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_POST, attributes);
        return uri;
    }
    public SyndieURI createManageURI(Hash forum) {
        Map attributes = new HashMap();
        if (forum != null)
            attributes.put("channel", forum.toBase64());
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_MANAGE, attributes);
        return uri;
    }
    public SyndieURI createMetaURI(Hash forum) {
        Map attributes = new HashMap();
        if (forum != null)
            attributes.put("channel", forum.toBase64());
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_META, attributes);
        return uri;
    }
    public SyndieURI createMetaRefsURI(Hash forum) {
        Map attributes = new HashMap();
        if (forum != null)
            attributes.put("channel", forum.toBase64());
        attributes.put(ManageForum.DETAIL, ManageForum.DETAIL_REFS);
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_META, attributes);
        return uri;
    }
    public SyndieURI createMetaArchivesURI(Hash forum) {
        Map attributes = new HashMap();
        if (forum != null)
            attributes.put("channel", forum.toBase64());
        attributes.put(ManageForum.DETAIL, ManageForum.DETAIL_ARCHIVES);
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_META, attributes);
        return uri;
    }
    public SyndieURI createMetaPostersURI(Hash forum) {
        Map attributes = new HashMap();
        if (forum != null)
            attributes.put("channel", forum.toBase64());
        attributes.put(ManageForum.DETAIL, ManageForum.DETAIL_POSTERS);
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_META, attributes);
        return uri;
    }
    public SyndieURI createMetaManagersURI(Hash forum) {
        Map attributes = new HashMap();
        if (forum != null)
            attributes.put("channel", forum.toBase64());
        attributes.put(ManageForum.DETAIL, ManageForum.DETAIL_MANAGER);
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_META, attributes);
        return uri;
    }
    
    public SyndieURI createTextUIURI() { return new SyndieURI(BrowserTab.TYPE_TEXTUI, new HashMap()); }
    public SyndieURI createLogsURI() { return new SyndieURI(BrowserTab.TYPE_LOGS, new HashMap()); }
    public SyndieURI createSQLURI() { return new SyndieURI(BrowserTab.TYPE_SQL, new HashMap()); }
    public SyndieURI createSyndicationURI() { return new SyndieURI(BrowserTab.TYPE_SYNDICATE, new HashMap()); }
    
    public CTabFolder getTabFolder() { return _tabs; }
    public DBClient getClient() { return _client; }
    public SyndicationManager getSyndicationManager() { return _syndicationManager; }

    private void bookmarkTab() {
        CTabItem item = _tabs.getSelection();
        if (item != null) {
            for (Iterator iter = _openTabs.keySet().iterator(); iter.hasNext(); ) {
                SyndieURI uri = (SyndieURI)iter.next();
                BrowserTab tab = (BrowserTab)_openTabs.get(uri);
                if (tab.getTabItem() == item) {
                    SyndieURI curURI = tab.getURI(); // may have changed since insert
                    bookmark(curURI);
                    return;
                }
            }
        }
    }
    private void copyTabLocation() {
        CTabItem item = _tabs.getSelection();
        if (item != null) {
            for (Iterator iter = _openTabs.keySet().iterator(); iter.hasNext(); ) {
                SyndieURI uri = (SyndieURI)iter.next();
                BrowserTab tab = (BrowserTab)_openTabs.get(uri);
                if (tab.getTabItem() == item) {
                    SyndieURI curURI = tab.getURI(); // may have changed since insert
                    //
                    return;
                }
            }
        }
    }
    
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
        if ( (msg != null) || (cause != null) )
            _client.logError(msg, cause);
    }

    public void statusMessage(String msg) {
        synchronized (_uiListeners) {
            for (int i = 0; i < _uiListeners.size(); i++)
                ((UIListener)_uiListeners.get(i)).statusMessage(msg);
        }
        if (msg != null)
            _client.logInfo(msg);
    }
    public void debugMessage(String msg) { debugMessage(msg, null); }
    public void debugMessage(String msg, Exception cause) {
        synchronized (_uiListeners) {
            for (int i = 0; i < _uiListeners.size(); i++)
                ((UIListener)_uiListeners.get(i)).debugMessage(msg, cause);
        }
        if ( (msg != null) || (cause != null) )
            _client.logDebug(msg, cause);
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
    
    private static final String T_SHELL_TITLE = "syndie.gui.browser.title";
    private static final String T_COPY_TAB_LOC = "syndie.gui.browser.tabmenu.copylocation";
    private static final String T_BOOKMARK_TAB = "syndie.gui.browser.tabmenu.bookmark";
    private static final String T_FILE_MENU_TITLE = "syndie.gui.browser.filemenu.title";
    private static final String T_FILE_MENU_TITLE_ACCELERATOR = "syndie.gui.browser.filemenu.title.accelerator";
    private static final String T_FILE_MENU_OPEN = "syndie.gui.browser.filemenu.open";
    private static final String T_FILE_MENU_IMPORT = "syndie.gui.browser.filemenu.import";
    private static final String T_FILE_MENU_EXPORT = "syndie.gui.browser.filemenu.export";
    private static final String T_FILE_MENU_EXIT = "syndie.gui.browser.filemenu.exit";
    private static final String T_FILE_MENU_EXIT_ACCELERATOR = "syndie.gui.browser.filemenu.exit.accelerator";
    private static final String T_POST_MENU_TITLE = "syndie.gui.browser.postmenu.title";
    private static final String T_POST_MENU_NEW = "syndie.gui.browser.postmenu.new";
    private static final String T_POST_MENU_RESUME = "syndie.gui.browser.postmenu.resume";
    private static final String T_SYNDICATE_MENU_TITLE = "syndie.gui.browser.syndicatemenu.title";
    private static final String T_SYNDICATE_MENU_ITEM = "syndie.gui.browser.syndicatemenu.item";
    private static final String T_ADVANCED_MENU_TITLE = "syndie.gui.browser.advancedmenu.title";
    private static final String T_ADVANCED_MENU_TEXTUI = "syndie.gui.browser.advancedmenu.textui";
    private static final String T_ADVANCED_MENU_LOGS = "syndie.gui.browser.advancedmenu.logs";
    private static final String T_ADVANCED_MENU_SQL = "syndie.gui.browser.advancedmenu.sql";
    private static final String T_HELP_MENU_TITLE = "syndie.gui.browser.helpmenu.title";
    private static final String T_HELP_MENU_ABOUT = "syndie.gui.browser.helpmenu.about";
    private static final String T_HELP_MENU_FAQ = "syndie.gui.browser.helpmenu.faq";
    private static final String T_HELP_MENU_GUIMAN = "syndie.gui.browser.helpmenu.guiman";
    private static final String T_HELP_MENU_TEXTMAN = "syndie.gui.browser.helpmenu.textman";
    private static final String T_SYSTRAY_TOOLTIP_TITLE = "syndie.gui.browser.systray.title";
    private static final String T_SYSTRAY_TOOLTIP_TEXT = "syndie.gui.browser.systray.text";
    // the confirm popup is created on the fly, so translated only on creation, not on translate(...)
    private static final String T_CONFIRM_EXIT_TITLE = "syndie.gui.browser.confirmexit.title";
    private static final String T_CONFIRM_EXIT_MESSAGE = "syndie.gui.browser.confirmexit.message";
    // the bookmark popup is created on the fly, so translated only on creation, not on translate(...)
    private static final String T_BOOKMARK_LOGS_NAME = "syndie.gui.browser.bookmarklogs.name";
    private static final String T_BOOKMARK_LOGS_DESC = "syndie.gui.browser.bookmarklogs.desc";
    private static final String T_BOOKMARK_POST_NAME = "syndie.gui.browser.bookmarkpost.name";
    private static final String T_BOOKMARK_POST_DESC = "syndie.gui.browser.bookmarkpost.desc";
    private static final String T_BOOKMARK_TEXTUI_NAME = "syndie.gui.browser.bookmarktextui.name";
    private static final String T_BOOKMARK_TEXTUI_DESC = "syndie.gui.browser.bookmarktextui.desc";

    private static final String T_BADURI_TITLE = "syndie.gui.browser.baduri.title";
    private static final String T_BADURI_MSG = "syndie.gui.browser.baduri.msg";
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText(T_SHELL_TITLE, "Syndie"));
        _copyTabLocation.setText(registry.getText(T_COPY_TAB_LOC, "copy tab location"));
        _bookmarkTab.setText(registry.getText(T_BOOKMARK_TAB, "bookmark tab"));
        
        _fileMenuRoot.setText(registry.getText(T_FILE_MENU_TITLE, "File"));
        _fileMenuRoot.setAccelerator(SWT.MOD1 | registry.getText(T_FILE_MENU_TITLE_ACCELERATOR, "f").charAt(0));
        _fileMenuOpen.setText(registry.getText(T_FILE_MENU_OPEN, "Open Syndie URI"));
        _fileMenuImport.setText(registry.getText(T_FILE_MENU_IMPORT, "Import"));
        _fileMenuExport.setText(registry.getText(T_FILE_MENU_EXPORT, "Export"));
        _fileMenuExit.setText(registry.getText(T_FILE_MENU_EXIT, "Exit"));
        _fileMenuExit.setAccelerator(SWT.MOD1 | registry.getText(T_FILE_MENU_EXIT_ACCELERATOR, "x").charAt(0));
    
        _postMenuRoot.setText(registry.getText(T_POST_MENU_TITLE, "Post"));
        _postMenuNew.setText(registry.getText(T_POST_MENU_NEW, "Post new"));
        _postMenuResume.setText(registry.getText(T_POST_MENU_RESUME, "Resume existing"));
        
        _syndicateMenuRoot.setText(registry.getText(T_SYNDICATE_MENU_TITLE, "Syndicate"));
        _syndicateMenuItem.setText(registry.getText(T_SYNDICATE_MENU_ITEM, "Now"));

        _advancedMenuRoot.setText(registry.getText(T_ADVANCED_MENU_TITLE, "Advanced"));
        _advancedMenuLogs.setText(registry.getText(T_ADVANCED_MENU_LOGS, "Logs"));
        _advancedMenuTextUI.setText(registry.getText(T_ADVANCED_MENU_TEXTUI, "Text interface"));
        _advancedMenuSQL.setText(registry.getText(T_ADVANCED_MENU_SQL, "SQL interface"));

        _helpMenuRoot.setText(registry.getText(T_HELP_MENU_TITLE, "Help"));
        _helpMenuAbout.setText(registry.getText(T_HELP_MENU_ABOUT, "About"));
        _helpMenuFAQ.setText(registry.getText(T_HELP_MENU_FAQ, "FAQ"));
        _helpMenuGUIManual.setText(registry.getText(T_HELP_MENU_GUIMAN, "GUI manual"));
        _helpMenuTextManual.setText(registry.getText(T_HELP_MENU_TEXTMAN, "Text interface manual"));
        
        _systrayTip.setText(registry.getText(T_SYSTRAY_TOOLTIP_TITLE, "Syndie"));
        _systrayTip.setMessage(registry.getText(T_SYSTRAY_TOOLTIP_TEXT, "Syndie is running"));
    }
}
