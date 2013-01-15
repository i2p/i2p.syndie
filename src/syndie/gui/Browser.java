package syndie.gui;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URISyntaxException;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.zip.ZipInputStream;

import net.i2p.data.Hash;
import net.i2p.util.SecureFileOutputStream;

import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTError;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabFolder2Listener;
import org.eclipse.swt.custom.CTabFolderEvent;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.DropTargetListener;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.DeviceData;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.FontDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolTip;
import org.eclipse.swt.widgets.Tray;
import org.eclipse.swt.widgets.TrayItem;
import org.eclipse.swt.widgets.TreeItem;

import syndie.Constants;
import syndie.Version;
import syndie.data.NymKey;
import syndie.data.Timer;
import syndie.data.WatchedChannel;
import syndie.db.HTTPServ;
import syndie.db.Importer;
import syndie.db.JobRunner;
import syndie.db.SharedArchive;
import syndie.db.SyncManager;
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
 * Main gui wrapper, holding three items -
 * the bookmarks on the left, the tabs on the right, and the status bar on the bottom.
 * Configures all the top-strip menus.
 */
public class Browser implements UI, BrowserControl, NavigationControl, Translatable, Themeable {
    private DBClient _client;
    private TextEngine _engine;
    private TranslationRegistry _translation;
    private ThemeRegistry _themes;
    private Shell _shell;
    private Composite _root;
    private Menu _mainMenu;
    private SashForm _sash;
    //private LinkBar _linkBar;
    private BrowserTree _bookmarks;
    private CTabFolder _tabs;
    private Menu _tabMenu;
    private MenuItem _copyTabLocation;
    private MenuItem _closeAllTabs;
    private MenuItem _closeOtherTabs;
    private MenuItem _bookmarkTab;
    private MenuItem _fileMenuRoot;
    private MenuItem _fileMenuOpen;
    private MenuItem _fileMenuMinimize;
    private MenuItem _fileMenuImport;
    private MenuItem _fileMenuImportBulk;
    private MenuItem _fileMenuExport;
    private MenuItem _fileMenuExit;
    private MenuItem _viewMenuRoot;
    private Menu _viewMenu;
    private MenuItem _viewMenuShow;
    private MenuItem _forumMenuRoot;
    private MenuItem _forumMenuSearch;
    private MenuItem _forumMenuBookmarked;
    private MenuItem _forumMenuBrowse;
    private MenuItem _forumMenuBrowseForums;
    private MenuItem _forumMenuReadPrivate;
    private MenuItem _forumMenuCreate;
    private MenuItem _forumMenuManageRoot;
    private Menu _forumMenuManageMenu;
    private MenuItem _postMenuRoot;
    private MenuItem _postMenuNew;
    private MenuItem _postMenuWebRip;
    private MenuItem _postMenuResumeRoot;
    private Menu _postMenuResumeMenu;
    private MenuItem _postMenuManageableRoot;
    private Menu _postMenuManageableMenu;
    private MenuItem _postMenuPostableRoot;
    private Menu _postMenuPostableMenu;
    private MenuItem _postMenuPublicRoot;
    private Menu _postMenuPublicMenu;
    private MenuItem _syndicateMenuRoot;
    private Menu _syndicateMenu;
    private MenuItem _syndicateMenuConfig;
    private MenuItem _syndicateMenuOnline;
    private MenuItem _syndicateMenuArchive;
    private Menu _languageMenu;
    private MenuItem _languageMenuRoot;
    private MenuItem _languageMenuEdit;
    private MenuItem _languageMenuRefresh;
    private Menu _styleMenu;
    private MenuItem _styleMenuRoot;
    private MenuItem _styleMenuChangeFont;
    private MenuItem _styleMenuIncreaseFont;
    private MenuItem _styleMenuDecreaseFont;
    private MenuItem _styleMenuReset;
    private MenuItem _styleMenuEdit;
    private MenuItem _advancedMenuRoot;
    private MenuItem _advancedMenuTextUI;
    private MenuItem _advancedMenuBackupSecrets;
    private MenuItem _advancedMenuRestoreSecrets;
    private MenuItem _advancedMenuSQL;
    private MenuItem _advancedMenuLogs;
    private MenuItem _advancedMenuDumpResources;
    private MenuItem _advancedMenuDumpResourcesDiff;
    private MenuItem _helpMenuRoot;
    private MenuItem _helpMenuAbout;
    private MenuItem _helpMenuBugReport;
    private MenuItem _helpMenuFAQ;
    private MenuItem _helpMenuGUIManual;
    private MenuItem _helpMenuTextManual;
    private Tray _systray;
    private TrayItem _systrayRoot;
    private ToolTip _systrayTip;
    
    private FileDialog _importFileDialog;
    private DirectoryDialog _importDirDialog;
    
    private StatusBar _statusBar;
    /** uri to BrowserTab */
    private Map _openTabs;
    /** CTabItem to uri */
    private Map _openTabURIs;
    
    private List _bookmarkCache;
    
    private UIListenerPusher _uiListenerPusher;
    private List _uiListeners;
    private List _commands;
    private volatile boolean _initialized;
    
    private List _runAfterStartup;
    
    private NavigationControl _navControl;
    
    private boolean _externalShell;
    
    public Browser(DBClient client) { this(client, null, null, null, null, null); }
    public Browser(DBClient client, Shell shell, Composite root, NavigationControl navControl, ThemeRegistry themes, TranslationRegistry trans) {
        _client = client;
        _shell = shell;
        _root = root;
        if (shell != null)
            _externalShell = true;
        else
            _externalShell = false;
        //if (navControl != null)
        //    _navControl = navControl;
        //else
            _navControl = this;
        _openTabs = new HashMap();
        _openTabURIs = new HashMap();
        _uiListeners = new ArrayList();
        _commands = new ArrayList();
        _runAfterStartup = new ArrayList();
        _initialized = false;
        _uiListenerPusher = new UIListenerPusher();
        if (trans != null) {
            _translation = trans;
        } else {
            _translation = new TranslationRegistry(this, client.getRootDir());
            _translation.loadTranslations();
        }
        
        if (themes != null) {
            _themes = themes;
            _themes.setToThemeLast(this);
        } else {
            _themes = new ThemeRegistry(client, this, this);
        }
        initComponentBuilder();
        Thread t = new Thread(_uiListenerPusher, "UI msg pusher");
        t.setDaemon(true);
        t.start();
        JobRunner.instance().setUI(getUI());
        debugMessage("browser construction.  isLoggedIn? " + client.isLoggedIn());
        if (client.isLoggedIn())
            Display.getDefault().syncExec(new Runnable() { public void run() { initComponents(new Timer("logged in init", getUI(), true, -1)); } });
    }
    
    private void initComponentBuilder() {
        ComponentBuilder b = ComponentBuilder.instance();
        b.setBookmarkControl(this);
        b.setDataCallback(this);
        b.setBanControl(this);
        b.setLocalMessageCallback(this);
        b.setNavigationControl(_navControl);
        b.setURIControl(URIHelper.instance());
        b.setDBClient(_client);
        b.setUI(this);
        b.setThemeRegistry(_themes);
        b.setTranslationRegistry(_translation);
    }
    
    private void initComponents(Timer timer) {
        timer.addEvent("begin initComponents");
        ColorUtil.init();
        timer.addEvent("color init");
        ImageUtil.init(_client.getTempDir(), timer);
        timer.addEvent("image init");
        SpellUtil.init();
        timer.addEvent("spell init");
        if (_client.isLoggedIn()) {
            // doing this at the start (if we are logged in) means we don't need
            // to retheme the components later
            _themes.loadTheme();
            timer.addEvent("doStartup themes loaded");
        }
        _initialized = true;
        if (_shell == null) {
            _shell = new Shell(Display.getDefault(), SWT.SHELL_TRIM);
            _shell.setLayout(new GridLayout(1, true));
            _shell.setImage(ImageUtil.ICON_SHELL);
            _root = _shell;
            // added, doesnt work FIXME
            //showWaitCursor(true);
        }
        
        timer.addEvent("main shell constructed");
        
        initMenu(timer);
        timer.addEvent("main menu constructed");
        initSystray();
        timer.addEvent("systray constructed");
        
        // Top 90%, holding the bookmarks and tabs
        _sash = new SashForm(_root, SWT.HORIZONTAL);
        _sash.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        timer.addEvent("sash constructed");
        //_linkBar = new LinkBar(_client, getUI(), getThemeRegistry(), getTranslationRegistry(), _navControl, this, URIHelper.instance(), _sash);
        
        // The left 20% area
        _bookmarks = ComponentBuilder.instance().createBrowserTree(this, timer, _sash, new BookmarkChoiceListener(), new BookmarkAcceptListener());
        timer.addEvent("browser tree constructed");
        
        // The right 80% area
        _tabs = new CTabFolder(_sash, SWT.MULTI | SWT.TOP | SWT.CLOSE | SWT.BORDER);
        _tabs.setSimple(true);
        _tabs.setMinimizeVisible(false);
        _tabs.setMinimumCharacters(20);
        _tabs.setUnselectedImageVisible(true);
        _tabs.setBorderVisible(true);
        _tabs.marginHeight = 0;
        _tabs.marginWidth = 0;
        
        _tabMenu = new Menu(_tabs);
        _tabs.setMenu(_tabMenu);
        Image bgImage = Splash.getImage(1, _client.getTempDir());
        if (bgImage != null)
            _tabs.setBackgroundImage(bgImage);
        
        _tabs.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                CTabItem item = _tabs.getSelection();
                Object uri = _openTabURIs.get(item);
                if (uri == null) return;
                BrowserTab tab = (BrowserTab)_openTabs.get(uri);
                if (tab == null) return;
                tab.tabShown();
            }
        });
        
        _closeAllTabs = new MenuItem(_tabMenu, SWT.PUSH);
        _closeAllTabs.addSelectionListener(new FireSelectionListener() {
            public void fire() { closeAllTabs(); }
        });
        _closeOtherTabs = new MenuItem(_tabMenu, SWT.PUSH);
        _closeOtherTabs.addSelectionListener(new FireSelectionListener() {
            public void fire() { closeOtherTabs(); }
        });
        
        new MenuItem(_tabMenu, SWT.SEPARATOR);
        
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
        
        timer.addEvent("folder constructed");
        
        // The bottom strip
        _statusBar = ComponentBuilder.instance().createStatusBar(this, _root, timer);
        _statusBar.getControl().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        timer.addEvent("status bar constructed");
        
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; exit(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) { resized(); }
            public void shellIconified(ShellEvent shellEvent) { resized(); }
        });
        _shell.addControlListener(new ControlListener() {
            public void controlMoved(ControlEvent controlEvent) {}
            public void controlResized(ControlEvent controlEvent) { resized(); }
        });
        
        timer.addEvent("gui construction loaded");
        _translation.register(this);
        timer.addEvent("main browser translation registered");
        _themes.register(this);
        timer.addEvent("main browser theming registered");
        
        //_sash.setWeights(new int[] { 5, 15, 80 });
        _sash.setWeights(new int[] { 20, 80 });
        _shell.setMinimumSize(_shell.computeSize(600, 300));
        // bah, default to hiding the bookmark tree
        _sash.setMaximizedControl(_tabs);
        
        debugMessage("=tabs sized: " +_tabs.getClientArea() + "/" + _tabs.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        
        timer.addEvent("main browser resized");
        initDnD();
        timer.addEvent("main browser DnD prepared");
        
        //JobRunner.instance().enqueue(new Runnable() {
        //    public void run() { _bookmarks.viewStartupItems(); }
        //});
        _bookmarks.viewStartupItems(timer); // ocurrs async
        timer.addEvent("async startup items scheduled");
        
        debugMessage("=tabs w items: " +_tabs.getClientArea() + "/" + _tabs.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        
        if (!_externalShell)
            _shell.setVisible(false);
        
        _themes.register(this);
    }
    
    private void initDnD() {
        initDnDTabs();
    }
    private void initDnDTabs() {
        int ops = DND.DROP_COPY | DND.DROP_LINK;
        Transfer transfer[] = new Transfer[] { TextTransfer.getInstance() };
        DropTarget target = new DropTarget(_tabs, ops);
        target.setTransfer(transfer);
        target.addDropListener(new DropTargetListener() {
            public void dragEnter(DropTargetEvent evt) {
                // we can take the element
                evt.detail = evt.operations | DND.DROP_COPY;
            }
            public void dragLeave(DropTargetEvent evt) {}
            public void dragOperationChanged(DropTargetEvent evt) {}
            public void dragOver(DropTargetEvent evt) {}
            public void drop(DropTargetEvent evt) {
                if (evt.data == null) {
                    evt.detail = DND.DROP_NONE;
                    return;
                } else {
                    String str = evt.data.toString();
                    try {
                        SyndieURI uri = new SyndieURI(str);
                        view(uri);
                    } catch (URISyntaxException use) {
                        getUI().debugMessage("invalid uri: " + str, use);
                    }
                }
            }
            public void dropAccept(DropTargetEvent evt) {}
        });
        
        transfer = new Transfer[] { TextTransfer.getInstance() };
        ops = DND.DROP_COPY;
        DragSource source = new DragSource(_tabs, ops);
        source.setTransfer(transfer);
        source.addDragListener(new DragSourceListener() {
            public void dragFinished(DragSourceEvent evt) {}
            public void dragSetData(DragSourceEvent evt) {
                CTabItem item = _tabs.getSelection();
                if (item != null) {
                    SyndieURI uri = (SyndieURI)_openTabURIs.get(item);
                    BrowserTab tab = (BrowserTab)_openTabs.get(uri);
                    SyndieURI curURI = tab.getURI(); // may have changed since creation

                    BookmarkDnD bookmark = new BookmarkDnD();
                    bookmark.desc = tab.getDescription();
                    bookmark.name = tab.getName();
                    bookmark.uri = curURI;
                    evt.data = bookmark.toString();
                }
            }
            public void dragStart(DragSourceEvent evt) {
                CTabItem item = _tabs.getSelection();
                if (item != null) {
                    SyndieURI uri = (SyndieURI)_openTabURIs.get(item);
                    BrowserTab tab = (BrowserTab)_openTabs.get(uri);
                    if (tab == null) {
                       evt.doit = false;
                    } else {
                        SyndieURI curURI = tab.getURI(); // may have changed since creation
                        if (curURI == null)
                            evt.doit = false; // don't drag when nothing is selected
                    }
                } else {
                    evt.doit = false; // don't drag when nothing is selected
                }
            }
        });
    }
    private void resized() {
        for (Iterator iter = _openTabs.values().iterator(); iter.hasNext(); ) {
            ((BrowserTab)iter.next()).resized();
        }
    }
    
    public void setEngine(TextEngine engine) { _engine = engine; }
    public void startup(final Timer timer) {
        timer.addEvent("startup: loggedIn? " + _client.isLoggedIn() + " initialized? " + _initialized);
        long beforeInit = System.currentTimeMillis();
        if (_client.isLoggedIn() && !_initialized) {
            _initialized = true;
            Display.getDefault().syncExec(new Runnable() { public void run() { initComponents(timer); doStartup(timer); } });
            long afterInit = System.currentTimeMillis();
            //System.out.println("browser startup: " + (afterInit-beforeInit) + " for init and start");
            return;
        }
        long afterInit = System.currentTimeMillis();
        Display.getDefault().syncExec(new Runnable() { public void run() { doStartup(new Timer("alternate startup", getUI(), true, -1)); } });
        long afterStart = System.currentTimeMillis();
        timer.addEvent("browser startup: " + (afterInit-beforeInit) + " for init, " + (afterStart-afterInit) + " for start");
    }

    private void doStartup(final Timer timer) {
        timer.addEvent("doStartup beginning");
        debugMessage("doStartup: loggedIn? " + _client.isLoggedIn() + " initialized? " + _initialized + " nymId? " + _client.getLoggedInNymId());
        if (!_initialized || (_client.getLoggedInNymId() < 0)) {
            // show a login prompt
            LoginPrompt prompt = new LoginPrompt(_client, this);
            prompt.login();
        } else {
            if (!_themes.themeLoaded()) {
                _themes.loadTheme();
                timer.addEvent("doStartup themes loaded");
            }
            
            enableKeyFilters();
            timer.addEvent("doStartup key filters loaded");
            
            if (!_shell.isVisible()) {
                loadPosition();
                timer.addEvent("doStartup position loaded");
                _shell.open();
                _shell.forceActive();
                _shell.forceFocus();
            }
            timer.addEvent("doStartup shell displayed");
            int cnt =  _runAfterStartup.size();
            timer.addEvent("enqueueing " + cnt + " run-after-startup jobs");
            JobRunner.instance().enqueue(new Runnable() { 
                public void run() {
                    while (_runAfterStartup.size() > 0) {
                        Runnable task = (Runnable)_runAfterStartup.remove(0);
                        _shell.getDisplay().asyncExec(task);
                        //timer.addEvent("doStartup: run deferred");
                    }
                }
            });
            timer.addEvent("enqueued " + cnt + " run-after-startup jobs");
        }
    }
    
    /**
     *  These will run in the Display thread, NOT on the job queue
     */
    void runAfterStartup(Runnable job) {
        debugMessage("Adding run-after-startup job " + job);
        _runAfterStartup.add(job);
    }
    
    private void enableKeyFilters() {
        Display d = _shell.getDisplay();
        d.addFilter(SWT.KeyDown, new Listener() {
            public void handleEvent(Event evt) {
                // this *should* fire every time a key is pressed when the Syndie
                // window has focus
                if (evt.keyCode == SWT.F5) {
                    refreshTab();
                } else if ( (evt.keyCode == SWT.ARROW_RIGHT) && ((evt.stateMask & SWT.MOD3) != 0) ) { // ALT->
                    evt.type = SWT.NONE;
                    nextTab();
                } else if ( (evt.keyCode == SWT.ARROW_LEFT) && ((evt.stateMask & SWT.MOD3) != 0) ) { // ALT<-
                    evt.type = SWT.NONE;
                    prevTab();
                } else if ( (evt.character == 0x17) && ((evt.stateMask & SWT.MOD1) != 0) ) { // ^W
                    closeTab();
                } else if ( (evt.character == 0x0B) && ((evt.stateMask & SWT.MOD1) != 0) ) { // ^K
                    toggleMaxEditor();
                } else if ( (evt.character == 0x0C) && ((evt.stateMask & SWT.MOD1) != 0) ) { // ^L
                    toggleMaxView();
                }
                //System.out.println("keyDown: " + (int)evt.character + " / " + evt.detail + " / " + evt.keyCode + " / " + evt.stateMask);
            }
        });
    }
    private void refreshTab() {
        CTabItem item = _tabs.getSelection();
        if (item != null) {
            SyndieURI uri = (SyndieURI)_openTabURIs.get(item);
            BrowserTab tab = (BrowserTab)_openTabs.get(uri);
            if (tab != null)
                tab.refresh();
        }
    }
    
    private boolean getShowBookmarksPref() {
        return Boolean.valueOf(_client.getNymPrefs().getProperty("browser.showBookmarks", "true")).booleanValue();
    }
    
    private void initMenu(Timer timer) {
        _mainMenu = new Menu(_shell, SWT.BAR);
        timer.addEvent("main menu constructed");
        
        _fileMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        
        Menu fileMenu = new Menu(_fileMenuRoot);
        _fileMenuRoot.setMenu(fileMenu);
        _fileMenuOpen = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuOpen.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { openPrompt(); }
            public void widgetSelected(SelectionEvent selectionEvent) { openPrompt(); }
        });
        _fileMenuOpen.setAccelerator(SWT.MOD1 + 'o');
        _fileMenuMinimize = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuMinimize.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _shell.setVisible(false); }
            public void widgetSelected(SelectionEvent selectionEvent) { _shell.setVisible(false); }
        });
        new MenuItem(fileMenu, SWT.SEPARATOR);
        _fileMenuImport = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuImport.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { importMessage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { importMessage(); }
        });
        _fileMenuImportBulk = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuImportBulk.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { importBulkMessage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { importBulkMessage(); }
        });
        _fileMenuExport = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuExport.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { exportMessage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { exportMessage(); }
        });
        new MenuItem(fileMenu, SWT.SEPARATOR);
        _fileMenuExit = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuExit.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { exit(); }
            public void widgetSelected(SelectionEvent selectionEvent) { exit(); }
        });
        _fileMenuExit.setAccelerator(SWT.MOD1 + 'q');
        
        timer.addEvent("file menu constructed");
        
        _viewMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        _viewMenu = new Menu(_viewMenuRoot);
        _viewMenuRoot.setMenu(_viewMenu);
        _viewMenuShow = new MenuItem(_viewMenu, SWT.CHECK);
        _viewMenuShow.setSelection(getShowBookmarksPref());
        _viewMenuShow.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                _sash.setMaximizedControl(_viewMenuShow.getSelection() ? null : _tabs);
            }
            public void widgetSelected(SelectionEvent selectionEvent) {
                _sash.setMaximizedControl(_viewMenuShow.getSelection() ? null : _tabs);
            }
        });
        _viewMenuShow.setAccelerator(SWT.MOD2 + SWT.ESC); // shift-escape to toggle bookmarks
        
        timer.addEvent("bookmark menu constructed");
        
        
        _languageMenuRoot = new MenuItem(_viewMenu, SWT.CASCADE);
        _languageMenu = new Menu(_languageMenuRoot);
        _languageMenuRoot.setMenu(_languageMenu);
        _languageMenuEdit = new MenuItem(_languageMenu, SWT.PUSH);
        _languageMenuEdit.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(URIHelper.instance().createTranslateURI()); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(URIHelper.instance().createTranslateURI()); }
        });
        _languageMenuEdit.setEnabled(false);
        _languageMenuRefresh = new MenuItem(_languageMenu, SWT.PUSH);
        _languageMenuRefresh.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { populateTranslations(); }
            public void widgetSelected(SelectionEvent selectionEvent) { populateTranslations(); }
        });
        
        // queue it up to run sometime soon, but it has to occur in the swt thread, hence the nest
        JobRunner.instance().enqueue(new Runnable() {
            public void run() {
                Display.getDefault().asyncExec(new Runnable() {
                    public void run() { populateTranslations(); }
                });
            }
        });
        //populateTranslations();
        
        timer.addEvent("language menu constructed");
        
        _styleMenuRoot = new MenuItem(_viewMenu, SWT.CASCADE);
        _styleMenu = new Menu(_styleMenuRoot);
        _styleMenuRoot.setMenu(_styleMenu);
        _styleMenuChangeFont = new MenuItem(_styleMenu, SWT.PUSH);
        _styleMenuChangeFont.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { changeFont(); }
            public void widgetSelected(SelectionEvent selectionEvent) { changeFont(); }
        });
        _styleMenuIncreaseFont = new MenuItem(_styleMenu, SWT.PUSH);
        _styleMenuIncreaseFont.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { increaseFont(); }
            public void widgetSelected(SelectionEvent selectionEvent) { increaseFont(); }
        });
        _styleMenuIncreaseFont.setAccelerator(SWT.MOD1 + '='); // '=' is an unshifted +
        _styleMenuDecreaseFont = new MenuItem(_styleMenu, SWT.PUSH);
        _styleMenuDecreaseFont.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { decreaseFont(); }
            public void widgetSelected(SelectionEvent selectionEvent) { decreaseFont(); }
        });
        _styleMenuDecreaseFont.setAccelerator(SWT.MOD1 + '-');
        _styleMenuReset = new MenuItem(_styleMenu, SWT.PUSH);
        _styleMenuReset.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { resetStyle(); }
            public void widgetSelected(SelectionEvent selectionEvent) { resetStyle(); }
        });
        _styleMenuEdit = new MenuItem(_styleMenu, SWT.PUSH);
        _styleMenuEdit.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { }
            public void widgetSelected(SelectionEvent selectionEvent) { }
        });
        _styleMenuEdit.setEnabled(false);
        
        timer.addEvent("style menu constructed");
        
        _forumMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        Menu forumMenu = new Menu(_forumMenuRoot);
        _forumMenuRoot.setMenu(forumMenu);
        _forumMenuSearch = new MenuItem(forumMenu, SWT.PUSH);
        _forumMenuSearch.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { searchForums(); }
            public void widgetSelected(SelectionEvent selectionEvent) { searchForums(); }
        });
        _forumMenuBookmarked = new MenuItem(forumMenu, SWT.PUSH);
        _forumMenuBookmarked.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewBookmarked(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewBookmarked(); }
        });
        _forumMenuBrowse = new MenuItem(forumMenu, SWT.PUSH);
        _forumMenuBrowse.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(SyndieURI.DEFAULT_SEARCH_URI); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(SyndieURI.DEFAULT_SEARCH_URI); }
        });
        _forumMenuBrowseForums = new MenuItem(forumMenu, SWT.PUSH);
        _forumMenuBrowseForums.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewAllByForums(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewAllByForums(); }
        });
        _forumMenuReadPrivate = new MenuItem(forumMenu, SWT.PUSH);
        _forumMenuReadPrivate.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewPrivate(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewPrivate(); }
        });
        _forumMenuCreate = new MenuItem(forumMenu, SWT.PUSH);
        _forumMenuCreate.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(URIHelper.instance().createManageURI(null)); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(URIHelper.instance().createManageURI(null)); }
        });
        _forumMenuManageRoot = new MenuItem(forumMenu, SWT.CASCADE);
        _forumMenuManageMenu = new Menu(_forumMenuManageRoot);
        _forumMenuManageRoot.setMenu(_forumMenuManageMenu);
        
        timer.addEvent("forum menu constructed");
        
        _postMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        Menu postMenu = new Menu(_postMenuRoot);
        _postMenuRoot.setMenu(postMenu);
        _postMenuNew = new MenuItem(postMenu, SWT.PUSH);
        _postMenuNew.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { postNew(); }
            public void widgetSelected(SelectionEvent selectionEvent) { postNew(); }
        });
        _postMenuNew.setAccelerator(SWT.MOD1 + 'n');
        _postMenuWebRip = new MenuItem(postMenu, SWT.PUSH);
        _postMenuWebRip.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { postWebRip(); }
            public void widgetSelected(SelectionEvent selectionEvent) { postWebRip(); }
        });
        _postMenuResumeRoot = new MenuItem(postMenu, SWT.CASCADE);
        _postMenuResumeMenu = new Menu(_postMenuResumeRoot);
        _postMenuResumeRoot.setMenu(_postMenuResumeMenu);
    
        // queue it up to run sometime soon, but it has to occur in the swt thread, hence the nest
        Display.getDefault().timerExec(300, new Runnable() {
            public void run() { populateResumeable(); }
        });
        timer.addEvent("post menu resumeable scheduled");
        
        _postMenuManageableRoot = new MenuItem(postMenu, SWT.CASCADE);
        _postMenuManageableMenu = new Menu(_postMenuManageableRoot);
        _postMenuManageableRoot.setMenu(_postMenuManageableMenu);
        
        _postMenuPostableRoot = new MenuItem(postMenu, SWT.CASCADE);
        _postMenuPostableMenu = new Menu(_postMenuPostableRoot);
        _postMenuPostableRoot.setMenu(_postMenuPostableMenu);
        
        _postMenuPublicRoot = new MenuItem(postMenu, SWT.CASCADE);
        _postMenuPublicMenu = new Menu(_postMenuPublicRoot);
        _postMenuPublicRoot.setMenu(_postMenuPublicMenu);
        
        timer.addEvent("post menu constructed");
        
        // queue it up to run sometime soon
        _shell.getDisplay().timerExec(1000, new Runnable() {
            public void run() {
                JobRunner.instance().enqueue(new Runnable() {
                    public void run() { populatePostMenus(); }
                });
            }
        });
        
        timer.addEvent("post menu population scheduled");
        
        _syndicateMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        _syndicateMenu = new Menu(_syndicateMenuRoot);
        _syndicateMenuRoot.setMenu(_syndicateMenu);
        _syndicateMenuConfig = new MenuItem(_syndicateMenu, SWT.PUSH);
        _syndicateMenuConfig.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(URIHelper.instance().createSyndicationConfigURI()); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(URIHelper.instance().createSyndicationConfigURI()); }
        });
        _syndicateMenuOnline = new MenuItem(_syndicateMenu, SWT.PUSH);
        _syndicateMenuOnline.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { toggleOnline(); }
            public void widgetSelected(SelectionEvent selectionEvent) { toggleOnline(); }
        });
        _syndicateMenuArchive = new MenuItem(_syndicateMenu, SWT.PUSH);
        _syndicateMenuArchive.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(URIHelper.instance().createArchiveManagerURI()); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(URIHelper.instance().createArchiveManagerURI()); }
        });
                
        new MenuItem(_syndicateMenu, SWT.SEPARATOR);

        MenuItem startServer = new MenuItem(_syndicateMenu, SWT.PUSH);
        startServer.setText(getTranslationRegistry().getText("Start HTTP archive server"));
        startServer.setImage(ImageUtil.ICON_STARTARCHIVESERVER);
        MenuItem stopServer = new MenuItem(_syndicateMenu, SWT.PUSH);
        stopServer.setText(getTranslationRegistry().getText("Stop HTTP archive server"));
        stopServer.setImage(ImageUtil.ICON_STOPARCHIVESERVER);
        MenuItem configServer = new MenuItem(_syndicateMenu, SWT.PUSH);
        configServer.setText(getTranslationRegistry().getText("Configure HTTP archive server"));
        configServer.setImage(ImageUtil.ICON_CONFIGUREARCHIVESERVER);
        
        stopServer.setEnabled(false);
        new ServerConfig(startServer, stopServer, configServer);

        timer.addEvent("syndicate menu constructed");
                
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
        new MenuItem(advancedMenu, SWT.SEPARATOR);
        _advancedMenuBackupSecrets = new MenuItem(advancedMenu, SWT.PUSH);
        _advancedMenuBackupSecrets.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { backupSecrets(); }
            public void widgetSelected(SelectionEvent selectionEvent) { backupSecrets(); }
        });
        _advancedMenuRestoreSecrets = new MenuItem(advancedMenu, SWT.PUSH);
        _advancedMenuRestoreSecrets.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { restoreSecrets(); }
            public void widgetSelected(SelectionEvent selectionEvent) { restoreSecrets(); }
        });
        new MenuItem(advancedMenu, SWT.SEPARATOR);
        _advancedMenuSQL = new MenuItem(advancedMenu, SWT.PUSH);
        _advancedMenuSQL.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(URIHelper.instance().createSQLURI()); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(URIHelper.instance().createSQLURI()); }
        });
        _advancedMenuDumpResources = new MenuItem(advancedMenu, SWT.PUSH);
        _advancedMenuDumpResources.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { dumpResources(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { dumpResources(true); }
        });
        _advancedMenuDumpResourcesDiff = new MenuItem(advancedMenu, SWT.PUSH);
        _advancedMenuDumpResourcesDiff.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { dumpResources(false); }
            public void widgetSelected(SelectionEvent selectionEvent) { dumpResources(false); }
        });
        
        timer.addEvent("advanced menu constructed");
        
        new MenuItem(_mainMenu, SWT.SEPARATOR);
        
        _helpMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        Menu helpMenu = new Menu(_helpMenuRoot);
        _helpMenuRoot.setMenu(helpMenu);
        _helpMenuAbout = new MenuItem(helpMenu, SWT.PUSH);
        _helpMenuAbout.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showAbout(); }
            public void widgetSelected(SelectionEvent selectionEvent) { showAbout(); }
        });
        _helpMenuBugReport = new MenuItem(helpMenu, SWT.PUSH);
        _helpMenuBugReport.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(URIHelper.instance().createBugReportURI()); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(URIHelper.instance().createBugReportURI()); }
        });
        _helpMenuFAQ = new MenuItem(helpMenu, SWT.PUSH);
        _helpMenuFAQ.setEnabled(false);
        _helpMenuGUIManual = new MenuItem(helpMenu, SWT.PUSH);
        _helpMenuGUIManual.setEnabled(false);
        _helpMenuTextManual = new MenuItem(helpMenu, SWT.PUSH);
        _helpMenuTextManual.setEnabled(false);
        
        _shell.setMenuBar(_mainMenu);
        
        timer.addEvent("help menu constructed");
        
        /*
        _syndicationManager.addListener(new SyndicationManager.SyndicationListener() {
            public void archiveAdded(SyndicationManager mgr, String name) { refreshSyndicationMenu(); }
            public void archiveRemoved(SyndicationManager mgr, String name) { refreshSyndicationMenu(); }
            public void archiveUpdated(SyndicationManager mgr, String oldName, String newName) { refreshSyndicationMenu(); }
            public void archivesLoaded(SyndicationManager mgr) { refreshSyndicationMenu(); }
            public void archiveIndexStatus(SyndicationManager mgr, SyndicationManager.StatusRecord record) {
                if (record.getStatus() == SyndicationManager.FETCH_INDEX_DIFF_OK)
                    refreshSyndicationMenu();
            }
            public void fetchStatusUpdated(SyndicationManager mgr, SyndicationManager.StatusRecord record) {
                if (record.getStatus() == SyndicationManager.FETCH_IMPORT_OK)
                    populatePostMenus(); // might have fetched a new publically postable forum's meta
            }
            public void syndicationComplete(SyndicationManager mgr) {}
            public void onlineStateAdjusted(boolean online) {}
        });
        */
        
        /*
        JobRunner.instance().enqueue(new Runnable() {
            public void run() { _syndicationManager.loadArchives(); }
        });
         */
    }
    
    

    private void toggleOnline() {
        SyncManager mgr = SyncManager.getInstance(getClient(), getUI());
        mgr.setIsOnline(!mgr.isOnline());
        //getSyndicationManager().setOnlineStatus(!getSyndicationManager().isOnline());
    }
    
    private void refreshSyndicationMenu() {
        //_syndicationManager.loadArchives();
        _shell.getDisplay().asyncExec(new Runnable() {
            public void run() { doRefreshSyndicationMenu(); }
        });
    }
    private void doRefreshSyndicationMenu() {
        MenuItem item[] = _syndicateMenu.getItems();
        for (int i = 0; i < item.length; i++) {
            if ( (item[i] != _syndicateMenuConfig) && (item[i] != _syndicateMenuRoot) && (item[i] != _syndicateMenuOnline) && (item[i] != _syndicateMenuArchive) )
                item[i].dispose();
        }
        
        new MenuItem(_syndicateMenu, SWT.SEPARATOR);

        MenuItem startServer = new MenuItem(_syndicateMenu, SWT.PUSH);
        startServer.setText(X + getTranslationRegistry().getText("Start HTTP archive server"));
        MenuItem stopServer = new MenuItem(_syndicateMenu, SWT.PUSH);
        stopServer.setText(X + getTranslationRegistry().getText("Stop HTTP archive server"));
        MenuItem configServer = new MenuItem(_syndicateMenu, SWT.PUSH);
        configServer.setText(X + getTranslationRegistry().getText("Configure HTTP archive server"));
        
        stopServer.setEnabled(false);
        new ServerConfig(startServer, stopServer, configServer);
    }
    
    private class ServerConfig {
        private MenuItem _start;
        private MenuItem _stop;
        private MenuItem _cfg;
        
        public ServerConfig(MenuItem start, MenuItem stop, MenuItem cfg) {
            _start = start;
            _stop = stop;
            _cfg = cfg;
            
            configListeners();
            
            if (getRunOnStartup())
                runAfterStartup(new Runnable() { public void run() { start(); } });
        }
        
        private void configListeners() {
            _start.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { start(); }
                public void widgetSelected(SelectionEvent selectionEvent) { start(); }
            });
            _stop.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { stop(); }
                public void widgetSelected(SelectionEvent selectionEvent) { stop(); }
            });
            _cfg.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { config(); }
                public void widgetSelected(SelectionEvent selectionEvent) { config(); }
            });
            
            if (HTTPServ.isAlive()) {
                _start.setEnabled(false);
                _stop.setEnabled(true);
                _cfg.setEnabled(false);
            } else {
                _start.setEnabled(true);
                _stop.setEnabled(false);
                _cfg.setEnabled(true);
            }
        }
        
        private void start() {
            _start.setEnabled(false);
            _stop.setEnabled(true);
            _cfg.setEnabled(false);
            
            int port = getPort();
            boolean writable = getWritable();
            
            getUI().insertCommand("httpserv --port " + port + " --writable " + writable);
        }
        
        private int getPort() {
            Properties prefs = _client.getNymPrefs();
            String portStr = prefs.getProperty("httpserv.port");
            if (portStr != null) {
                try {
                    return Integer.parseInt(portStr);
                } catch (NumberFormatException nfe) {}
            }
            return 8080;
        }
        private boolean getWritable() {
            Properties prefs = _client.getNymPrefs();
            boolean writable = false;
            String writableStr = prefs.getProperty("httpserv.writable");
            if (writableStr != null)
                writable = Boolean.valueOf(writableStr).booleanValue();
            return writable;
        }
        private boolean getRunOnStartup() {
            return Boolean.valueOf(_client.getNymPrefs().getProperty("httpserv.runOnStartup", "false")).booleanValue();
        }
        
        private void stop() {
            _start.setEnabled(true);
            _stop.setEnabled(false);
            _cfg.setEnabled(true);
            getUI().insertCommand("httpserv --kill true");
        }
        private void config() {
            final Shell s = new Shell(_shell, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
            s.setLayout(new GridLayout(2, false));
            Label portLabel = new Label(s, SWT.NONE);
            portLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
            final Text port = new Text(s, SWT.SINGLE | SWT.BORDER);
            GridData gd = new GridData(GridData.FILL, GridData.FILL, true, false);
            gd.widthHint = 100;
            port.setLayoutData(gd);
            
            final Button writable = new Button(s, SWT.CHECK);
            writable.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
            final Button runOnStartup = new Button(s, SWT.CHECK);
            runOnStartup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
            
            Button ok = new Button(s, SWT.PUSH);
            ok.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
            
            port.setText(getPort() + "");
            writable.setSelection(getWritable());
            runOnStartup.setSelection(getRunOnStartup());
            
            ok.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { saveConfig(port.getText(), writable.getSelection(), runOnStartup.getSelection()); s.dispose(); }
                public void widgetSelected(SelectionEvent selectionEvent) { saveConfig(port.getText(), writable.getSelection(), runOnStartup.getSelection()); s.dispose(); }
            });
            
            portLabel.setText(getTranslationRegistry().getText("HTTP listen port") + ':');
            writable.setText(getTranslationRegistry().getText("Others can post new messages to this server"));
            runOnStartup.setText(getTranslationRegistry().getText("Run on startup"));
            ok.setText(getTranslationRegistry().getText("OK"));
            
            portLabel.setFont(getThemeRegistry().getTheme().DEFAULT_FONT);
            port.setFont(getThemeRegistry().getTheme().DEFAULT_FONT);
            writable.setFont(getThemeRegistry().getTheme().DEFAULT_FONT);
            ok.setFont(getThemeRegistry().getTheme().BUTTON_FONT);
            
            s.pack();
            s.open();
        }
        private void saveConfig(String portStr, boolean writable, boolean runOnStartup) {
            int port = 8080;
            try {
                port = Integer.parseInt(portStr);
            } catch (NumberFormatException nfe) {}
            Properties prefs = _client.getNymPrefs();
            prefs.setProperty("httpserv.writable", "" + writable);
            prefs.setProperty("httpserv.runOnStartup", "" + runOnStartup);
            prefs.setProperty("httpserv.port", "" + port);
            _client.setNymPrefs(prefs);
        }
    }
    
    private void showAbout() {
        final Shell s = new Shell(_shell, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        s.setLayout(new GridLayout(2, false));
        s.setText(getTranslationRegistry().getText("About"));
        
        Label l = new Label(s, SWT.NONE);
        l.setText("Syndie " + Version.VERSION);
        l.setFont(getThemeRegistry().getTheme().SHELL_FONT);
        l.setLayoutData(new GridData(GridData.CENTER, GridData.FILL, true, false, 2, 1));
        
        Link link = new Link(s, SWT.NONE);
        link.setText("<a>http://syndie.i2p2.de/</a>");
        link.setFont(getThemeRegistry().getTheme().LINK_FONT);
        link.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        link.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(SyndieURI.createURL("http://syndie.i2p2.de/")); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(SyndieURI.createURL("http://syndie.i2p2.de/")); }
        });
        
        l = new Label(s, SWT.NONE);
        l.setText("JVM:");
        l.setFont(getThemeRegistry().getTheme().DEFAULT_FONT);
        l.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        l = new Label(s, SWT.NONE);
        l.setText(System.getProperty("java.version"));
        l.setFont(getThemeRegistry().getTheme().DEFAULT_FONT);
        l.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
        
        l = new Label(s, SWT.NONE);
        l.setText("OS:");
        l.setFont(getThemeRegistry().getTheme().DEFAULT_FONT);
        l.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        l = new Label(s, SWT.NONE);
        l.setText(System.getProperty("os.name") + " " + System.getProperty("os.version") + " " + System.getProperty("os.arch"));
        l.setFont(getThemeRegistry().getTheme().DEFAULT_FONT);
        l.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
        
        l = new Label(s, SWT.NONE);
        l.setText("SWT:");
        l.setFont(getThemeRegistry().getTheme().DEFAULT_FONT);
        l.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        l = new Label(s, SWT.NONE);
        l.setText(SWT.getPlatform() + " " + SWT.getVersion());
        l.setFont(getThemeRegistry().getTheme().DEFAULT_FONT);
        l.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));

        Button b = new Button(s, SWT.PUSH);
        b.setText(getTranslationRegistry().getText("Close"));
        b.setFont(getThemeRegistry().getTheme().BUTTON_FONT);
        b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        b.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { s.dispose(); }
            public void widgetSelected(SelectionEvent selectionEvent) { s.dispose(); }
        });
        
        s.pack();
        s.open();
    }
    
    private void initSystray() {
        _systray = _shell.getDisplay().getSystemTray();
        _systrayRoot = new TrayItem(_systray, SWT.NONE);
        _systrayTip = new ToolTip(_shell, SWT.BALLOON);
        //_systrayTip.setAutoHide(false);
        _systrayRoot.setToolTip(_systrayTip);
        _systrayRoot.setImage(createSystrayIcon());
        _systrayRoot.addSelectionListener(new SelectionListener() {
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
        confirm.setText(_translation.getText("Confirm exit"));
        confirm.setMessage(_translation.getText("Are you sure you want to exit Syndie?"));
        int rv = confirm.open();
        if (rv == SWT.YES) {
            // save before hiding/disposing anything
            savePosition();
            
            // windows doesn't clean up the systray icon so quickly on exit, so
            // lets force it to show nothing explicitly
            _systrayRoot.setImage(null);
            _systrayRoot.setVisible(false);
            _systrayRoot.dispose();
            _systray.dispose();
            _shell.setVisible(false);
            
            JobRunner.instance().stop();
            
            CTabItem tabs[] = _tabs.getItems();
            ArrayList uris = new ArrayList(tabs.length);
            for (int i = 0; i < tabs.length; i++) {
                SyndieURI baseURI = (SyndieURI)_openTabURIs.get(tabs[i]);
                BrowserTab tab = (BrowserTab)_openTabs.get(baseURI);
                SyndieURI curURI = tab.getURI();
                if (!uris.contains(curURI))
                    uris.add(curURI);
            }
            
            ReferenceChooserTree.savePrevTabs(_client, uris);
            new Thread(new Runnable() {
	      public void run() { _client.close(); System.exit(0); }
	    }).start();
            //_client.close();
            //System.exit(0);
        }
    }
    
    private void savePosition() {
        boolean max = _shell.getMaximized();
        Properties prefs = _client.getNymPrefs();
        prefs.setProperty("browser.maximize", Boolean.toString(max));
        
        boolean showBookmarks = (_sash.getMaximizedControl() == null);
        prefs.setProperty("browser.showBookmarks", Boolean.toString(showBookmarks));
        
        Rectangle rect = _shell.getBounds();
        prefs.setProperty("browser.bounds.x", Integer.toString(rect.x));
        prefs.setProperty("browser.bounds.y", Integer.toString(rect.y));
        prefs.setProperty("browser.bounds.width", Integer.toString(rect.width));
        prefs.setProperty("browser.bounds.height", Integer.toString(rect.height));

        _client.setNymPrefs(prefs);
    }
    private void loadPosition() {
        Properties prefs = _client.getNymPrefs();
        boolean max = Boolean.parseBoolean(prefs.getProperty("browser.maximize"));
        _shell.setMaximized(max);
        
        _sash.setMaximizedControl(getShowBookmarksPref() ? null : _tabs);
        
        Rectangle rect = getRect(prefs, "browser.bounds.");
        if (rect != null)
            _shell.setBounds(rect);
    }
    
    private Rectangle getRect(Properties prefs, String prefix) {
        String xStr = prefs.getProperty(prefix + "x");
        String yStr = prefs.getProperty(prefix + "y");
        String widthStr = prefs.getProperty(prefix + "width");
        String heightStr = prefs.getProperty(prefix + "height");
        
        if ( (xStr != null) && (yStr != null) && (widthStr != null) && (heightStr != null) ) {
            try {
                int x = Integer.parseInt(xStr);
                int y = Integer.parseInt(yStr);
                int width = Integer.parseInt(widthStr);
                int height = Integer.parseInt(heightStr);
                
                if ( (width > 0) && (height > 0) && (x >= 0) && (y >= 0) )
                    return new Rectangle(x, y, width, height);
                else
                    return null;
            } catch (NumberFormatException nfe) {
                return null;
            }
        } else {
            return null;
        }
    }
    
    private void populateTranslations() {
        int trans = _languageMenu.getItemCount();
        String selected = null;
        // remove the old translations, if any
        int items = _languageMenu.getItemCount();
        for (int i = 0; i < items; i++) {
            MenuItem item = _languageMenu.getItem(i);
            if (item == _languageMenuEdit) {
                continue;
            } else if (item == _languageMenuRefresh) {
                continue;
            } else {
                if (item.getSelection())
                    selected = item.getText();
                item.dispose();
                i--;
                items--;
            }
        }
        
        new MenuItem(_languageMenu, SWT.SEPARATOR);
        
        // now rebuild given what we know
        List translations = _translation.getTranslations();
        if (selected == null)
            selected = _translation.getTranslation();
        for (int i = 0; i < translations.size(); i++) {
            final String translation = (String)translations.get(i);
            MenuItem item = new MenuItem(_languageMenu, SWT.RADIO);
            item.setText(translation);
            item.setSelection(translation.equals(selected));
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    _translation.switchTranslation(translation);
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    _translation.switchTranslation(translation);
                }
            });
        }
    }
    
    /**
     *  It would be nice if we could call this to update the menus while syncing but it doesn't work.
     *  We need something besides 'populate' that udates them on the fly instead of
     *  deleting and readding.
     */
    public void forumCreated() {
        populatePostMenus();
        _bookmarks.localForumCreated();
    }
    
    private void populatePostMenus() {
        final DBClient.ChannelCollector chans = _client.getNymChannels(); //_client.getChannels(true, true, true, true);
        Display.getDefault().asyncExec(new Runnable() {
            public void run() { 
                populateManageable(chans); 
                populatePostable(chans); 
                populatePublicPostable(chans);
            }
        });
    }

    private void populateManageable(DBClient.ChannelCollector chans) {
        while (_postMenuManageableMenu.getItemCount() > 0)
            _postMenuManageableMenu.getItem(0).dispose();
        
        while (_forumMenuManageMenu.getItemCount() > 0)
            _forumMenuManageMenu.getItem(0).dispose();
        
        for (int i = 0; i < chans.getIdentityChannelCount(); i++) {
            final ChannelInfo info = chans.getIdentityChannel(i);
            MenuItem item = new MenuItem(_postMenuManageableMenu, SWT.PUSH);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText("ident: ") + info.getName());
            String displayName = UIUtil.displayName(info.getName(), info.getChannelHash());
            item.setText(displayName);
            
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    view(URIHelper.instance().createPostURI(info.getChannelHash(), null));
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    view(URIHelper.instance().createPostURI(info.getChannelHash(), null));
                }
            });
            
            item = new MenuItem(_forumMenuManageMenu, SWT.PUSH);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText("ident: ") + info.getName());
            item.setText(displayName);
            
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    view(URIHelper.instance().createManageURI(info.getChannelHash()));
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    view(URIHelper.instance().createManageURI(info.getChannelHash()));
                }
            });
        }
        for (int i = 0; i < chans.getManagedChannelCount(); i++) {
            final ChannelInfo info = chans.getManagedChannel(i);
            MenuItem item = new MenuItem(_postMenuManageableMenu, SWT.PUSH);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText("ident: ") + info.getName());
            String displayName = UIUtil.displayName(info.getName(), info.getChannelHash());
            item.setText(displayName);
            
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    view(URIHelper.instance().createPostURI(info.getChannelHash(), null));
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    view(URIHelper.instance().createPostURI(info.getChannelHash(), null));
                }
            });
            
            item = new MenuItem(_forumMenuManageMenu, SWT.PUSH);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText("ident: ") + info.getName());
            item.setText(displayName);
            
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    view(URIHelper.instance().createManageURI(info.getChannelHash()));
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    view(URIHelper.instance().createManageURI(info.getChannelHash()));
                }
            });
        }
        
        _postMenuManageableRoot.setEnabled(_postMenuManageableMenu.getItemCount() > 0);
        _forumMenuManageRoot.setEnabled(_forumMenuManageMenu.getItemCount() > 0);
    }
    
    private void populatePostable(DBClient.ChannelCollector chans) {
        while (_postMenuPostableMenu.getItemCount() > 0)
            _postMenuPostableMenu.getItem(0).dispose();
        
        for (int i = 0; i < chans.getPostChannelCount(); i++) {
            final ChannelInfo info = chans.getPostChannel(i);
            MenuItem item = new MenuItem(_postMenuPostableMenu, SWT.PUSH);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText("ident: ") + info.getName());
            String name = info.getName();
            if (name != null)
                item.setText(name + " (" + info.getChannelHash().toBase64().substring(0,6) + ")");
            else
                item.setText(info.getChannelHash().toBase64().substring(0,6));
            
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    view(URIHelper.instance().createPostURI(info.getChannelHash(), null));
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    view(URIHelper.instance().createPostURI(info.getChannelHash(), null));
                }
            });
        }
        
        _postMenuPostableRoot.setEnabled(_postMenuPostableMenu.getItemCount() > 0);
    }
    
    private void populatePublicPostable(DBClient.ChannelCollector chans) {
        while (_postMenuPublicMenu.getItemCount() > 0)
            _postMenuPublicMenu.getItem(0).dispose();
        
        for (int i = 0; i < chans.getPublicPostChannelCount(); i++) {
            final ChannelInfo info = chans.getPublicPostChannel(i);
            MenuItem item = new MenuItem(_postMenuPublicMenu, SWT.PUSH);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText("ident: ") + info.getName());
            String name = info.getName();
            if (name != null)
                item.setText(name + " (" + info.getChannelHash().toBase64().substring(0,6) + ")");
            else
                item.setText(info.getChannelHash().toBase64().substring(0,6));
            
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    view(URIHelper.instance().createPostURI(info.getChannelHash(), null));
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    view(URIHelper.instance().createPostURI(info.getChannelHash(), null));
                }
            });
        }
        
        _postMenuPublicRoot.setEnabled(_postMenuPublicMenu.getItemCount() > 0);
    }
    
    public void messageCreated(SyndieURI postedURI) { populateResumeable(); }
    public void messagePostponed(long postponementId) { populateResumeable(); }
    public void messageCancelled() { populateResumeable(); }
    
    private void populateResumeable() {
        while (_postMenuResumeMenu.getItemCount() > 0)
            _postMenuResumeMenu.getItem(0).dispose();
        TreeMap postponeIdToVersion = _client.getResumeable();
        
        if (postponeIdToVersion.size() == 0) {
            _postMenuResumeRoot.setEnabled(false);
        } else {
            _postMenuResumeRoot.setEnabled(true);
            for (Iterator iter = postponeIdToVersion.entrySet().iterator(); iter.hasNext(); ) {
                Map.Entry entry = (Map.Entry)iter.next();
                final long id = ((Long)entry.getKey()).longValue();
                final int ver = ((Integer)entry.getValue()).intValue();
                String when = getVersionTime(id);
                MenuItem item = new MenuItem(_postMenuResumeMenu, SWT.PUSH);
                item.setText(when);
                item.addSelectionListener(new SelectionListener() {
                    public void widgetDefaultSelected(SelectionEvent selectionEvent) { resumePost(id, ver); }
                    public void widgetSelected(SelectionEvent selectionEvent) { resumePost(id, ver); }
                });
            }
        }
        _statusBar.refreshDisplay();
    }
    
    private static final SimpleDateFormat _fmt = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
    public static final String getVersionTime(long ts) {
        synchronized (_fmt) { return _fmt.format(new Date(ts)); }
    }
    
    public void resumePost(long postponeId, int postponeVersion) {
        view(URIHelper.instance().createPostURI(postponeId, postponeVersion));
    }
    
    public void showWaitCursor(boolean show) {
        if (show)
            _shell.setCursor(ImageUtil.CURSOR_WAIT);
        else
            _shell.setCursor(null);
    }
    
    public NavigationControl getNavControl() { return _navControl; }

    public void view(SyndieURI uri) { view(uri, null, null); }

    public void view(SyndieURI uri, String suggestedName, String suggestedDescription) {
        showWaitCursor(true);
        try {
            doView(uri, suggestedName, suggestedDescription);
        } catch (Exception e) {
            errorMessage("Internal error viewing " + uri, e);
            MessageBox box = new MessageBox(_shell, SWT.ICON_ERROR);
            box.setText(getTranslationRegistry().getText("Internal error"));
            StringBuilder buf = new StringBuilder(2048);
            buf.append(getTranslationRegistry().getText("There was an internal error viewing"))
               .append(": ").append(uri.toString()).append("\n\n");
            UIUtil.display(buf, e);
            box.setMessage(buf.toString());
            box.open();
        }
        showWaitCursor(false);
    }

    private void doView(SyndieURI uri, String suggestedName, String suggestedDescription) {
        debugMessage("Viewing [" + uri + "]");
        if (uri == null) return;
        BrowserTab tab = null;
        Hash scope = uri.getHash("scope");
        Long msgId = uri.getMessageId();
        /*
        SyndieURI browseURI = null;
        if (uri.isSearch()) {
            if ( (scope != null) && (msgId != null) )
                browseURI = SyndieURI.createMessage(scope, msgId.longValue());
            else if (scope != null)
                browseURI = SyndieURI.createScope(scope);
        }
         */
        synchronized (_openTabs) {
            for (Iterator iter = _openTabs.values().iterator(); iter.hasNext(); ) {
                BrowserTab bt = (BrowserTab)iter.next();
                if (bt.canShow(uri)) {
                    tab = bt;
                    break;
                }
            }
            if (tab == null)
                tab = (BrowserTab)_openTabs.get(uri);
            //if ( (tab == null) && (browseURI != null) )
            //    tab = (BrowserTab)_openTabs.get(browseURI);
            if (tab == null) {
                //debugMessage("building tab");
                //debugMessage("building normal URI: " + uri);
                tab = BrowserTab.build(this, uri, suggestedName, suggestedDescription);
                if (tab != null) {
                    _openTabs.put(uri, tab);
                    _openTabURIs.put(tab.getTabItem(), uri);
                }
                //debugMessage("tab built: " + tab);
            } else {
                //debugMessage("canShow(" + uri + "): " + tab);
            }
        }
        if (tab != null) {
            if (_tabs.getBackgroundImage() != null) {
                _tabs.setRedraw(false);
                _tabs.setBackgroundImage(null);
                _tabs.setRedraw(true);
            }
            tab.show(uri);
            //debugMessage("showing tab");
            _tabs.showItem(tab.getTabItem());
            //debugMessage("tab shown");
            _tabs.setSelection(tab.getTabItem());
            //debugMessage("tab selected");
            tab.tabShown();
        } 
        if (tab == null) {
            uriUnhandled(uri);
        }
    }

    private void uriUnhandled(SyndieURI uri) {
        if (uri.isURL()) {
            final Shell shell = new Shell(_shell, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
            GridLayout gl = new GridLayout(1, true);
            shell.setLayout(gl);
            shell.setText(getTranslationRegistry().getText("External URL selected"));
            
            Text msg = new Text(shell, SWT.WRAP | SWT.MULTI | SWT.READ_ONLY);
            msg.setText(getTranslationRegistry().getText("The URL selected refers to a resource outside of Syndie.  You may load this in the browser of your choice, but doing so may be risky, as Syndie cannot protect your browser, and even following this link may compromise your identity or security."));
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
            b.setText(getTranslationRegistry().getText("Close"));
            gd = new GridData(GridData.FILL, GridData.FILL, true, false);
            gd.widthHint = 400;
            b.setLayoutData(gd);
            b.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { shell.dispose(); }
                public void widgetSelected(SelectionEvent selectionEvent) { shell.dispose(); }
            });
            
            //shell.setSize(shell.computeSize(400, SWT.DEFAULT));
            shell.pack(true);
            shell.open();
            //url.selectAll()
            url.forceFocus();
        } else {
            MessageBox box = new MessageBox(_shell, SWT.ICON_ERROR | SWT.OK);
            box.setText(getTranslationRegistry().getText("Invalid URI"));
            box.setMessage(getTranslationRegistry().getText("The URI visited is not understood by Syndie") + ": " + uri.toString());
            box.open();
        }
    }
    
    public void unview(SyndieURI uri) {
        BrowserTab tab = null;
        synchronized (_openTabs) {
            tab = (BrowserTab)_openTabs.remove(uri);
            if (tab != null)
                _openTabURIs.remove(tab.getTabItem());
        }
        if (tab != null)
            tab.dispose();
    }
    
    public void bookmarkCurrentTab() {
        CTabItem item = _tabs.getSelection();
        if (item != null) {
            SyndieURI uri = (SyndieURI)_openTabURIs.get(item);
            BrowserTab tab = (BrowserTab)_openTabs.get(uri);
            SyndieURI curURI = tab.getURI(); // may have changed since creation
            
            BookmarkDnD bookmark = new BookmarkDnD();
            bookmark.uri = curURI;
            bookmark.name = tab.getName();
            bookmark.desc = tab.getDescription();
            
            NymReferenceNode parent = StatusBar.getParent(this, _translation, this, bookmark);
            long parentGroupId = -1;
            if (parent != null)
                parentGroupId = parent.getGroupId();
            bookmark(new NymReferenceNode(bookmark.name, bookmark.uri, bookmark.desc, -1, -1, parentGroupId, 0, false, false, false), true);
        }
    }
    
    /** show a popup to bookmark the given uri in the user's set of bookmarked references */
    public void bookmark(SyndieURI uri) {
        String name = "bookmark name";
        String desc = "";
        bookmark(uri, name, desc, -1, true, 0);
    }
    
    /** show a popup to bookmark the given uri in the user's set of bookmarked references */
    public void bookmark(SyndieURI uri, long parentGroupId) {
        String name = "bookmark name";
        String desc = "";
        bookmark(uri, name, desc, parentGroupId, false, 0);
    }
    /** show a popup to bookmark the given uri in the user's set of bookmarked references */
    public void bookmark(SyndieURI uri, long parentGroupId, int siblingOrder) {
        String name = "bookmark name";
        String desc = "";
        bookmark(uri, name, desc, parentGroupId, false, siblingOrder);
    }
    private void bookmark(SyndieURI uri, String name, String desc, long parentGroupId, boolean pickParent, int siblingOrder) {
        debugMessage("bookmarking "+uri + " parent=" + parentGroupId + " siblingOrder=" + siblingOrder);
        boolean loadOnStart = false;
        
        if (desc.length() <= 0) {
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
            } else if (uri.isSearch()) {
                Hash scopes[] = uri.getSearchScopes();
                if ( (scopes != null) && (scopes.length == 1) ) {
                    long chanId = _client.getChannelId(scopes[0]);
                    ChannelInfo chan = _client.getChannel(chanId);
                    if (chan != null) {
                        name = chan.getName();
                        desc = chan.getDescription();
                    }
                }
            } else if (BrowserTab.TYPE_LOGS.equals(uri.getType())) {
                name = _translation.getText("logs");
                desc = _translation.getText("watch log messages");
            } else if (BrowserTab.TYPE_POST.equals(uri.getType())) {
                name = _translation.getText("post");
                desc = _translation.getText("post a new message");
            } else if (BrowserTab.TYPE_TEXTUI.equals(uri.getType())) {
                name = _translation.getText("text UI");
                desc = _translation.getText("text based interface");
            }
        }
        
        // bookmark should always set these to false (ban/ignore would set them to true)
        boolean ignored = false;
        boolean banned = false;
        
        // the following is set by addNymReference
        long uriId = -1;
        long groupId = -1;
        
        NymReferenceNode node = new NymReferenceNode(name, uri, desc, uriId, groupId, parentGroupId, siblingOrder, ignored, banned, loadOnStart);
    
        ReferenceEditorPopup editor = new ReferenceEditorPopup(_client, getUI(), _themes, _translation, _navControl, this, this, _shell);
        //BookmarkEditorPopup editor = getBookmarkEditor();
        editor.setReference(node);
        //editor.pickParent(pickParent);
        //editor.pickOrder(false);
        editor.open();
    }
    /** called by the bookmark editor, or other things that can populate the fields properly */
    public void bookmark(NymReferenceNode node, boolean doneBookmarking) {
        debugMessage("bookmarking node: parent=" + node.getParentGroupId() + " siblingOrder=" + node.getSiblingOrder());
        _bookmarks.bookmark(node);
        //_client.addNymReference(_client.getLoggedInNymId(), node);
        //if (doneBookmarking) {
        //    _bookmarks.refreshBookmarks();
        //    debugMessage("bookmarks refreshed");
        //}
    }
    public void deleteBookmark(long bookmarkGroupId) {
        _client.deleteNymReference(_client.getLoggedInNymId(), bookmarkGroupId);
        _bookmarks.refreshBookmarks();
    }
    public void deleteBookmarks(List bookmarkGroupIds) {
        for (int i = 0; i < bookmarkGroupIds.size(); i++) {
            Long groupId = (Long)bookmarkGroupIds.get(i);
            _client.deleteNymReference(_client.getLoggedInNymId(), groupId.longValue());
        }
        _bookmarks.refreshBookmarks();
    }
    public void updateBookmark(NymReferenceNode bookmark) {
        debugMessage("updating bookmark: parent=" + bookmark.getParentGroupId() + " siblingOrder=" + bookmark.getSiblingOrder());
        _client.updateNymReference(_client.getLoggedInNymId(), bookmark);
        _bookmarks.refreshBookmarks();
    }
    public boolean isBookmarked(SyndieURI syndieURI) { return _bookmarks.isBookmarked(syndieURI); }
    
    public UI getUI() { return this; }
    public TranslationRegistry getTranslationRegistry() { return _translation; }
    
    
    
    
    public boolean ban(Hash scope) {
        String scopeName = _client.getChannelName(scope);
        if (scopeName == null)
            scopeName = "";
        scopeName = scopeName + " [" + scope.toBase64().substring(0,6) + "]";

        MessageBox box = new MessageBox(_shell, SWT.ICON_QUESTION | SWT.YES | SWT.NO);
        box.setMessage(getTranslationRegistry().getText(
                "All of the messages in it will be removed and you will never receive " +
                "any messages in it again, or posts written by the forum's owner.  Do you want to ban: ") 
                + scopeName);
        box.setText(getTranslationRegistry().getText("Confirm ban"));
        int rc = box.open();
        if (rc == SWT.YES) {
            doBan(scope);
            return true;
        } else {
            return false;
        }
    }
    private void doBan(Hash scope) {
        _client.ban(scope, getUI(), true);
        //_client.ban(scope, getUI(), true, false); 
        MessageBox box = new MessageBox(_shell, SWT.ICON_INFORMATION | SWT.OK);
        box.setText(getTranslationRegistry().getText("Banned"));
        box.setMessage(getTranslationRegistry().getText("Selected forum/author banned"));
        box.open();
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
        box.setMessage(getTranslationRegistry().getText(
                "Do you really want to tell everyone to ignore this message: " + scopeName));
        box.setText(getTranslationRegistry().getText("Confirm cancel"));
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
        box.setText(getTranslationRegistry().getText("Cancelled"));
        box.setMessage(getTranslationRegistry().getText("Selected message cancelled"));
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
        box.setMessage(getTranslationRegistry().getText(
                "Do you really want to locally delete this message: " + scopeName));
        box.setText(getTranslationRegistry().getText("Confirm delete"));
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
        box.setText(getTranslationRegistry().getText("Deleted"));
        box.setMessage(getTranslationRegistry().getText("Selected message deleted"));
        box.open();
    }

    private void postNew() { view(URIHelper.instance().createPostURI(null, null)); }
    private void showTextUI() { view(URIHelper.instance().createTextUIURI()); }
    private void showLogs() { view(URIHelper.instance().createLogsURI()); }
    
    /**
     *  @since 1.102b-5
     */
    private void changeFont() {
        FontDialog dialog = new FontDialog(_shell);
        dialog.setText(_translation.getText("Select font"));
        dialog.setFontList(_themes.getTheme().getFontData());
        FontData data = dialog.open();
        if (data == null)
            return;
        Font font;
        try {
            font = new Font(Display.getDefault(), data);
        } catch (SWTError error) {
            errorMessage("error loading " + data, new Exception(error));
            return;
        }
        Theme theme = new Theme(font);
        _shell.setRedraw(false);
        _themes.loadTheme(theme);
        _shell.setRedraw(true);
    }
    
    private void increaseFont() {
        _shell.setRedraw(false);
        _themes.increaseFont();
        _shell.setRedraw(true);
    }

    private void decreaseFont() {
        _shell.setRedraw(false);
        _themes.decreaseFont();
        _shell.setRedraw(true);
    }

    private void resetStyle() {
        _shell.setRedraw(false);
        _themes.resetTheme();
        _shell.setRedraw(true);
    }
    
    private void postWebRip() {
        WebRipPostPopup popup = new WebRipPostPopup(_client, this, _themes, _translation, this, _navControl, _shell);
        popup.open();
    }
    
    private List _lastDumpedObj;
    private List _lastDumpedSrc;
    private void dumpResources(boolean full) {
        ArrayList dumpedObj = new ArrayList();
        ArrayList dumpedSrc = new ArrayList();
        DeviceData data = _shell.getDisplay().getDeviceData();
        if (data.tracking) {
            Object objs[] = data.objects;
            for (int i = 0; i < objs.length; i++)
                dumpedObj.add(objs[i]);
            Error srcs[] = data.errors;
            for (int i = 0; i < srcs.length; i++)
                dumpedSrc.add(srcs[i]);
            
            List lost = new ArrayList();
            List added = new ArrayList();
            int addedImages = 0;
            int addedFonts = 0;
            int addedColors = 0;
            int addedGC = 0;
            for (int i = 0; i < dumpedObj.size(); i++) {
                Object cur = dumpedObj.get(i);
                if ( (_lastDumpedObj == null) || (!_lastDumpedObj.contains(cur)) ) {
                    if (cur instanceof Image) {
                        addedImages++;
                    } else if (cur instanceof Font) {
                        addedFonts++;
                    } else if (cur instanceof Color) {
                        addedColors++;
                    } else if (cur instanceof GC) {
                        addedGC++;
                    }
                    added.add(cur);
                }
            }
            if (full) {
                for (int i = 0; i < dumpedObj.size(); i++) {
                    Object cur = dumpedObj.get(i);
                    Error e = (Error)dumpedSrc.get(i);
                    if (cur instanceof Image)
                        display("", (Image)cur, e);
                    else if (cur instanceof Font)
                        display("", (Font)cur, e);
                    else if (cur instanceof Color)
                        display("", (Color)cur, e);
                    else if (cur instanceof GC)
                        display("", (GC)cur, e);
                }
            }
            if (_lastDumpedObj != null) {
                for (int i = 0; i < _lastDumpedObj.size(); i++) {
                    Object cur = (Object)_lastDumpedObj.get(i);
                    if (!dumpedObj.contains(cur))
                        lost.add(cur);
                }
            }
            debugMessage("**DUMP: " + dumpedObj.size() + " total resources, " + added.size() + " new, " + lost.size() + " removed");
            StringBuilder buf = new StringBuilder();
            buf.append("**DUMP: added: ");
            for (int i = 0; i < added.size(); i++)
                buf.append(added.get(i)).append("/").append(System.identityHashCode(added.get(i))).append(" ");
            debugMessage(buf.toString());
            buf.setLength(0);
            buf.append("**DUMP: lost: ");
            for (int i = 0; i < lost.size(); i++)
                buf.append(lost.get(i)).append("/").append(System.identityHashCode(lost.get(i))).append(" ");
            debugMessage(buf.toString());
            if (addedImages > 0) {
                for (int i = 0; i < added.size(); i++) {
                    Object cur = added.get(i);
                    Error src = (Error)dumpedSrc.get(dumpedObj.indexOf(cur));
                    if (cur instanceof Image)
                        display("added", (Image)cur, src);
                }
            }
            if (addedColors > 0) {
                for (int i = 0; i < added.size(); i++) {
                    Object cur = added.get(i);
                    Error src = (Error)dumpedSrc.get(dumpedObj.indexOf(cur));
                    if (cur instanceof Color)
                        display("added", (Color)cur, src);
                }
            }
            if (addedFonts > 0) {
                for (int i = 0; i < added.size(); i++) {
                    Object cur = added.get(i);
                    Error src = (Error)dumpedSrc.get(dumpedObj.indexOf(cur));
                    if (cur instanceof Font)
                        display("added", (Font)cur, src);
                }
            }
            if (addedGC > 0) {
                for (int i = 0; i < added.size(); i++) {
                    Object cur = added.get(i);
                    Error src = (Error)dumpedSrc.get(dumpedObj.indexOf(cur));
                    if (cur instanceof GC)
                        display("added", (GC)cur, src);
                }
            }
            _lastDumpedObj = dumpedObj;
            _lastDumpedSrc = dumpedSrc;
        }
    }
    private static final String hex(int val) {
        if (val <= 0x0f)
            return "0" + Integer.toHexString(val);
        else
            return Integer.toHexString(val);
    }
    private void display(String prefix, Image img, Error src) {
        Rectangle rect = img.getBounds();
        debugMessage("**DUMP: " + prefix + " image: " + img.toString() + " is " + rect.width +"x" + rect.height + " [" + System.identityHashCode(img) + "]");
        if (src == null) return;
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        src.printStackTrace(pw);
        debugMessage(sw.toString());
    }
    private void display(String prefix, Color c, Error src) {
        debugMessage("**DUMP: " + prefix + " color: " + hex(c.getRed()) + hex(c.getGreen()) + hex(c.getBlue()) + " [" + System.identityHashCode(c) + "]");
        if (src == null) return;
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        src.printStackTrace(pw);
        debugMessage(sw.toString());
    }
    private void display(String prefix, Font f, Error src) {
        FontData fd[] = f.getFontData();
        debugMessage("**DUMP: " + prefix + " font: " + fd[0].getName()+ "/" + fd[0].getHeight() + "/" +
                     ((fd[0].getStyle() & SWT.BOLD) != 0 ? "bold" : "") + "/" +
                     ((fd[0].getStyle() & SWT.ITALIC) != 0 ? "italic" : "") +
                     " [" + System.identityHashCode(f) + "]");
        if (src == null) return;
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        src.printStackTrace(pw);
        debugMessage(sw.toString());
    }
    private void display(String prefix, GC gc, Error src) {
        debugMessage("**DUMP: " + prefix + " GC: " + gc + " [" + System.identityHashCode(gc) + "]");
        if (src == null) return;
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        src.printStackTrace(pw);
        debugMessage(sw.toString());
    }
    
    
    private void openPrompt() {
        final Shell shell = new Shell(_shell, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        shell.setLayout(new GridLayout(3, false));
        shell.setText(getTranslationRegistry().getText("Open Syndie URI"));
        Label label = new Label(shell, SWT.NONE);
        label.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        label.setText(getTranslationRegistry().getText("Location") + ": ");
        final Text field = new Text(shell, SWT.BORDER | SWT.SINGLE);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        gd.widthHint = ImageUtil.getWidth("abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz", field);
        field.setLayoutData(gd);
        final Button ok = new Button(shell, SWT.PUSH);
        ok.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        ok.setText(getTranslationRegistry().getText("Open"));
        
        ok.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                try {
                    String txt = field.getText();
                    String ttxt = txt.trim();
                    if (!txt.equals(ttxt))
                        field.setText(ttxt);
                    SyndieURI uri = new SyndieURI(ttxt);
                    shell.dispose();
                    view(uri);
                } catch (URISyntaxException use) {
                    shell.setText(use.getReason());
                }
            }
        });
        field.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) {
                    try {
                        String txt = field.getText();
                        String ttxt = txt.trim();
                        if (!txt.equals(ttxt))
                            field.setText(ttxt);
                        SyndieURI uri = new SyndieURI(ttxt);
                        shell.dispose();
                        view(uri);
                    } catch (URISyntaxException use) {
                        shell.setText(use.getReason());
                    }
                }
            }
        });
        
        shell.pack();
        shell.open();
    }
    
    private void prevTab() { switchTab(-1); }
    private void nextTab() { switchTab(1); }
    private void switchTab(int delta) {
        int tot = _tabs.getItemCount();
        if (tot <= 0) return;
        int cur = _tabs.getSelectionIndex();
        int nxt = cur + delta;
        if (nxt < 0) nxt += tot;
        else if (nxt >= tot)
            nxt -= tot;
        debugMessage("switch tab to " + nxt);
        _tabs.setSelection(nxt);
        int sel = _tabs.getSelectionIndex();
    }
    private void closeTab() {
        CTabItem cur = _tabs.getSelection();
        SyndieURI uri = null;
        BrowserTab tab = null;
        synchronized (_openTabs) {
            uri = (SyndieURI)_openTabURIs.get(cur);
            tab = (BrowserTab)_openTabs.get(uri);
        }
        if (tab != null) {
            if (tab.close()) {
                synchronized (_openTabs) {
                    _openTabURIs.remove(cur);
                    _openTabs.remove(uri);
                }
            }
        }
        //_tabs.getSelection().dispose();
        // need to verify if they want to close, then remove 'em from _openTabs and dispose the tab
    }
    
    private void toggleMaxView() {
        CTabItem cur = _tabs.getSelection();
        SyndieURI uri = null;
        BrowserTab tab = null;
        synchronized (_openTabs) {
            uri = (SyndieURI)_openTabURIs.get(cur);
            tab = (BrowserTab)_openTabs.get(uri);
        }
        if (tab != null)
            tab.toggleMaxView();
    }
    private void toggleMaxEditor() {
        CTabItem cur = _tabs.getSelection();
        SyndieURI uri = null;
        BrowserTab tab = null;
        synchronized (_openTabs) {
            uri = (SyndieURI)_openTabURIs.get(cur);
            tab = (BrowserTab)_openTabs.get(uri);
        }
        if (tab != null)
            tab.toggleMaxEditor();
    }

    
    private void importMessage() {
        if (_importFileDialog == null) {
            _importFileDialog = new FileDialog(_shell, SWT.OPEN | SWT.MULTI);
            _importFileDialog.setFilterExtensions(new String[] { "*.syndie", "*" });
        }
        // retranslate each time
        _importFileDialog.setText(_translation.getText("Import syndie file"));
        _importFileDialog.setFilterNames(new String[] { _translation.getText("Syndie files"), _translation.getText("All files") });
        if (null != _importFileDialog.open()) {
            final String path = _importFileDialog.getFilterPath();
            final String names[] = _importFileDialog.getFileNames();
            JobRunner.instance().enqueue(new Runnable() {
                public void run() {
                    int imported = 0;
                    List files = getFiles(names, path);
                    sortFiles(files);
                    _statusBar.setEnableRefresh(false);
                    final int total = files.size();
                    for (int i = 0; i < total; i++) {
                        boolean ok = importFile((File)files.get(i));
                        if (ok)
                            imported++;
                    }
                    final int successful = imported;
                    Display.getDefault().asyncExec(new Runnable() {
                        public void run() {
                            MessageBox box = new MessageBox(_shell, SWT.ICON_INFORMATION | SWT.OK);
                            box.setText(_translation.getText("Import complete"));
                            box.setMessage(_translation.getText("Messages imported successfully/total") + ": " + successful + "/" + total);
                            box.open();
                            _statusBar.setEnableRefresh(true);
                        }
                    });
                }
            });
        }
    }
    /** recursive import directories */
    private void importBulkMessage() {
        if (_importDirDialog == null)
            _importDirDialog = new DirectoryDialog(_shell, SWT.NONE);
        // retranslate each time
        _importDirDialog.setText(_translation.getText("Import directories recursively"));
        final String dir = _importDirDialog.open();
        if (dir != null) {
            JobRunner.instance().enqueue(new Runnable() {
                public void run() {
                    int imported = 0;
                    List<File> files = new ArrayList();
                    getFiles(new File(dir), files);
                    sortFiles(files);
                    _statusBar.setEnableRefresh(false);
                    final int total = files.size();
                    for (int i = 0; i < total; i++) {
                        boolean ok = importFile((File)files.get(i));
                        if (ok)
                            imported++;
                    }
                    final int successful = imported;
                    Display.getDefault().asyncExec(new Runnable() {
                        public void run() {
                            MessageBox box = new MessageBox(_shell, SWT.ICON_INFORMATION | SWT.OK);
                            box.setText(_translation.getText("Import complete"));
                            box.setMessage(_translation.getText("Messages imported successfully/total") + ": " + successful + "/" + total);
                            box.open();
                            _statusBar.setEnableRefresh(true);
                        }
                    });
                }
            });
        }
    }
    private List<File> getFiles(String names[], String path) {
        List<File> rv = new ArrayList();
        for (int i = 0; i < names.length; i++) {
            File f = new File(path, names[i]);
            getFiles(f, rv);
        }
        return rv;
    }
    private void getFiles(File f, List<File> rv) {
        if (f.exists()) {
            if (f.isFile() && f.getName().endsWith(Constants.FILENAME_SUFFIX)) {
                rv.add(f);
            } else if (f.isDirectory() && (!f.getName().equals(".") && !f.getName().equals(".."))) {
                File files[] = f.listFiles();
                for (int i = 0; i < files.length; i++)
                    getFiles(files[i], rv);
            }
        }
    }
    private void sortFiles(List<File> orig) {
        List<File> meta = new ArrayList();
        List<File> post = new ArrayList();
        for (int i = 0; i < orig.size(); i++) {
            File f = orig.get(i);
            if (f.getName().startsWith("meta"))
                meta.add(f);
            else
                post.add(f);
        }
        orig.clear();
        orig.addAll(meta);
        orig.addAll(post);
    }
    
    /** run outside the swt thread */
    private boolean importFile(File f) {
        Importer imp = new Importer(_client, null);
        if (f.exists()) {
            try {
                boolean rv = imp.processMessage(getUI(), _client, new FileInputStream(f), null, false, null, null);
                messageImported();
                return rv;
            } catch (IOException ioe) {
                errorMessage("error importing " + f.getPath(), ioe);
                return false;
            }
        } else {
            return false;
        }
    }
    
    private void exportMessage() {
        LinkBuilderPopup popup = new LinkBuilderPopup(_client, this, _themes, _translation, _navControl, this, this, _shell, new LinkBuilderPopup.LinkBuilderSource() {
            public void uriBuildingCancelled() {}
            public void uriBuilt(SyndieURI uri, String text) {
                exportMessage(uri);
            }
            public int getPageCount() { return 0; }
            public List getAttachmentDescriptions() { return Collections.EMPTY_LIST; }
        });
        popup.setShowText(false);
        popup.limitOptions(false, false, false, true, true, false, false, false, false, false, false);
        popup.showPopup(_translation.getText("Export") + "...");
    }
    private void exportMessage(SyndieURI uri) {
        Hash scope = uri.getScope();
        Long messageId = uri.getMessageId();
        if (scope == null) return;
        File dir = new File(_client.getArchiveDir(), scope.toBase64());
        if (!dir.exists()) return;
        File src = null;
        if (messageId == null)
            src = new File(dir, "meta" + Constants.FILENAME_SUFFIX);
        else
            src = new File(dir, messageId.toString() + Constants.FILENAME_SUFFIX);
        if (!src.exists()) return;
        
        FileDialog dialog = new FileDialog(_shell, SWT.SAVE | SWT.SINGLE);
        dialog.setFileName(src.getName());
        dialog.setText(_translation.getText("Export to"));
        String filename = dialog.open();
        
        if (filename != null) {
            File target = null;
            if (filename.indexOf(File.separator) >= 0) {
                target = new File(filename);
            } else {
                String path = dialog.getFilterPath();
                target = new File(path, filename);
            }
            FileInputStream fis = null;
            FileOutputStream fos = null;
            try {
                byte buf[] = new byte[4096];
                int read = 0;
                fis = new FileInputStream(src);
                fos = new SecureFileOutputStream(target);
                while ( (read = fis.read(buf)) != -1)
                    fos.write(buf, 0, read);
                fos.close();
                fos = null;
                fis.close();
                fis = null;
            } catch (IOException ioe) {
                getUI().errorMessage("Error saving the export", ioe);
            } finally {
                if (fis != null) try { fis.close(); } catch (IOException ioe) {}
                if (fos != null) try { fos.close(); } catch (IOException ioe) {}
            }
        }
    }
    
    private void backupSecrets() { view(URIHelper.instance().createBackupSecretsURI()); }
    private void restoreSecrets() {
        FileDialog dialog = new FileDialog(_shell, SWT.OPEN | SWT.SINGLE);
        dialog.setFileName("nymkeys.dat");
        dialog.setFilterExtensions(new String[] { "*.dat", "*.*" });
        dialog.setFilterNames(new String[] { 
            getTranslationRegistry().getText("Syndie secrets files"), 
            getTranslationRegistry().getText("All files")
        });
        dialog.setText(getTranslationRegistry().getText("Select Syndie secrets file to restore"));
        String file = dialog.open();
        if (file != null) {
            File f = new File(file);
            if (!f.exists()) return;
            BackupSecrets.restore(_client, this, _themes, _translation, _shell, f);
        }
    }
    
    
    private void searchForums() {
        final ReferenceChooserPopup popup = ComponentBuilder.instance().createReferenceChooserPopup(_shell, _x("Find forums"));
        popup.setListener(new ReferenceChooserTree.AcceptanceListener() {
            public void referenceAccepted(SyndieURI uri) { view(uri); }
            public void referenceChoiceAborted() { popup.dispose(); }
        });
        popup.show();
    }
    
    private void viewBookmarked() { view(URIHelper.instance().createHighlightWatchedURI(_client, true, false, true)); }
    
    private void viewAllByForums() {
        SyndieURI defURI = SyndieURI.DEFAULT_SEARCH_URI;
        Map attr = new HashMap(defURI.getAttributes());
        attr.put("byforum", "true");
        SyndieURI uri = new SyndieURI(defURI.getType(), attr);
        view(uri);
    }
    private void viewPrivate() {
        SyndieURI defURI = SyndieURI.DEFAULT_SEARCH_URI;
        Map attr = new HashMap(defURI.getAttributes());
        attr.put("byforum", "true");
        attr.put("authorized", "false"); // don't include posts readable by authorized users
        attr.put("pbe", "false"); // don't include passphrase protected posts
        attr.put("public", "false"); // don't include posts readable by anyone
        attr.put("private", "true"); // only include posts readable by the forum admin
        attr.put("threaded", "true"); // organize in threads
        SyndieURI uri = new SyndieURI(defURI.getType(), attr);
        view(uri);
    }
    
    public CTabFolder getTabFolder() { return _tabs; }
    public DBClient getClient() { return _client; }
    //public SyndicationManager getSyndicationManager() { return _syndicationManager; }
    public ThemeRegistry getThemeRegistry() { return _themes;} 

    private void bookmarkTab() {
        CTabItem item = _tabs.getSelection();
        if (item != null) {
            for (Iterator iter = _openTabs.keySet().iterator(); iter.hasNext(); ) {
                SyndieURI uri = (SyndieURI)iter.next();
                BrowserTab tab = (BrowserTab)_openTabs.get(uri);
                if (tab.getTabItem() == item) {
                    SyndieURI curURI = tab.getURI(); // may have changed since insert
                    bookmark(curURI, tab.getName(), tab.getDescription(), -1, true, 0);
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
                    
                    TextTransfer tt = TextTransfer.getInstance();
                    Clipboard clip = new Clipboard(_shell.getDisplay());
                    Transfer xf[] = new Transfer[] { tt };
                    Object data[] = new Object[] { curURI.toString() };
                    clip.setContents(data, xf);
                    clip.dispose();
                    return;
                }
            }
        }
    }
    private void closeAllTabs() {
        for (Iterator iter = _openTabs.keySet().iterator(); iter.hasNext(); ) {
            SyndieURI uri = (SyndieURI)iter.next();
            BrowserTab tab = (BrowserTab)_openTabs.get(uri);
            if (tab.allowClose()) {
                iter.remove();
                _openTabURIs.remove(tab.getTabItem());
                tab.dispose();
            }
        }
    }
    private void closeOtherTabs() {
        CTabItem item = _tabs.getSelection();
        for (Iterator iter = _openTabs.keySet().iterator(); iter.hasNext(); ) {
            SyndieURI uri = (SyndieURI)iter.next();
            BrowserTab tab = (BrowserTab)_openTabs.get(uri);
            if (item != tab.getTabItem()) {
                if (tab.allowClose()) {
                    iter.remove();
                    _openTabURIs.remove(tab.getTabItem());
                    tab.dispose();
                }
            }
        }
    }
    
    public void messageImported() { _statusBar.refreshDisplay(); }
    public void metaImported() { _statusBar.refreshDisplay(); }
    public void readStatusUpdated() { _statusBar.refreshDisplay(); }
    
    /** get the bookmarks (NymReferenceNode) currently loaded */
    public List getBookmarks() { 
        if (_bookmarkCache == null)
            return Collections.EMPTY_LIST;
        else
            return new ArrayList(_bookmarkCache);
    }
    
    void bookmarksUpdated(List nymRefs) {
        _bookmarkCache = nymRefs;
        MenuItem items[] = _viewMenu.getItems();
        for (int i = 0; i < items.length; i++)
            if ( (items[i] != _viewMenuShow) && (items[i] != _styleMenuRoot) && (items[i] != _languageMenuRoot) )
                items[i].dispose();
        
        new MenuItem(_viewMenu, SWT.SEPARATOR);
        
        for (int i = 0; i < nymRefs.size(); i++) {
            final NymReferenceNode ref = (NymReferenceNode)nymRefs.get(i);
            bookmarksUpdated(ref, _viewMenu);
        }
        _statusBar.refreshDisplay();
    }
    private void bookmarksUpdated(final NymReferenceNode ref, Menu parent) {
        MenuItem item = null;
        if (ref.getChildCount() == 0) {
            item = new MenuItem(parent, SWT.PUSH);
        } else {
            item = new MenuItem(parent, SWT.CASCADE);
            Menu sub = new Menu(item);
            item.setMenu(sub);
            for (int j = 0; j < ref.getChildCount(); j++) {
                NymReferenceNode child = (NymReferenceNode)ref.getChild(j);
                bookmarksUpdated(child, sub);
            }
        }
        item.setText(ref.getName());
        if (ref.getURI() != null) {
            item.setImage(ImageUtil.getTypeIcon(ref.getURI()));
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(ref.getURI()); }
                public void widgetSelected(SelectionEvent selectionEvent) { view(ref.getURI()); }
            });
        }
    }
    
    private class BookmarkChoiceListener implements ReferenceChooserTree.ChoiceListener {
        public void watchedChannelSelected(TreeItem item, WatchedChannel watched) { 
            Hash scope = getClient().getChannelHash(watched.getChannelId());
            if (scope != null)
                view(SyndieURI.createScope(scope)); 
        }
        public void bookmarkSelected(TreeItem item, NymReferenceNode node) { view(node.getURI()); }
        public void manageChannelSelected(TreeItem item, ChannelInfo channel) { view(SyndieURI.createScope(channel.getChannelHash())); }
        public void postChannelSelected(TreeItem item, ChannelInfo channel) { view(SyndieURI.createScope(channel.getChannelHash())); }
        public void searchResultSelected(String name, ReferenceNode node) { view(node.getURI()); }
        public void otherSelected(TreeItem item) {}
    }
    
    private class BookmarkAcceptListener implements ReferenceChooserTree.AcceptanceListener {
        // fired on bookmark/etc doubleclick/return
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
    
    private static final Exception NO_CAUSE = new Exception();
    private static final List NO_LOCATIONS = new ArrayList(0);
    
    private static final Integer TYPE_ORDER_ERROR = new Integer(0);
    private static final Integer TYPE_ORDER_STATUS = new Integer(1);
    private static final Integer TYPE_ORDER_DEBUG = new Integer(2);
    private static final Integer TYPE_ORDER_COMMAND_COMPLETE = new Integer(3);

    private static final boolean LOG_TO_STDERR = false;
    
    private class UIListenerPusher implements Runnable {
        private List _errMsgs;
        private List _errCauses;
        private List _statusMsgs;
        private List _debugMsgs;
        private List _debugCauses;
        private List _completeStatus;
        private List _completeLocations;
        private List _typeOrder;
        
        public UIListenerPusher() {
            _errMsgs = new ArrayList(4);
            _errCauses = new ArrayList(4);
            _statusMsgs = new ArrayList(4);
            _debugMsgs = new ArrayList(4);
            _debugCauses = new ArrayList(4);
            _completeStatus = new ArrayList(4);
            _completeLocations = new ArrayList(4);
            _typeOrder = new ArrayList();
        }
        
        public void run() {
            while (true) {
                String errMsg = null;
                Exception errCause = null;
                String statusMsg = null;
                String debugMsg = null;
                Exception debugCause = null;
                Integer completeStatus = null;
                List completeLocation = null;
                
                try {
                    synchronized (UIListenerPusher.this) {
                        if (_typeOrder.size() <= 0) {
                            UIListenerPusher.this.wait();
                        } else {
                            Object type = _typeOrder.remove(0);
                            if (type == TYPE_ORDER_ERROR) {
                                errMsg = (String)_errMsgs.remove(0);
                                errCause = (Exception)_errCauses.remove(0);
                                if (errCause == NO_CAUSE)
                                    errCause = null;
                            } else if (type == TYPE_ORDER_STATUS) {
                                statusMsg = (String)_statusMsgs.remove(0);
                            } else if (type == TYPE_ORDER_DEBUG) {
                                debugMsg = (String)_debugMsgs.remove(0);
                                debugCause = (Exception)_debugCauses.remove(0);
                                if (debugCause == NO_CAUSE)
                                    debugCause = null;
                            } else if (type == TYPE_ORDER_COMMAND_COMPLETE) {
                                completeStatus = (Integer)_completeStatus.remove(0);
                                completeLocation = (List)_completeLocations.remove(0);
                                if (completeLocation == NO_LOCATIONS)
                                    completeLocation = null;
                            }
                        }
                    }
                    if (errMsg != null) {
                        for (int i = 0; i < _uiListeners.size(); i++)
                            ((UI)_uiListeners.get(i)).errorMessage(errMsg, errCause);
                    } else if (statusMsg != null) {
                        for (int i = 0; i < _uiListeners.size(); i++)
                            ((UI)_uiListeners.get(i)).statusMessage(statusMsg);
                    } else if (debugMsg != null) {
                        for (int i = 0; i < _uiListeners.size(); i++)
                            ((UI)_uiListeners.get(i)).debugMessage(debugMsg, debugCause);
                    } else if (completeStatus != null) {
                        for (int i = 0; i < _uiListeners.size(); i++)
                            ((UI)_uiListeners.get(i)).commandComplete(completeStatus.intValue(), completeLocation);
                    }
                } catch (InterruptedException ie) {}
            }
        }
        
        void errorMessage(String msg, Exception cause) {
            if (LOG_TO_STDERR) {
                if (msg != null)
                    System.err.println(now() + ": " + msg);
                if (cause != null)
                    cause.printStackTrace();
            }
            synchronized (UIListenerPusher.this) {
                _typeOrder.add(TYPE_ORDER_ERROR);
                _errMsgs.add(msg);
                if (cause == null)
                    _errCauses.add(NO_CAUSE);
                else
                    _errCauses.add(cause);
                UIListenerPusher.this.notifyAll();
            }
        }
        void statusMessage(String msg) {
            if (LOG_TO_STDERR) {
                if (msg != null)
                    System.err.println(now() + ": " + msg);
            }
            synchronized (UIListenerPusher.this) {
                _typeOrder.add(TYPE_ORDER_STATUS);
                _statusMsgs.add(msg);
                UIListenerPusher.this.notifyAll();
            }
        }
        void debugMessage(String msg, Exception cause) {
            if (LOG_TO_STDERR) {
                if (msg != null)
                    System.err.println(now() + ": " + msg);
                if (cause != null)
                    cause.printStackTrace();
            }
            synchronized (UIListenerPusher.this) {
                _typeOrder.add(TYPE_ORDER_DEBUG);
                _debugMsgs.add(msg);
                if (cause == null)
                    _debugCauses.add(NO_CAUSE);
                else
                    _debugCauses.add(cause);
                UIListenerPusher.this.notifyAll();
            }
        }
        void commandComplete(int status, List location) {
            synchronized (UIListenerPusher.this) {
                _typeOrder.add(TYPE_ORDER_COMMAND_COMPLETE);
                _completeStatus.add(new Integer(status));
                if (location == null)
                    _completeLocations.add(NO_LOCATIONS);
                else
                    _completeLocations.add(location);
                UIListenerPusher.this.notifyAll();
            }
        }
    }
    private static final SimpleDateFormat _tsFmt = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss.SSS");
    private static final String now() { synchronized (_tsFmt) { return _tsFmt.format(new Date(System.currentTimeMillis())); } }
    
    public void errorMessage(String msg) { errorMessage(msg, null); }
    public void errorMessage(String msg, Exception cause) {
        _uiListenerPusher.errorMessage(msg, cause);
        if ( (msg != null) || (cause != null) )
            _client.logError(msg, cause);
    }

    public void statusMessage(String msg) {
        _uiListenerPusher.statusMessage(msg);
        if (msg != null)
            _client.logInfo(msg);
    }
    public void debugMessage(String msg) { debugMessage(msg, null); }
    public void debugMessage(String msg, Exception cause) {
        _uiListenerPusher.debugMessage(msg, cause);
        if ( (msg != null) || (cause != null) )
            _client.logDebug(msg, cause);
    }

    public void commandComplete(int status, List location) {
        _uiListenerPusher.commandComplete(status, location);
    }
    public boolean toggleDebug() { return true; }
    public boolean togglePaginate() { return false; }
    public String readStdIn() { 
        debugMessage("readStdIn()");
        return null;
    }
    
    private Image createSystrayIcon() {
        return ImageUtil.resize(ImageUtil.ICON_SHELL, 16, 16, false);
    }

    public void addUI(UI lsnr) { synchronized (_uiListeners) { _uiListeners.add(lsnr); } }
    public void removeUI(UI lsnr) { synchronized (_uiListeners) { _uiListeners.remove(lsnr); } }
    
    // the confirm popup is created on the fly, so translated only on creation, not on translate(...)
    // the bookmark popup is created on the fly, so translated only on creation, not on translate(...)

    /** Mnemonic - see MenuItem.setText() */
    private static final char X = '&';
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText("Syndie"));
        _closeAllTabs.setText(registry.getText("close all tabs"));
        _closeOtherTabs.setText(registry.getText("close other tabs"));
        _copyTabLocation.setText(registry.getText("copy tab location"));
        _bookmarkTab.setText(registry.getText("bookmark tab"));
        
        _fileMenuRoot.setText(X + registry.getText("File"));
        _fileMenuOpen.setText(X + registry.getText("Open Syndie URI"));
        _fileMenuMinimize.setText(X + registry.getText("Minimize to the systray"));
        _fileMenuImport.setText(X + registry.getText("Import"));
        _fileMenuImportBulk.setText(X + registry.getText("Import bulk"));
        _fileMenuExport.setText(X + registry.getText("Export"));
        _fileMenuExit.setText(X + registry.getText("Quit"));
        
        _fileMenuExit.setImage(ImageUtil.ICON_FM_EXIT);
        _fileMenuImport.setImage(ImageUtil.ICON_FM_IMPORT);
        _fileMenuExport.setImage(ImageUtil.ICON_FM_EXPORT);


        _viewMenuRoot.setText(X + registry.getText("View"));
        _viewMenuShow.setText(X + registry.getText("Show bookmarks"));
        _viewMenuShow.setImage(ImageUtil.ICON_VM_BOOKMARK);
        
        
        _forumMenuRoot.setText(X + registry.getText("Forums"));
        _forumMenuSearch.setText(X + registry.getText("Find forums"));
        _forumMenuBookmarked.setText(X + registry.getText("Read forums"));
        _forumMenuBrowse.setText(X + registry.getText("Read all"));
        _forumMenuBrowseForums.setText(X + registry.getText("Read all by forum"));
        _forumMenuReadPrivate.setText(X + registry.getText("Read private messages"));
        _forumMenuCreate.setText(X + registry.getText("Create"));
        _forumMenuCreate.setImage(ImageUtil.ICON_CREATEFORUM);
        _forumMenuManageRoot.setText(X + registry.getText("Manage"));
        
        _forumMenuSearch.setImage(ImageUtil.ICON_VIEWFORUM);
        _postMenuRoot.setText(X + registry.getText("Post"));
        _postMenuNew.setText(X + registry.getText("New post"));
        _postMenuNew.setImage(ImageUtil.ICON_PM_NEWPOST);
        _postMenuWebRip.setText(X + registry.getText("Post web rip"));
        _postMenuResumeRoot.setText(X + registry.getText("Resume draft"));

        _postMenuManageableRoot.setText(X + registry.getText("Manageable forums"));
        _postMenuPostableRoot.setText(X + registry.getText("Postable forums"));
        _postMenuPublicRoot.setText(X + registry.getText("Publically postable forums"));
        
        _syndicateMenuRoot.setText(X + registry.getText("Syndicate"));
        _syndicateMenuConfig.setText(X + registry.getText("Control syndication"));
        _syndicateMenuConfig.setImage(ImageUtil.ICON_CONTROLSYNDICATION);
        _syndicateMenuOnline.setText(X + registry.getText("Toggle online state"));
        _syndicateMenuArchive.setText(X + registry.getText("Manage archive"));
        _syndicateMenuArchive.setImage(ImageUtil.ICON_MANAGEARCHIVE);

        _languageMenuRoot.setText(X + registry.getText("Language"));
        _languageMenuRoot.setImage(ImageUtil.ICON_VM_LANGUAGE);
        _languageMenuEdit.setText(X + registry.getText("Translate"));
        _languageMenuRefresh.setText(X + registry.getText("Refresh translations"));

        _styleMenuRoot.setText(X + registry.getText("Style"));
        _styleMenuRoot.setImage(ImageUtil.ICON_VM_STYLE);
        _styleMenuChangeFont.setText(X + registry.getText("Change font"));
        _styleMenuIncreaseFont.setText(X + registry.getText("Increase font"));
        _styleMenuDecreaseFont.setText(X + registry.getText("Decrease font"));
        _styleMenuReset.setText(X + registry.getText("Reset style"));
        _styleMenuEdit.setText(X + registry.getText("Configure"));
        
        _advancedMenuRoot.setText(X + registry.getText("Advanced"));
        _advancedMenuLogs.setText(X + registry.getText("Logs"));
        _advancedMenuTextUI.setText(X + registry.getText("Text interface"));
        _advancedMenuBackupSecrets.setText(X + registry.getText("Backup secrets"));
        _advancedMenuRestoreSecrets.setText(X + registry.getText("Restore secrets"));
        _advancedMenuSQL.setText(X + registry.getText("SQL interface"));
        _advancedMenuDumpResources.setText(X + registry.getText("Dump resources"));
        _advancedMenuDumpResourcesDiff.setText(X + registry.getText("Dump resource differences"));

        _helpMenuRoot.setText(X + registry.getText("Help"));
        _helpMenuAbout.setText(X + registry.getText("About"));
        _helpMenuAbout.setImage(ImageUtil.ICON_HM_ABOUT);
        _helpMenuBugReport.setText(X + registry.getText("File a new bug report"));
        _helpMenuBugReport.setImage(ImageUtil.ICON_HM_BUG);
        _helpMenuFAQ.setText(X + registry.getText("FAQ"));
        _helpMenuGUIManual.setText(X + registry.getText("GUI manual"));
        _helpMenuTextManual.setText(X + registry.getText("Text interface manual"));
        
        _systrayTip.setText(registry.getText("Syndie"));
        _systrayTip.setMessage(registry.getText("Syndie is running"));
    }
    
    public void applyTheme(Theme theme) {
        if (!_initialized || _shell == null) return; // initComponents isn't complete yet
        long t1 = System.currentTimeMillis();
        _shell.setFont(theme.SHELL_FONT);
        _tabs.setFont(theme.TAB_FONT);
        long t2 = System.currentTimeMillis();
        // recursive layout of syndie is bloody heavy duty, so avoid the flicker
        showWaitCursor(true);
        _shell.setRedraw(false);
        long t3 = System.currentTimeMillis();
        _shell.layout(true, true);
        long t4 = System.currentTimeMillis();
        _shell.setRedraw(true);
        showWaitCursor(false);
        long t5 = System.currentTimeMillis();
        debugMessage("applyTheme time: " + (t2-t1) + ", redraw: " + (t3-t2) + ", layout: " + (t4-t3) + ", redrawAgain: " + (t5-t4));
    }

    /**
     *  Tagging for static initializers. Does not translate!
     *  @return s
     *  @since 1.102b-5
     */
    protected static final String _x(String s) {
        return s;
    }
}
