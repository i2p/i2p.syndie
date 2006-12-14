package syndie.gui;

import java.io.File;
import java.io.FileInputStream;
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
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
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
import syndie.data.NymKey;
import syndie.db.ArchiveDiff;
import syndie.db.ArchiveIndex;
import syndie.db.Importer;
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
public class Browser implements UI, BrowserControl, Translatable, Themeable {
    private DBClient _client;
    private TextEngine _engine;
    private TranslationRegistry _translation;
    private ThemeRegistry _themes;
    private SyndicationManager _syndicationManager;
    private MessageEditor.MessageEditorListener _editorListener;
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
    private MenuItem _fileMenuHighlights;
    private MenuItem _fileMenuMinimize;
    private MenuItem _fileMenuNextTab;
    private MenuItem _fileMenuPrevTab;
    private MenuItem _fileMenuCloseTab;
    private MenuItem _fileMenuImport;
    private MenuItem _fileMenuExport;
    private MenuItem _fileMenuExit;
    private MenuItem _bookmarkMenuRoot;
    private Menu _bookmarkMenu;
    private MenuItem _bookmarkMenuShow;
    private MenuItem _forumMenuRoot;
    private MenuItem _forumMenuSearch;
    private MenuItem _forumMenuBrowse;
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
    private MenuItem _syndicateMenuArchives;
    private MenuItem _syndicateMenuConfig;
    private MenuItem _syndicateMenuStatus;
    private Menu _languageMenu;
    private MenuItem _languageMenuRoot;
    private MenuItem _languageMenuEdit;
    private MenuItem _languageMenuRefresh;
    private Menu _styleMenu;
    private MenuItem _styleMenuRoot;
    private MenuItem _styleMenuIncreaseFont;
    private MenuItem _styleMenuDecreaseFont;
    private MenuItem _styleMenuReset;
    private MenuItem _styleMenuEdit;
    private MenuItem _advancedMenuRoot;
    private MenuItem _advancedMenuTextUI;
    private MenuItem _advancedMenuSQL;
    private MenuItem _advancedMenuLogs;
    private MenuItem _advancedMenuDumpResources;
    private MenuItem _advancedMenuDumpResourcesDiff;
    private MenuItem _helpMenuRoot;
    private MenuItem _helpMenuAbout;
    private MenuItem _helpMenuFAQ;
    private MenuItem _helpMenuGUIManual;
    private MenuItem _helpMenuTextManual;
    private Tray _systray;
    private TrayItem _systrayRoot;
    private ToolTip _systrayTip;
    
    private FileDialog _importFileDialog;
    
    private Composite _statusRow;
    private BookmarkEditorPopup _bookmarkEditor;
    /** uri to BrowserTab */
    private Map _openTabs;
    /** CTabItem to uri */
    private Map _openTabURIs;
    
    private List _bookmarkCache;
    
    private List _uiListeners;
    private List _commands;
    private volatile boolean _initialized;
    
    public Browser(DBClient client) {
        _client = client;
        _openTabs = new HashMap();
        _openTabURIs = new HashMap();
        _uiListeners = new ArrayList();
        _commands = new ArrayList();
        _initialized = false;
        _translation = new TranslationRegistry(this);
        _themes = new ThemeRegistry(this);
        _syndicationManager = new SyndicationManager(_client, this);
        _editorListener = new MsgEditorListener();
        JobRunner.instance().setUI(getUI());
        debugMessage("browser construction.  isLoggedIn? " + client.isLoggedIn());
        if (client.isLoggedIn())
            Display.getDefault().syncExec(new Runnable() { public void run() { initComponents(); } });
    }

    private void initComponents() {
        debugMessage("browser init statics");
        long beforeColor = System.currentTimeMillis();
        ColorUtil.init();
        long beforeImage = System.currentTimeMillis();
        ImageUtil.init();
        long beforeSpell = System.currentTimeMillis();
        SpellUtil.init();
        long afterSpell = System.currentTimeMillis();
        System.out.println("color init took " + (beforeImage-beforeColor) + " image: " + (beforeSpell-beforeImage) + " spell: " + (afterSpell-beforeSpell));
        debugMessage("browser initComponents");
        _initialized = true;
        _shell = new Shell(Display.getDefault(), SWT.SHELL_TRIM);
        _shell.setLayout(new GridLayout(1, true));
        
        long t1 = System.currentTimeMillis();
        
        debugMessage("before creating the menu");
        initMenu();
        long t2 = System.currentTimeMillis();
        debugMessage("before creating the systray");
        initSystray();
        long t3 = System.currentTimeMillis();
        
        _sash = new SashForm(_shell, SWT.HORIZONTAL);
        _sash.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        long t4 = System.currentTimeMillis();
        _bookmarks = new BrowserTree(this, _sash, new BookmarkChoiceListener(), new BookmarkAcceptListener());
        long t5 = System.currentTimeMillis();
        
        _tabs = new CTabFolder(_sash, SWT.MULTI | SWT.TOP | SWT.CLOSE);
        _tabs.setSimple(false);
        _tabs.setMinimizeVisible(false);
        _tabs.setMinimumCharacters(20);
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
        
        long t6 = System.currentTimeMillis();
        
        _statusRow = new Composite(_shell, SWT.BORDER);
        _statusRow.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _statusRow.setLayout(new FillLayout(SWT.HORIZONTAL));
        new Text(_statusRow, SWT.SINGLE|SWT.READ_ONLY|SWT.BORDER).setText("this is the status bar");
        
        long t7 = System.currentTimeMillis();
        
        long t8 = System.currentTimeMillis();
        
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; exit(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        long t9 = System.currentTimeMillis();
        _translation.register(this);
        long t10 = System.currentTimeMillis();
        _themes.register(this);
        long t11 = System.currentTimeMillis();
        
        _sash.setWeights(new int[] { 20, 80 });
        _shell.setMinimumSize(_shell.computeSize(600, 300));
        
        debugMessage("=tabs sized: " +_tabs.getClientArea() + "/" + _tabs.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        
        long t12 = System.currentTimeMillis();
        //JobRunner.instance().enqueue(new Runnable() {
        //    public void run() { _bookmarks.viewStartupItems(); }
        //});
        _bookmarks.viewStartupItems(); // ocurrs async
        long t13 = System.currentTimeMillis();
        debugMessage("=tabs w items: " +_tabs.getClientArea() + "/" + _tabs.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        
        System.out.println("startup total: " + (t13-t1) + ", details: " +(t2-t1)+"/"+(t3-t2)+"/"+
                           (t4-t3)+"/"+(t5-t4)+"/"+(t6-t5)+"/"+(t7-t6)+"/"+(t8-t7)+"/"+(t9-t8)+"/"+
                           (t10-t9)+"/"+(t11-t10)+"/"+(t12-t11)+"/"+(t13-t12));
        _shell.setVisible(false);
    }
    
    public void setEngine(TextEngine engine) { _engine = engine; }
    public void startup() {
        debugMessage("startup: loggedIn? " + _client.isLoggedIn() + " initialized? " + _initialized);
        long beforeInit = System.currentTimeMillis();
        if (_client.isLoggedIn() && !_initialized) {
            _initialized = true;
            Display.getDefault().syncExec(new Runnable() { public void run() { initComponents(); doStartup(); } });
            long afterInit = System.currentTimeMillis();
            System.out.println("browser startup: " + (afterInit-beforeInit) + " for init and start");
            return;
        }
        long afterInit = System.currentTimeMillis();
        Display.getDefault().syncExec(new Runnable() { public void run() { doStartup(); } });
        long afterStart = System.currentTimeMillis();
        System.out.println("browser startup: " + (afterInit-beforeInit) + " for init, " + (afterStart-afterInit) + " for start");
    }
    private void doStartup() {
        debugMessage("doStartup: loggedIn? " + _client.isLoggedIn() + " initialized? " + _initialized + " nymId? " + _client.getLoggedInNymId());
        if (!_initialized || (_client.getLoggedInNymId() < 0)) {
            // show a login prompt
            LoginPrompt prompt = new LoginPrompt(_client, this);
            prompt.login();
        } else {
            //_syndicationManager.loadArchives();
            long t1 = System.currentTimeMillis();
            _themes.loadTheme();
            long t2 = System.currentTimeMillis();
            if (!_shell.isVisible())
                _shell.open();
            long t3 = System.currentTimeMillis();
            System.out.println("start: theme: " + (t2-t1) + " open: " +(t3-t2));
        }
    }
    
    private void initMenu() {
        _mainMenu = new Menu(_shell, SWT.BAR);
        
        _fileMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        
        Menu fileMenu = new Menu(_fileMenuRoot);
        _fileMenuRoot.setMenu(fileMenu);
        _fileMenuOpen = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuOpen.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { openPrompt(); }
            public void widgetSelected(SelectionEvent selectionEvent) { openPrompt(); }
        });
        _fileMenuHighlights = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuHighlights.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(createHighlightsURI()); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(createHighlightsURI()); }
        });
        _fileMenuMinimize = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuMinimize.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _shell.setVisible(false); }
            public void widgetSelected(SelectionEvent selectionEvent) { _shell.setVisible(false); }
        });
        _fileMenuNextTab = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuNextTab.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { nextTab(); }
            public void widgetSelected(SelectionEvent selectionEvent) { nextTab(); }
        });
        _fileMenuNextTab.setAccelerator(SWT.MOD1 + SWT.ARROW_RIGHT);
        _fileMenuPrevTab = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuPrevTab.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { prevTab(); }
            public void widgetSelected(SelectionEvent selectionEvent) { prevTab(); }
        });
        _fileMenuPrevTab.setAccelerator(SWT.MOD1 + SWT.ARROW_LEFT);
        _fileMenuCloseTab = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuCloseTab.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { closeTab(); }
            public void widgetSelected(SelectionEvent selectionEvent) { closeTab(); }
        });
        _fileMenuCloseTab.setAccelerator(SWT.MOD1 + 'w');
        _fileMenuImport = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuImport.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { importMessage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { importMessage(); }
        });
        _fileMenuExport = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuExport.setEnabled(false);
        _fileMenuExit = new MenuItem(fileMenu, SWT.PUSH);
        _fileMenuExit.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { exit(); }
            public void widgetSelected(SelectionEvent selectionEvent) { exit(); }
        });
        
        _bookmarkMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        _bookmarkMenu = new Menu(_bookmarkMenuRoot);
        _bookmarkMenuRoot.setMenu(_bookmarkMenu);
        _bookmarkMenuShow = new MenuItem(_bookmarkMenu, SWT.CHECK);
        _bookmarkMenuShow.setSelection(true);
        _bookmarkMenuShow.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                _sash.setMaximizedControl(_bookmarkMenuShow.getSelection() ? null : _tabs);
            }
            public void widgetSelected(SelectionEvent selectionEvent) {
                _sash.setMaximizedControl(_bookmarkMenuShow.getSelection() ? null : _tabs);
            }
        });
        _bookmarkMenuShow.setAccelerator(SWT.MOD2 + SWT.ESC); // shift-escape to toggle bookmarks
        
        _forumMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        Menu forumMenu = new Menu(_forumMenuRoot);
        _forumMenuRoot.setMenu(forumMenu);
        _forumMenuSearch = new MenuItem(forumMenu, SWT.PUSH);
        _forumMenuSearch.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { searchForums(); }
            public void widgetSelected(SelectionEvent selectionEvent) { searchForums(); }
        });
        _forumMenuBrowse = new MenuItem(forumMenu, SWT.PUSH);
        _forumMenuBrowse.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(SyndieURI.DEFAULT_SEARCH_URI); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(SyndieURI.DEFAULT_SEARCH_URI); }
        });
        _forumMenuCreate = new MenuItem(forumMenu, SWT.PUSH);
        _forumMenuCreate.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(createManageURI(null)); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(createManageURI(null)); }
        });
        _forumMenuManageRoot = new MenuItem(forumMenu, SWT.CASCADE);
        _forumMenuManageMenu = new Menu(_forumMenuManageRoot);
        _forumMenuManageRoot.setMenu(_forumMenuManageMenu);
        
        _postMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        Menu postMenu = new Menu(_postMenuRoot);
        _postMenuRoot.setMenu(postMenu);
        _postMenuNew = new MenuItem(postMenu, SWT.PUSH);
        _postMenuNew.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { postNew(); }
            public void widgetSelected(SelectionEvent selectionEvent) { postNew(); }
        });
        _postMenuWebRip = new MenuItem(postMenu, SWT.PUSH);
        _postMenuWebRip.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { postWebRip(); }
            public void widgetSelected(SelectionEvent selectionEvent) { postWebRip(); }
        });
        _postMenuResumeRoot = new MenuItem(postMenu, SWT.CASCADE);
        _postMenuResumeMenu = new Menu(_postMenuResumeRoot);
        _postMenuResumeRoot.setMenu(_postMenuResumeMenu);
    
        // queue it up to run sometime soon, but it has to occur in the swt thread, hence the nest
        JobRunner.instance().enqueue(new Runnable() {
            public void run() {
                Display.getDefault().asyncExec(new Runnable() {
                    public void run() { populateResumeable(); }
                });
            }
        });
        
        _postMenuManageableRoot = new MenuItem(postMenu, SWT.CASCADE);
        _postMenuManageableMenu = new Menu(_postMenuManageableRoot);
        _postMenuManageableRoot.setMenu(_postMenuManageableMenu);
        
        _postMenuPostableRoot = new MenuItem(postMenu, SWT.CASCADE);
        _postMenuPostableMenu = new Menu(_postMenuPostableRoot);
        _postMenuPostableRoot.setMenu(_postMenuPostableMenu);
        
        _postMenuPublicRoot = new MenuItem(postMenu, SWT.CASCADE);
        _postMenuPublicMenu = new Menu(_postMenuPublicRoot);
        _postMenuPublicRoot.setMenu(_postMenuPublicMenu);
        
        // queue it up to run sometime soon
        JobRunner.instance().enqueue(new Runnable() { public void run() { populatePostMenus(); } });
        
        _syndicateMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        _syndicateMenu = new Menu(_syndicateMenuRoot);
        _syndicateMenuRoot.setMenu(_syndicateMenu);
        _syndicateMenuConfig = new MenuItem(_syndicateMenu, SWT.PUSH);
        _syndicateMenuConfig.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(createSyndicationConfigURI()); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(createSyndicationConfigURI()); }
        });
        _syndicateMenuArchives = new MenuItem(_syndicateMenu, SWT.PUSH);
        _syndicateMenuArchives.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(createSyndicationArchiveURI()); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(createSyndicationArchiveURI()); }
        });
        _syndicateMenuStatus = new MenuItem(_syndicateMenu, SWT.PUSH);
        _syndicateMenuStatus.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(createSyndicationStatusURI()); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(createSyndicationStatusURI()); }
        });
        
        _languageMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        _languageMenu = new Menu(_languageMenuRoot);
        _languageMenuRoot.setMenu(_languageMenu);
        _languageMenuEdit = new MenuItem(_languageMenu, SWT.PUSH);
        _languageMenuEdit.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(createTranslateURI()); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(createTranslateURI()); }
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
        
        _styleMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        _styleMenu = new Menu(_styleMenuRoot);
        _styleMenuRoot.setMenu(_styleMenu);
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
        
        new MenuItem(_mainMenu, SWT.SEPARATOR);
        
        _helpMenuRoot = new MenuItem(_mainMenu, SWT.CASCADE);
        Menu helpMenu = new Menu(_helpMenuRoot);
        _helpMenuRoot.setMenu(helpMenu);
        _helpMenuAbout = new MenuItem(helpMenu, SWT.PUSH);
        _helpMenuAbout.setEnabled(false);
        _helpMenuFAQ = new MenuItem(helpMenu, SWT.PUSH);
        _helpMenuFAQ.setEnabled(false);
        _helpMenuGUIManual = new MenuItem(helpMenu, SWT.PUSH);
        _helpMenuGUIManual.setEnabled(false);
        _helpMenuTextManual = new MenuItem(helpMenu, SWT.PUSH);
        _helpMenuTextManual.setEnabled(false);
        
        _shell.setMenuBar(_mainMenu);
        
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
        });
        
        JobRunner.instance().enqueue(new Runnable() {
            public void run() { _syndicationManager.loadArchives(); }
        });
    }
    private static final String T_SYNDICATE_FETCHDIFF = "syndie.gui.browser.syndicate.fetchdiff";
    private static final String T_SYNDICATE_SELECTEXPLICIT = "syndie.gui.browser.syndicate.selectedexplicit";
    private static final String T_SYNDICATE_PULLDIFF = "syndie.gui.browser.syndicate.pulldiff";
    private static final String T_SYNDICATE_PULLKNOWN = "syndie.gui.browser.syndicate.pullknown";
    private static final String T_SYNDICATE_PULLBOOKMARKED = "syndie.gui.browser.syndicate.pullbookmarked";
    private static final String T_SYNDICATE_PULLINDEXES = "syndie.gui.browser.syndicate.pullindexes";
    private static final String T_SYNDICATE_PUSHDIFF = "syndie.gui.browser.syndicate.pushdiff";
    
    private void refreshSyndicationMenu() {
        _syndicationManager.loadArchives();
        _shell.getDisplay().asyncExec(new Runnable() {
            public void run() { doRefreshSyndicationMenu(); }
        });
    }
    private void doRefreshSyndicationMenu() {
        MenuItem item[] = _syndicateMenu.getItems();
        for (int i = 0; i < item.length; i++) {
            if ( (item[i] != _syndicateMenuArchives) && (item[i] != _syndicateMenuConfig) && (item[i] != _syndicateMenuStatus) && (item[i] != _syndicateMenuRoot) )
                item[i].dispose();
        }
        new MenuItem(_syndicateMenu, SWT.SEPARATOR);
        final Set pendingDiff = new HashSet();
        int numDiffs = 0;
        long totalDiffBytes = 0;
        for (int i = 0; i < _syndicationManager.getArchiveCount(); i++) {
            final String name = _syndicationManager.getArchiveName(i);
            ArchiveDiff diff = _syndicationManager.getArchiveDiff(i);
            ArchiveIndex index = _syndicationManager.getArchiveIndex(i);
            boolean archiveIsOld = false;
            if ( (index != null) && (index.getBuiltOn() + 3*60*60*1000L < System.currentTimeMillis()) )
                archiveIsOld = true;
            MenuItem base = new MenuItem(_syndicateMenu, SWT.CASCADE);
            Menu menu = new Menu(base);
            base.setMenu(menu);
            base.setText(name);
            MenuItem fetchDiff = new MenuItem(menu, SWT.PUSH);
            if (diff == null || archiveIsOld)
                pendingDiff.add(name);
            fetchDiff.setText(getTranslationRegistry().getText(T_SYNDICATE_FETCHDIFF, "Refetch index"));
            fetchDiff.setEnabled(diff == null || archiveIsOld);
            fetchDiff.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    view(createSyndicationStatusURI());
                    _syndicationManager.startFetching(1);
                    _syndicationManager.fetchIndex(name);
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    view(createSyndicationStatusURI());
                    _syndicationManager.startFetching(1);
                    _syndicationManager.fetchIndex(name);
                }
            });
            
            MenuItem pullDiff = new MenuItem(menu, SWT.PUSH);
            pullDiff.setText(getTranslationRegistry().getText(T_SYNDICATE_PULLDIFF, "Pull all differences"));
            pullDiff.setEnabled(diff != null && diff.fetchNewBytes > 0);
            pullDiff.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    view(createSyndicationStatusURI());
                    _syndicationManager.startFetching(1);
                    Set names = new HashSet(1);
                    names.add(name);
                    _syndicationManager.sync((int)ArchiveIndex.DEFAULT_MAX_SIZE, SyndicationManager.PULL_STRATEGY_DELTA, -1, names);
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    view(createSyndicationStatusURI());
                    _syndicationManager.startFetching(1);
                    Set names = new HashSet(1);
                    names.add(name);
                    _syndicationManager.sync((int)ArchiveIndex.DEFAULT_MAX_SIZE, SyndicationManager.PULL_STRATEGY_DELTA, -1, names);
                }
            });
            
            MenuItem pullKnown = new MenuItem(menu, SWT.PUSH);
            pullKnown.setText(getTranslationRegistry().getText(T_SYNDICATE_PULLKNOWN, "Pull known forums"));
            pullKnown.setEnabled(diff != null && diff.fetchKnownBytes > 0);
            pullKnown.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    view(createSyndicationStatusURI());
                    _syndicationManager.startFetching(1);
                    Set names = new HashSet(1);
                    names.add(name);
                    _syndicationManager.sync((int)ArchiveIndex.DEFAULT_MAX_SIZE, SyndicationManager.PULL_STRATEGY_DELTAKNOWN, -1, names);
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    view(createSyndicationStatusURI());
                    _syndicationManager.startFetching(1);
                    Set names = new HashSet(1);
                    names.add(name);
                    _syndicationManager.sync((int)ArchiveIndex.DEFAULT_MAX_SIZE, SyndicationManager.PULL_STRATEGY_DELTAKNOWN, -1, names);
                }
            });
            
            MenuItem pullBookmarked = new MenuItem(menu, SWT.PUSH);
            pullBookmarked.setText(getTranslationRegistry().getText(T_SYNDICATE_PULLBOOKMARKED, "Pull bookmarked forums"));
            pullBookmarked.setEnabled(diff != null && diff.fetchKnownBytes > 0);
            pullBookmarked.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    view(createSyndicationStatusURI());
                    _syndicationManager.startFetching(1);
                    Set names = new HashSet(1);
                    names.add(name);
                    _syndicationManager.sync((int)ArchiveIndex.DEFAULT_MAX_SIZE, SyndicationManager.PULL_STRATEGY_DELTABOOKMARKED, -1, names);
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    view(createSyndicationStatusURI());
                    _syndicationManager.startFetching(1);
                    Set names = new HashSet(1);
                    names.add(name);
                    _syndicationManager.sync((int)ArchiveIndex.DEFAULT_MAX_SIZE, SyndicationManager.PULL_STRATEGY_DELTABOOKMARKED, -1, names);
                }
            });
            
            new MenuItem(menu, SWT.SEPARATOR);
            
            MenuItem pushDiff = new MenuItem(menu, SWT.PUSH);
            pushDiff.setText(getTranslationRegistry().getText(T_SYNDICATE_PUSHDIFF, "Push differences"));
            pushDiff.setEnabled(diff != null);
            pushDiff.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    view(createSyndicationStatusURI());
                    _syndicationManager.startFetching(1);
                    Set names = new HashSet(1);
                    names.add(name);
                    _syndicationManager.sync((int)ArchiveIndex.DEFAULT_MAX_SIZE, -1, SyndicationManager.PUSH_STRATEGY_DELTA, names);
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    view(createSyndicationStatusURI());
                    _syndicationManager.startFetching(1);
                    Set names = new HashSet(1);
                    names.add(name);
                    _syndicationManager.sync((int)ArchiveIndex.DEFAULT_MAX_SIZE, -1, SyndicationManager.PUSH_STRATEGY_DELTA, names);
                }
            });
            
            if (diff != null) {
                numDiffs++;
                totalDiffBytes += diff.fetchNewBytes;
            }
        }

        new MenuItem(_syndicateMenu, SWT.SEPARATOR);

        MenuItem pullIndexes = new MenuItem(_syndicateMenu, SWT.PUSH);
        pullIndexes.setText(getTranslationRegistry().getText(T_SYNDICATE_PULLINDEXES, "Refetch all pending indexes"));
        pullIndexes.setEnabled(pendingDiff.size() > 0);
        pullIndexes.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                view(createSyndicationStatusURI());
                _syndicationManager.startFetching(1);
                for (Iterator iter = pendingDiff.iterator(); iter.hasNext(); ) 
                    _syndicationManager.fetchIndex((String)iter.next());
            }
            public void widgetSelected(SelectionEvent selectionEvent) {
                view(createSyndicationStatusURI());
                _syndicationManager.startFetching(1);
                for (Iterator iter = pendingDiff.iterator(); iter.hasNext(); ) 
                    _syndicationManager.fetchIndex((String)iter.next());
            }
        });
        
        MenuItem manualDiff = new MenuItem(_syndicateMenu, SWT.PUSH);
        manualDiff.setText(getTranslationRegistry().getText(T_SYNDICATE_SELECTEXPLICIT, "Select items to pull explicitly"));
        manualDiff.setEnabled(numDiffs != 0 && totalDiffBytes > 0);
        manualDiff.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                view(createSyndicationDiffURI());
            }
            public void widgetSelected(SelectionEvent selectionEvent) {
                view(createSyndicationDiffURI());
            }
        });
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
        confirm.setText(_translation.getText(T_CONFIRM_EXIT_TITLE, "Confirm exit"));
        confirm.setMessage(_translation.getText(T_CONFIRM_EXIT_MESSAGE, "Are you sure you want to exit Syndie?"));
        int rv = confirm.open();
        if (rv == SWT.YES) {
            // windows doesn't clean up the systray icon so quickly on exit, so
            // lets force it to show nothing explicitly
            _systrayRoot.setImage(null);
            _systrayRoot.setVisible(false);
            _systrayRoot.dispose();
            _systray.dispose();
            _shell.setVisible(false);
            JobRunner.instance().stop();
            System.exit(0);
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
    
    private void populatePostMenus() {
        final DBClient.ChannelCollector chans = _client.getChannels(true, true, true, true);
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
            //item.setText(_browser.getTranslationRegistry().getText(T_MANAGE_IDENT_PREFIX, "ident: ") + info.getName());
            String name = info.getName();
            if (name != null)
                item.setText(info.getChannelHash().toBase64().substring(0,6) + ": " + name);
            else
                item.setText(info.getChannelHash().toBase64().substring(0,6));
            
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    view(createPostURI(info.getChannelHash(), null));
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    view(createPostURI(info.getChannelHash(), null));
                }
            });
            
            item = new MenuItem(_forumMenuManageMenu, SWT.PUSH);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText(T_MANAGE_IDENT_PREFIX, "ident: ") + info.getName());
            if (name != null)
                item.setText(info.getChannelHash().toBase64().substring(0,6) + ": " + name);
            else
                item.setText(info.getChannelHash().toBase64().substring(0,6));
            
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    view(createManageURI(info.getChannelHash()));
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    view(createManageURI(info.getChannelHash()));
                }
            });
        }
        for (int i = 0; i < chans.getManagedChannelCount(); i++) {
            final ChannelInfo info = chans.getManagedChannel(i);
            MenuItem item = new MenuItem(_postMenuManageableMenu, SWT.PUSH);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText(T_MANAGE_IDENT_PREFIX, "ident: ") + info.getName());
            String name = info.getName();
            if (name != null)
                item.setText(info.getChannelHash().toBase64().substring(0,6) + ": " + name);
            else
                item.setText(info.getChannelHash().toBase64().substring(0,6));
            
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    view(createPostURI(info.getChannelHash(), null));
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    view(createPostURI(info.getChannelHash(), null));
                }
            });
            
            item = new MenuItem(_forumMenuManageMenu, SWT.PUSH);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText(T_MANAGE_IDENT_PREFIX, "ident: ") + info.getName());
            if (name != null)
                item.setText(info.getChannelHash().toBase64().substring(0,6) + ": " + name);
            else
                item.setText(info.getChannelHash().toBase64().substring(0,6));
            
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    view(createManageURI(info.getChannelHash()));
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    view(createManageURI(info.getChannelHash()));
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
            //item.setText(_browser.getTranslationRegistry().getText(T_MANAGE_IDENT_PREFIX, "ident: ") + info.getName());
            String name = info.getName();
            if (name != null)
                item.setText(info.getChannelHash().toBase64().substring(0,6) + ": " + name);
            else
                item.setText(info.getChannelHash().toBase64().substring(0,6));
            
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    view(createPostURI(info.getChannelHash(), null));
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    view(createPostURI(info.getChannelHash(), null));
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
            //item.setText(_browser.getTranslationRegistry().getText(T_MANAGE_IDENT_PREFIX, "ident: ") + info.getName());
            String name = info.getName();
            if (name != null)
                item.setText(info.getChannelHash().toBase64().substring(0,6) + ": " + name);
            else
                item.setText(info.getChannelHash().toBase64().substring(0,6));
            
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    view(createPostURI(info.getChannelHash(), null));
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    view(createPostURI(info.getChannelHash(), null));
                }
            });
        }
        
        _postMenuPublicRoot.setEnabled(_postMenuPublicMenu.getItemCount() > 0);
    }
    
    public MessageEditor.MessageEditorListener getMessageEditorListener() { return _editorListener; }
    
    private class MsgEditorListener implements MessageEditor.MessageEditorListener {
        public void messageCreated(MessageEditor editor, SyndieURI postedURI) { Browser.this.populateResumeable(); }
        public void messagePostponed(MessageEditor editor, long postponementId) { Browser.this.populateResumeable(); }
        public void messageCancelled(MessageEditor editor) { Browser.this.populateResumeable(); }
    }
    
    private static final Comparator INVERSE_COMPARATOR = new Comparator() {
        public int compare(Object o1, Object o2) { return ((Comparable)o2).compareTo(o1); }
        public boolean equals(Object obj) { return obj == INVERSE_COMPARATOR; }
    };

    private static final String SQL_LIST_RESUMEABLE = "SELECT postponeId, MAX(postponeVersion) FROM nymMsgPostpone WHERE nymId = ? GROUP BY postponeId";    
    /**
     * ordered map of postponeId (Long) to the most recent version (Integer),
     * with the most recent messages first 
     */
    public TreeMap getResumeable() {
        TreeMap postponeIdToVersion = new TreeMap(INVERSE_COMPARATOR);
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _client.con().prepareStatement(SQL_LIST_RESUMEABLE);
            stmt.setLong(1, _client.getLoggedInNymId());
            rs = stmt.executeQuery();
            while (rs.next()) {
                long id = rs.getLong(1);
                int ver = rs.getInt(2);
                postponeIdToVersion.put(new Long(id), new Integer(ver));
            }
        } catch (SQLException se) {
            errorMessage("Internal eror populating resumeable list", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return postponeIdToVersion;
    }

    private void populateResumeable() {
        while (_postMenuResumeMenu.getItemCount() > 0)
            _postMenuResumeMenu.getItem(0).dispose();
        TreeMap postponeIdToVersion = getResumeable();
        
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
    }
    
    private static final SimpleDateFormat _fmt = new SimpleDateFormat("yyyy/MM/dd hh:mm:ss");
    private static final String getVersionTime(long ts) {
        synchronized (_fmt) { return _fmt.format(new Date(ts)); }
    }
    
    public void resumePost(long postponeId, int postponeVersion) {
        view(createPostURI(postponeId, postponeVersion));
    }
    
    public boolean reimport(SyndieURI uri, String passphrase) {
        if ( (uri == null) || (uri.getScope() == null) ) return false;
        File dir = new File(_client.getArchiveDir(), uri.getScope().toBase64());
        File msgFile = null;
        if (uri.getMessageId() != null)
            msgFile = new File(dir, uri.getMessageId().longValue() + Constants.FILENAME_SUFFIX);
        else
            msgFile = new File(dir, "meta" + Constants.FILENAME_SUFFIX);
        Importer imp = new Importer(_client, passphrase);
        FileInputStream fin = null;
        try {
            fin = new FileInputStream(msgFile);
            boolean ok = imp.processMessage(getUI(), _client, fin, passphrase, true);
            fin.close();
            fin = null;
            debugMessage("reimport ok? " + ok + "/" + imp.wasPBE() + "/" + imp.wasMissingKey() +": " + uri);
            // wasPBE is still true if the post *was* pbe'd but the passphrase was correct.
            // wasMissingKey is true if the post was valid and imported successfully, but we don't know how to read it
            return ok && !imp.wasMissingKey(); 
        } catch (IOException ioe) {
            errorMessage("Error reimporting " + uri, ioe);
            return false;
        } finally {
            if (fin != null) try { fin.close(); } catch (IOException ioe) {}
        }
    }
    
    public void showWaitCursor(boolean show) {
        if (show)
            _shell.setCursor(ImageUtil.CURSOR_WAIT);
        else
            _shell.setCursor(null);
    }
    
    public void view(SyndieURI uri) {
        showWaitCursor(true);
        try {
            doView(uri);
        } catch (Exception e) {
            errorMessage("Internal error viewing " + uri, e);
            MessageBox box = new MessageBox(_shell, SWT.ICON_ERROR);
            box.setText(getTranslationRegistry().getText(T_ERRORVIEW_TITLE, "Internal error"));
            box.setMessage(getTranslationRegistry().getText(T_ERRORVIEW_MSG, "There was an internal error viewing the given location: ") + uri.toString());
            box.open();
        }
        showWaitCursor(false);
    }
    private void doView(SyndieURI uri) {
        debugMessage("Viewing [" + uri + "]");
        if (uri == null) return;
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
                    if (tab != null) {
                        _openTabs.put(uri, tab);
                        _openTabURIs.put(tab.getTabItem(), uri);
                    }
                } else {
                    debugMessage("building browseURI: " + browseURI);
                    tab = BrowserTab.build(this, browseURI);
                    if (tab != null) {
                        _openTabs.put(browseURI, tab);
                        _openTabURIs.put(tab.getTabItem(), browseURI);
                    }
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
            if (tab != null)
                _openTabURIs.remove(tab.getTabItem());
        }
        if (tab != null)
            tab.dispose();
    }
    private BookmarkEditorPopup getBookmarkEditor() {
        if (_bookmarkEditor == null)
            _bookmarkEditor = new BookmarkEditorPopup(this, _shell);
        return _bookmarkEditor;
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
    
        BookmarkEditorPopup editor = getBookmarkEditor();
        editor.setBookmark(node);
        editor.open();
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
    public boolean isBookmarked(SyndieURI syndieURI) { return _bookmarks.isBookmarked(syndieURI); }
    
    public UI getUI() { return this; }
    public TranslationRegistry getTranslationRegistry() { return _translation; }
    
    private void postNew() { view(createPostURI(null, null)); }
    private void showTextUI() { view(createTextUIURI()); }
    private void showLogs() { view(createLogsURI()); }
    
    private void increaseFont() { _themes.increaseFont(); }
    private void decreaseFont() { _themes.decreaseFont(); }
    private void resetStyle() { _themes.resetTheme(); }
    
    private void postWebRip() {
        WebRipPostPopup popup = new WebRipPostPopup(this, _shell);
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
            StringBuffer buf = new StringBuffer();
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
    
    private static final String T_OPEN_LABEL = "syndie.gui.browser.openlabel";
    private static final String T_OPEN_BUTTON = "syndie.gui.browser.openbutton";
    private static final String T_OPEN_TITLE = "syndie.gui.browser.opentitle";
    
    private void openPrompt() {
        final Shell shell = new Shell(_shell, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        shell.setLayout(new GridLayout(3, false));
        shell.setText(getTranslationRegistry().getText(T_OPEN_TITLE, "Open Syndie URI"));
        Label label = new Label(shell, SWT.NONE);
        label.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        label.setText(getTranslationRegistry().getText(T_OPEN_LABEL, "Location: "));
        final Text field = new Text(shell, SWT.BORDER | SWT.SINGLE);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        gd.widthHint = ImageUtil.getWidth("abcdefghijklmnopqrstuvwxyz", field);
        field.setLayoutData(gd);
        final Button ok = new Button(shell, SWT.PUSH);
        ok.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        ok.setText(getTranslationRegistry().getText(T_OPEN_BUTTON, "Open"));
        
        ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                try {
                    SyndieURI uri = new SyndieURI(field.getText());
                    shell.dispose();
                    view(uri);
                } catch (URISyntaxException use) {}
            }
            public void widgetSelected(SelectionEvent selectionEvent) {
                try {
                    SyndieURI uri = new SyndieURI(field.getText());
                    shell.dispose();
                    view(uri);
                } catch (URISyntaxException use) {}
            }
        });
        field.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) {
                    try {
                        SyndieURI uri = new SyndieURI(field.getText());
                        shell.dispose();
                        view(uri);
                    } catch (URISyntaxException use) {}
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

    private static final String T_IMPORT_SYNDIE_TITLE = "syndie.gui.browser.importsyndietitle";
    private static final String T_IMPORT_SYNDIE_EXTENSION = "syndie.gui.browser.importsyndieextension";
    private static final String T_IMPORT_ALL_EXTENSION = "syndie.gui.browser.importallextension";
    private static final String T_IMPORT_COMPLETE = "syndie.gui.browser.importcomplete";
    private static final String T_IMPORT_COMPLETE_PREFIX = "syndie.gui.browser.importcompleteprefix";
    
    private void importMessage() {
        if (_importFileDialog == null) {
            _importFileDialog = new FileDialog(_shell, SWT.OPEN | SWT.MULTI);
            _importFileDialog.setFilterExtensions(new String[] { "*.syndie", "*" });
        }
        // retranslate each time
        _importFileDialog.setText(_translation.getText(T_IMPORT_SYNDIE_TITLE, "Import syndie file"));
        _importFileDialog.setFilterNames(new String[] { _translation.getText(T_IMPORT_SYNDIE_EXTENSION, "Syndie files"), _translation.getText(T_IMPORT_ALL_EXTENSION, "All files") });
        if (null != _importFileDialog.open()) {
            final String path = _importFileDialog.getFilterPath();
            final String names[] = _importFileDialog.getFileNames();
            JobRunner.instance().enqueue(new Runnable() {
                public void run() {
                    int imported = 0;
                    final int total = names.length;
                    for (int i = 0; i < total; i++) {
                        boolean ok = importFile(path, names[i]);
                        if (ok)
                            imported++;
                    }
                    final int successful = imported;
                    Display.getDefault().asyncExec(new Runnable() {
                        public void run() {
                            MessageBox box = new MessageBox(_shell, SWT.ICON_INFORMATION | SWT.OK);
                            box.setText(_translation.getText(T_IMPORT_COMPLETE, "Import complete"));
                            box.setMessage(_translation.getText(T_IMPORT_COMPLETE_PREFIX, "Messages imported successfully/total: ") + successful + "/" + total);
                            box.open();
                        }
                    });
                }
            });
        }
    }
    
    /** run outside the swt thread */
    private boolean importFile(String path, String filename) {
        Importer imp = new Importer(_client, null);
        File f = new File(path, filename);
        if (f.exists()) {
            try {
                return imp.processMessage(getUI(), _client, new FileInputStream(f), null, false);
            } catch (IOException ioe) {
                errorMessage("error importing " + path + "/" + filename, ioe);
                return false;
            }
        } else {
            return false;
        }
    }
    
    private static final String T_SEARCH_FORUM_TITLE = "syndie.gui.browser.searchforumtitle";
    
    private void searchForums() {
        final ReferenceChooserPopup popup = new ReferenceChooserPopup(_shell, this, T_SEARCH_FORUM_TITLE, "Forum search");
        popup.setListener(new ReferenceChooserTree.AcceptanceListener() {
            public void referenceAccepted(SyndieURI uri) { view(uri); }
            public void referenceChoiceAborted() { popup.dispose(); }
        });
        popup.show();
    }
    
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
    public SyndieURI createPostURI(long postponeId, int postponeVersion) {
        Map attributes = new HashMap();
        attributes.put("postponeid", new Long(postponeId));
        attributes.put("postponever", new Long(postponeVersion));
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
    public SyndieURI createTranslateURI() { return new SyndieURI(BrowserTab.TYPE_TRANSLATE, new HashMap()); }
    public SyndieURI createHighlightsURI() { return new SyndieURI(BrowserTab.TYPE_HIGHLIGHT, new HashMap()); }
    public SyndieURI createSyndicationArchiveURI() { return new SyndieURI(BrowserTab.TYPE_SYNDICATE_ARCHIVES, new HashMap()); }
    public SyndieURI createSyndicationConfigURI() { return new SyndieURI(BrowserTab.TYPE_SYNDICATE_CONFIG, new HashMap()); }
    public SyndieURI createSyndicationDiffURI() { return createSyndicationConfigURI(); }
    public SyndieURI createSyndicationStatusURI() { return new SyndieURI(BrowserTab.TYPE_SYNDICATE_STATUS, new HashMap()); }
    
    public CTabFolder getTabFolder() { return _tabs; }
    public DBClient getClient() { return _client; }
    public SyndicationManager getSyndicationManager() { return _syndicationManager; }
    public ThemeRegistry getThemeRegistry() { return _themes;} 

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
                    //SyndieURI curURI = tab.getURI(); // may have changed since insert
                    //
                    return;
                }
            }
        }
    }
    
    /** get the bookmarks (NymReferenceNode) currently loaded */
    public List getBookmarks() { 
        if (_bookmarkCache == null)
            return Collections.EMPTY_LIST;
        else
            return new ArrayList(_bookmarkCache);
    }
    
    void bookmarksUpdated(List nymRefs) {
        _bookmarkCache = nymRefs;
        MenuItem items[] = _bookmarkMenu.getItems();
        for (int i = 0; i < items.length; i++)
            if (items[i] != _bookmarkMenuShow)
                items[i].dispose();
        
        new MenuItem(_bookmarkMenu, SWT.SEPARATOR);
        
        for (int i = 0; i < nymRefs.size(); i++) {
            final NymReferenceNode ref = (NymReferenceNode)nymRefs.get(i);
            bookmarksUpdated(ref, _bookmarkMenu);
        }
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
        // todo: make this async too
        synchronized (_uiListeners) {
            for (int i = 0; i < _uiListeners.size(); i++)
                ((UIListener)_uiListeners.get(i)).errorMessage(msg, cause);
        }
        if ( (msg != null) || (cause != null) )
            _client.logError(msg, cause);
    }

    public void statusMessage(String msg) {
        // todo: make this async too
        synchronized (_uiListeners) {
            for (int i = 0; i < _uiListeners.size(); i++)
                ((UIListener)_uiListeners.get(i)).statusMessage(msg);
        }
        if (msg != null)
            _client.logInfo(msg);
    }
    public void debugMessage(String msg) { debugMessage(msg, null); }
    public void debugMessage(String msg, Exception cause) {
        // todo: make this async too
        synchronized (_uiListeners) {
            for (int i = 0; i < _uiListeners.size(); i++)
                ((UIListener)_uiListeners.get(i)).debugMessage(msg, cause);
        }
        if ( (msg != null) || (cause != null) )
            _client.logDebug(msg, cause);
    }

    public void commandComplete(int status, List location) {
        // todo: make this async too
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
    private static final String T_FILE_MENU_HIGHLIGHTS = "syndie.gui.browser.filemenu.highlights";
    private static final String T_FILE_MENU_MINIMIZE = "syndie.gui.browser.filemenu.minimize";
    private static final String T_FILE_MENU_PREVTAB = "syndie.gui.browser.filemenu.prevtab";
    private static final String T_FILE_MENU_CLOSETAB = "syndie.gui.browser.filemenu.closetab";
    private static final String T_FILE_MENU_NEXTTAB = "syndie.gui.browser.filemenu.nexttab";
    private static final String T_FILE_MENU_IMPORT = "syndie.gui.browser.filemenu.import";
    private static final String T_FILE_MENU_EXPORT = "syndie.gui.browser.filemenu.export";
    private static final String T_FILE_MENU_EXIT = "syndie.gui.browser.filemenu.exit";
    private static final String T_FILE_MENU_EXIT_ACCELERATOR = "syndie.gui.browser.filemenu.exit.accelerator";
    private static final String T_BOOKMARK_MENU_TITLE = "syndie.gui.browser.bookmarkmenu";
    private static final String T_BOOKMARK_MENU_SHOW = "syndie.gui.browser.bookmarkmenu.show";
    private static final String T_FORUM_MENU_TITLE = "syndie.gui.browser.forummenu.title";
    private static final String T_FORUM_MENU_SEARCH = "syndie.gui.browser.forummenu.search";
    private static final String T_FORUM_MENU_BROWSE = "syndie.gui.browser.forummenu.browse";
    private static final String T_FORUM_MENU_CREATE = "syndie.gui.browser.forummenu.create";
    private static final String T_FORUM_MENU_MANAGE = "syndie.gui.browser.forummenu.manage";
    private static final String T_POST_MENU_TITLE = "syndie.gui.browser.postmenu.title";
    private static final String T_POST_MENU_NEW = "syndie.gui.browser.postmenu.new";
    private static final String T_POST_MENU_WEBRIP = "syndie.gui.browser.postmenu.webrip";
    private static final String T_POST_MENU_RESUME = "syndie.gui.browser.postmenu.resume";
    private static final String T_POST_MENU_MANAGEABLE = "syndie.gui.browser.postmenu.manageable";
    private static final String T_POST_MENU_POSTABLE = "syndie.gui.browser.postmenu.postable";
    private static final String T_POST_MENU_PUBLIC = "syndie.gui.browser.postmenu.public";
    private static final String T_SYNDICATE_MENU_TITLE = "syndie.gui.browser.syndicatemenu.title";
    private static final String T_SYNDICATE_MENU_ARCHIVES = "syndie.gui.browser.syndicatemenu.archives";
    private static final String T_SYNDICATE_MENU_CONFIG = "syndie.gui.browser.syndicatemenu.config";
    private static final String T_SYNDICATE_MENU_STATUS = "syndie.gui.browser.syndicatemenu.status";
    private static final String T_LANGUAGE_MENU_TITLE = "syndie.gui.browser.language.title";
    private static final String T_LANGUAGE_MENU_EDIT = "syndie.gui.browser.language.edit";
    private static final String T_LANGUAGE_MENU_REFRESH = "syndie.gui.browser.language.refresh";
    private static final String T_STYLE_MENU_TITLE = "syndie.gui.browser.style.title";
    private static final String T_STYLE_MENU_INCREASE = "syndie.gui.browser.style.increase";
    private static final String T_STYLE_MENU_DECREASE = "syndie.gui.browser.style.decrease";
    private static final String T_STYLE_MENU_RESET = "syndie.gui.browser.style.reset";
    private static final String T_STYLE_MENU_EDIT = "syndie.gui.browser.style.edit";
    private static final String T_ADVANCED_MENU_TITLE = "syndie.gui.browser.advancedmenu.title";
    private static final String T_ADVANCED_MENU_TEXTUI = "syndie.gui.browser.advancedmenu.textui";
    private static final String T_ADVANCED_MENU_LOGS = "syndie.gui.browser.advancedmenu.logs";
    private static final String T_ADVANCED_MENU_SQL = "syndie.gui.browser.advancedmenu.sql";
    private static final String T_ADVANCED_MENU_DUMPRESOURCES = "syndie.gui.browser.advancedmenu.dumpresources";
    private static final String T_ADVANCED_MENU_DUMPRESOURCESDIFF = "syndie.gui.browser.advancedmenu.dumpresourcesdiff";
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
    
    private static final String T_ERRORVIEW_TITLE = "syndie.gui.browser.errorview.title";
    private static final String T_ERRORVIEW_MSG = "syndie.gui.browser.errorview.msg";
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText(T_SHELL_TITLE, "Syndie"));
        _copyTabLocation.setText(registry.getText(T_COPY_TAB_LOC, "copy tab location"));
        _bookmarkTab.setText(registry.getText(T_BOOKMARK_TAB, "bookmark tab"));
        
        _fileMenuRoot.setText(registry.getText(T_FILE_MENU_TITLE, "&File"));
        _fileMenuOpen.setText(registry.getText(T_FILE_MENU_OPEN, "&Open Syndie URI"));
        _fileMenuHighlights.setText(registry.getText(T_FILE_MENU_HIGHLIGHTS, "&Highlights"));
        _fileMenuMinimize.setText(registry.getText(T_FILE_MENU_MINIMIZE, "&Minimize to the systray"));
        _fileMenuNextTab.setText(registry.getText(T_FILE_MENU_NEXTTAB, "&Next tab"));
        _fileMenuPrevTab.setText(registry.getText(T_FILE_MENU_PREVTAB, "&Previous tab"));
        _fileMenuCloseTab.setText(registry.getText(T_FILE_MENU_CLOSETAB, "&Close tab"));
        _fileMenuImport.setText(registry.getText(T_FILE_MENU_IMPORT, "&Import"));
        _fileMenuExport.setText(registry.getText(T_FILE_MENU_EXPORT, "&Export"));
        _fileMenuExit.setText(registry.getText(T_FILE_MENU_EXIT, "E&xit"));

        _bookmarkMenuRoot.setText(registry.getText(T_BOOKMARK_MENU_TITLE, "&Bookmarks"));
        _bookmarkMenuShow.setText(registry.getText(T_BOOKMARK_MENU_SHOW, "&Manage"));
        
        _forumMenuRoot.setText(registry.getText(T_FORUM_MENU_TITLE, "F&orums"));
        _forumMenuSearch.setText(registry.getText(T_FORUM_MENU_SEARCH, "&Search"));
        _forumMenuBrowse.setText(registry.getText(T_FORUM_MENU_BROWSE, "&Read all"));
        _forumMenuCreate.setText(registry.getText(T_FORUM_MENU_CREATE, "&Create"));
        _forumMenuManageRoot.setText(registry.getText(T_FORUM_MENU_MANAGE, "&Manage"));
        
        _postMenuRoot.setText(registry.getText(T_POST_MENU_TITLE, "&Post"));
        _postMenuNew.setText(registry.getText(T_POST_MENU_NEW, "Post &new"));
        _postMenuWebRip.setText(registry.getText(T_POST_MENU_WEBRIP, "Post &web rip"));
        _postMenuResumeRoot.setText(registry.getText(T_POST_MENU_RESUME, "&Resume existing"));

        _postMenuManageableRoot.setText(registry.getText(T_POST_MENU_MANAGEABLE, "&Manageable forums"));
        _postMenuPostableRoot.setText(registry.getText(T_POST_MENU_POSTABLE, "&Postable forums"));
        _postMenuPublicRoot.setText(registry.getText(T_POST_MENU_PUBLIC, "&Publically postable forums"));
        
        _syndicateMenuRoot.setText(registry.getText(T_SYNDICATE_MENU_TITLE, "&Syndicate"));
        _syndicateMenuArchives.setText(registry.getText(T_SYNDICATE_MENU_ARCHIVES, "&Manage archives"));
        _syndicateMenuConfig.setText(registry.getText(T_SYNDICATE_MENU_CONFIG, "&Control syndication"));
        _syndicateMenuStatus.setText(registry.getText(T_SYNDICATE_MENU_STATUS, "View &status"));

        _languageMenuRoot.setText(registry.getText(T_LANGUAGE_MENU_TITLE, "&Language"));
        _languageMenuEdit.setText(registry.getText(T_LANGUAGE_MENU_EDIT, "&Translate"));
        _languageMenuRefresh.setText(registry.getText(T_LANGUAGE_MENU_REFRESH, "&Refresh translations"));

        _styleMenuRoot.setText(registry.getText(T_STYLE_MENU_TITLE, "S&tyle"));
        _styleMenuIncreaseFont.setText(registry.getText(T_STYLE_MENU_INCREASE, "&Increase font"));
        _styleMenuDecreaseFont.setText(registry.getText(T_STYLE_MENU_DECREASE, "&Decrease font"));
        _styleMenuReset.setText(registry.getText(T_STYLE_MENU_RESET, "&Reset style"));
        _styleMenuEdit.setText(registry.getText(T_STYLE_MENU_EDIT, "&Configure"));
        
        _advancedMenuRoot.setText(registry.getText(T_ADVANCED_MENU_TITLE, "&Advanced"));
        _advancedMenuLogs.setText(registry.getText(T_ADVANCED_MENU_LOGS, "&Logs"));
        _advancedMenuTextUI.setText(registry.getText(T_ADVANCED_MENU_TEXTUI, "&Text interface"));
        _advancedMenuSQL.setText(registry.getText(T_ADVANCED_MENU_SQL, "&SQL interface"));
        _advancedMenuDumpResources.setText(registry.getText(T_ADVANCED_MENU_DUMPRESOURCES, "Dump resources"));
        _advancedMenuDumpResourcesDiff.setText(registry.getText(T_ADVANCED_MENU_DUMPRESOURCESDIFF, "Dump resource differences"));

        _helpMenuRoot.setText(registry.getText(T_HELP_MENU_TITLE, "&Help"));
        _helpMenuAbout.setText(registry.getText(T_HELP_MENU_ABOUT, "&About"));
        _helpMenuFAQ.setText(registry.getText(T_HELP_MENU_FAQ, "&FAQ"));
        _helpMenuGUIManual.setText(registry.getText(T_HELP_MENU_GUIMAN, "&GUI manual"));
        _helpMenuTextManual.setText(registry.getText(T_HELP_MENU_TEXTMAN, "&Text interface manual"));
        
        _systrayTip.setText(registry.getText(T_SYSTRAY_TOOLTIP_TITLE, "Syndie"));
        _systrayTip.setMessage(registry.getText(T_SYSTRAY_TOOLTIP_TEXT, "Syndie is running"));
    }
    
    public void applyTheme(Theme theme) {
        long t1 = System.currentTimeMillis();
        _shell.setFont(theme.SHELL_FONT);
        _tabs.setFont(theme.TAB_FONT);
        _statusRow.setFont(theme.DEFAULT_FONT);
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
}
