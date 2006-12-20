package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.SyndieURI;
import syndie.db.SyndicationManager;

/**
 *
 */
public class SyndicationScheduler implements Themeable, Translatable, SyndicationManager.SyndicationListener {
    private BrowserControl _browser;
    private Composite _parent;
    private Composite _root;
    private Group _archiveGroup;
    private List _archives;
    private Button _archiveAdd;
    private Label _archiveNameLabel;
    private Text _archiveName;
    private Label _urlLabel;
    private Text _url;
    private Label _passphraseLabel;
    private Text _passphrase;
    private Label _lastSyncLabel;
    private Label _lastSync;
    private Label _nextSyncLabel;
    private Label _nextSync;
    private Button _nextSyncAdjust;
    private Menu _nextSyncAdjustMenu;
    private MenuItem _nextSyncAdjustNow;
    private MenuItem _nextSyncAdjustCancel;
    private Label _proxyLabel;
    private Button _proxyDefault;
    private Button _proxyNone;
    private Button _proxyCustom;
    private Label _proxyCustomHostLabel;
    private Text _proxyCustomHost;
    private Label _proxyCustomPortLabel;
    private Text _proxyCustomPort;
    private Button _saveArchive;
    private Button _revertArchive;
    private Button _deleteArchive;
    private Button _pullPolicy;
    private Menu _pullMenu;
    private MenuItem _pullRecentDelta;
    private MenuItem _pullAllDelta;
    private MenuItem _pullRecentKnown;
    private MenuItem _pullPIR;
    private MenuItem _pullNothing;
    private MenuItem _pullPrivate;
    private MenuItem _pullPBE;
    private MenuItem _pullSizePer;
    private Menu _pullSizePerMenu;
    private MenuItem _pullSizePer32;
    private MenuItem _pullSizePer64;
    private MenuItem _pullSizePer128;
    private MenuItem _pullSizePer256;
    private MenuItem _pullSizePer512;
    private MenuItem _pullSizePer1024;
    private MenuItem _pullSizePer2048;
    private MenuItem _pullSizePer4096;
    private Button _pushPolicy;
    private Menu _pushMenu;
    private MenuItem _pushAllDelta;
    private MenuItem _pushNewDelta;
    private MenuItem _pushLocalDelta;
    private MenuItem _pushNothing;
    private Group _eventGroup;
    private Table _eventTable;
    private TableColumn _colWhen;
    private TableColumn _colArchive;
    private TableColumn _colScope;
    private TableColumn _colEvent;
    private TableColumn _colDetail;
    private Button _eventClear;
    
    public SyndicationScheduler(BrowserControl browser, Composite parent) {
        _browser = browser;
        _parent = parent;
        initComponents();
    }
    
    public void dispose() {
        _browser.getSyndicationManager().removeListener(this);
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
    }
    
    private void refreshView() {
        SyndicationManager mgr = _browser.getSyndicationManager();
        _archives.setRedraw(false);
        String sel = null;
        String selection[] = _archives.getSelection();
        if ( (selection != null) && (selection.length > 0) )
            sel = selection[0];
        _archives.removeAll();
        int archives = mgr.getArchiveCount();
        for (int i = 0; i < archives; i++)
            _archives.add(mgr.getArchiveName(i));
        _archives.setSelection(new String[] { sel });
        if (sel != null) showArchive(sel);
        _archives.setRedraw(true);
    }
    
    private void showArchive() {
        String names[] = _archives.getSelection();
        if ( (names != null) && (names.length > 0) ) {
            showArchive(names[0]);
        }
    }
    private void showArchive(String name) {
        if ( (name != null) && (name.length() > 0) ) {
            SyndicationManager mgr = _browser.getSyndicationManager();
            int index = mgr.getArchiveNum(name);
            SyndieURI uri = mgr.getArchiveURI(index);
            String host = mgr.getCustomProxyHost(index);
            int port = mgr.getCustomProxyPort(index);
            long lastSync = mgr.getLastSyncDate(index);
            long nextSync = mgr.getNextSyncDate(index);

            _archiveName.setText(name);
            _url.setText(uri.getURL());
            String pass = uri.getString("passphrase");
            if (pass != null)
                _passphrase.setText(pass);
            else
                _passphrase.setText("");
            
            if (lastSync < 0)
                _lastSync.setText(_browser.getTranslationRegistry().getText(T_SYNC_NEVER, "Never"));
            else
                _lastSync.setText(Constants.getDateTime(lastSync));
            if (nextSync < 0)
                _nextSync.setText(_browser.getTranslationRegistry().getText(T_SYNC_NEVER, "Never"));
            else
                _nextSync.setText(Constants.getDateTime(nextSync));

            _proxyCustom.setSelection(false);
            _proxyDefault.setSelection(false);
            _proxyNone.setSelection(false);
            if ( (host != null) && (port > 0) ) {
                _proxyCustom.setSelection(true);
                _proxyCustomHost.setText(host);
                _proxyCustomPort.setText(port+"");
            } else {
                _proxyDefault.setSelection(true);
            }
        } else {
            _archiveName.setText("");
            _url.setText("");
            _passphrase.setText("");
            _lastSync.setText("");
            _nextSync.setText("");

            _proxyCustom.setSelection(false);
            _proxyDefault.setSelection(true);
            _proxyNone.setSelection(false);
        }
        
        _lastSync.getParent().layout(true);
    }
    
    private void scheduleSync(long when) {
        String names[] = _archives.getSelection();
        if ( (names != null) && (names.length > 0) ) {
            SyndicationManager mgr = _browser.getSyndicationManager();
            mgr.setNextSync(names[0], when);
            refreshView();
        }
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(3, false));
        
        _archiveGroup = new Group(_root, SWT.NONE);
        _archiveGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, true, 1, 6));
        _archiveGroup.setLayout(new GridLayout(1, true));
        
        _archives = new List(_archiveGroup, SWT.SINGLE | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, true, true);
        gd.widthHint = 150;
        _archives.setLayoutData(gd);
        _archives.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showArchive(); }
            public void widgetSelected(SelectionEvent selectionEvent) { showArchive(); }
        });
        
        _archiveAdd = new Button(_archiveGroup, SWT.PUSH);
        _archiveAdd.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _archiveAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _archives.setSelection(new int[0]); showArchive(""); }
            public void widgetSelected(SelectionEvent selectionEvent) { _archives.setSelection(new int[0]); showArchive(""); }
        });
        
        _archiveNameLabel = new Label(_root, SWT.NONE);
        _archiveNameLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        // we use a row for this 1-element text field so the margins/spacing/etc line up with the other 3 rows
        Composite row = new Composite(_root, SWT.NONE);
        row.setLayout(new GridLayout(1, true));
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _archiveName = new Text(row, SWT.SINGLE | SWT.BORDER);
        _archiveName.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        // url row
        _urlLabel = new Label(_root, SWT.NONE);
        _urlLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        row = new Composite(_root, SWT.NONE);
        row.setLayout(new GridLayout(3, false));
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _url = new Text(row, SWT.SINGLE | SWT.BORDER);
        _url.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _passphraseLabel = new Label(row, SWT.NONE);
        _passphraseLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _passphrase = new Text(row, SWT.SINGLE | SWT.BORDER);
        gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        gd.widthHint = 50;
        _passphrase.setLayoutData(gd);
        
        // date row
        _lastSyncLabel = new Label(_root, SWT.NONE);
        _lastSyncLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        row = new Composite(_root, SWT.NONE);
        row.setLayout(new GridLayout(4, false));
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _lastSync = new Label(row, SWT.NONE);
        gd = new GridData(GridData.BEGINNING, GridData.CENTER, false, false);
        _lastSync.setLayoutData(gd);
        
        _nextSyncLabel = new Label(row, SWT.NONE);
        _nextSyncLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _nextSync = new Label(row, SWT.NONE);
        gd = new GridData(GridData.BEGINNING, GridData.CENTER, false, false);
        _nextSync.setLayoutData(gd);
        
        _nextSyncAdjust = new Button(row, SWT.PUSH);
        _nextSyncAdjust.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _nextSyncAdjust.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _nextSyncAdjustMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _nextSyncAdjustMenu.setVisible(true); }
        });
        
        _nextSyncAdjustMenu = new Menu(_nextSyncAdjust);
        _nextSyncAdjust.setMenu(_nextSyncAdjustMenu);
        _nextSyncAdjustNow = new MenuItem(_nextSyncAdjustMenu, SWT.PUSH);
        _nextSyncAdjustNow.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { scheduleSync(System.currentTimeMillis()); }
            public void widgetSelected(SelectionEvent selectionEvent) { scheduleSync(System.currentTimeMillis()); }
        });
        _nextSyncAdjustCancel = new MenuItem(_nextSyncAdjustMenu, SWT.PUSH);
        _nextSyncAdjustCancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { scheduleSync(-1); }
            public void widgetSelected(SelectionEvent selectionEvent) { scheduleSync(-1); }
        });
        
        // proxy row
        _proxyLabel = new Label(_root, SWT.NONE);
        _proxyLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        row = new Composite(_root, SWT.NONE);
        row.setLayout(new GridLayout(7, false));
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _proxyDefault = new Button(row, SWT.RADIO);
        _proxyDefault.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _proxyDefault.setSelection(true);
        
        _proxyNone = new Button(row, SWT.RADIO);
        _proxyNone.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _proxyCustom = new Button(row, SWT.RADIO);
        _proxyCustom.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _proxyCustomHostLabel = new Label(row, SWT.NONE);
        _proxyCustomHostLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _proxyCustomHost = new Text(row, SWT.SINGLE | SWT.BORDER);
        _proxyCustomHost.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _proxyCustomPortLabel = new Label(row, SWT.NONE);
        _proxyCustomPortLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _proxyCustomPort = new Text(row, SWT.SINGLE | SWT.BORDER);
        gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        gd.widthHint = 50;
        _proxyCustomPort.setLayoutData(gd);
        
        row = new Composite(_root, SWT.NONE);
        row.setLayout(new FillLayout(SWT.HORIZONTAL));
        row.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 2, 1));
        
        _saveArchive = new Button(row, SWT.PUSH);
        _revertArchive = new Button(row, SWT.PUSH);
        _deleteArchive = new Button(row, SWT.PUSH);
        
        _eventGroup = new Group(_root, SWT.NONE);
        _eventGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 4));
        _eventGroup.setLayout(new GridLayout(1, false));
        
        _eventTable = new Table(_eventGroup, SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
        _eventTable.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _eventTable.setHeaderVisible(true);
        _eventTable.setLinesVisible(true);
        
        _colWhen = new TableColumn(_eventTable, SWT.LEFT);
        _colArchive = new TableColumn(_eventTable, SWT.LEFT);
        _colScope = new TableColumn(_eventTable, SWT.CENTER);
        _colEvent = new TableColumn(_eventTable, SWT.CENTER);
        _colDetail = new TableColumn(_eventTable, SWT.LEFT);
        
        _eventClear = new Button(_eventGroup, SWT.PUSH);
        _eventClear.setLayoutData(new GridData(GridData.BEGINNING, GridData.END, false, false));
        
        _pullPolicy = new Button(_root, SWT.PUSH);
        _pullPolicy.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _pullPolicy.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _pullMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _pullMenu.setVisible(true); }
        });
        
        _pullMenu = new Menu(_pullPolicy);
        _pullPolicy.setMenu(_pullMenu);
        
        _pullRecentDelta = new MenuItem(_pullMenu, SWT.RADIO);
        _pullAllDelta = new MenuItem(_pullMenu, SWT.RADIO);
        _pullRecentKnown = new MenuItem(_pullMenu, SWT.RADIO);
        _pullPIR = new MenuItem(_pullMenu, SWT.RADIO);
        _pullNothing = new MenuItem(_pullMenu, SWT.RADIO);
        _pullRecentDelta.setSelection(true);
        new MenuItem(_pullMenu, SWT.SEPARATOR);
        _pullPrivate = new MenuItem(_pullMenu, SWT.CHECK);
        _pullPBE = new MenuItem(_pullMenu, SWT.CHECK);
        _pullPrivate.setSelection(true);
        _pullPBE.setSelection(true);
        _pullSizePer = new MenuItem(_pullMenu, SWT.CASCADE);
        _pullSizePerMenu = new Menu(_pullSizePer);
        _pullSizePer.setMenu(_pullSizePerMenu);
        _pullSizePer32 = new MenuItem(_pullSizePerMenu, SWT.RADIO);
        _pullSizePer64 = new MenuItem(_pullSizePerMenu, SWT.RADIO);
        _pullSizePer128 = new MenuItem(_pullSizePerMenu, SWT.RADIO);
        _pullSizePer256 = new MenuItem(_pullSizePerMenu, SWT.RADIO);
        _pullSizePer512 = new MenuItem(_pullSizePerMenu, SWT.RADIO);
        _pullSizePer1024 = new MenuItem(_pullSizePerMenu, SWT.RADIO);
        _pullSizePer2048 = new MenuItem(_pullSizePerMenu, SWT.RADIO);
        _pullSizePer4096 = new MenuItem(_pullSizePerMenu, SWT.RADIO);
        _pullSizePer512.setSelection(true);
        
        _pushPolicy = new Button(_root, SWT.PUSH);
        _pushPolicy.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        _pushPolicy.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _pushMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _pushMenu.setVisible(true); }
        });
        
        _pushMenu = new Menu(_pushPolicy);
        _pushPolicy.setMenu(_pushMenu);

        _pushAllDelta = new MenuItem(_pushMenu, SWT.RADIO);
        _pushNewDelta = new MenuItem(_pushMenu, SWT.RADIO);
        _pushLocalDelta = new MenuItem(_pushMenu, SWT.RADIO);
        _pushNothing = new MenuItem(_pushMenu, SWT.RADIO);
        _pushNewDelta.setSelection(true);
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
        
        _browser.getSyndicationManager().loadArchives();
        _browser.getSyndicationManager().addListener(this);
        refreshView();
    }
    
    public void applyTheme(Theme theme) {
        _archiveGroup.setFont(theme.DEFAULT_FONT);
        _archives.setFont(theme.DEFAULT_FONT);
        _archiveAdd.setFont(theme.BUTTON_FONT);
        _archiveNameLabel.setFont(theme.DEFAULT_FONT);
        _archiveName.setFont(theme.DEFAULT_FONT);
        _urlLabel.setFont(theme.DEFAULT_FONT);
        _url.setFont(theme.DEFAULT_FONT);
        _passphraseLabel.setFont(theme.DEFAULT_FONT);
        _passphrase.setFont(theme.DEFAULT_FONT);
        _lastSyncLabel.setFont(theme.DEFAULT_FONT);
        _lastSync.setFont(theme.DEFAULT_FONT);
        _nextSyncLabel.setFont(theme.DEFAULT_FONT);
        _nextSync.setFont(theme.DEFAULT_FONT);
        _nextSyncAdjust.setFont(theme.BUTTON_FONT);
        _proxyLabel.setFont(theme.DEFAULT_FONT);
        _proxyDefault.setFont(theme.DEFAULT_FONT);
        _proxyNone.setFont(theme.DEFAULT_FONT);
        _proxyCustom.setFont(theme.DEFAULT_FONT);
        _proxyCustomHostLabel.setFont(theme.DEFAULT_FONT);
        _proxyCustomHost.setFont(theme.DEFAULT_FONT);
        _proxyCustomPortLabel.setFont(theme.DEFAULT_FONT);
        _proxyCustomPort.setFont(theme.DEFAULT_FONT);
        _saveArchive.setFont(theme.BUTTON_FONT);
        _revertArchive.setFont(theme.BUTTON_FONT);
        _deleteArchive.setFont(theme.BUTTON_FONT);
        _pullPolicy.setFont(theme.BUTTON_FONT);
        _pushPolicy.setFont(theme.BUTTON_FONT);
        _eventGroup.setFont(theme.DEFAULT_FONT);
        _eventTable.setFont(theme.TABLE_FONT);
        _eventClear.setFont(theme.BUTTON_FONT);
        
        _colWhen.pack();
        _colArchive.pack();
        _colScope.pack();
        _colEvent.pack();
        _colDetail.pack();
    }

    private static final String T_PULLRECENTDELTA = "syndie.gui.syndicationscheduler.pullrecentdelta";
    private static final String T_PULLALLDELTA = "syndie.gui.syndicationscheduler.pullalldelta";
    private static final String T_PULLRECENTKNOWN = "syndie.gui.syndicationscheduler.pullrecentknown";
    private static final String T_PULLPIR = "syndie.gui.syndicationscheduler.pullpir";
    private static final String T_PULLNOTHING = "syndie.gui.syndicationscheduler.pullnothing";
    private static final String T_PULLPRIVATE = "syndie.gui.syndicationscheduler.pullprivate";
    private static final String T_PULLPBE = "syndie.gui.syndicationscheduler.pullpbe";
    private static final String T_PULLSIZEPER = "syndie.gui.syndicationscheduler.pullsizeper";
    private static final String T_PULLSIZEPER32 = "syndie.gui.syndicationscheduler.pullsizeper32";
    private static final String T_PULLSIZEPER64 = "syndie.gui.syndicationscheduler.pullsizeper64";
    private static final String T_PULLSIZEPER128 = "syndie.gui.syndicationscheduler.pullsizeper128";
    private static final String T_PULLSIZEPER256 = "syndie.gui.syndicationscheduler.pullsizeper256";
    private static final String T_PULLSIZEPER512 = "syndie.gui.syndicationscheduler.pullsizeper512";
    private static final String T_PULLSIZEPER1024 = "syndie.gui.syndicationscheduler.pullsizeper1024";
    private static final String T_PULLSIZEPER2048 = "syndie.gui.syndicationscheduler.pullsizeper2048";
    private static final String T_PULLSIZEPER4096 = "syndie.gui.syndicationscheduler.pullsizeper4096";
    
    private static final String T_COLWHEN = "syndie.gui.syndicationscheduler.colwhen";
    private static final String T_COLARCHIVE = "syndie.gui.syndicationscheduler.colarchive";
    private static final String T_COLSCOPE = "syndie.gui.syndicationscheduler.colscope";
    private static final String T_COLEVENT = "syndie.gui.syndicationscheduler.colevent";
    private static final String T_COLDETAIL = "syndie.gui.syndicationscheduler.coldetail";
    
    private static final String T_ARCHIVES = "syndie.gui.syndicationscheduler.archives";
    private static final String T_ARCHIVEADD = "syndie.gui.syndicationscheduler.archiveadd";
    private static final String T_NAME = "syndie.gui.syndicationscheduler.name";
    private static final String T_URL = "syndie.gui.syndicationscheduler.url";
    private static final String T_PASS = "syndie.gui.syndicationscheduler.pass";
    private static final String T_LASTSYNC = "syndie.gui.syndicationscheduler.lastsync";
    private static final String T_NEXTSYNC = "syndie.gui.syndicationscheduler.nextsync";
    private static final String T_NEXTSYNCADJUST = "syndie.gui.syndicationscheduler.nextsyncadjust";
    private static final String T_NEXTSYNCNOW = "syndie.gui.syndicationscheduler.nextsyncnow";
    private static final String T_NEXTSYNCNEVER = "syndie.gui.syndicationscheduler.nextsyncnever";
    private static final String T_PROXY = "syndie.gui.syndicationscheduler.proxy";
    private static final String T_PROXYDEFAULT = "syndie.gui.syndicationscheduler.proxydefault";
    private static final String T_PROXYNONE = "syndie.gui.syndicationscheduler.proxynone";
    private static final String T_PROXYCUSTOM = "syndie.gui.syndicationscheduler.proxycustom";
    private static final String T_PROXYCUSTOMHOST = "syndie.gui.syndicationscheduler.proxycustomhost";
    private static final String T_PROXYCUSTOMPORT = "syndie.gui.syndicationscheduler.proxycustomport";
    private static final String T_SAVE = "syndie.gui.syndicationscheduler.save";
    private static final String T_REVERT = "syndie.gui.syndicationscheduler.revert";
    private static final String T_DELETE = "syndie.gui.syndicationscheduler.delete";
    private static final String T_PULL = "syndie.gui.syndicationscheduler.pull";
    private static final String T_CLEAREVENTS = "syndie.gui.syndicationscheduler.clearevents";
    private static final String T_PUSH = "syndie.gui.syndicationscheduler.push";
    private static final String T_EVENTS = "syndie.gui.syndicationscheduler.events";

    private static final String T_PUSHALLDELTA = "syndie.gui.syndicationscheduler.pushalldelta";
    private static final String T_PUSHNEWDELTA = "syndie.gui.syndicationscheduler.pushnewdelta";
    private static final String T_PUSHLOCALDELTA = "syndie.gui.syndicationscheduler.pushlocaldelta";
    private static final String T_PUSHNOTHING = "syndie.gui.syndicationscheduler.pushnothing";

    private static final String T_SYNC_NEVER = "syndie.gui.syndicationscheduler.sync.never";
    
    public void translate(TranslationRegistry registry) {
        _archiveGroup.setText(registry.getText(T_ARCHIVES, "Archives"));
        _archiveAdd.setText(registry.getText(T_ARCHIVEADD, "Add"));
        _archiveNameLabel.setText(registry.getText(T_NAME, "Name:"));
        _urlLabel.setText(registry.getText(T_URL, "URL:"));
        _passphraseLabel.setText(registry.getText(T_PASS, "Passphrase:"));
        _lastSyncLabel.setText(registry.getText(T_LASTSYNC, "Last sync:"));
        _nextSyncLabel.setText(registry.getText(T_NEXTSYNC, "Next sync:"));
        _nextSyncAdjust.setText(registry.getText(T_NEXTSYNCADJUST, "Adjust next sync..."));
        _nextSyncAdjustNow.setText(registry.getText(T_NEXTSYNCNOW, "Schedule for sync ASAP"));
        _nextSyncAdjustCancel.setText(registry.getText(T_NEXTSYNCNEVER, "Cancel sync until further notice"));
        _proxyLabel.setText(registry.getText(T_PROXY, "Proxy:"));
        _proxyDefault.setText(registry.getText(T_PROXYDEFAULT, "Default"));
        _proxyNone.setText(registry.getText(T_PROXYNONE, "None"));
        _proxyCustom.setText(registry.getText(T_PROXYCUSTOM, "Custom:"));
        _proxyCustomHostLabel.setText(registry.getText(T_PROXYCUSTOMHOST, "Host:"));
        _proxyCustomPortLabel.setText(registry.getText(T_PROXYCUSTOMPORT, "Port:"));
        _saveArchive.setText(registry.getText(T_SAVE, "Save"));
        _revertArchive.setText(registry.getText(T_REVERT, "Revert"));
        _deleteArchive.setText(registry.getText(T_DELETE, "Delete"));
        _pullPolicy.setText(registry.getText(T_PULL, "Pull policy..."));
        _eventClear.setText(registry.getText(T_CLEAREVENTS, "Clear event log"));
        _pushPolicy.setText(registry.getText(T_PUSH, "Push policy..."));
        _eventGroup.setText(registry.getText(T_EVENTS, "Event log:"));
        
        _colWhen.setText(registry.getText(T_COLWHEN, "When"));
        _colArchive.setText(registry.getText(T_COLARCHIVE, "Archive"));
        _colScope.setText(registry.getText(T_COLSCOPE, "Scope"));
        _colEvent.setText(registry.getText(T_COLEVENT, "Event"));
        _colDetail.setText(registry.getText(T_COLDETAIL, "Detail"));
                
        _pullRecentDelta.setText(registry.getText(T_PULLRECENTDELTA, "Recent messages we don't have"));
        _pullAllDelta.setText(registry.getText(T_PULLALLDELTA, "All messages we don't have"));
        _pullRecentKnown.setText(registry.getText(T_PULLRECENTKNOWN, "Recent messages in forums we know"));
        _pullPIR.setText(registry.getText(T_PULLPIR, "Everything the archive considers 'new' (PIR)"));
        _pullNothing.setText(registry.getText(T_PULLNOTHING, "Nothing"));
        _pullPrivate.setText(registry.getText(T_PULLPRIVATE, "Include private messages"));
        _pullPBE.setText(registry.getText(T_PULLPBE, "Include passphrase protected messages"));
        _pullSizePer.setText(registry.getText(T_PULLSIZEPER, "Maximum message size:"));
        _pullSizePer32.setText(registry.getText(T_PULLSIZEPER32, "32KB"));
        _pullSizePer64.setText(registry.getText(T_PULLSIZEPER64, "64KB"));
        _pullSizePer128.setText(registry.getText(T_PULLSIZEPER128, "128KB"));
        _pullSizePer256.setText(registry.getText(T_PULLSIZEPER256, "256KB"));
        _pullSizePer512.setText(registry.getText(T_PULLSIZEPER512, "512KB"));
        _pullSizePer1024.setText(registry.getText(T_PULLSIZEPER1024, "1024KB"));
        _pullSizePer2048.setText(registry.getText(T_PULLSIZEPER2048, "2048KB"));
        _pullSizePer4096.setText(registry.getText(T_PULLSIZEPER4096, "4096KB"));
        
        _pushAllDelta.setText(registry.getText(T_PUSHALLDELTA, "Send all differences"));
        _pushNewDelta.setText(registry.getText(T_PUSHNEWDELTA, "Send new differences"));
        _pushLocalDelta.setText(registry.getText(T_PUSHLOCALDELTA, "Send locally generated differences only"));
        _pushNothing.setText(registry.getText(T_PUSHNOTHING, "Send nothing"));
    }

    public void archiveAdded(SyndicationManager mgr, String name) {
        Display.getDefault().asyncExec(new Runnable() { public void run() { refreshView(); } });
    }
    public void archiveRemoved(SyndicationManager mgr, String name) {   
        Display.getDefault().asyncExec(new Runnable() { public void run() { refreshView(); } });
    }
    public void archiveUpdated(SyndicationManager mgr, String oldName, String newName) {
        Display.getDefault().asyncExec(new Runnable() { public void run() { refreshView(); } });
    }
    public void archivesLoaded(SyndicationManager mgr) {
        Display.getDefault().asyncExec(new Runnable() { public void run() { refreshView(); } });
    }
    public void archiveIndexStatus(SyndicationManager mgr, SyndicationManager.StatusRecord record) {}
    public void fetchStatusUpdated(SyndicationManager mgr, SyndicationManager.StatusRecord record) {}
    public void syndicationComplete(SyndicationManager mgr) {}
}
