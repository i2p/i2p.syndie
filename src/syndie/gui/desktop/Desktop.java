package syndie.gui.desktop;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
import syndie.data.SyndieURI;
import syndie.data.Timer;
import syndie.db.DBClient;
import syndie.db.UI;
import syndie.gui.*;

/**
 *
 */
class Desktop {
    private File _rootFile;
    private DesktopUI _ui;
    private Display _display;
    private Shell _shell;
    private List _listeners;
    private Composite _edgeNorthWest;
    private Composite _edgeNorthEast;
    private Composite _edgeSouthEast;
    private Composite _edgeSouthWest;

    private Composite _edgeNorth;
    private Composite _edgeEast;
    private Composite _edgeSouth;
    private Composite _edgeWest;
    private Composite _center;

    private DesktopEdge _edgeNorthDefault;
    private DesktopEdge _edgeEastDefault;
    private DesktopEdge _edgeSouthDefault;
    private LinkEdge _edgeWestDefault;
    
    private Composite _centerDefault;
    
    private StackLayout _centerStack;
    private StackLayout _edgeNorthStack;
    private StackLayout _edgeEastStack;
    private StackLayout _edgeSouthStack;
    private StackLayout _edgeWestStack;
    
    private StartupPanel _startupPanel;
    private ForumSelectionPanel _forumSelectionPanel;
    private TabPanel _tabs;
    
    private DBClient _client;
    private TranslationRegistry _translationRegistry;
    private ThemeRegistry _themeRegistry;
    
    private List _loadedPanels;
    private int _curPanelIndex;
    
    private NavigationControl _navControl;
    
    public Desktop(File rootFile, DesktopUI ui, Display display, Timer timer) {
        _rootFile = rootFile;
        _ui = ui;
        _display = display;
        _listeners = new ArrayList();
        _loadedPanels = new ArrayList();
        _curPanelIndex = -1;
        _navControl = new DesktopNavigationControl(this);
        initComponents(timer);
    }
    
    public interface DesktopListener {
        public void panelShown(DesktopPanel panel);
        public void panelHidden(DesktopPanel panel);
        public void destroyed(DesktopPanel panel);
    }
    public void addListener(DesktopListener lsnr) { synchronized (_listeners) { _listeners.add(lsnr); } }
    public void removeListener(DesktopListener lsnr) { synchronized (_listeners) { _listeners.add(lsnr); } }
    
    private boolean TRIM = false;
    
    private void initComponents(Timer timer) {
        timer.addEvent("init desktop components begin");
        prepareShell(timer);
        timer.addEvent("init desktop components: shell prepared");
        _startupPanel = new StartupPanel(this, _center, _ui, timer);
        show(_startupPanel, null, null, null);
        timer.addEvent("init desktop components: startup panel shown");
        
        initKeyFilters();
        timer.addEvent("init desktop components: key filters installed");
        
        show();
        timer.addEvent("init desktop components: desktop shown");
    }
    private void prepareShell(Timer timer) {
        if (TRIM)
            _shell = new Shell(_display, SWT.SHELL_TRIM);
        else
            _shell = new Shell(_display, SWT.NO_TRIM);
        timer.addEvent("shell created");
        prepareGrid();
        timer.addEvent("grid prepared");
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {
                _center.forceFocus(); // when the key filters are triggered, swt seems to lose track of the focus
            }
            public void shellClosed(ShellEvent evt) {
                evt.doit = false;
                close();
            }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
    }
    
    private void show() {
        Monitor mon[] = _display.getMonitors();
        Rectangle rect = null;
        if ( (mon != null) && (mon.length > 1) )
            rect = mon[0].getClientArea();
        else
            rect = _display.getClientArea();
        _shell.setSize(rect.width, rect.height);
        _shell.setMaximized(true);
        _shell.open();
        _shell.forceActive();
        _shell.forceFocus();
    }
    
    private void initKeyFilters() {
        _display.addFilter(SWT.KeyDown, new Listener() {
            public void handleEvent(Event evt) {
                if (evt.character == SWT.ESC) {
                    DesktopPanel panel = getCurrentPanel();
                    if (panel instanceof ForumSelectionPanel) {
                        ((ForumSelectionPanel)panel).forumSelectorCancelled();
                        evt.type = SWT.None;
                    }
                } else if ( (evt.character == 'l') && ((evt.stateMask & SWT.MOD3) == SWT.MOD3) ) { // ALT+L to show/hide forum selector
                    DesktopPanel panel = getCurrentPanel();
                    if (panel instanceof ForumSelectionPanel)
                        ((ForumSelectionPanel)panel).forumSelectorCancelled();
                    else
                        showForumSelectionPanel();
                    evt.type = SWT.None;
                } else if ( (evt.keyCode == SWT.ARROW_DOWN) && ((evt.stateMask & SWT.MOD3) != 0) ) { // ALT-down
                    evt.type = SWT.NONE;
                    showNextPanel();
                } else if ( (evt.keyCode == SWT.ARROW_UP) && ((evt.stateMask & SWT.MOD3) != 0) ) { // ALT-up
                    evt.type = SWT.NONE;
                    showPreviousPanel();
                } else if ( (evt.character == '=') && ((evt.stateMask & SWT.MOD1) != 0) ) { // ^= (aka ^+)
                    evt.type = SWT.NONE;
                    _themeRegistry.increaseFont();
                } else if ( (evt.character == '-') && ((evt.stateMask & SWT.MOD1) != 0) ) { // ^-
                    evt.type = SWT.NONE;
                    _themeRegistry.decreaseFont();
                }
            }
        });
    }
    
    void show(DesktopPanel panel, SyndieURI uri, String name, String desc) { show(panel, uri, name, desc, true); }
    void show(DesktopPanel panel, SyndieURI uri, String name, String desc, boolean notifyPrev) {
        panel.buildNorth(_edgeNorth);
        panel.buildEast(_edgeEast);
        panel.buildSouth(_edgeSouth);
        panel.buildWest(_edgeWest);
        _centerStack.topControl = panel.getRoot();
        _center.layout();
        setEdge(_edgeNorth, _edgeNorthStack, panel.getEdgeNorth(), _edgeNorthDefault);
        setEdge(_edgeEast, _edgeEastStack, panel.getEdgeEast(), _edgeEastDefault);
        setEdge(_edgeSouth, _edgeSouthStack, panel.getEdgeSouth(), _edgeSouthDefault);
        setEdge(_edgeWest, _edgeWestStack, panel.getEdgeWest(), _edgeWestDefault);
        panel.shown(this, uri, name, desc);
        if (notifyPrev) {
            if (_curPanelIndex >= 0) {
                DesktopPanel prev = (DesktopPanel)_loadedPanels.get(_curPanelIndex);
                if (prev != panel) {
                    prev.hidden();
                    synchronized (_listeners) {
                        for (int i = 0; i < _listeners.size(); i++)
                            ((DesktopListener)_listeners.get(i)).panelHidden(prev);
                    }
                }
            }
        }
        int idx = _loadedPanels.indexOf(panel);
        if (idx >= 0) {
            _curPanelIndex = idx;
        } else {
            _loadedPanels.add(panel);
            _curPanelIndex = _loadedPanels.size()-1;
        }
        synchronized (_listeners) {
            for (int i = 0; i < _listeners.size(); i++)
                ((DesktopListener)_listeners.get(i)).panelShown(panel);
        }
    }
    
    DesktopPanel getCurrentPanel() { return _curPanelIndex >= 0 ? (DesktopPanel)_loadedPanels.get(_curPanelIndex) : null; }
    List getPanels() { return new ArrayList(_loadedPanels); }
    NavigationControl getNavControl() { return _navControl; }
    Composite getCenter() { return _center; }
    
    boolean isShowing(DesktopPanel panel) { return getCurrentPanel() == panel; }
    
    void showPreviousPanel() { showPreviousPanel(true); }
    void showPreviousPanel(boolean notifyPrev) {
        if (_loadedPanels.size() > 0) {
            int idx = _curPanelIndex - 1;
            if (idx < 0) idx = _loadedPanels.size()-1;
            DesktopPanel panel = (DesktopPanel)_loadedPanels.get(idx);
            show(panel, null, null, null, notifyPrev);
        }
    }
    void showNextPanel() {
        if (_loadedPanels.size() > 0) {
            int idx = _curPanelIndex + 1;
            if (idx >= _loadedPanels.size()) idx = 0;
            DesktopPanel panel = (DesktopPanel)_loadedPanels.get(idx);
            show(panel, null, null, null);
        }
    }
    void panelDisposed(DesktopPanel panel, boolean showAnother) {
        int idx = _loadedPanels.indexOf(panel);
        if (idx >= 0) {
            _loadedPanels.remove(idx);
            synchronized (_listeners) {
                for (int i = 0; i < _listeners.size(); i++)
                    ((DesktopListener)_listeners.get(i)).destroyed(panel);
            }
            if (_curPanelIndex == idx) {
                if (showAnother)
                    showPreviousPanel(false);
            } else if (_curPanelIndex > idx)
                _curPanelIndex--;
        }
    }
    
    private void setEdge(Composite edge, StackLayout stack, DesktopEdge specificEdge, DesktopEdge defEdge) {
        if (specificEdge != null)
            stack.topControl = specificEdge.getEdgeRoot();
        else
            stack.topControl = defEdge.getEdgeRoot();
        edge.layout();
    }
    
    void startupComplete(boolean ok) {
        if (ok)
            _display.asyncExec(new Runnable() { public void run() { showForumSelectionPanel(); } });
        //if (ok)
        //    _display.asyncExec(new Runnable() { public void run() { showDesktopTabs(); } });
    }

    TabPanel getTabPanel(boolean create) {
        if ((_tabs == null) && create)
            _tabs = new TabPanel(_center, this);
        return _tabs;
    }
    void showDesktopTabs() { show(getTabPanel(true), null, null, null); }
    
    void exit() { close(); }
    
    File getRootFile() { return _rootFile; }
    void setDBClient(DBClient client) { _client = client; }
    DBClient getDBClient() { return _client; }
    UI getUI() { return _ui; }
    ThemeRegistry getThemeRegistry() { return _themeRegistry; }
    TranslationRegistry getTranslationRegistry() { return _translationRegistry; }
    
    void setTranslationRegistry(TranslationRegistry trans) { _translationRegistry = trans; }
    void setThemeRegistry(ThemeRegistry themes) { _themeRegistry = themes; }
    
    StartupPanel getStartupPanel() { return _startupPanel; }
    //void setThemeRegistry(ThemeRegistry registry) { _themes = registry; }
    //void setTranslationRegistry(TranslationRegistry registry) { _translations = registry; }
    
    private void close() {
        _shell.setVisible(false);
        new Thread(new Runnable() { public void run() { System.exit(0); } }).start();
    }
    
    private void prepareGrid() {
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 0;
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        gl.verticalSpacing = 0;
        _shell.setLayout(gl);
        
        _edgeNorthWest = new Composite(_shell, SWT.NONE);
        _edgeNorth = new Composite(_shell, SWT.NONE);
        _edgeNorthEast = new Composite(_shell, SWT.NONE);
        _edgeWest = new Composite(_shell, SWT.NONE);
        _center = new Composite(_shell, SWT.NONE);
        _edgeEast = new Composite(_shell, SWT.NONE);
        _edgeSouthWest = new Composite(_shell, SWT.NONE);
        _edgeSouth = new Composite(_shell, SWT.NONE);
        _edgeSouthEast = new Composite(_shell, SWT.NONE);
        
        _edgeNorthWest.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _edgeNorth.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _edgeNorthEast.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _edgeWest.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, true));
        _center.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _edgeEast.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, true));
        _edgeSouthWest.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _edgeSouth.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _edgeSouthEast.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _edgeNorthWest.setLayout(new FillLayout());
        _edgeNorthEast.setLayout(new FillLayout());
        _edgeSouthWest.setLayout(new FillLayout());
        _edgeSouthEast.setLayout(new FillLayout());
        
        setSize(_edgeNorthWest, BORDER_SIZE, BORDER_SIZE);
        setSize(_edgeNorthEast, BORDER_SIZE, BORDER_SIZE);
        setSize(_edgeSouthWest, BORDER_SIZE, BORDER_SIZE);
        setSize(_edgeSouthEast, BORDER_SIZE, BORDER_SIZE);
        
        setSize(_edgeNorth, -1, BORDER_SIZE);
        setSize(_edgeSouth, -1, BORDER_SIZE);
        setSize(_edgeEast, BORDER_SIZE, -1);
        setSize(_edgeWest, BORDER_SIZE, -1);
        
        new DesktopCornerDummy(SWT.COLOR_YELLOW, _edgeNorthWest, _ui);
        new DesktopCornerDummy(SWT.COLOR_BLUE, _edgeNorthEast, _ui);
        new DesktopCornerDummy(SWT.COLOR_MAGENTA, _edgeSouthEast, _ui);
        new DesktopCornerDummy(SWT.COLOR_RED, _edgeSouthWest, _ui);
        
        _edgeNorthDefault = new DesktopEdgeDummy(SWT.COLOR_GREEN, _edgeNorth, _ui);
        _edgeEastDefault = new DesktopEdgeDummy(SWT.COLOR_GREEN, _edgeEast, _ui);
        _edgeSouthDefault = new DesktopEdgeDummy(SWT.COLOR_GREEN, _edgeSouth, _ui);
        _edgeWestDefault = new LinkEdge(_edgeWest, _ui, this); //new DesktopEdgeDummy(SWT.COLOR_GREEN, _edgeWest, _ui);
        
        _centerStack = new StackLayout();
        _center.setLayout(_centerStack);

        _edgeNorthStack = new StackLayout();
        _edgeNorth.setLayout(_edgeNorthStack);
        _edgeEastStack = new StackLayout();
        _edgeEast.setLayout(_edgeEastStack);
        _edgeSouthStack = new StackLayout();
        _edgeSouth.setLayout(_edgeSouthStack);
        _edgeWestStack = new StackLayout();
        _edgeWest.setLayout(_edgeWestStack);
    }
    
    private static final int BORDER_SIZE = 64;
    
    private void setSize(Composite c, int width, int height) {
        GridData gd = (GridData)c.getLayoutData();
        gd.heightHint = height;
        gd.widthHint = width;
    }

    public void showForumSelectionPanel() { 
        if (_forumSelectionPanel == null)
            _forumSelectionPanel = new ForumSelectionPanel(this, _client, _themeRegistry, _translationRegistry, _center, _ui, _navControl);
        show(_forumSelectionPanel, null, null, null);
    }
}
