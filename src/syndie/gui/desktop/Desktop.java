package syndie.gui.desktop;

import java.io.File;
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
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
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
    private DesktopEdge _edgeWestDefault;
    
    private Composite _centerDefault;
    
    private StackLayout _centerStack;
    private StackLayout _edgeNorthStack;
    private StackLayout _edgeEastStack;
    private StackLayout _edgeSouthStack;
    private StackLayout _edgeWestStack;
    
    private StartupPanel _startupPanel;
    
    private DBClient _client;
    //private TranslationRegistry _translation;
    //private ThemeRegistry _themes;
    
    public Desktop(File rootFile, DesktopUI ui, Display display, Timer timer) {
        _rootFile = rootFile;
        _ui = ui;
        _display = display;
        initComponents(timer);
    }
    
    private boolean TRIM = true;
    
    private void initComponents(Timer timer) {
        if (TRIM)
            _shell = new Shell(_display, SWT.SHELL_TRIM);
        else
            _shell = new Shell(_display, SWT.NO_TRIM);
        prepareGrid();
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) {
                evt.doit = false;
                close();
            }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });

        _startupPanel = new StartupPanel(_center, _ui, timer);
        show(_startupPanel);
        
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
    
    void show(DesktopPanel panel) {
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
        panel.shown(this);
    }
    private void setEdge(Composite edge, StackLayout stack, DesktopEdge specificEdge, DesktopEdge defEdge) {
        if (specificEdge != null)
            stack.topControl = specificEdge.getRoot();
        else
            stack.topControl = defEdge.getRoot();
        edge.layout();
    }
    
    void showDesktopTabs() {
        TabPanel panel = new TabPanel(_center, this);
        show(panel);
    }
    
    void exit() { close(); }
    
    File getRootFile() { return _rootFile; }
    void setDBClient(DBClient client) { _client = client; }
    DBClient getDBClient() { return _client; }
    UI getUI() { return _ui; }
    
    StartupPanel getStartupPanel() { return _startupPanel; }
    //void setThemeRegistry(ThemeRegistry registry) { _themes = registry; }
    //void setTranslationRegistry(TranslationRegistry registry) { _translations = registry; }
    
    private void close() {
        _shell.setVisible(false);
        new Thread(new Runnable() { public void run() { System.exit(0); } }).start();
    }
    
    private void prepareGrid() {
        GridLayout gl = new GridLayout(3, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;
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
        _edgeWestDefault = new DesktopEdgeDummy(SWT.COLOR_GREEN, _edgeWest, _ui);
        
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
}
