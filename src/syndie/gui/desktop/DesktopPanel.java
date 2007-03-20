package syndie.gui.desktop;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;
import syndie.gui.*;

/**
 *
 */
class DesktopPanel {
    protected DBClient _client;
    protected ThemeRegistry _themeRegistry;
    protected TranslationRegistry _translationRegistry;
    protected UI _ui;
    private Composite _parent;
    private Composite _root;
    private SyndieURI _origURI;
    protected Desktop _desktop;
    
    protected DesktopEdge _edgeNorth;
    protected DesktopEdge _edgeEast;
    protected DesktopEdge _edgeSouth;
    protected DesktopEdge _edgeWest;
    
    public DesktopPanel(Desktop desktop, Composite parent, UI ui, SyndieURI origURI) {
        this(desktop, null, null, null, parent, ui, origURI);
    }
    public DesktopPanel(Desktop desktop, DBClient client, ThemeRegistry themes, TranslationRegistry trans, Composite parent, UI ui, SyndieURI origURI) {
        _desktop = desktop;
        _parent = parent;
        _ui = ui;
        _client = client;
        _themeRegistry = themes;
        _translationRegistry = trans;
        _origURI = origURI;
        initComponents();
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new FillLayout());
    }
    
    public SyndieURI getOriginalURI() { return _origURI; }
    public boolean canShow(SyndieURI uri) { return _origURI != null && _origURI.equals(uri); }
    public void close() { 
        dispose();
        _desktop.panelDisposed(this, true); 
    }
    protected void dispose() { _root.dispose(); }
    
    protected Composite getRoot() { return _root; }
    
    public String getPanelName() { return "desktop panel"; }
    public String getPanelDescription() { return "default desktop panel description for " + getClass().getName(); }
    
    /** callback after the panel has been completely shown on the desktop */
    void shown(Desktop desktop, SyndieURI uri, String suggestedName, String suggestedDescription) {
        getRoot().setVisible(true);
        getRoot().setEnabled(true);
    }
    void hidden() {
        getRoot().setVisible(false);
        getRoot().setEnabled(false);
    }
    
    /** instruct the panel to build its northern edge (or it can do nothing if it doesn't have one) */
    void buildNorth(Composite edge) {}
    /** instruct the panel to build its eastern edge (or it can do nothing if it doesn't have one) */
    void buildEast(Composite edge) {}
    /** instruct the panel to build its southern edge (or it can do nothing if it doesn't have one) */
    void buildSouth(Composite edge) {}
    /** instruct the panel to build its western edge (or it can do nothing if it doesn't have one) */
    void buildWest(Composite edge) {}
    
    void disposeEdges() {
        if (_edgeNorth != null)
            _edgeNorth.dispose();
        if (_edgeEast != null)
            _edgeEast.dispose();
        if (_edgeSouth != null)
            _edgeSouth.dispose();
        if (_edgeWest != null)
            _edgeWest.dispose();
    }

    /**
     * fetch the built edge (or null if the panel doesn't have any panel-specific edge, in which 
     * case the default edge will be used)
     */
    DesktopEdge getEdgeNorth() { return _edgeNorth; }
    /**
     * fetch the built edge (or null if the panel doesn't have any panel-specific edge, in which 
     * case the default edge will be used)
     */
    DesktopEdge getEdgeEast() { return _edgeEast; }
    /**
     * fetch the built edge (or null if the panel doesn't have any panel-specific edge, in which 
     * case the default edge will be used)
     */
    DesktopEdge getEdgeSouth() { return _edgeSouth; }
    /**
     * fetch the built edge (or null if the panel doesn't have any panel-specific edge, in which 
     * case the default edge will be used)
     */
    DesktopEdge getEdgeWest() { return _edgeWest; }
}
