package syndie.gui.desktop;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import syndie.db.UI;
import syndie.gui.*;

/**
 *
 */
class DesktopPanel {
    private UI _ui;
    private Composite _parent;
    private Composite _root;
    
    protected DesktopEdge _edgeNorth;
    protected DesktopEdge _edgeEast;
    protected DesktopEdge _edgeSouth;
    protected DesktopEdge _edgeWest;
   
    public DesktopPanel(Composite parent, UI ui) {
        _parent = parent;
        _ui = ui;
        initComponents();
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new FillLayout());
    }
    
    protected Composite getRoot() { return _root; }
    protected UI getUI() { return _ui; }
    
    /** callback after the panel has been completely shown on the desktop */
    void shown(Desktop desktop) {}
    
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
