package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import syndie.db.UI;

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
   
    protected DesktopCorner _cornerNW;
    protected DesktopCorner _cornerNE;
    protected DesktopCorner _cornerSE;
    protected DesktopCorner _cornerSW;
    
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
    
    void shown(Desktop desktop) {}
    
    void buildNorth(Composite edge) { 
        if (edge != null) _edgeNorth = new DesktopEdgeDummy(SWT.COLOR_GREEN, edge, _ui);
    }
    void buildEast(Composite edge) { 
        if (edge != null) _edgeEast = new DesktopEdgeDummy(SWT.COLOR_GREEN, edge, _ui);
    }
    void buildSouth(Composite edge) { 
        if (edge != null) _edgeSouth = new DesktopEdgeDummy(SWT.COLOR_GREEN, edge, _ui);
    }
    void buildWest(Composite edge) { 
        if (edge != null) _edgeWest = new DesktopEdgeDummy(SWT.COLOR_GREEN, edge, _ui);
    }
    
    void buildNorthWest(Composite corner) { 
        if (corner != null) _cornerNW = new DesktopCornerDummy(SWT.COLOR_GREEN, corner, _ui);
    }
    void buildNorthEast(Composite corner) { 
        if (corner != null) _cornerNE = new DesktopCornerDummy(SWT.COLOR_GREEN, corner, _ui);
    }
    void buildSouthEast(Composite corner) { 
        if (corner != null) _cornerSE = new DesktopCornerDummy(SWT.COLOR_GREEN, corner, _ui);
    }
    void buildSouthWest(Composite corner) { 
        if (corner != null) _cornerSW = new DesktopCornerDummy(SWT.COLOR_GREEN, corner, _ui);
    }
    
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
    void disposeCorners() {
        if (_cornerNW != null)
            _cornerNW.dispose();
        if (_cornerNE != null)
            _cornerNE.dispose();
        if (_cornerSE != null)
            _cornerSE.dispose();
        if (_cornerSW != null)
            _cornerSW.dispose();
    }
    
    DesktopEdge getEdgeNorth() { return _edgeNorth; }
    DesktopEdge getEdgeEast() { return _edgeEast; }
    DesktopEdge getEdgeSouth() { return _edgeSouth; }
    DesktopEdge getEdgeWest() { return _edgeWest; }
    
    DesktopCorner getCornerNW() { return _cornerNW; }
    DesktopCorner getCornerNE() { return _cornerNE; }
    DesktopCorner getCornerSE() { return _cornerSE; }
    DesktopCorner getCornerSW() { return _cornerSW; }
}

class DesktopEdgeDummy extends DesktopEdge {
    public DesktopEdgeDummy(int sysColor, Composite parent, UI ui) {
        super(parent, ui);
        Canvas c = new Canvas(getRoot(), SWT.NONE);
        c.setBackground(getRoot().getDisplay().getSystemColor(sysColor));
    }
}

class DesktopCornerDummy extends DesktopCorner {
    public DesktopCornerDummy(int sysColor, Composite parent, UI ui) {
        super(parent, ui);
        Canvas c = new Canvas(getRoot(), SWT.NONE);
        c.setBackground(getRoot().getDisplay().getSystemColor(sysColor));
    }
}
