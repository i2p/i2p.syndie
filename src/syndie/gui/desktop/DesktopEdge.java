package syndie.gui.desktop;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import syndie.db.UI;
import syndie.gui.*;

/**
 *
 */
class DesktopEdge {
    protected UI _ui;
    private Composite _parent;
    private Composite _root;
    
    public DesktopEdge(Composite parent, UI ui) {
        _parent = parent;
        _ui = ui;
        initComponents();
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new FillLayout());
    }
    
    protected Composite getRoot() { return _root; }
    
    public void dispose() { getRoot().dispose(); }
}
