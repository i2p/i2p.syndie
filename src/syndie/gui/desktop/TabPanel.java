package syndie.gui.desktop;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import syndie.data.Timer;
import syndie.gui.*;

class TabPanel extends DesktopPanel {
    private Desktop _desktop;
    private Composite _browserBase;
    private Browser _browser;
    
    public TabPanel(Composite parent, Desktop desktop) {
        super(parent, desktop.getUI());
        _desktop = desktop;
        initComponents();
    }
    
    private void initComponents() {
        Composite root = getRoot();
        _browserBase = new Composite(root, SWT.NONE);
        _browserBase.setLayout(new GridLayout(1, true));
        _browser = new Browser(_desktop.getDBClient(), root.getShell(), _browserBase);
        _browser.startup(new Timer("tab panel startup", _desktop.getUI()));
    }
    
    void buildSouth(Composite edge) {
        Control children[] = edge.getChildren();
        for (int i = 0; i < children.length; i++) children[i].dispose();
        _edgeSouth = new ShowStartupDesktopEdge(edge, _desktop);
        edge.layout(true, true);
    }
}

class ShowStartupDesktopEdge extends DesktopEdge {
    public ShowStartupDesktopEdge(Composite parent, final Desktop desktop) {
        super(parent, desktop.getUI());
        Button b = new Button(getRoot(), SWT.PUSH);
        b.setBackground(b.getDisplay().getSystemColor(SWT.COLOR_BLUE));
        b.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                desktop.show(desktop.getStartupPanel());
            }
        });
    }
}
