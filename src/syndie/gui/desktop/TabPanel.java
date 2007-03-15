package syndie.gui.desktop;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
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
        _browser.addUI(_desktop.getUI());
        Timer t = new Timer("tab panel startup", _desktop.getUI());
        _browser.startup(t);
        t.addEvent("done with the nested browser startup");
        t.complete();
    }
}
