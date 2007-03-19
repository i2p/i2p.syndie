package syndie.gui.desktop;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import syndie.data.SyndieURI;
import syndie.data.Timer;
import syndie.gui.*;

class TabPanel extends DesktopPanel {
    private Composite _browserBase;
    private Browser _browser;
    
    public TabPanel(Composite parent, Desktop desktop) {
        super(desktop, parent, desktop.getUI(), null);
        initComponents();
    }
    
    private void initComponents() {
        Composite root = getRoot();
        _browserBase = new Composite(root, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.verticalSpacing = 0;
        _browserBase.setLayout(gl);
    }
    
    public void shown(Desktop desktop, SyndieURI uri) {
        super.shown(desktop, uri);
        if (_browser == null) {
            _browser = new Browser(_desktop.getDBClient(), getRoot().getShell(), _browserBase, _desktop.getNavControl());
            _browser.addUI(_desktop.getUI());
            Timer t = new Timer("tab panel startup", _desktop.getUI());
            _browser.startup(t);
            t.addEvent("done with the nested browser startup");
            t.complete();
        }
        if (uri != null)
            _browser.view(uri);
    }
    
    public void unview(SyndieURI uri) { _browser.unview(uri); }
    
    public String getPanelName() { return "tabs"; }
    public String getPanelDescription() { return "Old style tabbed interface"; }
}
