package syndie.gui;

import java.util.HashMap;
import java.util.Map;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

class DesktopTabPanel extends DesktopPanel {
    private Desktop _desktop;
    private CTabFolder _tabs;
    private Menu _tabMenu;
    private MenuItem _closeAllTabs;
    private MenuItem _closeOtherTabs;
    private MenuItem _copyTabLocation;
    private MenuItem _bookmarkTab;
    
    /** uri to BrowserTab */
    private Map _openTabs;
    /** CTabItem to uri */
    private Map _openTabURIs;
    
    public DesktopTabPanel(Composite parent, Desktop desktop) {
        super(parent, desktop.getUI());
        _desktop = desktop;
        _openTabs = new HashMap();
        _openTabURIs = new HashMap();
        initComponents();
    }
    
    private void initComponents() {
        Composite root = getRoot();
        _tabs = new CTabFolder(root, SWT.MULTI | SWT.TOP | SWT.CLOSE | SWT.BORDER);
        _tabs.setSimple(true);
        _tabs.setMinimizeVisible(false);
        _tabs.setMinimumCharacters(20);
        _tabs.setUnselectedImageVisible(true);
        _tabs.setBorderVisible(true);
        _tabs.marginHeight = 0;
        _tabs.marginWidth = 0;
        
        _tabMenu = new Menu(_tabs);
        _tabs.setMenu(_tabMenu);
        
        _tabs.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                CTabItem item = _tabs.getSelection();
                Object uri = _openTabURIs.get(item);
                if (uri == null) return;
                BrowserTab tab = (BrowserTab)_openTabs.get(uri);
                if (tab == null) return;
                tab.tabShown();
            }
        });
        
        _closeAllTabs = new MenuItem(_tabMenu, SWT.PUSH);
        _closeAllTabs.addSelectionListener(new FireSelectionListener() {
            public void fire() { closeAllTabs(); }
        });
        _closeOtherTabs = new MenuItem(_tabMenu, SWT.PUSH);
        _closeOtherTabs.addSelectionListener(new FireSelectionListener() {
            public void fire() { closeOtherTabs(); }
        });
        
        new MenuItem(_tabMenu, SWT.SEPARATOR);
        
        _copyTabLocation = new MenuItem(_tabMenu, SWT.PUSH);
        _copyTabLocation.addSelectionListener(new FireSelectionListener() {
            public void fire() { copyTabLocation(); }
        });
        _bookmarkTab = new MenuItem(_tabMenu, SWT.PUSH);
        _bookmarkTab.addSelectionListener(new FireSelectionListener() {
            public void fire() { bookmarkTab(); }
        });
    }
    
    private void closeAllTabs() {}
    private void closeOtherTabs() {}
    private void copyTabLocation() {}
    private void bookmarkTab() {}
    
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
