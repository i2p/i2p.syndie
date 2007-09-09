package syndie.gui.desktop;

import net.i2p.data.Hash;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;
import syndie.gui.ArchiveManager;
import syndie.gui.NavigationControl;
import syndie.gui.ThemeRegistry;
import syndie.gui.TranslationRegistry;

public class ArchiveManagerPanel extends DesktopPanel {
    private ArchiveManager _manager;
    private NavigationControl _navControl;
    
    public ArchiveManagerPanel(Desktop desktop, DBClient client, ThemeRegistry themes, TranslationRegistry trans, Composite parent, UI ui, SyndieURI origURI, NavigationControl nav) {
        super(desktop, client, themes, trans, parent, ui, origURI);
        _navControl = nav;
        initComponents();
    }
    
    private void initComponents() {
        Composite root = getRoot();
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        root.setLayout(gl);

        _manager = new ArchiveManager(_client, _ui, _themeRegistry, _translationRegistry, root, _navControl);
        _manager.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
    }
    
    protected void dispose() { _manager.dispose(); }
    
    public String getPanelName() { return _translationRegistry.getText(T_NAME, "Archive manager"); }
    public String getPanelDescription() { return _translationRegistry.getText(T_DESC, "General management of the local archive"); }
    private static final String T_NAME = "syndie.gui.desktop.archivemanagerpanel.name";
    private static final String T_DESC = "syndie.gui.desktop.archivemanagerpanel.desc";
}
