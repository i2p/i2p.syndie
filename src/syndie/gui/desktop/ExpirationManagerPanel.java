package syndie.gui.desktop;

import net.i2p.data.Hash;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;
import syndie.gui.ExpirationManager;
import syndie.gui.ThemeRegistry;
import syndie.gui.TranslationRegistry;

class ExpirationManagerPanel extends DesktopPanel {
    private ExpirationManager _manager;
    
    public ExpirationManagerPanel(Desktop desktop, DBClient client, ThemeRegistry themes, TranslationRegistry trans, Composite parent, UI ui, SyndieURI origURI) {
        super(desktop, client, themes, trans, parent, ui, origURI);
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

        _manager = new ExpirationManager(_client, _ui, _themeRegistry, _translationRegistry, root);
        _manager.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
    }
    
    public void shown(Desktop desktop, SyndieURI uri, String name, String desc) {
        super.shown(desktop, uri, name, desc);
        _ui.debugMessage("expiration manager started with uri: " + uri);
        if (uri != null) {
            Hash scope = uri.getHash("scope");
            if (scope != null) {
                _ui.debugMessage("edit policy for " + scope);
                _manager.editPolicy(scope);
            }
        }
    }

    protected void dispose() { _manager.dispose(); }
    
    public String getPanelName() { return _translationRegistry.getText("Expiration manager"); }
    public String getPanelDescription() { return _translationRegistry.getText("Control the deletion of old messages"); }
}
