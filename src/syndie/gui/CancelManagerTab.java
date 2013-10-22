package syndie.gui;

import net.i2p.data.Hash;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.SWT;
import syndie.data.ChannelInfo;
import syndie.data.SyndieURI;

/**
 *
 */
class CancelManagerTab extends BrowserTab {
    private CancelManager _manager;
    
    public CancelManagerTab(BrowserControl browser, SyndieURI uri) { super(browser, uri); }
    
    protected void initComponents() {
        getRoot().setLayout(new FillLayout());
        _manager = new CancelManager(_client, _ui, _themeRegistry, _translationRegistry, getRoot());
        if (getURI() != null) edit(getURI());
    }
    
    public boolean canShow(SyndieURI uri) { 
        if (uri == null) return false;
        return getURI().getType().equals(uri.getType());
    }
    
    public void show(SyndieURI uri) {
        _ui.debugMessage("cancel manager shown: " + uri + " [orig: " + getURI() + "]");
        if (uri == null) return;
        if (getURI().equals(uri)) return;
        edit(uri);
    }
    private void edit(SyndieURI uri) {
        if (uri != null) {
            Hash scope = uri.getHash("scope");
            if (scope != null) {
                _ui.debugMessage("edit policy for " + scope);
                _manager.editPolicy(scope);
            }
        }
    }
        
    public Image getIcon() { return ImageUtil.ICON_TAB_ARCHIVE; }
    public String getName() { return "Cancel"; }
    public String getDescription() { return _translationRegistry.getText("Control whose cancel messages to honor"); }
    
    protected void disposeDetails() { _manager.dispose(); }
}
