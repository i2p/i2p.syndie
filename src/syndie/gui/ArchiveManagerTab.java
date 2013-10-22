package syndie.gui;

import net.i2p.data.Hash;

import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.SWT;

import syndie.data.ChannelInfo;
import syndie.data.SyndieURI;

/**
 *  Contains an ArchiveManager
 */
class ArchiveManagerTab extends BrowserTab {
    private ArchiveManager _manager;
    
    public ArchiveManagerTab(BrowserControl browser, SyndieURI uri) { super(browser, uri); }
    
    protected void initComponents() {
        getRoot().setLayout(new FillLayout());
        _manager = new ArchiveManager(_client, _ui, _themeRegistry, _translationRegistry, getRoot(), getBrowser().getNavControl());
    }
        
    public Image getIcon() { return ImageUtil.ICON_MANAGEARCHIVE; }
    public String getName() { return "Manage archive"; }
    public String getDescription() { return _translationRegistry.getText("Manage archive configuration"); }
    
    protected void disposeDetails() { _manager.dispose(); }
}
