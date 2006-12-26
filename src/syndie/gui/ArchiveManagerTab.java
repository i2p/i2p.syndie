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
public class ArchiveManagerTab extends BrowserTab {
    private ArchiveManager _manager;
    
    public ArchiveManagerTab(BrowserControl browser, SyndieURI uri) { super(browser, uri); }
    
    protected void initComponents() {
        getRoot().setLayout(new FillLayout());
        _manager = new ArchiveManager(getBrowser(), getRoot());
    }
        
    public Image getIcon() { return ImageUtil.ICON_TAB_ARCHIVE; }
    public String getName() { return "Archive"; }
    public String getDescription() { return "Manage archive configuration"; }
    
    protected void disposeDetails() { _manager.dispose(); }
}
