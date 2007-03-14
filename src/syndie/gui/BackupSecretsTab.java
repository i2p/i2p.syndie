package syndie.gui;

import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.SWT;
import syndie.data.SyndieURI;

/**
 *
 */
public class BackupSecretsTab extends BrowserTab {
    private BackupSecrets _backup;
    
    public BackupSecretsTab(BrowserControl browser, SyndieURI uri) { super(browser, uri); }
    
    protected void initComponents() {
        getRoot().setLayout(new FillLayout());
        _backup = new BackupSecrets(getBrowser(), getBrowser(), getRoot(), getURI());
    }
    
    public Image getIcon() { return ImageUtil.ICON_TAB_ARCHIVE; }
    public String getName() { return "Backup"; }
    public String getDescription() { return "Backup secret keys"; }
    
    protected void disposeDetails() { _backup.dispose(); }
}
