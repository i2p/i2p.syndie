package syndie.gui;

import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.SWT;
import syndie.data.SyndieURI;

/**
 *
 */
public class ViewForumTab extends BrowserTab {
    private ViewForum _view;
    
    public ViewForumTab(BrowserControl browser, SyndieURI uri) { super(browser, uri); }
    
    protected void initComponents() {
        getRoot().setLayout(new FillLayout());
        _view = new ViewForum(getBrowser(), getRoot(), getURI());
    }
    
    public Image getIcon() { return ImageUtil.ICON_TAB_ARCHIVE; }
    public String getName() { return "View forum"; }
    public String getDescription() { return "View forum"; }
    
    protected void disposeDetails() { _view.dispose(); }
}
