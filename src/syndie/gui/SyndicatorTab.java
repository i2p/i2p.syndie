package syndie.gui;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import syndie.data.SyndieURI;

public class SyndicatorTab extends BrowserTab {
    private Syndicator _syndicator;
    public SyndicatorTab(BrowserControl browser, SyndieURI uri) { super(browser, uri); }
    
    protected void initComponents() {
        getRoot().setLayout(new FillLayout());
        _syndicator = new Syndicator(getBrowser().getClient(), getBrowser().getUI(), getBrowser().getThemeRegistry(),
                                     getBrowser().getTranslationRegistry(), getBrowser().getNavControl(),
                                     getBrowser(),   // DataCallback
                                     getRoot());
    }
    
    public boolean canShow(SyndieURI uri) {
        return super.canShow(uri) || getURI().getType().equals(uri.getType()) || uri.isArchive();
    }
    
    public void show(SyndieURI uri) { _syndicator.show(uri); }
    protected void disposeDetails() { _syndicator.dispose(); }
    
    public Image getIcon() { return ImageUtil.ICON_TAB_SYNDICATE; }
    public String getName() { return "Syndicate"; }
    public String getDescription() { return "Gather and share messages"; }
}
