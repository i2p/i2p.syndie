package syndie.gui;

import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import syndie.data.ChannelInfo;
import syndie.data.SyndieURI;

/**
 *
 */
public class SyndicationTab extends BrowserTab implements Translatable {
    private SyndicationView _view;
    private String _name;
    private String _description;
    private Image _icon;
    
    public SyndicationTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri); 
        browser.getTranslationRegistry().register(this);
        reconfigItem();
    }
    
    protected void initComponents() {
        _view = new SyndicationView(getBrowser(), getRoot(), getURI());
        getRoot().setLayout(new FillLayout());
    }
    
    public Image getIcon() { return _icon; }
    public String getName() { return _name; }
    public String getDescription() { return _description; }
    
    protected void disposeDetails() { getBrowser().getTranslationRegistry().unregister(this); }
    
    private static final String T_NAME = "syndie.gui.syndicationtab.name";
    private static final String T_DESC = "syndie.gui.syndicationtab.desc";
    
    public void translate(TranslationRegistry registry) {
        _name = registry.getText(T_NAME, "Syndicate");
        _description = registry.getText(T_DESC, "Syndicate messages between archives");
        
        _icon = ImageUtil.ICON_TAB_SYNDICATE;
        reconfigItem();
    }
    
    
    protected void tabShown() { _view.shown(); }
}
