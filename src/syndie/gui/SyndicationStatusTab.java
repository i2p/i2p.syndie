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
public class SyndicationStatusTab extends BrowserTab implements Translatable {
    private SyndicationStatusView _view;
    private String _name;
    private String _description;
    private Image _icon;
    
    public SyndicationStatusTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri); 
        reconfigItem();
    }
    
    protected void initComponents() {
        _view = new SyndicationStatusView(getBrowser(), getRoot());
        getRoot().setLayout(new FillLayout());
        getBrowser().getTranslationRegistry().register(this);
    }
    
    public Image getIcon() { return _icon; }
    public String getName() { return _name; }
    public String getDescription() { return _description; }
    
    protected void disposeDetails() { 
        _view.dispose();
        getBrowser().getTranslationRegistry().unregister(this);
    }
    
    private static final String T_NAME = "syndie.gui.syndicationstatustab.name";
    private static final String T_DESC = "syndie.gui.syndicationstatustab.desc";
    
    public void translate(TranslationRegistry registry) {
        _name = registry.getText(T_NAME, "Syndication status");
        _description = registry.getText(T_DESC, "Progress of the syndication tasks");
        
        _icon = ImageUtil.ICON_TAB_SYNDICATE;
        reconfigItem();
    }
}
