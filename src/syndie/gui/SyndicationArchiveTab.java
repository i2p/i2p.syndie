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
public class SyndicationArchiveTab extends BrowserTab implements Translatable {
    private SyndicationArchiveView _view;
    private String _name;
    private String _description;
    private Image _icon;
    
    public SyndicationArchiveTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri); 
        reconfigItem();
    }
    
    protected void initComponents() {
        _view = new SyndicationArchiveView(getBrowser(), getRoot());
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
    
    private static final String T_NAME = "syndie.gui.syndicationarchivetab.name";
    private static final String T_DESC = "syndie.gui.syndicationarchivetab.desc";
    
    public void translate(TranslationRegistry registry) {
        _name = registry.getText(T_NAME, "Archives");
        _description = registry.getText(T_DESC, "Archives available for syndication");
        
        _icon = ImageUtil.ICON_TAB_SYNDICATE;
        reconfigItem();
    }
    
    protected void tabShown() { _view.shown(); }
}
