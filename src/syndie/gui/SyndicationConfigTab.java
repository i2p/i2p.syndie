package syndie.gui;

import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.MessageBox;
import syndie.data.ChannelInfo;
import syndie.data.SyndieURI;

/**
 *
 */
public class SyndicationConfigTab extends BrowserTab implements Translatable {
    private SyndicationConfigView _view;
    private SyndicationDiff _diff;
    private String _name;
    private String _description;
    private Image _icon;
    
    public SyndicationConfigTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri); 
        reconfigItem();
    }
    
    protected void initComponents() {
        getRoot().setLayout(new GridLayout(1, true));
        _view = new SyndicationConfigView(getBrowser(), getRoot());
        _view.getControl().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _diff = new SyndicationDiff(getBrowser(), getRoot());
        _diff.getControl().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _view.setDiff(_diff);
        getBrowser().getTranslationRegistry().register(this);
    }
    
    public Image getIcon() { return _icon; }
    public String getName() { return _name; }
    public String getDescription() { return _description; }
    
    protected void disposeDetails() { 
        _view.dispose();
        getBrowser().getTranslationRegistry().unregister(this);
    }
    
    private static final String T_NAME = "syndie.gui.syndicationconfigtab.name";
    private static final String T_DESC = "syndie.gui.syndicationconfigtab.desc";
    
    public void translate(TranslationRegistry registry) {
        _name = registry.getText(T_NAME, "Syndication config");
        _description = registry.getText(T_DESC, "Syndication policies and proxies");
        
        _icon = ImageUtil.ICON_TAB_SYNDICATE;
        reconfigItem();
    }
}
