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
public class ManageForumTab extends BrowserTab implements Translatable {
    private ManageForum _manage;
    private String _name;
    private String _description;
    private Image _icon;
    private boolean _editable;
    
    public ManageForumTab(BrowserControl browser, SyndieURI uri) { this(browser, uri, true); }
    public ManageForumTab(BrowserControl browser, SyndieURI uri, boolean editable) {
        super(browser, uri); 
        _editable = editable;
        deferredInit();
    }
    
    protected void initComponents() {}
    protected void deferredInit() {
        debug("manageforumtab.initComponents");
        _manage = new ManageForum(getBrowser(), getRoot(), new ManageListener(), _editable);
        debug("manageforumtab.initComponents: browseforum constructed");
        _manage.setForum(getURI());
        String detail = getURI().getString(ManageForum.DETAIL);
        if (detail != null)
            _manage.pickDetail(detail);
        getRoot().setLayout(new FillLayout());
        debug("manageforumtab.initComponents: complete");
        getBrowser().getTranslationRegistry().register(this);
        reconfigItem();
    }
    
    private class ManageListener implements ManageForum.ManageForumListener {
        public void manageComplete(ManageForum manage) { closeTab(); }
    }
        
    public Image getIcon() { return _icon; }
    public String getName() { return _name; }
    public String getDescription() { return _description; }
    
    protected void disposeDetails() { 
        getBrowser().getTranslationRegistry().unregister(this); 
        _manage.dispose();
    }
    
    public boolean close() {
        if (allowClose()) 
            return super.close();
        else
            return false;
    }
    
    protected boolean allowClose() { return _manage.confirmClose(); }
    
    public void translate(TranslationRegistry registry) {
        reconfigItem(registry);
    }
    
    private static final String T_MANAGE_DESC_PREFIX = "syndie.gui.manageforumtab.descprefix";
    private static final String T_MANAGE_NAME = "syndie.gui.manageforumtab.name";
    private static final String T_MANAGE_DESC = "syndie.gui.manageforumtab.desc";
    
    private void reconfigItem(TranslationRegistry registry) {
        SyndieURI uri = getURI();
        if (uri.getScope() != null) {
            long chanId = getClient().getChannelId(uri.getScope());
            if (chanId >= 0) {
                ChannelInfo chan = getClient().getChannel(chanId);
                if (chan != null) {
                    _name = chan.getName();
                    _description = chan.getDescription();
                    _icon = createAvatar(chan);
                    getBrowser().getUI().debugMessage("creating manage avatar: " + _icon);
                }
            }
            if (_name == null) {
                _name = uri.getScope().toBase64().substring(0,6);
                _description = registry.getText(T_MANAGE_DESC_PREFIX, "manage: ") + uri.getScope().toBase64();
                _icon = getRoot().getDisplay().getSystemImage(SWT.ICON_INFORMATION);
                getBrowser().getUI().debugMessage("using manage avatar for nameless: " + _icon);
            }
        } else {
            _name = registry.getText(T_MANAGE_NAME, "manage");
            _description = registry.getText(T_MANAGE_DESC, "manage forum");
            _icon = getRoot().getDisplay().getSystemImage(SWT.ICON_INFORMATION);
            getBrowser().getUI().debugMessage("using manage avatar for scopeless: " + _icon);
        }
    }
}
