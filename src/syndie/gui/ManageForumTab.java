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
public class ManageForumTab extends BrowserTab {
    private ManageForum _manage;
    private String _name;
    private String _description;
    private Image _icon;
    
    public ManageForumTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri); 
        debug("manageForum construct: super complete");
        if (uri.getScope() != null) {
            long chanId = getClient().getChannelId(uri.getScope());
            debug("manageForum construct: fetch chanId");
            if (chanId >= 0) {
                ChannelInfo chan = getClient().getChannel(chanId);
                debug("manageForum construct: fetch chan");
                if (chan != null) {
                    _name = chan.getName();
                    _description = chan.getDescription();
                    _icon = createAvatar(chan);
                }
            }
            if (_name == null) {
                _name = uri.getScope().toBase64().substring(0,6);
                _description = "manage: " + uri.getScope().toBase64();
                _icon = getRoot().getDisplay().getSystemImage(SWT.ICON_INFORMATION);
            }
        } else {
            _name = "manage";
            _description = "manage forum";
            _icon = getRoot().getDisplay().getSystemImage(SWT.ICON_INFORMATION);
        }
        debug("manageForum construct: done, now reconfig");
        reconfigItem();
    }
    
    protected void initComponents() {
        debug("manageforumtab.initComponents");
        _manage = new ManageForum(getBrowser(), getRoot(), new ManageListener());
        debug("manageforumtab.initComponents: browseforum constructed");
        _manage.setForum(getURI());
        getRoot().setLayout(new FillLayout());
        debug("manageforumtab.initComponents: complete");
    }
    
    private class ManageListener implements ManageForum.ManageForumListener {
        public void manageComplete(ManageForum manage) { closeTab(); }
    }
        
    public Image getIcon() { return _icon; }
    public String getName() { return _name; }
    public String getDescription() { return _description; }
    
    protected void disposeDetails() {}
    
    protected boolean allowClose() {
        return _manage.confirmClose();
    }
    
}
