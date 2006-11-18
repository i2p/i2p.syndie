package syndie.gui;

import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.SWT;
import syndie.data.ChannelInfo;
import syndie.data.SyndieURI;

/**
 *
 */
public class BrowseForumTab extends BrowserTab {
    private BrowseForum _browse;
    private String _name;
    private String _description;
    private Image _icon;
    
    public BrowseForumTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri); 
        debugMessage("browseForum construct: super complete");
        if (uri.getScope() != null) {
            long chanId = getClient().getChannelId(uri.getScope());
            debugMessage("browseForum construct: fetch chanId");
            if (chanId >= 0) {
                ChannelInfo chan = getClient().getChannel(chanId);
                debugMessage("browseForum construct: fetch chan");
                if (chan != null) {
                    _name = chan.getName();
                    _description = chan.getDescription();
                    _icon = createAvatar(chan);
                }
            }
            if (_name == null) {
                _name = uri.getScope().toBase64().substring(0,6);
                _description = "forum: " + uri.getScope().toBase64();
                _icon = getRoot().getDisplay().getSystemImage(SWT.ICON_INFORMATION);
            }
        } else {
            _name = "browse";
            _description = "browse forums";
            _icon = getRoot().getDisplay().getSystemImage(SWT.ICON_INFORMATION);
        }
        debugMessage("browseForum construct: done, now reconfig");
        reconfigItem();
    }
    
    protected void initComponents() {
        debugMessage("browseforumtab.initComponents");
        _browse = new BrowseForum(getRoot(), getBrowser(), new ForumListener());
        debugMessage("browseforumtab.initComponents: browseforum constructed");
        SyndieURI uri = getURI();
        if (uri.isChannel()) {
            _browse.setFilter(uri.createSearch());
            if (uri.getMessageId() != null)
                _browse.preview(uri);
        } else if (uri.isSearch()) {
            _browse.setFilter(uri);
        } else {
            _browse.setFilter(SyndieURI.DEFAULT_SEARCH_URI);
        }
        getRoot().setLayout(new FillLayout());
        debugMessage("browseforumtab.initComponents: complete");
    }
    
    private class ForumListener implements MessageTree.MessageTreeListener {
        public void messageSelected(MessageTree tree, SyndieURI uri, boolean toView) {}
        public void filterApplied(MessageTree tree, SyndieURI searchURI) {}
    }
        
    public Image getIcon() { return _icon; }
    public String getName() { return _name; }
    public String getDescription() { return _description; }
    
    protected void disposeDetails() {}
}
