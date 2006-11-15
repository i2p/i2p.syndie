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
    
    public BrowseForumTab(Browser browser, SyndieURI uri) {
        super(browser, uri); 
        if (uri.getScope() != null) {
            long chanId = getClient().getChannelId(uri.getScope());
            if (chanId >= 0) {
                ChannelInfo chan = getClient().getChannel(chanId);
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
        reconfigItem();
    }
    
    protected void initComponents() {
        _browse = new BrowseForum(getRoot(), getClient(), new ForumListener());
        SyndieURI uri = getURI();
        if (uri.isChannel())
            _browse.setFilter(uri.createSearch());
        else if (uri.isSearch())
            _browse.setFilter(uri);
        else
            _browse.setFilter(SyndieURI.DEFAULT_SEARCH_URI);
        getRoot().setLayout(new FillLayout());
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
