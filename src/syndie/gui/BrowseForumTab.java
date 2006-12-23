package syndie.gui;

import net.i2p.data.Hash;
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
        debug("browseForum construct: super complete");
        if (uri.getScope() != null) {
            long chanId = getClient().getChannelId(uri.getScope());
            debug("browseForum construct: fetch chanId");
            if (chanId >= 0) {
                ChannelInfo chan = getClient().getChannel(chanId);
                debug("browseForum construct: fetch chan");
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
        debug("browseForum construct: done, now reconfig");
        reconfigItem();
    }
    
    public void setFilter(SyndieURI uri) {
        if (!uri.equals(getURI())) {
            getBrowser().getUI().debugMessage("updating filter to " + uri);
            _browse.setFilter(uri);
        }
    }
    
    protected void initComponents() {
        SyndieURI uri = getURI();
        if (uri.isChannel()) {
            if (uri.getMessageId() != null) {
                getBrowser().getUI().debugMessage("browse forum w/ channel & msgId");
                _browse = new BrowseForum(getRoot(), getBrowser(), new ForumListener(), true);
                _browse.setFilter(uri.createSearch());
                _browse.preview(uri, true);
            } else {
                getBrowser().getUI().debugMessage("browse forum w/out channel & msgId");
                _browse = new BrowseForum(getRoot(), getBrowser(), new ForumListener(), false);
                _browse.setFilter(uri.createSearch());
            }
        } else if (uri.isSearch()) {
            getBrowser().getUI().debugMessage("browse forum w/ search");
            _browse = new BrowseForum(getRoot(), getBrowser(), new ForumListener(), false);
            _browse.setFilter(uri);
        } else {
            getBrowser().getUI().debugMessage("browse forum w/ other");
            _browse = new BrowseForum(getRoot(), getBrowser(), new ForumListener(), false);
            _browse.setFilter(SyndieURI.DEFAULT_SEARCH_URI);
        }
        getRoot().setLayout(new FillLayout());
        getBrowser().getThemeRegistry().register(this);
        debug("browseforumtab.initComponents: complete");
    }
    
    public boolean canShow(SyndieURI uri) {
        if (super.canShow(uri)) return true;
        if (!uri.isSearch() && !uri.isChannel()) return false;
        // now check for search vs. browse
        SyndieURI localURI = getURI();
        Hash scope = null;
        if (localURI.isChannel())
            scope = localURI.getScope();
        else if (localURI.isSearch())
            scope = localURI.getSearchScope();
        
        Hash newScope = null;
        if (uri.isChannel())
            newScope = uri.getScope();
        else if (uri.isSearch())
            newScope = uri.getSearchScope();
        
        return ( ( (newScope == null) && (scope == null) ) ||
                 ( (scope != null) && scope.equals(newScope) ) );
    }
    
    public void show(SyndieURI uri) {
        setFilter(uri);
    }
    
    private static class ForumListener implements MessageTree.MessageTreeListener {
        public void messageSelected(MessageTree tree, SyndieURI uri, boolean toView) {}
        public void filterApplied(MessageTree tree, SyndieURI searchURI) {}
    }
        
    public Image getIcon() { return _icon; }
    public String getName() { return _name; }
    public String getDescription() { return _description; }
    
    protected void disposeDetails() {
        _browse.dispose();
    }
}
