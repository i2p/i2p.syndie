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
    
    public BrowseForumTab(BrowserControl browser, SyndieURI uri, String suggestedName, String suggestedDesc) {
        super(browser, uri); 
        debug("browseForum construct: super complete: " + uri);
        Hash scope = uri.getScope();
        if (uri.isSearch()) {
            Hash scopes[] = uri.getSearchScopes();
            if ( (scopes != null) && (scopes.length == 1) )
                scope = scopes[0];
        }
        if (scope != null) {
            long chanId = getClient().getChannelId(scope);
            debug("browseForum construct: fetch chanId");
            if (chanId >= 0) {
                ChannelInfo chan = getClient().getChannel(chanId);
                debug("browseForum construct: fetch chan");
                if (chan != null) {
                    _name = chan.getName();
                    _description = chan.getDescription();
                    //_icon = createAvatar(chan);
                    _icon = ImageUtil.ICON_TAB_BROWSE;
                }
            }
            if (_name == null) {
                _name = scope.toBase64().substring(0,6);
                _description = "forum: " + scope.toBase64();
                _icon = ImageUtil.ICON_TAB_BROWSE;
            }
        } else {
            _name = "browse";
            _description = "browse forums";
            _icon = ImageUtil.ICON_TAB_BROWSE;
        }
        debug("browseForum construct: done, now reconfig");
        
        if (suggestedName != null)
            _name = suggestedName;
        if (suggestedDesc != null)
            _description = suggestedDesc;
        reconfigItem();
    }
    
    public SyndieURI getURI() {
        SyndieURI uri = null;
        if (_browse != null)
            uri = _browse.getURI();
        if (uri == null)
            uri = super.getURI();
        return uri;
    }
    
    public void setFilter(SyndieURI uri) {
        if (!uri.equals(getURI())) {
            getBrowser().getUI().debugMessage("updating filter to " + uri);
            _browse.setFilter(uri);
        }
    }
    
    protected void initComponents() {
        SyndieURI uri = getURI();
        boolean byForum = uri.getBoolean("byforum", false);
        if (uri.isChannel()) {
            if (uri.getMessageId() != null) {
                getBrowser().getUI().debugMessage("browse forum w/ channel & msgId");
                _browse = ComponentBuilder.instance().createBrowseForum(getRoot(), new ForumListener(), true, byForum);
                _browse.setFilter(uri.createSearch());
                _browse.preview(uri);
            } else {
                getBrowser().getUI().debugMessage("browse forum w/out channel & msgId");
                _browse = ComponentBuilder.instance().createBrowseForum(getRoot(), new ForumListener(), false, byForum);
                _browse.setFilter(uri.createSearch());
            }
        } else if (uri.isSearch()) {
            getBrowser().getUI().debugMessage("browse forum w/ search: " + uri.toString());
            _browse = ComponentBuilder.instance().createBrowseForum(getRoot(), new ForumListener(), false, byForum);
            _browse.setFilter(uri);
        } else {
            getBrowser().getUI().debugMessage("browse forum w/ other");
            _browse = ComponentBuilder.instance().createBrowseForum(getRoot(), new ForumListener(), false, byForum);
            _browse.setFilter(SyndieURI.DEFAULT_SEARCH_URI);
        }
        getRoot().setLayout(new FillLayout());
        getBrowser().getThemeRegistry().register(this);
        debug("browseforumtab.initComponents: complete");
    }
    
    public boolean canShow(SyndieURI uri) {
        if (super.canShow(uri)) return true;
        if (!uri.isSearch() && !uri.isChannel()) return false;
        if (uri.getMessageId() != null) return false; // individual messages go to MessageViewTab
        // now check for search vs. browse
        SyndieURI localURI = getURI();
        Hash scopes[] = null;
        Hash scope = null;
        if (localURI.isChannel()) {
            scope = localURI.getScope();
        } else if (localURI.isSearch()) {
            scopes = localURI.getSearchScopes();
            if ( (scopes != null) && (scopes.length == 1) )
                scope = scopes[0];
        }
        
        Hash newScopes[] = null;
        Hash newScope = null;
        if (uri.isChannel()) {
            newScope = uri.getScope();
        } else if (uri.isSearch()) {
            newScopes = uri.getSearchScopes();
            if ( (newScopes != null) && (newScopes.length == 1) )
                newScope = newScopes[0];
        }
        
        if ( (newScope == null) && (scope == null) && (newScopes == null) && (scopes == null) )
            return true;
        if ( (scope != null) && (newScope != null) && (scope.equals(newScope)) )
            return true;
        if ( (scopes != null) && (newScopes != null) && (scopes.length == newScopes.length) ) {
            for (int i = 0; i < scopes.length; i++) {
                boolean found = false;
                for (int j = 0; j < newScopes.length && !found; j++)
                    if (scopes[i].equals(newScopes[j]))
                        found = true;
                if (!found)
                    return false;
            }
            return true;
        }
        return false;
    }
    
    public void show(SyndieURI uri) {
        setFilter(uri);
    }
    public void refresh() { _browse.refresh(); }
    
    public void toggleMaxView() { _browse.toggleMaxView(); }
    
    private static class ForumListener implements MessageTree.MessageTreeListener {
        public void messageSelected(MessageTree tree, SyndieURI uri, boolean toView, boolean nodelay) {}
        public void filterApplied(MessageTree tree, SyndieURI searchURI) {}
    }
        
    public Image getIcon() { return _icon; }
    public String getName() { return _name; }
    public String getDescription() { return _description; }
    
    protected void disposeDetails() {
        _browse.dispose();
    }
}
