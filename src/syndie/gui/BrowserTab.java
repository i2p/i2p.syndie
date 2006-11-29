package syndie.gui;

import java.net.URISyntaxException;
import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabFolder2Listener;
import org.eclipse.swt.custom.CTabFolderEvent;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

/**
 *
 */
abstract class BrowserTab {
    private BrowserControl _browser;
    private CTabItem _item;
    private SyndieURI _uri;
    private ScrolledComposite _scroll;
    private Composite _root;
    
    private static final int TAB_ICON_SIZE = 16;
    static final String TYPE_POST = "post";
    static final String TYPE_TEXTUI = "textui";
    static final String TYPE_LOGS = "logs";
    static final String TYPE_META = "meta";
    static final String TYPE_MANAGE = "manage";
    static final String TYPE_SYNDICATE = "syndicate";
    
    public static BrowserTab build(BrowserControl browser, SyndieURI uri) {
        // build a new browser tab based on the uri pointed to
        if (TYPE_POST.equalsIgnoreCase(uri.getType())) {
            Hash scope = uri.getScope();
            String parentURI = uri.getString("parent");
            SyndieURI parent = null;
            if (parentURI != null) {
                try { parent = new SyndieURI(parentURI); } catch (URISyntaxException use) {}
            }
            boolean asReply = uri.getBoolean("reply", false);
            // create a new editor tab
            return new EditMessageTab(browser, uri, scope, parent, asReply);
        } else if ("channel".equalsIgnoreCase(uri.getType())) {
            if (uri.getScope() != null) {
                if (uri.getMessageId() == null) {
                    // browse the forum as a whole
                    return new BrowseForumTab(browser, uri);
                } else {
                    // view a specific message
                    // todo: perhaps a separate tab instead of this browse w/ preview?
                    return new BrowseForumTab(browser, uri);
                }
            }
        } else if (TYPE_MANAGE.equalsIgnoreCase(uri.getType())) {
            return new ManageForumTab(browser, uri);
        } else if (TYPE_META.equalsIgnoreCase(uri.getType())) {
            Hash forum = uri.getHash("channel");
            if (forum == null)
                forum = uri.getScope();
            // if we can manage this, do so
            if (forum != null) {
                List keys = browser.getClient().getNymKeys(forum, Constants.KEY_FUNCTION_MANAGE);
                if ( (keys != null) && (keys.size() > 0) )
                    return new ManageForumTab(browser, uri, true);
            }
            return new ManageForumTab(browser, uri, false);
        } else if (TYPE_TEXTUI.equals(uri.getType())) {
            return new TextUITab(browser, uri);
        } else if (TYPE_LOGS.equals(uri.getType())) {
            return new LogTab(browser, uri);
        } else if (uri.isArchive()) {
            return new SyndicationTab(browser, uri);
        } else if (TYPE_SYNDICATE.equals(uri.getType())) {
            return new SyndicationTab(browser, uri);
        }
        
        return null;
    }
    
    protected BrowserTab(BrowserControl browser, SyndieURI uri) {
        _browser = browser;
        debug("constructing base browser tab");
        _item = new CTabItem(browser.getTabFolder(), SWT.CLOSE | SWT.BORDER);
        _uri = uri;
        _scroll = new ScrolledComposite(browser.getTabFolder(), SWT.H_SCROLL | SWT.V_SCROLL);
        _root = new Composite(_scroll, SWT.NONE);
        _scroll.setContent(_root);
        _scroll.setExpandHorizontal(true);
        _scroll.setExpandVertical(true);
        debug("constructing base browser tab: initializing components");
        initComponents();
        Rectangle sz2 = browser.getTabFolder().getClientArea();
        Point sz1 = _root.computeSize(sz2.width, sz2.height);
        _scroll.setMinSize(sz1);
        debug("**constructing base browser tab: configuring the item: " + sz1 +" / " + sz2 + " - " + getClass().getName());
        configItem();
    }
    
    protected abstract void initComponents();
    protected boolean allowClose() { return true; }
    protected void tabShown() {}
    protected void configItem() {
        reconfigItem();
        _item.setControl(_scroll);
        _item.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent evt) { _browser.unview(_uri); }
        });
        _item.getParent().addCTabFolder2Listener(new CTabFolder2Listener() {
            public void close(CTabFolderEvent evt) {
                if (evt.item == _item) {
                    if (!allowClose())
                        evt.doit = false;
                }
            }
            public void maximize(CTabFolderEvent cTabFolderEvent) {}
            public void minimize(CTabFolderEvent cTabFolderEvent) {}
            public void restore(CTabFolderEvent cTabFolderEvent) {}
            public void showList(CTabFolderEvent cTabFolderEvent) {}
        });
    }
    protected void reconfigItem() {
        debug("reconfiguring item: begin");
        Image old = _item.getImage();
        Image icon = getIcon();
        if (icon != null) {
            Rectangle bounds = icon.getBounds();
            if ( (bounds.width > TAB_ICON_SIZE) || (bounds.height > TAB_ICON_SIZE) )
                icon = ImageUtil.resize(icon, TAB_ICON_SIZE, TAB_ICON_SIZE, true);
            _item.setImage(icon);
        } else {
            _item.setImage(null);
        }
        if (old != null)
            ImageUtil.dispose(old);
        _item.setText((null != getName() ? getName() : ""));
        _item.setToolTipText((null != getDescription() ? getDescription() : ""));
        debug("reconfiguring item: complete");
    }
    
    protected Composite getRoot() { return _root; }
    protected DBClient getClient() { return _browser.getClient(); }
    protected BrowserControl getBrowser() { return _browser; }
    protected void closeTab() { _browser.unview(getURI()); }
    
    public CTabItem getTabItem() { return _item; }
    public SyndieURI getURI() { return _uri; }
    public String getDescription() { return getURI().toString(); }
    public String getName() { return "tab"; }
    public Image getIcon() { return null; }
    public void dispose() {
        _item.dispose();
        ImageUtil.dispose(getIcon());
        disposeDetails();
    }
    protected abstract void disposeDetails();
    
    protected void debug(String msg) { _browser.getUI().debugMessage(msg); }
    protected void debug(String msg, Exception e) { _browser.getUI().debugMessage(msg, e); }
    protected void status(String msg) { _browser.getUI().statusMessage(msg); }
    protected void error(String msg) { _browser.getUI().errorMessage(msg); }
    protected void error(String msg, Exception e) { _browser.getUI().errorMessage(msg, e); }
    
    protected Image createAvatar(ChannelInfo chan) {
        return ImageUtil.resize(ImageUtil.ICON_QUESTION, 16, 16, false);
    }
}
