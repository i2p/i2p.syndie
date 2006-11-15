package syndie.gui;

import java.net.URISyntaxException;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabFolder2Listener;
import org.eclipse.swt.custom.CTabFolderEvent;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import syndie.data.ChannelInfo;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

/**
 *
 */
abstract class BrowserTab {
    private Browser _browser;
    private CTabItem _item;
    private SyndieURI _uri;
    private Composite _root;
    
    private static final int TAB_ICON_SIZE = 16;
    
    public static BrowserTab build(Browser browser, SyndieURI uri) {
        // build a new browser tab based on the uri pointed to
        if ("post".equalsIgnoreCase(uri.getType())) {
            Hash scope = uri.getScope();
            String parentURI = uri.getString("parent");
            SyndieURI parent = null;
            if (parentURI != null) {
                try { parent = new SyndieURI(parentURI); } catch (URISyntaxException use) {}
            }
            // create a new editor tab
            return new EditMessageTab(browser, uri, scope, parent);
        } else if ("channel".equalsIgnoreCase(uri.getType())) {
            if ( (uri.getScope() != null) && (uri.getMessageId() == null) )
                return new BrowseForumTab(browser, uri);
        }
        return null;
    }
    
    protected BrowserTab(Browser browser, SyndieURI uri) {
        _browser = browser;
        _item = new CTabItem(browser.getTabFolder(), SWT.CLOSE | SWT.BORDER);
        _uri = uri;
        _root = new Composite(browser.getTabFolder(), SWT.NONE);
        initComponents();
        configItem();
    }
    
    protected abstract void initComponents();
    protected boolean allowClose() { return true; }
    protected void configItem() {
        reconfigItem();
        _item.setControl(_root);
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
    }
    
    protected Composite getRoot() { return _root; }
    protected DBClient getClient() { return _browser.getClient(); }
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
    
    protected Image createAvatar(ChannelInfo chan) {
        return ImageUtil.resize(ImageUtil.ICON_QUESTION, 16, 16, false);
    }
}
