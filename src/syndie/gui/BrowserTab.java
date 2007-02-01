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
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

/**
 *
 */
abstract class BrowserTab implements Themeable {
    private BrowserControl _browser;
    private CTabItem _item;
    private SyndieURI _uri;
    private Composite _root;
    
    private static final int TAB_ICON_SIZE = 16;
    static final String TYPE_POST = "post";
    static final String TYPE_TEXTUI = "textui";
    static final String TYPE_LOGS = "logs";
    static final String TYPE_META = "meta";
    static final String TYPE_MANAGE = "manage";
    static final String TYPE_SYNDICATE_ARCHIVES = "syndicate";
    static final String TYPE_SYNDICATE_CONFIG = TYPE_SYNDICATE_ARCHIVES;
    static final String TYPE_SYNDICATE_STATUS = TYPE_SYNDICATE_ARCHIVES;
    static final String TYPE_SQL = "sql";
    static final String TYPE_TRANSLATE = "translate";
    static final String TYPE_HIGHLIGHT = "highlight";
    static final String TYPE_ARCHIVEMGR = "archivemgr";
    static final String TYPE_BUGREPORT = "bug";
    static final String TYPE_BACKUPSECRETS = "backupsecrets";
    static final String TYPE_VIEWFORUM = "viewforum";
    static final String TYPE_SYNC = "sync";
    
    public static BrowserTab build(BrowserControl browser, SyndieURI uri) {
        // build a new browser tab based on the uri pointed to
        if (TYPE_POST.equalsIgnoreCase(uri.getType())) {
            Long postponeId = uri.getLong("postponeid");
            Long postponeVer = uri.getLong("postponever");
            if ( (postponeId != null) && (postponeVer != null) ) {
                return new MessageEditorTab(browser, uri);
                //return new EditMessageTab(browser, uri);
            } else {
                Hash forum = uri.getScope();
                String parentURI = uri.getString("parent");
                SyndieURI parent = null;
                if (parentURI != null) {
                    try { parent = new SyndieURI(parentURI); } catch (URISyntaxException use) {}
                }
                boolean asReply = uri.getBoolean("reply", false);
                // create a new editor tab
                //return new EditMessageTab(browser, uri, scope, parent, asReply);
                return new MessageEditorTab(browser, uri, forum, parent, asReply);
            }
        } else if (uri.isChannel()) {
            if (uri.getScope() != null) {
                if (uri.getMessageId() == null) {
                    // browse the forum as a whole
                    return new BrowseForumTab(browser, uri);
                } else {
                    // view a specific message
                    return new MessageViewTab(browser, uri);
                }
            }
        } else if (TYPE_MANAGE.equalsIgnoreCase(uri.getType())) {
            return new ViewForumTab(browser, uri);
        } else if (TYPE_META.equalsIgnoreCase(uri.getType())) {
            return new ViewForumTab(browser, uri);
        } else if (TYPE_TEXTUI.equals(uri.getType())) {
            return new TextUITab(browser, uri);
        } else if (TYPE_LOGS.equals(uri.getType())) {
            return new LogTab(browser, uri);
        } else if (uri.isArchive()) {
            //return new SyndicationSchedulerTab(browser, uri);
            return new SyndicatorTab(browser, uri);
        } else if (TYPE_SYNDICATE_ARCHIVES.equals(uri.getType())) {
            //return new SyndicationArchiveTab(browser, uri);
            //return new SyndicationSchedulerTab(browser, uri);
            return new SyndicatorTab(browser, uri);
        } else if (TYPE_SYNDICATE_CONFIG.equals(uri.getType())) {
            //return new SyndicationConfigTab(browser, uri);
            //return new SyndicationSchedulerTab(browser, uri);
            return new SyndicatorTab(browser, uri);
        } else if (TYPE_SYNDICATE_STATUS.equals(uri.getType())) {
            //return new SyndicationStatusTab(browser, uri);
            //return new SyndicationSchedulerTab(browser, uri);
            return new SyndicatorTab(browser, uri);
        } else if (TYPE_SQL.equals(uri.getType())) {
            return new SQLTab(browser, uri);
        } else if (uri.isText()) {
            return new PageRendererTab(browser, uri);
        } else if (TYPE_HIGHLIGHT.equals(uri.getType())) {
            return new HighlightViewTab(browser, uri);
        } else if (uri.isSearch()) {
            return new BrowseForumTab(browser, uri);
        } else if (TYPE_ARCHIVEMGR.equals(uri.getType())) {
            return new ArchiveManagerTab(browser, uri);
        } else if (TYPE_BUGREPORT.equals(uri.getType())) {
            return new BugReportTab(browser, uri);
        } else if (TYPE_BACKUPSECRETS.equals(uri.getType())) {
            return new BackupSecretsTab(browser, uri);
        } else if (TYPE_VIEWFORUM.equals(uri.getType())) {
            return new ViewForumTab(browser, uri);
        } else if (TYPE_SYNC.equals(uri.getType())) {
            return new SyndicatorTab(browser, uri);
        }
        
        return null;
    }
    
    protected BrowserTab(BrowserControl browser, SyndieURI uri) {
        _browser = browser;
        debug("constructing base browser tab");
        _item = new CTabItem(browser.getTabFolder(), SWT.CLOSE | SWT.BORDER);
        _uri = uri;
        debug("constructing base browser tab: initializing components");
        _root = new Composite(browser.getTabFolder(), SWT.NONE);
        _item.setControl(_root);
        initComponents();
        //Point sz = _root.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        //debug("** constructing unpacked size: " + sz);
        //_root.pack(true);
        //sz = _root.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        //debug("** constructing packed size: " + sz);
        configItem();
    }
    
    protected abstract void initComponents();
    protected boolean allowClose() { return true; }
    protected void tabShown() {}
    protected void configItem() {
        reconfigItem();
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
        if ( (icon != null) && (!icon.isDisposed()) ) {
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
    /** ask the browser to close us (call this internally - do not use close()) */
    protected void closeTab() { _browser.unview(getURI()); }
    
    public CTabItem getTabItem() { return _item; }
    public SyndieURI getURI() { return _uri; }
    public String getDescription() { return getURI().toString(); }
    public String getName() { return "tab"; }
    public Image getIcon() { return null; }
    /** 
     * vetoable close and dispose (including the associated tab item), returning true
     * if the tab was closed, false if it was left intact (aka the tab vetoed closure).
     * call this from the Browser - internally use closeTab(), which then asks the Browser
     * to call close() (after doing some bookkeeping)
     */
    public boolean close() { dispose(); return true; }
    /** unvetoable close */
    public void dispose() {
        _browser.getThemeRegistry().unregister(this);
        if (!_item.isDisposed())
            _item.dispose();
        ImageUtil.dispose(getIcon());
        disposeDetails();
    }
    protected abstract void disposeDetails();
    
    public boolean canShow(SyndieURI uri) { 
        if (uri == null) return false;
        boolean eq = getURI().equals(uri);
        if (eq)
            _browser.getUI().debugMessage("tab is equal to the uri: " + getClass().getName() + " uri=" + getURI() + " newURI=" + uri);
        return eq;
    }
    public void resized() {}
    
    public void show(SyndieURI uri) {}
    public void refresh() {}
    
    public void toggleMaxView() {}
    public void toggleMaxEditor() {}
    
    protected void debug(String msg) { _browser.getUI().debugMessage(msg); }
    protected void debug(String msg, Exception e) { _browser.getUI().debugMessage(msg, e); }
    protected void status(String msg) { _browser.getUI().statusMessage(msg); }
    protected void error(String msg) { _browser.getUI().errorMessage(msg); }
    protected void error(String msg, Exception e) { _browser.getUI().errorMessage(msg, e); }
    
    protected Image createAvatar(ChannelInfo chan) {
        return ImageUtil.resize(ImageUtil.ICON_QUESTION, 16, 16, false);
    }
    
    public void applyTheme(Theme theme) { 
        //_item.setFont(theme.TAB_FONT); 
    }
}
