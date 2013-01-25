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
import syndie.db.UI;

/**
 *  Base for all tabs under Browser.
 */
public abstract class BrowserTab extends BaseComponent implements Themeable {
    private final BrowserControl _browser;
    /** this tab */
    private final CTabItem _item;
    private final SyndieURI _uri;
    private final Composite _root;
    
    //private static final int TAB_ICON_SIZE = 16;

    /**
     * Note that these types are for internal use, in addition to the
     * standard ones defined in SyndieURI.
     */
    public static final String TYPE_POST = "post";
    public static final String TYPE_RESUMEABLE = "resumeable";
    static final String TYPE_TEXTUI = "textui";
    static final String TYPE_LOGS = "logs";
    public static final String TYPE_META = "meta";
    public static final String TYPE_MANAGE = "manage";
    static final String TYPE_SYNDICATE_ARCHIVES = "syndicate";
    static final String TYPE_SYNDICATE_CONFIG = TYPE_SYNDICATE_ARCHIVES;
    public static final String TYPE_SYNDICATE_STATUS = TYPE_SYNDICATE_ARCHIVES;
    public static final String TYPE_SQL = "sql";
    static final String TYPE_TRANSLATE = "translate";
    public static final String TYPE_ARCHIVEMGR = "archivemgr";
    static final String TYPE_BUGREPORT = "bug";
    static final String TYPE_BACKUPSECRETS = "backupsecrets";
    public static final String TYPE_VIEWFORUM = "viewforum";
    public static final String TYPE_EXPIRATION = "expiration";
    public static final String TYPE_CANCEL = "cancel";
    static final String TYPE_SYNC = "sync";
    static final String TYPE_HELP = "help";
    static final String TYPE_REAL_BROWSER = "browser";
    
    private static final String PARENT = "parent";

    private static final int MAX_TAB_NAME_LEN = 22;

    public static BrowserTab build(BrowserControl browser, SyndieURI uri, String suggestedName, String suggestedDescription) {
        // build a new browser tab based on the uri pointed to
        if (TYPE_POST.equalsIgnoreCase(uri.getType())) {
            Long postponeId = uri.getLong("postponeid");
            Long postponeVer = uri.getLong("postponever");
            if ( (postponeId != null) && (postponeVer != null) ) {
                return new MessageEditorTab(browser, uri);
                //return new EditMessageTab(browser, uri);
            } else {
                Hash forum = uri.getScope();
                String parentURI = uri.getString(PARENT);
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
                    return new BrowseForumTab(browser, uri, suggestedName, suggestedDescription);
                } else {
                    // view a specific message
                    return new MessageViewTab(browser, uri, suggestedName, suggestedDescription);
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
        } else if (uri.isSearch()) {
            return new BrowseForumTab(browser, uri, suggestedName, suggestedDescription);
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
        } else if (TYPE_EXPIRATION.equals(uri.getType())) {
            return new ExpirationManagerTab(browser, uri);
        } else if (TYPE_CANCEL.equals(uri.getType())) {
            return new CancelManagerTab(browser, uri);
        } else if (TYPE_HELP.equals(uri.getType())) {
            // TEST
            return new HelpTab(browser, uri);
        } else if (TYPE_REAL_BROWSER.equals(uri.getType())) {
            // TEST
            return new RealBrowserTab(browser, uri);
        }
        
        return null;
    }
    
    protected BrowserTab(BrowserControl browser, SyndieURI uri) {
        super(browser.getClient(), browser.getUI(), browser.getThemeRegistry(), browser.getTranslationRegistry());
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
    
    /**
     *  If you have overridden applyTheme() or are Translatable, you must call
     *  _translationRegistry.register(this) (i.e. Translatable)
     *  and/or _themeRegistry.register(this) (i.e. Themable)
     *  in this method.
     *
     *  Note that BrowserTab is Themable but not Translatable.
     */
    protected abstract void initComponents();

    protected boolean allowClose() { return true; }

    /**
     * May be called multiple times.
     * Will be called after show(uri).
     * You may close() from here.
     */
    public void tabShown() { _root.layout(true); }

    protected void configItem() {
        reconfigItem();
        _item.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent evt) { _browser.getNavControl().unview(_uri); }
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
        //debug("reconfiguring item: begin");
        Image old = _item.getImage();
        Image icon = getIcon();
        if ( (icon != null) && (!icon.isDisposed()) ) {
            Rectangle bounds = icon.getBounds();
            //if ( (bounds.width > TAB_ICON_SIZE) || (bounds.height > TAB_ICON_SIZE) )
            //    icon = ImageUtil.resize(icon, TAB_ICON_SIZE, TAB_ICON_SIZE, true);
            _item.setImage(icon);
        } else {
            _item.setImage(null);
        }
        if ( (old != null) && (old != icon) ) {
            boolean disposed = ImageUtil.dispose(old);
            _ui.debugMessage("disposing old tab " + getClass().getName() + " image: " + disposed);
        }
        String title = UIUtil.truncate(getName(), MAX_TAB_NAME_LEN);
        _item.setText(title);
        _item.setToolTipText((null != getDescription() ? getDescription() : ""));
        //debug("reconfiguring item: complete");
    }
    
    protected Composite getRoot() { return _root; }

    /** you may also access protected final _client directly */
    protected DBClient getClient() { return _client; }

    protected BrowserControl getBrowser() { return _browser; }

    /** ask the browser to close us (call this internally - do not use close()) */
    protected void closeTab() { _browser.getNavControl().unview(getURI()); }
    
    /** this tab (final) */
    public CTabItem getTabItem() { return _item; }

    /**
     *  The original URI that the tab was created with.
     *  Override if you may change to a different URI.
     *  HOWEVER, note that Browser.doView() may again call show(uri)
     *  with the original uri.
     */
    public SyndieURI getURI() { return _uri; }

    /** this is the tooltip for the tab, override with a translated string */
    public String getDescription() { return getURI().toString(); }

    /** override with the real name */
    public String getName() { return "tab"; }

    /** 
     *  Override if you wish to allow others to change your name.
     *  Does nothing by default.
     *  Overriders should call reconfigItem()
     *
     *  @since 1.102b-10
     */
    public void setName(String name) {}

    /** this is the icon for the tab, default null, override with an icon */
    public Image getIcon() { return null; }

    /** 
     * veto-able close and dispose (including the associated tab item), returning true
     * if the tab was closed, false if it was left intact (aka the tab vetoed closure).
     * call this from the Browser - internally use closeTab(), which then asks the Browser
     * to call close() (after doing some bookkeeping)
     *
     * Note that this calls _themeRegistry.unregister(this) for you, but you must do
     * _translationRegistry.unregister(this) yourself
     */

    /** do not call this yourself */
    public boolean close() { dispose(); return true; }

    /** 
     * un-veto-able close
     *
     * Note that this calls _themeRegistry.unregister(this) for you, but you must do
     * _translationRegistry.unregister(this) yourself
     */
    public void dispose() {
        _themeRegistry.unregister(this);
        if (!_item.isDisposed())
            _item.dispose();
        ImageUtil.dispose(getIcon());
        disposeDetails();
    }

    protected abstract void disposeDetails();
    
    /**
     *  Returns true if uri == getURI().
     *  Override to show additional URIs.
     */
    public boolean canShow(SyndieURI uri) { 
        if (uri == null) return false;
        boolean eq = getURI().equals(uri);
        if (eq)
            _ui.debugMessage("tab is equal to the uri: " + getClass().getName() + " uri=" + getURI() + " newURI=" + uri);
        return eq;
    }

    public void resized() {}
    
    /**
     * Normally is only be called once.
     * HOWEVER, may be called again after user action, if canShow(uri) returns true,
     * OR the uri matches the original one when the tab was constructed.
     * See Browser.doView().
     * Do NOT close() from here.
     */
    public void show(SyndieURI uri) {}

    public void refresh() {}
    
    public void toggleMaxView() {}
    public void toggleMaxEditor() {}
    
    //protected Image createAvatar(ChannelInfo chan) {
    //    return ImageUtil.resize(ImageUtil.ICON_QUESTION, ImageUtil.TAB_ICON_SIZE, ImageUtil.TAB_ICON_SIZE, false);
    //}
    
    public void applyTheme(Theme theme) { 
        //_item.setFont(theme.TAB_FONT); 
    }
}
