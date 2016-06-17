package syndie.gui;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import net.i2p.data.Hash;
import net.i2p.data.PrivateKey;
import net.i2p.data.SessionKey;
import net.i2p.data.SigningPrivateKey;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import syndie.data.MessageInfo;
import syndie.data.SyndieURI;

/**
 *  Simple display of HTML(?) text at the top level of the browser.
 *  Only for "text" URIs.These are not the pages inside PageRenderer.
 *
 *  The URI params "name" and "body" supply the title and text.
 *
 *  These are not the pages inside PageRenderer.
 */
class PageRendererTab extends BrowserTab implements Translatable, Themeable, PageRenderer.PageActionListener {
    protected PageRenderer _renderer;
    private String _name;
    private String _desc;
    
    public PageRendererTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri);
    }
    
    static final SyndieURI DUMMY_URI = SyndieURI.createMessage(Hash.FAKE_HASH, Long.MAX_VALUE, 0);

    protected void show(String htmlText, String title, String desc) {
        _name = title;
        _desc = desc;
        reconfigItem();
        List pages = new ArrayList();
        pages.add(htmlText);
        List attachments = new ArrayList();
        List attachmentOrder = new ArrayList();

        MessageInfo msgInfo = new MessageInfo();
        msgInfo.setURI(DUMMY_URI);
        msgInfo.setTargetChannel(DUMMY_URI.getScope());
        msgInfo.setTargetChannelId(Long.MAX_VALUE);
        msgInfo.setScopeChannelId(Long.MAX_VALUE);
        msgInfo.setAuthorChannelId(Long.MAX_VALUE);
        msgInfo.setInternalId(Long.MAX_VALUE);
        msgInfo.setMessageId(DUMMY_URI.getMessageId().longValue());
        msgInfo.setPageCount(1);
        
        _renderer.renderPage(new PageRendererSourceMem(getBrowser().getClient(), getBrowser().getThemeRegistry(), msgInfo, pages, attachments, attachmentOrder, null), DUMMY_URI);
    }
    
    // to prevent tagging by xgettext
    private static final String NAME = "name";
    private static final String BODY = "body";

    protected void initComponents() {
        getRoot().setLayout(new FillLayout());
        _renderer = ComponentBuilder.instance().createPageRenderer(getRoot(), true, false);
        _renderer.setListener(this);
        
        getBrowser().getThemeRegistry().register(this);
        getBrowser().getTranslationRegistry().register(this);
        
        String name = getURI().getString(NAME);
        if (name == null) name = "";
        String desc = name;
        String body = getURI().getString(BODY);
        show(body, name, desc);
    }
    
    protected void disposeDetails() { 
        getBrowser().getTranslationRegistry().unregister(this);
        getBrowser().getThemeRegistry().unregister(this);
    }
    
    public Image getIcon() { return ImageUtil.ICON_TAB_PAGE; }
    public String getName() { return _name; }
    public String getDescription() { return _desc; }

    public void translate(TranslationRegistry registry) {
        // nothing translatable
    }
    public void applyTheme(Theme theme) {
        // nothing themeable
    }

    public void viewScopeMessages(PageRenderer renderer, Hash scope) {
        getBrowser().getNavControl().view(SyndieURI.createScope(scope));
    }
    public void viewScopeMetadata(PageRenderer renderer, Hash scope) {
        getBrowser().getNavControl().view(URIHelper.instance().createMetaURI(scope));
    }
    public void view(PageRenderer renderer, SyndieURI uri) {
        getBrowser().getNavControl().view(uri);
    }
    public void bookmark(PageRenderer renderer, SyndieURI uri) {
        getBrowser().bookmark(uri);
    }
    public void privateReply(PageRenderer renderer, Hash author, SyndieURI msg) {
        getBrowser().getNavControl().view(URIHelper.instance().createPostURI(author, msg, true));
    }

    public void replyToForum(PageRenderer renderer, Hash forum, SyndieURI msg) {
        getBrowser().getNavControl().view(URIHelper.instance().createPostURI(forum, msg));
    }

    public void banScope(PageRenderer renderer, Hash scope) {}
    public void viewImage(PageRenderer renderer, Image img) {}
    public void ignoreImageScope(PageRenderer renderer, Hash scope) {}
    public void importReadKey(PageRenderer renderer, Hash referencedBy, Hash keyScope, SessionKey key) {}
    public void importPostKey(PageRenderer renderer, Hash referencedBy, Hash keyScope, SigningPrivateKey key) {}
    public void importManageKey(PageRenderer renderer, Hash referencedBy, Hash keyScope, SigningPrivateKey key) {}
    public void importReplyKey(PageRenderer renderer, Hash referencedBy, Hash keyScope, PrivateKey key) {}
    public void importArchiveKey(PageRenderer renderer, Hash referencedBy, SyndieURI archiveURI, SessionKey key) {}
    public void saveAllImages(PageRenderer renderer, Map images) {}
    public void saveImage(PageRenderer renderer, String suggestedName, Image img) {}
    public void nextPage() {}
    public void prevPage() {}
    public void deleteMessage(PageRenderer renderer, SyndieURI msg) {}
    public void cancelMessage(PageRenderer renderer, SyndieURI msg) {}
}
