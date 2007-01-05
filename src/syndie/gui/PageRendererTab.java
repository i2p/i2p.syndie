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
 *
 */
public class PageRendererTab extends BrowserTab implements Translatable, Themeable, PageRenderer.PageActionListener {
    private PageRenderer _renderer;
    private String _name;
    private String _desc;
    
    public PageRendererTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri);
    }
    
    private static final SyndieURI _dummyURI = SyndieURI.createMessage(new Hash(new byte[Hash.HASH_LENGTH]), Long.MAX_VALUE, 0);
    private void show(String htmlText, String title, String desc) {
        _name = title;
        _desc = desc;
        reconfigItem();
        List pages = new ArrayList();
        pages.add(htmlText);
        List attachments = new ArrayList();
        List attachmentOrder = new ArrayList();

        MessageInfo msgInfo = new MessageInfo();
        msgInfo.setURI(_dummyURI);
        msgInfo.setTargetChannel(_dummyURI.getScope());
        msgInfo.setTargetChannelId(Long.MAX_VALUE);
        msgInfo.setScopeChannelId(Long.MAX_VALUE);
        msgInfo.setAuthorChannelId(Long.MAX_VALUE);
        msgInfo.setInternalId(Long.MAX_VALUE);
        msgInfo.setMessageId(_dummyURI.getMessageId().longValue());
        msgInfo.setPageCount(1);
        
        _renderer.renderPage(new PageRendererSourceMem(getBrowser(), null, msgInfo, pages, attachments, attachmentOrder), _dummyURI);
    }
    
    protected void initComponents() {
        getRoot().setLayout(new FillLayout());
        _renderer = new PageRenderer(getRoot(), true, getBrowser());
        _renderer.setListener(this);
        
        getBrowser().getThemeRegistry().register(this);
        getBrowser().getTranslationRegistry().register(this);
        
        String name = getURI().getString("name");
        if (name == null) name = "";
        String desc = name;
        String body = getURI().getString("body");
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
        getBrowser().view(SyndieURI.createScope(scope));
    }
    public void viewScopeMetadata(PageRenderer renderer, Hash scope) {
        getBrowser().view(getBrowser().createMetaURI(scope));
    }
    public void view(PageRenderer renderer, SyndieURI uri) {
        getBrowser().view(uri);
    }
    public void bookmark(PageRenderer renderer, SyndieURI uri) {
        getBrowser().bookmark(uri);
    }
    public void privateReply(PageRenderer renderer, Hash author, SyndieURI msg) {
        getBrowser().view(getBrowser().createPostURI(author, msg, true));
    }

    public void replyToForum(PageRenderer renderer, Hash forum, SyndieURI msg) {
        getBrowser().view(getBrowser().createPostURI(forum, msg));
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
}
