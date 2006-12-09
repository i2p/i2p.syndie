package syndie.gui;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;
import net.i2p.data.Hash;
import net.i2p.data.PrivateKey;
import net.i2p.data.SessionKey;
import net.i2p.data.SigningPrivateKey;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

/**
 *
 */
public class MessageView implements Translatable, Themeable {
    private BrowserControl _browser;
    private DBClient _client;
    private Composite _parent;
    private Composite _root;
    
    private ImageCanvas _avatar;
    private Label _headerSubject;
    private Label _headerAuthorLabel;
    private Label _headerAuthor;
    private Label _headerForumLabel;
    private Label _headerForum;
    private Label _headerDateLabel;
    private Label _headerDate;
    private MessageFlagBar _headerFlags;
    private Label _headerTags;
    
    private PageRenderer _body;
    
    private SyndieURI _uri;
    private int _page;
    private Hash _author;
    private Hash _target;
    
    public MessageView(BrowserControl browser, Composite parent, SyndieURI uri) {
        _browser = browser;
        _client = browser.getClient();
        _parent = parent;
        _uri = uri;
        Long page = uri.getPage();
        if (page == null)
            _page = 1;
        else
            _page = page.intValue();
        initComponents();
        showPage();
    }
    
    public Control getControl() { return _root; }
    
    public void dispose() {
        _headerFlags.dispose();
        _body.dispose();
    }
    
    public String getSubject() { return _headerSubject.getText(); }

    private MessageInfo getMessage() {
        if ( (_uri == null) || (_uri.getScope() == null) )
            return null;
        long chanId = _client.getChannelId(_uri.getScope());
        return _client.getMessage(chanId, _uri.getMessageId());
    }
    
    private void showPage() {
        int pageCount = 0;
        List refs = null;
        MessageInfo msg = getMessage();
        _headerFlags.setMessage(msg);
        if (msg == null) {
            _avatar.disposeImage();
            _headerSubject.setText("");
            _headerAuthor.setText("");
            _headerForum.setText("");
            _headerDate.setText("");
            _headerTags.setText("");
            _author = null;
            _target = null;
        } else {
            // perhaps we should check for the message avatar too...
            byte authorAvatar[] = _browser.getClient().getChannelAvatar(msg.getAuthorChannelId());
            if (authorAvatar != null) {
                Image authorAvatarImg = ImageUtil.createImage(authorAvatar);
                if (authorAvatarImg != null)
                    _avatar.setImage(authorAvatarImg);
                else
                    _avatar.disposeImage();
            } else {
                _avatar.disposeImage();
            }
            
            if (_avatar.getImage() == null) {
                _avatar.setVisible(false);
                _avatar.forceSize(1, Constants.MAX_AVATAR_HEIGHT);
            } else {
                _avatar.setVisible(true);
                _avatar.forceSize(Constants.MAX_AVATAR_WIDTH, Constants.MAX_AVATAR_HEIGHT);
            }
            String subject = msg.getSubject();
            if (subject == null)
                subject = "";
            _headerSubject.setText(subject);
            
            ChannelInfo authorChan = _browser.getClient().getChannel(msg.getAuthorChannelId());
            if (authorChan != null) {
                String name = authorChan.getName();
                if (name == null)
                    _headerAuthor.setText(authorChan.getChannelHash().toBase64().substring(0,6));
                else
                    _headerAuthor.setText("(" + authorChan.getChannelHash().toBase64().substring(0,6) + ") " + name);
            } else {
                _headerAuthor.setText(_browser.getTranslationRegistry().getText(T_NO_AUTHOR, "Unspecified"));
            }
            
            ChannelInfo forumChan = _browser.getClient().getChannel(msg.getTargetChannelId());
            if (forumChan != null) {
                String name = forumChan.getName();
                if (name == null)
                    _headerForum.setText(forumChan.getChannelHash().toBase64().substring(0,6));
                else
                    _headerForum.setText("(" + forumChan.getChannelHash().toBase64().substring(0,6) + ") " + name);
            } else {
                _headerForum.setText(_browser.getTranslationRegistry().getText(T_NO_FORUM, "Unspecified"));
            }
            
            boolean showAuthor = (msg.getTargetChannelId() != msg.getAuthorChannelId());
            ((GridData)_headerAuthorLabel.getLayoutData()).exclude = !showAuthor;
            ((GridData)_headerAuthor.getLayoutData()).exclude = !showAuthor;
            ((GridData)_headerForum.getLayoutData()).horizontalSpan = (showAuthor ? 1 : 3);
            
            _headerAuthorLabel.setVisible(showAuthor);
            _headerAuthor.setVisible(showAuthor);
            
            String date = Constants.getDate(msg.getMessageId());
            _headerDate.setText(date);
            
            Set tags = new TreeSet(msg.getPublicTags());
            tags.addAll(msg.getPrivateTags());
            StringBuffer buf = new StringBuffer();
            for (Iterator iter = tags.iterator(); iter.hasNext(); ) {
                buf.append(iter.next());
                if (iter.hasNext())
                    buf.append(", ");
            }
            _headerTags.setText(buf.toString());
            
            _author = (authorChan != null ? authorChan.getChannelHash() : msg.getTargetChannel());
            _target = msg.getTargetChannel();
        }
        
        SyndieURI uri = SyndieURI.createMessage(_uri.getScope(), _uri.getMessageId().longValue(), _page);
        _body.renderPage(new PageRendererSource(_browser), uri);
        _root.layout(true, true);
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(7, false));
    
        _avatar = new ImageCanvas(_root, false);
        GridData gd = new GridData(GridData.CENTER, GridData.CENTER, false, false, 1, 3);
        gd.heightHint = Constants.MAX_AVATAR_HEIGHT;
        gd.widthHint = Constants.MAX_AVATAR_WIDTH;
        _avatar.forceSize(gd.widthHint, gd.heightHint);
        _avatar.setLayoutData(gd);
        
        _headerSubject = new Label(_root, SWT.WRAP);
        _headerSubject.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 6, 1));
        
        _headerAuthorLabel = new Label(_root, SWT.NONE);
        _headerAuthorLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _headerAuthor = new Label(_root, SWT.WRAP);
        _headerAuthor.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));
        
        _headerForumLabel = new Label(_root, SWT.NONE);
        _headerForumLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _headerForum = new Label(_root, SWT.WRAP);
        _headerForum.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));
        
        _headerDateLabel = new Label(_root, SWT.NONE);
        _headerDateLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _headerDate = new Label(_root, SWT.WRAP);
        _headerDate.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
        
        _headerFlags = new MessageFlagBar(_browser, _root);
        _headerFlags.getControl().setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false, 2, 1));
        
        _headerTags = new Label(_root, SWT.WRAP);
        _headerTags.setLayoutData(new GridData(GridData.END, GridData.CENTER, true, false, 4, 1));
        
        _body = new PageRenderer(_root, true, _browser);
        _body.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 7, 1));
        _body.setListener(new PageRenderer.PageActionListener() {
            public void viewScopeMessages(PageRenderer renderer, Hash scope) {
                if (_browser != null)
                    _browser.view(SyndieURI.createScope(scope));
            }
            public void viewScopeMetadata(PageRenderer renderer, Hash scope) {
                if (_browser != null)
                    _browser.view(_browser.createManageURI(scope));
            }
            public void view(PageRenderer renderer, SyndieURI uri) {
                if (_browser != null)
                    _browser.view(uri);
            }
            public void bookmark(PageRenderer renderer, SyndieURI uri) {}
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
            public void privateReply(PageRenderer renderer, Hash author, SyndieURI msg) {
                if (_browser != null)
                    _browser.view(_browser.createPostURI(author, msg, true));
            }
            public void replyToForum(PageRenderer renderer, Hash forum, SyndieURI msg) {
                if (_browser != null)
                    _browser.view(_browser.createPostURI(forum, msg));
            }
        });
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
    }
    
    public void applyTheme(Theme theme) {
        _headerSubject.setFont(theme.DEFAULT_FONT);
        _headerAuthorLabel.setFont(theme.DEFAULT_FONT);
        _headerAuthor.setFont(theme.DEFAULT_FONT);
        _headerForumLabel.setFont(theme.DEFAULT_FONT);
        _headerForum.setFont(theme.DEFAULT_FONT);
        _headerDateLabel.setFont(theme.DEFAULT_FONT);
        _headerDate.setFont(theme.DEFAULT_FONT);
        _headerTags.setFont(theme.DEFAULT_FONT);
    }
    
    private static final String T_AUTHOR = "syndie.gui.messageview.author";
    private static final String T_FORUM = "syndie.gui.messageview.forum";
    private static final String T_DATE = "syndie.gui.messageview.date";

    private static final String T_NO_AUTHOR = "syndie.gui.messageview.noauthor";
    private static final String T_NO_FORUM = "syndie.gui.messageview.noforum";
        
    public void translate(TranslationRegistry registry) {
        _headerAuthorLabel.setText(registry.getText(T_AUTHOR, "Author:"));
        _headerForumLabel.setText(registry.getText(T_FORUM, "Forum:"));
        _headerDateLabel.setText(registry.getText(T_DATE, "Date:"));
    }
    
    /*
    //private void createRefChooser() { _refs = new ManageReferenceChooser(_stackPane, _browser, false); }
    
    // indexes into the _headerAction
    private static final int ACTION_VIEW = 1;
    private static final int ACTION_REPLY_TO_FORUM = 2;
    private static final int ACTION_REPLY_TO_AUTHOR = 3;
    private void honorAction() {
        int idx = _headerActions.getSelectionIndex();
        switch (idx) {
            case ACTION_VIEW:
                if (_browser != null)
                    _browser.view(_uri);
                break;
            case ACTION_REPLY_TO_AUTHOR:
                if (_browser != null)
                    _browser.view(_browser.createPostURI(_author, _uri, true));
                break;
            case ACTION_REPLY_TO_FORUM:
                if (_browser != null)
                    _browser.view(_browser.createPostURI(_target, _uri));
                break;
        }
        _headerActions.select(0);
    }
     */
}
