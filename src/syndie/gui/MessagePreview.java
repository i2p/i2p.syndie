package syndie.gui;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.HashSet;
import java.util.Set;
import java.util.Map;
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
import org.eclipse.swt.widgets.Button;
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
public class MessagePreview implements Themeable, Translatable {
    private BrowserControl _browser;
    private DBClient _client;
    private Composite _parent;
    private Composite _root;
    
    //private Composite _header;
    private Label _headerSubject;
    private Button _headerView;
    private MessageFlagBar _headerIcons;
    private Label _headerTags;
    
    private PageRenderer _body;
    private SyndieURI _uri;
    private int _page;
    private Hash _author;
    private Hash _target;
    
    public MessagePreview(BrowserControl browser, Composite parent) {
        _browser = browser;
        _client = browser.getClient();
        _parent = parent;
        initComponents();
    }
    
    public void preview(SyndieURI uri) {
        _uri = uri;
        Long page = uri.getPage();
        if (page == null)
            _page = 1;
        else
            _page = page.intValue();
        updatePreview();
    }
    
    public Control getControl() { return _root; }
    public void dispose() { _body.dispose(); }

    private MessageInfo getMessage() {
        if ( (_uri == null) || (_uri.getScope() == null) )
            return null;
        long chanId = _client.getChannelId(_uri.getScope());
        return _client.getMessage(chanId, _uri.getMessageId());
    }
    
    private void updatePreview() {
        MessageInfo msg = getMessage();
        if (msg != null) {
            updateMeta(msg);
            _body.renderPage(new PageRendererSource(_browser), _uri);
        } else {
            _headerIcons.setMessage(null);
        }
    }
    private void updateMeta(MessageInfo msg) {
        String subj = msg.getSubject();
        if (subj != null)
            _headerSubject.setText(subj);
        else
            _headerSubject.setText("");
        
        Set tags = new TreeSet(msg.getPublicTags());
        tags.addAll(msg.getPrivateTags());
        StringBuffer buf = new StringBuffer();
        for (Iterator iter = tags.iterator(); iter.hasNext(); ) {
            buf.append((String)iter.next());
            if (iter.hasNext())
                buf.append(", ");
        }
        _headerTags.setText(buf.toString());
        
        _headerIcons.setMessage(msg);
        
        _root.layout(true);
    }
    
    private static final int ICON_HEIGHT = 16;
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(2, false));
        
        _headerView = new Button(_root, SWT.PUSH | SWT.FLAT);
        _headerView.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
        _headerView.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { 
                if (_browser != null) _browser.view(_uri);
            }
            public void widgetSelected(SelectionEvent selectionEvent) {
                if (_browser != null) _browser.view(_uri);
            }
        });
        
        _headerSubject = new Label(_root, SWT.BORDER | SWT.SINGLE | SWT.WRAP);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        _headerSubject.setLayoutData(gd);
        _headerSubject.setText("");
        
        _headerIcons = new MessageFlagBar(_browser, _root);
        gd = new GridData(GridData.BEGINNING, GridData.CENTER, false, false);
        gd.heightHint = ICON_HEIGHT;
        _headerIcons.getControl().setLayoutData(gd);
        
        _headerTags = new Label(_root, SWT.SINGLE | SWT.WRAP | SWT.READ_ONLY);
        _headerTags.setLayoutData(new GridData(GridData.END, GridData.CENTER, true, false));
        
        _body = new PageRenderer(_root, true, _browser);
        _body.getComposite().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
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
    
    private static final String T_VIEW = "syndie.gui.messagepreview.view";
    
    public void translate(TranslationRegistry registry) {
        _headerView.setText(registry.getText(T_VIEW, "View"));
    }
    
    public void applyTheme(Theme theme) {
        _headerSubject.setFont(theme.DEFAULT_FONT);
        _headerTags.setFont(theme.DEFAULT_FONT);
        _headerView.setFont(theme.BUTTON_FONT);
    }
}
