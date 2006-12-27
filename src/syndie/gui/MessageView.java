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
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.MessageThreadBuilder;

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
    private Button _headerAuthorAction;
    private Menu _authorMenu;
    private MenuItem _authorMenuViewMsgs;
    private MenuItem _authorMenuViewMeta;
    private MenuItem _authorMenuBookmark;
    private MenuItem _authorMenuReplyPrivate;
    private MenuItem _authorMenuBan;
    private Label _headerForumLabel;
    private Label _headerForum;
    private Button _headerForumAction;
    private Menu _forumMenu;
    private MenuItem _forumMenuViewMsgs;
    private MenuItem _forumMenuViewMeta;
    private MenuItem _forumMenuBookmark;
    private MenuItem _forumMenuReplyPrivate;
    private MenuItem _forumMenuReplyPublic;
    private MenuItem _forumMenuBan;
    private Label _headerDateLabel;
    private Label _headerDate;
    private MessageFlagBar _headerFlags;
    private Label _headerTags;
    
    private PageRenderer _body;
    
    private Composite _footer;
    private Label _footerPageLabel;
    private Combo _footerPage;
    private Label _footerAttachmentLabel;
    private Combo _footerAttachment;
    private Label _footerReferenceLabel;
    private Combo _footerReference;
    private Label _footerThreadLabel;
    private Combo _footerThread;

    private AttachmentPreviewPopup _attachmentPopup;
    private ManageReferenceChooserPopup _refPopup;
    
    private SyndieURI _uri;
    private int _page;
    private Hash _author;
    private Hash _target;
    private int _footerThreadCurrentIndex;
    
    /** ReferenceNode items for those going into the footerReference combo (depth first traversal of the trees) */
    private List _refs;
    /** ReferenceNode items for the roots of those trees */
    private List _refRoots;
    /** SyndieURI for each of the _footerThread elements */
    private List _threadURIs;
    
    public MessageView(BrowserControl browser, Composite parent, SyndieURI uri) {
        _browser = browser;
        _client = browser.getClient();
        _parent = parent;
        _uri = uri;
        _refs = new ArrayList();
        _refRoots = new ArrayList();
        _threadURIs = new ArrayList();
        _footerThreadCurrentIndex = 0;
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
        if (_attachmentPopup != null)
            _attachmentPopup.dispose();
        if (_refPopup != null)
            _refPopup.dispose();
        _avatar.disposeImage();
    }
    
    public String getTitle() { 
        String rv = _headerSubject.getText();
        return _browser.getTranslationRegistry().getText(T_TITLE_PREFIX, "msg: ") + rv;
    }

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
        _browser.getUI().debugMessage("showPage: uri: " + _uri);
        _headerFlags.setMessage(msg);
        if (msg == null) {
            _root.setVisible(true);
            _avatar.disposeImage();
            _headerSubject.setText("");
            _headerAuthor.setText("");
            _headerForum.setText("");
            _headerDate.setText("");
            _headerTags.setText("");
            _author = null;
            _target = null;
        } else {
            if (msg.getPassphrasePrompt() != null) {
                _root.setVisible(false);
                PassphrasePrompt prompt = new PassphrasePrompt(_browser, _root.getShell(), false);
                prompt.setPassphrasePrompt(msg.getPassphrasePrompt());
                prompt.setPassphraseListener(new PassphrasePrompt.PassphraseListener() {
                    public void promptComplete(String passphraseEntered, String promptEntered) {
                        reimport(passphraseEntered);
                    }
                    public void promptAborted() { _browser.unview(_uri); }
                });
                prompt.open();
            } else {
                _root.setVisible(true);
            }
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
            ((GridData)_headerAuthorAction.getLayoutData()).exclude = !showAuthor;
            ((GridData)_headerForum.getLayoutData()).horizontalSpan = (showAuthor ? 1 : 4);
            
            _headerAuthorLabel.setVisible(showAuthor);
            _headerAuthor.setVisible(showAuthor);
            _headerAuthorAction.setVisible(showAuthor);
            
            String date = Constants.getDate(msg.getMessageId());
            String impDate = Constants.getDate(_client.getMessageImportDate(msg.getInternalId()));
            _headerDate.setText(date + " [" + impDate + "]");
            
            Set tags = new TreeSet(msg.getPublicTags());
            tags.addAll(msg.getPrivateTags());
            StringBuffer buf = new StringBuffer();
            for (Iterator iter = tags.iterator(); iter.hasNext(); ) {
                String str = (String)iter.next();
                str = str.trim();
                if (str.length() > 0) {
                    buf.append(str);
                    if (iter.hasNext())
                        buf.append(", ");
                }
            }
            _headerTags.setText(buf.toString());
            
            _author = (authorChan != null ? authorChan.getChannelHash() : msg.getTargetChannel());
            _target = msg.getTargetChannel();
        }
        
        updateFooter(msg);
        
        SyndieURI uri = SyndieURI.createMessage(_uri.getScope(), _uri.getMessageId().longValue(), _page);
        _body.renderPage(new PageRendererSource(_browser), uri);
        _root.layout(true, true);
    }
    
    private static final String T_REIMPORT_ERR_TITLE = "syndie.gui.messageview.reimporterrtitle";
    private static final String T_REIMPORT_ERR_MSG = "syndie.gui.messageview.reimporterrmsg";
    private void reimport(final String passphrase) {
        JobRunner.instance().enqueue(new Runnable() {
            public void run() {
                final boolean ok = _browser.reimport(_uri, passphrase);
                Display.getDefault().asyncExec(new Runnable() { 
                   public void run() {
                       MessageBox box = null;
                       if (!ok) {
                           box = new MessageBox(_root.getShell(), SWT.ICON_ERROR | SWT.YES | SWT.NO);
                           box.setText(_browser.getTranslationRegistry().getText(T_REIMPORT_ERR_TITLE, "Passphrase incorrect"));
                           box.setMessage(_browser.getTranslationRegistry().getText(T_REIMPORT_ERR_MSG, "The message could not be reimported - the passphrase was not correct.  Would you like to try again?"));
                           int rc = box.open();
                           if (rc == SWT.YES)
                               showPage();
                           else
                               _browser.unview(_uri);
                           return;
                       } else {
                           showPage();
                       }
                   }
                });
            }
        });
    }
    
    private void updateFooter(MessageInfo msg) {
        _footer.setRedraw(false);
        boolean footerNecessary = false;
        _footerAttachment.removeAll();
        if (msg != null) {
            int cnt = msg.getAttachmentCount();
            if (cnt > 0)
                footerNecessary = true;
            for (int i = 0; i < cnt; i++) {
                Properties cfg = _client.getMessageAttachmentConfig(msg.getInternalId(), i);
                StringBuffer buf = new StringBuffer();
                buf.append((i+1) + ": ");
                if (cfg != null) {
                    String name = cfg.getProperty(Constants.MSG_ATTACH_NAME);
                    String desc = cfg.getProperty(Constants.MSG_ATTACH_DESCRIPTION);
                    String type = cfg.getProperty(Constants.MSG_ATTACH_CONTENT_TYPE);
                    int bytes = _client.getMessageAttachmentSize(msg.getInternalId(), i);
        
                    if (name != null) {
                        buf.append(name.trim()).append(" ");
                        if (desc != null)
                            buf.append("- ");
                    }
                    if (desc != null)
                        buf.append(desc.trim()).append(" ");
                    
                    buf.append("[").append((bytes+1023)/1024).append("KB]");
                }
                _footerAttachment.add(buf.toString());
            }
        }
        _footerAttachmentLabel.setEnabled((msg != null) && (_footerAttachment.getItemCount() > 0));
        _footerAttachment.setEnabled((msg != null) && (_footerAttachment.getItemCount() > 0));
        
        int cur = 0;
        if (_footerPage.getItemCount() > 0)
            cur = _footerPage.getSelectionIndex();
        _footerPage.removeAll();
        if (msg != null) {
            int pages = msg.getPageCount();
            if (pages > 1)
                footerNecessary = true;
            for (int i = 0; i < pages; i++)
                _footerPage.add(Integer.toString(i+1));
            _footerPage.select(cur);
        }
        _footerPageLabel.setEnabled((msg != null) && (_footerPage.getItemCount() > 1));
        _footerPage.setEnabled((msg != null) && (_footerPage.getItemCount() > 1));
        
        _refs.clear();
        _refRoots.clear();
        _footerReference.removeAll();
        if (msg != null) {
            List refs = msg.getReferences();
            if (refs != null) {
                if (refs.size() > 0)
                    footerNecessary = true;
                for (int i = 0; i < refs.size(); i++) {
                    ReferenceNode ref = (ReferenceNode)refs.get(i);
                    _refRoots.add(ref);
                    addRef(ref, "");
                }
            }
        }
        _footerReferenceLabel.setEnabled((msg != null) && (_footerReference.getItemCount() > 0));
        _footerReference.setEnabled((msg != null) && (_footerReference.getItemCount() > 0));
        
        _footerThread.removeAll();
        _threadURIs.clear();
        if (msg != null)
            buildThread(msg);
        _footerThreadLabel.setEnabled(_footerThread.getItemCount() > 1);
        _footerThread.setEnabled(_footerThread.getItemCount() > 1);
        if (_footerThread.getItemCount() > 1)
            footerNecessary = true;
        
        if (footerNecessary) {
            ((GridData)_footer.getLayoutData()).exclude = false;
            _footer.setVisible(true);
        } else {
            ((GridData)_footer.getLayoutData()).exclude = true;
            _footer.setVisible(false);
        }
        _footer.setRedraw(true);
    }
    
    private void addRef(ReferenceNode ref, String parentText) {
        String txt = null;
        if (parentText.length() > 0)
            txt = parentText + " > " + ref.getName();
        else
            txt = ref.getName();
        _footerReference.add(txt);
        _refs.add(ref);
        for (int i = 0; i < ref.getChildCount(); i++)
            addRef(ref.getChild(i), txt);
    }
    
    /** 
     * populate the thread drop down with parents and replies.  this is ported from 
     * the TextUI's ReadMenu.buildThread
     */
    private void buildThread(MessageInfo msg) {
            
    /**
                [Thread:
                $position: $channel $date $subject $author
                $position: $channel $date $subject $author
                $position: $channel $date $subject $author
                $position: $channel $date $subject $author]
                (thread display includes nesting and the current position,
                e.g. "1: $hash 2006/08/01 'I did stuff' me"
                     "1.1: $hash 2006/08/02 'Liar, you did not' you"
                     "2: $hash 2006/08/03 'No more stuff talk' foo"
                     "2.1: $hash 2006/08/03 'wah wah wah' you"
                     "2.1.1: $hash 2006/08/03 'what you said' me"
                     "* 2.2: $hash 2006/08/03 'message being displayed...' blah"
                     "2.2.1: $hash 2006/08/04 'you still talking?' moo")
     */
        MessageThreadBuilder builder = new MessageThreadBuilder(_browser.getClient(), _browser.getUI());
        ReferenceNode root = builder.buildThread(msg);
        List roots = new ArrayList(1);
        roots.add(root);
        ThreadWalker walker = new ThreadWalker(msg);
        ReferenceNode.walk(roots, walker);
        int idx = walker.getCurrentIndex();
        _footerThread.select(idx);
    }
    
    private class ThreadWalker implements ReferenceNode.Visitor {
        private MessageInfo _currentMessage;
        private int _nodes;
        private int _currentIndex;
        public ThreadWalker(MessageInfo msg) { _currentMessage = msg; _nodes = 0; _currentIndex = -1; }
        public int getCurrentIndex() { return _currentIndex; }
        public void visit(ReferenceNode node, int indent, int siblingOrder) {
            SyndieURI uri = node.getURI();
            if (uri == null) return;
            Hash channel = uri.getScope();
            Long msgId = uri.getMessageId();
            if ( (channel == null) || (msgId == null) ) return;
            //_ui.debugMessage("Walking node " + _nodes + " - " + channel.toBase64() + ":" + msgId.longValue() + " [" + node.getTreeIndex() + "/" + node.getName() + "]");
            //if (node.getParent() == null)
            //    _ui.debugMessage("parent: none");
            //else
            //    _ui.debugMessage("parent: " + node.getParent().getURI());
            //_ui.debugMessage("Child count: " + node.getChildCount());
            
            StringBuffer walked = new StringBuffer();
            
            if ( (_currentMessage != null) && (_currentMessage.getScopeChannel().equals(channel)) && (msgId.longValue() == _currentMessage.getMessageId()) ) {
                _currentIndex = _nodes;
                _footerThreadCurrentIndex = _nodes;
                walked.append("* ");
            }
            
            walked.append(node.getTreeIndex()).append(": ");
            if (node.getDescription() == null) {
                // dummy element in the tree, representing a message we don't have locally
                walked.append(node.getName());
                walked.append(" (").append(channel.toBase64().substring(0,6)).append(") ");
                String when = Constants.getDate(msgId.longValue());
                walked.append(when).append(" ");
                walked.append("[message not locally known]");
            } else {
                walked.append(node.getName());
                walked.append(" (").append(channel.toBase64().substring(0,6)).append(") ");
                String when = Constants.getDate(msgId.longValue());
                walked.append(when).append(" ");
                walked.append(node.getDescription());
            }
            _footerThread.add(walked.toString());
            _threadURIs.add(uri);
            _nodes++;
        }
    }

    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(9, false));
    
        _avatar = new ImageCanvas(_root, false);
        GridData gd = new GridData(GridData.CENTER, GridData.CENTER, false, false, 1, 3);
        gd.heightHint = Constants.MAX_AVATAR_HEIGHT;
        gd.widthHint = Constants.MAX_AVATAR_WIDTH;
        _avatar.forceSize(gd.widthHint, gd.heightHint);
        _avatar.setLayoutData(gd);
        
        _headerSubject = new Label(_root, SWT.WRAP);
        _headerSubject.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 8, 1));
        
        _headerAuthorLabel = new Label(_root, SWT.NONE);
        _headerAuthorLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _headerAuthor = new Label(_root, SWT.WRAP);
        _headerAuthor.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
        _headerAuthor.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) { _authorMenu.setVisible(true); }
            public void mouseDown(MouseEvent mouseEvent) { _authorMenu.setVisible(true); }
            public void mouseUp(MouseEvent mouseEvent) { }
        });
        
        _headerAuthorAction = new Button(_root, SWT.ARROW | SWT.DOWN);
        _headerAuthorAction.setLayoutData(new GridData(GridData.BEGINNING, GridData.FILL, true, false));
        _headerAuthorAction.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _authorMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _authorMenu.setVisible(true); }
        });
        
        _authorMenu = new Menu(_headerAuthorAction);
        _headerAuthorAction.setMenu(_authorMenu);
        
        _authorMenuViewMsgs = new MenuItem(_authorMenu, SWT.PUSH);
        _authorMenuViewMsgs.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewAuthorMsgs(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewAuthorMsgs(); }
        });
        _authorMenuViewMeta = new MenuItem(_authorMenu, SWT.PUSH);
        _authorMenuViewMeta.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewAuthorMeta(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewAuthorMeta(); }
        });
        _authorMenuBookmark = new MenuItem(_authorMenu, SWT.PUSH);
        _authorMenuBookmark.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { bookmarkAuthor(); }
            public void widgetSelected(SelectionEvent selectionEvent) { bookmarkAuthor(); }
        });
        new MenuItem(_authorMenu, SWT.SEPARATOR);
        _authorMenuReplyPrivate = new MenuItem(_authorMenu, SWT.PUSH);
        _authorMenuReplyPrivate.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { replyPrivateAuthor(); }
            public void widgetSelected(SelectionEvent selectionEvent) { replyPrivateAuthor(); }
        });
        new MenuItem(_authorMenu, SWT.SEPARATOR);
        _authorMenuBan = new MenuItem(_authorMenu, SWT.PUSH);
        _authorMenuBan.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { banAuthor(); }
            public void widgetSelected(SelectionEvent selectionEvent) { banAuthor(); }
        });

        _headerForumLabel = new Label(_root, SWT.NONE);
        _headerForumLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _headerForum = new Label(_root, SWT.WRAP);
        _headerForum.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
        _headerForum.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) { _forumMenu.setVisible(true); }
            public void mouseDown(MouseEvent mouseEvent) { _forumMenu.setVisible(true); }
            public void mouseUp(MouseEvent mouseEvent) { }
        });
        
        _headerForumAction = new Button(_root, SWT.ARROW | SWT.DOWN);
        _headerForumAction.setLayoutData(new GridData(GridData.BEGINNING, GridData.FILL, true, false));
        _headerForumAction.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _forumMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _forumMenu.setVisible(true); }
        });
                
        _forumMenu = new Menu(_headerForumAction);
        _headerForumAction.setMenu(_forumMenu);
        
        _forumMenuViewMsgs = new MenuItem(_forumMenu, SWT.PUSH);
        _forumMenuViewMsgs.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewForumMsgs(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewForumMsgs(); }
        });
        _forumMenuViewMeta = new MenuItem(_forumMenu, SWT.PUSH);
        _forumMenuViewMeta.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewForumMeta(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewForumMeta(); }
        });
        _forumMenuBookmark = new MenuItem(_forumMenu, SWT.PUSH);
        _forumMenuBookmark.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { bookmarkForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { bookmarkForum(); }
        });
        new MenuItem(_forumMenu, SWT.SEPARATOR);
        _forumMenuReplyPrivate = new MenuItem(_forumMenu, SWT.PUSH);
        _forumMenuReplyPrivate.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { replyPrivateForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { replyPrivateForum(); }
        });
        _forumMenuReplyPublic = new MenuItem(_forumMenu, SWT.PUSH);
        _forumMenuReplyPublic.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { replyPublicForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { replyPublicForum(); }
        });
        new MenuItem(_forumMenu, SWT.SEPARATOR);
        _forumMenuBan = new MenuItem(_forumMenu, SWT.PUSH);
        _forumMenuBan.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { banForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { banForum(); }
        });

        _headerDateLabel = new Label(_root, SWT.NONE);
        _headerDateLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _headerDate = new Label(_root, SWT.WRAP);
        _headerDate.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
        
        _headerFlags = new MessageFlagBar(_browser, _root, true);
        _headerFlags.getControl().setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false, 4, 1));
        
        _headerTags = new Label(_root, SWT.WRAP);
        _headerTags.setLayoutData(new GridData(GridData.END, GridData.CENTER, true, false, 4, 1));
        
        _body = new PageRenderer(_root, true, _browser);
        _body.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 9, 1));
        _body.setListener(new PageListener());
        
        _footer = new Composite(_root, SWT.NONE);
        _footer.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 9, 1));
        _footer.setLayout(new RowLayout(SWT.HORIZONTAL));
        
        // label/combo pairs are in composites so the footer's row layout wraps them together
        
        Composite c = new Composite(_footer, SWT.NONE);
        c.setLayout(new RowLayout(SWT.HORIZONTAL));
        _footerPageLabel = new Label(c, SWT.NONE);
        _footerPage = new Combo(c, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.BORDER);
        _footerPage.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pageChosen(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pageChosen(); }
        });
        
        c = new Composite(_footer, SWT.NONE);
        c.setLayout(new RowLayout(SWT.HORIZONTAL));
        _footerAttachmentLabel = new Label(c, SWT.NONE);
        _footerAttachment = new Combo(c, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.BORDER);
        _footerAttachment.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { attachmentChosen(); }
            public void widgetSelected(SelectionEvent selectionEvent) { attachmentChosen(); }
        });
        
        c = new Composite(_footer, SWT.NONE);
        c.setLayout(new RowLayout(SWT.HORIZONTAL));
        _footerReferenceLabel = new Label(c, SWT.NONE);
        _footerReference = new Combo(c, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.BORDER);
        _footerReference.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { refChosen(); }
            public void widgetSelected(SelectionEvent selectionEvent) { refChosen(); }
        });
        
        c = new Composite(_footer, SWT.NONE);
        c.setLayout(new RowLayout(SWT.HORIZONTAL));
        _footerThreadLabel = new Label(c, SWT.NONE);
        _footerThread = new Combo(c, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.BORDER);
        _footerThread.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { threadChosen(); }
            public void widgetSelected(SelectionEvent selectionEvent) { threadChosen(); }
        });
        
        //_attachmentPopup = new AttachmentPreviewPopup(_browser, _root.getShell());
        //_refPopup = new ManageReferenceChooserPopup(_browser, _root.getShell());
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
    }
    
    private void replyPrivateAuthor() {
        if (_author != null)
            _browser.view(_browser.createPostURI(_author, _uri, true));
    }
    private void replyPrivateForum() {
        if (_target != null)
            _browser.view(_browser.createPostURI(_target, _uri, true));
    }
    private void replyPublicForum() {
        if (_target != null)
            _browser.view(_browser.createPostURI(_target, _uri, false));
    }

    private void viewAuthorMsgs() {
        if (_author != null)
            _browser.view(SyndieURI.createScope(_author));
    }
    private void viewAuthorMeta() {
        if (_author != null)
            _browser.view(_browser.createMetaURI(_author));
    }
    private void bookmarkAuthor() {
        if (_author != null)
            _browser.bookmark(SyndieURI.createScope(_author));
    }
    private void banAuthor() {
        if (_author != null) {
            MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO);
            box.setMessage(_browser.getTranslationRegistry().getText(T_CONFIRMBAN_MSGAUTHOR, "Do you want to ban the author?  All of the messages they have written will be removed and you will never receive a message from them again, or any posts on their forum"));
            box.setText(_browser.getTranslationRegistry().getText(T_CONFIRMBAN_NAME, "Confirm ban"));
            int rc = box.open();
            if (rc == SWT.YES)
                _browser.getClient().ban(_author, _browser.getUI(), true);
        }
    }
    private void viewForumMsgs() {
        if (_target != null)
            _browser.view(SyndieURI.createScope(_target));
    }
    private void viewForumMeta() {
        if (_target != null)
            _browser.view(_browser.createMetaURI(_target));
    }
    private void bookmarkForum() {
        if (_target != null)
            _browser.view(SyndieURI.createScope(_target));
    }
    private void banForum() {
        if (_target != null) {
            MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO);
            box.setMessage(_browser.getTranslationRegistry().getText(T_CONFIRMBAN_MSGFORUM, "Do you want to ban the forum?  All of the messages in it will be removed and you will never receive any messages in it again, or posts written by the forum's owner"));
            box.setText(_browser.getTranslationRegistry().getText(T_CONFIRMBAN_NAME, "Confirm ban"));
            int rc = box.open();
            if (rc == SWT.YES)
                _browser.getClient().ban(_target, _browser.getUI(), true);
        }
    }

    private void pageChosen() {
        int page = _footerPage.getSelectionIndex()+1;
        _page = page;
        SyndieURI uri = SyndieURI.createMessage(_uri.getScope(), _uri.getMessageId().longValue(), page);
        _body.renderPage(new PageRendererSource(_browser), uri);
    }
    private void attachmentChosen() {
        int attachment = _footerAttachment.getSelectionIndex()+1;
        SyndieURI uri = SyndieURI.createAttachment(_uri.getScope(), _uri.getMessageId().longValue(), attachment);
        if (_attachmentPopup == null)
            _attachmentPopup = new AttachmentPreviewPopup(_browser, _root.getShell());
        _attachmentPopup.showURI(uri);
        _footerAttachment.clearSelection();
        _footerAttachment.deselectAll();
    }
    private void refChosen() { 
        if (_refPopup == null)
            _refPopup = new ManageReferenceChooserPopup(_browser, _root.getShell());
        _refPopup.setReferences(_refRoots);
        _refPopup.show();
        _footerReference.clearSelection();
        _footerReference.deselectAll();
    }
    
    private static final String T_UNKNOWN_MSG = "syndie.gui.messageview.unknown.msg";
    private static final String T_UNKNOWN_TITLE = "syndie.gui.messageview.unknown.title";
    
    private void threadChosen() {
        int idx = _footerThread.getSelectionIndex();
        if ( (idx >= 0) && (idx < _threadURIs.size()) ) {
            SyndieURI uri = (SyndieURI)_threadURIs.get(idx);
            long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
            if (msgId >= 0) {
                _browser.view(uri);
            } else {
                MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_INFORMATION | SWT.OK);
                box.setMessage(_browser.getTranslationRegistry().getText(T_UNKNOWN_MSG, "The selected message isn't known locally"));
                box.setText(_browser.getTranslationRegistry().getText(T_UNKNOWN_TITLE, "Message unkown"));
                box.open();
            }
        }
        _footerThread.select(_footerThreadCurrentIndex);
    }
    
    private class PageListener implements PageRenderer.PageActionListener {
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
        
        _footerAttachment.setFont(theme.DEFAULT_FONT);
        _footerAttachmentLabel.setFont(theme.DEFAULT_FONT);
        _footerPage.setFont(theme.DEFAULT_FONT);
        _footerPageLabel.setFont(theme.DEFAULT_FONT);
        _footerReference.setFont(theme.DEFAULT_FONT);
        _footerReferenceLabel.setFont(theme.DEFAULT_FONT);
        _footerThread.setFont(theme.DEFAULT_FONT);
        _footerThreadLabel.setFont(theme.DEFAULT_FONT);
    }
    
    private static final String T_AUTHOR = "syndie.gui.messageview.author";
    private static final String T_FORUM = "syndie.gui.messageview.forum";
    private static final String T_DATE = "syndie.gui.messageview.date";

    private static final String T_NO_AUTHOR = "syndie.gui.messageview.noauthor";
    private static final String T_NO_FORUM = "syndie.gui.messageview.noforum";
    
    private static final String T_ATTACHMENT = "syndie.gui.messageview.attachment";
    private static final String T_PAGE = "syndie.gui.messageview.page";
    private static final String T_REFERENCES = "syndie.gui.messageview.references";
    private static final String T_THREAD = "syndie.gui.messageview.thread";

    private static final String T_TITLE_PREFIX = "syndie.gui.messageview.title";

    private static final String T_CONFIRMBAN_MSGFORUM = "syndie.gui.messageview.confirmbanforum";
    private static final String T_CONFIRMBAN_MSGAUTHOR = "syndie.gui.messageview.confirmbanauthor";
    private static final String T_CONFIRMBAN_NAME = "syndie.gui.messageview.confirmbanname";
    
    private static final String T_AUTHORBAN = "syndie.gui.messageview.authorban";
    private static final String T_AUTHORBOOKMARK = "syndie.gui.messageview.authorbookmark";
    private static final String T_AUTHORVIEWMETA = "syndie.gui.messageview.authorviewmeta";
    private static final String T_AUTHORVIEWMSGS = "syndie.gui.messageview.authorviewmsgs";
    
    private static final String T_FORUMBAN = "syndie.gui.messageview.forumban";
    private static final String T_FORUMBOOKMARK = "syndie.gui.messageview.forumbookmark";
    private static final String T_FORUMVIEWMETA = "syndie.gui.messageview.forumviewmeta";
    private static final String T_FORUMVIEWMSGS = "syndie.gui.messageview.forumviewmsgs";
    
    private static final String T_AUTHORREPLYPRIV = "syndie.gui.messageview.authorreplypriv";
    private static final String T_FORUMREPLYPUB = "syndie.gui.messageview.forumreplypub";
    private static final String T_FORUMREPLYPRIV = "syndie.gui.messageview.forumreplypriv";
    
    public void translate(TranslationRegistry registry) {
        _headerAuthorLabel.setText(registry.getText(T_AUTHOR, "Author:"));
        _headerForumLabel.setText(registry.getText(T_FORUM, "Forum:"));
        _headerDateLabel.setText(registry.getText(T_DATE, "Date:"));
        _footerAttachmentLabel.setText(registry.getText(T_ATTACHMENT, "Attachments:"));
        _footerPageLabel.setText(registry.getText(T_PAGE, "Pages:"));
        _footerReferenceLabel.setText(registry.getText(T_REFERENCES, "References:"));
        _footerThreadLabel.setText(registry.getText(T_THREAD, "Thread:"));
        
        _authorMenuBan.setText(registry.getText(T_AUTHORBAN, "Ban author"));
        _authorMenuBookmark.setText(registry.getText(T_AUTHORBOOKMARK, "Bookmark author"));
        _authorMenuViewMeta.setText(registry.getText(T_AUTHORVIEWMETA, "View author's information"));
        _authorMenuViewMsgs.setText(registry.getText(T_AUTHORVIEWMSGS, "View author's forum"));
        _authorMenuReplyPrivate.setText(registry.getText(T_AUTHORREPLYPRIV, "Send a private reply to the author"));
        
        _forumMenuBan.setText(registry.getText(T_FORUMBAN, "Ban forum"));
        _forumMenuBookmark.setText(registry.getText(T_FORUMBOOKMARK, "Bookmark forum"));
        _forumMenuViewMeta.setText(registry.getText(T_FORUMVIEWMETA, "View fourm information"));
        _forumMenuViewMsgs.setText(registry.getText(T_FORUMVIEWMSGS, "View forum messages"));
        _forumMenuReplyPrivate.setText(registry.getText(T_FORUMREPLYPRIV, "Send a private reply to the forum administrators"));
        _forumMenuReplyPublic.setText(registry.getText(T_FORUMREPLYPUB, "Send a public reply to the forum"));
    }
}
