package syndie.gui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
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
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
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
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.MessageThreadBuilder;
import syndie.db.ThreadAccumulatorJWZ;
import syndie.db.ThreadBuilder;
import syndie.db.ThreadMsgId;

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
    private Button _headerReply;
    private Menu _headerReplyMenu;
    private MenuItem _headerReplyAuthorPrivate;
    private MenuItem _headerReplyForumPrivate;
    private MenuItem _headerReplyForumPublic;
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
    
    /**
     * bodyContainer either holds the _tabFolder (if there are multiple pages, refs, attachments,
     * or threads) or the one _body (otherwise) 
     */
    private Composite _bodyContainer;
    /** the tabFolder exists if there are multiple pages, refs, attachments, or threads */
    private TabFolder _tabFolder;
    /** the tabs exist only if there are multiple pages, refs, attachments, or threads */
    private TabItem _tabs[];
    /** the tabRoots are the composites for each tab */
    private Composite _tabRoots[];
    private PageRenderer _body[];
    
    private MessageTree _threadTree;
    private MessagePreview _preview;
    private ManageReferenceChooser _refTree;
    private AttachmentPreview _attachmentPreviews[];
    
    private MaxView _maxView;
    
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
        if (_body != null)
            for (int i = 0; i < _body.length; i++)
                _body[i].dispose();
        if (_threadTree != null)
            _threadTree.dispose();
        if (_preview != null)
            _preview.dispose();
        if (_refTree != null)
            _refTree.dispose();
        if (_maxView != null)
            _maxView.dispose();
        _avatar.disposeImage();
    }
    
    public String getTitle() { 
        String rv = _headerSubject.getText();
        return _browser.getTranslationRegistry().getText(T_TITLE_PREFIX, "msg: ") + rv;
    }
    
    public void viewPage(int page) {
        _page = page;
        if (_tabFolder != null)
            _tabFolder.setSelection(_tabs[page-1]);
        //showPage();
        //_footerPage.select(_page-1);
    }
    
    public boolean isKnownLocally() { return _author != null; }

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
                if (MessageTree.shouldMarkReadOnView(_browser))
                    _browser.getClient().markMessageRead(msg.getInternalId());
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
            String subject = calculateSubject(msg);
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
        
        if (_body == null)
            initBody(msg);
        
        if (_tabFolder != null) {
            if (_page > 0) {
                _tabFolder.setSelection(_tabs[_page-1]);
            } else {
                _tabFolder.setSelection(_tabs[0]);
            }
        }
        //SyndieURI uri = SyndieURI.createMessage(_uri.getScope(), _uri.getMessageId().longValue(), _page);
        //_body.renderPage(new PageRendererSource(_browser), uri);
        _root.layout(true, true);
    }
    
    private String calculateSubject(MessageInfo msg) { return calculateSubject(_browser, msg); }
    public static String calculateSubject(BrowserControl browser, MessageInfo msg) {
        if (msg != null) {
            String subject = msg.getSubject();
            if ( (subject != null) && (subject.trim().length() > 0) )
                return subject;
            // message has no subject... try its ancestors (and append a "re: ")
            List ancestors = msg.getHierarchy();
            for (int i = 0; i < ancestors.size(); i++) {
                SyndieURI uri = (SyndieURI)ancestors.get(i);
                long msgId = browser.getClient().getMessageId(uri.getScope(), uri.getMessageId());
                if (msgId >= 0) {
                    String ancestorSubject = browser.getClient().getMessageSubject(msgId);
                    if ( (ancestorSubject != null) && (ancestorSubject.trim().length() > 0) ) {
                        if (Constants.lowercase(ancestorSubject).startsWith("re:"))
                            return ancestorSubject;
                        else
                            return "re: " + ancestorSubject.trim();
                    }
                }
            }
        }
        // no ancestors with a subject found
        return browser.getTranslationRegistry().getText(T_NO_SUBJECT, "No subject");
    }
    static String calculateSubject(BrowserControl browser, SyndieURI uri) {
        if (uri != null) {
            long msgId = browser.getClient().getMessageId(uri.getScope(), uri.getMessageId());
            if (msgId >= 0) {
                String subject = browser.getClient().getMessageSubject(msgId);
                if ( (subject != null) && (subject.trim().length() > 0) )
                    return subject;
                ThreadMsgId tmi = new ThreadMsgId(msgId);
                tmi.scope = uri.getScope();
                tmi.messageId = uri.getMessageId().longValue();
                Map ancestors = new HashMap();
                ThreadAccumulatorJWZ.buildAncestors(browser.getClient(), browser.getUI(), tmi, ancestors);
                List ids = (List)ancestors.get(tmi);
                if (ids != null) {
                    for (int i = 0; i < ids.size(); i++) {
                        ThreadMsgId id = (ThreadMsgId)ids.get(i);
                        if (id.msgId >= 0) {
                            subject = browser.getClient().getMessageSubject(id.msgId);
                            if ( (subject != null) && (subject.trim().length() > 0) ) {
                                if (Constants.lowercase(subject).startsWith("re:"))
                                    return subject;
                                else
                                    return "re: " + subject.trim();
                            }
                        }
                    }
                }
            }
        }
        // no ancestors with a subject found
        return browser.getTranslationRegistry().getText(T_NO_SUBJECT, "No subject");
    }
    private static final String T_NO_SUBJECT = "syndie.gui.messageview.nosubject";
    
    private int countMessages(List nodes) {
        NodeCounter counter = new NodeCounter();
        ReferenceNode.walk(nodes, counter);
        return counter.getCount();
    }
    
    private static class NodeCounter implements ReferenceNode.Visitor {
        private int _count = 0;
        public void visit(ReferenceNode node, int depth, int siblingOrder) {
            _count++;
        }
        public int getCount() { return _count; }
    }
    
    private void initBody(MessageInfo msg) {
        if (msg == null) return;
        int pageCount = msg.getPageCount();
        List refs = msg.getReferences();
        
        ThreadBuilder builder = new ThreadBuilder(_browser.getClient(), _browser.getUI());
        List msgs = new ArrayList(1);
        ThreadMsgId id = new ThreadMsgId(msg.getInternalId());
        id.messageId = msg.getMessageId();
        id.scope = msg.getScopeChannel();
        msgs.add(builder.buildThread(id));
        int threadSize = countMessages(msgs);
        _browser.getUI().debugMessage("thread for " + _uri + ":\n" + msgs);

        int attachments = msg.getAttachmentCount();
        if ( (pageCount == 0) && (attachments == 0) && ( (refs == null) || (refs.size() <= 0) ) && (threadSize <= 1) ) {
            _body = new PageRenderer[0];
        } else if ( (pageCount == 1) && (attachments == 0) && ( (refs == null) || (refs.size() <= 0) ) && (threadSize <= 1) ) {
            // create the renderer directly in the view, no tabs
            _body = new PageRenderer[1];
            _body[0] = new PageRenderer(_bodyContainer, true, _browser);
            _body[0].setListener(new PageListener());
            SyndieURI uri = SyndieURI.createMessage(msg.getScopeChannel(), msg.getMessageId(), 1);
            _body[0].renderPage(new PageRendererSource(_browser), uri);
            _body[0].addKeyListener(new MaxViewListener(uri));
        } else {
            int tabs = pageCount + attachments;
            if ( (refs != null) && (refs.size() > 0) ) tabs++;
            if (threadSize > 1) tabs++;
            
            _browser.getUI().debugMessage("tabs: " + tabs + " pages: " + pageCount + " attach: " + attachments + " refs? " + (refs != null ? refs.size() : 0) + " threadSize: " + threadSize);
            
            _tabFolder = new TabFolder(_bodyContainer, SWT.BORDER);
            _tabs = new TabItem[tabs];
            _tabRoots = new Composite[tabs];
            _body = new PageRenderer[pageCount];
            _tabFolder.setFont(_browser.getThemeRegistry().getTheme().TAB_FONT);
            PageListener lsnr = new PageListener();
            for (int i = 0; i < pageCount; i++) {
                _tabs[i] = new TabItem(_tabFolder, SWT.NONE);
                _tabRoots[i] = new Composite(_tabFolder, SWT.NONE);
                _tabs[i].setControl(_tabRoots[i]);
                _tabs[i].setText(_browser.getTranslationRegistry().getText(T_PAGE_PREFIX, "Page ") + (i+1));
                _tabRoots[i].setLayout(new FillLayout());
                _body[i] = new PageRenderer(_tabRoots[i], true, _browser);
                _body[i].setListener(lsnr);
                SyndieURI uri = SyndieURI.createMessage(msg.getScopeChannel(), msg.getMessageId(), i+1);
                _body[i].renderPage(new PageRendererSource(_browser), uri);
                _body[i].addKeyListener(new MaxViewListener(uri));
            }
            int off = pageCount;
            if (threadSize > 1) {
                _tabs[off] = new TabItem(_tabFolder, SWT.NONE);
                _tabRoots[off] = new Composite(_tabFolder, SWT.NONE);
                _tabs[off].setControl(_tabRoots[off]);
                _tabs[off].setText(_browser.getTranslationRegistry().getText(T_TAB_THREAD, "Thread"));
                _tabRoots[off].setLayout(new FillLayout());
                if (MessageTree.shouldShowPreview(_browser)) {
                    // show a preview pane too
                    final SashForm sash = new SashForm(_tabRoots[off], SWT.VERTICAL);
                    _threadTree = new MessageTree(_browser, sash, new MessageTree.MessageTreeListener() {
                            public void messageSelected(MessageTree tree, SyndieURI uri, boolean toView) {
                                if (toView) {
                                    _browser.view(uri);
                                } else {
                                    sash.setMaximizedControl(null);
                                    _preview.preview(uri);
                                }
                            }
                            public void filterApplied(MessageTree tree, SyndieURI searchURI) {}
                    }, true);
                    _threadTree.setMessages(msgs);
                    _threadTree.select(_uri);
                    _threadTree.setFilterable(false); // no sorting/refiltering/etc.  just a thread tree
                    
                    _preview = new MessagePreview(_browser, sash);
                    sash.setWeights(new int[] { 50, 50 });
                    sash.setMaximizedControl(_threadTree.getControl());
                } else {
                    _threadTree = new MessageTree(_browser, _tabRoots[off], new MessageTree.MessageTreeListener() {
                            public void messageSelected(MessageTree tree, SyndieURI uri, boolean toView) {
                                if (toView)
                                    _browser.view(uri);
                            }
                            public void filterApplied(MessageTree tree, SyndieURI searchURI) {}
                    }, true);
                    _threadTree.setMessages(msgs);
                    _threadTree.select(_uri);
                    _threadTree.setFilterable(false); // no sorting/refiltering/etc.  just a thread tree
                }
                off++;
            }
            if ( (refs != null) && (refs.size() > 0) ) {
                _tabs[off] = new TabItem(_tabFolder, SWT.NONE);
                _tabRoots[off] = new Composite(_tabFolder, SWT.NONE);
                _tabs[off].setControl(_tabRoots[off]);
                _tabs[off].setText(_browser.getTranslationRegistry().getText(T_TAB_REFS, "References"));
                _tabRoots[off].setLayout(new FillLayout());
                _refTree = new ManageReferenceChooser(_tabRoots[off], _browser, false);
                _refTree.setReferences(refs);
                off++;
            }
            if (attachments > 0)
                _attachmentPreviews = new AttachmentPreview[attachments];
            for (int i = 0; i < attachments; i++) {
                _tabs[off+i] = new TabItem(_tabFolder, SWT.NONE);
                _tabRoots[off+i] = new Composite(_tabFolder, SWT.NONE);
                _tabs[off+i].setControl(_tabRoots[off+i]);
                _tabs[off+i].setText(_browser.getTranslationRegistry().getText(T_ATTACH_PREFIX, "Attachment ") + (i+1));
                _tabRoots[off+i].setLayout(new FillLayout());
                
                SyndieURI uri = SyndieURI.createAttachment(_uri.getScope(), _uri.getMessageId().longValue(), i+1);
                _attachmentPreviews[i] = new AttachmentPreview(_browser, _tabRoots[off+i]);
                _attachmentPreviews[i].showURI(uri);
            }
        }
        _root.layout(true, true);
    }
    private static final String T_PAGE_PREFIX = "syndie.gui.messageview.pageprefix";
    private static final String T_ATTACH_PREFIX = "syndie.gui.messageview.attachprefix";
    private static final String T_TAB_THREAD = "syndie.gui.messageview.tabthread";
    private static final String T_TAB_REFS = "syndie.gui.messageview.tabrefs";
    
    private class MaxViewListener implements KeyListener {
        private SyndieURI _pageURI;
        public MaxViewListener(SyndieURI pageURI) { _pageURI = pageURI; }
        public void keyReleased(KeyEvent evt) { }
        public void keyPressed(KeyEvent evt) {
            switch (evt.character) {
                case 0x0C: // ^L
                    if ( (evt.stateMask & SWT.MOD1) != 0) {
                        if (_maxView != null)
                            _maxView.dispose();
                        _maxView = new MaxView(_pageURI);
                        evt.doit = false;
                    }
                    break;
            }
        }
    }
    
    private static final String T_MAXVIEW_UNMAX = "syndie.gui.messageview.maxview.unmax";
    private class MaxView {
        private Shell _shell;
        private PageRenderer _maxRenderer;
        
        public MaxView(SyndieURI pageURI) {
            _shell = new Shell(_root.getShell(), SWT.NO_TRIM | SWT.PRIMARY_MODAL);
            _shell.setLayout(new GridLayout(1, true));
            Button unmax = new Button(_shell, SWT.PUSH);
            unmax.setText(_browser.getTranslationRegistry().getText(T_MAXVIEW_UNMAX, "Restore normal size"));
            unmax.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            
            _maxRenderer = new PageRenderer(_shell, true, _browser);
            _maxRenderer.getComposite().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
            
            _maxRenderer.renderPage(new PageRendererSource(_browser), pageURI);
            
            unmax.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { unmax(); }
                public void widgetSelected(SelectionEvent selectionEvent) { unmax(); }
                private void fire() { unmax(); }
            });
            
            _maxRenderer.addKeyListener(new KeyListener() {
                public void keyPressed(KeyEvent evt) {
                    switch (evt.character) {
                        case 0x0C: // ^L
                            if ( (evt.stateMask & SWT.MOD1) != 0) {
                                unmax();
                                evt.doit = false;
                            }
                            break;
                    }
                }
                public void keyReleased(KeyEvent keyEvent) {}
            });
            
            Monitor mon[] = _root.getDisplay().getMonitors();
            Rectangle rect = null;
            if ( (mon != null) && (mon.length > 1) )
                rect = mon[0].getClientArea();
            else
                rect = _root.getDisplay().getClientArea();
            _shell.setSize(rect.width, rect.height);
            _shell.setMaximized(true);
            
            _shell.addShellListener(new ShellListener() {
                public void shellActivated(ShellEvent shellEvent) {}
                public void shellClosed(ShellEvent evt) {
                    evt.doit = false;
                    unmax();
                }
                public void shellDeactivated(ShellEvent shellEvent) {}
                public void shellDeiconified(ShellEvent shellEvent) {}
                public void shellIconified(ShellEvent shellEvent) {}
            });
            
            _shell.open();
            _maxRenderer.forceFocus();
        }
        
        private void unmax() {
            MaxView pv = _maxView;
            _maxView = null;
            pv.dispose();
        }
        public void dispose() { 
            _shell.dispose();
            _maxRenderer.dispose();
        }
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
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(9, false));
    
        _avatar = new ImageCanvas(_root, false);
        GridData gd = new GridData(GridData.CENTER, GridData.CENTER, false, false, 1, 3);
        gd.heightHint = Constants.MAX_AVATAR_HEIGHT;
        gd.widthHint = Constants.MAX_AVATAR_WIDTH;
        _avatar.forceSize(gd.widthHint, gd.heightHint);
        _avatar.setLayoutData(gd);
        
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
        
        _headerSubject = new Label(_root, SWT.WRAP);
        _headerSubject.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 7, 1));
        
        _headerReply = new Button(_root, SWT.PUSH);
        _headerReply.setLayoutData(new GridData(GridData.END, GridData.FILL, false, false));
        
        _headerReplyMenu = new Menu(_headerReply);
        _headerReply.setMenu(_headerReplyMenu);
        _headerReply.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { _headerReplyMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent evt) { _headerReplyMenu.setVisible(true); }
        });
        
        _headerReplyForumPublic = new MenuItem(_headerReplyMenu, SWT.PUSH);
        _headerReplyForumPublic.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { replyPublicForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { replyPublicForum(); }
        });
        _headerReplyAuthorPrivate = new MenuItem(_headerReplyMenu, SWT.PUSH);
        _headerReplyAuthorPrivate.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { replyPrivateAuthor(); }
            public void widgetSelected(SelectionEvent selectionEvent) { replyPrivateAuthor(); }
        });
        _headerReplyForumPrivate = new MenuItem(_headerReplyMenu, SWT.PUSH);
        _headerReplyForumPrivate.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { replyPrivateForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { replyPrivateForum(); }
        });
        
        _headerFlags = new MessageFlagBar(_browser, _root, true);
        _headerFlags.getControl().setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false, 4, 1));
        
        _headerTags = new Label(_root, SWT.WRAP);
        _headerTags.setLayoutData(new GridData(GridData.END, GridData.CENTER, true, false, 4, 1));
        
        _bodyContainer = new Composite(_root, SWT.NONE);
        _bodyContainer.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 9, 1));
        _bodyContainer.setLayout(new FillLayout());
        
        _headerReplyMenu.setVisible(false);
        
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
            if (_browser.ban(_author))
                _browser.unview(_uri);
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
            _browser.bookmark(SyndieURI.createScope(_target));
    }
    private void banForum() {
        if (_target != null) {
            if (_browser.ban(_target)) {
                _browser.unview(_uri);
            }
        }
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
                _browser.view(SyndieURI.resolveRelative(_uri, uri));
        }
        public void bookmark(PageRenderer renderer, SyndieURI uri) {
            if (_browser != null)
                _browser.bookmark(uri);
        }
        public void banScope(PageRenderer renderer, Hash scope) {
            if (_browser != null) {
                if (_browser.ban(scope))
                    _browser.unview(_uri);
            }
        }
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
        public void prevPage() {
            if (_tabFolder != null) {
                int idx = _tabFolder.getSelectionIndex();
                if (idx > 0) {
                    _tabFolder.setSelection(idx-1);
                    if (idx-1 < _body.length)
                        _body[idx-1].getComposite().forceFocus();
                }
            }
        }
        public void nextPage() {
            if (_tabFolder != null) {
                int idx = _tabFolder.getSelectionIndex();
                if (idx + 1 < _tabs.length) {
                    _tabFolder.setSelection(idx+1);
                    if (idx+1 < _body.length)
                        _body[idx+1].getComposite().forceFocus();
                }
            }
        }
    }
    
    public void applyTheme(Theme theme) {
        _headerSubject.setFont(theme.DEFAULT_FONT);
        _headerReply.setFont(theme.BUTTON_FONT);
        _headerAuthorLabel.setFont(theme.DEFAULT_FONT);
        _headerAuthor.setFont(theme.DEFAULT_FONT);
        _headerForumLabel.setFont(theme.DEFAULT_FONT);
        _headerForum.setFont(theme.DEFAULT_FONT);
        _headerDateLabel.setFont(theme.DEFAULT_FONT);
        _headerDate.setFont(theme.DEFAULT_FONT);
        _headerTags.setFont(theme.DEFAULT_FONT);

        if (_tabFolder != null)
            _tabFolder.setFont(_browser.getThemeRegistry().getTheme().TAB_FONT);
    }
    
    private static final String T_REPLY = "syndie.gui.messageview.reply";
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
        _headerReply.setText(registry.getText(T_REPLY, "Reply..."));
        _headerAuthorLabel.setText(registry.getText(T_AUTHOR, "Author:"));
        _headerForumLabel.setText(registry.getText(T_FORUM, "Forum:"));
        _headerDateLabel.setText(registry.getText(T_DATE, "Date:"));
        
        _headerReplyAuthorPrivate.setText(registry.getText(T_AUTHORREPLYPRIV, "Send a private reply to the author"));
        _headerReplyForumPrivate.setText(registry.getText(T_FORUMREPLYPRIV, "Send a private reply to the forum administrators"));
        _headerReplyForumPublic.setText(registry.getText(T_FORUMREPLYPUB, "Send a public reply to the forum"));
        
        // duplicate the reply menu 
        _authorMenuReplyPrivate.setText(registry.getText(T_AUTHORREPLYPRIV, "Send a private reply to the author"));
        _forumMenuReplyPrivate.setText(registry.getText(T_FORUMREPLYPRIV, "Send a private reply to the forum administrators"));
        _forumMenuReplyPublic.setText(registry.getText(T_FORUMREPLYPUB, "Send a public reply to the forum"));
        
        _authorMenuBan.setText(registry.getText(T_AUTHORBAN, "Ban author"));
        _authorMenuBookmark.setText(registry.getText(T_AUTHORBOOKMARK, "Bookmark author"));
        _authorMenuViewMeta.setText(registry.getText(T_AUTHORVIEWMETA, "View author's information"));
        _authorMenuViewMsgs.setText(registry.getText(T_AUTHORVIEWMSGS, "View author's forum"));
        
        _forumMenuBan.setText(registry.getText(T_FORUMBAN, "Ban forum"));
        _forumMenuBookmark.setText(registry.getText(T_FORUMBOOKMARK, "Bookmark forum"));
        _forumMenuViewMeta.setText(registry.getText(T_FORUMVIEWMETA, "View forum profile"));
        _forumMenuViewMsgs.setText(registry.getText(T_FORUMVIEWMSGS, "View forum messages"));
    }
}
