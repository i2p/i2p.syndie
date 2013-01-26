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
import org.eclipse.swt.custom.CTabFolder2Listener;
import org.eclipse.swt.custom.CTabFolderEvent;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.MenuListener;
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
import org.eclipse.swt.widgets.Text;

import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.Timer;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.MessageThreadBuilder;
import syndie.db.ThreadAccumulatorJWZ;
import syndie.db.ThreadBuilder;
import syndie.db.ThreadMsgId;
import syndie.db.UI;
import syndie.util.DateTime;
import syndie.util.StringUtil;


/**
 *  Contains the top header section and a MessageViewBody.
 */
public class MessageView extends BaseComponent implements Translatable, Themeable {
    private final NavigationControl _navControl;
    private final URIControl _uriControl;
    private final BookmarkControl _bookmarkControl;
    private final BanControl _banControl;
    private final DataCallback _dataCallback;
    private final Composite _parent;
    private Composite _root;
    
    private ImageCanvas _avatar;
    private Label _headerSubject;
    private Button _headerGoToNextInThread;
    private Button _headerGoToPrevInThread;
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
    private MessageViewBody _messageViewBody;
    
    //private MessagePreview _preview;
    
    private final SyndieURI _uri;
    private int _page;
    private Hash _author;
    private Hash _target;
    private boolean _enabled;
    private long _msgId;
    
    public MessageView(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl navControl, URIControl uriControl, BookmarkControl bookmarkControl, BanControl ban, Composite parent, SyndieURI uri, DataCallback dataCallback) {
        super(client, ui, themes, trans);
        _enabled = false;
        _navControl = navControl;
        _uriControl = uriControl;
        _bookmarkControl = bookmarkControl;
        _banControl = ban;
        _dataCallback = dataCallback;
        _parent = parent;
        _uri = uri;
        _msgId = -1;
        Long page = uri.getPage();
        if (page == null)
            _page = 1;
        else
            _page = page.intValue();
        Timer timer = new Timer("view page " + Integer.toHexString(System.identityHashCode(this)), _ui);
        initComponents(timer);
        showPage(timer);
        timer.complete();
    }
    
    public Control getControl() { return _root; }
    void enable() { 
        // wtf.  workaround for swt3.3M4 on linux/gtk where it fires the first button it finds
        // as part of an extra event when hitting return on a MessageTree.  by adding a new
        // event, this lets us ignore any previous events (since they're executed sequentially)
        _root.getDisplay().asyncExec(new Runnable() {
            public void run() { _enabled = true; }
        });
    }
    
    public void dispose() {
        _headerFlags.dispose();
        //if (_preview != null)
        //    _preview.dispose();
        _messageViewBody.dispose();
        _avatar.disposeImage();
    }
    
    public String getTitle() { 
        String rv = _headerSubject.getText();
        //return _translationRegistry.getText("msg: ") + rv;
        return rv;
    }
    
    public void viewPage(int page) {
        _page = page;
        _messageViewBody.switchPage(page);
        //showPage();
        //_footerPage.select(_page-1);
    }

    public void viewAttachment(int attachment) {
        if (attachment <= 0) return; // 1-indexed
        _messageViewBody.switchAttachment(attachment);
    }
    
    public boolean isKnownLocally() { return _author != null; }

    private MessageInfo _msg;

    private MessageInfo getMessage() {
        if ( (_uri == null) || (_uri.getScope() == null) )
            return null;
        long chanId = _client.getChannelId(_uri.getScope());
        long msgId = _client.getMessageId(chanId, _uri.getMessageId().longValue());
        if (msgId == _msgId) {
            return _msg;
        } else {
            MessageInfo msg = _client.getMessage(chanId, _uri.getMessageId());
            if (msg != null)
                _msgId = msg.getInternalId();
            _msg = msg;
            return msg;
        }
    }
    
    private void showPage(Timer timer) {
        int pageCount = 0;
        List refs = null;
        timer.addEvent("showing page");
        MessageInfo msg = getMessage();
        timer.addEvent("message loaded");
        _ui.debugMessage("showPage: uri: " + _uri);
        _headerFlags.setMessage(msg);
        timer.addEvent("header flags set");
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
                _ui.debugMessage("Password Required");
                _root.setVisible(false);
                PassphrasePrompt prompt = new PassphrasePrompt(_client, _ui, _themeRegistry, _translationRegistry, _root.getShell(), false);
                prompt.setPassphrasePrompt(msg.getPassphrasePrompt());
                prompt.setPassphraseListener(new PassphrasePrompt.PassphraseListener() {
                    public void promptComplete(String passphraseEntered, String promptEntered) {
                        reimport(passphraseEntered);
                    }
                    public void promptAborted() { _navControl.unview(_uri); }
                });
                prompt.open();
            } else {
                _root.setVisible(true);
                if (MessageTree.shouldMarkReadOnView(_client))
                    _client.markMessageRead(msg.getInternalId());
            }
            // perhaps we should check for the message avatar too...
            byte authorAvatar[] = _client.getChannelAvatar(msg.getAuthorChannelId());
            timer.addEvent("avatar data read");
            if (authorAvatar != null) {
                Image authorAvatarImg = ImageUtil.createImage(authorAvatar);
                if (authorAvatarImg != null)
                    _avatar.setImage(authorAvatarImg);
                else
                    _avatar.disposeImage();
            } else {
                _avatar.disposeImage();
            }
            timer.addEvent("avatar set");
            
            if (_avatar.getImage() == null) {
                _avatar.setVisible(false);
                _avatar.forceSize(1, Constants.MAX_AVATAR_HEIGHT);
            } else {
                _avatar.setVisible(true);
                _avatar.forceSize(Constants.MAX_AVATAR_WIDTH, Constants.MAX_AVATAR_HEIGHT);
            }
            String subject = calculateSubject(msg);
            _headerSubject.setText(subject);
            timer.addEvent("subject set");
            
            ChannelInfo authorChan = _client.getChannel(msg.getAuthorChannelId());
            timer.addEvent("author determined");
            Theme theme = _themeRegistry.getTheme();
            if (authorChan != null) {
                String name = authorChan.getName();
                _headerAuthor.setText(UIUtil.displayName(name, authorChan.getChannelHash()));
                _headerAuthor.setFont(theme.DEFAULT_FONT);
            } else {
                _headerAuthor.setText(_translationRegistry.getText("Unspecified"));
                _headerAuthor.setFont(theme.MSG_UNKNOWN_FONT);
            }
            timer.addEvent("author set");
            
            ChannelInfo forumChan = _client.getChannel(msg.getTargetChannelId());
            timer.addEvent("forum determined");
            if (forumChan != null) {
                String name = forumChan.getName();
                _headerForum.setText(UIUtil.displayName(name, forumChan.getChannelHash()));
                _headerForum.setFont(theme.DEFAULT_FONT);
            } else {
                _headerForum.setText(_translationRegistry.getText("Unspecified"));
                _headerForum.setFont(theme.MSG_UNKNOWN_FONT);
            }
            timer.addEvent("forum set");
            
            boolean showAuthor = (msg.getTargetChannelId() != msg.getAuthorChannelId());
            ((GridData)_headerAuthorLabel.getLayoutData()).exclude = !showAuthor;
            ((GridData)_headerAuthor.getLayoutData()).exclude = !showAuthor;
            ((GridData)_headerAuthorAction.getLayoutData()).exclude = !showAuthor;
            ((GridData)_headerForum.getLayoutData()).horizontalSpan = (showAuthor ? 1 : 4);
            
            _headerAuthorLabel.setVisible(showAuthor);
            _headerAuthor.setVisible(showAuthor);
            _headerAuthorAction.setVisible(showAuthor);
            
            String date = DateTime.getDateTime(msg.getMessageId());
            String impDate = DateTime.getDate(_client.getMessageImportDate(msg.getInternalId()));
            _headerDate.setText(date + "    [" + getText("Imported") + ' ' + impDate + ']');
            
            Set tags = new TreeSet(msg.getPublicTags());
            tags.addAll(msg.getPrivateTags());
            StringBuilder buf = new StringBuilder();
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
            
            timer.addEvent("target set");
        }
        
        configGoTo(new ArrayList(), null, 1); // blank it out to start
        _messageViewBody.viewMessage(msg, _page, timer);
        
        //SyndieURI uri = SyndieURI.createMessage(_uri.getScope(), _uri.getMessageId().longValue(), _page);
        //_body.renderPage(new PageRendererSource(_browser), uri);
        _root.layout(true, true);
        timer.addEvent("layout complete");
    }
    
    private String calculateSubject(MessageInfo msg) { return calculateSubject(_client, _translationRegistry, msg); }

    public static String calculateSubject(DBClient client, TranslationRegistry trans, MessageInfo msg) {
        if (msg != null) {
            String subject = msg.getSubject();
            if ( (subject != null) && (subject.trim().length() > 0) )
                return subject;
            // message has no subject... try its ancestors (and append a "re: ")
            List ancestors = msg.getHierarchy();
            for (int i = 0; i < ancestors.size(); i++) {
                SyndieURI uri = (SyndieURI)ancestors.get(i);
                long msgId = client.getMessageId(uri.getScope(), uri.getMessageId());
                if (msgId >= 0) {
                    String ancestorSubject = client.getMessageSubject(msgId);
                    if ( (ancestorSubject != null) && (ancestorSubject.trim().length() > 0) ) {
                        if (StringUtil.lowercase(ancestorSubject).startsWith("re:"))
                            return ancestorSubject;
                        else // TODO translate "re" ?
                            return "re: " + ancestorSubject.trim();
                    }
                }
            }
        }
        // no ancestors with a subject found
        // TODO return null and have caller set italic font?
        return trans.getText("No subject");
    }

    static String calculateSubject(DBClient client, UI ui, TranslationRegistry trans, SyndieURI uri) {
        long msgId = -1;
        if (uri != null)
            msgId = client.getMessageId(uri.getScope(), uri.getMessageId());
        return calculateSubject(client, ui, trans, msgId, uri.getScope(), uri.getMessageId(), true);
    }

    static String calculateSubject(DBClient client, UI ui, TranslationRegistry trans, long msgId, Hash scope, Long messageId, boolean mayHaveSubject) {
        if (msgId >= 0) {
            String subject = null;
            if (mayHaveSubject)
                subject = client.getMessageSubject(msgId);
            if ( (subject != null) && (subject.trim().length() > 0) )
                return subject;
            ThreadMsgId tmi = new ThreadMsgId(msgId);
            tmi.scope = scope;
            tmi.messageId = (messageId != null ? messageId.longValue() : -1);
            Map ancestors = new HashMap();
            ThreadAccumulatorJWZ.buildAncestors(client, ui, tmi, ancestors);
            List ids = (List)ancestors.get(tmi);
            if (ids != null) {
                for (int i = 0; i < ids.size(); i++) {
                    ThreadMsgId id = (ThreadMsgId)ids.get(i);
                    if (id.msgId >= 0) {
                        subject = client.getMessageSubject(id.msgId);
                        if ( (subject != null) && (subject.trim().length() > 0) ) {
                            if (StringUtil.lowercase(subject).startsWith("re:"))
                                return subject;
                            else
                                return "re: " + subject.trim();
                        }
                    }
                }
            }
        }
        // no ancestors with a subject found
        // TODO return null and have caller set italic font?
        return trans.getText("No subject");
    }
    
    /*
    public void toggleMaxView() {
        if (_maxView != null) {
            _maxView.unmax();
        } else {
            int page = 0;
            if (_tabFolder != null) {
                page = _tabFolder.getSelectionIndex();
            }
            // page may be beyond the last page
            if (_browser.getClient().getMessagePageConfig(_msgId, page) != null) {
                SyndieURI uri = SyndieURI.createMessage(_uri.getScope(), _uri.getMessageId().longValue(), page);
                _maxView = new MaxView(uri);
            }
        }
    }
     */
    
        
    public void toggleMaxView() {
        _ui.debugMessage("toggleMaxView: msgId=" + _msgId + " msgURI=" + _uri);
        _messageViewBody.toggleMaxView();
    }
    
    public void toggleMaxEditor() { }

    /*
    private class MaxView {
        private Shell _shell;
        private PageRenderer _maxRenderer;
        
        public MaxView(SyndieURI pageURI) {
            _shell = new Shell(_root.getShell(), SWT.NO_TRIM | SWT.PRIMARY_MODAL);
            _shell.setLayout(new GridLayout(1, true));
            Button unmax = new Button(_shell, SWT.PUSH);
            unmax.setText(_browser.getTranslationRegistry().getText("Restore normal size"));
            unmax.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            
            _maxRenderer = new PageRenderer(_shell, true, _browser);
            _maxRenderer.getComposite().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
            
            _maxRenderer.renderPage(new PageRendererSource(_browser), pageURI);
            
            unmax.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { unmax(); }
                public void widgetSelected(SelectionEvent selectionEvent) { unmax(); }
                private void fire() { unmax(); }
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
        
        public void unmax() {
            MaxView pv = _maxView;
            _maxView = null;
            pv.dispose();
        }
        public void dispose() { 
            _shell.dispose();
            _maxRenderer.dispose();
        }
    }
     */
    
    private void reimport(final String passphrase) {
        JobRunner.instance().enqueue(new Runnable() {
            public void run() {
                final boolean ok = _client.reimport(_uri, passphrase);
                _ui.debugMessage("Reimport done, success? " + ok);
                Display.getDefault().asyncExec(new Runnable() { 
                   public void run() {
                       MessageBox box = null;
                       if (!ok) {
                           box = new MessageBox(_root.getShell(), SWT.ICON_ERROR | SWT.YES | SWT.NO);
                           box.setText(_translationRegistry.getText("Passphrase incorrect"));
                           box.setMessage(_translationRegistry.getText("The message could not be reimported - the passphrase was not correct.  Would you like to try again?"));
                           int rc = box.open();
                           if (rc == SWT.YES) {
                               _navControl.unview(_uri);
                               _navControl.view(_uri);
                               //showPage();
                           } else {
                               _navControl.unview(_uri);
                           }
                       } else {
                           _navControl.unview(_uri);
                           _navControl.view(_uri);
                           // FIXME call back to StatusBar.refreshPBE() somehow to update the button
                           //showPage();
                       }
                   }
                });
            }
        });
    }
    
    private void initComponents(Timer timer) {
        timer.addEvent("initializing");
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
        _forumMenu.addMenuListener(new MenuListener() {
            public void menuHidden(MenuEvent menuEvent) {}
            public void menuShown(MenuEvent menuEvent) {
                // if the user isn't authorized to post a reply to the forum, don't offer to let them
                if (MessagePreview.allowedToReply(_client, _msgId))
                    _forumMenuReplyPublic.setEnabled(true);
                else
                    _forumMenuReplyPublic.setEnabled(false);
            }
        });

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
        
        Composite buttons = new Composite(_root, SWT.NONE);
        buttons.setLayoutData(new GridData(GridData.END, GridData.FILL, false, false));
        buttons.setLayout(new FillLayout(SWT.HORIZONTAL));
        
        _headerGoToPrevInThread = new Button(buttons, SWT.PUSH);
        _headerGoToNextInThread = new Button(buttons, SWT.PUSH);
        
        _headerReply = new Button(buttons, SWT.PUSH);
        _headerReply.setImage(ImageUtil.ICON_REPLYMESSAGE);
        
        _headerReplyMenu = new Menu(_headerReply);
        _headerReplyMenu.addMenuListener(new MenuListener() {
            public void menuHidden(MenuEvent menuEvent) {}
            public void menuShown(MenuEvent menuEvent) {
                // if the user isn't authorized to post a reply to the forum, don't offer to let them
                if (MessagePreview.allowedToReply(_client, _msgId))
                    _headerReplyForumPublic.setEnabled(true);
                else
                    _headerReplyForumPublic.setEnabled(false);
            }
        });

        _headerReply.setMenu(_headerReplyMenu);
        _headerReply.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { fire(); }
            public void widgetSelected(SelectionEvent evt) { fire(); }
            private void fire() {
                if (!_enabled) {
                    // gobble the extra event. see enable()
                    return;
                }
                _headerReplyMenu.setVisible(true);
            }
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
        
        _headerFlags = ComponentBuilder.instance().createMessageFlagBar(_root, true);
        _headerFlags.getControl().setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false, 4, 1));
        
        _headerTags = new Label(_root, SWT.WRAP);
        _headerTags.setLayoutData(new GridData(GridData.END, GridData.CENTER, true, false, 4, 1));
        
        _bodyContainer = new Composite(_root, SWT.NONE);
        _bodyContainer.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 9, 1));
        _bodyContainer.setLayout(new FillLayout());
        
        _messageViewBody = new MessageViewBody(_client, _ui, _themeRegistry, _translationRegistry, _navControl, _uriControl, _bookmarkControl, _banControl, _bodyContainer, _dataCallback);
        _messageViewBody.addThreadLoadedListener(new MessageViewBody.ThreadLoadedListener() {
            public void threadLoaded(List threadReferenceNodes, ThreadMsgId curMsg, int threadSize) {
                configGoTo(threadReferenceNodes, curMsg, threadSize);
            }
        });
        
        _headerReplyMenu.setVisible(false);
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
        timer.addEvent("initialized");
    }
    
    private static class ThreadLocator implements ReferenceNode.Visitor {
        private ThreadMsgId _target;
        private SyndieURI _prevURI;
        private SyndieURI _nextURI;
        private boolean _found;
        public ThreadLocator(ThreadMsgId target) { 
            _target = target;
            _prevURI = null;
            _nextURI = null;
            _found = false;
        }
        public void visit(ReferenceNode node, int depth, int siblingOrder) {
            if (_nextURI != null) return;
            if (_found) {
                _nextURI = node.getURI();
                return;
            }
            if (node.getUniqueId() == _target.msgId) {
                _found = true;
            } else {
                _prevURI = node.getURI();
            }
        }
        public SyndieURI getPrevURI() { return _prevURI; }
        public SyndieURI getNextURI() { return _nextURI; }
    }
    
    public void configGoTo(List threadReferenceNodes, ThreadMsgId curMsg, int threadSize) {
        // enable/disable based on whether there is another message in the thread
        if (threadSize > 1) {
            ThreadLocator loc = new ThreadLocator(curMsg);
            ReferenceNode.walk(threadReferenceNodes, loc);
            final SyndieURI nextURI = loc.getNextURI();
            final SyndieURI prevURI = loc.getPrevURI();
            if (nextURI != null) {
                _headerGoToNextInThread.setEnabled(true);
                _headerGoToNextInThread.addSelectionListener(new SelectionListener() {
                    public void widgetDefaultSelected(SelectionEvent evt) {
                        if (!_enabled) {
                            // gobble the extra event. see enable()
                            return;
                        }
                        _navControl.unview(_uri);
                        _navControl.view(nextURI);
                    }
                    public void widgetSelected(SelectionEvent evt) {
                        if (!_enabled) {
                            // gobble the extra event. see enable()
                            return;
                        }
                        _navControl.unview(_uri);
                        _navControl.view(nextURI);
                    }
                });
            } else {
                _headerGoToNextInThread.setEnabled(false);
            }
            if (prevURI != null) {
                _headerGoToPrevInThread.setEnabled(true);
                _headerGoToPrevInThread.addSelectionListener(new SelectionListener() {
                    public void widgetDefaultSelected(SelectionEvent evt) {
                        if (!_enabled) {
                            // gobble the extra event. see enable()
                            return;
                        }
                        _navControl.unview(_uri);
                        _navControl.view(prevURI);
                    }
                    public void widgetSelected(SelectionEvent evt) {
                        if (!_enabled) {
                            // gobble the extra event. see enable()
                            return;
                        }
                        _navControl.unview(_uri);
                        _navControl.view(prevURI);
                    }
                });
            } else {
                _headerGoToPrevInThread.setEnabled(false);
            }
        } else {
            _headerGoToNextInThread.setEnabled(false);
            _headerGoToPrevInThread.setEnabled(false);
        }
    }
    
    private void replyPrivateAuthor() {
        if (_author != null)
            _navControl.view(_uriControl.createPostURI(_author, _uri, true));
    }
    private void replyPrivateForum() {
        if (_target != null)
            _navControl.view(_uriControl.createPostURI(_target, _uri, true));
    }
    private void replyPublicForum() {
        if (_target != null)
            _navControl.view(_uriControl.createPostURI(_target, _uri, false));
    }

    private void viewAuthorMsgs() {
        if (_author != null)
            _navControl.view(SyndieURI.createScope(_author));
    }
    private void viewAuthorMeta() {
        if (_author != null)
            _navControl.view(_uriControl.createMetaURI(_author));
    }
    private void bookmarkAuthor() {
        if (_author != null)
            _bookmarkControl.bookmark(SyndieURI.createScope(_author));
    }
    private void banAuthor() {
        if (_author != null) {
            if (_banControl.ban(_author))
                _navControl.unview(_uri);
        }
    }
    private void viewForumMsgs() {
        if (_target != null)
            _navControl.view(SyndieURI.createScope(_target));
    }
    private void viewForumMeta() {
        if (_target != null)
            _navControl.view(_uriControl.createMetaURI(_target));
    }
    private void bookmarkForum() {
        if (_target != null)
            _bookmarkControl.bookmark(SyndieURI.createScope(_target));
    }
    private void banForum() {
        if (_target != null) {
            if (_banControl.ban(_target)) {
                _navControl.unview(_uri);
            }
        }
    }
    
    public void applyTheme(Theme theme) {
        _headerSubject.setFont(theme.DEFAULT_FONT);
        _headerGoToNextInThread.setFont(theme.BUTTON_FONT);
        _headerGoToPrevInThread.setFont(theme.BUTTON_FONT);
        _headerReply.setFont(theme.BUTTON_FONT);
        _headerAuthorLabel.setFont(theme.DEFAULT_FONT);
        _headerAuthor.setFont(theme.DEFAULT_FONT);
        _headerForumLabel.setFont(theme.DEFAULT_FONT);
        _headerForum.setFont(theme.DEFAULT_FONT);
        _headerDateLabel.setFont(theme.DEFAULT_FONT);
        _headerDate.setFont(theme.DEFAULT_FONT);
        _headerTags.setFont(theme.DEFAULT_FONT);
    }
    

    

    
    
    
    
    
    public void translate(TranslationRegistry registry) {
        _headerReply.setText(registry.getText("Reply") + "...");
        _headerAuthorLabel.setText(registry.getText("Author") + ':');
        _headerForumLabel.setText(registry.getText("Forum") + ':');
        _headerDateLabel.setText(registry.getText("Date") + ':');
        
        _headerReplyAuthorPrivate.setText(registry.getText("Send a private reply to the author"));
        _headerReplyForumPrivate.setText(registry.getText("Send a private reply to the forum administrators"));
        _headerReplyForumPublic.setText(registry.getText("Send a public reply to the forum"));
        
        // duplicate the reply menu 
        _authorMenuReplyPrivate.setText(registry.getText("Send a private reply to the author"));
        _forumMenuReplyPrivate.setText(registry.getText("Send a private reply to the forum administrators"));
        _forumMenuReplyPublic.setText(registry.getText("Send a public reply to the forum"));
        
        _authorMenuBan.setText(registry.getText("Ban author"));
        _authorMenuBookmark.setText(registry.getText("Bookmark author"));
        _authorMenuViewMeta.setText(registry.getText("View author's information"));
        _authorMenuViewMsgs.setText(registry.getText("View author's forum"));
        
        _forumMenuBan.setText(registry.getText("Ban forum"));
        _forumMenuBookmark.setText(registry.getText("Bookmark forum"));
        _forumMenuBookmark.setImage(ImageUtil.ICON_ADDBOOKMARK);
        _forumMenuViewMeta.setText(registry.getText("View forum profile"));
        _forumMenuViewMeta.setImage(ImageUtil.ICON_VIEW);
        _forumMenuViewMsgs.setText(registry.getText("View forum messages"));
        
        _headerGoToNextInThread.setText(registry.getText("Next"));
        _headerGoToPrevInThread.setText(registry.getText("Prev"));
    }
}
