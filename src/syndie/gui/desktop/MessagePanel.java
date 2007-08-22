package syndie.gui.desktop;

import java.util.ArrayList;
import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.MessageInfo;
import syndie.data.MessageIterator;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.Timer;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.ThreadBuilder;
import syndie.db.ThreadMessageIterator;
import syndie.db.ThreadMsgId;
import syndie.db.ThreadReferenceNode;
import syndie.db.UI;
import syndie.gui.BookmarkDnDHelper;
import syndie.gui.ChannelSelectorPanel;
import syndie.gui.ColorUtil;
import syndie.gui.FireSelectionListener;
import syndie.gui.ImageCanvas;
import syndie.gui.ImageUtil;
import syndie.gui.MessageFlagBar;
import syndie.gui.MessageTree;
import syndie.gui.MessageView;
import syndie.gui.MessageViewBody;
import syndie.gui.NavigationControl;
import syndie.gui.PassphrasePrompt;
import syndie.gui.Theme;
import syndie.gui.ThemeRegistry;
import syndie.gui.Themeable;
import syndie.gui.Translatable;
import syndie.gui.TranslationRegistry;
import syndie.gui.URIHelper;

/**
 *
 */
public class MessagePanel extends DesktopPanel implements Translatable, Themeable, DBClient.MessageStatusListener {
    private NavigationControl _navControl;
    private MessageViewBody _body;
    private MessageInfo _msg;
    private ThreadReferenceNode _messageThread;
    private MessageIterator _messageIterator;
    
    public MessagePanel(Desktop desktop, DBClient client, ThemeRegistry themes, TranslationRegistry trans, Composite parent, UI ui, NavigationControl navControl) {
        super(desktop, client, themes, trans, parent, ui, null);
        _navControl = navControl;
        initComponents();
    }
    
    public String getPanelName() { return (_msg != null ? _msg.getSubject() : "Message"); }
    public String getPanelDescription() { return "View message pages and attachments"; }
    public SyndieURI getOriginalURI() { return (_msg != null ? _msg.getURI() : null); }
    
    protected void dispose() {
        _client.removeMessageStatusListener(this);
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        _body.dispose();
        super.dispose();
    }

    private void initComponents() {
        Composite root = getRoot();
        _body = new MessageViewBody(_desktop.getDBClient(), _desktop.getUI(), _themeRegistry, _translationRegistry, _desktop.getNavControl(), URIHelper.instance(), _desktop.getBookmarkControl(), _desktop.getBanControl(), root);
        _client.addMessageStatusListener(this);
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }

    public void messageStatusUpdated(final long msgId, final int status) {
        if ( (_msg != null) && (_msg.getInternalId() == msgId) ) {
            Display.getDefault().asyncExec(new Runnable() { 
                public void run() { 
                    ((SouthEdge)_edgeSouth).updateActions(_msg.getURI(), msgId, _msg);
                }
            });
        }
    }
    
    public boolean canShow(SyndieURI uri) { 
        boolean rv = false;
        if (super.canShow(uri)) return true;
        if (uri.isChannel() && (uri.getMessageId() != null)) {
            if (_msg == null) return true; // we aren't yet initialized, so we can show any pages
            if (uri.getScope().equals(_msg.getScopeChannel()) && (uri.getMessageId().longValue() == _msg.getMessageId()) )
                return true;
        }
        if (true) return false; // no sharing
        
        if (uri.isChannel() && (uri.getMessageId() != null)) {
            if (_msg == null) return true; // we aren't yet initialized, so we can show any pages
            if (uri.getScope().equals(_msg.getScopeChannel()) && (uri.getMessageId().longValue() == _msg.getMessageId()) )
                return true;
            
            // should we have just one message panel, or should we have many?
            // if just one, return true here.  if we should have many panels with different messages,
            // do some work.  the following means one message panel per forum
            long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
            long target = _client.getMessageTarget(msgId);
            if (target == _msg.getTargetChannelId())
                return true;
        }
        return false;
    }
    
    public void shown(Desktop desktop, final SyndieURI uri, String suggestedName, String suggestedDescription) {
        shown(desktop, uri, suggestedName, suggestedDescription, null);
    }
    private void shown(Desktop desktop, final SyndieURI uri, String suggestedName, String suggestedDescription, MessageIterator iter) {
        super.shown(desktop, uri, suggestedName, suggestedDescription);
        if ( (uri == null) || (uri.getScope() == null) || (uri.getMessageId() == null) ) return;
        
        _messageIterator = iter;
        if (iter != null)
            _messageIterator.recenter(uri);
        
        final Timer timer = new Timer("show message", _ui);
        JobRunner.instance().enqueue(new Runnable() { 
            public void run() {
                timer.addEvent("async run");
                final long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                timer.addEvent("getMsgId");
                
                if (msgId < 0) {
                    Display.getDefault().asyncExec(new Runnable() {
                        public void run() {
                            MessageBox box = new MessageBox(getRoot().getShell(), SWT.ICON_INFORMATION | SWT.OK);
                            box.setText(_translationRegistry.getText(T_NOMSG_TITLE, "Message unknown"));
                            box.setMessage(_translationRegistry.getText(T_NOMSG, "The selected message is not known locally"));
                            _navControl.unview(uri);
                            box.open();
                            return;
                        }
                    });
                    return;
                }
                final MessageInfo msg = _client.getMessage(msgId);
                timer.addEvent("getMessage");
                Display.getDefault().asyncExec(new Runnable() { 
                    public void run() { 
                        timer.addEvent("syncRun");
                        shown(uri, msgId, msg, timer); 
                    }
                });
            }
        });
    }

    private static final String T_NOMSG_TITLE = "syndie.gui.desktop.messagepanel.nomsg.title";
    private static final String T_NOMSG = "syndie.gui.desktop.messagepanel.nomsg";

    private static final String T_UNAUTH_TITLE = "syndie.gui.desktop.messagepanel.unauth.title";
    private static final String T_UNAUTH = "syndie.gui.desktop.messagepanel.unauth";

    private void shown(final SyndieURI uri, long msgId, MessageInfo msg, Timer timer) {
        _msg = msg;        
        if (msg.getPassphrasePrompt() != null) {
            getRoot().setVisible(false);
            PassphrasePrompt prompt = new PassphrasePrompt(_client, _ui, _themeRegistry, _translationRegistry, getRoot().getShell(), false);
            prompt.setPassphrasePrompt(msg.getPassphrasePrompt());
            prompt.setPassphraseListener(new PassphrasePrompt.PassphraseListener() {
                public void promptComplete(String passphraseEntered, String promptEntered) {
                    reimport(passphraseEntered, uri);
                }
                public void promptAborted() { _navControl.unview(uri); }
            });
            prompt.open();
            return;
        } else if (msg.getReadKeyUnknown() || msg.getReplyKeyUnknown()) {
            MessageBox box = new MessageBox(getRoot().getShell(), SWT.ICON_INFORMATION | SWT.OK);
            box.setText(_translationRegistry.getText(T_UNAUTH_TITLE, "Not authorized"));
            box.setMessage(_translationRegistry.getText(T_UNAUTH, "You are not authorized to read that message"));
            _navControl.unview(uri);
            box.open();
            return;
        } else {
            getRoot().setVisible(true);
            if (MessageTree.shouldMarkReadOnView(_client))
                _client.markMessageRead(msg.getInternalId());
        }
        
        _body.viewMessage(msg, 1, timer, _messageThread);
        
        Long pageNum = uri.getPage();
        Long attachNum = uri.getAttachment();
        if (pageNum != null) {
            _body.switchPage(pageNum.intValue());
        } else if (attachNum != null) {
            _body.switchAttachment(attachNum.intValue());
        }
        
        ((SouthEdge)_edgeSouth).updateActions(uri, msgId, msg);
        timer.addEvent("actions updated");
        ((NorthEdge)_edgeNorth).updateMeta(uri, msgId, msg);
        timer.addEvent("meta updated");
        ((EastEdge)_edgeEast).updateNav(uri, msgId, msg);
        timer.addEvent("nav updated");
        timer.complete();
        
        Composite root = getRoot();
        root.layout(); //true, true);
    }
    public void hidden(Desktop desktop) {}
    
    private static final String T_REIMPORT_ERR_TITLE = "syndie.gui.desktop.messagepanel.reimporterrtitle";
    private static final String T_REIMPORT_ERR_MSG = "syndie.gui.desktop.messagepanel.reimporterrmsg";
    private void reimport(final String passphrase, final SyndieURI uri) {
        JobRunner.instance().enqueue(new Runnable() {
            public void run() {
                final boolean ok = _client.reimport(uri, passphrase);
                Display.getDefault().asyncExec(new Runnable() { 
                   public void run() {
                       MessageBox box = null;
                       if (!ok) {
                           box = new MessageBox(getRoot().getShell(), SWT.ICON_ERROR | SWT.YES | SWT.NO);
                           box.setText(_translationRegistry.getText(T_REIMPORT_ERR_TITLE, "Passphrase incorrect"));
                           box.setMessage(_translationRegistry.getText(T_REIMPORT_ERR_MSG, "The message could not be reimported - the passphrase was not correct.  Would you like to try again?"));
                           int rc = box.open();
                           if (rc == SWT.YES) {
                               _navControl.unview(uri);
                               _navControl.view(uri);
                               //showPage();
                           } else {
                               _navControl.unview(uri);
                           }
                       } else {
                           _navControl.unview(uri);
                           _navControl.view(uri);
                           //showPage();
                       }
                   }
                });
            }
        });
    }

    public void applyTheme(Theme theme) {
        if (_edgeSouth != null) ((Themeable)_edgeSouth).applyTheme(theme);
        if (_edgeNorth != null) ((Themeable)_edgeNorth).applyTheme(theme);
        if (_edgeEast != null) ((Themeable)_edgeEast).applyTheme(theme);
    }
    public void translate(TranslationRegistry registry) {
        if (_edgeSouth != null) ((Translatable)_edgeSouth).translate(registry);
        if (_edgeNorth != null) ((Translatable)_edgeNorth).translate(registry);
        if (_edgeEast != null) ((Translatable)_edgeEast).translate(registry);
    }
    
    protected void buildSouth(Composite edge) { 
        if (_edgeSouth == null) _edgeSouth = new SouthEdge(edge, _ui); 
    }
    protected void buildNorth(Composite edge) { 
        if (_edgeNorth == null) _edgeNorth = new NorthEdge(edge, _ui); 
    }
    protected void buildEast(Composite edge) { 
        if (_edgeEast == null) _edgeEast = new EastEdge(edge, _ui); 
    }

    private static final String T_REPLYTOFORUM = "syndie.gui.desktop.messagepanel.replytoforum";
    private static final String T_REPLYTOAUTHOR = "syndie.gui.desktop.messagepanel.replytoauthor";
    private static final String T_REPLYTOADMINS = "syndie.gui.desktop.messagepanel.replytoadmins";
    private static final String T_MARK_READ = "syndie.gui.desktop.messagepanel.toggleread.markread";
    private static final String T_MARK_UNREAD = "syndie.gui.desktop.messagepanel.toggleread.markunread";
    
    class SouthEdge extends DesktopEdge implements Themeable, Translatable {
        private Button _replyToForum;
        private Button _replyToAuthor;
        private Button _replyToAdmins;
        private Button _toggleRead;
        private boolean _alreadyRead;
        private long _msgId;
        public SouthEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        private void initComponents() {
            Composite root = getEdgeRoot();
            root.setLayout(new FillLayout(SWT.HORIZONTAL));
            
            _replyToForum = new Button(root, SWT.PUSH);
            _replyToForum.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    _navControl.view(URIHelper.instance().createPostURI(_msg.getTargetChannel(), _msg.getURI(), false));
                }
            });
            _replyToAuthor = new Button(root, SWT.PUSH);
            _replyToAuthor.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    Hash author = _client.getChannelHash(_msg.getAuthorChannelId());
                    _navControl.view(URIHelper.instance().createPostURI(author, _msg.getURI(), true));
                }
            });
            _replyToAdmins = new Button(root, SWT.PUSH);
            _replyToAuthor.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    _navControl.view(URIHelper.instance().createPostURI(_msg.getTargetChannel(), _msg.getURI(), true));
                }
            });
            _toggleRead = new Button(root, SWT.PUSH);
            _toggleRead.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    if (_alreadyRead) {
                        _client.markMessageUnread(_msgId);
                        _alreadyRead = false;
                        _toggleRead.setText(_translationRegistry.getText(T_MARK_READ, "Mark as read"));
                    } else {
                        _client.markMessageRead(_msgId);
                        _alreadyRead = true;
                        _toggleRead.setText(_translationRegistry.getText(T_MARK_UNREAD, "Mark as unread"));
                    }
                    getEdgeRoot().layout(true, true);
                }
            });

            translate(_translationRegistry);
            applyTheme(_themeRegistry.getTheme());
        }
        public void updateActions(SyndieURI uri, final long msgId, MessageInfo msg) {
            final long forumId = msg.getTargetChannelId();
            final long authorId = msg.getAuthorChannelId();
            _replyToForum.setEnabled(false);
            _replyToAdmins.setEnabled(authorId != forumId);
            JobRunner.instance().enqueue(new Runnable() { 
                public void run() {              
                    DBClient.ChannelCollector chans = _client.getNymChannels();
                    final boolean postable = chans.getAllIds().contains(new Long(forumId));
                    final boolean pubReply = _client.getChannelAllowPublicReplies(forumId);
                    final int status = _client.getMessageStatus(msgId);
                    Display.getDefault().asyncExec(new Runnable() { 
                        public void run() {
                            if (_toggleRead.isDisposed()) return;
                            _msgId = msgId;
                            switch (status) {
                                case DBClient.MSG_STATUS_READ:
                                    _alreadyRead = true;
                                    _toggleRead.setText(_translationRegistry.getText(T_MARK_UNREAD, "Mark as unread"));
                                    break;
                                case DBClient.MSG_STATUS_UNREAD:
                                    _alreadyRead = false;
                                    _toggleRead.setText(_translationRegistry.getText(T_MARK_READ, "Mark as read"));
                                    break;
                            }
                            _replyToForum.setEnabled(postable || pubReply);
                            
                            getEdgeRoot().layout(true, true);
                        }
                    });
                }
            });
        }
        public void translate(TranslationRegistry trans) {
            _replyToForum.setText(trans.getText(T_REPLYTOFORUM, "Reply to forum"));
            _replyToAuthor.setText(trans.getText(T_REPLYTOAUTHOR, "Reply to author"));
            _replyToAdmins.setText(trans.getText(T_REPLYTOADMINS, "Reply to admins"));
        }
        public void applyTheme(Theme theme) { 
            _replyToForum.setFont(theme.BUTTON_FONT);
            _replyToAuthor.setFont(theme.BUTTON_FONT);
            _replyToAdmins.setFont(theme.BUTTON_FONT);
            _toggleRead.setFont(theme.BUTTON_FONT);
        }
    }

    class NorthEdge extends DesktopEdge implements Themeable, Translatable {
        private ImageCanvas _authorAvatar;
        private Label _authorName;
        private ImageCanvas _forumAvatar;
        private Label _forumName;
        private MessageFlagBar _flagBar;
        private Label _subject;
        private Label _date;
        private SyndieURI _currentURI;
        
        private Hash _authorHash;
        private String _authorNameStr;
        private Hash _forumHash;
        private String _forumNameStr;
        
        private DragSource _authorAvatarSrc;
        private DragSource _authorNameSrc;
        private DragSource _forumAvatarSrc;
        private DragSource _forumNameSrc;
        
        public NorthEdge(Composite edge, UI ui) {
            super(edge, ui);
            _currentURI = null;
            initComponents();
        }
        private void initComponents() {
            Composite root = NorthEdge.this.getEdgeRoot();
            GridLayout gl = new GridLayout(5, false);
            //gl.horizontalSpacing = 0;
            gl.verticalSpacing = 0;
            gl.marginHeight = 0;
            gl.marginWidth = 0;
            root.setLayout(gl);
            
            _authorAvatar = new ImageCanvas(root, false, false, false); //Label(root, SWT.NONE);
            _authorAvatar.setImage(null);
            _authorAvatar.forceSize(64, 64);
            GridData gd = new GridData(64, SWT.DEFAULT);
            gd.verticalSpan = 2;
            gd.grabExcessVerticalSpace = true;
            gd.horizontalAlignment = GridData.CENTER;
            gd.verticalAlignment = GridData.CENTER;
            _authorAvatar.setLayoutData(gd); // the frame pegs the height at 64
            
            _authorName = new Label(root, SWT.SINGLE);
            _authorName.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, true, 2, 1));
            
            _forumName = new Label(root, SWT.SINGLE);
            _forumName.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, true));
            
            _forumAvatar = new ImageCanvas(root, false, false, false); //Label(root, SWT.NONE);
            _forumAvatar.setImage(null);
            _forumAvatar.forceSize(64, 64);
            gd = new GridData(64, SWT.DEFAULT);
            gd.verticalSpan = 2;
            gd.grabExcessVerticalSpace = true;
            gd.horizontalAlignment = GridData.CENTER;
            gd.verticalAlignment = GridData.CENTER;
            _forumAvatar.setLayoutData(gd); // the frame pegs the height at 64
            
            _flagBar = new MessageFlagBar(_client, _ui, _themeRegistry, _translationRegistry, _desktop.getBookmarkControl(), root, true);
            _flagBar.getControl().setLayoutData(new GridData(GridData.BEGINNING, GridData.FILL, false, false));
            
            _subject = new Label(root, SWT.SINGLE);
            _subject.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, true, false));
            
            _date = new Label(root, SWT.SINGLE);
            _date.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
            
            Color black = ColorUtil.getColor("black");
            Color white = ColorUtil.getColor("white");
            
            _authorAvatar.setBackground(white);
            _forumAvatar.setBackground(white);
            _authorName.setBackground(white);
            _forumName.setBackground(white);
            _subject.setBackground(white);
            _date.setBackground(white);
            _flagBar.setBackground(white);
            _authorName.setForeground(black);
            _forumName.setForeground(black);
            _subject.setForeground(black);
            _date.setForeground(black);
            root.setBackground(white);
            root.setForeground(black);
            
            BookmarkDnDHelper.SourceProvider authorSource = new BookmarkDnDHelper.SourceProvider() {
                public SyndieURI getURI() { return SyndieURI.createScope(_authorHash); }
                public String getName() { return _authorNameStr; }
                public String getDescription() { return null; }
            };
            BookmarkDnDHelper.SourceProvider forumSource = new BookmarkDnDHelper.SourceProvider() {
                public SyndieURI getURI() { return SyndieURI.createScope(_forumHash); }
                public String getName() { return _forumNameStr; }
                public String getDescription() { return null; }
            };
            _authorAvatarSrc = BookmarkDnDHelper.initSource(_authorAvatar, authorSource);
            _authorNameSrc = BookmarkDnDHelper.initSource(_authorName, authorSource);
            _forumAvatarSrc = BookmarkDnDHelper.initSource(_forumAvatar, forumSource);
            _forumNameSrc = BookmarkDnDHelper.initSource(_forumName, forumSource);
            
            translate(_translationRegistry);
            applyTheme(_themeRegistry.getTheme());
        }
        public void updateMeta(final SyndieURI uri, final long msgId, final MessageInfo msg) {
            if ( (uri == null) || (uri.getScope() == null) || (uri.getMessageId() == null) ) return;
            if ( (_currentURI != null) && (_currentURI.equals(uri)) )
                return; // no change
            
            //getEdgeRoot().setRedraw(false);
            JobRunner.instance().enqueue(new Runnable() {
                public void run() {
                    calcMeta(uri, msgId, msg);
                }
            });
        }
        
        private void calcMeta(final SyndieURI uri, final long msgId, final MessageInfo msg) {
            Long messageId = uri.getMessageId();
            // we could do the expensive MessageView.calculateSubject here instead
            final String subj = msg.getSubject();
            final long authorId = msg.getAuthorChannelId();
            final long forumId = msg.getTargetChannelId();

            final boolean authorIsWatched = _client.isWatched(authorId);
            final boolean forumIsWatched = _client.isWatched(forumId);

            final String authorName = _client.getChannelName(authorId);
            final Hash authorHash = _client.getChannelHash(authorId);
            final byte authorAvatar[] = _client.getChannelAvatar(authorId);

            final String forumName = (authorId != forumId ? _client.getChannelName(forumId) : null);
            final Hash forumHash = (authorId != forumId ? msg.getTargetChannel() : null);
            final byte forumAvatar[] = (authorId != forumId ? _client.getChannelAvatar(forumId) : null);

            final long importDate = _client.getMessageImportDate(msgId);

            _forumHash = (authorId != forumId ? forumHash : authorHash);
            _forumNameStr = (authorId != forumId ? forumName : authorName);
            _authorHash = authorHash;
            _authorNameStr = authorName;

            final String when = Constants.getDate(messageId.longValue()) + " [" + Constants.getDate(importDate) + "]";

            Display.getDefault().asyncExec(new Runnable() {
                public void run() {
                    redrawMeta(uri, authorIsWatched, authorAvatar, authorHash, authorName, forumHash, forumName, forumAvatar, forumId, authorId, forumIsWatched, subj, when, msg);
                }
            });
        }
        
        private void redrawMeta(SyndieURI uri, boolean authorIsWatched, byte[] authorAvatar, Hash authorHash, String authorName, Hash forumHash, String forumName, byte[] forumAvatar, long forumId, long authorId, boolean forumIsWatched, String subj, String when, MessageInfo msg) {
            _authorAvatar.disposeImage();
            _forumAvatar.disposeImage();
            //ImageUtil.dispose(_authorAvatar.getImage());
            //ImageUtil.dispose(_forumAvatar.getImage());

            _currentURI = uri;
            
            Image authorImg = null;
            Image forumImg = null;

            if (authorIsWatched) {
                if (authorAvatar != null) {
                    Image img = ImageUtil.createImage(authorAvatar);
                    ///_authorAvatar.setImage(img);
                    authorImg = img;
                } else {
                    ///_authorAvatar.setImage(ImageUtil.ICON_EDITOR_BOOKMARKED_NOAVATAR);
                    authorImg = ImageUtil.ICON_EDITOR_BOOKMARKED_NOAVATAR;
                }
            } else {
                ///_authorAvatar.setImage(ImageUtil.ICON_EDITOR_NOT_BOOKMARKED);
                authorImg = ImageUtil.ICON_EDITOR_NOT_BOOKMARKED;
            }
            String name = "";
            if (authorHash != null)
                name = (authorName != null ? authorName + " " : "") + "[" + authorHash.toBase64().substring(0,6) + "]";
            _authorName.setText(name);

            if (forumHash != null)
                name = (forumName != null ? forumName + " " : "") + "[" + forumHash.toBase64().substring(0,6) + "]";
            else
                name = "";
            _forumName.setText(name);
            if (forumId != authorId) {
                if (forumIsWatched) {
                    if (forumAvatar != null) {
                        Image img = ImageUtil.createImage(forumAvatar);
                        ///_forumAvatar.setImage(img);
                        forumImg = img;
                    } else {
                        ///_forumAvatar.setImage(ImageUtil.ICON_EDITOR_NOT_BOOKMARKED);
                        forumImg = ImageUtil.ICON_EDITOR_NOT_BOOKMARKED;
                    }
                } else {
                    ///_forumAvatar.setImage(ImageUtil.ICON_EDITOR_NOT_BOOKMARKED);
                    forumImg = ImageUtil.ICON_EDITOR_NOT_BOOKMARKED;
                }
            }

            if (subj != null)
                _subject.setText(subj);
            else 
                _subject.setText("");

            _date.setText(when);

            setMetaImages(authorImg, forumImg);
            _flagBar.setMessage(msg, false);
            layoutMeta();
        }
        private void setMetaImages(Image authorImg, Image forumImg) {
            _authorAvatar.setImage(authorImg);
            _forumAvatar.setImage(forumImg);
            _authorAvatar.redraw();
            _forumAvatar.redraw();
        }
        private void layoutMeta() {
            getEdgeRoot().layout(true, true);
            ////getEdgeRoot().layout(new Control[] { _authorAvatar, _forumAvatar, _authorName, _forumName, _subject, _date, _flagBar.getControl() });
            //getEdgeRoot().setRedraw(true);
        }
        
        public void applyTheme(Theme theme) {
            _authorName.setFont(theme.SHELL_FONT);
            _forumName.setFont(theme.SHELL_FONT);
            _subject.setFont(theme.SHELL_FONT);
            _date.setFont(theme.SHELL_FONT);
            getEdgeRoot().layout(true, true);
        }
        public void translate(TranslationRegistry trans) {}
        public void dispose() {
            ImageUtil.dispose(_authorAvatar.getImage());
            ImageUtil.dispose(_forumAvatar.getImage());
            if (_authorAvatarSrc != null) _authorAvatarSrc.dispose();
            if (_authorNameSrc != null) _authorNameSrc.dispose();
            if (_forumAvatarSrc != null) _forumAvatarSrc.dispose();
            if (_forumNameSrc != null) _forumNameSrc.dispose();
            _flagBar.dispose();
            super.dispose();
        }
    }
    
    class EastEdge extends DesktopEdge implements Themeable, Translatable {
        private Button _navNextNew;
        private Button _navPrevNew;
        private Button _navNextViaThread;
        private Button _navPrevViaThread;
        private Button _navNextInThread;
        private Button _navPrevInThread;
        private Button _navNextThread;
        private Button _navPrevThread;
        private Button _navForum;
        private MessageIterator _iter;
        public EastEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        private void initComponents() {
            Composite root = getEdgeRoot();
            GridLayout gl = new GridLayout(1, true);
            gl.marginHeight = 0;
            gl.marginWidth = 0;
            gl.verticalSpacing = 0;
            gl.horizontalSpacing = 0;
            root.setLayout(gl);
            
            _navNextNew = new Button(root, SWT.PUSH);
            _navNextNew.setImage(ImageUtil.ICON_MSGNAV_NEXTNEW);
            _navNextNew.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
            _navNextNew.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    SyndieURI uri = _iter.getNextNew();
                    if (uri != null)
                        shown(_desktop, uri, null, null, _iter);
                }
            });
            
            _navPrevNew = new Button(root, SWT.PUSH);
            _navPrevNew.setImage(ImageUtil.ICON_MSGNAV_PREVNEW);
            _navPrevNew.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
            _navPrevNew.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    SyndieURI uri = _iter.getPreviousNew();
                    if (uri != null) {
                        shown(_desktop, uri, null, null, _iter);
                    }
                }
            });
            
            _navNextViaThread = new Button(root, SWT.PUSH);
            _navNextViaThread.setImage(ImageUtil.ICON_MSGNAV_NEXTVIATHREAD);
            _navNextViaThread.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
            _navNextViaThread.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    SyndieURI uri = _iter.getNextViaThread();
                    if (uri != null)
                        shown(_desktop, uri, null, null, _iter);
                }
            });
            
            _navPrevViaThread = new Button(root, SWT.PUSH);
            _navPrevViaThread.setImage(ImageUtil.ICON_MSGNAV_PREVVIATHREAD);
            _navPrevViaThread.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
            _navPrevViaThread.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    SyndieURI uri = _iter.getPreviousViaThread();
                    if (uri != null)
                        shown(_desktop, uri, null, null, _iter);
                }
            });
            
            _navNextInThread = new Button(root, SWT.PUSH);
            _navNextInThread.setImage(ImageUtil.ICON_MSGNAV_NEXTINTHREAD);
            _navNextInThread.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
            _navNextInThread.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    SyndieURI uri = _iter.getNextInThread();
                    if (uri != null)
                        shown(_desktop, uri, null, null, _iter);
                }
            });
            
            _navPrevInThread = new Button(root, SWT.PUSH);
            _navPrevInThread.setImage(ImageUtil.ICON_MSGNAV_PREVINTHREAD);
            _navPrevInThread.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
            _navPrevInThread.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    SyndieURI uri = _iter.getPreviousInThread();
                    if (uri != null)
                        shown(_desktop, uri, null, null, _iter);
                }
            });
            
            _navNextThread = new Button(root, SWT.PUSH);
            _navNextThread.setImage(ImageUtil.ICON_MSGNAV_NEXTTHREAD);
            _navNextThread.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
            _navNextThread.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    SyndieURI uri = _iter.getNextThread();
                    if (uri != null)
                        shown(_desktop, uri, null, null, _iter);
                }
            });
            
            _navPrevThread = new Button(root, SWT.PUSH);
            _navPrevThread.setImage(ImageUtil.ICON_MSGNAV_PREVTHREAD);
            _navPrevThread.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
            _navPrevThread.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    SyndieURI uri = _iter.getPreviousThread();
                    if (uri != null)
                        shown(_desktop, uri, null, null, _iter);
                }
            });
            
            _navForum = new Button(root, SWT.PUSH);
            _navForum.setImage(ImageUtil.ICON_MSGNAV_FORUM);
            _navForum.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
            _navForum.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    SyndieURI uri = _iter.getMessageTreeURI();
                    if (uri != null) {
                        _navControl.view(uri);
                        close();
                    }
                }
            });
            
            translate(_translationRegistry);
            applyTheme(_themeRegistry.getTheme());
        }
        public void updateNav(final SyndieURI uri, final long msgId, final MessageInfo msg) {
            JobRunner.instance().enqueue(new Runnable() {
                public void run() {
                    _iter = getIterator(uri, msgId, msg);
                    Display.getDefault().asyncExec(new Runnable() {
                        public void run() {
                            updateNav(_iter);
                        }
                    });
                }
            });
        }
        private void updateNav(MessageIterator iter) {
            if (_navNextInThread.isDisposed()) return;
            _navNextInThread.setEnabled(iter.getNextInThread() != null);
            _navPrevInThread.setEnabled(iter.getPreviousInThread() != null);
            _navNextViaThread.setEnabled(iter.getNextViaThread() != null);
            _navPrevViaThread.setEnabled(iter.getPreviousViaThread() != null);
            _navNextThread.setEnabled(iter.getNextThread() != null);
            _navPrevThread.setEnabled(iter.getPreviousThread() != null);
            _navNextNew.setEnabled(iter.getNextNew() != null);
            _navPrevNew.setEnabled(iter.getPreviousNew() != null);
            _navForum.setEnabled(iter.getMessageTreeURI() != null);
        }
        public void translate(TranslationRegistry registry) {
            _navNextNew.setToolTipText(registry.getText(T_NAV_NEXT_NEW, "Next unread message"));
            _navPrevNew.setToolTipText(registry.getText(T_NAV_PREV_NEW, "Previous unread message"));
            _navNextViaThread.setToolTipText(registry.getText(T_NAV_NEXT_VIA_THREAD, "Next message (by threads)"));
            _navPrevViaThread.setToolTipText(registry.getText(T_NAV_PREV_VIA_THREAD, "Previous message (by threads)"));
            _navNextInThread.setToolTipText(registry.getText(T_NAV_NEXT_IN_THREAD, "Next message in the current thread only"));
            _navPrevInThread.setToolTipText(registry.getText(T_NAV_PREV_IN_THREAD, "Previous message in the current thread only"));
            _navNextThread.setToolTipText(registry.getText(T_NAV_NEXT_THREAD, "Beginning of the next thread"));
            _navPrevThread.setToolTipText(registry.getText(T_NAV_PREV_THREAD, "Beginning of the previous thread"));
            _navForum.setToolTipText(registry.getText(T_NAV_FORUM, "Jump back to the thread tree"));
        }
        public void applyTheme(Theme theme) {}
    }

    private static final String T_NAV_NEXT_NEW = "syndie.gui.desktop.messagepanel.nav.next.new";
    private static final String T_NAV_PREV_NEW = "syndie.gui.desktop.messagepanel.nav.prev.new";
    private static final String T_NAV_NEXT_VIA_THREAD = "syndie.gui.desktop.messagepanel.nav.next.via.thread";
    private static final String T_NAV_PREV_VIA_THREAD = "syndie.gui.desktop.messagepanel.nav.prev.via.thread";
    private static final String T_NAV_NEXT_IN_THREAD = "syndie.gui.desktop.messagepanel.nav.next.in.thread";
    private static final String T_NAV_PREV_IN_THREAD = "syndie.gui.desktop.messagepanel.nav.prev.in.thread";
    private static final String T_NAV_NEXT_THREAD = "syndie.gui.desktop.messagepanel.nav.next.thread";
    private static final String T_NAV_PREV_THREAD = "syndie.gui.desktop.messagepanel.nav.prev.thread";
    private static final String T_NAV_FORUM = "syndie.gui.desktop.messagepanel.nav.forum";

    /**
     * run this outside the gui thread to fetch the iterator to retrieve an iterator for
     * navigating. two quirks:
     * - if multiple forums are open that contain the same message, the traversal will work 
     *   against the first open forum's nodes.  this only matters when dealing with message
     *   tree panels that are browsing multiple forums at a time.
     * - the traversal of a message that is within an open forum goes according to that forum's 
     *   filters, even if it excludes some messages in the thread.  however, the traversal of 
     *   a message that is NOT within an open forum will pull the full thread (but not offer
     *   any traversal to other threads in that forum)
     *
     */
    private MessageIterator getIterator(SyndieURI uri, long msgId, MessageInfo msg) {
        if (_messageIterator != null) return _messageIterator;
        Hash forum = msg.getTargetChannel();
        List panels = _desktop.getPanels();
        for (int i = 0; i < panels.size(); i++) {
            DesktopPanel panel = (DesktopPanel)panels.get(i);
            if (panel instanceof MessageTreePanel) {
                MessageTreePanel mtp = (MessageTreePanel)panel;
                MessageIterator iter = mtp.getIterator(uri);
                if (iter != null) {
                    _messageThread = iter.getThreadRoot();
                    return iter;
                }
            }
        }
        
        // no panels contained the message, so it must have been launched through a link
        // or direct access.  build up a simple iterator that only operates against the
        // message's thread
        SyndieURI treeURI = SyndieURI.createScope(msg.getTargetChannel());
        ThreadBuilder builder = new ThreadBuilder(_client, _ui);
        ThreadMsgId tmi = new ThreadMsgId(msgId);
        tmi.scope = msg.getScopeChannel();
        tmi.authorScopeId = msg.getAuthorChannelId();
        tmi.messageId = msg.getMessageId();
        ThreadReferenceNode root = builder.buildThread(tmi);
        _messageThread = root;
        ThreadMessageIterator iter = new ThreadMessageIterator(root, treeURI);
        iter.recenter(msgId);
        return iter;
    }
}
