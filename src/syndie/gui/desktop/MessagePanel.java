package syndie.gui.desktop;

import java.util.ArrayList;
import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.MessageInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.Timer;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.UI;
import syndie.gui.ChannelSelectorPanel;
import syndie.gui.ColorUtil;
import syndie.gui.FireSelectionListener;
import syndie.gui.ImageUtil;
import syndie.gui.MessageTree;
import syndie.gui.MessageView;
import syndie.gui.MessageViewBody;
import syndie.gui.NavigationControl;
import syndie.gui.Theme;
import syndie.gui.ThemeRegistry;
import syndie.gui.Themeable;
import syndie.gui.Translatable;
import syndie.gui.TranslationRegistry;
import syndie.gui.URIHelper;

/**
 *
 */
public class MessagePanel extends DesktopPanel {
    private NavigationControl _navControl;
    private MessageViewBody _body;
    
    public MessagePanel(Desktop desktop, DBClient client, ThemeRegistry themes, TranslationRegistry trans, Composite parent, UI ui, NavigationControl navControl) {
        super(desktop, client, themes, trans, parent, ui, null);
        _navControl = navControl;
        initComponents();
    }
    
    public String getPanelName() { return "Message"; }
    public String getPanelDescription() { return "View message pages and attachments"; }

    private void initComponents() {
        Composite root = getRoot();
        _body = new MessageViewBody(_desktop.getDBClient(), _desktop.getUI(), _themeRegistry, _translationRegistry, _desktop.getNavControl(), URIHelper.instance(), _desktop.getBookmarkControl(), _desktop.getBanControl(), root);
    }
    
    public void shown(Desktop desktop, final SyndieURI uri, String suggestedName, String suggestedDescription) {
        super.shown(desktop, uri, suggestedName, suggestedDescription);
        if ( (uri == null) || (uri.getScope() == null) || (uri.getMessageId() == null) ) return;
        final Timer timer = new Timer("show message", _ui);
        JobRunner.instance().enqueue(new Runnable() { 
            public void run() {
                timer.addEvent("async run");
                final long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                timer.addEvent("getMsgId");
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
    private void shown(SyndieURI uri, long msgId, MessageInfo msg, Timer timer) {
        _body.viewMessage(msg, 1, timer);
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
    private static final String T_TOGGLEREAD= "syndie.gui.desktop.messagepanel.toggleread";
    class SouthEdge extends DesktopEdge implements Themeable, Translatable {
        private Button _replyToForum;
        private Button _replyToAuthor;
        private Button _replyToAdmins;
        private Button _toggleRead;
        public SouthEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        private void initComponents() {
            Composite root = getEdgeRoot();
            root.setLayout(new FillLayout(SWT.HORIZONTAL));
            
            _replyToForum = new Button(root, SWT.PUSH);
            _replyToAuthor = new Button(root, SWT.PUSH);
            _replyToAdmins = new Button(root, SWT.PUSH);
            _toggleRead = new Button(root, SWT.PUSH);
            
            _translationRegistry.register(SouthEdge.this);
            _themeRegistry.register(SouthEdge.this);
        }
        public void updateActions(SyndieURI uri, long msgId, MessageInfo msg) {}
        public void translate(TranslationRegistry trans) {
            _replyToForum.setText(trans.getText(T_REPLYTOFORUM, "Reply to forum"));
            _replyToAuthor.setText(trans.getText(T_REPLYTOAUTHOR, "Reply to author"));
            _replyToAdmins.setText(trans.getText(T_REPLYTOADMINS, "Reply to admins"));
            _toggleRead.setText(trans.getText(T_TOGGLEREAD, "Mark as read"));
        }
        public void applyTheme(Theme theme) { 
            _replyToForum.setFont(theme.BUTTON_FONT);
            _replyToAuthor.setFont(theme.BUTTON_FONT);
            _replyToAdmins.setFont(theme.BUTTON_FONT);
            _toggleRead.setFont(theme.BUTTON_FONT);
        }
    }

    class NorthEdge extends DesktopEdge implements Themeable {
        private Label _authorAvatar;
        private Label _authorName;
        private Label _forumAvatar;
        private Label _forumName;
        private Label _subject;
        private Label _date;
        private SyndieURI _currentURI;
        
        public NorthEdge(Composite edge, UI ui) {
            super(edge, ui);
            _currentURI = null;
            initComponents();
        }
        private void initComponents() {
            Composite root = NorthEdge.this.getEdgeRoot();
            GridLayout gl = new GridLayout(4, false);
            //gl.horizontalSpacing = 0;
            gl.verticalSpacing = 0;
            gl.marginHeight = 0;
            gl.marginWidth = 0;
            root.setLayout(gl);
            
            _authorAvatar = new Label(root, SWT.NONE);
            _authorAvatar.setImage(null);
            GridData gd = new GridData(64, SWT.DEFAULT);
            gd.verticalSpan = 2;
            gd.grabExcessVerticalSpace = true;
            gd.horizontalAlignment = GridData.CENTER;
            gd.verticalAlignment = GridData.CENTER;
            _authorAvatar.setLayoutData(gd); // the frame pegs the height at 64
            
            _authorName = new Label(root, SWT.SINGLE);
            _authorName.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, true));
            
            _forumName = new Label(root, SWT.SINGLE);
            _forumName.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, true));
            
            _forumAvatar = new Label(root, SWT.NONE);
            _forumAvatar.setImage(null);
            gd = new GridData(64, SWT.DEFAULT);
            gd.verticalSpan = 2;
            gd.grabExcessVerticalSpace = true;
            gd.horizontalAlignment = GridData.CENTER;
            gd.verticalAlignment = GridData.CENTER;
            _forumAvatar.setLayoutData(gd); // the frame pegs the height at 64
            
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
            _authorName.setForeground(black);
            _forumName.setForeground(black);
            _subject.setForeground(black);
            _date.setForeground(black);
            root.setBackground(white);
            root.setForeground(black);
            _themeRegistry.register(NorthEdge.this);
        }
        public void updateMeta(final SyndieURI uri, final long msgId, final MessageInfo msg) {
            if ( (uri == null) || (uri.getScope() == null) || (uri.getMessageId() == null) ) return;
            if ( (_currentURI != null) && (_currentURI.equals(uri)) )
                return; // no change
            
            getEdgeRoot().setRedraw(false);
            JobRunner.instance().enqueue(new Runnable() {
                public void run() {
                    Long messageId = uri.getMessageId();
                    // we could do the expensive MessageView.calculateSubject here instead
                    final String subj = msg.getSubject();
                    final long authorId = msg.getAuthorChannelId();
                    final long forumId = msg.getTargetChannelId();
                    
                    final String authorName = _client.getChannelName(authorId);
                    final Hash authorHash = _client.getChannelHash(authorId);
                    final byte authorAvatar[] = _client.getChannelAvatar(authorId);
                    
                    final String forumName = (authorId != forumId ? _client.getChannelName(forumId) : null);
                    final Hash forumHash = (authorId != forumId ? msg.getTargetChannel() : null);
                    final byte forumAvatar[] = (authorId != forumId ? _client.getChannelAvatar(forumId) : null);
                    
                    final long importDate = _client.getMessageImportDate(msgId);
                    
                    final String when = Constants.getDate(messageId.longValue()) + " [" + Constants.getDate(importDate) + "]";
                    
                    Display.getDefault().asyncExec(new Runnable() {
                        public void run() {
                            ImageUtil.dispose(_authorAvatar.getImage());
                            ImageUtil.dispose(_forumAvatar.getImage());
                            
                            _currentURI = uri;
                            
                            if (authorAvatar != null) {
                                Image img = ImageUtil.createImage(authorAvatar);
                                _authorAvatar.setImage(img);
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
                            if (forumAvatar != null) {
                                Image img = ImageUtil.createImage(forumAvatar);
                                _forumAvatar.setImage(img);
                            }
                            
                            if (subj != null)
                                _subject.setText(subj);
                            else 
                                _subject.setText("");
                            
                            _date.setText(when);
                            
                            getEdgeRoot().layout(true, true);
                            getEdgeRoot().setRedraw(true);
                        }
                    });
                }
            });
        }
        public void applyTheme(Theme theme) {
            _authorName.setFont(theme.SHELL_FONT);
            _forumName.setFont(theme.SHELL_FONT);
            _subject.setFont(theme.SHELL_FONT);
            _date.setFont(theme.SHELL_FONT);
            getEdgeRoot().layout(true, true);
        }
    }
    
    class EastEdge extends DesktopEdge implements Themeable, Translatable {
        public EastEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        private void initComponents() {
            Composite root = getEdgeRoot();
            root.setLayout(new FillLayout(SWT.VERTICAL));
            
            _translationRegistry.register(EastEdge.this);
            _themeRegistry.register(EastEdge.this);
        }
        public void updateNav(SyndieURI uri, long msgId, MessageInfo msg) {}
        public void translate(TranslationRegistry trans) {}
        public void applyTheme(Theme theme) {}
    }
}
