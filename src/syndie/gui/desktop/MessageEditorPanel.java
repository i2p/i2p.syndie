package syndie.gui.desktop;

import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;
import syndie.gui.ColorUtil;
import syndie.gui.FireSelectionListener;
import syndie.gui.ImageUtil;
import syndie.gui.LocalMessageCallback;
import syndie.gui.MessageEditor;
import syndie.gui.PageEditor;
import syndie.gui.Theme;
import syndie.gui.ThemeRegistry;
import syndie.gui.Themeable;
import syndie.gui.Translatable;
import syndie.gui.TranslationRegistry;
import syndie.gui.URIHelper;

/**
 *
 */
public class MessageEditorPanel extends DesktopPanel implements LocalMessageCallback, MessageEditor.EditorStatusListener, Translatable, Themeable {
    private MessageEditor _editor;
    private Listener _keyListener;
    
    // if resuming
    private long _postponeId;
    private int _postponeVersion;
    // if creating a new post
    private Hash _targetForum;
    private SyndieURI _parentURI;
    private boolean _asReply;

    public MessageEditorPanel(Desktop desktop, DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite parent, long postponeId, int postponeVersion, SyndieURI origURI) {
        super(desktop, client, themes, trans, parent, ui, origURI);
        _postponeId = postponeId;
        _postponeVersion = postponeVersion;
        initComponents();
    }
    public MessageEditorPanel(Desktop desktop, DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite parent, Hash forum, SyndieURI parentMsg, boolean asReply, SyndieURI origURI) {
        super(desktop, client, themes, trans, parent, ui, origURI);
        _targetForum = forum;
        _parentURI = parentMsg;
        ui.debugMessage("Editing message replying to " + parentMsg + " in forum " + forum);
        _asReply = asReply;
        initComponents();
    }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        _editor.dispose();
        super.dispose();
    }
    
    public void shown(Desktop desktop, SyndieURI uri, String name, String description) {
        Display.getDefault().addFilter(SWT.KeyDown, _keyListener);
        super.shown(desktop, uri, name, description);
    }
    public void hidden() {
        Display.getDefault().removeFilter(SWT.KeyDown, _keyListener);
    }
    
    public String getPanelName() { return "editor"; }
    public String getPanelDescription() { return "Message editor panel"; }
    
    Hash getTargetScope() { return _editor.getForum(); }
    
    private void initComponents() {
        _keyListener = new Listener() {
            public void handleEvent(Event evt) {
                if ( (evt.character == ' ') && ((evt.stateMask & SWT.MOD3) != 0) ) { // ALT+space to preview
                    togglePreview();
                    evt.type = SWT.None;
                }
            }
        };
        
        Composite root = getRoot();
        root.setLayout(new FillLayout());
        _editor = new MessageEditor(_client, _ui, _themeRegistry, _translationRegistry, _desktop.getDataCallback(), _desktop.getNavControl(), _desktop.getBookmarkControl(), _desktop.getBanControl(), URIHelper.instance(), root, this, false, false, false);
        _editor.addStatusListener(this);

        if ( (_postponeId > 0) && (_postponeVersion >= 0) ) {
            _editor.loadState(_postponeId, _postponeVersion);
            _editor.addListener(_desktop.getLocalMessageCallback());
            _editor.configurationComplete(getOriginalURI());
        } else {
            _ui.debugMessage("message editor initialized");
            
            _editor.setParentMessage(_parentURI);
            if (_targetForum != null)
                _editor.setForum(_targetForum);
            if (_asReply)
                _editor.setAsReply(true);
            _editor.addListener(_desktop.getLocalMessageCallback());
            _editor.configurationComplete(getOriginalURI());
        }
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
        root.layout(true, true);
    }
    
    private static final String T_CONFIRM_CLOSE_TITLE = "syndie.gui.desktop.messageeditorpanel.confirm.title";
    private static final String T_CONFIRM_CLOSE_MESSAGE = "syndie.gui.desktop.messageeditorpanel.confirm.message";
    
    protected boolean allowClose() {
        if (!_editor.isModifiedSinceOpen()) return true;
        MessageBox confirm = new MessageBox(getRoot().getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO | SWT.CANCEL);
        confirm.setText(_translationRegistry.getText(T_CONFIRM_CLOSE_TITLE, "Postpone message?"));
        confirm.setMessage(_translationRegistry.getText(T_CONFIRM_CLOSE_MESSAGE, "Do you want to postpone this message to resume it later?"));
        int rc = confirm.open();
        if (rc == SWT.YES) {
            _editor.postponeMessage();
            return true;
        } else if (rc == SWT.CANCEL) {
            return false;
        } else if (rc == SWT.NO) {
            _editor.cancelMessage(false);
            return true;
        } else {
            return false;
        }
    }
    
    private void togglePreview() {
        PageEditor page = _editor.getPageEditor();
        if (page != null) {
            if (MessageEditor.TYPE_HTML.equals(page.getContentType()))
                page.toggleFullPreview();
        }
        
        ((EastEdge)_edgeEast).setPreviewText(_translationRegistry);
    }
    
    private static final String T_POSTPREVIEW_TITLE = "syndie.gui.desktop.messageeditorpanel.postpreview.title";
    private static final String T_POSTPREVIEW = "syndie.gui.desktop.messageeditorpanel.postpreview";
    
    private void post() {
        final PageEditor page = _editor.getPageEditor();
        if (page != null) {
            if (MessageEditor.TYPE_HTML.equals(page.getContentType())) {
                if (page.isPreviewShowing()) {
                    _editor.postMessage();
                } else {
                    page.toggleFullPreview(new Runnable() {
                        public void run() {
                            getRoot().getDisplay().asyncExec(new Runnable() {
                                public void run() {
                                    MessageBox box = new MessageBox(getRoot().getShell(), SWT.ICON_INFORMATION | SWT.YES | SWT.NO);
                                    box.setText(_translationRegistry.getText(T_POSTPREVIEW_TITLE, "Preview post"));
                                    box.setMessage(_translationRegistry.getText(T_POSTPREVIEW, "Is this preview what you want to post?"));
                                    _ui.debugMessage("before previewPost confirm prompt");
                                    int rc = box.open();
                                    _ui.debugMessage("after previewPost confirm prompt");
                                    if (rc == SWT.YES)
                                        _editor.postMessage();
                                    else
                                        page.toggleFullPreview();
                                }
                            });
                        }
                    });
                }
            } else {
                _editor.postMessage();
            }
        } else {
            _editor.postMessage();
        }
    }
    
    protected void buildNorth(Composite edge) { _edgeNorth = new NorthEdge(edge, _ui); }
    protected void buildEast(Composite edge) { _edgeEast = new EastEdge(edge, _ui); }
    protected void buildSouth(Composite edge) { _edgeSouth = new SouthEdge(edge, _ui); }

    public void applyTheme(Theme theme) {
        if (_edgeNorth != null) ((Themeable)_edgeNorth).applyTheme(theme);
        if (_edgeEast != null) ((Themeable)_edgeEast).applyTheme(theme);
    }
    public void translate(TranslationRegistry registry) {
        if (_edgeNorth != null) ((Translatable)_edgeNorth).translate(registry);
        if (_edgeEast != null) ((Translatable)_edgeEast).translate(registry);
        if (_edgeSouth != null) ((Translatable)_edgeSouth).translate(registry);
    }
    
    // callbacks from the message editor based on the message being posted/cancelled/postponed
    public void messageCreated(SyndieURI postedURI) {
        _desktop.getNavControl().view(postedURI);
        close();
    }
    public void messagePostponed(long postponementId) { close(); }
    public void messageCancelled() { close(); }

    // callbacks from the message editor based on status change
    public void pickPrivacyPublic() { ((NorthEdge)_edgeNorth).pickPrivacyPublic(); }
    public void pickPrivacyPBE() { ((NorthEdge)_edgeNorth).pickPrivacyPBE(); }
    public void pickPrivacyPrivate() { ((NorthEdge)_edgeNorth).pickPrivacyPrivate(); }
    public void pickPrivacyAuthorized() { ((NorthEdge)_edgeNorth).pickPrivacyAuthorized(); }
    public void forumSelected(Hash forum, long channelId, String summary, boolean isManaged) {
        ((NorthEdge)_edgeNorth).forumSelected(channelId, summary, isManaged);
    }
    public void authorSelected(Hash author, long channelId, String summary) {
        ((NorthEdge)_edgeNorth).authorSelected(channelId, summary);
    }
    public void pickPageTypeHTML(boolean isHTML) {
        ((EastEdge)_edgeEast).updatePageType(isHTML);
    }
    public void statusUpdated(int page, int pages, int attachment, int attachments, String type, boolean pageLoaded, boolean isHTML, boolean hasAncestors) {
        ((SouthEdge)_edgeSouth).statusUpdated(page, pages, attachment, attachments, type, pageLoaded, isHTML, hasAncestors);
    }
    public void attachmentsRebuilt(List attachmentData, List attachmentSummary) {}
    
    private static final String T_PRIV_PUBLIC = "syndie.gui.desktop.messageeditorpanel.privpublic";
    private static final String T_PRIV_AUTHORIZED = "syndie.gui.desktop.messageeditorpanel.privauthorized";
    private static final String T_PRIV_PBE = "syndie.gui.desktop.messageeditorpanel.privpbe";
    private static final String T_PRIV_PRIVATE = "syndie.gui.desktop.messageeditorpanel.privprivate";
    
    private static final String T_FORUMNAME = "syndie.gui.desktop.messageeditorpanel.forumname";
    private static final String T_AUTHORNAME= "syndie.gui.desktop.messageeditorpanel.authorname";
    private static final String T_PRIVACY = "syndie.gui.desktop.messageeditorpanel.privacy";
    
    private class NorthEdge extends DesktopEdge implements Themeable, Translatable {
        private Label _authorAvatar;
        private Label _authorNameLabel;
        private Label _forumAvatar;
        private Label _forumNameLabel;
        private Label _privacyIcon;
        private Label _privacyLabel;
        private Label _authorName;
        private Label _forumName;
        private Label _privacy;
        
        public NorthEdge(Composite edge, UI ui) { 
            super(edge, ui);
            initComponents();
        }
        
        private void initComponents() {
            Composite edge = getEdgeRoot();
            GridLayout gl = new GridLayout(3, true);
            gl.marginWidth = 0;
            gl.marginHeight = 0;
            edge.setLayout(gl);
            
            // forum
            Composite forumGroup = new Composite(edge, SWT.NONE);
            gl = new GridLayout(2, false);
            gl.marginWidth = 0;
            gl.marginHeight = 0;
            forumGroup.setLayout(gl);
            forumGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
            
            _forumAvatar = new Label(forumGroup, SWT.NONE);
            GridData gd = new GridData(64, 64);
            gd.verticalSpan = 2;
            gd.verticalAlignment = GridData.CENTER;
            gd.horizontalAlignment = GridData.CENTER;
            _forumAvatar.setLayoutData(gd);
            _forumAvatar.addMouseListener(new MouseListener() {
                public void mouseDoubleClick(MouseEvent mouseEvent) {
                    Hash scope = _editor.getForum();
                    if (scope != null)
                        _desktop.getNavControl().view(SyndieURI.createScope(scope));
                }
                public void mouseDown(MouseEvent mouseEvent) {}
                public void mouseUp(MouseEvent mouseEvent) { _editor.showHeaders(); }
            });
            
            _forumNameLabel = new Label(forumGroup, SWT.NONE);
            _forumNameLabel.setLayoutData(new GridData(GridData.BEGINNING, GridData.END, true, true));
            
            _forumName = new Label(forumGroup, SWT.NONE);
            _forumName.setLayoutData(new GridData(GridData.BEGINNING, GridData.END, true, true));
            
            // author
            Composite authorGroup = new Composite(edge, SWT.NONE);
            gl = new GridLayout(2, false);
            gl.marginWidth = 0;
            gl.marginHeight = 0;
            authorGroup.setLayout(gl);
            authorGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
            
            _authorAvatar = new Label(authorGroup, SWT.NONE);
            gd = new GridData(64, 64);
            gd.verticalSpan = 2;
            gd.verticalAlignment = GridData.CENTER;
            gd.horizontalAlignment = GridData.CENTER;
            _authorAvatar.setLayoutData(gd);
            _authorAvatar.addMouseListener(new MouseListener() {
                public void mouseDoubleClick(MouseEvent mouseEvent) {
                    Hash scope = _editor.getAuthor();
                    if (scope != null)
                        _desktop.getNavControl().view(SyndieURI.createScope(scope));
                }
                public void mouseDown(MouseEvent mouseEvent) {}
                public void mouseUp(MouseEvent mouseEvent) { _editor.showHeaders(); }
            });
            
            _authorNameLabel = new Label(authorGroup, SWT.NONE);
            _authorNameLabel.setLayoutData(new GridData(GridData.BEGINNING, GridData.END, true, true));
            
            _authorName = new Label(authorGroup, SWT.NONE);
            _authorName.setLayoutData(new GridData(GridData.BEGINNING, GridData.END, true, true));
            
            // privacy 
            Composite privacyGroup = new Composite(edge, SWT.NONE);
            gl = new GridLayout(2, false);
            gl.marginWidth = 0;
            gl.marginHeight = 0;
            privacyGroup.setLayout(gl);
            privacyGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
            
            _privacyIcon = new Label(privacyGroup, SWT.NONE);
            gd = new GridData(64, 64);
            gd.verticalSpan = 2;
            gd.verticalAlignment = GridData.CENTER;
            gd.horizontalAlignment = GridData.CENTER;
            _privacyIcon.setLayoutData(gd);
            _privacyIcon.addMouseListener(new MouseListener() {
                public void mouseDoubleClick(MouseEvent mouseEvent) {}
                public void mouseDown(MouseEvent mouseEvent) {}
                public void mouseUp(MouseEvent mouseEvent) { _editor.showHeaders(); }
            });
            
            _privacyLabel = new Label(privacyGroup, SWT.NONE);
            _privacyLabel.setLayoutData(new GridData(GridData.BEGINNING, GridData.END, true, true));
            
            _privacy = new Label(privacyGroup, SWT.NONE);
            _privacy.setLayoutData(new GridData(GridData.BEGINNING, GridData.END, true, true));
            
            Color white = ColorUtil.getColor("white");
            Color black = ColorUtil.getColor("black");
            edge.setBackground(white);
            authorGroup.setBackground(white);
            _authorAvatar.setBackground(white);
            _authorName.setBackground(white);
            _authorNameLabel.setBackground(white);
            forumGroup.setBackground(white);
            _forumAvatar.setBackground(white);
            _forumName.setBackground(white);
            _forumNameLabel.setBackground(white);
            privacyGroup.setBackground(white);
            _privacyIcon.setBackground(white);
            _privacyLabel.setBackground(white);
            _privacy.setBackground(white);
            
            edge.setForeground(black);
            authorGroup.setForeground(black);
            _authorAvatar.setForeground(black);
            _authorName.setForeground(black);
            _authorNameLabel.setForeground(black);
            forumGroup.setForeground(black);
            _forumAvatar.setForeground(black);
            _forumName.setForeground(black);
            _forumNameLabel.setForeground(black);
            privacyGroup.setForeground(black);
            _privacyIcon.setForeground(black);
            _privacyLabel.setForeground(black);
            _privacy.setForeground(black);
            
            pickPrivacyAuthorized();
        }
        
        public void forumSelected(long channelId, String summary, boolean isManaged) {
            _forumAvatar.setRedraw(false);
            _forumName.setText(summary);
            
            // bah, rare enough to run it inline rather than async (am i crazy?)
            ImageUtil.dispose(_forumAvatar.getImage());
            _forumAvatar.setImage(null);
            if (channelId >= 0) {
                // don't show the forum avatar unless the forum is bookmarked or we own the channel -
                // this should help fight phishing attacks (to prevent spoofing w/ same 
                // icon & link <a href=...>send me your password</a>)
                if (isManaged || _client.isWatched(channelId)) { // _bookmarkControl.isBookmarked(SyndieURI.createScope(forum))) {
                    byte avatar[] = _client.getChannelAvatar(channelId);
                    if (avatar != null) {
                        Image img = ImageUtil.createImage(avatar);
                        _forumAvatar.setImage(img);
                    } else {
                        _forumAvatar.setImage(ImageUtil.ICON_EDITOR_BOOKMARKED_NOAVATAR);
                    }
                } else {
                    _forumAvatar.setImage(ImageUtil.ICON_EDITOR_NOT_BOOKMARKED);
                }
                _forumAvatar.setToolTipText(summary);
            } else {
                _forumAvatar.setImage(ImageUtil.ICON_EDITOR_NOT_BOOKMARKED);
                _forumAvatar.setToolTipText("");
            }
            _forumAvatar.setRedraw(true);
            getEdgeRoot().layout(true, true);
        }
        public void authorSelected(long channelId, String summary) {
            _authorAvatar.setRedraw(false);
            ImageUtil.dispose(_authorAvatar.getImage());
            _authorAvatar.setImage(null);
            byte avatar[] = _client.getChannelAvatar(channelId);
            if (avatar != null) {
                Image img = ImageUtil.createImage(avatar);
                _authorAvatar.setImage(img);
            } else {
                _authorAvatar.setImage(ImageUtil.ICON_EDITOR_BOOKMARKED_NOAVATAR);
            }
            _authorAvatar.setToolTipText(summary);
            _authorAvatar.setRedraw(true);
            _authorName.setText(summary);
            getEdgeRoot().layout(true, true);
        }
        
        public void pickPrivacyPublic() {
            _privacyIcon.setImage(ImageUtil.ICON_EDITOR_PRIVACY_PUBLIC);
            String txt = _translationRegistry.getText(T_PRIV_PUBLIC, "Anyone can read it");
            _privacyIcon.setToolTipText(txt);
            _privacy.setText(txt);
            getEdgeRoot().layout(true, true);
        }
        public void pickPrivacyPBE() {
            _privacyIcon.setImage(ImageUtil.ICON_EDITOR_PRIVACY_PBE);
            String txt = _translationRegistry.getText(T_PRIV_PBE, "Passphrase required");
            _privacyIcon.setToolTipText(txt);
            _privacy.setText(txt);
            getEdgeRoot().layout(true, true);
        }
        public void pickPrivacyPrivate() {
            _privacyIcon.setImage(ImageUtil.ICON_EDITOR_PRIVACY_REPLY);
            String txt = _translationRegistry.getText(T_PRIV_PRIVATE, "Forum admins only");
            _privacyIcon.setToolTipText(txt);
            _privacy.setText(txt);
            getEdgeRoot().layout(true, true);
        }
        public void pickPrivacyAuthorized() {
            _privacyIcon.setImage(ImageUtil.ICON_EDITOR_PRIVACY_AUTHORIZED);
            String txt = _translationRegistry.getText(T_PRIV_AUTHORIZED, "Authorized readers only");
            _privacyIcon.setToolTipText(txt);
            _privacy.setText(txt);
            getEdgeRoot().layout(true, true);
        }
        
        public void applyTheme(Theme theme) {
            _privacy.setFont(theme.DEFAULT_FONT);
            _privacyLabel.setFont(theme.BUTTON_FONT);
            _forumName.setFont(theme.DEFAULT_FONT);
            _forumNameLabel.setFont(theme.BUTTON_FONT);
            _authorName.setFont(theme.DEFAULT_FONT);
            _authorNameLabel.setFont(theme.BUTTON_FONT);
            getEdgeRoot().layout(true, true);
        }
        public void translate(TranslationRegistry registry) {
            _privacyLabel.setText(registry.getText(T_PRIVACY, "Who can read the post?"));
            _forumNameLabel.setText(registry.getText(T_FORUMNAME, "Forum (to):"));
            _authorNameLabel.setText(registry.getText(T_AUTHORNAME, "Author (from):"));
        }
    }
    
    private static final String T_SAVEFORLATER_TT = "syndie.gui.desktop.messageeditorpanel.saveforlater.tt";
    private static final String T_PREVIEW_TT = "syndie.gui.desktop.messageeditorpanel.preview.tt";
    private static final String T_POST_TT = "syndie.gui.desktop.messageeditorpanel.post.tt";
    private static final String T_CANCEL_TT = "syndie.gui.desktop.messageeditorpanel.cancel.tt";
    
    private class EastEdge extends DesktopEdge implements Translatable, Themeable {
        private Button _saveForLater;
        private Button _preview;
        private Button _post;
        private Button _cancel;
        public EastEdge(Composite edge, UI ui) { 
            super(edge, ui); 
            initComponents();
        }
        private void initComponents() {
            Composite edge = getEdgeRoot();
            edge.setLayout(new FillLayout(SWT.VERTICAL));
            
            _saveForLater = new Button(edge, SWT.PUSH);
            _preview = new Button(edge, SWT.PUSH);
            _post = new Button(edge, SWT.PUSH);
            _cancel = new Button(edge, SWT.PUSH);
            
            _saveForLater.addSelectionListener(new FireSelectionListener() {
                public void fire() { _editor.postponeMessage(); }
            });
            _preview.addSelectionListener(new FireSelectionListener() {
                public void fire() { togglePreview(); }
            });
            _post.addSelectionListener(new FireSelectionListener() {
                public void fire() { post(); }
            });
            _cancel.addSelectionListener(new FireSelectionListener() {
                public void fire() { _editor.cancelMessage(); }
            });
                        
            _saveForLater.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _saveForLater, _themeRegistry.getTheme().BUTTON_FONT, _translationRegistry.getText(T_SAVEFORLATER, "Save"));
                }
            });
            _preview.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    boolean previewing = false;
                    if ( (_editor == null) || (_editor.getPageEditor() == null) || (_editor.getPageEditor().isPreviewShowing()) )
                        previewing = true;

                    if (previewing)
                        ImageUtil.drawDescending(evt.gc, _preview, _themeRegistry.getTheme().BUTTON_FONT, _translationRegistry.getText(T_EDIT, "Edit"));
                    else
                        ImageUtil.drawDescending(evt.gc, _preview, _themeRegistry.getTheme().BUTTON_FONT, _translationRegistry.getText(T_PREVIEW, "Preview"));
                    
                }
            });
            _post.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _post, _themeRegistry.getTheme().BUTTON_FONT, _translationRegistry.getText(T_POST, "Post"));
                }
            });
            _cancel.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _cancel, _themeRegistry.getTheme().BUTTON_FONT, _translationRegistry.getText(T_CANCEL, "Cancel"));
                }
            });
        }
        public void updatePageType(boolean isHTML) {
            _preview.setEnabled(isHTML);
            setPreviewText(_translationRegistry);
        }
        public void translate(TranslationRegistry registry) {
            _saveForLater.setToolTipText(registry.getText(T_SAVEFORLATER_TT, "Save the message for later"));
            _preview.setToolTipText(registry.getText(T_PREVIEW_TT, "Preview the page"));
            _post.setToolTipText(registry.getText(T_POST_TT, "Post the message"));
            _cancel.setToolTipText(registry.getText(T_CANCEL_TT, "Cancel the message entirely"));
            
            _saveForLater.redraw();
            _preview.redraw();
            _post.redraw();
            _cancel.redraw();
            
            setPreviewText(registry);
        }
        
        public void setPreviewText(TranslationRegistry registry) {
            _preview.redraw();
        }

        public void applyTheme(Theme theme) {
            _saveForLater.redraw();
            _preview.redraw();
            _post.redraw();
            _cancel.redraw();
        }
    }
    
    private static final String T_SAVEFORLATER = "syndie.gui.desktop.messageeditorpanel.saveforlater";
    private static final String T_PREVIEW = "syndie.gui.desktop.messageeditorpanel.preview";
    private static final String T_EDIT = "syndie.gui.desktop.messageeditorpanel.edit";
    private static final String T_POST = "syndie.gui.desktop.messageeditorpanel.post";
    private static final String T_CANCEL = "syndie.gui.desktop.messageeditorpanel.cancel";
    
    private class SouthEdge extends DesktopEdge implements Translatable {
        private Button _addPage;
        private Button _removePage;
        private Button _addWebRip;
        private Button _toggleFormat;
        private Button _addImg;
        private Button _addFile;
        private Button _removeFile;
        private Button _addLink;
        private Button _htmlStyle;
        private Button _spell;
        private Button _find;
        private Button _quote;
        
        public SouthEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        private void initComponents() {
            Composite edge = getEdgeRoot();
            RowLayout rl = new RowLayout(SWT.HORIZONTAL);
            edge.setLayout(rl);
            
            _addPage = new Button(edge, SWT.PUSH);
            _addPage.setLayoutData(new RowData(64, 64));
            _removePage = new Button(edge, SWT.PUSH);
            _removePage.setLayoutData(new RowData(64, 64));
            _addWebRip = new Button(edge, SWT.PUSH);
            _addWebRip.setLayoutData(new RowData(64, 64));
            _toggleFormat = new Button(edge, SWT.PUSH);
            _toggleFormat.setLayoutData(new RowData(64, 64));
            _addImg = new Button(edge, SWT.PUSH);
            _addImg.setLayoutData(new RowData(64, 64));
            _addFile = new Button(edge, SWT.PUSH);
            _addFile.setLayoutData(new RowData(64, 64));
            _removeFile = new Button(edge, SWT.PUSH);
            _removeFile.setLayoutData(new RowData(64, 64));
            _addLink = new Button(edge, SWT.PUSH);
            _addLink.setLayoutData(new RowData(64, 64));
            _htmlStyle = new Button(edge, SWT.PUSH);
            _htmlStyle.setLayoutData(new RowData(64, 64));
            _spell = new Button(edge, SWT.PUSH);
            _spell.setLayoutData(new RowData(64, 64));
            _find = new Button(edge, SWT.PUSH);
            _find.setLayoutData(new RowData(64, 64));
            _quote = new Button(edge, SWT.PUSH);
            _quote.setLayoutData(new RowData(64, 64));
            
            _addPage.setImage(ImageUtil.ICON_EDITOR_ADDPAGE);
            _removePage.setImage(ImageUtil.ICON_EDITOR_REMOVEPAGE);
            _addWebRip.setImage(ImageUtil.ICON_EDITOR_WEBRIP);
            _toggleFormat.setImage(ImageUtil.ICON_EDITOR_TOGGLETYPE);
            _addImg.setImage(ImageUtil.ICON_EDITOR_ADDIMAGE);
            _addFile.setImage(ImageUtil.ICON_EDITOR_ADDFILE);
            _removeFile.setImage(ImageUtil.ICON_EDITOR_REMOVEFILE);
            _addLink.setImage(ImageUtil.ICON_EDITOR_LINK);
            _htmlStyle.setImage(ImageUtil.ICON_EDITOR_STYLE);
            _spell.setImage(ImageUtil.ICON_EDITOR_SPELL);
            _find.setImage(ImageUtil.ICON_EDITOR_SEARCH);
            _quote.setImage(ImageUtil.ICON_EDITOR_QUOTE);
            
            _addPage.addSelectionListener(new FireSelectionListener() {
                public void fire() { _editor.addPage(); }
            });
            _removePage.addSelectionListener(new FireSelectionListener() {
                public void fire() { _editor.removePage(); }
            });
            _addWebRip.addSelectionListener(new FireSelectionListener() {
                public void fire() { _editor.addWebRip(); }
            });
            _toggleFormat.addSelectionListener(new FireSelectionListener() {
                public void fire() { _editor.togglePageType(); }
            });
            _addImg.addSelectionListener(new FireSelectionListener() {
                public void fire() { _editor.showImagePopup(false); }
            });
            _addFile.addSelectionListener(new FireSelectionListener() {
                public void fire() { _editor.addAttachment(); }
            });
            _removeFile.addSelectionListener(new FireSelectionListener() {
                public void fire() { _editor.removeAttachment(); }
            });
            _addLink.addSelectionListener(new FireSelectionListener() {
                public void fire() { _editor.showLinkPopup(); }
            });
            _htmlStyle.addSelectionListener(new FireSelectionListener() {
                public void fire() { _editor.styleText(); }
            });
            _spell.addSelectionListener(new FireSelectionListener() {
                public void fire() { _editor.spellNext(); }
            });
            _find.addSelectionListener(new FireSelectionListener() {
                public void fire() { _editor.search(); }
            });
            _quote.addSelectionListener(new FireSelectionListener() {
                public void fire() { _editor.quote(); }
            });
        }

        public void statusUpdated(int page, int pages, int attachment, int attachments, String type, boolean pageLoaded, boolean isHTML, boolean hasAncestors) {
            _addImg.setEnabled(isHTML);
            _addLink.setEnabled(isHTML);
            _htmlStyle.setEnabled(isHTML);
            
            _removeFile.setEnabled(attachment >= 0);
            _removePage.setEnabled(page >= 0);

            _toggleFormat.setEnabled(page >= 0);

            _spell.setEnabled(pageLoaded && false); // disabled for the moment, pending revamp (line breaking, plurals, caps)

            _find.setEnabled(pageLoaded);
            _quote.setEnabled(page >= 0 && hasAncestors);
        }
        
        public void translate(TranslationRegistry registry) {
            _addPage.setToolTipText(registry.getText(T_ADDPAGE_TT, "Add page"));
            _removePage.setToolTipText(registry.getText(T_REMOVEPAGE_TT, "Remove page"));
            _addWebRip.setToolTipText(registry.getText(T_WEBRIP_TT, "Rip a web page"));
            _toggleFormat.setToolTipText(registry.getText(T_TOGGLEFORMAT_TT, "Toggle HTML/plain text"));
            _addImg.setToolTipText(registry.getText(T_ADDIMAGE_TT, "Add image"));
            _addFile.setToolTipText(registry.getText(T_ADDFILE_TT, "Add file"));
            _removeFile.setToolTipText(registry.getText(T_REMOVEFILE_TT, "Remove file"));
            _addLink.setToolTipText(registry.getText(T_ADDLINK_TT, "Add link"));
            _htmlStyle.setToolTipText(registry.getText(T_HTMLSTYLE_TT, "HTML style helper"));
            _spell.setToolTipText(registry.getText(T_SPELL_TT, "Check spelling"));
            _find.setToolTipText(registry.getText(T_FIND_TT, "Find/replace text"));
            _quote.setToolTipText(registry.getText(T_QUOTE_TT, "Quote the parent message"));
        }
    }
    private static final String T_ADDPAGE_TT = "syndie.gui.desktop.messageeditorpanel.addpage.tt";
    private static final String T_REMOVEPAGE_TT = "syndie.gui.desktop.messageeditorpanel.removepage.tt";
    private static final String T_WEBRIP_TT = "syndie.gui.desktop.messageeditorpanel.webrip.tt";
    private static final String T_TOGGLEFORMAT_TT = "syndie.gui.desktop.messageeditorpanel.toggleformat.tt";
    private static final String T_ADDIMAGE_TT = "syndie.gui.desktop.messageeditorpanel.addimage.tt";
    private static final String T_ADDFILE_TT = "syndie.gui.desktop.messageeditorpanel.addfile.tt";
    private static final String T_REMOVEFILE_TT = "syndie.gui.desktop.messageeditorpanel.removefile.tt";
    private static final String T_ADDLINK_TT = "syndie.gui.desktop.messageeditorpanel.addlink.tt";
    private static final String T_HTMLSTYLE_TT = "syndie.gui.desktop.messageeditorpanel.htmlstyle.tt";
    private static final String T_SPELL_TT = "syndie.gui.desktop.messageeditorpanel.spell.tt";
    private static final String T_FIND_TT = "syndie.gui.desktop.messageeditorpanel.find.tt";
    private static final String T_QUOTE_TT = "syndie.gui.desktop.messageeditorpanel.quote.tt";
}
