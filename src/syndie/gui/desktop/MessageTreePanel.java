package syndie.gui.desktop;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import syndie.Constants;
import syndie.data.MessageIterator;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.UI;
import syndie.gui.BanControl;
import syndie.gui.BookmarkControl;
import syndie.gui.BookmarkDnDHelper;
import syndie.gui.ColorUtil;
import syndie.gui.DataCallback;
import syndie.gui.FireSelectionListener;
import syndie.gui.ImageUtil;
import syndie.gui.LinkBuilderPopup;
import syndie.gui.MessageTree;
import syndie.gui.Theme;
import syndie.gui.ThemeRegistry;
import syndie.gui.Themeable;
import syndie.gui.Translatable;
import syndie.gui.TranslationRegistry;
import syndie.gui.URIHelper;
import syndie.gui.WatchedMessageTree;

public class MessageTreePanel extends DesktopPanel implements Themeable, Translatable {
    private WatchedMessageTree _tree;
    private Listener _keyListener;
    private Hash _actionScope;
    private long _actionScopeId;
    private SyndieURI _detailURI;
    private BanControl _banControl;
    
    public MessageTreePanel(Desktop desktop, SyndieURI origURI, BanControl ban) {
        super(desktop, desktop.getDBClient(), desktop.getThemeRegistry(), desktop.getTranslationRegistry(), desktop.getCenter(), desktop.getUI(), origURI);
        _banControl = ban;
        // add this filter to the display only when this panel is current
        _keyListener = new Listener() {
            public void handleEvent(Event evt) {
                if (evt.keyCode == SWT.F5) {
                    _tree.applyFilter();
                    evt.type = SWT.None;
                } else if ((evt.stateMask & SWT.MOD1) == SWT.MOD1) {
                    if (evt.character == 24) { // ^X
                        evt.type = SWT.None;
                        evt.doit = false;
                        expand(false);
                    } else if (evt.character == 3) { // ^C
                        evt.type = SWT.None;
                        evt.doit = false;
                        collapse(false);
                    } else if (evt.character == 16) { // ^P
                        evt.type = SWT.None;
                        evt.doit = false;
                        viewProfile();
                    } else if (evt.character == 22) { // ^V
                        evt.type = SWT.None;
                        evt.doit = false;
                        viewMessage();
                    } else if (evt.character == 13) { // ^M
                        evt.type = SWT.None;
                        evt.doit = false;
                        toggleRead();
                    } else if (evt.character == 18) { // ^R
                        evt.type = SWT.None;
                        evt.doit = false;
                        createRef();
                    }
                    forceFocus();
                }
            }
        };
        initComponents();
    }
    
    public String getPanelName() { return "threads"; }
    public String getPanelDescription() { return "Message tree panel"; }
    
    public SyndieURI getSelectedURI() { return _tree.getCurrentFilter(); }
    
    private void initComponents() {
        BookmarkControl bookmarkControl = _desktop.getBookmarkControl();
        DataCallback dataCallback = _desktop.getDataCallback();
        _tree = new WatchedMessageTree(_client, _ui, _themeRegistry, _translationRegistry, _banControl, _desktop.getNavControl(), URIHelper.instance(), 
                bookmarkControl, dataCallback, getRoot(), new MessageTree.MessageTreeListener() {
            public void messageSelected(MessageTree tree, SyndieURI uri, boolean toView, boolean nodelay) {
                if (toView) {
                    _desktop.getNavControl().view(uri);
                } else {
                    SyndieURI origURI = uri;
                    long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                    if (msgId >= 0) {
                        long targetId = _client.getMessageTarget(msgId);
                        if (targetId >= 0) {
                            Hash target = _client.getChannelHash(targetId);
                            if (target != null)
                                uri = SyndieURI.createScope(target);
                        }
                    }
                    setFocus(uri, origURI);
                }
            }

            public void filterApplied(MessageTree tree, SyndieURI searchURI) {
                _ui.debugMessage("message tree panel: filter applied: " + searchURI);
                ((NorthEdge)_edgeNorth).update(searchURI);
            }
        }, false);
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    public MessageIterator getIterator(SyndieURI uri) { return _tree.getIterator(uri); }

    public boolean canShow(SyndieURI uri) { 
        boolean rv = false;
        if (super.canShow(uri)) return true;
        if (uri.isSearch() || (uri.isChannel() && (uri.getMessageId() == null))) {
           rv = true;
           // should we have just one message tree, or should we have many?
           // if just one, return true here.  if we should have many panels with different trees,
           // do some work.  the following means one message tree panel per forum (or unique
           // collection of forums)
           Set existing = getScopes(_tree.getCurrentFilter());
           Set proposed = getScopes(uri);
           rv = existing.equals(proposed);
           //System.out.println("mtp can show? " + rv + " " + existing + " / " + proposed);
        }
        //System.out.println("mtp: canShow: " + uri + "? " + rv);
        return rv;
    }
    
    public void forceFocus() { _tree.forceFocus(); }
    
    private Set getScopes(SyndieURI uri) {
        Set rv = new HashSet();
        if (uri != null) {
            if (uri.isChannel()) {
                rv.add(uri.getScope());
            } else if (uri.isSearch()) {
                Hash scopes[] = uri.getSearchScopes();
                if (scopes != null) {
                    for (int i = 0; i < scopes.length; i++)
                        if (scopes[i] != null)
                            rv.add(scopes[i]);
                }
            }
        }
        return rv;
    }

    public void shown(Desktop desktop, SyndieURI uri, String name, String description) {
        //System.out.println("tree panel shown: " + uri);
        if (uri != null) {
            if (uri.isChannel())
                uri = SyndieURI.createSearch(uri.getScope(), false, true, MessageTree.shouldUseImportDate(_client));            
            if (!uri.equals(_tree.getCurrentFilter())) {
                _tree.applyFilter(uri.toString());
            }
        }
        if ( ( (name != null) && (name.length() > 0) ) || 
             ( (description != null) && (description.length() > 0) ) ) {
            ((NorthEdge)_edgeNorth).updateMulti(name, description);
        }
        setFocus(uri, uri);
        getRoot().getDisplay().addFilter(SWT.KeyDown, _keyListener);
        super.shown(desktop, uri, name, description);
    }
    
    private void setFocus(SyndieURI uri, SyndieURI detailURI) {
        ((SouthEdge)_edgeSouth).updateActions(uri, detailURI);
        ((EastEdge)_edgeEast).updateNav(uri, detailURI);
    }
    
    public void hidden() {
        getRoot().getDisplay().removeFilter(SWT.KeyDown, _keyListener);
    }
    
    protected void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        _tree.dispose();
        hidden();
        super.dispose();
    }
    
    public void applyTheme(Theme theme) {
        if (_edgeNorth != null)
            ((Themeable)_edgeNorth).applyTheme(theme);
        if (_edgeSouth != null)
            ((Themeable)_edgeSouth).applyTheme(theme);
    }
    public void translate(TranslationRegistry registry) {
        if (_edgeNorth != null)
            ((Translatable)_edgeNorth).translate(registry);
        if (_edgeSouth != null)
            ((Translatable)_edgeSouth).translate(registry);
    }
    
    public void buildNorth(Composite edge) { 
        if (_edgeNorth == null) {
            _edgeNorth = new NorthEdge(edge, _ui);
            ((NorthEdge)_edgeNorth).translate(_translationRegistry);
            ((NorthEdge)_edgeNorth).applyTheme(_themeRegistry.getTheme());
        }
    }
    public void buildEast(Composite edge) { 
        if (_edgeEast == null) {
            _edgeEast = new EastEdge(edge, _ui); 
            ((EastEdge)_edgeEast).translate(_translationRegistry);
            ((EastEdge)_edgeEast).applyTheme(_themeRegistry.getTheme());
        }
    }
    public void buildSouth(Composite edge) { 
        if (_edgeSouth == null) {
            _edgeSouth = new SouthEdge(edge, _ui); 
            ((SouthEdge)_edgeSouth).translate(_translationRegistry);
            ((SouthEdge)_edgeSouth).applyTheme(_themeRegistry.getTheme());
        }
    }
    
    private static final String T_MULTI = "syndie.gui.messagetreepanel.multi";
    private static final String T_REFS = "syndie.gui.messagetreepanel.refs";
    
    private class NorthEdge extends DesktopEdge implements Themeable, Translatable {
        private Label _avatar;
        private Label _summary;
        private Combo _refs;
        private Hash _scope;
        private List _refNodes;
        private String _name;
        private String _description;
        
        private DragSource _avatarSrc;
        private DragSource _summarySrc;
        
        public NorthEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        
        public void update(SyndieURI uri) {
            //System.out.println("north: update: " + uri);
            if (uri.isChannel()) {
                update(uri.getScope());
            } else if (uri.isSearch()) {
                Hash scopes[] = uri.getSearchScopes();
                if ( (scopes != null) && (scopes.length == 1) )
                    update(scopes[0]);
                else
                    updateMulti(null, null);
            } else {
                updateMulti(null, null);
            }
        }
        public void updateMulti(String name, String description) {
            _scope = null;
            if (_name == null)
                _name = name;
            if (_description == null)
                _description = description;
            getEdgeRoot().setRedraw(false);
            ImageUtil.dispose(_avatar.getImage());
            _avatar.setImage(ImageUtil.ICON_EDITOR_NOT_BOOKMARKED);
            if ( (_name != null) && (_description != null) ) {
                StringBuffer buf = new StringBuffer();
                if (_name != null)
                    buf.append(_name);
                if (_description != null)
                    buf.append(" - ").append(_description);
                _summary.setText(buf.toString());
            } else {
                _summary.setText(_translationRegistry.getText(T_MULTI, "Browsing multiple forums"));
            }
            ((GridData)_refs.getLayoutData()).exclude = true;
            _refs.setVisible(false);
            getEdgeRoot().layout(true, true);
            getEdgeRoot().setRedraw(true);
        }
        private void update(final Hash scope) {
            if ((_scope != null) && (_scope.equals(scope))) return;
            getEdgeRoot().setRedraw(false);
            ImageUtil.dispose(_avatar.getImage());
            _avatar.setImage(null);
            JobRunner.instance().enqueue(new Runnable() { public void run() { asyncUpdate(scope); } });
        }
        private void asyncUpdate(final Hash scope) {
            final long channelId = _client.getChannelId(scope);
            final byte avatar[] = _client.getChannelAvatar(channelId);
            final String name = _client.getChannelName(channelId);
            final String desc = _client.getChannelDescription(channelId);
            final List refs = _client.getChannelReferences(channelId);
            getEdgeRoot().getDisplay().asyncExec(new Runnable() {
                public void run() { syncUpdate(channelId, avatar, name, desc, scope, refs); }
            });
        }
        public void syncUpdate(long channelId, byte avatar[], String name, String desc, Hash scope, List refs) {
            _scope = scope;
            Image img = ImageUtil.createImage(avatar);
            if (img != null) {
                Rectangle bounds = img.getBounds();
                if ( (bounds.width > Constants.MAX_AVATAR_WIDTH) || (bounds.height > Constants.MAX_AVATAR_HEIGHT) )
                    img = ImageUtil.resize(img, Constants.MAX_AVATAR_WIDTH, Constants.MAX_AVATAR_HEIGHT, true);
            }
            _avatar.setImage(img);
            
            StringBuffer buf = new StringBuffer();
            if (name != null)
                buf.append(name).append(" ");
            if (scope != null)
                buf.append("[").append(scope.toBase64().substring(0,6)).append("] ");
            if (desc != null)
                buf.append(" - ").append(desc);
            _summary.setText(buf.toString());
            _refs.removeAll();
            _refNodes = new ArrayList();
            if (refs.size() > 0) {
                _refs.add(_translationRegistry.getText(T_REFS, "* Forum notices and references:"));
                ReferenceNode.walk(refs, new ReferenceNode.Visitor() {
                    public void visit(ReferenceNode node, int depth, int siblingOrder) {
                        if (node.getURI() != null) {
                            _refNodes.add(node);
                            StringBuffer ref = new StringBuffer();
                            if ( (node.getName() != null) && (node.getName().length() > 0) )
                                ref.append(node.getName());
                            if ( (node.getDescription() != null) && (node.getDescription().length() > 0) )
                                ref.append(" - ").append(node.getDescription());
                            _refs.add(ref.toString());
                        }
                    }
                });
                ((GridData)_refs.getLayoutData()).exclude = false;
                _refs.setVisible(true);
                _refs.select(0);
            } else {
                ((GridData)_refs.getLayoutData()).exclude = true;
                _refs.setVisible(false);
            }
            getEdgeRoot().layout(true, true);
            getEdgeRoot().setRedraw(true);
        } 
        
        private void initComponents() {
            Composite root = getEdgeRoot();
            GridLayout gl = new GridLayout(2, false);
            gl.horizontalSpacing = 0;
            gl.verticalSpacing = 0;
            gl.marginHeight = 0;
            gl.marginWidth = 0;
            root.setLayout(gl);
            
            _avatar = new Label(root, SWT.NONE);
            GridData gd = new GridData(64, SWT.DEFAULT);
            gd.verticalSpan = 2;
            gd.horizontalSpan = 1;
            _avatar.setLayoutData(gd);
            
            _summary = new Label(root, SWT.NONE);
            _summary.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, true, true));
            
            _refs = new Combo(root, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.BORDER);
            _refs.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
            _refs.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    int idx = _refs.getSelectionIndex();
                    if (idx > 0) { // skip the first - its the "these are refs" line
                        ReferenceNode node = (ReferenceNode)_refNodes.get(idx-1);
                        _desktop.getNavControl().view(node.getURI(), node.getName(), node.getDescription());
                    }
                }
            });

            Color white = ColorUtil.getColor("white");
            Color black = ColorUtil.getColor("black");
            _avatar.setBackground(white);
            _avatar.setForeground(black);
            _summary.setBackground(white);
            _summary.setForeground(black);
            _refs.setBackground(white);
            _refs.setForeground(black);
            root.setBackground(white);
            root.setForeground(black);
            
            
            BookmarkDnDHelper.SourceProvider src = new BookmarkDnDHelper.SourceProvider() {
                public SyndieURI getURI() { return SyndieURI.createScope(_scope); }
                public String getName() { return _name; }
                public String getDescription() { return _description; }
            };
            _avatarSrc = BookmarkDnDHelper.initSource(_avatar, src);
            _summarySrc = BookmarkDnDHelper.initSource(_summary, src);
        }
        
        public void dispose() {
            ImageUtil.dispose(_avatar.getImage());
            if (_avatarSrc != null) _avatarSrc.dispose();
            if (_summarySrc != null) _summarySrc.dispose();
            super.dispose();
        }

        public void applyTheme(Theme theme) {
            _summary.setFont(theme.SHELL_FONT);
            _refs.setFont(theme.DEFAULT_FONT);
            getEdgeRoot().layout(true, true);
        }
        public void translate(TranslationRegistry registry) {}
    }
    
    private static final String T_POST = "syndie.gui.messagetreepanel.post";
    private static final String T_REPLY = "syndie.gui.messagetreepanel.reply";
    private static final String T_PRIVMSG = "syndie.gui.messagetreepanel.privmsg";
    private static final String T_WATCH = "syndie.gui.messagetreepanel.watch";
    private static final String T_UNWATCH = "syndie.gui.messagetreepanel.unwatch";
    private static final String T_BAN = "syndie.gui.messagetreepanel.ban";
    private static final String T_UNBAN = "syndie.gui.messagetreepanel.unban";
    
    private class SouthEdge extends DesktopEdge implements Themeable, Translatable {
        private Button _post;
        private Button _reply;
        private Button _privmsg;
        private Button _watch;
        private Button _ban;
        
        public SouthEdge(Composite edge, UI ui) {
            super(edge, ui);
            _actionScope = null;
            _actionScopeId = -1;
            initComponents();
        }
        public void updateActions(final SyndieURI uri, final SyndieURI detailURI) {
            _ui.debugMessage("updateActions: " + uri);
            _detailURI = detailURI;
            if (uri == null) return; // no change, panel was just reshown
            Hash scope = null;
            if (uri.isChannel()) {
                scope = uri.getScope();
            } else if (uri.isSearch()) {
                Hash scopes[] = uri.getSearchScopes();
                if ( (scopes != null) && (scopes.length == 1) )
                    scope = scopes[0];
            }
            if ( ( (scope == null) && (_actionScope == null) ) ||
                 ( (scope != null) && (scope.equals(_actionScope)) ) ) {
                return; // no change
            } else if (scope != null) {
                // single scope.. crunch to figure out what we're allowed to do, then
                // come back and reenable the allowed buttons
                _post.setEnabled(false);
                _privmsg.setEnabled(false);
                _watch.setEnabled(false);
                _ban.setEnabled(false);
                final Hash actionScope = scope;
                JobRunner.instance().enqueue(new Runnable() {
                    public void run() {
                        final long channelId = _client.getChannelId(actionScope);
                        DBClient.ChannelCollector chans = _client.getNymChannels(); //getChannels(true, true, true, true, false);
                        final boolean postable = chans.getAllIds().contains(new Long(channelId));
                        final boolean privmsg = true; // true for all channels
                        final boolean watched = false; //_client.isWatched(channelId);
                        final boolean repliable = ((detailURI != null) && (detailURI.getMessageId() != null)) && (postable || _client.getChannelAllowPublicReplies(channelId));
                        final boolean banned = false; //_client.getBannedChannels().contains(actionScope);
                        _ui.debugMessage("update actions: scope=" + actionScope + " channelId=" + channelId + " postable? " + postable);
                        Display.getDefault().asyncExec(new Runnable() {
                            public void run() {
                                _actionScope = actionScope;
                                _actionScopeId = channelId;
                                _post.setEnabled(postable);
                                _reply.setEnabled(repliable);
                                _privmsg.setEnabled(privmsg);
                                translateWatch();
                                translateBan();
                            }
                        });
                    }
                });
            } else {
                // multiple scopes
                _post.setEnabled(false);
                _privmsg.setEnabled(false);
                translateWatch();
                translateBan();
            }
        }
        private void initComponents() {
            Composite root = getEdgeRoot();
            root.setLayout(new FillLayout(SWT.HORIZONTAL));
            
            _post = new Button(root, SWT.PUSH);
            _post.addSelectionListener(new FireSelectionListener() { public void fire() { post(); } });
            _reply = new Button(root, SWT.PUSH);
            _reply.addSelectionListener(new FireSelectionListener() { public void fire() { reply(); } });
            _privmsg = new Button(root, SWT.PUSH);
            _privmsg.addSelectionListener(new FireSelectionListener() { public void fire() { sendPM(); } });
            _watch = new Button(root, SWT.PUSH);
            _watch.addSelectionListener(new FireSelectionListener() {
                public void fire() { 
                    if (_actionScope != null) {
                        if (_client.isWatched(_actionScopeId))
                            _client.unwatchChannel(_actionScope);
                        else
                            _client.watchChannel(_actionScopeId, true, false, false, false, false);
                    }
                    translateWatch();
                }
            });
            _ban = new Button(root, SWT.PUSH);
            _ban.addSelectionListener(new FireSelectionListener() { 
                public void fire() { 
                    if (_actionScope != null) {
                        if (_client.getBannedChannels().contains(_actionScope)) {
                            _client.unban(_actionScope);
                        } else {
                            boolean banned = _banControl.ban(_actionScope);
                            //_client.ban(_actionScope, _ui, true);
                            if (banned) 
                                translateBan();
                        }
                    }
                } 
            });
            
            _post.setEnabled(false);
            _reply.setEnabled(false);
            _privmsg.setEnabled(false);
            translateWatch();
            translateBan();
        }
        public void applyTheme(Theme theme) {
            _post.setFont(theme.BUTTON_FONT);
            _reply.setFont(theme.BUTTON_FONT);
            _privmsg.setFont(theme.BUTTON_FONT);
            _watch.setFont(theme.BUTTON_FONT);
            _ban.setFont(theme.BUTTON_FONT);
            getEdgeRoot().layout(true, true);
        }
        public void translate(TranslationRegistry registry) {
            _post.setText(registry.getText(T_POST, "Post a new message"));
            _reply.setText(registry.getText(T_REPLY, "Reply"));
            _privmsg.setText(registry.getText(T_PRIVMSG, "Send a private message"));
            translateWatch();
            translateBan();
        }

        private void translateWatch() {
            if ( (_actionScope != null) && (_actionScopeId >= 0) ) {
                _watch.setEnabled(true);
                if (_client.isWatched(_actionScopeId))
                    _watch.setText(_translationRegistry.getText(T_UNWATCH, "Unwatch the forum"));
                else
                    _watch.setText(_translationRegistry.getText(T_WATCH, "Watch the forum"));
            } else {
                _watch.setEnabled(false);
            }
        }
        private void translateBan() {
            if ( (_actionScope != null) && (_actionScopeId >= 0) ) {
                _ban.setEnabled(true);
                if (_client.getBannedChannels().contains(_actionScope))
                    _ban.setText(_translationRegistry.getText(T_UNBAN, "Unban the forum"));
                else
                    _ban.setText(_translationRegistry.getText(T_BAN, "Ban the forum"));
            } else {
                _ban.setEnabled(false);
            }
        }
        
        private void post() {
            _desktop.getNavControl().view(URIHelper.instance().createPostURI(_actionScope, null, false));
        }
        private void reply() {
            _desktop.getNavControl().view(URIHelper.instance().createPostURI(_actionScope, _detailURI, false));
        }
        private void sendPM() {
            _desktop.getNavControl().view(URIHelper.instance().createPostURI(_actionScope, null, true));
        }
    }
    
    private static final String T_PROFILE_TT = "syndie.gui.messagetreepanel.profile.tt";
    private static final String T_PROFILE = "syndie.gui.messagetreepanel.profile";
    private static final String T_EXPAND = "syndie.gui.messagetreepanel.expand";
    private static final String T_COLLAPSE = "syndie.gui.messagetreepanel.collapse";
    private static final String T_VIEW = "syndie.gui.messagetreepanel.view";
    private static final String T_TOGGLEREAD = "syndie.gui.messagetreepanel.toggleread";
    private static final String T_CREATEREF = "syndie.gui.messagetreepanel.createref";
    
    private class EastEdge extends DesktopEdge implements Themeable, Translatable {
        private Hash _actionScope;
        private SyndieURI _detailURI;
        private Button _expand;
        private Button _collapse;
        private Button _profile;
        private Button _view;
        private Button _toggleRead;
        private Button _createRef;
        
        public EastEdge(Composite edge, UI ui) {
            super(edge, ui);
            _actionScope = null;
            initComponents();
        }
        private void initComponents() {
            final Composite root = getEdgeRoot();
            root.setLayout(new FillLayout(SWT.VERTICAL));
            
            _expand = new Button(root, SWT.PUSH);
            _expand.setEnabled(false);
            _expand.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent evt) {
                    if ((evt.stateMask & SWT.MOD1) == SWT.MOD1) {
                        _ui.debugMessage("expand all (default selected)");
                        expand(true);
                    } else {
                        _ui.debugMessage("expand single (default selected)");
                        expand(false);
                    }
                    _tree.forceFocus();
                }
                public void widgetSelected(SelectionEvent evt) {
                    if ((evt.stateMask & SWT.MOD1) == SWT.MOD1) {
                        _ui.debugMessage("expand all (selected)");
                        expand(true);
                    } else {
                        _ui.debugMessage("expand single (selected)");
                        expand(false);
                    }
                    _tree.forceFocus();
                }
            });
            _expand.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _profile, _themeRegistry.getTheme().SHELL_FONT, _translationRegistry.getText(T_EXPAND, "Expand"));
                }
            });
            
            _collapse = new Button(root, SWT.PUSH);
            _collapse.setEnabled(false);
            _collapse.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent evt) {
                    if ((evt.stateMask & SWT.MOD1) == SWT.MOD1) {
                        _ui.debugMessage("collapse all (default selected)");
                        collapse(true);
                    } else {
                        _ui.debugMessage("collapse (default selected)");
                        collapse(false);
                    }
                    _tree.forceFocus();
                }
                public void widgetSelected(SelectionEvent evt) {
                    if ((evt.stateMask & SWT.MOD1) == SWT.MOD1) {
                        _ui.debugMessage("collapse all (selected)");
                        collapse(true);
                    } else {
                        _ui.debugMessage("collapse (selected)");
                        collapse(false);
                    }
                    _tree.forceFocus();
                }
            });
            _collapse.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _profile, _themeRegistry.getTheme().SHELL_FONT, _translationRegistry.getText(T_COLLAPSE, "Collapse"));
                }
            });
            
            _profile = new Button(root, SWT.PUSH);
            _profile.addSelectionListener(new FireSelectionListener() { 
                public void fire() { 
                    viewProfile(); 
                    _tree.forceFocus();
                } 
            });
            _profile.setEnabled(false);
            _profile.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _profile, _themeRegistry.getTheme().SHELL_FONT, _translationRegistry.getText(T_PROFILE, "Profile"));
                }
            });

            _view = new Button(root, SWT.PUSH);
            _view.addSelectionListener(new FireSelectionListener() { 
                public void fire() { 
                    viewMessage(); 
                    _tree.forceFocus();
                } 
            });
            _view.setEnabled(false);
            _view.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _profile, _themeRegistry.getTheme().SHELL_FONT, _translationRegistry.getText(T_VIEW, "View"));
                }
            });

            _toggleRead = new Button(root, SWT.PUSH);
            _toggleRead.addSelectionListener(new FireSelectionListener() { 
                public void fire() { 
                    toggleRead(); 
                    _tree.forceFocus();
                } 
            });
            _toggleRead.setEnabled(false);
            _toggleRead.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _profile, _themeRegistry.getTheme().SHELL_FONT, _translationRegistry.getText(T_TOGGLEREAD, "Mark read"));
                }
            });

            _createRef = new Button(root, SWT.PUSH);
            _createRef.addSelectionListener(new FireSelectionListener() { 
                public void fire() { 
                    createRef(); 
                    _tree.forceFocus();
                } 
            });
            _createRef.setEnabled(false);
            _createRef.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _profile, _themeRegistry.getTheme().SHELL_FONT, _translationRegistry.getText(T_CREATEREF, "Create ref"));
                }
            });
        }
        public void applyTheme(Theme theme) {
            _expand.redraw();
            _collapse.redraw();
            _profile.redraw();
            _view.redraw();
            _toggleRead.redraw();
            _createRef.redraw();
            getEdgeRoot().layout(true, true);
        }
        public void translate(TranslationRegistry registry) {
            _expand.redraw();
            _collapse.redraw();
            _profile.redraw();
            _view.redraw();
            _toggleRead.redraw();
            _createRef.redraw();
        }
        public void updateNav(SyndieURI uri, SyndieURI detailURI) {
            if (uri == null) return; // no change, reshown
            _ui.debugMessage("updating nav w/ detailURI: " + detailURI);
            Hash scope = null;
            if (uri.isChannel()) {
                scope = uri.getScope();
            } else if (uri.isSearch()) {
                Hash scopes[] = uri.getSearchScopes();
                if ( (scopes != null) && (scopes.length == 1) )
                    scope = scopes[0];
            }
            _actionScope = scope;
            _detailURI = detailURI;
            _profile.setEnabled(scope != null);
            _expand.setEnabled(detailURI != null);
            _collapse.setEnabled(detailURI != null);
            _toggleRead.setEnabled(detailURI != null);
            _view.setEnabled(detailURI != null);
            _createRef.setEnabled(detailURI != null);
        }
    }
    
    private void expand(boolean all) { _tree.expandSelected(all); }
    private void collapse(boolean all) { _tree.collapseSelected(all); }
    private void toggleRead() { _tree.toggleRead(); }
    private void viewProfile() {
        if (_actionScope != null)
            _desktop.getNavControl().view(URIHelper.instance().createMetaURI(_actionScope));
    }
    private void viewMessage() {
        if (_detailURI != null)
            _desktop.getNavControl().view(_detailURI);
    }
    private void createRef() {
        AddReferenceSource src = new AddReferenceSource(_client, _ui, _themeRegistry, _translationRegistry, getRoot());
        LinkBuilderPopup popup = new LinkBuilderPopup(_client, _ui, _themeRegistry, _translationRegistry, getRoot().getShell(), src);
        src.setPopup(popup);
        popup.showPopup(_detailURI);
    }

}

