package syndie.gui.desktop;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
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
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.UI;
import syndie.gui.BookmarkControl;
import syndie.gui.ColorUtil;
import syndie.gui.DataCallback;
import syndie.gui.FireSelectionListener;
import syndie.gui.ImageUtil;
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
    
    public MessageTreePanel(Desktop desktop, SyndieURI origURI) {
        super(desktop, desktop.getDBClient(), desktop.getThemeRegistry(), desktop.getTranslationRegistry(), desktop.getCenter(), desktop.getUI(), origURI);
        // add this filter to the display only when this panel is current
        _keyListener = new Listener() {
            public void handleEvent(Event evt) {
                if (evt.keyCode == SWT.F5) {
                    _tree.applyFilter();
                    evt.type = SWT.None;
                }
            }
        };
        initComponents();
    }
    
    private void initComponents() {
        BookmarkControl bookmarkControl = null;
        DataCallback dataCallback = null;
        _tree = new WatchedMessageTree(_client, _ui, _themeRegistry, _translationRegistry, _desktop.getNavControl(), URIHelper.instance(), 
                bookmarkControl, dataCallback, getRoot(), new MessageTree.MessageTreeListener() {
            public void messageSelected(MessageTree tree, SyndieURI uri, boolean toView, boolean nodelay) {
                if (toView)
                    _desktop.getNavControl().view(uri);
            }

            public void filterApplied(MessageTree tree, SyndieURI searchURI) {
                _ui.debugMessage("message tree panel: filter applied: " + searchURI);
                ((NorthEdge)_edgeNorth).update(searchURI);
            }
        }, false);
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }

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
        ((SouthEdge)_edgeSouth).updateActions(uri);
        ((EastEdge)_edgeEast).updateNav(uri);
        getRoot().getDisplay().addFilter(SWT.KeyDown, _keyListener);
        super.shown(desktop, uri, name, description);
    }
    
    public void hidden() {
        getRoot().getDisplay().removeFilter(SWT.KeyDown, _keyListener);
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
        }

        public void applyTheme(Theme theme) {
            _summary.setFont(theme.SHELL_FONT);
            _refs.setFont(theme.DEFAULT_FONT);
            getEdgeRoot().layout(true, true);
        }
        public void translate(TranslationRegistry registry) {}
    }
    
    private static final String T_POST = "syndie.gui.messagetreepanel.post";
    private static final String T_PRIVMSG = "syndie.gui.messagetreepanel.privmsg";
    private static final String T_WATCH = "syndie.gui.messagetreepanel.watch";
    private static final String T_BAN = "syndie.gui.messagetreepanel.ban";
    
    private class SouthEdge extends DesktopEdge implements Themeable, Translatable {
        private Button _post;
        private Button _privmsg;
        private Button _watch;
        private Button _ban;
        private Hash _actionScope;
        
        public SouthEdge(Composite edge, UI ui) {
            super(edge, ui);
            _actionScope = null;
            initComponents();
        }
        public void updateActions(final SyndieURI uri) {
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
                        long channelId = _client.getChannelId(actionScope);
                        DBClient.ChannelCollector chans = _client.getChannels(true, true, true, true, false);
                        final boolean postable = chans.getAllIds().contains(new Long(channelId));
                        final boolean privmsg = true; // true for all channels
                        final boolean watched = _client.isWatched(channelId);
                        final boolean banned = _client.getBannedChannels().contains(actionScope);
                        Display.getDefault().asyncExec(new Runnable() {
                            public void run() {
                                _actionScope = actionScope;
                                _post.setEnabled(postable);
                                _privmsg.setEnabled(privmsg);
                                _watch.setEnabled(!watched);
                                _ban.setEnabled(!banned);
                            }
                        });
                    }
                });
            } else {
                // multiple scopes
                _post.setEnabled(false);
                _privmsg.setEnabled(false);
                _watch.setEnabled(false);
                _ban.setEnabled(false);
            }
        }
        private void initComponents() {
            Composite root = getEdgeRoot();
            root.setLayout(new FillLayout(SWT.HORIZONTAL));
            
            _post = new Button(root, SWT.PUSH);
            _post.addSelectionListener(new FireSelectionListener() { public void fire() { post(); } });
            _privmsg = new Button(root, SWT.PUSH);
            _privmsg.addSelectionListener(new FireSelectionListener() { public void fire() { sendPM(); } });
            _watch = new Button(root, SWT.PUSH);
            _watch.addSelectionListener(new FireSelectionListener() { public void fire() { watch(); } });
            _ban = new Button(root, SWT.PUSH);
            _ban.addSelectionListener(new FireSelectionListener() { public void fire() { ban(); } });
            
            _post.setEnabled(false);
            _privmsg.setEnabled(false);
            _watch.setEnabled(false);
            _ban.setEnabled(false);
        }
        public void applyTheme(Theme theme) {
            _post.setFont(theme.BUTTON_FONT);
            _privmsg.setFont(theme.BUTTON_FONT);
            _watch.setFont(theme.BUTTON_FONT);
            _ban.setFont(theme.BUTTON_FONT);
            getEdgeRoot().layout(true, true);
        }
        public void translate(TranslationRegistry registry) {
            _post.setText(registry.getText(T_POST, "Post a new message"));
            _privmsg.setText(registry.getText(T_PRIVMSG, "Send a private message"));
            _watch.setText(registry.getText(T_WATCH, "Watch the forum"));
            _ban.setText(registry.getText(T_BAN, "Ban the forum"));
        }
        
        private void post() {
            _desktop.getNavControl().view(URIHelper.instance().createPostURI(_actionScope, null, false));
        }
        private void sendPM() {
            _desktop.getNavControl().view(URIHelper.instance().createPostURI(_actionScope, null, true));
        }
        private void watch() {
            _client.watchChannel(_actionScope, true, false, false, false, false);
        }
        private void ban() {
            _desktop.getBanControl().ban(_actionScope);
        }
    }

    public String getT_MULTI() {
        return T_MULTI;
    }

    public String getT_BAN() {
        return T_BAN;
    }
    
    private static final String T_PROFILE_TT = "syndie.gui.messagetreepanel.profile.tt";
    
    private class EastEdge extends DesktopEdge implements Themeable, Translatable {
        private Button _profile;
        private Hash _actionScope;
        
        public EastEdge(Composite edge, UI ui) {
            super(edge, ui);
            _actionScope = null;
            initComponents();
        }
        private void initComponents() {
            Composite root = getEdgeRoot();
            root.setLayout(new FillLayout());
            _profile = new Button(root, SWT.PUSH);
            _profile.addSelectionListener(new FireSelectionListener() { public void fire() { viewProfile(); } });
            _profile.setEnabled(false);
        }
        public void applyTheme(Theme theme) {
            getEdgeRoot().layout(true, true);
        }
        public void translate(TranslationRegistry registry) {
            _profile.setToolTipText(registry.getText(T_PROFILE_TT, "View the forum's profile"));
        }
        public void updateNav(SyndieURI uri) {
            if (uri == null) return; // no change, reshown
            Hash scope = null;
            if (uri.isChannel()) {
                scope = uri.getScope();
            } else if (uri.isSearch()) {
                Hash scopes[] = uri.getSearchScopes();
                if ( (scopes != null) && (scopes.length == 1) )
                    scope = scopes[0];
            }
            _actionScope = scope;
            _profile.setEnabled(scope != null);
        }
        private void viewProfile() {
            if (_actionScope != null)
                _desktop.getNavControl().view(URIHelper.instance().createMetaURI(_actionScope));
        }
    }
}

