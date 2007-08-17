package syndie.gui.desktop;

import java.util.ArrayList;
import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;
import syndie.gui.ChannelSelectorPanel;
import syndie.gui.ColorUtil;
import syndie.gui.FireSelectionListener;
import syndie.gui.ImageUtil;
import syndie.gui.LinkBuilderPopup;
import syndie.gui.MessageTree;
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
public class ForumSelectionPanel extends DesktopPanel implements ChannelSelectorPanel.ChannelSelectorListener {
    private ChannelSelectorPanel _channels;
    private NavigationControl _navControl;
    private boolean _preferRefs;
    /** last time we picked a forum, were we looking at the manageable forums only? */
    private boolean _prevWasManageable;
    
    public ForumSelectionPanel(Desktop desktop, DBClient client, ThemeRegistry themes, TranslationRegistry trans, Composite parent, UI ui, NavigationControl navControl) {
        super(desktop, client, themes, trans, parent, ui, null);
        _navControl = navControl;
        _preferRefs = false;
        initComponents();
    }
    
    public String getPanelName() { return "Forum selection"; }
    public String getPanelDescription() { return "Main screen to select forums"; }

    protected void dispose() {
        // noop.  do not call super.dispose, because we don't want to actually go away
        _channels.resetPanel();
        ((NorthEdge)_edgeNorth).updateInfo(null, -1, null, null, null);
    }
    
    private void initComponents() {
        Composite root = getRoot();
        _channels = new ChannelSelectorPanel(_client, _ui, _themeRegistry, _translationRegistry, root, this);
    }
    public void preferRefs(boolean preferRefs) { _preferRefs = preferRefs; }
    
    public void shown(Desktop desktop, SyndieURI uri, String suggestedName, String suggestedDescription) {
        _desktop = desktop;
        if ( (_channels.getRecordCount() == 0) || (_prevWasManageable) ) {
            if (_preferRefs)
                _channels.showReferences(null);
            else
                _channels.showWatched(false, null);
        }
        _prevWasManageable = false;
        super.shown(desktop, uri, suggestedName, suggestedDescription);
    }
    public void hidden(Desktop desktop) {}
    public void showManageable(Desktop desktop, SyndieURI uri, String suggestedName, String suggestedDescription) {
        _prevWasManageable = true;
        _desktop = desktop;
        _channels.showManageable(null);
        super.shown(desktop, uri, suggestedName, suggestedDescription);
    }

    public void channelReviewed(SyndieURI uri, long channelId, String name, String description, Image avatar) {
        //_ui.debugMessage("channel reviewed: " + scope + ": " + name);
        Hash scope = null;
        if (uri != null) {
            if (uri.isChannel()) {
                scope = uri.getScope();
            } else if (uri.isSearch()) {
                Hash scopes[] = uri.getSearchScopes();
                if ( (scopes != null) && (scopes.length == 1) )
                    scope = scopes[0];
            }
        }
                
        ((NorthEdge)_edgeNorth).updateInfo(scope, channelId, name, description, avatar);
    }

    public void channelSelected(SyndieURI uri, int matchedIndex) {
        _ui.debugMessage("channel selected: " + uri);
        if (uri == null) {
            List nodes = _channels.getMatchingNodes();
            if ( (nodes != null) && (matchedIndex < nodes.size()) ) {
                final ReferenceNode node = (ReferenceNode)nodes.get(matchedIndex);
                if (node.getChildCount() > 0) {
                    _channels.setChannelIdSource(new ChannelSelectorPanel.ChannelIdSource() {
                        public List listChannelIds() { return null; }
                        public List getReferenceNodes() {
                            List rv = new ArrayList();
                            for (int i = 0; i < node.getChildCount(); i++)
                                rv.add(node.getChild(i));
                            return rv;
                        }
                    });
                    _channels.recalcChannels();
                } else {
                    // view an empty folder.  noop
                }
            }
        } else {
            _navControl.view(uri);
            _desktop.panelDisposed(this, false);
        }
    }
    
    public void channelProfileSelected(SyndieURI uri, int matchedIndex) {
        if ( (uri != null) && (uri.getScope() != null) ) {
            _navControl.view(URIHelper.instance().createManageURI(uri.getScope()));
            _desktop.panelDisposed(this, false);
        }
    }
    
    public void viewMatches() {
        List scopes = _channels.getMatches(true);
        boolean unread = _channels.isUnreadOnly();
        boolean threaded = true;
        boolean useImportDate = MessageTree.shouldUseImportDate(_client);
        _ui.debugMessage("all matching channels selected: " + scopes);
        _navControl.view(SyndieURI.createSearch(scopes, unread, threaded, useImportDate));
        _desktop.panelDisposed(this, false);
    }

    public void forumSelectorCancelled() {
        _ui.debugMessage("channel selector cancelled");
        // don't actually dispose the panel, because the desktop keeps one of these around, but
        // tell the desktop to remove it from the active list
        _desktop.panelDisposed(this, true);
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

    private static final String T_CANCEL = "syndie.gui.desktop.forumselectionpanel.cancel";
    private static final String T_VIEWMATCHES = "syndie.gui.desktop.forumselectionpanel.viewmatches";
    private static final String T_ADDNYM = "syndie.gui.desktop.forumselectionpanel.addnym";
    private static final String T_ADDFORUM = "syndie.gui.desktop.forumselectionpanel.addforum";
    private static final String T_ADDREF = "syndie.gui.desktop.forumselectionpanel.addref";
    class SouthEdge extends DesktopEdge implements Themeable, Translatable {
        private Button _viewMatches;
        private Button _addNym;
        private Button _addForum;
        private Button _addReference;
        private Button _cancel;
        public SouthEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        private void initComponents() {
            Composite root = getEdgeRoot();
            root.setLayout(new FillLayout(SWT.HORIZONTAL));
            _viewMatches = new Button(root, SWT.PUSH);
            _viewMatches.addSelectionListener(new FireSelectionListener() {
                public void fire() { viewMatches(); }
            });
            _addNym = new Button(root, SWT.PUSH);
            _addNym.addSelectionListener(new FireSelectionListener() {
                public void fire() { _navControl.view(URIHelper.instance().createManageURI(null)); }
            });
            _addForum = new Button(root, SWT.PUSH);
            _addForum.addSelectionListener(new FireSelectionListener() {
                public void fire() { _navControl.view(URIHelper.instance().createManageURI(null)); }
            });
            _addReference = new Button(root, SWT.PUSH);
            _addReference.addSelectionListener(new FireSelectionListener() {
                public void fire() { addReference(); }
            });
            _cancel = new Button(root, SWT.PUSH);
            _cancel.addSelectionListener(new FireSelectionListener() {
                public void fire() { forumSelectorCancelled(); }
            });
            _translationRegistry.register(SouthEdge.this);
            _themeRegistry.register(SouthEdge.this);
        }
        public void translate(TranslationRegistry trans) {
            _viewMatches.setText(trans.getText(T_VIEWMATCHES, "View combined"));
            _addNym.setText(trans.getText(T_ADDNYM, "Create nym"));
            _addForum.setText(trans.getText(T_ADDFORUM, "Create forum"));
            _addReference.setText(trans.getText(T_ADDREF, "Create reference"));
            _cancel.setText(trans.getText(T_CANCEL, "Cancel"));
        }
        public void applyTheme(Theme theme) { 
            _viewMatches.setFont(theme.BUTTON_FONT);
            _addNym.setFont(theme.BUTTON_FONT);
            _addForum.setFont(theme.BUTTON_FONT);
            _addReference.setFont(theme.BUTTON_FONT);
            _cancel.setFont(theme.BUTTON_FONT);
        }
        
        private void addReference() {
            AddReferenceSource src = new AddReferenceSource(_client, _ui, _themeRegistry, _translationRegistry, getRoot());
            LinkBuilderPopup popup = new LinkBuilderPopup(_client, _ui, _themeRegistry, _translationRegistry, getRoot().getShell(), src);
            src.setPopup(popup);
            popup.showPopup();
        }
        
    }

    class NorthEdge extends DesktopEdge implements Themeable {
        private Label _avatar;
        private Label _info;
        public NorthEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        private void initComponents() {
            Composite root = NorthEdge.this.getEdgeRoot();
            GridLayout gl = new GridLayout(2, false);
            //gl.horizontalSpacing = 0;
            gl.verticalSpacing = 0;
            gl.marginHeight = 0;
            gl.marginWidth = 0;
            root.setLayout(gl);
            _avatar = new Label(root, SWT.NONE);
            _avatar.setImage(null);
            _avatar.setLayoutData(new GridData(64, SWT.DEFAULT)); // the frame pegs the height at 64
            _info = new Label(root, SWT.SINGLE);
            _info.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, true, true));
            
            Color black = ColorUtil.getColor("black");
            Color white = ColorUtil.getColor("white");
            _avatar.setBackground(white);
            _info.setBackground(white);
            _info.setForeground(black);
            root.setBackground(white);
            root.setForeground(black);
            _themeRegistry.register(NorthEdge.this);
        }
        public void updateInfo(Hash scope, long channelId, String name, String description, Image avatar) {
            _avatar.setImage(avatar);
            StringBuffer buf = new StringBuffer();
            if ( (name != null) && (name.length() > 0) )
                buf.append(name).append(" ");
            if (scope != null)
                buf.append("[").append(scope.toBase64().substring(0,6)).append("] ");
            if (description != null)
                buf.append(description);
            _info.setText(buf.toString());
            NorthEdge.this.getEdgeRoot().layout(true, true);
        }
        public void applyTheme(Theme theme) {
            _info.setFont(theme.SHELL_FONT);
            getEdgeRoot().layout(true, true);
        }
    }
    
    private static final String T_WATCHED_TT = "syndie.gui.forumselectionpanel.watched.tt";
    private static final String T_REFS_TT = "syndie.gui.forumselectionpanel.refs.tt";
    private static final String T_IDENT_TT = "syndie.gui.forumselectionpanel.ident.tt";
    private static final String T_MANAGEABLE_TT = "syndie.gui.forumselectionpanel.manageable.tt";
    private static final String T_POSTABLE_TT = "syndie.gui.forumselectionpanel.postable.tt";

    private static final String T_WATCHED = "syndie.gui.forumselectionpanel.watched";
    private static final String T_REFS = "syndie.gui.forumselectionpanel.refs";
    private static final String T_IDENT = "syndie.gui.forumselectionpanel.ident";
    private static final String T_MANAGEABLE = "syndie.gui.forumselectionpanel.manageable";
    private static final String T_POSTABLE = "syndie.gui.forumselectionpanel.postable";
    
    class EastEdge extends DesktopEdge implements Themeable, Translatable {
        private Button _watched;
        private Button _refs;
        private Button _ident;
        private Button _manageable;
        private Button _postable;
        public EastEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        private void initComponents() {
            Composite root = getEdgeRoot();
            root.setLayout(new FillLayout(SWT.VERTICAL));
            
            _watched = new Button(root, SWT.PUSH);
            _watched.addSelectionListener(new FireSelectionListener() { 
                public void fire() { _channels.showWatched(null); } 
            });
            _watched.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _watched, _themeRegistry.getTheme().BUTTON_FONT, _translationRegistry.getText(T_WATCHED, "Watched"));
                }
            });
            
            _refs = new Button(root, SWT.PUSH);
            _refs.addSelectionListener(new FireSelectionListener() { 
                public void fire() { _channels.showReferences(null); } 
            });
            _refs.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _refs, _themeRegistry.getTheme().BUTTON_FONT, _translationRegistry.getText(T_REFS, "References"));
                }
            });
            _ident = new Button(root, SWT.PUSH);
            _ident.addSelectionListener(new FireSelectionListener() { 
                public void fire() { _channels.showIdent(null); } 
            });
            _ident.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _ident, _themeRegistry.getTheme().BUTTON_FONT, _translationRegistry.getText(T_IDENT, "Identities"));
                }
            });
            _manageable = new Button(root, SWT.PUSH);
            _manageable.addSelectionListener(new FireSelectionListener() { 
                public void fire() { _channels.showManageable(null); } 
            });
            _manageable.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _manageable, _themeRegistry.getTheme().BUTTON_FONT, _translationRegistry.getText(T_MANAGEABLE, "Manageable"));
                }
            });

            _postable = new Button(root, SWT.PUSH);
            _postable.addSelectionListener(new FireSelectionListener() { 
                public void fire() { _channels.showPostable(null); } 
            });            
            _postable.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _postable, _themeRegistry.getTheme().BUTTON_FONT, _translationRegistry.getText(T_POSTABLE, "Postable"));
                }
            });

            
            //Color color = ColorUtil.getColor("yellow");
            //_watched.setBackground(color);
            //_refs.setBackground(color);
            //_ident.setBackground(color);
            //_manageable.setBackground(color);
            //_postable.setBackground(color);
            
            _translationRegistry.register(EastEdge.this);
            _themeRegistry.register(EastEdge.this);
        }
        public void translate(TranslationRegistry trans) {
            _watched.setToolTipText(trans.getText(T_WATCHED_TT, "Show watched forums"));
            _refs.setToolTipText(trans.getText(T_REFS_TT, "Show references"));
            _ident.setToolTipText(trans.getText(T_IDENT_TT, "Show nyms"));
            _manageable.setToolTipText(trans.getText(T_MANAGEABLE_TT, "Show manageable forums"));
            _postable.setToolTipText(trans.getText(T_POSTABLE_TT, "Show postable forums"));

            _watched.redraw();
            _refs.redraw();
            _ident.redraw();
            _manageable.redraw();
            _postable.redraw();
        }
        
        public void applyTheme(Theme theme) { 
            _watched.redraw();
            _refs.redraw();
            _ident.redraw();
            _manageable.redraw();
            _postable.redraw();
        }
    }
}
