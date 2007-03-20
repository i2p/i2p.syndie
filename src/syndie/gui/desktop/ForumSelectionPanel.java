package syndie.gui.desktop;

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
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;
import syndie.gui.ChannelSelectorPanel;
import syndie.gui.ColorUtil;
import syndie.gui.FireSelectionListener;
import syndie.gui.MessageTree;
import syndie.gui.NavigationControl;
import syndie.gui.Theme;
import syndie.gui.ThemeRegistry;
import syndie.gui.Themeable;
import syndie.gui.Translatable;
import syndie.gui.TranslationRegistry;

/**
 *
 */
public class ForumSelectionPanel extends DesktopPanel implements ChannelSelectorPanel.ChannelSelectorListener {
    private ChannelSelectorPanel _channels;
    private NavigationControl _navControl;
    
    public ForumSelectionPanel(Desktop desktop, DBClient client, ThemeRegistry themes, TranslationRegistry trans, Composite parent, UI ui, NavigationControl navControl) {
        super(desktop, client, themes, trans, parent, ui, null);
        _navControl = navControl;
        initComponents();
    }
    
    public String getPanelName() { return "Forum selection"; }
    public String getPanelDescription() { return "Main screen to select forums"; }

    private void initComponents() {
        Composite root = getRoot();
        _channels = new ChannelSelectorPanel(_client, _ui, _themeRegistry, _translationRegistry, root, this);
    }
    
    public void shown(Desktop desktop, SyndieURI uri, String suggestedName, String suggestedDescription) {
        _desktop = desktop;
        if (_channels.getRecordCount() == 0)
            _channels.showWatched(false, null);
        super.shown(desktop, uri, suggestedName, suggestedDescription);
    }
    public void hidden(Desktop desktop) {}

    public void channelReviewed(Hash scope, long channelId, String name, String description, Image avatar) {
        //_ui.debugMessage("channel reviewed: " + scope + ": " + name);
        ((NorthEdge)_edgeNorth).updateInfo(scope, channelId, name, description, avatar);
    }

    public void channelSelected(Hash scope) {
        _ui.debugMessage("channel selected: " + scope);
        _navControl.view(SyndieURI.createScope(scope));
        _desktop.panelDisposed(this, false);
    }
    
    public void viewMatches() {
        List scopes = _channels.getMatches();
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
    class SouthEdge extends DesktopEdge implements Themeable, Translatable {
        private Button _viewMatches;
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
            _cancel = new Button(root, SWT.PUSH);
            _cancel.addSelectionListener(new FireSelectionListener() {
                public void fire() { forumSelectorCancelled(); }
            });
            _translationRegistry.register(SouthEdge.this);
            _themeRegistry.register(SouthEdge.this);
        }
        public void translate(TranslationRegistry trans) {
            _viewMatches.setText(trans.getText(T_VIEWMATCHES, "View all matches"));
            _cancel.setText(trans.getText(T_CANCEL, "Cancel"));
        }
        public void applyTheme(Theme theme) { 
            _viewMatches.setFont(theme.BUTTON_FONT);
            _cancel.setFont(theme.BUTTON_FONT);
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
    private static final String T_IDENT_TT = "syndie.gui.forumselectionpanel.ident.tt";
    private static final String T_MANAGEABLE_TT = "syndie.gui.forumselectionpanel.manageable.tt";
    private static final String T_POSTABLE_TT = "syndie.gui.forumselectionpanel.postable.tt";
    
    class EastEdge extends DesktopEdge implements Themeable, Translatable {
        private Button _watched;
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
            _ident = new Button(root, SWT.PUSH);
            _ident.addSelectionListener(new FireSelectionListener() { 
                public void fire() { _channels.showIdent(null); } 
            });
            _manageable = new Button(root, SWT.PUSH);
            _manageable.addSelectionListener(new FireSelectionListener() { 
                public void fire() { _channels.showManageable(null); } 
            });
            _postable = new Button(root, SWT.PUSH);
            _postable.addSelectionListener(new FireSelectionListener() { 
                public void fire() { _channels.showPostable(null); } 
            });
            
            _watched.setBackground(ColorUtil.getColor("blue"));
            _ident.setBackground(ColorUtil.getColor("blue"));
            _manageable.setBackground(ColorUtil.getColor("blue"));
            _postable.setBackground(ColorUtil.getColor("blue"));
            
            _translationRegistry.register(EastEdge.this);
            _themeRegistry.register(EastEdge.this);
        }
        public void translate(TranslationRegistry trans) {
            _watched.setToolTipText(trans.getText(T_WATCHED_TT, "Show watched forums"));
            _ident.setToolTipText(trans.getText(T_IDENT_TT, "Show nyms"));
            _manageable.setToolTipText(trans.getText(T_MANAGEABLE_TT, "Show manageable forums"));
            _postable.setToolTipText(trans.getText(T_POSTABLE_TT, "Show postable forums"));
        }
        public void applyTheme(Theme theme) { 
            _watched.setFont(theme.BUTTON_FONT);
            _ident.setFont(theme.BUTTON_FONT);
            _manageable.setFont(theme.BUTTON_FONT);
            _postable.setFont(theme.BUTTON_FONT);
        }
    }
}
