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
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;
import syndie.gui.BanControl;
import syndie.gui.BookmarkControl;
import syndie.gui.NymChannelTree;
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
class ForumSelectionPanel extends DesktopPanel {
    private NymChannelTree _channels;
    private BanControl _banControl;
    private NavigationControl _navControl;
    private BookmarkControl _bookmarkControl;
    private boolean _preferRefs;
    /** last time we picked a forum, were we looking at the manageable forums only? */
    private boolean _prevWasManageable;
    
    public ForumSelectionPanel(Desktop desktop, DBClient client, ThemeRegistry themes, TranslationRegistry trans, Composite parent, UI ui, NavigationControl navControl, BanControl banControl, BookmarkControl bookmarkControl) {
        super(desktop, client, themes, trans, parent, ui, null);
        _navControl = navControl;
        _banControl = banControl;
        _bookmarkControl = bookmarkControl;
        _preferRefs = false;
        initComponents();
    }
    
    public String getPanelName() { return "Forum selection"; }
    public String getPanelDescription() { return "Main screen to select forums"; }

    protected void dispose() {
        // noop.  do not call super.dispose, because we don't want to actually go away
        //_channels.resetPanel();
        ((NorthEdge)_edgeNorth).updateInfo(null, -1, null, null, null);
    }
    
    private void initComponents() {
        Composite root = getRoot();
        _channels = new NymChannelTree(_client, _ui, _themeRegistry, _translationRegistry, _navControl, _banControl, _bookmarkControl, root, new NymChannelTree.NymChannelTreeListener() {
            public void channelSelected(SyndieURI uri) { 
                _navControl.view(uri);
                _desktop.panelDisposed(ForumSelectionPanel.this, false);
            }
            public void channelProfileSelected(SyndieURI uri) {
                _navControl.view(uri);
                _desktop.panelDisposed(ForumSelectionPanel.this, false);
            }
            public void channelManageSelected(SyndieURI manageURI) {
                _navControl.view(manageURI);
                _desktop.panelDisposed(ForumSelectionPanel.this, false);
            }
            public void channelPostSelected(SyndieURI postURI) {
                _navControl.view(postURI);
                _desktop.panelDisposed(ForumSelectionPanel.this, false);
            }
            public void channelPreviewed(Hash scope, long chanId, String name, String desc, Image avatar) {
                if (scope != null)
                    ((NorthEdge)_edgeNorth).updateInfo(scope, chanId, name, desc, avatar);
                else
                    ((NorthEdge)_edgeNorth).updateInfo(null, -1, null, null, null);
            }
        }, false);
        _channels.showNymChannels();
        forceFocus();
    }
    public void preferRefs(boolean preferRefs) { _preferRefs = preferRefs; }
    public boolean isShowingRefs() { return _channels.isShowingRefs(); }
    
    void bookmarksUpdated() { _channels.bookmarksUpdated(); }
    void nymChannelsUpdated() { _channels.nymChannelsUpdated(); }
    
    public void forceFocus() { _channels.forceFocus(); }
    
    public void shown(Desktop desktop, SyndieURI uri, String suggestedName, String suggestedDescription) {
        _desktop = desktop;
        _prevWasManageable = false;
        
        if (_preferRefs)
            _channels.showBookmarks();
        else
            _channels.showNymChannels();
        
        super.shown(desktop, uri, suggestedName, suggestedDescription);
    }
    public void hidden(Desktop desktop) {}
    
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
    //protected void buildEast(Composite edge) { 
    //    if (_edgeEast == null) _edgeEast = new EastEdge(edge, _ui); 
    //}

    class SouthEdge extends DesktopEdge implements Themeable, Translatable {
        private Button _viewMatches;
        private Button _addNym;
        private Button _addForum;
        private Button _addReference;
        public SouthEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        private void initComponents() {
            Composite root = getEdgeRoot();
            root.setLayout(new FillLayout(SWT.HORIZONTAL));
            _viewMatches = new Button(root, SWT.PUSH);
            _viewMatches.addSelectionListener(new FireSelectionListener() {
                public void fire() { _channels.viewMatching(); }
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
            _translationRegistry.register(SouthEdge.this);
            _themeRegistry.register(SouthEdge.this);
        }
        public void translate(TranslationRegistry trans) {
            _viewMatches.setText(trans.getText("View combined"));
            _addNym.setText(trans.getText("Create nym"));
            _addForum.setText(trans.getText("Create forum"));
            _addReference.setText(trans.getText("Create bookmark"));
        }
        public void applyTheme(Theme theme) { 
            _viewMatches.setFont(theme.BUTTON_FONT);
            _addNym.setFont(theme.BUTTON_FONT);
            _addForum.setFont(theme.BUTTON_FONT);
            _addReference.setFont(theme.BUTTON_FONT);
        }
        
        private void addReference() {
            AddReferenceSource src = new AddReferenceSource(_client, _ui, _themeRegistry, _translationRegistry, getRoot());
            LinkBuilderPopup popup = new LinkBuilderPopup(_client, _ui, _themeRegistry, _translationRegistry, _navControl, _banControl, _bookmarkControl, getRoot().getShell(), src);
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
            StringBuilder buf = new StringBuilder();
            if ( (name != null) && (name.length() > 0) )
                buf.append(name).append(" ");
            //if (scope != null)
            //    buf.append("[").append(scope.toBase64().substring(0,6)).append("] ");
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
}
