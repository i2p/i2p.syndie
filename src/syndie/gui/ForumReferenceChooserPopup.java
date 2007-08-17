package syndie.gui;

import java.util.ArrayList;
import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 *
 */
public class ForumReferenceChooserPopup extends BaseComponent implements ReferenceChooserPopup, ChannelSelectorPanel.ChannelSelectorListener, Themeable, Translatable {
    private Composite _parent;
    private Shell _shell;
    private ChannelSelectorPanel _channels;
    private boolean _preferRefs;
    private ReferenceChooserTree.AcceptanceListener _acceptListener;
    // control the channels
    private Button _watched;
    private Button _refs;
    private Button _ident;
    private Button _manageable;
    private Button _postable;
    
    private Button _cancel;
    
    public ForumReferenceChooserPopup(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite parent, ReferenceChooserTree.AcceptanceListener acceptLsnr) {
        super(client, ui, themes, trans);
        _parent = parent;
        _preferRefs = false;
        _acceptListener = acceptLsnr;
        initComponents();
    }
    
    public void dispose() {
        _themeRegistry.unregister(this);
        _translationRegistry.unregister(this);
        if (!_shell.isDisposed()) _shell.dispose();
        _channels.dispose();
    }
    
    private void initComponents() {
        _shell = new Shell(_parent.getShell(), SWT.SHELL_TRIM);
        GridLayout gl = new GridLayout(5, true);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        _shell.setLayout(gl);
        
        _watched = new Button(_shell, SWT.PUSH);
        _watched.addSelectionListener(new FireSelectionListener() { 
            public void fire() { _channels.showWatched(null); } 
        });
        _watched.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _refs = new Button(_shell, SWT.PUSH);
        _refs.addSelectionListener(new FireSelectionListener() { 
            public void fire() { _channels.showReferences(null); } 
        });
        _refs.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _ident = new Button(_shell, SWT.PUSH);
        _ident.addSelectionListener(new FireSelectionListener() { 
            public void fire() { _channels.showIdent(null); } 
        });
        _ident.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _manageable = new Button(_shell, SWT.PUSH);
        _manageable.addSelectionListener(new FireSelectionListener() { 
            public void fire() { _channels.showManageable(null); } 
        });
        _manageable.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _postable = new Button(_shell, SWT.PUSH);
        _postable.addSelectionListener(new FireSelectionListener() { 
            public void fire() { _channels.showPostable(null); } 
        });
        _postable.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));

        Color color = ColorUtil.getColor("yellow");
        _watched.setBackground(color);
        _refs.setBackground(color);
        _ident.setBackground(color);
        _manageable.setBackground(color);
        _postable.setBackground(color);
        
        _channels = new ChannelSelectorPanel(_client, _ui, _themeRegistry, _translationRegistry, _shell, this);
        _channels.getRoot().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 5, 1));
        
        _cancel = new Button(_shell, SWT.PUSH);
        _cancel.addSelectionListener(new FireSelectionListener() { public void fire() { forumSelectorCancelled(); } });
        _cancel.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 5, 1));
        
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) {
                evt.doit = false;
                forumSelectorCancelled();
            }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }

    public void setListener(ReferenceChooserTree.AcceptanceListener lsnr) { _acceptListener = lsnr; }
    public void show() { open(); }
    public void hide() { dispose(); }
    
    public void open() {
        if ( (_shell == null) || (_shell.isDisposed()) )
            initComponents(); // reopened
        
        if (_channels.getRecordCount() == 0) {
            if (_preferRefs)
                _channels.showReferences(null);
            else
                _channels.showWatched(false, null);
        }
        _shell.pack(true);
        _shell.setSize(_shell.computeSize(SWT.DEFAULT, 500));
        _shell.open();
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
                
        //((NorthEdge)_edgeNorth).updateInfo(scope, channelId, name, description, avatar);
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
            _acceptListener.referenceAccepted(uri);
            //_navControl.view(uri);
            dispose();
            //_desktop.panelDisposed(this, false);
        }
    }
    
    public void channelProfileSelected(SyndieURI uri, int matchedIndex) {
        // ignore in this scenario
    }
    
    public void forumSelectorCancelled() {
        _ui.debugMessage("channel selector cancelled");
        _acceptListener.referenceChoiceAborted();
        dispose();
    }

    public void applyTheme(Theme theme) {
        _shell.setFont(theme.SHELL_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
        _watched.setFont(theme.BUTTON_FONT);
        _refs.setFont(theme.BUTTON_FONT);
        _ident.setFont(theme.BUTTON_FONT);
        _manageable.setFont(theme.BUTTON_FONT);
        _postable.setFont(theme.BUTTON_FONT);
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
    
    private static final String T_TITLE = "syndie.gui.forumselectionpanel.title";
    private static final String T_CANCEL = "syndie.gui.forumselectionpanel.cancel";
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText(T_TITLE, "Select forum"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel"));
        
        _watched.setToolTipText(registry.getText(T_WATCHED_TT, "Show watched forums"));
        _refs.setToolTipText(registry.getText(T_REFS_TT, "Show references"));
        _ident.setToolTipText(registry.getText(T_IDENT_TT, "Show nyms"));
        _manageable.setToolTipText(registry.getText(T_MANAGEABLE_TT, "Show manageable forums"));
        _postable.setToolTipText(registry.getText(T_POSTABLE_TT, "Show postable forums"));

        _watched.setText(registry.getText(T_WATCHED, "Watched"));
        _refs.setText(registry.getText(T_REFS, "Referenced"));
        _ident.setText(registry.getText(T_IDENT, "Nyms"));
        _manageable.setText(registry.getText(T_MANAGEABLE, "Manageable"));
        _postable.setText(registry.getText(T_POSTABLE, "Postable"));
    }
}
