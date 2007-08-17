package syndie.gui.desktop;

import java.util.ArrayList;
import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
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
import org.eclipse.swt.widgets.Label;
import syndie.Constants;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.UI;
import syndie.gui.BookmarkDnDHelper;
import syndie.gui.ColorUtil;
import syndie.gui.FireSelectionListener;
import syndie.gui.ImageCanvas;
import syndie.gui.ImageUtil;
import syndie.gui.ManageForum;
import syndie.gui.Syndicator;
import syndie.gui.Theme;
import syndie.gui.ThemeRegistry;
import syndie.gui.Themeable;
import syndie.gui.Translatable;
import syndie.gui.TranslationRegistry;
import syndie.gui.URIHelper;
import syndie.gui.ViewForum;

/**
 *
 */
public class ProfilePanel extends DesktopPanel implements Themeable, Translatable {
    private ViewForum _view;
    private ManageForum _manage;
    private Hash _scope;
    private long _scopeId;
    private boolean _editable;
  
    public ProfilePanel(Desktop desktop, DBClient client, ThemeRegistry themes, TranslationRegistry trans, Composite parent, UI ui, SyndieURI origURI) {
        super(desktop, client, themes, trans, parent, ui, origURI);
        initComponents();
    }
    
    public String getPanelName() { return "Profile"; }
    public String getPanelDescription() { return "View or manage the forum profile"; }
    
    private void initComponents() {
        Composite root = getRoot();
        root.setLayout(new FillLayout());
    
        SyndieURI uri = getOriginalURI();
        _editable = false;
        Hash scope = uri.getScope();
        if (scope == null)
            scope = uri.getHash("scope");
        if (scope != null) {
            Long chanId = new Long(_client.getChannelId(scope));
            _scope = scope;
            _scopeId = chanId.longValue();
            DBClient.ChannelCollector chans = _client.getNymChannels();
            if (chans.getIdentityChannelIds().contains(chanId) ||
                chans.getManagedChannelIds().contains(chanId)) {
                _editable = true;
            }
            
            if (_editable) {
                Long val = uri.getLong("editable");
                if ( (val != null) && (val.longValue() == 0) )
                    _editable = false;
            }
        } else {
            _editable = true;
        }
        
        if (_editable)
            _manage = new ManageForum(_client, _ui, _themeRegistry, _translationRegistry, _desktop.getNavControl(), URIHelper.instance(), _desktop.getDataCallback(), getRoot(), uri, true);
        else
            _view = new ViewForum(_client, _ui, _themeRegistry, _translationRegistry, _desktop.getBanControl(), _desktop.getNavControl(), URIHelper.instance(), getRoot(), uri);
        
        ((NorthEdge)_edgeNorth).update();
        ((SouthEdge)_edgeSouth).update();
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        if (_view != null)
            _view.dispose();
        if (_manage != null)
            _manage.dispose();
        super.dispose();
    }
    
    
    public void applyTheme(Theme theme) {
        if (_edgeNorth != null)
            ((Themeable)_edgeNorth).applyTheme(theme);
        if (_edgeEast != null)
            ((Themeable)_edgeEast).applyTheme(theme);
        if (_edgeSouth != null)
            ((Themeable)_edgeSouth).applyTheme(theme);
    }
    
    public void translate(TranslationRegistry registry) {
        if (_edgeNorth != null)
            ((Translatable)_edgeNorth).translate(registry);
        if (_edgeEast != null)
            ((Translatable)_edgeEast).translate(registry);
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
    
    private class NorthEdge extends DesktopEdge implements Themeable, Translatable {
        private Label _avatar;
        private Label _summary;
        private Combo _refs;
        private List _refNodes;
        private String _name;
        private String _description;
        
        private DragSource _avatarSrc;
        private DragSource _summarySrc;
        
        public NorthEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        
        public void update() {
            _ui.debugMessage("North edge updated");
            if (_avatar.isDisposed()) return;
            getEdgeRoot().setRedraw(false);
            ImageUtil.dispose(_avatar.getImage());
            _avatar.setImage(null);
            JobRunner.instance().enqueue(new Runnable() { public void run() { asyncUpdate(); } });
        }
        private void asyncUpdate() {
            if (_avatar.isDisposed()) return;
            final long channelId = _client.getChannelId(_scope);
            final byte avatar[] = _client.getChannelAvatar(channelId);
            final String name = _client.getChannelName(channelId);
            final String desc = _client.getChannelDescription(channelId);
            final List refs = _client.getChannelReferences(channelId);
            Display.getDefault().asyncExec(new Runnable() {
                public void run() { syncUpdate(channelId, avatar, name, desc, _scope, refs); }
            });
        }
        public void syncUpdate(long channelId, byte avatar[], String name, String desc, Hash scope, List refs) {
            //_scope = scope;
            if (_avatar.isDisposed()) return;
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
            _ui.debugMessage("North edge updated w/ new summary=" + buf.toString() + " and refs=" + refs);
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
        public void translate(TranslationRegistry registry) {
            //if (_isNew)
            //    _title.setText(registry.getText(T_TITLE, "New profile"));
        }
    }
    private static final String T_REFS = "syndie.gui.desktop.profilepanel.refs";

    
    private class EastEdge extends DesktopEdge implements Themeable, Translatable {
        private Button _messages;
        public EastEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        
        private void initComponents() {
            _messages = new Button(getEdgeRoot(), SWT.PUSH);
            _messages.addSelectionListener(new FireSelectionListener() {
                public void fire() { 
                    _desktop.getNavControl().view(SyndieURI.createScope(getOriginalURI().getScope()));
                }
            });
            _messages.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _messages, _themeRegistry.getTheme().SHELL_FONT, _translationRegistry.getText(T_MESSAGES, "Messages"));
                }
            });

        }
        
        public void applyTheme(Theme theme) {
            _messages.redraw();
        }
        public void translate(TranslationRegistry registry) {
            _messages.redraw();
        }
    }
    private static final String T_MESSAGES = "syndie.gui.desktop.profilepanel.messages";
    
    private class SouthEdge extends DesktopEdge implements Themeable, Translatable, ManageForum.StateListener {
        private Button _watch;
        private Button _ban;
        private Button _saveChanges;
        private Button _cancelChanges;
        public SouthEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        
        private void initComponents() {
            Composite edge = getEdgeRoot();
            edge.setLayout(new FillLayout(SWT.HORIZONTAL));
            
            _saveChanges = new Button(edge, SWT.PUSH);
            _saveChanges.addSelectionListener(new FireSelectionListener() {
                public void fire() { 
                    _manage.saveChanges(); 
                }
            });
            _cancelChanges = new Button(edge, SWT.PUSH);
            _cancelChanges.addSelectionListener(new FireSelectionListener() {
                public void fire() { _manage.cancelChanges(); }
            });
            _saveChanges.setEnabled(false);
            _cancelChanges.setEnabled(false);
            _watch = new Button(edge, SWT.PUSH);
            _watch.addSelectionListener(new FireSelectionListener() {
                public void fire() { 
                    if (_client.isWatched(_scopeId))
                        _client.unwatchChannel(_scope);
                    else
                        _client.watchChannel(_scopeId, true, false, false, false, false);
                }
            });
            _ban = new Button(edge, SWT.PUSH);
            _ban.addSelectionListener(new FireSelectionListener() {
                public void fire() { 
                    if (_scope != null) {
                        if (_client.getBannedChannels().contains(_scope))
                            _client.unban(_scope);
                        else
                            _client.ban(_scope, _ui, true);
                    }
                }
            });
        }
        
        public void settingsModified(boolean saveable) {
            if (!saveable)
                ((NorthEdge)_edgeNorth).update();
            _saveChanges.setEnabled(saveable);
            _cancelChanges.setEnabled(saveable);
        }
        
        public void update() {
            _saveChanges.setEnabled(false);
            _cancelChanges.setEnabled(false);
            if (_scopeId >= 0) {
                if (_manage != null) _manage.addListener(SouthEdge.this);
                _watch.setEnabled(true);
                translateWatch();
                translateBan();
            } else {
                _watch.setEnabled(false);
                _ban.setEnabled(false);
            }
        }
        
        public void applyTheme(Theme theme) {
            _watch.setFont(theme.BUTTON_FONT);
            _saveChanges.setFont(theme.BUTTON_FONT);
            _cancelChanges.setFont(theme.BUTTON_FONT);
            _ban.setFont(theme.BUTTON_FONT);
        }
        public void translate(TranslationRegistry registry) {
            translateWatch();
            translateBan();
            _saveChanges.setText(registry.getText(T_SAVE, "Save changes"));
            _cancelChanges.setText(registry.getText(T_CANCEL, "Cancel changes"));
        }
        private void translateWatch() {
            if (_client.isWatched(_scopeId))
                _watch.setText(_translationRegistry.getText(T_UNWATCH, "Unwatch the forum"));
            else
                _watch.setText(_translationRegistry.getText(T_WATCH, "Watch the forum"));
        }
        private void translateBan() {
            if ((_scope != null) && (_client.getBannedChannels().contains(_scope)))
                _ban.setText(_translationRegistry.getText(T_UNBAN, "Unban the forum"));
            else
                _ban.setText(_translationRegistry.getText(T_BAN, "Ban the forum"));
        }

    }
    
    private static final String T_WATCH = "syndie.gui.desktop.profilepanel.watch";
    private static final String T_UNWATCH = "syndie.gui.desktop.profilepanel.unwatch";
    private static final String T_BAN = "syndie.gui.desktop.profilepanel.ban";
    private static final String T_UNBAN = "syndie.gui.desktop.profilepanel.unban";
    private static final String T_SAVE = "syndie.gui.desktop.profilepanel.save";
    private static final String T_CANCEL = "syndie.gui.desktop.profilepanel.cancel";
}
