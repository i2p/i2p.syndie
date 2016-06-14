package syndie.gui.desktop;

import java.util.ArrayList;
import java.util.Set;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;
import syndie.gui.ColorUtil;
import syndie.gui.FireSelectionListener;
import syndie.gui.ImageUtil;
import syndie.gui.Syndicator;
import syndie.gui.Theme;
import syndie.gui.ThemeRegistry;
import syndie.gui.Themeable;
import syndie.gui.Translatable;
import syndie.gui.TranslationRegistry;
import syndie.gui.URIHelper;

/**
 *
 */
class SyndicatorPanel extends DesktopPanel implements Themeable, Translatable {
    private Syndicator _syndicator;
    
    public SyndicatorPanel(Desktop desktop, DBClient client, ThemeRegistry themes, TranslationRegistry trans, Composite parent, UI ui, SyndieURI origURI) {
        super(desktop, client, themes, trans, parent, ui, origURI);
        initComponents();
    }
    
    public String getPanelName() { return "Peers"; }
    public String getPanelDescription() { return "Control the peer syndication"; }
    
    private void initComponents() {
        Composite root = getRoot();
        root.setLayout(new FillLayout());
        _syndicator = new Syndicator(_client, _ui, _themeRegistry, _translationRegistry, _desktop.getNavControl(),
                                     null, root, false);
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    public boolean canShow(SyndieURI uri) {
        return super.canShow(uri) || getOriginalURI().getType().equals(uri.getType()) || uri.isArchive();
    }
    
    void shown(Desktop desktop, SyndieURI uri, String suggestedName, String suggestedDescription) {
        super.shown(desktop, uri, suggestedName, suggestedDescription);
        if (uri != null)
            _syndicator.show(uri);
    }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        _syndicator.dispose();
        super.dispose();
    }
    
    public void forceFocus() { _syndicator.forceFocus(); }
    
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
    
    private static class NorthEdge extends DesktopEdge implements Themeable, Translatable {
        private Label _title;
        public NorthEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        
        private void initComponents() {
            Composite edge = getEdgeRoot();
            edge.setLayout(new GridLayout(1, false));
            _title = new Label(edge, SWT.NONE);
            _title.setLayoutData(new GridData(GridData.CENTER, GridData.CENTER, true, true));
            Color white = ColorUtil.getColor("white");
            Color black = ColorUtil.getColor("black");
            _title.setBackground(white);
            _title.setForeground(black);
            edge.setBackground(white);
            edge.setForeground(black);
        }
        
        public void applyTheme(Theme theme) {
            _title.setFont(theme.SHELL_FONT);
            getEdgeRoot().layout(true, true);
        }
        public void translate(TranslationRegistry registry) {
            _title.setText(registry.getText("Peer syndication"));
        }
    }
    
    private class EastEdge extends DesktopEdge implements Themeable, Translatable {
        private Button _details;
        private Button _viewFetched;
        public EastEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        
        private void initComponents() {
            getEdgeRoot().setLayout(new FillLayout(SWT.VERTICAL));

            _details = new Button(getEdgeRoot(), SWT.PUSH);
            _details.addSelectionListener(new FireSelectionListener() {
                public void fire() { _syndicator.viewDetail(); }
            });
            _details.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _details, _themeRegistry.getTheme().SHELL_FONT, _translationRegistry.getText("Archive details"));
                }
            });
            _viewFetched = new Button(getEdgeRoot(), SWT.PUSH);
            _viewFetched.addSelectionListener(new FireSelectionListener() {
                public void fire() { 
                    Set scopes = _syndicator.getFetchedScopes();
                    if (scopes.size() > 0)
                        _desktop.getNavControl().view(SyndieURI.createSearch(new ArrayList(scopes), true, true, true));
                }
            });
            _viewFetched.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _viewFetched, _themeRegistry.getTheme().SHELL_FONT, _translationRegistry.getText("View fetched messages"));
                }
            });

        }
        
        public void applyTheme(Theme theme) {
            _details.redraw();
        }
        public void translate(TranslationRegistry registry) {
            _details.redraw();
        }
    }
    
    private class SouthEdge extends DesktopEdge implements Themeable, Translatable {
        private Button _addArchive;
        private Button _syncOneOff;
        private Button _syncRecurring;
        private Button _deleteArchive;
        public SouthEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        
        private void initComponents() {
            Composite edge = getEdgeRoot();
            edge.setLayout(new FillLayout(SWT.HORIZONTAL));
            
            _addArchive = new Button(edge, SWT.PUSH);
            _addArchive.addSelectionListener(new FireSelectionListener() {
                public void fire() { _syndicator.add(); }
            });
            _syncOneOff = new Button(edge, SWT.PUSH);
            _syncOneOff.addSelectionListener(new FireSelectionListener() {
                public void fire() { _syndicator.syncNowOneTime(); }
            });
            _syncRecurring = new Button(edge, SWT.PUSH);
            _syncRecurring.addSelectionListener(new FireSelectionListener() {
                public void fire() { _syndicator.syncNowRecurring(); }
            });
            _deleteArchive = new Button(edge, SWT.PUSH);
            _deleteArchive.addSelectionListener(new FireSelectionListener() {
                public void fire() { _syndicator.delete(); }
            });
        }
        
        public void applyTheme(Theme theme) {
            _addArchive.setFont(theme.BUTTON_FONT);
            _syncOneOff.setFont(theme.BUTTON_FONT);
            _syncRecurring.setFont(theme.BUTTON_FONT);
            _deleteArchive.setFont(theme.BUTTON_FONT);
        }
        public void translate(TranslationRegistry registry) {
            _addArchive.setText(registry.getText("Add archive"));
            _syncOneOff.setText(registry.getText("Sync (just once)"));
            _syncRecurring.setText(registry.getText("Sync (recurring)"));
            _deleteArchive.setText(registry.getText("Delete archive"));
        }
    }
}
