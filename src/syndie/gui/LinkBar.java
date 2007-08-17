package syndie.gui;

import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Transform;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import syndie.data.SyndieURI;
import syndie.data.Timer;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.UI;

public class LinkBar extends BaseComponent implements Translatable, Themeable {
    private NavigationControl _nav;
    private BookmarkControl _bookmarkControl;
    private URIControl _uriControl;
    private Composite _parent;
    private Composite _root;
    private Button _watchedButton;
    private Composite _screens;
    private Button _referencesButton;

    private Shell _watchedShell;
    private ChannelSelectorPanel _watchedPanel;
    
    private Shell _referencesShell;
    private ReferencesPanel _referencesPanel;

    private Shell _myForumsShell;
    private ChannelSelectorPanel _myForumsPanel;
    
    private Shell _myNymsShell;
    private ChannelSelectorPanel _myNymsPanel;
    
    public LinkBar(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl nav, BookmarkControl bookmark, URIControl uriControl, Composite parent) {
        super(client, ui, themes, trans);
        _nav = nav;
        _bookmarkControl = bookmark;
        _uriControl = uriControl;
        _parent = parent;
        initComponents();
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        _root.setLayout(gl);
        
        _watchedButton = new Button(_root, SWT.PUSH);
        _watchedButton.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _watchedButton.addSelectionListener(new FireSelectionListener() {
            public void fire() { toggleWatchedShell(); }
        });
        _watchedButton.setBackground(ColorUtil.getColor("green"));
        _watchedButton.setForeground(ColorUtil.getColor("green"));
        
        _screens = new Composite(_root, SWT.NONE);
        _screens.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _screens.setLayout(new RowLayout(SWT.VERTICAL));
        
        _referencesButton = new Button(_root, SWT.PUSH);
        _referencesButton.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _referencesButton.addSelectionListener(new FireSelectionListener() {
            public void fire() { toggleReferencesShell(); }
        });
        _referencesButton.setBackground(ColorUtil.getColor("blue"));
        _referencesButton.setForeground(ColorUtil.getColor("blue"));
        
        /*
        final Transform ccw = new Transform(_root.getDisplay());
        ccw.rotate(-90); // render text bottom to top
        
        _referencesButton.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent evt) {
                _ui.debugMessage("** refButton paint");
                evt.gc.setTransform(ccw);
                evt.gc.setBackground(ColorUtil.getColor("blue"));
                evt.gc.setForeground(ColorUtil.getColor("black"));
                evt.gc.setFont(_themeRegistry.getTheme().BUTTON_FONT);
                Rectangle bounds = _referencesButton.getBounds();
                evt.gc.drawString(_translationRegistry.getText(T_REFERENCES_BUTTON, "References"), 0, bounds.height);
            }
        });
        _watchedButton.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent evt) {
                evt.gc.setTransform(ccw);
                evt.gc.setBackground(ColorUtil.getColor("green"));
                evt.gc.setForeground(ColorUtil.getColor("black"));
                evt.gc.setFont(_themeRegistry.getTheme().BUTTON_FONT);
                String str = _translationRegistry.getText(T_WATCHED_BUTTON, "Watched");
                Point extent = evt.gc.stringExtent(str);
                Rectangle bounds = _watchedButton.getBounds();
                int bottom = bounds.height/2 + extent.x/2;
                int left = bounds.width/2 + extent.y/2;
                _ui.debugMessage("** watchedButton paint: bottom=" + bottom + " left=" + left + " bounds=" + bounds + " extent=" + extent);
                evt.gc.drawString(str, left, bottom);
            }
        });
         */
        
        
        initDnD();
        initKeyControls();
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
    }
    
    private void initDnD() {
        // we dont need to remember the drop targets here, since they're necessary for the life
        // of the associated button and SWT disposes the drop targets as dispose listeners on them
        BookmarkDnDHelper.initWatchTarget(_client, _ui, _watchedButton);
        BookmarkDnDHelper.initBookmarkDnDTarget(_ui, _referencesButton, new BookmarkDnDHelper.BookmarkDnDTarget() {
            public void dropped(SyndieURI uri, String name, String desc) { _bookmarkControl.bookmark(uri); }        
        });
    }
    
    private void toggleReferencesShell() { 
        if (_referencesShell != null) {
            _referencesShell.dispose();
            _referencesPanel.dispose();
            _referencesShell = null;
            _referencesPanel = null;
        } else {
            final Shell s = new Shell(_root.getShell(), SWT.NO_TRIM | SWT.PRIMARY_MODAL);
            s.setLayout(new FillLayout());
            ReferencesPanel refs = new ReferencesPanel(_client, _ui, _themeRegistry, _translationRegistry, _nav, _bookmarkControl, s, 
                    new Runnable() {
                        public void run() { toggleReferencesShell(); }
                    }
            );
            s.addShellListener(new ShellListener() {
                public void shellActivated(ShellEvent shellEvent) {}
                public void shellClosed(ShellEvent evt) {
                    _referencesPanel.dispose();
                    _referencesShell = null;
                    _referencesPanel = null;
                }
                public void shellDeactivated(ShellEvent evt) {
                    /*
                    _referencesShell.dispose();
                    _referencesPanel.dispose();
                    _referencesShell = null;
                    _referencesPanel = null;
                     */
                }
                public void shellDeiconified(ShellEvent shellEvent) {}
                public void shellIconified(ShellEvent shellEvent) {}
            });
            //s.pack();
            s.setSize(600, 400);
            
            recenter(s, false);
            _referencesPanel = refs;
            _referencesShell = s;
            s.open();
        }
        _ui.debugMessage("toggle references shell"); 
    }
   
    private void toggleWatchedShell() { 
        if (_watchedShell != null) {
            _watchedShell.dispose();
            _watchedPanel.dispose();
            _watchedShell = null;
            _watchedPanel = null;
        } else {
            final Shell s = new Shell(_root.getShell(), SWT.NO_TRIM | SWT.PRIMARY_MODAL);
            s.setLayout(new FillLayout());
            final ChannelSelectorPanel panel = new ChannelSelectorPanel(_client, _ui, _themeRegistry, _translationRegistry, s, new ChannelSelectorPanel.ChannelSelectorListener() {
                public void channelSelected(SyndieURI uri, int idx) { _nav.view(uri); toggleWatchedShell(); }
                public void channelReviewed(SyndieURI uri, long channelId, String name, String description, Image avatar) {}
                public void channelSelectorCancelled() { toggleWatchedShell(); }
                public void channelProfileSelected(SyndieURI uri, int matchedIndex) {
                    if ( (uri != null) && (uri.getScope() != null) ) {
                        _nav.view(URIHelper.instance().createManageURI(uri.getScope()));
                        toggleWatchedShell();
                    }
                }
            });
            
            s.addShellListener(new ShellListener() {
                public void shellActivated(ShellEvent shellEvent) {}
                public void shellClosed(ShellEvent evt) {
                    _watchedPanel.dispose();
                    _watchedShell = null;
                    _watchedPanel = null;
                }
                public void shellDeactivated(ShellEvent evt) {}
                public void shellDeiconified(ShellEvent shellEvent) {}
                public void shellIconified(ShellEvent shellEvent) {}
            });
            
            _watchedPanel = panel;
            _watchedShell = s;
                
            final Timer timer = new Timer("watched", _ui);
            panel.showWatched(false, new Runnable() {
                public void run() {
                    timer.addEvent("after set, before open");
                    recenter(s, false);
                    s.open(); 
                    timer.addEvent("async after open");
                    timer.complete();
                }
            });
        }
        _ui.debugMessage("toggle watched");
    }
    
    private void toggleMyForumsShell() { 
        if (_myForumsShell != null) {
            _myForumsShell.dispose();
            _myForumsPanel.dispose();
            _myForumsShell = null;
            _myForumsPanel = null;
        } else {
            final Shell s = new Shell(_root.getShell(), SWT.NO_TRIM | SWT.PRIMARY_MODAL);
            s.setLayout(new FillLayout());
            final ChannelSelectorPanel panel = new ChannelSelectorPanel(_client, _ui, _themeRegistry, _translationRegistry, s, new ChannelSelectorPanel.ChannelSelectorListener() {
                public void channelSelected(SyndieURI uri, int idx) { _nav.view(uri); toggleMyForumsShell(); }
                public void channelReviewed(SyndieURI uri, long channelId, String name, String description, Image avatar) {}
                public void channelSelectorCancelled() { toggleMyForumsShell(); }
                public void channelProfileSelected(SyndieURI uri, int matchedIndex) {
                    if ( (uri != null) && (uri.getScope() != null) ) {
                        _nav.view(URIHelper.instance().createManageURI(uri.getScope()));
                        toggleWatchedShell();
                    }
                }
            });
            
            s.addShellListener(new ShellListener() {
                public void shellActivated(ShellEvent shellEvent) {}
                public void shellClosed(ShellEvent evt) {
                    _myForumsPanel.dispose();
                    _myForumsShell = null;
                    _myForumsPanel = null;
                }
                public void shellDeactivated(ShellEvent evt) {}
                public void shellDeiconified(ShellEvent shellEvent) {}
                public void shellIconified(ShellEvent shellEvent) {}
            });
            
            _myForumsPanel = panel;
            _myForumsShell = s;
                
            final Timer timer = new Timer("my forums", _ui);
            panel.showPostable(new Runnable() {
                public void run() {
                    timer.addEvent("after set, before open");
                     recenter(s, false);
                    s.open(); 
                    timer.addEvent("async after open");
                    timer.complete();
                }
            });
        }
        _ui.debugMessage("toggle my forums shell"); 
    }
    
    private void toggleMyNymsShell() { 
        if (_myNymsShell != null) {
            _myNymsShell.dispose();
            _myNymsPanel.dispose();
            _myNymsShell = null;
            _myNymsPanel = null;
        } else {
            final Shell s = new Shell(_root.getShell(), SWT.NO_TRIM | SWT.PRIMARY_MODAL);
            s.setLayout(new FillLayout());
            final ChannelSelectorPanel panel = new ChannelSelectorPanel(_client, _ui, _themeRegistry, _translationRegistry, s, new ChannelSelectorPanel.ChannelSelectorListener() {
                public void channelSelected(SyndieURI uri, int idx) { _nav.view(uri); toggleMyNymsShell(); }
                public void channelReviewed(SyndieURI uri, long channelId, String name, String description, Image avatar) {}
                public void channelSelectorCancelled() { toggleMyNymsShell(); }
                public void channelProfileSelected(SyndieURI uri, int matchedIndex) {
                    if ( (uri != null) && (uri.getScope() != null) ) {
                        _nav.view(URIHelper.instance().createManageURI(uri.getScope()));
                        toggleWatchedShell();
                    }
                }
            });
            
            s.addShellListener(new ShellListener() {
                public void shellActivated(ShellEvent shellEvent) {}
                public void shellClosed(ShellEvent evt) {
                    _myNymsPanel.dispose();
                    _myNymsShell = null;
                    _myNymsPanel = null;
                }
                public void shellDeactivated(ShellEvent evt) {}
                public void shellDeiconified(ShellEvent shellEvent) {}
                public void shellIconified(ShellEvent shellEvent) {}
            });
            
            _myNymsPanel = panel;
            _myNymsShell = s;
                
            final Timer timer = new Timer("my nyms", _ui);
            panel.showIdent(new Runnable() {
                public void run() {
                    timer.addEvent("after set, before open");
                    recenter(s, false);
                    s.open(); 
                    timer.addEvent("async after open");
                    timer.complete();
                }
            });
        }
        _ui.debugMessage("toggle my nyms shell"); 
    }
    
    private void recenter(Shell s, boolean pack) {
        if (pack)
            s.pack(true);
        //Rectangle size = s.getBounds();
        Composite parent = s.getParent();
        Rectangle size = null;
        int edgeX = 0;
        int edgeY = 0;
        if (true || (parent == null)) {
            size = Splash.getScreenSize(s);
            edgeX = 0;//size.width/10;
            edgeY = 0;//size.height/10;
        } else {
            size = parent.getClientArea();
            edgeX = size.width/10;
            edgeY = size.height/10;
        }
        s.setBounds(edgeX, edgeY, size.width-edgeX*2, size.height-edgeY*2);
    }
    
    private void initKeyControls() {
        Display d = _root.getDisplay();
        d.addFilter(SWT.KeyDown, new Listener() {
            public void handleEvent(Event evt) {
                if ( (evt.character == 'w') && ((evt.stateMask & SWT.MOD3) != 0) ) { // ALT+w
                    toggleWatchedShell();
                    //new ForumSelector(_client, _ui, _themeRegistry, _translationRegistry, _nav, _bookmarkControl, _uriControl, _root.getShell(), ForumSelector.METHOD_WATCHED);
                } else if ( (evt.character == 'r') && ((evt.stateMask & SWT.MOD3) != 0) ) { // ALT+r
                    toggleReferencesShell();
                    //new ForumSelector(_client, _ui, _themeRegistry, _translationRegistry, _nav, _bookmarkControl, _uriControl, _root.getShell(), ForumSelector.METHOD_REFERENCES);
                } else if ( (evt.character == 'm') && ((evt.stateMask & SWT.MOD3) != 0) ) { // ALT+m
                    toggleMyForumsShell();
                    //new ForumSelector(_client, _ui, _themeRegistry, _translationRegistry, _nav, _bookmarkControl, _uriControl, _root.getShell(), ForumSelector.METHOD_MYFORUMS);
                } else if ( (evt.character == 'n') && ((evt.stateMask & SWT.MOD3) != 0) ) { // ALT+n
                    toggleMyNymsShell();
                    //new ForumSelector(_client, _ui, _themeRegistry, _translationRegistry, _nav, _bookmarkControl, _uriControl, _root.getShell(), ForumSelector.METHOD_MYNYMS);
                }
            }
        });
    }

    private static final String T_WATCHED_BUTTON = "syndie.gui.linkbar.watchedbutton";
    private static final String T_REFERENCES_BUTTON = "syndie.gui.linkbar.referencesbutton";
    private static final String T_WATCHED_TOOLTIP = "syndie.gui.linkbar.watchedtooltip";
    private static final String T_REFERENCES_TOOLTIP = "syndie.gui.linkbar.referencestooltip";
    
    public void translate(TranslationRegistry registry) {
        _watchedButton.redraw();
        _referencesButton.redraw();
        _watchedButton.setToolTipText(registry.getText(T_WATCHED_TOOLTIP, "Watched forums"));
        _referencesButton.setToolTipText(registry.getText(T_REFERENCES_TOOLTIP, "Referenced resources"));
    }
    public void applyTheme(Theme theme) {
        _watchedButton.redraw();
        _referencesButton.redraw();
    }
}
