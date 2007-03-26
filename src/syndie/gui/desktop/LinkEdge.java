package syndie.gui.desktop;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import syndie.data.SyndieURI;
import syndie.data.Timer;
import syndie.data.WatchedChannel;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.UI;
import syndie.gui.ColorUtil;
import syndie.gui.FireSelectionListener;
import syndie.gui.ImageGrid;
import syndie.gui.ImageUtil;
import syndie.gui.BookmarkDnDHelper;
import syndie.gui.Theme;
import syndie.gui.Themeable;
import syndie.gui.Translatable;
import syndie.gui.TranslationRegistry;

/**
 * desktop edge containing the watched forums, bookmarked references, and the
 * active panel list
 */
public class LinkEdge extends DesktopEdge implements Themeable, Translatable {
    private Desktop _desktop;
    private Button _favorites;
    
    public LinkEdge(Composite parent, UI ui, Desktop desktop) {
        super(parent, ui);
        _desktop = desktop;
        initComponents();
    }
    
    private void initComponents() {
        Composite root = getEdgeRoot();
        root.setLayout(new FillLayout(SWT.VERTICAL));
        _favorites = new Button(root, SWT.PUSH);
        _favorites.setBackground(ColorUtil.getColor("yellow"));
        _favorites.addSelectionListener(new FireSelectionListener() { public void fire() { _desktop.showForumSelectionPanel(); } }); 
        BookmarkDnDHelper.initBookmarkDnDTarget(_ui, _favorites, new BookmarkDnDHelper.WatchTarget() { 
            public void dropped(SyndieURI uri, String name, String desc) {
                _desktop.getDBClient().watchChannel(uri.getScope(), true, true, false, false, false);
            }
        });
        
        /*
        _refs = new Button(root, SWT.PUSH);
        _refs.setText(" ");
        _refs.setBackground(ColorUtil.getColor("yellow"));
        _refs.setForeground(ColorUtil.getColor("yellow"));
        _refs.addSelectionListener(new FireSelectionListener() { public void fire() { _desktop.showForumSelectionPanel(true); } }); 
        BookmarkDnDHelper.initBookmarkDnDTarget(_ui, _refs, new BookmarkDnDHelper.WatchTarget() { 
            public void dropped(SyndieURI uri, String name, String desc) {
                _desktop.getBookmarkControl().bookmark(uri);
            }
        });
         */
    }
    
    public void startupComplete() {
        _desktop.getThemeRegistry().register(this);
        _desktop.getTranslationRegistry().register(this);
    }
    
    public void applyTheme(Theme theme) { _favorites.setFont(theme.SHELL_FONT); }
    public void translate(TranslationRegistry registry) {
        _favorites.setText(registry.getText(T_FAVORITES, "F\na\nv\no\nr\ni\nt\ne\ns"));
    }
    private static final String T_FAVORITES = "syndie.gui.desktop.linkedge.favorites";
}
