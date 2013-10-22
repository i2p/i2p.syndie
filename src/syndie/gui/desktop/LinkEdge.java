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
import syndie.data.NymReferenceNode;
import syndie.data.SyndieURI;
import syndie.util.Timer;
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
 * left edge serving as a drag and drop target to bookmark/watch forums/resources as
 * well as to access the forum selection panel
 */
class LinkEdge extends DesktopEdge implements Themeable, Translatable {
    private Desktop _desktop;
    private Button _specialChannels;
    private Button _bookmarks;
    
    public LinkEdge(Composite parent, UI ui, Desktop desktop) {
        super(parent, ui);
        _desktop = desktop;
        initComponents();
    }

    private void initComponents() {
        Composite root = getEdgeRoot();
        root.setLayout(new FillLayout(SWT.VERTICAL));
        _specialChannels = new Button(root, SWT.PUSH);
        _specialChannels.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent evt) {
                if ( (_desktop.getThemeRegistry() == null) || (_desktop.getTranslationRegistry() == null) )
                    return;
                ImageUtil.drawAscending(evt.gc, _specialChannels, _desktop.getThemeRegistry().getTheme().SHELL_FONT, _desktop.getTranslationRegistry().getText("Special forums"));
            }
        });
            
        _specialChannels.addSelectionListener(new FireSelectionListener() { public void fire() { _desktop.toggleForumSelectionPanel(false, true); } }); 
        BookmarkDnDHelper.initBookmarkDnDTarget(_ui, _specialChannels, new BookmarkDnDHelper.WatchTarget() { 
            public void dropped(SyndieURI uri, String name, String desc) {
                _desktop.getDBClient().watchChannel(uri.getScope(), true, true, false, false, false);
                _desktop.nymChannelsUpdated();
            }
        });

        _bookmarks = new Button(root, SWT.PUSH);
        _bookmarks.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent evt) {
                if ( (_desktop.getThemeRegistry() == null) || (_desktop.getTranslationRegistry() == null) )
                    return;
                ImageUtil.drawAscending(evt.gc, _specialChannels, _desktop.getThemeRegistry().getTheme().SHELL_FONT, _desktop.getTranslationRegistry().getText("Bookmarks"));
            }
        });
            
        _bookmarks.addSelectionListener(new FireSelectionListener() { public void fire() { _desktop.toggleForumSelectionPanel(true, true); } }); 
        BookmarkDnDHelper.initBookmarkDnDTarget(_ui, _bookmarks, new BookmarkDnDHelper.WatchTarget() { 
            public void dropped(SyndieURI uri, String name, String desc) {
                if (uri == null) 
                    return;
                if (name == null) name = uri.toString();
                if (desc == null) desc = "";
                NymReferenceNode node = new NymReferenceNode(name, uri, desc, -1, -1, -1, 0, false, false, false);
                _ui.debugMessage("bookmarking the node: " + node);
                _desktop.getBookmarkControl().bookmark(node, true);
                _desktop.bookmarksUpdated();
            }
        });
    }
    
    public void startupComplete() {
        _desktop.getThemeRegistry().register(this);
        _desktop.getTranslationRegistry().register(this);
    }
    
    public void applyTheme(Theme theme) {
        _specialChannels.redraw();
        _bookmarks.redraw();
    }
    public void translate(TranslationRegistry registry) {
        _specialChannels.redraw();
        _bookmarks.redraw();
    }
    
}
