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
import syndie.gui.LinkBar;

/**
 * desktop edge containing the watched forums, bookmarked references, and the
 * active panel list
 */
public class LinkEdge extends DesktopEdge {
    private Desktop _desktop;
    private Button _button;
    
    public LinkEdge(Composite parent, UI ui, Desktop desktop) {
        super(parent, ui);
        _desktop = desktop;
        initComponents();
    }
    
    private void initComponents() {
        _button = new Button(getRoot(), SWT.PUSH);
        _button.setText(" ");
        _button.setBackground(ColorUtil.getColor("blue"));
        _button.setForeground(ColorUtil.getColor("blue"));
        _button.addSelectionListener(new FireSelectionListener() { public void fire() { _desktop.showForumSelectionPanel(); } }); 
        BookmarkDnDHelper.initWatchTarget(_desktop.getDBClient(), _ui, _button);
    }
}
