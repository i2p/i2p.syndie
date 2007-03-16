package syndie.gui.desktop;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
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
import syndie.gui.FireSelectionListener;
import syndie.gui.ImageGrid;
import syndie.gui.ImageUtil;
import syndie.gui.BookmarkDnDHelper;

/**
 * desktop edge containing the watched forums, bookmarked references, and the
 * active panel list
 */
public class LinkEdge extends DesktopEdge {
    private Desktop _desktop;
    private Button _watchedButton;
    private Composite _watchedAvatars;
    private Composite _panels;
    private Composite _bookmarkIcons;
    private Button _bookmarkButton;
    
    private ImageGrid _watchedAvatarGrid;
    private ImageGrid _bookmarkIconGrid;
    
    private Map _panelButtons;

    /** each new panel gets a different color, rotating through this list */
    private int _panelCounter;
    private static final int PANEL_COLORS[] = new int[] { SWT.COLOR_RED, SWT.COLOR_GREEN, SWT.COLOR_BLUE, SWT.COLOR_CYAN, SWT.COLOR_MAGENTA, SWT.COLOR_DARK_YELLOW, SWT.COLOR_DARK_RED, SWT.COLOR_DARK_GREEN, SWT.COLOR_DARK_BLUE, SWT.COLOR_DARK_CYAN, SWT.COLOR_DARK_MAGENTA };
    
    public LinkEdge(Composite parent, UI ui, Desktop desktop) {
        super(parent, ui);
        _desktop = desktop;
        _panelButtons = new HashMap(8);
        _panelCounter = 0;
        initComponents();
    }
    void startupComplete() {
        _desktop.getDBClient().addWatchEventListener(new DBClient.WatchEventListener() {
            public void watchesUpdated() { 
                Timer timer = new Timer("on watch event, link edge rendering", _desktop.getUI());
                refreshWatched(timer); 
                timer.addEvent("completed");
                timer.complete();
            }
        });
        final Timer timer = new Timer("link edge rendering", _desktop.getUI(), true, 10);
        JobRunner.instance().enqueue(new Runnable() { 
            public void run() {
                timer.addEvent("begin refresh watched");
                refreshWatched(timer); 
                timer.addEvent("begin refresh bookmarked");
                refreshBookmarked();
                timer.addEvent("begin init dnd");
                initDnD();
                timer.addEvent("completed link edge startup rendering");
                timer.complete();
            } 
        });
    }
    private void refreshWatched(Timer timer) {
        Display display = getRoot().getDisplay();
        DBClient client = _desktop.getDBClient();
        List chans = client.getWatchedChannels();
        timer.addEvent("watched fetched");
        final List drawnIds = new ArrayList();
        display.syncExec(new Runnable() { 
            public void run() { 
                _watchedAvatars.setRedraw(false);
                drawnIds.addAll(_watchedAvatarGrid.getIds());
            } 
        });
        timer.addEvent("already drawn fetched");
        final List toUndrawIds = new ArrayList(drawnIds);
        List chanIds = new ArrayList();
        Map chanIdToUnreadCount = new HashMap();
        boolean withUnreadOnly = showUnreadWatchedOnly();
        for (int i = 0; i < chans.size(); i++) {
            WatchedChannel c = (WatchedChannel)chans.get(i);
            if (withUnreadOnly) {
                int unread = client.countUnreadMessages(c.getChannelId());
                if (unread == 0)
                    continue;
                Long key = new Long(c.getChannelId());
                chanIds.add(key);
                chanIdToUnreadCount.put(key, new Integer(unread));
            } else {
                chanIds.add(new Long(c.getChannelId()));
            }
        }
        timer.addEvent("unread counted");
        toUndrawIds.removeAll(chanIds);
        final List toAddIds = new ArrayList(chanIds);
        toAddIds.removeAll(drawnIds);
        
        final List toAddImgs = new ArrayList(toAddIds.size());
        final List toAddTooltips = new ArrayList(toAddIds.size());
        final List toAddJobs = new ArrayList(toAddIds.size());
        final List toAddURIs = new ArrayList(toAddIds.size());
        
        timer.addEvent("prepare add for " + toAddIds.size());
        for (int i = 0; i < toAddIds.size(); i++) {
            final Long id = (Long)toAddIds.get(i);
            Hash chan = client.getChannelHash(id.longValue());
            toAddURIs.add(SyndieURI.createScope(chan));
            String tooltip = client.getChannelName(id.longValue());
            if (tooltip == null)
                tooltip = chan.toBase64().substring(0,6);
            Integer numUnread = (Integer)chanIdToUnreadCount.get(id);
            if (numUnread != null)
                tooltip = tooltip + ": " + numUnread;
            final byte avatar[] = client.getChannelAvatar(id.longValue());
            if (avatar != null) {
                display.syncExec(new Runnable() {
                    public void run() {
                        Image img = ImageUtil.createImage(avatar);
                        if (img != null) {
                            Rectangle rect = img.getBounds();
                            int sz = _watchedAvatarGrid.getImageSize();
                            if ( (rect.height > sz) || (rect.width > sz) )
                                img = ImageUtil.resize(img, sz, sz, true);
                            toAddImgs.add(img);
                        }
                    }
                });
                if (toAddImgs.size() == toAddTooltips.size()) {
                    // create failed
                    toAddImgs.add(getDefaultWatchedImage());
                }
            } else {
                toAddImgs.add(getDefaultWatchedImage());
            }
            Runnable job = new Runnable() { 
                public void run() {
                    _desktop.getUI().debugMessage("view channel " + id);
                }
            };
            toAddJobs.add(job);
            toAddTooltips.add(tooltip);
        }
        
        timer.addEvent("render the watched changes");
        // ok now we know what to do, so lets do it
        display.asyncExec(new Runnable() {
            public void run() {
                for (int i = 0; i < toUndrawIds.size(); i++)
                    ImageUtil.dispose(_watchedAvatarGrid.remove(toUndrawIds.get(i)));
                for (int i = 0; i < toAddIds.size(); i++) {
                    Control c = _watchedAvatarGrid.add(toAddIds.get(i), (Image)toAddImgs.get(i), (String)toAddTooltips.get(i), (Runnable)toAddJobs.get(i));
                    //LinkEdgeDnD.initSource(_desktop, _desktop.getUI(), c, (SyndieURI)toAddURIs.get(i));
                }
                _watchedAvatars.setRedraw(true);
                _watchedAvatarGrid.layout(true);
            }
        });
    }
    
    private Image getDefaultWatchedImage() { return ImageUtil.ICON_MSG_FLAG_BOOKMARKED_AUTHOR; }
    private boolean showUnreadWatchedOnly() {
        // if true, it has to do some more crunching, though it certainly could be optimized
        // away by adding the COUNT(unread) as a join on a custom query to fetch the to-add
        // channel's data.  or maybe higher up as a new parameter to the getWatchedChannels.
        //
        // well, for now, speed speed speed.
        return false;
    }
    
    private void refreshBookmarked() {
        ImageUtil.init(_desktop.getDBClient().getTempDir());
        getRoot().getDisplay().asyncExec(new Runnable() { public void run() { dummyPopulate(); } });
    }
    
    private void initDnD() {
        getRoot().getDisplay().asyncExec(new Runnable() {
            public void run() {
                /*
                BookmarkDnDHelper.initWatchTarget(_desktop, _desktop.getUI(), _watchedAvatars);
                BookmarkDnDHelper.initWatchTarget(_desktop, _desktop.getUI(), _watchedButton);      
                BookmarkDnDHelper.initSource(_desktop, _desktop.getUI(), _watchedAvatarGrid, SyndieURI.createURL("http://foo"));
                 */
            }
        });
    }
    
    private void dummyPopulate() {
        Image imgs[] = new Image[] {
            ImageUtil.ICON_ARCHIVE_TYPE_SYNDIE, ImageUtil.ICON_ARCHIVE_TYPE_URL,
            ImageUtil.ICON_MSG_FLAG_PUBLIC, ImageUtil.ICON_MSG_FLAG_AUTHENTICATED,
            ImageUtil.ICON_MSG_FLAG_AUTHORIZED, ImageUtil.ICON_MSG_FLAG_BANNED,
            ImageUtil.ICON_MSG_FLAG_BOOKMARKED_AUTHOR, ImageUtil.ICON_MSG_FLAG_HASARCHIVES,
            ImageUtil.ICON_MSG_FLAG_HASATTACHMENTS, ImageUtil.ICON_MSG_FLAG_HASKEYS,
            ImageUtil.ICON_MSG_FLAG_HASREFS, ImageUtil.ICON_MSG_FLAG_ISNEW,
            ImageUtil.ICON_MSG_FLAG_PBE, ImageUtil.ICON_MSG_FLAG_PUBLIC,
            ImageUtil.ICON_MSG_FLAG_READKEYUNKNOWN, ImageUtil.ICON_MSG_FLAG_REPLYKEYUNKNOWN,
            ImageUtil.ICON_MSG_FLAG_SCHEDULEDFOREXPIRE, ImageUtil.ICON_MSG_FLAG_UNREADABLE,
            ImageUtil.ICON_MSG_TYPE_META, ImageUtil.ICON_MSG_TYPE_NORMAL,
            ImageUtil.ICON_MSG_TYPE_PRIVATE
        };
        //for (int i = 0; i < imgs.length; i++) {
        //    final int imgNum = i;
        //    _watchedAvatarGrid.add(i+"", imgs[i], "image " + i, new Runnable() { public void run() { _desktop.getUI().debugMessage("watched selected: " + imgNum); } });
        //}
        for (int i = 0; i < 9; i++) {
            final int imgNum = i;
            _bookmarkIconGrid.add(i+"", imgs[i], "image " + i, new Runnable() { public void run() { _desktop.getUI().debugMessage("bookmark selected: " + imgNum); } });
        }
        //_watchedAvatarGrid.layout(true);
        _bookmarkIconGrid.layout(true);
    }
    
    private void initComponents() {
        Composite root = getRoot();
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        root.setLayout(gl);
        
        _watchedButton = new Button(root, SWT.PUSH);
        _watchedButton.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _watchedButton.setText("Watched");
        
        _watchedAvatars = new Composite(root, SWT.NONE);
        _watchedAvatars.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _watchedAvatars.setLayout(new FillLayout());
        _watchedAvatarGrid = new ImageGrid(_watchedAvatars, 32, false);
        _watchedAvatarGrid.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) {}
            public void mouseDown(MouseEvent mouseEvent) {}
            public void mouseUp(MouseEvent mouseEvent) {
                _desktop.getUI().debugMessage("watched avatar grid selected");
            }
        });
        
        _panels = new Composite(root, SWT.NONE);
        _panels.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        gl = new GridLayout(1, true);
        gl.horizontalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.verticalSpacing = 0;
        _panels.setLayout(gl);
        
        _bookmarkIcons = new Composite(root, SWT.NONE);
        _bookmarkIcons.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _bookmarkIcons.setLayout(new FillLayout());
        _bookmarkIconGrid = new ImageGrid(_bookmarkIcons, 32, true);
        _bookmarkIconGrid.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) {}
            public void mouseDown(MouseEvent mouseEvent) {}
            public void mouseUp(MouseEvent mouseEvent) {
                _desktop.getUI().debugMessage("bookmark icon grid selected");
            }
        });
        
        _bookmarkButton = new Button(root, SWT.PUSH);
        _bookmarkButton.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _bookmarkButton.setText("Bookmarks");
        
        _desktop.addListener(new Desktop.DesktopListener() {
            public void panelShown(DesktopPanel panel) { addPanel(panel); }
            public void destroyed(DesktopPanel panel) { removePanel(panel); }
        });
    }
    
    private void addPanel(final DesktopPanel panel) {
        if (_panelButtons.containsKey(panel)) return;
        String name = panel.getPanelName();
        String desc = panel.getPanelDescription();
        Button b = new Button(_panels, SWT.PUSH);
        b.addSelectionListener(new FireSelectionListener() {
            public void fire() { _desktop.show(panel); }
        });
        StringBuffer tooltip = new StringBuffer();
        if (name != null) {
            b.setText(name);
            tooltip.append(name);
            if (desc != null)
                tooltip.append(": ").append(desc);
        } else if (desc != null) {
            b.setText(desc);
            tooltip.append(desc);
        } else {
            b.setText("panel");
            tooltip.append(panel.getClass().getName());
        }
        b.setToolTipText(tooltip.toString());
        b.setBackground(_panels.getDisplay().getSystemColor(PANEL_COLORS[_panelCounter % PANEL_COLORS.length]));
        b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _panelButtons.put(panel, b);
        getRoot().layout(true, true);
        _panelCounter++;
    }
    
    private void removePanel(DesktopPanel panel) {
        Button b = (Button)_panelButtons.remove(panel);
        if (b != null)
            b.dispose();
    }
}
