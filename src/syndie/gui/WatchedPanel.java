package syndie.gui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import syndie.Constants;
import syndie.data.SyndieURI;
import syndie.data.WatchedChannel;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.UI;

/**
 * the overlay display panel showing the various watched forum avatars
 */
public class WatchedPanel extends BaseComponent implements Themeable, Translatable {
    private NavigationControl _navControl;
    private URIControl _uriControl;
    private Runnable _onClose;
    private Runnable _onRefresh;
    private Composite _parent;
    private Composite _root;
    private Composite _avatars;
    private Composite _bottomControl;
    private Button _unreadOnly;
    private Button _close;
    
    /** channel Id (Long) to Image for loaded avatars */
    private Map _avatarIdToImage;
    private Map _avatarIdToTooltipShort;
    private Map _avatarIdToTooltip;
    private Map _avatarIdToURI;
    private TreeMap _avatarTooltipShortToAvatarId;
    
    public WatchedPanel(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl nav, URIControl uriControl, Composite parent, Runnable onClose, Runnable onRefresh) {
        super(client, ui, themes, trans);
        _navControl = nav;
        _uriControl = uriControl;
        _parent = parent;
        _onClose = onClose;
        _onRefresh = onRefresh;
        _avatarIdToImage = new HashMap();
        _avatarIdToTooltipShort = new HashMap();
        _avatarIdToTooltip = new HashMap();
        _avatarIdToURI = new HashMap();
        _avatarTooltipShortToAvatarId = new TreeMap();
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
        
        _unreadOnly = new Button(_root, SWT.CHECK | SWT.BORDER);
        _unreadOnly.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _unreadOnly.addSelectionListener(new FireSelectionListener() { public void fire() { toggleUnread(); } });
        
        _avatars = new Composite(_root, SWT.NONE);
        _avatars.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
        _avatars.setBackground(ColorUtil.getColor("green"));
        gl = new GridLayout(4, true);
        _avatars.setLayout(gl);
        
        _close = new Button(_root, SWT.PUSH);
        _close.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _close.addSelectionListener(new FireSelectionListener() { public void fire() { close(); } });
        
        loadData();
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    private void loadData() {
        _avatars.setRedraw(false);
        Control avatars[] = _avatars.getChildren();
        for (int i = 0; i < avatars.length; i++) avatars[i].dispose();
        final boolean unreadOnly = _unreadOnly.getSelection();
        JobRunner.instance().enqueue(new Runnable() { public void run() { asyncLoadData(unreadOnly); } });
    }
    private void asyncLoadData(final boolean unreadOnly) {
        List chans = _client.getWatchedChannels(); 
        List loadedIds = new ArrayList(_avatarIdToImage.keySet());
        final List toUndrawIds = new ArrayList(loadedIds);
        List chanIds = new ArrayList();
        Map chanIdToUnreadCount = new HashMap();
        for (int i = 0; i < chans.size(); i++) {
            WatchedChannel c = (WatchedChannel)chans.get(i);
            Long key = new Long(c.getChannelId());
            if (unreadOnly) {
                int unread = _client.countUnreadMessages(c.getChannelId());
                if (unread == 0)
                    continue;
                chanIdToUnreadCount.put(key, new Integer(unread));
            }
            chanIds.add(key);
        }
        toUndrawIds.removeAll(chanIds);
        final List toAddIds = new ArrayList(chanIds);
        toAddIds.removeAll(loadedIds);
        
        _ui.debugMessage("to undraw: " + toUndrawIds.size() + " toAdd: " + toAddIds.size() + " existing: " + loadedIds.size());
        
        final List toAddAvatarData = new ArrayList(toAddIds.size());
        final List toAddImgs = new ArrayList(toAddIds.size());
        
        for (int i = 0; i < toAddIds.size(); i++) {
            final Long id = (Long)toAddIds.get(i);
            Hash chan = _client.getChannelHash(id.longValue());
            StringBuffer tooltipBuf = new StringBuffer();
            String name = _client.getChannelName(id.longValue());
            if (name != null)
                tooltipBuf.append(name).append(" ");
            tooltipBuf.append("(").append(chan.toBase64().substring(0,6)).append(")");
            String tooltipShort = tooltipBuf.toString();
            Integer numUnread = (Integer)chanIdToUnreadCount.get(id);
            if (numUnread != null)
                tooltipBuf.append(": ").append(numUnread);
            
            final byte avatar[] = _client.getChannelAvatar(id.longValue());
            if (avatar != null)
                toAddAvatarData.add(avatar);
            else
                toAddAvatarData.add(new byte[0]);
            
            // guaranteed unique, because it ends in "$scope: $number"
            String tooltip = tooltipBuf.toString();
            _avatarIdToTooltipShort.put(id, tooltipShort);
            _avatarIdToTooltip.put(id, tooltip);
            _avatarTooltipShortToAvatarId.put(tooltipShort, id);
            _avatarIdToURI.put(id, SyndieURI.createScope(chan));
        }
        
        final List toDisposeImages = new ArrayList(toUndrawIds.size());
        for (int i = 0; i < toUndrawIds.size(); i++) {
            Long id = (Long)toUndrawIds.get(i);
            Image img = (Image)_avatarIdToImage.remove(id);
            String tooltip = (String)_avatarIdToTooltipShort.remove(id);
            _avatarIdToTooltip.remove(id);
            _avatarIdToURI.remove(id);
            _avatarTooltipShortToAvatarId.remove(tooltip);
            if (img != null)
                toDisposeImages.add(img);
        }
        
        // ok now we know what to do, so lets do it
        _root.getDisplay().asyncExec(new Runnable() {
            public void run() {
                _ui.debugMessage("disposing " + toDisposeImages.size() + " and adding " + toAddIds.size());
                
                for (int i = 0; i < toDisposeImages.size(); i++)
                    ImageUtil.dispose((Image)toDisposeImages.get(i));
                
                // now lets create the Images that we don't already have
                for (int i = 0; i < toAddIds.size(); i++) {
                    Long id = (Long)toAddIds.get(i);
                    byte avatar[] = (byte[])toAddAvatarData.get(i);
                    Image img = null;
                    if (avatar.length > 0)
                        img = ImageUtil.createImage(avatar);
                    if (img != null) {
                        Rectangle rect = img.getBounds();
                        int sz = Constants.MAX_AVATAR_HEIGHT;
                        if ( (rect.height > sz) || (rect.width > sz) )
                            img = ImageUtil.resize(img, sz, sz, true);
                        _avatarIdToImage.put(id, img);
                    } else {
                        _avatarIdToImage.put(id, ImageUtil.ICON_EDITOR_BOOKMARKED_NOAVATAR);
                    }
                }
                
                _ui.debugMessage("images loaded/disposed, creating " + _avatarIdToImage.size() + " buttons");
                
                // now we have all the data we need.  build the buttons!
                for (Iterator iter = _avatarTooltipShortToAvatarId.entrySet().iterator(); iter.hasNext(); ) {
                    Map.Entry entry = (Map.Entry)iter.next();
                    String tooltipShort = (String)entry.getKey();
                    Long chanId = (Long)entry.getValue();
                    String tooltip = (String)_avatarIdToTooltip.get(chanId);
                    Image img = (Image)_avatarIdToImage.get(chanId);
                    final SyndieURI uri = (SyndieURI)_avatarIdToURI.get(chanId);
                    
                    final SyndieURI unreadURI = SyndieURI.createSearch(uri.getScope(), true, true, MessageTree.shouldUseImportDate(_client));
                    final SyndieURI msgsURI = SyndieURI.createSearch(uri.getScope(), false, true, MessageTree.shouldUseImportDate(_client));
                    
                    Button b = new Button(_avatars, SWT.PUSH);
                    b.setImage(img);
                    b.setToolTipText(tooltip);
                    b.addSelectionListener(new FireSelectionListener() {
                        public void fire() { 
                            if (unreadOnly)
                                _navControl.view(unreadURI);
                            else
                                _navControl.view(msgsURI);
                            close();
                        }
                    });
                    Menu m = new Menu(b);
                    b.setMenu(m);
                    MenuItem viewUnread = new MenuItem(m, SWT.PUSH);
                    viewUnread.addSelectionListener(new FireSelectionListener() { public void fire() { _navControl.view(unreadURI); close(); } });
                    viewUnread.setText(_translationRegistry.getText(T_BUTTON_VIEW_UNREAD, "View unread messages"));
                    
                    MenuItem viewAll = new MenuItem(m, SWT.PUSH);
                    viewAll.addSelectionListener(new FireSelectionListener() { public void fire() { _navControl.view(msgsURI); close(); } });
                    viewAll.setText(_translationRegistry.getText(T_BUTTON_VIEW_ALL, "View all messages"));
                    
                    MenuItem profile = new MenuItem(m, SWT.PUSH);
                    profile.addSelectionListener(new FireSelectionListener() { public void fire() { _navControl.view(_uriControl.createMetaURI(uri.getScope())); close(); } });
                    profile.setText(_translationRegistry.getText(T_BUTTON_PROFILE, "View profile"));
                    
                    MenuItem pm = new MenuItem(m, SWT.PUSH);
                    pm.addSelectionListener(new FireSelectionListener() { public void fire() { _navControl.view(_uriControl.createPostURI(uri.getScope(), null, true)); } });
                    pm.setText(_translationRegistry.getText(T_BUTTON_PM, "Send a private message"));
                    
                    new MenuItem(m, SWT.SEPARATOR);
                    
                    MenuItem unwatch = new MenuItem(m, SWT.PUSH);
                    unwatch.addSelectionListener(new FireSelectionListener() { 
                        public void fire() { 
                            _client.unwatchChannel(uri.getScope());
                        } 
                    });
                    unwatch.setText(_translationRegistry.getText(T_BUTTON_UNWATCH, "Stop watching the forum"));
                    
                    BookmarkDnDHelper.initSource(b, uri, tooltip);
                }
                
                int total = _avatarIdToImage.size();
                if (total <= 4)
                    ((GridLayout)_avatars.getLayout()).numColumns = 1;
                else if (total < 8)
                    ((GridLayout)_avatars.getLayout()).numColumns = 2;
                else if (total < 16)
                    ((GridLayout)_avatars.getLayout()).numColumns = 4;
                else if (total < 32)
                    ((GridLayout)_avatars.getLayout()).numColumns = 6;
                else
                    ((GridLayout)_avatars.getLayout()).numColumns = 8;
                _onRefresh.run();
                _avatars.setRedraw(true);
                _root.layout(true, true);
                //_parent.pack(true);
                _ui.debugMessage("watched panel gui update complete");
            }
        });
    }

    private static final String T_BUTTON_VIEW_ALL = "syndie.gui.watchedpanel.buttonviewall";
    private static final String T_BUTTON_VIEW_UNREAD = "syndie.gui.watchedpanel.buttonviewunread";
    private static final String T_BUTTON_PROFILE = "syndie.gui.watchedpanel.buttonprofile";
    private static final String T_BUTTON_PM = "syndie.gui.watchedpanel.buttonpm";
    private static final String T_BUTTON_UNWATCH = "syndie.gui.watchedpanel.buttonunwatch";
    
    public void close() {
        if (_onClose != null) _onClose.run();
        dispose();
    }
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        _ui.debugMessage("disposing the watched panel, dropping " + _avatarIdToImage.size() + " images");
        for (Iterator iter = _avatarIdToImage.values().iterator(); iter.hasNext(); )
            ImageUtil.dispose((Image)iter.next());
        _avatarIdToImage.clear();
        _avatarIdToTooltipShort.clear();
        _avatarIdToURI.clear();
        _avatarTooltipShortToAvatarId.clear();
    }
    private void toggleUnread() { loadData(); }
    
    private static final String T_UNREADONLY = "syndie.gui.watchedpanel.unreadonly";
    private static final String T_CLOSE = "syndie.gui.watchedpanel.close";
    
    public void translate(TranslationRegistry registry) {
        _unreadOnly.setText(registry.getText(T_UNREADONLY, "Only include watched forums with unread messages"));
        _close.setText(registry.getText(T_CLOSE, "Close"));
    }
    public void applyTheme(Theme theme) {
        _unreadOnly.setFont(theme.DEFAULT_FONT);
        _close.setFont(theme.BUTTON_FONT);
    }
}
