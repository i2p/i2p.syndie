package syndie.gui;

import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.NymKey;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.Timer;
import syndie.data.WatchedChannel;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.UI;

/**
 * coordinates the selection of channels, displaying each one's avatar as a button,
 * firing off hooks when an avatar is selected or focus is gained, and allowing the
 * filtering of the channels based on either a search term or whether the channel has
 * any unread messages.
 */
public class ChannelSelectorPanel extends BaseComponent implements Themeable, Translatable {
    private Composite _parent;
    
    private Composite _root;
    private Composite _top;
    private Button _unreadOnly;
    private Text _search;
    private Button _searchButton;
    private Composite _scrollContainer;
    private ScrolledComposite _scroll;
    private Composite _buttons;
    
    private List _records;
    private boolean _isUnreadOnly; // whether _records contains unread only
    
    public interface ChannelSelectorListener {
        public void channelReviewed(SyndieURI uri, long channelId, String name, String description, Image avatar);
        public void channelSelected(SyndieURI uri, int matchedIndex);
    }
    private ChannelSelectorListener _lsnr;
    
    public interface ChannelIdSource {
        public List listChannelIds();
        public List getReferenceNodes();
    }
    abstract class BasicIdSource implements ChannelIdSource { public List getReferenceNodes() { return null; } }
    private ChannelIdSource _idSource;
    
    public ChannelSelectorPanel(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite parent, ChannelSelectorPanel.ChannelSelectorListener lsnr) {
        super(client, ui, themes, trans);
        _parent = parent;
        _lsnr = lsnr;
        _records = new ArrayList();
        _isUnreadOnly = false;
        initComponents();
    }

    public Control getRoot() { return _root; }
    protected Composite getTop() { return _top; }
    
    public int getRecordCount() { return _records.size(); }
    public List getMatches() {
        List rv = new ArrayList();
        for (int i = 0; i < _records.size(); i++) {
            Record r = (Record)_records.get(i);
            rv.add(r.scope);
        }
        return rv;
    }
    public List getMatchingNodes() {
        List rv = new ArrayList();
        for (int i = 0; i < _records.size(); i++) {
            Record r = (Record)_records.get(i);
            rv.add(r.node);
        }
        return rv;
    }
    public boolean isUnreadOnly() { return _isUnreadOnly; }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        _root.setLayout(gl);
        
        _top = new Composite(_root, SWT.NONE);
        _top.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        initTop();
        
        // scrolledContainer is an extra container to tell us how much room we have to 
        // jam the scrolled area into without having to compare against _root and _top's size
        _scrollContainer = new Composite(_root, SWT.NONE);
        _scrollContainer.setLayout(new FillLayout());
        _scrollContainer.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _scroll = new ScrolledComposite(_scrollContainer, SWT.V_SCROLL);
        //_scroll.setExpandHorizontal(true);
        //_scroll.setExpandVertical(true);
        //_scroll.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _buttons = new Composite(_scroll, SWT.NONE);
        _scroll.setContent(_buttons);
        //_buttons.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        /*
        RowLayout rl = new RowLayout(SWT.HORIZONTAL);
        rl.fill = true;
        rl.pack = false;
        _buttons.setLayout(rl);
         */
        _buttons.setLayout(new GridLayout(1, true));
        _buttons.setBackground(ColorUtil.getColor("green"));
        _scroll.setBackground(ColorUtil.getColor("green"));
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    protected void initTop() {
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        _top.setLayout(gl);
        _unreadOnly = new Button(_top, SWT.CHECK);
        _unreadOnly.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _unreadOnly.addSelectionListener(new FireSelectionListener() { public void fire() { recalcChannels(); } });    
        _search = new Text(_top, SWT.SINGLE);
        _search.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, true));
        _search.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) search(_search.getText());
            }
        });
        /*
        _search.addFocusListener(new FocusListener() {
            public void focusGained(FocusEvent focusEvent) { _search.selectAll(); }
            public void focusLost(FocusEvent focusEvent) {}
        });
         */
        _searchButton = new Button(_top, SWT.PUSH);
        _searchButton.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _searchButton.addSelectionListener(new FireSelectionListener() { public void fire() { search(_search.getText()); } });
    }

    protected void search(final String term) {
        setChannelIdSource(new BasicIdSource() { public List listChannelIds() { return _client.getChannelIds(term); } });
        recalcChannels();
    }
    
    /** 
     * tell the table to render the given channelIds.  this should NOT be run from the 
     * swt thread.
     * @param src either a list of channel ids (Long) or bookmarks (ReferenceNode) to render
     */
    public void setChannels(List src, final Runnable afterSet) { setChannels(false, src, afterSet); }
    private void setChannels(boolean unreadOnly, List src, final Runnable afterSet) {
        if ( (src == null) || (src.size() == 0) ) return;
        _isUnreadOnly = unreadOnly;
        
        final Timer timer = new Timer("set channels", _ui);
        Display d = _root.getDisplay();
        // step 1 gets the basic data per channel
        // step 2 grabs the avatar data
        // step 3 runs in the GUI thread, creating avatars and rows
        final List records = getRecordsBasic(src); // returns channels ordered by name
        timer.addEvent("record basics fetched");
        getAvatarData(records);
        timer.addEvent("avatar data fetched");
        d.asyncExec(new Runnable() { public void run() { setChannelData(records, afterSet, timer); } });
    }
    
    private static final String T_VIEWALL = "syndie.gui.channelselectorpanel.viewall";
    
    private void setChannelData(List records, Runnable afterSet, Timer timer) {
        _buttons.setRedraw(false);
        Control buttons[] = _buttons.getChildren();
        List existingRecords = new ArrayList(_records);
        _records.clear();
        Map chanIdToOldRecord = new HashMap();
        for (int i = 0; i < buttons.length; i++)
            buttons[i].dispose();
        for (int i = 0; i < existingRecords.size(); i++) {
            Record rec = (Record)existingRecords.get(i);
            if (rec.channelId >= 0)
                chanIdToOldRecord.put(new Long(rec.channelId), rec);
            else if (rec.node != null)
                chanIdToOldRecord.put(new Long(rec.node.getUniqueId()), rec);
        }
        
        int maxWidth = 48;
        
        Button first = null;
        Button last = null;
        for (int i = 0; i < records.size(); i++) {
            final Record r = (Record)records.get(i); // no need to sort - getRecordsBasic did for us
            long id = r.channelId;
            if (id < 0) id = r.node.getUniqueId();
            if (r.avatarData != null) {
                Record oldRecord = (Record)chanIdToOldRecord.get(new Long(id));
                if (oldRecord != null) {
                    r.avatar = oldRecord.avatar;
                    oldRecord.avatar = null; // so we don't dispose it
                } else {
                   r.avatar = ImageUtil.createImage(r.avatarData);
                   if (r.avatar != null) {
                       Rectangle rect = r.avatar.getBounds();
                       if ( (rect.height > Constants.MAX_AVATAR_HEIGHT) || (rect.width > Constants.MAX_AVATAR_WIDTH) )
                           r.avatar = ImageUtil.resize(r.avatar, Constants.MAX_AVATAR_WIDTH, Constants.MAX_AVATAR_HEIGHT, true);
                   }
                }
                r.avatarData = null;
            }
            if (r.avatar == null) {
                if ( (r.node == null) || (r.node.getChildCount() == 0) ) {
                    r.avatar = ImageUtil.ICON_EDITOR_BOOKMARKED_NOAVATAR; // message or other ref type
                } else {
                    r.avatar = ImageUtil.ICON_EDITOR_LINK; // folder
                }
            }
            
            int width = r.avatar.getBounds().width;
            if (width > maxWidth)
                maxWidth = width;
            
            String tooltip = null;
            if (r.node == null) {
                if (r.name != null)
                    tooltip = r.name + " [" + r.scope.toBase64().substring(0,6) + "]";
                else
                    tooltip = "[" + r.scope.toBase64().substring(0,6) + "]";
                if (r.desc != null)
                    tooltip = tooltip + " - " + r.desc;
            } else {
                if (r.node.getName() != null)
                    tooltip = r.node.getName();
                if (r.desc != null)
                    tooltip = tooltip + " - " + r.node.getDescription();
            }

            final String selectedText = tooltip;
            
            final Button b = new Button(_buttons, SWT.PUSH);
            b.setImage(r.avatar);
            b.setToolTipText(tooltip);
            configButtonMenu(b, r.channelId, r.scope, r.name);
            final int matchedIndex = i;
            b.addSelectionListener(new FireSelectionListener() { public void fire() { _lsnr.channelSelected(r.uri, matchedIndex); } });
            b.addFocusListener(new FocusListener() {
                public void focusGained(FocusEvent focusEvent) { 
                    _lsnr.channelReviewed(r.uri, r.channelId, r.name, r.desc, r.avatar);
                }
                public void focusLost(FocusEvent focusEvent) {}
            });
            b.addMouseTrackListener(new MouseTrackListener() {
                public void mouseEnter(MouseEvent mouseEvent) {
                    _lsnr.channelReviewed(r.uri, r.channelId, r.name, r.desc, r.avatar);
                }
                public void mouseExit(MouseEvent mouseEvent) {}
                public void mouseHover(MouseEvent mouseEvent) {}
            });
            if ( (r.node != null) && (r.node.getChildCount() > 0) ) {
                Menu m = new Menu(b);
                b.setMenu(m);
                List roots = new ArrayList(1);
                roots.add(r.node);
                final List scopes = new ArrayList();
                ReferenceNode.walk(roots, new ReferenceNode.Visitor() {
                    public void visit(ReferenceNode node, int depth, int siblingOrder) {
                        SyndieURI uri = node.getURI();
                        if (uri == null) {
                            // ignore
                        } else if (uri.isChannel() && (uri.getMessageId() == null)) {
                            Hash scope = uri.getScope();
                            if (!scopes.contains(scope))
                                scopes.add(scope);
                        } else if (uri.isSearch()) {
                            Hash search[] = uri.getSearchScopes();
                            if (search != null) {
                                for (int i = 0; i < search.length; i++) {
                                    if (!scopes.contains(search[i]))
                                        scopes.add(search[i]);
                                }
                            }
                        }
                    }
                });
                final boolean useImportDate = MessageTree.shouldUseImportDate(_client);
                MenuItem viewAll = new MenuItem(m, SWT.PUSH);
                viewAll.addSelectionListener(new FireSelectionListener() {
                    public void fire() { 
                        SyndieURI mergedURI = SyndieURI.createSearch(scopes, _unreadOnly.getSelection(), true, useImportDate);
                        _lsnr.channelSelected(mergedURI, -1); 
                    }
                });
                viewAll.setText(_translationRegistry.getText(T_VIEWALL, "Combined view of this category's forums, recursively"));
            }
            
            // buttons don't traverse on arrow keys by default, but these should
            final boolean isFirst = (i == 0);
            final boolean isLast = (i == records.size()-1);
            b.addTraverseListener(new TraverseListener() {
                public void keyTraversed(TraverseEvent evt) {
                    if ( (evt.detail == SWT.TRAVERSE_ARROW_NEXT) && (!isLast) )
                        b.traverse(SWT.TRAVERSE_TAB_NEXT);
                    else if ( (evt.detail == SWT.TRAVERSE_ARROW_PREVIOUS) && (!isFirst) )
                        b.traverse(SWT.TRAVERSE_TAB_PREVIOUS);
                }
            });
            
            if (isFirst) first = b;
            _records.add(r);
            
            //timer.addEvent("record " + i + " rendered");
        }
        timer.addEvent("all records rendered");
    
        for (int i = 0; i < existingRecords.size(); i++) {
            Record r = (Record)existingRecords.get(i);
            if (r.avatar != null)
                ImageUtil.dispose(r.avatar);
        }
        timer.addEvent("columns packed");
        Rectangle bounds = _scrollContainer.getBounds();
        bounds = _scrollContainer.getClientArea();
    
        int numCols = bounds.width / (maxWidth + 18); // 18?  trial and error.
        ((GridLayout)_buttons.getLayout()).numColumns = numCols;
        
        Point sz = _buttons.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        //_ui.errorMessage("bounds for the scroll container: " + bounds.width + "x" + bounds.height + " : " + sz);
        _buttons.setSize(sz);
        _root.layout(true, true);
        first.forceFocus();
        _buttons.setRedraw(true);
        timer.addEvent("redraw reenabled");
        timer.complete();
        if (afterSet != null)
            afterSet.run();
    }
    
    protected void configButtonMenu(Button button, long channelId, Hash scope, String name) {}
    
    private void disposeExisting() {
        while (_records.size() > 0)
            ImageUtil.dispose(((Record)_records.remove(0)).avatar);
        _records.clear();
    }
    
    private static final String SQL_GET_RECORD_BEGIN = "SELECT name, channelHash, description, channelId FROM channel WHERE channelId IN (";
    private List getRecordsBasic(List src) {
        if ( (src == null) || (src.size() <= 0) ) return src;
        Object first = src.get(0);
        if (first instanceof Long)
            return getRecordsFromIds(src);
        else
            return getRecordsFromNodes(src);
    }
    private List getRecordsFromIds(List channelIds) {
        StringBuffer query = new StringBuffer(SQL_GET_RECORD_BEGIN);
        int numIds = channelIds.size();
        for (int i = 0; i < numIds; i++) {
            Long id = (Long)channelIds.get(i);
            query.append(id.longValue());
            if (i + 1 < numIds)
                query.append(", ");
        }
        query.append(") ORDER BY name");
        Statement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _client.con().createStatement();
            rs = stmt.executeQuery(query.toString());
            List rv = new ArrayList();
            while (rs.next()) {
                String name = rs.getString(1);
                byte hash[] = rs.getBytes(2);
                String desc = rs.getString(3);
                long channelId = rs.getLong(4);
                
                if ( (channelId < 0) || (hash == null) || (hash.length != Hash.HASH_LENGTH) )
                    continue;
                
                Record r = new Record();
                r.channelId = channelId;
                r.name = name;
                r.desc = desc;
                r.scope = new Hash(hash);
                r.uri = SyndieURI.createScope(r.scope);
                rv.add(r);
            }
            return rv;
        } catch (SQLException se) {
            _ui.errorMessage("Internal error loading the basic data", se);
            return new ArrayList();
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private List getRecordsFromNodes(List nodes) {
        List rv = new ArrayList();
        for (int i = 0; i < nodes.size(); i++) {
            ReferenceNode node = (ReferenceNode)nodes.get(i);
            String name = node.getName();
            Hash scope = null;
            String desc = node.getDescription();
            long channelId = -1;
            
            SyndieURI uri = node.getURI();
            if (uri != null) {
                if (uri.isChannel()) {
                    scope = uri.getScope();
                    channelId = _client.getChannelId(scope);
                } else if (uri.isSearch()) {
                    Hash scopes[] = uri.getSearchScopes();
                    if ( (scopes != null) && (scopes.length == 1) ) {
                        scope = scopes[0];
                        channelId = _client.getChannelId(scopes[0]);
                    }
                }
            }
            
            Record r = new Record();
            r.channelId = channelId;
            r.name = name;
            r.desc = desc;
            r.scope = scope;
            r.uri = uri;
            r.node = node;
            rv.add(r);
        }
        return rv;
    }
    
    private void getAvatarData(List records) {
        for (int i = 0; i < records.size(); i++) {
            Record r = (Record)records.get(i);
            if ( (r.node != null) && (r.node.getURI() != null) && (r.node.getURI().getMessageId() != null) ) {
                // dont use the avatar for messages
                r.avatarData = null;
            } else {
                byte avatar[] = (r.channelId >= 0 ? _client.getChannelAvatar(r.channelId) : null);
                r.avatarData = avatar;
            }
            //if (avatar == null)
            //    _ui.debugMessage("no avatar for channelId " + r.channelId + " [" + r.name + "]");
        }
    }
    
    /** load up as much as we can async before mucking with the gui thread */
    private class Record {
        long channelId;
        String name;
        Hash scope;
        String desc;
        byte avatarData[];
        Image avatar;
        // for bookmarks
        SyndieURI uri;
        ReferenceNode node;
    }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        disposeExisting();
    }
    
    public void setChannelIdSource(ChannelIdSource source) { _idSource = source; }
    /** 
     * queue up the recalc task - call this from the SWT thread
     */
    public void recalcChannels() { recalcChannels(null); }
    /** @param afterSet task to run in the SWT thread after the panel has been updated */
    public void recalcChannels(final Runnable afterSet) {
        recalcChannels(_unreadOnly.getSelection(), afterSet);
    }
    public void recalcChannels(final boolean unreadOnly, final Runnable afterSet) {
        JobRunner.instance().enqueue(new Runnable() {
            public void run() {
                final Timer timer = new Timer("recalc channels, unread only? " + unreadOnly, _ui);
                List chanIds = _idSource.listChannelIds();
                if (chanIds != null) {
                    for (int i = 0; i < chanIds.size(); i++) {
                        Long id = (Long)chanIds.get(i);
                        if (unreadOnly) {
                            int unread = _client.countUnreadMessages(id.longValue());
                            if (unread == 0) {
                                chanIds.remove(i);
                                i--;
                                continue;
                            }
                        }
                    }
                } else {
                    chanIds = _idSource.getReferenceNodes();
                }
                setChannels(unreadOnly, chanIds, afterSet);
            }
        });
    }
    
    public void showWatched(Runnable afterSet) { showWatched(_unreadOnly.getSelection(), afterSet); }
    public void showWatched(boolean unreadOnly, Runnable afterSet) {
        setChannelIdSource(new BasicIdSource() {
            public List listChannelIds() {
                List chans = _client.getWatchedChannels(); 
                List chanIds = new ArrayList();
                for (int i = 0; i < chans.size(); i++) {
                    WatchedChannel c = (WatchedChannel)chans.get(i);
                    chanIds.add(new Long(c.getChannelId()));
                }
                return chanIds;
            }
        });
        recalcChannels(unreadOnly, afterSet);
    }
    public void showIdent(Runnable afterSet) {
        setChannelIdSource(new BasicIdSource() {
            public List listChannelIds() { return _client.getChannels(false, true, false, false, false).getAllIds(); }
        });
        recalcChannels(afterSet);
    }
    public void showManageable(Runnable afterSet) {
        setChannelIdSource(new BasicIdSource() {
            public List listChannelIds() { return _client.getChannels(true, true, false, false, false).getAllIds(); }
        });
        recalcChannels(afterSet);
    }
    public void showReplyReadable(Runnable afterSet) {
        /*
        JobRunner.instance().enqueue(new Runnable() {
            public void run() {
                final Timer timer = new Timer("my nyms", _ui);
                List channelIds = _client.getChannels(false, false, false, false, false).getAllIds();
                timer.addEvent("channels found");
                setChannels(channelIds, afterSet);
            }
        });
         */
    }
    public void showPostable(Runnable afterSet) {
        setChannelIdSource(new BasicIdSource() {
            public List listChannelIds() { return _client.getChannels(true, true, true, false, false).getAllIds(); }
        });
        recalcChannels(afterSet);
    }
    public void showReferences(Runnable afterSet) {
        setChannelIdSource(new ChannelIdSource() {
            public List listChannelIds() { return null; }
            public List getReferenceNodes() { return _client.getNymReferences(_client.getLoggedInNymId()); }
        });
        recalcChannels(afterSet);
    }
    
    
    private static final String T_UNREADONLY = "syndie.gui.channelselectorpanel.unreadonly";
    private static final String T_SEARCH = "syndie.gui.channelselectorpanel.search";
    private static final String T_SEARCH_BUTTON = "syndie.gui.channelselectorpanel.search.button";

    public void translate(TranslationRegistry registry) {
        _unreadOnly.setText(registry.getText(T_UNREADONLY, "Only include forums with unread messages"));
        _search.setText(registry.getText(T_SEARCH, "search term"));
        _searchButton.setText(registry.getText(T_SEARCH_BUTTON, "search"));
    }
    
    public void applyTheme(Theme theme) {
        _unreadOnly.setFont(theme.DEFAULT_FONT);
        _search.setFont(theme.DEFAULT_FONT);
        _searchButton.setFont(theme.BUTTON_FONT);
        _root.layout(true, true);
    }
}