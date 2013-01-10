package syndie.gui;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

import net.i2p.data.Hash;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.events.TreeEvent;
import org.eclipse.swt.events.TreeListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import syndie.data.ChannelInfo;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.Timer;
import syndie.data.WatchedChannel;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.UI;

/**
 * The reference chooser tree has five roots, plus a search results list
 * - Watched forums
 *  - $forumName
 *  - $forumName
 * - manageable forums
 *   - $forumName
 * - postable forums
 *   - $forumName
 * - Categorized bookmarks
 *   - $forumName
 *   - $categoryName
 *     - $forumName
 *     - $categoryName
 *       - $otherResource
 * - Imported resources
 *  - $forumName
 *   - $archive|$ban|$bookmark
 */
public class ReferenceChooserTree extends BaseComponent implements Translatable, Themeable, DBClient.WatchEventListener {
    protected BanControl _dataControl;
    protected NavigationControl _navControl;
    protected URIControl _uriControl;
    private Composite _parent;
    /** list of NymReferenceNode instances for the roots of the reference trees */
    private ArrayList _nymRefs;
    /** true during nymRef rebuild */
    private volatile boolean _rebuilding;
    /** true if viewOnStartup was called before rebuilding was complete */
    private volatile boolean _viewOnStartup;
    /** organizes the channels the nym can post to or manage */
    private DBClient.ChannelCollector _nymChannels;
    /** list of ReferenceNode instances matching the search criteria */
    private ArrayList _searchResults;
    private Map _watchedItemToWatchedChannel;
    
    private Composite _root;
    private Tree _tree;
    private List _searchList;

    private TreeItem _watchedRoot;
    private TreeItem _importedRoot;
    
    private TreeItem _bookmarkRoot;
    /** map of TreeItem to NymReferenceNode */
    private Map _bookmarkNodes;
    private TreeItem _postRoot;
    /** map of TreeItem to ChannelInfo */
    private Map _postChannels;
    private TreeItem _manageRoot;
    /** map of TreeItem to ChannelInfo */
    private Map _manageChannels;

    private ChoiceListener _choiceListener;
    private AcceptanceListener _acceptanceListener;
    private boolean _chooseAllStartupItems;
    private boolean _showSearchList;
    
    /** don't open a zillion tabs at startup */
    private static final int MAX_PREV_TABS = 4;

    public ReferenceChooserTree(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl nav, URIControl uriControl, Composite parent, ChoiceListener lsnr, AcceptanceListener accept) {
        this(client, ui, themes, trans, nav, uriControl, parent, lsnr, accept, false, true, null);
    }
    public ReferenceChooserTree(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl nav, URIControl uriControl, Composite parent, ChoiceListener lsnr, AcceptanceListener accept, boolean chooseAllStartupItems, boolean showSearchList) {
        this(client, ui, themes, trans, nav, uriControl, parent, lsnr, accept, chooseAllStartupItems, showSearchList, null);
    }    
    public ReferenceChooserTree(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl nav, URIControl uriControl, Composite parent, ChoiceListener lsnr, AcceptanceListener accept, Timer timer) {
        this(client, ui, themes, trans, nav, uriControl, parent, lsnr, accept, false, true, timer);
    }
    public ReferenceChooserTree(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl nav, URIControl uriControl, Composite parent, ChoiceListener lsnr, AcceptanceListener accept, boolean chooseAllStartupItems, boolean showSearchList, Timer timer) {
        super(client, ui, themes, trans);
        _navControl = nav;
        _uriControl = uriControl;
        _parent = parent;
        _choiceListener = lsnr;
        _acceptanceListener = accept;
        _chooseAllStartupItems = chooseAllStartupItems;
        _nymRefs = new ArrayList();
        _watchedItemToWatchedChannel = new HashMap();
        _bookmarkNodes = new HashMap();
        _postChannels = new HashMap();
        _manageChannels = new HashMap();
        _searchResults = new ArrayList();
        _showSearchList = showSearchList;
        initComponents(true, false, timer);
    }

    DBClient getClient() { return _client; }
    public Control getControl() { return _root; }
    /**
     * update the search results to display.  this then redraws the seach result entry in the
     * swt display thread.
     * @param resultNodes ReferenceNode instances to display as search results
     */
    public void setSearchResults(Collection resultNodes) {
        _searchResults.clear();
        for (Iterator iter = resultNodes.iterator(); iter.hasNext(); ) {
            ReferenceNode node = (ReferenceNode)iter.next();
            _searchResults.add(node);
        }
        _tree.getDisplay().asyncExec(new Runnable() { public void run() { redrawSearchResults(true); } });
    }
    public void setListener(ChoiceListener lsnr) { _choiceListener = lsnr; }
    public void setAcceptanceListener(AcceptanceListener lsnr) { _acceptanceListener = lsnr; }

    protected ChoiceListener getChoiceListener() { return _choiceListener; }
    protected AcceptanceListener getAcceptanceListener() { return _acceptanceListener; }
    protected Tree getTree() { return _tree; }
    protected BanControl getDataControl() { return _dataControl; }
    protected WatchedChannel getWatchedChannel(TreeItem item) {
        return (WatchedChannel)_watchedItemToWatchedChannel.get(item);
    }
    protected TreeItem getSelectedItem() {
        TreeItem selected[] = getTree().getSelection();
        if ( (selected != null) && (selected.length == 1) )
            return selected[0];
        else
            return null;
    }

    
    public interface ChoiceListener {
        public void watchedChannelSelected(TreeItem item, WatchedChannel channel);
        public void bookmarkSelected(TreeItem item, NymReferenceNode node);
        public void manageChannelSelected(TreeItem item, ChannelInfo channel);
        public void postChannelSelected(TreeItem item, ChannelInfo channel);
        public void searchResultSelected(String name, ReferenceNode node);
        public void otherSelected(TreeItem item);
    }
    
    public interface AcceptanceListener {
        public void referenceAccepted(SyndieURI uri);
        public void referenceChoiceAborted();
    }

    public void refreshBookmarks() {
        _tree.setRedraw(false);
        boolean rootWasOpen = _bookmarkRoot.getExpanded();
        ArrayList openGroupIds = new ArrayList();
        if (rootWasOpen) {
            for (int i = 0; i < _bookmarkRoot.getItemCount(); i++)
                getOpenGroupIds(_bookmarkRoot.getItem(i), openGroupIds);
        }
        rebuildBookmarks();
        _ui.debugMessage("refresh bookmarks: open groupIds: " + openGroupIds + " rootOpen: " + rootWasOpen);
        if (rootWasOpen) {
            _bookmarkRoot.setExpanded(true);
            ArrayList pending = new ArrayList();
            TreeItem cur = _bookmarkRoot;
            while (cur != null) {
                boolean includeChildren = false;
                if (cur != _bookmarkRoot) {
                    NymReferenceNode curNode = (NymReferenceNode)_bookmarkNodes.get(cur);
                    if ( (curNode != null) && (openGroupIds.contains(new Long(curNode.getGroupId()))) ) {
                        cur.setExpanded(true);
                        includeChildren = true;
                    }
                } else {
                    includeChildren = true; 
                }
                if (includeChildren) {
                    for (int i = 0; i < cur.getItemCount(); i++)
                        pending.add(cur.getItem(i));
                }
                if (pending.size() > 0)
                    cur = (TreeItem)pending.remove(0);
                else
                    cur = null;
            }
        }
        _tree.setRedraw(true);
    }
    
    public boolean isBookmarked(SyndieURI uri) {
        if (uri == null) return false;
        Hash scope = uri.getScope();
        long chanId = _client.getChannelId(scope);
        if (chanId >= 0) {
            ArrayList watched = new ArrayList(_client.getWatchedChannels());
            for (int i = 0; i < watched.size(); i++) {
                WatchedChannel chan = (WatchedChannel)watched.get(i);
                if (chan.getChannelId() == chanId)
                    return true;
            }
        }
        for (int i = 0; i < _nymRefs.size(); i++) {
            ReferenceNode ref = (ReferenceNode)_nymRefs.get(i);
            if (isBookmarked(ref, uri))
                return true;
        }
        return false;
    }
    private boolean isBookmarked(ReferenceNode ref, SyndieURI uri) {
        if ( (ref.getURI() != null) && (ref.getURI().equals(uri)) )
            return true;
        for (int i = 0; i < ref.getChildCount(); i++)
            if (isBookmarked(ref.getChild(i), uri))
                return true;
        return false;
    }
    
    private long getOpenGroupIds(TreeItem base, ArrayList openGroupIds) {
        NymReferenceNode node = (NymReferenceNode)_bookmarkNodes.get(base);
        long sel = -1;
        if (base.getExpanded()) {
            openGroupIds.add(new Long(node.getGroupId()));
            for (int i = 0; i < base.getItemCount(); i++) {
                long id = getOpenGroupIds(base.getItem(i), openGroupIds);
                if (id >= 0)
                    sel = id;
            }
        }
        if (node != null) {
            TreeItem selection[] = _tree.getSelection();
            if (selection != null) {
                for (int i = 0; i < selection.length; i++) {
                    if (selection[i] == base) {
                        sel = node.getGroupId();
                        break;
                    }
                }
            }
        }
        return sel;
    }
    
    public void localForumCreated() {
        JobRunner.instance().enqueue(new Runnable() {
            public void run() {
                refetchNymChannels();
                Display.getDefault().asyncExec(new Runnable() {
                   public void run() {
                       redrawPostable();
                       redrawManageable();
                       redrawSearchResults(false);
                   } 
                });
            }
        });
    }
    
    protected void watch(SyndieURI uri, TreeItem item) {
        Hash scope = uri.getScope();
        if (scope == null) {
            Hash scopes[] = uri.getSearchScopes();
            if ( (scopes == null) || (scopes.length != 1) )
                return;
            scope = scopes[0];
        }
        boolean highlight = true;
        boolean impArchives = true;
        boolean impBookmarks = true;
        boolean impBans = false;
        boolean impKeys = false;
        
        long chanId = _client.getChannelId(scope);
        Collection chans = _client.getWatchedChannels();
        WatchedChannel chan = null;
        for (Iterator iter = chans.iterator(); iter.hasNext(); ) {
            WatchedChannel cur = (WatchedChannel)iter.next();
            if (cur.getChannelId() == chanId) {
                chan = cur;
                break;
            }
        }
        
        if (chan != null) { // already watched, so just make it highlighted
            highlight = true;
            impArchives = chan.getImportArchives();
            impBans = chan.getImportBans();
            impBookmarks = chan.getImportBookmarks();
            impKeys = chan.getImportKeys();
        } else { 
            // new, so use defaults from above
        }
        
        _client.watchChannel(scope, highlight, impArchives, impBookmarks, impBans, impKeys);
    }
    
    private void rebuildWatched() {
        _tree.setRedraw(false);
        boolean wasExpanded = _watchedRoot.getExpanded();
        _watchedItemToWatchedChannel.clear();
        _watchedRoot.removeAll();
        ArrayList watched = new ArrayList(_client.getWatchedChannels());
        for (int i = 0; i < watched.size(); i++) {
            WatchedChannel chan = (WatchedChannel)watched.get(i);
            String name = _client.getChannelName(chan.getChannelId());
            Hash hash = _client.getChannelHash(chan.getChannelId());
            if (hash == null) continue;
            
            if (name == null) name = "";
            name = name + " [" + hash.toBase64().substring(0,6) + "]";
            
            TreeItem item = new TreeItem(_watchedRoot, SWT.NONE);
            item.setText(name);
            item.setImage(ImageUtil.ICON_MSG_FLAG_BOOKMARKED_FORUM);
            _watchedItemToWatchedChannel.put(item, chan);
        }
        if (wasExpanded && watched.size() > 0)
            _watchedRoot.setExpanded(true);
        _tree.setRedraw(true);
    }
    
    protected void initComponents(boolean register, boolean multipleSelections, Timer timer) {
        if (timer != null) timer.addEvent("refChooserTree init");
        long t1 = System.currentTimeMillis();
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(1, true));
        _tree = new Tree(_root, SWT.BORDER | (multipleSelections ? SWT.MULTI : SWT.SINGLE));
        _tree.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _watchedRoot = new TreeItem(_tree, SWT.NONE);
        _bookmarkRoot = new TreeItem(_tree, SWT.NONE);
        _postRoot = new TreeItem(_tree, SWT.NONE);
        _manageRoot = new TreeItem(_tree, SWT.NONE);
        _importedRoot = new TreeItem(_tree, SWT.NONE);
        //_searchRoot = new TreeItem(_tree, SWT.NONE);
        
        SyndieTreeListener lsnr = new SyndieTreeListener(_tree) {
            /** the user doubleclicked on the selected row */
            public void doubleclick() {
                TreeItem item = getSelected();
                if (item != null)
                    fire(item);                
            }
            /** the user hit return on the selected row */
            public void returnHit() {
                TreeItem item = getSelected();
                if (item != null)
                    fire(item);
            }
            public void deleteHit() { deleteSelected(); }
            private void fire(TreeItem item) {
                if (_acceptanceListener == null)
                    return;
                NymReferenceNode node = (NymReferenceNode)_bookmarkNodes.get(item);
                if (node != null) {
                    _acceptanceListener.referenceAccepted(node.getURI());
                    return;
                }
                ChannelInfo info = (ChannelInfo)_postChannels.get(item);
                if (info != null) {
                    _acceptanceListener.referenceAccepted(SyndieURI.createScope(info.getChannelHash()));
                    return;
                }
                info = (ChannelInfo)_manageChannels.get(item);
                if (info != null) {
                    _acceptanceListener.referenceAccepted(SyndieURI.createScope(info.getChannelHash()));
                    return;
                }
                WatchedChannel chan = (WatchedChannel)_watchedItemToWatchedChannel.get(item);
                if (chan != null) {
                    Hash scope = _client.getChannelHash(chan.getChannelId());
                    _acceptanceListener.referenceAccepted(SyndieURI.createScope(scope));
                }
            }
        };
        _tree.addTraverseListener(lsnr);
        _tree.addSelectionListener(lsnr);
        _tree.addMouseListener(lsnr);
        _tree.addKeyListener(lsnr);
        
        _searchList = new List(_root, SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, true, true);
        _searchList.setLayoutData(gd);
        _searchList.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { searchSelected(false); }
            public void widgetSelected(SelectionEvent selectionEvent) { searchSelected(false); }
        });
        _searchList.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN)
                    searchSelected(true);
            }
        });
        _searchList.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) {
                searchSelected(true);
            }
            public void mouseDown(MouseEvent mouseEvent) {}
            public void mouseUp(MouseEvent mouseEvent) {}
        });
        if (!_showSearchList) {
            gd.exclude = true;
            _searchList.setVisible(false);
        }
        
        long t2 = System.currentTimeMillis();
        configTreeListeners(_tree);
        long t3 = System.currentTimeMillis();
        
        if (timer != null) timer.addEvent("refChooserTree init: gui built");
        //_browser.getUI().debugMessage("init refChooserTree", new Exception("source"));
        rebuildBookmarks();
        if (timer != null) timer.addEvent("refChooserTree init: bookmarks rebuilt");
        long t4 = System.currentTimeMillis();
        _root.getDisplay().timerExec(500, new Runnable() {
            public void run() {
                JobRunner.instance().enqueue(new Runnable() {
                    public void run() {
                        refetchNymChannels();
                        Display.getDefault().asyncExec(new Runnable() {
                           public void run() {
                               redrawPostable();
                               redrawManageable();
                               redrawSearchResults(false);
                               rebuildWatched();
                           } 
                        });
                    }
                });
            }
        });
        if (timer != null) timer.addEvent("refChooserTree init: after enqueue");
        long t8 = System.currentTimeMillis();
        
        _chooseAllStartupItems = false;
        if (register) {
            _translationRegistry.register(this);
            _themeRegistry.register(this);
        }
        if (timer != null) timer.addEvent("refChooserTree init: after register");
        
        _client.addWatchEventListener(this);
        long t9 = System.currentTimeMillis();
        if (timer != null) timer.addEvent("refChooserTree init: complete");
        //System.out.println("tree init: " + (t2-t1)+"/"+(t3-t2)+"/"+(t4-t3)+"/"+(t8-t4)+"/"+(t9-t8));
    }
    
    protected void deleteSelected() { }
    
    private void searchSelected(boolean accept) {
        int idx = _searchList.getSelectionIndex();
        if (idx >= 0) {
            ReferenceNode node = (ReferenceNode)_searchResults.get(idx);
            _choiceListener.searchResultSelected(_searchList.getItem(idx), node);
            if (accept && (_acceptanceListener != null))
                _acceptanceListener.referenceAccepted(node.getURI());
        }
    }
    
    public void dispose() {
        _client.removeWatchEventListener(this);
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
    }
    
    protected void configTreeListeners(final Tree tree) {
        SyndieTreeListener lsnr = new SyndieTreeListener(tree) { 
            public void selected() { fireSelectionEvents(); }
        };
        tree.addKeyListener(lsnr);
        tree.addTraverseListener(lsnr);
        tree.addSelectionListener(lsnr);
        tree.addControlListener(lsnr);
    }

    protected NymReferenceNode getBookmark(TreeItem item) { return (NymReferenceNode)_bookmarkNodes.get(item); }
    protected ChannelInfo getPostChannel(TreeItem item) { return (ChannelInfo)_postChannels.get(item); }
    protected ChannelInfo getManageChannel(TreeItem item) { return (ChannelInfo)_manageChannels.get(item); }
    protected ReferenceNode getSearchResult(String name) { 
        int idx = _searchList.indexOf(name);
        if (idx >= 0)
            return (ReferenceNode)_searchResults.get(idx);
        else
            return null;
    }
    protected ArrayList getBookmarks() { return new ArrayList(_bookmarkNodes.values()); }
    protected ArrayList getManageableChannels() { return new ArrayList(_manageChannels.values()); }
    protected ArrayList getPostableChannels() { return new ArrayList(_postChannels.values()); }
    
    //protected TreeItem getSearchRoot() { return _searchRoot; }
    protected TreeItem getManageRoot() { return _manageRoot; }
    protected TreeItem getPostRoot() { return _postRoot; }
    protected TreeItem getBookmarkRoot() { return _bookmarkRoot; }
    protected TreeItem getWatchedRoot() { return _watchedRoot; }
    
    protected void bookmarksRebuilt(ArrayList nymRefs) {}
    
    private void rebuildBookmarks() {
        JobRunner.instance().enqueue(new Rebuilder());
    }
    private class Rebuilder implements Runnable {
        public void run() {
            final long t1 = System.currentTimeMillis();
            synchronized (_nymRefs) {
                if (_rebuilding) return;
                _rebuilding = true;
            }
            _ui.debugMessage("rebuilder started");
            final ArrayList refs = new ArrayList(_client.getNymReferences(_client.getLoggedInNymId()));
            final long t2 = System.currentTimeMillis();
            synchronized (_nymRefs) {
                _nymRefs.clear();
                _nymRefs.addAll(refs);
            }
            final long t3 = System.currentTimeMillis();
            Display.getDefault().asyncExec(new Runnable() {
                public void run() { 
                    long t4 = System.currentTimeMillis();
                    bookmarksRebuilt(refs);
                    _tree.setRedraw(false);
                    redrawBookmarks();
                    long t5 = System.currentTimeMillis();
                    boolean view = false;
                    synchronized (_nymRefs) {
                        _rebuilding = false;
                        view = _viewOnStartup;
                        _viewOnStartup = false;
                        _nymRefs.notifyAll();
                    }
                    _ui.debugMessage("view? " + view);
                    if (view) {
                        int started = viewStartupItems(getBookmarkRoot());
                        Splash.dispose();
                    }
                    //getBookmarkRoot().setExpanded(true);
                    _tree.setRedraw(true);
                    long t6 = System.currentTimeMillis();
                    _ui.debugMessage("redraw after rebuild: " + (t6-t1) + " view? " + view + " " + (t2-t1)+"/"+(t3-t2)+"/"+(t4-t3)+"/"+(t5-t4)+"/"+(t6-t5));
                    if (shouldScheduleNow()) // only returns true once
                        askScheduleArchives();
                }
            });
        }
    }
    
    
    private void askScheduleArchives() {
        // rewrite this to detect whether the number of scheduled fetches is 0, and if so,
        // to point the user at the syndication page to config/schedule 
        _root.getDisplay().asyncExec(new Runnable() {
            public void run() {
                MessageBox box = new MessageBox(_tree.getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO);
                box.setMessage(_translationRegistry.getText("To use Syndie, you need to 'syndicate' messages between you and some remote archives.  Would you like to configure your syndication now?"));
                box.setText(_translationRegistry.getText("Schedule syndication?"));
                int rc = box.open();
                if (rc == SWT.YES)
                    _navControl.view(_uriControl.createSyndicationConfigURI());
            }
        });
    }
    
    private boolean shouldScheduleNow() {
        Properties prefs = _client.getNymPrefs();
        return Boolean.valueOf(prefs.getProperty("browser.promptScheduleNow")).booleanValue();
    }
    
    static void savePrevTabs(DBClient client, ArrayList uris) {
        Properties props = client.getNymPrefs();
        props.setProperty("browser.prevTabs", Integer.toString(uris.size()));
        for (int i = 0; i < uris.size(); i++)
            props.setProperty("browser.prevTab." + i, ((SyndieURI)uris.get(i)).toString());
        client.setNymPrefs(props);
    }
    
    private SyndieURI[] getPrevTabs() {
        Properties props = _client.getNymPrefs();
        String numTabs = props.getProperty("browser.prevTabs");
        int num = -1;
        if (numTabs != null) {
            try {
                int val = Integer.parseInt(numTabs);
                num = val;
            } catch (NumberFormatException nfe) {
                num = -1;
            }
        }
        if (num < 0)
            return null;
        
        SyndieURI rv[] = new SyndieURI[num];
        for (int i = 0; i < num; i++) {
            String val = props.getProperty("browser.prevTab." + i);
            if (val != null) {
                try {
                    rv[i] = new SyndieURI(val);
                } catch (URISyntaxException use) {}
            }
        }
        
        return rv;
    }
    
    /** run from any thread */
    public void viewStartupItems(Timer timer) { 
        timer.addEvent("begin view startup items");
        boolean rebuild = false;
        boolean scheduled = false;
        synchronized (_nymRefs) {
            if (!_rebuilding && (_nymRefs.size() <= 0)) {
                rebuild = true;
            }
            _viewOnStartup = true;
        }
        if (rebuild) {
            rebuildBookmarks();
            timer.addEvent("startup items: rebuild bookmarks fired");
        }
        
        SyndieURI prevTabs[] = getPrevTabs();
        // always give them something
        if (prevTabs == null || prevTabs.length == 0) {
            timer.addEvent("startup items: viewing default tab");
            _navControl.view(_uriControl.createSyndicationArchiveURI());
        } else {
            int count = 0;
            timer.addEvent("startup items: previous tabs identified");
            for (int i = 0; i < prevTabs.length; i++) {
                if (prevTabs[i] != null) {
                    viewStartupItem(prevTabs[i]);
                    timer.addEvent("startup items: previous tab opened: " + i + " " + prevTabs[i].toString());
                    if (++count >= MAX_PREV_TABS)
                        break;
                }
            }
            timer.addEvent("startup items: all previous tabs loaded");
        }       
    }

    protected void viewStartupItem(SyndieURI uri) { _navControl.view(uri); }

    // depth first traversal, so its the same each time, rather than using super._bookmarkNodes
    private int viewStartupItems(TreeItem item) {
        if (true) return 0; // dont use these, use the "last viewed" tabs
        if (item == null) {
            _ui.debugMessage("view startup items - no root? " + item);
            return 0;
        }
        int rv = 0;
        NymReferenceNode node = getBookmark(item);
        if (node == null)
            _ui.debugMessage("no node for the root? " + item);
        if ( (node != null) && node.getLoadOnStart()) {
            _ui.debugMessage("viewing node on startup: " + node.getURI());
            _navControl.view(node.getURI());
            rv++;
        }
        for (int i = 0; i < item.getItemCount(); i++)
            rv += viewStartupItems(item.getItem(i));
        return rv;
    }
    
    private void refetchNymChannels() {
        _nymChannels = _client.getNymChannels(); //_client.getChannels(true, true, true, true);
    }

    private void redrawManageable() {
        _manageRoot.removeAll();
        _manageChannels.clear();
        for (int i = 0; i < _nymChannels.getIdentityChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getIdentityChannel(i);
            TreeItem item = new TreeItem(_manageRoot, SWT.NONE);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText("ident: ") + info.getName());
            setChannelText(item, info);
            _manageChannels.put(item, info);
        }
        for (int i = 0; i < _nymChannels.getManagedChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getManagedChannel(i);
            TreeItem item = new TreeItem(_manageRoot, SWT.NONE);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText("manage: ") + info.getName());
            setChannelText(item, info);
            _manageChannels.put(item, info);
        }
    }

    private void redrawPostable() {
        _postRoot.removeAll();
        _postChannels.clear();
        for (int i = 0; i < _nymChannels.getIdentityChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getIdentityChannel(i);
            TreeItem item = new TreeItem(_postRoot, SWT.NONE);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText("ident: ") + info.getName());
            setChannelText(item, info);
            _postChannels.put(item, info);
        }
        for (int i = 0; i < _nymChannels.getManagedChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getManagedChannel(i);
            TreeItem item = new TreeItem(_postRoot, SWT.NONE);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText("manage: ") + info.getName());
            setChannelText(item, info);
            _postChannels.put(item, info);
        }
        for (int i = 0; i < _nymChannels.getPostChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getPostChannel(i);
            TreeItem item = new TreeItem(_postRoot, SWT.NONE);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText("post: ") + info.getName());
            setChannelText(item, info);
            _postChannels.put(item, info);
        }
        for (int i = 0; i < _nymChannels.getPublicPostChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getPublicPostChannel(i);
            TreeItem item = new TreeItem(_postRoot, SWT.NONE);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText("public: ") + info.getName());
            setChannelText(item, info);
            _postChannels.put(item, info);
        }
    }

    /**
     *  Make a nice name for the channel
     */
    private void setChannelText(TreeItem item, ChannelInfo info) {
        long id = info.getChannelId();
        StringBuilder buf = new StringBuilder();
        Font f;
        if (info.getPassphrasePrompt() != null) {
            buf.append(info.getName())
               .append(" (")
               .append(_translationRegistry.getText("Requires password"))
               .append(')');
            f = _themeRegistry.getTheme().MSG_UNKNOWN_FONT;
        } else if (info.getReadKeyUnknown()) {
            Hash hash = info.getChannelHash();
            buf.append('[')
               .append(hash.toBase64().substring(0, 6))
               .append("] (")
               .append(_translationRegistry.getText("Read key unknown"))
               .append(')');
            f = _themeRegistry.getTheme().MSG_UNKNOWN_FONT;
        } else {
            buf.append(info.getName());
            int un = _client.countUnreadMessages(id);
            int msgs = _client.countMessages(id);
            if (un > 0) {
                buf.append(" (").append(un).append('/').append(msgs).append(')');
                f = _themeRegistry.getTheme().MSG_NEW_UNREAD_FONT;
            } else if (msgs > 0) {
                buf.append(" (").append(msgs).append(')');
                f = _themeRegistry.getTheme().MSG_OLD_FONT;
            } else {
                f = _themeRegistry.getTheme().MSG_OLD_FONT;
            }
        }
        item.setText(buf.toString());
        item.setFont(f);
    }

    private void redrawBookmarks() {
        boolean rootWasOpen = _bookmarkRoot.getExpanded();
        long selectedGroupId = -1;
        ArrayList openGroupIds = new ArrayList();
        if (rootWasOpen) {
            for (int i = 0; i < _bookmarkRoot.getItemCount(); i++) {
                long id = getOpenGroupIds(_bookmarkRoot.getItem(i), openGroupIds);
                if (id >= 0)
                    selectedGroupId = id;
            }
        }
        
        _bookmarkRoot.removeAll();
        _bookmarkNodes.clear();
        TreeItem sel = null;
        for (int i = 0; i < _nymRefs.size(); i++) {
            NymReferenceNode ref = (NymReferenceNode)_nymRefs.get(i);
            _ui.debugMessage("redrawBookmarks: add root ref " + i + ": " + ref.getGroupId());
            TreeItem found = add(_bookmarkRoot, ref, selectedGroupId);
            if (found != null)
                sel = found;
        }
        
        _ui.debugMessage("redraw bookmarks: open groupIds: " + openGroupIds + " rootOpen: " + rootWasOpen);
        if (rootWasOpen) {
            _bookmarkRoot.setExpanded(true);
            ArrayList pending = new ArrayList();
            TreeItem cur = _bookmarkRoot;
            while (cur != null) {
                boolean includeChildren = false;
                if (cur != _bookmarkRoot) {
                    NymReferenceNode curNode = (NymReferenceNode)_bookmarkNodes.get(cur);
                    if ( (curNode != null) && (openGroupIds.contains(new Long(curNode.getGroupId()))) ) {
                        cur.setExpanded(true);
                        includeChildren = true;
                    }
                } else {
                    includeChildren = true; 
                }
                if (includeChildren) {
                    for (int i = 0; i < cur.getItemCount(); i++)
                        pending.add(cur.getItem(i));
                }
                if (pending.size() > 0)
                    cur = (TreeItem)pending.remove(0);
                else
                    cur = null;
            }
        }
        
        if (sel != null) {
            _tree.showItem(sel);
            _tree.setSelection(sel);
        }
    }
    private TreeItem add(TreeItem parent, NymReferenceNode child, long wantedGroupId) {
        _ui.debugMessage("redrawBookmarks: parent = " + parent.getText() + ", child = " + child.getGroupId() + "/" + child.getParentGroupId() + "/" + child.getName() + "/" + child.getChildCount());
        TreeItem childItem = new TreeItem(parent, SWT.NONE);
        if (_chooseAllStartupItems && child.getLoadOnStart()) {
            if (_choiceListener != null)
                _choiceListener.bookmarkSelected(childItem, child);
        }
        String desc = null; //child.getDescription();
        if ( (desc != null) && (desc.trim().length() > 0) )
            childItem.setText(child.getName() + "-" + child.getDescription());
        else
            childItem.setText(child.getName());
        childItem.setImage(ImageUtil.getTypeIcon(child.getURI()));
        _bookmarkNodes.put(childItem, child);
        TreeItem rv = null;
        if (child.getGroupId() == wantedGroupId)
            rv = childItem;
        for (int i = 0; i < child.getChildCount(); i++) {
            NymReferenceNode sub = (NymReferenceNode)child.getChild(i);
            _ui.debugMessage("redrawBookmarks: add child ref " + i + " of " + child.getGroupId() +": " + sub.getGroupId());
            TreeItem val = add(childItem, sub, wantedGroupId);
            if (val != null)
                rv = val;
        }
        return rv;
    }
    private void redrawSearchResults(boolean forceExpand) {
        _searchList.setRedraw(false);
        _searchList.removeAll();
        for (int i = 0; i < _searchResults.size(); i++) {
            ReferenceNode node = (ReferenceNode)_searchResults.get(i);
            String str = null;
            if ( (node.getName() != null) && (node.getName().trim().length() > 0) )
                str = node.getName();
            else if ( (node.getDescription() != null) && (node.getDescription().trim().length() > 0) )
                str = node.getDescription();
            else if (node.getURI().getScope() != null)
                str = node.getURI().getScope().toBase64().substring(0,6);
            else
                str = "?";
            _searchList.add(str);
        }
        _searchList.setRedraw(true);
    }
    /*
    private void add(TreeItem parent, ReferenceNode child) {
        TreeItem childItem = new TreeItem(parent, SWT.NONE);
        if (child.getDescription() != null)
            childItem.setText(child.getDescription());
        else 
            childItem.setText(child.getName());
        childItem.setImage(ImageUtil.getTypeIcon(child.getURI()));
        _searchNodes.put(childItem, child);
        for (int i = 0; i < child.getChildCount(); i++)
            add(childItem, child.getChild(i));
    }
     */

    /** tell the listener about each selected item */
    private void fireSelectionEvents() {
        ChoiceListener lsnr = _choiceListener;
        if (lsnr == null) return;
        
        TreeItem items[] = _tree.getSelection();
        if (items != null) {
            for (int i = 0; i < items.length; i++) {
                NymReferenceNode nref = (NymReferenceNode)_bookmarkNodes.get(items[i]);
                if (nref != null) {
                    lsnr.bookmarkSelected(items[i], nref);
                    continue;
                }
                ChannelInfo info = (ChannelInfo)_manageChannels.get(items[i]);
                if (info != null) {
                    lsnr.manageChannelSelected(items[i], info);
                    continue;
                }
                info = (ChannelInfo)_postChannels.get(items[i]);
                if (info != null) {
                    lsnr.postChannelSelected(items[i], info);
                    continue;
                }
                WatchedChannel watched = (WatchedChannel)_watchedItemToWatchedChannel.get(items[i]);
                if (watched != null) {
                    lsnr.watchedChannelSelected(items[i], watched);
                    continue;
                }
                // if reached, its one of the meta-entries (roots, etc)
                lsnr.otherSelected(items[i]);
            }
        }
    }
    
    public void watchesUpdated() { rebuildWatched(); }

    

    
    
    public void translate(TranslationRegistry registry) {
        _bookmarkRoot.setText(registry.getText("Bookmarked references"));
        _bookmarkRoot.setImage(ImageUtil.ICON_VM_BOOKMARK);
        _postRoot.setText(registry.getText("Writable forums"));
        _postRoot.setImage(ImageUtil.ICON_WRITEABLEFORUM);
        _manageRoot.setText(registry.getText("Manageable forums"));
        _manageRoot.setImage(ImageUtil.ICON_MANAGEABLEFORUM);
        _watchedRoot.setText(registry.getText("Watched forums"));
        _watchedRoot.setImage(ImageUtil.ICON_WATCHEDFORUM);
        _importedRoot.setText(registry.getText("Imported resources"));
        _importedRoot.setImage(ImageUtil.ICON_IMPORTEDRESOURCES);
        //_searchRoot.setText(registry.getText("Search results") + "...");
        //refreshBookmarks();
        //redrawPostable();
        //redrawManageable();
        //redrawSearchResults();
    }

    public void applyTheme(Theme theme) {
        // applies to items and columns
        _tree.setFont(theme.TREE_FONT);
    }
}
