package syndie.gui;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.events.TreeEvent;
import org.eclipse.swt.events.TreeListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.ChannelInfo;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.UI;

/**
 * The reference chooser tree has four roots:
 *  - the nym's bookmarks
 *  - the channels the nym can post to
 *  - the channels the nym can manage
 *  - search results
 */
public class ReferenceChooserTree implements Translatable, Themeable {
    private BrowserControl _browser;
    private DBClient _client;
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
    
    private Composite _root;
    private Tree _tree;
    private TreeItem _bookmarkRoot;
    /** map of TreeItem to NymReferenceNode */
    private Map _bookmarkNodes;
    private TreeItem _postRoot;
    /** map of TreeItem to ChannelInfo */
    private Map _postChannels;
    private TreeItem _manageRoot;
    /** map of TreeItem to ChannelInfo */
    private Map _manageChannels;
    private TreeItem _searchRoot;
    /** map of TreeItem to ReferenceNode */
    private Map _searchNodes;

    private ChoiceListener _choiceListener;
    private AcceptanceListener _acceptanceListener;
    private boolean _chooseAllStartupItems;
    private UI _ui;
    
    public ReferenceChooserTree(BrowserControl control, Composite parent, ChoiceListener lsnr, AcceptanceListener accept) {
        this(control, parent, lsnr, accept, false);
    }
    public ReferenceChooserTree(BrowserControl control, Composite parent, ChoiceListener lsnr, AcceptanceListener accept, boolean chooseAllStartupItems) {
        _ui = control.getUI();
        _client = control.getClient();
        _browser = control;
        _parent = parent;
        _choiceListener = lsnr;
        _acceptanceListener = accept;
        _chooseAllStartupItems = chooseAllStartupItems;
        _nymRefs = new ArrayList();
        _bookmarkNodes = new HashMap();
        _postChannels = new HashMap();
        _manageChannels = new HashMap();
        _searchNodes = new HashMap();
        _searchResults = new ArrayList();
        initComponents(true);
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
    protected BrowserControl getBrowser() { return _browser; }
    
    public interface ChoiceListener {
        public void bookmarkSelected(TreeItem item, NymReferenceNode node);
        public void manageChannelSelected(TreeItem item, ChannelInfo channel);
        public void postChannelSelected(TreeItem item, ChannelInfo channel);
        public void searchResultSelected(TreeItem item, ReferenceNode node);
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
    
    private void getOpenGroupIds(TreeItem base, ArrayList openGroupIds) {
        if (base.getExpanded()) {
            openGroupIds.add(new Long((((NymReferenceNode)_bookmarkNodes.get(base)).getGroupId())));
            for (int i = 0; i < base.getItemCount(); i++)
                getOpenGroupIds(base.getItem(i), openGroupIds);
        }
    }
    
    protected void initComponents(boolean register) {
        long t1 = System.currentTimeMillis();
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(1, true));
        _tree = new Tree(_root, SWT.BORDER | SWT.SINGLE);
        _tree.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _bookmarkRoot = new TreeItem(_tree, SWT.NONE);
        _postRoot = new TreeItem(_tree, SWT.NONE);
        _manageRoot = new TreeItem(_tree, SWT.NONE);
        _searchRoot = new TreeItem(_tree, SWT.NONE);
        
        long t2 = System.currentTimeMillis();
        configTreeListeners(_tree);
        long t3 = System.currentTimeMillis();
        
        //_browser.getUI().debugMessage("init refChooserTree", new Exception("source"));
        rebuildBookmarks();
        long t4 = System.currentTimeMillis();
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
        long t8 = System.currentTimeMillis();
        
        _chooseAllStartupItems = false;
        if (register) {
            _browser.getTranslationRegistry().register(this);
            _browser.getThemeRegistry().register(this);
        }
        long t9 = System.currentTimeMillis();
        System.out.println("tree init: " + (t2-t1)+"/"+(t3-t2)+"/"+(t4-t3)+"/"+(t8-t4)+"/"+(t9-t8));
    }
    
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
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
    protected ReferenceNode getSearchResult(TreeItem item) { return (ReferenceNode)_searchNodes.get(item); }
    
    protected TreeItem getSearchRoot() { return _searchRoot; }
    protected TreeItem getManageRoot() { return _manageRoot; }
    protected TreeItem getPostRoot() { return _postRoot; }
    protected TreeItem getBookmarkRoot() { return _bookmarkRoot; }
    
    protected void bookmarksRebuilt(List nymRefs) {}
    
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
            _browser.getUI().debugMessage("rebuilder started");
            final List refs = _client.getNymReferences(_client.getLoggedInNymId());
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
                    if (view)
                        viewStartupItems(getBookmarkRoot());
                    _tree.setRedraw(true);
                    long t6 = System.currentTimeMillis();
                    _ui.debugMessage("redraw after rebuild: " + (t6-t1) + " view? " + view + " " + (t2-t1)+"/"+(t3-t2)+"/"+(t4-t3)+"/"+(t5-t4)+"/"+(t6-t5));
                }
            });
        }
    }
    
    /** run from any thread */
    public void viewStartupItems() { 
        boolean rebuild = false;
        boolean scheduled = false;
        synchronized (_nymRefs) {
            if (!_rebuilding) {
                rebuild = true;
            }
            _viewOnStartup = true;
        }
        if (rebuild)
            rebuildBookmarks();
    }
    // depth first traversal, so its the same each time, rather than using super._bookmarkNodes
    private void viewStartupItems(TreeItem item) {
        if (item == null) {
            _ui.debugMessage("view startup items - no root? " + item);
            return;
        }
        NymReferenceNode node = getBookmark(item);
        if (node == null)
            _ui.debugMessage("no node for the root? " + item);
        if ( (node != null) && node.getLoadOnStart()) {
            _ui.debugMessage("viewing node on startup: " + node.getURI());
            getBrowser().view(node.getURI());
        }
        for (int i = 0; i < item.getItemCount(); i++)
            viewStartupItems(item.getItem(i));
    }
    
    private void refetchNymChannels() {
        _nymChannels = _client.getChannels(true, true, true, true);
    }
    private void redrawManageable() {
        _manageRoot.removeAll();
        _manageChannels.clear();
        for (int i = 0; i < _nymChannels.getIdentityChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getIdentityChannel(i);
            TreeItem item = new TreeItem(_manageRoot, SWT.NONE);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText(T_MANAGE_IDENT_PREFIX, "ident: ") + info.getName());
            item.setText(info.getName());
            _manageChannels.put(item, info);
        }
        for (int i = 0; i < _nymChannels.getManagedChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getManagedChannel(i);
            TreeItem item = new TreeItem(_manageRoot, SWT.NONE);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText(T_MANAGE_PREFIX, "manage: ") + info.getName());
            item.setText(info.getName());
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
            //item.setText(_browser.getTranslationRegistry().getText(T_POST_IDENT_PREFIX, "ident: ") + info.getName());
            item.setText(info.getName());
            _postChannels.put(item, info);
        }
        for (int i = 0; i < _nymChannels.getManagedChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getManagedChannel(i);
            TreeItem item = new TreeItem(_postRoot, SWT.NONE);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText(T_POST_MANAGE_PREFIX, "manage: ") + info.getName());
            item.setText(info.getName());
            _postChannels.put(item, info);
        }
        for (int i = 0; i < _nymChannels.getPostChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getPostChannel(i);
            TreeItem item = new TreeItem(_postRoot, SWT.NONE);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText(T_POST_PREFIX, "post: ") + info.getName());
            item.setText(info.getName());
            _postChannels.put(item, info);
        }
        for (int i = 0; i < _nymChannels.getPublicPostChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getPublicPostChannel(i);
            TreeItem item = new TreeItem(_postRoot, SWT.NONE);
            item.setImage(ImageUtil.getTypeIcon(SyndieURI.createScope(info.getChannelHash())));
            //item.setText(_browser.getTranslationRegistry().getText(T_POST_PUBLIC_PREFIX, "public: ") + info.getName());
            item.setText(info.getName());
            _postChannels.put(item, info);
        }
    }
    private void redrawBookmarks() {
        _bookmarkRoot.removeAll();
        _bookmarkNodes.clear();
        for (int i = 0; i < _nymRefs.size(); i++) {
            NymReferenceNode ref = (NymReferenceNode)_nymRefs.get(i);
            _ui.debugMessage("redrawBookmarks: add root ref " + i + ": " + ref.getGroupId());
            add(_bookmarkRoot, ref);
        }
    }
    private void add(TreeItem parent, NymReferenceNode child) {
        _ui.debugMessage("redrawBookmarks: parent = " + parent.getText() + ", child = " + child.getGroupId() + "/" + child.getParentGroupId() + "/" + child.getName() + "/" + child.getChildCount());
        TreeItem childItem = new TreeItem(parent, SWT.NONE);
        if (_chooseAllStartupItems && child.getLoadOnStart()) {
            if (_choiceListener != null)
                _choiceListener.bookmarkSelected(childItem, child);
        }
        String desc = child.getDescription();
        if ( (desc != null) && (desc.trim().length() > 0) )
            childItem.setText(child.getName() + "-" + child.getDescription());
        else
            childItem.setText(child.getName());
        childItem.setImage(ImageUtil.getTypeIcon(child.getURI()));
        _bookmarkNodes.put(childItem, child);
        for (int i = 0; i < child.getChildCount(); i++) {
            NymReferenceNode sub = (NymReferenceNode)child.getChild(i);
            _ui.debugMessage("redrawBookmarks: add child ref " + i + " of " + child.getGroupId() +": " + sub.getGroupId());
            add(childItem, sub);
        }
    }
    private void redrawSearchResults(boolean forceExpand) {
        _searchRoot.removeAll();
        _searchNodes.clear();
        for (int i = 0; i < _searchResults.size(); i++) {
            ReferenceNode node = (ReferenceNode)_searchResults.get(i);
            add(_searchRoot, node);
        }
        if (forceExpand)
            _searchRoot.setExpanded(true);
    }
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
                ReferenceNode ref = (ReferenceNode)_searchNodes.get(items[i]);
                if (ref != null) {
                    lsnr.searchResultSelected(items[i], ref);
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
                // if reached, its one of the meta-entries (roots, etc)
                lsnr.otherSelected(items[i]);
            }
        }
    }

    private static final String T_BOOKMARK_ROOT = "syndie.gui.refchoosertree.bookmarkroot";
    private static final String T_POST_ROOT = "syndie.gui.refchoosertree.postroot";
    private static final String T_MANAGE_ROOT = "syndie.gui.refchoosertree.manageroot";
    private static final String T_SEARCH_ROOT = "syndie.gui.refchoosertree.searchroot";
    
    private static final String T_MANAGE_IDENT_PREFIX = "syndie.gui.refchoosertree.manage.identprefix";
    private static final String T_MANAGE_PREFIX = "syndie.gui.refchoosertree.manage.prefix";

    private static final String T_POST_IDENT_PREFIX = "syndie.gui.refchoosertree.post.identprefix";
    private static final String T_POST_MANAGE_PREFIX = "syndie.gui.refchoosertree.post.manageprefix";
    private static final String T_POST_PREFIX = "syndie.gui.refchoosertree.post.prefix";
    private static final String T_POST_PUBLIC_PREFIX = "syndie.gui.refchoosertree.post.publicprefix";
    
    public void translate(TranslationRegistry registry) {
        _bookmarkRoot.setText(registry.getText(T_BOOKMARK_ROOT, "Bookmarked references"));
        _postRoot.setText(registry.getText(T_POST_ROOT, "Writable forums"));
        _manageRoot.setText(registry.getText(T_MANAGE_ROOT, "Manageable forums"));
        _searchRoot.setText(registry.getText(T_SEARCH_ROOT, "Search results..."));
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
