package syndie.gui;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import net.i2p.data.Base64;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TreeEditor;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.DropTargetListener;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.MenuListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.ScrollBar;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.ChannelInfo;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.Timer;
import syndie.data.WatchedChannel;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 *
 */
class BrowserTree extends ReferenceChooserTree implements Translatable, Themeable {
    private Browser _browserInstance;
    private BookmarkControl _bookmarkControl;
    private Menu _menu;
    
    private Button _searchAdvanced;
    
    private BookmarkEditorPopup _bookmarkEditor;
    private ReferenceChooserSearch _searchDetail;
    private Shell _searchDetailPopup;

    private long _startInit;
    private long _superInit;
    
    private List _nymRefs;
    
    public BrowserTree(Browser browser, DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl nav, URIControl uriControl, BookmarkControl bookmarkControl, Composite parent, ChoiceListener lsnr, AcceptanceListener accept, Timer timer) {
        super(client, ui, themes, trans, nav, uriControl, parent, lsnr, accept, false, false, timer);
        _browserInstance = browser;
        _bookmarkControl = bookmarkControl;
        long t1 = System.currentTimeMillis();
        //_bookmarkEditor = new BookmarkEditorPopup(browser, parent.getShell());
        long t2 = System.currentTimeMillis();
        //System.out.println("tree curInit: " + (t1-_superInit) + ", superInit:" + (_superInit-_startInit) + " editor init: " + (t2-t1));
        if (_nymRefs != null)
            _browserInstance.bookmarksUpdated(_nymRefs);
        _nymRefs = null;
    }
    private BookmarkEditorPopup getBookmarkEditor() {
        // only called by the swt thread, so no need to sync
        if (_bookmarkEditor == null)
            _bookmarkEditor = ComponentBuilder.instance().createBookmarkEditorPopup(getControl().getShell());
        return _bookmarkEditor;
    }
    
    protected void initComponents(boolean register, boolean multi, Timer timer) {
        _startInit = System.currentTimeMillis();
        super.initComponents(false, true, timer);
        _superInit = System.currentTimeMillis();
        
        _searchAdvanced = new Button((Composite)getControl(), SWT.PUSH);
        _searchAdvanced.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _searchAdvanced.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { searchAdvanced(); }
            public void widgetSelected(SelectionEvent selectionEvent) { searchAdvanced(); }
        });
        
        //createSearchDetailPopup();
        
        timer.addEvent("browsertree init: search");
        initDnD();
        timer.addEvent("browsertree init: dnd");
        
        getTree().addMouseTrackListener(new MouseTrackListener() {
            public void mouseEnter(MouseEvent mouseEvent) {}
            public void mouseExit(MouseEvent mouseEvent) {}
            public void mouseHover(MouseEvent evt) {
                TreeItem item = getTree().getItem(new Point(evt.x, evt.y));
                if (item != null) {
                    NymReferenceNode bookmark = getBookmark(item);
                    if (bookmark != null) {
                        if (bookmark.getDescription() != null)
                            getTree().setToolTipText(bookmark.getDescription());
                        else if (bookmark.getName() != null)
                            getTree().setToolTipText(bookmark.getName());
                        else
                            getTree().setToolTipText("");
                        return;
                    }
                    ChannelInfo chan = getPostChannel(item);
                    if (chan != null) {
                        if (chan.getDescription() != null)
                            getTree().setToolTipText(chan.getDescription());
                        else if (chan.getName() != null)
                            getTree().setToolTipText(chan.getName());
                        else
                            getTree().setToolTipText("");
                        return;
                    }
                    chan = getManageChannel(item);
                    if (chan != null) {
                        if (chan.getDescription() != null)
                            getTree().setToolTipText(chan.getDescription());
                        else if (chan.getName() != null)
                            getTree().setToolTipText(chan.getName());
                        else
                            getTree().setToolTipText("");
                        return;
                    }
                    WatchedChannel watched = getWatchedChannel(item);
                    if (watched != null) {
                        Hash scope = _client.getChannelHash(watched.getChannelId());
                        String name = _client.getChannelName(watched.getChannelId());
                        if (name == null)
                            name = scope.toBase64();
                        getTree().setToolTipText(name);
                        return;
                    }
                    
                    getTree().setToolTipText("");
                }
            }
        });
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
        timer.addEvent("browsertree init: register");
    }
    
    // we don't override dispose here, since its never called - the browsertree is never destroyed
    //public void dispose() { super.dispose(); }
    
    private void search() {
        if (_searchDetail == null)
            createSearchDetailPopup();
        _searchDetail.search();
        _searchDetailPopup.setVisible(false);
    }
    private static final String T_SEARCH_FORUM_TITLE = "syndie.gui.browsertree.searchforumtitle";
    private void searchAdvanced() { 
        final ReferenceChooserPopup popup = ComponentBuilder.instance().createReferenceChooserPopup(getControl().getShell(), T_SEARCH_FORUM_TITLE, "Forum search");
        popup.setListener(new ReferenceChooserTree.AcceptanceListener() {
            public void referenceAccepted(SyndieURI uri) { _navControl.view(uri); }
            public void referenceChoiceAborted() { popup.dispose(); }
        });
        popup.show();
        /*
        if (_searchDetail == null)
            createSearchDetailPopup();
        _searchDetailPopup.open(); 
         */
    }
    
    public void setSearchResults(Collection resultNodes) {
        super.setSearchResults(resultNodes);
        if (_searchDetail != null)
            _searchDetailPopup.setVisible(false);
    }
    protected void viewStartupItem(final SyndieURI uri) { 
        _browserInstance.runAfterStartup(new Runnable() { public void run() { _navControl.view(uri); } });
    }
    
    private void createSearchDetailPopup() {
        _searchDetailPopup = new Shell(_searchAdvanced.getShell(), SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        _searchDetailPopup.setLayout(new FillLayout());
        _searchDetail = new ReferenceChooserSearch(_client, _ui, _themeRegistry, _translationRegistry, _searchDetailPopup, this);
        _searchDetailPopup.setSize(_searchDetailPopup.computeSize(300, SWT.DEFAULT));
        _searchDetailPopup.setText(_translationRegistry.getText(T_SEARCH_DETAIL_POPUP, "Search"));
        _searchDetailPopup.setFont(_themeRegistry.getTheme().SHELL_FONT);
        
        // intercept the shell closing, since that'd cause the shell to be disposed rather than just hidden
        _searchDetailPopup.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; _searchDetailPopup.setVisible(false); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
    }
    
    protected void configTreeListeners(final Tree tree) {
        BrowserTreeListener lsnr = new BrowserTreeListener(tree);
        tree.addKeyListener(lsnr);
        tree.addTraverseListener(lsnr);
        tree.addSelectionListener(lsnr);
        tree.addControlListener(lsnr);
        tree.addMouseListener(lsnr);

        _menu = new Menu(tree);
        _menu.addMenuListener(new MenuListener() {
            public void menuHidden(MenuEvent menuEvent) {}
            public void menuShown(MenuEvent evt) { buildMenu(); }
        });
        tree.setMenu(_menu);
    }
    
    private void buildMenu() {
        MenuItem items[] = _menu.getItems();
        for (int i = 0; i < items.length; i++) items[i].dispose();
        
        
        TreeItem selected[] = getTree().getSelection();
        if ( (selected == null) || (selected.length < 1) ) { 
            // noop
        } else if (selected.length == 1) {
            TreeItem item = selected[0];

            if (item == getBookmarkRoot()) {
                buildBookmarkMenu(null, item);
                return;
            }
            
            NymReferenceNode bookmark = getBookmark(item);
            if ( (bookmark != null) || (item.equals(getBookmarkRoot())) ) {
                buildBookmarkMenu(bookmark, item);
                return;
            }
            ChannelInfo chan = getPostChannel(item);
            if ( (chan != null) || (item.equals(getPostRoot())) ) {
                buildPostMenu(chan);
                return;
            }
            chan = getManageChannel(item);
            if ( (chan != null) || (item.equals(getManageRoot())) ) {
                buildManageMenu(chan);
                return;
            }
            WatchedChannel watched = getWatchedChannel(item);
            if (watched != null) {
                buildWatchedMenu(watched);
                return;
            }
        } else {
            final List toView = new ArrayList();
            final List watched = new ArrayList();
            final List bookmarkedGroupIds = new ArrayList();
            boolean otherIncluded = false;
            for (int i = 0; i < selected.length; i++) {
                NymReferenceNode bookmark = getBookmark(selected[i]);
                if ( (bookmark != null) && (bookmark.getURI() != null) ) {
                    toView.add(bookmark.getURI());
                    bookmarkedGroupIds.add(new Long(bookmark.getGroupId()));
                } else {
                    ChannelInfo chan = getPostChannel(selected[i]);
                    if (chan == null)
                        chan = getManageChannel(selected[i]);
                    if (chan != null) {
                        otherIncluded = true;
                        toView.add(SyndieURI.createScope(chan.getChannelHash()));
                    } else {
                        WatchedChannel cur = getWatchedChannel(selected[i]);
                        if (watched != null) {
                            watched.add(cur);
                            toView.add(_client.getChannelHash(cur.getChannelId()));
                        }
                    }
                }
            }
            MenuItem mitem = new MenuItem(_menu, SWT.PUSH);
            mitem.setText(_translationRegistry.getText(T_VIEW_ALL, "View all selected"));
            mitem.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    for (int i = 0; i < toView.size(); i++)
                        _navControl.view((SyndieURI)toView.get(i));
                }
            });
            if ( (watched.size() > 0) && (bookmarkedGroupIds.size() == 0) && !otherIncluded) {
                mitem = new MenuItem(_menu, SWT.PUSH);
                mitem.setText(_translationRegistry.getText(T_UNWATCH_ALL, "Unwatch all selected"));
                mitem.addSelectionListener(new FireSelectionListener() {
                    public void fire() {
                        WatchedChannel channels[] = new WatchedChannel[watched.size()];
                        for (int i = 0; i < channels.length; i++)
                            channels[i] = (WatchedChannel)watched.get(i);
                        _client.unwatchChannels(channels);
                    }
                });   
            } else if ( (watched.size() == 0) && (bookmarkedGroupIds.size() > 0) && !otherIncluded) {
                mitem = new MenuItem(_menu, SWT.PUSH);
                mitem.setText(_translationRegistry.getText(T_UNBOOKMARK_ALL, "Unbookmark all selected"));
                mitem.addSelectionListener(new FireSelectionListener() {
                    public void fire() {
                        MessageBox box = new MessageBox(getControl().getShell(), SWT.ICON_WARNING | SWT.YES | SWT.NO);
                        box.setMessage(_translationRegistry.getText(T_CONFIRM_DELETEALL_MESSAGE, "Are you sure you want to delete these bookmarks?"));
                        box.setText(_translationRegistry.getText(T_CONFIRM_DELETEALL_TITLE, "Confirm"));
                        int rc = box.open();
                        if (rc == SWT.YES)
                            _bookmarkControl.deleteBookmarks(bookmarkedGroupIds);
                    }
                });   
            }
        }
    }
    
    private static final String T_VIEW_ALL = "syndie.gui.browsertree.viewall";
    private static final String T_UNWATCH_ALL = "syndie.gui.browsertree.unwatchall";
    private static final String T_UNBOOKMARK_ALL = "syndie.gui.browsertree.unbookmarkall";
    private static final String T_CONFIRM_DELETEALL_TITLE = "syndie.gui.confirmdeleteall.title";
    private static final String T_CONFIRM_DELETEALL_MESSAGE = "syndie.gui.confirmdeleteall.message";
    
    
    private void buildManageMenu(final ChannelInfo chan) {
        if (chan != null) {
            MenuItem item = new MenuItem(_menu, SWT.PUSH);
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent evt) { _navControl.view(_uriControl.createManageURI(chan.getChannelHash())); }
                public void widgetSelected(SelectionEvent evt) { _navControl.view(_uriControl.createManageURI(chan.getChannelHash())); }
            });
            item.setText(_translationRegistry.getText(T_MANAGE_TITLE, "Manage"));
        }
    }
    private void buildPostMenu(final ChannelInfo chan) {
        if (chan != null) {
            MenuItem item = new MenuItem(_menu, SWT.PUSH);
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent evt) { _navControl.view(_uriControl.createPostURI(chan.getChannelHash(), null, false)); }
                public void widgetSelected(SelectionEvent evt) { _navControl.view(_uriControl.createPostURI(chan.getChannelHash(), null, false)); }
            });        
            item.setText(_translationRegistry.getText(T_POST_TITLE, "Post"));
        }
    }
    private void buildBookmarkMenu(final NymReferenceNode bookmark, final TreeItem selected) {
        MenuItem item = null;

        if (bookmark == null) {
            item = new MenuItem(_menu, SWT.PUSH);
            item.addSelectionListener(new FireSelectionListener() {
                public void fire() { addBookmark(-1); }
            });
            item.setText(_translationRegistry.getText(T_BOOKMARK_ADD, "Add bookmark"));

            item = new MenuItem(_menu, SWT.PUSH);
            item.addSelectionListener(new FireSelectionListener() {
                public void fire() { addFolder(-1); }
            });
            item.setText(_translationRegistry.getText(T_BOOKMARK_ADDFOLDER, "Add folder"));
            return;
        }

        
        if (bookmark.getURI() != null) {
            item = new MenuItem(_menu, SWT.PUSH);
            item.addSelectionListener(new FireSelectionListener() {
                public void fire() { _navControl.view(bookmark.getURI(), bookmark.getName(), bookmark.getDescription()); }
            });
            item.setText(_translationRegistry.getText(T_BOOKMARK_VIEW, "View selected"));
        } else if (bookmark.getChildCount() > 0) {
            item = new MenuItem(_menu, SWT.PUSH);
            item.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    for (int i = 0; i < bookmark.getChildCount(); i++) {
                        ReferenceNode child = bookmark.getChild(i);
                        SyndieURI uri = child.getURI();
                        if (uri != null)
                            _navControl.view(uri, child.getName(), child.getDescription());
                    }
                }
            });
            item.setText(_translationRegistry.getText(T_BOOKMARK_VIEWALL, "View all children"));
        }
        
        item = new MenuItem(_menu, SWT.PUSH);
        item.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { editBookmark(selected); }
            public void widgetSelected(SelectionEvent evt) { editBookmark(selected); }
        });
        item.setText(_translationRegistry.getText(T_BOOKMARK_EDIT, "Edit"));
        
        item = new MenuItem(_menu, SWT.PUSH);
        item.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { deleteBookmark(selected); }
            public void widgetSelected(SelectionEvent evt) { deleteBookmark(selected); }
        });
        item.setText(_translationRegistry.getText(T_BOOKMARK_DELETE, "Delete"));
        
        if (bookmark.getURI() == null) {
            item = new MenuItem(_menu, SWT.PUSH);
            item.addSelectionListener(new FireSelectionListener() {
                public void fire() { addBookmark(bookmark.getGroupId()); }
            });
            item.setText(_translationRegistry.getText(T_BOOKMARK_ADD, "Add bookmark"));

            item = new MenuItem(_menu, SWT.PUSH);
            item.addSelectionListener(new FireSelectionListener() {
                public void fire() { addFolder(bookmark.getGroupId()); }
            });
            item.setText(_translationRegistry.getText(T_BOOKMARK_ADDFOLDER, "Add folder"));
        }
    }

    private SyndieURI getBookmarkURI(TreeItem item) {
        NymReferenceNode node = getBookmark(item);
        if (node != null)
            return node.getURI();
        else
            return null;
    }
    
    private void editBookmark(final TreeItem item) {
        final NymReferenceNode node = getBookmark(item);
        if (node != null) {
            if (node.getURI() == null) {
                // folder, so edit it in-place
                final TreeEditor ed = new TreeEditor(getTree());
                ed.grabHorizontal = true;
                ed.horizontalAlignment = SWT.LEFT;
                ed.minimumWidth = 100;
                
                final Text field = new Text(getTree(), SWT.SINGLE);
                field.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
                field.setText((node.getName() != null ? node.getName() : ""));
                field.addTraverseListener(new TraverseListener() {
                    public void keyTraversed(TraverseEvent evt) {
                        switch (evt.detail) {
                            case SWT.TRAVERSE_RETURN:
                            case SWT.TRAVERSE_TAB_NEXT:
                            case SWT.TRAVERSE_TAB_PREVIOUS:
                                node.setName(field.getText().trim());
                                _bookmarkControl.updateBookmark(node);
                                field.dispose();
                                ed.dispose();
                                return;
                            case SWT.TRAVERSE_ESCAPE:
                                field.dispose();
                                ed.dispose();
                                return;
                        }
                    }
                });
                field.addFocusListener(new FocusListener() {
                    public void focusGained(FocusEvent focusEvent) {}
                    public void focusLost(FocusEvent focusEvent) {
                        if (!field.isDisposed()) {
                            node.setName(field.getText().trim());
                            _bookmarkControl.updateBookmark(node);
                            field.dispose();
                        }
                        ed.dispose();
                    }
                });
                field.selectAll();
                field.setFocus();
                ed.setEditor(field, item);
            } else {
                BookmarkEditorPopup popup = getBookmarkEditor();
                popup.setBookmark(node);
                popup.pickParent(false);
                popup.pickOrder(false);
                popup.pickTarget(true);
                popup.open();
            }
        }
    }
    
    private void addBookmark(long parentGroupId) {
        _ui.debugMessage("addBookmark underneath " + parentGroupId);
        BookmarkEditorPopup popup = getBookmarkEditor();
        popup.pickParent(false);
        popup.pickOrder(false);
        popup.pickTarget(true);
        popup.setBookmark(new NymReferenceNode("", null, "", -1, -1, parentGroupId, -1, false, false, false));
        popup.open();
    }
    
    private void addFolder(long parentGroupId) {
        _ui.debugMessage("addFolder underneath " + parentGroupId);
        String prefix = _translationRegistry.getText(T_NEWFOLDER_NAME, "New folder") + " ";
        String folderName = getFolderName(prefix, parentGroupId);
        NymReferenceNode node = new NymReferenceNode(folderName, null, "", -1, -1, parentGroupId, -1, false, false, false);
        _bookmarkControl.bookmark(node, true);
    }
    private static final String T_NEWFOLDER_NAME = "syndie.gui.browsertree.newfoldername";
    
    private String getFolderName(String prefix, long parentGroupId) {
        if (_nymRefs == null) return prefix + 1;
        ReferenceNode parent = null;
        if (parentGroupId >= 0)
            parent = getParent(parentGroupId);
        if (parent == null) {
            int idx = 1;
            while (true) {
                boolean found = false;
                for (int i = 0; i < _nymRefs.size(); i++) {
                    ReferenceNode node = (ReferenceNode)_nymRefs.get(i);
                    String name = prefix + idx;
                    if (name.equals(node.getName())) {
                        found = true;
                        break;
                    }
                }
                if (!found)
                    return prefix + idx;
                else
                    idx++;
            }
        } else {
            int idx = 1;
            while (true) {
                boolean found = false;
                for (int i = 0; i < parent.getChildCount(); i++) {
                    ReferenceNode node = parent.getChild(i);
                    String name = prefix + idx;
                    if (name.equals(node.getName())) {
                        found = true;
                        break;
                    }
                }
                if (!found)
                    return prefix + idx;
                else
                    idx++;
            }
        }
    }
    private ReferenceNode getParent(long parentGroupId) {
        if (_nymRefs == null) return null;
        for (int i = 0; i < _nymRefs.size(); i++) {
            ReferenceNode node = (ReferenceNode)_nymRefs.get(i);
            ReferenceNode found = node.getByUniqueId(parentGroupId);
            if (found != null)
                return found;
        }
        return null;
    }
    
    private void deleteBookmark(TreeItem item) {
        NymReferenceNode node = getBookmark(item);
        if (node != null) {
            MessageBox box = new MessageBox(getControl().getShell(), SWT.ICON_WARNING | SWT.YES | SWT.NO);
            box.setMessage(_translationRegistry.getText(T_CONFIRM_DELETE_MESSAGE, "Are you sure you want to delete this bookmark?"));
            box.setText(_translationRegistry.getText(T_CONFIRM_DELETE_TITLE, "Confirm"));
            int rc = box.open();
            if (rc == SWT.YES) {
                _bookmarkControl.deleteBookmark(node.getGroupId());
            }
        }
    }

    protected void deleteSelected() { deleteBookmark(getSelectedItem()); }
    
    private Hash getPostScope(TreeItem item) {
        ChannelInfo chan = getPostChannel(item);
        if (chan != null)
            return chan.getChannelHash();
        else
            return null;
    }
    
    private Hash getManageScope(TreeItem item) {
        ChannelInfo chan = getManageChannel(item);
        if (chan != null)
            return chan.getChannelHash();
        else
            return null;
    }
    
    public void saveBookmarks() {
        ReferenceNode.walk(_nymRefs, new Renumberer());
        _client.setNymReferences(_nymRefs);
        refreshBookmarks();
    }
    
    /**
     * Update the groupId and ordering in the reference nodes based on their position from
     * a depth first traversal
     */
    private class Renumberer implements ReferenceNode.Visitor {
        private int _groupId;
        public Renumberer() { _groupId = 0; }
        public void visit(ReferenceNode node, int depth, int siblingOrder) {
            NymReferenceNode n = (NymReferenceNode)node;
            n.setGroupId(_groupId);
            _groupId++;
            NymReferenceNode parent = (NymReferenceNode)n.getParent();
            if (parent != null)
                parent.addChild(n); // handles the sibling order
            else
                n.setSiblingOrder(siblingOrder);
            _ui.debugMessage("renumbering bookmarks: " + n.getGroupId() + " under " + n.getParentGroupId() + " order " + n.getSiblingOrder() + ": " + n.getName());
        }
        
    }
    
    private static final String T_WATCH_VIEW = "syndie.gui.browsertree.watch.view";
    private static final String T_WATCH_HIGHLIGHT = "syndie.gui.browsertree.watch.highlight";
    private static final String T_WATCH_IMPBOOKMARKS = "syndie.gui.browsertree.watch.impbookmarks";
    private static final String T_WATCH_IMPARCHIVES = "syndie.gui.browsertree.watch.imparchives";
    private static final String T_WATCH_IMPKEYS = "syndie.gui.browsertree.watch.impkeys";
    private static final String T_WATCH_IMPBANS = "syndie.gui.browsertree.watch.impbans";
    private static final String T_WATCH_UNWATCH = "syndie.gui.browsertree.watch.unwatch";
    
    private void buildWatchedMenu(final WatchedChannel watched) {
        final Hash scope = _client.getChannelHash(watched.getChannelId());
        _ui.debugMessage("Building watch menu for " + scope + "/" + watched.getChannelId(), new Exception("source"));
        MenuItem item = new MenuItem(_menu, SWT.PUSH);
        item.setText(_translationRegistry.getText(T_WATCH_VIEW, "View the selected forum"));
        item.addSelectionListener(new FireSelectionListener() {
            public void fire() { _navControl.view(SyndieURI.createScope(scope)); }
        });
        new MenuItem(_menu, SWT.SEPARATOR);
        item = new MenuItem(_menu, SWT.CHECK);
        item.setText(_translationRegistry.getText(T_WATCH_HIGHLIGHT, "Highlight the forum's unread messages"));
        item.setSelection(watched.getHighlight());
        item.addSelectionListener(new FireSelectionListener() {
            public void fire() { 
                _client.watchChannel(watched.getChannelId(), !watched.getHighlight(), watched.getImportArchives(), watched.getImportBookmarks(), watched.getImportBans(), watched.getImportKeys());
            }
        });

        item = new MenuItem(_menu, SWT.CHECK);
        item.setText(_translationRegistry.getText(T_WATCH_IMPBOOKMARKS, "Import their recommended bookmarks"));
        item.setSelection(watched.getImportBookmarks());
        item.addSelectionListener(new FireSelectionListener() {
            public void fire() { 
                _client.watchChannel(watched.getChannelId(), watched.getHighlight(), watched.getImportArchives(), !watched.getImportBookmarks(), watched.getImportBans(), watched.getImportKeys());
            }
        });

        item = new MenuItem(_menu, SWT.CHECK);
        item.setText(_translationRegistry.getText(T_WATCH_IMPARCHIVES, "Import their recommended archives"));
        item.setSelection(watched.getImportArchives());
        item.addSelectionListener(new FireSelectionListener() {
            public void fire() { 
                _client.watchChannel(watched.getChannelId(), watched.getHighlight(), !watched.getImportArchives(), watched.getImportBookmarks(), watched.getImportBans(), watched.getImportKeys());
            }
        });
        
        item = new MenuItem(_menu, SWT.CHECK);
        item.setText(_translationRegistry.getText(T_WATCH_IMPKEYS, "Import keys they recommended"));
        item.setSelection(watched.getImportKeys());
        item.addSelectionListener(new FireSelectionListener() {
            public void fire() { 
                _client.watchChannel(watched.getChannelId(), watched.getHighlight(), watched.getImportArchives(), watched.getImportBookmarks(), watched.getImportBans(), !watched.getImportKeys());
            }
        });
        
        item = new MenuItem(_menu, SWT.CHECK);
        item.setText(_translationRegistry.getText(T_WATCH_IMPBANS, "Import and honor bans they recommended"));
        item.setSelection(watched.getImportBans());
        item.addSelectionListener(new FireSelectionListener() {
            public void fire() { 
                _client.watchChannel(watched.getChannelId(), watched.getHighlight(), watched.getImportArchives(), watched.getImportBookmarks(), !watched.getImportBans(), watched.getImportKeys());
            }
        });
        
        new MenuItem(_menu, SWT.SEPARATOR);
        
        item = new MenuItem(_menu, SWT.PUSH);
        item.setText(_translationRegistry.getText(T_WATCH_UNWATCH, "Stop watching the selected forum"));
        item.addSelectionListener(new FireSelectionListener() {
            public void fire() { _client.unwatchChannel(watched); }
        });
    }
    
    private class BrowserTreeListener extends SyndieTreeListener {
        public BrowserTreeListener(Tree tree) { super(tree); }
        /** the user doubleclicked on the selected row */
        public void doubleclick() {
            TreeItem item = getSelected();
            _ui.debugMessage("browserTree doubleclick on " + item);
            if (item != null)
                fireDefaultAction(item);
        }
        /** the user hit return on the selected row */
        public void returnHit() {
            TreeItem item = getSelected();
            if (item != null)
                fireDefaultAction(item);
        }
    }
    
    private void fireDefaultAction(TreeItem item) {
        NymReferenceNode bookmark = getBookmark(item);
        if (bookmark != null) {
            _navControl.view(bookmark.getURI(), bookmark.getName(), bookmark.getDescription());
            return;
        }
        ChannelInfo chan = getPostChannel(item);
        if (chan != null) {
            _navControl.view(SyndieURI.createScope(chan.getChannelHash()));
            return;
        }
        chan = getManageChannel(item);
        if (chan != null) {
            _navControl.view(SyndieURI.createScope(chan.getChannelHash()));
            return;
        }
    }

    protected void bookmarksRebuilt(ArrayList nymRefs) {
        if (_browserInstance != null)
            _browserInstance.bookmarksUpdated(nymRefs);
        _nymRefs = nymRefs;
    }

    public void bookmark(NymReferenceNode node) {
        //_client.addNymReference(_client.getLoggedInNymId(), node);
        if (node == null) return;
        NymReferenceNode parent = null;
        if (node.getParentGroupId() != -1) {
            for (int i = 0; i < _nymRefs.size(); i++) {
                NymReferenceNode cur = (NymReferenceNode)_nymRefs.get(i);
                parent = (NymReferenceNode)cur.getByUniqueId(node.getParentGroupId());
                if (parent != null)
                    break;
            }
        }
        if (parent != null) {
            parent.addChild(node);
        } else {
            // remove dups
            for (int i = 0; i < _nymRefs.size(); i++) {
                ReferenceNode child = (ReferenceNode)_nymRefs.get(i);
                if ( (node.getURI() != null) && (node.getURI().equals(child.getURI())) ) {
                    // its a link to the same URL in the same category
                    _nymRefs.remove(i);
                    i--;
                }
            }
            
            int order = node.getSiblingOrder();
            if ( (order < 0) || (order >= _nymRefs.size()) ) {
                _nymRefs.add(node);
            } else {
                _nymRefs.add(order, node);
            }
        }
        saveBookmarks();
    }
    
    private static final String T_BOOKMARK_VIEW = "syndie.gui.browsertree.bookmark.view";
    private static final String T_BOOKMARK_VIEWALL = "syndie.gui.browsertree.bookmark.viewall";
    private static final String T_BOOKMARK_EDIT = "syndie.gui.browsertree.bookmark.edit";
    private static final String T_BOOKMARK_DELETE = "syndie.gui.browsertree.bookmark.delete";
    private static final String T_BOOKMARK_ADD = "syndie.gui.browsertree.bookmark.add";
    private static final String T_BOOKMARK_ADDFOLDER = "syndie.gui.browsertree.bookmark.addfolder";
    private static final String T_POST_TITLE = "syndie.gui.browsertree.post.title";
    private static final String T_MANAGE_TITLE = "syndie.gui.browsertree.manage.title";
    private static final String T_SEARCH_VIEW = "syndie.gui.browsertree.search.view";
    private static final String T_SEARCH_ADVANCED = "syndie.gui.browsertree.searchadvanced";
    private static final String T_SEARCH_DETAIL_POPUP = "syndie.gui.browsertree.searchdetailpopup";
    // confirm delete is created on demand, and translated on creation
    private static final String T_CONFIRM_DELETE_TITLE = "syndie.gui.confirmdelete.title";
    private static final String T_CONFIRM_DELETE_MESSAGE = "syndie.gui.confirmdelete.message";
    
    public void translate(TranslationRegistry registry) {
        super.translate(registry);
        //_searchMenuView.setText(registry.getText(T_SEARCH_VIEW, "View"));
        _searchAdvanced.setText(registry.getText(T_SEARCH_ADVANCED, "Search..."));
        if (_searchDetailPopup != null)
            _searchDetailPopup.setText(registry.getText(T_SEARCH_DETAIL_POPUP, "Search"));
    }
    
    public void applyTheme(Theme theme) {
        super.applyTheme(theme);
        if (_searchDetailPopup != null)
            _searchDetailPopup.setFont(theme.SHELL_FONT);
        _searchAdvanced.setFont(theme.BUTTON_FONT);
    }
    
    private void initDnD() {
        initDnDTarget();
        initDnDSource();
    }
    private void initDnDSource() {
        new ReferenceChooserTreeDnDSource(_client, _ui, this);
    }
    
    // we expand a node if we are hovering over it for a half second or more
    private long _dndHoverBegin;
    private TreeItem _dndHoverCurrent;
    
    /** currently the insert marks in SWT3.3M4 don't work on linux */
    private static final boolean USE_INSERT_MARKS = (System.getProperty("os.name").indexOf("nix") == -1);
    
    private void initDnDTarget() {
        if (USE_INSERT_MARKS) {
            getTree().addMouseTrackListener(new MouseTrackListener() {
                public void mouseEnter(MouseEvent mouseEvent) { 
                    getTree().setInsertMark(null, true);
                }
                public void mouseExit(MouseEvent mouseEvent) { 
                    getTree().setInsertMark(null, true); 
                }
                public void mouseHover(MouseEvent mouseEvent) {}
            });
        }
        
        int ops = DND.DROP_COPY | DND.DROP_LINK; // move doesn't seem to work properly...
        Transfer transfer[] = new Transfer[] { TextTransfer.getInstance() };
        DropTarget target = new DropTarget(getControl(), ops);
        target.setTransfer(transfer);
        target.addDropListener(new DropTargetListener() {
            public void dragEnter(DropTargetEvent evt) {
                evt.detail = DND.DROP_COPY;
                evt.feedback = DND.FEEDBACK_SELECT | DND.FEEDBACK_EXPAND | DND.FEEDBACK_SCROLL;
            }
            public void dragLeave(DropTargetEvent evt) {
                //System.out.println("dragLeave: " + evt + "/" + evt.feedback + "/" + evt.operations + "/" + evt.detail);
                if (USE_INSERT_MARKS)
                    getTree().setInsertMark(null, true);
            }
            public void dragOperationChanged(DropTargetEvent evt) {}
            public void dragOver(DropTargetEvent evt) {
                Tree tree = getTree();
                Point pt = null;
                // swt3.3M4/gtk/linux seems to put the original drag-from location in evt.x/evt.y
                // when dragging and dropping from the same tree, which is completely useless
                //pt = tree.toControl(evt.x, evt.y);
                pt = tree.toControl(tree.getDisplay().getCursorLocation());
                
                TreeItem item = tree.getItem(pt);
                //System.out.println("dragOver: " + item + " pt:" + pt + " evt: " + evt.x + "/" + evt.y);
                setFeedback(item, evt, pt);
                expand(item, evt, pt);
                scroll(tree, item, evt, pt);
            }
            private void setFeedback(TreeItem item, DropTargetEvent evt, Point pt) {
                if (item != null) {
                    TreeItem root = item;
                    while (root.getParentItem() != null)
                        root = root.getParentItem();

                    boolean isWatch = (root == getWatchedRoot());
                    boolean isBookmark = (root == getBookmarkRoot());

                    if (isWatch) {
                        evt.detail = DND.DROP_COPY;
                        evt.feedback = DND.FEEDBACK_SELECT | DND.FEEDBACK_SCROLL;
                    } else if (isBookmark) {
                        NymReferenceNode node = getBookmark(item);
                        if (root == item) { // bookmark root, (node==null)
                            evt.detail = DND.DROP_COPY;
                            evt.feedback = DND.FEEDBACK_SELECT | DND.FEEDBACK_EXPAND | DND.FEEDBACK_SCROLL;
                        } else if (node.getURI() == null) {
                            evt.detail = DND.DROP_COPY;
                            evt.feedback = DND.FEEDBACK_SELECT | DND.FEEDBACK_EXPAND | DND.FEEDBACK_SCROLL;
                        } else {
                            evt.detail = DND.DROP_COPY;
                            evt.feedback = DND.FEEDBACK_INSERT_AFTER;
                        }
                    } else {
                        evt.detail = DND.DROP_NONE;
                        evt.feedback = DND.FEEDBACK_NONE;
                        //System.out.println("selected is not a bookmark or a watch, setting detail/feedback to none");
                    }
                } else {
                    evt.detail = DND.DROP_NONE;
                    evt.feedback = DND.FEEDBACK_NONE;
                    //System.out.println("no item selected, setting detail/feedback to none");
                }
            }
            private void expand(TreeItem item, DropTargetEvent evt, Point pt) {
                if (item != null) {
                    if (item.getItemCount() > 0) {
                        if (!item.getExpanded()) {
                            if ( (_dndHoverBegin > 0) && (_dndHoverBegin + 500 < System.currentTimeMillis()) && (_dndHoverCurrent == item) ) {
                                item.setExpanded(true);
                                _dndHoverBegin = -1;
                                _dndHoverCurrent = null;
                            } else if (_dndHoverCurrent != item) {
                                _dndHoverBegin = System.currentTimeMillis();
                                _dndHoverCurrent = item;
                            }
                        }
                        if (USE_INSERT_MARKS)
                            getTree().setInsertMark(null, true);
                    } else {
                        _dndHoverBegin = -1;
                        _dndHoverCurrent = null;
                    }
                } else {
                    _dndHoverBegin = -1;
                    _dndHoverCurrent = null;
                    if (USE_INSERT_MARKS)
                        getTree().setInsertMark(null, true);
                }
            }
            private void scroll(Tree tree, TreeItem item, DropTargetEvent evt, Point pt) {
                int height = tree.getClientArea().height;
                // scroll up/down when over items at the top/bottom 5% of the height
                int margin = height/20;
                if (pt.y <= margin)
                    scrollUp(tree);
                else if (pt.y >= height-margin)
                    scrollDown(tree);
            }
            private void scrollUp(Tree tree) {
                ScrollBar bar = tree.getVerticalBar();
                if ( (bar != null) && (bar.getSelection() > bar.getMinimum()) )
                    bar.setSelection(bar.getSelection() - bar.getIncrement());
            }
            private void scrollDown(Tree tree) {
                ScrollBar bar = tree.getVerticalBar();
                if ( (bar != null) && (bar.getSelection() < bar.getMaximum()) )
                    bar.setSelection(bar.getSelection() + bar.getIncrement());
            }
            private boolean isPointFirstHalf(Tree tree, Point point, TreeItem item) {
                Rectangle loc = item.getBounds();
                int margin = loc.height / 2;
                return (point.y < (loc.y + margin));
            }
            public void drop(DropTargetEvent evt) {
                //System.out.println("drop: " + evt);
                if (USE_INSERT_MARKS)
                    getTree().setInsertMark(null, true);
                
                Tree tree = getTree();
                Point pt = tree.toControl(evt.x, evt.y);
                TreeItem item = tree.getItem(pt);
                boolean before = isPointFirstHalf(tree, pt, item);
                
                TreeItem root = item;
                while (root.getParentItem() != null)
                    root = root.getParentItem();
                
                boolean isWatch = (root == getWatchedRoot());
                boolean isBookmark = (root == getBookmarkRoot());
                
                long parentGroupId = -1;
                int siblingOrder = 0;
                
                if (isBookmark) {
                    NymReferenceNode cur = getBookmark(item);
                    if (cur != null) {
                        if (cur.getURI() == null) {
                            parentGroupId = cur.getGroupId();
                            siblingOrder = 0;
                        } else {
                            parentGroupId = cur.getParentGroupId();
                            TreeItem parent = item.getParentItem();
                            if (parent != null) {
                                TreeItem siblings[] = parent.getItems();
                                for (int i = 0; i < siblings.length; i++) {
                                    if (item == siblings[i]) {
                                        if (before)
                                            siblingOrder = i;
                                        else
                                            siblingOrder = i+1;
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
                
                if (evt.data == null) {
                    evt.detail = DND.DROP_NONE;
                    return;
                } else {
                    _ui.debugMessage("dropping bookmark under " + parentGroupId + " order " + siblingOrder + " (before? " + before + " target: " + item + ")");
                    BookmarkDnD bookmark = new BookmarkDnD();
                    bookmark.fromString(evt.data.toString());
                    if (bookmark.uri != null) { // parsed fine
                        if (isWatch) {
                            watch(bookmark.uri, item);
                        } else if (isBookmark) {
                            _bookmarkControl.bookmark(new NymReferenceNode(bookmark.name, bookmark.uri, bookmark.desc, -1, -1, parentGroupId, siblingOrder, false, false, false), true);
                        }
                    } else { // wasn't in bookmark syntax, try as a uri
                        String str = evt.data.toString();
                        try {
                            SyndieURI uri = new SyndieURI(str);
                            if (isWatch)
                                watch(uri, item);
                            else if (isBookmark)
                                _browserInstance.bookmark(uri, parentGroupId, siblingOrder);
                        } catch (URISyntaxException use) {
                            _ui.debugMessage("invalid uri: " + str, use);
                            byte val[] = Base64.decode(str);
                            if ( (val != null) && (val.length == Hash.HASH_LENGTH) ) {
                                SyndieURI uri = SyndieURI.createScope(new Hash(val));
                                if (isWatch)
                                    watch(uri, item);
                                else if (isBookmark)
                                    _browserInstance.bookmark(uri, parentGroupId, siblingOrder);
                            }
                        }
                    }
                }
            }
            public void dropAccept(DropTargetEvent evt) {
                //System.out.println("dropAccept: " + evt);
            }
        });
    }
}
