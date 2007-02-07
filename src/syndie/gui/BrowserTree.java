package syndie.gui;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import net.i2p.data.Base64;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.DropTargetListener;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
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
import syndie.data.WatchedChannel;
import syndie.db.DBClient;

/**
 *
 */
class BrowserTree extends ReferenceChooserTree implements Translatable, Themeable {
    private Browser _browser;
    private Menu _menu;
    
    private Button _searchAdvanced;
    
    private BookmarkEditorPopup _bookmarkEditor;
    private ReferenceChooserSearch _searchDetail;
    private Shell _searchDetailPopup;

    private long _startInit;
    private long _superInit;
    
    private List _nymRefs;
    
    public BrowserTree(Browser browser, Composite parent, ChoiceListener lsnr, AcceptanceListener accept) {
        super(browser, parent, lsnr, accept, false, false);
        _browser = browser;
        long t1 = System.currentTimeMillis();
        //_bookmarkEditor = new BookmarkEditorPopup(browser, parent.getShell());
        long t2 = System.currentTimeMillis();
        //System.out.println("tree curInit: " + (t1-_superInit) + ", superInit:" + (_superInit-_startInit) + " editor init: " + (t2-t1));
        if (_nymRefs != null)
            _browser.bookmarksUpdated(_nymRefs);
        _nymRefs = null;
    }
    private BookmarkEditorPopup getBookmarkEditor() {
        // only called by the swt thread, so no need to sync
        if (_bookmarkEditor == null)
            _bookmarkEditor = new BookmarkEditorPopup(getBrowser(), getControl().getShell());
        return _bookmarkEditor;
    }
    
    protected void initComponents(boolean register) {
        _startInit = System.currentTimeMillis();
        super.initComponents(false);
        _superInit = System.currentTimeMillis();
        
        _searchAdvanced = new Button((Composite)getControl(), SWT.PUSH);
        _searchAdvanced.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _searchAdvanced.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { searchAdvanced(); }
            public void widgetSelected(SelectionEvent selectionEvent) { searchAdvanced(); }
        });
        
        //createSearchDetailPopup();
        
        initDnD();
        
        getBrowser().getTranslationRegistry().register(this);
        getBrowser().getThemeRegistry().register(this);
    }
    
    private void search() {
        if (_searchDetail == null)
            createSearchDetailPopup();
        _searchDetail.search();
        _searchDetailPopup.setVisible(false);
    }
    private static final String T_SEARCH_FORUM_TITLE = "syndie.gui.browsertree.searchforumtitle";
    private void searchAdvanced() { 
        final ReferenceChooserPopup popup = new ReferenceChooserPopup(getControl().getShell(), _browser, T_SEARCH_FORUM_TITLE, "Forum search");
        popup.setListener(new ReferenceChooserTree.AcceptanceListener() {
            public void referenceAccepted(SyndieURI uri) { _browser.view(uri); }
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
    
    private void createSearchDetailPopup() {
        _searchDetailPopup = new Shell(_searchAdvanced.getShell(), SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        _searchDetailPopup.setLayout(new FillLayout());
        _searchDetail = new ReferenceChooserSearch(_searchDetailPopup, this, getBrowser());
        _searchDetailPopup.setSize(_searchDetailPopup.computeSize(300, SWT.DEFAULT));
        _searchDetailPopup.setText(getBrowser().getTranslationRegistry().getText(T_SEARCH_DETAIL_POPUP, "Search"));
        _searchDetailPopup.setFont(getBrowser().getThemeRegistry().getTheme().SHELL_FONT);
        
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
        
        TreeItem item = getSelectedItem();
        
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
    }
    
    private void buildManageMenu(final ChannelInfo chan) {
        MenuItem item = new MenuItem(_menu, SWT.PUSH);
        item.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { getBrowser().view(getBrowser().createManageURI(chan.getChannelHash())); }
            public void widgetSelected(SelectionEvent evt) { getBrowser().view(getBrowser().createManageURI(chan.getChannelHash())); }
        });
        item.setText(_browser.getTranslationRegistry().getText(T_MANAGE_TITLE, "Manage"));
    }
    private void buildPostMenu(final ChannelInfo chan) {
        MenuItem item = new MenuItem(_menu, SWT.PUSH);
        item.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { getBrowser().view(getBrowser().createPostURI(chan.getChannelHash(), null, false)); }
            public void widgetSelected(SelectionEvent evt) { getBrowser().view(getBrowser().createPostURI(chan.getChannelHash(), null, false)); }
        });        
        item.setText(_browser.getTranslationRegistry().getText(T_POST_TITLE, "Post"));
    }
    private void buildBookmarkMenu(final NymReferenceNode bookmark, final TreeItem selected) {
        MenuItem item = new MenuItem(_menu, SWT.PUSH);
        item.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { getBrowser().view(bookmark.getURI()); }
            public void widgetSelected(SelectionEvent evt) { getBrowser().view(bookmark.getURI()); }
        });
        item.setText(_browser.getTranslationRegistry().getText(T_BOOKMARK_VIEW, "View"));
        
        item = new MenuItem(_menu, SWT.PUSH);
        item.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { editBookmark(selected); }
            public void widgetSelected(SelectionEvent evt) { editBookmark(selected); }
        });
        item.setText(_browser.getTranslationRegistry().getText(T_BOOKMARK_EDIT, "Edit"));
        
        item = new MenuItem(_menu, SWT.PUSH);
        item.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { deleteBookmark(selected); }
            public void widgetSelected(SelectionEvent evt) { deleteBookmark(selected); }
        });
        item.setText(_browser.getTranslationRegistry().getText(T_BOOKMARK_DELETE, "Delete"));
        
        item = new MenuItem(_menu, SWT.PUSH);
        item.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { addBookmark(); }
            public void widgetSelected(SelectionEvent evt) { addBookmark(); }
        });
        item.setText(_browser.getTranslationRegistry().getText(T_BOOKMARK_ADD, "Add"));
    }

    private SyndieURI getBookmarkURI(TreeItem item) {
        NymReferenceNode node = getBookmark(item);
        if (node != null)
            return node.getURI();
        else
            return null;
    }
    
    private void editBookmark(TreeItem item) {
        NymReferenceNode node = getBookmark(item);
        if (node != null) {
            BookmarkEditorPopup popup = getBookmarkEditor();
            popup.setBookmark(node);
            popup.open();
        }
    }
    
    private void addBookmark() {
        BookmarkEditorPopup popup = getBookmarkEditor();
        popup.setBookmark(null);
        popup.open();
    }
    
    private void deleteBookmark(TreeItem item) {
        NymReferenceNode node = getBookmark(item);
        if (node != null) {
            MessageBox box = new MessageBox(getControl().getShell(), SWT.ICON_WARNING | SWT.YES | SWT.NO);
            box.setMessage(getBrowser().getTranslationRegistry().getText(T_CONFIRM_DELETE_MESSAGE, "Are you sure you want to delete this bookmark?"));
            box.setText(getBrowser().getTranslationRegistry().getText(T_CONFIRM_DELETE_TITLE, "Confirm"));
            int rc = box.open();
            if (rc == SWT.YES)
                getBrowser().deleteBookmark(node.getGroupId());
        }
    }
    
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
    
    private TreeItem getSelectedItem() {
        TreeItem selected[] = getTree().getSelection();
        if ( (selected != null) && (selected.length == 1) )
            return selected[0];
        else
            return null;
    }

    private static final String T_WATCH_VIEW = "syndie.gui.browsertree.watch.view";
    private static final String T_WATCH_HIGHLIGHT = "syndie.gui.browsertree.watch.highlight";
    private static final String T_WATCH_IMPBOOKMARKS = "syndie.gui.browsertree.watch.impbookmarks";
    private static final String T_WATCH_IMPARCHIVES = "syndie.gui.browsertree.watch.imparchives";
    private static final String T_WATCH_IMPKEYS = "syndie.gui.browsertree.watch.impkeys";
    private static final String T_WATCH_IMPBANS = "syndie.gui.browsertree.watch.impbans";
    private static final String T_WATCH_UNWATCH = "syndie.gui.browsertree.watch.unwatch";
    
    private void buildWatchedMenu(final WatchedChannel watched) {
        final Hash scope = _browser.getClient().getChannelHash(watched.getChannelId());
        _browser.getUI().debugMessage("Building watch menu for " + scope + "/" + watched.getChannelId(), new Exception("source"));
        MenuItem item = new MenuItem(_menu, SWT.PUSH);
        item.setText(_browser.getTranslationRegistry().getText(T_WATCH_VIEW, "View the selected forum"));
        item.addSelectionListener(new FireSelectionListener() {
            void fire() { _browser.view(SyndieURI.createScope(scope)); }
        });
        new MenuItem(_menu, SWT.SEPARATOR);
        item = new MenuItem(_menu, SWT.CHECK);
        item.setText(_browser.getTranslationRegistry().getText(T_WATCH_HIGHLIGHT, "Highlight the forum's unread messages"));
        item.setSelection(watched.getHighlight());
        item.addSelectionListener(new FireSelectionListener() {
            void fire() { 
                _browser.getClient().watchChannel(watched.getChannelId(), !watched.getHighlight(), watched.getImportArchives(), watched.getImportBookmarks(), watched.getImportBans(), watched.getImportKeys());
            }
        });

        item = new MenuItem(_menu, SWT.CHECK);
        item.setText(_browser.getTranslationRegistry().getText(T_WATCH_IMPBOOKMARKS, "Import their recommended bookmarks"));
        item.setSelection(watched.getImportBookmarks());
        item.addSelectionListener(new FireSelectionListener() {
            void fire() { 
                _browser.getClient().watchChannel(watched.getChannelId(), watched.getHighlight(), watched.getImportArchives(), !watched.getImportBookmarks(), watched.getImportBans(), watched.getImportKeys());
            }
        });

        item = new MenuItem(_menu, SWT.CHECK);
        item.setText(_browser.getTranslationRegistry().getText(T_WATCH_IMPARCHIVES, "Import their recommended archives"));
        item.setSelection(watched.getImportArchives());
        item.addSelectionListener(new FireSelectionListener() {
            void fire() { 
                _browser.getClient().watchChannel(watched.getChannelId(), watched.getHighlight(), !watched.getImportArchives(), watched.getImportBookmarks(), watched.getImportBans(), watched.getImportKeys());
            }
        });
        
        item = new MenuItem(_menu, SWT.CHECK);
        item.setText(_browser.getTranslationRegistry().getText(T_WATCH_IMPKEYS, "Import keys they recommended"));
        item.setSelection(watched.getImportKeys());
        item.addSelectionListener(new FireSelectionListener() {
            void fire() { 
                _browser.getClient().watchChannel(watched.getChannelId(), watched.getHighlight(), watched.getImportArchives(), watched.getImportBookmarks(), watched.getImportBans(), !watched.getImportKeys());
            }
        });
        
        item = new MenuItem(_menu, SWT.CHECK);
        item.setText(_browser.getTranslationRegistry().getText(T_WATCH_IMPBANS, "Import and honor bans they recommended"));
        item.setSelection(watched.getImportBans());
        item.addSelectionListener(new FireSelectionListener() {
            void fire() { 
                _browser.getClient().watchChannel(watched.getChannelId(), watched.getHighlight(), watched.getImportArchives(), watched.getImportBookmarks(), !watched.getImportBans(), watched.getImportKeys());
            }
        });
        
        new MenuItem(_menu, SWT.SEPARATOR);
        
        item = new MenuItem(_menu, SWT.PUSH);
        item.setText(_browser.getTranslationRegistry().getText(T_WATCH_UNWATCH, "Stop watching the selected forum"));
        item.addSelectionListener(new FireSelectionListener() {
            void fire() { _browser.getClient().unwatchChannel(watched); }
        });
    }
    
    private class BrowserTreeListener extends SyndieTreeListener {
        public BrowserTreeListener(Tree tree) { super(tree); }
        /** the user doubleclicked on the selected row */
        public void doubleclick() {
            TreeItem item = getSelected();
            getBrowser().getUI().debugMessage("browserTree doubleclick on " + item);
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
            getBrowser().view(bookmark.getURI());
            return;
        }
        ChannelInfo chan = getPostChannel(item);
        if (chan != null) {
            getBrowser().view(SyndieURI.createScope(chan.getChannelHash()));
            return;
        }
        chan = getManageChannel(item);
        if (chan != null) {
            getBrowser().view(SyndieURI.createScope(chan.getChannelHash()));
            return;
        }
    }

    protected void bookmarksRebuilt(ArrayList nymRefs) {
        if (_browser != null)
            _browser.bookmarksUpdated(nymRefs);
        else
            _nymRefs = nymRefs;
    }
    
    private static final String T_BOOKMARK_VIEW = "syndie.gui.browsertree.bookmark.view";
    private static final String T_BOOKMARK_EDIT = "syndie.gui.browsertree.bookmark.edit";
    private static final String T_BOOKMARK_DELETE = "syndie.gui.browsertree.bookmark.delete";
    private static final String T_BOOKMARK_ADD = "syndie.gui.browsertree.bookmark.add";
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
        Transfer transfer[] = new Transfer[] { TextTransfer.getInstance() };
        int ops = DND.DROP_COPY | DND.DROP_MOVE;
        DragSource source = new DragSource(getTree(), ops);
        source.setTransfer(transfer);
        source.addDragListener(new DragSourceListener() {
            public void dragFinished(DragSourceEvent evt) {
                System.out.println("dragFinished");
                if ( (evt.detail & DND.DROP_MOVE) == DND.DROP_MOVE) {
                    // remove the old item
                }
            }
            public void dragSetData(DragSourceEvent evt) {
                TreeItem item = getSelectedItem();
                System.out.println("dragSetData: " + item);
                if (item == null) {
                    evt.doit = false;
                    return;
                }

                BookmarkDnD src = null;
                
                NymReferenceNode bookmark = getBookmark(item);
                if ( (bookmark != null) || (item.equals(getBookmarkRoot())) ) {
                    src = new BookmarkDnD();
                    src.desc = bookmark.getDescription();
                    String name = bookmark.getName();
                    src.name = name;
                    src.uri = bookmark.getURI();
                }
                if (src == null) {
                    ChannelInfo chan = getPostChannel(item);
                    if ( (chan != null) || (item.equals(getPostRoot())) ) {
                        src = new BookmarkDnD();
                        src.desc = chan.getDescription();
                        String name = chan.getName();
                        if (name == null)
                            name = chan.getChannelHash().toBase64();
                        src.name = name;
                        src.uri = SyndieURI.createScope(chan.getChannelHash());
                    }
                }
                if (src == null) {
                    ChannelInfo chan = getManageChannel(item);
                    if ( (chan != null) || (item.equals(getManageRoot())) ) {
                        src = new BookmarkDnD();
                        src.desc = chan.getDescription();
                        String name = chan.getName();
                        if (name == null)
                            name = chan.getChannelHash().toBase64();
                        src.name = name;
                        src.uri = SyndieURI.createScope(chan.getChannelHash());
                    }
                }
                if (src == null) {
                    WatchedChannel watched = getWatchedChannel(item);
                    if (watched != null) {
                        Hash scope = _browser.getClient().getChannelHash(watched.getChannelId());
                        src = new BookmarkDnD();
                        src.desc = "";
                        String name = _browser.getClient().getChannelName(watched.getChannelId());
                        if (name == null)
                            name = scope.toBase64();
                        src.name = name;
                        src.uri = SyndieURI.createScope(scope);
                    }
                }
                
                //BookmarkDnD bookmark = getBookmark(sel[0], (ReferenceNode)_itemToNode.get(sel[0]));
                if (src != null) {
                    evt.data = src.toString();
                    evt.doit = true;
                } else {
                    evt.doit = false;
                }
                System.out.println("dragSetData: " + evt.data);
            }
            public void dragStart(DragSourceEvent evt) {
                if (!isDraggable()) 
                    evt.doit = false; // don't drag when nothing is selected
                System.out.println("dragStart: " + evt.doit);
            }
        });
    }
    
    private boolean isDraggable() {
        TreeItem item = getSelectedItem();
        if (item == null) return false;
        
        NymReferenceNode bookmark = getBookmark(item);
        if ( (bookmark != null) || (item.equals(getBookmarkRoot())) ) {
            return bookmark.getURI() != null;
        }
        ChannelInfo chan = getPostChannel(item);
        if ( (chan != null) || (item.equals(getPostRoot())) ) {
            return true;
        }
        chan = getManageChannel(item);
        if ( (chan != null) || (item.equals(getManageRoot())) ) {
            return true;
        }
        WatchedChannel watched = getWatchedChannel(item);
        if (watched != null) {
            return true;
        }
        return false;
    }
    
    // we expand a node if we are hovering over it for a half second or more
    private long _dndHoverBegin;
    private TreeItem _dndHoverCurrent;
    
    private void initDnDTarget() {
        getTree().addMouseTrackListener(new MouseTrackListener() {
            public void mouseEnter(MouseEvent mouseEvent) { 
                getTree().setInsertMark(null, true);
            }
            public void mouseExit(MouseEvent mouseEvent) { 
                getTree().setInsertMark(null, true); 
            }
            public void mouseHover(MouseEvent mouseEvent) {}
        });
        int ops = DND.DROP_COPY | DND.DROP_LINK | DND.DROP_MOVE;
        Transfer transfer[] = new Transfer[] { TextTransfer.getInstance() };
        DropTarget target = new DropTarget(getControl(), ops);
        target.setTransfer(transfer);
        target.addDropListener(new DropTargetListener() {
            public void dragEnter(DropTargetEvent evt) {
                // we can take the element
                if (false && (evt.operations & DND.DROP_MOVE) == DND.DROP_MOVE) {
                    evt.detail = evt.operations | DND.DROP_MOVE;
                    System.out.println("dragEnter: move");
                } else {
                    evt.detail = evt.operations | DND.DROP_COPY;
                    System.out.println("dragEnter: copy");
                }
                evt.feedback |= DND.FEEDBACK_EXPAND | DND.FEEDBACK_SCROLL | DND.FEEDBACK_SELECT;
            }
            public void dragLeave(DropTargetEvent evt) {
                getTree().setInsertMark(null, true);
            }
            public void dragOperationChanged(DropTargetEvent evt) {
                System.out.println("dragOperationChanged: " + evt);
            }
            public void dragOver(DropTargetEvent evt) {
                Tree tree = getTree();
                //tree.setInsertMark(null, true);
                Point pt = tree.toControl(evt.x, evt.y);
                TreeItem item = tree.getItem(pt);
                System.out.println("dragOver: " + item);
                setFeedback(item, evt, pt);
                scroll(tree, item, evt, pt);
            }
            private void setFeedback(TreeItem item, DropTargetEvent evt, Point pt) {
                if (item != null) {
                    if (item.getItemCount() > 0) {
                        if (!item.getExpanded()) {
                            if ( (_dndHoverBegin > 0) && (_dndHoverBegin + 500 < System.currentTimeMillis()) && (_dndHoverCurrent == item) ) {
                                item.setExpanded(true);
                                evt.feedback |= DND.FEEDBACK_SCROLL | DND.FEEDBACK_INSERT_AFTER;
                                _dndHoverBegin = -1;
                                _dndHoverCurrent = null;
                                System.out.println("dragOver: setFeedback: expand");
                            } else if (_dndHoverCurrent != item) {
                                evt.feedback |= DND.FEEDBACK_EXPAND | DND.FEEDBACK_SCROLL;
                                _dndHoverBegin = System.currentTimeMillis();
                                _dndHoverCurrent = item;
                                System.out.println("dragOver: setFeedback: delay before expand");
                            }
                        } else {
                            System.out.println("dragOver: setFeedback: expanded");
                        }
                        getTree().setInsertMark(null, true);
                    } else {
                        if (isPointFirstHalf(getTree(), pt, item)) {
                            getTree().setInsertMark(item, true);
                            evt.feedback = DND.FEEDBACK_INSERT_BEFORE;
                            System.out.println("dragOver: setFeedback: insertBefore");
                        } else {
                            getTree().setInsertMark(item, false);
                            evt.feedback = DND.FEEDBACK_INSERT_AFTER;
                            System.out.println("dragOver: setFeedback: insertAfter");
                        }
                        evt.feedback |= DND.FEEDBACK_SCROLL;
                        _dndHoverBegin = -1;
                        _dndHoverCurrent = null;
                    }
                } else {
                    evt.feedback |= DND.FEEDBACK_EXPAND | DND.FEEDBACK_SCROLL;
                    _dndHoverBegin = -1;
                    _dndHoverCurrent = null;
                    getTree().setInsertMark(null, true);
                    System.out.println("dragOver: setFeedback: no item");
                }
            }
            private void scroll(Tree tree, TreeItem item, DropTargetEvent evt, Point pt) {
                int height = tree.getClientArea().height;
                // scroll up/down when over items at the top/bottom 5% of the height
                int margin = height/20;
                if (pt.y <= margin) {
                    scrollUp(tree);
                } else if (pt.y >= height-margin) {
                    scrollDown(tree);
                }
            }
            private void scrollUp(Tree tree) {
                ScrollBar bar = tree.getVerticalBar();
                if ( (bar != null) && (bar.getSelection() > bar.getMinimum()) ) {
                    bar.setSelection(bar.getSelection() - bar.getIncrement());
                }
            }
            private void scrollDown(Tree tree) {
                ScrollBar bar = tree.getVerticalBar();
                if ( (bar != null) && (bar.getSelection() < bar.getMaximum()) ) {
                    bar.setSelection(bar.getSelection() + bar.getIncrement());
                }
            }
            private boolean isPointFirstHalf(Tree tree, Point point, TreeItem item) {
                Rectangle loc = item.getBounds();
                int margin = loc.height / 2;
                return (point.y < (loc.y + margin));
            }
            public void drop(DropTargetEvent evt) {
                System.out.println("drop: " + evt);
                getTree().setInsertMark(null, true);
                
                Tree tree = getTree();
                //tree.setInsertMark(null, true);
                Point pt = tree.toControl(evt.x, evt.y);
                TreeItem item = tree.getItem(pt);
                boolean before = isPointFirstHalf(tree, pt, item);
                
                boolean isWatch = false;
                TreeItem root = item;
                while (root.getParentItem() != null)
                    root = root.getParentItem();
                
                if (evt.data == null) {
                    evt.detail = DND.DROP_NONE;
                    return;
                } else {
                    BookmarkDnD bookmark = new BookmarkDnD();
                    bookmark.fromString(evt.data.toString());
                    if (bookmark.uri != null) { // parsed fine
                        if (root == getWatchedRoot()) {
                            watch(bookmark.uri, item);
                        } else if (root == getBookmarkRoot()) {
                            NymReferenceNode parent = StatusBar.getParent(_browser, bookmark);
                            long parentGroupId = -1;
                            if (parent != null)
                                parentGroupId = parent.getGroupId();
                            _browser.bookmark(new NymReferenceNode(bookmark.name, bookmark.uri, bookmark.desc, -1, -1, parentGroupId, 0, false, false, false));
                        }
                    } else { // wasn't in bookmark syntax, try as a uri
                        String str = evt.data.toString();
                        try {
                            SyndieURI uri = new SyndieURI(str);
                            if (root == getWatchedRoot()) {
                                watch(uri, item);
                            } else if (root == getBookmarkRoot()) {
                                _browser.bookmark(uri);
                            }
                        } catch (URISyntaxException use) {
                            _browser.getUI().debugMessage("invalid uri: " + str, use);
                            byte val[] = Base64.decode(str);
                            if ( (val != null) && (val.length == Hash.HASH_LENGTH) ) {
                                SyndieURI uri = SyndieURI.createScope(new Hash(val));
                                if (root == getWatchedRoot()) {
                                    watch(uri, item);
                                } else if (root == getBookmarkRoot()) {
                                    _browser.bookmark(uri);
                                }
                            }
                        }
                    }
                }
            }
            public void dropAccept(DropTargetEvent evt) {
                System.out.println("dropAccept: " + evt);
            }
        });
    }
}
