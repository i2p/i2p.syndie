package syndie.gui;

import java.util.Collection;
import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.ChannelInfo;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

/**
 *
 */
class BrowserTree extends ReferenceChooserTree implements Translatable, Themeable {
    private Browser _browser;
    private Menu _bookmarkMenu;
    private Menu _postMenu;
    private Menu _manageMenu;
    private Menu _searchMenu;
    
    private Button _searchAdvanced;
    
    private BookmarkEditorPopup _bookmarkEditor;
    private ReferenceChooserSearch _searchDetail;
    private Shell _searchDetailPopup;

    private MenuItem _bookmarkMenuView;
    private MenuItem _bookmarkMenuEdit;
    private MenuItem _bookmarkMenuDelete;
    private MenuItem _bookmarkMenuAdd;
    private MenuItem _postMenuItem;
    private MenuItem _manageMenuItem;
    private MenuItem _searchMenuView;
    private long _startInit;
    private long _superInit;
    
    private List _nymRefs;
    
    public BrowserTree(Browser browser, Composite parent, ChoiceListener lsnr, AcceptanceListener accept) {
        super(browser, parent, lsnr, accept, false);
        _browser = browser;
        long t1 = System.currentTimeMillis();
        //_bookmarkEditor = new BookmarkEditorPopup(browser, parent.getShell());
        long t2 = System.currentTimeMillis();
        System.out.println("tree curInit: " + (t1-_superInit) + ", superInit:" + (_superInit-_startInit) + " editor init: " + (t2-t1));
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
        
        getBrowser().getTranslationRegistry().register(this);
        getBrowser().getThemeRegistry().register(this);
    }
    
    private void search() {
        if (_searchDetail == null)
            createSearchDetailPopup();
        _searchDetail.search();
        _searchDetailPopup.setVisible(false);
    }
    private void searchAdvanced() { 
        if (_searchDetail == null)
            createSearchDetailPopup();
        _searchDetailPopup.open(); 
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
    
        _bookmarkMenu = new Menu(tree);
        _bookmarkMenuView = new MenuItem(_bookmarkMenu, SWT.PUSH);
        _bookmarkMenuView.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { getBrowser().view(getBookmarkURI(getSelectedItem())); }
            public void widgetSelected(SelectionEvent evt) { getBrowser().view(getBookmarkURI(getSelectedItem())); }
        });
        _bookmarkMenuEdit = new MenuItem(_bookmarkMenu, SWT.PUSH);
        _bookmarkMenuEdit.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { editBookmark(getSelectedItem()); }
            public void widgetSelected(SelectionEvent evt) { editBookmark(getSelectedItem()); }
        });
        _bookmarkMenuDelete = new MenuItem(_bookmarkMenu, SWT.PUSH);
        _bookmarkMenuDelete.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { deleteBookmark(getSelectedItem()); }
            public void widgetSelected(SelectionEvent evt) { deleteBookmark(getSelectedItem()); }
        });
        _bookmarkMenuAdd = new MenuItem(_bookmarkMenu, SWT.PUSH);
        _bookmarkMenuAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { addBookmark(); }
            public void widgetSelected(SelectionEvent evt) { addBookmark(); }
        });
        
        _postMenu = new Menu(tree);
        _postMenuItem = new MenuItem(_postMenu, SWT.PUSH);
        _postMenuItem.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { getBrowser().view(getBrowser().createPostURI(getPostScope(getSelectedItem()), null, false)); }
            public void widgetSelected(SelectionEvent evt) { getBrowser().view(getBrowser().createPostURI(getPostScope(getSelectedItem()), null, false)); }
        });
        
        _manageMenu = new Menu(tree);
        _manageMenuItem = new MenuItem(_manageMenu, SWT.PUSH);
        _manageMenuItem.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { getBrowser().view(getBrowser().createManageURI(getManageScope(getSelectedItem()))); }
            public void widgetSelected(SelectionEvent evt) { getBrowser().view(getBrowser().createManageURI(getManageScope(getSelectedItem()))); }
        });
        
        _searchMenu = new Menu(tree);
        _searchMenuView = new MenuItem(_searchMenu, SWT.PUSH);
        _searchMenuView.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { getBrowser().view(getSearchResultURI(getSelectedItem())); }
            public void widgetSelected(SelectionEvent evt) { getBrowser().view(getSearchResultURI(getSelectedItem())); }
        });
        
        tree.setMenu(null);
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
    
    private SyndieURI getSearchResultURI(TreeItem item) {
        ReferenceNode node = getSearchResult(item);
        if (node != null)
            return node.getURI();
        else
            return null;
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
    
    private void pickMenu(Tree tree, TreeItem item) {
        if (item == null) {
            tree.setMenu(null);
            return;
        }
        
        NymReferenceNode bookmark = getBookmark(item);
        if ( (bookmark != null) || (item.equals(getBookmarkRoot())) ) {
            tree.setMenu(_bookmarkMenu);
            return;
        }
        ChannelInfo chan = getPostChannel(item);
        if ( (chan != null) || (item.equals(getPostRoot())) ) {
            tree.setMenu(_postMenu);
            return;
        }
        chan = getManageChannel(item);
        if ( (chan != null) || (item.equals(getManageRoot())) ) {
            tree.setMenu(_manageMenu);
            return;
        }
        ReferenceNode search = getSearchResult(item);
        if ( (search != null) || (item.equals(getSearchRoot())) ) {
            tree.setMenu(_searchMenu);
            return;
        }
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
        public void mouseDoubleClick(MouseEvent evt) {
            pickMenu(getTree(), getTree().getItem(new Point(evt.x, evt.y)));
            super.mouseDoubleClick(evt);
        }
        public void mouseDown(MouseEvent evt) {
            pickMenu(getTree(), getTree().getItem(new Point(evt.x, evt.y)));
            super.mouseDown(evt);
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
        ReferenceNode search = getSearchResult(item);
        if (search != null) {
            getBrowser().view(search.getURI());
            return;
        }
    }

    protected void bookmarksRebuilt(List nymRefs) {
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
        _bookmarkMenuView.setText(registry.getText(T_BOOKMARK_VIEW, "View"));
        _bookmarkMenuEdit.setText(registry.getText(T_BOOKMARK_EDIT, "Edit"));
        _bookmarkMenuDelete.setText(registry.getText(T_BOOKMARK_DELETE, "Delete"));
        _bookmarkMenuAdd.setText(registry.getText(T_BOOKMARK_ADD, "Add"));
        _postMenuItem.setText(registry.getText(T_POST_TITLE, "Post"));
        _manageMenuItem.setText(registry.getText(T_MANAGE_TITLE, "Manage"));
        _searchMenuView.setText(registry.getText(T_SEARCH_VIEW, "View"));
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
}
