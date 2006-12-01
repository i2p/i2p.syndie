package syndie.gui;

import java.util.Collection;
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
    private Menu _bookmarkMenu;
    private Menu _postMenu;
    private Menu _manageMenu;
    private Menu _searchMenu;
    
    private Text _search;
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
    
    public BrowserTree(BrowserControl browser, Composite parent, ChoiceListener lsnr, AcceptanceListener accept) {
        super(browser, parent, lsnr, accept, false);
        _bookmarkEditor = new BookmarkEditorPopup(browser, parent.getShell());
    }
    
    protected void initComponents(boolean register) {
        super.initComponents(false);
        
        Composite searchRow = new Composite((Composite)getControl(), SWT.NONE);
        searchRow.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        searchRow.setLayout(new GridLayout(2, false));
        
        _search = new Text(searchRow, SWT.BORDER | SWT.SINGLE);
        _search.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _search.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN)
                    search();
            }
        });
        
        _searchAdvanced = new Button(searchRow, SWT.PUSH);
        _searchAdvanced.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, true));
        _searchAdvanced.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { searchAdvanced(); }
            public void widgetSelected(SelectionEvent selectionEvent) { searchAdvanced(); }
        });
        
        createSearchDetailPopup();
        
        getBrowser().getTranslationRegistry().register(this);
        getBrowser().getThemeRegistry().register(this);
    }
    
    private void search() {
        String txt = _search.getText();
        _searchDetail.setTags(txt);
        _searchDetail.search();
        _searchDetailPopup.setVisible(false);
    }
    private void searchAdvanced() { 
        String txt = _search.getText();
        _searchDetail.setTags(txt);
        _searchDetailPopup.open(); 
    }
    
    public void setSearchResults(Collection resultNodes) {
        super.setSearchResults(resultNodes);
        _searchDetailPopup.setVisible(false);
    }
    
    private void createSearchDetailPopup() {
        _searchDetailPopup = new Shell(_search.getShell(), SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        _searchDetailPopup.setLayout(new FillLayout());
        _searchDetail = new ReferenceChooserSearch(_searchDetailPopup, this, getBrowser());
        _searchDetailPopup.setSize(_searchDetailPopup.computeSize(300, SWT.DEFAULT));
        
        // intercept the shell closing, since that'd cause the shell to be disposed rather than just hidden
        _searchDetailPopup.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; _searchDetailPopup.setVisible(false); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });

    }
    
    public void viewStartupItems() { viewStartupItems(getBookmarkRoot()); }
    // depth first traversal, so its the same each time, rather than using super._bookmarkNodes
    private void viewStartupItems(TreeItem item) {
        if (item == null) return;
        NymReferenceNode node = getBookmark(item);
        if ( (node != null) && node.getLoadOnStart())
            getBrowser().view(node.getURI());
        for (int i = 0; i < item.getItemCount(); i++)
            viewStartupItems(item.getItem(i));
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
            _bookmarkEditor.setBookmark(node);
            _bookmarkEditor.open();
        }
    }
    
    private void addBookmark() {
        _bookmarkEditor.setBookmark(null);
        _bookmarkEditor.open();
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
    
    private void showMenu(Tree tree, TreeItem selected) {
        pickMenu(tree, selected);
        Menu menu = tree.getMenu();
        if (menu != null)
            menu.setVisible(true);
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
        _searchAdvanced.setText(registry.getText(T_SEARCH_ADVANCED, "Advanced..."));
        _searchDetailPopup.setText(registry.getText(T_SEARCH_DETAIL_POPUP, "Search"));
    }
    
    public void applyTheme(Theme theme) {
        super.applyTheme(theme);
        _searchDetailPopup.setFont(theme.SHELL_FONT);
        _search.setFont(theme.DEFAULT_FONT);
        _searchAdvanced.setFont(theme.BUTTON_FONT);
    }
}
