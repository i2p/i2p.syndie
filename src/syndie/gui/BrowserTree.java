package syndie.gui;

import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
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
class BrowserTree extends ReferenceChooserTree {
    private BrowserControl _browser;
    private Menu _bookmarkMenu;
    private Menu _postMenu;
    private Menu _manageMenu;
    private Menu _searchMenu;
    
    private BookmarkEditorPopup _bookmarkEditor;
    
    public BrowserTree(BrowserControl browser, Composite parent, ChoiceListener lsnr, AcceptanceListener accept) {
        super(browser.getUI(), browser.getClient(), parent, lsnr, accept, false);
        _browser = browser;
        _bookmarkEditor = new BookmarkEditorPopup(browser, parent.getShell());
    }
    
    public void viewStartupItems() { viewStartupItems(getBookmarkRoot()); }
    // depth first traversal, so its the same each time, rather than using super._bookmarkNodes
    private void viewStartupItems(TreeItem item) {
        if (item == null) return;
        NymReferenceNode node = getBookmark(item);
        if ( (node != null) && node.getLoadOnStart())
            _browser.view(node.getURI());
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
        MenuItem view = new MenuItem(_bookmarkMenu, SWT.PUSH);
        view.setText("View");
        view.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { _browser.view(getBookmarkURI(getSelectedItem())); }
            public void widgetSelected(SelectionEvent evt) { _browser.view(getBookmarkURI(getSelectedItem())); }
        });
        MenuItem edit = new MenuItem(_bookmarkMenu, SWT.PUSH);
        edit.setText("Edit");
        edit.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { editBookmark(getSelectedItem()); }
            public void widgetSelected(SelectionEvent evt) { editBookmark(getSelectedItem()); }
        });
        MenuItem delete = new MenuItem(_bookmarkMenu, SWT.PUSH);
        delete.setText("Delete");
        delete.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { deleteBookmark(getSelectedItem()); }
            public void widgetSelected(SelectionEvent evt) { deleteBookmark(getSelectedItem()); }
        });
        MenuItem add = new MenuItem(_bookmarkMenu, SWT.PUSH);
        add.setText("Add");
        add.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { addBookmark(); }
            public void widgetSelected(SelectionEvent evt) { addBookmark(); }
        });
        
        _postMenu = new Menu(tree);
        MenuItem post = new MenuItem(_postMenu, SWT.PUSH);
        post.setText("Post");
        post.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { _browser.view(_browser.createPostURI(getPostScope(getSelectedItem()), null, false)); }
            public void widgetSelected(SelectionEvent evt) { _browser.view(_browser.createPostURI(getPostScope(getSelectedItem()), null, false)); }
        });
        
        _manageMenu = new Menu(tree);
        MenuItem manage = new MenuItem(_manageMenu, SWT.PUSH);
        manage.setText("Manage");
        manage.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { _browser.view(_browser.createManageURI(getManageScope(getSelectedItem()))); }
            public void widgetSelected(SelectionEvent evt) { _browser.view(_browser.createManageURI(getManageScope(getSelectedItem()))); }
        });
        
        _searchMenu = new Menu(tree);
        view = new MenuItem(_searchMenu, SWT.PUSH);
        view.setText("View");
        view.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { _browser.view(getSearchResultURI(getSelectedItem())); }
            public void widgetSelected(SelectionEvent evt) { _browser.view(getSearchResultURI(getSelectedItem())); }
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
            box.setMessage("Are you sure you want to delete this bookmark?");
            box.setText("Confirm");
            int rc = box.open();
            if (rc == SWT.YES)
                _browser.deleteBookmark(node.getGroupId());
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
            _browser.getUI().debugMessage("browserTree doubleclick on " + item);
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
            _browser.view(bookmark.getURI());
            return;
        }
        ChannelInfo chan = getPostChannel(item);
        if (chan != null) {
            _browser.view(SyndieURI.createScope(chan.getChannelHash()));
            return;
        }
        chan = getManageChannel(item);
        if (chan != null) {
            _browser.view(SyndieURI.createScope(chan.getChannelHash()));
            return;
        }
        ReferenceNode search = getSearchResult(item);
        if (search != null) {
            _browser.view(search.getURI());
            return;
        }
    }
}
