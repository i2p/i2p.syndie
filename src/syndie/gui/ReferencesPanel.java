package syndie.gui;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.MenuListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.NymReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.UI;

/**
 *
 */
public class ReferencesPanel extends BaseComponent implements Translatable, Themeable {
    private NavigationControl _nav;
    private BookmarkControl _bookmark;
    private Composite _parent;
    private Composite _root;
    private Tree _tree;
    private TreeColumn _colName;
    private TreeColumn _colType;
    private TreeColumn _colDesc;
    private Button _addFolder;
    private Button _addReference;
    private Button _delete;
    private Button _close;
    private Runnable _onClose;
    private Set _createdImages;
    
    public ReferencesPanel(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl nav, BookmarkControl bookmark, Composite parent, Runnable onClose) {
        super(client, ui, themes, trans);
        _nav = nav;
        _bookmark = bookmark;
        _parent = parent;
        _onClose = onClose;
        initComponents();
    }
    
    public Control getRoot() { return _root; }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        GridLayout gl = new GridLayout(4, true);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        _root.setLayout(gl);
        
        _tree = new Tree(_root, SWT.BORDER | SWT.MULTI);
        _tree.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 4, 1));
        _tree.setHeaderVisible(true);
        _tree.setLinesVisible(true);
        _colName = new TreeColumn(_tree, SWT.LEFT);
        _colType = new TreeColumn(_tree, SWT.CENTER);
        _colDesc = new TreeColumn(_tree, SWT.LEFT);
        
        final Menu menu = new Menu(_tree);
        _tree.setMenu(menu);
        menu.addMenuListener(new MenuListener() {
            public void menuHidden(MenuEvent menuEvent) {}
            public void menuShown(MenuEvent menuEvent) { buildMenu(menu); }
        });
        SyndieTreeListener lsnr = new SyndieTreeListener(_tree) {
            /** the tree's selection was updated */
            public void selected() {
            }
            /** the user doubleclicked on the selected row */
            public void doubleclick() {
                TreeItem item = getSelected();
                viewSelected(item);
            }
            /** the user hit return on the selected row */
            public void returnHit() {
                TreeItem item = getSelected();
                viewSelected(item);
            }

        };
        _tree.addSelectionListener(lsnr);
        _tree.addTraverseListener(lsnr);
        _tree.addKeyListener(lsnr);
        _tree.addMouseListener(lsnr);
        
        _addFolder = new Button(_root, SWT.PUSH);
        _addFolder.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _addReference = new Button(_root, SWT.PUSH);
        _addReference.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _delete = new Button(_root, SWT.PUSH);
        _delete.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _close = new Button(_root, SWT.PUSH);
        _close.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _close.addSelectionListener(new FireSelectionListener() { public void fire() { close(); } });
        
        loadData();
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    public void close() {
        _onClose.run();
        dispose();
    }
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        for (Iterator iter = _createdImages.iterator(); iter.hasNext(); )
            ImageUtil.dispose((Image)iter.next());
    }
    
    private void buildMenu(Menu menu) {
        MenuItem items[] = menu.getItems();
        for (int i = 0; i < items.length; i++) items[i].dispose();
        
        TreeItem sel[] = _tree.getSelection();
        if ( (sel == null) || (sel.length == 0) ) return;
        if (sel.length == 1)
            buildMenu(menu, sel[0]);
        else
            buildMenu(menu, sel);
    }
    private void buildMenu(Menu menu, final TreeItem sel) {
        NymReferenceNode node = (NymReferenceNode)sel.getData("node");
        final SyndieURI uri = node.getURI();
        
        MenuItem view = new MenuItem(menu, SWT.PUSH);
        view.setText(_translationRegistry.getText("View"));
        if (uri != null)
            view.addSelectionListener(new FireSelectionListener() { public void fire() { viewSelected(sel); } });
        else
            view.setEnabled(false);
        
        // view all makes sense in the tabbed view, but not in the desktop
        /*
        MenuItem viewAll = new MenuItem(menu, SWT.PUSH);
        view.setText(_translationRegistry.getText("View children"));
        if (node.getChildCount() > 0) {
            view.addSelectionListener(new FireSelectionListener() { 
                public void fire() {
                    _nav.view(uri);
                } 
            });
        }
        else
            viewAll.setEnabled(false);
         */
        
        MenuItem copy = new MenuItem(menu, SWT.PUSH);
        copy.setText(_translationRegistry.getText("Copy target URI"));
        copy.addSelectionListener(new FireSelectionListener() { 
            public void fire() {
                TextTransfer tt = TextTransfer.getInstance();
                Clipboard clip = new Clipboard(_root.getDisplay());
                Transfer xf[] = new Transfer[] { tt };
                Object data[] = new Object[] { uri.toString() };
                clip.setContents(data, xf);
                clip.dispose();
            } 
        });
        
        MenuItem edit = new MenuItem(menu, SWT.PUSH);
        edit.setText(_translationRegistry.getText("Edit"));
        edit.addSelectionListener(new FireSelectionListener() { public void fire() { edit(sel); } });
        
        MenuItem delete = new MenuItem(menu, SWT.PUSH);
        delete.setText(_translationRegistry.getText("Delete"));
        delete.addSelectionListener(new FireSelectionListener() { public void fire() { delete(sel); } });
    }
    
    private void buildMenu(Menu menu, final TreeItem sel[]) {
        MenuItem view = new MenuItem(menu, SWT.PUSH);
        view.setText(_translationRegistry.getText("View"));
        view.setImage(ImageUtil.ICON_VIEW);
        view.setEnabled(false);
        
        MenuItem edit = new MenuItem(menu, SWT.PUSH);
        edit.setText(_translationRegistry.getText("Edit"));
        edit.setImage(ImageUtil.ICON_EDIT);
        edit.addSelectionListener(new FireSelectionListener() { public void fire() { edit(sel); } });
        
        MenuItem delete = new MenuItem(menu, SWT.PUSH);
        delete.setText(_translationRegistry.getText("Delete"));
        delete.setImage(ImageUtil.ICON_DELETE);
        delete.addSelectionListener(new FireSelectionListener() { public void fire() { delete(sel); } });
    }
    
    private void viewSelected(TreeItem item) {
        if (item != null) {
            NymReferenceNode node = (NymReferenceNode)item.getData("node");
            SyndieURI uri = node.getURI();
            if (uri != null) {
                _nav.view(uri, node.getName(), node.getDescription());
                close();
            }
        }
    }
    private void delete(TreeItem item) {}
    private void delete(TreeItem item[]) {}
    private void edit(TreeItem item) {}
    private void edit(TreeItem item[]) {}
    
    public void loadData() {
        _tree.setRedraw(false);
        JobRunner.instance().enqueue(new Runnable() { public void run() { asyncLoadData(); } });
    }
    private void asyncLoadData() {
        final List bookmarks = _bookmark.getBookmarks();
        final Map nodeToAvatarData = new HashMap();
        for (int i = 0; i < bookmarks.size(); i++)
            loadAvatarData((NymReferenceNode)bookmarks.get(i), nodeToAvatarData);
        
        _root.getDisplay().asyncExec(new Runnable() { 
            public void run() {
                Set createdImages = new HashSet();
                for (int i = 0; i < bookmarks.size(); i++)
                    syncLoad((NymReferenceNode)bookmarks.get(i), null, nodeToAvatarData, createdImages);
                _createdImages = createdImages;
                _colName.pack();
                _colType.pack();
                _colDesc.pack();
                _tree.setRedraw(true);
            }
        });
    }
    private void syncLoad(NymReferenceNode node, TreeItem parent, Map nodeToAvatarData, Set createdImages) {
        SyndieURI uri = node.getURI();
        String name = node.getName();
        String desc = node.getDescription();
        
        Image avatar = null;
        byte data[] = (byte[])nodeToAvatarData.get(node);
        if (data != null) {
            avatar = ImageUtil.createImage(data);
            if (avatar != null)
                createdImages.add(avatar);
        }
        if ( (avatar == null) && (uri != null) )
            avatar = ImageUtil.ICON_EDITOR_BOOKMARKED_NOAVATAR;
        //if (img == null)
        //    img = ImageUtil.getTypeIcon(uri);
        Image type = ImageUtil.getTypeIcon(uri);
        
        TreeItem item = null;
        if (parent == null)
            item = new TreeItem(_tree, SWT.NONE);
        else
            item = new TreeItem(parent, SWT.NONE);
        item.setData("node", node);
        
        item.setImage(avatar);
        item.setText(name != null ? name : "");
        
        item.setImage(1, type);
        item.setText(2, desc != null ? desc : "");
        
        for (int i = 0; i < node.getChildCount(); i++)
            syncLoad((NymReferenceNode)node.getChild(i), item, nodeToAvatarData, createdImages);
        item.setExpanded(true);
    }
    
    private void loadAvatarData(NymReferenceNode node, Map nodeToAvatarData) {
        SyndieURI uri = node.getURI();
        Image img = null;
        if (uri == null) {
            // use nothing
        } else if (uri.isChannel() && (uri.getScope() != null) ) {
            // use the channel's avatar
            long chanId = _client.getChannelId(uri.getScope());
            if (chanId >= 0) {
                final byte avatar[] = _client.getChannelAvatar(chanId);
                if (avatar != null)
                    nodeToAvatarData.put(node, avatar);
            }
        } else if (uri.isSearch()) {
            Hash scopes[] = uri.getSearchScopes();
            if ( (scopes != null) && (scopes.length == 1) ) {
                // use the channel's avatar
                long chanId = _client.getChannelId(scopes[0]);
                if (chanId >= 0) {
                    final byte avatar[] = _client.getChannelAvatar(chanId);
                    if (avatar != null)
                        nodeToAvatarData.put(node, avatar);
                }
            }
        }
        for (int i = 0; i < node.getChildCount(); i++)
            loadAvatarData((NymReferenceNode)node.getChild(i), nodeToAvatarData);
    }
    
    
    public void applyTheme(Theme theme) {
        _tree.setFont(theme.TREE_FONT);
        _addFolder.setFont(theme.BUTTON_FONT);
        _addReference.setFont(theme.BUTTON_FONT);
        _delete.setFont(theme.BUTTON_FONT);
        _close.setFont(theme.BUTTON_FONT);
    }
    
    public void translate(TranslationRegistry trans) {
        _addFolder.setText(trans.getText("Add folder"));
        _addReference.setText(trans.getText("Add reference"));
        _delete.setText(trans.getText("Delete"));
        _close.setText(trans.getText("Close"));
        _colName.setText(trans.getText("Name"));
        _colType.setText(trans.getText("Type"));
        _colDesc.setText(trans.getText("Description"));
    }
}
