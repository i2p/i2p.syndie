package syndie.gui;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.MenuListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import syndie.data.ArchiveInfo;
import syndie.data.SyndieURI;

/**
 *
 */
public class ManageForumArchiveChooser implements Translatable, Themeable {
    private BrowserControl _browser;
    private ManageForum _forum;
    private Composite _parent;
    private Table _table;
    private TableColumn _colName;
    private TableColumn _colType;
    private TableColumn _colPublic;
    private TableColumn _colLocation;
    private TableColumn _colDesc;
    private MenuItem _view;
    private MenuItem _add;
    private MenuItem _edit;
    private MenuItem _remove;
    private MenuItem _setPublic;
    private ArrayList _uris;
    
    private AddToForumPopup _addPopup;
    private AddToLocalPopup _addToLocalArchive;
    
    private boolean _editable;
    
    public ManageForumArchiveChooser(Composite parent, BrowserControl browser, ManageForum forum, boolean editable) {
        _browser = browser;
        _forum = forum;
        _parent = parent;
        _editable = editable;
        _uris = new ArrayList();
        initComponents();
    }
    
    private void initComponents() {
        _table = new Table(_parent, SWT.MULTI | SWT.FULL_SELECTION | SWT.BORDER);
        _colName = new TableColumn(_table, SWT.LEFT);
        _colType = new TableColumn(_table, SWT.CENTER);
        _colPublic = new TableColumn(_table, SWT.CENTER);
        _colLocation = new TableColumn(_table, SWT.LEFT);
        _colDesc = new TableColumn(_table, SWT.LEFT);
        _table.setLinesVisible(true);
        _table.setHeaderVisible(true);
        
        Menu menu = new Menu(_table);
        _table.setMenu(menu);
        _view = new MenuItem(menu, SWT.PUSH);
        _view.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { view(); }
            public void widgetSelected(SelectionEvent selectionEvent) { view(); }
        });
        _add = new MenuItem(menu, SWT.PUSH);
        _add.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { add(); }
            public void widgetSelected(SelectionEvent selectionEvent) { add(); }
        });
        _add.setEnabled(_editable);
        _edit = new MenuItem(menu, SWT.PUSH);
        _edit.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { edit(); }
            public void widgetSelected(SelectionEvent selectionEvent) { edit(); }
        });
        _edit.setEnabled(_editable);
        _remove = new MenuItem(menu, SWT.PUSH);
        _remove.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { remove(); }
            public void widgetSelected(SelectionEvent selectionEvent) { remove(); }
        });
        _remove.setEnabled(_editable);
        _setPublic = new MenuItem(menu, SWT.CHECK);
        _setPublic.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { setPublic(); }
            public void widgetSelected(SelectionEvent selectionEvent) { setPublic(); }
        });
        _setPublic.setEnabled(_editable);
        menu.addMenuListener(new MenuListener() {
            public void menuHidden(MenuEvent menuEvent) {}
            public void menuShown(MenuEvent evt) { configMenu(); }
        });
        
        _addPopup = new AddToForumPopup();
        _addToLocalArchive = new AddToLocalPopup();
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
    }
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
        _addPopup.dispose();
        _addToLocalArchive.dispose();
    }
    
    private void configMenu() {
        int indexes[] = _table.getSelectionIndices();
        if ( (indexes != null) && (indexes.length == 1) ) {
            _setPublic.setEnabled(true);
            TableItem item = _table.getItem(indexes[0]);
            _setPublic.setSelection("X".equals(item.getText(2)));
        } else {
            _setPublic.setEnabled(false);
        }
    }
    private void setPublic() {
        int indexes[] = _table.getSelectionIndices();
        if ( (indexes != null) && (indexes.length == 1) ) {
            if (_setPublic.getEnabled()) {
                TableItem item = _table.getItem(indexes[0]);
                if (_setPublic.getSelection())
                    item.setText(2, "X");
                else
                    item.setText(2, "");
            }
        }
    }
    private void view() {
        int indexes[] = _table.getSelectionIndices();
        if ( (indexes != null) && (indexes.length == 1) ) {
            SyndieURI uri = (SyndieURI)_uris.get(indexes[0]);
            _addToLocalArchive.config(uri, null, -1);
            _addToLocalArchive.open();
        }
    }
    private void remove() {
        int indexes[] = _table.getSelectionIndices();
        if ( (indexes != null) && (indexes.length == 1) ) {
            TableItem item = _table.getItem(indexes[0]);
            item.dispose();
        }
    }
    private void edit() {
        int indexes[] = _table.getSelectionIndices();
        if ( (indexes != null) && (indexes.length == 1) ) {
            TableItem item = _table.getItem(indexes[0]);
            SyndieURI uri = (SyndieURI)_uris.get(indexes[0]);
            _addPopup.edit(item, uri, "X".equals(item.getText(2)));
        }
    }
    
    private void add() { _addPopup.open(); }
    
    /** set of ArchiveInfo that should be publicly visible */
    public Set getPublicArchives() { 
        Set rv = new HashSet();
        for (int i = 0; i < _uris.size(); i++) {
            SyndieURI uri = (SyndieURI)_uris.get(i);
            TableItem item = _table.getItem(i);
            if ("X".equals(item.getText(2)))
                rv.add(new ArchiveInfo(uri));
        }
        return rv;
    }
    
    /** set of ArchiveInfo that should be encrypted within the metadata */
    public Set getPrivateArchives() {
        Set rv = new HashSet();
        for (int i = 0; i < _uris.size(); i++) {
            SyndieURI uri = (SyndieURI)_uris.get(i);
            TableItem item = _table.getItem(i);
            if (!"X".equals(item.getText(2)))
                rv.add(new ArchiveInfo(uri));
        }
        return rv;
    }
    
    public void setArchives(Set publicArchives, Set privateArchives) {
        _table.setRedraw(false);
        _uris.clear();
        
        if (publicArchives != null) {
            for (Iterator iter = publicArchives.iterator(); iter.hasNext(); ) {
                ArchiveInfo info = (ArchiveInfo)iter.next();
                add(info.getURI(), true, null, null);
            }
        }
        if (privateArchives != null) {
            for (Iterator iter = privateArchives.iterator(); iter.hasNext(); ) {
                ArchiveInfo info = (ArchiveInfo)iter.next();
                add(info.getURI(), false, null, null);
            }
        }
        
        _colDesc.pack();
        _colLocation.pack();
        _colName.pack();
        _colPublic.pack();
        _colType.pack();
        _table.setRedraw(true);
    }
    
    //private void add(String name, SyndieURI uri, boolean selected, boolean asPublic) {
    private void add(SyndieURI uri, boolean asPublic, TableItem item, SyndieURI oldURI) {
        String name = uri.getString("name");
        String desc = uri.getString("desc");
        //String tags[] = uri.getStringArray("tag");
        
        if (item == null)
            item = new TableItem(_table, SWT.NONE);
        if (name != null)
            item.setText(0, name);
        else
            item.setText(0, "");
        
        int type = getType(uri);
        if (type == TYPE_SYNDIE)
            item.setImage(1, ImageUtil.ICON_ARCHIVE_TYPE_SYNDIE);
        else if (type == TYPE_URL)
            item.setImage(1, ImageUtil.ICON_ARCHIVE_TYPE_URL);
        else if (type == TYPE_FREENET)
            item.setImage(1, ImageUtil.ICON_ARCHIVE_TYPE_FREENET);
        else if (type == TYPE_FILE)
            item.setImage(1, ImageUtil.ICON_ARCHIVE_TYPE_FILE);
        else
            item.setImage(1, null);
        item.setText(2, asPublic ? "X" : "");
        item.setText(3, getLocation(uri));
        if (desc != null)
            item.setText(4, desc);
        else
            item.setText(4, "");
        
        if (oldURI == null) {
            _uris.add(uri);
        } else {
            int idx = _uris.indexOf(oldURI);
            _uris.set(idx, uri);
        }
    }
    
    private static final int TYPE_SYNDIE = 0;
    private static final int TYPE_FREENET = 1;
    private static final int TYPE_FILE = 2;
    private static final int TYPE_URL = 3;
    
    public static final String getLocation(SyndieURI uri) {
        if (uri != null) {
            String url = uri.getURL();
            if (url == null) {
                return uri.toString();
            } else if ( (url.indexOf("SSK@") >= 0) || (url.indexOf("CHK@") >= 0) || (url.indexOf("USK@") >= 0) ) {
                int idx = url.indexOf("CHK@");
                if (idx < 0)
                    idx = url.indexOf("SSK@");
                if (idx < 0)
                    idx = url.indexOf("USK@");
                int end = url.indexOf('?', idx);
                if (end > 0)
                    return url.substring(idx, end);
                else
                    return url.substring(idx);
            } else if ( url.startsWith("/") || 
                        ((url.length() > 2) && (url.charAt(1) == ':') && (url.charAt(2) == '\\')) || 
                        (url.startsWith("file://")) ) {
                return url;
            } else {
                return url;
            }
        } else {
            return "";
        }
    }
    
    private int getType(SyndieURI uri) {
        if (uri != null) {
            String url = uri.getURL();
            if (url == null) {
                return TYPE_SYNDIE;
            } else if ( (url.indexOf("SSK@") >= 0) || (url.indexOf("CHK@") >= 0) || (url.indexOf("USK@") >= 0) ) {
                return TYPE_FREENET;
            } else if ( url.startsWith("/") || 
                        ((url.length() > 2) && (url.charAt(1) == ':') && (url.charAt(2) == '\\')) || 
                        (url.startsWith("file://")) ) {
                return TYPE_FILE;
            } else {
                return TYPE_URL;
            }
        } else {
            return TYPE_SYNDIE;
        }
    }
    
    private static final String T_NAME = "syndie.gui.manageforumarchivechooser.name";
    private static final String T_TYPE = "syndie.gui.manageforumarchivechooser.type";
    private static final String T_PUBLIC = "syndie.gui.manageforumarchivechooser.public";
    private static final String T_PUBLIC_TOOLTIP = "syndie.gui.manageforumarchivechooser.public_tooltip";
    private static final String T_LOCATION = "syndie.gui.manageforumarchivechooser.location";
    private static final String T_DESC = "syndie.gui.manageforumarchivechooser.desc";
    private static final String T_SETPUBLIC = "syndie.gui.manageforumarchivechooser.setpublic";
    private static final String T_ADD = "syndie.gui.manageforumarchivechooser.add";
    private static final String T_EDIT = "syndie.gui.manageforumarchivechooser.edit";
    private static final String T_REMOVE = "syndie.gui.manageforumarchivechooser.remove";
    private static final String T_VIEW = "syndie.gui.manageforumarchivechooser.view";
    
    public void translate(TranslationRegistry registry) {
        _colName.setText(registry.getText(T_NAME, "Name"));
        _colType.setText(registry.getText(T_TYPE, "Type"));
        _colPublic.setText(registry.getText(T_PUBLIC, "Public?"));
        _colPublic.setToolTipText(registry.getText(T_PUBLIC_TOOLTIP, "Can anyone see this archive, or only those who can read the post?"));
        _colLocation.setText(registry.getText(T_LOCATION, "Location"));
        _colDesc.setText(registry.getText(T_DESC, "Description"));
        
        _view.setText(registry.getText(T_VIEW, "View"));
        _add.setText(registry.getText(T_ADD, "Add"));
        _edit.setText(registry.getText(T_ADD, "Edit"));
        _remove.setText(registry.getText(T_ADD, "Remove"));
        _setPublic.setText(registry.getText(T_SETPUBLIC, "Public?"));
    }
    
    public void applyTheme(Theme theme) { _table.setFont(theme.TABLE_FONT); }
    
    private class AddToForumPopup extends SyndicationArchivePopup {
        private TableItem _curItem;
        private SyndieURI _curURI;
        private boolean _asPublic;
        public AddToForumPopup() {
            super(_browser, _parent.getShell(), false);
        }
        protected void fireAccept(String oldName, SyndieURI uri, String proxy, int port) {
            add(uri, _asPublic, _curItem, _curURI);
            _curItem = null;
            _curURI = null;
            _asPublic = false;
        }
        public void edit(TableItem item, SyndieURI uri, boolean asPublic) {
            config(uri, null, -1);
            _curItem = item;
            _curURI = uri;
            _asPublic = asPublic;
            _addPopup.open();
        }
        
    }
    
    private class AddToLocalPopup extends SyndicationArchivePopup {
        public AddToLocalPopup() {
            super(_browser, _parent.getShell(), false);
        }
        protected void fireAccept(String oldName, SyndieURI uri, String proxy, int port) {
            super.fireAccept(oldName, uri, proxy, port);
            _browser.view(uri);
        }    
    }
}
