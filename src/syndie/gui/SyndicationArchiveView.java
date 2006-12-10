package syndie.gui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.SyndieURI;
import syndie.db.ArchiveDiff;
import syndie.db.SyndicationManager;

/**
 *
 */
public class SyndicationArchiveView implements Translatable, Themeable, SyndicationManager.SyndicationListener {
    private BrowserControl _browser;
    private SyndicationManager _manager;
    private Composite _parent;
    private Table _table;
    private TableColumn _colType;
    private TableColumn _colName;
    private TableColumn _colNumForums;
    private TableColumn _colNumMsgs;
    private TableColumn _colNumNewForums;
    private TableColumn _colNumNewMsgs;
    private TableColumn _colLastSync;
    private TableColumn _colCustomProxy;
    private TableColumn _colError;
    private ArrayList _names;
    private Map _errors;    
    private Menu _menu;
    private MenuItem _menuAdd;
    private MenuItem _menuEdit;
    private MenuItem _menuDelete;
    
    private SyndicationArchivePopup _editPopup;
    
    public SyndicationArchiveView(BrowserControl browser, Composite parent) {
        _browser = browser;
        _parent = parent;
        _manager = _browser.getSyndicationManager();
        _manager.addListener(this);
        _names = new ArrayList(16);
        _errors = new HashMap();
        initComponents();
        redrawArchives();
    }
    
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
        _editPopup.dispose();
    }
    
    public Control getControl() { return _table; }
    public void shown() { redrawArchives(); }
    
    public void highlight(SyndieURI uri) {
        if (uri == null) return;
        String url = uri.getURL();
        
        // this could be an already listed one, or a brand new one
        for (int i = 0; i < _manager.getArchiveCount(); i++) {
            SyndieURI cur = _manager.getArchiveURI(i);
            if ((cur.getURL() != null) && (cur.getURL().equals(url))) {
                redrawArchives(_manager.getArchiveName(i));
                return;
            }
        }
    }
    
    private void redrawArchives() { redrawArchives(null); }
    private void redrawArchives(String highlightName) {
        _table.setRedraw(false);
        int selected[] = _table.getSelectionIndices();
        List selectedNames = new ArrayList(selected.length);
        for (int i = 0; i < selected.length; i++)
            selectedNames.add(_names.get(selected[i]));
        int top = _table.getTopIndex();
        if (top < 0) top = 0;
        
        TableItem items[] = _table.getItems();
        for (int i = 0; i < items.length; i++)
            items[i].dispose();
        _table.removeAll();
        _names.clear();
        for (int i = 0; i < _manager.getArchiveCount(); i++) {
            ArchiveDiff diff = _manager.getArchiveDiff(i);
            SyndieURI uri = _manager.getArchiveURI(i);
            String name = _manager.getArchiveName(i);
            String proxyHost = _manager.getCustomProxyHost(i);
            int proxyPort = _manager.getCustomProxyPort(i);
            long lastSync = _manager.getLastSyncDate(i);
            
            if (name.equals(highlightName))
                top = i;
            
            String url = uri.getURL();
            Image img = null;
            if (url != null) {
                if ( (url.indexOf("SSK@") >= 0) || (url.indexOf("CHK@") >= 0) || (url.indexOf("USK@") >= 0) ) {
                    img = ImageUtil.ICON_SYNDICATE_TYPE_FREENET;
                } else if ( (proxyHost != null) && (proxyHost.trim().length() == 0) ) {
                    img = ImageUtil.ICON_SYNDICATE_TYPE_DIRECT;
                } else if (url.startsWith("/") || url.startsWith("file://")) {
                    img = ImageUtil.ICON_SYNDICATE_TYPE_DIRECT;
                } else {
                    img = ImageUtil.ICON_SYNDICATE_TYPE_INDIRECT;
                }
            }
            
            String numForums = (diff == null ? "?" : diff.totalChannels+"");
            String numMsgs = (diff == null ? "?" : diff.totalMessages+"");
            String numNewForums = (diff == null ? "?" : diff.totalNewChannels+"");
            String numNewMsgs = (diff == null ? "?" : diff.totalNewMessages+"");
            String lastSyncStr = (lastSync > 0 ? Constants.getDate(lastSync) : "");
            String proxy = (proxyHost != null && proxyHost.trim().length() > 0 ? proxyHost + ":" + proxyPort : "");
            String error = (String)_errors.get(name);
            if (error == null)
                error = "";
            else
                error = error.trim();
            
            _browser.getUI().debugMessage("archive row: " + name + "/" + numForums + "/" + numMsgs + "/" + numNewForums + "/" + numNewMsgs + "/" + lastSyncStr + "/" + proxy);
            
            TableItem row = new TableItem(_table, SWT.NONE);
            row.setImage(0, img);
            row.setText(1, name);
            row.setText(2, numForums);
            row.setText(3, numMsgs);
            row.setText(4, numNewForums);
            row.setText(5, numNewMsgs);
            row.setText(6, lastSyncStr);
            row.setText(7, proxy);
            row.setText(8, error);
            _names.add(name);
        }
        _browser.getUI().debugMessage("archive rows: " + _names.size());
        
        _table.setTopIndex(top);
        if (highlightName != null) {
            _table.setSelection(top);
        } else if (selectedNames.size() > 0) {
            int indexes[] = new int[selectedNames.size()];
            for (int i = 0; i < selectedNames.size(); i++)
                indexes[i] = _names.indexOf(selectedNames.get(i));
            _table.setSelection(indexes);
        }
        
        _colType.pack();
        _colNumForums.pack();
        _colNumMsgs.pack();
        _colNumNewForums.pack();
        _colNumNewMsgs.pack();
        _colLastSync.pack();
        _colCustomProxy.pack();
        _colError.pack();
        _colName.pack();
        
        _table.setRedraw(true);
    }
    
    private void initComponents() {
        _table = new Table(_parent, SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION);
        _colType = new TableColumn(_table, SWT.RIGHT);
        _colName = new TableColumn(_table, SWT.LEFT);
        _colNumForums = new TableColumn(_table, SWT.LEFT);
        _colNumMsgs = new TableColumn(_table, SWT.LEFT);
        _colNumNewForums = new TableColumn(_table, SWT.LEFT);
        _colNumNewMsgs = new TableColumn(_table, SWT.LEFT);
        _colLastSync = new TableColumn(_table, SWT.LEFT);
        _colCustomProxy = new TableColumn(_table, SWT.LEFT);
        _colError = new TableColumn(_table, SWT.LEFT);
        _colName.setResizable(true);
        _table.setLinesVisible(true);
        _table.setHeaderVisible(true);

        _menu = new Menu(_table);
        _table.setMenu(_menu);
        _menuAdd = new MenuItem(_menu, SWT.PUSH);
        _menuAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { add(); }
            public void widgetSelected(SelectionEvent selectionEvent) { add(); }
        });
        _menuEdit = new MenuItem(_menu, SWT.PUSH);
        _menuEdit.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { edit(); }
            public void widgetSelected(SelectionEvent selectionEvent) { edit(); }
        });
        _menuDelete = new MenuItem(_menu, SWT.PUSH);
        _menuDelete.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { delete(); }
            public void widgetSelected(SelectionEvent selectionEvent) { delete(); }
        });
        
        _editPopup = new SyndicationArchivePopup(_browser, _table.getShell());
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
    }
    
    private void add() { _editPopup.open(); }
    private void edit() {
        int idx = _table.getSelectionIndex();
        if (idx >= 0) {
            String name = (String)_names.get(idx);
            int archive = _manager.getArchiveNum(name);
            SyndieURI uri = _manager.getArchiveURI(archive);
            String proxy = _manager.getCustomProxyHost(archive);
            int port = _manager.getCustomProxyPort(archive);
            _editPopup.config(name, uri, proxy, port);
        }
        _editPopup.open();
    }
    private void delete() {
        ArrayList names = getSelectedNames();
        if (names.size() > 0) {
            MessageBox confirm = new MessageBox(_table.getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO);
            confirm.setText(_browser.getTranslationRegistry().getText(T_CONFIRM_DELETE_TITLE, "Delete archive?"));
            confirm.setMessage(_browser.getTranslationRegistry().getText(T_CONFIRM_DELETE_MESSAGE, "Remove the archive from the monitored list?"));
            int rc = confirm.open();
            if (rc == SWT.YES) {
                for (int i = 0; i < names.size(); i++) {
                    String name = (String)names.get(i);
                    _browser.getUI().debugMessage("Deleting " + name);
                    _manager.delete(name);
                    _names.remove(name);
                }
                _browser.getUI().debugMessage("Archives deleted");
            }
        }
    }

    /** selected archive names, or if none are selected explicitly, all names */
    public ArrayList getSelectedNames() {
        ArrayList rv = new ArrayList();
        int indexes[] = _table.getSelectionIndices();
        if ( (indexes != null) && (indexes.length != 0) ) {
            for (int i = 0; i < indexes.length; i++)
                rv.add(_names.get(indexes[i]));
        } else {
            rv.addAll(_names);
        }
        return rv;
    }

    private static final String T_TYPE = "syndie.gui.syndicationarchiveview.coltype";
    private static final String T_TYPE_TOOLTIP = "syndie.gui.syndicationarchiveview.coltype_tooltip";
    private static final String T_NAME = "syndie.gui.syndicationarchiveview.colname";
    private static final String T_NAME_TOOLTIP = "syndie.gui.syndicationarchiveview.colname_tooltip";
    private static final String T_NUMFORUMS = "syndie.gui.syndicationarchiveview.numforums";
    private static final String T_NUMFORUMS_TOOLTIP = "syndie.gui.syndicationarchiveview.numforums_tooltip";
    private static final String T_NUMMSGS = "syndie.gui.syndicationarchiveview.nummsgs";
    private static final String T_NUMMSGS_TOOLTIP = "syndie.gui.syndicationarchiveview.nummsgs_tooltip";
    private static final String T_NUMNEWFORUMS = "syndie.gui.syndicationarchiveview.numnewforums";
    private static final String T_NUMNEWFORUMS_TOOLTIP = "syndie.gui.syndicationarchiveview.numnewforums_tooltip";
    private static final String T_NUMNEWMSGS = "syndie.gui.syndicationarchiveview.numnewmsgs";
    private static final String T_NUMNEWMSGS_TOOLTIP = "syndie.gui.syndicationarchiveview.numnewmsgs_tooltip";
    private static final String T_LASTSYNC = "syndie.gui.syndicationarchiveview.lastsync";
    private static final String T_LASTSYNC_TOOLTIP = "syndie.gui.syndicationarchiveview.lastsync_tooltip";
    private static final String T_CUSTOMPROXY = "syndie.gui.syndicationarchiveview.customproxy";
    private static final String T_CUSTOMPROXY_TOOLTIP = "syndie.gui.syndicationarchiveview.customproxy_tooltip";
    private static final String T_ERROR = "syndie.gui.syndicationarchiveview.error";
    private static final String T_ERROR_TOOLTIP = "syndie.gui.syndicationarchiveview.error_tooltip";
    
    private static final String T_MENU_ADD = "syndie.gui.syndicationarchiveview.menuadd";
    private static final String T_MENU_EDIT = "syndie.gui.syndicationarchiveview.menuedit";
    private static final String T_MENU_DELETE = "syndie.gui.syndicationarchiveview.menudelete";

    private static final String T_CONFIRM_DELETE_TITLE = "syndie.gui.syndicationarchiveview.delete_title";
    private static final String T_CONFIRM_DELETE_MESSAGE = "syndie.gui.syndicationarchiveview.delete_message";
    
    public void translate(TranslationRegistry registry) {
        _colType.setText(registry.getText(T_TYPE, ""));
        _colType.setToolTipText(registry.getText(T_TYPE_TOOLTIP, "What type of archive this refers to"));
        _colName.setText(registry.getText(T_NAME, "Name"));
        _colName.setToolTipText(registry.getText(T_TYPE_TOOLTIP, "Local nickname of the archive"));
        _colNumForums.setText(registry.getText(T_NUMFORUMS, "# forums"));
        _colNumForums.setToolTipText(registry.getText(T_NUMFORUMS_TOOLTIP, "How many forums they knew, as of the last sync date"));
        _colNumMsgs.setText(registry.getText(T_NUMMSGS, "# msgs"));
        _colNumMsgs.setToolTipText(registry.getText(T_NUMMSGS_TOOLTIP, "How many messages they knew, as of the last sync date"));
        _colNumNewForums.setText(registry.getText(T_NUMNEWFORUMS, "# new forums"));
        _colNumNewForums.setToolTipText(registry.getText(T_NUMNEWFORUMS_TOOLTIP, "How many new forums they knew, as of the last sync date"));
        _colNumNewMsgs.setText(registry.getText(T_NUMNEWMSGS, "# new msgs"));
        _colNumNewMsgs.setToolTipText(registry.getText(T_NUMNEWMSGS_TOOLTIP, "How many new messages they knew, as of the last sync date"));
        _colLastSync.setText(registry.getText(T_LASTSYNC, "last sync"));
        _colLastSync.setToolTipText(registry.getText(T_LASTSYNC_TOOLTIP, "When did we last fetch their index?"));
        _colCustomProxy.setText(registry.getText(T_CUSTOMPROXY, "proxy"));
        _colCustomProxy.setToolTipText(registry.getText(T_CUSTOMPROXY_TOOLTIP, "Proxy override"));
        _colError.setText(registry.getText(T_ERROR, ""));
        _colError.setToolTipText(registry.getText(T_ERROR_TOOLTIP, "Any error from the last index sync"));
        
        _menuAdd.setText(registry.getText(T_MENU_ADD, "Add"));
        _menuEdit.setText(registry.getText(T_MENU_EDIT, "Edit"));
        _menuDelete.setText(registry.getText(T_MENU_DELETE, "Delete"));
    }
    
    public void applyTheme(Theme theme) {
        _table.setFont(theme.TABLE_FONT);
        _colType.setWidth(24);
        _colNumForums.pack();
        _colNumMsgs.pack();
        _colNumNewForums.pack();
        _colNumNewMsgs.pack();
        _colLastSync.pack();
        _colCustomProxy.pack();
        _colError.pack();
        _colName.pack();
    }

    public void archiveAdded(SyndicationManager mgr, String name) { redrawArchives(name); }
    public void archiveRemoved(SyndicationManager mgr, String name) { 
        _browser.getUI().debugMessage("redraw after deleting " + name);
        _table.getDisplay().asyncExec(new Runnable() { public void run() { redrawArchives(); } });
    }
    public void archiveUpdated(SyndicationManager mgr, String oldName, final String newName) {
        _table.getDisplay().asyncExec(new Runnable() { public void run() { redrawArchives(newName); } });
    }
    
    public void archiveIndexStatus(SyndicationManager mgr, final SyndicationManager.StatusRecord record) {
        final String archiveName = record.getSource();
        String msg = record.getDetail();
        if (msg != null)
            _errors.put(archiveName, msg);
        else
            _errors.put(archiveName, "");
        _table.getDisplay().asyncExec(new Runnable() {
            public void run() { redrawArchives(); }
        });
    }

    public void fetchStatusUpdated(SyndicationManager mgr, SyndicationManager.StatusRecord record) {}
    public void archivesLoaded(SyndicationManager mgr) { 
        _table.getDisplay().asyncExec(new Runnable() { public void run() { redrawArchives(); } });
    }
    public void syndicationComplete(SyndicationManager mgr) {}
}
