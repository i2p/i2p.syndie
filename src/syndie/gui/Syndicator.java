package syndie.gui;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.MenuListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TreeEvent;
import org.eclipse.swt.events.TreeListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import syndie.Constants;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.SyncArchive;
import syndie.db.SyncManager;
import syndie.db.UI;

/**
 *
 */
public class Syndicator extends BaseComponent implements Translatable, Themeable, SyncManager.SyncListener, SyncArchive.SyncArchiveListener {
    private NavigationControl _navControl;
    private Composite _parent;
    
    private Composite _root;
    private Composite _actions;
    private Tree _tree;
    private TreeColumn _colName;
    private TreeColumn _colTime;
    private TreeColumn _colStatus;
    private TreeColumn _colSummary;
    private Menu _treeMenu;
    private Disposable _detail;
    
    private Button _add;
    private Button _cancel;
    private Button _delete;
    
    private Map _archiveNameToRootItem;
    private Map _archiveNameToIndexItem;
    /** name to TreeItem for the subtree item */
    private Map _archiveNameToIncomingItem;
    /** name to Map of SyndieURI to TreeItem for pending fetches */
    private Map _archiveNameToIncoming;
    /** name to TreeItem for the subtree item */
    private Map _archiveNameToOutgoingItem;
    /** name to Map of SyndieURI to TreeItem for pending fetches */
    private Map _archiveNameToOutgoing;

    /** 
     * map of TreeItem to descriptive detail about the item they represent.  The values
     * are of different types, depending on the item selected.  For archive roots,
     * they contain a SyncArchive.  For the elements directly under the archive root, they
     * contain a string ("fetchindex", "incoming", "outgoing"), and for elements under
     * the incoming and outgoing branches, they contain SyncArchive.InboundAction and 
     * SyncArchive.OutboundAction elements, respectively
     */
    private Map _items;
    
    private boolean _disposed;
    
    private boolean _showActions;
    
    public Syndicator(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl navControl, Composite parent) {
        this(client, ui, themes, trans, navControl, parent, true);
    }
    public Syndicator(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl navControl, Composite parent, boolean showActions) {
        super(client, ui, themes, trans);
        _navControl = navControl;
        _parent = parent;
        _showActions = showActions;
        
        _archiveNameToRootItem = new HashMap();
        _archiveNameToIndexItem = new HashMap();
        _archiveNameToIncomingItem = new HashMap();
        _archiveNameToIncoming = new HashMap();
        _archiveNameToOutgoingItem = new HashMap();
        _archiveNameToOutgoing = new HashMap();
        _items = new HashMap();
        
        _disposed = false;

        initComponents();
    }
    
    interface SyndicationDetailListener {
        public void cancelled();
        public void saved();
        public void scheduleUpdated();
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        _root.setLayout(gl);
        
        if (_showActions) {
            _actions = new Composite(_root, SWT.NONE);
            _actions.setLayout(new FillLayout(SWT.HORIZONTAL));
            _actions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));

            // actual action buttons...
            _add = new Button(_actions, SWT.PUSH);
            _cancel = new Button(_actions, SWT.PUSH);
            _delete = new Button(_actions, SWT.PUSH);

            _add.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { add(); }
                public void widgetSelected(SelectionEvent selectionEvent) { add(); }
            });
            _cancel.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { cancel(); }
                public void widgetSelected(SelectionEvent selectionEvent) { cancel(); }
            });
            _delete.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { delete(); }
                public void widgetSelected(SelectionEvent selectionEvent) { delete(); }
            });
        }
        
        _tree = new Tree(_root, SWT.FULL_SELECTION | SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        _tree.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _tree.setHeaderVisible(true);
        _tree.setLinesVisible(true);
        
        _colName = new TreeColumn(_tree, SWT.LEFT);
        _colTime = new TreeColumn(_tree, SWT.LEFT);
        _colStatus = new TreeColumn(_tree, SWT.CENTER);
        _colSummary = new TreeColumn(_tree, SWT.LEFT);
        
        SyndieTreeListener lsnr = new SyndieTreeListener(_tree) {
            public void selected() {
                TreeItem items[] = _tree.getSelection();
                if ( (items != null) && (items.length == 1) )
                    fireSelection(items[0], false);
            }
            public void doubleclick() {
                TreeItem items[] = _tree.getSelection();
                if ( (items != null) && (items.length == 1) )
                    fireSelection(items[0], true);
            }
            public void returnHit() {
                TreeItem items[] = _tree.getSelection();
                if ( (items != null) && (items.length == 1) )
                    fireSelection(items[0], true);
            }
        };
        _tree.addKeyListener(lsnr);
        _tree.addMouseListener(lsnr);
        _tree.addSelectionListener(lsnr);
        _tree.addTraverseListener(lsnr);
        
        _treeMenu = new Menu(_tree);
        _tree.setMenu(_treeMenu);
        _treeMenu.addMenuListener(new MenuListener() {
            public void menuHidden(MenuEvent menuEvent) {}
            public void menuShown(MenuEvent evt) { buildMenu(); }
        });
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
        
        loadData();
    }
    
    public void show(SyndieURI uri) {
        if (uri.isArchive()) {
            SyncManager mgr = SyncManager.getInstance(_client, _ui);
            int cnt = mgr.getArchiveCount();
            for (int i = 0; i < cnt; i++) {
                SyncArchive archive = mgr.getArchive(i);
                if (archive.getURL().equals(uri.getURL())) {
                    _ui.debugMessage("viewing [" + archive.getURL() + "] [" + archive.getName() + "]");
                    TreeItem item = (TreeItem)_archiveNameToRootItem.get(archive.getName());
                    if (item != null) {
                        _tree.setSelection(item);
                        viewDetailArchive(archive, false);
                    }
                    return;
                }
            }
            // it isn't an existing archive, so show it in the details pane w/out a treeitem
            _tree.setSelection(new TreeItem[0]);
            SyncArchive archive = new SyncArchive(mgr, _client);
            archive.setURL(uri.getURL());
            archive.setName(uri.getString("name"));
            _ui.debugMessage("editing as if we are viewing [" + archive.getURL() + "] [" + archive.getName() + "]");
            viewDetailArchive(archive, false);
        }
    }
    
    public void dispose() {
        _disposed = true;
        if (_detail != null) _detail.dispose();
        SyncManager mgr = SyncManager.getInstance(_client, _ui);
        mgr.removeListener(this, this);
        
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
    }
    
    public void add() {
        SyncManager mgr = SyncManager.getInstance(_client, _ui);
        viewDetailArchive(new SyncArchive(mgr, _client), false);
    }
    private void cancel() {
        SyncManager mgr = SyncManager.getInstance(_client, _ui);
        for (int i = 0; i < mgr.getArchiveCount(); i++) {
            SyncArchive archive = mgr.getArchive(i);
            archive.stop(_translationRegistry.getText(T_CANCELLED, "Cancelled"));
        }
    }
    private static final String T_CANCELLED = "syndie.gui.syndicator.cancelled";
    public void delete() { 
        if (_tree.getSelectionCount() != 1) return;
        TreeItem item = _tree.getSelection()[0];
        Object val = _items.get(item);
        if (val == null) return;
        if (val instanceof SyncArchive) {
            SyncArchive archive = (SyncArchive)val;
            MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO);
            box.setMessage(_translationRegistry.getText(T_DELETE_CONFIRM, "Are you sure you want to delete this archive? ") + archive.getName());
            box.setText(_translationRegistry.getText(T_DELETE_CONFIRM_TITLE, "Delete?"));
            int rc = box.open();
            if (rc == SWT.YES)
                archive.delete();
        }
    }
    
    private static final String T_DELETE_CONFIRM = "syndie.gui.syndicator.delete.confirm";
    private static final String T_DELETE_CONFIRM_TITLE = "syndie.gui.syndicator.delete.confirm.title";
    
    private void buildMenu() {
        // depending on what type of item is selected, we want to show different options
        MenuItem items[] = _treeMenu.getItems();
        for (int i = 0; i < items.length; i++) items[i].dispose();
        if (_tree.getSelectionCount() != 1) return;
        TreeItem item = _tree.getSelection()[0];
        Object val = _items.get(item);
        if (val == null) return;
        if (val instanceof SyncArchive) {
            buildMenuArchive((SyncArchive)val);
        } else if (val instanceof String) {
            if ("incoming".equals(val))
                buildMenuIncoming(item);
            else if ("outgoing".equals(val))
                buildMenuOutgoing(item);
            else if ("fetchindex".equals(val))
                buildMenuFetchIndex(item);
        } else if (val instanceof SyncArchive.IncomingAction) {
            buildMenuIncoming((SyncArchive.IncomingAction)val);
        } else if (val instanceof SyncArchive.OutgoingAction) {
            buildMenuOutgoing((SyncArchive.OutgoingAction)val);
        } else {
            // unknown type
            _ui.debugMessage("Unknown type is selected: " + val + " for " + item);
        }
    }
    private void fireSelection(TreeItem item, boolean fireDefaultAction) {
        // depending on what type of item is selected, show a different details panel
        Object val = _items.get(item);
        if (val == null) return;
        if (val instanceof SyncArchive) {
            if (!fireDefaultAction)
                return;
            viewDetailArchive((SyncArchive)val, fireDefaultAction);
        } else if (val instanceof String) {
            if ("incoming".equals(val))
                viewDetailIncoming(item, fireDefaultAction);
            else if ("outgoing".equals(val))
                viewDetailOutgoing(item, fireDefaultAction);
            else if ("fetchindex".equals(val))
                viewDetailFetchIndex(item, fireDefaultAction);
        } else if (val instanceof SyncArchive.IncomingAction) {
            viewDetailIncoming((SyncArchive.IncomingAction)val, fireDefaultAction);
        } else if (val instanceof SyncArchive.OutgoingAction) {
            viewDetailOutgoing((SyncArchive.OutgoingAction)val, fireDefaultAction);
        } else {
            // unknown type
            _ui.debugMessage("Unknown type is selected: " + val + " for " + item);
        }
    }
    
    public void syncNowRecurring() {
        if (_tree.getSelectionCount() != 1) return;
        TreeItem item = _tree.getSelection()[0];
        Object val = _items.get(item);
        if (val == null) return;
        if (val instanceof SyncArchive) {
            SyncArchive archive = (SyncArchive)val;
            archive.setNextSyncOneOff(false);
            archive.setNextSyncTime(System.currentTimeMillis());
            archive.store(true);
        }
    }
    
    public void syncNowOneTime() {
        if (_tree.getSelectionCount() != 1) return;
        TreeItem item = _tree.getSelection()[0];
        Object val = _items.get(item);
        if (val == null) return;
        if (val instanceof SyncArchive) {
            SyncArchive archive = (SyncArchive)val;
            archive.setNextSyncOneOff(true);
            archive.setNextSyncTime(System.currentTimeMillis());
            archive.store(true);
        }
    }
    
    private void buildMenuArchive(final SyncArchive archive) {
        MenuItem item = new MenuItem(_treeMenu, SWT.PUSH);
        item.setText(_translationRegistry.getText(T_MENU_ARCHIVE_CFG, "Settings..."));
        item.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
            public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
            private void fire() {
                viewDetailArchive(archive, true);
            }
        });
        
        item = new MenuItem(_treeMenu, SWT.PUSH);
        item.setText(_translationRegistry.getText(T_MENU_SYNC_NOW, "Sync now (recurring)"));
        item.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
            public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
            private void fire() {
                archive.setNextSyncOneOff(false);
                archive.setNextSyncTime(System.currentTimeMillis());
                archive.store(true);
            }
        });
        
        item = new MenuItem(_treeMenu, SWT.PUSH);
        item.setText(_translationRegistry.getText(T_MENU_SYNC_ONEOFF, "Sync now (one time)"));
        item.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
            public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
            private void fire() {
                archive.setNextSyncOneOff(true);
                archive.setNextSyncTime(System.currentTimeMillis());
                archive.store(true);
            }
        });
        
        if (archive.getNextSyncTime() > 0) {
            item = new MenuItem(_treeMenu, SWT.PUSH);
            item.setText(_translationRegistry.getText(T_MENU_SYNC_CANCEL, "Cancel next sync"));
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                private void fire() {
                    archive.stop(_translationRegistry.getText(T_CANCELLED, "Cancelled"));
                }
            });
        }
        // only allow clear when there isn't a sync in progress
        if ( !syncRunning(archive) && ( (archive.getIncomingActionCount() > 0) || (archive.getOutgoingActionCount() > 0) ) ) {
            item = new MenuItem(_treeMenu, SWT.PUSH);
            item.setText(_translationRegistry.getText(T_MENU_CLEAR_ACTIONS, "Clear complete actions"));
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                private void fire() {
                    archive.clearCompletedActions(true, true);
                }
            });
        }
    }
    private static final String T_MENU_ARCHIVE_CFG = "syndie.gui.syndicator.menu.archive.cfg";
    private static final String T_MENU_SYNC_NOW = "syndie.gui.syndicator.menu.sync.now";
    private static final String T_MENU_SYNC_ONEOFF = "syndie.gui.syndicator.menu.sync.oneoff";
    private static final String T_MENU_SYNC_CANCEL = "syndie.gui.syndicator.menu.sync.cancel";
    private static final String T_MENU_CLEAR_ACTIONS = "syndie.gui.syndicator.menu.clearactions";
    
    private void buildMenuIncoming(TreeItem item) {
        final SyncArchive archive = (SyncArchive)_items.get(item.getParentItem());
        if (!syncRunning(archive) && archive.getIncomingActionCount() > 0) {
            MenuItem clear = new MenuItem(_treeMenu, SWT.PUSH);
            clear.setText(_translationRegistry.getText(T_MENU_CLEAR_ACTIONS, "Clear complete actions"));
            clear.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                private void fire() {
                    archive.clearCompletedActions(true, false);
                }
            });
        }
    }
    private void buildMenuOutgoing(TreeItem item) {
        final SyncArchive archive = (SyncArchive)_items.get(item.getParentItem());
        if (!syncRunning(archive) && archive.getOutgoingActionCount() > 0) {
            MenuItem clear = new MenuItem(_treeMenu, SWT.PUSH);
            clear.setText(_translationRegistry.getText(T_MENU_CLEAR_ACTIONS, "Clear complete actions"));
            clear.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                private void fire() {
                    archive.clearCompletedActions(false, true);
                }
            });
        }
    }
    private void buildMenuFetchIndex(final TreeItem item) {
        final SyncArchive archive = (SyncArchive)_items.get(item.getParentItem());
        MenuItem clear = new MenuItem(_treeMenu, SWT.PUSH);
        clear.setText(_translationRegistry.getText(T_MENU_CLEAR_ACTION, "Clear"));
        clear.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
            public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
            private void fire() {
                // there isn't any associated element within the archive we need to clear
                _items.remove(item);
                _archiveNameToIndexItem.remove(archive.getName());
                item.dispose();
            }
        });
    }
    private void buildMenuIncoming(final SyncArchive.IncomingAction action) {
        if (action.isComplete()) {
            final SyndieURI uri = action.getURI();
            MenuItem item = new MenuItem(_treeMenu, SWT.PUSH);
            item.setText(_translationRegistry.getText(T_MENU_VIEW, "View"));
            item.addSelectionListener(new FireSelectionListener() {
                public void fire() { _navControl.view(uri); }
            });
            if (action.getPBEPrompt() != null) {
                item = new MenuItem(_treeMenu, SWT.PUSH);
                item.setText(_translationRegistry.getText(T_MENU_PBE, "Resolve passphrase prompt"));
                item.addSelectionListener(new FireSelectionListener() {
                    public void fire() { _navControl.view(uri); }
                });
            }
            if (!syncRunning(action.getArchive())) {
                item = new MenuItem(_treeMenu, SWT.PUSH);
                item.setText(_translationRegistry.getText(T_MENU_CLEAR_ACTION, "Clear action"));
                item.addSelectionListener(new SelectionListener() {
                    public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                    public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                    private void fire() {
                        action.dispose();
                    }
                });
            }
        } else {
            MenuItem item = new MenuItem(_treeMenu, SWT.PUSH);
            item.setText(_translationRegistry.getText(T_MENU_CANCEL_FETCH, "Cancel"));
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                private void fire() {
                    action.cancel(_translationRegistry.getText(T_CANCELLED, "Cancelled"));
                }
            });
        }
    }
    private static final String T_MENU_CLEAR_ACTION = "syndie.gui.syndicator.menu.clearaction";
    private static final String T_MENU_CANCEL_FETCH = "syndie.gui.syndicator.menu.cancelfetch";
    private void buildMenuOutgoing(final SyncArchive.OutgoingAction action) {
        final SyndieURI uri = action.getURI();
        MenuItem item = new MenuItem(_treeMenu, SWT.PUSH);
        item.setText(_translationRegistry.getText(T_MENU_VIEW, "View"));
        item.addSelectionListener(new FireSelectionListener() {
            public void fire() { _navControl.view(uri); }
        });
        
        if (!syncRunning(action.getArchive()) && action.isComplete()) {
            item = new MenuItem(_treeMenu, SWT.PUSH);
            item.setText(_translationRegistry.getText(T_MENU_CLEAR_ACTION, "Clear action"));
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                private void fire() {
                    action.dispose();
                }
            });
        } else if (action.isScheduled()) {
            item = new MenuItem(_treeMenu, SWT.PUSH);
            item.setText(_translationRegistry.getText(T_MENU_CANCEL_FETCH, "Cancel"));
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                private void fire() {
                    action.cancel(_translationRegistry.getText(T_CANCELLED, "Cancelled"));
                }
            });
        }
    }
    private static final String T_MENU_VIEW = "syndie.gui.syndicator.menu.view";
    private static final String T_MENU_PBE = "syndie.gui.syndicator.menu.pbe";
    
    private boolean syncRunning(SyncArchive archive) {
        if ( (archive.getNextSyncTime() > 0) && (archive.getNextSyncTime() <= System.currentTimeMillis()) && (archive.getIndexFetchComplete() || archive.getIndexFetchInProgress()) )
            return true;
        return false;
    }
    
    public int getSelectionCount() { return _tree.getSelectionCount(); }
    
    public void viewDetail() {
        int sel = _tree.getSelectionCount();
        _ui.debugMessage("view detail: sel=" + sel);
        if (sel != 1) return;
        TreeItem item = _tree.getSelection()[0];
        Object val = _items.get(item);
        _ui.debugMessage("view detail: val=" + val);
        if (val == null) return;
        if (val instanceof SyncArchive) {
            _ui.debugMessage("viewing detail of archive");
            SyncArchive archive = (SyncArchive)val;
            viewDetailArchive(archive, true);
        }
    }
    public Set getFetchedScopes() {
        Set scopes = new HashSet();
        for (Iterator iter = _archiveNameToIncoming.values().iterator(); iter.hasNext(); ) {
            Map uriToItem = (Map)iter.next();
            for (Iterator uriIter = uriToItem.keySet().iterator(); uriIter.hasNext(); ) {
                SyndieURI uri = (SyndieURI)uriIter.next();
                if (uri == null) continue;
                if (scopes.contains(uri.getScope())) continue;
                if (uri.getMessageId() != null) {
                    long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                    if (msgId >= 0)
                        scopes.add(uri.getScope());
                }
            }
        }
        return scopes;
    }
    
    private void viewDetailArchive(SyncArchive archive, boolean fireDefaultAction) {
        if (_detail != null) _detail.dispose();
        // SyndicatorDetailHTTPArchive deals with HTTP and Freenet archives (and though
        // it covers for the file based archives, a separate file based archive config
        // would be better).  down the line we may need to pick different ones here
        
        final Shell s = new Shell(_root.getShell(), SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        s.setText(_translationRegistry.getText(T_DETAIL_HTTPARCHIVE, "HTTP archive"));
        s.setFont(_themeRegistry.getTheme().SHELL_FONT);
        s.setLayout(new FillLayout());
        _detail = new SyndicatorDetailHTTPArchive(_client, _ui, _themeRegistry, _translationRegistry, s, archive, new SyndicationDetailListener() {
            public void cancelled() { s.dispose(); }
            public void saved() { s.dispose(); }
            public void scheduleUpdated() { s.dispose(); }
        });
        s.pack();
        s.open();
        TreeItem item = (TreeItem)_archiveNameToRootItem.get(archive.getName());
        if (item != null) // null for new ones
            _tree.showItem(item);
    }
    private void viewDetailIncoming(TreeItem item, boolean fireDefaultAction) {}
    private void viewDetailOutgoing(TreeItem item, boolean fireDefaultAction) {}
    private void viewDetailFetchIndex(TreeItem item, boolean fireDefaultAction) {}
    private void viewDetailIncoming(SyncArchive.IncomingAction action, boolean fireDefaultAction) {
        if ( fireDefaultAction && (action.getCompletionTime() > 0) && (action.getFetchErrorMsg() == null) )
            _navControl.view(action.getURI());
    }
    private void viewDetailOutgoing(SyncArchive.OutgoingAction action, boolean fireDefaultAction) {
        if ( fireDefaultAction && (action.getCompletionTime() > 0) && (action.getErrorMsg() == null) )
            _navControl.view(action.getURI());
    }
    
    private void loadData() {
        SyncManager mgr = SyncManager.getInstance(_client, _ui);
        mgr.loadArchives();
        mgr.addListener(this);
    
        int cnt = mgr.getArchiveCount();
        _ui.debugMessage("archives loaded: " + cnt);
        if (cnt == 0) {
            showArchiveDefaults();
        } else {
            for (int i = 0; i < cnt; i++) {
                SyncArchive archive = mgr.getArchive(i);
                archive.addListener(this);
                loadData(archive);
            }
        }
    }
    
    private void showArchiveDefaults() { new ArchiveDefaultsPopup(_client, _ui, _root.getShell(), _themeRegistry, _translationRegistry); }
    
    // create or update a subtree for the archive, including an item for the index fetch (if scheduled),
    // a subtree for scheduled message/meta fetches, and a subtree for scheduled pushes.  do we want
    // a subtree for uncleared imported messages/meta fetches?  or just keep them in the fetches subtree,
    // cleared on demand
    private void loadData(SyncArchive archive) {
        TreeItem rootItem = loadDataRoot(archive);
        loadDataFetches(archive, rootItem);
        loadDataPushes(archive);
    }
    
    private long getNextTime(SyncArchive archive) { return archive.getNextSyncTime(); }
    
    private TreeItem loadDataRoot(SyncArchive archive) {
        long nextTime = getNextTime(archive);
        _ui.debugMessage("loadDataRoot(" + archive + "): nextTime in " + (nextTime-System.currentTimeMillis()) + "ms");
        TreeItem rootItem = (TreeItem)_archiveNameToRootItem.get(archive.getName());
        if (rootItem == null) {
            rootItem = new TreeItem(_tree, SWT.NONE);
            _archiveNameToRootItem.put(archive.getName(), rootItem);
            _items.put(rootItem, archive);
        }
        rootItem.setText(0, archive.getName());
        if (nextTime <= 0) {
            rootItem.setText(1, _translationRegistry.getText(T_WHEN_NEVER, "Never"));
            rootItem.setImage(2, null);
            rootItem.setText(3, _translationRegistry.getText(T_SUMMARY_NEVER, "No syndication scheduled"));
        } else if (!SyncManager.getInstance(_client, _ui).isOnline()) {
            rootItem.setText(1, Constants.getDateTime(nextTime));
            rootItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
            rootItem.setText(3, _translationRegistry.getText(T_INDEX_OFFLINE, "Offline - set as online to start"));
        } else if (nextTime < System.currentTimeMillis()) {
            if (archive.getIndexFetchInProgress()) {
                rootItem.setText(1, _translationRegistry.getText(T_WHEN_INDEXFETCHINPROGRESS, "Now"));
                rootItem.setText(3, _translationRegistry.getText(T_SUMMARY_INDEXFETCH, "Index fetch in progress"));
            } else {
                rootItem.setText(1, _translationRegistry.getText(T_WHEN_INDEXFETCHCOMPLETE, "Now"));
                rootItem.setText(3, _translationRegistry.getText(T_SUMMARY_EXECUTEPOLICY, "Syndicating"));
            }
            rootItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_INPROGRESS);
        } else {
            rootItem.setText(1, Constants.getDateTime(nextTime));
            rootItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
            rootItem.setText(3, _translationRegistry.getText(T_SUMMARY_SCHEDULED, "Syndication scheduled"));
        }
    
        resizeCols(rootItem);
        
        loadDataIndex(archive, rootItem);
        
        return rootItem;
    }
    
    private void loadDataIndex(SyncArchive archive, TreeItem rootItem) {
        long nextTime = getNextTime(archive);
        _ui.debugMessage("loadDataIndex(" + archive + "): nextTime in " + (nextTime-System.currentTimeMillis()) + "ms");
        
        TreeItem indexItem = (TreeItem)_archiveNameToIndexItem.get(archive.getName());
        if (nextTime <= 0) {
            // keep it around to view the details
            //if (indexItem != null) {
            //    indexItem.dispose();
            //    _items.remove(indexItem);
            //}
            //_archiveNameToIndexItem.remove(archive.getName());
            
            // use the later to redraw the error/etc
            //return;
        }
        
        if (indexItem == null) {
            if (nextTime <= 0) return;
            indexItem = new TreeItem(rootItem, SWT.NONE);
            _archiveNameToIndexItem.put(archive.getName(), indexItem);
            _items.put(indexItem, "fetchindex");
            rootItem.setExpanded(true);
        }

        indexItem.setText(0, _translationRegistry.getText(T_INDEX_NAME, "Fetch index"));
        if (archive.getIndexFetchInProgress()) {
            indexItem.setText(1, _translationRegistry.getText(T_WHEN_NOW, "Now"));
            indexItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_INPROGRESS);
            indexItem.setText(3, _translationRegistry.getText(T_SUMMARY_INDEXFETCH, "Index fetch in progress"));
        } else {
            if (nextTime > 0) // otherwise, keep the previous value
                indexItem.setText(1, Constants.getDateTime(nextTime));

            if (archive.getLastIndexFetchErrorMsg() != null) {
                indexItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_ERROR);
                indexItem.setText(3, _translationRegistry.getText(T_INDEX_ERROR, "Fetch error: ") + archive.getLastIndexFetchErrorMsg());
            } else if (!SyncManager.getInstance(_client, _ui).isOnline()) {
                indexItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
                indexItem.setText(3, _translationRegistry.getText(T_INDEX_OFFLINE, "Offline - set as online to start"));
            } else if ( (nextTime > 0) /* && (nextTime <= System.currentTimeMillis()) */ && (!archive.getIndexFetchComplete()) && (!archive.getIndexFetchInProgress()) ) {
                indexItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
                indexItem.setText(3, _translationRegistry.getText(T_INDEX_SCHEDULED, "Scheduled"));
            } else { //( /*(nextTime > 0) && */ (nextTime <= System.currentTimeMillis()) && (archive.getIndexFetchComplete()) ) {
                indexItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_OK);
                indexItem.setText(3, _translationRegistry.getText(T_INDEX_COMPLETE, "Fetch complete"));
            }
        }
        resizeCols(indexItem);
    }
    
    private void loadDataFetches(SyncArchive archive, TreeItem rootItem) {
        int count = archive.getIncomingActionCount();
        if (count > 0) {
            for (int i = 0; i < count; i++) {
                SyncArchive.IncomingAction action = archive.getIncomingAction(i);
                loadDataFetch(action);
            }
        } else {
            TreeItem incomingItem = (TreeItem)_archiveNameToIncomingItem.remove(archive.getName());
            if (incomingItem != null) {
                incomingItem.dispose();
                _items.remove(incomingItem);
            }
            _archiveNameToIncoming.remove(archive.getName());
        }
    }

    /** create (or update) a fetch record (may create or update parents) */
    private void loadDataFetch(SyncArchive.IncomingAction action) {
        if (action != null) {
            SyndieURI uri = action.getURI();
            if (uri == null) return;
            Hash scope = uri.getScope();
            if (scope == null) return;

            Map incomingURIToItem = (Map)_archiveNameToIncoming.get(action.getArchive().getName());
            if (incomingURIToItem == null) {
                incomingURIToItem = new HashMap();
                _archiveNameToIncoming.put(action.getArchive().getName(), incomingURIToItem);
            }
            
            TreeItem incomingItem = (TreeItem)_archiveNameToIncomingItem.get(action.getArchive().getName());
            
            if (incomingItem == null) {
                TreeItem rootItem = loadDataRoot(action.getArchive());
                incomingItem = new TreeItem(rootItem, SWT.NONE);
                _archiveNameToIncomingItem.put(action.getArchive().getName(), incomingItem);
                _items.put(incomingItem, "incoming");
            }
            incomingItem.setText(0, _translationRegistry.getText(T_INCOMING_NAME, "fetches"));
            incomingItem.setText(3, action.getArchive().getIncomingActionCount()+"");
    
            resizeCols(incomingItem);
            
            TreeItem actionItem = (TreeItem)incomingURIToItem.get(uri);
            
            if (action.isDisposed()) {
                incomingURIToItem.remove(uri);
                if (actionItem != null) {
                    actionItem.dispose();
                    _items.remove(actionItem);
                }
                
                if (action.getArchive().getIncomingActionCount() == 0) {
                    _archiveNameToIncomingItem.remove(action.getArchive().getName());
                    _archiveNameToIncoming.remove(action.getArchive().getName());
                    incomingItem.dispose();
                    _items.remove(incomingItem);
                }
                
                return;
            }
            
            if (actionItem == null) {
                actionItem = new TreeItem(incomingItem, SWT.NONE);
                incomingURIToItem.put(uri, actionItem);
                _items.put(actionItem, action);
            }

            String forum = _client.getChannelName(scope);
            if (forum != null)
                actionItem.setText(0, forum + " [" + scope.toBase64().substring(0,6) + "]" + (uri.getMessageId() != null ? " - " + uri.getMessageId() : ""));
            else
                actionItem.setText(0, "[" + scope.toBase64().substring(0,6) + "]" + (uri.getMessageId() != null ? " - " + uri.getMessageId() : ""));

            if (action.isScheduled()) {
                actionItem.setText(1, _translationRegistry.getText(T_WHEN_ASAP, "ASAP"));
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
            } else if (action.isReadKeyUnknown()) {
                actionItem.setText(1, Constants.getDateTime(action.getCompletionTime()));                    
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_NOKEY);
                actionItem.setText(3, _translationRegistry.getText(T_FETCH_READKEYUNKNOWN, "Read key unknown"));
            } else if (action.getPBEPrompt() != null) {
                actionItem.setText(1, Constants.getDateTime(action.getCompletionTime()));                    
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_PBE);
                actionItem.setText(3, _translationRegistry.getText(T_FETCH_PBE, "Passphrase required: ") + action.getPBEPrompt());
            } else if (action.isReplyKeyUnknown()) {
                actionItem.setText(1, Constants.getDateTime(action.getCompletionTime()));                    
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_NOKEY);
                actionItem.setText(3, _translationRegistry.getText(T_FETCH_REPLYKEYUNKNOWN, "Reply key unknown"));
            } else if (action.isCorrupt()) {
                actionItem.setText(1, Constants.getDateTime(action.getCompletionTime()));                    
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_ERROR);
                actionItem.setText(3, _translationRegistry.getText(T_FETCH_CORRUPT, "Message is corrupt"));
            } else if (action.getFetchErrorMsg() != null) {
                actionItem.setText(1, Constants.getDateTime(action.getCompletionTime()));
                String msg = action.getFetchErrorMsg();
                if (action.getFetchError() != null)
                    msg = msg + " - " + action.getFetchError().getMessage();
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_ERROR);
                actionItem.setText(3, _translationRegistry.getText(T_FETCH_FAILED, "Fetch failed: ") + msg);
            } else if (action.isExecuting()) {
                actionItem.setText(1, _translationRegistry.getText(T_WHEN_NOW, "Now"));
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_INPROGRESS);
            } else { // complete
                actionItem.setText(1, Constants.getDateTime(action.getCompletionTime()));                    actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_OK);
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_OK);
                actionItem.setText(3, _translationRegistry.getText(T_FETCH_OK, "Imported"));
            }
            resizeCols(actionItem);
        }
    }
    
    private void loadDataPushes(SyncArchive archive) {
        int count = archive.getOutgoingActionCount();
        if (count > 0) {
            for (int i = 0; i < count; i++) {
                SyncArchive.OutgoingAction action = archive.getOutgoingAction(i);
                loadDataPush(action);
            }
        } else {
            TreeItem outgoingItem = (TreeItem)_archiveNameToOutgoingItem.remove(archive.getName());
            if (outgoingItem != null) {
                outgoingItem.dispose();
                _items.remove(outgoingItem);
            }
            _archiveNameToOutgoing.remove(archive.getName());
        }
    }

    /** create (or update) a push record (may create or update parents) */
    private void loadDataPush(SyncArchive.OutgoingAction action) {
        if (action != null) {
            SyndieURI uri = action.getURI();
            if (uri == null) return;
            Hash scope = uri.getScope();
            if (scope == null) return;

            TreeItem outgoingItem = (TreeItem)_archiveNameToOutgoingItem.get(action.getArchive().getName());
            
            if (outgoingItem == null) {
                TreeItem rootItem = loadDataRoot(action.getArchive());
                outgoingItem = new TreeItem(rootItem, SWT.NONE);
                _archiveNameToOutgoingItem.put(action.getArchive().getName(), outgoingItem);
                _items.put(outgoingItem, "outgoing");
            }
            outgoingItem.setText(0, _translationRegistry.getText(T_OUTGOING_NAME, "pushes"));
            outgoingItem.setText(3, action.getArchive().getOutgoingActionCount()+"");

            Map outgoingURIToItem = (Map)_archiveNameToOutgoing.get(action.getArchive().getName());
            if (outgoingURIToItem == null) {
                outgoingURIToItem = new HashMap();
                _archiveNameToOutgoing.put(action.getArchive().getName(), outgoingURIToItem);
            }
            resizeCols(outgoingItem);
            
            TreeItem actionItem = (TreeItem)outgoingURIToItem.get(uri);
            
            if (action.isDisposed()) {
                outgoingURIToItem.remove(uri);
                if (actionItem != null) {
                    actionItem.dispose();
                    _items.remove(actionItem);
                }
                
                if (action.getArchive().getOutgoingActionCount() == 0) {
                    _archiveNameToOutgoingItem.remove(action.getArchive().getName());
                    _archiveNameToOutgoing.remove(action.getArchive().getName());
                    outgoingItem.dispose();
                    _items.remove(outgoingItem);
                }
                return;
            }
            
            if (actionItem == null) {
                actionItem = new TreeItem(outgoingItem, SWT.NONE);
                outgoingURIToItem.put(uri, actionItem);
                _items.put(actionItem, action);
            }

            String forum = _client.getChannelName(scope);
            if (forum != null)
                actionItem.setText(0, forum + " [" + scope.toBase64().substring(0,6) + "]" + (uri.getMessageId() != null ? " - " + uri.getMessageId() : ""));
            else
                actionItem.setText(0, "[" + scope.toBase64().substring(0,6) + "]" + (uri.getMessageId() != null ? " - " + uri.getMessageId() : ""));

            if (action.isScheduled()) {
                actionItem.setText(1, _translationRegistry.getText(T_WHEN_ASAP, "ASAP"));
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
            } else if (action.isExecuting()) {
                actionItem.setText(1, _translationRegistry.getText(T_WHEN_NOW, "Now"));
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_INPROGRESS);
            } else if (action.getErrorMsg() != null) {
                actionItem.setText(1, Constants.getDateTime(action.getCompletionTime()));
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_ERROR);
                actionItem.setText(3, _translationRegistry.getText(T_PUSH_ERROR, "Error pushing: ") + action.getErrorMsg());
            } else { // complete
                actionItem.setText(1, Constants.getDateTime(action.getCompletionTime()));
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_OK);
                actionItem.setText(3, _translationRegistry.getText(T_PUSH_OK, "Pushed"));
            }
            resizeCols(actionItem);
        }
    }
    
    private void resizeCols(TreeItem item) {
        setMinWidth(_colName, item.getText(0), 0, 300);
        setMinWidth(_colTime, item.getText(1), 0, 150);
        setMinWidth(_colStatus, item.getText(2), 0, 50);
        setMinWidth(_colSummary, item.getText(3), 0, 100);
    }
    
    protected void setMinWidth(TreeColumn col, String txt, int extra, int min) {
        int width = ImageUtil.getWidth(txt, _tree) + _tree.getGridLineWidth()*2 + extra;
        if (width < min)
            width = min;
        int existing = col.getWidth();
        if (width > existing)
            col.setWidth(width);
    }
    
    private static final String T_WHEN_NEVER = "syndie.gui.syndicator.when.never";
    private static final String T_WHEN_INDEXFETCHINPROGRESS = "syndie.gui.syndicator.when.indexfetchinprogress";
    private static final String T_WHEN_INDEXFETCHCOMPLETE = "syndie.gui.syndicator.when.indexfetchcomplete";
    private static final String T_SUMMARY_NEVER = "syndie.gui.syndicator.summary.never";
    private static final String T_SUMMARY_INDEXFETCH = "syndie.gui.syndicator.summary.indexfetch";
    private static final String T_SUMMARY_EXECUTEPOLICY = "syndie.gui.syndicator.summary.executepolicy";
    private static final String T_SUMMARY_SCHEDULED = "syndie.gui.syndicator.summary.scheduled";
    
    private static final String T_FETCH_READKEYUNKNOWN = "syndie.gui.syndicator.fetch.readkeyunknown";
    private static final String T_FETCH_REPLYKEYUNKNOWN = "syndie.gui.syndicator.fetch.replykeyunknown";
    private static final String T_FETCH_PBE = "syndie.gui.syndicator.fetch.pbe";
    private static final String T_FETCH_OK = "syndie.gui.syndicator.fetch.ok";
    private static final String T_FETCH_FAILED = "syndie.gui.syndicator.fetch.failed";
    private static final String T_FETCH_CORRUPT = "syndie.gui.syndicator.fetch.corrupt";
    
    private static final String T_PUSH_OK = "syndie.gui.syndicator.push.ok";
    private static final String T_PUSH_ERROR = "syndie.gui.syndicator.push.error";

    private static final String T_WHEN_NOW = "syndie.gui.syndicator.now";
    private static final String T_WHEN_ASAP = "syndie.gui.syndicator.asap";
    
    private static final String T_INDEX_NAME = "syndie.gui.syndicator.index.name";
    private static final String T_INDEX_ERROR = "syndie.gui.syndicator.index.error";
    private static final String T_INDEX_COMPLETE = "syndie.gui.syndicator.index.complete";
    private static final String T_INDEX_SCHEDULED = "syndie.gui.syndicator.index.scheduled";
    private static final String T_INDEX_OFFLINE = "syndie.gui.syndicator.index.offline";
    
    private static final String T_INCOMING_NAME = "syndie.gui.syndicator.incoming.name";
    private static final String T_OUTGOING_NAME = "syndie.gui.syndicator.outgoing.name";
    
    private static final String T_COLNAME = "syndie.gui.syndicator.colname";
    private static final String T_COLTIME = "syndie.gui.syndicator.coltime";
    private static final String T_COLSTATUS = "syndie.gui.syndicator.colstatus";
    private static final String T_COLSUMMARY = "syndie.gui.syndicator.colsummary";
    
    private static final String T_ADD = "syndie.gui.syndicator.add";
    private static final String T_CANCEL = "syndie.gui.syndicator.cancel";
    private static final String T_DELETE = "syndie.gui.syndicator.delete";
    
    private static final String T_DETAIL_HTTPARCHIVE = "syndie.gui.syndicator.detail.httparchive";
    
    public void translate(TranslationRegistry registry) {
        _colName.setText(registry.getText(T_COLNAME, ""));
        _colTime.setText(registry.getText(T_COLTIME, "Time"));
        _colStatus.setText(registry.getText(T_COLSTATUS, "Status"));
        _colSummary.setText(registry.getText(T_COLSUMMARY, "Summary"));
        
        if (_showActions) {
            _add.setText(registry.getText(T_ADD, "Add archive"));
            _cancel.setText(registry.getText(T_CANCEL, "Cancel syndications"));
            _delete.setText(registry.getText(T_DELETE, "Delete archive"));
        }
    }
    
    public void applyTheme(Theme theme) {
        _tree.setFont(theme.TREE_FONT);
        
        _colName.pack();
        _colTime.pack();
        _colStatus.pack();
        _colSummary.pack();
        
        if (_showActions) {
            _add.setFont(theme.BUTTON_FONT);
            _cancel.setFont(theme.BUTTON_FONT);
            _delete.setFont(theme.BUTTON_FONT);
        }
    }

    //
    // now for the SyncArchive/SyncManager callbacks (will be called from arbitrary threads
    //
    
    public void archiveAdded(final SyncArchive archive) {
        if (_disposed) return;
        _ui.debugMessage("archive added: " + archive);
        archive.addListener(this);
        Display.getDefault().syncExec(new Runnable() { public void run() { loadData(archive); } });
    }

    public void archiveRemoved(final SyncArchive archive) {
        if (_disposed) return;
        _ui.debugMessage("archive removed: " + archive);
        Display.getDefault().syncExec(new Runnable() { 
            public void run() { 
                // putting these map removals in the swt thread means we can skip synchronization
                TreeItem rootItem = (TreeItem)_archiveNameToRootItem.remove(archive.getName());
                _archiveNameToIncoming.remove(archive.getName());
                _archiveNameToIncomingItem.remove(archive.getName());
                _archiveNameToIndexItem.remove(archive.getName());
                _archiveNameToOutgoing.remove(archive.getName());
                _archiveNameToOutgoingItem.remove(archive.getName());
                if (rootItem != null) {
                    rootItem.dispose(); 
                    _items.remove(rootItem);
                }
            } 
        });
    }

    public void archiveLoaded(final SyncArchive archive) {
        if (_disposed) return;
        _ui.debugMessage("archive loaded: " + archive);
        archive.addListener(this);
        Display.getDefault().syncExec(new Runnable() { public void run() { loadData(archive); } });
    }

    public void incomingUpdated(final SyncArchive.IncomingAction action) {
        if (_disposed) return;
        _ui.debugMessage("incoming action updated: " + action);
        Display.getDefault().syncExec(new Runnable() { public void run() { loadDataFetch(action); } });
    }

    public void incomingUpdated(final List actions) {
        if (_disposed) return;
        _ui.debugMessage("incoming actions updated: " + actions.size());
        Display.getDefault().syncExec(new Runnable() { 
            public void run() {
                for (int i = 0; i < actions.size(); i++)
                    loadDataFetch((SyncArchive.IncomingAction)actions.get(i));
            }
        });
    }

    public void outgoingUpdated(final SyncArchive.OutgoingAction action) {
        if (_disposed) return;
        _ui.debugMessage("outgoing action updated: " + action);
        Display.getDefault().syncExec(new Runnable() { public void run() { loadDataPush(action); } });
    }

    public void archiveUpdated(final SyncArchive archive) {
        if (_disposed) return;
        _ui.debugMessage("archiveUpdated(" + archive + ")");
        Display.getDefault().syncExec(new Runnable() { public void run() { loadData(archive); } });
    }

    public void onlineStatusUpdated(boolean nowOnline) {}
}
