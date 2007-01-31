package syndie.gui;

import java.util.HashMap;
import java.util.Map;
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
import syndie.db.SyncArchive;
import syndie.db.SyncManager;

/**
 *
 */
public class Syndicator implements Translatable, Themeable, SyncManager.SyncListener, SyncArchive.SyncArchiveListener {
    private BrowserControl _browser;
    private Composite _parent;
    
    private Composite _root;
    private Composite _actions;
    private SashForm _sash;
    private Tree _tree;
    private TreeColumn _colName;
    private TreeColumn _colTime;
    private TreeColumn _colStatus;
    private TreeColumn _colSummary;
    private Menu _treeMenu;
    private ScrolledComposite _detailRoot;
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
    
    public Syndicator(BrowserControl browser, Composite parent) {
        _browser = browser;
        _parent = parent;
        
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
        _root.setLayout(new GridLayout(1, true));
        
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
        
        _sash = new SashForm(_root, SWT.VERTICAL);
        _sash.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _tree = new Tree(_sash, SWT.FULL_SELECTION | SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        //_tree.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
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
        
        _detailRoot = new ScrolledComposite(_sash, SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
        //_detailRoot.setLayout(new FillLayout());
        //_detailRoot.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _sash.setWeights(new int[] { 50, 50 });
        _sash.setMaximizedControl(_tree);
        //_sash.setMaximizedControl(null);
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
        
        loadData();
    }
    
    public void show(SyndieURI uri) {
        if (uri.isArchive()) {
            SyncManager mgr = SyncManager.getInstance(_browser.getClient(), _browser.getUI());
            int cnt = mgr.getArchiveCount();
            for (int i = 0; i < cnt; i++) {
                SyncArchive archive = mgr.getArchive(i);
                if (archive.getURL().equals(uri.getURL())) {
                    _browser.getUI().debugMessage("viewing [" + archive.getURL() + "] [" + archive.getName() + "]");
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
            SyncArchive archive = new SyncArchive(mgr, _browser.getClient());
            archive.setURL(uri.getURL());
            archive.setName(uri.getString("name"));
            _browser.getUI().debugMessage("editing as if we are viewing [" + archive.getURL() + "] [" + archive.getName() + "]");
            viewDetailArchive(archive, false);
        }
    }
    
    public void dispose() {
        _disposed = true;
        if (_detail != null) _detail.dispose();
        SyncManager mgr = SyncManager.getInstance(_browser.getClient(), _browser.getUI());
        mgr.removeListener(this);
        int idx = mgr.getArchiveCount();
        
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
    }
    
    private void add() {
        SyncManager mgr = SyncManager.getInstance(_browser.getClient(), _browser.getUI());
        viewDetailArchive(new SyncArchive(mgr, _browser.getClient()), false);
    }
    private void cancel() {
        SyncManager mgr = SyncManager.getInstance(_browser.getClient(), _browser.getUI());
        for (int i = 0; i < mgr.getArchiveCount(); i++) {
            SyncArchive archive = mgr.getArchive(i);
            archive.stop(_browser.getTranslationRegistry().getText(T_CANCELLED, "Cancelled"));
        }
    }
    private static final String T_CANCELLED = "syndie.gui.syndicator.cancelled";
    private void delete() { 
        if (_tree.getSelectionCount() != 1) return;
        TreeItem item = _tree.getSelection()[0];
        Object val = _items.get(item);
        if (val == null) return;
        if (val instanceof SyncArchive) {
            SyncArchive archive = (SyncArchive)val;
            MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO);
            box.setMessage(_browser.getTranslationRegistry().getText(T_DELETE_CONFIRM, "Are you sure you want to delete this archive? ") + archive.getName());
            box.setText(_browser.getTranslationRegistry().getText(T_DELETE_CONFIRM_TITLE, "Delete?"));
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
            _browser.getUI().debugMessage("Unknown type is selected: " + val + " for " + item);
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
            _browser.getUI().debugMessage("Unknown type is selected: " + val + " for " + item);
        }
    }
    
    private void buildMenuArchive(final SyncArchive archive) {
        MenuItem item = new MenuItem(_treeMenu, SWT.PUSH);
        item.setText(_browser.getTranslationRegistry().getText(T_MENU_ARCHIVE_CFG, "Settings..."));
        item.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
            public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
            private void fire() {
                viewDetailArchive(archive, true);
            }
        });
        
        item = new MenuItem(_treeMenu, SWT.PUSH);
        item.setText(_browser.getTranslationRegistry().getText(T_MENU_SYNC_NOW, "Sync now (recurring)"));
        item.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
            public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
            private void fire() {
                archive.setNextPullOneOff(false);
                archive.setNextPullTime(System.currentTimeMillis());
                archive.setNextPushOneOff(false);
                archive.setNextPushTime(System.currentTimeMillis());
                archive.store(true);
            }
        });
        
        item = new MenuItem(_treeMenu, SWT.PUSH);
        item.setText(_browser.getTranslationRegistry().getText(T_MENU_SYNC_ONEOFF, "Sync now (one time)"));
        item.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
            public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
            private void fire() {
                archive.setNextPullOneOff(true);
                archive.setNextPullTime(System.currentTimeMillis());
                archive.setNextPushOneOff(true);
                archive.setNextPushTime(System.currentTimeMillis());
                archive.store(true);
            }
        });
        
        if ( (archive.getNextPullTime() > 0) || (archive.getNextPushTime() > 0) ) {
            item = new MenuItem(_treeMenu, SWT.PUSH);
            item.setText(_browser.getTranslationRegistry().getText(T_MENU_SYNC_CANCEL, "Cancel next sync"));
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                private void fire() {
                    archive.stop(_browser.getTranslationRegistry().getText(T_CANCELLED, "Cancelled"));
                }
            });
        }
        // only allow clear when there isn't a sync in progress
        if ( !syncRunning(archive) && ( (archive.getIncomingActionCount() > 0) || (archive.getOutgoingActionCount() > 0) ) ) {
            item = new MenuItem(_treeMenu, SWT.PUSH);
            item.setText(_browser.getTranslationRegistry().getText(T_MENU_CLEAR_ACTIONS, "Clear complete actions"));
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
            clear.setText(_browser.getTranslationRegistry().getText(T_MENU_CLEAR_ACTIONS, "Clear complete actions"));
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
            clear.setText(_browser.getTranslationRegistry().getText(T_MENU_CLEAR_ACTIONS, "Clear complete actions"));
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
        clear.setText(_browser.getTranslationRegistry().getText(T_MENU_CLEAR_ACTION, "Clear"));
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
            item.setText(_browser.getTranslationRegistry().getText(T_MENU_VIEW, "View"));
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { _browser.view(uri); }
                public void widgetSelected(SelectionEvent selectionEvent) { _browser.view(uri); }
            });
            if (action.getPBEPrompt() != null) {
                item = new MenuItem(_treeMenu, SWT.PUSH);
                item.setText(_browser.getTranslationRegistry().getText(T_MENU_PBE, "Resolve passphrase prompt"));
                item.addSelectionListener(new SelectionListener() {
                    public void widgetDefaultSelected(SelectionEvent selectionEvent) { _browser.view(uri); }
                    public void widgetSelected(SelectionEvent selectionEvent) { _browser.view(uri); }
                });
            }
            if (!syncRunning(action.getArchive())) {
                item = new MenuItem(_treeMenu, SWT.PUSH);
                item.setText(_browser.getTranslationRegistry().getText(T_MENU_CLEAR_ACTION, "Clear action"));
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
            item.setText(_browser.getTranslationRegistry().getText(T_MENU_CANCEL_FETCH, "Cancel"));
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                private void fire() {
                    action.cancel(_browser.getTranslationRegistry().getText(T_CANCELLED, "Cancelled"));
                }
            });
        }
    }
    private static final String T_MENU_CLEAR_ACTION = "syndie.gui.syndicator.menu.clearaction";
    private static final String T_MENU_CANCEL_FETCH = "syndie.gui.syndicator.menu.cancelfetch";
    private void buildMenuOutgoing(final SyncArchive.OutgoingAction action) {
        final SyndieURI uri = action.getURI();
        MenuItem item = new MenuItem(_treeMenu, SWT.PUSH);
        item.setText(_browser.getTranslationRegistry().getText(T_MENU_VIEW, "View"));
        item.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _browser.view(uri); }
            public void widgetSelected(SelectionEvent selectionEvent) { _browser.view(uri); }
        });
        
        if (!syncRunning(action.getArchive()) && action.isComplete()) {
            item = new MenuItem(_treeMenu, SWT.PUSH);
            item.setText(_browser.getTranslationRegistry().getText(T_MENU_CLEAR_ACTION, "Clear action"));
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                private void fire() {
                    action.dispose();
                }
            });
        } else if (action.isScheduled()) {
            item = new MenuItem(_treeMenu, SWT.PUSH);
            item.setText(_browser.getTranslationRegistry().getText(T_MENU_CANCEL_FETCH, "Cancel"));
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                private void fire() {
                    action.cancel(_browser.getTranslationRegistry().getText(T_CANCELLED, "Cancelled"));
                }
            });
        }
    }
    private static final String T_MENU_VIEW = "syndie.gui.syndicator.menu.view";
    private static final String T_MENU_PBE = "syndie.gui.syndicator.menu.pbe";
    
    private boolean syncRunning(SyncArchive archive) {
        if ( (archive.getNextPullTime() > 0) && (archive.getNextPullTime() <= System.currentTimeMillis()) )
            return true;
        if ( (archive.getNextPushTime() > 0) && (archive.getNextPushTime() <= System.currentTimeMillis()) )
            return true;
        return false;
    }
    
    private void viewDetailArchive(SyncArchive archive, boolean fireDefaultAction) {
        if (_detail != null) _detail.dispose();
        //_sash.setMaximizedControl(null);
        // SyndicatorDetailHTTPArchive deals with HTTP and Freenet archives (and though
        // it covers for the file based archives, a separate file based archive config
        // would be better).  down the line we may need to pick different ones here
        
        final Shell s = new Shell(_root.getShell(), SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        s.setText(_browser.getTranslationRegistry().getText(T_DETAIL_HTTPARCHIVE, "HTTP archive"));
        s.setFont(_browser.getThemeRegistry().getTheme().SHELL_FONT);
        s.setLayout(new FillLayout());
        //_detail = new SyndicatorDetailHTTPArchive(_browser, _detailRoot, archive);
        _detail = new SyndicatorDetailHTTPArchive(_browser, s, archive, new SyndicationDetailListener() {
            public void cancelled() { s.dispose(); }
            public void saved() { s.dispose(); }
            public void scheduleUpdated() { s.dispose(); }
        });
        s.pack();
        s.open();
        //_detailRoot.setExpandHorizontal(true);
        //_detailRoot.setExpandVertical(true);
        ////_detailRoot.setContent(_detail.getControl());
        //Rectangle rect = _tree.getClientArea();
        //Point dSz = _detail.getControl().computeSize(rect.width-50, SWT.DEFAULT);
        //_detail.getControl().setSize(dSz);
        //_root.layout(true, true);
        TreeItem item = (TreeItem)_archiveNameToRootItem.get(archive.getName());
        if (item != null) // null for new ones
            _tree.showItem(item);
    }
    private void viewDetailIncoming(TreeItem item, boolean fireDefaultAction) {}
    private void viewDetailOutgoing(TreeItem item, boolean fireDefaultAction) {}
    private void viewDetailFetchIndex(TreeItem item, boolean fireDefaultAction) {}
    private void viewDetailIncoming(SyncArchive.IncomingAction action, boolean fireDefaultAction) {
        if ( fireDefaultAction && (action.getCompletionTime() > 0) && (action.getFetchErrorMsg() == null) )
            _browser.view(action.getURI());
    }
    private void viewDetailOutgoing(SyncArchive.OutgoingAction action, boolean fireDefaultAction) {
        if ( fireDefaultAction && (action.getCompletionTime() > 0) && (action.getErrorMsg() == null) )
            _browser.view(action.getURI());
    }
    
    private void loadData() {
        SyncManager mgr = SyncManager.getInstance(_browser.getClient(), _browser.getUI());
        mgr.loadArchives();
        mgr.addListener(this);
    
        int cnt = mgr.getArchiveCount();
        _browser.getUI().debugMessage("archives loaded: " + cnt);
        for (int i = 0; i < cnt; i++) {
            SyncArchive archive = mgr.getArchive(i);
            archive.addListener(this);
            loadData(archive);
        }
    }
    
    // create or update a subtree for the archive, including an item for the index fetch (if scheduled),
    // a subtree for scheduled message/meta fetches, and a subtree for scheduled pushes.  do we want
    // a subtree for uncleared imported messages/meta fetches?  or just keep them in the fetches subtree,
    // cleared on demand
    private void loadData(SyncArchive archive) {
        TreeItem rootItem = loadDataRoot(archive);
        loadDataIndex(archive, rootItem);
        loadDataFetches(archive, rootItem);
        loadDataPushes(archive);
    }
    
    private long getNextTime(SyncArchive archive) {
        if (archive.getNextPullTime() <= 0)
            return archive.getNextPushTime();
        else if (archive.getNextPushTime() <= 0)
            return archive.getNextPullTime();
        else
            return Math.min(archive.getNextPullTime(), archive.getNextPushTime());
    }
    
    private TreeItem loadDataRoot(SyncArchive archive) {
        long nextTime = getNextTime(archive);
        _browser.getUI().debugMessage("loadDataRoot(" + archive + "): nextTime in " + (nextTime-System.currentTimeMillis()) + "ms");
        TreeItem rootItem = (TreeItem)_archiveNameToRootItem.get(archive.getName());
        if (rootItem == null) {
            rootItem = new TreeItem(_tree, SWT.NONE);
            _archiveNameToRootItem.put(archive.getName(), rootItem);
            _items.put(rootItem, archive);
        }
        rootItem.setText(0, archive.getName());
        if (nextTime <= 0) {
            rootItem.setText(1, _browser.getTranslationRegistry().getText(T_WHEN_NEVER, "Never"));
            rootItem.setImage(2, null);
            rootItem.setText(3, _browser.getTranslationRegistry().getText(T_SUMMARY_NEVER, "No syndication scheduled"));
        } else if (!SyncManager.getInstance(_browser.getClient(), _browser.getUI()).isOnline()) {
            rootItem.setText(1, Constants.getDateTime(nextTime));
            rootItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
            rootItem.setText(3, _browser.getTranslationRegistry().getText(T_INDEX_OFFLINE, "Offline - set as online to start"));
        } else if (nextTime < System.currentTimeMillis()) {
            if (archive.getIndexFetchInProgress()) {
                rootItem.setText(1, _browser.getTranslationRegistry().getText(T_WHEN_INDEXFETCHINPROGRESS, "Now"));
                rootItem.setText(3, _browser.getTranslationRegistry().getText(T_SUMMARY_INDEXFETCH, "Index fetch in progress"));
            } else {
                rootItem.setText(1, _browser.getTranslationRegistry().getText(T_WHEN_INDEXFETCHCOMPLETE, "Now"));
                rootItem.setText(3, _browser.getTranslationRegistry().getText(T_SUMMARY_EXECUTEPOLICY, "Syndicating"));
            }
            rootItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_INPROGRESS);
        } else {
            rootItem.setText(1, Constants.getDateTime(nextTime));
            rootItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
            rootItem.setText(3, _browser.getTranslationRegistry().getText(T_SUMMARY_SCHEDULED, "Syndication scheduled"));
        }
    
        resizeCols(rootItem);
        
        return rootItem;
    }
    
    private void loadDataIndex(SyncArchive archive, TreeItem rootItem) {
        long nextTime = getNextTime(archive);
        _browser.getUI().debugMessage("loadDataIndex(" + archive + "): nextTime in " + (nextTime-System.currentTimeMillis()) + "ms");
        
        TreeItem indexItem = (TreeItem)_archiveNameToIndexItem.get(archive.getName());
        if (nextTime <= 0) {
            // keep it around to view the details
            //if (indexItem != null) {
            //    indexItem.dispose();
            //    _items.remove(indexItem);
            //}
            //_archiveNameToIndexItem.remove(archive.getName());
            return;
        }
        
        if (indexItem == null) {
            indexItem = new TreeItem(rootItem, SWT.NONE);
            _archiveNameToIndexItem.put(archive.getName(), indexItem);
            _items.put(indexItem, "fetchindex");
            rootItem.setExpanded(true);
        }

        indexItem.setText(0, _browser.getTranslationRegistry().getText(T_INDEX_NAME, "Fetch index"));
        if (archive.getIndexFetchInProgress()) {
            indexItem.setText(1, _browser.getTranslationRegistry().getText(T_WHEN_NOW, "Now"));
            indexItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_INPROGRESS);
            indexItem.setText(3, _browser.getTranslationRegistry().getText(T_SUMMARY_INDEXFETCH, "Index fetch in progress"));
        } else {
            indexItem.setText(1, Constants.getDateTime(nextTime));

            if (archive.getLastIndexFetchErrorMsg() != null) {
                indexItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_ERROR);
                indexItem.setText(3, _browser.getTranslationRegistry().getText(T_INDEX_ERROR, "Fetch error: ") + archive.getLastIndexFetchErrorMsg());
            } else if (!SyncManager.getInstance(_browser.getClient(), _browser.getUI()).isOnline()) {
                indexItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
                indexItem.setText(3, _browser.getTranslationRegistry().getText(T_INDEX_OFFLINE, "Offline - set as online to start"));
            } else if ( (nextTime > 0) && (nextTime <= System.currentTimeMillis()) && (archive.getIndexFetchComplete()) ) {
                indexItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_OK);
                indexItem.setText(3, _browser.getTranslationRegistry().getText(T_INDEX_COMPLETE, "Fetch complete"));
            } else {
                indexItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
                indexItem.setText(3, _browser.getTranslationRegistry().getText(T_INDEX_SCHEDULED, "Scheduled"));
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
            incomingItem.setText(0, _browser.getTranslationRegistry().getText(T_INCOMING_NAME, "fetches"));
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

            String forum = _browser.getClient().getChannelName(scope);
            if (forum != null)
                actionItem.setText(0, forum + " [" + scope.toBase64().substring(0,6) + "]" + (uri.getMessageId() != null ? " - " + uri.getMessageId() : ""));
            else
                actionItem.setText(0, "[" + scope.toBase64().substring(0,6) + "]" + (uri.getMessageId() != null ? " - " + uri.getMessageId() : ""));

            if (action.isScheduled()) {
                actionItem.setText(1, _browser.getTranslationRegistry().getText(T_WHEN_ASAP, "ASAP"));
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
            } else if (action.isReadKeyUnknown()) {
                actionItem.setText(1, Constants.getDateTime(action.getCompletionTime()));                    
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_NOKEY);
                actionItem.setText(3, _browser.getTranslationRegistry().getText(T_FETCH_READKEYUNKNOWN, "Read key unknown"));
            } else if (action.getPBEPrompt() != null) {
                actionItem.setText(1, Constants.getDateTime(action.getCompletionTime()));                    
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_PBE);
                actionItem.setText(3, _browser.getTranslationRegistry().getText(T_FETCH_PBE, "Passphrase required: ") + action.getPBEPrompt());
            } else if (action.isReplyKeyUnknown()) {
                actionItem.setText(1, Constants.getDateTime(action.getCompletionTime()));                    
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_NOKEY);
                actionItem.setText(3, _browser.getTranslationRegistry().getText(T_FETCH_REPLYKEYUNKNOWN, "Reply key unknown"));
            } else if (action.isCorrupt()) {
                actionItem.setText(1, Constants.getDateTime(action.getCompletionTime()));                    
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_ERROR);
                actionItem.setText(3, _browser.getTranslationRegistry().getText(T_FETCH_CORRUPT, "Message is corrupt"));
            } else if (action.getFetchErrorMsg() != null) {
                actionItem.setText(1, Constants.getDateTime(action.getCompletionTime()));                    
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_ERROR);
                actionItem.setText(3, _browser.getTranslationRegistry().getText(T_FETCH_FAILED, "Fetch failed: ") + action.getFetchErrorMsg());
            } else if (action.isExecuting()) {
                actionItem.setText(1, _browser.getTranslationRegistry().getText(T_WHEN_NOW, "Now"));
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_INPROGRESS);
            } else { // complete
                actionItem.setText(1, Constants.getDateTime(action.getCompletionTime()));                    actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_OK);
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_OK);
                actionItem.setText(3, _browser.getTranslationRegistry().getText(T_FETCH_OK, "Imported"));
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
            outgoingItem.setText(0, _browser.getTranslationRegistry().getText(T_OUTGOING_NAME, "pushes"));
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

            String forum = _browser.getClient().getChannelName(scope);
            if (forum != null)
                actionItem.setText(0, forum + " [" + scope.toBase64().substring(0,6) + "]" + (uri.getMessageId() != null ? " - " + uri.getMessageId() : ""));
            else
                actionItem.setText(0, "[" + scope.toBase64().substring(0,6) + "]" + (uri.getMessageId() != null ? " - " + uri.getMessageId() : ""));

            if (action.isScheduled()) {
                actionItem.setText(1, _browser.getTranslationRegistry().getText(T_WHEN_ASAP, "ASAP"));
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
            } else if (action.isExecuting()) {
                actionItem.setText(1, _browser.getTranslationRegistry().getText(T_WHEN_NOW, "Now"));
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_INPROGRESS);
            } else if (action.getErrorMsg() != null) {
                actionItem.setText(1, Constants.getDateTime(action.getCompletionTime()));
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_ERROR);
                actionItem.setText(3, _browser.getTranslationRegistry().getText(T_PUSH_ERROR, "Error pushing: ") + action.getErrorMsg());
            } else { // complete
                actionItem.setText(1, Constants.getDateTime(action.getCompletionTime()));
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_OK);
                actionItem.setText(3, _browser.getTranslationRegistry().getText(T_PUSH_OK, "Pushed"));
            }
            resizeCols(actionItem);
        }
    }
    
    private void resizeCols(TreeItem item) {
        setMinWidth(_colName, item.getText(0), 0, 200);
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
        
        _add.setText(registry.getText(T_ADD, "Add archive"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel syndications"));
        _delete.setText(registry.getText(T_DELETE, "Delete archive"));
    }
    
    public void applyTheme(Theme theme) {
        _tree.setFont(theme.TREE_FONT);
        
        _colName.pack();
        _colTime.pack();
        _colStatus.pack();
        _colSummary.pack();
        
        _add.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
        _delete.setFont(theme.BUTTON_FONT);
    }

    //
    // now for the SyncArchive/SyncManager callbacks (will be called from arbitrary threads
    //
    
    public void archiveAdded(final SyncArchive archive) {
        if (_disposed) return;
        _browser.getUI().debugMessage("archive added: " + archive);
        archive.addListener(this);
        Display.getDefault().syncExec(new Runnable() { public void run() { loadData(archive); } });
    }

    public void archiveRemoved(final SyncArchive archive) {
        if (_disposed) return;
        _browser.getUI().debugMessage("archive removed: " + archive);
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
        _browser.getUI().debugMessage("archive loaded: " + archive);
        archive.addListener(this);
        Display.getDefault().syncExec(new Runnable() { public void run() { loadData(archive); } });
    }

    public void incomingUpdated(final SyncArchive.IncomingAction action) {
        if (_disposed) return;
        _browser.getUI().debugMessage("incoming action updated: " + action);
        Display.getDefault().syncExec(new Runnable() { public void run() { loadDataFetch(action); } });
    }

    public void outgoingUpdated(final SyncArchive.OutgoingAction action) {
        if (_disposed) return;
        _browser.getUI().debugMessage("outgoing action updated: " + action);
        Display.getDefault().syncExec(new Runnable() { public void run() { loadDataPush(action); } });
    }

    public void archiveUpdated(final SyncArchive archive) {
        if (_disposed) return;
        _browser.getUI().debugMessage("archiveUpdated(" + archive + ")");
        Display.getDefault().syncExec(new Runnable() { public void run() { loadData(archive); } });
    }

    public void onlineStatusUpdated(boolean nowOnline) {}
}
