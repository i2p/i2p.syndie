package syndie.gui;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.i2p.data.DataHelper;
import net.i2p.data.Hash;

import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
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

import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.ImportResult;
import static syndie.db.ImportResult.Detail.*;
import syndie.db.SyncArchive;
import syndie.db.SyncManager;
import syndie.db.UI;
import syndie.util.DateTime;


/**
 *  All the work for the SyndicatorTab
 */
public class Syndicator extends BaseComponent implements Translatable, Themeable, SyncManager.SyncListener, SyncArchive.SyncArchiveListener {
    private NavigationControl _navControl;
    private Composite _parent;
    private final DataCallback _dataCallback;
    
    private Composite _root;
    private Composite _top;
    private Tree _tree;
    private TreeColumn _colName;
    private TreeColumn _colTime;
    private TreeColumn _colStatus;
    private TreeColumn _colSummary;
    private Menu _treeMenu;
    private Composite _bottom;
    private Disposable _detail;
    
    private Button _add;
    private Button _cancel;
    private Button _delete;
    private Button _syncRecurring;
    private Button _syncOnce;
    
    private final Map<String, TreeItem> _archiveNameToRootItem;
    private final Map<String, TreeItem> _archiveNameToIndexItem;
    /** name to TreeItem for the subtree item */
    private final Map<String, TreeItem> _archiveNameToIncomingItem;
    /** name to Map of SyndieURI to TreeItem for pending fetches */
    private final Map<String, Map<SyndieURI, TreeItem>> _archiveNameToIncoming;
    /** name to TreeItem for the subtree item */
    private final Map<String, TreeItem> _archiveNameToOutgoingItem;
    /** name to Map of SyndieURI to TreeItem for pending fetches */
    private final Map<String, Map<SyndieURI, TreeItem>> _archiveNameToOutgoing;

    /** 
     * map of TreeItem to descriptive detail about the item they represent.  The values
     * are of different types, depending on the item selected.  For archive roots,
     * they contain a SyncArchive.  For the elements directly under the archive root, they
     * contain a string ("fetchindex", "incoming", "outgoing"), and for elements under
     * the incoming and outgoing branches, they contain SyncArchive.InboundAction and 
     * SyncArchive.OutboundAction elements, respectively
     */
    private final Map<TreeItem, Object> _items;
    
    private boolean _disposed;
    
    private final boolean _showActions;
    
    private final Object _pullLock = new Object();
    private final Object _pushLock = new Object();

    /**
     *  Limit how many fetches or pushes are shown for a single archive,
     *  as it really slows down the UI when there are 10K shown
     */
    private static final int MAX_SHOW_RECORDS = 50;


    /** @param callback may be null */
    public Syndicator(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl navControl,
                      DataCallback callback, Composite parent) {
        this(client, ui, themes, trans, navControl, callback, parent, true);
    }

    /** @param callback may be null */
    public Syndicator(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl navControl,
                      DataCallback callback, Composite parent, boolean showActions) {
        super(client, ui, themes, trans);
        _navControl = navControl;
        _parent = parent;
        _dataCallback = callback;
        _showActions = showActions;
        
        _archiveNameToRootItem = new HashMap();
        _archiveNameToIndexItem = new HashMap();
        _archiveNameToIncomingItem = new HashMap();
        _archiveNameToIncoming = new HashMap();
        _archiveNameToOutgoingItem = new HashMap();
        _archiveNameToOutgoing = new HashMap();
        _items = new HashMap();

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
            _top = new Composite(_root, SWT.NONE);
            _top.setLayout(new FillLayout(SWT.HORIZONTAL));
            _top.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));

            // actual action buttons...
            _add = new Button(_top, SWT.PUSH);
            _cancel = new Button(_top, SWT.PUSH);
            _delete = new Button(_top, SWT.PUSH);

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
        
        if (_showActions) {
            _bottom = new Composite(_root, SWT.NONE);
            _bottom.setLayout(new FillLayout(SWT.HORIZONTAL));
            _bottom.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));

            _syncRecurring = new Button(_bottom, SWT.PUSH);
            _syncRecurring.addSelectionListener(new FireSelectionListener() { public void fire() { syncNowRecurring(); } });
            _syncOnce = new Button(_bottom, SWT.PUSH);
            _syncOnce.addSelectionListener(new FireSelectionListener() { public void fire() { syncNowOneTime(); } });
        }
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
        
        loadData();
    }

    // to prevent tagging by xgettext
    private static final String NAME = "name";
    
    public void show(SyndieURI uri) {
        if (uri.isArchive()) {
            SyncManager mgr = SyncManager.getInstance(_client, _ui);
            for (SyncArchive archive : mgr.getArchives()) {
                if (archive.getURL().equals(uri.getURL())) {
                    _ui.debugMessage("viewing [" + archive.getURL() + "] [" + archive.getName() + "]");
                    TreeItem item = _archiveNameToRootItem.get(archive.getName());
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
            archive.setName(uri.getString(NAME));
            _ui.debugMessage("editing as if we are viewing [" + archive.getURL() + "] [" + archive.getName() + "]");
            viewDetailArchive(archive, false);
        }
    }
    
    public void forceFocus() { _tree.forceFocus(); }

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
        for (SyncArchive archive : mgr.getArchives()) {
            archive.stop(getText("Cancelled"));
        }
    }
    public void delete() { 
        if (_tree.getSelectionCount() != 1) return;
        TreeItem item = _tree.getSelection()[0];
        Object val = _items.get(item);
        if (val == null) return;
        if (val instanceof SyncArchive) {
            SyncArchive archive = (SyncArchive)val;
            MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO);
            box.setMessage(getText("Are you sure you want to delete this archive? ") + archive.getName());
            box.setText(getText("Delete?"));
            int rc = box.open();
            if (rc == SWT.YES)
                archive.delete();
        }
    }
    
    
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

    /**
     *  @param fireDefaultAction false for select, true for doubleclick/return
     */
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
    
    private void setNextSyncTime(SyncArchive archive) {
        archive.setNextSyncTime();
        archive.store(true);
    }

    private void setNextSyncTime(SyncArchive archive, long when) {
        archive.setNextSyncTime(when);
        archive.store(true);
    }
    
    public void syncNowRecurring() {
        if (_tree.getSelectionCount() == 0) {
            TreeItem[] items = _tree.getItems();
            int count = _tree.getItemCount(); 
            for (int c = 0; c < count; c++) {
                Object val = _items.get(items[c]);
                if (val instanceof SyncArchive)
                    syncNowRecurring((SyncArchive) val);
            }
        } else {
            TreeItem[] items = _tree.getSelection();
            int count = _tree.getSelectionCount();
            for (int c = 0; c < count; c++) {
                Object val = _items.get(items[c]);
                if (val instanceof SyncArchive)
                    syncNowRecurring((SyncArchive) val);
            }
        }
    }

    private void syncNowRecurring(SyncArchive archive) {
        archive.setNextSyncOneOff(false);
        setNextSyncTime(archive, System.currentTimeMillis());
    }
    
    public void syncNowOneTime() {
        if (_tree.getSelectionCount() == 0) {
            TreeItem[] items = _tree.getItems();
            int count = _tree.getItemCount(); 
            for (int c = 0; c < count; c++) {
                Object val = _items.get(items[c]);
                if (val instanceof SyncArchive)
                    syncNowOneTime((SyncArchive) val);
            }
        } else {
            TreeItem[] items = _tree.getSelection();
            int count = _tree.getSelectionCount();
            for (int c = 0; c < count; c++) {
                Object val = _items.get(items[c]);
                if (val instanceof SyncArchive)
                    syncNowOneTime((SyncArchive) val);
            }
        }
    }

    private void syncNowOneTime(SyncArchive archive) {
        archive.setNextSyncOneOff(true);
        setNextSyncTime(archive, System.currentTimeMillis());
    }
    
    private void buildMenuArchive(final SyncArchive archive) {
        MenuItem item = new MenuItem(_treeMenu, SWT.PUSH);
        item.setText(getText("Settings") + "...");
        item.setImage(ImageUtil.ICON_SETTINGS);
        item.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                viewDetailArchive(archive, true);
            }
        });
        
        item = new MenuItem(_treeMenu, SWT.PUSH);
        item.setText(getText("Sync now (recurring)"));
        item.setImage(ImageUtil.ICON_SYNC);
        item.addSelectionListener(new FireSelectionListener() { public void fire() { syncNowRecurring(archive); } });
        
        item = new MenuItem(_treeMenu, SWT.PUSH);
        item.setText(getText("Sync now (one time)"));
        item.setImage(ImageUtil.ICON_SYNC);
        item.addSelectionListener(new FireSelectionListener() { public void fire() { syncNowOneTime(archive); } });
        
        item = new MenuItem(_treeMenu, SWT.PUSH);
        if (syncRunning(archive)) {
            item.setText(getText("Cancel sync"));
            item.setImage(ImageUtil.ICON_CANCEL);
            item.addSelectionListener(new FireSelectionListener() { public void fire() { archive.stop(getText("Cancelled")); } });
        } else if (archive.getNextSyncTime() > 0) {
            item.setText(getText("Cancel next sync"));
            item.setImage(ImageUtil.ICON_CANCEL);
            item.addSelectionListener(new FireSelectionListener() { public void fire() { setNextSyncTime(archive, -1); archiveUpdated(archive); } });
        } else {
            item.setText(getText("Schedule sync"));
            item.setImage(ImageUtil.ICON_SYNC);
            item.addSelectionListener(new FireSelectionListener() { public void fire() { setNextSyncTime(archive); archiveUpdated(archive); } });
        }
        
        // only allow clear when there isn't a sync in progress
        if ( !syncRunning(archive) && ( (archive.getIncomingActionCount() > 0) || (archive.getOutgoingActionCount() > 0) ) ) {
            item = new MenuItem(_treeMenu, SWT.PUSH);
            item.setText(getText("Clear complete actions"));
            item.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    archive.clearCompletedActions(true, true);
                }
            });
        }
    }
    
    private void buildMenuIncoming(TreeItem item) {
        final SyncArchive archive = (SyncArchive)_items.get(item.getParentItem());
        if (!syncRunning(archive) && archive.getIncomingActionCount() > 0) {
            MenuItem clear = new MenuItem(_treeMenu, SWT.PUSH);
            clear.setText(getText("Clear complete actions"));
            clear.setImage(ImageUtil.ICON_CLEAR);
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
            clear.setText(getText("Clear complete actions"));
            clear.setImage(ImageUtil.ICON_CLEAR);
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
        clear.setText(getText("Clear"));
        clear.setImage(ImageUtil.ICON_CLEAR);
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
            item.setText(getText("View"));
            item.setImage(ImageUtil.ICON_VIEW);
            item.addSelectionListener(new FireSelectionListener() {
                public void fire() { _navControl.view(uri); }
            });
            if (action.getPBEPrompt() != null) {
                item = new MenuItem(_treeMenu, SWT.PUSH);
                item.setText(getText("Resolve passphrase prompt"));
                item.addSelectionListener(new FireSelectionListener() {
                    public void fire() { _navControl.view(uri); }
                });
            }
            if (!syncRunning(action.getArchive())) {
                item = new MenuItem(_treeMenu, SWT.PUSH);
                item.setText(getText("Clear action"));
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
            item.setText(getText("Cancel"));
            item.setImage(ImageUtil.ICON_DELETE);
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                private void fire() {
                    action.cancel(getText("Cancelled"));
                }
            });
        }
    }
    private void buildMenuOutgoing(final SyncArchive.OutgoingAction action) {
        final SyndieURI uri = action.getURI();
        MenuItem item = new MenuItem(_treeMenu, SWT.PUSH);
        item.setText(getText("View"));
        item.setImage(ImageUtil.ICON_VIEW);
        item.addSelectionListener(new FireSelectionListener() {
            public void fire() { _navControl.view(uri); }
        });
        
        if (!syncRunning(action.getArchive()) && action.isComplete()) {
            item = new MenuItem(_treeMenu, SWT.PUSH);
            item.setText(getText("Clear action"));
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                private void fire() {
                    action.dispose();
                }
            });
        } else if (action.isScheduled()) {
            item = new MenuItem(_treeMenu, SWT.PUSH);
            item.setText(getText("Cancel"));
            item.setImage(ImageUtil.ICON_DELETE);
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                private void fire() {
                    action.cancel(getText("Cancelled"));
                }
            });
        }
    }
    
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

    public Set<Hash> getFetchedScopes() {
        Set<Hash> scopes = new HashSet();
        for (Map<SyndieURI, TreeItem> uriToItem : _archiveNameToIncoming.values()) {
            for (SyndieURI uri : uriToItem.keySet()) {
                if (uri == null) continue;
                if (scopes.contains(uri.getScope())) continue;
                if (uri.getMessageId() != null) {
                    long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                    if (msgId >= 0) {
                        long targetId = _client.getMessageTarget(msgId);
                        scopes.add(_client.getChannelHash(targetId));
                    }
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
        s.setText(getText("HTTP archive"));
        s.setFont(_themeRegistry.getTheme().SHELL_FONT);
        s.setLayout(new FillLayout());
        _detail = new SyndicatorDetailHTTPArchive(_client, _ui, _themeRegistry, _translationRegistry, s, archive, new SyndicationDetailListener() {
            public void cancelled() { s.dispose(); }
            public void saved() { s.dispose(); }
            public void scheduleUpdated() { s.dispose(); }
        });
        s.pack();
        s.open();
        TreeItem item = _archiveNameToRootItem.get(archive.getName());
        if (item != null) // null for new ones
            _tree.showItem(item);
    }

    private void viewDetailIncoming(TreeItem item, boolean fireDefaultAction) {}
    private void viewDetailOutgoing(TreeItem item, boolean fireDefaultAction) {}
    private void viewDetailFetchIndex(TreeItem item, boolean fireDefaultAction) {}

    /** view the message if we can */
    private void viewDetailIncoming(SyncArchive.IncomingAction action, boolean fireDefaultAction) {
        ImportResult.Result result = action.getResult();
        if (fireDefaultAction && action.isComplete() &&
            action.getFetchErrorMsg() == null &&
            result != IMPORT_NO_READ_KEY &&
            result != IMPORT_NO_REPLY_KEY &&
            result != IMPORT_UNREADABLE &&
            result != IMPORT_CANCEL_STUB) {
            _navControl.view(action.getURI());
        }
    }

    /** view the message if we can */
    private void viewDetailOutgoing(SyncArchive.OutgoingAction action, boolean fireDefaultAction) {
        if ( fireDefaultAction && (action.isComplete()) && (action.getErrorMsg() == null) )
            _navControl.view(action.getURI());
    }
    
    private void loadData() {
        SyncManager mgr = SyncManager.getInstance(_client, _ui);
        mgr.loadArchives();
        mgr.addListener(this);
        
        for (SyncArchive archive : mgr.getArchives()) {
                archive.addListener(this);
                loadData(archive);
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
        //_ui.debugMessage("loadDataRoot(" + archive + ")", new Exception("I did it"));
        _ui.debugMessage("loadDataRoot(" + archive + "): nextTime in " + (nextTime-System.currentTimeMillis()) + "ms");
        TreeItem rootItem = _archiveNameToRootItem.get(archive.getName());
        if (rootItem == null) {
            rootItem = new TreeItem(_tree, SWT.NONE);
            _archiveNameToRootItem.put(archive.getName(), rootItem);
            _items.put(rootItem, archive);
            refreshHeaders();
        }
        rootItem.setText(0, archive.getName());
        if (archive.getSyncInProgress()) {
            rootItem.setText(1, getText("Now"));
            rootItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_INPROGRESS);
            rootItem.setText(3, getText("Syndicating"));
        } else if (nextTime <= 0) {
            rootItem.setText(1, getText("Never"));
            rootItem.setImage(2, null);
            rootItem.setText(3, getText("No syndication scheduled"));
        } else if (!SyncManager.getInstance(_client, _ui).isOnline()) {
            rootItem.setText(1, DateTime.getDateTime(nextTime));
            rootItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
            rootItem.setText(3, getText("Offline: Set as online to start"));
        } else {
            rootItem.setText(1, DateTime.getDateTime(nextTime));
            rootItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
            rootItem.setText(3, getText("Syndication scheduled"));
        }
        
        resizeCols(rootItem);
        
        loadDataIndex(archive, rootItem);
        
        return rootItem;
    }
    
    private void loadDataIndex(SyncArchive archive, TreeItem rootItem) {
        long nextTime = getNextTime(archive);
        _ui.debugMessage("loadDataIndex(" + archive + "): nextTime in " + (nextTime-System.currentTimeMillis()) + "ms");
        
        TreeItem indexItem = _archiveNameToIndexItem.get(archive.getName());
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

        indexItem.setText(0, getText("Fetch index"));
        if (archive.getIndexFetchInProgress()) {
            indexItem.setText(1, getText("Now"));
            indexItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_INPROGRESS);
            StringBuilder buf = new StringBuilder();
            buf.append(getText("In Progress"));
            long p = archive.getIndexFetchRcvdBytes();
            long t = archive.getIndexFetchSize();
            if (p > 0)
                buf.append(":  ").append(formatSize(p));
            else if (t > 0)
                buf.append(":  0");
            if (t > 0)
                buf.append(" / ").append(formatSize(t));
            indexItem.setText(3, buf.toString());
        } else if (archive.getLastSyncTime() > 0) {
            indexItem.setText(1, DateTime.getDateTime(archive.getLastSyncTime()));
            if (archive.getLastIndexFetchErrorMsg() != null) {
                indexItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_ERROR);
                indexItem.setText(3, getText("Fetch error") + ": " + archive.getLastIndexFetchErrorMsg());
            } else {
                indexItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_OK);
                StringBuilder buf = new StringBuilder();
                buf.append(getText("Fetch complete"));
                long t = archive.getIndexFetchSize();
                if (t > 0)
                    buf.append(":  ").append(formatSize(t));
                indexItem.setText(3, buf.toString());
            }
        } else {
            // never successfully fetched
            indexItem.setText(1, getText("Never"));
            if (archive.getLastIndexFetchErrorMsg() != null) {
                indexItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_ERROR);
                indexItem.setText(3, getText("Fetch error") + ": " + archive.getLastIndexFetchErrorMsg());
            } else {
                indexItem.setImage(2, null);
                indexItem.setText(3, "");
            }
        }
        resizeCols(indexItem);
    }
    
    private void loadDataFetches(SyncArchive archive, TreeItem rootItem) {
        int count = archive.getIncomingActionCount();
        if (count > 0) {
            if (count > MAX_SHOW_RECORDS)
                count = MAX_SHOW_RECORDS;
            for (int i = 0; i < count; i++) {
                SyncArchive.IncomingAction action = archive.getIncomingAction(i);
                loadDataFetch(action);
            }
        } else {
            TreeItem incomingItem = _archiveNameToIncomingItem.remove(archive.getName());
            if (incomingItem != null) {
                incomingItem.dispose();
                _items.remove(incomingItem);
            }
            _archiveNameToIncoming.remove(archive.getName());
        }
    }

    /** create (or update) a fetch record (may create or update parents) */
    private void loadDataFetch(SyncArchive.IncomingAction action) {
        try {
            synchronized (_pullLock) {
                locked_loadDataFetch(action);
            }
        } catch (SWTException swt) {
            // widget is disposed... fixme...
            _ui.debugMessage("loadDataFetch", swt);
        }
    }

    /** synch to avoid disposed errors */
    private void locked_loadDataFetch(SyncArchive.IncomingAction action) {
        if (action != null) {
            SyndieURI uri = action.getURI();
            if (uri == null) return;
            Hash scope = uri.getScope();
            if (scope == null) return;

            Map<SyndieURI, TreeItem> incomingURIToItem = _archiveNameToIncoming.get(action.getArchive().getName());
            if (incomingURIToItem == null) {
                incomingURIToItem = new HashMap();
                _archiveNameToIncoming.put(action.getArchive().getName(), incomingURIToItem);
            }
            
            TreeItem incomingItem = _archiveNameToIncomingItem.get(action.getArchive().getName());
            
            if (incomingItem == null) {
                TreeItem rootItem = loadDataRoot(action.getArchive());
                incomingItem = new TreeItem(rootItem, SWT.NONE);
                _archiveNameToIncomingItem.put(action.getArchive().getName(), incomingItem);
                _items.put(incomingItem, "incoming");
                // text doesn't change
                incomingItem.setText(0, getText("Fetches"));
            }
            int rem = action.getArchive().getIncomingActionsInProgress();
            if (rem > 0) {
                int tot = action.getArchive().getIncomingActionCount();
                int done = Math.max(0, Math.min(tot, tot - rem));
                incomingItem.setText(3, done + " / " + tot);
            } else {
                incomingItem.setText(3, getText("Complete"));
            }
    
            resizeCols(incomingItem);
            
            TreeItem actionItem = incomingURIToItem.get(uri);
            
            if (action.isDisposed()) {
                incomingURIToItem.remove(uri);
                if (actionItem != null) {
                    actionItem.dispose();
                    _items.remove(actionItem);
                    //_ui.debugMessage("Disposed and removed sz = " + incomingItem.getItemCount());
                }
                
                if (action.getArchive().getIncomingActionCount() == 0) {
                    _archiveNameToIncomingItem.remove(action.getArchive().getName());
                    _archiveNameToIncoming.remove(action.getArchive().getName());
                    incomingItem.dispose();
                    _items.remove(incomingItem);
                }
                
                //_ui.debugMessage("Disposed " + action);
                return;
            }
            
            if (actionItem == null) {
                if(incomingItem.getItemCount() >= MAX_SHOW_RECORDS) {
                    // make room if possible
                    TreeItem firstItem = incomingItem.getItem(0);
                    SyncArchive.IncomingAction firstAction = (SyncArchive.IncomingAction) _items.get(firstItem);
                    if (firstAction != null && firstAction.isComplete()) {
                        firstItem.dispose();
                        _items.remove(firstItem);
                        //_ui.debugMessage("Full, removed firstItem");
                    } else {
                        // no room
                        //if (firstAction != null)
                        //    _ui.debugMessage("Full but firstItem not complete, firstItem = " + firstItem);
                        return;
                    }
                }
                actionItem = new TreeItem(incomingItem, SWT.NONE);
                incomingURIToItem.put(uri, actionItem);
                _items.put(actionItem, action);
                //_ui.debugMessage("Added " + action);
            } else if (actionItem.isDisposed()) {
                _items.remove(actionItem);
                incomingURIToItem.remove(uri);
                return;
            }

            String forum = _client.getChannelName(scope);
            actionItem.setText(0, UIUtil.displayName(forum, scope));
            Long id = uri.getMessageId();
            if (id != null)
                actionItem.setText(1, DateTime.getDateTime(id.longValue()));
            else
                actionItem.setText(1, getText("Forum info"));

            ImportResult.Result result = action.getResult();
            if (action.isScheduled()) {
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
            } else if (result == IMPORT_NO_READ_KEY ||
                       result == IMPORT_NO_REPLY_KEY ||
                       result == IMPORT_UNREADABLE) {
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_NOKEY);
                actionItem.setText(3, getText(result.msg()));
            } else if (result == IMPORT_PASS_REQD) {
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_PBE);
                String prompt = action.getPBEPrompt();
                if (prompt != null && prompt.length() > 0)
                    actionItem.setText(3, getText("Passphrase required") + ": " + action.getPBEPrompt());
                else
                    actionItem.setText(3, getText("Passphrase required"));
            } else if (result == IMPORT_CANCEL_STUB) {
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_OK);
                actionItem.setText(3, getText(result.msg()));
            } else if (action.getFetchErrorMsg() != null) {
                // from ImportResult, tagged there
                String msg = getText(action.getFetchErrorMsg());
                if (action.getFetchError() != null)
                    msg = msg + " - " + action.getFetchError().getMessage();
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_ERROR);
                actionItem.setText(3, getText("Fetch failed") + ": " + msg);
            } else if (action.isFetchingMeta()) {
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_INPROGRESS);
                StringBuilder buf = new StringBuilder();
                buf.append(getText("Fetching keys"));
                long p = action.getReceived();
                long t = action.getSize();
                if (p > 0)
                    buf.append(":  ").append(formatSize(p));
                else if (t > 0)
                    buf.append(":  0");
                if (t > 0)
                    buf.append(" / ").append(formatSize(t));
                actionItem.setText(3, buf.toString());
            } else if (action.isFetchingBody()) {
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_INPROGRESS);
                StringBuilder buf = new StringBuilder();
                buf.append(getText("Fetching message"));
                long p = action.getReceived();
                long t = action.getSize();
                if (p > 0)
                    buf.append(":  ").append(formatSize(p));
                else if (t > 0)
                    buf.append(":  0");
                if (t > 0)
                    buf.append(" / ").append(formatSize(t));
                actionItem.setText(3, buf.toString());
            } else if (action.isQueuedForProcessing()) {
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
                actionItem.setText(3, getText("Queued for Import"));
            } else if (action.isProcessing()) {
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_INPROGRESS);
                actionItem.setText(3, getText("Importing"));
            } else if (action.isExecuting()) {
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
                actionItem.setText(3, getText("Queued"));
            } else { // complete
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_OK);
                StringBuilder buf = new StringBuilder();
                buf.append(getText("Imported"));
                long t = action.getSize();
                if (t > 0)
                    buf.append(":  ").append(formatSize(t));
                actionItem.setText(3, buf.toString());
            }

            //_ui.debugMessage("Updated " + actionItem);
            resizeCols(actionItem);
        }
    }
    
    private void loadDataPushes(SyncArchive archive) {
        int count = archive.getOutgoingActionCount();
        if (count > 0) {
            if (count > MAX_SHOW_RECORDS)
                count = MAX_SHOW_RECORDS;
            for (int i = 0; i < count; i++) {
                SyncArchive.OutgoingAction action = archive.getOutgoingAction(i);
                loadDataPush(action);
            }
        } else {
            TreeItem outgoingItem = _archiveNameToOutgoingItem.remove(archive.getName());
            if (outgoingItem != null) {
                outgoingItem.dispose();
                _items.remove(outgoingItem);
            }
            _archiveNameToOutgoing.remove(archive.getName());
        }
    }

    /** create (or update) a push record (may create or update parents) */
    private void loadDataPush(SyncArchive.OutgoingAction action) {
        try {
            synchronized (_pushLock) {
                locked_loadDataPush(action);
            }
        } catch (SWTException swt) {
            // widget is disposed... fixme...
            _ui.debugMessage("loadDataPush", swt);
        }
    }

    /** synch to avoid disposed errors */
    private void locked_loadDataPush(SyncArchive.OutgoingAction action) {
        if (action != null) {
            SyndieURI uri = action.getURI();
            if (uri == null) return;
            Hash scope = uri.getScope();
            if (scope == null) return;

            TreeItem outgoingItem = _archiveNameToOutgoingItem.get(action.getArchive().getName());
            
            if (outgoingItem == null) {
                TreeItem rootItem = loadDataRoot(action.getArchive());
                outgoingItem = new TreeItem(rootItem, SWT.NONE);
                _archiveNameToOutgoingItem.put(action.getArchive().getName(), outgoingItem);
                _items.put(outgoingItem, "outgoing");
                // text doesn't change
                outgoingItem.setText(0, getText("Pushes"));
            }
            int rem = action.getArchive().getOutgoingActionsInProgress();
            if (rem > 0) {
                int tot = action.getArchive().getOutgoingActionCount();
                int done = Math.max(0, Math.min(tot, tot - rem));
                outgoingItem.setText(3, done + " / " + tot);
            } else {
                outgoingItem.setText(3, getText("Complete"));
            }

            Map<SyndieURI, TreeItem> outgoingURIToItem = _archiveNameToOutgoing.get(action.getArchive().getName());
            if (outgoingURIToItem == null) {
                outgoingURIToItem = new HashMap();
                _archiveNameToOutgoing.put(action.getArchive().getName(), outgoingURIToItem);
            }
            resizeCols(outgoingItem);
            
            TreeItem actionItem = outgoingURIToItem.get(uri);
            
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
                if (outgoingItem.getItemCount() >= MAX_SHOW_RECORDS) {
                    // make room if possible
                    TreeItem firstItem = outgoingItem.getItem(0);
                    SyncArchive.OutgoingAction firstAction = (SyncArchive.OutgoingAction) _items.get(firstItem);
                    if (firstAction != null && firstAction.isComplete()) {
                        firstItem.dispose();
                        _items.remove(firstItem);
                    } else {
                        return;
                    }
                }
                actionItem = new TreeItem(outgoingItem, SWT.NONE);
                outgoingURIToItem.put(uri, actionItem);
                _items.put(actionItem, action);
            } else if (actionItem.isDisposed()) {
                _items.remove(actionItem);
                outgoingURIToItem.remove(uri);
                return;
            }

            String forum = _client.getChannelName(scope);
            actionItem.setText(0, UIUtil.displayName(forum, scope));
            Long id = uri.getMessageId();
            if (id != null)
                actionItem.setText(1, DateTime.getDateTime(id.longValue()));
            else
                actionItem.setText(1, getText("Forum info"));

            if (action.isScheduled()) {
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
            } else if (action.isPushingMeta()) {
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_INPROGRESS);
                StringBuilder buf = new StringBuilder();
                buf.append(getText("Pushing keys"));
                long t = action.getSize();
                if (t > 0)
                    buf.append(":  ").append(formatSize(t));
                actionItem.setText(3, buf.toString());
            } else if (action.isPushingBody()) {
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_INPROGRESS);
                StringBuilder buf = new StringBuilder();
                buf.append(getText("Pushing message"));
                long t = action.getSize();
                if (t > 0)
                    buf.append(":  ").append(formatSize(t));
                actionItem.setText(3, buf.toString());
            } else if (action.isExecuting()) {
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
                StringBuilder buf = new StringBuilder();
                buf.append(getText("Queued"));
                long t = action.getSize();
                if (t > 0)
                    buf.append(":  ").append(formatSize(t));
                actionItem.setText(3, buf.toString());
            } else if (action.getErrorMsg() != null) {
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_ERROR);
                actionItem.setText(3, getText("Error pushing") + ": " + action.getErrorMsg());
            } else { // complete
                actionItem.setImage(2, ImageUtil.ICON_SYNDICATE_STATUS_OK);
                StringBuilder buf = new StringBuilder();
                buf.append(getText("Pushed"));
                long t = action.getSize();
                if (t > 0)
                    buf.append(":  ").append(formatSize(t));
                actionItem.setText(3, buf.toString());
            }
            resizeCols(actionItem);
        }
    }
    
    private static String formatSize(long sz) {
        return DataHelper.formatSize2(sz, false) + 'B';
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
    
    /** @since 1.102b-8 */
    private void refreshHeaders() {
        translate(_translationRegistry);
    }
    
    public void translate(TranslationRegistry registry) {
        boolean haveItems = !_archiveNameToRootItem.isEmpty();
        if (haveItems) {
            _colName.setText("");
            _colName.setImage(ImageUtil.ICON_REF_ARCHIVE);
            _colTime.setText(registry.getText("Time"));
            _colStatus.setText(registry.getText("Status"));
            _colSummary.setText(registry.getText("Summary"));
        } else {
            _colName.setText(registry.getText("No archives defined - click 'Add archive' to add one"));
            _colName.setImage(null);
            _colTime.setText("");
            _colStatus.setText("");
            _colSummary.setText("");
            setMinWidth(_colName, "No archives defined - click 'Add archive' to add one", 0, 300);
            if (_showActions) {
                _add.forceFocus();
            }
        }
        if (_showActions) {
            _add.setText(registry.getText("Add archive"));
            _add.setImage(ImageUtil.ICON_ADDARCHIVE);
            _cancel.setText(registry.getText("Cancel syndications"));
            _cancel.setImage(ImageUtil.ICON_CANCELSYNDICATIONS);
            _cancel.setEnabled(haveItems);
            _delete.setText(registry.getText("Delete archive"));
            _delete.setImage(ImageUtil.ICON_DELETEARCHIVE);
            _delete.setEnabled(haveItems);
            
            _syncRecurring.setText(registry.getText("Sync Now"));
            _syncRecurring.setImage(ImageUtil.ICON_SYNC);
            _syncOnce.setText(registry.getText("Sync Now (one time only)"));
            _syncOnce.setImage(ImageUtil.ICON_SYNC);
            _syncRecurring.setEnabled(haveItems);
            _syncOnce.setEnabled(haveItems);
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
            _syncRecurring.setFont(theme.BUTTON_FONT);
            _syncOnce.setFont(theme.BUTTON_FONT);
        }
    }

    //
    // now for the SyncArchive/SyncManager callbacks (will be called from arbitrary threads
    //
    
    public void archiveAdded(final SyncArchive archive) {
        if (_disposed) return;
        _ui.debugMessage("archive added: " + archive);
        archive.addListener(this);
        Display.getDefault().syncExec(new Runnable() {
            public void run() {
                loadData(archive);
                refreshHeaders();
            }
        });
    }

    public void archiveRemoved(final SyncArchive archive) {
        if (_disposed) return;
        _ui.debugMessage("archive removed: " + archive);
        Display.getDefault().syncExec(new Runnable() { 
            public void run() { 
                // putting these map removals in the swt thread means we can skip synchronization
                TreeItem rootItem = _archiveNameToRootItem.remove(archive.getName());
                _archiveNameToIncoming.remove(archive.getName());
                _archiveNameToIncomingItem.remove(archive.getName());
                _archiveNameToIndexItem.remove(archive.getName());
                _archiveNameToOutgoing.remove(archive.getName());
                _archiveNameToOutgoingItem.remove(archive.getName());
                if (rootItem != null) {
                    rootItem.dispose(); 
                    _items.remove(rootItem);
                }
                refreshHeaders();
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
        // Update ReferenceChooserTree
        // This doesn't work, see Browser.forumCreated()
        //if (_dataCallback != null)
        //    _dataCallback.forumCreated();
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
        Display.getDefault().syncExec(new Runnable() {
            public void run() {
                String oldName = archive.getPreviousName();
                String newName = archive.getName();
                if (oldName != null && newName != null && !oldName.equals(newName)) {
                    // Big fixup on archive rename
                    TreeItem t = _archiveNameToRootItem.remove(oldName);
                    if (t != null) {
                        _archiveNameToRootItem.put(newName, t);
                        t.setText(0, newName);
                        resizeCols(t);
                        refreshHeaders();
                    }
                    t = _archiveNameToIndexItem.remove(oldName);
                    if (t != null)
                        _archiveNameToIndexItem.put(newName, t);
                    t = _archiveNameToIncomingItem.remove(oldName);
                    if (t != null)
                        _archiveNameToIncomingItem.put(newName, t);
                    t = _archiveNameToOutgoingItem.remove(oldName);
                    if (t != null)
                        _archiveNameToIncomingItem.put(newName, t);
                    Map<SyndieURI, TreeItem> m = _archiveNameToIncoming.remove(oldName);
                    if (m != null)
                        _archiveNameToIncoming.put(newName, m);
                    m = _archiveNameToOutgoing.remove(oldName);
                    if (m != null)
                        _archiveNameToOutgoing.put(newName, m);
                    // TODO does not resort
                }
                loadData(archive);
            }
        });
    }

    public void onlineStatusUpdated(boolean nowOnline) {}
}
