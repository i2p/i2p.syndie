package syndie.gui;

import java.util.ArrayList;
import java.util.HashMap;
import net.i2p.data.DataHelper;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
import syndie.data.SyndieURI;
import syndie.db.SyndicationManager;

/**
 *
 */
public class SyndicationStatusView implements Translatable, SyndicationManager.SyndicationListener {
    private BrowserControl _browser;
    private Composite _parent;
    private Composite _root;
    private Table _table;
    /** icon for the type of record - metadata/post/private */
    private TableColumn _colType;
    /** target, but if not yet imported, just the scope */
    private TableColumn _colTarget;
    /** author, if imported and authenticated */
    private TableColumn _colAuthor;
    /** msgId */
    private TableColumn _colMsgId;
    /** archive fetched from */
    private TableColumn _colSource;
    /** stage in the update */
    private TableColumn _colStatus;
    /** error/pbe/etc */
    private TableColumn _colDetail;
    /** syndieURI to TableItem */
    private HashMap _uriToTableItem;
    /** sorted list of SyndieURIs */
    private ArrayList _sortedURIs;
    private MenuItem _view;
    private MenuItem _clear;
    private MenuItem _stop;
    
    public SyndicationStatusView(BrowserControl browser, Composite parent) {
        _browser = browser;
        _parent = parent;
        _uriToTableItem = new HashMap();
        _sortedURIs = new ArrayList(16);
        initComponents();
    }
    
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(1, true));
        
        _table = new Table(_root, SWT.MULTI|SWT.BORDER);
        _table.setHeaderVisible(true);
        _table.setLinesVisible(true);
        _table.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        Menu menu = new Menu(_table);
        _table.setMenu(menu);
        _view = new MenuItem(menu, SWT.PUSH);
        _view.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewSelected(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewSelected(); }
        });
        _clear = new MenuItem(menu, SWT.PUSH);
        _clear.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { clearSelected(); }
            public void widgetSelected(SelectionEvent selectionEvent) { clearSelected(); }
        });
        _stop = new MenuItem(menu, SWT.PUSH);
        _stop.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { stopSelected(); }
            public void widgetSelected(SelectionEvent selectionEvent) { stopSelected(); }
        });
        
        _colType = new TableColumn(_table, SWT.RIGHT);
        _colTarget = new TableColumn(_table, SWT.LEFT);
        _colAuthor = new TableColumn(_table, SWT.LEFT);
        _colMsgId = new TableColumn(_table, SWT.LEFT);
        _colSource = new TableColumn(_table, SWT.LEFT);
        _colStatus = new TableColumn(_table, SWT.CENTER);
        _colDetail = new TableColumn(_table, SWT.LEFT);
        
        _browser.getTranslationRegistry().register(this);
        _browser.getSyndicationManager().addListener(this);
    }
    
    private void add(SyndicationManager.FetchRecord record) {
        SyndieURI uri = record.getURI();
        byte scope[] = uri.getScope().getData();
        long msgId = (uri.getMessageId() != null ? uri.getMessageId().longValue() : -1);
        int insertIndex = 0;
        boolean added = false;
        synchronized (_sortedURIs) {
            for (int i = 0; i < _sortedURIs.size(); i++) {
                SyndieURI cur = (SyndieURI)_sortedURIs.get(i);
                if (cur.equals(uri)) return; // nothing to add
                int compare = DataHelper.compareTo(scope, cur.getScope().getData());
                if (compare < 0) {
                    // new record goes later
                } else if (compare == 0) {
                    if (cur.getMessageId() != null) {
                        if (msgId < cur.getMessageId().longValue()) {
                            insertIndex = i;
                            _sortedURIs.add(insertIndex, uri);
                            added = true;
                            break;
                        } else {
                            // later in the forum
                        }
                    } else {
                        // metadata always comes first
                    }
                } else {
                    insertIndex = i;
                    _sortedURIs.add(insertIndex, uri);
                    added = true;
                    break;
                }
            }
            if (!added) {
                insertIndex = _sortedURIs.size();
                _sortedURIs.add(uri);
            }
        }
        
        add(record, insertIndex);
    }
    
    private void add(final SyndicationManager.FetchRecord record, final int insertIndex) {
         // called from arbitrary thread, so be safe
        _root.getDisplay().syncExec(new Runnable() { public void run() { doAdd(record, insertIndex); } });
    }
    private void doAdd(final SyndicationManager.FetchRecord record, final int insertIndex) {
        TableItem item = new TableItem(_table, SWT.NONE, insertIndex);
        _uriToTableItem.put(record.getURI(), item);
        String prefix = record.getURI().getScope().toBase64().substring(0,6);
        update(item, record, false, "", prefix);
        setMinWidth(_colTarget, item.getText(1));
        setMinWidth(_colAuthor, item.getText(2));
        setMinWidth(_colMsgId, item.getText(3));
        setMinWidth(_colSource, item.getText(4));
    }
    private void setMinWidth(TableColumn col, String text) {
        int width = ImageUtil.getWidth(text, _table) + _table.getGridLineWidth()*2;
        int existing = col.getWidth();
        if (width > existing) {
            _browser.getUI().debugMessage("Increasing the width on " + col.getText() + " from " + existing + " to " + width);
            col.setWidth(width);
        } else {
            _browser.getUI().debugMessage("Keeping the width on " + col.getText() + " at " + existing + " (new width would be " + width + ")");
        }
    }
    private void update(final SyndicationManager.FetchRecord record) {
        final TableItem item = (TableItem)_uriToTableItem.get(record.getURI());
        
        // this db access is done outside the display thread in most (all?) cases
        String authorName = null;
        String targetName = null;
        boolean priv = false;
        if ((record.getStatus() == SyndicationManager.FETCH_IMPORT_PBE) ||
              (record.getStatus() == SyndicationManager.FETCH_IMPORT_OK)) {
            if (record.getURI().getMessageId() != null){
                long chanId = _browser.getClient().getChannelId(record.getURI().getScope());
                MessageInfo msg = _browser.getClient().getMessage(chanId, record.getURI().getMessageId());
                if (msg != null) {
                    priv = msg.getWasPrivate();
                    long targetId = msg.getTargetChannelId();
                    ChannelInfo target = _browser.getClient().getChannel(targetId);
                    if (target != null)
                        targetName = target.getName() + ": " + target.getChannelHash().toBase64().substring(0,6);
                    else
                        targetName = msg.getTargetChannel().toBase64().substring(0,6);

                    long authorId = msg.getAuthorChannelId();
                    if (authorId == targetId) {
                        authorName = targetName;
                    } else {
                        ChannelInfo author = _browser.getClient().getChannel(targetId);
                        if (author != null)
                            authorName = author.getName() + ": " + author.getChannelHash().toBase64().substring(0,6);
                        else
                            authorName = msg.getScopeChannel().toBase64().substring(0,6);
                    }
                }
            } else {
                // meta
                long chanId = _browser.getClient().getChannelId(record.getURI().getScope());
                ChannelInfo target = _browser.getClient().getChannel(chanId);
                if (target != null)
                    targetName = target.getName() + ": " + target.getChannelHash().toBase64().substring(0,6);
                else
                    targetName = record.getURI().getScope().toBase64().substring(0,6);
            }
        }
        final boolean isPrivate = priv;
        final String target = (targetName == null ? record.getURI().getScope().toBase64().substring(0,6) : targetName);
        final String author = (authorName == null ? "" : authorName);

        // called from arbitrary thread, so be safe
        if (item != null)
            _root.getDisplay().syncExec(new Runnable() { public void run() { update(item, record, isPrivate, author, target); } });
    }
    private void update(TableItem item, SyndicationManager.FetchRecord record, boolean isPrivate, String author, String target) {
        SyndieURI uri = record.getURI();
        if (record.getURI().getMessageId() == null) {
            item.setImage(0, ImageUtil.ICON_MSG_TYPE_META);
        } else if ( (record.getStatus() == SyndicationManager.FETCH_IMPORT_PBE) ||
                    (record.getStatus() == SyndicationManager.FETCH_IMPORT_OK) ) {
            if (isPrivate)
                item.setImage(0, ImageUtil.ICON_MSG_TYPE_PRIVATE);
            else
                item.setImage(0, ImageUtil.ICON_MSG_TYPE_NORMAL);
        } else {
            item.setImage(0, null);
        }
        
        if ( (record.getStatus() == SyndicationManager.FETCH_IMPORT_PBE) ||
             (record.getStatus() == SyndicationManager.FETCH_IMPORT_OK) ) {
            setMinWidth(_colAuthor, author);
            setMinWidth(_colTarget, target);
        }
        
        item.setText(1, target);
        item.setText(2, author);
        if (uri.getMessageId() != null)
            item.setText(3, uri.getMessageId().toString());
        else
            item.setText(3, "");
        item.setText(4, record.getSource());
        
        switch (record.getStatus()) {
            case SyndicationManager.FETCH_COMPLETE:
                item.setImage(5, ImageUtil.ICON_SYNDICATE_STATUS_INPROGRESS);
                //item.setText(5, "fetchComplete");
                break;
            case SyndicationManager.FETCH_FAILED:
            case SyndicationManager.FETCH_IMPORT_CORRUPT:
                item.setImage(5, ImageUtil.ICON_SYNDICATE_STATUS_ERROR);
                break;
            case SyndicationManager.FETCH_IMPORT_OK:
                item.setImage(5, ImageUtil.ICON_SYNDICATE_STATUS_OK);
                //item.setText(5, "importOK");
                break;
            case SyndicationManager.FETCH_IMPORT_PBE:
                item.setImage(5, ImageUtil.ICON_SYNDICATE_STATUS_PBE);
                //item.setText(5, "pbe");
                break;
            case SyndicationManager.FETCH_IMPORT_NOKEY:
                item.setImage(5, ImageUtil.ICON_SYNDICATE_STATUS_NOKEY);
                break;
            case SyndicationManager.FETCH_SCHEDULED:
                item.setImage(5, ImageUtil.ICON_SYNDICATE_STATUS_SCHEDULED);
                //item.setText(5, "scheduled");
                break;
            case SyndicationManager.FETCH_STARTED:
                item.setImage(5, ImageUtil.ICON_SYNDICATE_STATUS_INPROGRESS);
                //item.setText(5, "started");
                break;
            case SyndicationManager.FETCH_STOPPED:
                item.setImage(5, ImageUtil.ICON_SYNDICATE_STATUS_ERROR);
                //item.setText(5, "started");
                break;
        }
        String detail = record.getDetail();
        if (detail == null)
            item.setText(6, "");
        else
            item.setText(6, detail);
    }
    
    private void packCols() {
        _colAuthor.pack();
        _colDetail.pack();
        _colMsgId.pack();
        _colSource.pack();
        _colTarget.pack();
        _colType.setWidth(24);
        _colStatus.setWidth(24);
    }
    
    private void viewSelected() {
        int indexes[] = _table.getSelectionIndices();
        if (indexes != null) {
            for (int i = 0; i < indexes.length; i++) {
                SyndieURI uri = (SyndieURI)_sortedURIs.get(indexes[i]);
                _browser.view(uri);
            }
        }
    }

    private void clearSelected() {
        int indexes[] = _table.getSelectionIndices();
        if (indexes != null) {
            ArrayList removed = new ArrayList();
            ArrayList removedItems = new ArrayList();
            synchronized (_sortedURIs) {
                for (int i = 0; i < indexes.length; i++) {
                    SyndieURI uri = (SyndieURI)_sortedURIs.get(indexes[i]);
                    TableItem item = (TableItem)_uriToTableItem.remove(uri);
                    removedItems.add(item);
                    removed.add(uri);
                }
            }
            for (int i = 0; i < removedItems.size(); i++)
                ((TableItem)removedItems.get(i)).dispose();
            _sortedURIs.removeAll(removed);
        }
    }
    
    private void stopSelected() {
        int indexes[] = _table.getSelectionIndices();
        if (indexes != null) {
            for (int i = 0; i < indexes.length; i++) {
                SyndieURI uri = (SyndieURI)_sortedURIs.get(indexes[i]);
                TableItem item = (TableItem)_uriToTableItem.get(uri);
                item.setImage(5, ImageUtil.ICON_SYNDICATE_STATUS_ERROR);
                _browser.getSyndicationManager().stopFetching(uri);
            }
        }
    }

    private static final String T_TYPE = "syndie.gui.syndicationstatusview.type";
    private static final String T_TYPE_TOOLTIP = "syndie.gui.syndicationstatusview.type_tooltip";
    private static final String T_TARGET = "syndie.gui.syndicationstatusview.target";
    private static final String T_TARGET_TOOLTIP = "syndie.gui.syndicationstatusview.target_tooltip";
    private static final String T_AUTHOR = "syndie.gui.syndicationstatusview.author";
    private static final String T_AUTHOR_TOOLTIP = "syndie.gui.syndicationstatusview.author_tooltip";
    private static final String T_MSGID = "syndie.gui.syndicationstatusview.msgid";
    private static final String T_MSGID_TOOLTIP = "syndie.gui.syndicationstatusview.msgid_tooltip";
    private static final String T_SOURCE = "syndie.gui.syndicationstatusview.source";
    private static final String T_SOURCE_TOOLTIP = "syndie.gui.syndicationstatusview.source_tooltip";
    private static final String T_STATUS = "syndie.gui.syndicationstatusview.status";
    private static final String T_STATUS_TOOLTIP = "syndie.gui.syndicationstatusview.status_tooltip";
    private static final String T_DETAILS = "syndie.gui.syndicationstatusview.details";
    private static final String T_DETAILS_TOOLTIP = "syndie.gui.syndicationstatusview.details_tooltip";
    private static final String T_VIEW = "syndie.gui.syndicationstatusview.view";
    private static final String T_CLEAR = "syndie.gui.syndicationstatusview.clear";
    private static final String T_STOP = "syndie.gui.syndicationstatusview.stop";

    public void translate(TranslationRegistry registry) {
        _colType.setText(registry.getText(T_TYPE, ""));
        _colType.setToolTipText(registry.getText(T_TYPE_TOOLTIP, "Type of resource"));
        _colTarget.setText(registry.getText(T_TARGET, "Forum"));
        _colTarget.setToolTipText(registry.getText(T_TARGET_TOOLTIP, "Forum being fetched"));
        _colAuthor.setText(registry.getText(T_AUTHOR, "Author"));
        _colAuthor.setToolTipText(registry.getText(T_AUTHOR_TOOLTIP, "Author of the message"));
        _colMsgId.setText(registry.getText(T_MSGID, "#"));
        _colMsgId.setToolTipText(registry.getText(T_MSGID_TOOLTIP, "Individual message ID"));
        _colSource.setText(registry.getText(T_SOURCE, "Source"));
        _colSource.setToolTipText(registry.getText(T_SOURCE_TOOLTIP, "Archive containing the resource"));
        _colStatus.setText(registry.getText(T_STATUS, ""));
        _colStatus.setToolTipText(registry.getText(T_STATUS_TOOLTIP, "Current fetch status"));
        _colDetail.setText(registry.getText(T_DETAILS, "Details"));
        _colDetail.setToolTipText(registry.getText(T_DETAILS_TOOLTIP, "Details regarding the fetch"));
        _view.setText( registry.getText(T_VIEW, "View selected"));
        _clear.setText(registry.getText(T_VIEW, "Clear selected"));
        _stop.setText(registry.getText(T_VIEW, "Stop selected"));
        packCols();
    }
    
    public void archiveAdded(SyndicationManager mgr, String name) {}
    public void archiveRemoved(SyndicationManager mgr, String name) {}
    public void archiveUpdated(SyndicationManager mgr, String oldName, String newName) {}
    public void archiveIndexStatus(SyndicationManager mgr, String archiveName, int status, String msg) {}
    public void fetchStatusUpdated(SyndicationManager mgr, SyndicationManager.FetchRecord record) {
        _browser.getUI().debugMessage("fetchStatus of " + record.getURI() + ": " + record.getStatus());
        if (!_uriToTableItem.containsKey(record.getURI())) //if (record.getStatus() == SyndicationManager.FETCH_SCHEDULED)
            add(record);
        else
            update(record);
    }
}
