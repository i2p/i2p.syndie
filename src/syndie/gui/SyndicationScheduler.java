package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.MenuListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.SyndieURI;
import syndie.db.SharedArchiveEngine;
import syndie.db.SyndicationManager;

/**
 *
 */
public class SyndicationScheduler implements Themeable, Translatable, SyndicationManager.SyndicationListener {
    private BrowserControl _browser;
    private Composite _parent;
    private Composite _root;
    private Group _archiveGroup;
    private List _archives;
    private Button _archiveAdd;
    private Label _archiveNameLabel;
    private Text _archiveName;
    private Label _urlLabel;
    private Text _url;
    private Label _passphraseLabel;
    private Text _passphrase;
    private Label _lastSyncLabel;
    private Label _lastSync;
    private Label _nextSyncLabel;
    private Combo _nextSyncCombo;
    private Label _proxyLabel;
    private Button _proxyDefault;
    private Button _proxyNone;
    private Button _proxyCustom;
    private Label _proxyCustomHostLabel;
    private Text _proxyCustomHost;
    private Label _proxyCustomPortLabel;
    private Text _proxyCustomPort;
    private Button _saveArchive;
    private Button _revertArchive;
    private Button _deleteArchive;
    private Button _pullPolicy;
    private Menu _pullMenu;
    private MenuItem _pullRecentDelta;
    private MenuItem _pullAllDelta;
    private MenuItem _pullRecentKnown;
    private MenuItem _pullAllKnown;
    private MenuItem _pullPIR;
    private MenuItem _pullNothing;
    private MenuItem _pullPrivate;
    private MenuItem _pullPBE;
    private MenuItem _pullSizePer;
    private Menu _pullSizePerMenu;
    private MenuItem _pullSizePer32;
    private MenuItem _pullSizePer64;
    private MenuItem _pullSizePer128;
    private MenuItem _pullSizePer256;
    private MenuItem _pullSizePer512;
    private MenuItem _pullSizePer1024;
    private MenuItem _pullSizePer2048;
    private MenuItem _pullSizePer4096;
    private MenuItem _pullDiscover;
    private Button _pushPolicy;
    private Menu _pushMenu;
    private MenuItem _pushAllDelta;
    private MenuItem _pushLocalDelta;
    private MenuItem _pushNothing;
    private Group _eventGroup;
    private Table _eventTable;
    private TableColumn _colWhen;
    private TableColumn _colArchive;
    private TableColumn _colScope;
    private TableColumn _colEvent;
    private TableColumn _colDetail;
    private Menu _eventMenu;
    private MenuItem _eventView;
    private MenuItem _eventClearSelected;
    private MenuItem _eventClearAll;
    private Button _eventClear;
    
    public SyndicationScheduler(BrowserControl browser, Composite parent) {
        _browser = browser;
        _parent = parent;
        initComponents();
    }
    
    public void dispose() {
        _browser.getSyndicationManager().removeListener(this);
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
    }

    public void show(SyndieURI uri) {
        if (uri != null) {
            if (uri.isArchive()) {
                SyndicationManager mgr = _browser.getSyndicationManager();
                String name = mgr.getArchiveName(uri);
                if (name != null)
                    refreshView(name);
                else if (uri.getURL() != null)
                    showNewArchive(uri.getString("name"), uri.getURL());
            }
        }
    }
    
    private void refreshView() { refreshView(null); }
    private void refreshView(String forceSelection) { refreshView(forceSelection, false); }
    private void refreshView(String forceSelection, boolean forceAsNow) {
        SyndicationManager mgr = _browser.getSyndicationManager();
        _root.setRedraw(false);
        String sel = forceSelection;
        if (sel == null) {
            String selection[] = _archives.getSelection();
            if ( (selection != null) && (selection.length > 0) )
                sel = selection[0];
        }
        _archives.removeAll();
        int archives = mgr.getArchiveCount();
        for (int i = 0; i < archives; i++)
            _archives.add(mgr.getArchiveName(i));
        if ( (sel == null) && (archives > 0) )
            sel = _archives.getItem(0);
        _archives.setSelection(new String[] { sel });
        showArchive(sel, forceAsNow);
        _root.setRedraw(true);
    }
    
    private void showArchive() {
        String names[] = _archives.getSelection();
        if ( (names != null) && (names.length > 0) ) {
            showArchive(names[0]);
        } else {
            showArchive(null);
        }
    }
    private void showArchive(String name) { showArchive(name, false); }
    private void showArchive(String name, boolean forceAsNow) {
        boolean enable = false;
        if ( (name != null) && (name.length() > 0) ) {
            enable = true;
            SyndicationManager mgr = _browser.getSyndicationManager();
            int index = mgr.getArchiveNum(name);
            SyndieURI uri = mgr.getArchiveURI(index);
            String host = mgr.getCustomProxyHost(index);
            int port = mgr.getCustomProxyPort(index);
            long lastSync = mgr.getLastSyncDate(index);
            long nextSync = mgr.getNextSyncDate(index);

            _archiveName.setText(name);
            _url.setText(uri.getURL());
            String pass = uri.getArchivePassphrase();
            if (pass != null)
                _passphrase.setText(pass);
            else
                _passphrase.setText("");
            
            if (lastSync < 0)
                _lastSync.setText(_browser.getTranslationRegistry().getText(T_SYNC_NEVER, "Never"));
            else
                _lastSync.setText(Constants.getDate(lastSync));
            
            populateNextSyncCombo(_browser.getTranslationRegistry(), forceAsNow ? System.currentTimeMillis() : nextSync);

            _proxyCustom.setSelection(false);
            _proxyDefault.setSelection(false);
            _proxyNone.setSelection(false);
            if ( (host != null) && (port > 0) ) {
                _proxyCustom.setSelection(true);
                _proxyCustomHost.setText(host);
                _proxyCustomPort.setText(port+"");
            } else {
                _proxyDefault.setSelection(true);
                _proxyCustomHost.setText("");
                _proxyCustomPort.setText("");
            }
        } else if ( (name != null) && (name.length() == 0) ) {
            enable = true;
            _archiveName.setText("");
            _url.setText("");
            _passphrase.setText("");
            _lastSync.setText("");
            
            populateNextSyncCombo(_browser.getTranslationRegistry(), -1);

            _proxyCustom.setSelection(false);
            _proxyDefault.setSelection(true);
            _proxyNone.setSelection(false);
            _proxyCustomHost.setText("");
            _proxyCustomPort.setText("");
        } else {
            _archiveName.setText("");
            _url.setText("");
            _passphrase.setText("");
            _lastSync.setText("");
            
            populateNextSyncCombo(_browser.getTranslationRegistry(), -1);

            _proxyCustom.setSelection(false);
            _proxyDefault.setSelection(true);
            _proxyNone.setSelection(false);
            _proxyCustomHost.setText("");
            _proxyCustomPort.setText("");
        }
        
        showArchiveEnable(enable, name);
    }
    
    private void showNewArchive(String name, String url) {
        _archives.setSelection(new int[0]);
        boolean enable = true;
        if ( (name == null) || (name.trim().length() <= 0) )
            name = ""+System.currentTimeMillis();
        _archiveName.setText(name);
        _url.setText(url);
        _passphrase.setText("");
        _lastSync.setText("");
        populateNextSyncCombo(_browser.getTranslationRegistry(), -1);

        _proxyCustom.setSelection(false);
        _proxyDefault.setSelection(true);
        _proxyNone.setSelection(false);
        _proxyCustomHost.setText("");
        _proxyCustomPort.setText("");
        showArchiveEnable(enable, name);
        _saveArchive.setEnabled(true);
        _revertArchive.setEnabled(true);
    }
    
    private void showArchiveEnable(boolean enable, String name) {
        _archiveNameLabel.setEnabled(enable);
        _archiveName.setEnabled(enable);
        _urlLabel.setEnabled(enable);
        _url.setEnabled(enable);
        _passphraseLabel.setEnabled(enable);
        _passphrase.setEnabled(enable);
        _lastSyncLabel.setEnabled(enable);
        _lastSync.setEnabled(enable);
        _nextSyncLabel.setEnabled(enable);
        _nextSyncCombo.setEnabled(enable);
        _proxyLabel.setEnabled(enable);
        _proxyDefault.setEnabled(enable);
        _proxyNone.setEnabled(enable);
        _proxyCustom.setEnabled(enable);
        _proxyCustomHostLabel.setEnabled(enable);
        _proxyCustomHost.setEnabled(enable);
        _proxyCustomPortLabel.setEnabled(enable);
        _proxyCustomPort.setEnabled(enable);
        _saveArchive.setEnabled(false);
        _revertArchive.setEnabled(false);
        _deleteArchive.setEnabled(enable && (name.length() > 0));
        
        _lastSync.getParent().layout(true);
    }
    
    private void scheduleSync(long when) {
        String names[] = _archives.getSelection();
        if ( (names != null) && (names.length > 0) ) {
            SyndicationManager mgr = _browser.getSyndicationManager();
            mgr.setNextSync(names[0], when);
            refreshView();
        }
    }
    
    private void saveArchive() {
        String oldName = null;
        String names[] = _archives.getSelection();
        if ( (names != null) && (names.length > 0) )
            oldName = names[0];
        String name = _archiveName.getText();
        String url = _url.getText();
        String pass = _passphrase.getText();
        boolean proxyNone = _proxyNone.getSelection();
        String customHost = _proxyCustomHost.getText();
        String customPortStr = _proxyCustomPort.getText();
        int customPort = -1;
        if ( (customPortStr.length() > 0) && (_proxyCustom.getSelection()) ) {
            try { 
                customPort = Integer.parseInt(customPortStr);
            } catch (NumberFormatException nfe) {}
        }
        if ( !_proxyCustom.getSelection() || (customPort <= 0) || (customHost.trim().length() <= 0) ) {
            customHost = null;
            customPort = -1;
        }
        if (oldName == null)
            _browser.getSyndicationManager().add(name, SyndieURI.createArchive(url, pass), customHost, customPort, null, null);
        else
            _browser.getSyndicationManager().update(oldName, name, SyndieURI.createArchive(url, pass), customHost, customPort, null, null);

        boolean forceNow = false;
        switch (_nextSyncCombo.getSelectionIndex()) {
            case NEXT_SYNC_NEVER:
                _browser.getSyndicationManager().setNextSync(name, -1);
                break;
            case NEXT_SYNC_NOW:
                _browser.getSyndicationManager().setNextSync(name, System.currentTimeMillis());
                forceNow = true;
                break;
            case NEXT_SYNC_CUSTOM:
            default:
                //_browser.getSyndicationManager().setNextSync(name, -1);
                // noop.  the custom value came from the syndication manager's scheduler, not the
                // user
                break;
        }
        refreshView(name, forceNow);
    }
    
    private static final String T_DELETE_CONFIRM_MSG = "syndie.gui.syndicationscheduler.deleteconfirmmsg";
    private static final String T_DELETE_CONFIRM_TITLE = "syndie.gui.syndicationscheduler.deleteconfirmtitle";
    
    private void deleteArchive() {
        String names[] = _archives.getSelection();
        if ( (names != null) && (names.length > 0) ) {
            MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO);
            box.setMessage(_browser.getTranslationRegistry().getText(T_DELETE_CONFIRM_MSG, "Are you sure you want to delete the archive?"));
            box.setText(_browser.getTranslationRegistry().getText(T_DELETE_CONFIRM_TITLE, "Confirm"));
            int rc = box.open();
            if (rc == SWT.YES) {
                SyndicationManager mgr = _browser.getSyndicationManager();
                mgr.delete(names[0]);
                _archives.setSelection(new int[0]);
                refreshView();
            }
        }
    }
    
    private void loadPullPolicy() {
        SharedArchiveEngine.PullStrategy strategy = _browser.getSyndicationManager().getPullStrategy();
        _pullAllDelta.setSelection(false);
        _pullAllKnown.setSelection(false);
        _pullNothing.setSelection(false);
        _pullPBE.setSelection(false);
        _pullPIR.setSelection(false);
        _pullPrivate.setSelection(false);
        _pullRecentDelta.setSelection(false);
        _pullRecentKnown.setSelection(false);
        _pullSizePer1024.setSelection(false);
        _pullSizePer128.setSelection(false);
        _pullSizePer2048.setSelection(false);
        _pullSizePer256.setSelection(false);
        _pullSizePer32.setSelection(false);
        _pullSizePer4096.setSelection(false);
        _pullSizePer512.setSelection(false);
        _pullSizePer64.setSelection(false);
        
        _pullDiscover.setSelection(strategy.discoverArchives);
        
        if (strategy.includeDupForPIR) {
            _pullPIR.setSelection(true);
        } else {
            if (strategy.includePBEMessages)
                _pullPBE.setSelection(true);
            if (strategy.includePrivateMessages)
                _pullPrivate.setSelection(true);
            
            if (strategy.includeRecentMessagesOnly && strategy.knownChannelsOnly)
                _pullRecentKnown.setSelection(true);
            else if (strategy.includeRecentMessagesOnly)
                _pullRecentDelta.setSelection(true);
            else if (strategy.knownChannelsOnly)
                _pullAllKnown.setSelection(true);
            else if (strategy.pullNothing)
                _pullNothing.setSelection(true);
            else
                _pullAllDelta.setSelection(true);
            
            if (strategy.maxKBPerMessage <= 32)
                _pullSizePer32.setSelection(true);
            else if (strategy.maxKBPerMessage <= 64)
                _pullSizePer64.setSelection(true);
            else if (strategy.maxKBPerMessage <= 128)
                _pullSizePer128.setSelection(true);
            else if (strategy.maxKBPerMessage <= 256)
                _pullSizePer256.setSelection(true);
            else if (strategy.maxKBPerMessage <= 512)
                _pullSizePer512.setSelection(true);
            else if (strategy.maxKBPerMessage <= 1024)
                _pullSizePer1024.setSelection(true);
            else if (strategy.maxKBPerMessage <= 2048)
                _pullSizePer2048.setSelection(true);
            else
                _pullSizePer4096.setSelection(true);
        }
    }
    private void savePullPolicy() {
        SharedArchiveEngine.PullStrategy strategy = new SharedArchiveEngine.PullStrategy();
        
        strategy.discoverArchives = _pullDiscover.getSelection();
        
        if (_pullPIR.getSelection()) {
            strategy.includeDupForPIR = true;
        } else {
            strategy.includeDupForPIR = false;
            if (_pullAllDelta.getSelection()) {
                strategy.knownChannelsOnly = false;
                strategy.includeRecentMessagesOnly = false;
            } else if (_pullAllKnown.getSelection()) {
                strategy.knownChannelsOnly = true;
                strategy.includeRecentMessagesOnly = false;
            } else if (_pullRecentDelta.getSelection()) {
                strategy.knownChannelsOnly = false;
                strategy.includeRecentMessagesOnly = true;
            } else if (_pullRecentKnown.getSelection()) {
                strategy.knownChannelsOnly = true;
                strategy.includeRecentMessagesOnly = true;
            } else {
                strategy.pullNothing = true;
            }
            
            strategy.includePBEMessages = _pullPBE.getSelection();
            strategy.includePrivateMessages = _pullPrivate.getSelection();
            
            if (_pullSizePer32.getSelection()) strategy.maxKBPerMessage = 32;
            else if (_pullSizePer64.getSelection()) strategy.maxKBPerMessage = 64;
            else if (_pullSizePer128.getSelection()) strategy.maxKBPerMessage = 128;
            else if (_pullSizePer256.getSelection()) strategy.maxKBPerMessage = 256;
            else if (_pullSizePer512.getSelection()) strategy.maxKBPerMessage = 512;
            else if (_pullSizePer1024.getSelection()) strategy.maxKBPerMessage = 1024;
            else if (_pullSizePer2048.getSelection()) strategy.maxKBPerMessage = 2048;
            else if (_pullSizePer4096.getSelection()) strategy.maxKBPerMessage = 4096;
        }
        _browser.getSyndicationManager().setPullStrategy(strategy);
    }
    
    private void loadPushPolicy() {
        SharedArchiveEngine.PushStrategy strategy = _browser.getSyndicationManager().getPushStrategy();
        _pushAllDelta.setSelection(false);
        _pushLocalDelta.setSelection(false);
        _pushNothing.setSelection(false);
        if (strategy.sendNothing)
            _pushNothing.setSelection(true);
        else if (strategy.sendLocalNewOnly)
            _pushLocalDelta.setSelection(true);
        else
            _pushAllDelta.setSelection(true);
    }
    private void savePushPolicy() {
        SharedArchiveEngine.PushStrategy strategy = new SharedArchiveEngine.PushStrategy();
        strategy.sendHashcashForAll = false;
        strategy.sendHashcashForLocal = false;
        if (_pushNothing.getSelection())
            strategy.sendNothing = true;
        else if (_pushLocalDelta.getSelection())
            strategy.sendLocalNewOnly = true;
        _browser.getSyndicationManager().setPushStrategy(strategy);
    }
    
    /** this only gets terminal events, so just append new records, no need to track status */
    private void addEvent(SyndicationManager.StatusRecord record) {
        TableItem item = new TableItem(_eventTable, SWT.NONE);
        item.setText(0, Constants.getDateTime(record.getEventTime()));
        String src = record.getSource();
        if (src == null) src = "";
        item.setText(1, src);
        SyndieURI uri = record.getURI();
        if ( (uri == null) || (uri.getScope() == null) ) {
            item.setText(2, "");
        } else {
            if (uri.getMessageId() == null) {
                item.setImage(2, ImageUtil.ICON_MSG_TYPE_META);
            } else {
                item.setImage(2, ImageUtil.ICON_MSG_TYPE_NORMAL);
            }
            item.setData("uri", uri);
        }
        switch (record.getStatus()) {
            case SyndicationManager.PUSH_SENT: // push
                item.setImage(3, ImageUtil.ICON_SYNDICATE_PUSH);
                break;
            case SyndicationManager.FETCH_IMPORT_OK: // import msg/meta
            case SyndicationManager.FETCH_INDEX_DIFF_OK: // index fetch
                item.setImage(3, ImageUtil.ICON_SYNDICATE_STATUS_OK);
                break;
            case SyndicationManager.FETCH_IMPORT_PBE:
                item.setImage(3, ImageUtil.ICON_SYNDICATE_STATUS_PBE);
                break;
            case SyndicationManager.FETCH_IMPORT_NOKEY:
                item.setImage(3, ImageUtil.ICON_SYNDICATE_STATUS_NOKEY);
                break;
            default:
                _browser.getUI().debugMessage("record is another status: " + record.getStatus() + ": " + record);
                item.setImage(3, ImageUtil.ICON_SYNDICATE_STATUS_ERROR);
                break;
        }
        String detail = record.getDetail();
        if (detail == null) detail = "";
        item.setText(4, detail);
        
        _colWhen.pack();
        _colArchive.pack();
        _colScope.pack();
        _colEvent.pack();
        _colDetail.pack();
    }
    
    private static final int NEXT_SYNC_NEVER = 0;
    private static final int NEXT_SYNC_NOW = 1;
    private static final int NEXT_SYNC_CUSTOM = 2;
    
    private static final String T_NEXTSYNC_NEVER = "syndie.gui.syndicationscheduler.nextsync.never";
    private static final String T_NEXTSYNC_NOW = "syndie.gui.syndicationscheduler.nextsync.now";
    
    private void populateNextSyncCombo(TranslationRegistry registry, long when) {
        _nextSyncCombo.setRedraw(false);
        _nextSyncCombo.removeAll();
        _nextSyncCombo.add(registry.getText(T_NEXTSYNC_NEVER, "Never"));
        _nextSyncCombo.add(registry.getText(T_NEXTSYNC_NOW, "Now"));
        if (when > 0) {
            _nextSyncCombo.add(Constants.getDateTime(when));
            _nextSyncCombo.select(NEXT_SYNC_CUSTOM);
        } else {
            _nextSyncCombo.select(NEXT_SYNC_NEVER);
        }
        _nextSyncCombo.setRedraw(true);
    }
    
    private void addArchive() {
        _archives.setSelection(new int[0]);
        showArchive("");
        _archiveName.forceFocus(); 
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(3, false));
        
        _archiveGroup = new Group(_root, SWT.NONE);
        _archiveGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, true, 1, 6));
        _archiveGroup.setLayout(new GridLayout(1, true));
        
        _archives = new List(_archiveGroup, SWT.SINGLE | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, true, true);
        gd.widthHint = 150;
        _archives.setLayoutData(gd);
        _archives.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showArchive(); }
            public void widgetSelected(SelectionEvent selectionEvent) { showArchive(); }
        });
        
        _archiveAdd = new Button(_archiveGroup, SWT.PUSH);
        _archiveAdd.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _archiveAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addArchive(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addArchive(); }
        });
        
        _deleteArchive = new Button(_archiveGroup, SWT.PUSH);
        _deleteArchive.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _deleteArchive.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { deleteArchive(); }
            public void widgetSelected(SelectionEvent selectionEvent) { deleteArchive(); }
        });
        
        
        _archiveNameLabel = new Label(_root, SWT.NONE);
        _archiveNameLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        // we use a row for this 1-element text field so the margins/spacing/etc line up with the other 3 rows
        Composite row = new Composite(_root, SWT.NONE);
        row.setLayout(new GridLayout(1, true));
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _archiveName = new Text(row, SWT.SINGLE | SWT.BORDER);
        _archiveName.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        // url row
        _urlLabel = new Label(_root, SWT.NONE);
        _urlLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        row = new Composite(_root, SWT.NONE);
        row.setLayout(new GridLayout(3, false));
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _url = new Text(row, SWT.SINGLE | SWT.BORDER);
        _url.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _passphraseLabel = new Label(row, SWT.NONE);
        _passphraseLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _passphrase = new Text(row, SWT.SINGLE | SWT.BORDER);
        gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        gd.widthHint = 50;
        _passphrase.setLayoutData(gd);
        
        // date row
        _lastSyncLabel = new Label(_root, SWT.NONE);
        _lastSyncLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        row = new Composite(_root, SWT.NONE);
        row.setLayout(new GridLayout(3, false));
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _lastSync = new Label(row, SWT.NONE);
        gd = new GridData(GridData.BEGINNING, GridData.CENTER, false, false);
        _lastSync.setLayoutData(gd);
        
        _nextSyncLabel = new Label(row, SWT.NONE);
        _nextSyncLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _nextSyncCombo = new Combo(row, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.BORDER);
        _nextSyncCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        // proxy row
        _proxyLabel = new Label(_root, SWT.NONE);
        _proxyLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        row = new Composite(_root, SWT.NONE);
        row.setLayout(new GridLayout(7, false));
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _proxyDefault = new Button(row, SWT.RADIO);
        _proxyDefault.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _proxyDefault.setSelection(true);
        
        _proxyNone = new Button(row, SWT.RADIO);
        _proxyNone.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _proxyCustom = new Button(row, SWT.RADIO);
        _proxyCustom.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _proxyCustomHostLabel = new Label(row, SWT.NONE);
        _proxyCustomHostLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _proxyCustomHost = new Text(row, SWT.SINGLE | SWT.BORDER);
        _proxyCustomHost.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _proxyCustomPortLabel = new Label(row, SWT.NONE);
        _proxyCustomPortLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _proxyCustomPort = new Text(row, SWT.SINGLE | SWT.BORDER);
        gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        gd.widthHint = 50;
        _proxyCustomPort.setLayoutData(gd);
        
        row = new Composite(_root, SWT.NONE);
        row.setLayout(new FillLayout(SWT.HORIZONTAL));
        row.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 2, 1));
        
        _saveArchive = new Button(row, SWT.PUSH);
        _saveArchive.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { saveArchive(); }
            public void widgetSelected(SelectionEvent selectionEvent) { saveArchive(); }
        });
        _revertArchive = new Button(row, SWT.PUSH);
        _revertArchive.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showArchive(); }
            public void widgetSelected(SelectionEvent selectionEvent) { showArchive(); }
        });
        
        ModListener lsnr = new ModListener();
        _archiveName.addModifyListener(lsnr);
        _url.addModifyListener(lsnr);
        _passphrase.addModifyListener(lsnr);
        _nextSyncCombo.addSelectionListener(lsnr);
        _proxyDefault.addSelectionListener(lsnr);
        _proxyNone.addSelectionListener(lsnr);
        _proxyCustom.addSelectionListener(lsnr);
        _proxyCustomHost.addModifyListener(lsnr);
        _proxyCustomPort.addModifyListener(lsnr);
        
        _eventGroup = new Group(_root, SWT.NONE);
        _eventGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 4));
        _eventGroup.setLayout(new GridLayout(1, false));
        
        _eventTable = new Table(_eventGroup, SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
        _eventTable.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _eventTable.setHeaderVisible(true);
        _eventTable.setLinesVisible(true);
        
        _colWhen = new TableColumn(_eventTable, SWT.LEFT);
        _colArchive = new TableColumn(_eventTable, SWT.LEFT);
        _colScope = new TableColumn(_eventTable, SWT.CENTER);
        _colEvent = new TableColumn(_eventTable, SWT.CENTER);
        _colDetail = new TableColumn(_eventTable, SWT.LEFT);
        
        _eventMenu = new Menu(_eventTable);
        _eventTable.setMenu(_eventMenu);
        _eventView = new MenuItem(_eventMenu, SWT.PUSH);
        _eventView.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) {
                TableItem items[] = _eventTable.getSelection();
                for (int i = 0; i < items.length; i++) {
                    SyndieURI uri = (SyndieURI)items[i].getData("uri");
                    if (uri != null)
                        _browser.view(uri);
                }
            }
            public void widgetSelected(SelectionEvent evt) {
                TableItem items[] = _eventTable.getSelection();
                for (int i = 0; i < items.length; i++) {
                    SyndieURI uri = (SyndieURI)items[i].getData("uri");
                    if (uri != null)
                        _browser.view(uri);
                }
            }
        });
        new MenuItem(_eventMenu, SWT.SEPARATOR);
        _eventClearSelected = new MenuItem(_eventMenu, SWT.PUSH);
        _eventClearSelected.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) {
                TableItem items[] = _eventTable.getSelection();
                for (int i = 0; i < items.length; i++)
                    items[i].dispose();
            }
            public void widgetSelected(SelectionEvent evt) {
                TableItem items[] = _eventTable.getSelection();
                for (int i = 0; i < items.length; i++)
                    items[i].dispose();
            }
        });
        _eventClearAll = new MenuItem(_eventMenu, SWT.PUSH);
        _eventClearAll.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _eventTable.removeAll(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _eventTable.removeAll(); }
        });
        
        _eventClear = new Button(_eventGroup, SWT.PUSH);
        _eventClear.setLayoutData(new GridData(GridData.BEGINNING, GridData.END, false, false));
        _eventClear.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _eventTable.removeAll(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _eventTable.removeAll(); }
        });
        
        _pullPolicy = new Button(_root, SWT.PUSH);
        _pullPolicy.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _pullPolicy.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _pullMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _pullMenu.setVisible(true); }
        });
        
        _pullMenu = new Menu(_pullPolicy);
        _pullPolicy.setMenu(_pullMenu);
        
        _pullRecentDelta = new MenuItem(_pullMenu, SWT.RADIO);
        _pullAllDelta = new MenuItem(_pullMenu, SWT.RADIO);
        _pullRecentKnown = new MenuItem(_pullMenu, SWT.RADIO);
        _pullAllKnown = new MenuItem(_pullMenu, SWT.RADIO);
        _pullPIR = new MenuItem(_pullMenu, SWT.RADIO);
        _pullNothing = new MenuItem(_pullMenu, SWT.RADIO);
        _pullRecentDelta.setSelection(true);
        new MenuItem(_pullMenu, SWT.SEPARATOR);
        _pullPrivate = new MenuItem(_pullMenu, SWT.CHECK);
        _pullPBE = new MenuItem(_pullMenu, SWT.CHECK);
        _pullPrivate.setSelection(true);
        _pullPBE.setSelection(true);
        _pullSizePer = new MenuItem(_pullMenu, SWT.CASCADE);
        _pullSizePerMenu = new Menu(_pullSizePer);
        _pullSizePer.setMenu(_pullSizePerMenu);
        _pullSizePer32 = new MenuItem(_pullSizePerMenu, SWT.RADIO);
        _pullSizePer64 = new MenuItem(_pullSizePerMenu, SWT.RADIO);
        _pullSizePer128 = new MenuItem(_pullSizePerMenu, SWT.RADIO);
        _pullSizePer256 = new MenuItem(_pullSizePerMenu, SWT.RADIO);
        _pullSizePer512 = new MenuItem(_pullSizePerMenu, SWT.RADIO);
        _pullSizePer1024 = new MenuItem(_pullSizePerMenu, SWT.RADIO);
        _pullSizePer2048 = new MenuItem(_pullSizePerMenu, SWT.RADIO);
        _pullSizePer4096 = new MenuItem(_pullSizePerMenu, SWT.RADIO);
        _pullSizePer512.setSelection(true);
        new MenuItem(_pullMenu, SWT.SEPARATOR);
        _pullDiscover = new MenuItem(_pullMenu, SWT.CHECK);
        
        _pullMenu.addMenuListener(new MenuListener() {
            public void menuHidden(MenuEvent menuEvent) { }
            public void menuShown(MenuEvent menuEvent) { loadPullPolicy(); }
        });
        
        SelectionListener savePull = new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { savePullPolicy(); }
            public void widgetSelected(SelectionEvent selectionEvent) { savePullPolicy(); }
        };
        
        _pullRecentDelta.addSelectionListener(savePull);
        _pullAllDelta.addSelectionListener(savePull);
        _pullRecentKnown.addSelectionListener(savePull);
        _pullAllKnown.addSelectionListener(savePull);
        _pullPIR.addSelectionListener(savePull);
        _pullNothing.addSelectionListener(savePull);
        _pullPrivate.addSelectionListener(savePull);
        _pullPBE.addSelectionListener(savePull);
        _pullPrivate.addSelectionListener(savePull);
        _pullPBE.addSelectionListener(savePull);
        _pullSizePer32.addSelectionListener(savePull);
        _pullSizePer64.addSelectionListener(savePull);
        _pullSizePer128.addSelectionListener(savePull);
        _pullSizePer256.addSelectionListener(savePull);
        _pullSizePer512.addSelectionListener(savePull);
        _pullSizePer1024.addSelectionListener(savePull);
        _pullSizePer2048.addSelectionListener(savePull);
        _pullSizePer4096.addSelectionListener(savePull);
        _pullDiscover.addSelectionListener(savePull);
        
        _pushPolicy = new Button(_root, SWT.PUSH);
        _pushPolicy.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        _pushPolicy.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _pushMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _pushMenu.setVisible(true); }
        });
        
        _pushMenu = new Menu(_pushPolicy);
        _pushPolicy.setMenu(_pushMenu);

        _pushAllDelta = new MenuItem(_pushMenu, SWT.RADIO);
        _pushLocalDelta = new MenuItem(_pushMenu, SWT.RADIO);
        _pushNothing = new MenuItem(_pushMenu, SWT.RADIO);
        
        _pushMenu.addMenuListener(new MenuListener() {
            public void menuHidden(MenuEvent menuEvent) {}
            public void menuShown(MenuEvent menuEvent) { loadPushPolicy(); }
        });
        SelectionListener savePush = new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { savePushPolicy(); }
            public void widgetSelected(SelectionEvent selectionEvent) { savePushPolicy(); }
        };
        _pushAllDelta.addSelectionListener(savePush);
        _pushLocalDelta.addSelectionListener(savePush);
        _pushNothing.addSelectionListener(savePush);
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
        
        _browser.getSyndicationManager().loadArchives();
        _browser.getSyndicationManager().addListener(this);
        refreshView();
    }
    
    private class ModListener implements SelectionListener, ModifyListener {
        private void modified() {
            _saveArchive.setEnabled(true);
            _revertArchive.setEnabled(true);
        }
        public void widgetSelected(SelectionEvent selectionEvent) { modified(); }
        public void widgetDefaultSelected(SelectionEvent selectionEvent) { modified(); }
        public void modifyText(ModifyEvent modifyEvent) { modified(); }
    }
    
    public void applyTheme(Theme theme) {
        _archiveGroup.setFont(theme.DEFAULT_FONT);
        _archives.setFont(theme.DEFAULT_FONT);
        _archiveAdd.setFont(theme.BUTTON_FONT);
        _archiveNameLabel.setFont(theme.DEFAULT_FONT);
        _archiveName.setFont(theme.DEFAULT_FONT);
        _urlLabel.setFont(theme.DEFAULT_FONT);
        _url.setFont(theme.DEFAULT_FONT);
        _passphraseLabel.setFont(theme.DEFAULT_FONT);
        _passphrase.setFont(theme.DEFAULT_FONT);
        _lastSyncLabel.setFont(theme.DEFAULT_FONT);
        _lastSync.setFont(theme.DEFAULT_FONT);
        _nextSyncLabel.setFont(theme.DEFAULT_FONT);
        _nextSyncCombo.setFont(theme.DEFAULT_FONT);
        _proxyLabel.setFont(theme.DEFAULT_FONT);
        _proxyDefault.setFont(theme.DEFAULT_FONT);
        _proxyNone.setFont(theme.DEFAULT_FONT);
        _proxyCustom.setFont(theme.DEFAULT_FONT);
        _proxyCustomHostLabel.setFont(theme.DEFAULT_FONT);
        _proxyCustomHost.setFont(theme.DEFAULT_FONT);
        _proxyCustomPortLabel.setFont(theme.DEFAULT_FONT);
        _proxyCustomPort.setFont(theme.DEFAULT_FONT);
        _saveArchive.setFont(theme.BUTTON_FONT);
        _revertArchive.setFont(theme.BUTTON_FONT);
        _deleteArchive.setFont(theme.BUTTON_FONT);
        _pullPolicy.setFont(theme.BUTTON_FONT);
        _pushPolicy.setFont(theme.BUTTON_FONT);
        _eventGroup.setFont(theme.DEFAULT_FONT);
        _eventTable.setFont(theme.TABLE_FONT);
        _eventClear.setFont(theme.BUTTON_FONT);
        
        _colWhen.pack();
        _colArchive.pack();
        _colScope.pack();
        _colEvent.pack();
        _colDetail.pack();
    }

    private static final String T_PULLRECENTDELTA = "syndie.gui.syndicationscheduler.pullrecentdelta";
    private static final String T_PULLALLDELTA = "syndie.gui.syndicationscheduler.pullalldelta";
    private static final String T_PULLRECENTKNOWN = "syndie.gui.syndicationscheduler.pullrecentknown";
    private static final String T_PULLALLKNOWN = "syndie.gui.syndicationscheduler.pullallknown";
    private static final String T_PULLPIR = "syndie.gui.syndicationscheduler.pullpir";
    private static final String T_PULLNOTHING = "syndie.gui.syndicationscheduler.pullnothing";
    private static final String T_PULLPRIVATE = "syndie.gui.syndicationscheduler.pullprivate";
    private static final String T_PULLPBE = "syndie.gui.syndicationscheduler.pullpbe";
    private static final String T_PULLSIZEPER = "syndie.gui.syndicationscheduler.pullsizeper";
    private static final String T_PULLSIZEPER32 = "syndie.gui.syndicationscheduler.pullsizeper32";
    private static final String T_PULLSIZEPER64 = "syndie.gui.syndicationscheduler.pullsizeper64";
    private static final String T_PULLSIZEPER128 = "syndie.gui.syndicationscheduler.pullsizeper128";
    private static final String T_PULLSIZEPER256 = "syndie.gui.syndicationscheduler.pullsizeper256";
    private static final String T_PULLSIZEPER512 = "syndie.gui.syndicationscheduler.pullsizeper512";
    private static final String T_PULLSIZEPER1024 = "syndie.gui.syndicationscheduler.pullsizeper1024";
    private static final String T_PULLSIZEPER2048 = "syndie.gui.syndicationscheduler.pullsizeper2048";
    private static final String T_PULLSIZEPER4096 = "syndie.gui.syndicationscheduler.pullsizeper4096";
    private static final String T_PULLDISCOVER = "syndie.gui.syndicationscheduler.pulldiscover";
    
    private static final String T_COLWHEN = "syndie.gui.syndicationscheduler.colwhen";
    private static final String T_COLARCHIVE = "syndie.gui.syndicationscheduler.colarchive";
    private static final String T_COLSCOPE = "syndie.gui.syndicationscheduler.colscope";
    private static final String T_COLEVENT = "syndie.gui.syndicationscheduler.colevent";
    private static final String T_COLDETAIL = "syndie.gui.syndicationscheduler.coldetail";
    
    private static final String T_ARCHIVES = "syndie.gui.syndicationscheduler.archives";
    private static final String T_ARCHIVEADD = "syndie.gui.syndicationscheduler.archiveadd";
    private static final String T_NAME = "syndie.gui.syndicationscheduler.name";
    private static final String T_URL = "syndie.gui.syndicationscheduler.url";
    private static final String T_PASS = "syndie.gui.syndicationscheduler.pass";
    private static final String T_LASTSYNC = "syndie.gui.syndicationscheduler.lastsync";
    private static final String T_NEXTSYNC = "syndie.gui.syndicationscheduler.nextsync";
    private static final String T_PROXY = "syndie.gui.syndicationscheduler.proxy";
    private static final String T_PROXYDEFAULT = "syndie.gui.syndicationscheduler.proxydefault";
    private static final String T_PROXYNONE = "syndie.gui.syndicationscheduler.proxynone";
    private static final String T_PROXYCUSTOM = "syndie.gui.syndicationscheduler.proxycustom";
    private static final String T_PROXYCUSTOMHOST = "syndie.gui.syndicationscheduler.proxycustomhost";
    private static final String T_PROXYCUSTOMPORT = "syndie.gui.syndicationscheduler.proxycustomport";
    private static final String T_SAVE = "syndie.gui.syndicationscheduler.save";
    private static final String T_REVERT = "syndie.gui.syndicationscheduler.revert";
    private static final String T_DELETE = "syndie.gui.syndicationscheduler.delete";
    private static final String T_PULL = "syndie.gui.syndicationscheduler.pull";
    private static final String T_CLEAREVENTS = "syndie.gui.syndicationscheduler.clearevents";
    private static final String T_PUSH = "syndie.gui.syndicationscheduler.push";
    private static final String T_EVENTS = "syndie.gui.syndicationscheduler.events";

    private static final String T_PUSHALLDELTA = "syndie.gui.syndicationscheduler.pushalldelta";
    private static final String T_PUSHLOCALDELTA = "syndie.gui.syndicationscheduler.pushlocaldelta";
    private static final String T_PUSHNOTHING = "syndie.gui.syndicationscheduler.pushnothing";

    private static final String T_SYNC_NEVER = "syndie.gui.syndicationscheduler.sync.never";
    
    private static final String T_EVENT_CLEARALL = "syndie.gui.syndicationscheduler.event.clearall";
    private static final String T_EVENT_CLEARSELECTED = "syndie.gui.syndicationscheduler.event.clearselected";
    private static final String T_EVENT_VIEW = "syndie.gui.syndicationscheduler.event.view";
    
    public void translate(TranslationRegistry registry) {
        _archiveGroup.setText(registry.getText(T_ARCHIVES, "Archives"));
        _archiveAdd.setText(registry.getText(T_ARCHIVEADD, "Add"));
        _archiveNameLabel.setText(registry.getText(T_NAME, "Name:"));
        _urlLabel.setText(registry.getText(T_URL, "URL:"));
        _passphraseLabel.setText(registry.getText(T_PASS, "Posting key:"));
        _lastSyncLabel.setText(registry.getText(T_LASTSYNC, "Last sync:"));
        _nextSyncLabel.setText(registry.getText(T_NEXTSYNC, "Next sync:"));
        _proxyLabel.setText(registry.getText(T_PROXY, "Proxy:"));
        _proxyDefault.setText(registry.getText(T_PROXYDEFAULT, "Default"));
        _proxyNone.setText(registry.getText(T_PROXYNONE, "None"));
        _proxyCustom.setText(registry.getText(T_PROXYCUSTOM, "Custom:"));
        _proxyCustomHostLabel.setText(registry.getText(T_PROXYCUSTOMHOST, "Host:"));
        _proxyCustomPortLabel.setText(registry.getText(T_PROXYCUSTOMPORT, "Port:"));
        _saveArchive.setText(registry.getText(T_SAVE, "Save"));
        _revertArchive.setText(registry.getText(T_REVERT, "Cancel"));
        _deleteArchive.setText(registry.getText(T_DELETE, "Delete"));
        _pullPolicy.setText(registry.getText(T_PULL, "Pull policy..."));
        _eventClear.setText(registry.getText(T_CLEAREVENTS, "Clear event log"));
        _pushPolicy.setText(registry.getText(T_PUSH, "Push policy..."));
        _eventGroup.setText(registry.getText(T_EVENTS, "Event log:"));
        
        _eventClearAll.setText(registry.getText(T_EVENT_CLEARALL, "Clear all records"));
        _eventClearSelected.setText(registry.getText(T_EVENT_CLEARSELECTED, "Clear selected record"));
        _eventView.setText(registry.getText(T_EVENT_VIEW, "View selected scope"));
        
        _colWhen.setText(registry.getText(T_COLWHEN, "When"));
        _colArchive.setText(registry.getText(T_COLARCHIVE, "Archive"));
        _colScope.setText(registry.getText(T_COLSCOPE, "Scope"));
        _colEvent.setText(registry.getText(T_COLEVENT, "Event"));
        _colDetail.setText(registry.getText(T_COLDETAIL, "Detail"));
                
        _pullRecentDelta.setText(registry.getText(T_PULLRECENTDELTA, "Recent messages we don't have"));
        _pullAllDelta.setText(registry.getText(T_PULLALLDELTA, "All messages we don't have"));
        _pullRecentKnown.setText(registry.getText(T_PULLRECENTKNOWN, "Recent messages in forums we know"));
        _pullAllKnown.setText(registry.getText(T_PULLALLKNOWN, "All messages in forums we know"));
        _pullPIR.setText(registry.getText(T_PULLPIR, "Everything the archive considers 'new' (PIR)"));
        _pullNothing.setText(registry.getText(T_PULLNOTHING, "Nothing"));
        _pullPrivate.setText(registry.getText(T_PULLPRIVATE, "Include private messages"));
        _pullPBE.setText(registry.getText(T_PULLPBE, "Include passphrase protected messages"));
        _pullSizePer.setText(registry.getText(T_PULLSIZEPER, "Maximum message size:"));
        _pullSizePer32.setText(registry.getText(T_PULLSIZEPER32, "32KB"));
        _pullSizePer64.setText(registry.getText(T_PULLSIZEPER64, "64KB"));
        _pullSizePer128.setText(registry.getText(T_PULLSIZEPER128, "128KB"));
        _pullSizePer256.setText(registry.getText(T_PULLSIZEPER256, "256KB"));
        _pullSizePer512.setText(registry.getText(T_PULLSIZEPER512, "512KB"));
        _pullSizePer1024.setText(registry.getText(T_PULLSIZEPER1024, "1024KB"));
        _pullSizePer2048.setText(registry.getText(T_PULLSIZEPER2048, "2048KB"));
        _pullSizePer4096.setText(registry.getText(T_PULLSIZEPER4096, "4096KB"));
        _pullDiscover.setText(registry.getText(T_PULLDISCOVER, "Discover new archives"));
        
        _pushAllDelta.setText(registry.getText(T_PUSHALLDELTA, "Send all differences"));
        _pushLocalDelta.setText(registry.getText(T_PUSHLOCALDELTA, "Send locally generated differences only"));
        _pushNothing.setText(registry.getText(T_PUSHNOTHING, "Send nothing"));
        
        populateNextSyncCombo(registry, -1);
    }

    public void archiveAdded(SyndicationManager mgr, String name) {
        Display.getDefault().asyncExec(new Runnable() { public void run() { refreshView(); } });
    }
    public void archiveRemoved(SyndicationManager mgr, String name) {   
        Display.getDefault().asyncExec(new Runnable() { public void run() { refreshView(); } });
    }
    public void archiveUpdated(SyndicationManager mgr, String oldName, String newName) {
        Display.getDefault().asyncExec(new Runnable() { public void run() { refreshView(); } });
    }
    public void archivesLoaded(SyndicationManager mgr) {
        Display.getDefault().asyncExec(new Runnable() { public void run() { refreshView(); } });
    }
    public void archiveIndexStatus(SyndicationManager mgr, final SyndicationManager.StatusRecord record) {
        if (record.isTerminal())
            Display.getDefault().asyncExec(new Runnable() { public void run() { addEvent(record); } });
    }
    public void fetchStatusUpdated(SyndicationManager mgr, final SyndicationManager.StatusRecord record) {
        if (record.isTerminal()) {
            Display.getDefault().asyncExec(new Runnable() { public void run() { addEvent(record); } });
        }
    }
    public void syndicationComplete(SyndicationManager mgr) {}
}
