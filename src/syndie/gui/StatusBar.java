package syndie.gui;

import net.i2p.data.DataHelper;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import syndie.db.SyndicationManager;

/**
 *
 */
public class StatusBar implements Translatable {
    private BrowserControl _browser;
    private Composite _parent;
    private Composite _root;
    private Label _onlineState;
    private Label _nextSyncLabel;
    private Label _nextSyncDate;
    
    public StatusBar(BrowserControl browser, Composite parent) {
        _browser = browser;
        _parent = parent;
        initComponents();
    }
    
    public Control getControl() { return _root; }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        _root.setLayout(gl);
        
        _onlineState = new Label(_root, SWT.SHADOW_OUT | SWT.BORDER);
        _onlineState.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        
        _nextSyncLabel = new Label(_root, SWT.NONE);
        _nextSyncLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _nextSyncDate = new Label(_root, SWT.NONE);
        _nextSyncDate.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));

        _onlineState.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) {}
            public void mouseDown(MouseEvent mouseEvent) { toggleOnline(); }
            public void mouseUp(MouseEvent mouseEvent) {}
        });
        
        _browser.getSyndicationManager().addListener(new SyndicationManager.SyndicationListener() {
            public void archiveAdded(SyndicationManager mgr, String name) {}
            public void archiveRemoved(SyndicationManager mgr, String name) {}
            public void archiveUpdated(SyndicationManager mgr, String oldName, String newName) {
                updateNextSync();
            }
            public void archivesLoaded(SyndicationManager mgr) {
                updateNextSync();
            }
            public void archiveIndexStatus(SyndicationManager mgr, SyndicationManager.StatusRecord record) {
                updateNextSync();
            }
            public void fetchStatusUpdated(SyndicationManager mgr, SyndicationManager.StatusRecord record) {}
            public void syndicationComplete(SyndicationManager mgr) {
                updateNextSync();
            }
            public void onlineStateAdjusted(final boolean nowOnline) {
                Display.getDefault().asyncExec(new Runnable() { 
                    public void run() { 
                        displayOnlineState(nowOnline);
                    }
                });
            }
        });
        
        Refresh r = new Refresh();
        Display.getDefault().timerExec(30*1000, r);

        displayOnlineState(_browser.getSyndicationManager().isOnline());
        _browser.getTranslationRegistry().register(this);
    }
    private class Refresh implements Runnable {
        public void run() {
            updateNextSync();
            Display.getDefault().timerExec(30*1000, Refresh.this);
        }
    }
    
    private void toggleOnline() {
        _browser.getSyndicationManager().setOnlineStatus(!_browser.getSyndicationManager().isOnline());
    }
    
    private static final String T_ONLINE = "syndie.gui.statusbar.online";
    private static final String T_OFFLINE = "syndie.gui.statusbar.offline";
    
    private void displayOnlineState(boolean online) {
        if (online) {
            _onlineState.setImage(ImageUtil.ICON_ONLINE);
            _onlineState.setToolTipText(_browser.getTranslationRegistry().getText(T_ONLINE, "Online: syndication is enabled"));
        } else {
            _onlineState.setImage(ImageUtil.ICON_OFFLINE);
            _onlineState.setToolTipText(_browser.getTranslationRegistry().getText(T_OFFLINE, "Offline: syndication is deferred"));
        }
        updateNextSync();
    }
    
    private void updateNextSync() {
        final long nextSync = _browser.getSyndicationManager().getNextSyncDate();
        Display.getDefault().asyncExec(new Runnable() {
            public void run() { setNextSync(nextSync, _browser.getSyndicationManager().isOnline()); }
        });
    }
    
    private static final String T_NEXT_SYNC_OFFLINE = "syndie.gui.statusbar.nextsync.offline";
    private static final String T_NEXT_SYNC_NOW = "syndie.gui.statusbar.nextsync.now";
    private static final String T_NEXT_SYNC_NONE = "syndie.gui.statusbar.nextsync.none";
    public void setNextSync(long when, boolean online) {
        if (!online)
            _nextSyncDate.setText(_browser.getTranslationRegistry().getText(T_NEXT_SYNC_OFFLINE, "Deferred..."));
        else if (when <= 0)
            _nextSyncDate.setText(_browser.getTranslationRegistry().getText(T_NEXT_SYNC_NONE, "None scheduled"));
        else if (when-System.currentTimeMillis() <= 0)
            _nextSyncDate.setText(_browser.getTranslationRegistry().getText(T_NEXT_SYNC_NOW, "Now"));
        else
            _nextSyncDate.setText(DataHelper.formatDuration(when-System.currentTimeMillis())); // 3h, 39m, 2d, etc
        _root.layout(true);
    }
    
    private static final String T_NEXT_SYNC = "syndie.gui.statusbar.nextsync";
    public void translate(TranslationRegistry registry) {
        _nextSyncLabel.setText(registry.getText(T_NEXT_SYNC, "Next sync:"));
        _root.layout(true);
    }
}
