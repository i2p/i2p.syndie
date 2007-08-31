package syndie.gui;

import java.util.ArrayList;
import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.SharedArchive;
import syndie.db.LocalArchiveManager;
import syndie.db.SyncArchive;
import syndie.db.SyncManager;
import syndie.db.UI;

/**
 *
 */
public class ArchiveManager extends BaseComponent implements Translatable, Themeable {
    private Composite _parent;
    private Composite _root;

    private Button _expiration;
    
    private Label _advertisedLabel;
    private Label _advertised;
    private Button _advertisedManage;
    
    private Label _deniableLabel;
    private Label _deniable;
    private Button _deniableManage;
    
    private Label _bannedLabel;
    private Label _banned;
    private Button _bannedManage;
    
    private Button _save;
    private Button _cancel;
    
    private List _uris;
    private List _bannedScopes;
    
    private NavigationControl _navControl;
    
    public ArchiveManager(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite parent, NavigationControl navControl) {
        super(client, ui, themes, trans);
        _parent = parent;
        _uris = new ArrayList();
        _bannedScopes = new ArrayList();
        _navControl = navControl;
        initComponents();
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(4, false));
    
        _expiration = new Button(_root, SWT.PUSH);
        _expiration.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        _expiration.addSelectionListener(new FireSelectionListener() { public void fire() { _navControl.view(URIHelper.instance().createExpirationURI(null)); } });
        
        Composite manageable = new Composite(_root, SWT.NONE);
        manageable.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        manageable.setLayout(new GridLayout(3, false));
        
        _advertisedLabel = new Label(manageable, SWT.NONE);
        _advertisedLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _advertised = new Label(manageable, SWT.NONE);
        _advertised.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        _advertisedManage = new Button(manageable, SWT.PUSH);
        _advertisedManage.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _advertisedManage.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { manageAdvertised(); }
            public void widgetSelected(SelectionEvent selectionEvent) { manageAdvertised(); }
        });
        
        _deniableLabel = new Label(manageable, SWT.NONE);
        _deniableLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _deniable = new Label(manageable, SWT.NONE);
        _deniable.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        _deniableManage = new Button(manageable, SWT.PUSH);
        _deniableManage.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _bannedLabel = new Label(manageable, SWT.NONE);
        _bannedLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _banned = new Label(manageable, SWT.NONE);
        _banned.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        _bannedManage = new Button(manageable, SWT.PUSH);
        _bannedManage.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _bannedManage.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { manageBanned(); }
            public void widgetSelected(SelectionEvent selectionEvent) { manageBanned(); }
        });
        
        Composite actions = new Composite(_root, SWT.NONE);
        actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        actions.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 4, 1));
        
        _save = new Button(actions, SWT.PUSH);
        _cancel = new Button(actions, SWT.PUSH);
        
        _save.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { saveConfig(); }
            public void widgetSelected(SelectionEvent selectionEvent) { saveConfig(); }
        });
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { loadConfig(); }
            public void widgetSelected(SelectionEvent selectionEvent) { loadConfig(); }
        });
        
        _deniable.setEnabled(false);
        _deniableLabel.setEnabled(false);
        _deniableManage.setEnabled(false);
        
        populateCombos();
        
        loadConfig();
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    private void loadConfig() {
        SyncManager mgr = SyncManager.getInstance(_client, _ui);
        SharedArchive.About cfg = LocalArchiveManager.getLocalAbout(_client, mgr.getDefaultPullStrategy());
        SyndieURI uris[] = cfg.getAlternateArchives();
        if (uris != null)
            _advertised.setText(uris.length+"");
        else
            _advertised.setText(0+"");
        
        _uris.clear();
        if (uris != null)
            for (int i = 0; i < uris.length; i++)
                _uris.add(uris[i]);
        //int size = cfg.maxMessageSize();
        List chans = _client.getBannedChannels();
        _bannedScopes.clear();
        _bannedScopes.addAll(chans);
        _banned.setText(chans.size()+"");
        
        _deniable.setText(0+"");
        _root.layout(true, true);
    }
    
    public void saveConfig() {
        SyncManager mgr = SyncManager.getInstance(_client, _ui);
        SharedArchive.About cfg = LocalArchiveManager.getLocalAbout(_client, mgr.getDefaultPullStrategy());
        cfg.setAlternativeArchives((SyndieURI[])_uris.toArray(new SyndieURI[0]));
        LocalArchiveManager.setLocalAbout(_client, _ui, cfg);
        
        List chans = _client.getBannedChannels();
        List unbanned = new ArrayList();
        for (int i = 0; i < chans.size(); i++)
            if (!_bannedScopes.contains(chans.get(i)))
                unbanned.add(chans.get(i));
        for (int i = 0; i < unbanned.size(); i++)
            _client.unban((Hash)unbanned.get(i));
        
        loadConfig();
    }
    
    private void populateCombos() {}
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
    }

    private static final String T_ADVERTISED_POPUP = "syndie.gui.archivemanager.adpopup.title";
    private static final String T_ADVERTISED_POPUP_DESC = "syndie.gui.archivemanager.adpopup.desc";
    private static final String T_ADVERTISED_POPUP_OK = "syndie.gui.archivemanager.adpopup.ok";
    private void manageAdvertised() {
        final Shell s = new Shell(_root.getShell(), SWT.DIALOG_TRIM | SWT.RESIZE | SWT.PRIMARY_MODAL);
        s.setText(_translationRegistry.getText(T_ADVERTISED_POPUP, "Advertised archives"));
        s.setLayout(new GridLayout(1, true));
        Label desc = new Label(s, SWT.WRAP);
        desc.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        desc.setText(_translationRegistry.getText(T_ADVERTISED_POPUP_DESC, "If you allow people to sync off your archive, you can tell them about some alternate archives they can sync off as well"));
        final Table archives = new Table(s, SWT.SINGLE | SWT.CHECK | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        archives.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        final List uris = new ArrayList();
        SyncManager mgr = SyncManager.getInstance(_client, _ui);
        for (int i = 0; i < mgr.getArchiveCount(); i++) {
            SyncArchive archive = mgr.getArchive(i);
            String name = archive.getName();
            SyndieURI uri = archive.getArchiveURI();
            uris.add(uri);
            boolean checked = _uris.contains(uri);
            TableItem item = new TableItem(archives, SWT.NONE);
            item.setText(name + " - " + uri.getURL());
            item.setChecked(checked);
        }
        Button ok = new Button(s, SWT.PUSH);
        ok.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        ok.setText(_translationRegistry.getText(T_ADVERTISED_POPUP_OK, "OK"));
        ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { setArchives(archives, uris); s.dispose(); }
            public void widgetSelected(SelectionEvent selectionEvent) { setArchives(archives, uris); s.dispose(); }
        });

        Theme theme = _themeRegistry.getTheme();
        s.setFont(theme.SHELL_FONT);
        desc.setFont(theme.DEFAULT_FONT);
        archives.setFont(theme.TABLE_FONT);
        ok.setFont(theme.BUTTON_FONT);
        
        //s.pack();
        //s.setSize(s.computeSize(400, SWT.DEFAULT));
        s.layout(true, true);
        s.open();
    }
    
    private void setArchives(Table table, List uris) {
        _uris.clear();
        TableItem items[] = table.getItems();
        for (int i = 0; i < items.length; i++) {
            if (items[i].getChecked()) {
                SyndieURI uri = (SyndieURI)uris.get(i);
                if (!_uris.contains(uri))
                    _uris.add(uri);
            }
        }
        _advertised.setText(_uris.size() + "");
    }

    private static final String T_BANNED_POPUP = "syndie.gui.archivemanager.bannedpopup.title";
    private static final String T_BANNED_POPUP_DESC = "syndie.gui.archivemanager.bannedpopup.desc";
    private static final String T_BANNED_POPUP_OK = "syndie.gui.archivemanager.bannedpopup.ok";
    private void manageBanned() {
        final Shell s = new Shell(_root.getShell(), SWT.DIALOG_TRIM | SWT.RESIZE | SWT.PRIMARY_MODAL);
        s.setText(_translationRegistry.getText(T_BANNED_POPUP, "Banned"));
        s.setLayout(new GridLayout(1, true));
        Label desc = new Label(s, SWT.WRAP);
        GridData gd = new GridData(GridData.FILL, GridData.BEGINNING, false, false);
        gd.widthHint = 400;
        desc.setLayoutData(gd);
        desc.setText(_translationRegistry.getText(T_BANNED_POPUP_DESC, "These authors and forums are completely ignored, with all of the associated messages refused"));
        final Table banned = new Table(s, SWT.SINGLE | SWT.CHECK | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        gd = new GridData(GridData.FILL, GridData.FILL, true, true);
        gd.widthHint = 400;
        gd.heightHint = 200;
        banned.setLayoutData(gd);
        List scopes = _bannedScopes;
        for (int i = 0; i < scopes.size(); i++) {
            Hash scope = (Hash)scopes.get(i);
            String name = _client.getChannelName(scope);
            TableItem item = new TableItem(banned, SWT.NONE);
            if (name != null)
                item.setText(scope.toBase64().substring(0,6) + " - " + name);
            else
                item.setText(scope.toBase64().substring(0,6));
        }
        Button ok = new Button(s, SWT.PUSH);
        ok.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        ok.setText(_translationRegistry.getText(T_BANNED_POPUP_OK, "Unban checked"));
        ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { setBanned(banned); s.dispose(); }
            public void widgetSelected(SelectionEvent selectionEvent) { setBanned(banned); s.dispose(); }
        });

        Theme theme = _themeRegistry.getTheme();
        s.setFont(theme.SHELL_FONT);
        desc.setFont(theme.DEFAULT_FONT);
        banned.setFont(theme.TABLE_FONT);
        ok.setFont(theme.BUTTON_FONT);
        
        //s.setSize(s.computeSize(400, SWT.DEFAULT));
        //s.pack();
        s.layout(true, true);
        s.open();
    }
    
    private void setBanned(Table table) {
        TableItem items[] = table.getItems();
        int scope = 0;
        for (int i = 0; i < items.length; i++, scope++) {
            if (items[i].getChecked()) {
                _bannedScopes.remove(scope);
                scope--;
            }
        }
        _banned.setText(_bannedScopes.size() + "");
    }
    
    public void applyTheme(Theme theme) {
        _expiration.setFont(theme.BUTTON_FONT);
        
        _advertisedLabel.setFont(theme.DEFAULT_FONT);
        _advertised.setFont(theme.DEFAULT_FONT);
        _advertisedManage.setFont(theme.BUTTON_FONT);

        _deniableLabel.setFont(theme.DEFAULT_FONT);
        _deniable.setFont(theme.DEFAULT_FONT);
        _deniableManage.setFont(theme.BUTTON_FONT);

        _bannedLabel.setFont(theme.DEFAULT_FONT);
        _banned.setFont(theme.DEFAULT_FONT);
        _bannedManage.setFont(theme.BUTTON_FONT);

        _save.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
        _root.layout(true, true);
    }
    
    private static final String T_EXPIRATION = "syndie.gui.archivemanager.expiration";
    private static final String T_ADVERTISED = "syndie.gui.archivemanager.advertised";
    private static final String T_ADVERTISEDMANAGE = "syndie.gui.archivemanager.advertisedmanage";
    private static final String T_DENIABLE = "syndie.gui.archivemanager.deniable";
    private static final String T_DENIABLEMANAGE = "syndie.gui.archivemanager.deniablemanage";
    private static final String T_BANNED = "syndie.gui.archivemanager.banned";
    private static final String T_BANNEDMANAGE = "syndie.gui.archivemanager.bannedmanage";
    private static final String T_SAVE = "syndie.gui.archivemanager.save";
    private static final String T_CANCEL = "syndie.gui.archivemanager.cancel";
        
    public void translate(TranslationRegistry registry) {
        _expiration.setText(registry.getText(T_EXPIRATION, "Manage expiration policies"));
    
        _advertisedLabel.setText(registry.getText(T_ADVERTISED, "Advertised archives:"));
        _advertisedManage.setText(registry.getText(T_ADVERTISEDMANAGE, "Manage..."));
        
        _deniableLabel.setText(registry.getText(T_DENIABLE, "Deniable forums:"));
        _deniableManage.setText(registry.getText(T_DENIABLEMANAGE, "Manage..."));
        
        _bannedLabel.setText(registry.getText(T_BANNED, "Banned forums/authors:"));
        _bannedManage.setText(registry.getText(T_BANNEDMANAGE, "Manage..."));
        
        _save.setText(registry.getText(T_SAVE, "Save"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel"));
    }
}
