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
import syndie.db.SharedArchive;
import syndie.db.SyndicationManager;

/**
 *
 */
public class ArchiveManager implements Translatable, Themeable {
    private BrowserControl _browser;
    private Composite _parent;
    private Composite _root;
    
    private Label _maxMsgSizeLabel;
    private Combo _maxMsgSizeCombo;
    private Label _maxMsgAgeLabel;
    private Combo _maxMsgAgeCombo;
    private Label _maxForumSizeLabel;
    private Combo _maxForumSizeCombo;
    private Label _maxForumAgeLabel;
    private Combo _maxForumAgeCombo;
    
    private Label _advertizedLabel;
    private Label _advertized;
    private Button _advertizedManage;
    
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
    
    public ArchiveManager(BrowserControl browser, Composite parent) {
        _browser = browser;
        _parent = parent;
        _uris = new ArrayList();
        _bannedScopes = new ArrayList();
        initComponents();
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(4, false));
    
        _maxMsgSizeLabel = new Label(_root, SWT.NONE);
        _maxMsgSizeLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _maxMsgSizeCombo = new Combo(_root, SWT.DROP_DOWN | SWT.READ_ONLY);
        _maxMsgSizeCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _maxMsgAgeLabel = new Label(_root, SWT.NONE);
        _maxMsgAgeLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _maxMsgAgeCombo = new Combo(_root, SWT.DROP_DOWN | SWT.READ_ONLY);
        _maxMsgAgeCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
    
        _maxForumSizeLabel = new Label(_root, SWT.NONE);
        _maxForumSizeLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _maxForumSizeCombo = new Combo(_root, SWT.DROP_DOWN | SWT.READ_ONLY);
        _maxForumSizeCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _maxForumAgeLabel = new Label(_root, SWT.NONE);
        _maxForumAgeLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _maxForumAgeCombo = new Combo(_root, SWT.DROP_DOWN | SWT.READ_ONLY);
        _maxForumAgeCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
    
        Composite manageable = new Composite(_root, SWT.NONE);
        manageable.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        manageable.setLayout(new GridLayout(3, false));
        
        _advertizedLabel = new Label(manageable, SWT.NONE);
        _advertizedLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _advertized = new Label(manageable, SWT.NONE);
        _advertized.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        _advertizedManage = new Button(manageable, SWT.PUSH);
        _advertizedManage.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _advertizedManage.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { manageAdvertized(); }
            public void widgetSelected(SelectionEvent selectionEvent) { manageAdvertized(); }
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
        
        _maxMsgSizeLabel.setEnabled(false);
        _maxMsgSizeCombo.setEnabled(false);
        
        _maxMsgAgeLabel.setEnabled(false);
        _maxMsgAgeCombo.setEnabled(false);
        
        _maxForumSizeLabel.setEnabled(false);
        _maxForumSizeCombo.setEnabled(false);
        
        _maxForumAgeLabel.setEnabled(false);
        _maxForumAgeCombo.setEnabled(false);
        
        _deniable.setEnabled(false);
        _deniableLabel.setEnabled(false);
        _deniableManage.setEnabled(false);
        
        populateCombos();
        
        loadConfig();
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
    }
    
    private void loadConfig() {
        SharedArchive.About cfg = _browser.getSyndicationManager().getLocalAbout();
        SyndieURI uris[] = cfg.getAlternateArchives();
        if (uris != null)
            _advertized.setText(uris.length+"");
        else
            _advertized.setText(0+"");
        
        _uris.clear();
        if (uris != null)
            for (int i = 0; i < uris.length; i++)
                _uris.add(uris[i]);
        //int size = cfg.maxMessageSize();
        List chans = _browser.getClient().getBannedChannels();
        _bannedScopes.clear();
        _bannedScopes.addAll(chans);
        _banned.setText(chans.size()+"");
        
        _deniable.setText(0+"");
        _root.layout(true, true);
    }
    
    public void saveConfig() {
        SharedArchive.About cfg = _browser.getSyndicationManager().getLocalAbout();
        cfg.setAlternativeArchives((SyndieURI[])_uris.toArray(new SyndieURI[0]));
        _browser.getSyndicationManager().setLocalAbout(cfg);
        
        List chans = _browser.getClient().getBannedChannels();
        List unbanned = new ArrayList();
        for (int i = 0; i < chans.size(); i++)
            if (!_bannedScopes.contains(chans.get(i)))
                unbanned.add(chans.get(i));
        for (int i = 0; i < unbanned.size(); i++)
            _browser.getClient().unban((Hash)unbanned.get(i));
        
        loadConfig();
    }
    
    private void populateCombos() {}
    
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
    }

    private static final String T_ADVERTIZED_POPUP = "syndie.gui.archivemanager.adpopup.title";
    private static final String T_ADVERTIZED_POPUP_DESC = "syndie.gui.archivemanager.adpopup.desc";
    private static final String T_ADVERTIZED_POPUP_OK = "syndie.gui.archivemanager.adpopup.ok";
    private void manageAdvertized() {
        final Shell s = new Shell(_root.getShell(), SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        s.setText(_browser.getTranslationRegistry().getText(T_ADVERTIZED_POPUP, "Advertized archives"));
        s.setLayout(new GridLayout(1, true));
        Label desc = new Label(s, SWT.WRAP);
        desc.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        desc.setText(_browser.getTranslationRegistry().getText(T_ADVERTIZED_POPUP_DESC, "If you allow people to sync off your archive, you can tell them about some alternate archives they can sync off as well"));
        final Table archives = new Table(s, SWT.SINGLE | SWT.CHECK | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        archives.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        final List uris = new ArrayList();
        SyndicationManager mgr = _browser.getSyndicationManager();
        for (int i = 0; i < mgr.getArchiveCount(); i++) {
            String name = mgr.getArchiveName(i);
            SyndieURI uri = mgr.getArchiveURI(i);
            uris.add(uri);
            boolean checked = _uris.contains(uri);
            TableItem item = new TableItem(archives, SWT.NONE);
            item.setText(name + " - " + uri.getURL());
            item.setChecked(checked);
        }
        Button ok = new Button(s, SWT.PUSH);
        ok.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        ok.setText(_browser.getTranslationRegistry().getText(T_ADVERTIZED_POPUP_OK, "OK"));
        ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { setArchives(archives, uris); s.dispose(); }
            public void widgetSelected(SelectionEvent selectionEvent) { setArchives(archives, uris); s.dispose(); }
        });

        Theme theme = _browser.getThemeRegistry().getTheme();
        s.setFont(theme.SHELL_FONT);
        desc.setFont(theme.DEFAULT_FONT);
        archives.setFont(theme.TABLE_FONT);
        ok.setFont(theme.BUTTON_FONT);
        
        s.setSize(s.computeSize(400, 200));
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
        _advertized.setText(_uris.size() + "");
    }

    private static final String T_BANNED_POPUP = "syndie.gui.archivemanager.bannedpopup.title";
    private static final String T_BANNED_POPUP_DESC = "syndie.gui.archivemanager.bannedpopup.desc";
    private static final String T_BANNED_POPUP_OK = "syndie.gui.archivemanager.bannedpopup.ok";
    private void manageBanned() {
        final Shell s = new Shell(_root.getShell(), SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        s.setText(_browser.getTranslationRegistry().getText(T_BANNED_POPUP, "Banned"));
        s.setLayout(new GridLayout(1, true));
        Label desc = new Label(s, SWT.WRAP);
        desc.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        desc.setText(_browser.getTranslationRegistry().getText(T_BANNED_POPUP_DESC, "These authors and forums are completely ignored, with all of the associated messages refused"));
        final Table banned = new Table(s, SWT.SINGLE | SWT.CHECK | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        banned.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        List scopes = _bannedScopes;
        for (int i = 0; i < scopes.size(); i++) {
            Hash scope = (Hash)scopes.get(i);
            String name = _browser.getClient().getChannelName(scope);
            TableItem item = new TableItem(banned, SWT.NONE);
            if (name != null)
                item.setText(scope.toBase64().substring(0,6) + " - " + name);
            else
                item.setText(scope.toBase64().substring(0,6));
        }
        Button ok = new Button(s, SWT.PUSH);
        ok.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        ok.setText(_browser.getTranslationRegistry().getText(T_BANNED_POPUP_OK, "Unban checked"));
        ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { setBanned(banned); s.dispose(); }
            public void widgetSelected(SelectionEvent selectionEvent) { setBanned(banned); s.dispose(); }
        });

        Theme theme = _browser.getThemeRegistry().getTheme();
        s.setFont(theme.SHELL_FONT);
        desc.setFont(theme.DEFAULT_FONT);
        banned.setFont(theme.TABLE_FONT);
        ok.setFont(theme.BUTTON_FONT);
        
        s.setSize(s.computeSize(400, 200));
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
        _maxMsgSizeLabel.setFont(theme.DEFAULT_FONT);
        _maxMsgSizeCombo.setFont(theme.DEFAULT_FONT);
        _maxMsgAgeLabel.setFont(theme.DEFAULT_FONT);
        _maxMsgAgeCombo.setFont(theme.DEFAULT_FONT);
        _maxForumSizeLabel.setFont(theme.DEFAULT_FONT);
        _maxForumSizeCombo.setFont(theme.DEFAULT_FONT);
        _maxForumAgeLabel.setFont(theme.DEFAULT_FONT);
        _maxForumAgeCombo.setFont(theme.DEFAULT_FONT);

        _advertizedLabel.setFont(theme.DEFAULT_FONT);
        _advertized.setFont(theme.DEFAULT_FONT);
        _advertizedManage.setFont(theme.BUTTON_FONT);

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
    
    private static final String T_MAXMSGSIZE = "syndie.gui.archivemanager.maxmsgsize";
    private static final String T_MAXMSGAGE = "syndie.gui.archivemanager.maxmsgage";
    private static final String T_MAXFORUMSIZE = "syndie.gui.archivemanager.maxforumsize";
    private static final String T_MAXFORUMAGE = "syndie.gui.archivemanager.maxforumage";
    private static final String T_ADVERTIZED = "syndie.gui.archivemanager.advertized";
    private static final String T_ADVERTIZEDMANAGE = "syndie.gui.archivemanager.advertizedmanage";
    private static final String T_DENIABLE = "syndie.gui.archivemanager.deniable";
    private static final String T_DENIABLEMANAGE = "syndie.gui.archivemanager.deniablemanage";
    private static final String T_BANNED = "syndie.gui.archivemanager.banned";
    private static final String T_BANNEDMANAGE = "syndie.gui.archivemanager.bannedmanage";
    private static final String T_SAVE = "syndie.gui.archivemanager.save";
    private static final String T_CANCEL = "syndie.gui.archivemanager.cancel";
        
    public void translate(TranslationRegistry registry) {
        _maxMsgSizeLabel.setText(registry.getText(T_MAXMSGSIZE, "Max message size:"));
        _maxMsgAgeLabel.setText(registry.getText(T_MAXMSGAGE, "Max message age:"));
        _maxForumSizeLabel.setText(registry.getText(T_MAXFORUMSIZE, "Max forum size:"));
        _maxForumAgeLabel.setText(registry.getText(T_MAXFORUMAGE, "Max forum age:"));
    
        _advertizedLabel.setText(registry.getText(T_ADVERTIZED, "Advertized archives:"));
        _advertizedManage.setText(registry.getText(T_ADVERTIZEDMANAGE, "Manage..."));
        
        _deniableLabel.setText(registry.getText(T_DENIABLE, "Deniable forums:"));
        _deniableManage.setText(registry.getText(T_DENIABLEMANAGE, "Manage..."));
        
        _bannedLabel.setText(registry.getText(T_BANNED, "Banned forums/authors:"));
        _bannedManage.setText(registry.getText(T_BANNEDMANAGE, "Manage..."));
        
        _save.setText(registry.getText(T_SAVE, "Save"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel"));
    }
}
