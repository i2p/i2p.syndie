package syndie.gui;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.SyncArchive;
import syndie.db.SyncManager;
import syndie.db.UI;

public class ManageForumArchives extends BaseComponent implements Themeable, Translatable {
    private final ManageForum _manage;
    private Shell _shell;
    private Table _table;
    private TableColumn _colName;
    private TableColumn _colType;
    private TableColumn _colLocation;
    private TableColumn _colPublic;
    private Label _addLabel;
    private Text _name;
    private Button _choiceHTTP;
    private Text _httpURL;
    private Button _choiceFreenet;
    private Text _freenetKey;
    private Button _choiceSyndie;
    private Text _syndieURI;
    private Button _isPublic;
    private Button _add;
    private Button _ok;
    private Button _cancel;
    
    private final Map<TableItem, SyndieURI> _itemToURI;
    
    private TableItem _curItem;
    
    public ManageForumArchives(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, ManageForum manage) {
        super(client, ui, themes, trans);
        _manage = manage;
        _itemToURI = new HashMap();
        _curItem = null;
        initComponents();
    }
    
    private void initComponents() {
        _shell = new Shell(_manage.getRoot().getShell(), SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        _shell.setLayout(new GridLayout(3, false));
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) {
                evt.doit = false;
                dispose();
            }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        _table = new Table(_shell, SWT.MULTI | SWT.CHECK | SWT.BORDER);
        _table.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 1));
        _table.setHeaderVisible(true);
        _table.setLinesVisible(true);
        
        _table.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) { toggleChecked(); }
            public void mouseDown(MouseEvent mouseEvent) {}
            public void mouseUp(MouseEvent mouseEvent) {}
        });
        _table.addKeyListener(new KeyListener() {
            public void keyPressed(KeyEvent keyEvent) {}
            public void keyReleased(KeyEvent evt) {
                switch (evt.character) {
                    case ' ':
                    case '\n':
                    case '\r':
                        toggleChecked();
                }
            }
        });
        _table.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
            public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
            private void fire() {
                if (_table.getSelectionCount() == 1)
                    showSelected();
            }
        });
        
        _colName = new TableColumn(_table, SWT.LEFT);
        _colType = new TableColumn(_table, SWT.CENTER);
        _colLocation = new TableColumn(_table, SWT.LEFT);
        _colPublic = new TableColumn(_table, SWT.CENTER);
        
        _addLabel = new Label(_shell, SWT.NONE);
        _addLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _name = new Text(_shell, SWT.SINGLE | SWT.BORDER);
        _name.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _add = new Button(_shell, SWT.PUSH);
        _add.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _add.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { clearForm(); }
            public void widgetSelected(SelectionEvent selectionEvent) { clearForm(); }
        });
        
        _choiceHTTP = new Button(_shell, SWT.RADIO);
        _choiceHTTP.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _httpURL = new Text(_shell, SWT.SINGLE | SWT.BORDER);
        _httpURL.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _choiceFreenet = new Button(_shell, SWT.RADIO);
        _choiceFreenet.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _freenetKey = new Text(_shell, SWT.SINGLE | SWT.BORDER);
        _freenetKey.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _choiceSyndie = new Button(_shell, SWT.RADIO);
        _choiceSyndie.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _syndieURI = new Text(_shell, SWT.SINGLE | SWT.BORDER);
        _syndieURI.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _isPublic = new Button(_shell, SWT.CHECK);
        _isPublic.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        
        Composite actions = new Composite(_shell, SWT.NONE);
        actions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        
        _ok = new Button(actions, SWT.PUSH);
        _ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { ok(); }
            public void widgetSelected(SelectionEvent selectionEvent) { ok(); }
        });
        
        _cancel = new Button(actions, SWT.PUSH);
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { dispose(); }
            public void widgetSelected(SelectionEvent selectionEvent) { dispose(); }
        });

        _choiceHTTP.setSelection(true);
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
        
        loadData();
        
        _name.setEnabled(false);
        _choiceHTTP.setEnabled(false);
        _choiceFreenet.setEnabled(false);
        _choiceSyndie.setEnabled(false);
        _httpURL.setEnabled(false);
        _freenetKey.setEnabled(false);
        _syndieURI.setEnabled(false);
        _isPublic.setEnabled(false);
        
        //_shell.pack();
        _shell.setSize(_shell.computeSize(SHELL_WIDTH, 400));
        _shell.open();
    }
    
    private static final int SHELL_WIDTH = 600;
    
    private void toggleChecked() {
        TableItem items[] = _table.getSelection();
        if ( (items != null) && (items.length > 0) ) {
            boolean firstWasChecked = items[0].getChecked();
            for (int i = 0; i < items.length; i++)
                items[i].setChecked(!firstWasChecked);
        }
    }
    
    private void showSelected() {
        if (_curItem != null)
            add(false); // save changes before going on to the next one
        
        _name.setEnabled(true);
        _choiceHTTP.setEnabled(true);
        _choiceFreenet.setEnabled(true);
        _choiceSyndie.setEnabled(true);
        _httpURL.setEnabled(true);
        _freenetKey.setEnabled(true);
        _syndieURI.setEnabled(true);
        _isPublic.setEnabled(true);
        
        TableItem item = _table.getSelection()[0]; // we are only called if selectionCount == 1
        SyndieURI uri = (SyndieURI)_itemToURI.get(item);
        String name = item.getText(0);
        String url = uri.getURL();
        boolean pub = "X".equals(item.getText(3));
        
        if (name != null)
            _name.setText(name);
        else
            _name.setText("");
        
        if (url.startsWith("http://")) {
            _choiceFreenet.setSelection(false);
            _choiceSyndie.setSelection(false);
            _choiceHTTP.setSelection(true);
            _httpURL.setText(url);
        } else if (url.startsWith("USK@") || url.startsWith("KSK@") || url.startsWith("CHK@") || url.startsWith("SSK@")) {
            _choiceSyndie.setSelection(false);
            _choiceHTTP.setSelection(false);
            _choiceFreenet.setSelection(true);
            _freenetKey.setText(url);
        } else {
            _choiceFreenet.setSelection(false);
            _choiceHTTP.setSelection(false);
            _choiceSyndie.setSelection(true);
            _syndieURI.setText(uri.toString());
        }
        
        _isPublic.setSelection(pub);
        _curItem = item;
    }
    
    private void loadData() {
        List pub = _manage.getPublicArchiveURIs();
        List priv = _manage.getPrivateArchiveURIs();
        
        Set pubURLs = new HashSet();
        for (int i = 0; i < pub.size(); i++)
            pubURLs.add(getURL((SyndieURI)pub.get(i)));
        Set privURLs = new HashSet();
        for (int i = 0; i < priv.size(); i++)
            privURLs.add(getURL((SyndieURI)priv.get(i)));
       
        _ui.debugMessage("loadData: pub=" + pubURLs + " priv=" + privURLs);
        
        SyncManager mgr = SyncManager.getInstance(_client, _ui);
        int cnt = mgr.getArchiveCount();
        for (int i = 0; i < cnt; i++) {
            SyncArchive archive = mgr.getArchive(i);
            String url = archive.getURL();
            if (url == null) url = "";
            boolean selected = false;
            boolean isPub = false;
            if (pubURLs.contains(url)) {
                isPub = true;
                selected = true;
                pubURLs.remove(url); // remaining are handled below
            } else if (privURLs.contains(url)) {
                isPub = false;
                selected = true;
                privURLs.remove(url); // remaining are handled below
            }
        
            _ui.debugMessage("loadData: sel?" + selected + " pub?" + isPub + " url: " + url);
            
            TableItem item = addArchive(archive.getName(), url, isPub, selected);
            _itemToURI.put(item, archive.getArchiveURI());
        }
        
        // ok, now deal with archive URIs that aren't in our current syndication set -
        for (int i = 0; i < pub.size(); i++) {
            SyndieURI uri = (SyndieURI)pub.get(i);
            String url = getURL(uri);
            if (pubURLs.contains(url)) { // not handled above
                String name = uri.getString("name");
                url = uri.getURL();
                if (url == null) url = "";
                boolean isPub = true;
                TableItem item = addArchive(name, url, isPub, true);
                _itemToURI.put(item, uri);
            }
        }
        for (int i = 0; i < priv.size(); i++) {
            SyndieURI uri = (SyndieURI)priv.get(i);
            String url = getURL(uri);
            if (privURLs.contains(url)) { // not handled above
                String name = uri.getString("name");
                url = uri.getURL();
                if (url == null) url = "";
                boolean isPub = false;
                TableItem item = addArchive(name, url, isPub, true);
                _itemToURI.put(item, uri);
            }
        }
    }
    
    private TableItem addArchive(String name, String url, boolean isPub, boolean selected) {
        TableItem item = _curItem;
        TableItem items[] = _table.getItems();
        if (item == null) {
            for (int i = 0; i < items.length; i++) {
                if (items[i].getText(0).equals(name)) {
                    item = items[i];
                    break;
                }
            }
        }
        if (item == null)
            item = new TableItem(_table, SWT.NONE);
        
        item.setText(0, name);
        
        if (url.startsWith("USK@") || url.startsWith("KSK@") || url.startsWith("SSK@") || url.startsWith("CHK@"))
            item.setImage(1, ImageUtil.ICON_ARCHIVE_TYPE_FREENET);
        else if (url.startsWith("http://"))
            item.setImage(1, ImageUtil.ICON_ARCHIVE_TYPE_URL);
        else if (url.startsWith("/") || url.startsWith("file://"))
            item.setImage(1, ImageUtil.ICON_ARCHIVE_TYPE_FILE);
        else
            item.setImage(1, ImageUtil.ICON_ARCHIVE_TYPE_SYNDIE);
        
        item.setText(2, url);
        
        if (isPub)
            item.setText(3, "X");
        else
            item.setText(3, "");
        
        if (selected)
            item.setChecked(true);

        _colName.pack();
        _colType.pack();
        _colLocation.pack();
        _colPublic.pack();
        
        int locWidth = SHELL_WIDTH - _colName.getWidth() - _colType.getWidth() - _colPublic.getWidth() - 5*_table.getGridLineWidth() - 32;
        _colLocation.setWidth(locWidth);
        
        return item;
    }
    
    private String getURL(SyndieURI uri) {
        String url = uri.getURL();
        if (url == null)
            return uri.toString();
        else
            return url;
    }
    
    private void clearForm() {
        if (_curItem != null)
            add(false);
        _curItem = null;
        
        _name.setText("");
        _choiceHTTP.setSelection(true);
        _choiceFreenet.setSelection(false);
        _choiceSyndie.setSelection(false);
        _httpURL.setText("");
        _freenetKey.setText("");
        _syndieURI.setText("");
        _isPublic.setSelection(false);
        
        _name.setEnabled(true);
        _choiceHTTP.setEnabled(true);
        _choiceFreenet.setEnabled(true);
        _choiceSyndie.setEnabled(true);
        _httpURL.setEnabled(true);
        _freenetKey.setEnabled(true);
        _syndieURI.setEnabled(true);
        _isPublic.setEnabled(true);
    }
    
    private void add(boolean asChecked) {
        String name = _name.getText().trim();
        if (name.length() <= 0) return;
        
        boolean isPub = _isPublic.getSelection();
        
        if (_choiceSyndie.getSelection()) {
            String str = _syndieURI.getText().trim();
            try {
                SyndieURI uri = new SyndieURI(str);
                if (uri.getURL() != null) {
                    TableItem item = addArchive(name, uri.getURL(), isPub, asChecked);
                    _itemToURI.put(item, uri);
                }
            } catch (URISyntaxException use) {
                _ui.errorMessage("Invalid URI: " + str, use);
            }
        } else if (_choiceFreenet.getSelection()) {
            String key = _freenetKey.getText().trim();
            if (key.length() > 3) {
                TableItem item = addArchive(name, key, isPub, asChecked);
                _itemToURI.put(item, SyndieURI.createArchive(key, name, ""));
            }
        } else if (_choiceHTTP.getSelection()) {
            String url = _httpURL.getText().trim();
            if (url.length() > 0) {
                if (url.indexOf("://") < 0)
                    url = "http://" + url;
                TableItem item = addArchive(name, url, isPub, asChecked);
                _itemToURI.put(item, SyndieURI.createArchive(url, name, ""));
            }
        }
        _curItem = null;
    }
    
    private void ok() {
        add(_curItem == null);
        
        List pubURIs = new ArrayList();
        List privURIs = new ArrayList();
        for (Iterator iter = _itemToURI.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry)iter.next();
            TableItem item = (TableItem)entry.getKey();
            SyndieURI uri = (SyndieURI)entry.getValue();
            
            if (item.getChecked()) {
                if (item.getText(3).equals("X"))
                    pubURIs.add(uri);
                else
                    privURIs.add(uri);
                _ui.debugMessage("selected archive: " + getURL(uri) + " [" + uri + "]");
            }
        }
        
        _manage.setArchives(pubURIs, privURIs);
        
        dispose();
    }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        
        if (!_shell.isDisposed()) _shell.dispose();
    }
    
    public void applyTheme(Theme theme) {
        _table.setFont(theme.TABLE_FONT);
        
        _name.setFont(theme.DEFAULT_FONT);
        _choiceHTTP.setFont(theme.DEFAULT_FONT);
        _httpURL.setFont(theme.DEFAULT_FONT);
        _choiceFreenet.setFont(theme.DEFAULT_FONT);
        _freenetKey.setFont(theme.DEFAULT_FONT);
        _choiceSyndie.setFont(theme.DEFAULT_FONT);
        _syndieURI.setFont(theme.DEFAULT_FONT);
        _addLabel.setFont(theme.DEFAULT_FONT);
        _isPublic.setFont(theme.DEFAULT_FONT);
        _add.setFont(theme.BUTTON_FONT);
        _ok.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);

        _colName.pack();
        _colType.pack();
        _colLocation.pack();
        _colPublic.pack();
    }
    

    

    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText("Archives advertised"));
        
        _colName.setText(registry.getText("Name"));
        _colType.setText(registry.getText("Type"));
        _colLocation.setText(registry.getText("Location"));
        _colPublic.setText(registry.getText("Public?"));

        _isPublic.setText(registry.getText("Anyone can see this, not just authorized readers"));

        _addLabel.setText(registry.getText("Archive name") + ':');
        _choiceHTTP.setText(registry.getText("HTTP URL") + ':');
        _choiceFreenet.setText(registry.getText("Freenet key") + ':');
        _choiceSyndie.setText(registry.getText("Syndie URI") + ':');
        _add.setText(registry.getText("Add"));
        _ok.setText(registry.getText("OK"));
        _cancel.setText(registry.getText("Cancel"));
    }
}
