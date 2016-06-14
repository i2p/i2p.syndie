package syndie.gui;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import net.i2p.data.DataHelper;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
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

import syndie.db.DBClient;
import syndie.db.PullStrategy;
import syndie.db.PushStrategy;
import syndie.db.SharedArchiveEngine;
import syndie.db.SyncArchive;
import syndie.db.SyncManager;
import syndie.db.UI;
import syndie.util.StringUtil;

/**
 *
 */
class ArchiveDefaults extends Composite implements Themeable, Translatable {
    private final DBClient _client;
    private final UI _ui;
    private final ThemeRegistry _themeRegistry;
    private final TranslationRegistry _translationRegistry;
    
    private Table _archives;
    private TableColumn _colImport;
    private TableColumn _colLocation;
    private TableColumn _colProxy;
    private TableColumn _colPullOnly;
    private TableColumn _colAutomatic;
    
    private static final Image readOnlyImage = ImageUtil.ICON_CANCEL;
    private static final Image autoImage = ImageUtil.ICON_SYNC;
    
    public ArchiveDefaults(Composite parent, DBClient client, UI ui, ThemeRegistry themeRegistry, TranslationRegistry translationRegistry) {
        super(parent, SWT.NONE);
        _client = client;
        _ui = ui;
        _themeRegistry = themeRegistry;
        _translationRegistry = translationRegistry;
        initComponents();
    }
    
    private static final int TABLE_WIDTH = 750;
    
    private void initComponents() {
        this.setLayout(new FillLayout());
        
        _archives = new Table(this, SWT.SINGLE | SWT.CHECK | SWT.BORDER | SWT.FULL_SELECTION);
        _archives.setHeaderVisible(true);
        _archives.setLinesVisible(true);
        
        _archives.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) {
                TableItem item = _archives.getSelection()[0];
                int col = getCol();
                edit(item, col);
            }
            public void widgetSelected(SelectionEvent selectionEvent) {}
            private int getCol() {
                Point loc = getShell().getDisplay().getCursorLocation();
                Point reloc = getShell().getDisplay().map(null, _archives, loc);
                int col = -1;
                int cols = _archives.getColumnCount();
                TableItem item = _archives.getSelection()[0];
                for (int i = 0; i < cols; i++) {
                    Rectangle rect = item.getBounds(i);
                    if (rect.x + rect.width > reloc.x) {
                        col = i;
                        break;
                    }
                }
                if (col < 0) col = cols-1;
                return col;
            }
        });
        
        _colImport = new TableColumn(_archives, SWT.LEFT);
        _colLocation = new TableColumn(_archives, SWT.LEFT);
        _colProxy = new TableColumn(_archives, SWT.LEFT);
        _colPullOnly = new TableColumn(_archives, SWT.CENTER);
        _colAutomatic = new TableColumn(_archives, SWT.CENTER);
        
        loadDefaults(); // packed on translate
        
        //_archives.setSize(TABLE_WIDTH, 700);
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }

    /** 
     * check both defaultarchives.txt in the instance root directory and inside the
     * syndie code itself (as the "/defaultarchives.txt" resource")
     */
    private void loadDefaults() {
        File externalDefaults = new File(_client.getRootDir(), "defaultarchives.txt");
        if (externalDefaults.exists()) {
            Properties props = new Properties();
            try {
                DataHelper.loadProps(props, externalDefaults);
                addToDefaults(props);
                return;
            } catch (IOException ioe) {
                _ui.debugMessage("Unable to find an instance specific defaultarchives.txt", ioe);
            }
        }
        InputStream packagedDefaults = getClass().getResourceAsStream("/defaultarchives.txt");
        if (packagedDefaults != null) {
            Properties props = new Properties();
            try {
                DataHelper.loadProps(props, packagedDefaults);
                addToDefaults(props);
                return;
            } catch (IOException ioe) {
                _ui.debugMessage("Unable to find a package specific /defaultarchives.txt", ioe);
            }
        }
    }

    /**
     * archive.1.name=Standard archive (direct)
     * archive.1.url=http://syndie.i2p2.de:8080/
     * [archive.1.proxy=localhost:8118]
     * [archive.1.pullpolicy=RecentMessagesOnly IncludePBE IncludePrivate AllChannels DiscoverArchives]
     * [archive.1.pushpolcy=SendNothing]
     * archive.1.syncbydefault=true
     */
    private void addToDefaults(Properties props) {
        int i = 1;
        while (true) {
            String name = props.getProperty("archive." + i + ".name");
            if (name == null)
                break;
            String url = props.getProperty("archive." + i + ".url");
            if (url == null)
                continue;
            String proxy = props.getProperty("archive." + i + ".proxy");
            if ( (proxy != null) && (proxy.trim().length() <= 0) )
                proxy = null;
                
            String proxyHost = null;
            int proxyPort = -1;
            if (proxy != null) {
                String str[] = StringUtil.split(':', proxy);
                if ( (str != null) && (str.length == 2) ) {
                    try {
                        int port = Integer.parseInt(str[1]);
                        if (port > 0) {
                            proxyPort = port;
                            proxyHost = str[0].trim();
                        }
                    } catch (NumberFormatException nfe) {}
                }
            }
            
            String pullPolicy = props.getProperty("archive." + i + ".pullpolicy");
            if ( (pullPolicy != null) && (pullPolicy.trim().length() <= 0) )
                pullPolicy = null;
            String pushPolicy = props.getProperty("archive." + i + ".pushpolicy");
            if ( (pushPolicy != null) && (pushPolicy.trim().length() <= 0) )
                pushPolicy = null;
            String syncByDefaultStr = props.getProperty("archive." + i + ".syncbydefault");
            boolean syncByDefault = (syncByDefaultStr != null) && ("true".equalsIgnoreCase(syncByDefaultStr));
            
            addToDefaults(name, url, proxyHost, proxyPort, pullPolicy, pushPolicy, syncByDefault);
            
            i++;
        }
    }
    
    
    private void addToDefaults(String name, String url, String proxyHost, int proxyPort, String pullPolicy, String pushPolicy, boolean syncByDefault) {
        TableItem item = new TableItem(_archives, SWT.LEFT);
        item.setChecked(true);
        
        item.setText(0, name);
        item.setText(1, url);
        if ( (proxyHost != null) && (proxyPort > 0) )
            item.setText(2, proxyHost + ":" + proxyPort);
        else
            item.setText(2, _translationRegistry.getText("no proxy"));
        if ((pushPolicy != null) && (pushPolicy.indexOf("SendNothing") != -1))
            item.setText(3, "X");
        else
            item.setText(3, "");
        if (syncByDefault)
            item.setText(4, "X");
        else
            item.setText(4, "");
        item.setData("pushpolicy", pushPolicy);
        item.setData("pullpolicy", pullPolicy);
    }
    
    private void edit(TableItem item, int col) {
        switch (col) {
            case 0: // edit name
                editName(item);
                break;
            case 1: // edit url
                editURL(item);
                break;
            case 2: // edit proxy
                editProxy(item);
                break;
            case 3: // toggle read only
                if (item.getText(3).equals("X"))
                    item.setText(3, "");
                else
                    item.setText(3, "X");
                break;
            case 4: // toggle auto
                if (item.getText(4).equals("X"))
                    item.setText(4, "");
                else
                    item.setText(4, "X");
                break;
        }
    }
    
    
    private void editName(final TableItem item) {
        final Shell shell = new Shell(getShell(), SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL | SWT.BORDER);
        GridLayout gl = new GridLayout(4, false);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        shell.setLayout(gl);
        shell.setText(_translationRegistry.getText("Edit name"));
        
        shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent shellEvent) { shell.dispose(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        Label l = new Label(shell, SWT.NONE);
        l.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
        l.setText(_translationRegistry.getText("Archive name") + ": ");
        l.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        
        final Text name = new Text(shell, SWT.SINGLE | SWT.BORDER);
        name.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        name.setText(item.getText(0));
        name.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        name.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) {
                    item.setText(0, name.getText());
                    shell.dispose();
                }
            }
        });
        
        Button b = new Button(shell, SWT.PUSH);
        b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        b.setText(_translationRegistry.getText("Cancel"));
        b.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        b.addSelectionListener(new FireSelectionListener() {
            public void fire() { shell.dispose(); }
        });
        
        b = new Button(shell, SWT.PUSH);
        b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        b.setText(_translationRegistry.getText("OK"));
        b.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        b.addSelectionListener(new FireSelectionListener() {
            public void fire() { 
                item.setText(0, name.getText()); 
                shell.dispose();
            }
        });
        
        shell.pack();
        shell.open();
    }
    
    
    private void editURL(final TableItem item) {
        final Shell shell = new Shell(getShell(), SWT.DIALOG_TRIM | SWT.BORDER | SWT.PRIMARY_MODAL);
        GridLayout gl = new GridLayout(4, false);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        shell.setLayout(gl);
        shell.setText(_translationRegistry.getText("Edit URL"));
        shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent shellEvent) { shell.dispose(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        Label l = new Label(shell, SWT.NONE);
        l.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
        l.setText(_translationRegistry.getText("Archive location") + ": ");
        l.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        
        final Text url = new Text(shell, SWT.SINGLE | SWT.BORDER | SWT.WRAP);
        url.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        url.setText(item.getText(1));
        url.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        url.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) {
                    item.setText(1, url.getText());
                    shell.dispose();
                }
            }
        });
        
        Button b = new Button(shell, SWT.PUSH);
        b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        b.setText(_translationRegistry.getText("Cancel"));
        b.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        b.addSelectionListener(new FireSelectionListener() {
            public void fire() { shell.dispose(); }
        });
        
        b = new Button(shell, SWT.PUSH);
        b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        b.setText(_translationRegistry.getText("OK"));
        b.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        b.addSelectionListener(new FireSelectionListener() {
            public void fire() { 
                item.setText(1, url.getText()); 
                shell.dispose();
            }
        });
        
        shell.pack();
        shell.open();
    }
    
    
    private void editProxy(final TableItem item) {
        String proxy = item.getText(2);
        String proxyHost = null;
        int proxyPort = -1;
        if ( (proxy != null) && (proxy.indexOf(':') > 0) ) {
            String str[] = StringUtil.split(':', proxy);
            if ( (str != null) && (str.length == 2) ) {
                try {
                    int port = Integer.parseInt(str[1]);
                    if (port > 0) {
                        proxyPort = port;
                        proxyHost = str[0].trim();
                    }
                } catch (NumberFormatException nfe) {}
            }
        }
        
        final Shell shell = new Shell(getShell(), SWT.DIALOG_TRIM | SWT.BORDER | SWT.PRIMARY_MODAL);
        GridLayout gl = new GridLayout(6, false);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        shell.setLayout(gl);
        shell.setText(_translationRegistry.getText("Edit proxy"));
        shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent shellEvent) { shell.dispose(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        Label l = new Label(shell, SWT.NONE);
        l.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
        l.setText(_translationRegistry.getText("Proxy host") + ": ");
        l.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        
        final Text host = new Text(shell, SWT.SINGLE | SWT.BORDER | SWT.WRAP);
        host.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        host.setText(proxyHost != null && proxyPort > 0  ? proxyHost : "");
        host.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        
        l = new Label(shell, SWT.NONE);
        l.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
        l.setText(_translationRegistry.getText("Port") + ": ");
        l.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        
        final Text port = new Text(shell, SWT.SINGLE | SWT.BORDER | SWT.WRAP);
        port.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        port.setTextLimit(6);
        port.setText(proxyHost != null && proxyPort > 0 ? proxyPort+"" : "");
        port.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        port.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) {
                    String h = host.getText().trim();
                    String p = port.getText().trim();
                    int portNum = -1;
                    try { portNum = Integer.parseInt(p); } catch (NumberFormatException nfe) {}
                    if ( (h.length() > 0) && (portNum > 0) ) {
                        item.setText(2, h + ":" + portNum);
                        shell.dispose();
                    } else {
                        item.setText(2, _translationRegistry.getText("no proxy"));
                        shell.dispose();
                    }
                }
            }
        });
        
        Button b = new Button(shell, SWT.PUSH);
        b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        b.setText(_translationRegistry.getText("Cancel"));
        b.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        b.addSelectionListener(new FireSelectionListener() {
            public void fire() { shell.dispose(); }
        });
        
        b = new Button(shell, SWT.PUSH);
        b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        b.setText(_translationRegistry.getText("OK"));
        b.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        b.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                String h = host.getText().trim();
                String p = port.getText().trim();
                int portNum = -1;
                try { portNum = Integer.parseInt(p); } catch (NumberFormatException nfe) {}
                if ( (h.length() > 0) && (portNum > 0) ) {
                    item.setText(2, h + ":" + portNum);
                    shell.dispose();
                } else {
                    item.setText(2, _translationRegistry.getText("no proxy"));
                    shell.dispose();
                }
            }
        });
        
        shell.pack();
        shell.open();
    }
    
    public void save() {
        SyncManager mgr = SyncManager.getInstance(_client, _ui);
        TableItem items[]= _archives.getItems();
        for (int i = 0; i < items.length; i++) {
            if (!items[i].getChecked())
                continue;
            String name = items[i].getText(0);
            String url = items[i].getText(1);
            String proxy = items[i].getText(2);
            String proxyHost = null;
            int proxyPort = -1;
            if (proxy != null) {
                String str[] = StringUtil.split(':', proxy);
                if ( (str != null) && (str.length == 2) ) {
                    try {
                        int port = Integer.parseInt(str[1]);
                        if (port > 0) {
                            proxyPort = port;
                            proxyHost = str[0].trim();
                        }
                    } catch (NumberFormatException nfe) {}
                }
            }
            
            String pushPolicy = (String)items[i].getData("pushpolicy");
            if (items[i].getText(3).equals("X")) {
                if (pushPolicy == null)
                    pushPolicy = "SendNothing";
                else if (pushPolicy.indexOf("SendNothing") == -1)
                    pushPolicy = pushPolicy + " SendNothing";
            } else {
                if (pushPolicy != null) {
                    StringBuilder buf = new StringBuilder(pushPolicy);
                    int idx = buf.indexOf("SendNothing");
                    if (idx >= 0) {
                        buf = buf.delete(idx, idx + "SendNothing".length());
                    }
                    pushPolicy = buf.toString();
                }
            }
            
            boolean syncByDefault = items[i].getText(4).equals("X");
            String pullPolicy = (String)items[i].getData("pullpolicy");
            
            SyncArchive archive = new SyncArchive(mgr, _client);
            archive.setName(name);
            archive.setURL(url);
            archive.setHTTPProxyHost(proxyHost);
            archive.setHTTPProxyPort(proxyPort);
            //_archives.add(archive);
            if (pullPolicy != null)
                archive.setPullStrategy(new PullStrategy(pullPolicy));
            if (pushPolicy != null)
                archive.setPushStrategy(new PushStrategy(pushPolicy));
            if (syncByDefault)
                archive.setNextSyncTime(System.currentTimeMillis());
            else
                archive.setNextSyncTime(-1);
            archive.store();
        }
        
        Properties prefs = _client.getNymPrefs();
        prefs.setProperty("browser.promptScheduleNow", Boolean.FALSE.toString());
        _client.setNymPrefs(prefs);
    }
    
    public void dispose() {
        super.dispose();
        _themeRegistry.unregister(this);
        _translationRegistry.unregister(this);
    }
    
    
    public void translate(TranslationRegistry registry) {
        _colImport.setText(registry.getText("Archive"));
        _colLocation.setText(registry.getText("URL"));
        _colProxy.setText(registry.getText("Proxy"));
        _colPullOnly.setText(registry.getText("Read only"));
        _colAutomatic.setText(registry.getText("Auto sync"));
        
        _colImport.setToolTipText(registry.getText("Archive"));
        _colLocation.setToolTipText(registry.getText("Archive location"));
        _colProxy.setToolTipText(registry.getText("HTTP Proxy to access it"));
        _colPullOnly.setToolTipText(registry.getText("Read only archive (pull only, do not push)?"));
        _colAutomatic.setToolTipText(registry.getText("Schedule sharing immediately"));
        

        _colAutomatic.setWidth(100);
        _colPullOnly.setWidth(50);
        _colProxy.setWidth(150);
        _colImport.setWidth(300);
        _colLocation.setWidth(200);
    }
    
    public void applyTheme(Theme theme) {
        _archives.setFont(theme.TABLE_FONT);
    }
}

