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
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.db.DBClient;
import syndie.db.SharedArchiveEngine;
import syndie.db.SyncArchive;
import syndie.db.SyncManager;
import syndie.db.UI;

/**
 *
 */
public class ArchiveDefaultsPopup implements Themeable, Translatable {
    private DBClient _client;
    private UI _ui;
    private Shell _parent;
    private ThemeRegistry _themeRegistry;
    private TranslationRegistry _translationRegistry;
    
    private Shell _shell;
    private Label _explanationWhy;
    private Table _archives;
    private TableColumn _colImport;
    private TableColumn _colLocation;
    private TableColumn _colProxy;
    private TableColumn _colPullOnly;
    private TableColumn _colAutomatic;
    private Label _instructions;
    private Button _save;
    private Button _cancel;
    
    public ArchiveDefaultsPopup(DBClient client, UI ui, Shell parent, ThemeRegistry themeRegistry, TranslationRegistry translationRegistry) {
        _client = client;
        _ui = ui;
        _parent = parent;
        _themeRegistry = themeRegistry;
        _translationRegistry = translationRegistry;
        initComponents();
    }
    
    private static final int TABLE_WIDTH = 750;
    
    private void initComponents() {
        _shell = new Shell(_parent, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        GridLayout gl = new GridLayout(2, true);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        _shell.setLayout(gl);
        
        _explanationWhy = new Label(_shell, SWT.WRAP);
        _explanationWhy.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _archives = new Table(_shell, SWT.SINGLE | SWT.CHECK | SWT.BORDER | SWT.FULL_SELECTION);
        _archives.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
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
                Point loc = _shell.getDisplay().getCursorLocation();
                Point reloc = _shell.getDisplay().map(null, _archives, loc);
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
        
        _instructions = new Label(_shell, SWT.WRAP);
        _instructions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _save = new Button(_shell, SWT.PUSH);
        _save.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _save.addSelectionListener(new FireSelectionListener() {
            public void fire() { save(); }
        });
        
        _cancel = new Button(_shell, SWT.PUSH);
        _cancel.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _cancel.addSelectionListener(new FireSelectionListener() {
            public void fire() { dispose(); }
        });
        
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent shellEvent) { dispose(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });

        loadDefaults(); // packed on translate
        
        _archives.setSize(TABLE_WIDTH, 700);
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
        
        _shell.layout(true, true);
        _shell.open();
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
     * archive.1.url=http://syndie.i2p.net:8080/
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
                String str[] = Constants.split(':', proxy);
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

    private static final String T_PROXY_NONE = "syndie.gui.archivedefaultspopup.proxy_none";
    
    private void addToDefaults(String name, String url, String proxyHost, int proxyPort, String pullPolicy, String pushPolicy, boolean syncByDefault) {
        TableItem item = new TableItem(_archives, SWT.LEFT);
        item.setChecked(true);
        
        item.setText(0, name);
        item.setText(1, url);
        if ( (proxyHost != null) && (proxyPort > 0) )
            item.setText(2, proxyHost + ":" + proxyPort);
        else
            item.setText(2, _translationRegistry.getText(T_PROXY_NONE, "no proxy"));
        if ( (pushPolicy != null) && (pushPolicy.contains("SendNothing")) )
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
                if (item.getText(col).equals("X"))
                    item.setText(col, "");
                else
                    item.setText(col, "X");
                break;
            case 4: // toggle auto
                if (item.getText(col).equals("X"))
                    item.setText(col, "");
                else
                    item.setText(col, "X");
                break;
        }
    }
    
    private static final String T_EDIT_NAME = "syndie.gui.archivedefaultspopup.editname";
    private static final String T_EDIT_NAME_TITLE = "syndie.gui.archivedefaultspopup.editname.title";
    private static final String T_EDIT_NAME_OK = "syndie.gui.archivedefaultspopup.editname.ok";
    private static final String T_EDIT_NAME_CANCEL = "syndie.gui.archivedefaultspopup.editname.cancel";
    
    private void editName(final TableItem item) {
        final Shell shell = new Shell(_shell, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL | SWT.BORDER);
        GridLayout gl = new GridLayout(4, false);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        shell.setLayout(gl);
        shell.setText(_translationRegistry.getText(T_EDIT_NAME_TITLE, "Edit name"));
        
        shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent shellEvent) { shell.dispose(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        Label l = new Label(shell, SWT.NONE);
        l.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
        l.setText(_translationRegistry.getText(T_EDIT_NAME, "Archive name: "));
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
        b.setText(_translationRegistry.getText(T_EDIT_NAME_OK, "OK"));
        b.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        b.addSelectionListener(new FireSelectionListener() {
            public void fire() { 
                item.setText(0, name.getText()); 
                shell.dispose();
            }
        });
        
        b = new Button(shell, SWT.PUSH);
        b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        b.setText(_translationRegistry.getText(T_EDIT_NAME_CANCEL, "Cancel"));
        b.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        b.addSelectionListener(new FireSelectionListener() {
            public void fire() { shell.dispose(); }
        });
        
        shell.pack();
        shell.open();
    }

    private static final String T_EDIT_URL = "syndie.gui.archivedefaultspopup.editurl";
    private static final String T_EDIT_URL_TITLE = "syndie.gui.archivedefaultspopup.editurl.title";
    private static final String T_EDIT_URL_OK = "syndie.gui.archivedefaultspopup.editurl.ok";
    private static final String T_EDIT_URL_CANCEL = "syndie.gui.archivedefaultspopup.editurl.cancel";
    
    private void editURL(final TableItem item) {
        final Shell shell = new Shell(_shell, SWT.DIALOG_TRIM | SWT.BORDER | SWT.PRIMARY_MODAL);
        GridLayout gl = new GridLayout(4, false);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        shell.setLayout(gl);
        shell.setText(_translationRegistry.getText(T_EDIT_URL_TITLE, "Edit URL"));
        shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent shellEvent) { shell.dispose(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        Label l = new Label(shell, SWT.NONE);
        l.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
        l.setText(_translationRegistry.getText(T_EDIT_URL, "Archive location: "));
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
        b.setText(_translationRegistry.getText(T_EDIT_URL_OK, "OK"));
        b.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        b.addSelectionListener(new FireSelectionListener() {
            public void fire() { 
                item.setText(1, url.getText()); 
                shell.dispose();
            }
        });
        
        b = new Button(shell, SWT.PUSH);
        b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        b.setText(_translationRegistry.getText(T_EDIT_URL_CANCEL, "Cancel"));
        b.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        b.addSelectionListener(new FireSelectionListener() {
            public void fire() { shell.dispose(); }
        });
        
        shell.pack();
        shell.open();
    }
    
    private static final String T_EDIT_PROXY = "syndie.gui.archivedefaultspopup.editproxy";
    private static final String T_EDIT_PROXY_TITLE = "syndie.gui.archivedefaultspopup.editproxy.title";
    private static final String T_EDIT_PROXY_OK = "syndie.gui.archivedefaultspopup.editproxy.ok";
    private static final String T_EDIT_PROXY_CANCEL = "syndie.gui.archivedefaultspopup.editproxy.cancel";
    
    private void editProxy(final TableItem item) {
        String proxy = item.getText(2);
        String proxyHost = null;
        int proxyPort = -1;
        if ( (proxy != null) || (proxy.indexOf(':') > 0) ) {
            String str[] = Constants.split(':', proxy);
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
        
        final Shell shell = new Shell(_shell, SWT.DIALOG_TRIM | SWT.BORDER | SWT.PRIMARY_MODAL);
        GridLayout gl = new GridLayout(6, false);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        shell.setLayout(gl);
        shell.setText(_translationRegistry.getText(T_EDIT_PROXY_TITLE, "Edit proxy"));
        shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent shellEvent) { shell.dispose(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        Label l = new Label(shell, SWT.NONE);
        l.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
        l.setText(_translationRegistry.getText(T_EDIT_URL, "Proxy host: "));
        l.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        
        final Text host = new Text(shell, SWT.SINGLE | SWT.BORDER | SWT.WRAP);
        host.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        host.setText(proxyHost != null && proxyPort > 0  ? proxyHost : "");
        host.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        
        l = new Label(shell, SWT.NONE);
        l.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
        l.setText(_translationRegistry.getText(T_EDIT_URL, "port: "));
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
                        item.setText(2, _translationRegistry.getText(T_PROXY_NONE, "no proxy"));
                        shell.dispose();
                    }
                }
            }
        });
        
        Button b = new Button(shell, SWT.PUSH);
        b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        b.setText(_translationRegistry.getText(T_EDIT_URL_OK, "OK"));
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
                    item.setText(2, _translationRegistry.getText(T_PROXY_NONE, "no proxy"));
                    shell.dispose();
                }
            }
        });
        
        b = new Button(shell, SWT.PUSH);
        b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        b.setText(_translationRegistry.getText(T_EDIT_URL_CANCEL, "Cancel"));
        b.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        b.addSelectionListener(new FireSelectionListener() {
            public void fire() { shell.dispose(); }
        });
        
        shell.pack();
        shell.open();
    }
    
    private void save() {
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
                String str[] = Constants.split(':', proxy);
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
                else if (!pushPolicy.contains("SendNothing"))
                    pushPolicy = pushPolicy + " SendNothing";
            } else {
                if (pushPolicy != null) {
                    StringBuffer buf = new StringBuffer(pushPolicy);
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
                archive.setPullStrategy(new SharedArchiveEngine.PullStrategy(pullPolicy));
            if (pushPolicy != null)
                archive.setPushStrategy(new SharedArchiveEngine.PushStrategy(pushPolicy));
            if (syncByDefault)
                archive.setNextSyncTime(System.currentTimeMillis());
            else
                archive.setNextSyncTime(-1);
            archive.store();
        }
        dispose();
    }
    
    private void dispose() {
        _themeRegistry.unregister(this);
        _translationRegistry.unregister(this);
        _shell.dispose();
    }

    private static final String T_TITLE = "syndie.gui.archivedefaultspopup.title";
    private static final String T_EXPLANATIONWHY = "syndie.gui.archivedefaultspopup.explanationwhy";
    private static final String T_COLIMPORT = "syndie.gui.archivedefaultspopup.import";
    private static final String T_COLLOCATION = "syndie.gui.archivedefaultspopup.collocation";
    private static final String T_COLPROXY = "syndie.gui.archivedefaultspopup.colproxy";
    private static final String T_COLPULLONLY = "syndie.gui.archivedefaultspopup.colpullonly";
    private static final String T_COLAUTOMATIC = "syndie.gui.archivedefaultspopup.colautomatic";
    private static final String T_COLIMPORT_TT = "syndie.gui.archivedefaultspopup.import.tt";
    private static final String T_COLLOCATION_TT = "syndie.gui.archivedefaultspopup.collocation.tt";
    private static final String T_COLPROXY_TT = "syndie.gui.archivedefaultspopup.colproxy.tt";
    private static final String T_COLPULLONLY_TT = "syndie.gui.archivedefaultspopup.colpullonly.tt";
    private static final String T_COLAUTOMATIC_TT = "syndie.gui.archivedefaultspopup.colautomatic.tt";
    private static final String T_INSTRUCTIONS = "syndie.gui.archivedefaultspopup.instructions";
    private static final String T_SAVE = "syndie.gui.archivedefaultspopup.save";
    private static final String T_CANCEL = "syndie.gui.archivedefaultspopup.cancel";
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText(T_TITLE, "Archive defaults"));
        _explanationWhy.setText(registry.getText(T_EXPLANATIONWHY, "You have no remote archives configured, preventing you from either reading other people's posts or from sharing your own posts with them."));
        _colImport.setText(registry.getText(T_COLIMPORT, "Archive"));
        _colLocation.setText(registry.getText(T_COLLOCATION, "URL"));
        _colProxy.setText(registry.getText(T_COLPROXY, "Proxy"));
        _colPullOnly.setText(registry.getText(T_COLPULLONLY, "r/o?"));
        _colAutomatic.setText(registry.getText(T_COLAUTOMATIC, "auto?"));

        _colImport.setToolTipText(registry.getText(T_COLIMPORT_TT, "Archive"));
        _colLocation.setToolTipText(registry.getText(T_COLLOCATION_TT, "Archive location"));
        _colProxy.setToolTipText(registry.getText(T_COLPROXY_TT, "Proxy to access it"));
        _colPullOnly.setToolTipText(registry.getText(T_COLPULLONLY_TT, "Read only archive?"));
        _colAutomatic.setToolTipText(registry.getText(T_COLAUTOMATIC_TT, "Schedule sharing immediately"));

        _instructions.setText(registry.getText(T_INSTRUCTIONS, "The default archives shipped with your Syndie instance are listed above.  Please make any necessary changes, unchecking any that you don't want"));
        _save.setText(registry.getText(T_SAVE, "Save archives as your own"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel"));

        /*
        _colAutomatic.pack();
        _colImport.pack();
        _colLocation.pack();
        _colProxy.pack();
        _colPullOnly.pack();
         */
        
        _colAutomatic.setWidth(100);
        _colPullOnly.setWidth(50);
        _colProxy.setWidth(150);
        _colImport.setWidth(300);
        _colLocation.setWidth(200);
    }
    public void applyTheme(Theme theme) {
        _shell.setFont(theme.SHELL_FONT);
        _explanationWhy.setFont(theme.DEFAULT_FONT);
        _archives.setFont(theme.TABLE_FONT);
        _instructions.setFont(theme.DEFAULT_FONT);
        _save.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
        _shell.layout(true, true);
    }
}
