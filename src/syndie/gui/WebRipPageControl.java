package syndie.gui;

import java.io.File;
import java.util.Collections;
import java.util.List;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import syndie.data.WebRipRunner;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 * rip a web page to add as a new page to an existing post
 */
public class WebRipPageControl extends BaseComponent implements Translatable, Themeable, WebRipRunner.RipListener {
    private Composite _parent;
    private Composite _root;
    private Label _urlLabel;
    private Text _url;
    private Button _urlOptions;
    private Button _rip;
    private Button _cancel;
    private Label _status;
    private Menu _optionMenu;
    private MenuItem _optionImages;
    private MenuItem _optionTorrents;
    private MenuItem _optionAllowFiles;
    private MenuItem _optionProxy;
    
    private Shell _proxyShell;
    private Label _proxyHostLabel;
    private Text _proxyHost;
    private Label _proxyPortLabel;
    private Text _proxyPort;
    private Button _proxyAsDefault;
    private Button _proxyOk;
    
    private int _existingAttachments;
    
    private WebRipRunner _ripRunner;
    private List _errorMessages;
    private RipControlListener _listener;
    
    public WebRipPageControl(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite parent) {
        super(client, ui, themes, trans);
        _parent = parent;
        _existingAttachments = 0;
        _errorMessages = Collections.EMPTY_LIST;
        initComponents();
    }
    
    public void setListener(RipControlListener lsnr) { _listener = lsnr; }
    public void setExistingAttachments(int count) { _existingAttachments = count; }
    
    public interface RipControlListener {
        /** run from within the swt thread */
        public void ripComplete(boolean successful, WebRipRunner runner);
    }
    
    public List getErrorMessages() { 
        if (_ripRunner == null)
            return _errorMessages;
        else
            return _ripRunner.getErrorMessages(); 
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(3, false));
        
        _urlLabel = new Label(_root, SWT.NONE);
        _urlLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _url = new Text(_root, SWT.SINGLE | SWT.BORDER);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        gd.widthHint = 300;
        _url.setLayoutData(gd);
        _urlOptions = new Button(_root, SWT.PUSH);
        _urlOptions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _urlOptions.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _optionMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _optionMenu.setVisible(true); }
        });
        
        _optionMenu = new Menu(_urlOptions);
        createOptions(_optionMenu);
        
        createAttributeFields(_root);
        
        _rip = new Button(_root, SWT.PUSH);
        _rip.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        _rip.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { rip(); }
            public void widgetSelected(SelectionEvent selectionEvent) { rip(); }
        });
        
        _cancel = new Button(_root, SWT.PUSH);
        _cancel.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { cancel(); }
            public void widgetSelected(SelectionEvent selectionEvent) { cancel(); }
        });
        
        _status = new Label(_root, SWT.NONE);
        gd = new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1);
        _status.setLayoutData(gd);
    
        _proxyShell = new Shell(_root.getShell(), SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        _proxyShell.setLayout(new GridLayout(2, false));
        _proxyHostLabel = new Label(_proxyShell, SWT.NONE);
        _proxyHostLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _proxyHost = new Text(_proxyShell, SWT.SINGLE | SWT.BORDER);
        gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        gd.widthHint = 75;
        _proxyHost.setLayoutData(gd);
        _proxyPortLabel = new Label(_proxyShell, SWT.NONE);
        _proxyPortLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _proxyPort = new Text(_proxyShell, SWT.SINGLE | SWT.BORDER);
        gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        gd.widthHint = 75;
        _proxyPort.setLayoutData(gd);
        _proxyAsDefault = new Button(_proxyShell, SWT.CHECK);
        _proxyAsDefault.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        _proxyOk = new Button(_proxyShell, SWT.PUSH);
        _proxyOk.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        _proxyOk.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { hideProxyConfig(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { hideProxyConfig(true); }
        });
        _proxyShell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; hideProxyConfig(false); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        _proxyShell.pack();
        
        String host = _client.getDefaultHTTPProxyHost();
        int port = _client.getDefaultHTTPProxyPort();
        if ( (port > 0) && (host != null) && (host.length() > 0) ) {
            _proxyPort.setText(Integer.toString(port));
            _proxyHost.setText(host);
            _proxyAsDefault.setSelection(true);
        } else {
            _proxyPort.setText("");
            _proxyHost.setText("");
            _proxyAsDefault.setSelection(false);
        }
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    public void cancel() {
        if (_ripRunner != null)
            _ripRunner.abortRip();
        if (_listener != null)
            _listener.ripComplete(false, _ripRunner);
    }
    
    /** 
     * be sure to use the ripRunner's data before disposing this control, as 
     * the files are deleted here
     */
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        if (!_proxyShell.isDisposed()) 
            _proxyShell.dispose();
        if (_ripRunner != null) {
            _ripRunner.abortRip();
            _ripRunner.cleanupData();
        }
    }
    
    protected void createOptions(Menu optionMenu) {
        _optionImages = new MenuItem(optionMenu, SWT.CHECK);
        _optionImages.setSelection(true);
        _optionTorrents = new MenuItem(optionMenu, SWT.CHECK);
        _optionTorrents.setSelection(false);
        _optionAllowFiles = new MenuItem(optionMenu, SWT.CHECK);
        _optionAllowFiles.setSelection(false);
        _optionProxy = new MenuItem(optionMenu, SWT.PUSH);
        _optionProxy.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { configProxy(); }
            public void widgetSelected(SelectionEvent selectionEvent) { configProxy(); }
        });
    }
    
    
    protected void createAttributeFields(Composite root) {}
    
    private void configProxy() { _proxyShell.open(); }
    private void hideProxyConfig(boolean save) { 
        _proxyShell.setVisible(false);
        _ui.debugMessage("hiding proxy config (" + save + "): saveAsDefault? " + _proxyAsDefault.getSelection());
        if (save && _proxyAsDefault.getSelection()) {
            String host = _proxyHost.getText().trim();
            String port = _proxyPort.getText().trim();
            try {
                int portNum = Integer.parseInt(port);
                if ( (host.length() > 0) && (portNum > 0) ) {
                    _client.setDefaultHTTPProxyHost(host);
                    _client.setDefaultHTTPProxyPort(portNum);
                } else {
                    _client.setDefaultHTTPProxyHost(null);
                    _client.setDefaultHTTPProxyPort(-1);
                }
            } catch (NumberFormatException nfe) {
                _client.setDefaultHTTPProxyHost(null);
                _client.setDefaultHTTPProxyPort(-1);
            }
            _client.saveProxyConfig();
        }
    }
    
    public String getURL() { return _url.getText().trim(); }
    
    protected void rip() {
        _rip.setEnabled(false);
        _urlOptions.setEnabled(false);
        _url.setEnabled(false);
        _urlLabel.setEnabled(false);
        _root.layout(true, true);
        String url = _url.getText();
        if (url.trim().length() <= 0) {
            cancel();
            return;
        }
        
        _ui.debugMessage("ripping (existing attachments: " + _existingAttachments + ")");
        
        String proxy = _proxyHost.getText();
        int proxyPort = -1;
        try {
            proxyPort = Integer.parseInt(_proxyPort.getText());
        } catch (NumberFormatException nfe) {}
        _ripRunner = new WebRipRunner(_ui, _url.getText(), proxy, proxyPort, _client.getTempDir());
        _ripRunner.configure(_optionImages.getSelection(), _optionTorrents.getSelection(), getMaxAttachKB(), getMaxTotalKB(), _optionAllowFiles.getSelection(), _existingAttachments);
        _ripRunner.setListener(this);
        statusUpdated(T_STATUS_INIT, DEFAULT_STATUS_INIT, "", false, false);
        
        _ui.debugMessage("nbrip start");
        _ripRunner.nonblockingRip();
    }

    private static final String T_STATUS_INIT = "syndie.gui.webripcontrol.status.init";
    private static final String DEFAULT_STATUS_INIT = "Step 1 of 6: Rip initialized";
    private static final String T_STATUS_FETCHING_HTML = "syndie.gui.webripcontrol.status.fetchhtml";
    private static final String DEFAULT_STATUS_FETCHING_HTML = "Step 2 of 6: Fetching web page";
    private static final String T_STATUS_HTML_FETCHED = "syndie.gui.webripcontrol.status.htmlfetched";
    private static final String DEFAULT_STATUS_HTML_FETCHED = "Step 3 of 6: Web page fetched";
    private static final String T_STATUS_FETCHING_ATTACHMENTS = "syndie.gui.webripcontrol.status.fetchingattachments";
    private static final String DEFAULT_STATUS_FETCHING_ATTACHMENTS = "Step 4 of 6: Fetching attachments - ";
    private static final String T_STATUS_ATTACHMENTS_FETCHED = "syndie.gui.webripcontrol.status.attachmentsfetched";
    private static final String DEFAULT_STATUS_ATTACHMENTS_FETCHED = "Step 5 of 6: Attachments fetched";
    private static final String T_STATUS_REWRITTEN = "syndie.gui.webripcontrol.status.rewritten";
    private static final String DEFAULT_STATUS_REWRITTEN = "Step 6 of 6: Web page rewritten";
    
    private static final String T_STATUS_ERROR = "syndie.gui.webripcontrol.status.error";
    private static final String DEFAULT_STATUS_ERROR = "Rip failed";
    
    public void statusUpdated(WebRipRunner runner) {
        if (_root.isDisposed()) return;
        _ui.debugMessage("statusUpdated caled in the wrc");
        int state = runner.getState();
        final String key;
        final String def;
        final String detail;
        final boolean terminal;
        final boolean success;
        switch (state) {
            case WebRipRunner.STATE_FETCH_ATTACHMENTS_COMPLETE:
                key = T_STATUS_ATTACHMENTS_FETCHED;
                def = DEFAULT_STATUS_ATTACHMENTS_FETCHED;
                detail = null;
                terminal = false;
                success = false;
                break;
            case WebRipRunner.STATE_FETCH_HTML_COMPLETE:
                key = T_STATUS_HTML_FETCHED;
                def = DEFAULT_STATUS_HTML_FETCHED;
                detail = null;
                terminal = false;
                success = false;
                break;
            case WebRipRunner.STATE_REWRITE_HTML_COMPLETE:
                key = T_STATUS_REWRITTEN;
                def = DEFAULT_STATUS_REWRITTEN;
                detail = null;
                terminal = true;
                success = true;
                break;
            case WebRipRunner.STATE_STARTED_FETCH_ATTACHMENTS:
                key = T_STATUS_FETCHING_ATTACHMENTS;
                def = DEFAULT_STATUS_FETCHING_ATTACHMENTS;
                int total = runner.getTotalAttachments();
                int fetched = total - runner.getPendingAttachments();
                detail = fetched + "/" + total;
                terminal = false;
                success = false;
                break;
            case WebRipRunner.STATE_STARTED_FETCH_HTML:
                key = T_STATUS_FETCHING_HTML;
                def = DEFAULT_STATUS_FETCHING_HTML;
                detail = null;
                terminal = false;
                success = false;
                break;
            case WebRipRunner.STATE_INIT:
            case WebRipRunner.STATE_CONFIGURED:
                key = T_STATUS_INIT;
                def = DEFAULT_STATUS_INIT;
                detail = null;
                terminal = false;
                success = false;
                break;
            case WebRipRunner.STATE_ERROR:
            default:
                key = T_STATUS_ERROR;
                def = DEFAULT_STATUS_ERROR;
                StringBuilder buf = new StringBuilder();
                _errorMessages = runner.getErrorMessages();
                for (int i = 0; i < _errorMessages.size(); i++) {
                    String msg = (String)_errorMessages.get(i);
                    _ui.debugMessage(msg);
                    buf.append(msg).append(" ");
                }
                detail = buf.toString().trim();
                terminal = true;
                success = false;
                List exceptions = runner.getExceptions();
                for (int i = 0; i < exceptions.size(); i++)
                    _ui.errorMessage("error during rip", ((Exception)exceptions.get(i)));
                break;
        }
        
        _ui.debugMessage("nbrip status updated: " + def + "/" + detail + "/" + terminal + "/"+ success);
        
        _root.getDisplay().asyncExec(new Runnable() { public void run() { statusUpdated(key, def, detail, terminal, success); } });
    }

    /** run in the swt thread to update the status bar */
    private void statusUpdated(String translationKey, String translationDefault, String detail, boolean terminal, boolean success) {
        if (_status.isDisposed()) return;
        if ( (detail != null) && (detail.length() > 0) )
            _status.setText(_translationRegistry.getText(translationKey, translationDefault) + ": " + detail);
        else
            _status.setText(_translationRegistry.getText(translationKey, translationDefault));
        
        if (terminal && (_listener != null))
            _listener.ripComplete(success, _ripRunner);
    }
    
    private int getMaxAttachKB() { return 64; }
    private int getMaxTotalKB() { return 4*1024; }
    
    private static final String T_URL = "syndie.gui.webripcontrol.url";
    private static final String T_OPTIONS = "syndie.gui.webripcontrol.options";
    private static final String T_RIP = "syndie.gui.webripcontrol.rip";
    private static final String T_CANCEL = "syndie.gui.webripcontrol.cancel";
    private static final String T_OPT_ALLOWFILES = "syndie.gui.webripcontrol.opt.allowfiles";
    private static final String T_OPT_IMAGES = "syndie.gui.webripcontrol.opt.images";
    private static final String T_OPT_PROXY = "syndie.gui.webripcontrol.opt.proxy";
    private static final String T_OPT_TORRENTS = "syndie.gui.webripcontrol.opt.torrents";
    
    private static final String T_PROXY_HOST = "syndie.gui.webripcontrol.proxy.host";
    private static final String T_PROXY_PORT = "syndie.gui.webripcontrol.proxy.port";
    private static final String T_PROXY_OK = "syndie.gui.webripcontrol.proxy.ok";
    private static final String T_PROXY_ASDEFAULT = "syndie.gui.webripcontrol.proxy.asdefault";
    private static final String T_PROXY_SHELL = "syndie.gui.webripcontrol.proxy.shell";
    
    public void translate(TranslationRegistry registry) {
        _urlLabel.setText(registry.getText(T_URL, "Rip URL:"));
        _urlOptions.setText(registry.getText(T_OPTIONS, "Options..."));
        _rip.setText(registry.getText(T_RIP, "Rip!"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel"));
        _optionAllowFiles.setText(registry.getText(T_OPT_ALLOWFILES, "Import file attachments (dangerous!)"));
        _optionImages.setText(registry.getText(T_OPT_IMAGES, "Attach images"));
        _optionProxy.setText(registry.getText(T_OPT_PROXY, "Configure proxy..."));
        _optionTorrents.setText(registry.getText(T_OPT_TORRENTS, "Attach torrents"));
        
        _proxyHostLabel.setText(registry.getText(T_PROXY_HOST, "HTTP proxy host:"));
        _proxyPortLabel.setText(registry.getText(T_PROXY_PORT, "HTTP proxy port:"));
        _proxyAsDefault.setText(registry.getText(T_PROXY_ASDEFAULT, "Save as my default HTTP proxy"));
        _proxyOk.setText(registry.getText(T_PROXY_OK, "OK"));
        _proxyShell.setText(registry.getText(T_PROXY_SHELL, "Proxy"));
    }

    public void applyTheme(Theme theme) {
        _url.setFont(theme.DEFAULT_FONT);
        _rip.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
        _status.setFont(theme.LOG_FONT);
        _urlLabel.setFont(theme.DEFAULT_FONT);
        _urlOptions.setFont(theme.BUTTON_FONT);
        
        _proxyHost.setFont(theme.DEFAULT_FONT);
        _proxyHostLabel.setFont(theme.DEFAULT_FONT);
        _proxyOk.setFont(theme.BUTTON_FONT);
        _proxyPort.setFont(theme.DEFAULT_FONT);
        _proxyPortLabel.setFont(theme.DEFAULT_FONT);
        _proxyAsDefault.setFont(theme.DEFAULT_FONT);
        _proxyShell.setFont(theme.SHELL_FONT);
        
        _proxyShell.pack(true);
    }
}
