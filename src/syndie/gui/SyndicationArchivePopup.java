package syndie.gui;

import java.net.URISyntaxException;
import javax.print.attribute.URISyntax;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

/**
 *
 */
class SyndicationArchivePopup implements Translatable {
    private BrowserControl _browser;
    private Shell _parent;
    private Shell _shell;
    private Label _nameLabel;
    private Text _name;
    private Button _urlRadio;
    private Text _url;
    private Button _freenetRadio;
    private Text _freenetKey;
    private Button _syndieRadio;
    private Text _syndie;
    private Label _proxyLabel;
    private Button _proxyDefault;
    private Button _proxyNone;
    private Button _proxyCustom;
    private Label _proxyCustomHostLabel;
    private Text _proxyCustomHost;
    private Label _proxyCustomPortLabel;
    private Text _proxyCustomPort;
    
    private Button _ok;
    private Button _cancel;
    
    private SyndieURI _uri;
    private String _oldName;
    
    public SyndicationArchivePopup(BrowserControl browser, Shell parent) {
        _browser = browser;
        _parent = parent;
        _oldName = null;
        initComponents();
    }
    
    public void config(String name, SyndieURI uri, String proxy, int port) {
        _name.setText(name);
        _uri = uri;
        _oldName = ("".equals(name) ? null : name);
        
        _urlRadio.setSelection(false);
        _freenetRadio.setSelection(false);
        _syndieRadio.setSelection(false);
        _syndie.setText("");
        _freenetKey.setText("");
        _url.setText("");
        _proxyDefault.setSelection(false);
        _proxyNone.setSelection(false);
        _proxyCustom.setSelection(false);
        _proxyCustomHost.setText("");
        _proxyCustomPort.setText("");
        
        if (uri != null) {
            String url = uri.getURL();
            if (url == null) {
                _syndie.setText(uri.toString());
                _syndieRadio.setSelection(true);
            } else if ( (url.indexOf("SSK@") >= 0) || (url.indexOf("CHK@") >= 0) || (url.indexOf("USK@") >= 0) ) {
                int idx = url.indexOf("CHK@");
                if (idx < 0)
                    idx = url.indexOf("SSK@");
                if (idx < 0)
                    idx = url.indexOf("USK@");
                int end = url.indexOf('?', idx);
                if (end > 0)
                    _freenetKey.setText(url.substring(idx, end));
                else
                    _freenetKey.setText(url.substring(idx));
                _freenetRadio.setSelection(true);
            } else {
                _url.setText(url);
                _urlRadio.setSelection(true);
            }
        }
        
        if (proxy == null) {
            _proxyDefault.setSelection(true);
        } else if (proxy.trim().length() == 0) {
            _proxyNone.setSelection(true);
        } else {
            _proxyCustom.setSelection(true);
            _proxyCustomHost.setText(proxy.trim());
            _proxyCustomPort.setText(port+"");
        }
    }
    
    public void open() {
        _shell.pack();
        _shell.setSize(_shell.computeSize(400, SWT.DEFAULT));
        _shell.setVisible(true);
    }
    
    private void initComponents() {
        _shell = new Shell(_parent, SWT.SHELL_TRIM | SWT.APPLICATION_MODAL);
        _shell.setLayout(new GridLayout(2, false));
        
        _nameLabel = new Label(_shell, SWT.NONE);
        _nameLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _name = new Text(_shell, SWT.SINGLE | SWT.BORDER);
        _name.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        Composite target = new Composite(_shell, SWT.NONE);
        target.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        target.setLayout(new GridLayout(2, false));
        
        _urlRadio = new Button(target, SWT.RADIO);
        _urlRadio.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _url = new Text(target, SWT.BORDER);
        _url.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _freenetRadio = new Button(target, SWT.RADIO);
        _freenetRadio.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _freenetKey = new Text(target, SWT.BORDER);
        _freenetKey.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _syndieRadio = new Button(target, SWT.RADIO);
        _syndieRadio.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _syndie = new Text(target, SWT.BORDER);
        _syndie.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        Composite proxy = new Composite(_shell, SWT.NONE);
        proxy.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        proxy.setLayout(new GridLayout(6, false));
        
        _proxyLabel = new Label(proxy, SWT.NONE);
        _proxyLabel.setLayoutData(new GridData(GridData.END, GridData.BEGINNING, false, false, 1, 3));
        
        _proxyDefault = new Button(proxy, SWT.RADIO);
        _proxyDefault.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 5, 1));
        _proxyNone = new Button(proxy, SWT.RADIO);
        _proxyNone.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 5, 1));
        _proxyCustom = new Button(proxy, SWT.RADIO);
        _proxyCustom.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _proxyCustomHostLabel = new Label(proxy, SWT.NONE);
        _proxyCustomHostLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _proxyCustomHost = new Text(proxy, SWT.BORDER);
        _proxyCustomHost.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _proxyCustomPortLabel = new Label(proxy, SWT.NONE);
        _proxyCustomPortLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _proxyCustomPort = new Text(proxy, SWT.BORDER);
        _proxyCustomPort.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
    
        Composite actions = new Composite(_shell, SWT.NONE);
        actions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        _ok = new Button(actions, SWT.NONE);
        _ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { ok(); }
            public void widgetSelected(SelectionEvent selectionEvent) { ok(); }
        });
        _cancel = new Button(actions, SWT.NONE);
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { cancel(); }
            public void widgetSelected(SelectionEvent selectionEvent) { cancel(); }
        });
        
        // intercept the shell closing, since that'd cause the shell to be disposed rather than just hidden
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; cancel(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        _browser.getTranslationRegistry().register(this);
    }
    
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
    }
    
    private void ok() {
        // actually save the val
        SyndieURI uri = _uri;
        if (_name.getText().trim().length() <= 0) {
            MessageBox box = new MessageBox(_shell, SWT.ICON_ERROR | SWT.OK);
            box.setText(_browser.getTranslationRegistry().getText(T_ERROR, "Error"));
            box.setMessage(_browser.getTranslationRegistry().getText(T_ERROR_NONAME, "The archive name is required"));
            box.open();
            return;
        }
        if (_urlRadio.getSelection()) {
            if (_url.getText().trim().length() <= 0) {
                MessageBox box = new MessageBox(_shell, SWT.ICON_ERROR | SWT.OK);
                box.setText(_browser.getTranslationRegistry().getText(T_ERROR, "Error"));
                box.setMessage(_browser.getTranslationRegistry().getText(T_ERROR_NOURL, "The URL is required for HTTP archives"));
                box.open();
                return;
            } else {
                uri = SyndieURI.createURL(_url.getText().trim());
            }
        } else if (_freenetRadio.getSelection()) {
            if (_freenetKey.getText().trim().length() <= 0) {
                MessageBox box = new MessageBox(_shell, SWT.ICON_ERROR | SWT.OK);
                box.setText(_browser.getTranslationRegistry().getText(T_ERROR, "Error"));
                box.setMessage(_browser.getTranslationRegistry().getText(T_ERROR_NOFREENET, "The freenet key is required for Freenet archives"));
                box.open();
                return;
            } else {
                uri = SyndieURI.createURL(_freenetKey.getText().trim());
            }
        } else if (_syndieRadio.getSelection()) {
            if (_syndie.getText().trim().length() <= 0) {
                MessageBox box = new MessageBox(_shell, SWT.ICON_ERROR | SWT.OK);
                box.setText(_browser.getTranslationRegistry().getText(T_ERROR, "Error"));
                box.setMessage(_browser.getTranslationRegistry().getText(T_ERROR_NOSYNDIE, "The full syndie URI is required for all other archives"));
                box.open();
                return;
            } else {
                try {
                    uri = new SyndieURI(_syndie.getText());
                } catch (URISyntaxException use) {
                    MessageBox box = new MessageBox(_shell, SWT.ICON_ERROR | SWT.OK);
                    box.setText(_browser.getTranslationRegistry().getText(T_ERROR, "Error"));
                    box.setMessage(_browser.getTranslationRegistry().getText(T_ERROR_BADSYNDIE, "The full syndie URI is not valid"));
                    box.open();
                    return;
                }
            }
        } else {
            MessageBox box = new MessageBox(_shell, SWT.ICON_ERROR | SWT.OK);
            box.setText(_browser.getTranslationRegistry().getText(T_ERROR, "Error"));
            box.setMessage(_browser.getTranslationRegistry().getText(T_ERROR_NOTARGET, "The archive location must be specified"));
            box.open();
            return;
        }
        
        String name = _name.getText().trim();
        String proxy = null;
        int port = -1;
        
        if (_proxyNone.getSelection()) {
            proxy = "";
        } else if (_proxyCustom.getSelection()) {
            proxy = _proxyCustomHost.getText().trim();
            try {
                port = Integer.parseInt(_proxyCustomPort.getText().trim());
            } catch (NumberFormatException nfe) {
                port = -1;
            }
            if ( (proxy.length() <= 0) || (port <= 0) ) {
                MessageBox box = new MessageBox(_shell, SWT.ICON_ERROR | SWT.OK);
                box.setText(_browser.getTranslationRegistry().getText(T_ERROR, "Error"));
                box.setMessage(_browser.getTranslationRegistry().getText(T_ERROR_BADPROXY, "The custom proxy is invalid"));
                box.open();
                return;                
            }
        }
        
        _shell.setVisible(false);
        if (_oldName != null)
            _browser.getSyndicationManager().update(_oldName, name, uri, proxy, port, null, null);
        else
            _browser.getSyndicationManager().add(name, uri, proxy, port, null, null);
        config("", null, null, -1);
    }
    private void cancel() {
        config("", null, null, -1);
        _shell.setVisible(false);
    }

    private static final String T_ERROR = "syndie.gui.syndicationarchivepopup.error";
    private static final String T_ERROR_NONAME = "syndie.gui.syndicationarchivepopup.error.noname";
    private static final String T_ERROR_NOURL = "syndie.gui.syndicationarchivepopup.error.nourl";
    private static final String T_ERROR_NOFREENET = "syndie.gui.syndicationarchivepopup.error.nofreenet";
    private static final String T_ERROR_NOSYNDIE = "syndie.gui.syndicationarchivepopup.error.nosyndie";
    private static final String T_ERROR_BADSYNDIE = "syndie.gui.syndicationarchivepopup.error.badsyndie";
    private static final String T_ERROR_BADPROXY = "syndie.gui.syndicationarchivepopup.error.badproxy";
    private static final String T_ERROR_NOTARGET = "syndie.gui.syndicationarchivepopup.error.notarget";
    
    private static final String T_TITLE = "syndie.gui.syndicationarchivepopup.title";
    private static final String T_NAME = "syndie.gui.syndicationarchivepopup.name";
    private static final String T_URL = "syndie.gui.syndicationarchivepopup.url";
    private static final String T_FREENET = "syndie.gui.syndicationarchivepopup.freenet";
    private static final String T_OTHER = "syndie.gui.syndicationarchivepopup.other";
    private static final String T_PROXY = "syndie.gui.syndicationarchivepopup.proxy";
    private static final String T_PROXY_DEFAULT = "syndie.gui.syndicationarchivepopup.default";
    private static final String T_PROXY_NONE = "syndie.gui.syndicationarchivepopup.none";
    private static final String T_PROXY_CUSTOM = "syndie.gui.syndicationarchivepopup.custom";
    private static final String T_PROXY_CUSTOM_HOST = "syndie.gui.syndicationarchivepopup.customhost";
    private static final String T_PROXY_CUSTOM_PORT = "syndie.gui.syndicationarchivepopup.customport";
    private static final String T_OK = "syndie.gui.syndicationarchivepopup.ok";
    private static final String T_CANCEL = "syndie.gui.syndicationarchivepopup.cancel";
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText(T_TITLE, "Archive"));
        _nameLabel.setText(registry.getText(T_NAME, "Name: "));
        _urlRadio.setText(registry.getText(T_URL, "HTTP archive URL: "));
        _freenetRadio.setText(registry.getText(T_FREENET, "Freenet archive URL: "));
        _syndieRadio.setText(registry.getText(T_OTHER, "Other archive URL: "));
        _proxyLabel.setText(registry.getText(T_PROXY, "Proxy: "));
        _proxyDefault.setText(registry.getText(T_PROXY_DEFAULT, "Default"));
        _proxyNone.setText(registry.getText(T_PROXY_NONE, "None"));
        _proxyCustom.setText(registry.getText(T_PROXY_CUSTOM, "Custom"));
        _proxyCustomHostLabel.setText(registry.getText(T_PROXY_CUSTOM_HOST, "Host: "));
        _proxyCustomPortLabel.setText(registry.getText(T_PROXY_CUSTOM_PORT, "Port: "));
        _ok.setText(registry.getText(T_OK, "Save"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel"));
    }
}
