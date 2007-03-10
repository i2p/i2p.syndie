package syndie.gui;

import java.io.IOException;
import java.io.OutputStream;
import java.net.Socket;
import java.util.List;
import java.util.Map;
import net.i2p.data.DataHelper;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.db.SharedArchiveEngine;
import syndie.db.SyncArchive;
import syndie.db.SyncManager;
import syndie.db.SyncOutboundPusher;

/**
 *
 */
class SyndicatorDetailHTTPArchive implements Themeable, Translatable, Disposable, SyncArchive.SyncArchiveListener {
    private BrowserControl _browser;
    private Composite _parent;
    private SyncArchive _archive;
    
    private Composite _root;
    private Label _nameLabel;
    private Text _name;
    private Label _locationLabel;
    private Text _location;
    private Button _locationAdvanced;
    private Menu _locationAdvancedMenu;
    private MenuItem _locationAdvancedSendKey;
    private MenuItem _locationAdvancedReadKey;
    private MenuItem _locationAdvancedFreenet;
    private Label _proxyHostLabel;
    private Text _proxyHost;
    private Label _proxyPortLabel;
    private Text _proxyPort;
    private Label _pushPolicyLabel;
    private Combo _pushPolicy;
    private Label _pushMaxSizeLabel;
    private Combo _pushMaxSize;
    private Label _pullPolicyLabel;
    private Combo _pullPolicy;
    private Label _pullMaxSizeLabel;
    private Combo _pullMaxSize;
    private Label _pullNewAgeLabel;
    private Combo _pullNewAge;
    private Label _pushAgeLabel;
    private Combo _pushAge;
    private Button _pullPrivate;
    private Button _pullPrivateLocalOnly;
    private Button _pullPBE;
    private Label _lastSyncLabel;
    private Label _lastSync;
    private Label _nextSyncLabel;
    private Label _nextSync;
    private Button _nextSyncNow;
    private Button _nextSyncNever;
    private Button _nextSyncOneOff;
    private Label _nextSyncDelayLabel;
    private Combo _nextSyncDelay;
    private Label _failuresLabel;
    private Label _failures;
    private Button _backOffOnFailures;
    private Button _save;
    private Button _cancel;
    private Syndicator.SyndicationDetailListener _listener;
    
    public SyndicatorDetailHTTPArchive(BrowserControl browser, Composite parent, SyncArchive archive, Syndicator.SyndicationDetailListener lsnr) {
        _browser = browser;
        _parent = parent;
        _archive = archive;
        _listener = lsnr;
        initComponents();
        _archive.addListener(this);
    }
    
    public Control getControl() { return _root; }
    
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
        if (!_root.isDisposed()) _root.dispose();
        _archive.removeListener(this);
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(2, false));
        
        // name row
        
        _nameLabel = new Label(_root, SWT.NONE);
        _nameLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        Composite row = new Composite(_root, SWT.NONE);
        row.setLayout(new GridLayout(4, false));
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _name = new Text(row, SWT.SINGLE | SWT.BORDER);
        _name.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _name.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent modifyEvent) {
                _save.setEnabled(_name.getText().trim().length() > 0);
            }
        });
        
        _locationLabel = new Label(row, SWT.NONE);
        _locationLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _location = new Text(row, SWT.SINGLE | SWT.BORDER);
        _location.setLayoutData(new GridData(200, SWT.DEFAULT));
        
        _locationAdvanced = new Button(row, SWT.PUSH);
        _locationAdvanced.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _locationAdvancedMenu = new Menu(_locationAdvanced);
        _locationAdvanced.setMenu(_locationAdvancedMenu);
        _locationAdvanced.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _locationAdvancedMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _locationAdvancedMenu.setVisible(true); }
        });
        
        _locationAdvancedSendKey = new MenuItem(_locationAdvancedMenu, SWT.PUSH);
        _locationAdvancedSendKey.setEnabled(false);
        _locationAdvancedReadKey = new MenuItem(_locationAdvancedMenu, SWT.PUSH);
        _locationAdvancedReadKey.setEnabled(false);
        _locationAdvancedFreenet = new MenuItem(_locationAdvancedMenu, SWT.PUSH);
        _locationAdvancedFreenet.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { configFreenet(); }
            public void widgetSelected(SelectionEvent selectionEvent) { configFreenet(); }
        });
        
        // last sync row
        
        _lastSyncLabel = new Label(_root, SWT.NONE);
        _lastSyncLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        row = new Composite(_root, SWT.NONE);
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        row.setLayout(new GridLayout(3, false));
        
        _lastSync = new Label(row, SWT.NONE);
        _lastSync.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        
        _nextSyncLabel = new Label(row, SWT.NONE);
        _nextSyncLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _nextSync = new Label(row, SWT.NONE);
        _nextSync.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, true, false));
        
        // next action row
        
        // stub for the first column
        new Composite(_root, SWT.NONE).setLayoutData(new GridData(1, 1));
        
        row = new Composite(_root, SWT.NONE);
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        row.setLayout(new GridLayout(5, false));
        
        _nextSyncNow = new Button(row, SWT.PUSH);
        _nextSyncNow.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _nextSyncNever = new Button(row, SWT.PUSH);
        _nextSyncNever.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _nextSyncOneOff = new Button(row, SWT.PUSH);
        _nextSyncOneOff.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _nextSyncDelayLabel = new Label(row, SWT.NONE);
        _nextSyncDelayLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _nextSyncDelay = new Combo(row, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.BORDER);
        _nextSyncDelay.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        // proxy row
        
        _proxyHostLabel = new Label(_root, SWT.NONE);
        _proxyHostLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        row = new Composite(_root, SWT.NONE);
        row.setLayout(new GridLayout(3, false));
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _proxyHost = new Text(row, SWT.SINGLE | SWT.BORDER);
        _proxyHost.setLayoutData(new GridData(200, SWT.DEFAULT));
        
        _proxyPortLabel = new Label(row, SWT.NONE);
        _proxyPortLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _proxyPort = new Text(row, SWT.SINGLE | SWT.BORDER);
        _proxyPort.setLayoutData(new GridData(75, SWT.DEFAULT));
        
        // push policy row
        
        _pushPolicyLabel = new Label(_root, SWT.NONE);
        _pushPolicyLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        row = new Composite(_root, SWT.NONE);
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        row.setLayout(new GridLayout(3, false));
        
        _pushPolicy = new Combo(row, SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
        _pushPolicy.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _pushMaxSizeLabel = new Label(row, SWT.NONE);
        _pushMaxSizeLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _pushMaxSize = new Combo(row, SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
        _pushMaxSize.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        // pull policy row
        
        _pullPolicyLabel = new Label(_root, SWT.NONE);
        _pullPolicyLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        row = new Composite(_root, SWT.NONE);
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        row.setLayout(new GridLayout(3, false));
        
        _pullPolicy = new Combo(row, SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
        _pullPolicy.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _pullMaxSizeLabel = new Label(row, SWT.NONE);
        _pullMaxSizeLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _pullMaxSize = new Combo(row, SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
        _pullMaxSize.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        // pull new age row
        
        // stub for the first column
        new Composite(_root, SWT.NONE).setLayoutData(new GridData(1, 1));
        
        Composite newAge = new Composite(_root, SWT.NONE);
        newAge.setLayout(new GridLayout(2, false));
        newAge.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _pullNewAgeLabel = new Label(newAge, SWT.NONE);
        _pullNewAgeLabel.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        
        _pullNewAge = new Combo(newAge, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.BORDER);
        _pullNewAge.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        // push age row
        
        // stub for the first column
        new Composite(_root, SWT.NONE).setLayoutData(new GridData(1, 1));
        
        Composite pushAge = new Composite(_root, SWT.NONE);
        pushAge.setLayout(new GridLayout(2, false));
        pushAge.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _pushAgeLabel = new Label(pushAge, SWT.NONE);
        _pushAgeLabel.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        
        _pushAge = new Combo(pushAge, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.BORDER);
        _pushAge.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        // pull private row
        
        // stub for the first column
        new Composite(_root, SWT.NONE).setLayoutData(new GridData(1, 1));
        
        _pullPrivate = new Button(_root, SWT.CHECK);
        _pullPrivate.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        // pull private only row 
        
        // stub for the first column
        new Composite(_root, SWT.NONE).setLayoutData(new GridData(1, 1));
        
        _pullPrivateLocalOnly = new Button(_root, SWT.CHECK);
        _pullPrivateLocalOnly.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        // pull pbe row
        
        // stub for the first column
        new Composite(_root, SWT.NONE).setLayoutData(new GridData(1, 1));
        
        _pullPBE = new Button(_root, SWT.CHECK);
        _pullPBE.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        // failure row
        
        _failuresLabel = new Label(_root, SWT.NONE);
        _failuresLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        row = new Composite(_root, SWT.NONE);
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        row.setLayout(new GridLayout(2, false));
        
        _failures = new Label(row, SWT.NONE);
        _failures.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        
        _backOffOnFailures = new Button(row, SWT.CHECK);
        _backOffOnFailures.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        // action row
        
        Composite actions = new Composite(_root, SWT.NONE);
        actions.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, true, 2, 1));
        actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        
        _save = new Button(actions, SWT.PUSH);
        _save.setEnabled(false);
        _cancel = new Button(actions, SWT.PUSH);
        
        configActions();
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
        
        loadData();
    }
    
    private void configActions() {
        _nextSyncNever.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
            public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
            private void fire() {
                if (save(false)) {
                    _archive.setNextSyncTime(-1);
                    _archive.setNextSyncOneOff(false);
                    _archive.store(true);
                    _listener.scheduleUpdated();
                }
            }
        });
        _nextSyncNow.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
            public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
            private void fire() {
                if (save(false)) {
                    _archive.setNextSyncTime(System.currentTimeMillis());
                    _archive.setNextSyncOneOff(false);
                    _archive.store(true);
                    _listener.scheduleUpdated();
                }
            }
        });
        _nextSyncOneOff.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
            public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
            private void fire() {
                if (save(false)) {
                    _archive.setNextSyncTime(System.currentTimeMillis());
                    _archive.setNextSyncOneOff(true);
                    _archive.store(true);
                    _listener.scheduleUpdated();
                }
            }
        });
        
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { loadData(); _listener.cancelled(); }
            public void widgetSelected(SelectionEvent selectionEvent) { loadData(); _listener.cancelled(); }
        });
        _save.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { if (save()) _listener.cancelled(); }
            public void widgetSelected(SelectionEvent selectionEvent) { if (save()) _listener.cancelled(); }
        });
    }
    
    private void configFreenet() {
        final Shell dialog = new Shell(_root.getShell(), SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        dialog.setText(_browser.getTranslationRegistry().getText(T_FREENET_TITLE, "Freenet configuration"));
        dialog.setLayout(new GridLayout(1, true));
        
        Composite row = new Composite(dialog, SWT.NONE);
        row.setLayout(new GridLayout(2, false));
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        Label pubLabel = new Label(row, SWT.NONE);
        pubLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        pubLabel.setText(_browser.getTranslationRegistry().getText(T_FREENET_PUB, "Public key:"));
        final Text pub = new Text(row, SWT.SINGLE | SWT.BORDER);
        pub.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        String loc = _location.getText().trim();
        if (loc.startsWith("USK@") || loc.startsWith("SSK@") || loc.startsWith("KSK@") || loc.startsWith("CHK@"))
            pub.setText(loc);
        
        row = new Composite(dialog, SWT.NONE);
        row.setLayout(new FillLayout(SWT.HORIZONTAL));
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        final Button readOnly = new Button(row, SWT.RADIO);
        readOnly.setText(_browser.getTranslationRegistry().getText(T_FREENET_READONLY, "Read only"));
        final Button writeOnly = new Button(row, SWT.RADIO);
        writeOnly.setText(_browser.getTranslationRegistry().getText(T_FREENET_WRITEONLY, "Write only"));
        
        row = new Composite(dialog, SWT.NONE);
        row.setLayout(new GridLayout(4, false));
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        Label fcpHostLabel = new Label(row, SWT.NONE);
        fcpHostLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        fcpHostLabel.setText(_browser.getTranslationRegistry().getText(T_FREENET_FCPHOST, "FCP host:"));
        final Text fcpHost = new Text(row, SWT.SINGLE | SWT.BORDER);
        fcpHost.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        if ( (_archive.getFCPHost() != null) && (_archive.getFCPHost().trim().length() > 0) )
            fcpHost.setText(_archive.getFCPHost());
        else
            fcpHost.setText("127.0.0.1");
        Label fcpPortLabel = new Label(row, SWT.NONE);
        fcpPortLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        fcpPortLabel.setText(_browser.getTranslationRegistry().getText(T_FREENET_FCPPORT, "port:"));
        final Text fcpPort = new Text(row, SWT.SINGLE | SWT.BORDER);
        fcpPort.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        if (_archive.getFCPPort() > 0)
            fcpPort.setText(_archive.getFCPPort() + "");
        else
            fcpPort.setText(9481 + "");
        
        row = new Composite(dialog, SWT.NONE);
        row.setLayout(new GridLayout(3, false));
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        Label privLabel = new Label(row, SWT.NONE);
        privLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        privLabel.setText(_browser.getTranslationRegistry().getText(T_FREENET_PRIV, "Private key:"));
        final Text privKey = new Text(row, SWT.SINGLE | SWT.BORDER);
        privKey.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        if (_archive.getPostKey() != null)
            privKey.setText(_archive.getPostKey());
        else
            privKey.setText("");
        Button gen = new Button(row, SWT.PUSH);
        gen.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        gen.setText(_browser.getTranslationRegistry().getText(T_FREENET_NEWKEY, "Generate new"));
        gen.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { gen(); }
            public void widgetSelected(SelectionEvent selectionEvent) { gen(); }
            private void gen() {
                genKey(dialog, fcpHost.getText(), getInt(fcpPort.getText().trim()), pub, privKey);
                readOnly.setSelection(false);
                writeOnly.setSelection(true);
            }
        });
        privKey.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent modifyEvent) {
                readOnly.setSelection(false);
                writeOnly.setSelection(true);
            }
        });
        
        String priv = null;
        if (_archive.getPostKey() == null) {
            readOnly.setSelection(true);
            writeOnly.setSelection(false);
        } else {
            readOnly.setSelection(false);
            writeOnly.setSelection(true);
        }
        
        row = new Composite(dialog, SWT.NONE);
        row.setLayout(new FillLayout(SWT.HORIZONTAL));
        row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        Button save = new Button(row, SWT.PUSH);
        save.setText(_browser.getTranslationRegistry().getText(T_FREENET_SAVE, "Save"));
        save.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { saveFreenet(); }
            public void widgetSelected(SelectionEvent selectionEvent) { saveFreenet(); }
            private void saveFreenet() {
                if (writeOnly.getSelection() && (privKey.getText().trim().length() > 0))
                    _archive.setPostKey(privKey.getText().trim());
                else
                    _archive.setPostKey(null);
                _archive.setFCPHost(fcpHost.getText().trim());
                _archive.setFCPPort(getInt(fcpPort.getText().trim()));
                _archive.setURL(pub.getText().trim());
                dialog.dispose();
                //loadData();
            }
        });
        Button cancel = new Button(row, SWT.PUSH);
        cancel.setText(_browser.getTranslationRegistry().getText(T_FREENET_CANCEL, "Cancel"));
        cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { dialog.dispose(); }
            public void widgetSelected(SelectionEvent selectionEvent) { dialog.dispose(); }
        });
        
        dialog.pack();
        dialog.setSize(dialog.computeSize(400, SWT.DEFAULT));
        dialog.open();
    }
    private static final String T_FREENET_TITLE = "syndie.gui.syndicatordetailhttparchive.freenet.title";
    private static final String T_FREENET_PUB = "syndie.gui.syndicatordetailhttparchive.freenet.pub";
    private static final String T_FREENET_READONLY = "syndie.gui.syndicatordetailhttparchive.freenet.readonly";
    private static final String T_FREENET_WRITEONLY = "syndie.gui.syndicatordetailhttparchive.freenet.writeonly";
    private static final String T_FREENET_FCPHOST = "syndie.gui.syndicatordetailhttparchive.freenet.fcphost";
    private static final String T_FREENET_FCPPORT = "syndie.gui.syndicatordetailhttparchive.freenet.fcpport";
    private static final String T_FREENET_PRIV = "syndie.gui.syndicatordetailhttparchive.freenet.priv";
    private static final String T_FREENET_NEWKEY = "syndie.gui.syndicatordetailhttparchive.freenet.newkey";
    private static final String T_FREENET_SAVE = "syndie.gui.syndicatordetailhttparchive.freenet.save";
    private static final String T_FREENET_CANCEL = "syndie.gui.syndicatordetailhttparchive.freenet.cancel";
    
    private int getInt(String str) {
        if (str == null) return -1;
        try { return Integer.parseInt(str); } catch (NumberFormatException nfe) { return -1; }
    }
    private void genKey(Shell parent, String fcpHost, int fcpPort, Text targetPub, Text targetPriv) {
        System.out.println("genKey called");
        try {
            Socket s = new Socket(fcpHost, fcpPort);
            OutputStream out = s.getOutputStream();
            long msgId = System.currentTimeMillis();
            out.write(DataHelper.getUTF8("ClientHello\r\n" +
            "Name=syndie" + msgId + "\r\n" +
            "ExpectedVersion=2.0\r\n" +
            "Identifier=" + msgId + "\r\n" +
            "EndMessage\r\n" +
            "GenerateSSK\r\n" +
            "Identifier=" + (msgId+1) + "\r\n" +
            "EndMessage\r\n"));
            Map rv = SyncOutboundPusher.readResults(s.getInputStream(), _browser.getUI());
            if (rv == null) {
                MessageBox box = new MessageBox(parent, SWT.ICON_ERROR | SWT.OK);
                box.setMessage(_browser.getTranslationRegistry().getText(T_FREENET_ERROR, "Error communicating with the Freenet server"));
                box.setText(_browser.getTranslationRegistry().getText(T_FREENET_ERROR_TITLE, "Error"));
                box.open();
                //_ui.commandComplete(-1, null);
            } else {
                
                // from http://wiki.freenetproject.org/FCP2p0SSKKeypair the response will
                // have values like
                //   InsertURI=freenet:SSK@AKTTKG6YwjrHzWo67laRcoPqibyiTdyYufjVg54fBlWr,AwUSJG5ZS-FDZTqnt6skTzhxQe08T-fbKXj8aEHZsXM/
                //   RequestURI=freenet:SSK@BnHXXv3Fa43w~~iz1tNUd~cj4OpUuDjVouOWZ5XlpX0,AwUSJG5ZS-FDZTqnt6skTzhxQe08T-fbKXj8aEHZsXM,AQABAAE/
                
                targetPub.setText(getRequestURI((String)rv.get("RequestURI")));
                targetPriv.setText(getInsertURI((String)rv.get("InsertURI")));
            }
            s.close();
        } catch (Exception e) {
            //_error = "Error generating a new Freenet keypair";
            _browser.getUI().debugMessage("Error generating a new Freenet keypair", e);
            MessageBox box = new MessageBox(parent, SWT.ICON_ERROR | SWT.OK);
            box.setMessage(_browser.getTranslationRegistry().getText(T_FREENET_ERROR, "Error communicating with the Freenet server") + ": " + e.getMessage());
            box.setText(_browser.getTranslationRegistry().getText(T_FREENET_ERROR_TITLE, "Error"));
            box.open();
            //_ui.commandComplete(-1, null);
        }
    }
    private static final String T_FREENET_ERROR = "syndie.gui.syndicatordetailhttparchive.freenet.error";
    private static final String T_FREENET_ERROR_TITLE = "syndie.gui.syndicatordetailhttparchive.freenet.error.title";
    
    /** 
     * turn the SSK public key retrieved from fred and make it a USK w/ version -1 
     * RequestURI=freenet:SSK@BnHXXv3Fa43w~~iz1tNUd~cj4OpUuDjVouOWZ5XlpX0,AwUSJG5ZS-FDZTqnt6skTzhxQe08T-fbKXj8aEHZsXM,AQABAAE/
     */
    private String getRequestURI(String publicSSK) {
        int index = publicSSK.indexOf("SSK@");
        String key = publicSSK.substring(index+4);
        while (key.endsWith("/"))
            key = key.substring(0, key.length()-1);
        return "USK@" + key + "/archive/-1/";
    }
    
    /** 
     * turn the SSK public key retrieved from fred and make it a USK w/ version 0
     * InsertURI=freenet:SSK@AKTTKG6YwjrHzWo67laRcoPqibyiTdyYufjVg54fBlWr,AwUSJG5ZS-FDZTqnt6skTzhxQe08T-fbKXj8aEHZsXM/
     */
    private String getInsertURI(String privateSSK) {
        int index = privateSSK.indexOf("SSK@");
        String key = privateSSK.substring(index+4);
        while (key.endsWith("/"))
            key = key.substring(0, key.length()-1);
        return "USK@" + key + "/archive/0/";
    }
    
    private boolean save() { return save(true); }
    private boolean save(boolean store) {
        String name = _name.getText();
        if (name.trim().length() <= 0) return false;
        _archive.setName(name);
        String host = _proxyHost.getText().trim();
        int port = -1;
        if (host.length() > 0) {
            String str = _proxyPort.getText().trim();
            try {
                port = Integer.parseInt(str);
            } catch (NumberFormatException nfe) {
                host = null;
            }
        } else {
            host = null;
        }
        _archive.setHTTPProxyHost(host);
        _archive.setHTTPProxyPort(port);
        
        SharedArchiveEngine.PullStrategy pullStrategy = createPullStrategy();
        _archive.setPullStrategy(pullStrategy);
        
        SharedArchiveEngine.PushStrategy pushStrategy = createPushStrategy();
        _archive.setPushStrategy(pushStrategy);
        
        _archive.setURL(_location.getText().trim());
        
        if (store)
            _archive.store(true);
        return true;
    }
    
    private SharedArchiveEngine.PullStrategy createPullStrategy() {
        SharedArchiveEngine.PullStrategy pullStrategy = new SharedArchiveEngine.PullStrategy();
        
        pullStrategy.discoverArchives = true;
        pullStrategy.includePBEMessages = _pullPBE.getSelection();
        pullStrategy.includePrivateMessages = _pullPrivate.getSelection();
        pullStrategy.maxKBPerMessage = SIZES[_pullMaxSize.getSelectionIndex()];
        pullStrategy.newAgeDays = NEWAGEDAYS[_pullNewAge.getSelectionIndex()];
        
        switch (_pullPolicy.getSelectionIndex()) {
            case PULL_POLICY_ALL_DELTA: 
                pullStrategy.knownChannelsOnly = false;
                pullStrategy.includeRecentMessagesOnly = false;
                pullStrategy.pullNothing = false;
                pullStrategy.includeDupForPIR = false;
                break;
            case PULL_POLICY_ALL_KNOWN:
                pullStrategy.knownChannelsOnly = true;
                pullStrategy.includeRecentMessagesOnly = false;
                pullStrategy.pullNothing = false;
                pullStrategy.includeDupForPIR = false;
                break;
            case PULL_POLICY_NOTHING:
                pullStrategy.knownChannelsOnly = false;
                pullStrategy.includeRecentMessagesOnly = false;
                pullStrategy.pullNothing = true;
                pullStrategy.includeDupForPIR = false;
                break;
            case PULL_POLICY_PIR: 
                pullStrategy.knownChannelsOnly = false;
                pullStrategy.includeRecentMessagesOnly = false;
                pullStrategy.pullNothing = false;
                pullStrategy.includeDupForPIR = true;
                break;
            case PULL_POLICY_RECENT_DELTA:
                pullStrategy.knownChannelsOnly = false;
                pullStrategy.includeRecentMessagesOnly = true;
                pullStrategy.pullNothing = false;
                pullStrategy.includeDupForPIR = false;
                break;
            case PULL_POLICY_RECENT_KNOWN:
                pullStrategy.knownChannelsOnly = true;
                pullStrategy.includeRecentMessagesOnly = true;
                pullStrategy.pullNothing = false;
                pullStrategy.includeDupForPIR = false;
                break;
        }
        return pullStrategy;
    }
    
    private SharedArchiveEngine.PushStrategy createPushStrategy() {
        SharedArchiveEngine.PushStrategy pushStrategy = new SharedArchiveEngine.PushStrategy();
        
        pushStrategy.maxKBPerMessage = SIZES[_pushMaxSize.getSelectionIndex()];
        pushStrategy.sendMaxAge = SENDAGEDAYS[_pushAge.getSelectionIndex()];
        
        switch (_pushPolicy.getSelectionIndex()) {
            case PUSH_POLICY_ALL_DELTA: 
                pushStrategy.sendLocalNewOnly = false;
                pushStrategy.sendNothing = false;
                break;
            case PUSH_POLICY_LOCAL_DELTA:
                pushStrategy.sendLocalNewOnly = true;
                pushStrategy.sendNothing = false;
                break;
            case PUSH_POLICY_NOTHING:
                pushStrategy.sendLocalNewOnly = false;
                pushStrategy.sendNothing = true;
                break;
        }
        return pushStrategy;
    }
    
    private static final String str(String str) { return str != null ? str : ""; }
    
    private SharedArchiveEngine.PushStrategy getPushStrategy() {
        SharedArchiveEngine.PushStrategy rv = _archive.getPushStrategy();
        if (rv == null)
            rv = SyncManager.getInstance(_browser.getClient(), _browser.getUI()).getDefaultPushStrategy();
        return rv;
    }
    
    private SharedArchiveEngine.PullStrategy getPullStrategy() {
        SharedArchiveEngine.PullStrategy rv = _archive.getPullStrategy();
        if (rv == null)
            rv = SyncManager.getInstance(_browser.getClient(), _browser.getUI()).getDefaultPullStrategy();
        return rv;
    }
    
    private void loadData() {
        if (_root.isDisposed()) return;
        _name.setText(str(_archive.getName()));
        _location.setText(str(_archive.getURL()));
        _proxyHost.setText(str(_archive.getHTTPProxyHost()));
        _proxyPort.setText(_archive.getHTTPProxyPort() > 0 ? _archive.getHTTPProxyPort()+"" : "");
        
        SharedArchiveEngine.PushStrategy push = getPushStrategy();
        if (push.sendLocalNewOnly)
            _pushPolicy.select(PUSH_POLICY_LOCAL_DELTA);
        else if (push.sendNothing)
            _pushPolicy.select(PUSH_POLICY_NOTHING);
        else
            _pushPolicy.select(PUSH_POLICY_ALL_DELTA);
        
        for (int i = 0; i < SIZES.length; i++) {
            if (SIZES[i] >= push.maxKBPerMessage) {
                _pushMaxSize.select(i);
                break;
            }
        }
        
        SharedArchiveEngine.PullStrategy pull = getPullStrategy();
        if (pull.includeDupForPIR) {
            _pullPolicy.select(PULL_POLICY_PIR);
        } else if (pull.pullNothing) {
            _pullPolicy.select(PULL_POLICY_NOTHING);
        } else if (pull.includeRecentMessagesOnly) {
            if (pull.knownChannelsOnly)
                _pullPolicy.select(PULL_POLICY_RECENT_KNOWN);
            else
                _pullPolicy.select(PULL_POLICY_RECENT_DELTA);
        } else {
            if (pull.knownChannelsOnly)
                _pullPolicy.select(PULL_POLICY_ALL_KNOWN);
            else
                _pullPolicy.select(PULL_POLICY_ALL_DELTA);
        }
        
        for (int i = 0; i < SIZES.length; i++) {
            if (SIZES[i] >= pull.maxKBPerMessage) {
                _pullMaxSize.select(i);
                break;
            }
        }
        
        _pullPrivate.setSelection(pull.includePrivateMessages);
        _pullPrivateLocalOnly.setEnabled(false);
        _pullPBE.setSelection(pull.includePBEMessages);
        
        if (pull.newAgeDays <= 0)
            _pullNewAge.select(0);
        else if (pull.newAgeDays <= 7)
            _pullNewAge.select(1);
        else if (pull.newAgeDays <= 14)
            _pullNewAge.select(2);
        else if (pull.newAgeDays <= 32)
            _pullNewAge.select(3);
        else
            _pullNewAge.select(4);
        
        if (push.sendMaxAge <= 0)
            _pushAge.select(6);
        else if (push.sendMaxAge <= 7)
            _pushAge.select(0);
        else if (push.sendMaxAge <= 14)
            _pushAge.select(1);
        else if (push.sendMaxAge <= 31)
            _pushAge.select(2);
        else if (push.sendMaxAge <= 64)
            _pushAge.select(3);
        else if (push.sendMaxAge <= 186)
            _pushAge.select(4);
        else // 1y
            _pushAge.select(5);
        
        long last = _archive.getLastSyncTime();
        if (last > 0)
            _lastSync.setText(Constants.getDateTime(last));
        else
            _lastSync.setText(_browser.getTranslationRegistry().getText(T_NEVER, "Never"));
        
        long nxt = _archive.getNextSyncTime();
        if (nxt > 0) {
            if (nxt <= System.currentTimeMillis())
                _nextSync.setText(_browser.getTranslationRegistry().getText(T_ASAP, "ASAP"));
            else
                _nextSync.setText(Constants.getDateTime(nxt));
        } else {
            _nextSync.setText(_browser.getTranslationRegistry().getText(T_NEVER, "Never"));
        }
    
        if (_name.getText().trim().length() > 0)
            _save.setEnabled(true);
        _nextSyncDelay.setEnabled(false);
        _failures.setText(_archive.getConsecutiveFailures() + "");
        _backOffOnFailures.setEnabled(false);
    }
    
    private static final String T_NEVER = "syndie.gui.syndicatordetailhttparchive.never";
    private static final String T_ASAP = "syndie.gui.syndicatordetailhttparchive.asap";

    // callbacks from the archive engine, may occur on arbitrary threads
    public void incomingUpdated(SyncArchive.IncomingAction action) {}
    public void incomingUpdated(List actions) {}
    public void outgoingUpdated(SyncArchive.OutgoingAction action) {}
    public void archiveUpdated(SyncArchive archive) { 
        Display.getDefault().asyncExec(new Runnable() { 
            public void run() { 
                loadData(); 
                if (!_root.isDisposed())
                    _root.layout(true, true); 
            } 
        });
    }
    
    public void applyTheme(Theme theme) {    
        if (_root.isDisposed()) return;
        _nameLabel.setFont(theme.DEFAULT_FONT);
        _name.setFont(theme.DEFAULT_FONT);
        _locationLabel.setFont(theme.DEFAULT_FONT);
        _location.setFont(theme.DEFAULT_FONT);
        _proxyHostLabel.setFont(theme.DEFAULT_FONT);
        _proxyHost.setFont(theme.DEFAULT_FONT);
        _proxyPortLabel.setFont(theme.DEFAULT_FONT);
        _proxyPort.setFont(theme.DEFAULT_FONT);
        _pushPolicyLabel.setFont(theme.DEFAULT_FONT);
        _pushPolicy.setFont(theme.DEFAULT_FONT);
        _pushMaxSizeLabel.setFont(theme.DEFAULT_FONT);
        _pushMaxSize.setFont(theme.DEFAULT_FONT);
        _pullPolicyLabel.setFont(theme.DEFAULT_FONT);
        _pullPolicy.setFont(theme.DEFAULT_FONT);
        _pullMaxSizeLabel.setFont(theme.DEFAULT_FONT);
        _pullMaxSize.setFont(theme.DEFAULT_FONT);
        _lastSyncLabel.setFont(theme.DEFAULT_FONT);
        _lastSync.setFont(theme.DEFAULT_FONT);
        _nextSyncLabel.setFont(theme.DEFAULT_FONT);
        _nextSync.setFont(theme.DEFAULT_FONT);
        _pullPrivate.setFont(theme.DEFAULT_FONT);
        _pullPrivateLocalOnly.setFont(theme.DEFAULT_FONT);
        _pullPBE.setFont(theme.DEFAULT_FONT);
        _nextSyncDelayLabel.setFont(theme.DEFAULT_FONT);
        _nextSyncDelay.setFont(theme.DEFAULT_FONT);
        _failuresLabel.setFont(theme.DEFAULT_FONT);
        _failures.setFont(theme.DEFAULT_FONT);
        _backOffOnFailures.setFont(theme.DEFAULT_FONT);
        _pullNewAge.setFont(theme.DEFAULT_FONT);
        _pullNewAgeLabel.setFont(theme.DEFAULT_FONT);
        _pushAge.setFont(theme.DEFAULT_FONT);
        _pushAgeLabel.setFont(theme.DEFAULT_FONT);
    
        _locationAdvanced.setFont(theme.BUTTON_FONT);
        _nextSyncNow.setFont(theme.BUTTON_FONT);
        _nextSyncNever.setFont(theme.BUTTON_FONT);
        _nextSyncOneOff.setFont(theme.BUTTON_FONT);
        _save.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
    }
    
    private static final String T_NAME = "syndie.gui.syndicatordetailhttparchive.name";
    private static final String T_LOCATION = "syndie.gui.syndicatordetailhttparchive.location";
    private static final String T_LOCATIONADVANCED = "syndie.gui.syndicatordetailhttparchive.locationadvanced";
    private static final String T_LOCATIONADVANCED_SEND = "syndie.gui.syndicatordetailhttparchive.locationadvanced.send";
    private static final String T_LOCATIONADVANCED_READ = "syndie.gui.syndicatordetailhttparchive.locationadvanced.read";
    private static final String T_LOCATIONADVANCED_FREENET = "syndie.gui.syndicatordetailhttparchive.locationadvanced.freenet";
    private static final String T_PROXYHOST = "syndie.gui.syndicatordetailhttparchive.proxyhost";
    private static final String T_PROXYPORT = "syndie.gui.syndicatordetailhttparchive.proxyport";
    private static final String T_PUSHPOLICY = "syndie.gui.syndicatordetailhttparchive.pushpolicy";
    private static final String T_PUSHMAXSIZE = "syndie.gui.syndicatordetailhttparchive.pushmaxsize";
    private static final String T_PULLPOLICY = "syndie.gui.syndicatordetailhttparchive.pullpolicy";
    private static final String T_PULLMAXSIZE = "syndie.gui.syndicatordetailhttparchive.pullmaxsize";
    private static final String T_PULLPRIVATE = "syndie.gui.syndicatordetailhttparchive.pullprivate";
    private static final String T_PULLPRIVATELOCALONLY = "syndie.gui.syndicatordetailhttparchive.pullprivateonly";
    private static final String T_PBE = "syndie.gui.syndicatordetailhttparchive.pbe";
    private static final String T_LASTSYNC = "syndie.gui.syndicatordetailhttparchive.lastsync";
    private static final String T_NEXTSYNC = "syndie.gui.syndicatordetailhttparchive.nextsync";
    private static final String T_SYNCNOW = "syndie.gui.syndicatordetailhttparchive.syncnow";
    private static final String T_SYNCNEVER = "syndie.gui.syndicatordetailhttparchive.syncnever";
    private static final String T_SYNCONEOFF = "syndie.gui.syndicatordetailhttparchive.synconeoff";
    private static final String T_SYNCDELAY = "syndie.gui.syndicatordetailhttparchive.syncdelay";
    private static final String T_FAILURES = "syndie.gui.syndicatordetailhttparchive.failures";
    private static final String T_BACKOFF = "syndie.gui.syndicatordetailhttparchive.backoff";
    private static final String T_PUSHPOLICY_ALLDELTA = "syndie.gui.syndicatordetailhttparchive.pushpolicy.alldelta";
    private static final String T_PUSHPOLICY_LOCALDELTA = "syndie.gui.syndicatordetailhttparchive.pushpolicy.localdelta";
    private static final String T_PUSHPOLICY_NOTHING = "syndie.gui.syndicatordetailhttparchive.pushpolicy.nothing";
    private static final String T_SIZE_SUFFIX = "syndie.gui.syndicatordetailhttparchive.sizesuffix";
    private static final String T_PULLPOLICY_RECENTDELTA = "syndie.gui.syndicatordetailhttparchive.pullpolicy.recentdelta";
    private static final String T_PULLPOLICY_ALLDELTA = "syndie.gui.syndicatordetailhttparchive.pullpolicy.alldelta";
    private static final String T_PULLPOLICY_RECENTKNOWN = "syndie.gui.syndicatordetailhttparchive.pullpolicy.recentknown";
    private static final String T_PULLPOLICY_ALLKNOWN = "syndie.gui.syndicatordetailhttparchive.pullpolicy.allknown";
    private static final String T_PULLPOLICY_PIR = "syndie.gui.syndicatordetailhttparchive.pullpolicy.pir";
    private static final String T_PULLPOLICY_NOTHING = "syndie.gui.syndicatordetailhttparchive.pullpolicy.nothing";
    private static final String T_SYNC_DELAY_SUFFIX = "syndie.gui.syndicatordetailhttparchive.syncdelaysuffix";
    private static final String T_SAVE = "syndie.gui.syndicatordetailhttparchive.save";
    private static final String T_CANCEL = "syndie.gui.syndicatordetailhttparchive.cancel";
    private static final String T_PULLNEWAGE = "syndie.gui.syndicatordetailhttparchive.pullnewage";
    private static final String T_PUSHAGE = "syndie.gui.syndicatordetailhttparchive.pushage";
    
    public void translate(TranslationRegistry registry) {
        _nameLabel.setText(registry.getText(T_NAME, "Name:"));
        _locationLabel.setText(registry.getText(T_LOCATION, "Archive URL:"));
        _locationAdvanced.setText(registry.getText(T_LOCATIONADVANCED, "Advanced..."));
        _locationAdvancedSendKey.setText(registry.getText(T_LOCATIONADVANCED_SEND, "Specify posting key"));
        _locationAdvancedReadKey.setText(registry.getText(T_LOCATIONADVANCED_READ, "Specify reading key"));
        _locationAdvancedFreenet.setText(registry.getText(T_LOCATIONADVANCED_FREENET, "Configure freenet settings"));
        _proxyHostLabel.setText(registry.getText(T_PROXYHOST, "Proxy host:"));
        _proxyPortLabel.setText(registry.getText(T_PROXYPORT, "Port:"));
        _pushPolicyLabel.setText(registry.getText(T_PUSHPOLICY, "Push policy:"));
        _pushMaxSizeLabel.setText(registry.getText(T_PUSHMAXSIZE, "Max message size:"));
        _pullPolicyLabel.setText(registry.getText(T_PULLPOLICY, "Pull policy:"));
        _pullMaxSizeLabel.setText(registry.getText(T_PULLMAXSIZE, "Max message size:"));
        _pullPrivate.setText(registry.getText(T_PULLPRIVATE, "Pull any private messages?"));
        _pullPrivateLocalOnly.setText(registry.getText(T_PULLPRIVATELOCALONLY, "Pull private messages for us only?"));
        _pullPBE.setText(registry.getText(T_PBE, "Pull passphrase protected messages?"));
        _lastSyncLabel.setText(registry.getText(T_LASTSYNC, "Last sync:"));
        _nextSyncLabel.setText(registry.getText(T_NEXTSYNC, "Next sync:"));
        _nextSyncNow.setText(registry.getText(T_SYNCNOW, "Sync ASAP"));
        _nextSyncNever.setText(registry.getText(T_SYNCNEVER, "Never sync"));
        _nextSyncOneOff.setText(registry.getText(T_SYNCONEOFF, "Sync only once"));
        _nextSyncDelayLabel.setText(registry.getText(T_SYNCDELAY, "Min sync delay:"));
        _failuresLabel.setText(registry.getText(T_FAILURES, "Sync failures:"));
        _backOffOnFailures.setText(registry.getText(T_BACKOFF, "Back off after failing?"));
        _save.setText(registry.getText(T_SAVE, "Save"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel"));

        _pullNewAgeLabel.setText(registry.getText(T_PULLNEWAGE, "Age to treat as 'recent'"));
        _pushAgeLabel.setText(registry.getText(T_PUSHAGE, "Oldest message to send"));
        
        translateCombos(registry);
    }
    
    private static final int PUSH_POLICY_ALL_DELTA = 0;
    private static final int PUSH_POLICY_LOCAL_DELTA = 1;
    private static final int PUSH_POLICY_NOTHING = 2;
    private static final int PUSH_POLICY_DEFAULT = PUSH_POLICY_ALL_DELTA;
    
    private static final int PULL_POLICY_RECENT_DELTA = 0;
    private static final int PULL_POLICY_ALL_DELTA = 1;
    private static final int PULL_POLICY_RECENT_KNOWN = 2;
    private static final int PULL_POLICY_ALL_KNOWN = 3;
    private static final int PULL_POLICY_PIR = 4;
    private static final int PULL_POLICY_NOTHING = 5;
    private static final int PULL_POLICY_DEFAULT = 0;
    
    private static final int[] SIZES = new int[] { 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096 };
    private static final int SIZE_DEFAULT_INDEX = 7; // 512KB
    
    private static final int[] SYNC_DELAY = new int[] { 1, 2, 4, 6, 12, 18, 24 };
    private static final int SYNC_DELAY_DEFAULT_INDEX = 0;
    
    private static final String T_NEWAGE_DEFAULT = "syndie.gui.syndicatordetailhttparchive.newage.default";
    private static final String T_NEWAGE_1W = "syndie.gui.syndicatordetailhttparchive.newage.1w";
    private static final String T_NEWAGE_2W = "syndie.gui.syndicatordetailhttparchive.newage.2w";
    private static final String T_NEWAGE_1M = "syndie.gui.syndicatordetailhttparchive.newage.1m";
    private static final String T_NEWAGE_6M = "syndie.gui.syndicatordetailhttparchive.newage.6m";
    private static final int NEWAGE_DEFAULT_INDEX = 0;
    private static final int[] NEWAGEDAYS = new int[] { -1, 7, 14, 31, 183 };
    
    private static final String T_SENDAGE_1W = "syndie.gui.syndicatordetailhttparchive.sendage.1w";
    private static final String T_SENDAGE_2W = "syndie.gui.syndicatordetailhttparchive.sendage.2w";
    private static final String T_SENDAGE_1M = "syndie.gui.syndicatordetailhttparchive.sendage.1m";
    private static final String T_SENDAGE_2M = "syndie.gui.syndicatordetailhttparchive.sendage.2m";
    private static final String T_SENDAGE_6M = "syndie.gui.syndicatordetailhttparchive.sendage.6m";
    private static final String T_SENDAGE_1Y = "syndie.gui.syndicatordetailhttparchive.sendage.1y";
    private static final String T_SENDAGE_INF = "syndie.gui.syndicatordetailhttparchive.sendage.inf";
    private static final int SENDAGE_DEFAULT_INDEX = 0;
    private static final int[] SENDAGEDAYS = new int[] { 7, 31, 62, 183, 365, -1 };
        
    private void translateCombos(TranslationRegistry registry) {
        int cnt = _pushPolicy.getItemCount();
        int sel = (cnt > 0 ? _pushPolicy.getSelectionIndex() : PUSH_POLICY_DEFAULT);
        _pushPolicy.removeAll();
        _pushPolicy.add(registry.getText(T_PUSHPOLICY_ALLDELTA, "Send all messages they don't have"));
        _pushPolicy.add(registry.getText(T_PUSHPOLICY_LOCALDELTA, "Send locally created messages they don't have"));
        _pushPolicy.add(registry.getText(T_PUSHPOLICY_NOTHING, "Send nothing"));
        _pushPolicy.select(sel);
        
        cnt = _pushMaxSize.getItemCount();
        sel = (cnt > 0 ? _pushMaxSize.getSelectionIndex() : SIZE_DEFAULT_INDEX);
        _pushMaxSize.removeAll();
        for (int i = 0; i < SIZES.length; i++)
            _pushMaxSize.add(SIZES[i] + registry.getText(T_SIZE_SUFFIX, " KBytes"));
        _pushMaxSize.select(sel);
        
        cnt = _pullPolicy.getItemCount();
        sel = (cnt > 0 ? _pullPolicy.getSelectionIndex() : PULL_POLICY_DEFAULT);
        _pullPolicy.removeAll();
        _pullPolicy.add(registry.getText(T_PULLPOLICY_RECENTDELTA, "Recent messages we don't have"));
        _pullPolicy.add(registry.getText(T_PULLPOLICY_ALLDELTA, "All messages we don't have"));
        _pullPolicy.add(registry.getText(T_PULLPOLICY_RECENTKNOWN, "Recent messages in forums we know"));
        _pullPolicy.add(registry.getText(T_PULLPOLICY_ALLKNOWN, "All messages in forums we know"));
        _pullPolicy.add(registry.getText(T_PULLPOLICY_PIR, "Everything the archive considers recent (PIR)"));
        _pullPolicy.add(registry.getText(T_PULLPOLICY_NOTHING, "Nothing"));
        _pullPolicy.select(sel);
        
        cnt = _pullMaxSize.getItemCount();
        sel = (cnt > 0 ? _pullMaxSize.getSelectionIndex() : SIZE_DEFAULT_INDEX);
        _pullMaxSize.removeAll();
        for (int i = 0; i < SIZES.length; i++)
            _pullMaxSize.add(SIZES[i] + registry.getText(T_SIZE_SUFFIX, " KBytes"));
        _pullMaxSize.select(sel);

        cnt = _pullNewAge.getItemCount();
        sel = (cnt > 0 ? _pullNewAge.getSelectionIndex() : NEWAGE_DEFAULT_INDEX);
        _pullNewAge.removeAll();
        _pullNewAge.add(registry.getText(T_NEWAGE_DEFAULT, "Whatever the archive advertises as new (default)"));
        _pullNewAge.add(registry.getText(T_NEWAGE_1W, "1 week"));
        _pullNewAge.add(registry.getText(T_NEWAGE_1M, "1 month"));
        _pullNewAge.add(registry.getText(T_NEWAGE_6M, "6 months"));
        _pullNewAge.select(sel);

        cnt = _pushAge.getItemCount();
        sel = (cnt > 0 ? _pushAge.getSelectionIndex() : SENDAGE_DEFAULT_INDEX);
        _pushAge.removeAll();
        _pushAge.add(registry.getText(T_SENDAGE_1W, "1 week"));
        _pushAge.add(registry.getText(T_SENDAGE_2W, "2 weeks"));
        _pushAge.add(registry.getText(T_SENDAGE_1M, "1 month"));
        _pushAge.add(registry.getText(T_SENDAGE_2M, "2 months"));
        _pushAge.add(registry.getText(T_SENDAGE_6M, "6 months"));
        _pushAge.add(registry.getText(T_SENDAGE_1Y, "1 year"));
        _pushAge.add(registry.getText(T_SENDAGE_INF, "Unlimited (only use if your clock is off)"));
        _pushAge.select(sel);
        
        cnt = _nextSyncDelay.getItemCount();
        sel = (cnt > 0 ? _nextSyncDelay.getSelectionIndex() : SYNC_DELAY_DEFAULT_INDEX);
        _nextSyncDelay.removeAll();
        for (int i = 0; i < SYNC_DELAY.length; i++)
            _nextSyncDelay.add(SYNC_DELAY[i] + registry.getText(T_SYNC_DELAY_SUFFIX, " hour(s)"));
        _nextSyncDelay.select(sel);
    }
}
