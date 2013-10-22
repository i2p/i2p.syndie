package syndie.gui.desktop;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import net.i2p.util.SimpleTimer2;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import syndie.Constants;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.HTTPServ;
import syndie.db.Importer;
import syndie.db.ImportResult;
import syndie.db.JobRunner;
import syndie.db.TextEngine;
import syndie.db.UI;
import syndie.gui.*;

/**
 *
 */
class ControlMenuPanel extends DesktopPanel implements Themeable, Translatable {
    private Label _importLabel;
    private Button _importFile;
    private Button _importBulk;
    private Text _importLocation;
    private Label _importStatus;
    private Button _importBrowse;
    private Button _import;
    
    private Label _openLabel;
    private Text _openLocation;
    private Button _open;
    
    private Label _httpservLabel;
    private Button _httpservStatus;
    private Button _httpservOnStart;
    private Button _httpservOptions;
    
    //private Button _tabbedUI;
    private Button _changePass;
    
    private Label _switchLabel;
    private Text _switchDir;
    private Button _switchBrowse;
    private Button _switchOpen;
    
    //private Button _expiration;
    //private Button _cancel;
    private Button _archiveMgr;
    private Button _welcome;
    private Button _sql;
    
    private Button _exit;
    
    public ControlMenuPanel(Desktop desktop, DBClient client, ThemeRegistry themes, TranslationRegistry trans, Composite parent, UI ui) {
        super(desktop, client, themes, trans, parent, ui, null);
        initComponents();
    }
    
    public String getPanelName() { return "Control menu"; }
    public String getPanelDescription() { return "Main control menu"; }

    protected void dispose() {
        // we never really dispose this panel, we just stop showing it for a while
        
        //_translationRegistry.unregister(this);
        //_themeRegistry.unregister(this);
        //super.dispose();
    }
    
    private void initComponents() {
        Composite root = getRoot();
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        root.setLayout(gl);
        
        Composite row = new Composite(root, SWT.NONE);
        row.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false));
        gl = new GridLayout(7, false);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        row.setLayout(gl);
        
        _importLabel = new Label(row, SWT.NONE);
        _importLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
    
        _importFile = new Button(row, SWT.RADIO);
        _importFile.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _importBulk = new Button(row, SWT.RADIO);
        _importBulk.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _importLocation = new Text(row, SWT.SINGLE | SWT.BORDER);
        _importLocation.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _importLocation.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN)
                    fireImport();
            }
        });
        
        _importFile.setSelection(true);
        _importBulk.setSelection(false);
        
        _importStatus = new Label(row, SWT.NONE);
        _importStatus.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _importBrowse = new Button(row, SWT.PUSH);
        _importBrowse.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _importBrowse.addSelectionListener(new FireSelectionListener() { public void fire() { fireImportBrowse(); } });
        
        _import = new Button(row, SWT.PUSH);
        _import.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _import.addSelectionListener(new FireSelectionListener() { public void fire() { fireImport(); } });
        
        row = new Composite(root, SWT.NONE);
        row.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false));
        gl = new GridLayout(3, false);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        row.setLayout(gl);
        
        _openLabel = new Label(row, SWT.NONE);
        _openLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _openLocation = new Text(row, SWT.SINGLE | SWT.BORDER);
        _openLocation.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _openLocation.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN)
                    fireOpen();
            }
        });
        _open = new Button(row, SWT.PUSH);
        _open.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _open.addSelectionListener(new FireSelectionListener() { public void fire() { fireOpen(); } });
        
        row = new Composite(root, SWT.NONE);
        row.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false));
        gl = new GridLayout(4, false);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        row.setLayout(gl);
        
        _httpservLabel = new Label(row, SWT.NONE);
        _httpservLabel.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        _httpservStatus = new Button(row, SWT.PUSH);
        _httpservStatus.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _httpservStatus.addSelectionListener(new FireSelectionListener() {
            public void fire() { toggleHttpservStatus(); }
        });
        _httpservOnStart = new Button(row, SWT.CHECK);
        _httpservOnStart.setLayoutData(new GridData(GridData.BEGINNING, GridData.FILL, false, false));
        _httpservOnStart.addSelectionListener(new FireSelectionListener() {
            public void fire() { httpservOnStartChanged(); }
        });
        _httpservOptions = new Button(row, SWT.PUSH);
        _httpservOptions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _httpservOptions.addSelectionListener(new FireSelectionListener() {
            public void fire() { showHttpservOptions(); }
        });
        
        String onStartup = _client.getNymPrefs().getProperty("httpserv.runOnStartup");
        if ( (onStartup != null) && ("true".equalsIgnoreCase(onStartup)) )
            _httpservOnStart.setSelection(true);

        _changePass = new Button(root, SWT.PUSH);
        _changePass.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _changePass.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                changePass();
            }
        });
        
        row = new Composite(root, SWT.NONE);
        row.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false));
        gl = new GridLayout(4, false);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        row.setLayout(gl);
        
        _switchLabel = new Label(row, SWT.NONE);
        _switchLabel.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        _switchDir = new Text(row, SWT.SINGLE | SWT.BORDER);
        _switchDir.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _switchDir.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN)
                    switchRootDir();
            }
        });
        _switchBrowse = new Button(row, SWT.PUSH);
        _switchBrowse.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _switchBrowse.addSelectionListener(new FireSelectionListener() {
            public void fire() { browseForRootDir(); }
        });
        _switchOpen = new Button(row, SWT.PUSH);
        _switchOpen.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _switchOpen.addSelectionListener(new FireSelectionListener() {
            public void fire() { switchRootDir(); }
        });
        
        _switchDir.setText(_client.getRootDir().getAbsolutePath());
        
        _archiveMgr = new Button(root, SWT.PUSH);
        _archiveMgr.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false));
        _archiveMgr.addSelectionListener(new FireSelectionListener() { public void fire() { 
            _desktop.getNavControl().view(URIHelper.instance().createArchiveManagerURI()); 
        } });
        
        _welcome = new Button(root, SWT.PUSH);
        _welcome.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false));
        _welcome.addSelectionListener(new FireSelectionListener() { public void fire() { new WelcomePopup(_desktop, _ui, getRoot().getShell(), _themeRegistry, _translationRegistry); } });
        
        _sql = new Button(root, SWT.PUSH);
        _sql.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false));
        _sql.addSelectionListener(new FireSelectionListener() { public void fire() { _desktop.getNavControl().view(URIHelper.instance().createSQLURI()); } });

        /*
        _tabbedUI = new Button(root, SWT.PUSH);
        _tabbedUI.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _tabbedUI.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                showTabbedUI();
            }
        });
        */
                
        _exit = new Button(root, SWT.PUSH);
        _exit.setLayoutData(new GridData(GridData.FILL, GridData.END, true, true));
        _exit.addSelectionListener(new FireSelectionListener() { public void fire() { _desktop.exit(); } });
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
        getRoot().layout(true, true);
    }
    
    
    private void updateHttpservStatus() {
        if (HTTPServ.startFailed() && (HTTPServ.getStartFailedMessage() != null)) {
            MessageBox box = new MessageBox(getRoot().getShell(), SWT.ICON_ERROR | SWT.OK);
            box.setText(_translationRegistry.getText("Server failed"));
            box.setMessage(_translationRegistry.getText("There was an error starting up the HTTP server") + ": " + HTTPServ.getStartFailedMessage());
            box.open();
            HTTPServ.clearFailedMessage();
        }
        
        if (HTTPServ.isAlive() && !HTTPServ.startInProgress()) {
            _httpservStatus.setText(_translationRegistry.getText("Running - press to stop"));
            _httpservStatus.setEnabled(true);
        } else if (HTTPServ.startInProgress()) {
            _httpservStatus.setText(_translationRegistry.getText("Startup in progress") + "...");
            _httpservStatus.setEnabled(false);
        } else {
            _httpservStatus.setText(_translationRegistry.getText("Not running - press to start"));
            _httpservStatus.setEnabled(true);
        }
        getRoot().layout(true, true); // resized due to text changes
    }
    private void toggleHttpservStatus() {
        if (HTTPServ.isAlive()) {
            _ui.debugMessage("Killing the httpserv");
            JobRunner.instance().enqueue(new Runnable() {
                public void run() {
                    HTTPServ.killAll();
                    getRoot().getDisplay().asyncExec(new Runnable() {
                        public void run() { updateHttpservStatus(); }
                    });
                }
            });
        } else {
            JobRunner.instance().enqueue(new Runnable() {
                public void run() {
                    // this spawns all necessary threads using the config options saved in the nym prefs
                    new HTTPServ(_client, _ui);
                    getRoot().getDisplay().asyncExec(new Runnable() {
                        public void run() { updateHttpservStatus(); }
                    });
                    UpdateStatusOnStart usos = new UpdateStatusOnStart();
                    usos.schedule(100);
                }
            });
        }
    }
    private class UpdateStatusOnStart extends SimpleTimer2.TimedEvent {
    	UpdateStatusOnStart() {
    		super(SimpleTimer2.getInstance());
    	}
        public void timeReached() {
            getRoot().getDisplay().asyncExec(new Runnable() {
                public void run() { updateHttpservStatus(); }
            });

            if (HTTPServ.isAlive() || HTTPServ.startFailed())
                return;
            else
            	schedule(100);
        }
    }
    private void httpservOnStartChanged() {
        Properties prefs = _client.getNymPrefs();
        if (_httpservOnStart.getSelection())
            prefs.setProperty("httpserv.runOnStartup", "true");
        else
            prefs.setProperty("httpserv.runOnStartup", "false");
        _client.setNymPrefs(prefs);
    }
    
    
    private void showHttpservOptions() {
        final Shell shell = new Shell(getRoot().getShell(), SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        shell.setFont(_themeRegistry.getTheme().SHELL_FONT);
        shell.setText(_translationRegistry.getText("Archive server options"));
        
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        shell.setLayout(gl);
        
        Label l = new Label(shell, SWT.SINGLE);
        l.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        l.setText(_translationRegistry.getText("TCP port to listen on") + ": ");
        l.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        
        final Text port = new Text(shell, SWT.SINGLE | SWT.BORDER);
        port.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        port.setText(HTTPServ.getListenPort(_client) + "");
        port.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        // overzealous to update on focus, but it works
        port.addFocusListener(new FocusListener() {
            public void focusGained(FocusEvent focusEvent) {}
            public void focusLost(FocusEvent focusEvent) {
                String val = port.getText();
                int num = -1;
                try { num = Integer.parseInt(val); } catch (NumberFormatException nfe) {}
                if (num > 0) {
                    Properties prefs = _client.getNymPrefs();
                    prefs.setProperty("httpserv.port", port.getText());
                    _client.setNymPrefs(prefs);
                }
            }
        });

        l = new Label(shell, SWT.SINGLE);
        l.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        l.setText(_translationRegistry.getText("Minimum number of handlers") + ": ");
        l.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        
        final Spinner minListeners = new Spinner(shell, SWT.READ_ONLY | SWT.BORDER);
        minListeners.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        minListeners.setMinimum(1);
        minListeners.setMaximum(50);
        minListeners.setIncrement(1);
        minListeners.setSelection(HTTPServ.getMinListeners(_client));
        minListeners.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        minListeners.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent modifyEvent) {
                Properties prefs = _client.getNymPrefs();
                prefs.setProperty("httpserv.minListeners", minListeners.getSelection()+"");
                _client.setNymPrefs(prefs);
            }
        });
        
        l = new Label(shell, SWT.SINGLE);
        l.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        l.setText(_translationRegistry.getText("Can people post messages to the server? "));
        l.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        
        final Button writable = new Button(shell, SWT.CHECK);
        writable.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        writable.setSelection(HTTPServ.getWritable(_client));
        writable.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                Properties prefs = _client.getNymPrefs();
                prefs.setProperty("httpserv.writable", writable.getSelection() ? "true" : "false");
                _client.setNymPrefs(prefs);
            }
        });
        
        Button done = new Button(shell, SWT.PUSH);
        done.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        done.setText(_translationRegistry.getText("Done"));
        done.setFont(_themeRegistry.getTheme().BUTTON_FONT);
        done.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                shell.dispose();
            }
        });
        
        shell.pack();
        shell.open();
    }
    

    private void browseForRootDir() {
        DirectoryDialog dialog = new DirectoryDialog(getRoot().getShell(), SWT.OPEN);
        dialog.setFilterPath(_switchDir.getText());
        dialog.setMessage(_translationRegistry.getText("Root of the Syndie directory tree (containing db/, archive/, etc)"));
        dialog.setText(_translationRegistry.getText("Root directory"));
        String dir = dialog.open();
        if (dir != null)
            _switchDir.setText(dir);
    }
    private void switchRootDir() {
        String dir = _switchDir.getText();
        _ui.debugMessage("switching to " + dir);
        _desktop.restart(dir);
    }
    
    private void changePass() { new ChangePassPopup(_desktop, _ui, getRoot().getShell(), _themeRegistry, _translationRegistry); }
    
    private void showTabbedUI() { _desktop.showDesktopTabs(); }
    
    private void fireImportBrowse() {
        if (_importBulk.getSelection()) {
            DirectoryDialog dialog = new DirectoryDialog(getRoot().getShell(), SWT.NONE);
            String sel = dialog.open();
            if (sel != null) {
                _importLocation.setText(sel);
                fireImport();
            }
        } else {
            FileDialog dialog = new FileDialog(getRoot().getShell(), SWT.OPEN | SWT.SINGLE);
            dialog.setFilterExtensions(new String[] { "*.syndie", "*" });
            // retranslate each time
            dialog.setText(_translationRegistry.getText("Import syndie file"));
            dialog.setFilterNames(new String[] { _translationRegistry.getText("Syndie files"), _translationRegistry.getText("All files") });
            String file = dialog.open();
            if (file != null) {
                _importLocation.setText(file);
                fireImport();
            }
        }
    }
    
    private void fireOpen() {
        try { 
            SyndieURI uri = new SyndieURI(_openLocation.getText().trim());
            _desktop.getNavControl().view(uri);
        } catch (URISyntaxException use) {
            _ui.errorMessage("Bad URI", use);
        }
    }
    
    private void fireImport() {
        String location = _importLocation.getText().trim();
        final File loc = new File(location);
        if (!loc.exists()) {
            _ui.debugMessage("location does not exist: " + loc);
            return;
        }
        
        _import.setEnabled(false);
        _importBrowse.setEnabled(false);
        _ui.debugMessage("enqueieing import of " + loc);
        JobRunner.instance().enqueue(new Runnable() {
            public void run() {
                int imported = 0;
                List matches = new ArrayList();
                getFiles(loc, matches);
                sortFiles(matches);
                _ui.debugMessage("matching files: " + matches);
                final int total = matches.size();
                Display.getDefault().asyncExec(new Runnable() {
                    public void run() { 
                        _importStatus.setText(0 + "/" + total); 
                        _importStatus.getParent().layout(true, true);
                    }
                });
                for (int i = 0; i < total; i++) {
                    boolean ok = importFile((File)matches.get(i));
                    if (ok) {
                        _ui.debugMessage("imported: " + matches.get(i));
                        imported++;
                    } else {
                        _ui.debugMessage("NOT imported: " + matches.get(i));
                    }
                    
                    final int pass = i+1;
                    final int okCount = imported;
                    Display.getDefault().asyncExec(new Runnable() {
                        public void run() { 
                            _importStatus.setText(pass + " (" + okCount + ") /" + total); 
                            _importStatus.getParent().layout(true, true);
                        }
                    });
                }
                final int successful = imported;
                Display.getDefault().asyncExec(new Runnable() {
                    public void run() {
                        MessageBox box = new MessageBox(getRoot().getShell(), SWT.ICON_INFORMATION | SWT.OK);
                        box.setText(_translationRegistry.getText("Import complete"));
                        box.setMessage(_translationRegistry.getText("Messages imported successfully/total") + ": " + successful + "/" + total);
                        box.open();
                        _import.setEnabled(true);
                        _importBrowse.setEnabled(true);
                        _importStatus.getParent().layout(true, true);
                    }
                });
            }
        });
    }
    
    private void getFiles(File f, List rv) {
        if (f.exists()) {
            if (f.isFile() && f.getName().endsWith(Constants.FILENAME_SUFFIX)) {
                rv.add(f);
            } else if (f.isDirectory() && (!f.getName().equals(".") && !f.getName().equals(".."))) {
                File files[] = f.listFiles();
                for (int i = 0; i < files.length; i++)
                    getFiles(files[i], rv);
            }
        }
    }
    private void sortFiles(List orig) {
        List meta = new ArrayList();
        List post = new ArrayList();
        for (int i = 0; i < orig.size(); i++) {
            File f = (File)orig.get(i);
            if (f.getName().startsWith("meta"))
                meta.add(f);
            else
                post.add(f);
        }
        orig.clear();
        orig.addAll(meta);
        orig.addAll(post);
    }
    
    /**
     *  run outside the swt thread
     *  @return success
     */
    private boolean importFile(File f) {
        Importer imp = new Importer(_client, null);
        if (f.exists()) {
            try {
                ImportResult.Result result = imp.processMessage(_ui, _client, new FileInputStream(f), null, false, null, null);
                return result.ok();
            } catch (IOException ioe) {
                _ui.errorMessage("error importing " + f.getPath(), ioe);
                return false;
            }
        } else {
            return false;
        }
    }
    
    
    public void applyTheme(Theme theme) {
        _importLabel.setFont(theme.DEFAULT_FONT);
        _importFile.setFont(theme.DEFAULT_FONT);
        _importBulk.setFont(theme.DEFAULT_FONT);
        _importLocation.setFont(theme.DEFAULT_FONT);
        _importBrowse.setFont(theme.BUTTON_FONT);
        _import.setFont(theme.BUTTON_FONT);
        _openLabel.setFont(theme.DEFAULT_FONT);
        _openLocation.setFont(theme.DEFAULT_FONT);
        _open.setFont(theme.BUTTON_FONT);
        _switchBrowse.setFont(theme.BUTTON_FONT);
        _switchDir.setFont(theme.DEFAULT_FONT);
        _switchLabel.setFont(theme.DEFAULT_FONT);
        _switchOpen.setFont(theme.BUTTON_FONT);
        _exit.setFont(theme.BUTTON_FONT);
        _sql.setFont(theme.BUTTON_FONT);
        _welcome.setFont(theme.BUTTON_FONT);
        _archiveMgr.setFont(theme.BUTTON_FONT);
        //_tabbedUI.setFont(theme.BUTTON_FONT);
        _changePass.setFont(theme.BUTTON_FONT);
        _httpservLabel.setFont(theme.DEFAULT_FONT);
        _httpservOnStart.setFont(theme.DEFAULT_FONT);
        _httpservOptions.setFont(theme.BUTTON_FONT);
        _httpservStatus.setFont(theme.BUTTON_FONT);
    }
    
    
    
    public void translate(TranslationRegistry registry) {
        _importLabel.setText(registry.getText("Import") + ": ");
        _importFile.setText(registry.getText("Individual message"));
        _importBulk.setText(registry.getText("Directory"));
        _importBrowse.setText(registry.getText("Browse") + "...");
        _import.setText(registry.getText("Import"));
        //_tabbedUI.setText(registry.getText("Advanced: Display the tabbed UI"));
        _changePass.setText(registry.getText("Change Syndie instance passphrase"));
        _openLabel.setText(registry.getText("Open Syndie URI") + ':');
        _sql.setText(registry.getText("Advanced SQL interface"));
        _open.setText(registry.getText("Open"));
        _switchLabel.setText(registry.getText("Log in to a different Syndie instance") + ':');
        _switchBrowse.setText(registry.getText("Browse") + "...");
        _switchOpen.setText(registry.getText("Open selected"));
        _welcome.setText(registry.getText("Reshow welcome screen"));
        _archiveMgr.setText(registry.getText("Archive manager"));
        _exit.setText(registry.getText("Exit"));

        _httpservLabel.setText(registry.getText("Integrated HTTP-accessible archive server") + ':');
        _httpservOnStart.setText(registry.getText("Run on startup"));
        _httpservOptions.setText(registry.getText("Configure") + "...");
        
        updateHttpservStatus();
    }
}
