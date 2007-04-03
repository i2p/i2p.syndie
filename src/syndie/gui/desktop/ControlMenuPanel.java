package syndie.gui.desktop;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.Importer;
import syndie.db.JobRunner;
import syndie.db.UI;
import syndie.gui.*;

/**
 *
 */
public class ControlMenuPanel extends DesktopPanel implements Themeable, Translatable {
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

    private Button _exit;
    
    public ControlMenuPanel(Desktop desktop, DBClient client, ThemeRegistry themes, TranslationRegistry trans, Composite parent, UI ui) {
        super(desktop, client, themes, trans, parent, ui, null);
        initComponents();
    }
    
    public String getPanelName() { return "Control menu"; }
    public String getPanelDescription() { return "Main control menu"; }

    protected void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
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
        _open = new Button(row, SWT.PUSH);
        _open.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _open.addSelectionListener(new FireSelectionListener() { public void fire() { fireOpen(); } });
        
        _exit = new Button(root, SWT.PUSH);
        _exit.setLayoutData(new GridData(GridData.FILL, GridData.END, true, true));
        _exit.addSelectionListener(new FireSelectionListener() { public void fire() { _desktop.exit(); } });
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
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
            dialog.setText(_translationRegistry.getText(T_IMPORT_SYNDIE_TITLE, "Import syndie file"));
            dialog.setFilterNames(new String[] { _translationRegistry.getText(T_IMPORT_SYNDIE_EXTENSION, "Syndie files"), _translationRegistry.getText(T_IMPORT_ALL_EXTENSION, "All files") });
            String file = dialog.open();
            if (file != null) {
                _importLocation.setText(file);
                fireImport();
            }
        }
    }
    private static final String T_IMPORT_SYNDIE_TITLE = "syndie.gui.desktop.controlmenupanel.importsyndietitle";
    private static final String T_IMPORT_SYNDIE_EXTENSION = "syndie.gui.desktop.controlmenupanel.importsyndieextension";
    private static final String T_IMPORT_ALL_EXTENSION = "syndie.gui.desktop.controlmenupanel.importallextension";
    
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
                        box.setText(_translationRegistry.getText(T_IMPORT_COMPLETE, "Import complete"));
                        box.setMessage(_translationRegistry.getText(T_IMPORT_COMPLETE_PREFIX, "Messages imported successfully/total: ") + successful + "/" + total);
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
    
    /** run outside the swt thread */
    private boolean importFile(File f) {
        Importer imp = new Importer(_client, null);
        if (f.exists()) {
            try {
                boolean rv = imp.processMessage(_ui, _client, new FileInputStream(f), null, false, null, null);
                return rv;
            } catch (IOException ioe) {
                _ui.errorMessage("error importing " + f.getPath(), ioe);
                return false;
            }
        } else {
            return false;
        }
    }
    
    private static final String T_IMPORT_COMPLETE = "syndie.gui.desktop.controlmenupanel.importcomplete";
    private static final String T_IMPORT_COMPLETE_PREFIX = "syndie.gui.desktop.controlmenupanel.importcomplete.prefix";
    
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
        _exit.setFont(theme.BUTTON_FONT);
    }
    
    private static final String T_IMPORTLABEL = "syndie.gui.desktop.controlmenupanel.importlabel";
    private static final String T_IMPORTFILE = "syndie.gui.desktop.controlmenupanel.importfile";
    private static final String T_IMPORTBULK = "syndie.gui.desktop.controlmenupanel.importbulk";
    private static final String T_IMPORTBROWSE = "syndie.gui.desktop.controlmenupanel.importbrowse";
    private static final String T_IMPORT = "syndie.gui.desktop.controlmenupanel.import";
    private static final String T_OPENLABEL = "syndie.gui.desktop.controlmenupanel.openlabel";
    private static final String T_OPEN = "syndie.gui.desktop.controlmenupanel.open";
    private static final String T_EXIT = "syndie.gui.desktop.controlmenupanel.exit";
    
    public void translate(TranslationRegistry registry) {
        _importLabel.setText(registry.getText(T_IMPORTLABEL, "Import: "));
        _importFile.setText(registry.getText(T_IMPORTFILE, "individual message"));
        _importBulk.setText(registry.getText(T_IMPORTBULK, "directory (recursively)"));
        _importBrowse.setText(registry.getText(T_IMPORTBROWSE, "Browse..."));
        _import.setText(registry.getText(T_IMPORT, "Import"));
        _openLabel.setText(registry.getText(T_OPENLABEL, "Open Syndie URI:"));
        _open.setText(registry.getText(T_OPEN, "Open"));
        _exit.setText(registry.getText(T_EXIT, "Exit"));
    }
}
