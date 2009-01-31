package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 *
 */
public class ArchiveDefaultsPopup implements Themeable, Translatable {
    private Shell _parent;
    private DBClient _client;
    private UI _ui;
    private ThemeRegistry _themeRegistry;
    private TranslationRegistry _translationRegistry;
    
    private Shell _shell;

    private Label _explanationWhy;
    private ArchiveDefaults _archiveDefaults;
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
    
    private void initComponents() {
        _shell = new Shell(_parent, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        _shell.setLayout(new GridLayout(1, false));
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent shellEvent) { dispose(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        _explanationWhy = new Label(_shell, SWT.WRAP);
        _explanationWhy.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _archiveDefaults = new ArchiveDefaults(_shell, _client, _ui, _themeRegistry, _translationRegistry);
        _archiveDefaults.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _instructions = new Label(_shell, SWT.WRAP);
        _instructions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        Composite buttonBar = new Composite(_shell, SWT.NONE);
        buttonBar.setLayout(new FillLayout());
        buttonBar.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _save = new Button(buttonBar, SWT.PUSH);
        _save.addSelectionListener(new FireSelectionListener() { public void fire() { save(); } });
        
        _cancel = new Button(buttonBar, SWT.PUSH);
        _cancel.addSelectionListener(new FireSelectionListener() { public void fire() { dispose(); } });
        
        _themeRegistry.register(this);
        _translationRegistry.register(this);
        
        _shell.pack();
        _shell.open();
    }
    
    private void save() {
        _archiveDefaults.save();
        dispose();
    }
    
    private void dispose() {
        _themeRegistry.unregister(this);
        _translationRegistry.unregister(this);
        _archiveDefaults.dispose();
        _shell.dispose();
    }
    
    private static final String T_TITLE = "syndie.gui.archivedefaultspopup.title";
    private static final String T_EXPLANATIONWHY = "syndie.gui.archivedefaultspopup.explanationwhy";
    private static final String T_INSTRUCTIONS = "syndie.gui.archivedefaultspopup.instructions";
    private static final String T_SAVE = "syndie.gui.archivedefaultspopup.save";
    private static final String T_CANCEL = "syndie.gui.archivedefaultspopup.cancel";
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText(T_TITLE, "Archive defaults"));
        _explanationWhy.setText(registry.getText(T_EXPLANATIONWHY, "You have no remote archives configured, preventing you from either reading other people's posts or from sharing your own posts with them."));
        _instructions.setText(registry.getText(T_INSTRUCTIONS, "The default archives shipped with your Syndie instance are listed above.  Please make any necessary changes, unchecking any that you don't want"));
        _save.setText(registry.getText(T_SAVE, "Save archives as your own"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel"));
    }
    
    public void applyTheme(Theme theme) {
        _shell.setFont(theme.SHELL_FONT);
        _explanationWhy.setFont(theme.DEFAULT_FONT);
        _instructions.setFont(theme.DEFAULT_FONT);
        _save.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
        _shell.pack();
    }
}
