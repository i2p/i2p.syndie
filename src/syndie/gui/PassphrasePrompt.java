package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 *
 */
public class PassphrasePrompt extends BaseComponent implements Translatable, Themeable {
    private Shell _parent;
    private Shell _shell;
    private boolean _creatingNewPassphrase;
    private PassphraseListener _listener;
    
    private Label _passphraseLabel;
    private Text _passphrase;
    private Label _passphrasePromptLabel;
    private Text _passphrasePrompt;
    private Button _ok;
    private Button _cancel;
    
    public PassphrasePrompt(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Shell parent, boolean creatingNewPassphrase) {
        super(client, ui, themes, trans);
        _parent = parent;
        _creatingNewPassphrase = creatingNewPassphrase;
        initComponents();
    }
    
    public void open() {
        if (!_creatingNewPassphrase && (_passphrasePrompt.getText().trim().length() <= 0)) {
            ((GridData)_passphrasePrompt.getLayoutData()).exclude = true;
            ((GridData)_passphrasePromptLabel.getLayoutData()).exclude = true;
            _passphrasePrompt.setVisible(false);
            _passphrasePromptLabel.setVisible(false);
            _shell.layout(true, true);
        }
        _shell.setSize(_shell.computeSize(300, SWT.DEFAULT));
        _shell.open();
    }
    
    public void setPassphrase(String passphrase) {
        if (passphrase == null) passphrase = "";
        _passphrase.setText(passphrase);
    }
    public void setPassphrasePrompt(String passphrasePrompt) { 
        if (passphrasePrompt == null) passphrasePrompt = "";
        _passphrasePrompt.setText(passphrasePrompt);
    }
    public void setPassphraseListener(PassphraseListener lsnr) { _listener = lsnr; }
    
    public static interface PassphraseListener {
        public void promptComplete(String passphraseEntered, String promptEntered);
        public void promptAborted();
    }
    
    private void initComponents() {
        _shell = new Shell(_parent, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        _shell.setLayout(new GridLayout(2, false));
        
        _passphraseLabel = new Label(_shell, SWT.NONE);
        _passphraseLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _passphrase = new Text(_shell, SWT.SINGLE | SWT.BORDER);
        _passphrase.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        if (!_creatingNewPassphrase)
            _passphrase.setEchoChar('*');
        _passphrase.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) complete(true);
            }
        });
        _passphrasePromptLabel = new Label(_shell, SWT.NONE);
        _passphrasePromptLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _passphrasePrompt = new Text(_shell, SWT.SINGLE | SWT.BORDER);
        _passphrasePrompt.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        if (!_creatingNewPassphrase)
            _passphrasePrompt.setEditable(false);

        _cancel = new Button(_shell, SWT.PUSH);
        _cancel.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { complete(false); }
            public void widgetSelected(SelectionEvent selectionEvent) { complete(false); }
        });
        _ok = new Button(_shell, SWT.PUSH);
        _ok.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { complete(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { complete(true); }
        });
        
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; complete(false); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    private void complete(boolean ok) {
        String pass = _passphrase.getText();
        String prompt = _passphrasePrompt.getText();
        _shell.dispose();
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        if (_listener != null) {
            if (ok)
                _listener.promptComplete(pass, prompt);
            else
                _listener.promptAborted();
        }
    }
    
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText("Passphrase"));
        _passphraseLabel.setText(registry.getText("Passphrase") + ':');
        _passphrasePromptLabel.setText(registry.getText("Publicly visible hint") + ':');
        _ok.setText(registry.getText("OK"));
        _cancel.setText(registry.getText("Cancel"));
    }
    
    public void applyTheme(Theme theme) {
        _shell.setFont(theme.SHELL_FONT);
        _passphraseLabel.setFont(theme.DEFAULT_FONT);
        _passphrase.setFont(theme.DEFAULT_FONT);
        _passphrasePromptLabel.setFont(theme.DEFAULT_FONT);
        _passphrasePrompt.setFont(theme.DEFAULT_FONT);
        _ok.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
    }
}
