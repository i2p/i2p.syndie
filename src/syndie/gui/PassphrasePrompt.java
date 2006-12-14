package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 *
 */
public class PassphrasePrompt implements Translatable, Themeable {
    private BrowserControl _browser;
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
    
    public PassphrasePrompt(BrowserControl browser, Shell parent, boolean creatingNewPassphrase) {
        _browser = browser;
        _parent = parent;
        _creatingNewPassphrase = creatingNewPassphrase;
        initComponents();
    }
    
    public void open() {
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
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
    }
    
    private void complete(boolean ok) {
        String pass = _passphrase.getText();
        String prompt = _passphrasePrompt.getText();
        _shell.dispose();
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
        if (ok && (_listener != null))
            _listener.promptComplete(pass, prompt);
    }
    
    private static final String T_SHELL = "syndie.gui.passphraseprompt.shell";
    private static final String T_PASSPHRASE = "syndie.gui.passphraseprompt.passphrase";
    private static final String T_PROMPT = "syndie.gui.passphraseprompt.prompt";
    private static final String T_OK = "syndie.gui.passphraseprompt.ok";
    private static final String T_CANCEL = "syndie.gui.passphraseprompt.cancel";
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText(T_SHELL, "Passphrase"));
        _passphraseLabel.setText(registry.getText(T_PASSPHRASE, "Passphrase:"));
        _passphrasePromptLabel.setText(registry.getText(T_PROMPT, "Publicly visible hint:"));
        _ok.setText(registry.getText(T_OK, "OK"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel"));
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
