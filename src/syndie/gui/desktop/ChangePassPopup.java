package syndie.gui.desktop;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import syndie.db.TextEngine;
import syndie.db.UI;
import syndie.gui.FireSelectionListener;
import syndie.gui.Splash;
import syndie.gui.ThemeRegistry;
import syndie.gui.TranslationRegistry;

public class ChangePassPopup {
    private Desktop _desktop;
    private UI _ui;
    private Shell _parent;
    private ThemeRegistry _themeRegistry;
    private TranslationRegistry _translationRegistry;
    
    public ChangePassPopup(Desktop desktop, UI ui, Shell parent, ThemeRegistry themes, TranslationRegistry trans) {
        _desktop = desktop;
        _ui = ui;
        _parent = parent;
        _themeRegistry = themes;
        _translationRegistry = trans;
        initComponents();
    }
        

    private void initComponents() {        
        // show a special warning/error screen
        final Shell s = new Shell(_parent, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        s.setText(_translationRegistry.getText("Change passphrase"));
        s.setLayout(new GridLayout(2, false));
        s.setFont(_themeRegistry.getTheme().SHELL_FONT);

        Label l = new Label(s, SWT.SINGLE | SWT.WRAP);
        l.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        l.setText(_translationRegistry.getText("Old passphrase") + ':');
        l.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        final Text oldPass = new Text(s, SWT.SINGLE | SWT.WRAP | SWT.PASSWORD | SWT.BORDER);
        oldPass.setText("");
        oldPass.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        oldPass.setFont(_themeRegistry.getTheme().DEFAULT_FONT);

        l = new Label(s, SWT.SINGLE | SWT.WRAP);
        l.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        l.setText(_translationRegistry.getText("New passphrase") + ':');
        l.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        final Text newPass = new Text(s, SWT.SINGLE | SWT.WRAP | SWT.PASSWORD | SWT.BORDER);
        newPass.setText("");
        newPass.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        newPass.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        
        l = new Label(s, SWT.SINGLE | SWT.WRAP);
        l.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        l.setText(_translationRegistry.getText("New passphrase (again)") + ':');
        l.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        final Text newPass2 = new Text(s, SWT.SINGLE | SWT.WRAP | SWT.PASSWORD | SWT.BORDER);
        newPass2.setText("");
        newPass2.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        newPass2.setFont(_themeRegistry.getTheme().DEFAULT_FONT);

        Button b = new Button(s, SWT.PUSH);
        b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        b.setText(_translationRegistry.getText("Reset passphrase"));
        b.setFont(_themeRegistry.getTheme().BUTTON_FONT);
        b.addSelectionListener(new FireSelectionListener() { 
            public void fire() {
                String old = oldPass.getText();
                String newP = newPass.getText();
                String newP2 = newPass2.getText();
                
                s.dispose();
                changePass(old, newP, newP2);
            }
        });

        b = new Button(s, SWT.PUSH);
        b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        b.setText(_translationRegistry.getText("Cancel"));
        b.setFont(_themeRegistry.getTheme().BUTTON_FONT);
        b.addSelectionListener(new FireSelectionListener() { 
            public void fire() {
                s.dispose();
            }
        });

        s.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent shellEvent) {
                s.dispose();
            }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        s.pack();
        Rectangle sSize = s.getBounds();
        Rectangle screenSize = Splash.getScreenSize(s);
        int x = screenSize.width/2-sSize.width/2;
        int y = screenSize.height/2-sSize.height/2;
        s.setBounds(x, y, sSize.width, sSize.height);
        s.open();
    }
    
    private void changePass(String old, String newPass, String newPass2) {
        if (old.equals("")) {
            if (_desktop.getPassphrase() == null)
                old = TextEngine.DEFAULT_PASS;
        }
        if ( (_desktop.getPassphrase() != null) && (!_desktop.getPassphrase().equals(old)) ) {
            _ui.debugMessage("old passphrase is not correct");
            MessageBox box = new MessageBox(_parent, SWT.ICON_ERROR | SWT.OK | SWT.PRIMARY_MODAL);
            box.setMessage(_translationRegistry.getText("Old passphrase is incorrect"));
            box.setText(_translationRegistry.getText("Invalid passphrase"));
            box.open();
            return;
        }
        
        _ui.debugMessage("old passphrase is correct");
        
        if (newPass.equals(newPass2)) {
            _ui.debugMessage("new passphrases match");
            _desktop.changePassphrase(newPass);
        } else {
            _ui.debugMessage("new passphrases do not match");
            MessageBox box = new MessageBox(_parent, SWT.ICON_ERROR | SWT.OK | SWT.PRIMARY_MODAL);
            box.setMessage(_translationRegistry.getText("New passphrases don't match"));
            box.setText(_translationRegistry.getText("Invalid passphrase"));
            box.open();
            return;
        }
    }
    
}
