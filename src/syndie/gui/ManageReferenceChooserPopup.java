package syndie.gui;

import java.util.List;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Shell;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

/**
 *
 */
public class ManageReferenceChooserPopup implements Themeable, Translatable {
    private BrowserControl _browser;
    private Shell _parentShell;
    private Shell _shell;
    private ManageReferenceChooser _refs;
    private Button _close;
    
    public ManageReferenceChooserPopup(BrowserControl browser, Shell parentShell) {
        _browser = browser;
        _parentShell = parentShell;
        initComponents();
    }
    
    public void setReferences(List refs) { _refs.setReferences(refs); }
    public void show() {
        //_shell.pack(true); 
        _shell.open();
    }
    public void hide() { _shell.setVisible(false); }
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
        _refs.dispose();
    }
    
    private void initComponents() {
        _shell = new Shell(_parentShell, SWT.SHELL_TRIM | SWT.PRIMARY_MODAL);
        _shell.setLayout(new GridLayout(1, true));
        _refs = new ManageReferenceChooser(_shell, _browser, false);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1);
        //gd.widthHint = 600;
        //gd.heightHint = 300;
        _refs.getControl().setLayoutData(gd);
        
        _close = new Button(_shell, SWT.PUSH);
        _close.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _close.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { hide(); }
            public void widgetSelected(SelectionEvent selectionEvent) { hide(); }
        });
        
        // intercept the shell closing, since that'd cause the shell to be disposed rather than just hidden
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; hide(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        //_shell.pack();
        //Point sz = _refs.getControl().computeSize(SWT.DEFAULT, SWT.DEFAULT);
        //sz.x += 50;
        //sz.y += 200;
        //_shell.setSize(sz.x, sz.y);
        
        _shell.setSize(_shell.computeSize(400, 200));
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
    }
    
    public void applyTheme(Theme theme) {
        _close.setFont(theme.BUTTON_FONT);
        _shell.setFont(theme.SHELL_FONT);
    }
    
    private static final String T_CLOSE = "syndie.gui.managereferencechooserpopup.close";
    private static final String T_SHELL = "syndie.gui.managereferencechooserpopup.shell";
    
    public void translate(TranslationRegistry registry) {
        _close.setText(registry.getText(T_CLOSE, "Close"));
        _shell.setText(registry.getText(T_SHELL, "References"));
    }
}
