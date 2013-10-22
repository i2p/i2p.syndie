package syndie.gui;

import java.util.ArrayList;
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
import syndie.db.UI;

/**
 *
 */
class ManageReferenceChooserPopup extends BaseComponent implements Themeable, Translatable {
    private Shell _parentShell;
    private Shell _shell;
    private ManageReferenceChooser _refs;
    private Button _close;
    private boolean _editable;
    private List _closeListeners;
    
    public ManageReferenceChooserPopup(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Shell parentShell) { this(client, ui, themes, trans, parentShell, false); }
    public ManageReferenceChooserPopup(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Shell parentShell, boolean editable) {
        super(client, ui, themes, trans);
        _parentShell = parentShell;
        _editable = editable;
        _closeListeners = new ArrayList();
        initComponents();
    }
    
    public void setReferences(List refs) { _refs.setReferences(refs); }
    public void show() {
        //_shell.pack(true); 
        _shell.open();
    }
    public void hide() { 
        _shell.setVisible(false);
        if (_closeListeners.size() > 0) {
            List refs = _refs.getReferenceNodes();
            for (int i = 0; i < _closeListeners.size(); i++)
                ((CloseListener)_closeListeners.get(i)).closed(refs);
        }
    }
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        _refs.dispose();
    }
    
    public interface CloseListener { public void closed(List refRoots); }
    public void addCloseListener(CloseListener lsnr) { _closeListeners.add(lsnr); }
    
    private void initComponents() {
        _shell = new Shell(_parentShell, SWT.SHELL_TRIM | SWT.PRIMARY_MODAL);
        _shell.setLayout(new GridLayout(1, true));
        _refs = ComponentBuilder.instance().createManageReferenceChooser(_shell, _editable);
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
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    public void applyTheme(Theme theme) {
        _close.setFont(theme.BUTTON_FONT);
        _shell.setFont(theme.SHELL_FONT);
    }
    
    
    public void translate(TranslationRegistry registry) {
        _close.setText(registry.getText("Close"));
        _shell.setText(registry.getText("References"));
    }
}
