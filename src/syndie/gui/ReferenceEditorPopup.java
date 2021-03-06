package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Shell;
import syndie.data.NymReferenceNode;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 *  For adding bookmarks, etc. Contains a ReferenceEditor.
 */
class ReferenceEditorPopup extends BaseComponent implements Translatable, Themeable, ReferenceEditor.ReferenceEditorListener {
    private final Shell _parent;
    private Shell _shell;
    private ReferenceEditor _editor;
    private final NavigationControl _navControl;
    private final BanControl _banControl;
    private final BookmarkControl _bookmarkControl;
    
    public ReferenceEditorPopup(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl navControl, BanControl banControl, BookmarkControl bookmarkControl, Shell parent) {
        super(client, ui, themes, trans);
        _parent = parent;
        _navControl = navControl;
        _banControl = banControl;
        _bookmarkControl = bookmarkControl;
        initComponents();
    }
    
    private void initComponents() {
        _shell = new Shell(_parent, SWT.SHELL_TRIM | SWT.APPLICATION_MODAL);
        _shell.setLayout(new FillLayout());
        _editor = new ReferenceEditor(_client, _ui, _themeRegistry, _translationRegistry, _shell, _navControl, _banControl, _bookmarkControl, this);
        
        // intercept the shell closing, since that'd cause the shell to be disposed rather than just hidden
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; cancel(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    public void dispose() {
        if (!_shell.isDisposed())
            _shell.dispose();
        _editor.dispose();
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        _editor.dispose();
    }
    
    public void open() { 
        if ( (_shell == null) || (_shell.isDisposed()) )
            initComponents();
        
        _shell.layout(true, true);
        _shell.setSize(_editor.getControl().computeSize(500, 500));
        _shell.open();
    }
    
    public void setReference(NymReferenceNode node) { _editor.setReference(node); }
    
    private void cancel() { dispose(); }
    
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText("Reference editor"));
    }
    public void applyTheme(Theme theme) {
        _shell.setFont(theme.SHELL_FONT);
    }

    public void cancelled(ReferenceEditor editor) { cancel(); }
    public void saved(ReferenceEditor editor) { cancel(); }
}
