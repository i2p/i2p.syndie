package syndie.gui;

import java.util.List;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Shell;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;

/**
 *
 */
class MessageReferenceEditorPopup implements Translatable {
    private BrowserControl _browser;
    private Shell _parent;
    private Shell _shell;
    private ManageReferenceChooser _chooser;
    private MessageEditor _editor;
    private Button _ok;
    private Button _cancel;
    private PopupListener _lsnr;
    
    public MessageReferenceEditorPopup(BrowserControl control, Shell parent, MessageEditor editor, PopupListener lsnr) {
        _browser = control;
        _parent = parent;
        _editor = editor;
        _lsnr = lsnr;
        initComponents();
    }
    
    private void initComponents() {
        _shell = new Shell(_parent, SWT.SHELL_TRIM | SWT.APPLICATION_MODAL);
        _shell.setLayout(new GridLayout(2, true));
        _chooser = new ManageReferenceChooser(_shell, _browser, _editor);
        _chooser.getControl().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
        _ok = new Button(_shell, SWT.PUSH);
        _ok.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { ok(); }
            public void widgetSelected(SelectionEvent selectionEvent) { ok(); }
        });
        _cancel = new Button(_shell, SWT.PUSH);
        _cancel.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { cancel(); }
            public void widgetSelected(SelectionEvent selectionEvent) { cancel(); }
        });
        _shell.pack();
        
        // intercept the shell closing, since that'd cause the shell to be disposed rather than just hidden
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; cancel(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        _browser.getTranslationRegistry().register(this);
    }
    public interface PopupListener {
        public void referencesSelected(List referenceNodes);
    }
    
    public void open() { 
        _chooser.setReferences(_editor.getReferenceNodes());
        _shell.setSize(_shell.computeSize(300, 200));
        _shell.open();
    }
    private void cancel() { _shell.setVisible(false); }
    private void ok() {
        if (_lsnr != null)
            _lsnr.referencesSelected(_chooser.getReferenceNodes());
        _shell.setVisible(false);
    }
    
    public void dispose() {
        _chooser.dispose();
        _browser.getTranslationRegistry().unregister(this);
    }

    private static final String T_TITLE = "syndie.gui.messagereferenceeditorpopup.title";
    private static final String T_OK = "syndie.gui.messagereferenceeditorpopup.ok";
    private static final String T_CANCEL = "syndie.gui.messagereferenceeditorpopup.cancel";
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText(T_TITLE, "References"));
        _ok.setText(registry.getText(T_OK, "Ok"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel"));
    }
}
