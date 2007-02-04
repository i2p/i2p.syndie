package syndie.gui;

import java.util.ArrayList;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import syndie.data.SyndieURI;

/**
 *
 */
class ManageForumAuthReply implements Themeable, Translatable {
    private BrowserControl _browser;
    private ManageForum _manage;
    
    private Shell _shell;
    private Composite _root;
    
    private Button _rotate;
    private Button _sendNewSelected;
    private List _sendNewSelectedList;
    private Button _sendNewSelectedAdd;
    private Button _sendNewSelectedDel;
    private Button _sendNewPBE;
    private Label _sendNewPBEPassLabel;
    private Text _sendNewPBEPass;
    private Label _sendNewPBEPromptLabel;
    private Text _sendNewPBEPrompt;
    private Button _ok;
    private Button _cancel;
    
    private ReferenceChooserPopup _chooser;
    /** channels (Hash) allowed to manage, ordered by the _sendNewSelectedList */
    private ArrayList _sendNewSelectedForums;
    
    public ManageForumAuthReply(BrowserControl browser, ManageForum manage) {
        _browser = browser;
        _manage = manage;
        _sendNewSelectedForums = new ArrayList();
        initComponents();
    }
    
    private void initComponents() {
        _shell = new Shell(_manage.getRoot().getShell(), SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        _shell.setLayout(new FillLayout());
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) {
                evt.doit = false;
                hide();
            }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        _root = new Composite(_shell, SWT.NONE);
        _root.setLayout(new GridLayout(4, false));
        
        _rotate = new Button(_root, SWT.CHECK);
        _rotate.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        
        _sendNewSelected = new Button(_root, SWT.CHECK);
        _sendNewSelected.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        
        _sendNewSelectedList = new List(_root, SWT.MULTI | SWT.BORDER | SWT.V_SCROLL);
        _sendNewSelectedList.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 2));
        
        _sendNewSelectedAdd = new Button(_root, SWT.PUSH);
        _sendNewSelectedAdd.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        
        _sendNewSelectedDel = new Button(_root, SWT.PUSH);
        _sendNewSelectedDel.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        
        _sendNewPBE = new Button(_root, SWT.CHECK | SWT.WRAP);
        _sendNewPBE.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        
        _sendNewPBEPassLabel = new Label(_root, SWT.WRAP);
        _sendNewPBEPassLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _sendNewPBEPass = new Text(_root, SWT.SINGLE | SWT.BORDER);
        _sendNewPBEPass.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _sendNewPBEPromptLabel = new Label(_root, SWT.WRAP);
        _sendNewPBEPromptLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _sendNewPBEPrompt = new Text(_root, SWT.SINGLE | SWT.BORDER);
        _sendNewPBEPrompt.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        Composite actions = new Composite(_root, SWT.NONE);
        actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        actions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        
        _ok = new Button(actions, SWT.PUSH);
        _cancel = new Button(actions, SWT.PUSH);
        
        _ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { ok(); }
            public void widgetSelected(SelectionEvent selectionEvent) { ok(); }
        });
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { hide(); }
            public void widgetSelected(SelectionEvent selectionEvent) { hide(); }
        });
        
        _rotate.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { refreshEnabled(); }
            public void widgetSelected(SelectionEvent selectionEvent) { refreshEnabled(); }
        });
        _sendNewSelected.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { refreshEnabled(); }
            public void widgetSelected(SelectionEvent selectionEvent) { refreshEnabled(); }
        });
        _sendNewPBE.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { refreshEnabled(); }
            public void widgetSelected(SelectionEvent selectionEvent) { refreshEnabled(); }
        });
        
        _sendNewSelectedAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addAuthorizedForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addAuthorizedForum(); }
        });
        _sendNewSelectedDel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { delAuthorizedForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { delAuthorizedForum(); }
        });
        
        refreshEnabled();
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
    }
    
    public void show() { _shell.pack(); _shell.open(); }
    private void hide() { _shell.setVisible(false); }
    private void ok() { _manage.modified(); hide(); }
    
    public boolean getRotate() { return _rotate.getSelection(); }
    public ArrayList getSendNewExplicit() { return _sendNewSelectedForums; }
    public boolean getPostPBE() { return _sendNewPBE.getSelection(); }
    public String getSendPassphrase() { return getPostPBE() ? _sendNewPBEPass.getText().trim() : null; }
    public String getSendPassphrasePrompt() { return getPostPBE() ? _sendNewPBEPrompt.getText().trim() : null; }
    
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
        if (!_shell.isDisposed())
            _shell.dispose();
        if (_chooser != null)
            _chooser.dispose();
    }
    
    private void refreshEnabled() {
        _sendNewSelected.setEnabled(_rotate.getSelection());
        _sendNewSelectedList.setEnabled(_sendNewSelected.getSelection() && _rotate.getSelection());
        _sendNewSelectedAdd.setEnabled(_sendNewSelected.getSelection() && _rotate.getSelection());
        _sendNewSelectedDel.setEnabled(_sendNewSelected.getSelection() && _rotate.getSelection());
        _sendNewPBE.setEnabled(_rotate.getSelection());
        _sendNewPBEPassLabel.setEnabled(_rotate.getSelection() && _sendNewPBE.getSelection());
        _sendNewPBEPass.setEnabled(_rotate.getSelection() && _sendNewPBE.getSelection());
        _sendNewPBEPromptLabel.setEnabled(_rotate.getSelection() && _sendNewPBE.getSelection());
        _sendNewPBEPrompt.setEnabled(_rotate.getSelection() && _sendNewPBE.getSelection());
    }
    
    private void addAuthorizedForum() {
        if (_chooser == null)
            _chooser = new ReferenceChooserPopup(_shell, _browser);
        _chooser.setListener(new ReferenceChooserTree.AcceptanceListener() {
            public void referenceAccepted(SyndieURI uri) {
                if ( (uri != null) && (uri.getScope() != null) ) {
                    if (!_sendNewSelectedForums.contains(uri.getScope())) {
                        _sendNewSelectedForums.add(uri.getScope());
                        String name = _browser.getClient().getChannelName(uri.getScope());
                        if (name != null)
                            _sendNewSelectedList.add(name + " [" + uri.getScope().toBase64().substring(0,6) + "]");
                        else
                            _sendNewSelectedList.add("[" + uri.getScope().toBase64().substring(0,6) + "]");
                    }
                }
            }
            public void referenceChoiceAborted() {}
        });
        _chooser.show();
    }
    private void delAuthorizedForum() {
        int idx = _sendNewSelectedList.getSelectionIndex();
        if ( (idx >= 0) && (idx <= _sendNewSelectedForums.size()) ) {
            _sendNewSelectedForums.remove(idx);
            _sendNewSelectedList.remove(idx);
        }
    }
    
    public void applyTheme(Theme theme) {
        _shell.setFont(theme.SHELL_FONT);
        _rotate.setFont(theme.DEFAULT_FONT);
        _sendNewSelected.setFont(theme.DEFAULT_FONT);
        _sendNewSelectedList.setFont(theme.DEFAULT_FONT);
        _sendNewSelectedAdd.setFont(theme.DEFAULT_FONT);
        _sendNewSelectedDel.setFont(theme.DEFAULT_FONT);
        _sendNewPBE.setFont(theme.DEFAULT_FONT);
        _sendNewPBEPassLabel.setFont(theme.DEFAULT_FONT);
        _sendNewPBEPass.setFont(theme.DEFAULT_FONT);
        _sendNewPBEPromptLabel.setFont(theme.DEFAULT_FONT);
        _sendNewPBEPrompt.setFont(theme.DEFAULT_FONT);
        _ok.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
    }
    
    private static final String T_SHELL = "syndie.gui.viewforumauthreply.shell";
    
    private static final String T_ROTATE = "syndie.gui.viewforumauthreply.rotate";
    private static final String T_SEND_NEW_SELECTED = "syndie.gui.viewforumauthreply.send.new.selected";
    private static final String T_SEND_NEW_SELECTED_ADD = "syndie.gui.viewforumauthreply.send.new.selected.add";
    private static final String T_SEND_NEW_SELECTED_DEL = "syndie.gui.viewforumauthreply.send.new.selected.del";
    private static final String T_SEND_NEW_PBE = "syndie.gui.viewforumauthreply.pbe";
    private static final String T_SEND_NEW_PBE_PASS_LABEL = "syndie.gui.viewforumauthreply.pbe.pass.label";
    private static final String T_SEND_NEW_PBE_PROMPT_LABEL = "syndie.gui.viewforumauthreply.pbe.prompt.label";
    private static final String T_OK = "syndie.gui.viewforumauthreply.ok";
    private static final String T_CANCEL = "syndie.gui.viewforumauthreply.cancel";
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText(T_SHELL, "Read forum feedback"));
        
        _rotate.setText(registry.getText(T_ROTATE, "Rotate the reply key"));
        
        _sendNewSelected.setText(registry.getText(T_SEND_NEW_SELECTED, "Send the new key to the administrators of the selected forums:"));
        _sendNewSelectedAdd.setText(registry.getText(T_SEND_NEW_SELECTED_ADD, "Add"));
        _sendNewSelectedDel.setText(registry.getText(T_SEND_NEW_SELECTED_DEL, "Delete"));
        
        _sendNewPBE.setText(registry.getText(T_SEND_NEW_PBE, "Post the new key in a passphrase protected message to the forum"));
        _sendNewPBEPassLabel.setText(registry.getText(T_SEND_NEW_PBE_PASS_LABEL, "Passphrase:"));
        _sendNewPBEPromptLabel.setText(registry.getText(T_SEND_NEW_PBE_PROMPT_LABEL, "Prompt:"));
        
        _ok.setText(registry.getText(T_OK, "OK"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel"));
    }
}
