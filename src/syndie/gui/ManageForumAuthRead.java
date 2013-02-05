package syndie.gui;

import java.util.ArrayList;
import java.util.Set;
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
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.NymKey;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 *
 */
class ManageForumAuthRead extends BaseComponent implements Themeable, Translatable {
    private ManageForum _manage;
    
    private Shell _shell;
    private Composite _root;
    
    private Button _choiceAnyone;
    private Button _choiceAnyoneRetroactive;
    private Button _choiceAllowed;
    private Button _choiceAllowedRotate;
    private Button _choiceNew;
    
    private Button _sendSelected;
    private List _sendSelectedList;
    private Button _sendSelectedAdd;
    private Button _sendSelectedDel;
    private Button _sendPBE;
    private Label _sendPBEPassLabel;
    private Text _sendPBEPass;
    private Label _sendPBEPromptLabel;
    private Text _sendPBEPrompt;
    private Button _ok;
    private Button _cancel;
    
    private ReferenceChooserPopup _chooser;
    /** channels (Hash) to receive posts, ordered by the _sendSelectedList */
    private ArrayList _sendSelectedForums;
    
    public ManageForumAuthRead(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, ManageForum manage) {
        super(client, ui, themes, trans);
        _manage = manage;
        _sendSelectedForums = new ArrayList();
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
        
        _choiceAnyone = new Button(_root, SWT.RADIO | SWT.WRAP);
        _choiceAnyone.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        
        _choiceAnyoneRetroactive = new Button(_root, SWT.CHECK);
        _choiceAnyoneRetroactive.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _choiceAllowed = new Button(_root, SWT.RADIO | SWT.WRAP);
        _choiceAllowed.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        
        _choiceAllowedRotate = new Button(_root, SWT.CHECK);
        _choiceAllowedRotate.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _choiceNew = new Button(_root, SWT.RADIO | SWT.WRAP);
        _choiceNew.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        
        _sendSelected = new Button(_root, SWT.CHECK | SWT.WRAP);
        _sendSelected.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        
        _sendSelectedList = new List(_root, SWT.MULTI | SWT.BORDER | SWT.V_SCROLL);
        _sendSelectedList.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 2));
        
        _sendSelectedAdd = new Button(_root, SWT.PUSH);
        _sendSelectedAdd.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        
        _sendSelectedDel = new Button(_root, SWT.PUSH);
        _sendSelectedDel.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        
        _sendPBE = new Button(_root, SWT.CHECK | SWT.WRAP);
        _sendPBE.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        
        _sendPBEPassLabel = new Label(_root, SWT.WRAP);
        _sendPBEPassLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _sendPBEPass = new Text(_root, SWT.SINGLE | SWT.BORDER);
        _sendPBEPass.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _sendPBEPromptLabel = new Label(_root, SWT.WRAP);
        _sendPBEPromptLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _sendPBEPrompt = new Text(_root, SWT.SINGLE | SWT.BORDER);
        _sendPBEPrompt.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        Composite actions = new Composite(_root, SWT.NONE);
        actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        actions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        
        _cancel = new Button(actions, SWT.PUSH);
        _ok = new Button(actions, SWT.PUSH);
        
        _choiceAnyone.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickChoiceAnyone(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickChoiceAnyone(); }
        });
        _choiceAllowed.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickChoiceAllowed(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickChoiceAllowed(); }
        });
        _choiceNew.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickChoiceNew(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickChoiceNew(); }
        });
        
        _ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { ok(); }
            public void widgetSelected(SelectionEvent selectionEvent) { ok(); }
        });
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { hide(); }
            public void widgetSelected(SelectionEvent selectionEvent) { hide(); }
        });
        
        _sendPBE.addSelectionListener(new SelectionListener() {
            // pickChoiceNew refreshes the enable state of the pbe fields
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickSendPBE(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickSendPBE(); }
        });
        _sendSelected.addSelectionListener(new SelectionListener() {
            // pickChoiceNew refreshes the enable state of the selected fields
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickSendSelected(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickSendSelected(); }
        });
        
        _sendSelectedAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addForum(); }
        });
        _sendSelectedDel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { delForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { delForum(); }
        });

        loadData();
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    public void show() { _shell.pack(); _shell.open(); }
    private void hide() { _shell.setVisible(false); }
    private void ok() { _manage.modified(); hide(); }
    
    private void loadData() {
        ChannelInfo info = _manage.getChannelInfo();
        if (info != null) {
            Set readKeys = info.getReadKeys();
            if ( (readKeys != null) && (readKeys.size() > 0) ) {
                if (info.getReadKeysArePublic())
                    pickChoiceAnyone();
                else
                    pickChoiceAllowed();
            } else {
                // there are no read keys... must be public
                pickChoiceAnyone();
            }
        } else {
            // new channel, assume publicly readable
            pickChoiceAnyone();
        }
    }
    
    public boolean getEncryptMetadata() { return !_choiceAnyone.getSelection(); }
    public boolean getReadKeyPublicRetroactive() { return _choiceAnyone.getSelection() && _choiceAnyoneRetroactive.getSelection(); }
    public boolean getNewKey() { return _choiceNew.getSelection(); }
    public ArrayList getSendExplicit() { return _sendSelectedForums; }
    public boolean getPostPBE() { return _sendPBE.getSelection(); }
    public String getSendPassphrase() { return getPostPBE() ? _sendPBEPass.getText().trim() : null; }
    public String getSendPassphrasePrompt() { return getPostPBE() ? _sendPBEPrompt.getText().trim() : null; }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        if (!_shell.isDisposed())
            _shell.dispose();
        if (_chooser != null)
            _chooser.dispose();
    }
    
    private void pickChoiceAnyone() {
        _choiceAnyone.setSelection(true);
        _choiceAnyoneRetroactive.setEnabled(true);
        _choiceAllowed.setSelection(false);
        _choiceNew.setSelection(false);
        _choiceAllowedRotate.setEnabled(false);
        _sendSelected.setEnabled(false);
        _sendSelectedList.setEnabled(false);
        _sendSelectedAdd.setEnabled(false);
        _sendSelectedDel.setEnabled(false);
        _sendPBE.setEnabled(false);
        _sendPBEPass.setEnabled(false);
        _sendPBEPassLabel.setEnabled(false);
        _sendPBEPrompt.setEnabled(false);
        _sendPBEPromptLabel.setEnabled(false);
    }
    private void pickChoiceAllowed() {
        _choiceAllowed.setSelection(true);
        _choiceAnyone.setSelection(false);
        _choiceAnyoneRetroactive.setEnabled(false);
        _choiceNew.setSelection(false);
        _choiceAllowedRotate.setEnabled(true);
        _sendSelected.setEnabled(true);
        _sendSelectedList.setEnabled(_sendSelected.getSelection());
        _sendSelectedAdd.setEnabled(_sendSelected.getSelection());
        _sendSelectedDel.setEnabled(_sendSelected.getSelection());
        _sendPBE.setEnabled(true);
        _sendPBEPass.setEnabled(_sendPBE.getSelection());
        _sendPBEPassLabel.setEnabled(_sendPBE.getSelection());
        _sendPBEPrompt.setEnabled(_sendPBE.getSelection());
        _sendPBEPromptLabel.setEnabled(_sendPBE.getSelection());
    }
    private void pickSendPBE() {
        _sendPBEPass.setEnabled(_sendPBE.getSelection());
        _sendPBEPassLabel.setEnabled(_sendPBE.getSelection());
        _sendPBEPrompt.setEnabled(_sendPBE.getSelection());
        _sendPBEPromptLabel.setEnabled(_sendPBE.getSelection());
    }
    private void pickSendSelected() {
        _sendSelectedList.setEnabled(_sendSelected.getSelection());
        _sendSelectedAdd.setEnabled(_sendSelected.getSelection());
        _sendSelectedDel.setEnabled(_sendSelected.getSelection());
    }
    private void pickChoiceNew() {
        _choiceNew.setSelection(true);
        _choiceAnyone.setSelection(false);
        _choiceAllowed.setSelection(false);
        _choiceAllowedRotate.setEnabled(false);
        _sendSelected.setEnabled(true);
        _sendSelectedList.setEnabled(_sendSelected.getSelection());
        _sendSelectedAdd.setEnabled(_sendSelected.getSelection());
        _sendSelectedDel.setEnabled(_sendSelected.getSelection());
        _sendPBE.setEnabled(true);
        _sendPBEPass.setEnabled(_sendPBE.getSelection());
        _sendPBEPassLabel.setEnabled(_sendPBE.getSelection());
        _sendPBEPrompt.setEnabled(_sendPBE.getSelection());
        _sendPBEPromptLabel.setEnabled(_sendPBE.getSelection());
    }
    
    private void addForum() {
        if (_chooser == null)
            _chooser = ComponentBuilder.instance().createReferenceChooserPopup(_shell);
        _chooser.setListener(new ReferenceChooserTree.AcceptanceListener() {
            public void referenceAccepted(SyndieURI uri) {
                if ( (uri != null) && (uri.getScope() != null) ) {
                    if (!_sendSelectedForums.contains(uri.getScope())) {
                        _sendSelectedForums.add(uri.getScope());
                        String name = _client.getChannelName(uri.getScope());
                        if (name != null)
                            _sendSelectedList.add(name + " [" + uri.getScope().toBase64().substring(0,6) + "]");
                        else
                            _sendSelectedList.add("[" + uri.getScope().toBase64().substring(0,6) + "]");
                    }
                }
            }
            public void referenceChoiceAborted() {}
        });
        _chooser.show();
    }
    private void delForum() {
        int idx = _sendSelectedList.getSelectionIndex();
        if ( (idx >= 0) && (idx <= _sendSelectedForums.size()) ) {
            _sendSelectedForums.remove(idx);
            _sendSelectedList.remove(idx);
        }
    }
    
    public void applyTheme(Theme theme) {
        _shell.setFont(theme.SHELL_FONT);
        _choiceAnyone.setFont(theme.DEFAULT_FONT);
        _choiceAnyoneRetroactive.setFont(theme.DEFAULT_FONT);
        _choiceAllowed.setFont(theme.DEFAULT_FONT);
        _choiceAllowedRotate.setFont(theme.DEFAULT_FONT);
        _choiceNew.setFont(theme.DEFAULT_FONT);
        _sendSelected.setFont(theme.DEFAULT_FONT);
        _sendSelectedList.setFont(theme.TABLE_FONT);
        _sendSelectedAdd.setFont(theme.BUTTON_FONT);
        _sendSelectedDel.setFont(theme.BUTTON_FONT);
        _sendPBE.setFont(theme.DEFAULT_FONT);
        _sendPBEPassLabel.setFont(theme.DEFAULT_FONT);
        _sendPBEPass.setFont(theme.DEFAULT_FONT);
        _sendPBEPromptLabel.setFont(theme.DEFAULT_FONT);
        _sendPBEPrompt.setFont(theme.DEFAULT_FONT);
        _ok.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
    }
    
    
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText("Who can read posts?"));
        
        _choiceAnyone.setText(registry.getText("Anyone can read posts"));
        _choiceAnyoneRetroactive.setText(registry.getText("Apply retroactively"));
        _choiceAllowed.setText(registry.getText("Those already allowed to read posts"));
        _choiceAllowedRotate.setText(registry.getText("Rotate the keys used"));
        _choiceNew.setText(registry.getText("Anyone who has a newly created key"));
        _sendSelected.setText(registry.getText("Send the key explicitly to the managers of the following forums") + ':');
        _sendSelectedAdd.setText(registry.getText("Add"));
        _sendSelectedDel.setText(registry.getText("Delete"));
        _sendPBE.setText(registry.getText("Post the key in a passphrase protected message to the forum"));
        _sendPBEPassLabel.setText(registry.getText("Passphrase") + ':');
        _sendPBEPromptLabel.setText(registry.getText("Prompt") + ':');
        _ok.setText(registry.getText("OK"));
        _cancel.setText(registry.getText("Cancel"));
    }
}
