package syndie.gui;

import java.util.ArrayList;
import java.util.Iterator;
import net.i2p.data.Hash;
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
import syndie.data.ChannelInfo;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 *
 */
class ManageForumAuthManage extends BaseComponent implements Themeable, Translatable {
    private ManageForum _manage;
    
    private Shell _shell;
    private Composite _root;
    
    private List _selectedList;
    private Button _selectedAdd;
    private Button _selectedDel;
    private Button _sendNew;
    private Label _sendNewLabel;
    private List _sendNewList;
    private Button _sendNewAdd;
    private Button _sendNewDel;
    private Button _sendNewPBE;
    private Label _sendNewPBEPassLabel;
    private Text _sendNewPBEPass;
    private Label _sendNewPBEPromptLabel;
    private Text _sendNewPBEPrompt;
    private Button _ok;
    private Button _cancel;
    
    private ReferenceChooserPopup _chooser;
    /** channels (Hash) allowed to manage, ordered by the _selectedList */
    private ArrayList _selectedForums;
    /** channels (Hash) to receive the new management key, ordered by the _sendNewList */
    private ArrayList _sendNewForums;
    
    public ManageForumAuthManage(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, ManageForum manage) {
        super(client, ui, themes, trans);
        _manage = manage;
        _selectedForums = new ArrayList();
        _sendNewForums = new ArrayList();
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
        
        _selectedList = new List(_root, SWT.MULTI | SWT.BORDER | SWT.V_SCROLL);
        _selectedList.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 2));
        
        _selectedAdd = new Button(_root, SWT.PUSH);
        _selectedAdd.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        
        _selectedDel = new Button(_root, SWT.PUSH);
        _selectedDel.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        
        _sendNew = new Button(_root, SWT.CHECK | SWT.WRAP);
        _sendNew.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        
        _sendNewLabel = new Label(_root, SWT.WRAP);
        _sendNewLabel.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        
        _sendNewList = new List(_root, SWT.MULTI | SWT.BORDER | SWT.V_SCROLL);
        _sendNewList.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 2));
        
        _sendNewAdd = new Button(_root, SWT.PUSH);
        _sendNewAdd.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false));
        
        _sendNewDel = new Button(_root, SWT.PUSH);
        _sendNewDel.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false));
        
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
        
        _sendNew.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { refreshEnabled(); }
            public void widgetSelected(SelectionEvent selectionEvent) { refreshEnabled(); }
        });
        _sendNewPBE.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { refreshEnabled(); }
            public void widgetSelected(SelectionEvent selectionEvent) { refreshEnabled(); }
        });
        
        _selectedAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addAuthorizedForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addAuthorizedForum(); }
        });
        _selectedDel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { delAuthorizedForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { delAuthorizedForum(); }
        });
        _sendNewAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addNewForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addNewForum(); }
        });
        _sendNewDel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { delNewForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { delNewForum(); }
        });

        loadData();
        refreshEnabled();
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    private void loadData() {
        ChannelInfo info = _manage.getChannelInfo();
        
        if (info != null) {
            for (Iterator iter = info.getAuthorizedManagerHashes().iterator(); iter.hasNext(); ) {
                Hash scope = (Hash)iter.next();
                _selectedForums.add(scope);
                String name = _client.getChannelName(scope);
                if (name != null)
                    _selectedList.add(name + " [" + scope.toBase64().substring(0,6) + "]");
                else
                    _selectedList.add("[" + scope.toBase64().substring(0,6) + "]");
            }
        }
    }
    
    public void show() { _shell.pack(); _shell.open(); }
    private void hide() { _shell.setVisible(false); }
    private void ok() { _manage.modified(); hide(); }
    public ArrayList getAuthorizedManagers() { return _selectedForums; }
    public boolean getNewIdentity() { return _sendNew.getSelection(); }
    public ArrayList getSendNewExplicit() { return _sendNewForums; }
    public boolean getPostPBE() { return _sendNewPBE.getSelection(); }
    public String getSendPassphrase() { return getPostPBE() ? _sendNewPBEPass.getText().trim() : null; }
    public String getSendPassphrasePrompt() { return getPostPBE() ? _sendNewPBEPrompt.getText().trim() : null; }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        if (!_shell.isDisposed())
            _shell.dispose();
        if (_chooser != null)
            _chooser.dispose();
    }
    
    private void refreshEnabled() {
        _sendNewLabel.setEnabled(_sendNew.getSelection());
        _sendNewAdd.setEnabled(_sendNew.getSelection());
        _sendNewDel.setEnabled(_sendNew.getSelection());
        _sendNewList.setEnabled(_sendNew.getSelection());
        _sendNewPBE.setEnabled(_sendNew.getSelection());
        _sendNewPBEPass.setEnabled(_sendNew.getSelection() && _sendNewPBE.getSelection());
        _sendNewPBEPassLabel.setEnabled(_sendNew.getSelection() && _sendNewPBE.getSelection());
        _sendNewPBEPrompt.setEnabled(_sendNew.getSelection() && _sendNewPBE.getSelection());
        _sendNewPBEPromptLabel.setEnabled(_sendNew.getSelection() && _sendNewPBE.getSelection());
    }
    
    private void addAuthorizedForum() {
        if (_chooser == null)
            _chooser = ComponentBuilder.instance().createReferenceChooserPopup(_shell);
        _chooser.setListener(new ReferenceChooserTree.AcceptanceListener() {
            public void referenceAccepted(SyndieURI uri) {
                if ( (uri != null) && (uri.getScope() != null) ) {
                    if (!_selectedForums.contains(uri.getScope())) {
                        _selectedForums.add(uri.getScope());
                        String name = _client.getChannelName(uri.getScope());
                        if (name != null)
                            _selectedList.add(name + " [" + uri.getScope().toBase64().substring(0,6) + "]");
                        else
                            _selectedList.add("[" + uri.getScope().toBase64().substring(0,6) + "]");
                    }
                }
            }
            public void referenceChoiceAborted() {}
        });
        _chooser.show();
    }
    private void delAuthorizedForum() {
        int idx = _selectedList.getSelectionIndex();
        if ( (idx >= 0) && (idx <= _selectedForums.size()) ) {
            _selectedForums.remove(idx);
            _selectedList.remove(idx);
        }
    }
    
    private void addNewForum() {
        if (_chooser == null)
            _chooser = ComponentBuilder.instance().createReferenceChooserPopup(_shell);
        _chooser.setListener(new ReferenceChooserTree.AcceptanceListener() {
            public void referenceAccepted(SyndieURI uri) {
                if ( (uri != null) && (uri.getScope() != null) ) {
                    if (!_sendNewForums.contains(uri.getScope())) {
                        _sendNewForums.add(uri.getScope());
                        String name = _client.getChannelName(uri.getScope());
                        if (name != null)
                            _sendNewList.add(name + " [" + uri.getScope().toBase64().substring(0,6) + "]");
                        else
                            _sendNewList.add("[" + uri.getScope().toBase64().substring(0,6) + "]");
                    }
                }
            }
            public void referenceChoiceAborted() {}
        });
        _chooser.show();
    }
    private void delNewForum() {
        int idx = _sendNewList.getSelectionIndex();
        if ( (idx >= 0) && (idx <= _sendNewForums.size()) ) {
            _sendNewForums.remove(idx);
            _sendNewList.remove(idx);
        }
    }
    
    public void applyTheme(Theme theme) {
        _shell.setFont(theme.SHELL_FONT);
        _selectedList.setFont(theme.DEFAULT_FONT);
        _selectedAdd.setFont(theme.DEFAULT_FONT);
        _selectedDel.setFont(theme.DEFAULT_FONT);
        _sendNew.setFont(theme.DEFAULT_FONT);
        _sendNewLabel.setFont(theme.DEFAULT_FONT);
        _sendNewList.setFont(theme.DEFAULT_FONT);
        _sendNewAdd.setFont(theme.DEFAULT_FONT);
        _sendNewDel.setFont(theme.DEFAULT_FONT);
        _sendNewPBE.setFont(theme.DEFAULT_FONT);
        _sendNewPBEPassLabel.setFont(theme.DEFAULT_FONT);
        _sendNewPBEPass.setFont(theme.DEFAULT_FONT);
        _sendNewPBEPromptLabel.setFont(theme.DEFAULT_FONT);
        _sendNewPBEPrompt.setFont(theme.DEFAULT_FONT);
        _ok.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
    }
    
    
        
        
        
    
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText("Who can manage the forum?"));
        
        _selectedAdd.setText(registry.getText("Add"));
        _selectedDel.setText(registry.getText("Delete"));
        
        _sendNew.setText(registry.getText("Create a new identity and allow it to manage the forum"));
        _sendNewLabel.setText(registry.getText("Send the new identity key to the administrators of the selected forums") + ':');
        _sendNewAdd.setText(registry.getText("Add"));
        _sendNewDel.setText(registry.getText("Delete"));
        _sendNewPBE.setText(registry.getText("Post the new identity's key in a passphrase protected message to the forum"));
        _sendNewPBEPassLabel.setText(registry.getText("Passphrase") + ':');
        _sendNewPBEPromptLabel.setText(registry.getText("Prompt") + ':');
        
        _ok.setText(registry.getText("OK"));
        _cancel.setText(registry.getText("Cancel"));
    }
}
