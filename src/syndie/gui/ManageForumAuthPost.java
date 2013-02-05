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
class ManageForumAuthPost extends BaseComponent implements Themeable, Translatable {
    private ManageForum _manage;
    
    private Shell _shell;
    private Composite _root;
    
    private Button _choiceAnyone;
    private Button _choiceAllowed;
    private Button _choiceReplies;
    
    private Group _selected;
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
    /** channels (Hash) allowed to post, ordered by the _selectedList */
    private ArrayList _selectedForums;
    /** channels (Hash) to receive the new posting key, ordered by the _sendNewList */
    private ArrayList _sendNewForums;
    
    public ManageForumAuthPost(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, ManageForum manage) {
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
        _root.setLayout(new GridLayout(1, false));
        
        _choiceAnyone = new Button(_root, SWT.RADIO | SWT.WRAP);
        _choiceAnyone.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _choiceReplies = new Button(_root, SWT.RADIO | SWT.WRAP);
        _choiceReplies.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _choiceAllowed = new Button(_root, SWT.RADIO | SWT.WRAP);
        _choiceAllowed.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _selected = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _selected.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _selected.setLayout(new GridLayout(4, false));
        
        _selectedList = new List(_selected, SWT.MULTI | SWT.BORDER | SWT.V_SCROLL);
        _selectedList.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 2));
        
        _selectedAdd = new Button(_selected, SWT.PUSH);
        _selectedAdd.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        
        _selectedDel = new Button(_selected, SWT.PUSH);
        _selectedDel.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        
        _sendNew = new Button(_selected, SWT.CHECK | SWT.WRAP);
        _sendNew.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        
        _sendNewLabel = new Label(_selected, SWT.WRAP);
        _sendNewLabel.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        
        _sendNewList = new List(_selected, SWT.MULTI | SWT.BORDER | SWT.V_SCROLL);
        _sendNewList.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 2));
        
        _sendNewAdd = new Button(_selected, SWT.PUSH);
        _sendNewAdd.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false));
        
        _sendNewDel = new Button(_selected, SWT.PUSH);
        _sendNewDel.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false));
        
        _sendNewPBE = new Button(_selected, SWT.CHECK | SWT.WRAP);
        _sendNewPBE.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        
        _sendNewPBEPassLabel = new Label(_selected, SWT.WRAP);
        _sendNewPBEPassLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _sendNewPBEPass = new Text(_selected, SWT.SINGLE | SWT.BORDER);
        _sendNewPBEPass.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _sendNewPBEPromptLabel = new Label(_selected, SWT.WRAP);
        _sendNewPBEPromptLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _sendNewPBEPrompt = new Text(_selected, SWT.SINGLE | SWT.BORDER);
        _sendNewPBEPrompt.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        Composite actions = new Composite(_root, SWT.NONE);
        actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        actions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
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
        _choiceReplies.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickChoiceReplies(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickChoiceReplies(); }
        });
        
        _ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { ok(); }
            public void widgetSelected(SelectionEvent selectionEvent) { ok(); }
        });
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { hide(); }
            public void widgetSelected(SelectionEvent selectionEvent) { hide(); }
        });
        
        _sendNew.addSelectionListener(new SelectionListener() {
            // pickChoiceAllowed refreshes the enable state of the related fields
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickChoiceAllowed(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickChoiceAllowed(); }
        });
        _sendNewPBE.addSelectionListener(new SelectionListener() {
            // pickChoiceAllowed refreshes the enable state of the related fields
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickChoiceAllowed(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickChoiceAllowed(); }
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
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    private void loadData() {
        ChannelInfo info = _manage.getChannelInfo();
        
        if (info != null) {
            if (info.getAllowPublicPosts())
                pickChoiceAnyone();
            else if (info.getAllowPublicReplies())
                pickChoiceReplies();
            else
                pickChoiceAllowed();
            
            // managers are implicit, and managed via ViewForumAuthManage
            /*
            for (Iterator iter = info.getAuthorizedManagerHashes().iterator(); iter.hasNext(); ) {
                Hash scope = (Hash)iter.next();
                _selectedForums.add(scope);
                String name = _browser.getClient().getChannelName(scope);
                if (name != null)
                    _selectedList.add(name + " [" + scope.toBase64().substring(0,6) + "]");
                else
                    _selectedList.add("[" + scope.toBase64().substring(0,6) + "]");
            }
             */
            for (Iterator iter = info.getAuthorizedPosterHashes().iterator(); iter.hasNext(); ) {
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
    
    public boolean getAllowPublicReplies() { return _choiceReplies.getSelection(); }
    public boolean getAllowPublicPosts() { return _choiceAnyone.getSelection(); }
    public ArrayList getAuthorizedPosters() {
        return getAllowPublicPosts() ? new ArrayList() : _selectedForums;
    }
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
    
    private void pickChoiceAnyone() {
        _choiceAnyone.setSelection(true);
        _choiceAllowed.setSelection(false);
        _choiceReplies.setSelection(false);
        _selected.setEnabled(false);
        _selectedList.setEnabled(false);
        _selectedAdd.setEnabled(false);
        _selectedDel.setEnabled(false);
        _sendNew.setEnabled(false);
        _sendNewLabel.setEnabled(false);
        _sendNewAdd.setEnabled(false);
        _sendNewDel.setEnabled(false);
        _sendNewList.setEnabled(false);
        _sendNewPBE.setEnabled(false);
        _sendNewPBEPass.setEnabled(false);
        _sendNewPBEPassLabel.setEnabled(false);
        _sendNewPBEPrompt.setEnabled(false);
        _sendNewPBEPromptLabel.setEnabled(false);
    }
    private void pickChoiceAllowed() {
        _choiceAnyone.setSelection(false);
        _choiceAllowed.setSelection(true);
        _choiceReplies.setSelection(false);
        _selected.setEnabled(true);
        _selectedList.setEnabled(true);
        _selectedAdd.setEnabled(true);
        _selectedDel.setEnabled(true);
        _sendNew.setEnabled(true);
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
    private void pickChoiceReplies() {
        _choiceAnyone.setSelection(false);
        _choiceAllowed.setSelection(false);
        _choiceReplies.setSelection(true);
        _selected.setEnabled(true);
        _selectedList.setEnabled(true);
        _selectedAdd.setEnabled(true);
        _selectedDel.setEnabled(true);
        _sendNew.setEnabled(true);
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
        _choiceAnyone.setFont(theme.DEFAULT_FONT);
        _choiceAllowed.setFont(theme.DEFAULT_FONT);
        _choiceReplies.setFont(theme.DEFAULT_FONT);
        _selected.setFont(theme.DEFAULT_FONT);
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
        _shell.setText(registry.getText("Who can post in the forum?"));
        
        _choiceAnyone.setText(registry.getText("Anyone can post"));
        _choiceAllowed.setText(registry.getText("Only authorized users can post or reply"));
        _choiceReplies.setText(registry.getText("Anyone can reply to an authorized post"));
        
        _selected.setText(registry.getText("What identities are authorized?"));
        _selectedAdd.setText(registry.getText("Add"));
        _selectedDel.setText(registry.getText("Delete"));
        
        _sendNew.setText(registry.getText("Create a new identity and allow it to post"));
        _sendNewLabel.setText(registry.getText("Send the new identity's key to the administrators of the selected forums") + ':');
        _sendNewAdd.setText(registry.getText("Add"));
        _sendNewDel.setText(registry.getText("Delete"));
        _sendNewPBE.setText(registry.getText("Post the new identity's key in a passphrase protected message to the forum"));
        _sendNewPBEPassLabel.setText(registry.getText("Passphrase") + ':');
        _sendNewPBEPromptLabel.setText(registry.getText("Prompt") + ':');
        
        _ok.setText(registry.getText("OK"));
        _cancel.setText(registry.getText("Cancel"));
    }
}
