package syndie.gui;

import java.util.ArrayList;
import java.util.List;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

class MessageEditorSpell implements Themeable, Translatable {
    private ThemeRegistry _themeRegistry;
    private TranslationRegistry _translationRegistry;
    private MessageEditor _editor;
    private Shell _spellShell;
    private StyledText _spellContext;
    private Label _spellWordLabel;
    private Text _spellWord;
    private Label _spellSuggestionsLabel;
    private Combo _spellSuggestions;
    private Button _spellReplace;
    private Button _spellReplaceAll;
    private Button _spellIgnore;
    private Button _spellIgnoreAll;
    private Button _spellAdd;
    private Button _spellCancel;
    /** list of words we are ignoring for the current spellcheck iteration */
    private ArrayList _spellIgnoreAllList;
    
    public MessageEditorSpell(ThemeRegistry themes, TranslationRegistry trans, MessageEditor editor) {
        _themeRegistry = themes;
        _translationRegistry = trans;
        _editor = editor;
        initComponents();
    }
 
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        _spellShell.dispose();
    }
    
    public String getSpellWordOrig() {
        if (_spellShell.isDisposed()) return null;
        return _spellWord.getText().trim();
    }
    public String getSuggestion() { 
        if (_spellShell.isDisposed()) return null;
        return _spellSuggestions.getText().trim();
    }
    public List getIgnoreAllList() { return _spellIgnoreAllList; }
    public void updateSuggestions(ArrayList suggestions, String lineText, String word) {
        if (_spellShell.isDisposed()) return;
        _spellWord.setText(word);
        _spellSuggestions.removeAll();
        for (int i = 0; i < suggestions.size(); i++)
            _spellSuggestions.add((String)suggestions.get(i));
        _spellSuggestions.select(0);
        _spellContext.setText(lineText);
    }
    public void showSpell(boolean wordSet) {
        if (_spellShell.isDisposed()) return;
        if (wordSet) {
            _spellContext.setLineBackground(0, 1, null);
            _spellWord.setEnabled(true);
            _spellSuggestions.setEnabled(true);
            _spellAdd.setEnabled(false); // todo: user-specific dictionary
            _spellCancel.setEnabled(true);
            _spellCancel.setText(_translationRegistry.getText("cancel"));
            _spellIgnore.setEnabled(true);
            _spellIgnoreAll.setEnabled(true);
            _spellReplace.setEnabled(true);
            _spellReplaceAll.setEnabled(true);
        } else {
            _spellContext.setText(_translationRegistry.getText("End of content reached"));
            _spellContext.setLineBackground(0, 1, ColorUtil.getColor("red", null));
            _spellWord.setText("");
            _spellWord.setEnabled(false);
            _spellSuggestions.removeAll();
            _spellSuggestions.setEnabled(false);
            _spellAdd.setEnabled(false);
            _spellCancel.setEnabled(true);
            _spellCancel.setText(_translationRegistry.getText("ok"));
            _spellIgnore.setEnabled(false);
            _spellIgnoreAll.setEnabled(false);
            _spellReplace.setEnabled(false);
            _spellReplaceAll.setEnabled(false);
        }
        _spellShell.open();
    }
    
    private void initComponents() {
        _spellShell = new Shell(_editor.getPageRoot().getShell(), SWT.DIALOG_TRIM);
        GridLayout gl = new GridLayout(2, false);
        _spellShell.setLayout(gl);
        
        _spellIgnoreAllList = new ArrayList();
        
        _spellContext = new StyledText(_spellShell, SWT.MULTI | SWT.WRAP | SWT.BORDER | SWT.READ_ONLY);
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalSpan = 2;
        _spellContext.setLayoutData(gd);
        
        _spellWordLabel = new Label(_spellShell, SWT.NONE);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = false;
        _spellWordLabel.setLayoutData(gd);
        _spellWord = new Text(_spellShell, SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE | SWT.LEFT);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        _spellWord.setLayoutData(gd);

        _spellSuggestionsLabel = new Label(_spellShell, SWT.NONE);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = false;
        _spellSuggestionsLabel.setLayoutData(gd);
        _spellSuggestions = new Combo(_spellShell, SWT.DROP_DOWN);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        _spellSuggestions.setLayoutData(gd);
        
        Composite actionLine = new Composite(_spellShell, SWT.NONE);
        actionLine.setLayout(new FillLayout(SWT.HORIZONTAL));
        _spellReplace = new Button(actionLine, SWT.PUSH);
        _spellReplaceAll = new Button(actionLine, SWT.PUSH);
        _spellIgnore = new Button(actionLine, SWT.PUSH);
        _spellIgnoreAll = new Button(actionLine, SWT.PUSH);
        _spellAdd = new Button(actionLine, SWT.PUSH);
        _spellCancel = new Button(actionLine, SWT.PUSH);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalSpan = 2;
        actionLine.setLayoutData(gd);
        
        _spellIgnore.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.spellIgnore(); }
        });
        _spellIgnoreAll.addSelectionListener(new FireSelectionListener() {
            public void fire() { _spellIgnoreAllList.add(_spellWord.getText().trim().toLowerCase()); _editor.spellIgnore(); }
        });
        _spellReplace.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.spellReplaceWord(false); _editor.spellNext(); }
        });
        _spellReplaceAll.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.spellReplaceWord(true); _editor.spellNext(); }
        });

        _spellCancel.addSelectionListener(new FireSelectionListener() {
            public void fire() { cancelSpell(); }
        });
        
        _spellShell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) {
                evt.doit = false;
                cancelSpell();
            }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    void resetSpellcheck() {
        _spellIgnoreAllList.clear();
        _editor.resetSpellcheck();
    }
    void cancelSpell() {
        resetSpellcheck();
        if (_spellShell.isDisposed()) return;
        _spellShell.setVisible(false); 
    }
    
    public void applyTheme(Theme theme) {
        _spellShell.setFont(theme.SHELL_FONT);
        _spellWordLabel.setFont(theme.DEFAULT_FONT);
        _spellWord.setFont(theme.DEFAULT_FONT);
        _spellSuggestionsLabel.setFont(theme.DEFAULT_FONT);
        _spellSuggestions.setFont(theme.DEFAULT_FONT);
        _spellReplace.setFont(theme.BUTTON_FONT);
        _spellReplaceAll.setFont(theme.BUTTON_FONT);
        _spellIgnore.setFont(theme.BUTTON_FONT);
        _spellIgnoreAll.setFont(theme.BUTTON_FONT);
        _spellAdd.setFont(theme.BUTTON_FONT);
        _spellCancel.setFont(theme.BUTTON_FONT);
        _spellShell.pack();
    }
    
 
    
    public void translate(TranslationRegistry registry) {
        _spellShell.setText(registry.getText("Spell checker"));
        _spellWordLabel.setText(registry.getText("Word: "));
        _spellSuggestionsLabel.setText(registry.getText("Suggestions: "));
        _spellReplace.setText(registry.getText("replace"));
        _spellReplaceAll.setText(registry.getText("replace all"));
        _spellIgnore.setText(registry.getText("ignore"));
        _spellIgnoreAll.setText(registry.getText("ignore all"));
        _spellAdd.setText(registry.getText("add"));
        _spellCancel.setText(registry.getText("cancel"));
    }
}
