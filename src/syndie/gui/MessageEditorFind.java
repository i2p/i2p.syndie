package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

class MessageEditorFind implements Translatable, Themeable {
    private ThemeRegistry _themeRegistry;
    private TranslationRegistry _translationRegistry;
    private MessageEditor _editor;
    private Shell _findShell;
    private Label _findTextLabel;
    private Text _findText;
    private Label _findReplaceLabel;
    private Text _findReplace;
    private Button _findMatchCase;
    private Button _findWrapAround;
    private Button _findBackwards;
    private Button _findNext;
    private Button _close;
    private Button _replace;
    private Button _replaceAll;
    
    public MessageEditorFind(ThemeRegistry themes, TranslationRegistry trans, MessageEditor editor) {
        _editor = editor;
        _themeRegistry = themes;
        _translationRegistry = trans;
        initComponents();
    }
    
    public void hide() { _findShell.setVisible(false); }
    public void open() {
        _findText.setText("");
        _findReplace.setText("");
        _findBackwards.setSelection(false);
        _findMatchCase.setSelection(false);
        _findWrapAround.setSelection(false);
        _findShell.pack();
        _findShell.open();
        _findText.forceFocus();
    }
    public void dispose() {
        _translationRegistry.unregister(this);
        _translationRegistry.unregister(this);
        _findShell.dispose();
    }
    
    /** current search term used */
    public String getSearchTerm() { return _findText.getText(); }
    /** current replacement for the search term used */
    public String getSearchReplacement() { return _findReplace.getText(); }
    /** are searches case sensitive? */
    public boolean getSearchCaseSensitive() { return _findMatchCase.getSelection(); }
    /** do we want to search backwards? */
    public boolean getSearchBackwards() { return _findBackwards.getSelection(); }
    /** do we want to search around the end/beginning of the page? */
    public boolean getSearchWrap() { return _findWrapAround.getSelection(); }
    
    private void initComponents() {
        _findShell = new Shell(_editor.getPageRoot().getShell(), SWT.DIALOG_TRIM);
        GridLayout gl = new GridLayout(2, false);
        _findShell.setLayout(gl);
    
        _findTextLabel = new Label(_findShell, SWT.NONE);
        _findTextLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _findText = new Text(_findShell, SWT.BORDER | SWT.SINGLE);
        _findText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _findReplaceLabel = new Label(_findShell, SWT.NONE);
        _findReplaceLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _findReplace = new Text(_findShell, SWT.BORDER | SWT.SINGLE);
        _findReplace.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _findMatchCase = new Button(_findShell, SWT.CHECK | SWT.LEFT);
        _findMatchCase.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _findWrapAround = new Button(_findShell, SWT.CHECK | SWT.LEFT);
        _findWrapAround.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _findBackwards = new Button(_findShell, SWT.CHECK | SWT.LEFT);
        _findBackwards.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        Composite actionRow = new Composite(_findShell, SWT.NONE);
        actionRow.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        actionRow.setLayout(new FillLayout(SWT.HORIZONTAL));
        
        _findNext = new Button(actionRow, SWT.PUSH);
        _findNext.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.findNext(); }
        });
        
        _close = new Button(actionRow, SWT.PUSH);
        _close.addSelectionListener(new FireSelectionListener() {
            public void fire() { cancelFind(); }
        });
        
        _replace = new Button(actionRow, SWT.PUSH);
        _replace.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.findReplace(); }
        });
        
        _replaceAll = new Button(actionRow, SWT.PUSH);
        _replaceAll.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.findReplaceAll(); }
        });

        _findText.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) {
                    _editor.findNext();
                    _findNext.forceFocus();
                    evt.doit = false;
                }
            }
        });
        _findReplace.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) {
                    _editor.findReplace();
                    _replace.forceFocus();
                    evt.doit = false;
                }
            }
        });
        
        _findShell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; cancelFind(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        _translationRegistry.register(this);
        _translationRegistry.register(this);
    }
    
    private void cancelFind() {
        _findText.setText("");
        _editor.cancelFind();
    }
 
    public void applyTheme(Theme theme) {
        _findShell.setFont(theme.SHELL_FONT);
        _findTextLabel.setFont(theme.DEFAULT_FONT);
        _findText.setFont(theme.DEFAULT_FONT);
        _findReplaceLabel.setFont(theme.DEFAULT_FONT);
        _findReplace.setFont(theme.DEFAULT_FONT);
        _findMatchCase.setFont(theme.DEFAULT_FONT);
        _findWrapAround.setFont(theme.DEFAULT_FONT);
        _findBackwards.setFont(theme.DEFAULT_FONT);
        _findNext.setFont(theme.BUTTON_FONT);
        _close.setFont(theme.BUTTON_FONT);
        _replace.setFont(theme.BUTTON_FONT);
        _replaceAll.setFont(theme.BUTTON_FONT);
        
        _findShell.pack();
    }
    
    
    public void translate(TranslationRegistry registry) {
        _findShell.setText(registry.getText("Find"));
        _findTextLabel.setText(registry.getText("Find what") + ": ");
        _findReplaceLabel.setText(registry.getText("Replace with") + ": ");
        _findMatchCase.setText(registry.getText("Match case"));
        _findWrapAround.setText(registry.getText("Wrap around"));
        _findBackwards.setText(registry.getText("Backwards"));
        _findNext.setText(registry.getText("Find next"));
        _findNext.setToolTipText(registry.getText("Find the next occurrence of the word"));
        _close.setText(registry.getText("Close"));
        _close.setToolTipText(registry.getText("Finish searching"));
        _replace.setText(registry.getText("Replace"));
        _replace.setToolTipText(registry.getText("Replace the current occurrence of the word"));
        _replaceAll.setText(registry.getText("Replace all"));
        _replaceAll.setToolTipText(registry.getText("Replace all remaining occurrences of the word"));
    }
}
