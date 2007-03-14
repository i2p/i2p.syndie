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
 *
 */
class BookmarkEditorPopup extends BaseComponent implements BookmarkEditor.BookmarkEditorListener, Translatable, Themeable {
    private BookmarkControl _bookmarkControl;
    private Shell _parent;
    private Shell _shell;
    private BookmarkEditor _editor;
    
    public BookmarkEditorPopup(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, BookmarkControl bookmarkControl, Shell parent) {
        super(client, ui, themes, trans);
        _bookmarkControl = bookmarkControl;
        _parent = parent;
        initComponents();
    }
    
    private void initComponents() {
        _shell = new Shell(_parent, SWT.SHELL_TRIM | SWT.APPLICATION_MODAL);
        _shell.setLayout(new FillLayout());
        _editor = new BookmarkEditor(_client, _ui, _themeRegistry, _translationRegistry, _shell, this);
        _shell.pack();
        
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
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        _editor.dispose();
    }
    
    public void setBookmark(NymReferenceNode node) { _editor.setBookmark(node); }
    public void open() { 
        _shell.layout(true, true);
        _shell.setSize(_shell.computeSize(300, SWT.DEFAULT));
        _shell.open();
    }
    public void pickParent(boolean pick) { _editor.pickParent(pick); }
    public void pickOrder(boolean pick) { _editor.pickOrder(pick); }
    public void pickTarget(boolean pick) { _editor.pickTarget(pick); }
    private void cancel() { _shell.setVisible(false); }

    public void updateBookmark(BookmarkEditor editor, NymReferenceNode bookmark, boolean delete) {
        _shell.setVisible(false);
        if (bookmark.getGroupId() < 0)
            _bookmarkControl.bookmark(bookmark, true);
        else if (delete)
            _bookmarkControl.deleteBookmark(bookmark.getGroupId());
        else
            _bookmarkControl.updateBookmark(bookmark);
    }

    public void cancelEditor(BookmarkEditor editor) {
        _shell.setVisible(false);
    }
    
    private static final String T_TITLE = "syndie.gui.bookmarkeditor.title";
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText(T_TITLE, "Bookmark editor"));
    }
    public void applyTheme(Theme theme) {
        _shell.setFont(theme.SHELL_FONT);
    }
}
