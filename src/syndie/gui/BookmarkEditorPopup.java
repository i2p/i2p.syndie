package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Shell;
import syndie.data.NymReferenceNode;

/**
 *
 */
class BookmarkEditorPopup implements BookmarkEditor.BookmarkEditorListener, Translatable {
    private BrowserControl _browser;
    private Shell _parent;
    private Shell _shell;
    private BookmarkEditor _editor;
    
    public BookmarkEditorPopup(BrowserControl control, Shell parent) {
        _browser = control;
        _parent = parent;
        initComponents();
    }
    
    private void initComponents() {
        _shell = new Shell(_parent, SWT.SHELL_TRIM | SWT.APPLICATION_MODAL);
        _shell.setLayout(new FillLayout());
        _editor = new BookmarkEditor(_browser, _shell, this);
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
    
    public void setBookmark(NymReferenceNode node) { _editor.setBookmark(node); }
    public void open() { 
        _shell.setSize(_shell.computeSize(300, SWT.DEFAULT));
        _shell.open();
    }
    private void cancel() { _shell.setVisible(false); }

    public void updateBookmark(BookmarkEditor editor, NymReferenceNode bookmark, boolean delete) {
        _shell.setVisible(false);
        if (bookmark.getGroupId() < 0)
            _browser.bookmark(bookmark);
        else if (delete)
            _browser.deleteBookmark(bookmark.getGroupId());
        else
            _browser.updateBookmark(bookmark);
    }

    public void cancelEditor(BookmarkEditor editor) {
        _shell.setVisible(false);
    }
    
    private static final String T_TITLE = "syndie.gui.bookmarkeditor.title";
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText(T_TITLE, "Bookmark editor"));
    }
}
