package syndie.gui;

import java.util.ArrayList;
import java.util.List;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import syndie.db.NullUI;
import syndie.db.UI;

/**
 * maintain the text / caret position / scroll index for the styled text, undoing
 * them on ^Z and redoing them on ^Y (clearing any redoables on a non-redo text change)
 *
 * Can this just extend TextChangeManager?
 */
class StyledTextChangeManager {
    private UI _ui;
    private StyledText _text;
    /** list of mementos */
    private List _undoable;
    /** list of mementos */
    private List _redoable;
    /** we are currently executing an undo/redo, so don't count this change as an undoable action */
    private boolean _changeUpdateInProgress;
    private String _beforeText;
    private int _beforeTextPosition;
    private int _beforeTextScrollPixel;
    
    private static final int MAX_UNDOABLE = 200;
    
    public StyledTextChangeManager(StyledText text, UI ui) {
        _ui = ui;
        _text = text;
        _undoable = new ArrayList();
        _redoable = new ArrayList();
        _changeUpdateInProgress = false;
        _beforeText = text.getText();
        _beforeTextPosition = text.getCaretOffset();
        // verify is called before the modification is applied, and modify is called
        // afterwards.  we simply record the full text at both
        _text.addVerifyListener(new VerifyListener() {
            public void verifyText(VerifyEvent verifyEvent) {
                //_ui.debugMessage("verify [changeInProgress? " + _changeUpdateInProgress + "]");
                if (!_changeUpdateInProgress) {
                    _beforeText = _text.getText();
                    _beforeTextPosition = _text.getCaretOffset();
                    _beforeTextScrollPixel = _text.getTopPixel();
                }
            }
        });
        _text.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent modifyEvent) {
                //_ui.debugMessage("modify [changeInProgress? " + _changeUpdateInProgress + "]");
                if (!_changeUpdateInProgress) {
                    String txt = _text.getText();
                    int pos = _text.getCaretOffset();
                    int top = _text.getTopPixel();
                    Memento memento = new TextMemento(_beforeText, txt, _beforeTextPosition, pos, _beforeTextScrollPixel, top);
                    _beforeText = txt;
                    _beforeTextPosition = pos;
                    _beforeTextScrollPixel = top;
                    _redoable.clear();
                    _undoable.add(memento);
                    while (_undoable.size() > MAX_UNDOABLE)
                        _undoable.remove(0);
                }
            }
        });
        _text.addKeyListener(new KeyListener() {
            public void keyPressed(KeyEvent keyEvent) {}
            public void keyReleased(KeyEvent evt) {
                if ( (!_changeUpdateInProgress) && ((evt.stateMask & SWT.MOD1) != 0) ) {
                    if (evt.character == 0x1A) { // ^Z
                        undo();
                        evt.doit = false;
                    } else if (evt.character == 0x19) { // ^Y
                        redo();
                        evt.doit = false;
                    }
                }
            }
        });
    }
    
    public long getBufferSize() {
        long rv = 0;
        for (int i = 0; i < _undoable.size(); i++)
            rv += ((Memento)_undoable.get(i)).size();
        for (int i = 0; i < _redoable.size(); i++)
            rv += ((Memento)_redoable.get(i)).size();
        return rv;
    }
    
    public interface Memento {
        public void redo();
        public void undo();
        public long size();
    }
    
    private void redo() {
        //_ui.debugMessage("redo w/ " + _redoable.size() + "/" + _undoable.size());
        if (_redoable.size() > 0) {
            Memento memento = null;
            memento = (Memento)_redoable.remove(_redoable.size()-1);
            _undoable.add(memento);
            _changeUpdateInProgress = true;
            memento.redo();
            _changeUpdateInProgress = false;
        }
    }
    private void undo() {
        //_ui.debugMessage("undo w/ " + _redoable.size() + "/" + _undoable.size());
        if (_undoable.size() > 0) {
            Memento memento = null;
            memento = (Memento)_undoable.remove(_undoable.size()-1);
            _redoable.add(memento);
            _changeUpdateInProgress = true;
            memento.undo();
            _changeUpdateInProgress = false;
        }
    }
    
    private class TextMemento implements Memento {
        private final String _before;
        private final String _after;
        private final int _beforeCaret;
        private final int _afterCaret;
        private final int _beforeScroll;
        private final int _afterScroll;

        public TextMemento(String before, String after, int beforeCaret, int afterCaret, int beforeScroll, int afterScroll) {
            _before = before;
            _after = after;
            _beforeCaret = beforeCaret;
            _afterCaret = afterCaret;
            _beforeScroll = beforeScroll;
            _afterScroll = afterScroll;
        }
        public void undo() {
            _text.setText(_before);
            _text.setCaretOffset(_beforeCaret);
            _text.setTopPixel(_beforeScroll);
        }
        public void redo() { 
            _text.setText(_after);
            _text.setCaretOffset(_afterCaret);
            _text.setTopPixel(_afterScroll);
        }
        public long size() { return _before.length() + _after.length(); }
    }
    
    public static void main(String args[]) {
        UI ui = new NullUI() { 
            public void debugMessage(String msg) { debugMessage(msg, null); }
            public void debugMessage(String msg, Exception e) { 
                System.out.println(msg); 
                if (e != null)
                    e.printStackTrace();
            } 
        };
    
        Display d = Display.getDefault();
        final Shell shell = new Shell(d, SWT.SHELL_TRIM);
        shell.setLayout(new FillLayout());
        StyledText txt = new StyledText(shell, SWT.MULTI | SWT.WRAP | SWT.BORDER);
        StyledTextChangeManager mgr = new StyledTextChangeManager(txt, ui);
        shell.setMaximized(true);
        shell.open();
        shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent shellEvent) { shell.dispose(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });

        while (!shell.isDisposed()) {
            try { 
                if (!d.readAndDispatch()) d.sleep(); 
            } catch (RuntimeException e) {
                ui.debugMessage("Internal error", e);
            }
        }
    
    }
}
