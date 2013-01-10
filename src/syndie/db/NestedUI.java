package syndie.db;

import java.util.List;

/**
 */
public class NestedUI implements UI {
    protected final UI _real;
    private int _exit;

    public NestedUI(UI real) { _real = real; _exit = 0; }
    public int getExitCode() { return _exit; }
    public Opts readCommand() { return _real.readCommand(); }
    public Opts readCommand(boolean displayPrompt) { return _real.readCommand(displayPrompt); }
    public void errorMessage(String msg) { _real.errorMessage(msg); }
    public void errorMessage(String msg, Exception cause) { _real.errorMessage(msg, cause); }
    public void statusMessage(String msg) { _real.statusMessage(msg); }
    public void debugMessage(String msg) { _real.debugMessage(msg); }
    public void debugMessage(String msg, Exception cause) { _real.debugMessage(msg, cause); }
    public void commandComplete(int status, List location) {
        _exit = status;
        // don't propogate the command completion, as we are nested
    }
    public boolean toggleDebug() { return _real.toggleDebug(); }
    public boolean togglePaginate() { return _real.togglePaginate(); }
    public void insertCommand(String cmd) { _real.insertCommand(cmd); }
    public String readStdIn() { return _real.readStdIn(); }

    public void addUI(UI ui) { _real.addUI(ui); }
    public void removeUI(UI ui) { _real.removeUI(ui); }
}
