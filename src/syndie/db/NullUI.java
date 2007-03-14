package syndie.db;

import java.util.List;

/**
 *
 */
public class NullUI implements UI {
    public Opts readCommand() { return null; }
    public Opts readCommand(boolean displayPrompt) { return null; }
    public void errorMessage(String msg) {}
    public void errorMessage(String msg, Exception cause) {}
    public void statusMessage(String msg) {}
    public void debugMessage(String msg) {}
    public void debugMessage(String msg, Exception cause) {}
    public void commandComplete(int status, List location) {}
    public boolean toggleDebug() { return false; }
    public boolean togglePaginate() { return false; }
    public void insertCommand(String commandline) {}
    public String readStdIn() { return null; }
    public void addUI(UI ui) {}
    public void removeUI(UI ui) {}
}
