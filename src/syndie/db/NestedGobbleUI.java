package syndie.db;

import java.util.List;

/**
 * Gobble up any normal status messages (but still display error messages,
 * as well as debug messages, if configured to do so)
 *
 */
public class NestedGobbleUI extends NestedUI {
    public NestedGobbleUI(UI real) { super(real); }
    public void statusMessage(String msg) { debugMessage(msg); }
    public Opts readCommand() { return super.readCommand(false); }
    protected void displayPrompt() { System.out.println("nested displayPrompt"); }
    public void commandComplete(int status, List location) {}
}
