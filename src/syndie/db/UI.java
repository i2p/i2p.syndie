package syndie.db;

import java.util.List;

/**
 * interface that the client engine queries and updates as it executes the
 * requested commands
 */
public interface UI {
    public Opts readCommand();
    public Opts readCommand(boolean displayPrompt);
    public void errorMessage(String msg);
    public void errorMessage(String msg, Exception cause);
    public void statusMessage(String msg);
    public void debugMessage(String msg);
    public void debugMessage(String msg, Exception cause);
    /**
     * the running command completed
     * @param status nonnegative for successful status, negative for failure status
     * @param location list of contextual locations (String), generic to specific (most generic first)
     */
    public void commandComplete(int status, List location);
    /**
     * toggle between displaying debug messages and not displaying them
     * @return new state
     */
    public boolean toggleDebug();
    /**
     * toggle between paginating the status output and not
     * @return new state
     */
    public boolean togglePaginate();
    
    /** inject the given command to run next, so it will be the next thing out of readCommand() */
    public void insertCommand(String commandline);

    /**
     * read the standard input, replacing os-dependent newline characters with \n (0x0A).
     * This reads until a sinle line with just "." is put on it (SMTP-style).
     */
    public String readStdIn();
}
