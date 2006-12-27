package syndie.db;

import java.io.*;
import java.util.*;
import net.i2p.data.DataHelper;
import syndie.Constants;

/**
 * Main scriptable text UI
 */
public class TextUI implements UI {
    private boolean _debug = false;
    private boolean _paginate = true;
    private List _insertedCommands;
    private int _linesSinceInput;
    private PrintStream _debugOut;
    private BufferedReader _in;
    private TextEngine _engine;
    private boolean _readStdin;
    
    /** @param wantsDebug if true, we want to display debug messages */
    public TextUI(boolean wantsDebug) {
        _debug = wantsDebug;
        _insertedCommands = new ArrayList();
        try {
            _in = new BufferedReader(new InputStreamReader(System.in, "UTF-8"));
            try {
                _debugOut = new PrintStream(new FileOutputStream("debug.log"), true);
            } catch (IOException ioe) {
                _debugOut = new PrintStream(new NullOutputStream());
            }
        } catch (UnsupportedEncodingException uee) {
            errorMessage("internal error, your JVM doesn't support UTF-8?", uee);
            throw new RuntimeException("Broken JVM");
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
    }
    private static final class NullOutputStream extends OutputStream {
      public void write(int b) {}
    }
    private void display(String msg) { display(msg, true); }
    private void display(String msg, boolean nl) {
        if (nl)
            System.out.println(msg);
        else
            System.out.print(msg);
        if (_debug) {
            if (nl)
                _debugOut.println(msg);
            else
                _debugOut.print(msg);
        }
    }
    private void display(Exception e) {
        e.printStackTrace();
        if (_debug)
            e.printStackTrace(_debugOut);
    }
    
    private String readLine() {
        if (!_readStdin) throw new IllegalStateException("no stdin");
        try {
            return _in.readLine();
        } catch (IOException ioe) {
            errorMessage("Error reading STDIN", ioe);
            return "";
        }
    }

    public Opts readCommand() { return readCommand(true); }
    public Opts readCommand(boolean displayPrompt) {
        Opts rv = null;
        while (rv == null) {
            if (displayPrompt)
                display("* Next command: ", false);
            try {
                _linesSinceInput = 0;
                String line = null;
                if (!_readStdin) {
                    while (line == null) {
                        try {
                            synchronized (_insertedCommands) {
                                if (_insertedCommands.size() > 0)
                                        line = (String)_insertedCommands.remove(0);
                                else
                                    _insertedCommands.wait();
                            }
                        } catch (InterruptedException ie) {}
                    }
                } else {
                    if (_insertedCommands.size() == 0) {
                        line = readLine(); //DataHelper.readLine(System.in);
                        if ( (line != null) && (!line.startsWith("#")) )
                            debugMessage("command line read [" + line + "]");
                    } else {
                        line = (String)_insertedCommands.remove(0);
                        line = line.trim();
                        if (!line.startsWith("#"))
                            debugMessage("command line inserted [" + line + "]");
                    }
                }
                if (line == null) {
                    // EOF, so assume "exit"
                    rv = new Opts("exit");
                } else if (line.startsWith("#")) {
                    // skip comment lines
                    rv = null;
                    displayPrompt = false;
                } else {
                    rv = new Opts(line);
                    if (!rv.getParseOk()) {
                        errorMessage("Error parsing the command [" + line + "]");
                        rv = null;
                    }
                }
            } catch (Exception e) {
                errorMessage("Error parsing the command", e);
            }
        }
        return rv;
    }

    public void errorMessage(String msg) { errorMessage(msg, null); }
    public void errorMessage(String msg, Exception cause) {
        //System.err.println(msg);
        display(msg);
        if (cause != null) {
            display(cause);
        }
    }

    public void statusMessage(String msg) {
        String lines[] = Constants.split('\n', msg); //msg.split("\n");
        if (lines != null) {
            for (int i = 0; i < lines.length; i++) {
                beforeDisplayLine();
                display(lines[i]);
            }
        }
    }
    public void debugMessage(String msg) { debugMessage(msg, null); }
    public void debugMessage(String msg, Exception cause) {
        if (!_debug) return;
        if (msg != null)
            display(msg);
        if (cause != null)
            display(cause);
    }
    public void commandComplete(int status, List location) {
        display("* Command execution complete. ");
        display("* Status: " + status);
        StringBuffer buf = new StringBuffer();
        if (location != null) {
            for (int i = 0; i < location.size(); i++) {
                buf.append(location.get(i).toString()).append("> ");
            }
        }
        display("* Location: " + buf.toString());
    }
    public boolean toggleDebug() { _debug = !_debug; return _debug; }
    public boolean togglePaginate() { _paginate = !_paginate; return _paginate; }
    
    private void beforeDisplayLine() {
        _linesSinceInput++;
        if (_paginate && _readStdin) {
            if (_linesSinceInput > 10) {
                System.out.print("[Hit enter to continue]");
                readLine();
                _linesSinceInput = 0;
            }
        }
    }
    
    public void insertCommand(String cmd) {
        if (cmd == null) return;
        
        // trim off any trailing newlines
        while (cmd.length() > 0) {
            char c = cmd.charAt(cmd.length()-1);
            if ( (c == '\n') || (c == '\r') ) {
                cmd = cmd.substring(0, cmd.length()-1);
            } else {
                if (cmd.length() > 0) {
                    synchronized (_insertedCommands) {
                        _insertedCommands.add(cmd);
                        _insertedCommands.notifyAll();
                    }
                }
                return;
            }
        }
        // blank line
        return;
    }
    
    public String readStdIn() {
        StringBuffer buf = new StringBuffer();
        statusMessage("Reading standard input until a line containing a single \".\" is reached");
        String line = null;
        while (true) {
            if (_readStdin) {
                if (_insertedCommands.size() == 0)
                    line = readLine();
                else
                    line = (String)_insertedCommands.remove(0);
            } else {
                line = null;
                while (line == null) {
                    try {
                        synchronized (_insertedCommands) {
                            if (_insertedCommands.size() > 0)
                                line = (String)_insertedCommands.remove(0);
                            else
                                _insertedCommands.wait();
                        }
                    } catch (InterruptedException ie) {}
                }
            }
            
            if ( (line == null) || ( (line.length() == 1) && (line.charAt(0) == '.') ) )
                break;
            
            buf.append(line).append('\n');
        }
        return buf.toString();
    }
    
    public static void main(String args[]) {
        TextUI ui = new TextUI(args);
        ui.run();
    }
    public TextUI(String args[]) {
        this(false);
        System.setProperty("jbigi.dontLog", "true");
        System.setProperty("jcpuid.dontLog", "true");
        
        String rootDir = TextEngine.getRootPath();
        String script = null;
        boolean readStdin = true;
        for (int i = 0; i < args.length; i++) {
            if (args[i].startsWith("@"))
                script = args[i].substring(1);
            else if (args[i].equals("--nostdin"))
                readStdin = false;
            else
                rootDir = args[i];
        }
        
        // this way the logs won't go to ./logs/log-#.txt (i2p's default)
        // (this has to be set before the I2PAppContext instantiates the LogManager)
        System.setProperty("loggerFilenameOverride", rootDir + "/logs/syndie-log-#.txt");

        if (script != null) {
            BufferedReader in = null;
            try {
                in = new BufferedReader(new InputStreamReader(new FileInputStream(script), "UTF-8"));
                String line = null;
                while ( (line = in.readLine()) != null)
                    insertCommand(line);
                in.close();
                in = null;
            } catch (UnsupportedEncodingException uee) {
                errorMessage("internal error, your JVM doesn't support UTF-8?", uee);
            } catch (IOException ioe) {
                errorMessage("Error running the script " + script, ioe);
            } finally {
                if (in != null) try { in.close(); } catch (IOException ioe) {}
            }
        }
        _readStdin = readStdin;
        _engine = new TextEngine(rootDir, this);
    }
    
    public TextEngine getEngine() { return _engine; }

    /**
     * run the UI against a new text engine - this call doesn't return
     * until the engine tells it to
     */
    public void run() {
        _engine.run();
    }
}
