package syndie.db;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.*;
import net.i2p.I2PAppContext;
import net.i2p.data.Hash;
import syndie.Constants;
import syndie.Version;
import syndie.data.SyndieURI;

public class TextEngine {
    private UI _ui;
    private boolean _exit;
    private DBClient _client;
    private List _menus;
    private String _currentMenu;
    private String _rootFile;
    private File _rootDir;
    private File _dbDir;
    private File _tmpDir;
    private File _archiveDir;
    private File _outboundDir;
    private File _logDir;
    private File _scriptDir;
    private NestedGobbleUI _gobbleUI;
    private UI _realUI;
    private List _commandHistory;
        
    public TextEngine(String rootDir, UI ui) {
        _realUI = new MenuUI(ui);
        _ui = _realUI;
        _gobbleUI = new NestedGobbleUI(_realUI);
        _exit = false;
        _rootFile = rootDir;
        _commandHistory = new ArrayList();
        rebuildMenus();
        buildInstallDir();
        _client.runScript(_ui, "startup");
    }
    
    /** clear all the old state in the various menus, and put us back at the not-logged-in menu */
    private void rebuildMenus() {
        _menus = new ArrayList();
        _menus.add(new StartMenu());
        _menus.add(new LoggedInMenu());
        _menus.add(new ReadMenu(this));
        _menus.add(new ManageMenu(this));
        _menus.add(new PostMenu(this));
        _menus.add(new SyndicateMenu(this));
        _currentMenu = StartMenu.NAME;
    }
    
    public void run() {
        while (!_exit) {
            if (runStep()) {
                // keep going
            } else {
                break;
            }
        }
        _ui.statusMessage("Syndie engine exiting");
    }

    public boolean runStep() {
        try {
	    return doRunStep();
	} catch (RuntimeException re) {
	    _ui.errorMessage("Internal error", re);
	    return true;
	}
    }

    private boolean doRunStep() {
        Opts opts = _ui.readCommand();
        if (opts == null) return false;
        String cmdStr = opts.getCommand();
        boolean ignored = true;
        String origLine = opts.getOrigLine();
        if ( (cmdStr == null) || (cmdStr.trim().startsWith("--")) ) {
            // noop
        } else if (processMeta(opts) || processMenu(opts)) {
            ignored = false;
            if (origLine.startsWith("!") || (origLine.startsWith("^")))
                ignored = true;
        } else {
            CLI.Command cmd = CLI.getCommand(opts.getCommand());
            if (cmd == null) {
                if ( (_client != null) && (_client.getLoggedInNymId() >= 0) ) {
                    Map aliases = _client.getAliases(_client.getLoggedInNymId());
                    String value = (String)aliases.get(opts.getCommand());
                    if (value != null) {
                        executeAlias(value);
                        return true;
                    }
                }
                unknownCommand(opts.getCommand());
                _ui.commandComplete(-1, null);
            } else {
                ignored = false;
                _client = cmd.runCommand(opts, _ui, _client);
                if ( (_client == null) || (!_client.isLoggedIn()) )
                    rebuildMenus();
            }
        }
        if (!ignored)
            _commandHistory.add(origLine);
        return true;
    }
    
    private void processLogout() {
        if (_client != null)
            _client.close();
        rebuildMenus();
    }
    
    private void buildInstallDir() {
        _rootDir = new File(_rootFile);
        _dbDir = new File(_rootDir, "db");
        _tmpDir = new File(_rootDir, "tmp");
        _archiveDir = new File(_rootDir, "archive");
        _outboundDir = new File(_rootDir, "outbound");
        _logDir = new File(_rootDir, "logs");
        _scriptDir = new File(_rootDir, "scripts");

        boolean dbDirCreated = false;
        if (!_rootDir.exists()) _rootDir.mkdirs();
        if (!_dbDir.exists()) { _dbDir.mkdir(); dbDirCreated = true; }
        if (_tmpDir.exists())
            rmdir(_tmpDir, _rootDir);
        _tmpDir.mkdir();
        if (!_archiveDir.exists()) _archiveDir.mkdir();
        if (!_outboundDir.exists()) _outboundDir.mkdir();
        if (!_logDir.exists()) _logDir.mkdir();
        if (!_scriptDir.exists()) {
            _scriptDir.mkdir();
            // bundle any scripts we ship with in the .jar
            installResource("/defaultprefs", new File(_scriptDir, "defaultprefs"));
            installResource("/defaultaliases", new File(_scriptDir, "defaultaliases"));
            installResource("/onstartup", new File(_scriptDir, "startup"));
            installResource("/onlogin", new File(_scriptDir, "login"));
        }
        
        File archiveIntro = new File(_archiveDir, "index.html");
        if (!archiveIntro.exists())
            installResource("/defaultarchiveindex", archiveIntro);
        
        _client = new DBClient(I2PAppContext.getGlobalContext(), _rootDir);
        
        if (dbDirCreated) {
            // so it doesn't gather 'command completed'/etc messages on the screen
            _ui.insertCommand("gobble");
            _ui.insertCommand("init");
            //--root '" + _rootFile + "' 
            _ui.insertCommand("register --db '" + getDefaultURL() + "' --login " + DEFAULT_LOGIN + " --pass '" + DEFAULT_PASS + "' --name 'Default account'");
            _ui.insertCommand("ungobble");
        }
        
        /*
        $base/db/syndie.*
        tmp/
        archive/index.txt
                $scopeHash/meta.snd
                          /$msgId.snd
        outbound/$scopeHash/meta.snd
                           /$msgId.snd
        logs/
        lib/{mini-i2p.jar,hsqldb_gcj.jar,syndie.jar}
        bin/{runtext.sh,runcli.sh}
         */
    }
    
    private void installResource(String name, File toFile) {
        InputStream in = null;
        FileOutputStream fos = null;
        try {
            in = getClass().getResourceAsStream(name);
            fos = new FileOutputStream(toFile);
            if (in != null) {
                byte buf[] = new byte[1024];
                int read = -1;
                while ( (read = in.read(buf)) != -1)
                    fos.write(buf, 0, read);
                in.close();
                in = null;
            }
            fos.close();
            fos = null;
        } catch (IOException ioe) {
            // ignore... script wasn't a resource, or we couldn't write to the dir, etc
        } finally {
            if (in != null) try { in.close(); } catch (IOException ioe) {}
            if (fos != null) try { fos.close(); } catch (IOException ioe) {}
        }
    }
    
    private void rmdir(File dir, File root) {
        if (!isAncestor(dir, root)) return;
        if (dir.isDirectory()) {
            File children[] = dir.listFiles();
            for (int i = 0; i < children.length; i++) {
                if (isAncestor(children[i], root)) // safety for symlinks/etc
                    rmdir(children[i], root);
            }
        }
        dir.delete();
    }
    /** true if the child is underneath the root, directly or indirectly */
    private boolean isAncestor(File child, File root) {
        while (child != null) {
            if (child.equals(root))
                return true;
            File parent = child.getParentFile();
            if (parent.equals(child))
                break;
            child = parent;
        }
        return false;
    }
    
    public String getDBFile() { return _dbDir.getPath() + File.separator + "syndie"; }
    public static String getRootPath() { return System.getProperty("user.home") + File.separator + ".syndie"; }
    public DBClient getClient() { return _client; }
    
    static final String DEFAULT_LOGIN = "user";
    static final String DEFAULT_PASS = "pass";
    
    private String getDefaultURL() { return "jdbc:hsqldb:file:" + getDBFile() + ";hsqldb.nio_data_file=false"; }
    
    private void processLogin(Opts opts) {
        String db = opts.getOptValue("db");
        String login = opts.getOptValue("login");
        String pass = opts.getOptValue("pass");
        
        if (db == null)
            db = getDefaultURL();
        if (login == null) {
            login = DEFAULT_LOGIN;
            pass = DEFAULT_PASS;
        }
        
        if (_client == null)
            _client = new DBClient(I2PAppContext.getGlobalContext(), _rootDir);
        else
            _client.close();
        try {
            if (pass == null)
                pass = "";
            _ui.debugMessage("Attempting to log into [" + db + "] w/ ["+login + "]=["+pass +"]");
            long nymId = _client.connect(db, login, pass);
            if (nymId >= 0) {
                _ui.statusMessage("Login successful (nymId " + nymId + ")");
                rebuildMenus();
                _currentMenu = LoggedInMenu.NAME;
                
                Properties prefs = _client.getNymPrefs(nymId);
                doSetPrefs(prefs);
                _client.runScript(_ui, "login");
            } else {
                _ui.statusMessage("Login failed");
                rebuildMenus();
            }
        } catch (SQLException se) {
            // *UUUUGLY*
            String msg = se.getMessage();
            // "org.hsqldb.HsqlException: The database is already in use by another 
            //  process: org.hsqldb.persist.NIOLockFile@1f4e3045[
            //  file =/mnt/data/ux/.syndie/db/syndie.lck, exists=true, locked=false, 
            //  valid=false, fl=null ]: java.lang.Exception: checkHeartbeat(): 
            //  lock file [/mnt/data/ux/.syndie/db/syndie.lck] is presumably 
            //  locked by another process."
            //  out of all that, checkHeartbeat is probably the only part that isn't
            //  internationalized (and specifically refers to not being able to log in)
            if ( (msg != null) && (msg.indexOf("checkHeartbeat()") >= 0) ) {
                _ui.debugMessage("Unable to log in", se);
                _ui.errorMessage("Unable to log in, as there is already another");
                _ui.errorMessage("syndie instance accessing that database.");
            } else {
                _ui.errorMessage("Error trying to login", se);
            }
        }
    }
    private void processSwitchMenu(Opts opts) {
        String targetMenu = null;
        if (opts.size() > 0)
            targetMenu = opts.getArg(0);
        if ( (_client == null) || (!_client.isLoggedIn()) ) {
            if ( (targetMenu != null) && (StartMenu.NAME.equals(targetMenu)) ) {
                // leave it be
            } else {
                // not logged in, so shove 'em to the start
                targetMenu = null;
            }
        }
        if (targetMenu != null) {
            for (int i = 0; i < _menus.size(); i++) {
                Menu cur = (Menu)_menus.get(i);
                if (cur.getName().equals(targetMenu)) {
                    _currentMenu = targetMenu;
                    break;
                }
            }
        }
        if (targetMenu == null) {
            _ui.statusMessage("Available menus: ");
            boolean loggedIn = (_client != null) && (_client.isLoggedIn());
            for (int i = 0; i < _menus.size(); i++) {
                Menu menu = (Menu)_menus.get(i);
                if (!menu.requireLoggedIn() || loggedIn)
                    _ui.statusMessage(" " + menu.getName() + padBlank(menu.getName(), 16) + "(" + menu.getDescription() + ")");
                /*
                _ui.statusMessage(" manage   (to manage channels)");
                _ui.statusMessage(" read     (to read posts)");
                _ui.statusMessage(" priv     (to read private messages)");
                _ui.statusMessage(" post     (to create private messages or posts)");
                _ui.statusMessage(" archive  (archive management)");
                _ui.statusMessage(" key      (key management)");
                _ui.statusMessage(" search   (search through messages)");
                _ui.statusMessage(" watched  (review and manage favorite channels/tags/resources)");
                _ui.statusMessage(" sql      (advanced SQL interface to the backend database)");
                */
            }
        }
    }
    private static String padBlank(String name, int paddedSize) {
        StringBuffer buf = new StringBuffer();
        int pad = paddedSize - name.length();
        for (int i = 0; i < pad; i++)
            buf.append(' ');
        return buf.toString();
    }
    private Menu getCurrentMenu() {
        for (int i = 0; i < _menus.size(); i++) {
            Menu menu = (Menu)_menus.get(i);
            if (menu.getName().equals(_currentMenu))
                return menu;
        }
        return null;
    }
    /**
     * Process any menu commands, returning true if the command was
     * a handled meta command, false if not
     */
    private boolean processMenu(Opts opts) {
        String cmd = opts.getCommand();
        if ("logout".equalsIgnoreCase(cmd)) {
            processLogout();
            _ui.commandComplete(0, null);
            return true;
        } else if ("menu".equalsIgnoreCase(cmd)) {
            processSwitchMenu(opts);
            _ui.commandComplete(0, null);
            return true;
        } else if ("up".equalsIgnoreCase(cmd)) {
            if (_currentMenu != StartMenu.NAME)
                _currentMenu = LoggedInMenu.NAME;
            _ui.commandComplete(0, null);
            return true;
        } else if ("prefs".equalsIgnoreCase(cmd)) {
            if (_currentMenu != StartMenu.NAME)
                processPrefs(opts);
            return true;
        } else if ("version".equalsIgnoreCase(cmd)) {
            _ui.statusMessage("Syndie version: " + Version.VERSION + " (http://syndie.i2p.net/)");
            _ui.commandComplete(0, null);
            return true;
        } else {
            Menu menu = getCurrentMenu();
            if (menu != null)
                return menu.processCommands(_client, _ui, opts);
            return false;
        }
    }
    
    /**
     * Process any meta commands (configuring the text engine), returning true
     * if the command was a handled meta command, false if not
     */
    private boolean processMeta(Opts opts) {
        String cmd = opts.getCommand();
        if (cmd == null)
            cmd = "";
        if ("exit".equalsIgnoreCase(cmd) || "quit".equalsIgnoreCase(cmd)) {
            processLogout();
            _ui.commandComplete(0, null);
            _exit = true;
            return true;
        } else if ("gobble".equalsIgnoreCase(cmd)) {
           _ui = _gobbleUI;
           _ui.statusMessage("Gobbling all normal status messages (until you \"ungobble\")");
           //_ui.commandComplete(0, null);
           return true;
        } else if ("ungobble".equalsIgnoreCase(cmd)) {
           _ui.statusMessage("No longer gobbling normal status messages");
           _ui = _realUI;
           //_ui.commandComplete(0, null);
           return true;
        } else if ("togglePaginate".equalsIgnoreCase(cmd)) {
            boolean newState = _ui.togglePaginate();
            if (newState)
                _ui.statusMessage("Paginating the output every 10 lines");
            else
                _ui.statusMessage("Not paginating the output");
            _ui.commandComplete(0, null);
            return true;
        } else if ("toggleDebug".equalsIgnoreCase(cmd)) {
            boolean newState = _ui.toggleDebug();
            if (newState)
                _ui.statusMessage("Displaying debug messages (and logging them to debug.log)");
            else
                _ui.statusMessage("Not displaying debug messages");
            _ui.commandComplete(0, null);
            return true;
        } else if ("init".equalsIgnoreCase(cmd)) {
            processInit(opts);
            rebuildMenus();
            return true;
        } else if ("builduri".equalsIgnoreCase(cmd)) {
            processBuildURI(opts);
            return true;
        } else if ("history".equalsIgnoreCase(cmd)) {
            processHistory(opts);
            return true;
        } else if (cmd.startsWith("!")) {
            processHistoryBang(opts);
            return true;
        } else if (cmd.startsWith("^")) {
            processHistoryReplace(opts);
            return true;
        } else if ("alias".equalsIgnoreCase(cmd)) {
            processAlias(opts);
            return true;
        } else if ("?".equalsIgnoreCase(cmd) || "help".equalsIgnoreCase(cmd)) {
            help();
            _ui.commandComplete(0, null);
            return true;
        } else {
            return false;
        }
    }
    
    private void processHistory(Opts opts) {
        for (int i = 0; i < _commandHistory.size(); i++)
            _ui.statusMessage((i+1) + ": " + (String)_commandHistory.get(i));
    }
    /** deal with !!, !123, and !-123 */
    private void processHistoryBang(Opts opts) {
        String cmd = opts.getCommand();
        if (cmd.startsWith("!!")) {
            if (_commandHistory.size() > 0) {
                String prevCmd = (String)_commandHistory.get(_commandHistory.size()-1);
                _ui.insertCommand(prevCmd);
            } else {
                _ui.errorMessage("No commands in the history buffer");
                _ui.commandComplete(-1, null);
            }
        } else {
            try {
                if (cmd.length() > 1) {
                    int num = Integer.parseInt(cmd.substring(1));
                    if (num < 0)
                        num = _commandHistory.size() + num;
                    num--;
                    if (_commandHistory.size() > num) {
                        _ui.insertCommand((String)_commandHistory.get(num));
                    } else {
                        _ui.errorMessage("Command history element out of range");
                        _ui.commandComplete(-1, null);
                    }
                } else {
                    _ui.errorMessage("Usage: !$num or !-$num");
                    _ui.commandComplete(-1, null);
                }
            } catch (NumberFormatException nfe) {
                _ui.errorMessage("Usage: !$num or !-$num");
                _ui.commandComplete(-1, null);
            }
        }
    }
    /** deal with ^a[^b] */
    private void processHistoryReplace(Opts opts) {
        if (_commandHistory.size() > 0) {
            String prev = (String)_commandHistory.get(_commandHistory.size()-1);
            String cmd = opts.getCommand();
            String orig = null;
            String replacement = null;
            int searchEnd = cmd.indexOf('^', 1);
            if (searchEnd < 0) {
                orig = cmd.substring(1);
            } else {
                orig = cmd.substring(1, searchEnd);
                replacement = cmd.substring(searchEnd+1);
            }
            String newVal = replace(prev, orig, replacement, 1);
            _ui.insertCommand(newVal);
        } else {
            _ui.errorMessage("No history to mangle");
            _ui.commandComplete(-1, null);
        }
    }
    
    private static final String replace(String orig, String oldval, String newval, int howManyReplacements) {
        if ( (orig == null) || (oldval == null) || (oldval.length() <= 0) ) return orig;
        
        StringBuffer rv = new StringBuffer();
        char origChars[] = orig.toCharArray();
        char search[] = oldval.toCharArray();
        int numReplaced = 0;
        for (int i = 0; i < origChars.length; i++) {
            boolean match = true;
            if (howManyReplacements <= numReplaced)
                match = false; // matched enough, stop
            for (int j = 0; match && j < search.length && (j + i < origChars.length); j++) {
                if (search[j] != origChars[i+j])
                    match = false;
            }
            if (match) {
                if (newval != null)
                    rv.append(newval);
                i += search.length-1;
                numReplaced++;
            } else {
                rv.append(origChars[i]);
            }
        }
        return rv.toString();
    }
    
    private void processAlias(Opts opts) {
        List args = opts.getArgs();
        if (args.size() <= 0) {
            displayAliases();
        } else {
            String name = (String)args.get(0);
            StringBuffer buf = new StringBuffer();
            for (int i = 1; i < args.size(); i++) {
                String str = (String)args.get(i);
                buf.append(str).append(" ");
            }
            String value = buf.toString().trim();
            _client.addAlias(_client.getLoggedInNymId(), name, value);
            if (value.length() == 0)
                _ui.statusMessage("Alias removed for '" + name + "'");
            else
                _ui.statusMessage("New alias for '" + name + "': " + value);
        }
    }
    
    private void displayAliases() {
        Map aliases = _client.getAliases(_client.getLoggedInNymId());
        for (Iterator iter = aliases.keySet().iterator(); iter.hasNext(); ) {
            String name = (String)iter.next();
            String value = (String)aliases.get(name);
            _ui.statusMessage("Alias '" + name + "': " + value);
        }
    }
    
    private void executeAlias(String aliasedValue) {
        String cmds[] = Constants.split(';', aliasedValue);
        for (int i = 0; i < cmds.length; i++) {
            _ui.debugMessage("aliased command " + i + ": " + cmds[i]);
            _ui.insertCommand(cmds[i]);
        }
    }
    
    private void unknownCommand(String cmd) {
        _ui.errorMessage("Command unknown: " + cmd);
        _ui.errorMessage("Type ? for help");
    }
    
    private void help() {
       _ui.statusMessage("Commands: ");
        Menu menu = getCurrentMenu();
        if (menu != null) {
            menu.listCommands(_ui);
            if (menu.requireLoggedIn())
                _ui.statusMessage(" logout             : disconnect from the database, but do not exit syndie");
            if (!_currentMenu.equals(LoggedInMenu.NAME))
                _ui.statusMessage(" up                 : go up a menu");
        }
        _ui.statusMessage(" init $jdbcURL      : create a new syndie database");
        _ui.statusMessage(" menu [$newMenu]    : switch between the menus, or view available menus");
        _ui.statusMessage(" builduri (--url $url | --channel $chanHash [--message $num [--page $num] )");
        _ui.statusMessage("                    : helper method for building Syndie URIs");
        _ui.statusMessage(" toggleDebug        : turn on or off debugging output");
        _ui.statusMessage(" togglePaginate     : turn on or off output pagination");
        _ui.statusMessage(" prefs [--debug $boolean] [--paginate $boolean] ");
        _ui.statusMessage("       [--httpproxyhost $hostname --httpproxyport $portNum]");
        _ui.statusMessage("       [--archive $archiveURL]");
        _ui.statusMessage("                    : update or display the logged in nym's preferences");
        _ui.statusMessage(" exit               : exit syndie");
    }
    
    private void processSQL(Opts opts) {
        StringBuffer query = new StringBuffer();
        List args = opts.getArgs();
        for (int i = 0; i < args.size(); i++)
            query.append(args.get(i).toString()).append(' ');
        _client.exec(query.toString(), _ui);
    }
    
    private List getMenuLocation() {
        List rv = new ArrayList();
        Menu menu = getCurrentMenu();
        if (menu != null) {
            if (menu.requireLoggedIn())
                rv.add("logged in as " + _client.getLogin());
            rv.addAll(menu.getMenuLocation(_client, _ui));
        } else {
            _ui.debugMessage("No menu found, current = " + _currentMenu);
            rv.add("logged out");
        }
        return rv;
    }
    
    private void processInit(Opts opts) {
        List args = opts.getArgs();
        String url = getDefaultURL();
        if (args.size() == 1)
            url = (String)args.get(0);
        try {
            _client = new DBClient(I2PAppContext.getGlobalContext(), _rootDir);
            _client.connect(url);
            //_client.close();
            _ui.statusMessage("Database created at " + url);
            _ui.commandComplete(0, null);
            return;
        } catch (SQLException se) {
            _ui.errorMessage("Error creating the database", se);
            _ui.commandComplete(-1, null);
            return;
        }
    }
    
    private static final SimpleDateFormat _backupFmt = new SimpleDateFormat("yyyy-MM-dd");
    private void processBackup(Opts opts) {
        if ( (_client == null) || (!_client.isLoggedIn()) ) {
            _ui.errorMessage("You must be logged in to backup the database");
            _ui.commandComplete(-1, null);
            return;
        }
        String out = opts.getOptValue("out");
        if ( (out == null) || (out.length() <= 0) ) {
            _ui.errorMessage("Usage: backup --out $filename [--includeArchive $boolean]");
            _ui.commandComplete(-1, null);
            return;
        }
        int dateBegin = out.indexOf("DATE");
        if (dateBegin >= 0) {
            String pre = "";
            String post = "";
            if (dateBegin > 0)
                pre = out.substring(0, dateBegin);
            if (dateBegin < out.length()-4)
                post = out.substring(0, dateBegin);
            synchronized (_backupFmt) {
                out = pre + _backupFmt.format(new Date(System.currentTimeMillis())) + post;
            }
        }
        boolean includeArchive = opts.getOptBoolean("includeArchive", false);
        _client.backup(_ui, out, includeArchive);
    }
    
    private void processBuildURI(Opts opts) {
        SyndieURI uri = null;
        String url = opts.getOptValue("url");
        if (url != null) {
            uri = SyndieURI.createURL(url);
        } else {
            byte chan[] = opts.getOptBytes("channel");
            if ( (chan != null) && (chan.length == Hash.HASH_LENGTH) ) {
                long msgId = opts.getOptLong("message", -1);
                if (msgId >= 0) {
                    long page = opts.getOptLong("page", -1);
                    if (page >= 0) {
                        uri = SyndieURI.createMessage(new Hash(chan), msgId, (int)page);
                    } else {
                        uri = SyndieURI.createMessage(new Hash(chan), msgId);
                    }
                } else {
                    uri = SyndieURI.createScope(new Hash(chan));
                }
            } else {
                String archive = opts.getOptValue("archive");
                String pass = opts.getOptValue("pass");
                if (archive != null)
                    uri = SyndieURI.createArchive(archive, pass);
            }
        }
        
        if (uri != null) {
            _ui.statusMessage("Encoded Syndie URI: " + uri.toString());
            _ui.commandComplete(0, null);
        } else {
            _ui.errorMessage("Could not build the Syndie URI");
            _ui.commandComplete(-1, null);
        }
    }

    private void processPrefs(Opts opts) {
        Properties prefs = _client.getNymPrefs(_client.getLoggedInNymId());
        if (opts.getOptNames().size() > 0) {
            // some were set, so actually adjust things rather than simply display
            for (Iterator iter = opts.getOptNames().iterator(); iter.hasNext(); ) {
                String name = (String)iter.next();
                String val = opts.getOptValue(name);
                if ( (val == null) || (val.length() <= 0) )
                    prefs.remove(name);
                else
                    prefs.setProperty(name, val);
            }
        } else {
            //System.out.println("Prefs have no opts, defaults are: " + prefs);
        }
        _client.setNymPrefs(_client.getLoggedInNymId(), prefs);
        doSetPrefs(prefs);
        _ui.commandComplete(0, null);
    }
    
    private void doSetPrefs(Properties prefs) {
        String dbgVal = prefs.getProperty("debug");
        if (dbgVal != null) {
            boolean debug = Boolean.valueOf(dbgVal).booleanValue();
            boolean isNowDebug = _ui.toggleDebug();
            if (isNowDebug) {
                if (debug) {
                    // already debugging
                } else {
                    _ui.toggleDebug();
                }
            } else {
                if (debug) {
                    _ui.toggleDebug();
                } else {
                    // already not debugging
                }
            }
            _ui.statusMessage("Preference: display debug messages? " + debug);
        }
        String paginateVal = prefs.getProperty("paginate");
        if (paginateVal != null) {
            boolean paginate = Boolean.valueOf(paginateVal).booleanValue();
            boolean isNowPaginate = _ui.togglePaginate();
            if (isNowPaginate) {
                if (paginate) {
                    // already paginating
                } else {
                    _ui.togglePaginate();
                }
            } else {
                if (paginate) {
                    _ui.togglePaginate();
                } else {
                    // already not paginating
                }
            }
            _ui.statusMessage("Preference: paginate output? " + paginate);
        }
        _client.setDefaultHTTPProxyHost(prefs.getProperty("httpproxyhost"));
        String port = prefs.getProperty("httpproxyport");
        if (port != null) {
            try {
                int num = Integer.parseInt(port);
                _client.setDefaultHTTPProxyPort(num);
            } catch (NumberFormatException nfe) {
                _ui.errorMessage("HTTP proyx port preference is invalid", nfe);
                _client.setDefaultHTTPProxyPort(-1);
            }
        } else {
            _client.setDefaultHTTPProxyPort(-1);
        }
        _client.setDefaultHTTPArchive(prefs.getProperty("archive"));
        
        _client.setDefaultFreenetPrivateKey(prefs.getProperty("freenetPrivateKey"));
        _client.setDefaultFreenetPublicKey(prefs.getProperty("freenetPublicKey"));
        _client.setDefaultFreenetHost(prefs.getProperty("fcpHost"));
        port = prefs.getProperty("fcpPort");
        if (port != null) {
            try {
                int num = Integer.parseInt(port);
                _client.setDefaultFreenetPort(num);
            } catch (NumberFormatException nfe) {
                _ui.errorMessage("Freenet port preference is invalid", nfe);
                _client.setDefaultFreenetPort(-1);
            }
        } else {
            _client.setDefaultFreenetPort(-1);
        }
        
        if ( (_client.getDefaultHTTPProxyHost() != null) && (_client.getDefaultHTTPProxyPort() > 0) )
            _ui.statusMessage("Preference: default HTTP proxy: " + _client.getDefaultHTTPProxyHost() + ":" + _client.getDefaultHTTPProxyPort());
        else
            _ui.statusMessage("Preference: default HTTP proxy: none");
        if (_client.getDefaultHTTPArchive() != null)
            _ui.statusMessage("Preference: default archive: " + _client.getDefaultHTTPArchive());
        else
            _ui.statusMessage("Preference: default archive: none");
        
        if ( (_client.getDefaultFreenetHost() != null) && (_client.getDefaultFreenetPort() > 0) )
            _ui.statusMessage("Preference: Freenet FCP instance at " + 
                              _client.getDefaultFreenetHost() + ":" + _client.getDefaultFreenetPort());
        if (_client.getDefaultFreenetPrivateKey() != null)
            _ui.statusMessage("Preference: Private Freenet key known");
        if (_client.getDefaultFreenetPublicKey() != null)
            _ui.statusMessage("Preference: Public Freenet key known: " + _client.getDefaultFreenetPublicKey());
    }
    
    private class MenuUI extends NestedUI {
        public MenuUI(UI ui) { super(ui); }
        public void commandComplete(int status, List location) {
            _real.commandComplete(status, getMenuLocation());
        }
    }
    
    public interface Menu {
        public String getName();
        public String getDescription();
        public boolean requireLoggedIn();
        public void listCommands(UI ui);
        public boolean processCommands(DBClient client, UI ui, Opts opts);
        public List getMenuLocation(DBClient client, UI ui);
    }

    public class StartMenu implements Menu {
        public static final String NAME = "start";
        public String getName() { return NAME; }
        public String getDescription() { return "root syndie menu"; }
        public boolean requireLoggedIn() { return false; }
        public void listCommands(UI ui) {
            ui.statusMessage(" login [--db $jdbcURL] [--login $nymLogin --pass $nymPass]");
            ui.statusMessage(" restore --in $file [--db $jdbcURL]: restore the database");
        }
        public boolean processCommands(DBClient client, UI ui, Opts opts) {
            if ("login".equalsIgnoreCase(opts.getCommand())) {
                processLogin(opts);
                _ui.commandComplete(0, null);
                return true;
            } else if ("restore".equalsIgnoreCase(opts.getCommand())) {
                String in = opts.getOptValue("in");
                String db = opts.getOptValue("db");
                if (db == null)
                    db = getDefaultURL();
                if (client == null) {
                    client = new DBClient(I2PAppContext.getGlobalContext(), new File(_rootFile));
                }
                client.restore(ui, in, db);
                return true;
            } else {
                return false;
            }
        }
        public List getMenuLocation(DBClient client, UI ui) { return Collections.EMPTY_LIST; }
    }
    
    public class LoggedInMenu implements Menu {
        public static final String NAME = "loggedin";
        public String getName() { return NAME; }
        public String getDescription() { return "logged in menu"; }
        public boolean requireLoggedIn() { return true; }
        public void listCommands(UI ui) {
            ui.statusMessage(" register [--db $jdbcURL] --login $nymLogin --pass $nymPass --name $nymName");
            ui.statusMessage(" sql $sqlQueryStatement");
            ui.statusMessage(" backup --out $file [--includeArchive $boolean]");
            ui.statusMessage("                    : back up the database to the given (compressed) file,");
            ui.statusMessage("                    : optionally including the signed archive files");
        }
        public boolean processCommands(DBClient client, UI ui, Opts opts) {
            if ("sql".equalsIgnoreCase(opts.getCommand())) {
                processSQL(opts);
                return true;
            } else if ("backup".equalsIgnoreCase(opts.getCommand())) {
                processBackup(opts);
                return true;
            }
            return false;
        }
        public List getMenuLocation(DBClient client, UI ui) { return Collections.EMPTY_LIST; }
    }
}
