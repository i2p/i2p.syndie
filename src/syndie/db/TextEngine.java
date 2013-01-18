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
import net.i2p.util.FileUtil;
import net.i2p.util.SecureFile;
import net.i2p.util.SecureFileOutputStream;

import syndie.Constants;
import syndie.Version;
import syndie.data.SyndieURI;

/**
 *  CLI and startup
 */
public class TextEngine {
    private UI _ui;
    private boolean _exit;
    private DBClient _client;
    private final List<Menu> _menus;
    private String _currentMenu;
    private String _rootFile;
    private File _rootDir;
    private File _dbDir;
    private File _tmpDir;
    private File _archiveDir;
    private File _outboundDir;
    private File _logDir;
    private File _scriptDir;
    private final NestedGobbleUI _gobbleUI;
    private final UI _realUI;
    private final UI _baseUI;
    private final List<String> _commandHistory;
    // no method to add a 2nd one
    private final List<ScriptListener> _scriptListeners;
    private boolean _newNymCreated;
    private boolean _newDatabaseScriptRan;
        
    /** install these if the scripts dir does not exist */
    private static final String[] SCRIPTS = {"defaultprefs", "defaultaliases", "startup", "login", "newdatabase" };

    /**
     * CLI only, will instantiate DBClient
     *
     * This runs the script "startup", which hopefully contains "login"
     */
    public TextEngine(String rootDir, UI ui) { this(rootDir, ui, null); }

    /**
     * CLI only, will instantiate DBClient
     *
     * This runs the script "startup", which hopefully contains "login"
     */
    public TextEngine(String rootDir, UI ui, ScriptListener lsnr) {
        _scriptListeners = new ArrayList();
        if (lsnr != null)
            _scriptListeners.add(lsnr);
        _baseUI = ui;
        _realUI = new MenuUI(ui);
        _ui = _realUI;
        _gobbleUI = new NestedGobbleUI(_realUI);
        _rootFile = rootDir;
        _commandHistory = new ArrayList();
        _menus = new ArrayList();
        rebuildMenus();
        // this instantiates DBClient in CLI-only mode
        buildInstallDir();
        _client.runScript(_ui, "startup");
    }

    /**
     * CLI helper under GUI
     *
     * This runs the script "startup", which hopefully contains "login"
     */
    public TextEngine(DBClient client, UI ui) { this(client, ui, null); }

    /**
     * CLI helper under GUI
     *
     * This runs the script "startup", which hopefully contains "login"
     */
    public TextEngine(DBClient client, UI ui, ScriptListener lsnr) {
        _scriptListeners = new ArrayList();
        if (lsnr != null)
            _scriptListeners.add(lsnr);
        _client = client;
        _baseUI = ui;
        _realUI = new MenuUI(ui);
        _ui = _realUI;
        _gobbleUI = new NestedGobbleUI(_realUI);
        _rootFile = client.getRootDir().getAbsolutePath();
        _commandHistory = new ArrayList();
        _ui.debugMessage("intantiating textengine");
        _menus = new ArrayList();
        rebuildMenus();
        buildInstallDir();
        if ( (_client != null) && (_client.isLoggedIn()) ) {
            _ui.statusMessage("Already logged in");
            _currentMenu = LoggedInMenu.NAME;
            processPrefs(new Opts());
        } else {
            _ui.statusMessage("Custom text engine is not yet logged in");
        }
        _client.runScript(_ui, "startup");
    }
    
    public interface ScriptListener {
        public void scriptComplete(String script);
        public void alreadyRunning();
        public void loginFailed(Exception cause);
        public void loginFailedBadPassphrase();
        public void loginFailedBadLogin();
    }
    
    /** clear all the old state in the various menus, and put us back at the not-logged-in menu */
    private void rebuildMenus() {
        _menus.clear();
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
            _ui.commandComplete(-1, null);
            return true;
        }
    }
    
    /** this really means "new database created" */
    public boolean newNymCreated() { return _newNymCreated; }

    private boolean doRunStep() {
        Opts opts = _ui.readCommand();
        if (opts == null) return false;
        String cmdStr = opts.getCommand();
        boolean ignored = true;
        String origLine = opts.getOrigLine();
        _ui.debugMessage("read command: " + origLine);
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
    
    /**
     *  This also instantiates the DBClient in CLI-only mode
     */
    private void buildInstallDir() {
        _rootDir = new SecureFile(_rootFile);
        _dbDir = new SecureFile(_rootDir, "db");
        _tmpDir = new SecureFile(_rootDir, "tmp");
        _archiveDir = new SecureFile(_rootDir, "archive");
        _outboundDir = new SecureFile(_rootDir, "outbound");
        _logDir = new SecureFile(_rootDir, "logs");
        _scriptDir = new SecureFile(_rootDir, "scripts");
        File indexDir = new SecureFile(_rootDir, "indexes");
        File webDir = new SecureFile(_rootDir, "web");

        boolean dbDirCreated = false;
        if (!_rootDir.exists()) _rootDir.mkdirs();
        if (!_dbDir.exists()) { _dbDir.mkdir(); dbDirCreated = true; }
        if (_tmpDir.exists())
            rmdir(_tmpDir, _rootDir);
        _tmpDir.mkdir();
        if (!_archiveDir.exists()) _archiveDir.mkdir();
        if (!_outboundDir.exists()) _outboundDir.mkdir();
        if (!_logDir.exists()) _logDir.mkdir();
        if (!indexDir.exists()) indexDir.mkdir();
        if (!webDir.exists()) webDir.mkdir();

        // migrate shared index from old to new location
        File oldSI = new File(_archiveDir, LocalArchiveManager.SHARED_INDEX_FILE);
        File newSI = new File(webDir, LocalArchiveManager.SHARED_INDEX_FILE);
        if (oldSI.exists() && !newSI.exists()) {
            FileUtil.rename(oldSI, newSI);
            (new File(_archiveDir, "index.html")).delete();
        }

        if (!_scriptDir.exists()) {
            _scriptDir.mkdir();
            // bundle any scripts we ship with in the .jar
            for (int i = 0; i < SCRIPTS.length; i++) {
                String s = SCRIPTS[i];
                installResource("/scripts/" + s, new File(_scriptDir, s));
            }
        }
        
        File f = new File(webDir, "index.html");
        if (!f.exists())
            installResource("/archive/index.html", f);
        f = new File(webDir, "favicon.ico");
        if (!f.exists())
            installResource("/archive/favicon.ico", f);
        f = new File(webDir, "robots.txt");
        if (!f.exists())
            installResource("/archive/robots.txt", f);
        
        if (_client == null) {
            _client = new DBClient(I2PAppContext.getGlobalContext(), _rootDir);
            // required to prevent NPE when running startup scripts
            _client.setDefaultUI(_ui);
        }        

        if (dbDirCreated) {
            // so it doesn't gather 'command completed'/etc messages on the screen
            _ui.insertCommand("gobble");
            _ui.insertCommand("init");
            //--root '" + _rootFile + "' 
            _ui.insertCommand("register --db '" + getDefaultURL() + "' --login " + DEFAULT_LOGIN + " --pass '" + DEFAULT_PASS + "' --name 'Default account'");
            _ui.insertCommand("ungobble");
            _newNymCreated = true;
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
            in = TextEngine.class.getResourceAsStream(name);
            fos = new SecureFileOutputStream(toFile);
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
            _ui.debugMessage("Can't install", ioe);
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
    
    /** @return /path/to/.syndie/db/syndie (i.e. without the .data suffix) */
    public String getDBFile() { return _dbDir.getPath() + File.separator + "syndie"; }

    /**
     *  TODO: move this to a proper place on Windows & OSX --zab
     *  @return /path/to/.syndie
     */
    public static String getRootPath() { return System.getProperty("user.home") + File.separator + ".syndie"; }

    public DBClient getClient() { return _client; }
    
    public static final String DEFAULT_LOGIN = "user";
    public static final String DEFAULT_PASS = "pass";
    
    private String getDefaultURL() { return "jdbc:hsqldb:file:" + getDBFile() + ";hsqldb.nio_data_file=false"; }
    
    /**
     *  This logs in.
     *  If a new database was created,
     *  it imports default messages for the given UI and runs the 'newdatabase' script.
     *  Then it runs the 'login' script.
     */
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
        
        if (_client == null) {
            _client = new DBClient(I2PAppContext.getGlobalContext(), _rootDir);
        } else if (_client.isLoggedIn()) {
            if (_client.getLogin().equals(login)) {
                _ui.statusMessage("Login successful (already logged in)");
                if (_newNymCreated && !_newDatabaseScriptRan) {
                    _client.runScript(_ui, "newdatabase");
                    _newDatabaseScriptRan = true;
                }
                _client.runScript(_ui, "login");
                return;
            }
        }
        try {
            if (pass == null)
                pass = "";
            _ui.debugMessage("Attempting to log into [" + db + "] w/ ["+login + "]=["+pass +"]");
            _client.setDefaultUI(_ui);
            long nymId = _client.connect(db, login, pass);
            if (nymId >= 0) {
                _ui.statusMessage("Login successful (nymId " + nymId + ")");
                rebuildMenus();
                _currentMenu = LoggedInMenu.NAME;
                
                Properties prefs = _client.getNymPrefs(nymId);
                doSetPrefs(prefs);
                if (_newNymCreated && !_newDatabaseScriptRan) {
                    _client.runScript(_ui, "newdatabase");
                    _newDatabaseScriptRan = true;
                }
                _client.runScript(_ui, "login");
            } else {
                _ui.statusMessage("Login failed");
                if (nymId == DBClient.NYM_ID_LOGIN_UNKNOWN) {
                    for (int i = 0; i < _scriptListeners.size(); i++) {
                        ScriptListener lsnr = (ScriptListener)_scriptListeners.get(i);
                        lsnr.loginFailedBadLogin();
                    }
                } else if (nymId == DBClient.NYM_ID_PASSPHRASE_INVALID) {
                    for (int i = 0; i < _scriptListeners.size(); i++) {
                        ScriptListener lsnr = (ScriptListener)_scriptListeners.get(i);
                        lsnr.loginFailedBadPassphrase();
                    }
                } else {
                    for (int i = 0; i < _scriptListeners.size(); i++) {
                        ScriptListener lsnr = (ScriptListener)_scriptListeners.get(i);
                        lsnr.loginFailed(new Exception("Unknown error - connect rv = " + nymId));
                    }
                }
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
                for (int i = 0; i < _scriptListeners.size(); i++) {
                    ScriptListener lsnr = (ScriptListener)_scriptListeners.get(i);
                    lsnr.alreadyRunning();
                }
            } else {
                _ui.errorMessage("Error trying to login: " + se.getMessage());
                for (int i = 0; i < _scriptListeners.size(); i++) {
                    ScriptListener lsnr = (ScriptListener)_scriptListeners.get(i);
                    lsnr.loginFailed(se);
                }
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
                    return;
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
        } else {
            _ui.errorMessage("No such menu: " + targetMenu);
        }
    }
    private static String padBlank(String name, int paddedSize) {
        StringBuilder buf = new StringBuilder();
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
            _ui.statusMessage("Syndie version: " + Version.VERSION + " (http://syndie.i2p2.de/)");
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
        } else if ("definecmd".equalsIgnoreCase(cmd)) {
            processDefineCommand(opts);
            return true;
        } else if ("?".equalsIgnoreCase(cmd) || "help".equalsIgnoreCase(cmd)) {
            help();
            _ui.commandComplete(0, null);
            return true;
        } else if ("notifyscriptend".equals(cmd)) {
            List args = opts.getArgs();
            _ui.debugMessage("notifyscriptend found: " + args);
            if (args.size() > 0) {
                String script = (String)args.get(0);
                _ui.debugMessage("notifying for " + script);
                for (int i = 0; i < _scriptListeners.size(); i++)
                    ((ScriptListener)_scriptListeners.get(i)).scriptComplete(script);
            }
            return true;
        } else {
            return false;
        }
    }
    
    private void processHistory(Opts opts) {
        for (int i = 0; i < _commandHistory.size(); i++)
            _ui.statusMessage((i+1) + ": " + (String)_commandHistory.get(i));
        _ui.commandComplete(0, null);
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
            String newVal = Constants.replace(prev, orig, replacement, 1);
            _ui.insertCommand(newVal);
        } else {
            _ui.errorMessage("No history to mangle");
            _ui.commandComplete(-1, null);
        }
    }
    
    private void processDefineCommand(Opts opts) {
        String cmdName = opts.getOptValue("name");
        String className = opts.getOptValue("class");
        if ( (cmdName != null) && (className != null) ) {
            try {
                Class cls = Class.forName(className);
                if (CLI.Command.class.isAssignableFrom(cls)) {
                    CLI.setCommand(cmdName, cls);
                    _ui.statusMessage("Defined [" + cmdName + "] to run the command [" + cls.getName() + "]");
                    _ui.commandComplete(0, null);
                } else {
                    _ui.errorMessage("Specified command [" + cls.getName() + "] is not a valid CLI.Command");
                    _ui.commandComplete(-1, null);
                }
            } catch (ClassNotFoundException cnfe) {
                _ui.errorMessage("Specified command [" + className + "] was not found");
                _ui.commandComplete(-1, null);
            }
        } else {
            _ui.errorMessage("Usage: definecmd --name $commandName --class javaClassName");
            _ui.commandComplete(-1, null);
        }
    }
    
    private void processAlias(Opts opts) {
        List args = opts.getArgs();
        if (args.size() <= 0) {
            displayAliases();
        } else {
            String name = (String)args.get(0);
            StringBuilder buf = new StringBuilder();
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
        _ui.commandComplete(0, null);
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
        Menu menu = getCurrentMenu();
        if (menu != null) {
           _ui.statusMessage("<<< " + _currentMenu + " menu commands >>>");
            menu.listCommands(_ui);
        }
        // alphabetical please
        _ui.statusMessage("<<< top menu commands >>>");
        _ui.statusMessage(" alias [aliasName] [command [args...]]");
        _ui.statusMessage(" builduri (--url $url | --channel $chanHash [--message $num [--page $num] )");
        _ui.statusMessage("                    : helper method for building Syndie URIs");
        _ui.statusMessage(" definecmd --name $commandName --class javaClassName");
        _ui.statusMessage(" exit               : exit syndie");
        _ui.statusMessage(" gobble             : suppress all normal status messages (until you 'ungobble')");
        _ui.statusMessage(" history");
        _ui.statusMessage(" init $jdbcURL      : create a new syndie database");
        if (menu != null && menu.requireLoggedIn())
            _ui.statusMessage(" logout             : disconnect from the database, but do not exit syndie");
        _ui.statusMessage(" menu [$newMenu]    : switch between the menus, or view available menus");
        _ui.statusMessage(" prefs [--debug $boolean] [--paginate $boolean] ");
        _ui.statusMessage("       [--httpproxyhost $hostname --httpproxyport $portNum]");
        _ui.statusMessage("       [--archive $archiveURL]");
        _ui.statusMessage("                    : update or display the logged in nym's preferences");
        _ui.statusMessage(" quit               : exit syndie");
        _ui.statusMessage(" toggleDebug        : turn on or off debugging output");
        _ui.statusMessage(" togglePaginate     : turn on or off output pagination");
        _ui.statusMessage(" ungobble");
        if (menu != null && !_currentMenu.equals(LoggedInMenu.NAME))
            _ui.statusMessage(" up                 : go up a menu");
        _ui.statusMessage(" version");
        _ui.statusMessage(" !!                 : repeat last command");
        _ui.statusMessage(" !$num              : repeat command $num from history");
        _ui.statusMessage(" !-$num             : repeat command $num back in history");
        _ui.statusMessage(" ^[from[^to]]       : repeat last command, optionally replacing 'from' with 'to'");
        _ui.statusMessage("<<< custom commands >>>");
        List<String> clicmds = CLI.getHelp();
        for (String cmd : clicmds) {
            _ui.statusMessage(' ' + cmd);
        }
    }
    
    private void processSQL(Opts opts) {
        StringBuilder query = new StringBuilder();
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
        if ( (_client != null) && (_client.isLoggedIn()) ) return;
        List args = opts.getArgs();
        String url = getDefaultURL();
        if (args.size() == 1)
            url = (String)args.get(0);
        try {
            if (_client == null)
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
    
    /**
     *  Import all resources for a given UI
     *  @since 1.102b-8
     */
    private void processImportDefaults(Opts opts) {
        importMsgs("/imports/default");
        if (_baseUI != null)
            importMsgs("/imports/" + _baseUI.getClass().getSimpleName());
    }

    /**
     *  Import all resources in a directory in the jar
     *  @since 1.102b-8 moved from Desktop
     */
    private void importMsgs(String jarPath) {
        _ui.debugMessage("Importing resources from " + jarPath);
        int index = 1;
        while (importMsg(jarPath + "/import_meta" + index + ".syndie"))
            index++;
        index = 1;
        while (importMsg(jarPath + "/import_post" + index + ".syndie"))
            index++;
    }

    /**
     *  Import a single resource from the jar
     *  @since 1.102b-8 moved from Desktop
     */
    private boolean importMsg(String resourceName) {
        try {
            InputStream in = getClass().getResourceAsStream(resourceName);
            if (in == null) {
                return false;
            }
            _ui.debugMessage("Importing resource " + resourceName);
            Importer imp = new Importer(_client);
            boolean ok = imp.processMessage(_ui, in, _client.getLoggedInNymId(), _client.getPass(), null, false, null, null);
            return true;
        } catch (IOException ioe) {
            _ui.errorMessage("Error importing packaged message " + resourceName);
            return false;
        }
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
                        uri = SyndieURI.createMessage(Hash.create(chan), msgId, (int)page);
                    } else {
                        uri = SyndieURI.createMessage(Hash.create(chan), msgId);
                    }
                } else {
                    uri = SyndieURI.createScope(Hash.create(chan));
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
        _client.loadProxyConfig(prefs);
        _client.setDefaultHTTPArchive(prefs.getProperty("archive"));
        
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
                if (client == null)
                    client = new DBClient(I2PAppContext.getGlobalContext(), new File(_rootFile));
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
            ui.statusMessage(" backup --out $file [--includeArchive $boolean]");
            ui.statusMessage("                    : back up the database to the given (compressed) file,");
            ui.statusMessage("                    : optionally including the signed archive files");
            ui.statusMessage(" importDefaults     : import default keys and posts from the jar");
            ui.statusMessage(" register [--db $jdbcURL] --login $nymLogin --pass $nymPass --name $nymName");
            ui.statusMessage(" sql $sqlQueryStatement");
        }
        public boolean processCommands(DBClient client, UI ui, Opts opts) {
            if ("sql".equalsIgnoreCase(opts.getCommand())) {
                processSQL(opts);
                return true;
            } else if ("backup".equalsIgnoreCase(opts.getCommand())) {
                processBackup(opts);
                return true;
            } else if ("importdefaults".equalsIgnoreCase(opts.getCommand())) {
                processImportDefaults(opts);
                return true;
            }
            return false;
        }
        public List getMenuLocation(DBClient client, UI ui) { return Collections.EMPTY_LIST; }
    }
}
