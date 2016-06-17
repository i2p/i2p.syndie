package syndie.db;

import java.io.*;
import java.net.URISyntaxException;
import java.sql.*;
import java.text.Collator;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;
import javax.sql.rowset.serial.SerialBlob;
import javax.sql.rowset.serial.SerialClob;

import net.i2p.I2PAppContext;
import net.i2p.crypto.KeyGenerator;
import net.i2p.data.Base64;
import net.i2p.data.DataHelper;
import net.i2p.data.SessionKey;
import net.i2p.data.PrivateKey;
import net.i2p.data.PublicKey;
import net.i2p.data.SigningPrivateKey;
import net.i2p.data.SigningPublicKey;
import net.i2p.data.Signature;
import net.i2p.data.Hash;
import net.i2p.util.Log;
import net.i2p.util.SecureFile;
import net.i2p.util.SecureFileOutputStream;
import net.i2p.util.SimpleTimer2;

import syndie.Constants;
import syndie.data.ArchiveInfo;
import syndie.data.BugConfig;
import syndie.data.CancelPolicy;
import syndie.data.ChannelInfo;
import syndie.data.ExpirationPolicy;
import syndie.data.MessageInfo;
import syndie.data.NymKey;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.WatchedChannel;
import static syndie.db.ImportResult.Detail.*;
import syndie.util.StringUtil;


/**
 *  The interface to the database itself
 *
 *  Note: "messageId" parameters are generally the external ID (timestamp),
 *  and "msgId" paramters are the internal database ID.
 *
 */
public class DBClient {

    private static final Class[] _gcjKludge = new Class[] { 
        org.hsqldb.jdbcDriver.class
        //, org.hsqldb.GCJKludge.class
        //, org.hsqldb.persist.GCJKludge.class
    };

    static {
        try {
            Class.forName("org.hsqldb.jdbcDriver");
        } catch(Exception exc) {
        }
    }

    private final I2PAppContext _context;
    private UI _ui;
    private final Log _log;
    
    /**
     * should we defrag the hsqldb every 10 shutdowns?  it can take a while, so
     * its probably best to disable this when doing development 
     *
     * Actually, it needs to be disabled for release, until we can
     * make it much smarter or faster. Don't run defrag unless
     * we imported a lot of stuff, for example.
     */
    private static final boolean DEFRAG = false;
    
    /** for createEdition() */
    private static final long PAST_RANDOM_PERIOD = 6*60*60*1000l;

    /** for createEdition() */
    private static final long FUTURE_RANDOM_PERIOD = 60*60*1000l;

    public static final long MIN_ATT_BLOB_SIZE = 16*1024;
    public static final long MIN_PAGE_CLOB_SIZE = 32*1024;


    private Connection _con;
    private final SyndieURIDAO _uriDAO;
    /** for the DB, default USER/PASS */
    private String _login;
    private String _pass;
    /** for the nym, defautl user/pass */
    private String _nymLogin;
    private String _nymPass;
    private long _nymId;
    private File _rootDir;
    private String _url;
    private Thread _shutdownHook;
    private boolean _shutdownInProgress;
    private boolean _shouldDefrag;
    private String _defaultArchive;
    private String _httpProxyHost;
    private int _httpProxyPort;
    private String _fcpHost;
    private int _fcpPort;
    private String _freenetPrivateKey;
    private String _freenetPublicKey;
    
    private int _numNymKeysWithoutPass;
    
    private ExpireEvent _expireEvent;
        
    private static final String DEFAULT_ADMIN = "SA";


    /**
     *  @param rootDir should be a SecureFile
     */
    public DBClient(I2PAppContext ctx, File rootDir) {
        _context = ctx;
        // we are probably safe with the small exponent size, but asym 
        // encryption and decryption is rare enough in syndie that its reasonable
        // to go up to the full 2048bits
		// NOTE: we use the *short* exponent size here now!
        // KeyGenerator.PUBKEY_EXPONENT_SIZE = KeyGenerator.PUBKEY_EXPONENT_SIZE_FULL;
		
        _log = ctx.logManager().getLog(getClass());
        _rootDir = rootDir;
        _shutdownInProgress = false;
        _shouldDefrag = DEFRAG;
        _uriDAO = new SyndieURIDAO(this);
    }
    
    public void restart(String rootDir) {
        _rootDir = new SecureFile(rootDir);
        _shutdownInProgress = false;
        disconnect();
    }
    
    //private static final String SQL_CREATE_USER = "CREATE USER ? PASSWORD ? ADMIN";
    
    /**
     *  Initialize the DB, update to latest version if necessary, and connect.
     *  Does nothing if already connected.
     */
    void connect(String url) throws SQLException { 
        if (_con != null && !_con.isClosed()) {
            // don't leak connections
            log("connect() called, already connected");
        }
        _login = TextEngine.DEFAULT_LOGIN;
        if (_pass == null) _pass = TextEngine.DEFAULT_PASS;
        long start = System.currentTimeMillis();
        //System.out.println("Connecting to " + url);
        _url = url;
        try {
            _con = DriverManager.getConnection(url);
            if (_con != null) {
                String version = _con.getMetaData().getDatabaseProductVersion();
                log("default connection successful, version: " + version);
                Statement stmt = null;
                ResultSet rs= null;
                try {
                    // test if user exists
                    if (DBUpgrade.isHsqldb20(_con)) {
                        // This works on hsqldb 2.0 and higher
                        // The table is there in 1.8 but you can't select from it
                        stmt = _con.prepareStatement("SELECT * FROM INFORMATION_SCHEMA.SYSTEM_USERS WHERE user_name = ?");
                        ((PreparedStatement) stmt).setString(1, _login);
                        rs = ((PreparedStatement) stmt).executeQuery();
                        if (!rs.next())
                            throw new SQLException("Login does not exist: " + _login);
                        rs.close();
                    } else {
                        // Does not work as of hsqldb 2.0 (and should have been a method name anyway)
                        stmt = _con.createStatement();
                        stmt.execute("GRANT ALL ON CLASS \"java.lang.String.valueOf\" TO \"" + _login + '"');
                    }
                    log("Account " + _login + " already exists");
                    stmt.close();
                    stmt = null;
                    
                    byte val[] = new byte[16];
                    _context.random().nextBytes(val);
                    String rand = Base64.encode(val);
                    log("changing default admin account passphrase to something random");
                    stmt = _con.createStatement();
                    stmt.execute("ALTER USER \"" + DEFAULT_ADMIN + "\" SET PASSWORD '" + rand + "'");
                    stmt.close();
                    stmt = null;
                    log("sysadmin passphrase changed to a random value");
                    
                    _con.close();
                    _con = null;
                } catch (SQLException se) {
                    // this is the usual path for a new database
                    log("Unable to check up on " + _login + ", so lets create the account (" + se.getMessage() + ")");
                    if (stmt != null)
                        stmt.close();
                    if (rs != null)
                        rs.close();
                    stmt = null;
                    
                    try {
                        stmt = _con.createStatement();
                        stmt.execute("CREATE USER \"" + _login + "\" PASSWORD '" + _pass + "' ADMIN");
                        stmt.close();
                        stmt = null;
                        log("new user created [" + _login + "] / [" + _pass + "]");

                        _con.close();
                        _con = null;
                    } catch (SQLException cse) {
                        log("Error creating new db account", cse);
                        _con.close();
                        _con = null;
                    }
                }
            }
        } catch (SQLException se) {
            // this is the typical path for an existing db
            log("Unable to connect with default db params: " + se);
            if (_con != null) _con.close();
            _con = null;
            // checkHeartbeat is in the error message for when the db is already open
            if ( (se.getMessage() != null) && (se.getMessage().indexOf("checkHeartbeat()") >= 0) ) {
                throw se;
            }
        }
         
        try {
            // this is the typical path for an existing db
            log("connecting as [" + _login + "] / [" + _pass + "]");
            _con = DriverManager.getConnection(url, _login, _pass);
            String version = _con.getMetaData().getDatabaseProductVersion();
            log("connection successful, version: " + version);
        } catch (SQLException se) {
            _con = null;
            throw se;
        }
        if (_con == null) // might be redundant...
            throw new SQLException("Unable to connect to [" + _login + "]");
        
        long connected = System.currentTimeMillis();
        if (_shutdownHook == null) {
            _shutdownHook = new Thread(new Runnable() {
                public void run() {
                    _shutdownInProgress = true;
                    close();
                }
            }, "DB shutdown");
            Runtime.getRuntime().addShutdownHook(_shutdownHook);
        } else {
            //throw new RuntimeException("already connected");
        }

        // If we upgraded hsqldb, SHUTDOWN COMPACT and reconnect
        // We must do this before initDB() so ddl updates will work correctly
        boolean shouldDefrag = DBUpgrade.postConnect(_con);
        if (shouldDefrag) {
            _log.logAlways(Log.WARN, "Starting database shutdown with compaction, this may take a while");
            // shutdown sometimes fails with random errors, this seems to help
            try {
                Thread.sleep(5000);
            } catch (InterruptedException ie) {}
            PreparedStatement stmt = null;
            try {
                stmt = _con.prepareStatement("SHUTDOWN COMPACT");
                stmt.execute();
                _log.logAlways(Log.WARN, "Database shutdown complete");
            } catch (SQLException se) {
                // only sometimes...
                // java.sql.SQLException: error in script file line: 107 org.hsqldb.HsqlException: user lacks privilege or object not found: SYSTEM_LOBS.APPVERSION in statement [SHUTDOWN COMPACT]
                _log.error("Database shutdown failed", se);
                // keep going, hope it works anyway
                // but this is very unlikely to work
            } finally {
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
                try {
                    _con.close();
                } catch (SQLException se) {
                    _log.error("Error closing conn", se);
                }
                _con = null;
            }
            log("reconnecting as [" + _login + "] / [" + _pass + "]");
            _con = DriverManager.getConnection(url, _login, _pass);
            String version = _con.getMetaData().getDatabaseProductVersion();
            log("reconnection successful, version: " + version);
            _shouldDefrag = true;
        }
        
        // process all updates
        DBInit dbi = new DBInit(_context, _con);
        dbi.initDB();

        if (shouldDefrag) {
            log("migrating to lobs start");
            migrateToLob("messageAttachmentData", "msgId", "attachmentNum", "dataBinary", MIN_ATT_BLOB_SIZE, true);
            migrateToLob("messagePageData", "msgId", "pageNum", "dataString", MIN_PAGE_CLOB_SIZE, false);
            // not worth fixing getResumable() and reencryptPostponed()
            //migrateToLob("nymMsgPostpone", "postponeId", "postponeVersion", "encryptedData", 65536, false);
            log("migrating to lobs done");
        }

        long init = System.currentTimeMillis();
        //_login = null;
        //_pass = null;
        _nymId = -1;
        log("connecting: driver connection time: " + (connected-start) + " initDb time: " + (init-connected));
        
        // We must switch from the uppper-case to lower-case default password now
        // or verifyNymKeyEncryption() will fail
        if (_pass.equals(TextEngine.DEFAULT_PASS))
            _nymPass = TextEngine.DEFAULT_NYMKEY_PASS;
        else
            _nymPass = _pass;

        boolean ok = verifyNymKeyEncryption();
        if (!ok) {
            log("db connection successfull, but we can't access the nym keys, so discon");
            disconnect();
        } else {
            // We must switch from the uppper-case to lower-case default login now
            // or getNymId() in connect() below will fail
            if (_login.equals(TextEngine.DEFAULT_LOGIN))
                _nymLogin = TextEngine.DEFAULT_NYMKEY_LOGIN;
            else
                _nymLogin = _login;
            if (_expireEvent == null) {
                long delay = _context.random().nextLong(60*60*1000l) + 24*60*60*1000l;
                _expireEvent = new ExpireEvent();
                _expireEvent.schedule(delay);
            }
        }
    }
    
    /**
     *  The current hsqldb library version
     *
     *  @return library version or "unknown"
     *  @since 1.104b-2
     */
    public String getHsqldbVersion() {
        return DBUpgrade.getHsqldbVersion(_con);
    }

    private class ExpireEvent extends SimpleTimer2.TimedEvent {
    	ExpireEvent() {
    		super(SimpleTimer2.getInstance());
    	}
        public void timeReached() {
            _ui.debugMessage("running periodic expiration");
            Expirer expirer = new Expirer(DBClient.this, _ui);
            expirer.expireMessages();
            _ui.debugMessage("periodic expiration complete");
            long delay = _context.random().nextLong(60*60*1000l) + 24*60*60*1000l;
            reschedule(delay);
        }
    }
    
    /**
     *  Initialize the DB, update to latest version if necessary, and connect.
     *  Does nothing if already connected with this login
     *  Called from TextEngine.processLogin()
     *
     *  @return the logged-in nym ID
     *  @throws SQLException if already connected with a different login
     */
    long connect(String url, String login, String passphrase) throws SQLException {
        if (_con != null && !_con.isClosed()) {
            // don't leak connections or change logins on the fly
            log("connect(...) called, already connected as \"" + _login + '"', new Exception());
            if (StringUtil.lowercase(login).equals(StringUtil.lowercase(_login)))
                return getNymId(_nymLogin, _nymPass);
            throw new SQLException("Already logged in as \"" + _login + '"');
        }
        _login = login;
        _pass = passphrase;
        try {
            connect(url);
            if (!isLoggedIn())
                return -1;
        } catch (SQLException se) {
            _con = null;
            throw se;
        }
        // connect() above sets _nymLogin and _nymPass
        return getNymId(_nymLogin, _nymPass);
    }

    public boolean reconnect(String passphrase) {
        log("reconnecting to url=[" + _url + "] login=[" + _login + "] pass=[" + passphrase + "]");
        try {
            long id = connect(_url, _login, passphrase);
            log("connected w/ id=" + id);
            if (id >= 0) {
                return true;
            } else {
                _con = null;
                return false;
            }
        } catch (SQLException se) {
            log("Error reconnecting: " + se.getMessage());
            _con = null;
            return false;
        }
    }

    public void disconnect() {
        clearNymChannelCache();
        try {
            if ( (_con != null) && (!_con.isClosed()) ) {
                log("Disconnecting from DB");
                _con.close();
                _con = null;
            }
        } catch (SQLException se) {
            log("Error disconnecting", se);
            _con = null;
        }
        if (_expireEvent != null)
            _expireEvent.cancel();
    }

    I2PAppContext ctx() { return _context; }
    public Connection con() { return _con; }
    public Hash sha256(byte data[]) { return _context.sha().calculateHash(data); }
    public void setDefaultUI(UI ui) { _ui = ui; }
    
    /**
     *  If logged in, the login used is returned here.
     *  This is the DB login, NOT the nym login.
     */
    String getLogin() { return _login; }

    /**
     *  If logged in, the password authenticating it is returned here.
     *  This is the nym password, NOT the DB password.
     */
    String getPass() { return _nymPass; }

    /**
     *  Sets the nym password, NOT the DB password.
     *  Used only by desktop.
     */
    public void setPass(String encryptionPass) { _nymPass = encryptionPass; }

    /**
     *  TODO, ensureLoggedIn() may still throw an ISE even if isLoggedIn() returns true
     */
    public boolean isLoggedIn() { return _con != null && _login != null; }

    /** if logged in, the internal nymId associated with that login */
    public long getLoggedInNymId() { return _nymId; }
    
    public File getRootDir() { return _rootDir; }
    public File getTempDir() { return new SecureFile(_rootDir, "tmp"); }

    /** used for locally generated messages */
    public File getOutboundDir() { return new File(_rootDir, "outbound"); }
    public File getArchiveDir() { return new File(_rootDir, "archive"); }
    public File getWebDir() { return new File(_rootDir, "web"); }
    
    public String getDefaultHTTPProxyHost() { return _httpProxyHost; }
    public void setDefaultHTTPProxyHost(String host) { _httpProxyHost = host; }
    public int getDefaultHTTPProxyPort() { return _httpProxyPort; }
    public void setDefaultHTTPProxyPort(int port) { _httpProxyPort = port; }
    public String getDefaultHTTPArchive() { return _defaultArchive; }
    public void setDefaultHTTPArchive(String archive) { _defaultArchive = archive; }
    
    public String getDefaultFreenetHost() { return _fcpHost; }
    public void setDefaultFreenetHost(String host) { _fcpHost = host; }
    public int getDefaultFreenetPort() { return _fcpPort; }
    public void setDefaultFreenetPort(int port) { _fcpPort = port; }
    public String getDefaultFreenetPrivateKey() { return _freenetPrivateKey; }
    public void setDefaultFreenetPrivateKey(String privateSSK) { _freenetPrivateKey = privateSSK; }
    public String getDefaultFreenetPublicKey() { return _freenetPublicKey; }
    public void setDefaultFreenetPublicKey(String publicSSK) { _freenetPublicKey = publicSSK; }
    
    public void close() {
        _login = null;
        _pass = null;
        _nymLogin = null;
        _nymPass = null;
        _nymId = -1;
        _defaultArchive = null;
        _httpProxyHost = null;
        _httpProxyPort = -1;
        _fcpHost = null;
        _fcpPort = -1;
        _freenetPrivateKey = null;
        _freenetPublicKey = null;
        PreparedStatement stmt = null;
        try {
            if (_con == null) return;
            if (_con.isClosed()) return;
            if (_shouldDefrag) { // && System.currentTimeMillis() % 100 > 95) // every 10 times defrag the db
                _log.logAlways(Log.WARN, "Starting database shutdown with compaction, this may take a while");
                stmt = _con.prepareStatement("SHUTDOWN COMPACT");
            } else {
                _log.logAlways(Log.WARN, "Starting database shutdown");
                stmt = _con.prepareStatement("SHUTDOWN");
            }
            stmt.execute();
            _log.logAlways(Log.WARN, "Database shutdown complete");
            //if (_log.shouldLog(Log.INFO))
            //    _log.info("Database shutdown complete", new Exception("shutdown by"));
            stmt.close();
            stmt = null;
            _con.close();
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error closing the connection and shutting down the database", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        Thread hook = _shutdownHook;
        _shutdownHook = null;
        if (!_shutdownInProgress && (hook != null))
            Runtime.getRuntime().removeShutdownHook(hook);
    }
    
    String getString(String query, int column, long keyVal) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(query);
            stmt.setLong(1, keyVal);
            rs = stmt.executeQuery();
            if (rs.next()) {
                String rv = rs.getString(column);
                if (!rs.wasNull())
                    return rv;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error fetching the string", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return null;
    }
  
    public static final long NYM_ID_LOGIN_UNKNOWN = -1;
    public static final long NYM_ID_PASSPHRASE_INVALID = -2;
    public static final long NYM_ID_LOGIN_ALREADY_EXISTS = -3;
    
    private static final String SQL_GET_NYM_ID = "SELECT nymId, passSalt, passHash FROM nym WHERE login = ?";

    /**
     * if the passphrase is blank, simply get the nymId for the login, otherwise
     * authenticate the passphrase, returning -1 if the login doesn't exist, -2
     * if the passphrase is invalid, or the nymId if it is correct.  If the nym and
     * password are both set and are authenticated, they are stored in memory on
     * the DBClient itself and can be queried with getLogin() and getPass().
     */
    public long getNymId(String login, String passphrase) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_NYM_ID);
            stmt.setString(1, login);
            rs = stmt.executeQuery();
            if (rs.next()) {
                long nymId = rs.getLong(1);
                byte salt[] = rs.getBytes(2);
                byte hash[] = rs.getBytes(3);
                if (passphrase == null) {
                    return nymId;
                } else {
                    byte calc[] = _context.keyGenerator().generateSessionKey(salt, DataHelper.getUTF8(passphrase)).getData();
                    if (DataHelper.eq(calc, hash)) {
                        _nymLogin = login;
                        _nymPass = passphrase;
                        _nymId = nymId;
                        log("passphrase is correct in the nym table for \"" + login + '"');
                        
                        Properties prefs = getNymPrefs(nymId);
                        loadProxyConfig(prefs);
                        return nymId;
                    } else {
                        log("Invalid passphrase for the nymId \"" + login + '"');
                        return NYM_ID_PASSPHRASE_INVALID;
                    }
                }
            } else {
                log("no nymId values are known for \"" + login + '"');
                return NYM_ID_LOGIN_UNKNOWN;
            }
        } catch (SQLException se) {
            log("Unable to check the get the nymId", se);
            return NYM_ID_LOGIN_UNKNOWN;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_NYMIDS = "SELECT nymId FROM nym";

    public List<Long> getNymIds() {
        ensureLoggedIn();
        List<Long> rv = new ArrayList();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_NYMIDS);
            rs = stmt.executeQuery();
            while (rs.next()) {
                long nymId = rs.getLong(1);
                if (!rs.wasNull())
                    rv.add(Long.valueOf(nymId));
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Unable to list the nymIds", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return rv;
    }
    
    private static final String SQL_INSERT_NYM = "INSERT INTO nym (nymId, login, publicName, passSalt, passHash, isDefaultUser) VALUES (?, ?, ?, ?, ?, ?)";

    /**
     *  All params case-sensitive!
     */
    public long register(String login, String passphrase, String publicName) {
        long nymId = nextId("nymIdSequence");
        byte salt[] = new byte[16];
        _context.random().nextBytes(salt);
        byte hash[] = _context.keyGenerator().generateSessionKey(salt, DataHelper.getUTF8(passphrase)).getData();
        
        PreparedStatement stmt = null;
        try {
            stmt = _con.prepareStatement(SQL_INSERT_NYM);
            stmt.setLong(1, nymId);
            stmt.setString(2, login);
            stmt.setString(3, publicName);
            stmt.setBytes(4, salt);
            stmt.setBytes(5, hash);
            stmt.setBoolean(6, false);
            int rows = stmt.executeUpdate();
            if (rows != 1)
                return NYM_ID_LOGIN_ALREADY_EXISTS;
            else
                return nymId;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Unable to register the nymId", se);
            return NYM_ID_LOGIN_ALREADY_EXISTS;
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    public long nextId(String seq) {
        if (_con == null) throw new IllegalStateException("not connected");
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            //String query = "SELECT NEXT VALUE FOR " + seq + " FROM information_schema.system_sequences WHERE sequence_name = '" + seq.toUpperCase() + "'";
            String query = "CALL NEXT VALUE FOR " + seq;
            stmt = _con.prepareStatement(query);
            rs = stmt.executeQuery();
            if (rs.next()) {
                long rv = rs.getLong(1);
                if (rs.wasNull())
                    return -1;
                else
                    return rv;
            } else {
                return -1;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the next sequence ID", se);
            return -1;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    /**
     *  @return stored URI
     */
    SyndieURI getURI(long uriId) {
        ensureLoggedIn(false);
        return _uriDAO.fetch(uriId);
    }

    /**
     *  @return urlID
     */
    long addURI(SyndieURI uri) {
        ensureLoggedIn(false);
        return _uriDAO.add(uri);
    }
    
/****
    public static void main(String args[]) {
        DBClient client = new DBClient(I2PAppContext.getGlobalContext(), new SecureFile(TextEngine.getRootPath()));
        try {
            client.connect("jdbc:hsqldb:file:/tmp/testSynDB;hsqldb.nio_data_file=false");
            client.close();
        } catch (SQLException se) {
            se.printStackTrace();
        }
    }
****/
    
    public void saveProxyConfig() {
        Properties props = getNymPrefs(_nymId);
        if ( (getDefaultFreenetHost() == null) || (getDefaultFreenetPort() <= 0) ) {
            props.remove("fcpHost");
            props.remove("fcpPort");
        } else {
            props.setProperty("fcpHost", getDefaultFreenetHost());
            props.setProperty("fcpPort", getDefaultFreenetPort()+"");
        }
        
        if ( (getDefaultFreenetPrivateKey() == null) || (getDefaultFreenetPublicKey() == null) ) {
            props.remove("freenetPrivateKey");
            props.remove("freenetPublicKey");
        } else {
            props.setProperty("freenetPrivateKey", getDefaultFreenetPrivateKey());
            props.setProperty("freenetPublicKey", getDefaultFreenetPublicKey());
        }
        
        if ( (getDefaultHTTPProxyHost() == null) || (getDefaultHTTPProxyPort() <= 0) ) {
            props.remove("httpproxyhost");
            props.remove("httpproxyport");
        } else {
            props.setProperty("httpproxyhost", getDefaultHTTPProxyHost());
            props.setProperty("httpproxyport", getDefaultHTTPProxyPort()+"");
        }
        
        log("saveProxyConfig [" + getDefaultHTTPProxyHost() +'/' + getDefaultHTTPProxyPort() + "]: " + props);
        setNymPrefs(_nymId, props);
    }
    
    public void loadProxyConfig(Properties prefs) {
        if (prefs == null) prefs = new Properties();
        setDefaultHTTPProxyHost(prefs.getProperty("httpproxyhost"));
        String port = prefs.getProperty("httpproxyport");
        if (port != null) {
            try {
                int num = Integer.parseInt(port);
                setDefaultHTTPProxyPort(num);
            } catch (NumberFormatException nfe) {
                log("HTTP proxy port preference is invalid", nfe);
                setDefaultHTTPProxyPort(-1);
                setDefaultHTTPProxyHost(null);
            }
        } else {
            setDefaultHTTPProxyPort(-1);
            setDefaultHTTPProxyHost(null);
        }
        
        setDefaultFreenetPrivateKey(prefs.getProperty("freenetPrivateKey"));
        setDefaultFreenetPublicKey(prefs.getProperty("freenetPublicKey"));
        setDefaultFreenetHost(prefs.getProperty("fcpHost"));
        port = prefs.getProperty("fcpPort");
        if (port != null) {
            try {
                int num = Integer.parseInt(port);
                setDefaultFreenetPort(num);
            } catch (NumberFormatException nfe) {
                log("Freenet port preference is invalid", nfe);
                setDefaultFreenetPort(-1);
            }
        } else {
            setDefaultFreenetPort(-1);
        }
    }
    
    /**
     *  Must be logged in, caller must ensure isLoggedIn()
     */
    private void exec(String cmd) throws SQLException {
        if (_log.shouldLog(Log.DEBUG))
            _log.debug("Exec [" + cmd + "]");
        PreparedStatement stmt = null;
        try {
            stmt = _con.prepareStatement(cmd);
            stmt.executeUpdate();
        } finally { 
            if (stmt != null) stmt.close();
        }
    }

    /**
     *  Must be logged in, caller must ensure isLoggedIn()
     */
    int exec(String sql, long param1) throws SQLException {
        //if (_log.shouldLog(Log.DEBUG))
        //    _log.debug("Exec param [" + sql + "]");
        PreparedStatement stmt = null;
        try {
            stmt = _con.prepareStatement(sql);
            stmt.setLong(1, param1);
            return stmt.executeUpdate();
        } finally { 
            if (stmt != null) stmt.close();
        }
    }

    /**
     *  Must be logged in, caller must ensure isLoggedIn()
     */
    int exec(String sql, long param1, long param2) throws SQLException {
        //if (_log.shouldLog(Log.DEBUG))
        //    _log.debug("Exec param [" + sql + "]");
        PreparedStatement stmt = null;
        try {
            stmt = _con.prepareStatement(sql);
            stmt.setLong(1, param1);
            stmt.setLong(2, param2);
            return stmt.executeUpdate();
        } finally { 
            if (stmt != null) stmt.close();
        }
    }

    /**
     *  Must be logged in, caller must ensure isLoggedIn()
     */
    void exec(String query, UI ui) {
        ui.debugMessage("Executing [" + query + "]");
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(query);
            String lc = StringUtil.lowercase(query);
            if (!lc.startsWith("select") && !lc.startsWith("call")) {
                int rows = stmt.executeUpdate();
                ui.statusMessage("Command completed, updating " + rows + " rows");
                ui.commandComplete(rows, null);
                return;
            }
            rs = stmt.executeQuery();
            ResultSetMetaData md = stmt.getMetaData();
            int rows = 0;
            while (rs.next()) {
                rows++;
                ui.statusMessage("----------------------------------------------------------");
                for (int i = 0; i < md.getColumnCount(); i++) {
                    Object obj = rs.getObject(i+1);
                    if (obj != null) {
                        if (obj instanceof byte[]) {
                            String str = Base64.encode((byte[])obj);
                            if (str.length() <= 32)
                                ui.statusMessage(md.getColumnLabel(i+1) + ":\t" + str);
                            else
                                ui.statusMessage(md.getColumnLabel(i+1) + ":\t" + str.substring(0,32) + "...");
                        } else {
                            ui.statusMessage(md.getColumnLabel(i+1) + ":\t" + obj.toString());
                        }
                    } else {
                        ui.statusMessage(md.getColumnLabel(i+1) + ":\t[null value]");
                    }
                }
            }
            ui.statusMessage("Rows matching the query: " + rows);
            ui.commandComplete(rows, null);
        } catch (SQLException se) {
            ui.errorMessage("Error executing the query", se);
            ui.commandComplete(-1, null);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }


    private static final String SQL_GET_READ_KEY_PRIVACY = "SELECT wasPublic FROM channelReadKey WHERE channelId = ? AND keyData = ? ORDER BY keyStart ASC";
    
    public boolean getChannelReadKeyIsPublic(Hash channel, SessionKey key) {
        if ( (key == null) || (key.getData() == null) ) return false;
        long channelId = getChannelId(channel);
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_READ_KEY_PRIVACY);
            stmt.setLong(1, channelId);
            stmt.setBytes(2, key.getData());
            rs = stmt.executeQuery();
            if (rs.next()) {
                boolean wasPublic = rs.getBoolean(1);
                if (rs.wasNull()) wasPublic = false;
                return wasPublic;
            }
            return false;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the read key's status", se);
            return false;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_READKEYS = "SELECT keyType, keyData, keySalt, authenticated, keyPeriodBegin, keyPeriodEnd " +
                                                   "FROM nymKey WHERE " + 
                                                   "keyChannel = ? AND nymId = ? AND keyFunction = '" + Constants.KEY_FUNCTION_READ + "'";
    private static final String SQL_GET_CHANREADKEYS_RW = "SELECT DISTINCT keyData, keyStart FROM channelReadKey WHERE channelId = ? AND keyEnd IS NULL ORDER BY keyStart ASC";
    private static final String SQL_GET_CHANREADKEYS_RO = "SELECT DISTINCT keyData, keyStart FROM channelReadKey WHERE channelId = ? ORDER BY keyStart ASC";

    /** 
     * list of SessionKey instances that the nym specified can use to try and read/write 
     * posts to the given identHash channel
     *
     * @param onlyIncludeForWriting if true, only list the read keys we can use for writing a post (meaning
     *        those that have not been deprecated)
     * @return non-null
     */
    public List<SessionKey> getReadKeys(Hash identHash, boolean onlyIncludeForWriting) {
        return getReadKeys(identHash, _nymId, _nymPass, onlyIncludeForWriting);
    }

    /** 
     * list of SessionKey instances that the nym specified can use to try and read/write 
     * posts to the given identHash channel
     *
     * @param onlyIncludeForWriting if true, only list the read keys we can use for writing a post (meaning
     *        those that have not been deprecated)
     * @return non-null
     */
    public List<SessionKey> getReadKeys(Hash identHash, long nymId, String nymPassphrase, boolean onlyIncludeForWriting) {
        List<SessionKey> rv = new ArrayList(4);
        if (identHash == null) return null;
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_READKEYS);
            stmt.setBytes(1, identHash.getData());
            stmt.setLong(2, nymId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                String type = rs.getString(1);
                byte data[] = rs.getBytes(2);
                byte salt[] = rs.getBytes(3);
                boolean auth= rs.getBoolean(4);
                //Date begin  = rs.getDate(5);
                //Date end    = rs.getDate(6);
                
                if (Constants.KEY_TYPE_AES256.equals(type)) {
                    if (salt != null) {
                        byte decr[] = pbeDecrypt(data, salt);
                        rv.add(new SessionKey(decr));
                        //byte readKey[] = new byte[SessionKey.KEYSIZE_BYTES];
                        //SessionKey saltedKey = _context.keyGenerator().generateSessionKey(salt, pass);
                        //_context.aes().decrypt(data, 0, readKey, 0, saltedKey, salt, data.length);
                        //int pad = (int)readKey[readKey.length-1];
                        //byte key[] = new byte[readKey.length-pad];
                        //System.arraycopy(readKey, 0, key, 0, key.length);
                        //rv.add(new SessionKey(key));
                    } else {
                        rv.add(new SessionKey(data));
                    }
                } else {
                    // we dont know how to deal with anything but AES256
                }
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the read keys", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        // ok, that covers nym-local keys, now lets look for any channelReadKeys that came from
        // signed channel metadata
        long channelId = getChannelId(identHash);
        try {
            if (onlyIncludeForWriting)
                stmt = _con.prepareStatement(SQL_GET_CHANREADKEYS_RW);
            else
                stmt = _con.prepareStatement(SQL_GET_CHANREADKEYS_RO);
            //stmt.setBytes(1, identHash.getData());
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                byte key[] = rs.getBytes(1);
                if ( (key != null) && (key.length == SessionKey.KEYSIZE_BYTES) )
                    rv.add(new SessionKey(key));
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel read keys", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return rv;
    }

    private static final String SQL_GET_KNOWN_EDITION = "SELECT MAX(edition) FROM channel WHERE channelHash = ?";

    /** highest channel meta edition, or -1 if unknown */
    public long getKnownEdition(Hash ident) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_KNOWN_EDITION);
            stmt.setBytes(1, ident.getData());
            rs = stmt.executeQuery();
            if (rs.next()) {
                long edition = rs.getLong(1);
                if (rs.wasNull())
                    return -1;
                else
                    return edition;
            } else {
                return -1;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel's meta edition", se);
            return -1;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    private static final String SQL_GET_CHANNEL_IDS = "SELECT channelId, channelHash FROM channel";

    /** retrieve a mapping of channelId (Long) to channel hash (Hash) */
    public Map<Long, Hash> getChannelIds() {
        Map<Long, Hash> rv = new HashMap<Long, Hash>();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_IDS);
            rs = stmt.executeQuery();
            while (rs.next()) {
                long id = rs.getLong(1);
                if (rs.wasNull())
                    continue;
                byte hash[] = rs.getBytes(2);
                if (rs.wasNull())
                    continue;
                if (hash.length != Hash.HASH_LENGTH)
                    continue;
                rv.put(Long.valueOf(id), Hash.create(hash));
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel list", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return rv;
    }
    
    private static final String SQL_GET_CHANNEL_HASH = "SELECT channelHash FROM channel WHERE channelId = ?";

    public Hash getChannelHash(long channelId) {
        if (channelId < 0) return null;
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_HASH);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                byte chanHash[] = rs.getBytes(1);
                if ( (chanHash != null) && (chanHash.length == Hash.HASH_LENGTH) )
                    return Hash.create(chanHash);
                return null;
            } else {
                return null;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel hash", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_CHANNEL_ID = "SELECT channelId FROM channel WHERE channelHash = ?";

    public long getChannelId(Hash channel) {
        if (channel == null) return -1;
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_ID);
            stmt.setBytes(1, channel.getData());
            rs = stmt.executeQuery();
            if (rs.next()) {
                long id = rs.getLong(1);
                if (rs.wasNull())
                    return -1;
                else
                    return id;
            } else {
                return -1;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel id", se);
            return -1;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_CHANNEL_NAME = "SELECT name, petName FROM channel c LEFT OUTER JOIN nymChannelPetName ncpn ON c.channelId = ncpn.channelId WHERE channelHash = ?";

    public String getChannelName(Hash channel) {
        if (channel == null) return null;
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_NAME);
            stmt.setBytes(1, channel.getData());
            rs = stmt.executeQuery();
            if (rs.next()) {
                String name = rs.getString(1);
                String petName = rs.getString(2);
                if ( (petName == null) || (petName.trim().length() == 0) )
                    return name;
                else
                    return petName;
            } else {
                return null;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel name", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_CHANNEL_NAME_ID = "SELECT name, petName FROM channel c LEFT OUTER JOIN nymChannelPetName ncpn ON c.channelId = ncpn.channelId WHERE channelId = ?";
    public String getChannelName(long chanId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_NAME_ID);
            stmt.setLong(1, chanId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                String name = rs.getString(1);
                String petName = rs.getString(2);
                if ( (petName == null) || (petName.trim().length() == 0) )
                    return name;
                else
                    return petName;
            } else {
                return null;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel name", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_CHANNEL_DESCRIPTION = "SELECT description, petdesc FROM channel c LEFT OUTER JOIN nymChannelPetName ncpn ON c.channelId = ncpn.channelId WHERE channelId = ?";

    public String getChannelDescription(long chanId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_DESCRIPTION);
            stmt.setLong(1, chanId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                String desc = rs.getString(1);
                String petDesc = rs.getString(2);
                if ( (petDesc == null) || (petDesc.trim().length() == 0) )
                    return desc;
                else
                    return petDesc;
            } else {
                return null;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel description", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    /**
     * return the set of channels (Hash) explicitly authorized to post in a scope.
     * this does not include any implicitly authorized channels, such as those replying
     * to an authorized post (if allowed) or those posting to a channel that allows anyone
     * to post.
     *
     * @param includeIdent if true, include the target scope in the set
     * @param includeManagers if true, include the explicit forum managers
     * @param includePosters if true, include the explicit forum posters
     */
    public Set<Hash> getChannelAuthorizedPosters(long scopeId, boolean includeIdent, boolean includeManagers, boolean includePosters) {
        Set<Hash> rv = new HashSet();
        if (scopeId < 0) return rv;
        
        if (includeIdent)
            rv.add(getChannelHash(scopeId));
        
        if (includeManagers) {
            Set keys = getChannelManageKeys(scopeId);
            if (keys != null) {
                for (Iterator iter = keys.iterator(); iter.hasNext(); ) {
                    SigningPublicKey pub = (SigningPublicKey)iter.next();
                    rv.add(pub.calculateHash());
                }
            }
        }
        
        if (includePosters) {
            Set keys = getChannelPostKeys(scopeId);
            if (keys != null) {
                for (Iterator iter = keys.iterator(); iter.hasNext(); ) {
                    SigningPublicKey pub = (SigningPublicKey)iter.next();
                    rv.add(pub.calculateHash());
                }
            }
        }
        
        return rv;
    }
    
    private static final String SQL_GET_SIGNKEYS = "SELECT keyType, keyData, keySalt, authenticated, keyPeriodBegin, keyPeriodEnd " +
                                                   "FROM nymKey WHERE " + 
                                                   "keyChannel = ? AND nymId = ? AND "+
                                                   "(keyFunction = '" + Constants.KEY_FUNCTION_MANAGE + "' OR keyFunction = '" + Constants.KEY_FUNCTION_POST + "')";
    /** 
     * list of SigningPrivateKey instances that the nym specified can use to
     * try and authenticate/authorize posts to the given identHash channel
     */
    public List<SigningPrivateKey> getSignKeys(Hash identHash) { return getSignKeys(identHash, _nymId, _nymPass); }

    public List<SigningPrivateKey> getSignKeys(Hash identHash, long nymId, String nymPassphrase) {
        ensureLoggedIn();
        if (identHash == null) throw new IllegalArgumentException("you need an identHash (or you should use getNymKeys())");
        List<SigningPrivateKey> rv = new ArrayList(1);
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_SIGNKEYS);
            stmt.setBytes(1, identHash.getData());
            stmt.setLong(2, nymId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                String type = rs.getString(1);
                byte data[] = rs.getBytes(2);
                byte salt[] = rs.getBytes(3);
                boolean auth= rs.getBoolean(4);
                //Date begin  = rs.getDate(5);
                //Date end    = rs.getDate(6);
                
                if (Constants.KEY_TYPE_DSA.equals(type)) {
                    if (salt != null) {
                        byte decr[] = pbeDecrypt(data, salt);
                        rv.add(new SigningPrivateKey(decr));
                        //byte readKey[] = new byte[data.length];
                        //SessionKey saltedKey = _context.keyGenerator().generateSessionKey(salt, pass);
                        //_context.aes().decrypt(data, 0, readKey, 0, saltedKey, salt, data.length);
                        //int pad = (int)readKey[readKey.length-1];
                        //byte key[] = new byte[readKey.length-pad];
                        //System.arraycopy(readKey, 0, key, 0, key.length);
                        //rv.add(new SigningPrivateKey(key));
                    } else {
                        rv.add(new SigningPrivateKey(data));
                    }
                } else {
                    // we dont know how to deal with anything but DSA signing keys
                }
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the signing keys", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return rv;
    }

    private static final String SQL_GET_REPLY_KEY = "SELECT encryptKey FROM channel WHERE channelId = ?";

    public PublicKey getReplyKey(long channelId) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_REPLY_KEY);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                byte rv[] = rs.getBytes(1);
                if (rs.wasNull())
                    return null;
                else
                    return new PublicKey(rv);
            } else {
                return null;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel's reply key", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    /**
     * Return a list of NymKey structures.
     * Does not verify encryption (validate with the passphrase).
     *
     * Side effect: sets _numKeysWithoutPass
     *
     * @param channel null for all
     * @param keyFunction null for all
     * @return no particular order
     */
    public List<NymKey> getNymKeys(Hash channel, String keyFunction) { return getNymKeys(getLoggedInNymId(), getPass(), channel, keyFunction); }
    
    private static final String SQL_GET_NYMKEYS = "SELECT keyType, keyData, keySalt, authenticated, keyPeriodBegin, keyPeriodEnd, keyFunction, keyChannel " +
                                                   "FROM nymKey WHERE nymId = ?";

    /**
     * Return a list of NymKey structures.
     * Does not verify encryption (validate with the passphrase).
     *
     * Side effect: sets _numKeysWithoutPass
     *
     * @param pass unused, must be in _nymPass (current password)
     * @param channel null for all
     * @param keyFunction null for all
     * @return no particular order
     */
    public List<NymKey> getNymKeys(long nymId, String pass, Hash channel, String keyFunction) {
        return getNymKeys(nymId, pass, channel, keyFunction, false);
    }

    /**
     * Return a list of NymKey structures.
     *
     * Side effect: sets _numKeysWithoutPass
     *
     * @param pass unused, must be in _nymPass (current password)
     * @param channel null for all
     * @param keyFunction null for all
     * @return no particular order
     */
    public List<NymKey> getNymKeys(long nymId, String pass, Hash channel, String keyFunction, boolean verifyEncryption) {
        ensureLoggedIn(!verifyEncryption);
        List<NymKey> rv = new ArrayList(1);
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            String query = SQL_GET_NYMKEYS;
            if (channel != null)
                query = query + " AND keyChannel = ?";
            if (keyFunction != null)
                query = query + " AND keyFunction = ?";
            stmt = _con.prepareStatement(query);
            stmt.setLong(1, nymId);
            if (channel != null) {
                stmt.setBytes(2, channel.getData());
                if (keyFunction != null)
                    stmt.setString(3, keyFunction);
            } else if (keyFunction != null) {
                stmt.setString(2, keyFunction);
            }
            
            _numNymKeysWithoutPass = 0;
            rs = stmt.executeQuery();
            while (rs.next()) {
                String type = rs.getString(1);
                byte data[] = rs.getBytes(2);
                byte salt[] = rs.getBytes(3);
                boolean auth= rs.getBoolean(4);
                //Date begin  = rs.getDate(5);
                //Date end    = rs.getDate(6);
                String function = rs.getString(7);
                byte chan[] = rs.getBytes(8);
                
                if (salt != null) {
                    byte key[] = pbeDecrypt(data, salt);
                    data = key;
                    if (key == null) {
                        log("Invalid passphrase to a nymKey: \"" + pass + '"');
                        _numNymKeysWithoutPass++;
                        continue;
                    }
                    
                    /*
                    SessionKey saltedKey = _context.keyGenerator().generateSessionKey(salt, passB);
                    //_log.debug("salt: " + Base64.encode(salt));
                    //_log.debug("passB: " + Base64.encode(passB));
                    //_log.debug("encrypted: " + Base64.encode(data));
                    byte decr[] = new byte[data.length];
                    _context.aes().decrypt(data, 0, decr, 0, saltedKey, salt, data.length);
                    int pad = (int)decr[decr.length-1];
                    //_log.debug("pad: " + pad);
                    byte key[] = new byte[decr.length-pad];
                    System.arraycopy(decr, 0, key, 0, key.length);
                    //_log.debug("key: " + Base64.encode(key));
                    data = key;
                     */
                }
                
                rv.add(new NymKey(type, data, _context.sha().calculateHash(data).toBase64(), auth, function, nymId, (chan != null ? Hash.create(chan) : null)));
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the keys", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return rv;        
    }
    
    public boolean verifyNymKeyEncryption() {
        getNymKeys(0, _nymPass, null, null, true);
        return _numNymKeysWithoutPass == 0;
    }
    
    /** 
     * @return non-null
     */
    public List<PrivateKey> getReplyKeys(Hash identHash, long nymId, String pass) {
        List<NymKey> keys = getNymKeys(nymId, pass, identHash, Constants.KEY_FUNCTION_REPLY);
        List<PrivateKey> rv = new ArrayList();
        for (int i = 0; i < keys.size(); i++)
            rv.add(new PrivateKey(keys.get(i).getData()));
        return rv;
    }

    public List<SigningPublicKey> getAuthorizedPosters(Hash channel) {
        return getAuthorizedPosters(getChannelId(channel), true, true, true);
    }

    private static final String SQL_GET_AUTHORIZED_OWNER = "SELECT identKey FROM channel WHERE channelId = ?";
    private static final String SQL_GET_AUTHORIZED_POSTER = "SELECT authPubKey FROM channelPostKey WHERE channelId = ?";
    private static final String SQL_GET_AUTHORIZED_MANAGER = "SELECT authPubKey FROM channelManageKey WHERE channelId = ?";

    /**
     * @param owner include the owner's identity 
     * @param manager include the identity of anyone allowed to manage the channel
     * @param authorizedPoster include the identity of anyone explicitly allowed to post in the channel
     * @return list of SigningPublicKey instances
     */
    public List<SigningPublicKey> getAuthorizedPosters(long channelId, boolean owner, boolean manager, boolean authorizedPoster) {
        ensureLoggedIn();
        List<SigningPublicKey> rv = new ArrayList();
        if (owner) getAuthorizedPosters(channelId, rv, SQL_GET_AUTHORIZED_OWNER);
        if (manager) getAuthorizedPosters(channelId, rv, SQL_GET_AUTHORIZED_MANAGER);
        if (authorizedPoster) getAuthorizedPosters(channelId, rv, SQL_GET_AUTHORIZED_POSTER);
        return rv;
    }

    /** @param rv out parameter */
    private void getAuthorizedPosters(long channelId, List<SigningPublicKey> rv, String query) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(query);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                byte key[] = rs.getBytes(1);
                if (rs.wasNull()) {
                    continue;
                } else {
                    SigningPublicKey pub = new SigningPublicKey(key);
                    if (!rv.contains(pub))
                        rv.add(pub);
                }
            }
            rs.close();
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel's authorized posting keys", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    private static final String SQL_GET_IDENT_KEY = "SELECT identKey FROM channel WHERE channelHash = ?";

    public SigningPublicKey getIdentKey(Hash hash) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_IDENT_KEY);
            stmt.setBytes(1, hash.getData());
            rs = stmt.executeQuery();
            if (rs.next()) {
                byte rv[] = rs.getBytes(1);
                if (rs.wasNull())
                    return null;
                else
                    return new SigningPublicKey(rv);
            } else {
                return null;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel's ident key", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    private static final String SQL_PRIVATE_CHANNEL_READ_KEYS = "SELECT DISTINCT channelHash, keyData, c.channelId, keyEnd FROM channelReadKey crk JOIN channel c ON crk.channelId = c.channelId WHERE wasPublic = false ORDER BY c.channelId";

    public List<NymKey> getPrivateChannelReadKeys() {
        ensureLoggedIn();
        List<NymKey> rv = new ArrayList();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_PRIVATE_CHANNEL_READ_KEYS);
            rs = stmt.executeQuery();
            while (rs.next()) {
                byte chan[] = rs.getBytes(1);
                byte key[] = rs.getBytes(2);
                long chanId = rs.getLong(3);
                if (rs.wasNull()) chanId = -1;
                boolean isExpired = (rs.getDate(4) == null);
                if ( (chan != null) && (chan.length == Hash.HASH_LENGTH) && (key != null) && (key.length == SessionKey.KEYSIZE_BYTES) )
                    rv.add(new NymKey(Constants.KEY_TYPE_AES256, key, true, Constants.KEY_FUNCTION_READ, _nymId, Hash.create(chan), isExpired));
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error listing private channel read keys", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return rv;
    }
    
    private static final String SQL_ALLOW_PUB_REPLIES = "SELECT allowPubPost, allowPubReply FROM channel WHERE channelId = ?";

    public boolean getChannelAllowPublicReplies(long targetChannelId) {        
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_ALLOW_PUB_REPLIES);
            stmt.setLong(1, targetChannelId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                boolean pubPost = rs.getBoolean(1);
                if (rs.wasNull()) pubPost = false;
                boolean pubReply = rs.getBoolean(2);
                if (rs.wasNull()) pubReply = false;
                return pubPost || pubReply;
            } else {
                return false;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error seeing if the channel allows public replies", se);
            return false;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    public boolean getChannelAllowPublicPosts(long targetChannelId) {        
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_ALLOW_PUB_REPLIES);
            stmt.setLong(1, targetChannelId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                boolean pubPost = rs.getBoolean(1);
                if (rs.wasNull()) pubPost = false;
                boolean pubReply = rs.getBoolean(2);
                if (rs.wasNull()) pubReply = false;
                return pubPost;
            } else {
                return false;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error seeing if the channel allows public posts", se);
            return false;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    /*
    private static final String SQL_GET_INTERNAL_MESSAGE_ID_FULL = "SELECT msgId FROM channelMessage WHERE authorChannelHash = ? AND messageId = ? AND targetChannelId = ?";
    private static final String SQL_GET_INTERNAL_MESSAGE_ID_NOAUTH = "SELECT msgId FROM channelMessage WHERE authorChannelHash IS NULL AND messageId = ? AND targetChannelId = ?";
    private static final String SQL_GET_INTERNAL_MESSAGE_ID_NOMSG = "SELECT msgId FROM channelMessage WHERE authorChannelHash = ? AND messageId IS NULL AND targetChannelId = ?";
    long getInternalMessageId(Hash author, long targetChannelId, Long messageId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            if ( (author != null) && (messageId != null) ) {
                stmt = _con.prepareStatement(SQL_GET_INTERNAL_MESSAGE_ID_FULL);
                stmt.setBytes(1, author.getData());
                stmt.setLong(2, messageId.longValue());
                stmt.setLong(3, targetChannelId);
            } else if ( (author == null) && (messageId != null) ) {
                stmt = _con.prepareStatement(SQL_GET_INTERNAL_MESSAGE_ID_NOAUTH);
                stmt.setLong(1, messageId.longValue());
                stmt.setLong(2, targetChannelId);
            } else if ( (author != null) && (messageId == null) ) {
                stmt = _con.prepareStatement(SQL_GET_INTERNAL_MESSAGE_ID_NOMSG);
                stmt.setBytes(1, author.getData());
                stmt.setLong(2, targetChannelId);
            } else {
                return -1;
            }
            rs = stmt.executeQuery();
            if (rs.next()) {
                long rv = rs.getLong(1);
                if (rs.wasNull())
                    return -1;
                else
                    return rv;
            } else {
                return -1;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the internal message id", se);
            return -1;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
     */


    /** 
     * gather a bunch of nym-scoped channel details.  the ids are found immediately, but
     * the details are lazy loaded
     */
    public class ChannelCollector {
        /** list of ChannelInfo for matching channels */
        private ChannelInfo _identityChannels[];
        private ChannelInfo _managedChannels[];
        private ChannelInfo _postChannels[];
        private ChannelInfo _publicPostChannels[];
        
        final List<Long> _internalIds;

        final List<Long> _identityChannelIds;
        final List<Long> _managedChannelIds;
        final List<Long> _postChannelIds;
        final List<Long> _publicPostChannelIds;
        
        public ChannelCollector() {
            _identityChannels = new ChannelInfo[0];
            _managedChannels = new ChannelInfo[0];
            _postChannels = new ChannelInfo[0];
            _publicPostChannels = new ChannelInfo[0];
            
            _internalIds = new ArrayList();
            _identityChannelIds = new ArrayList();
            _managedChannelIds = new ArrayList();
            _postChannelIds = new ArrayList();
            _publicPostChannelIds = new ArrayList();
        }
        public int getIdentityChannelCount() { return _identityChannelIds.size(); }
        public ChannelInfo getIdentityChannel(int idx) { 
            if (_identityChannels.length != _identityChannelIds.size())
                _identityChannels = new ChannelInfo[_identityChannelIds.size()];
            
            if (_identityChannels[idx] == null) {
                ChannelInfo info = getChannel(_identityChannelIds.get(idx).longValue());
                _identityChannels[idx] = info;
            }
            return _identityChannels[idx];
        }
        public int getManagedChannelCount() { return _managedChannelIds.size(); }
        public ChannelInfo getManagedChannel(int idx) {
            if (_managedChannels.length != _managedChannelIds.size())
                _managedChannels = new ChannelInfo[_managedChannelIds.size()];
            
            if (_managedChannels[idx] == null) {
                ChannelInfo info = getChannel(_managedChannelIds.get(idx).longValue());
                _managedChannels[idx] = info;
            }
            return _managedChannels[idx];
        }
        public int getPostChannelCount() { return _postChannelIds.size(); }
        public ChannelInfo getPostChannel(int idx) { 
            if (_postChannels.length != _postChannelIds.size())
                _postChannels = new ChannelInfo[_postChannelIds.size()];
            
            if (_postChannels[idx] == null) {
                ChannelInfo info = getChannel(_postChannelIds.get(idx).longValue());
                _postChannels[idx] = info;
            }
            return _postChannels[idx];
        }
        public int getPublicPostChannelCount() { return _publicPostChannelIds.size(); }
        public ChannelInfo getPublicPostChannel(int idx) { 
            if (_publicPostChannels.length != _publicPostChannelIds.size())
                _publicPostChannels = new ChannelInfo[_publicPostChannelIds.size()];
            
            if (_publicPostChannels[idx] == null) {
                ChannelInfo info = getChannel(_publicPostChannelIds.get(idx).longValue());
                _publicPostChannels[idx] = info;
            }
            return _publicPostChannels[idx];
        }
        
        // only loaded if the above isn't loaded
        public List<Long> getIdentityChannelIds() { return _identityChannelIds; }
        public List<Long> getManagedChannelIds() { return _managedChannelIds; }
        public List<Long> getPostChannelIds() { return _postChannelIds; }
        public List<Long> getPublicPostChannelIds() { return _publicPostChannelIds; }
        
        public List<Long> getAllIds() { return _internalIds; }
    }
    
    private ChannelCollector _channelCache;
    private boolean _channelDedupComplete = false;
    private final Object _channelCacheLock = new Object();

    /**
     * the channel cache should be cleared when:
     * - new channels are imported
     * - new keys are imported
     */
    void clearNymChannelCache() {
        synchronized(_channelCacheLock) {
             _channelCache = null;
        }
    }

    public ChannelCollector getNymChannels() {
        synchronized(_channelCacheLock) {
            if (_channelCache == null) {
                if (!_channelDedupComplete) {
                    dedupChannels();
                    _channelDedupComplete = true;
                }
                _channelCache = getChannels(true, true, true, true, false);
            }
            return _channelCache;
        }
    }

    /**
     *  For some reason we have duplicate channel hashes
     *  when we shouldn't. Not clear if a bug in previous or current
     *  version of hsqldb, or something wrong in our schema,
     *  but the hashes should be unique.
     *  @since 1.106b-1
     */
    private void dedupChannels() {
        long start = System.currentTimeMillis();
        Map<Long, Hash> channels = getChannelIds();
        Set<Hash> hashes = new HashSet<Hash>(channels.size());
        Set<Long> dups = new HashSet<Long>(16);
        for (Map.Entry<Long, Hash> e : channels.entrySet()) {
            if (!hashes.add(e.getValue())) {
                if (_log.shouldLog(Log.WARN))
                    _log.warn("Duplicate Hash in channel ID " + e.getKey() + ": " + e.getValue());
                dups.add(e.getKey());
            }
        }
        if (!dups.isEmpty()) {
            for (Long id : dups) {
                deleteFromDB(id.longValue(), _ui);
            }
            if (_log.shouldLog(Log.WARN)) {
                long t = System.currentTimeMillis() - start;
                _log.warn("Deleted " + dups.size() + " dups in " + t + " ms");
            }
        }
    }
    
    private static final String SQL_LIST_MANAGED_CHANNELS = "SELECT channelId FROM channelManageKey WHERE authPubKey = ?";
    private static final String SQL_LIST_POST_CHANNELS = "SELECT channelId FROM channelPostKey WHERE authPubKey = ?";

    /**
     *  Returns authenticated channels only
     */
    public ChannelCollector getChannels(boolean includeManage, boolean includeIdent, boolean includePost, boolean includePublicPost) {
        return getChannels(includeManage, includeIdent, includePost, includePublicPost, true);
    }

    /**
     *  Returns authenticated channels only
     *  @param fetchinfo unused
     */
    public ChannelCollector getChannels(boolean includeManage, boolean includeIdent, boolean includePost, boolean includePublicPost, boolean fetchInfo) {
        ChannelCollector rv = new ChannelCollector();
        
        List<Long> identIds = new ArrayList<Long>();
        List<Long> manageIds = new ArrayList<Long>();
        List<Long> postIds = new ArrayList<Long>();
        List<Long> pubPostIds = new ArrayList<Long>();
        
        List<SigningPublicKey> pubKeys = new ArrayList<SigningPublicKey>();
        List<NymKey> manageKeys = getNymKeys(getLoggedInNymId(), getPass(), null, Constants.KEY_FUNCTION_MANAGE);

        // first, go through and find all the 'identity' channels - those that we have
        // the actual channel signing key for
        for (int i = 0; i < manageKeys.size(); i++) {
            NymKey key = manageKeys.get(i);
            if (key.getAuthenticated()) {
                SigningPrivateKey priv;
                try {
                    priv = new SigningPrivateKey(key.getData());
                } catch (IllegalArgumentException iae) {
                    _ui.errorMessage("Key is " + key + " Length is " + key.getData().length, iae);
                    continue;
                }
                // FIXME we don't store the pubkeys? We have to call to KeyGenerator
                // and do BigInteger math every time?
                SigningPublicKey pub = KeyGenerator.getSigningPublicKey(priv);
                pubKeys.add(pub);
                if (includeIdent) {
                    Hash chan = pub.calculateHash();
                    long chanId = getChannelId(chan);
                    if (chanId >= 0) {
                        identIds.add(Long.valueOf(chanId));
                    } else {
                        //ui.debugMessage("nym has a key that is not an identity key (" + chan.toBase64() + ")");
                    }
                }
            }
        }

        Connection con = con();
        PreparedStatement stmt = null;
        ResultSet rs = null;

        if (includeManage) {
            // now, go through and see what other channels our management keys are
            // authorized to manage (beyond their identity channels)
            try {
                stmt = con.prepareStatement(SQL_LIST_MANAGED_CHANNELS);
                for (int i = 0; i < pubKeys.size(); i++) {
                    SigningPublicKey key = pubKeys.get(i);
                    stmt.setBytes(1, key.getData());
                    rs = stmt.executeQuery();
                    while (rs.next()) {
                        // channelId
                        long chanId = rs.getLong(1);
                        if (!rs.wasNull()) {
                            Long id = Long.valueOf(chanId);
                            if (!identIds.contains(id) && !manageIds.contains(id)) {
                                manageIds.add(id);
                            }
                        }
                    }
                    rs.close();
                }
            } catch (SQLException se) {
                //ui.errorMessage("Internal error listing channels", se);
                //ui.commandComplete(-1, null);
                log(se);
            } finally {
                if (rs != null) try { rs.close(); } catch (SQLException se) {}
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }
        }
        
        if (includePost) {
            // continue on to see what channels our management keys are
            // authorized to post in (beyond their identity and manageable channels)
            stmt = null;
            rs = null;
            try {
                stmt = con.prepareStatement(SQL_LIST_POST_CHANNELS);
                for (int i = 0; i < pubKeys.size(); i++) {
                    SigningPublicKey key = pubKeys.get(i);
                    stmt.setBytes(1, key.getData());
                    rs = stmt.executeQuery();
                    while (rs.next()) {
                        // channelId
                        long chanId = rs.getLong(1);
                        if (!rs.wasNull()) {
                            Long id = Long.valueOf(chanId);
                            if (!identIds.contains(id) && !manageIds.contains(id) && !postIds.contains(id)) {
                                postIds.add(id);
                            }
                        }
                    }
                    rs.close();
                }
            } catch (SQLException se) {
                //ui.errorMessage("Internal error listing channels", se);
                //ui.commandComplete(-1, null);
                log(se);
            } finally {
                if (rs != null) try { rs.close(); } catch (SQLException se) {}
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }
        }
            
        if (includePublicPost) {
            List<Long> channelIds = getPublicPostingChannelIds();
            for (int i = 0; i < channelIds.size(); i++) {
                Long id = (Long)channelIds.get(i);
                if (!identIds.contains(id) && !manageIds.contains(id) && !postIds.contains(id) && !pubPostIds.contains(id)) {
                    pubPostIds.add(id);
                }
            }
        }
        
        // ok, now sort the identIds/manageIds/postIds/pubPostIds by their names
        sortChannels(identIds);
        sortChannels(manageIds);
        sortChannels(postIds);
        sortChannels(pubPostIds);
        
        // unused
        //int totalGetEvents = 0;
        //long totalGetTime = 0;
        //ArrayList getTimes = new ArrayList();
        
        for (int i = 0; i < identIds.size(); i++) {
            Long chanId = identIds.get(i);
            rv._internalIds.add(chanId);
            rv._identityChannelIds.add(chanId);
        }
        for (int i = 0; i < manageIds.size(); i++) {
            Long chanId = manageIds.get(i);
            rv._internalIds.add(chanId);
            rv._managedChannelIds.add(chanId);
        }
        for (int i = 0; i < postIds.size(); i++) {
            Long chanId = postIds.get(i);
            rv._internalIds.add(chanId);
            rv._postChannelIds.add(chanId);
        }
        for (int i = 0; i < pubPostIds.size(); i++) {
            Long chanId = pubPostIds.get(i);
            rv._internalIds.add(chanId);
            rv._publicPostChannelIds.add(chanId);
        }
        
        //_ui.debugMessage("getChannels: total time: " + totalGetTime + "ms\n" + getTimes);
        return rv;
    }

    /**
     *  Sorts by name in current locale
     *  @param chanIds in: unsorted; out: sorted
     */
    private void sortChannels(List<Long> chanIds) {
        TreeMap<String, Long> nameToId = new TreeMap(Collator.getInstance());
        for (int i = 0; i < chanIds.size(); i++) {
            Long id = chanIds.get(i);
            String name = getChannelName(id.longValue());
            if (name == null) name = "";
            name = name + ' ' + id.toString(); // guaranteed to be unique
            nameToId.put(name, id);
        }
        chanIds.clear();
        chanIds.addAll(nameToId.values());
    }

    public static class ChannelSearchCriteria {
        private String _name;
        private Set<String> _tagsInclude;
        private Set<String> _tagsRequire;
        private Set<String> _tagsExclude;
        private String _hashPrefix;
        
        public ChannelSearchCriteria() {
            _name = null;
            _tagsInclude = new HashSet();
            _tagsRequire = new HashSet();
            _tagsExclude = new HashSet();
            _hashPrefix = null;
        }
        
        public String getName() { return _name; }
        public String getHashPrefix() { return _hashPrefix; }
        public void setName(String name) { _name = name; }
        public void setHashPrefix(String prefix) { _hashPrefix = prefix; }
        public void requireTag(String tag) { _tagsRequire.add(tag); }
        public void includeTag(String tag) { _tagsInclude.add(tag); }
        public void excludeTag(String tag) { _tagsExclude.add(tag); }
        
        public Set<String> getInclude() { return _tagsInclude; }
        public Set<String> getExclude() { return _tagsExclude; }
        public Set<String> getRequire() { return _tagsRequire; }
    }
    
    /* --- OLD JDOC entry
     * search through the channels for those matching the given criteria
     * @param name channel name must start with this
     * @param tagsInclude channel tags should include one or more of these
     * @param tagsRequire channel tags must include all of these
     * @param tagsExclude channel tags must not include any of these
     * @param hashPrefix channel hash must start with this base64 value
     * @return list of matching channels (ChannelInfo)
     */

    /**
     * search through the channels for those matching the given criteria
     * @return list of matching channels (ChannelInfo)
     */
    public List<ChannelInfo> getChannels(ChannelSearchCriteria criteria) { //String name, Set tagsInclude, Set tagsRequire, Set tagsExclude, String hashPrefix) {
        String name = criteria.getName();
        String hashPrefix = criteria.getHashPrefix();
        Set tagsInclude = criteria.getInclude();
        Set tagsExclude = criteria.getExclude();
        Set tagsRequire = criteria.getRequire();

        if ( (name != null) && (name.trim().length() <= 0) ) name = null;
        if ( (hashPrefix != null) && (hashPrefix.trim().length() <= 0) ) hashPrefix = null;
        if ( (tagsInclude != null) && (tagsInclude.size() <= 0) ) tagsInclude = null;
        if ( (tagsRequire != null) && (tagsRequire.size() <= 0) ) tagsRequire = null;
        if ( (tagsExclude != null) && (tagsExclude.size() <= 0) ) tagsExclude = null;
        
        // this could of course be optimized to do the work in the db, saving some memory churn
        // instead of all these getChannel calls.  but this'll do the trick for now
        List<ChannelInfo> rv = new ArrayList();
        Map<Long, Hash> allIds = getChannelIds();
        for (Iterator iter = allIds.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry)iter.next();
            Long chanId = (Long)entry.getKey();
            Hash chan = (Hash)entry.getValue();
            if ( (hashPrefix != null) && (!chan.toBase64().startsWith(hashPrefix)) )
                continue;
            ChannelInfo info = getChannel(chanId.longValue());
            if (info == null) continue;
            if ( (name != null) && (info.getName() != null) && (!info.getName().toLowerCase().startsWith(name.toLowerCase())) )
                continue;
            Set<String> pub = info.getPublicTags();
            Set<String> priv= info.getPrivateTags();
            if (tagsExclude != null) {
                boolean found = false;
                for (Iterator titer = tagsExclude.iterator(); titer.hasNext(); ) {
                    String tag = (String)titer.next();
                    if (pub.contains(tag) || priv.contains(tag)) { 
                        //System.out.println("Not including " + info.getChannelHash().toBase64() + " found tag [" + tag + "]");
                        found = true;
                        break; 
                    }
                }
                if (found) {
                    continue;
                }
            }
            if (tagsRequire != null) {
                boolean foundAll = true;
                for (Iterator titer = tagsRequire.iterator(); titer.hasNext(); ) {
                    String tag = (String)titer.next();
                    if ( (!pub.contains(tag)) && (!priv.contains(tag)) ) {
                        foundAll = false;
                        //System.out.println("Not including " + info.getChannelHash().toBase64() + " missing tag [" + tag + "]");
                        break;
                    }
                }
                if (!foundAll) {
                    continue;
                }
            }
            if (tagsInclude != null) {
                boolean found = false;
                for (Iterator titer = tagsInclude.iterator(); titer.hasNext(); ) {
                    String tag = (String)titer.next();
                    if ( (pub.contains(tag)) || (priv.contains(tag)) ) {
                        found = true;
                        break;
                    } else {
                        //System.out.println("tag '" + tag + "' was not found in " + pub + " or " + priv);
                    }
                }
                if (!found) {
                    //System.out.println("Not including " + info.getChannelHash().toBase64() + " pub: " + pub + "/" + pub.size() + " priv: " + priv);
                    continue;
                }
            }
            
            rv.add(info);
        }
        return rv;
    }
    
    private static final String SQL_SEARCH_CHANNEL_IDS = "SELECT channelId FROM channel WHERE name LIKE ? OR description LIKE ? " +
            "UNION " +
            "SELECT channelId FROM channelTag WHERE tag LIKE ?";

    public List<Long> getChannelIds(String term) {
        List<Long> rv = new ArrayList();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_SEARCH_CHANNEL_IDS);
            stmt.setString(1, "%" + term + "%");
            stmt.setString(2, "%" + term + "%");
            stmt.setString(3, "%" + term + "%");
            rs = stmt.executeQuery();
            while (rs.next()) {
                long id = rs.getLong(1);
                rv.add(Long.valueOf(id));
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the query matches", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        log("search for [" + term + "] found " + rv.size() + " matches: " + rv);
        return rv;
    }
    
    private static final String SQL_GET_CHANNEL_INFO = "SELECT channelId, channelHash, identKey, encryptKey, edition, name, description, allowPubPost, allowPubReply, expiration, readKeyMissing, pbePrompt, importDate, petname, petdesc FROM channel c LEFT OUTER JOIN nymChannelPetName ncpn ON c.channelId = ncpn.channelId WHERE channelId = ?";
    private static final String SQL_GET_CHANNEL_TAG = "SELECT tag, wasEncrypted FROM channelTag WHERE channelId = ?";
    private static final String SQL_GET_CHANNEL_POST_KEYS = "SELECT authPubKey FROM channelPostKey WHERE channelId = ?";
    private static final String SQL_GET_CHANNEL_MANAGE_KEYS = "SELECT authPubKey FROM channelManageKey WHERE channelId = ?";
    private static final String SQL_GET_CHANNEL_ARCHIVES = "SELECT archiveId, wasEncrypted FROM channelArchive WHERE channelId = ?";
    private static final String SQL_GET_CHANNEL_READ_KEYS = "SELECT keyData, wasPublic FROM channelReadKey WHERE channelId = ? AND keyEnd IS NULL";
    private static final String SQL_GET_CHANNEL_META_HEADERS = "SELECT headerName, headerValue, wasEncrypted FROM channelMetaHeader WHERE channelId = ?";
    private static final String SQL_GET_CHANNEL_REFERENCES = "SELECT groupId, parentGroupId, siblingOrder, name, description, uriId, referenceType, wasEncrypted FROM channelReferenceGroup WHERE channelId = ? ORDER BY parentGroupId ASC, siblingOrder ASC";

    public ChannelInfo getChannel(long channelId) {
        ensureLoggedIn();
        long start = System.currentTimeMillis();
        if (_trace) _getChanCount++;
        ChannelInfo info = new ChannelInfo();
        if (!getChannelInfo(channelId, info))
            return null;
        
        if (!getChannelTags(channelId, info))
            return null;

        Set<SigningPublicKey> postKeys = getChannelPostKeys(channelId);
        if (postKeys == null)
            return null;
        info.setAuthorizedPosters(postKeys);
        
        Set<SigningPublicKey> manageKeys = getChannelManageKeys(channelId);
        if (manageKeys == null)
            return null;
        info.setAuthorizedManagers(manageKeys);
        
        if (!getChannelArchives(channelId, info))
            return null;
        
        if (!getChannelReadKeys(channelId, info))
            return null;
        
        if (!getChannelMetaHeaders(channelId, info))
            return null;
        
        List<ReferenceNode> roots = getChannelReferences(channelId);
        if (roots == null)
            return null;
        info.setReferences(roots);
        
        long end = System.currentTimeMillis();
        if (_trace)
            _getChanTime += (end-start);
        return info;
    }
    
    private boolean getChannelInfo(long channelId, ChannelInfo info) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_INFO);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                // channelId, channelHash, identKey, encryptKey, edition, name, 
                // description, allowPubPost, allowPubReply, expiration, readKeyMissing, pbePrompt
                byte chanHash[] = rs.getBytes(2);
                byte identKey[] = rs.getBytes(3);
                byte encryptKey[] = rs.getBytes(4);
                long edition = rs.getLong(5);
                if (rs.wasNull()) edition = -1;
                String name = rs.getString(6);
                String desc = rs.getString(7);
                boolean allowPost = rs.getBoolean(8);
                if (rs.wasNull()) allowPost = false;
                boolean allowReply = rs.getBoolean(9);
                if (rs.wasNull()) allowReply = false;
                java.sql.Date exp = rs.getDate(10);
                boolean readKeyMissing = rs.getBoolean(11);
                if (rs.wasNull()) readKeyMissing = false;
                String pbePrompt = rs.getString(12);
                Date importDate = rs.getDate(13);
                String petname = rs.getString(14);
                String petdesc = rs.getString(15);
                
                info.setChannelId(channelId);
                info.setChannelHash(Hash.create(chanHash));
                info.setIdentKey(new SigningPublicKey(identKey));
                info.setEncryptKey(new PublicKey(encryptKey));
                info.setEdition(edition);
                info.setName(petname == null ? name : petname);
                info.setDescription(petdesc == null ? desc : petdesc);
                info.setAllowPublicPosts(allowPost);
                info.setAllowPublicReplies(allowReply);
                if (exp != null)
                    info.setExpiration(exp.getTime());
                else
                    info.setExpiration(-1);
                info.setReadKeyUnknown(readKeyMissing);
                info.setPassphrasePrompt(pbePrompt);
                info.setReceiveDate(importDate.getTime());
                return true;
            } else {
                return false;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel's info", se);
            return false;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private boolean getChannelTags(long channelId, ChannelInfo info) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_TAG);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            Set encrypted = new HashSet();
            Set unencrypted = new HashSet();
            while (rs.next()) {
                // tag, wasEncrypted
                String tag = rs.getString(1);
                boolean enc = rs.getBoolean(2);
                if (rs.wasNull())
                    enc = true;
                if (enc)
                    encrypted.add(tag);
                else
                    unencrypted.add(tag);
            }
            info.setPublicTags(unencrypted);
            info.setPrivateTags(encrypted);
            return true;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel's tags", se);
            return false;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private boolean getChannelArchives(long channelId, ChannelInfo info) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_ARCHIVES);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            Set pubIds = new HashSet();
            Set privIds = new HashSet();
            while (rs.next()) {
                // archiveId, wasEncrypted
                long archiveId = rs.getLong(1);
                if (rs.wasNull())
                    archiveId = -1;
                boolean enc = rs.getBoolean(2);
                if (rs.wasNull())
                    enc = true;
                if (enc)
                    privIds.add(Long.valueOf(archiveId));
                else
                    pubIds.add(Long.valueOf(archiveId));
            }
            rs.close();
            rs = null;
            stmt.close();
            stmt = null;
            
            Set pub = new HashSet();
            Set priv = new HashSet();
            for (Iterator iter = pubIds.iterator(); iter.hasNext(); ) {
                Long id = (Long)iter.next();
                ArchiveInfo archive = getArchive(id.longValue());
                if (archive != null)
                    pub.add(archive);
            }
            for (Iterator iter = privIds.iterator(); iter.hasNext(); ) {
                Long id = (Long)iter.next();
                ArchiveInfo archive = getArchive(id.longValue());
                if (archive != null)
                    priv.add(archive);
            }
            
            info.setPublicArchives(pub);
            info.setPrivateArchives(priv);
            return true;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel's managers", se);
            return false;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private boolean getChannelReadKeys(long channelId, ChannelInfo info) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_READ_KEYS);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            Set keys = new HashSet();
            boolean pub = true;
            while (rs.next()) {
                // readKey
                byte key[] = rs.getBytes(1);
                if (!rs.wasNull())
                    keys.add(new SessionKey(key));
                boolean curPub = rs.getBoolean(2);
                if (!rs.wasNull())
                    pub = pub && curPub;
            }
            info.setReadKeys(keys);
            info.setReadKeysArePublic(pub);
            return true;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel's managers", se);
            return false;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private boolean getChannelMetaHeaders(long channelId, ChannelInfo info) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_META_HEADERS);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            Properties pub = new Properties();
            Properties priv = new Properties();
            while (rs.next()) {
                // headerName, headerValue, wasEncrypted
                String name = rs.getString(1);
                String val = rs.getString(2);
                boolean enc = rs.getBoolean(3);
                if (rs.wasNull())
                    enc = true;
                if (enc)
                    priv.setProperty(name, val);
                else
                    pub.setProperty(name, val);
            }
            info.setPublicHeaders(pub);
            info.setPrivateHeaders(priv);
            return true;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel's managers", se);
            return false;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    public List<ReferenceNode> getChannelReferences(long channelId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_REFERENCES);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            HashMap<Long, DBReferenceNode> groupIdToNode = new HashMap();
            while (rs.next()) {
                // groupId, parentGroupId, siblingOrder, name, description, 
                // uriId, referenceType, wasEncrypted 
                
                // ORDER BY parentGroupId, siblingOrder
                long groupId = rs.getLong(1);
                if (rs.wasNull()) groupId = -1;
                long parentGroupId = rs.getLong(2);
                if (rs.wasNull()) parentGroupId = -1;
                int order = rs.getInt(3);
                if (rs.wasNull()) order = 0;
                String name = rs.getString(4);
                String desc = rs.getString(5);
                long uriId = rs.getLong(6);
                if (rs.wasNull()) uriId = -1;
                String type = rs.getString(7);
                boolean enc = rs.getBoolean(8);
                if (rs.wasNull()) enc = true;
                
                SyndieURI uri = getURI(uriId);
                DBReferenceNode ref = new DBReferenceNode(name, uri, desc, type, uriId, groupId, parentGroupId, order, enc);
                groupIdToNode.put(Long.valueOf(groupId), ref);
            }
            
            // now build the tree out of the nodes
            List<ReferenceNode> roots = new ArrayList();
            for (Iterator iter = groupIdToNode.values().iterator(); iter.hasNext(); ) {
                DBReferenceNode cur = (DBReferenceNode)iter.next();
                long parentId = cur.getParentGroupId();
                if (parentId >= 0) {
                    DBReferenceNode parent = (DBReferenceNode)groupIdToNode.get(Long.valueOf(parentId));
                    if (parent != null)
                        parent.addChild(cur);
                    else
                        roots.add(cur);
                } else {
                    roots.add(cur);
                }
            }
            // another pass to sort the children
            for (Iterator iter = groupIdToNode.values().iterator(); iter.hasNext(); ) {
                DBReferenceNode cur = (DBReferenceNode)iter.next();
                cur.sortChildren();
            }
            // sort the roots
            TreeMap<Integer, DBReferenceNode> sorted = new TreeMap();
            for (int i = 0; i < roots.size(); i++) {
                DBReferenceNode cur = (DBReferenceNode)roots.get(i);
                int off = 0;
                while (sorted.containsKey(Integer.valueOf(cur.getSiblingOrder()+off)))
                    off++;
                sorted.put(Integer.valueOf(cur.getSiblingOrder()+off), cur);
            }
            roots.clear();
            roots.addAll(sorted.values());
            return roots;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel's managers", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static class DBReferenceNode extends ReferenceNode {
        private long _uriId;
        private long _groupId;
        private long _parentGroupId;
        private int _siblingOrder;
        private boolean _encrypted;
        
        public DBReferenceNode(String name, SyndieURI uri, String description, String type, long uriId, long groupId, long parentGroupId, int siblingOrder, boolean encrypted) {
            super(name, uri, description, type);
            _uriId = uriId;
            _groupId = groupId;
            _parentGroupId = parentGroupId;
            _siblingOrder = siblingOrder;
            _encrypted = encrypted;
        }
        public long getURIId() { return _uriId; }
        public long getGroupId() { return _groupId; }
        public long getParentGroupId() { return _parentGroupId; }
        public int getSiblingOrder() { return _siblingOrder; }
        public boolean getEncrypted() { return _encrypted; }
        public long getUniqueId() { return _groupId; }
        
        public void sortChildren() {
            TreeMap<Long, DBReferenceNode> sorted = new TreeMap();
            for (int i = 0; i < _children.size(); i++) {
                DBReferenceNode child = (DBReferenceNode)_children.get(i);
                int off = 0;
                while (sorted.containsKey(Long.valueOf(child.getSiblingOrder()+off)))
                    off++;
                sorted.put(Long.valueOf(child.getSiblingOrder()+off), child);
            }
            _children.clear();
            for (Iterator iter = sorted.values().iterator(); iter.hasNext(); ) {
                DBReferenceNode child = (DBReferenceNode)iter.next();
                _children.add(child);
            }
        }
    }
    
    public Set<SigningPublicKey> getChannelPostKeys(long channelId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_POST_KEYS);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            Set<SigningPublicKey> keys = new HashSet();
            while (rs.next()) {
                // authPub
                byte key[] = rs.getBytes(1);
                if (!rs.wasNull())
                    keys.add(new SigningPublicKey(key));
            }
            return keys;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel's posters", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    public Set<SigningPublicKey> getChannelManageKeys(long channelId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_MANAGE_KEYS);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            Set<SigningPublicKey> keys = new HashSet();
            while (rs.next()) {
                // authPub
                byte key[] = rs.getBytes(1);
                if (!rs.wasNull())
                    keys.add(new SigningPublicKey(key));
            }
            return keys;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel's managers", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_NYM_CHANNEL_PETNAME_DEFINED = "SELECT COUNT(channelId) FROM nymChannelPetName WHERE channelId = ? AND petname IS NOT NULL";

    public boolean getNymChannelPetNameDefined(long channelId) { 
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_NYM_CHANNEL_PETNAME_DEFINED);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                long count = rs.getLong(1);
                return !rs.wasNull() && (count > 0);
            } else {
                return false;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error determining if the petname was defined", se);
            return false;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    private static final String SQL_GET_NYM_CHANNEL_PETDESC_DEFINED = "SELECT COUNT(channelId) FROM nymChannelPetName WHERE channelId = ? AND petdesc IS NOT NULL";

    public boolean getNymChannelPetDescriptionDefined(long channelId) { 
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_NYM_CHANNEL_PETDESC_DEFINED);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                long count = rs.getLong(1);
                return !rs.wasNull() && (count > 0);
            } else {
                return false;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error determining if the petdesc was defined", se);
            return false;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
                
    private static final String SQL_UNSET_PETNAME = "DELETE FROM nymChannelPetName WHERE channelId = ?";
    private static final String SQL_SET_PETNAME = "INSERT INTO nymChannelPetName (channelId, petname, petdesc) VALUES (?, ?, ?)";

    /**
     * override the channel's name and description locally
     */
    public void setNymChannelPetName(long channelId, String name, String desc) {
        ensureLoggedIn();
        
        PreparedStatement stmt = null;
        try {
            exec(SQL_UNSET_PETNAME, channelId);
            stmt = _con.prepareStatement(SQL_SET_PETNAME);
            stmt.setLong(1, channelId);
            if ( (name != null) && (name.trim().length() > 0) )
                stmt.setString(2, name);
            else
                stmt.setNull(2, Types.VARCHAR);
            if ( (desc != null) && (desc.trim().length() > 0) )
                stmt.setString(3, desc);
            else
                stmt.setNull(3, Types.VARCHAR);
            stmt.executeUpdate();
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error setting the petname", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_UNSET_CUSTOM_AVATAR = "DELETE FROM nymCustomIcon WHERE targetType = 0 AND targetId = ?";
    private static final String SQL_SET_CUSTOM_AVATAR = "INSERT INTO nymCustomIcon (targetType, targetId, data) VALUES (0, ?, ?)";

    /**
     * user-specified avatar overriding the channel's published avatar
     */
    public void setNymChannelAvatar(long channelId, byte avatar[]) {
        ensureLoggedIn();
        
        log("Setting custom avatar for " + channelId + ": " + (avatar != null ? avatar.length : -1));
        
        PreparedStatement stmt = null;
        try {
            exec(SQL_UNSET_CUSTOM_AVATAR, channelId);
            stmt = _con.prepareStatement(SQL_SET_CUSTOM_AVATAR);
            stmt.setLong(1, channelId);
            stmt.setBytes(2, avatar);
            stmt.executeUpdate();
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error setting the custom avatar", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_CUSTOM_AVATAR = "SELECT data FROM nymCustomIcon WHERE targetType = 0 AND targetId = ?";

    /**
     * user-specified avatar overriding the channel's published avatar
     */
    public byte[] getNymChannelAvatar(long channelId) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CUSTOM_AVATAR);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                byte data[] = rs.getBytes(1);
                return data;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the custom avatar", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return null;
    }

    private static final String SQL_GET_CUSTOM_AVATAR_DEFINED = "SELECT COUNT(targetId) FROM nymCustomIcon WHERE targetType = 0 AND targetId = ?";

    /**
     * returns true if a custom channel avatar has been defined
     */
    public boolean getNymChannelAvatarDefined(long channelId) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CUSTOM_AVATAR_DEFINED);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                return rs.getLong(1) > 0;
            } else {
                return false;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error determining if the custom avatar is defined", se);
            return false;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_CHANNEL_AVATAR = "SELECT avatarData FROM channelAvatar WHERE channelId = ?";

    public byte[] getChannelAvatar(long channelId) {
        ensureLoggedIn();
        byte rv[] = getNymChannelAvatar(channelId);
        if (rv != null)
            return rv;
        
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_AVATAR);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                byte data[] = rs.getBytes(1);
                return data;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the avatar", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return null;
    }
    
    private static final String SQL_GET_ARCHIVE = "SELECT postAllowed, readAllowed, uriId FROM archive WHERE archiveId = ?";

    private ArchiveInfo getArchive(long archiveId) { 
        ensureLoggedIn();
        ArchiveInfo info = new ArchiveInfo();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_ARCHIVE);
            stmt.setLong(1, archiveId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                // postAllowed, readAllowed, uriId
                boolean post = rs.getBoolean(1);
                if (rs.wasNull()) post = false;
                boolean read = rs.getBoolean(2);
                if (rs.wasNull()) read = false;
                long uriId = rs.getLong(3);
                if (rs.wasNull()) uriId = -1;
                if (uriId >= 0) {
                    SyndieURI uri = getURI(uriId);
                    info.setArchiveId(archiveId);
                    info.setPostAllowed(post);
                    info.setReadAllowed(read);
                    info.setURI(uri);
                    return info;
                }
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the archive", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return null;
    }

    private static final String SQL_GET_MESSAGES_PRIVATE = "SELECT msgId, messageId FROM channelMessage WHERE targetChannelId = ? AND wasPrivate = TRUE AND wasAuthenticated = TRUE ORDER BY messageId ASC";

    public List<Long> getMessageIdsPrivate(Hash chan) {
        ensureLoggedIn();
        List<Long> rv = new ArrayList();
        long chanId = getChannelId(chan);
        if (chanId >= 0) {
            PreparedStatement stmt = null;
            ResultSet rs = null;
            try {
                stmt = _con.prepareStatement(SQL_GET_MESSAGES_PRIVATE);
                stmt.setLong(1, chanId);
                rs = stmt.executeQuery();
                while (rs.next()) {
                    // msgId, messageId
                    long msgId = rs.getLong(1);
                    if (!rs.wasNull())
                        rv.add(Long.valueOf(msgId));
                }
            } catch (SQLException se) {
                if (_log.shouldLog(Log.ERROR))
                    _log.error("Error retrieving the message list", se);
            } finally {
                if (rs != null) try { rs.close(); } catch (SQLException se) {}
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }

        }
        return rv;
    }

    // the order here (targetChannelId ASC, importDate DESC) shows messages targetting
    // the local ident first (since channelId 0 is the one created on install), then
    // sorts newest first
    private static final String SQL_GET_PRIVATE_ALL = "SELECT msgId FROM channelMessage cm WHERE wasPrivate = TRUE AND wasAuthenticated = TRUE AND replyKeyMissing = FALSE AND readKeyMissing = FALSE AND pbePrompt IS NULL ORDER BY targetChannelId ASC, importDate DESC";

    public List<Long> getPrivateMsgIds(boolean alreadyRead) { return getPrivateMsgIds(_nymId, alreadyRead); }

    public List<Long> getPrivateMsgIds(long nymId, boolean alreadyRead) {
        ensureLoggedIn();
        List<Long> rv = new ArrayList();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_PRIVATE_ALL);
            rs = stmt.executeQuery();
            while (rs.next()) {
                // msgId
                long msgId = rs.getLong(1);
                if (!rs.wasNull())
                    rv.add(Long.valueOf(msgId));
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the private messages", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        // now filter
        for (int i = 0; i < rv.size(); i++) {
            Long msgId = (Long)rv.get(i);
            int status = getMessageStatus(msgId.longValue());
            if (status == MSG_STATUS_UNREAD) {
                if (alreadyRead) {
                    rv.remove(i);
                    i--;
                }
            } else {
                if (!alreadyRead) {
                    rv.remove(i);
                    i--;
                }
            }   
        }
        return rv;
    }
    
    /** 
     * syndie URI for elements still requiring a passphrase.
     * @param meta include forum metadata that still need a passphrase
     * @param msgs include forum messages that still need a passphrase
     */
    public List<SyndieURI> getPBERequired(boolean meta, boolean msgs) {
        ensureLoggedIn();
        List<SyndieURI> rv = new ArrayList();
        if (meta)
            getPBERequiredMeta(rv);
        if (msgs)
            getPBERequiredMsgs(rv);
        return rv;
    }

    private static final String SQL_GET_PBEREQUIRED_META = "SELECT channelHash FROM channel WHERE pbePrompt IS NOT NULL";

    /** @param rv out parameter */
    private void getPBERequiredMeta(List<SyndieURI> rv) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_PBEREQUIRED_META);
            rs = stmt.executeQuery();
            while (rs.next()) {
                byte hash[] = rs.getBytes(1);
                if ( (hash != null) && (hash.length == Hash.HASH_LENGTH) )
                    rv.add(SyndieURI.createScope(Hash.create(hash)));
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the pbe meta", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    //private static final String SQL_GET_PBEREQUIRED_MSGS = "SELECT channelHash, messageId FROM channelMessage JOIN channel ON channelId = scopeChannelId WHERE pbePrompt IS NOT NULL";
    // as of 1.104b-5, don't return deleted messages
    private static final String SQL_GET_PBEREQUIRED_MSGS = "SELECT channelHash, messageId FROM channelMessage JOIN channel ON channelId = scopeChannelId WHERE pbePrompt IS NOT NULL AND deletionCause IS NULL";

    /** @param rv out parameter */
    private void getPBERequiredMsgs(List<SyndieURI> rv) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_PBEREQUIRED_MSGS);
            rs = stmt.executeQuery();
            while (rs.next()) {
                byte hash[] = rs.getBytes(1);
                long messageId = rs.getLong(2);
                if (rs.wasNull())
                    continue;
                if ( (hash != null) && (hash.length == Hash.HASH_LENGTH) )
                    rv.add(SyndieURI.createMessage(Hash.create(hash), messageId));
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the pbe meta", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_MESSAGES_AUTHORIZED = "SELECT msgId, messageId FROM channelMessage WHERE targetChannelId = ? AND wasPrivate = FALSE AND wasAuthorized = TRUE ORDER BY messageId ASC";

    public List<Long> getMessageIdsAuthorized(Hash chan) {
        ensureLoggedIn();
        List<Long> rv = new ArrayList();
        long chanId = getChannelId(chan);
        if (chanId >= 0) {
            PreparedStatement stmt = null;
            ResultSet rs = null;
            try {
                stmt = _con.prepareStatement(SQL_GET_MESSAGES_AUTHORIZED);
                stmt.setLong(1, chanId);
                rs = stmt.executeQuery();
                while (rs.next()) {
                    // msgId, messageId
                    long msgId = rs.getLong(1);
                    if (!rs.wasNull())
                        rv.add(Long.valueOf(msgId));
                }
            } catch (SQLException se) {
                if (_log.shouldLog(Log.ERROR))
                    _log.error("Error retrieving the message list", se);
            } finally {
                if (rs != null) try { rs.close(); } catch (SQLException se) {}
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }

        }
        return rv;
    }

    private static final String SQL_GET_MESSAGES_AUTHENTICATED = "SELECT msgId, messageId FROM channelMessage WHERE targetChannelId = ? AND wasPrivate = FALSE AND wasAuthorized = FALSE AND wasAuthenticated = TRUE ORDER BY messageId ASC";

    public List<Long> getMessageIdsAuthenticated(Hash chan) {
        ensureLoggedIn();
        List<Long> rv = new ArrayList();
        long chanId = getChannelId(chan);
        if (chanId >= 0) {
            PreparedStatement stmt = null;
            ResultSet rs = null;
            try {
                stmt = _con.prepareStatement(SQL_GET_MESSAGES_AUTHENTICATED);
                stmt.setLong(1, chanId);
                rs = stmt.executeQuery();
                while (rs.next()) {
                    // msgId, messageId
                    long msgId = rs.getLong(1);
                    if (!rs.wasNull())
                        rv.add(Long.valueOf(msgId));
                }
            } catch (SQLException se) {
                if (_log.shouldLog(Log.ERROR))
                    _log.error("Error retrieving the message list", se);
            } finally {
                if (rs != null) try { rs.close(); } catch (SQLException se) {}
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }

        }
        return rv;
    }

    private static final String SQL_GET_MESSAGES_UNAUTHENTICATED = "SELECT msgId, messageId FROM channelMessage WHERE targetChannelId = ? AND wasPrivate = FALSE AND wasAuthorized = FALSE AND wasAuthenticated = FALSE ORDER BY messageId ASC";

    public List<Long> getMessageIdsUnauthenticated(Hash chan) {
        ensureLoggedIn();
        List<Long> rv = new ArrayList();
        long chanId = getChannelId(chan);
        if (chanId >= 0) {
            PreparedStatement stmt = null;
            ResultSet rs = null;
            try {
                stmt = _con.prepareStatement(SQL_GET_MESSAGES_UNAUTHENTICATED);
                stmt.setLong(1, chanId);
                rs = stmt.executeQuery();
                while (rs.next()) {
                    // msgId, messageId
                    long msgId = rs.getLong(1);
                    if (!rs.wasNull())
                        rv.add(Long.valueOf(msgId));
                }
            } catch (SQLException se) {
                if (_log.shouldLog(Log.ERROR))
                    _log.error("Error retrieving the message list", se);
            } finally {
                if (rs != null) try { rs.close(); } catch (SQLException se) {}
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }

        }
        return rv;
    }
    
    
    private static final String SQL_GET_INTERNAL_MESSAGE_ID = "SELECT msgId FROM channelMessage WHERE scopeChannelId = ? AND messageId = ?";

    /**
     *  @param messageId external ID (timestamp)
     *  @return message info or null
     */
    public MessageInfo getMessage(long scopeId, Long messageId) {
        ensureLoggedIn();
        if (messageId == null) return null;
        return getMessage(scopeId, messageId.longValue());
    }

    /**
     *  @param messageId external ID (timestamp)
     *  @return message info or null
     */
    public MessageInfo getMessage(long scopeId, long messageId) {
        long msgId = getMessageId(scopeId, messageId);
        if (msgId >= 0)
            return getMessage(msgId);
        else
            return null;
    }

    /**
     *  @param messageId external ID (timestamp)
     *  @return internal message id or -1 if not found
     */
    public long getMessageId(long scopeId, long messageId) {
        long msgId = -1;
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_INTERNAL_MESSAGE_ID);
            stmt.setLong(1, scopeId);
            stmt.setLong(2, messageId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                msgId = rs.getLong(1);
                if (rs.wasNull())
                    msgId = -1;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the message's id", se);
            return -1;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return msgId;
    }

    /**
     *  @param messageId external ID (timestamp)
     *  @return internal message id or -1 if not found
     */
    public long getMessageId(Hash scope, Long messageId) {
        if (messageId == null)
            return -1;
        else
            return getMessageId(scope, messageId.longValue());
    }

    /**
     *  @param messageId external ID (timestamp)
     *  @return internal message id or -1 if not found
     */
    public long getMessageId(Hash scope, long messageId) {
        long chanId = getChannelId(scope);
        if (chanId >= 0)
            return getMessageId(chanId, messageId);
        else
            return -1;
    }
    
    /**
     *  @param messageId external ID (timestamp)
     *  @return date or -1
     */
    public long getMessageImportDate(Hash scope, long messageId) {
        long msgId = getMessageId(scope, messageId);
        if (msgId >= 0)
            return getMessageImportDate(msgId);
        else
            return -1;
    }
    private static final String SQL_GET_MESSAGE_IMPORT_DATE = "SELECT importDate FROM channelMessage WHERE msgId = ?";

    /**
     *  @param msgId internal ID
     *  @return date or -1
     */
    public long getMessageImportDate(long msgId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_IMPORT_DATE);
            stmt.setLong(1, msgId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                Date when = rs.getDate(1);
                if (rs.wasNull())
                    return -1;
                else
                    return when.getTime();
            } else {
                return -1;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the message's import date", se);
            return -1;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_MESSAGE_SCOPE = "SELECT channelHash FROM channel JOIN channelMessage ON scopeChannelId = channelId WHERE msgId = ?";

    public Hash getMessageScope(long msgId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_SCOPE);
            stmt.setLong(1, msgId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                byte hash[] = rs.getBytes(1);
                if ( (hash != null) && (hash.length == Hash.HASH_LENGTH) )
                    return Hash.create(hash);
                else
                    return null;
            } else {
                return null;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the message's scope", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_MESSAGE_ID = "SELECT messageId FROM channelMessage WHERE msgId = ?";

    /**
     *  @param msgId internal message id
     *  @return external ID (timestamp) or -1
     */
    public long getMessageId(long msgId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_ID);
            stmt.setLong(1, msgId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                long messageId = rs.getLong(1);
                if (rs.wasNull())
                    return -1;
                else
                    return messageId;
            } else {
                return -1;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the message's id", se);
            return -1;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_CHANNEL_VERSION = "SELECT edition FROM channel WHERE channelId = ?";

    /** locally known edition of the given scope, or -1 if not known */
    public long getChannelVersion(Hash scope) {        
        if ( (scope == null) || (scope.getData() == null) ) return -1;
        long channelId = getChannelId(scope);
        if (channelId < 0) return -1;
        return getChannelVersion(channelId);
    }

    public long getChannelVersion(long channelId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_VERSION);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                long edition = rs.getLong(1);
                if (rs.wasNull())
                    edition = -1;
                return edition;
            } else {
                return -1;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the message's id", se);
            return -1;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }   
    }
    
    private static final String SQL_GET_CHANNEL_IDENT_KEY = "SELECT identKey FROM channel WHERE channelHash = ?";

    public SigningPublicKey getChannelIdentKey(Hash scope) {
        if (scope == null) return null;
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_IDENT_KEY);
            stmt.setBytes(1, scope.getData());
            rs = stmt.executeQuery();
            if (rs.next()) {
                byte data[] = rs.getBytes(1);
                if ( (data != null) && (data.length == SigningPublicKey.KEYSIZE_BYTES) )
                    return new SigningPublicKey(data);
                else
                    return null;
            } else {
                return null;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel ident key", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_CHANNEL_REPLY_KEY = "SELECT encryptKey FROM channel WHERE channelHash = ?";

    public PublicKey getChannelReplyKey(Hash scope) {
        if (scope == null) return null;
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_REPLY_KEY);
            stmt.setBytes(1, scope.getData());
            rs = stmt.executeQuery();
            if (rs.next()) {
                byte data[] = rs.getBytes(1);
                if ( (data != null) && (data.length == PublicKey.KEYSIZE_BYTES) )
                    return new PublicKey(data);
                else
                    return null;
            } else {
                return null;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel reply key", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_CHANNEL_IMPORT_DATE = "SELECT importDate FROM channel WHERE channelId = ?";

    /** when we imported the scope, or -1 if never */
    public long getChannelImportDate(Hash scope) {        
        if ( (scope == null) || (scope.getData() == null) ) return -1;
        long channelId = getChannelId(scope);
        if (channelId < 0) return -1;
        return getChannelImportDate(channelId);
    }

    public long getChannelImportDate(long channelId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CHANNEL_IMPORT_DATE);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                Date when = rs.getDate(1);
                if (rs.wasNull())
                    return -1;
                else
                    return when.getTime();
            } else {
                return -1;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the channel's import date", se);
            return -1;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }   
    }
    
    private static final String SQL_GET_MESSAGE_INFO = "SELECT authorChannelId, messageId, targetChannelId, subject, overwriteScopeHash, overwriteMessageId, " +
                                                       "forceNewThread, refuseReplies, wasEncrypted, wasPrivate, wasAuthorized, wasAuthenticated, isCancelled, expiration, scopeChannelId, wasPBE, readKeyMissing, replyKeyMissing, pbePrompt, importDate, deletionCause " +
                                                       "FROM channelMessage WHERE msgId = ?";
    private static final String SQL_GET_MESSAGE_HIERARCHY = "SELECT referencedChannelHash, referencedMessageId FROM messageHierarchy WHERE msgId = ? ORDER BY referencedCloseness ASC";
    private static final String SQL_GET_MESSAGE_TAG = "SELECT tag, isPublic FROM messageTag WHERE msgId = ?";
    private static final String SQL_GET_MESSAGE_PAGE_COUNT = "SELECT COUNT(*) FROM messagePage WHERE msgId = ?";
    private static final String SQL_GET_MESSAGE_ATTACHMENT_COUNT = "SELECT COUNT(*) FROM messageAttachment WHERE msgId = ?";

    public MessageInfo getMessage(long internalMessageId) {
        ensureLoggedIn();
        long start = System.currentTimeMillis();
        if (_trace) _getMsgCount++;
        MessageInfo info = new MessageInfo();
        info.setInternalId(internalMessageId);
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_INFO);
            stmt.setLong(1, internalMessageId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                // authorChannelId, messageId, targetChannelId, subject, overwriteScopeHash, overwriteMessageId,
                // forceNewThread, refuseReplies, wasEncrypted, wasPrivate, wasAuthorized, 
                // wasAuthenticated, isCancelled, expiration, scopeChannelId, wasPBE, importDate, deletionCause
                long authorId = rs.getLong(1);
                if (rs.wasNull()) authorId = -1;
                //byte author[] = rs.getBytes(1);
                long messageId = rs.getLong(2);
                if (rs.wasNull()) messageId = -1;
                long targetChannelId = rs.getLong(3);
                String subject = rs.getString(4);
                byte overwriteChannel[] = rs.getBytes(5);
                long overwriteMessage = rs.getLong(6);
                if (rs.wasNull()) overwriteMessage = -1;
                boolean forceNewThread = rs.getBoolean(7);
                if (rs.wasNull()) forceNewThread = false;
                boolean refuseReplies = rs.getBoolean(8);
                if (rs.wasNull()) refuseReplies = false;
                boolean wasEncrypted = rs.getBoolean(9);
                if (rs.wasNull()) wasEncrypted = true;
                boolean wasPrivate = rs.getBoolean(10);
                if (rs.wasNull()) wasPrivate = false;
                boolean wasAuthorized = rs.getBoolean(11);
                if (rs.wasNull()) wasAuthorized = false;
                boolean wasAuthenticated = rs.getBoolean(12);
                if (rs.wasNull()) wasAuthenticated = false;
                boolean cancelled = rs.getBoolean(13);
                if (rs.wasNull()) cancelled = false;
                java.sql.Date exp = rs.getDate(14);
                long scopeChannelId = rs.getLong(15);
                boolean wasPBE = rs.getBoolean(16);
                if (rs.wasNull())
                    wasPBE = false;
                
                boolean readKeyMissing = rs.getBoolean(17);
                if (rs.wasNull()) readKeyMissing = false;
                boolean replyKeyMissing = rs.getBoolean(18);
                if (rs.wasNull()) replyKeyMissing = false;
                String pbePrompt = rs.getString(19);
                Date importDate = rs.getDate(20);
                int deletionCause = rs.getInt(21);
                if (rs.wasNull()) deletionCause = -1;
                
                if (deletionCause > 0 ) {
                    _ui.debugMessage("message " + internalMessageId + " was deleted: " + deletionCause);
                    return null;
                }
                
                info.setReadKeyUnknown(readKeyMissing);
                info.setReplyKeyUnknown(replyKeyMissing);
                info.setPassphrasePrompt(pbePrompt);
                
                if (authorId >= 0) info.setAuthorChannelId(authorId);
                //if (author != null) info.setAuthorChannel(Hash.create(author));
                info.setMessageId(messageId);
                info.setScopeChannelId(scopeChannelId);
                Hash scope = getChannelHash(scopeChannelId);
                //ChannelInfo scope = getChannel(scopeChannelId);
                if (scope != null)
                    info.setURI(SyndieURI.createMessage(scope, messageId));
                info.setTargetChannelId(targetChannelId);
                Hash chan = getChannelHash(targetChannelId);
                //ChannelInfo chan = getChannel(targetChannelId);
                if (chan != null)
                    info.setTargetChannel(chan);//chan.getChannelHash());
                info.setSubject(subject);
                if ( (overwriteChannel != null) && (overwriteMessage >= 0) ) {
                    info.setOverwriteChannel(Hash.create(overwriteChannel));
                    info.setOverwriteMessage(overwriteMessage);
                }
                info.setForceNewThread(forceNewThread);
                info.setRefuseReplies(refuseReplies);
                info.setWasEncrypted(wasEncrypted);
                info.setWasPassphraseProtected(wasPBE);
                info.setWasPrivate(wasPrivate);
                info.setWasAuthorized(wasAuthorized);
                info.setWasAuthenticated(wasAuthenticated);
                info.setIsCancelled(cancelled);
                if (exp != null)
                    info.setExpiration(exp.getTime());
                else
                    info.setExpiration(-1);
                if (importDate != null)
                    info.setReceiveDate(importDate.getTime());
            } else {
                _ui.debugMessage("no matches for " + internalMessageId);
                return null;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the message's info", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }

        stmt = null;
        rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_HIERARCHY);
            stmt.setLong(1, internalMessageId);
            rs = stmt.executeQuery();
            List<SyndieURI> uris = new ArrayList();
            while (rs.next()) {
                // referencedChannelHash, referencedMessageId
                byte chan[] = rs.getBytes(1);
                long refId = rs.getLong(2);
                if (!rs.wasNull() && (chan != null) )
                    uris.add(SyndieURI.createMessage(Hash.create(chan), refId));
            }
            info.setHierarchy(uris);
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the message list", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }

        stmt = null;
        rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_TAG);
            stmt.setLong(1, internalMessageId);
            rs = stmt.executeQuery();
            Set encrypted = new HashSet();
            Set unencrypted = new HashSet();
            while (rs.next()) {
                // tag, wasEncrypted
                String tag = rs.getString(1);
                boolean isPublic = rs.getBoolean(2);
                if (rs.wasNull())
                    isPublic = false;
                if (isPublic)
                    unencrypted.add(tag);
                else
                    encrypted.add(tag);
            }
            info.setPublicTags(unencrypted);
            info.setPrivateTags(encrypted);
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the message's tags", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    
        stmt = null;
        rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_PAGE_COUNT);
            stmt.setLong(1, internalMessageId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                int pages = rs.getInt(1);
                if (!rs.wasNull())
                    info.setPageCount(pages);
            } else {
                info.setPageCount(0);
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the message's tags", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        info.setAttachmentCount(getMessageAttachmentCount(internalMessageId));
        
        // get the refs...
        MessageReferenceBuilder builder = new MessageReferenceBuilder(this);
        try {
            info.setReferences(builder.loadReferences(internalMessageId));
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the message references", se);
            return null;
        }
        
        long end = System.currentTimeMillis();
        if (_trace)
            _getMsgTime += (end-start);
        return info;
    }

    /**
     *  @return 0 on error
     *  @since 1.102b-11 pulled out of getMessage() above
     */
    public int getMessageAttachmentCount(long internalMessageId) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_ATTACHMENT_COUNT);
            stmt.setLong(1, internalMessageId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                int count = rs.getInt(1);
                if (!rs.wasNull())
                    return count;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the message's attachment count", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return 0;
    }

    public Set<String> getMessageTags(long chanId, long messageId, boolean includePrivate, boolean includePublic) {
        return getMessageTags(getMessageId(chanId, messageId), includePrivate, includePublic);
    }
        
    public Set<String> getMessageTags(long msgId, boolean includePrivate, boolean includePublic) {
        ensureLoggedIn();
        Set<String> rv = new HashSet();
        if (msgId < 0) 
            return null;
        
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_TAG);
            stmt.setLong(1, msgId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                // tag, wasEncrypted
                String tag = rs.getString(1);
                boolean isPublic = rs.getBoolean(2);
                if (rs.wasNull())
                    isPublic = false;
                if (isPublic && includePublic)
                    rv.add(tag);
                else if (!isPublic && includePrivate)
                    rv.add(tag);
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the message's tags", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }        
        return rv;
    }

    public Set<String> getMessageTags(Set msgIds, boolean includePublic, boolean includePrivate) {
        ensureLoggedIn();
        Set<String> rv = new HashSet();
        if ( (msgIds == null) || (msgIds.size() <= 0) ) return rv;
        
        Statement stmt = null;
        ResultSet rs = null;
        try {
            // i hate writing dynamic SQL - its ugly and bad for databases.  but,
            // putting this all in as a single sql statement has substantial performance benefits,
            // so...
            StringBuilder query = new StringBuilder("SELECT DISTINCT tag, isPublic FROM messageTag WHERE msgId IN (");
            for (Iterator iter = msgIds.iterator(); iter.hasNext(); ) {
                Long id = (Long)iter.next();
                query.append(id.longValue());
                if (iter.hasNext())
                    query.append(", ");
                else
                    query.append(")");
            }
            stmt = _con.createStatement();
            rs = stmt.executeQuery(query.toString());
            while (rs.next()) {
                // tag, wasEncrypted
                String tag = rs.getString(1);
                boolean isPublic = rs.getBoolean(2);
                if (rs.wasNull())
                    isPublic = false;
                if (isPublic && includePublic)
                    rv.add(tag);
                else if (!isPublic && includePrivate)
                    rv.add(tag);
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the group of message's tags", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return rv;
    }
    
    public SyndieURI getMessageURI(long msgId) {
        Hash scope = getMessageScope(msgId);
        long messageId = getMessageId(msgId);
        if ( (scope != null) && (messageId >= 0) )
            return SyndieURI.createMessage(scope, messageId);
        else
            return null;
    }
    private static final String SQL_GET_MESSAGE_AUTHOR = "SELECT authorChannelId FROM channelMessage WHERE msgId = ?";

    public long getMessageAuthor(long chanId, long messageId) { 
        return getMessageAuthor(getMessageId(chanId, messageId));
    }

    public long getMessageAuthor(long msgId) {
        if (msgId < 0) return -1;
        
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_AUTHOR);
            stmt.setLong(1, msgId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                long authorChanId = rs.getLong(1);
                if (rs.wasNull())
                    return -1;
                else
                    return authorChanId;
            } else {
                return -1;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the message's authorChanId", se);
            return -1;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_MESSAGE_SUBJECT = "SELECT subject FROM channelMessage WHERE msgId = ?";

    public String getMessageSubject(long msgId) {
        if (msgId < 0) return null;
        
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_SUBJECT);
            stmt.setLong(1, msgId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                return rs.getString(1);
            } else {
                return null;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the message's subject", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    /** FIXME doesn't cover CLOBS */
    private static final String SQL_MATCH_MESSAGE_KEYWORD = "SELECT msgId FROM channelMessage WHERE msgId = ? AND subject LIKE ?" +
                                                            " UNION " +
                                                            "SELECT msgId FROM messagePageData WHERE msgId = ? AND dataString LIKE ?";

    public boolean messageKeywordMatch(long msgId, String keyword) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_MATCH_MESSAGE_KEYWORD);
            stmt.setLong(1, msgId);
            stmt.setString(2, "%" + keyword + "%");
            stmt.setLong(3, msgId);
            stmt.setString(4, "%" + keyword + "%");
            rs = stmt.executeQuery();
            boolean match = rs.next();
            return match;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error searching for the keyword", se);
            return false;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    public static final int PRIVACY_UNKNOWN = -1;
    public static final int PRIVACY_PBE = 0;
    public static final int PRIVACY_PRIVREPLY = 1;
    public static final int PRIVACY_AUTHORIZEDONLY = 2;
    public static final int PRIVACY_PUBLIC = 3;

    private static final String SQL_GET_MESSAGE_PRIVACY = "SELECT wasEncrypted, wasPBE, wasPrivate, wasAuthorized FROM channelMessage WHERE msgId = ? AND readKeyMissing = FALSE AND pbePrompt IS NULL AND replyKeyMissing = FALSE";

    public int getMessagePrivacy(long msgId) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_PRIVACY);
            stmt.setLong(1, msgId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                boolean encrypted = rs.getBoolean(1);
                if (rs.wasNull()) encrypted = false;
                boolean pbe = rs.getBoolean(2);
                if (rs.wasNull()) pbe = false;
                boolean privReply = rs.getBoolean(3);
                if (rs.wasNull()) privReply = false;
                boolean authorized = rs.getBoolean(4);
                if (rs.wasNull()) authorized = false;
                
                if (!encrypted)
                    return PRIVACY_PUBLIC;
                else if (pbe)
                    return PRIVACY_PBE;
                else if (privReply)
                    return PRIVACY_PRIVREPLY;
                else
                    return PRIVACY_AUTHORIZEDONLY;
            } else {
                return PRIVACY_UNKNOWN;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error getting message privacy", se);
            return PRIVACY_UNKNOWN;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_MESSAGE_PASSPHRASE_PROMPT = "SELECT pbePrompt FROM channelMessage WHERE msgId = ?";

    /**
     * return the passphrase prompt required to decrypt the pbe encrypted message, 
     * or null if the message is already decrypted or does not require a passphrase
     */
    public String getMessagePassphrasePrompt(long msgId) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_PASSPHRASE_PROMPT);
            stmt.setLong(1, msgId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                return rs.getString(1);
            } else {
                return null;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error getting message passphrase prompt", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }        
    }
    
    /** page number starts at 0 */
    private static final String SQL_GET_MESSAGE_PAGE_DATA_TYPE = "SELECT storageType FROM messagePageData WHERE msgId = ? AND pageNum = ?";
    private static final String SQL_GET_MESSAGE_PAGE_DATA = "SELECT dataString FROM messagePageData WHERE msgId = ? AND pageNum = ?";
    private static final String SQL_GET_MESSAGE_PAGE_DATA_CLOB = "SELECT lob FROM messagePageData WHERE msgId = ? AND pageNum = ?";

    public String getMessagePageData(long internalMessageId, int pageNum) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        // get the storage type
        int type = -1;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_PAGE_DATA_TYPE);
            stmt.setLong(1, internalMessageId);
            stmt.setInt(2, pageNum);
            rs = stmt.executeQuery();
            if (!rs.next())
                return null;
            type = rs.getInt(1);
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the page data", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        // now get the data
        try {
            if (type == 0)
                stmt = _con.prepareStatement(SQL_GET_MESSAGE_PAGE_DATA);
            else if (type == 1)
                stmt = _con.prepareStatement(SQL_GET_MESSAGE_PAGE_DATA_CLOB);
            else
                throw new SQLException("Unknown storage type " + type);
            stmt.setLong(1, internalMessageId);
            stmt.setInt(2, pageNum);
            rs = stmt.executeQuery();
            if (rs.next())
                return rs.getString(1);
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the page data", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return null;
    }

    /** page number starts at 0 */
    private static final String SQL_GET_MESSAGE_PAGE_CONFIG = "SELECT dataString FROM messagePageConfig WHERE msgId = ? AND pageNum = ?";

    public String getMessagePageConfig(long internalMessageId, int pageNum) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_PAGE_CONFIG);
            stmt.setLong(1, internalMessageId);
            stmt.setInt(2, pageNum);
            rs = stmt.executeQuery();
            if (rs.next())
                return rs.getString(1);
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the page config", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return null;
    }

    /** attachment number starts at 0 */    
    private static final String SQL_GET_MESSAGE_ATTACHMENT_DATA_TYPE = "SELECT storageType FROM messageAttachmentData WHERE msgId = ? AND attachmentNum = ?";
    private static final String SQL_GET_MESSAGE_ATTACHMENT_DATA = "SELECT dataBinary FROM messageAttachmentData WHERE msgId = ? AND attachmentNum = ?";
    private static final String SQL_GET_MESSAGE_ATTACHMENT_DATA_BLOB = "SELECT lob FROM messageAttachmentData WHERE msgId = ? AND attachmentNum = ?";

    /**
     *  TODO can we get this as a stream or otherwise not load it all into memory?
     */
    public byte[] getMessageAttachmentData(long internalMessageId, int attachmentNum) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        // get the storage type
        int type = -1;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_ATTACHMENT_DATA_TYPE);
            stmt.setLong(1, internalMessageId);
            stmt.setInt(2, attachmentNum);
            rs = stmt.executeQuery();
            if (!rs.next())
                return null;
            type = rs.getInt(1);
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the attachment data", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        // now get the data
        try {
            if (type == 0)
                stmt = _con.prepareStatement(SQL_GET_MESSAGE_ATTACHMENT_DATA);
            else if (type == 1)
                stmt = _con.prepareStatement(SQL_GET_MESSAGE_ATTACHMENT_DATA_BLOB);
            else
                throw new SQLException("Unknown storage type " + type);
            stmt.setLong(1, internalMessageId);
            stmt.setInt(2, attachmentNum);
            rs = stmt.executeQuery();
            if (rs.next())
                return rs.getBytes(1);
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the attachment data", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return null;
    }

    /**
     *  Get as stream
     *  @return null on error
     *  @since 1.104b-2
     */
    public InputStream getMessageAttachmentAsStream(long internalMessageId, int attachmentNum) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        // get the storage type
        int type = -1;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_ATTACHMENT_DATA_TYPE);
            stmt.setLong(1, internalMessageId);
            stmt.setInt(2, attachmentNum);
            rs = stmt.executeQuery();
            if (!rs.next())
                return null;
            type = rs.getInt(1);
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the attachment data", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        // now get the data
        try {
            if (type == 0)
                stmt = _con.prepareStatement(SQL_GET_MESSAGE_ATTACHMENT_DATA);
            else if (type == 1)
                stmt = _con.prepareStatement(SQL_GET_MESSAGE_ATTACHMENT_DATA_BLOB);
            else
                throw new SQLException("Unknown storage type " + type);
            stmt.setLong(1, internalMessageId);
            stmt.setInt(2, attachmentNum);
            rs = stmt.executeQuery();
            if (rs.next()) {
                if (type == 0) {
                    byte[] b = rs.getBytes(1);
                    return b != null ? new ByteArrayInputStream(b) : null;
                } else {
                    return rs.getBinaryStream(1);
                }
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the attachment data", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return null;
    }
    
    private static final String SQL_GET_MESSAGE_ATTACHMENT_SIZE = "SELECT attachmentSize FROM messageAttachment WHERE msgId = ? AND attachmentNum = ?";

    /**
     *  FIXME should store and return long
     *  @param attachmentNum starts at 0
     */
    public long getMessageAttachmentSize(long internalMessageId, int attachmentNum) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_ATTACHMENT_SIZE);
            stmt.setLong(1, internalMessageId);
            stmt.setInt(2, attachmentNum);
            rs = stmt.executeQuery();
            if (rs.next()) {
                return rs.getLong(1);
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the attachment data", se);
            return 0;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return 0;
    }
    
    /** attachment number starts at 0 */
    public Properties getMessageAttachmentConfig(long internalMessageId, int attachmentNum) {
        String cfg = getMessageAttachmentConfigRaw(internalMessageId, attachmentNum);
        Properties rv = new Properties();
        if (cfg != null)
            CommandImpl.parseProps(cfg, rv);
        return rv;
    }
    
    private static final String SQL_GET_MESSAGE_ATTACHMENT_CONFIG = "SELECT dataString FROM messageAttachmentConfig WHERE msgId = ? AND attachmentNum = ?";

    /** attachment number starts at 0 */
    String getMessageAttachmentConfigRaw(long internalMessageId, int attachmentNum) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_ATTACHMENT_CONFIG);
            stmt.setLong(1, internalMessageId);
            stmt.setInt(2, attachmentNum);
            rs = stmt.executeQuery();
            if (rs.next())
                return rs.getString(1);
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the attachment config", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return null;
    }

    /**
     *  Move big things to LOBs.
     *  Table must have lob and storageType columns.
     *  Required DB version 25 (ddl_update24.txt) and hsqldb 2.3.0
     *
     *  Did not work through hsqldb 2.2.9:
     *  http://sourceforge.net/projects/hsqldb/forums/forum/73674/topic/5519631
     *
     *  @param col1 primary key (long)
     *  @param col2 primary key (int)
     *  @since 1.104b-2
     */
    private void migrateToLob(String table, String col1, String col2, String bigColumn, long maxLen, boolean isBinary) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        PreparedStatement stmt2 = null;
        PreparedStatement stmt3 = null;
        ResultSet rs = null;
        ResultSet rs2 = null;
        try {
            // statement to get all matching items
            if (isBinary)
                stmt = _con.prepareStatement(
                       "SELECT " + col1 + ", " + col2 +
                       " FROM " + table +
                       " WHERE OCTET_LENGTH(" + bigColumn + ") > " + maxLen +
                       " AND storageType = 0");
            else
                stmt = _con.prepareStatement(
                       "SELECT " + col1 + ", " + col2 +
                       " FROM " + table +
                       " WHERE LENGTH(" + bigColumn + ") > " + maxLen +
                       " AND storageType = 0");
            rs = stmt.executeQuery();
            // statement to get one large byte[] or string
            stmt2 = _con.prepareStatement(
                       "SELECT " + bigColumn +
                       " FROM " + table +
                       " WHERE " + col1 + " = ? " +
                       " AND " + col2 + " = ? ");
            // statement to set one BLOB or CLOB and null out the large byte[] or string
            stmt3 = _con.prepareStatement(
                       "UPDATE " + table +
                       " SET lob = ?, storageType = 1, " + bigColumn + " = NULL" +
                       " WHERE " + col1 + " = ? " +
                       " AND " + col2 + " = ? ");
            while (rs.next()) {
                // fetch the items one at a time so we don't OOM
                long k1 = rs.getLong(1);
                int k2 = rs.getInt(2);
                stmt2.setLong(1, k1);
                stmt2.setInt(2, k2);
                rs2 = stmt2.executeQuery();
                if (rs2.next()) {
                    int sz;
                    if (isBinary) {
                        byte[] b = rs2.getBytes(1);
                        sz = b.length;
                        stmt3.setBlob(1, new SerialBlob(b));
                    } else {
                        String s = rs2.getString(1);
                        sz = s.length();
                        stmt3.setClob(1, new SerialClob(s.toCharArray()));
                    }
                    if (_log.shouldLog(Log.WARN))
                        _log.warn("Migrating " + table + '(' + k1 + ',' + k2 + ") size " + sz);
                    stmt3.setLong(2, k1);
                    stmt3.setInt(3, k2);
                    stmt3.executeUpdate();
                } else {
                    if (_log.shouldLog(Log.WARN))
                        _log.warn("Huh no rs2 result?");
                }
                rs2.close();
            }
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error migrating to lobs", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (rs2 != null) try { rs2.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            if (stmt2 != null) try { stmt2.close(); } catch (SQLException se) {}
            if (stmt3 != null) try { stmt3.close(); } catch (SQLException se) {}
        }
    }

    private static final String SQL_GET_PUBLIC_POSTING_CHANNELS = "SELECT channelId, name, petname FROM channel c LEFT OUTER JOIN nymChannelPetName ncpn ON c.channelId = ncpn.channelId WHERE allowPubPost = TRUE ORDER BY petname, name ASC";

    /** list of channel ids (Long) that anyone is allowed to post to */
    public List<Long> getPublicPostingChannelIds() {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_PUBLIC_POSTING_CHANNELS);
            rs = stmt.executeQuery();
            List<Long> rv = new ArrayList();
            while (rs.next()) {
                long id = rs.getLong(1);
                if (!rs.wasNull())
                    rv.add(Long.valueOf(id));
            }
            return rv;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the public posting channels", se);
            return Collections.EMPTY_LIST;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_BANNED = "SELECT channelHash, bannedOn FROM banned";

    /** list of channels (Hash) that this archive wants nothing to do with */
    public ArrayList<Hash> getBannedChannels() { return getBannedChannels(false); }

    public ArrayList<Hash> getBannedChannels(boolean newOnly) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_BANNED);
            rs = stmt.executeQuery();
            ArrayList<Hash> rv = new ArrayList();
            while (rs.next()) {
                byte chan[] = rs.getBytes(1);
                Date when = rs.getDate(2);
                if ( (chan != null) && (chan.length == Hash.HASH_LENGTH) ) {
                    if (newOnly && (when != null) && (when.getTime() <= (System.currentTimeMillis()-SharedArchiveBuilder.PERIOD_NEW)) )
                        continue;
                    rv.add(Hash.create(chan));
                }
            }
            return rv;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the banned channels", se);
            return new ArrayList(0);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    /**
     * ban the author or channel so that no more posts from that author
     * or messages by any author in that channel will be allowed into the
     * Syndie archive.  If delete is specified, the messages themselves
     * will be removed from the archive as well as the database
     */
    public void ban(Hash bannedChannel, UI ui, boolean deleteMessages) { ban(bannedChannel, ui, deleteMessages, deleteMessages); }

    public void ban(Hash bannedChannel, UI ui, boolean deleteMessages, boolean deleteMeta) {
        ensureLoggedIn();
        addBan(bannedChannel, ui);
        if (deleteMessages || deleteMeta)
            executeDelete(bannedChannel, ui, deleteMessages || deleteMeta, deleteMeta, DELETION_CAUSE_BAN);
    }

    private static final String SQL_BAN = "INSERT INTO banned (channelHash) VALUES (?)";

    private void addBan(Hash bannedChannel, UI ui) {
        if (getBannedChannels().contains(bannedChannel)) {
            ui.debugMessage("Channel already banned");
            return;
        }
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_BAN);
            stmt.setBytes(1, bannedChannel.getData());
            int rows = stmt.executeUpdate();
            if (rows != 1) {
                throw new SQLException("Ban added " + rows + " rows?");
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error banning the channel", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_UNBAN = "DELETE FROM banned WHERE channelHash = ?";

    public void unban(Hash bannedChannel) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_UNBAN);
            stmt.setBytes(1, bannedChannel.getData());
            int rows = stmt.executeUpdate();
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error unbanning the channel", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private void executeDelete(Hash bannedChannel, UI ui, boolean deleteMessages, boolean deleteMeta, int cause) {
        // delete the banned channel itself from the archive
        // then list any messages posted by that author in other channels and
        // delete them too
        // (implicit index regen?)
        List<SyndieURI> urisToDelete = getURIsToDelete(bannedChannel, deleteMessages, deleteMeta);
        ui.debugMessage("Delete the following URIs: " + urisToDelete);
        for (int i = 0; i < urisToDelete.size(); i++) {
            SyndieURI uri = (SyndieURI)urisToDelete.get(i);
            deleteFromArchive(uri, ui);
            deleteFromDB(uri, ui, cause);
        }
    }

    private void deleteFromArchive(SyndieURI uri, UI ui) {
        File archiveDir = getArchiveDir();
        File chanDir = new File(archiveDir, uri.getScope().toBase64());
        if (uri.getMessageId() == null) {
            File metaFile = new File(chanDir, "meta" + Constants.FILENAME_SUFFIX);
            metaFile.delete();
            ui.debugMessage("Deleted metadata file " + metaFile.getPath());
            ui.statusMessage("Deleted the channel metadata " + uri.getScope().toBase64() + " from the archive");
            /*
            // delete the whole channel - all posts, metadata, and even the dir
            File f[] = chanDir.listFiles();
            for (int i = 0; i < f.length; i++) {
                f[i].delete();
                ui.debugMessage("Deleted channel file " + f[i].getPath());
            }
            chanDir.delete();
            ui.debugMessage("Deleted channel dir " + chanDir.getPath());
            ui.statusMessage("Deleted " + (f.length-1) + " messages and the metadata for channel " + uri.getScope().toBase64() + " from the archive");
             */
        } else {
            // delete just the given message
            File msgFile = new File(chanDir, uri.getMessageId().longValue() + Constants.FILENAME_SUFFIX);
            msgFile.delete();
            ui.debugMessage("Deleted message file " + msgFile.getPath());
            ui.statusMessage("Deleted the post " + uri.getScope().toBase64() + " from the archive");
        }
        if (chanDir != null) {
            String files[] = chanDir.list();
            if ( (files == null) || (files.length == 0) )
                chanDir.delete();
        }
    }
    
    public void cancelMessage(final SyndieURI cancelledURI, UI ui) {
        _ui.errorMessage("cancelling " + cancelledURI);
        
        final long msgId = getMessageId(cancelledURI.getScope(), cancelledURI.getMessageId());
        if (msgId < 0)
            return;
        final long scopeId = getChannelId(cancelledURI.getScope());
        final long targetId = getMessageTarget(msgId);
        final Hash targetChannel = getChannelHash(targetId);
      
        long metaChannelId = -1;
        //
        // if we have the identity key for the author, post as that.  otherwise, if we
        // have the identity key for the message's forum, post as that.
        //
        DBClient.ChannelCollector chans = getNymChannels();
        Hash sendAs = null;
        if (chans.getIdentityChannelIds().contains(Long.valueOf(scopeId))) {
            sendAs = getChannelHash(scopeId);
            metaChannelId = scopeId;
            _ui.debugMessage("sending cancel message as the original author: " + cancelledURI);
        } else if (chans.getIdentityChannelIds().contains(Long.valueOf(targetId))) {
            _ui.debugMessage("sending cancel message as the forum owner: " + cancelledURI);
            metaChannelId = targetId;
            sendAs = targetChannel;
        } else {
            _ui.errorMessage("cancel messages can only be sent by the message's author or the forum's owner at the moment");
            return;
        }

        final Hash cancelMsgAuthor = sendAs;
        final long cancelMetaId = metaChannelId;
        
        final MessageCreator creator = new MessageCreatorDirect(new MessageCreatorSource() {
            public MessageCreator.ExecutionListener getListener() {
                return new MessageCreator.ExecutionListener() {
                    public void creationComplete(MessageCreator exec, SyndieURI uri, String errors, boolean successful, SessionKey replySessionKey, byte[] replyIV, File msg) {
                        if (successful) {
                            boolean ok = exec.importCreated(DBClient.this, _ui, uri, msg, replyIV, replySessionKey, null);
                            if (ok) {
                                if (cancelMetaId >= 0) {
                                    buildCancelMeta(cancelMetaId, cancelledURI);
                                } else {
                                    _ui.debugMessage("not building a new metadata for the cancel message: " + cancelMsgAuthor + ": " + cancelledURI);
                                }
                            } else {
                                _ui.errorMessage("Error importing the newly created cancel message of " + cancelledURI);
                            }
                        } else {
                            _ui.errorMessage("Error generating the cancel message of " + cancelledURI);
                        }
                        exec.cleanup();
                    }
                    
                };
            }
            public DBClient getClient() { return DBClient.this; }
            public UI getUI() { return _ui; }
            public Hash getAuthor() { return cancelMsgAuthor; }
            public Hash getTarget() { return targetChannel; }
            public Hash getSignAs() { return null; }
            public boolean getAuthorHidden() { return false; }
            public String getPageTitle(int page) { return "cancel: " + cancelledURI; }
            public int getPageCount() { return 0; }
            public String getPageContent(int page) { return ""; }
            public String getPageType(int page) { return ""; }
            public List<String> getAttachmentNames() { return new ArrayList(); }
            public List<String> getAttachmentTypes() { return new ArrayList(); }
            public byte[] getAttachmentData(int attachmentIndex) { return null; }
            public String getSubject() { return ""; }
            public boolean getPrivacyPBE() { return false; }
            public String getPassphrase() { return null; }
            public String getPassphrasePrompt() { return null; }
            public boolean getPrivacyPublic() { return true; }
            public String getAvatarUnmodifiedFilename() { return null; }
            public byte[] getAvatarModifiedData() { return null; }
            public boolean getPrivacyReply() { return false; }
            public String[] getPublicTags() { return new String[0]; }
            public String[] getPrivateTags() { return new String[0]; }
            public List<NymReferenceNode> getReferenceNodes() { return new ArrayList(); }
            public int getParentCount() { return 0; }
            public SyndieURI getParent(int depth) { return null; }
            public String getExpiration() { return null; }
            public boolean getForceNewThread() { return true; }
            public boolean getRefuseReplies() { return true; }
            public List getCancelURIs() { 
                ArrayList rv = new ArrayList();
                rv.add(cancelledURI);
                return rv;
            }
        });
        creator.execute();
        
        // don't explicitly delete here - wait to honor the cancel message
        // deleteMessage(cancelledURI, ui, true, true);
    }
    
    private void buildCancelMeta(final long channelId, final SyndieURI cancelledURI) {
        _ui.debugMessage("building cancel meta for " + channelId + "/" + getChannelHash(channelId) + " to include " + cancelledURI);
        final ChannelInfo info = getChannel(channelId);
        ManageForumExecutor exec = new ManageForumExecutor(this, _ui, new ManageForumExecutor.ManageForumState() {
            public byte[] getAvatarData() { return getChannelAvatar(channelId); }
            public String getName() { return info.getName(); }
            public String getDescription() { return info.getDescription(); }
            public long getLastEdition() { return info.getEdition(); }
            public boolean getAllowPublicPosts() { return info.getAllowPublicPosts(); }
            public boolean getAllowPublicReplies() { return info.getAllowPublicReplies(); }
            public Set<String> getPublicTags() { return info.getPublicTags(); }
            public Set<String> getPrivateTags() { return info.getPrivateTags(); }
            public Set<SigningPublicKey> getAuthorizedPosters() { return info.getAuthorizedPosters(); }
            public Set<SigningPublicKey> getAuthorizedManagers() { return info.getAuthorizedManagers(); }
            public String getReferences() { 
                List refs = info.getReferences();
                if ( (refs != null) && (refs.size() > 0) )
                    return ReferenceNode.walk(refs);
                else
                    return "";
            }
            public Set getPublicArchives() { return info.getPublicArchives(); }
            public Set getPrivateArchives() { return info.getPrivateArchives(); }
            public boolean getEncryptContent() { return info.getReadKeysArePublic(); }
            public long getChannelId() { return channelId; }
            public boolean getPBE() { return false; }
            public String getPassphrase() { return null; }
            public String getPassphrasePrompt() { return null; }
            public List getCurrentReadKeys() { 
                Set keys = info.getReadKeys();
                if (keys != null)
                    return new ArrayList(keys);
                else
                    return new ArrayList();
            }
            /** should we create a new read key? */
            public boolean getCreateReadKey() { return false; }
            /** should we create a new forum and include its hash in our authorized-posters set? */
            public boolean getCreatePostIdentity() { return false; }
            /** should we create a new forum and include its hash in our authorized-managers set? */
            public boolean getCreateManageIdentity() { return false; }
            /** should we create a new reply key? */
            public boolean getCreateReplyKey() { return false; }
            public List<SyndieURI> getCancelledURIs() {
                List<SyndieURI> cancelled = getChannelCancelURIs(channelId);
                cancelled.add(cancelledURI);
                while (cancelled.size() > Constants.MAX_CANCELLED_PER_META)
                    cancelled.remove(0);
                return cancelled;
            }
        });
        exec.execute();
        String err = exec.getErrors();
        if ( (err != null) && (err.length() > 0) )
            _ui.errorMessage("error building cancel metadata for " + channelId + "/" + cancelledURI + ": " + err);
        else
            _ui.debugMessage("cancel metadata built for " + channelId + "/" + cancelledURI);
    }
    
    private static final int DELETION_CAUSE_OTHER = -1;
    private static final int DELETION_CAUSE_BAN = 0;
    private static final int DELETION_CAUSE_EXPLICIT = 1;
    private static final int DELETION_CAUSE_EXPIRE = 2;
    private static final int DELETION_CAUSE_CANCELLED = 3;
    /** the message was only a stub message containing cancel requests */
    private static final int DELETION_CAUSE_STUB = 4;
    
    private static final String SQL_DELETE_MESSAGE = "DELETE FROM channelMessage WHERE msgId = ?";
    private static final String SQL_DELETE_CHANNEL = "DELETE FROM channel WHERE channelId = ?";
    private static final String SQL_DELETE_READ_KEYS = "DELETE FROM channelReadKey WHERE channelId = ?";
    private static final String SQL_DELETE_UNREAD_CHANNELS = "DELETE FROM nymUnreadChannel WHERE channelId = ?";
    private static final String SQL_DELETE_UNREAD_MESSAGE = "DELETE FROM nymUnreadMessage WHERE msgId = ?";
    private static final String SQL_UPDATE_MESSAGE_DELETION_CAUSE = "UPDATE channelMessage SET deletionCause = ? WHERE msgId = ?";
    
    public void deleteMessage(SyndieURI uri, UI ui, boolean deleteDB) {
        deleteMessage(uri, ui, deleteDB, true);
    }

    public void deleteMessage(SyndieURI uri, UI ui, boolean deleteDB, boolean deleteFromArchive) {
        deleteFromArchive(uri, ui);
        if (deleteDB)
            deleteFromDB(uri, ui, DELETION_CAUSE_EXPLICIT);
    }
    
    void deleteFromDB(SyndieURI uri, UI ui) { deleteFromDB(uri, ui, DELETION_CAUSE_OTHER); }

    void deleteFromDB(SyndieURI uri, UI ui, int deletionCause) {
        long scopeId = getChannelId(uri.getScope());
        if (uri.getMessageId() == null) {
            // delete the whole channel, though all of the posts
            // will be deleted separately
            if (deletionCause < 0)
                ui.debugMessage("Deleting the channel " + uri.getScope().toBase64() + " from the database" +
                                " cause = " + deletionCause, new Exception("I did it"));
            deleteFromDB(scopeId, ui);
        } else {
            // delete just the given message
            long internalId = getMessageId(scopeId, uri.getMessageId().longValue());
            Exception exception = deleteMessageFromDB(internalId, deletionCause);
            if (exception == null) {
                if (deletionCause < 0)
                    ui.debugMessage("Deleted the post " + uri.getScope().toBase64() + ":" + uri.getMessageId() + " from the database" +
                                 " cause = " + deletionCause, new Exception("I did it"));
            } else {
                ui.errorMessage("Error deleting the post " + uri, exception);
            }
        }
    }

    /**
     *  Delete a channel
     *  @since 1.104b-3 from method above
     */
    private void deleteFromDB(long scopeId, UI ui) {
            try {
                exec(ImportMeta.SQL_DELETE_TAGS, scopeId);
                exec(ImportMeta.SQL_DELETE_POSTKEYS, scopeId);
                exec(ImportMeta.SQL_DELETE_MANAGEKEYS, scopeId);
                exec(ImportMeta.SQL_DELETE_ARCHIVE_URIS, scopeId);
                exec(ImportMeta.SQL_DELETE_ARCHIVES, scopeId);
                exec(ImportMeta.SQL_DELETE_CHAN_ARCHIVES, scopeId);
                exec(SQL_DELETE_READ_KEYS, scopeId);
                exec(ImportMeta.SQL_DELETE_CHANNEL_META_HEADER, scopeId);
                exec(ImportMeta.SQL_DELETE_CHANNEL_REF_URIS, scopeId);
                exec(ImportMeta.SQL_DELETE_CHANNEL_REFERENCES, scopeId);
                exec(SQL_DELETE_CHANNEL, scopeId);
                exec(SQL_DELETE_UNREAD_CHANNELS, scopeId);
            } catch (SQLException se) {
                ui.errorMessage("Unable to delete the channel " + scopeId, se);
            }
    }

    public Exception expireMessageFromDB(long msgId) { return deleteMessageFromDB(msgId, DELETION_CAUSE_EXPIRE); }

    Exception deleteMessageFromDB(long msgId, int deletionCause) {
        try {
            exec(ImportPost.SQL_DELETE_MESSAGE_HIERARCHY, msgId);
            exec(ImportPost.SQL_DELETE_MESSAGE_TAGS, msgId);
            exec(ImportPost.SQL_DELETE_MESSAGE_ATTACHMENT_DATA, msgId);
            exec(ImportPost.SQL_DELETE_MESSAGE_ATTACHMENT_CONFIG, msgId);
            exec(ImportPost.SQL_DELETE_MESSAGE_ATTACHMENTS, msgId);
            exec(ImportPost.SQL_DELETE_MESSAGE_PAGE_DATA, msgId);
            exec(ImportPost.SQL_DELETE_MESSAGE_PAGE_CONFIG, msgId);
            exec(ImportPost.SQL_DELETE_MESSAGE_PAGES, msgId);
            exec(ImportPost.SQL_DELETE_MESSAGE_REF_URIS, msgId);
            exec(ImportPost.SQL_DELETE_MESSAGE_REFS, msgId);
            exec(SQL_DELETE_UNREAD_MESSAGE, msgId);
            switch (deletionCause) {
                case DELETION_CAUSE_BAN:
                    exec(SQL_DELETE_MESSAGE, msgId);
                    break;
                case DELETION_CAUSE_CANCELLED:
                case DELETION_CAUSE_EXPIRE:
                case DELETION_CAUSE_EXPLICIT:
                case DELETION_CAUSE_STUB:
                default:
                    exec(SQL_UPDATE_MESSAGE_DELETION_CAUSE, deletionCause, msgId);
                    break;
            }
            return null;
        } catch (SQLException se) {
            return se;
        }
    }
    
    private static final String SQL_GET_SCOPE_MESSAGES = "SELECT msgId, scopeChannelId, messageId FROM channelMessage WHERE scopeChannelId = ? OR authorChannelId = ? OR targetChannelId = ?";

    private List<SyndieURI> getURIsToDelete(Hash bannedChannel, boolean deleteMessages, boolean deleteMeta) {
        List<SyndieURI> urisToDelete = new ArrayList();
        if (deleteMeta)
            urisToDelete.add(SyndieURI.createScope(bannedChannel));
        if (deleteMessages) {
            long scopeId = getChannelId(bannedChannel);
            if (scopeId >= 0) {
                PreparedStatement stmt = null;
                ResultSet rs = null;
                try {
                    stmt = _con.prepareStatement(SQL_GET_SCOPE_MESSAGES);
                    stmt.setLong(1, scopeId);
                    stmt.setLong(2, scopeId);
                    stmt.setLong(3, scopeId);
                    rs = stmt.executeQuery();
                    while (rs.next()) {
                        //long msgId = rs.getLong(1);
                        //if (rs.wasNull())
                        //    msgId = -1;
                        long scopeChanId = rs.getLong(2);
                        if (rs.wasNull())
                            scopeChanId = -1;
                        long messageId = rs.getLong(3);
                        if (rs.wasNull())
                            messageId = -1;
                        if ( (messageId >= 0) && (scopeChanId >= 0) ) {
                            ChannelInfo chanInfo = getChannel(scopeChanId);
                            if (chanInfo != null)
                                urisToDelete.add(SyndieURI.createMessage(chanInfo.getChannelHash(), messageId));
                        }
                    }
                } catch (SQLException se) {
                    if (_log.shouldLog(Log.ERROR))
                        _log.error("Error retrieving the messages to delete", se);
                    return Collections.EMPTY_LIST;
                } finally {
                    if (rs != null) try { rs.close(); } catch (SQLException se) {}
                    if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
                }
            } else {
                // not known.  noop
            }
        }
        return urisToDelete;
    }

    private Properties _nymPrefsCached;

    private static final String SQL_GET_NYMPREFS = "SELECT prefName, prefValue FROM nymPref WHERE nymId = ?";

    /**
     *  Current nymId. Cached.
     *  @return non-null
     */
    public Properties getNymPrefs() { 
        return getNymPrefs(_nymId);
    }

    /**
     *  Cached if nymId == getNymId()
     *  @return non-null
     */
    public Properties getNymPrefs(long nymId) {
        if (_nymId == nymId && _nymPrefsCached != null) 
            return _nymPrefsCached;
        ensureLoggedIn();
        Properties rv = new Properties();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_NYMPREFS);
            stmt.setLong(1, nymId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                String name = rs.getString(1);
                String val = rs.getString(2);
                rv.setProperty(name, val);
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error getting the nym's preferences", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return rv;
    }

    private static final String SQL_SET_NYMPREFS = "INSERT INTO nymPref (nymId, prefName, prefValue) VALUES (?, ?, ?)";
    private static final String SQL_DELETE_NYMPREFS = "DELETE FROM nymPref WHERE nymId = ?";

    /**
     *  Current nymId. Cached.
     */
    public void setNymPrefs(Properties prefs) { 
        setNymPrefs(_nymId, prefs);
    }

    /**
     *  Cached if nymId == getNymId()
     */
    public void setNymPrefs(long nymId, Properties prefs) {
        if (_nymId == nymId) 
            _nymPrefsCached = (Properties)prefs.clone();
        ensureLoggedIn();
        PreparedStatement stmt = null;
        try {
            exec(SQL_DELETE_NYMPREFS, nymId);
            stmt = _con.prepareStatement(SQL_SET_NYMPREFS);
            for (Iterator iter = prefs.keySet().iterator(); iter.hasNext(); ) {
                String name = (String)iter.next();
                String val = prefs.getProperty(name);
                stmt.setLong(1, nymId);
                stmt.setString(2, name);
                stmt.setString(3, val);
                stmt.executeUpdate();
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error setting the nym's preferences", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_NYMARCHIVENAMES = "SELECT name FROM nymArchive where nymId = ? ORDER BY name ASC";

    public List<String> getNymArchiveNames() { return getNymArchiveNames(_nymId); }

    public List<String> getNymArchiveNames(long nymId) { 
        ensureLoggedIn();
        ArrayList<String> rv = new ArrayList();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_NYMARCHIVENAMES);
            stmt.setLong(1, nymId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                String name = rs.getString(1);
                rv.add(name);
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error getting the nym's archive names", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return rv;
    }

    private static final String SQL_GET_NYM_REFERENCES = "SELECT groupId, parentGroupId, siblingOrder, name, description, uriId FROM resourceGroup WHERE nymId = ? ORDER BY parentGroupId ASC, siblingOrder ASC";

    /** return a list of NymReferenceNode instances for the nym's bookmarks / banned / ignored */
    public List<ReferenceNode> getNymReferences() { return getNymReferences(_nymId); }

    public List<ReferenceNode> getNymReferences(long nymId) {
        ensureLoggedIn();
        Map<Long, NymReferenceNode> groupIdToNode = new TreeMap();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_NYM_REFERENCES);
            stmt.setLong(1, nymId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                // groupId, parentGroupId, siblingOrder, name, description, uriId
                long groupId = rs.getLong(1);
                if (rs.wasNull()) groupId = -1;
                long parentGroupId = rs.getLong(2);
                if (rs.wasNull()) parentGroupId = -1;
                int order = rs.getInt(3);
                if (rs.wasNull()) order = 0;
                String name = rs.getString(4);
                String desc = rs.getString(5);
                long uriId = rs.getLong(6);
                if (rs.wasNull()) uriId = -1;
                
                SyndieURI uri = getURI(uriId);
                NymReferenceNode ref = new NymReferenceNode(name, uri, desc, uriId, groupId, parentGroupId, order, false, false, false);
                //_ui.debugMessage("DB fetch ref: " + groupId + "/" + parentGroupId + ": " + name + "/" + desc + ": " + uri);
                groupIdToNode.put(Long.valueOf(groupId), ref);
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the nym's references", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        // now build the tree out of the nodes
        List<ReferenceNode> roots = new ArrayList();
        for (Iterator iter = groupIdToNode.values().iterator(); iter.hasNext(); ) {
            NymReferenceNode cur = (NymReferenceNode)iter.next();
            long parentId = cur.getParentGroupId();
            if (parentId >= 0) {
                NymReferenceNode parent = (NymReferenceNode)groupIdToNode.get(Long.valueOf(parentId));
                if (parent != null)
                    parent.addChild(cur);
                else
                    roots.add(cur);
            } else {
                roots.add(cur);
            }
        }
        // another pass to sort the children
        for (Iterator iter = groupIdToNode.values().iterator(); iter.hasNext(); ) {
            NymReferenceNode cur = (NymReferenceNode)iter.next();
            cur.sortChildren();
        }
        // sort the roots
        TreeMap<Integer, NymReferenceNode> sorted = new TreeMap();
        for (int i = 0; i < roots.size(); i++) {
            NymReferenceNode cur = (NymReferenceNode)roots.get(i);
            int off = 0;
            while (sorted.containsKey(Integer.valueOf(cur.getSiblingOrder()+off)))
                off++;
            sorted.put(Integer.valueOf(cur.getSiblingOrder()+off), cur);
        }
        roots.clear();
        roots.addAll(sorted.values());
        
        _ui.debugMessage("DB fetched nym refs: " + roots.size());
        return roots;
    }
    
    public Set<Hash> getReferencedScopes(long groupId) {
        HashSet<Hash> scopes = new HashSet();
        if (groupId < 0)
            return scopes;
        List<ReferenceNode> refs = getNymReferences();
        for (int i = 0; i < refs.size(); i++) {
            NymReferenceNode ref = (NymReferenceNode)refs.get(i);
            getReferencedScopes(ref, scopes, groupId, false);
        }
        return scopes;
    }

    /** @param scopes out parameter*/
    private void getReferencedScopes(NymReferenceNode ref, Set<Hash> scopes, long groupId, boolean groupFound) {
        if (ref == null) return;
        if (ref.getGroupId() == groupId)
            groupFound = true;
        int kids = ref.getChildCount();
        if (kids > 0) {
            for (int i = 0; i < kids; i++)
                getReferencedScopes((NymReferenceNode)ref.getChild(i), scopes, groupId, groupFound);
        } else if (groupFound) {
            SyndieURI uri = ref.getURI();
            if (uri != null) {
                if (uri.getScope() != null) {
                    scopes.add(uri.getScope());
                } else if (uri.getSearchScopes() != null) {
                    Hash s[] = uri.getSearchScopes();
                    for (int i = 0; i < s.length; i++)
                        scopes.add(s[i]);
                } else if (uri.getHash("scope") != null) {
                    scopes.add(uri.getHash("scope"));
                }
            }
        }
    }

    private static final String SQL_EXPAND_NYM_REFERENCE_ORDER = "UPDATE resourceGroup SET siblingOrder = siblingOrder + 1 WHERE parentGroupId = ? AND nymId = ? AND siblingOrder >= ?";
    //private static final String SQL_UPDATE_NYM_REFERENCE_ORDER = "UPDATE resourceGroup SET siblingOrder = ? WHERE groupId = ? AND nymId = ?";
    /**
     * make sure the given parent/siblingOrder value is not in use by incrementing the siblingOrder
     * of all equal or greater siblingOrder values
     */
    private void createNymReferenceOrderHole(long nymId, long parentGroupId, int siblingOrder) {
        PreparedStatement stmt = null;
        try {
            stmt = _con.prepareStatement(SQL_EXPAND_NYM_REFERENCE_ORDER);
            stmt.setLong(1, parentGroupId);
            stmt.setLong(2, nymId);
            stmt.setInt(3, siblingOrder);
            int rows = stmt.executeUpdate();
            stmt.close();
            stmt = null;
            if (rows > 0) {
                // ok, some items were reordered, so we need to know by how much, and then contract them
                // todo: contract them
            }
        } catch (SQLException se) {
            log(se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_DELETE_URI = "DELETE FROM uriAttribute WHERE uriId = ?";
    private static final String SQL_UPDATE_NYM_REFERENCE = "UPDATE resourceGroup SET parentGroupId = ?, siblingOrder = ?, name = ?, description = ?, uriId = ? WHERE groupId = ?";
    /** update the reference in the database, keyed off the nymId and newValue's getGroupId() field */
    public void updateNymReference(long nymId, NymReferenceNode newValue) {
        ensureLoggedIn();
        createNymReferenceOrderHole(nymId, newValue.getParentGroupId(), newValue.getSiblingOrder());
        
        long uriId = -1;
        if (newValue.getURI() != null) {
            if (newValue.getURIId() >= 0) {
                // ok, no change
                uriId = newValue.getURIId();
            } else {
                uriId = addURI(newValue.getURI());
                newValue.updateData(newValue.getGroupId(), newValue.getSiblingOrder(), -1);
            }
        } else {
            if (newValue.getURIId() >= 0) {
                try {
                    exec(SQL_DELETE_URI, newValue.getURIId());
                } catch (SQLException se) {
                    log(se);
                }
                newValue.updateData(newValue.getGroupId(), newValue.getSiblingOrder(), -1);
            } else {
                // ok, no change
            }
        }
        
        PreparedStatement stmt = null;
        try {
            stmt = _con.prepareStatement(SQL_UPDATE_NYM_REFERENCE);
            //"parentGroupId = ?, siblingOrder = ?, name = ?, description = ?, uriId = ?
            //WHERE groupId = ?";
            stmt.setLong(1, newValue.getParentGroupId());
            stmt.setInt(2, newValue.getSiblingOrder());
            if (newValue.getName() != null)
                stmt.setString(3, newValue.getName());
            else
                stmt.setNull(3, Types.VARCHAR);
            if (newValue.getDescription() != null)
                stmt.setString(4, newValue.getDescription());
            else
                stmt.setNull(4, Types.VARCHAR);
            if (uriId >= 0)
                stmt.setLong(5, uriId);
            else
                stmt.setNull(5, Types.INTEGER);
            stmt.setLong(6, newValue.getGroupId());
            
            log("updating ref w/ parent=" + newValue.getParentGroupId() + ", sibling=" + newValue.getSiblingOrder() + " groupId=" + newValue.getGroupId());

            int rc = stmt.executeUpdate();
            if (rc == 1) {
                // whee!
            } else {
                // wtf
            }
        } catch (SQLException se) {
            log(se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_ADD_NYM_REFERENCE = "INSERT INTO resourceGroup (groupId, parentGroupId, siblingOrder, name, description, uriId, nymId) VALUES (?, ?, ?, ?, ?, ?, ?)";
    /** add a new reference recursively, then updating the groupId, uriId, and siblingOrder fields in newValue */
    public void addNymReference(long nymId, NymReferenceNode newValue) { addNymReference(nymId, newValue, true); }
    public void addNymReference(long nymId, NymReferenceNode newValue, boolean recurse) {
        ensureLoggedIn();
        if (newValue == null) {
            _ui.debugMessage("add nym ref: abort because it is null");
            return;
        }
        if (!isNewNymReference(nymId, newValue)) {
            _ui.debugMessage("add nym ref: abort because it isn't new: " + newValue);
            return;
        }
        addNymReferenceDetail(nymId, newValue);
        if (recurse) {
            for (int i = 0; i < newValue.getChildCount(); i++) {
                NymReferenceNode child = (NymReferenceNode)newValue.getChild(i);
                child.setParentGroupId(newValue.getGroupId());
                child.setSiblingOrder(i);
                addNymReference(nymId, child);
            }
        }
    }
    
    private static final String SQL_GET_MAX_GROUPID = "SELECT MAX(groupId) FROM resourceGroup WHERE nymId = ?";
    private static final String SQL_GET_MAX_SIBLING = "SELECT MAX(siblingOrder) FROM resourceGroup WHERE nymId = ? AND parentGroupId = ?";

    private void addNymReferenceDetail(long nymId, NymReferenceNode newValue) {
        //createNymReferenceOrderHole(nymId, newValue.getParentGroupId(), newValue.getSiblingOrder());
        
        _ui.debugMessage("add nym ref: adding detail... (id: " + newValue.getGroupId() + ")");
        
        long groupId = newValue.getGroupId();
        if (groupId < 0) {
            PreparedStatement stmt = null;
            ResultSet rs = null;
            try {
                stmt = _con.prepareStatement(SQL_GET_MAX_GROUPID);
                stmt.setLong(1, nymId);
                rs = stmt.executeQuery();
                if (rs.next())
                    groupId = rs.getLong(1) + 1;
                else
                    groupId = 1;
                rs.close();
                rs = null;
                stmt.close();
                stmt = null;
            } catch (SQLException se) {
                log("Error figuring out the new groupId to use", se);
                return;
            } finally {
                if (rs != null) try { rs.close(); } catch (SQLException se) {}
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }
        }
        int siblingOrder = newValue.getSiblingOrder();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MAX_SIBLING);
            stmt.setLong(1, nymId);
            stmt.setLong(2, newValue.getParentGroupId());
            rs = stmt.executeQuery();
            if (rs.next())
                siblingOrder = rs.getInt(1) + 1;
            else
                siblingOrder = 1;
            newValue.setSiblingOrder(siblingOrder);
            rs.close();
            rs = null;
            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            log("Error figuring out the new sibling order to use", se);
            return;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        long uriId = -1;
        if (newValue.getURI() != null)
            uriId = addURI(newValue.getURI());
        
        log("add nym reference [" + groupId + "/" + newValue.getParentGroupId() + "/" + siblingOrder + "/" + newValue.getName() + "/" + newValue.getDescription() + "/" +  uriId + "]: " + newValue.getURI());
        
        try {
            stmt = _con.prepareStatement(SQL_ADD_NYM_REFERENCE);
            // (groupId,parentGroupId,siblingOrder,name,description,uriId,nymId)
            stmt.setLong(1, groupId);
            stmt.setLong(2, newValue.getParentGroupId());
            stmt.setInt(3, siblingOrder);
            if (newValue.getName() != null)
                stmt.setString(4, newValue.getName());
            else
                stmt.setNull(4, Types.VARCHAR);
            if (newValue.getDescription() != null)
                stmt.setString(5, newValue.getDescription());
            else
                stmt.setNull(5, Types.VARCHAR);
            stmt.setLong(6, uriId);
            stmt.setLong(7, nymId);
            
            int rc = stmt.executeUpdate();
            if (rc == 1) {
                newValue.updateData(groupId, siblingOrder, uriId);
            } else {
                // wtf
            }
        } catch (SQLException se) {
            log(se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    private boolean isNewNymReference(long nymId, NymReferenceNode node) {
        long parentId = node.getParentGroupId();
        if ( (parentId < 0) && (node.getParent() != null) )
            parentId = ((NymReferenceNode)node.getParent()).getGroupId();
        
        List<ReferenceNode> siblings = getNymSiblings(nymId, parentId);
        if (node.getURI() == null) {
            String name = node.getName();
            if (name == null) name = "";
            for (int i = 0; i < siblings.size(); i++) {
                NymReferenceNode sib = (NymReferenceNode)siblings.get(i);
                if ( (sib.getURI() == null) && (sib.getName() != null) && (sib.getName().equals(name)) )
                    return false;
            }
            return true;
        } else {
            for (int i = 0; i < siblings.size(); i++) {
                NymReferenceNode sib = (NymReferenceNode)siblings.get(i);
                if ( (sib.getURI() != null) && (sib.getURI().equals(node.getURI())) )
                    return false;
            }
            return true;
        }
    }

    private List<ReferenceNode> getNymSiblings(long nymId, long parentGroupId) {
        List<ReferenceNode> refs = getNymReferences(nymId);
        if (parentGroupId == -1) {
            return refs;
        } else {
            for (int i = 0; i < refs.size(); i++) {
                NymReferenceNode node = (NymReferenceNode)refs.get(i);
                List<ReferenceNode> rv = getNymSiblings(parentGroupId, node);
                if (rv != null)
                    return rv;
            }
        }
        return new ArrayList();
    }

    private List<ReferenceNode> getNymSiblings(long parentGroupId, NymReferenceNode node) {
        if (node.getGroupId() == parentGroupId) {
            List<ReferenceNode> rv = new ArrayList();
            for (int i = 0; i < node.getChildCount(); i++)
                rv.add(node.getChild(i));
            return rv;
        } else {
            for (int i = 0; i < node.getChildCount(); i++) {
                List<ReferenceNode> rv = getNymSiblings(parentGroupId, (NymReferenceNode)node.getChild(i));
                if (rv != null)
                    return rv;
            }
        }
        return null;
    }
    
    private static final String SQL_DELETE_NYM_REFERENCE = "DELETE FROM resourceGroup WHERE groupId = ?";
    private static final String SQL_DELETE_NYM_REFERENCE_URI = "DELETE FROM uriAttribute WHERE uriId IN (SELECT uriId FROM resourceGroup WHERE groupId = ?)";

    /** recursively delete the reference, any children, and any URIs they refer to */
    public void deleteNymReference(long nymId, long groupId) {
        ensureLoggedIn();
        
        ArrayList<Long> groupIdsToDelete = new ArrayList();
        groupIdsToDelete.add(Long.valueOf(groupId));
        while (groupIdsToDelete.size() > 0) {
            Long id = (Long)groupIdsToDelete.remove(0);
            try {
                exec(SQL_DELETE_NYM_REFERENCE_URI, id.longValue());
            } catch (SQLException se) {
                log(se);
            }
            try {
                exec(SQL_DELETE_NYM_REFERENCE, id.longValue());
            } catch (SQLException se) {
                log(se);
            }
            getNymReferenceChildIds(id.longValue(), groupIdsToDelete);
        }
    }
    
    private static final String SQL_GET_NYM_REFERENCE_CHILD_IDS = "SELECT groupId FROM resourceGroup WHERE parentGroupId = ?";

    /** @param addTo out parameter */
    private void getNymReferenceChildIds(long parentGroupId, ArrayList<Long> addTo) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_NYM_REFERENCE_CHILD_IDS);
            stmt.setLong(1, parentGroupId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                long group = rs.getLong(1);
                if (!rs.wasNull()) {
                    Long grp = Long.valueOf(group);
                    if (!addTo.contains(grp))
                        addTo.add(grp);
                }
            }
        } catch (SQLException se) {
            log(se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_DELETE_NYM_REF_URIS = "DELETE FROM uriAttribute WHERE uriId IN (SELECT uriId FROM resourceGroup WHERE nymId = ? AND uriId > -1)";
    private static final String SQL_DELETE_NYM_REFS = "DELETE FROM resourceGroup WHERE nymId = ?";

    public void setNymReferences(List nymRefNodes) { setNymReferences(_nymId, nymRefNodes); }

    public void setNymReferences(long nymId, List nymRefNodes) {
        ensureLoggedIn();
        try { 
            exec(SQL_DELETE_NYM_REF_URIS, nymId); 
            exec(SQL_DELETE_NYM_REFS, nymId); 
        } catch (SQLException se) { 
            log(se); 
        }
        if (nymRefNodes != null)
            ReferenceNode.walk(nymRefNodes, new PersistNymRefs());
    }
    
    private class PersistNymRefs implements ReferenceNode.Visitor {
        public PersistNymRefs() {}
        public void visit(ReferenceNode node, int depth, int siblingOrder) {
            addNymReference(_nymId, (NymReferenceNode)node, false);
        }
    }
    
    
    private static final String SQL_UNSET_REFERENCE_ICON = "DELETE FROM nymCustomIcon WHERE targetType = 1 AND targetId = ?";
    private static final String SQL_SET_REFERENCE_ICON = "INSERT INTO nymCustomIcon (targetType, targetId, data) VALUES (1, ?, ?)";
    /**
     * user-specified avatar overriding the channel's published avatar
     */
    public void setNymReferenceIcon(long groupId, byte avatar[]) {
        ensureLoggedIn();

        log("Setting custom icon for " + groupId + ": " + (avatar != null ? avatar.length : -1));
        
        PreparedStatement stmt = null;
        try {
            exec(SQL_UNSET_REFERENCE_ICON, groupId);
            
            stmt = _con.prepareStatement(SQL_SET_REFERENCE_ICON);
            stmt.setLong(1, groupId);
            stmt.setBytes(2, avatar);
            stmt.executeUpdate();
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error setting the reference avatar", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_REFERENCE_ICON = "SELECT data FROM nymCustomIcon WHERE targetType = 1 AND targetId = ?";
    /**
     * user-specified avatar overriding the channel's published avatar
     */
    public byte[] getNymReferenceIcon(long groupId) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_REFERENCE_ICON);
            stmt.setLong(1, groupId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                byte data[] = rs.getBytes(1);
                return data;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error retrieving the custom reference group icon", se);
            return null;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return null;
    }

    private static final String SQL_GET_REFERENCE_ICON_DEFINED = "SELECT COUNT(targetId) FROM nymCustomIcon WHERE targetType = 1 AND targetId = ?";
    /**
     * returns true if a custom channel avatar has been defined
     */
    public boolean getNymReferenceIconDefined(long groupId) {
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_REFERENCE_ICON_DEFINED);
            stmt.setLong(1, groupId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                return rs.getLong(1) > 0;
            } else {
                return false;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error determining if the custom reference icon is defined", se);
            return false;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    /*
     * CREATE TABLE nymWatchedChannel (
     *        nymId                   INTEGER
     *        , channelId             BIGINT
     *        , importKeys            BOOLEAN
     *        , importBookmarks       BOOLEAN
     *        , importBans            BOOLEAN
     *        , importArchives        BOOLEAN
     *        , highlightUnread       BOOLEAN
     *);
     */
    
    private static final String SQL_GET_WATCHED_CHANNELS = "SELECT channelId, importKeys, importBookmarks, importBans, importArchives, highlightUnread FROM nymWatchedChannel nwc JOIN channel c ON c.channelId = nwc.channelId WHERE nymId = ? ORDER BY UPPER(name) ASC";
    
    /** get a list of WatchedChannel for the nym, ordered by the channel's name */
    public List getWatchedChannels() { return getWatchedChannels(_nymId); }
    public List getWatchedChannels(long nymId) {
        ensureLoggedIn();
        List rv = new ArrayList();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_WATCHED_CHANNELS);
            stmt.setLong(1, nymId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                // channelId, importKeys, importBookmarks, importBans, importArchives, highlightUnread
                long channelId = rs.getLong(1);
                boolean keys = rs.getBoolean(2);
                if (rs.wasNull()) keys = false;
                boolean bookmarks = rs.getBoolean(3);
                if (rs.wasNull()) bookmarks = false;
                boolean bans = rs.getBoolean(4);
                if (rs.wasNull()) bans = false;
                boolean archives = rs.getBoolean(5);
                if (rs.wasNull()) archives = false;
                boolean highlight = rs.getBoolean(6);
                if (rs.wasNull()) highlight = true;
                
                rv.add(new WatchedChannel(channelId, highlight, keys, bookmarks, bans, archives));
            }
        } catch (SQLException se) {
            log(se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return rv;
    }
    
    private static final String SQL_IS_WATCHED = "SELECT COUNT(*) FROM nymWatchedChannel WHERE nymId = ? AND channelId = ?";
    public boolean isWatched(Hash scope) { return isWatched(getChannelId(scope)); }
    public boolean isWatched(long channelId) { return isWatched(_nymId, channelId); }
    public boolean isWatched(long nymId, long channelId) { 
        ensureLoggedIn();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_IS_WATCHED);
            stmt.setLong(1, nymId);
            stmt.setLong(2, channelId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                long count = rs.getLong(1);
                if (count > 0)
                    return true;
            }
            return false;
        } catch (SQLException se) {
            log(se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return false;
    }
    
    
    /*
     * CREATE TABLE nymWatchedChannel (
     *        nymId                   INTEGER
     *        , channelId             BIGINT
     *        , importKeys            BOOLEAN
     *        , importBookmarks       BOOLEAN
     *        , importBans            BOOLEAN
     *        , importArchives        BOOLEAN
     *        , highlightUnread       BOOLEAN
     *);
     */
    private static final String SQL_WATCH_CHANNEL = "INSERT INTO nymWatchedChannel (nymId, channelId, importKeys, importBookmarks, importBans, importArchives, highlightUnread) VALUES (?, ?, ?, ?, ?, ?, ?)";

    public void watchChannel(Hash scope, boolean highlight, boolean impArchives, boolean impBookmarks, boolean impBans, boolean impKeys) {
        watchChannel(_nymId, scope, highlight, impArchives, impBookmarks, impBans, impKeys);
    }

    public void watchChannel(long nymId, Hash scope, boolean highlight, boolean impArchives, boolean impBookmarks, boolean impBans, boolean impKeys) {
        ensureLoggedIn();
        long channelId = getChannelId(scope);
        watchChannel(nymId, channelId, highlight, impArchives, impBookmarks, impBans, impKeys);
    }

    public void watchChannel(long channelId, boolean highlight, boolean impArchives, boolean impBookmarks, boolean impBans, boolean impKeys) {
        watchChannel(_nymId, channelId, highlight, impArchives, impBookmarks, impBans, impKeys);
    }

    public void watchChannel(long nymId, long channelId, boolean highlight, boolean impArchives, boolean impBookmarks, boolean impBans, boolean impKeys) {
        ensureLoggedIn();
        if (channelId < 0) return;
        
        unwatchChannel(nymId, channelId, false);
        log("watch channel " + channelId + " for nym " + nymId);
        
        PreparedStatement stmt = null;
        try {
            stmt = _con.prepareStatement(SQL_WATCH_CHANNEL);
            // nymId, channelId, importKeys, importBookmarks, importBans, importArchives, highlightUnread
            stmt.setLong(1, nymId);
            stmt.setLong(2, channelId);
            stmt.setBoolean(3, impKeys);
            stmt.setBoolean(4, impBookmarks);
            stmt.setBoolean(5, impBans);
            stmt.setBoolean(6, impArchives);
            stmt.setBoolean(7, highlight);
            stmt.executeUpdate();
        } catch (SQLException se) {
            log(se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        notifyWatchListeners();
    }
    
    private static final String SQL_UNWATCH_CHANNEL = "DELETE FROM nymWatchedChannel WHERE nymId = ? AND channelId = ?";

    public void unwatchChannel(Hash scope) { unwatchChannel(_nymId, scope, true); }

    public void unwatchChannel(long nymId, Hash scope) { unwatchChannel(nymId, scope, true); }

    public void unwatchChannel(WatchedChannel channel) { unwatchChannel(_nymId, channel.getChannelId(), true); }

    public void unwatchChannels(WatchedChannel channels[]) {
        if ( (channels != null) && (channels.length > 0) ) {
            for (int i = 0; i < channels.length; i++)
                unwatchChannel(_nymId, channels[i].getChannelId(), false);
            notifyWatchListeners();
        }
    }
    private void unwatchChannel(long nymId, Hash scope, boolean notifyListeners) { 
        long channelId = getChannelId(scope);
        if (channelId < 0) return;
        unwatchChannel(nymId, channelId, notifyListeners);
    }

    private void unwatchChannel(long nymId, long channelId, boolean notifyListeners) { 
        PreparedStatement stmt = null;
        try {
            stmt = _con.prepareStatement(SQL_UNWATCH_CHANNEL);
            stmt.setLong(1, nymId);
            stmt.setLong(2, channelId);
            int rows = stmt.executeUpdate();
            log("unwatch channel " + channelId + " for nym " + nymId + ": " + rows);
        } catch (SQLException se) {
            log(se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        if (notifyListeners)
            notifyWatchListeners();
    }

    private void notifyWatchListeners() {
        List toNotify = new ArrayList();
        synchronized (_watchListeners) { toNotify.addAll(_watchListeners); }
        for (int i = 0; i < toNotify.size(); i++)
            ((WatchEventListener)toNotify.get(i)).watchesUpdated();
    }
    
    public interface WatchEventListener {
        public void watchesUpdated();
    }

    private List _watchListeners = new ArrayList();

    public void addWatchEventListener(WatchEventListener lsnr) { 
        synchronized (_watchListeners) { _watchListeners.add(lsnr); }
    }

    public void removeWatchEventListener(WatchEventListener lsnr) {
        synchronized (_watchListeners) { _watchListeners.remove(lsnr); }
    }
    
    public interface MessageStatusListener {
        public void messageStatusUpdated(long msgId, int newStatus);
    }

    private List _msgStatusListeners = new ArrayList();

    public void addMessageStatusListener(MessageStatusListener lsnr) { 
        synchronized (_msgStatusListeners) { _msgStatusListeners.add(lsnr); }
    }

    public void removeMessageStatusListener(MessageStatusListener lsnr) {
        synchronized (_msgStatusListeners) { _msgStatusListeners.remove(lsnr); }
    }

    private void notifyMessageStatusListeners(long msgId, int newStatus) {
        List toNotify = new ArrayList();
        synchronized (_msgStatusListeners) { toNotify.addAll(_msgStatusListeners); }
        _ui.debugMessage("notifyMessageStatus(" + msgId + ", " + newStatus + "): listener count = " + toNotify.size());
        for (int i = 0; i < toNotify.size(); i++)
            ((MessageStatusListener)toNotify.get(i)).messageStatusUpdated(msgId, newStatus);
    }
    
    /**
     *  TODO, ensureLoggedIn() may still throw an ISE even if isLoggedIn() returns true
     */
    private void ensureLoggedIn() throws IllegalStateException { ensureLoggedIn(true); }

    /**
     *  TODO, ensureLoggedIn() may still throw an ISE even if isLoggedIn() returns true
     */
    private void ensureLoggedIn(boolean verifyNymId) throws IllegalStateException {
        try {
            if ( (_con != null) && (!_con.isClosed()) && (_nymId >= 0 || !verifyNymId) )
                return;
        } catch (SQLException se) {
            // problem detecting isClosed?
            log(se);
        }
        throw new IllegalStateException("Not logged in");
    }

    /**
     *  Online backup, MUST be connected
     *  @param out /path/to/zipfile
     *  @param includeArchive true unsupported
     */
    void backup(UI ui, String out, boolean includeArchive) {
        String dbFileRoot = getDBFileRoot();
        if (dbFileRoot == null) {
            ui.errorMessage("Unable to determine the database file root.  Is this a HSQLDB file URL?");
            ui.commandComplete(-1, null);
            return;
        }
        ui.debugMessage("Backing up the database from " + dbFileRoot + " to " + out);
        try {
            if (!isLoggedIn())
                throw new SQLException("not logged in");
            exec("CHECKPOINT");
        } catch (SQLException se) {
            ui.errorMessage("Error halting the database to back it up!", se);
            ui.commandComplete(-1, null);
            return;
        }
        List<String> suffixes = new ArrayList(4);
        suffixes.add(".properties");
        suffixes.add(".script");
        if (DBUpgrade.isHsqldb20(_con))
            suffixes.add(".data");
        else
            suffixes.add(".backup");
        try {
            backup(dbFileRoot, "db", suffixes, out);
            if (includeArchive)
                backupArchive(ui, null);
            ui.statusMessage("Database backed up to " + out);
            ui.commandComplete(0, null);
        } catch (IOException ioe) {
            ui.errorMessage("Error backing up the database", ioe);
            ui.commandComplete(-1, null);
        }
    }

    /**
     *  Offline backup, must NOT be connected
     *  @param dbFileRoot /path/to/.syndie/db/syndie (i.e. without the .data suffix)
     *  @param out /path/to/zipfile
     *  @since 1.104b-2
     */
    static void offlineBackup(String dbFileRoot, String out) throws IOException {
        List<String> suffixes = new ArrayList(4);
        suffixes.add(".properties");
        suffixes.add(".script");
        suffixes.add(".data");
        backup(dbFileRoot, "db", suffixes, out);
    }

    /**
     *  Do the backup
     *  @param dbFileRoot /path/to/.syndie/db/syndie (i.e. without the .data suffix)
     *  @param out /path/to/zipfile
     *  @since 1.104b-2
     */
    private static void backup(String dbFileRoot, String prefix, List<String> suffixes, String out) throws IOException {
        ZipOutputStream zos = new ZipOutputStream(new SecureFileOutputStream(out));
        try {
            for (String suffix : suffixes) {
                ZipEntry entry = new ZipEntry(prefix + suffix);
                File f = new File(dbFileRoot + suffix);
                entry.setSize(f.length());
                entry.setTime(f.lastModified());
                zos.putNextEntry(entry);
                copy(f, zos);
                zos.closeEntry();
            }  
        } finally {
            zos.finish();
            zos.close();
        }
    }
    
    /** just spits out a message */
    private void backupArchive(UI ui, ZipOutputStream out) throws IOException {
        ui.errorMessage("Backing up the archive is not yet supported.");
        ui.errorMessage("However, you can just, erm, tar cjvf the $data/archive/ dir");
    }
    
    /** @return /path/to/.syndie/db/syndie (i.e. without the .data suffix) */
    private String getDBFileRoot() { return getDBFileRoot(_url); }

    /** @return /path/to/.syndie/db/syndie (i.e. without the .data suffix) */
    private static String getDBFileRoot(String url) {
        if (url.startsWith("jdbc:hsqldb:file:")) {
            String file = url.substring("jdbc:hsqldb:file:".length());
            int end = file.indexOf(";");
            if (end != -1)
                file = file.substring(0, end);
            return file;
        } else {
            return null;
        }
    }
    
    private static void copy(File in, OutputStream out) throws IOException {
        byte buf[] = new byte[4096];
        InputStream fis = null;
        try {
            fis = new BufferedInputStream(new FileInputStream(in));
            int read = -1;
            while ( (read = fis.read(buf)) != -1)
                out.write(buf, 0, read);
            fis.close();
            fis = null;
        } finally {
            if (fis != null) fis.close();
        }
    }

    /**
     * @param in zip archive containing db.{properties,script,backup,data}
     *           to be extracted onto the given db
     * @param db JDBC url (but it must be an HSQLDB file URL).  If the database
     *           already exists (and is of a nonzero size), it will NOT be
     *           overwritten
     */
    void restore(UI ui, String in, String db) {
        File inFile = new File(in);
        if ( (!inFile.exists()) || (inFile.length() <= 0) ) {
            ui.errorMessage("Database backup does not exist: " + inFile.getPath());
            ui.commandComplete(-1, null);
            return;
        }
        
        String root = getDBFileRoot(db);
        if (root == null) {
            ui.errorMessage("Database restoration is only possible with file urls");
            ui.commandComplete(-1, null);
            return;
        }
        File prop = new File(root + ".properties");
        File script = new File(root + ".script");
        File backup = new File(root + ".backup");
        File data = new File(root + ".data");
        if ( (prop.exists() && (prop.length() > 0)) ||
             (script.exists() && (script.length() > 0)) ||
             (backup.exists() && (backup.length() > 0)) ||
             (data.exists() && (data.length() > 0)) ) {
            ui.errorMessage("Not overwriting existing non-empty database files: ");
            ui.errorMessage(prop.getPath());
            ui.errorMessage(script.getPath());
            ui.errorMessage(backup.getPath());
            ui.errorMessage(data.getPath());
            ui.errorMessage("If they are corrupt or you really want to replace them,");
            ui.errorMessage("delete them first, then rerun the restore command");
            ui.commandComplete(-1, null);
            return;
        }

        String url = _url;
        String login = _login;
        String pass = _pass;
        long nymId = _nymId;
        
        if (_con != null) {
            ui.statusMessage("Disconnecting from the database to restore...");
            close();
        }
        
        ui.statusMessage("Restoring the database from " + in + " to " + root);
        
        try {
            ZipInputStream zis = new ZipInputStream(new FileInputStream(in));
            
            while (true) {
                ZipEntry entry = zis.getNextEntry();
                if (entry == null)
                    break;
                String name = entry.getName();
                if ("db.properties".equals(name)) {
                    copy(zis, prop);
                } else if ("db.script".equals(name)) {
                    copy(zis, script);
                } else if ("db.backup".equals(name)) {
                    copy(zis, backup);
                } else if ("db.data".equals(name)) {
                    copy(zis, data);
                } else {
                    ui.debugMessage("Ignoring backed up file " + name + " for now");
                }
            }
            
            zis.close();
            
            ui.statusMessage("Database restored from " + in);
            
            if ( (url != null) && (login != null) && (pass != null) ) {
                ui.statusMessage("Reconnecting to the database");
                try {
                    connect(url, login, pass);
                } catch (SQLException se) {
                    ui.errorMessage("Not able to log back into the database", se);
                }
            }
            ui.commandComplete(0, null);
        } catch (IOException ioe) {
            ui.errorMessage("Error backing up the database", ioe);
            ui.commandComplete(-1, null);
        }
    }
    
    private void copy(InputStream in, File out) throws IOException {
        byte buf[] = new byte[4096];
        FileOutputStream fos = null;
        try {
            fos = new SecureFileOutputStream(out);
            int read = -1;
            while ( (read = in.read(buf)) != -1)
                fos.write(buf, 0, read);
            fos.close();
            fos = null;
        } finally {
            if (fos != null) fos.close();
        }
    }

    private static final String SQL_GET_ALIASES = "SELECT aliasName, aliasValue FROM nymCommandAlias WHERE nymId = ? ORDER BY aliasName ASC";

    /** map of command name (String) to command line (String) */
    public Map<String, String> getAliases(long nymId) {
        TreeMap<String, String> rv = new TreeMap();
        if (!isLoggedIn()) return rv;
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_ALIASES);
            stmt.setLong(1, nymId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                String name = (String)rs.getString(1);
                String value = rs.getString(2);
                if ( (name != null) && (value != null) && (name.length() > 0) )
                    rv.put(name, value);
            }
            rs.close();
            rs = null;
            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error fetching aliases", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return rv;
    }

    private static final String SQL_DELETE_ALIAS = "DELETE FROM nymCommandAlias WHERE nymId = ? AND aliasName = ?";
    private static final String SQL_ADD_ALIAS = "INSERT INTO nymCommandAlias (nymId, aliasName, aliasValue) VALUES (?, ?, ?)";

    public void addAlias(long nymId, String name, String value) {
        PreparedStatement stmt = null;
        try {
            stmt = _con.prepareStatement(SQL_DELETE_ALIAS);
            stmt.setLong(1, nymId);
            stmt.setString(2, name);
            stmt.executeUpdate();
            stmt.close();
            
            if ( (value != null) && (value.length() > 0) ) {
                stmt = _con.prepareStatement(SQL_ADD_ALIAS);
                stmt.setLong(1, nymId);
                stmt.setString(2, name);
                stmt.setString(3, value);
                stmt.executeUpdate();
                stmt.close();
            }
            stmt = null;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error updating alias", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    public Properties getDefaultPrefs() { return readScriptProps("defaultprefs"); }
    public Properties getDefaultAliases() { return readScriptProps("defaultaliases"); }
    private Properties readScriptProps(String propName) {
        Properties rv = new Properties();
        File scriptDir = new File(_rootDir, "scripts");
        File scriptFile = new File(scriptDir, propName);
        if (scriptFile.exists()) {
            BufferedReader in = null;
            try {
                in = new BufferedReader(new InputStreamReader(new FileInputStream(scriptFile), "UTF-8"));
                String line = null;
                while ( (line = in.readLine()) != null) {
                    int split = line.indexOf('=');
                    if ( (split <= 0) || (split >= line.length()) )
                        continue;
                    String name = line.substring(0, split).trim();
                    String val = line.substring(split+1).trim();
                    if (name.length() <= 0) continue;
                    if (name.startsWith("//") || (name.startsWith("--")) || (name.startsWith("#"))) continue;
                    rv.setProperty(name, val);
                }
                in.close();
                in = null;
            } catch (UnsupportedEncodingException uee) {
                //ui.errorMessage("internal error, your JVM doesn't support UTF-8?", uee);
            } catch (IOException ioe) {
                //ignore
            } finally {
                if (in != null) try { in.close(); } catch (IOException ioe) {}
            }
        }
        return rv;
    }

    /** the nym has previously marked all messages through this one as being read */
    public static final int MSG_STATUS_READ = 1;
    /** the message hasn't been read */
    public static final int MSG_STATUS_UNREAD = 3;

    private static final String SQL_GET_MSG_STATUS = "SELECT msgId FROM nymUnreadMessage WHERE nymId = ? AND msgId = ?";

    /**
     *  @return MSG_STATUS_READ (1) or MSG_STATUS_UNREAD (3)
     */
    /**
     *  @return MSG_STATUS_READ (1) or MSG_STATUS_UNREAD (3)
     */
    public int getMessageStatus(long msgId) { return getMessageStatus(msgId, -1); }

    /**
     *  @return MSG_STATUS_READ (1) or MSG_STATUS_UNREAD (3)
     */
    public int getMessageStatus(long msgId, long targetChanId) { return getMessageStatus(_nymId, msgId, targetChanId); }

    public int getMessageStatus(long nymId, long msgId, long targetChanId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MSG_STATUS);
            stmt.setLong(1, nymId);
            stmt.setLong(2, msgId);
            rs = stmt.executeQuery();
            
            if (rs.next()) {
                return MSG_STATUS_UNREAD;
            } else {
                return MSG_STATUS_READ;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error getting message status", se);
            return MSG_STATUS_UNREAD;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    private static final String SQL_GET_MSG_READ = 
            "SELECT msgId FROM nymUnreadMessage WHERE nymId = ? AND msgId IN (";

    /** get a list of msgIds (Long) from the given set who have already been read */
    public List<Long> getUnread(long msgIds[]) { return getUnread(_nymId, msgIds); }

    public List<Long> getUnread(long nymId, long msgIds[]) {
        long begin = System.currentTimeMillis();
        List<Long> rv = new ArrayList();
        StringBuilder buf = new StringBuilder(SQL_GET_MSG_READ);
        for (int i = 0; i < msgIds.length; i++) {
            buf.append(msgIds[i]);
            if (i+1 < msgIds.length)
                buf.append(", ");
        }
        
        buf.append(")");
        String query = buf.toString();
        //_ui.debugMessage("getUnread query: [" + nymId + "]: " + query);
        
        long beforePrep = System.currentTimeMillis();
        long afterPrep = -1;
        long afterExec = -1;
        
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(query);
            afterPrep = System.currentTimeMillis();
            stmt.setLong(1, nymId);
            rs = stmt.executeQuery();
            afterExec = System.currentTimeMillis();
            
            while (rs.next()) {
                long msgId = rs.getLong(1);
                if (!rs.wasNull())
                    rv.add(Long.valueOf(msgId));
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error getting read messages from the list", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        long afterMatch = System.currentTimeMillis();
        log("getUnread in bulk took " + (afterMatch-begin) + "/" +(afterMatch-afterExec)
                         + "/" + (afterExec-afterPrep) + "/" + (afterPrep-beforePrep) 
                         + ": found matches: " + rv.size() + "/" + msgIds.length);
        return rv;
    }
    
    private static final String SQL_GET_MESSAGE_DECRYPTED = "SELECT true FROM channelMessage WHERE msgId = ? AND readKeyMissing = FALSE AND replyKeyMissing = FALSE AND pbePrompt IS NULL";

    public boolean getMessageDecrypted(long msgId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_DECRYPTED);
            stmt.setLong(1, msgId);
            rs = stmt.executeQuery();
            
            if (rs.next()) {
                boolean rv = rs.getBoolean(1);
                if (rs.wasNull())
                    rv = false;
                return rv;
            } else {
                return false;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error determining if the message was decrypted", se);
            return false;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }        
    }
    
    private static final String SQL_GET_MESSAGE_DELETED = "SELECT true FROM channelMessage WHERE msgId = ? AND deletionCause IS NOT NULL";

    public boolean getMessageDeleted(long msgId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_DELETED);
            stmt.setLong(1, msgId);
            rs = stmt.executeQuery();
            
            if (rs.next()) {
                boolean rv = rs.getBoolean(1);
                if (rs.wasNull())
                    rv = false;
                return rv;
            } else {
                return false;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error determining if the message was deleted", se);
            return false;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }        
    }
    
    private static final String SQL_GET_MESSAGE_TARGET = "SELECT targetChannelId FROM channelMessage WHERE msgId = ?";
    public long getMessageTarget(long msgId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_MESSAGE_TARGET);
            stmt.setLong(1, msgId);
            rs = stmt.executeQuery();
            
            if (rs.next()) {
                long chanId = rs.getLong(1);
                if (!rs.wasNull())
                    return chanId;
                else
                    return -1;
            } else {
                return -1;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error getting message target", se);
            return -1;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    private static final String SQL_MARK_MESSAGE_READ = "DELETE FROM nymUnreadMessage WHERE nymId = ? AND msgId = ?";
    public void markMessageRead(long msgId) { markMessageRead(_nymId, msgId); }
    public void markMessageRead(long nymId, long msgId) {
        PreparedStatement stmt = null;
        try {
            stmt = _con.prepareStatement(SQL_MARK_MESSAGE_READ);
            stmt.setLong(1, nymId);
            stmt.setLong(2, msgId);
            stmt.executeUpdate();
            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error marking message read", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        // when we read a message, consider that we've "seen" the new forum
        long chanId = getMessageTarget(msgId);
        if (chanId >= 0)
            markChannelNotNew(chanId);
        
        notifyMessageStatusListeners(msgId, DBClient.MSG_STATUS_READ);
    }

    private static final String SQL_MARK_MESSAGE_UNREAD = "INSERT INTO nymUnreadMessage (nymId, msgId) VALUES (?, ?)";
    public void markMessageUnread(long msgId) { markMessageUnread(_nymId, msgId); }
    public void markMessageUnread(long nymId, long msgId) {
        markMessageRead(nymId, msgId); // delete then we insert below
        PreparedStatement stmt = null;
        try {
            stmt = _con.prepareStatement(SQL_MARK_MESSAGE_UNREAD);
            stmt.setLong(1, nymId);
            stmt.setLong(2, msgId);
            stmt.executeUpdate();
            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error marking message unread", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        notifyMessageStatusListeners(msgId, DBClient.MSG_STATUS_UNREAD);
    }

    private static final String SQL_MARK_CHANNELMSG_READ = "DELETE FROM nymUnreadMessage WHERE nymId = ? AND msgId IN (SELECT msgId FROM channelMessage WHERE targetChannelId = ?)";
    public void markChannelRead(long chanId) { markChannelRead(_nymId, chanId); }
    public void markChannelRead(long nymId, long chanId) {
        PreparedStatement stmt = null;
        try {
            stmt = _con.prepareStatement(SQL_MARK_CHANNELMSG_READ);
            stmt.setLong(1, nymId);
            stmt.setLong(2, chanId);
            stmt.executeUpdate();
            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error marking message read", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        markChannelNotNew(nymId, chanId);
    }
    
    private static final String SQL_MARK_CHANNEL_READ = "DELETE FROM nymUnreadChannel WHERE nymId = ? AND channelId = ?";
    public void markChannelNotNew(long chanId) { markChannelNotNew(_nymId, chanId); }
    public void markChannelNotNew(long nymId, long chanId) {
        PreparedStatement stmt = null;
        try {
            stmt = _con.prepareStatement(SQL_MARK_CHANNEL_READ);
            stmt.setLong(1, nymId);
            stmt.setLong(2, chanId);
            stmt.executeUpdate();
            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error marking channel read", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_COUNT_MESSAGES = "SELECT COUNT(msgId) FROM channelMessage WHERE targetChannelId = ? AND isCancelled = FALSE AND readKeyMissing = FALSE AND replyKeyMissing = FALSE AND pbePrompt IS NULL AND deletionCause IS NULL";
    public int countMessages(long chanId) { return countMessages(_nymId, chanId); }
    public int countMessages(long nymId, long chanId) { 
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_COUNT_MESSAGES);
            stmt.setLong(1, chanId);
            rs = stmt.executeQuery();
            
            if (rs.next()) {
                int count = rs.getInt(1);
                if (rs.wasNull())
                    return 0;
                else
                    return count;
            } else {
                return 0;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error getting message count", se);
            return 0;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    private static final String SQL_GET_LASTPOST_DATE = "SELECT MAX(importDate) FROM channelMessage WHERE targetChannelId = ? AND isCancelled = FALSE AND readKeyMissing = FALSE AND replyKeyMissing = FALSE AND pbePrompt IS NULL AND deletionCause IS NULL";
    public long getChannelLastPost(long chanId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_LASTPOST_DATE);
            stmt.setLong(1, chanId);
            rs = stmt.executeQuery();
            
            if (rs.next()) {
                Date when = rs.getDate(1);
                if (when == null)
                    return 0;
                else
                    return when.getTime();
            } else {
                return 0;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error getting last post date", se);
            return 0;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_COUNT_UNREAD_MESSAGES = "SELECT COUNT(msgId) FROM nymUnreadMessage num JOIN channelMessage cm ON num.msgId = cm.msgId WHERE nymId = ? AND targetChannelId = ? AND cm.readKeyMissing = FALSE AND cm.replyKeyMissing = FALSE AND cm.pbePrompt IS NULL AND deletionCause IS NULL";
    public int countUnreadMessages(Hash scope) { return countUnreadMessages(_nymId, scope); }
    public int countUnreadMessages(long nymId, Hash scope) { return countUnreadMessages(nymId, getChannelId(scope)); }
    public int countUnreadMessages(long channelId) { return countUnreadMessages(_nymId, channelId); }
    public int countUnreadMessages(long nymId, long chan) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_COUNT_UNREAD_MESSAGES);
            stmt.setLong(1, nymId);
            stmt.setLong(2, chan);
            rs = stmt.executeQuery();
            
            if (rs.next()) {
                int count = rs.getInt(1);
                if (rs.wasNull())
                    return 0;
                else
                    return count;
            } else {
                return 0;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error getting unread message count", se);
            return 0;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    
    private static final String SQL_COUNT_UNREAD_PRIVATE_MESSAGES = "SELECT COUNT(msgId) FROM nymUnreadMessage num JOIN channelMessage cm ON num.msgId = cm.msgId WHERE nymId = ? AND cm.wasPrivate = true AND targetChannelId = ? AND cm.readKeyMissing = FALSE AND cm.replyKeyMissing = FALSE AND cm.pbePrompt IS NULL AND deletionCause IS NULL";
    private static final String SQL_COUNT_PRIVATE_MESSAGES = "SELECT COUNT(msgId) FROM channelMessage cm WHERE cm.wasPrivate = true AND targetChannelId = ? AND cm.readKeyMissing = FALSE AND cm.replyKeyMissing = FALSE AND cm.pbePrompt IS NULL AND deletionCause IS NULL";
    public int countPrivateMessages(long chan, boolean unreadOnly) { return countPrivateMessages(_nymId, chan, unreadOnly); }
    public int countPrivateMessages(long nymId, long chan, boolean unreadOnly) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            if (unreadOnly) {
                stmt = _con.prepareStatement(SQL_COUNT_UNREAD_PRIVATE_MESSAGES);
                stmt.setLong(1, nymId);
                stmt.setLong(2, chan);
            } else {
                stmt = _con.prepareStatement(SQL_COUNT_PRIVATE_MESSAGES);
                stmt.setLong(1, chan);
            }
            rs = stmt.executeQuery();
            
            if (rs.next()) {
                int count = rs.getInt(1);
                if (rs.wasNull())
                    return 0;
                else
                    return count;
            } else {
                return 0;
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error getting private message count", se);
            return 0;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_NEW_CHANNEL_IDS = "SELECT channelId FROM nymUnreadChannel WHERE nymId = ?";
    /** channels may have been deleted (banned), so drop 'em */
    private static final String SQL_DELETE_REMOVED_CHANNELS = "DELETE FROM nymUnreadChannel WHERE nymId = ? AND channelId NOT IN (SELECT channelId FROM CHANNEL)";

    /** forums that haven't been marked as read */
    public List<Long> getNewChannelIds() { return getNewChannelIds(_nymId); }

    public List<Long> getNewChannelIds(long nymId) {
        List<Long> rv = new ArrayList();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            exec(SQL_DELETE_REMOVED_CHANNELS, nymId);
            stmt = _con.prepareStatement(SQL_GET_NEW_CHANNEL_IDS);
            stmt.setLong(1, nymId);
            rs = stmt.executeQuery();
            
            while (rs.next()) {
                long chanId = rs.getLong(1);
                if (!rs.wasNull())
                    rv.add(Long.valueOf(chanId));
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error getting new channel ids", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return rv;
    }

    private static final String SQL_LIST_RESUMEABLE = "SELECT postponeId, MAX(postponeVersion) FROM nymMsgPostpone WHERE nymId = ? GROUP BY postponeId";

    /**
     * ordered map of postponeId (Long) to the most recent version (Integer),
     * with the most recent messages first 
     */
    public TreeMap<Long, Integer> getResumeable() {
        TreeMap<Long, Integer> postponeIdToVersion = new TreeMap(INVERSE_COMPARATOR);
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = con().prepareStatement(SQL_LIST_RESUMEABLE);
            stmt.setLong(1, _nymId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                long id = rs.getLong(1);
                int ver = rs.getInt(2);
                postponeIdToVersion.put(Long.valueOf(id), Integer.valueOf(ver));
            }
        } catch (SQLException se) {
            log("Internal eror populating resumeable list", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return postponeIdToVersion;
    }

    private static final Comparator INVERSE_COMPARATOR = new Comparator() {
        public int compare(Object o1, Object o2) { return ((Comparable)o2).compareTo(o1); }
        //public boolean equals(Object obj) { return obj == INVERSE_COMPARATOR; }
    };

    public boolean reimport(SyndieURI uri, String passphrase) {
        if ( (uri == null) || (uri.getScope() == null) ) return false;
        File dir = new File(getArchiveDir(), uri.getScope().toBase64());
        File msgFile = null;
        if (uri.getMessageId() != null)
            msgFile = new File(dir, uri.getMessageId().longValue() + Constants.FILENAME_SUFFIX);
        else
            msgFile = new File(dir, "meta" + Constants.FILENAME_SUFFIX);
        Importer imp = new Importer(this, passphrase);
        FileInputStream fin = null;
        try {
            fin = new FileInputStream(msgFile);
            ImportResult.Result result = imp.processMessage(_ui, this, fin, passphrase, true, null, null);
            fin.close();
            fin = null;
            log("reimport ok? " + result + ": " + uri);
            // wasPBE is still true if the post *was* pbe'd but the passphrase was correct.
            // wasMissingKey is true if the post was valid and imported successfully, but we don't know how to read it
            boolean rv = result.ok() &&
                         result != IMPORT_UNREADABLE &&
                         result != IMPORT_NO_READ_KEY &&
                         result != IMPORT_NO_REPLY_KEY;
            // the Importer should take care of reimporting messages with the new read keys
            //if (uri.getMessageId() == null)
            //    metaImported();
            //else
            //    messageImported();
            return rv;
        } catch (IOException ioe) {
            log("Error reimporting " + uri, ioe);
            return false;
        } finally {
            if (fin != null) try { fin.close(); } catch (IOException ioe) {}
        }
    }

    private static final String SQL_GET_EXPIRATION_POLICIES = "SELECT isDataFilePolicy, policyScopeId, maxNumMessages, maxSizeKB, maxAgeDays, mimicDefault FROM expirationPolicy";

    /**
     *  @return Should contain default policies, but may not, see fix in Expirer.loadPolicies()
     */
    public Set getExpirationPolicies() {
        Set rv = new HashSet();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_EXPIRATION_POLICIES);
            rs = stmt.executeQuery();
            
            while (rs.next()) {
                // isDataFilePolicy, policyScopeId, maxNumMessages, maxSizeKB, maxAgeDays, mimicDefault
                boolean isDataFile = rs.getBoolean(1);
                long scopeId = rs.getLong(2);
                long msgs = rs.getLong(3);
                long maxSizeKB = rs.getLong(4);
                long maxAgeDays = rs.getLong(5);
                boolean mimicDefault = rs.getBoolean(6);
                if (rs.wasNull()) mimicDefault = false;
                
                ExpirationPolicy policy = new ExpirationPolicy();
                if (isDataFile)
                    policy.setIsDataFilePolicy(); 
                else
                    policy.setIsDBPolicy();
                if (scopeId == -1)
                    policy.setIsDefaultPolicy();
                else if (scopeId == -2)
                    policy.setIsWatchedPolicy();
                else
                    policy.setPolicyChannelId(scopeId);
                policy.setMaxNumMessages(msgs);
                policy.setMaxSizeKB((int)maxSizeKB);
                policy.setMaxAgeDays((int)maxAgeDays);
                policy.setMimicDefault(mimicDefault);
                rv.add(policy);
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error getting expiration policies", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        return rv;
    }
    
    private static final String SQL_ADD_EXPIRATION_POLICY = "INSERT INTO expirationPolicy (isDataFilePolicy, policyScopeId, maxNumMessages, maxSizeKB, maxAgeDays, mimicDefault) VALUES (?, ?, ?, ?, ?, ?)";
    private static final String SQL_UPDATE_EXPIRATION_POLICY = "UPDATE expirationPolicy SET maxNumMessages = ?, maxSizeKB = ?, maxAgeDays = ?, mimicDefault = ? WHERE isDataFilePolicy = ? AND policyScopeId = ?";
    public void saveExpirationPolicy(ExpirationPolicy policy) {
        boolean isDataFile = false;
        long policyScopeId = 0;
        long maxNumMessages = 0;
        int maxSizeKB = 0;
        int maxAgeDays = 0;
        boolean mimicDefault = policy.getMimicDefault();

        isDataFile = policy.isDataFilePolicy();
        
        if (mimicDefault) {
            maxNumMessages = -1;
            maxSizeKB = -1;
            maxAgeDays = -1;
        } else {
            maxNumMessages = policy.getMaxNumMessages();
            maxSizeKB = policy.getMaxSizeKB();
            maxAgeDays = policy.getMaxAgeDays();
        }

        if (policy.isDefaultPolicy())
            policyScopeId = -1;
        else if (policy.isWatchedPolicy())
            policyScopeId = -2;
        else
            policyScopeId = policy.getPolicyChannelId();

        if (policy.getIsNew()) {
            _ui.debugMessage("add new policy: " + isDataFile + "/" + policyScopeId + "/" + maxNumMessages + "/" + maxSizeKB + "/" + maxAgeDays);
            PreparedStatement stmt = null;
            try {
                stmt = _con.prepareStatement(SQL_ADD_EXPIRATION_POLICY);
                //isDataFilePolicy, policyScopeId, maxNumMessages, maxSizeKB, maxAgeDays
                stmt.setBoolean(1, isDataFile);
                stmt.setLong(2, policyScopeId);
                stmt.setLong(3, maxNumMessages);
                stmt.setInt(4, maxSizeKB);
                stmt.setInt(5, maxAgeDays);
                stmt.setBoolean(6, mimicDefault);
                stmt.executeUpdate();
            } catch (SQLException se) {
                if (_log.shouldLog(Log.WARN))
                    _log.warn("Error adding expiration policy", se);
            } finally {
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }
        } else {
            _ui.debugMessage("update policy: " + isDataFile + "/" + policyScopeId + "/" + maxNumMessages + "/" + maxSizeKB + "/" + maxAgeDays);
            PreparedStatement stmt = null;
            try {
                stmt = _con.prepareStatement(SQL_UPDATE_EXPIRATION_POLICY);
                //maxNumMessages = ?, maxSizeKB = ?, maxAgeDays = ? WHERE isDataFilePolicy = ? AND policyScopeId = ?
                stmt.setLong(1, maxNumMessages);
                stmt.setInt(2, maxSizeKB);
                stmt.setInt(3, maxAgeDays);
                stmt.setBoolean(4, mimicDefault);
                stmt.setBoolean(5, isDataFile);
                stmt.setLong(6, policyScopeId);
                stmt.executeUpdate();
            } catch (SQLException se) {
                if (_log.shouldLog(Log.WARN))
                    _log.warn("Error updating expiration policy", se);
            } finally {
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }
        }
    }

    private static final String SQL_DELETE_EXPIRATION_POLICY = "DELETE FROM expirationPolicy WHERE isDataFilePolicy = ? AND policyScopeId = ?";
    public void deleteExpirationPolicy(ExpirationPolicy policy) {
        if (policy.getIsNew()) return; // noop
        
        long policyScopeId = 0;
        if (policy.isDefaultPolicy())
            policyScopeId = -1;
        else if (policy.isWatchedPolicy())
            policyScopeId = -2;
        else
            policyScopeId = policy.getPolicyChannelId();

        PreparedStatement stmt = null;
        try {
            stmt = _con.prepareStatement(SQL_DELETE_EXPIRATION_POLICY);
            //isDataFilePolicy = ? AND policyScopeId = ?
            stmt.setBoolean(1, policy.isDataFilePolicy());
            stmt.setLong(2, policyScopeId);
            stmt.executeUpdate();
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error deleting expiration policy", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    private static final String SQL_GET_CANCEL_POLICIES = "SELECT policyScopeId, honorFromAuthor, honorFromForumOwner, honorFromForumManager, honorFromAuthPoster FROM cancelPolicy";
    public Set getCancelPolicies() {
        Set rv = new HashSet();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CANCEL_POLICIES);
            rs = stmt.executeQuery();
            
            while (rs.next()) {
                // policyScopeId, honorFromAuthor, honorFromForumOwner, honorFromForumManager, honorFromAuthPoster
                long scopeId = rs.getLong(1);
                boolean honorAuthor = rs.getBoolean(2);
                if (rs.wasNull()) honorAuthor = true;
                boolean honorOwner = rs.getBoolean(3);
                if (rs.wasNull()) honorOwner = true;
                boolean honorManager = rs.getBoolean(4);
                if (rs.wasNull()) honorManager = true;
                boolean honorAuthPoster = rs.getBoolean(5);
                if (rs.wasNull()) honorAuthPoster = true;
                
                CancelPolicy policy = null;
                if (scopeId == -1)
                    policy = new CancelPolicy(true);
                else if (scopeId == -2)
                    policy = new CancelPolicy(false);
                else // channel specific policy
                    policy = new CancelPolicy(scopeId);
                
                policy.setHonorFromAuthor(honorAuthor);
                policy.setHonorFromForumOwner(honorOwner);
                policy.setHonorFromForumManager(honorManager);
                policy.setHonorFromForumAuthorizedPoster(honorAuthPoster);
                
                rv.add(policy);
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error getting cancel policies", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        return rv;
    }
    
    private static final String SQL_ADD_CANCEL_POLICY = "INSERT INTO cancelPolicy (policyScopeId, honorFromAuthor, honorFromForumOwner, honorFromForumManager, honorFromAuthPoster) VALUES (?, ?, ?, ?, ?)";
    private static final String SQL_UPDATE_CANCEL_POLICY = "UPDATE cancelPolicy SET honorFromAuthor = ?, honorFromForumOwner = ?, honorFromForumManager = ?, honorFromAuthPoster = ? WHERE policyScopeId = ?";
    public void saveCancelPolicy(CancelPolicy policy) {
        long policyScopeId = 0;
        
        if (policy.getScopeApplyToAll())
            policyScopeId = -1;
        else if (policy.getScopeApplyToLocallyManaged())
            policyScopeId = -2;
        else
            policyScopeId = policy.getScopeApplyToChannelId();

        if (policy.getIsNew()) {
            _ui.debugMessage("add new policy: " + policy);
            PreparedStatement stmt = null;
            try {
                stmt = _con.prepareStatement(SQL_ADD_CANCEL_POLICY);
                //policyScopeId, honorFromAuthor, honorFromForumOwner, honorFromForumManager, honorFromAuthPoster
                stmt.setLong(1, policyScopeId);
                stmt.setBoolean(2, policy.getHonorFromAuthor());
                stmt.setBoolean(3, policy.getHonorFromForumOwner());
                stmt.setBoolean(4, policy.getHonorFromForumManager());
                stmt.setBoolean(5, policy.getHonorFromForumAuthorizedPoster());
                stmt.executeUpdate();
            } catch (SQLException se) {
                if (_log.shouldLog(Log.WARN))
                    _log.warn("Error adding cancel policy", se);
            } finally {
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }
        } else {
            _ui.debugMessage("update policy: " + policy);
            PreparedStatement stmt = null;
            try {
                stmt = _con.prepareStatement(SQL_UPDATE_CANCEL_POLICY);
                //honorFromAuthor = ?, honorFromForumOwner = ?, honorFromForumManager = ?, honorFromAuthPoster = ? WHERE policyScopeId = ?
                stmt.setBoolean(1, policy.getHonorFromAuthor());
                stmt.setBoolean(2, policy.getHonorFromForumOwner());
                stmt.setBoolean(3, policy.getHonorFromForumManager());
                stmt.setBoolean(4, policy.getHonorFromForumAuthorizedPoster());
                stmt.setLong(5, policyScopeId);
                stmt.executeUpdate();
            } catch (SQLException se) {
                if (_log.shouldLog(Log.WARN))
                    _log.warn("Error updating cancel policy", se);
            } finally {
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }
        }
    }

    private static final String SQL_DELETE_CANCEL_POLICY = "DELETE FROM cancelPolicy WHERE policyScopeId = ?";
    public void deleteCancelPolicy(CancelPolicy policy) {
        if (policy.getIsNew()) return; // noop
        
        long policyScopeId = 0;
        if (policy.getScopeApplyToAll())
            policyScopeId = -1;
        else if (policy.getScopeApplyToLocallyManaged())
            policyScopeId = -2;
        else
            policyScopeId = policy.getScopeApplyToChannelId();

        PreparedStatement stmt = null;
        try {
            stmt = _con.prepareStatement(SQL_DELETE_CANCEL_POLICY);
            stmt.setLong(1, policyScopeId);
            stmt.executeUpdate();
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error deleting cancel policy", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_GET_CANCEL_URIS = "SELECT cancelledURI FROM channelCancel WHERE channelId = ? ORDER BY cancelOrder ASC";

    /** @return non-null */
    public List<SyndieURI> getChannelCancelURIs(long channelId) {
        ArrayList<SyndieURI> rv = new ArrayList();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CANCEL_URIS);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                String uri = rs.getString(1);
                if (uri != null) {
                    try {
                        SyndieURI parsed = new SyndieURI(uri);
                        if (parsed.getScope() != null)
                            rv.add(parsed);
                    } catch (URISyntaxException use) {}
                }
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error fetching the channel cancels", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return rv;
    }
    
    private static final String SQL_ADD_CANCEL_URI = "INSERT INTO channelCancel (cancelledURI, channelId, cancelOrder) VALUES (?, ?, ?)";
    private static final String SQL_DELETE_CANCEL_URIS = "DELETE FROM channelCancel WHERE channelId = ?";

    /**
     *  TODO validate in ImportMeta and change uris to List<SyndieURI> ?
     */
    public void setChannelCancelURIs(long channelId, List<String> uris) {
        try {
            exec(SQL_DELETE_CANCEL_URIS, channelId);
        } catch (SQLException se) {
            _ui.errorMessage("error clearing the old cancel uris for " + channelId, se);
        }
        
        if ( (uris == null) || (uris.size() <= 0) )
            return;
        
        PreparedStatement stmt = null;
        try {
            stmt = _con.prepareStatement(SQL_ADD_CANCEL_URI);
            for (int i = 0; (i < uris.size()) && (i < Constants.MAX_CANCELLED_PER_META); i++) {
                String uri = uris.get(i);
                stmt.setString(1, uri);
                stmt.setLong(2, channelId);
                stmt.setInt(3, i);
                stmt.executeUpdate();
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error adding the channel cancels", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_ADD_CANCEL_REQUEST = "INSERT INTO cancelHistory (cancelRequestedBy, cancelledURI, cancelRequestedOn) VALUES (?, ?, NOW())";
    private static final String SQL_DELETE_OLD_CANCEL_REQUESTS = "DELETE FROM cancelHistory WHERE cancelRequestedOn < ?";

    public void recordCancelRequests(long requestedByChannelId, List<SyndieURI> urisToCancel) {
        if ( (urisToCancel == null) || (urisToCancel.size() <= 0) )
            return;
        
        PreparedStatement stmt = null;
        try {
            long when = System.currentTimeMillis() - Constants.MAX_CANCELLED_HISTORY_DAYS*24*60*60*1000l;
            stmt = _con.prepareStatement(SQL_DELETE_OLD_CANCEL_REQUESTS);
            stmt.setDate(1, new Date(when));
            stmt.executeUpdate();
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error clearing the old channel cancels", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }   
        
        stmt = null;
        try {
            stmt = _con.prepareStatement(SQL_ADD_CANCEL_REQUEST);
            for (SyndieURI u : urisToCancel) {
                if (getCancelledBy(u) < 0) {
                    // only if not cancelled already
                    try {
                        String uri = u.toString();
                        stmt.setLong(1, requestedByChannelId);
                        stmt.setString(2, uri);
                        stmt.executeUpdate();
                    } catch (SQLException se) {
                        if (_log.shouldLog(Log.WARN))
                            _log.warn("Error cancelling " + u, se);
                    }
                }
            }
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error adding the channel cancels", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    private static final String SQL_GET_CANCELLED_BY = "SELECT cancelRequestedBy FROM cancelHistory WHERE cancelledURI = ?";

    public long getCancelledBy(SyndieURI uri) {
        if (uri == null) return -1;
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_CANCELLED_BY);
            stmt.setString(1, uri.toString());
            rs = stmt.executeQuery();
            if (rs.next())
                return rs.getLong(1);
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Error determining if it was cancelled", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return -1;
    }
    
    void honorCancel(SyndieURI uri, long msgId) {
        deleteFromArchive(uri, _ui);
        deleteMessageFromDB(msgId, DELETION_CAUSE_CANCELLED);
    }
    
    void deleteStubMessage(SyndieURI uri) {
        long msgId = getMessageId(uri.getScope(), uri.getMessageId());
        _ui.debugMessage("Deleting stub message for " + uri + " / " + msgId);
        if (msgId >= 0)
            deleteMessageFromDB(msgId, DELETION_CAUSE_STUB);
    }
    
    /** run the given syndie script in the $scriptDir, such as "register", "login" or "startup" */
    public void runScript(UI ui, String scriptName) {
        File scriptDir = new File(_rootDir, "scripts");
        File script = new File(scriptDir, scriptName);
        if (script.exists()) {
            BufferedReader in = null;
            try {
                ui.debugMessage("running script from " + script.getAbsolutePath());
                in = new BufferedReader(new InputStreamReader(new FileInputStream(script), "UTF-8"));
                String line = null;
                while ( (line = in.readLine()) != null) {
                    if (line.startsWith("//") || line.startsWith("#") || line.startsWith(";"))
                        continue;
                    ui.insertCommand(line);
                }
                in.close();
                in = null;
            } catch (UnsupportedEncodingException uee) {
                ui.errorMessage("internal error, your JVM doesn't support UTF-8?", uee);
            } catch (IOException ioe) {
                ui.errorMessage("Error running the script " + script, ioe);
            } finally {
                if (in != null) try { in.close(); } catch (IOException ioe) {}
            }
        } else {
            ui.debugMessage("script does not exist [" + script.getAbsolutePath() + "]");
        }
        ui.insertCommand("notifyscriptend " + scriptName);
        ui.debugMessage("added notifyscriptend " + scriptName);
    }
    
    
    private static final String SQL_UPDATE_NYM_PASS = "UPDATE nym SET passSalt = ?, passHash = ? WHERE nymId = ?";

    public void changePassphrase(String newPass) {
        try {
            _con.setAutoCommit(false);
            log("changing passphrase from [" + _nymPass + "] to [" + newPass + "]");
            // reencrypt all of the keys under the new passphrase
            if (!reencryptKeys(_nymPass, newPass)) {
                log("reencryptKeys failed");
                log("Passphrase NOT changed");
                _con.rollback();
                return;
            }
            // reencrypt all of the postponed messages under the new passphrase
            if (!reencryptPostponed(_nymPass, newPass)) {
                log("reencryptPostponed failed");
                log("Passphrase NOT changed");
                _con.rollback();
                return;
            }

            byte salt[] = new byte[16];
            _context.random().nextBytes(salt);
            byte hash[] = _context.keyGenerator().generateSessionKey(salt, DataHelper.getUTF8(newPass)).getData();

            PreparedStatement pstmt = null;
            try {
                pstmt = _con.prepareStatement(SQL_UPDATE_NYM_PASS);
                pstmt.setBytes(1, salt);
                pstmt.setBytes(2, hash);
                pstmt.setLong(3, _nymId);
                int rows = pstmt.executeUpdate();
                log("nym pass hash updated");
            } catch (SQLException se) {
                log("Unable to update the nym pass hash", se);
                log("Passphrase NOT changed");
                _con.rollback();
                return;
            } finally {
                if (pstmt != null) try { pstmt.close(); } catch (SQLException se) {}
            }

            Statement stmt = null;
            try {
                log("changing db passphrase...");
                stmt = _con.createStatement();
                stmt.execute("ALTER USER \"" + TextEngine.DEFAULT_LOGIN + "\" SET PASSWORD '" + newPass + '\'');
                stmt.close();
                stmt = null;
                log("Passphrase changed to " + newPass);
                
                byte val[] = new byte[16];
                _context.random().nextBytes(val);
                String rand = Base64.encode(val);
                log("changing default admin account passphrase to something random");
                stmt = _con.createStatement();
                stmt.execute("ALTER USER \"" + DEFAULT_ADMIN + "\" SET PASSWORD '" + rand + '\'');
                stmt.close();
                stmt = null;
                log("sysadmin passphrase changed to a random value");
            } catch (SQLException se) {
                log("Error changing the database passphrase", se);
                log("Passphrase NOT changed");
                _con.rollback();
            } finally {
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }
            _nymPass = newPass;
            _con.commit();
        } catch (SQLException se) {
            log("Error partway through the passphrase changing...?", se);
        } finally {
            try { _con.setAutoCommit(true); } catch (SQLException se) {}
        }
    }

    private static class NymKeyData {
        String keyType;
        byte keyData[];
        byte keySalt[];
        boolean auth;
        Date periodBegin;
        Date periodEnd;
        String function;
        Hash channel;
        //long nymId;
    }
    
    private static final String SQL_DELETE_NYMKEYS = "DELETE FROM nymKey WHERE nymId = ?";
    private static final String SQL_INSERT_NYMKEY = "INSERT INTO nymKey " +
                                                    "(nymId, keyChannel, keyFunction, keyType, keyData, keySalt, authenticated, keyPeriodBegin, keyPeriodEnd)" +
                                                    " VALUES " +
                                                    "(?, ?, ?, ?, ?, ?, ?, ?, ?)";

    private boolean reencryptKeys(String oldPass, String newPass) {
        ensureLoggedIn();
        List<NymKeyData> rv = new ArrayList(1);
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            String query = SQL_GET_NYMKEYS;
            stmt = _con.prepareStatement(query);
            stmt.setLong(1, _nymId);
            
            rs = stmt.executeQuery();
            while (rs.next()) {
                // keyType, keyData, keySalt, authenticated, keyPeriodBegin, keyPeriodEnd, keyFunction, keyChannel
                NymKeyData data = new NymKeyData();
                data.keyType = rs.getString(1);
                data.keyData = rs.getBytes(2);
                data.keySalt = rs.getBytes(3);
                data.auth = rs.getBoolean(4);
                data.periodBegin = rs.getDate(5);
                data.periodEnd = rs.getDate(6);
                data.function = rs.getString(7);
                data.channel = Hash.create(rs.getBytes(8));
                //data.nymId = _nymId;
                
                if (data.keySalt != null) {
                    byte key[] = pbeDecrypt(data.keyData, oldPass, data.keySalt);
                    if (key == null) {
                        log("decrypt of old key failed: " + rv.size());
                        continue;
                    }
                    data.keySalt = null;
                    data.keyData = key;
                }
                
                rv.add(data);
                log("decrypted old key " + rv.size());
            }
            
            rs.close();
            rs = null;
            stmt.close();
            stmt = null;
            
            stmt = _con.prepareStatement(SQL_DELETE_NYMKEYS);
            stmt.setLong(1, _nymId);
            stmt.executeUpdate();
            
            stmt.close();
            stmt = null;
            
            stmt = _con.prepareStatement(SQL_INSERT_NYMKEY);
            for (int i = 0; i < rv.size(); i++) {
                NymKeyData data = (NymKeyData)rv.get(i);
                
                byte salt[] = new byte[16]; // overwritten by pbeEncrypt
                byte encr[] = pbeEncrypt(data.keyData, newPass, salt);

                stmt.setLong(1, _nymId);
                stmt.setBytes(2, data.channel.getData());
                stmt.setString(3, data.function);
                stmt.setString(4, data.keyType);
                stmt.setBytes(5, encr);
                stmt.setBytes(6, salt);
                stmt.setBoolean(7, data.auth);
                stmt.setDate(8, data.periodBegin);
                stmt.setDate(9, data.periodEnd);
                
                int rows = stmt.executeUpdate();
                if (rows != 1)
                    throw new SQLException("Error importing a key: row count of " + rows);
                log("reencrypted old key " + (i+1));
            }
            log("keys reencrypted: " + rv.size());
            return true;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error reencrypting the keys", se);
            return false;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static class PostponedData {
        long nymId;
        long postponeId;
        int version;
        String rawB64;
    }
    
    private static final String SQL_GET_POSTPONED = "SELECT nymId, postponeId, postponeVersion, encryptedData FROM nymMsgPostpone WHERE nymId = ?";
    private static final String SQL_DROP_POSTPONED = "DELETE FROM nymMsgPostpone WHERE nymId = ?";
    private static final String SQL_INSERT_POSTPONED = "INSERT INTO nymMsgPostpone (nymId, postponeId, postponeVersion, encryptedData) VALUES (?, ?, ?, ?)";

    private boolean reencryptPostponed(String oldPass, String newPass) {
        ensureLoggedIn();
        List<PostponedData> rv = new ArrayList(1);
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement(SQL_GET_POSTPONED);
            stmt.setLong(1, _nymId);
            
            rs = stmt.executeQuery();
            while (rs.next()) {
                PostponedData data = new PostponedData();
                data.nymId = rs.getLong(1);
                data.postponeId = rs.getLong(2);
                data.version = rs.getInt(3);
                data.rawB64 = rs.getString(4);
                rv.add(data);
            }
            
            rs.close();
            rs = null;
            stmt.close();
            stmt = null;
            
            stmt = _con.prepareStatement(SQL_DROP_POSTPONED);
            stmt.setLong(1, _nymId);
            stmt.executeUpdate();
            stmt.close();
            stmt = null;
            
            stmt = _con.prepareStatement(SQL_INSERT_POSTPONED);
            int count = 0;
            for (int i = 0; i < rv.size(); i++) {
                PostponedData data = (PostponedData)rv.get(i);
                
                String salt = data.rawB64.substring(0, 24);
                String body = data.rawB64.substring(24);
                byte decr[] = pbeDecrypt(Base64.decode(body), oldPass, Base64.decode(salt));
                
                log("decrypted old postponed: " + (i+1));
                
                byte newSalt[] = new byte[16];
                byte encr[] = pbeEncrypt(decr, newPass, newSalt);
                data.rawB64 = Base64.encode(newSalt) + Base64.encode(encr);
                log("reencrypted old postponed: " + (i+1));

                //(nymId, postponeId, postponeVersion, encryptedData) 
                stmt.setLong(1, data.nymId);
                stmt.setLong(2, data.postponeId);
                stmt.setInt(3, data.version);
                stmt.setString(4, data.rawB64);
                stmt.executeUpdate();
                count++;
            }
            log("done reencrypting postponed messages [" + count + "]");
            return true;
        } catch (SQLException se) {
            log("Error reencrypting the postponed msgs", se);
            return false;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private void log(SQLException se) {
        if (_ui != null)
            _ui.errorMessage("Internal error", se);
        else
            se.printStackTrace();
    }
    private void log(String msg, Exception e) {
        if (_ui != null)
            _ui.errorMessage(msg, e);
        else
            _log.error(msg, e);
    }
    private void log(String msg) {
        if (_ui != null)
            _ui.debugMessage(msg);
        else
            _log.error(msg);
    }
    
    /**
     *  Generate a new number, greater than lastValue.
     *  If lastValue < now, generate a value between (max(now - 6h, lastValue), now).
     *  If lastValue >= now, generate a value between (lastValue, lastValue + 1h).
     */
    public long createEdition(long lastValue) {
        long now = System.currentTimeMillis();
        long rv;
        if (lastValue < now) {
            rv = Math.max(lastValue, now - PAST_RANDOM_PERIOD);
            rv += _context.random().nextLong(now - rv);
        } else if (lastValue < Long.MAX_VALUE - FUTURE_RANDOM_PERIOD) {
            rv = lastValue + _context.random().nextLong(FUTURE_RANDOM_PERIOD);
        } else if (lastValue < Long.MAX_VALUE) {
            // almost pinned
            rv = lastValue + 1;
        } else {
            // pinned
            rv = lastValue;
        }
        return rv;
    }
    
    public void logError(String msg, Exception cause) { 
        if (_log.shouldLog(Log.ERROR))
            _log.error(msg, cause);
    }

    public void logInfo(String msg) { 
        if (_log.shouldLog(Log.INFO)) 
            _log.info(msg); 
    }

    /** 
     *  Logs at WARN level if cause != null, otherwise DEBUG
     */
    public void logDebug(String msg, Exception cause) { 
        // make it easier to find the real problems
        if (cause != null && _log.shouldLog(Log.WARN)) 
            _log.warn(msg, cause); 
        else if (_log.shouldLog(Log.DEBUG)) 
            _log.debug(msg, cause); 
    }
    
    /** 
     * encrypt the orig data w/ the current passphrase, generating a new salt and
     * saving it in saltTarget.  The result is the padded encrypted data
     */
    public byte[] pbeEncrypt(byte orig[], byte saltTarget[]) {
        return pbeEncrypt(orig, _nymPass, saltTarget, I2PAppContext.getGlobalContext());
    }

    public byte[] pbeEncrypt(byte orig[], String pass, byte saltTarget[]) {
        return pbeEncrypt(orig, pass, saltTarget, I2PAppContext.getGlobalContext());
    }

    public static byte[] pbeEncrypt(byte orig[], String pass, byte saltTarget[], I2PAppContext ctx) {
        ctx.random().nextBytes(saltTarget);
        SessionKey saltedKey = ctx.keyGenerator().generateSessionKey(saltTarget, DataHelper.getUTF8(pass));
        int pad = 16-(orig.length%16);
        if (pad == 0) pad = 16;
        byte pre[] = new byte[orig.length+pad];
        System.arraycopy(orig, 0, pre, 0, orig.length);
        for (int i = 0; i < pad; i++)
            pre[pre.length-1-i] = (byte)(pad&0xff);
        byte encrypted[] = new byte[pre.length];
        ctx.aes().encrypt(pre, 0, encrypted, 0, saltedKey, saltTarget, pre.length);
        return encrypted;
    }
    
    /** pbe decrypt the data with the current passphrase, returning the decrypted data, stripped of any padding */
    public byte[] pbeDecrypt(byte orig[], byte salt[]) { return pbeDecrypt(orig, _nymPass, salt); }

    public byte[] pbeDecrypt(byte orig[], String pass, byte salt[]) {
        return pbeDecrypt(orig, 0, salt, 0, pass, orig.length, _context);
    }
    
    public byte[] pbeDecrypt(byte orig[], int origOffset, byte salt[], int saltOffset, String pass, int len) {
        return pbeDecrypt(orig, origOffset, salt, saltOffset, pass, len, _context);
    }

    public static byte[] pbeDecrypt(byte orig[], int origOffset, byte salt[], int saltOffset, String pass, int len, I2PAppContext ctx) {
        byte saltCopy[] = new byte[16];
        System.arraycopy(salt, saltOffset, saltCopy, 0, saltCopy.length);
        SessionKey saltedKey = ctx.keyGenerator().generateSessionKey(saltCopy, DataHelper.getUTF8(pass));
        byte decr[] = new byte[len];
        ctx.aes().decrypt(orig, origOffset, decr, 0, saltedKey, saltCopy, len);
        int pad = (int)decr[decr.length-1];
        if ( (pad < 0) || (pad > decr.length) ) // decrypt failed
            return null;
        byte rv[] = new byte[decr.length-pad];
        System.arraycopy(decr, 0, rv, 0, rv.length);
        return rv;
    }
    
    private BugConfig _bugConfig;
    public BugConfig getBugConfig() {
        ensureLoggedIn();
        if (_bugConfig == null) {
            BugConfig cfg = new BugConfig();
            try {
                cfg.load(_con);
            } catch (SQLException se) {
                _ui.errorMessage("Error loading the bug config", se);
                return null;
            }
            _bugConfig = cfg;
        }
        return _bugConfig;
    }
    
    private boolean _trace;
    private int _getMsgCount;
    private int _getChanCount;
    private long _getMsgTime;
    private long _getChanTime;
    private long _traceStart;
    public void beginTrace() { 
        _trace = true;
        _traceStart = System.currentTimeMillis();
        _getMsgCount = 0;
        _getChanCount = 0;
        _getMsgTime = 0;
        _getChanTime = 0;
    }
    public String completeTrace() {
        long end = System.currentTimeMillis();
        _trace = false;
        return "time: " + (end-_traceStart) + " getMsg: " + _getMsgCount + "/" + _getMsgTime + " getChan: " + _getChanCount + "/" + _getChanTime;
    }
}
