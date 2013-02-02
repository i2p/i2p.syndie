package syndie.db;

import java.io.BufferedInputStream;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Properties;

import net.i2p.I2PAppContext;
import net.i2p.data.DataHelper;
import net.i2p.util.FileUtil;
import net.i2p.util.Log;
import net.i2p.util.SecureFileOutputStream;
import net.i2p.util.VersionComparator;


/**
 *  Methods to manage upgrading hsqldb versions
 *
 *  ref: http://hsqldb.org/doc/2.0/guide/deployment-chapt.html#dec_upgrade_database
 *
 *  @since 1.103b-x
 */
class DBUpgrade {

    private static String _oldVersion;

    private static final String HSQLDB_VERSION_2 = "2";

    /** valid and public from at least 1.8.1.3 - 2.2.9 */
    private static final String HSQLDB_VERSION_CLASS = "org.hsqldb.persist.HsqlDatabaseProperties";
    /** not THIS_FULL_VERSION, which is sometimes 4 digits */
    private static final String HSQLDB_VERSION_FIELD = "THIS_VERSION";

    /** fails on hsqldb 2.0 */
    private static final String BAD_LINE = "GRANT ALL ON CLASS \"java.lang.String\" TO ";

    private static final String SQL_INCREMENT = "SET FILES BACKUP INCREMENT TRUE";

    /**
     *  Call before connecting to the database
     *
     *  @param dbPath /path/to/.syndie/db/syndie (i.e. without the .data suffix)
     *  @return previous DB version or null
     */
    public static String preConnect(String dbPath) {
        _oldVersion = getOldVersion(dbPath);
        String curVersion = getHsqldbVersion();
        VersionComparator comp = new VersionComparator();
        if (_oldVersion != null && curVersion != null) {
            int diff = comp.compare(curVersion, _oldVersion);
            if (diff > 0)
                log("Upgrading from hsqldb version " + _oldVersion + " to " + curVersion +
                    "; this may take a while, please be patient, do not interrupt");
            else if (diff < 0)
                log("Downrading from hsqldb version " + _oldVersion + " to " + curVersion +
                    "; this may not work!");
        }
        if (_oldVersion != null && comp.compare(_oldVersion, HSQLDB_VERSION_2) < 0 &&
            curVersion != null && comp.compare(curVersion, HSQLDB_VERSION_2) >= 0) {
            // backup everything?
            migrateLog(dbPath);
            migrateScript(dbPath);
        }
        return _oldVersion;
    }

    /**
     *  Previous database version
     *
     *  @param dbPath /path/to/.syndie/db/syndie (i.e. without the .data suffix)
     *  @return previous DB version or null
     */
    private static String getOldVersion(String dbPath) {
        InputStream is = null;
        try {
            is = new BufferedInputStream(new FileInputStream(dbPath + ".properties"));
            Properties props = new Properties();
            props.load(is);
            return props.getProperty("version");
        } catch (IOException ioe) {
            return null;
        } finally {
            if (is != null) try { is.close(); } catch (IOException ioe) {}
        }
    }

    /**
     *  Hack to get the current hsqldb library version before we've connected to the database
     *
     *  @return library version or null
     */
    private static String getHsqldbVersion() {
        try {
            Class ver = Class.forName(HSQLDB_VERSION_CLASS, true, DBUpgrade.class.getClassLoader());
            Field field = ver.getField(HSQLDB_VERSION_FIELD);
            return (String) field.get(null);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     *  The current hsqldb library version
     *
     *  @return library version or "unknown"
     */
    public static String getHsqldbVersion(Connection dbConn) {
        try {
            if (dbConn != null && !dbConn.isClosed()) {
                return dbConn.getMetaData().getDatabaseProductVersion();
            }
        } catch (SQLException se) {}
        String rv = getHsqldbVersion();
        return rv != null ? rv : "unknown";
    }

    /**
     *  Remove old log file after crash, since we are updating (sorry)
     *  Unused, let's try migrating it
     *  @param dbPath /path/to/.syndie/db/syndie (i.e. without the .data suffix)
     */
    private static void removeOldLog(String dbPath) {
        File f = new File(dbPath + ".log");
        if (f.delete())
            log("Deleted old log file before migration: " + f);
    }

    /**
     *  Strip the offensive line from syndie.script
     *
     *  @param dbPath /path/to/.syndie/db/syndie (i.e. without the .data suffix)
     */
    private static void migrateScript(String dbPath) {
        migrate(dbPath, ".script");
    }

    /**
     *  Strip the offensive line from syndie.script
     *
     *  @param dbPath /path/to/.syndie/db/syndie (i.e. without the .data suffix)
     */
    private static void migrateLog(String dbPath) {
        migrate(dbPath, ".log");
    }

    /**
     *  Strip the offensive line from syndie.script
     *
     *  @param dbPath /path/to/.syndie/db/syndie (i.e. without the .data suffix)
     */
    private static void migrate(String dbPath, String suffix) {
        File oldFile = new File(dbPath + suffix);
        if (!oldFile.exists())
            return;
        File newFile = new File(dbPath + suffix + ".tmp");
        InputStream in = null;
        PrintWriter out = null;
        try {
            in = new BufferedInputStream(new FileInputStream(oldFile));
            out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new SecureFileOutputStream(newFile), "UTF-8")));
            boolean foundIt = false;
            String s = null;
            while ((s = DataHelper.readLine(in)) != null) {
                if (s.startsWith(BAD_LINE))
                    foundIt = true;
                else
                    out.println(s);
            }
            if (foundIt) {
                in.close();
                out.close();
                FileUtil.rename(newFile, oldFile);
                log("Migrated: " + oldFile);
            }
        } catch (IOException ioe) {
        } finally {
            if (in != null) try { in.close(); } catch (IOException ioe) {}
            if (out != null) out.close();
            newFile.delete();
        }
    }

    /**
     *  Are we hsqldb version 2.0 or higher?
     */
    public static boolean isHsqldb20(Connection dbConn) {
        String version = getHsqldbVersion(dbConn);
        VersionComparator comp = new VersionComparator();
        return comp.compare(version, HSQLDB_VERSION_2) >= 0;
    }

    /**
     *  Call after connecting to the database.
     *  Must have called preConnect() previously, if the database existed.
     *
     *  @param dbConn non-null
     *  @return true if DB was upgraded (false if new DB or same version)
     */
    public static boolean postConnect(Connection dbConn) {
        VersionComparator comp = new VersionComparator();
        String version;
        try {
            version = dbConn.getMetaData().getDatabaseProductVersion();
        } catch (SQLException se) {
            return false;
        }
        log("Post connect, version: " + version  /*, new Exception() */ );
        boolean is20 = comp.compare(version, HSQLDB_VERSION_2) >= 0;
        if (is20 && (_oldVersion == null || comp.compare(_oldVersion, HSQLDB_VERSION_2) < 0)) {
            setIncrement(dbConn);
        }
        boolean isNewer = _oldVersion != null && comp.compare(version, _oldVersion) > 0;
        // so we don't do this twice
        _oldVersion = version;
        return isNewer;
    }
    
    /**
     *  New stuff only valid in 2.0 or higher
     */
    private static void setIncrement(Connection dbConn) {
        PreparedStatement stmt = null;
        try {
            stmt = dbConn.prepareStatement(SQL_INCREMENT);
            stmt.execute();
            log("Set increment successful");
        } catch (SQLException se) {
            log("Set increment failed: " + se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    private static void log(String s) {
        I2PAppContext.getGlobalContext().logManager().getLog(DBUpgrade.class).logAlways(Log.WARN, s);
    }

    private static void log(String s, Throwable t) {
        I2PAppContext.getGlobalContext().logManager().getLog(DBUpgrade.class).error(s, t);
    }
}
