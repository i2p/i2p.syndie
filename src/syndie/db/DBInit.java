package syndie.db;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import net.i2p.I2PAppContext;
import net.i2p.util.Log;


/**
 *  Initialize the database, creating and updating to the latest version if necessary
 *
 *  @since 1.103b-x moved from DBClient
 */
class DBInit {

    private final Connection _con;
    private final Log _log;

    public DBInit(I2PAppContext ctx, Connection dbConn) {
        _con = dbConn;
        _log = ctx.logManager().getLog(DBInit.class);
    }

    /**
     *  Initialize the DB, update to latest version if necessary
     */
    public void initDB() throws SQLException {
        int version = checkDBVersion();
        if (_log.shouldLog(Log.DEBUG))
            _log.debug("Known DB version: " + version);
        if (version < 0) {
            if (_log.shouldLog(Log.INFO))
                _log.info("Building the database...");
        }
        int updates = getDBUpdateCount(); // syndie/db/ddl_update$n.txt
        for (int i = 0; i <= updates; i++) {
            if (i >= version) {
                if (_log.shouldLog(Log.DEBUG))
                    _log.debug("Updating database version " + i + " to " + (i+1));
                updateDB(i);
            } else {
               // if (_log.shouldLog(Log.DEBUG))
               //     _log.debug("No need for update " + i + " (version: " + version + ")");
            }
        }
    }

    private int checkDBVersion() {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _con.prepareStatement("SELECT versionNum FROM appVersion WHERE app = 'syndie.db'");
            rs = stmt.executeQuery();
            while (rs.next()) {
                int rv = rs.getInt(1);
                if (!rs.wasNull())
                    return rv;
            }
            return -1;
        } catch (SQLException se) {
            if (_log.shouldLog(Log.WARN))
                _log.warn("Unable to check the database version (does not exist?)", se);
            return -1;
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    private int getDBUpdateCount() {
        int updates = 0;
        while (true) {
            InputStream in = getClass().getResourceAsStream("ddl_update" + (updates+1) + ".txt");
            if (in != null) {
                updates++;
                try { in.close(); } catch (IOException ioe) {}
            } else {
                if (_log.shouldLog(Log.DEBUG))
                    _log.debug("There were " + updates + " database updates known for " + getClass().getName() + " ddl_update*.txt");
                return updates;
            }
        }
    }

    /**
     *  Create a new DB with the version 1 ddl_update0.txt, or
     *  Update from oldVersion to oldVersion + 1 using ddl_update{oldVersion}.txt
     */
    private void updateDB(int oldVersion) throws SQLException {
        BufferedReader r = null;
        try {
            InputStream in = getClass().getResourceAsStream("ddl_update" + oldVersion + ".txt");
            if (in != null) {
                r = new BufferedReader(new InputStreamReader(in));
                StringBuilder cmdBuf = new StringBuilder();
                String line = null;
                while ( (line = r.readLine()) != null) {
                    line = line.trim();
                    if (line.startsWith("//") || line.startsWith("--"))
                        continue;
                    cmdBuf.append(' ').append(line);
                    if (line.endsWith(";")) {
                        exec(cmdBuf.toString());
                        cmdBuf.setLength(0);
                    }
                }
                r.close();
                r = null;
            }
        } catch (IOException ioe) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error reading the db script", ioe);
            // SQLException(Throwable) as of Java 6
            throw new SQLException(ioe.toString());
        } catch (SQLException se) {
            if (_log.shouldLog(Log.ERROR))
                _log.error("Error building the db", se);
            throw se;
        } finally {
            if (r != null) try { r.close(); } catch (IOException ioe) {}
        }
    }

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
}
