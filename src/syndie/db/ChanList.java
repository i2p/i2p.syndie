package syndie.db;

import java.io.File;
import java.sql.SQLException;
import java.util.*;

import net.i2p.I2PAppContext;
import net.i2p.data.Hash;
import net.i2p.util.SecureFile;

/**
 *CLI chanlist
 * --db $url
 * --login $login
 * --pass $pass
 */
public class ChanList extends CommandImpl {

    public static String getHelp(String cmd) {
        return "          : lists all channels";
    }

    public DBClient runCommand(Opts args, UI ui, DBClient client) {
        if ( (client == null) || (!client.isLoggedIn()) ) {
            List missing = args.requireOpts(new String[] { "db" });
            if (missing.size() > 0) {
                ui.errorMessage("Invalid options, missing " + missing);
                ui.commandComplete(-1, null);
                return client;
            }
        }
        
        try {
            if (args.dbOptsSpecified()) {
                if (client == null)
                    client = new DBClient(I2PAppContext.getGlobalContext(), new SecureFile(TextEngine.getRootPath()));
                else
                    client.close();
                client.connect(args.getOptValue("db"));
            }
            Map ids = client.getChannelIds();
            for (Iterator iter = ids.keySet().iterator(); iter.hasNext(); ) {
                Long id = (Long)iter.next();
                Hash chan = (Hash)ids.get(id);
                ui.statusMessage("Channel " + id + ": " + chan.toBase64());
            }
            ui.commandComplete(0, null);
        } catch (SQLException se) {
            ui.errorMessage("Invalid database URL", se);
            ui.commandComplete(-1, null);
        }
        return client;
    }
    
    public static void main(String args[]) {
        try {
        CLI.main(new String[] { "chanlist", 
                                "--db", "jdbc:hsqldb:file:/tmp/cli",
                                "--login", "j",
                                "--pass", "j" });
        } catch (Exception e) { e.printStackTrace(); }
    }
}
