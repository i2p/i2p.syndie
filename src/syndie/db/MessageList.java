package syndie.db;

import java.io.File;
import java.sql.SQLException;
import java.util.*;

import net.i2p.I2PAppContext;
import net.i2p.data.Hash;
import net.i2p.util.SecureFile;

/**
 *CLI messagelist
 * --db $url
 * --channel $base64(channelHash)
 */
public class MessageList extends CommandImpl {

    public static String getHelp(String cmd) {
        return "--channel $base64channelhash";
    }

    public DBClient runCommand(Opts args, UI ui, DBClient client) {
        if ( (client == null) || (!client.isLoggedIn()) ) {
            List missing = args.requireOpts(new String[] { "db", "channel" });
            if (missing.size() > 0) {
                ui.errorMessage("Invalid options, missing " + missing);
                ui.commandComplete(-1, null);
                return client;
            }
        } else {
            List missing = args.requireOpts(new String[] { "channel" });
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
            Hash chan = Hash.create(args.getOptBytes("channel"));
            ui.statusMessage("Channel " + chan.toBase64());
            List internalIds = client.getMessageIdsPrivate(chan);
            if (internalIds.size() > 0) {
                ui.statusMessage("Private messages available: ");
                for (int i = 0; i < internalIds.size(); i++)
                    ui.statusMessage("\tmessage " + internalIds.get(i));
            }
            internalIds = client.getMessageIdsAuthorized(chan);
            if (internalIds.size() > 0) {
                ui.statusMessage("Authorized messages available: ");
                for (int i = 0; i < internalIds.size(); i++)
                    ui.statusMessage("\tmessage " + internalIds.get(i));
            }
            internalIds = client.getMessageIdsAuthenticated(chan);
            if (internalIds.size() > 0) {
                ui.statusMessage("Authenticated yet unauthorized messages available: ");
                for (int i = 0; i < internalIds.size(); i++)
                    ui.statusMessage("\tmessage " + internalIds.get(i));
            }
            internalIds = client.getMessageIdsUnauthenticated(chan);
            if (internalIds.size() > 0) {
                ui.statusMessage("Unauthenticated and unauthorized messages available: ");
                for (int i = 0; i < internalIds.size(); i++)
                    ui.statusMessage("\tmessage " + internalIds.get(i));
            }
        } catch (SQLException se) {
            ui.errorMessage("Invalid database URL", se);
        //} finally {
        //    if (client != null) client.close();
        }
        return client;
    }
    
    public static void main(String args[]) {
        try {
        CLI.main(new String[] { "messagelist", 
                                "--db", "jdbc:hsqldb:file:/tmp/cli",
                                "--login", "j",
                                "--pass", "j",
                                "--channel", "2klF2vDob7M82j8ZygZ-s9LmOHfaAdso5V0DzLvHISI=" });
        } catch (Exception e) { e.printStackTrace(); }
    }
}
