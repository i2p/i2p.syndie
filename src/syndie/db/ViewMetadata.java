package syndie.db;

import java.io.File;
import java.sql.SQLException;
import java.util.*;
import net.i2p.I2PAppContext;
import net.i2p.data.Hash;
import syndie.data.ChannelInfo;

/**
 *CLI viewmetadata
 * --db $url
 * --login $login
 * --pass $pass
 * --channel $base64(channelHash)
 */
public class ViewMetadata extends CommandImpl {
    ViewMetadata() {}
    public DBClient runCommand(Opts args, UI ui, DBClient client) {
        if ( (client == null) || (!client.isLoggedIn()) ) {
            List missing = args.requireOpts(new String[] { "db", "login", "pass", "channel" });
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
            long nymId = -1;
            if (args.dbOptsSpecified()) {
                if (client == null)
                    client = new DBClient(I2PAppContext.getGlobalContext(), new File(TextEngine.getRootPath()));
                //else
                //    client.close();
                nymId = client.connect(args.getOptValue("db"), args.getOptValue("login"), args.getOptValue("pass"));
                if (nymId < 0) {
                    ui.errorMessage("Login incorrect");
                    ui.commandComplete(-1, null);
                    return client;
                }
            } else {
                nymId = client.getLoggedInNymId();
                if (nymId < 0) {
                    ui.errorMessage("Not logged in");
                    ui.commandComplete(-1, null);
                    return client;
                }
            }
            Hash channel = new Hash(args.getOptBytes("channel"));
            long channelId = client.getChannelId(channel);
            if (channelId < 0) {
                ui.errorMessage("Channel is not known");
                ui.commandComplete(-1, null);
            } else {
                ChannelInfo info = client.getChannel(channelId);
                if (info != null) {
                    ui.statusMessage(info.toString());
                    ui.commandComplete(0, null);
                } else {
                    ui.errorMessage("Error fetching channel " + channelId);
                    ui.commandComplete(-1, null);
                }
            }
        } catch (SQLException se) {
            ui.errorMessage("Invalid database URL", se);
            ui.commandComplete(-1, null);
        //} finally {
        //    if (client != null) client.close();
        }
        return client;
    }
    
    public static void main(String args[]) {
        try {
        CLI.main(new String[] { "viewmetadata", 
                                "--db", "jdbc:hsqldb:file:/tmp/cli",
                                "--login", "j",
                                "--pass", "j",
                                "--channel", "2klF2vDob7M82j8ZygZ-s9LmOHfaAdso5V0DzLvHISI=" });
        } catch (Exception e) { e.printStackTrace(); }
    }
}
