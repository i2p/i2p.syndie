package syndie.db;

import java.io.IOException;
import java.util.*;
import net.i2p.data.Base64;
import net.i2p.data.DataHelper;

/**
 *
 */
public class CLI {
    private static final String PREFIX = CLI.class.getName().substring(0, CLI.class.getName().lastIndexOf("."));

    public static interface Command {
        public DBClient runCommand(Opts opts, UI ui, DBClient client);
    }

    private static Object _commands[][] = new Object[][] {
        new Object[] { "import", Importer.class },
        new Object[] { "register", LoginManager.class },
//        new Object[] { "login", LoginManager.class },
        new Object[] { "changen", ChanGen.class },
        new Object[] { "chanlist", ChanList.class },
        new Object[] { "keyimport", KeyImport.class },
        new Object[] { "keygen", KeyGen.class },
        new Object[] { "keylist", KeyList.class },
        new Object[] { "messagegen", MessageGen.class },
        new Object[] { "messageextract", MessageExtract.class },
        new Object[] { "viewmetadata", ViewMetadata.class },
        new Object[] { "messagelist", MessageList.class },
        new Object[] { "viewmessage", ViewMessage.class },
        new Object[] { "httpserv", HTTPServ.class },
        new Object[] { "ctrlserv", ControlServer.class }
    };
    
    public static void main(String args[]) {
        //args = new String[] { "Importer" };
        if ( (args == null) || (args.length <= 0) ) {
            usage();
            return;
        }
        
        Command cmd = getCommand(args[0]);
        if (cmd != null) {
            DBClient client = null;
            try {
                String params[] = new String[args.length-1];
                System.arraycopy(args, 1, params, 0, params.length);
                Opts opts = new Opts(args[0], params);
                client = cmd.runCommand(opts, new TextUI(opts.getOptBoolean("debug", false)), null);
            } catch (Exception e) {
                e.printStackTrace();
            } finally {
                if (client != null)
                    client.close();
            }
        } else {
            usage();
        }
    }

    public synchronized static Command getCommand(String name) {
        Class cls = null;
        for (int i = 0; i < _commands.length; i++) {
            if (name.equalsIgnoreCase(_commands[i][0].toString())) {
                cls = (Class)_commands[i][1];
                break;
            }
        }
        if (cls != null) {
            try {
                return (Command)cls.newInstance();
            } catch (Exception e) {
                return null;
            }
        } else {
            return null;
        }
    }

    /**
     * allow new commands to be added to the set of known commands,
     * or old commands to be replaced with new ones.
     */
    public synchronized static void setCommand(String name, Class cmdClass) {
        if (getCommand(name) == null) {
            Object old[][] = _commands;
            Object newcmd[][] = new Object[old.length+1][2];
            for (int i = 0; i < old.length; i++) {
                newcmd[i][0] = old[i][0];
                newcmd[i][1] = old[i][1];
            }
            newcmd[old.length][0] = name;
            newcmd[old.length][1] = cmdClass;
            _commands = newcmd;
        } else {
            for (int i = 0; i < _commands.length; i++) {
                if (name.equalsIgnoreCase(_commands[i][0].toString())) {
                    _commands[i][1] = cmdClass;
                }
            }
        }
    }

    /** unsorted */
    public synchronized static List<String> getCommands() {
        List<String> rv = new ArrayList(_commands.length);
        for (int i = 0; i < _commands.length; i++) {
            rv.add(_commands[i][0].toString());
        }
        return rv;
    }

    private synchronized static final void usage() {
        System.err.println("Usage: $command [$args]*");
        System.err.print("Known commands: ");
        for (int i = 0; i < _commands.length; i++)
            System.err.print(_commands[i][0].toString() + " ");
        System.err.println();
    }
}
