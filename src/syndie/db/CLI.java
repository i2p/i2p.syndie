package syndie.db;

import java.io.IOException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;

import net.i2p.data.Base64;
import net.i2p.data.DataHelper;

/**
 *  Custom commands
 */
public class CLI {

    /**
     *  Implementing classes should also provide:
     *  public static String getHelp(String cmd)
     */
    public static interface Command {
        public DBClient runCommand(Opts opts, UI ui, DBClient client);
    }

    private static final Map<String, Class<? extends Command>> _commands = new TreeMap();

    static {
        // must be lower case here
        _commands.put("changen", ChanGen.class);
        _commands.put("chanlist", ChanList.class);
        _commands.put("ctrlserv", ControlServer.class);
        _commands.put("httpserv", HTTPServ.class);
        _commands.put("import", Importer.class);
        _commands.put("keygen", KeyGen.class);
        _commands.put("keyimport", KeyImport.class);
        _commands.put("keylist", KeyList.class);
        _commands.put("messageextract", MessageExtract.class);
        _commands.put("messagegen", MessageGen.class);
        _commands.put("messagelist", MessageList.class);
        _commands.put("register", LoginManager.class);
        _commands.put("viewmessage", ViewMessage.class);
        _commands.put("viewmetadata", ViewMetadata.class);
    }
    
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
        name = name.toLowerCase(Locale.US);
        Class<? extends Command> cls = _commands.get(name);
        if (cls != null) {
            try {
                return cls.newInstance();
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
    public synchronized static void setCommand(String name, Class<Command> cmdClass) {
        name = name.toLowerCase(Locale.US);
        _commands.put(name, cmdClass);
    }

    /** sorted */
    public synchronized static List<String> getCommands() {
        return new ArrayList(_commands.keySet());
    }

    /** sorted */
    public synchronized static List<String> getHelp() {
        List<String> rv = new ArrayList(_commands.size());
        for (Map.Entry<String, Class<? extends Command>> e : _commands.entrySet()) {
            String cmd = e.getKey();
            Class cls = e.getValue();
            try {
                Method m = cls.getMethod("getHelp", String.class);
                String help = (String) m.invoke(null, cmd);
                rv.add(cmd + ' ' + help);
            } catch (Exception exc) {
                rv.add(cmd);
            }
        }
        return rv;
    }

    private synchronized static final void usage() {
        System.err.println("Usage: $command [$args]*");
        System.err.print("Known commands: ");
        for (String msg : getHelp())
            System.err.print(msg);
        System.err.println();
    }
}
