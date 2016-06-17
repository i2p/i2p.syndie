package syndie.db;

import java.io.*;
import java.sql.SQLException;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import net.i2p.I2PAppContext;
import net.i2p.data.Hash;
import net.i2p.util.SecureFile;
import net.i2p.util.SecureFileOutputStream;

import syndie.Constants;
import syndie.data.NymKey;

/**
 * register 
 *  --db $jdbcURL
 *  --login $nymLogin
 *  --pass $nymPass
 *  --name $nymName
 *  [--root $dir]
 *  [--simple $boolean] // implies that successful registration should be followed by changen & keyimport, allowing all of their args on the cli
 */
public class LoginManager extends CommandImpl {
    private DBClient _client;

    public LoginManager(DBClient client) { _client = client; }

    /** for CLI */
    LoginManager() {}
    
    public static String getHelp(String cmd) {
        return "--login $nymLogin --pass $nymPass --name $nymName";
    }

    public DBClient runCommand(Opts args, UI ui, DBClient client) {
        if ( (client == null) || (!client.isLoggedIn()) ) {
            List missing = args.requireOpts(new String[] { "db", "login", "pass", "name" });
            if (missing.size() > 0) {
                ui.errorMessage("Usage: register [--db $jdbcURL] --login $nym --pass $password --name $publicName [--simple $boolean]");
                ui.errorMessage("The JDBC URL can be an in-memory database (e.g. jdbc:hsqldb:mem:test),");
                ui.errorMessage("an on-disk database (jdbc:hsqldb:file:/some/path), ");
                ui.errorMessage("a remote database (jdbc:hsqldb:hsql:hostname:port:dbName), or ");
                ui.errorMessage("any other JDBC database URL");
                ui.errorMessage("The nym and password refer to the syndie-specific nym, not to the database");
                ui.errorMessage("The name is the publicly visible name of the nym (in their created blog)");
                ui.errorMessage("If simple is true (it is by default), it automatically creates a blog for the new nym,");
                ui.errorMessage("and imports all of the appropriate keys for the nym's account.  If it is false, it simply");
                ui.errorMessage("creates the nym but without any channels or keys.");
                ui.debugMessage("you have: " + args);
                ui.commandComplete(-1, null);
                return client;
            }
        } else {
            List missing = args.requireOpts(new String[] { "login", "pass", "name" });
            if (missing.size() > 0) {
                ui.errorMessage("Usage: register [--db $jdbcURL] --login $nym --pass $password --name $publicName [--simple $boolean]");
                ui.errorMessage("The JDBC URL can be an in-memory database (e.g. jdbc:hsqldb:mem:test),");
                ui.errorMessage("an on-disk database (jdbc:hsqldb:file:/some/path), ");
                ui.errorMessage("a remote database (jdbc:hsqldb:hsql:hostname:port:dbName), or ");
                ui.errorMessage("any other JDBC database URL");
                ui.errorMessage("The nym and password refer to the syndie-specific nym, not to the database");
                ui.errorMessage("The name is the publicly visible name of the nym (in their created blog)");
                ui.errorMessage("If simple is true (it is by default), it automatically creates a blog for the new nym,");
                ui.errorMessage("and imports all of the appropriate keys for the nym's account.  If it is false, it simply");
                ui.errorMessage("creates the nym but without any channels or keys.");
                ui.commandComplete(-1, null);
                return client;
            }
        }
        
        try {
            if (args.dbOptsSpecified()) {
                if (client == null) {
                    String root = args.getOptValue("root");
                    if (root == null)
                        root = TextEngine.getRootPath();
                    client = new DBClient(I2PAppContext.getGlobalContext(), new SecureFile(root));
                    client.connect(args.getOptValue("db"));
                } else {
                    //client.close();
                }
            }
            long nymId = client.register(args.getOptValue("login"), args.getOptValue("pass"), args.getOptValue("name"));
            if (DBClient.NYM_ID_LOGIN_ALREADY_EXISTS == nymId) {
                ui.errorMessage("Login already exists");
                ui.commandComplete(-1, null);
                return client;
            }
        } catch (SQLException se) {
            ui.errorMessage("Invalid database URL", se);
            ui.commandComplete(-1, null);
            return client;
        //} finally {
        //    if (client != null) client.close();
        }
        
        ui.statusMessage("Local nym created for " + args.getOptValue("login"));
        
        if (args.getOptBoolean("simple", true)) {
            String login = client.getLogin();
            String pass = client.getPass();
            // log in to the new nym
            long newId = client.getNymId(args.getOptValue("login"), args.getOptValue("pass"));
            boolean ok = processSimple(args, client, ui);
            if (ok) {
                client.setNymPrefs(newId, client.getDefaultPrefs());
                Properties aliases = client.getDefaultAliases();
                for (Iterator iter = aliases.keySet().iterator(); iter.hasNext(); ) {
                    String name = (String)iter.next();
                    String val = aliases.getProperty(name);
                    client.addAlias(newId, name, val);
                }
                // Store the default author into nym preferences early on,
                // preventing an NPE later (specifically: when we post our first message
                // with default settings to our own forum).
                long authorId = client.getNymChannels().getIdentityChannelIds().get(0).longValue();
                Hash author = client.getChannelHash(authorId);
                Properties prefs = client.getNymPrefs();
                prefs.setProperty("editor.defaultAuthor", author.toBase64());
                client.setNymPrefs(prefs);
            }
            client.getNymId(login, pass); // relogin to the orig login/pass (not the newly registered one)
            if (!ok)
                return client;
        }
        
        ui.commandComplete(0, null);
        return client;
    }
    
    private static final boolean DELETE = true;
    
    private boolean processSimple(Opts args, DBClient client, UI ui) {
        boolean loggedIn = client.isLoggedIn();
        File tmpDir = client.getTempDir(); //"~/.syndie/tmp/" + client.getLogin());
        if (!tmpDir.exists()) tmpDir.mkdirs();
        File metaOutFile = new File(tmpDir, "metaOut");
        File manageOutFile = new File(tmpDir, "manageOut");
        File replyOutFile = new File(tmpDir, "replyOut");

        ChanGen cmd = new ChanGen();
        Opts changenOpts = new Opts(args);
        changenOpts.addOptValue("pubTag", "blog");
        if (changenOpts.getOptValue("metaOut") != null)
            metaOutFile = new File(changenOpts.getOptValue("metaOut"));
        else
            changenOpts.addOptValue("metaOut", metaOutFile.getPath());

        if (changenOpts.getOptValue("keyManageOut") != null)
            manageOutFile = new File(changenOpts.getOptValue("keyManageOut"));
        else
            changenOpts.addOptValue("keyManageOut", manageOutFile.getPath());

        if (changenOpts.getOptValue("keyReplyOut") != null)
            replyOutFile = new File(changenOpts.getOptValue("keyReplyOut"));
        else
            changenOpts.addOptValue("keyReplyOut", replyOutFile.getPath());
        changenOpts.setCommand("changen");
        NestedUI nestedUI = new NestedUI(ui);
        client = cmd.runCommand(changenOpts, nestedUI, client);
        if (nestedUI.getExitCode() < 0) {
            ui.debugMessage("Failed in the nested changen command");
            ui.commandComplete(nestedUI.getExitCode(), null);
            return false;
        }

        ui.debugMessage("Channel created for the nym");

        if (metaOutFile.exists()) {
            // generated correctly, import the metadata and private keys
            Importer msgImp = new Importer();
            Opts msgImpOpts = new Opts(); // $dbURL $login $password $filenameToImport
            //msgImpOpts.setOptValue("db", args.getOptValue("db"));
            //msgImpOpts.setOptValue("login", args.getOptValue("login"));
            //msgImpOpts.setOptValue("pass", args.getOptValue("pass"));
            msgImpOpts.setOptValue("in", metaOutFile.getPath());
            msgImpOpts.setCommand("import");
            nestedUI = new NestedUI(ui);
            client = msgImp.runCommand(msgImpOpts, nestedUI, client);
            if (nestedUI.getExitCode() < 0) {
                ui.debugMessage("Failed in the nested import command (logged in? " + client.isLoggedIn() + "/" + loggedIn + ")");
                ui.commandComplete(nestedUI.getExitCode(), null);
                return false;
            }
            ui.debugMessage("Blog channel metadata imported");

            KeyImport imp = new KeyImport();
            Opts impOpts = new Opts(); //args);
            impOpts.setOptValue("keyfile", manageOutFile.getPath());
            impOpts.setOptValue("authentic", "true");
            impOpts.setCommand("keyimport");
            nestedUI = new NestedUI(ui);
            client = imp.runCommand(impOpts, nestedUI, client);
            if (DELETE)
                manageOutFile.delete();
            if (nestedUI.getExitCode() < 0) {
                ui.debugMessage("Failed in the nested management key import command");
                ui.commandComplete(nestedUI.getExitCode(), null);
                return false;
            }
            ui.debugMessage("Blog channel management key imported");

            impOpts = new Opts(); //args);
            impOpts.setOptValue("keyfile", replyOutFile.getPath());
            impOpts.setOptValue("authentic", "true");
            impOpts.setCommand("keyimport");
            nestedUI = new NestedUI(ui);
            client = imp.runCommand(impOpts, nestedUI, client);
            if (DELETE)
                replyOutFile.delete();
            if (nestedUI.getExitCode() < 0) {
                ui.debugMessage("Failed in the nested reply key import command");
                ui.commandComplete(nestedUI.getExitCode(), null);
                return false;
            }
            ui.debugMessage("Blog chanel reply key imported");
            
            Hash chan = null;
            List keys = client.getNymKeys(client.getLoggedInNymId(), client.getPass(), null, Constants.KEY_FUNCTION_MANAGE);
            if (keys.size() > 0) {
                NymKey key = (NymKey)keys.get(0);
                chan = key.getChannel();
            }
            if (chan != null) {
                File channelDir = new SecureFile(client.getOutboundDir(), chan.toBase64());
                File meta = new File(channelDir, "meta" + Constants.FILENAME_SUFFIX);
                FileInputStream fis = null;
                FileOutputStream fos = null;
                try {
                    channelDir.mkdirs();
                    fis = new FileInputStream(metaOutFile);
                    fos = new SecureFileOutputStream(meta);
                    byte buf[] = new byte[4096];
                    int read = -1;
                    while ( (read = fis.read(buf)) != -1)
                        fos.write(buf, 0, read);
                    fos.close();
                    fis.close();
                    fis = null;
                    fos = null;
                    metaOutFile.delete();
                    ui.statusMessage("Sharable channel metadata saved to " + metaOutFile.getPath());
                } catch (IOException ioe) {
                    ui.errorMessage("Error migrating the metadata file to the output dir", ioe);
                } finally {
                    if (fis != null) try { fis.close(); } catch (IOException ioe) {}
                    if (fos != null) try { fos.close(); } catch (IOException ioe) {}
                }
            }
        }
        return true;
    }
    
/****
    public static void main(String args[]) {
        if ( (args == null) || (args.length == 0) )
            args = new String[] { "nymgen", "jdbc:hsqldb:mem:test", "jr", "jrPass", "jay arr" };
        if ( (args == null) || (args.length != 5) )
            throw new RuntimeException("Usage: LoginManager nymgen $dbURL $login $password $publicName");

        DBClient client = null;
        try {
            client = new DBClient(I2PAppContext.getGlobalContext(), new SecureFile(TextEngine.getRootPath()));
            client.connect(args[1]);
            long nymId = client.register(args[2], args[3], args[4]);
            if (DBClient.NYM_ID_LOGIN_ALREADY_EXISTS == nymId)
                throw new RuntimeException("Login already exists");
            else
                System.out.println("Registered as nymId " + nymId);
        } catch (SQLException se) {
            throw new RuntimeException("Invalid database URL: " + se.getMessage(), se);
        } finally {
            if (client != null) client.close();
        }
    }
****/
}
