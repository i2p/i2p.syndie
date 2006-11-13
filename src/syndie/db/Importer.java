package syndie.db;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.sql.SQLException;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import net.i2p.I2PAppContext;
import net.i2p.data.Hash;
import net.i2p.data.Signature;
import net.i2p.data.SigningPublicKey;
import syndie.Constants;
import syndie.data.Enclosure;

/**
 * Import a message for the user, using the keys known to that user and
 * storing the data in the database they can access.
 * CLI import
 * --db $dbURL
 * --login $login
 * --pass $pass
 * --in $filename
 * [--reimport $boolean]
 * [--passphrase $bodyPassphrase]
 */
public class Importer extends CommandImpl {
    private DBClient _client;
    private String _passphrase;
    private boolean _wasPBE;
    
    public Importer(DBClient client, String pass) {
        _client = client;
        _passphrase = pass;
        _wasPBE = false;
    }
    public Importer() {}
    public DBClient runCommand(Opts args, UI ui, DBClient client) {
        if ( (client == null) || (!client.isLoggedIn()) ) {
            List missing = args.requireOpts(new String[] { "db", "login", "pass", "in" });
            if (missing.size() > 0) {
                ui.errorMessage("Invalid options, missing " + missing);
                ui.commandComplete(-1, null);
                return client;
            }
        } else {
            List missing = args.requireOpts(new String[] { "in" });
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
                else
                    client.close();
                client.connect(args.getOptValue("db"));
                nymId = client.getNymId(args.getOptValue("login"), args.getOptValue("pass"));
                if (DBClient.NYM_ID_LOGIN_UNKNOWN == nymId) {
                    ui.errorMessage("Unknown login '" + args.getOptValue("login") + "'");
                    ui.commandComplete(-1, null);
                    return client;
                } else if (DBClient.NYM_ID_PASSPHRASE_INVALID == nymId) {
                    ui.errorMessage("Invalid passphrase");
                    ui.commandComplete(-1, null);
                    return client;
                }
            } else {
                nymId = client.getLoggedInNymId();
                if (nymId < 0) {
                    ui.errorMessage("Login details required");
                    ui.commandComplete(-1, null);
                    return client;
                }
            }
            
            File file = new File(args.getOptValue("in"));
            if (!file.isFile()) {
                ui.errorMessage("File does not exist");
                ui.commandComplete(-1, null);
                return client;
            }
            
            _client = client;
            _passphrase = client.getPass();
            boolean ok = processMessage(ui, new FileInputStream(file), nymId, client.getPass(), args.getOptValue("passphrase"), args.getOptBoolean("reimport", false));
            ui.debugMessage("Metadata processed");
            if (!ok) // successful imports specify whether they were decrypted (exit code of 0) or undecryptable (exit code of 1)
                ui.commandComplete(-1, null);
        } catch (SQLException se) {
            ui.errorMessage("Invalid database URL", se);
            ui.commandComplete(-1, null);
        } catch (IOException ioe) {
            ui.errorMessage("Error importing the message", ioe);
            ui.commandComplete(-1, null);
        //} finally {
        //    if (client != null) client.close();
        }
        return client;
    }
    
    public static void main(String args[]) {
        try {
        CLI.main(new String[] { "import", 
                                "--db", "jdbc:hsqldb:file:/tmp/cli",
                                "--login", "j",
                                "--pass", "j",
                                "--in", "/tmp/metaOut" });
        } catch (Exception e) { e.printStackTrace(); }
    }
    
    public static void omain(String args[]) {
        if ( (args == null) || (args.length != 4) )
            throw new RuntimeException("Usage: Importer $dbURL $login $password $filenameToImport");
        DBClient client = null;
        try {
            client = new DBClient(I2PAppContext.getGlobalContext(), new File(TextEngine.getRootPath()));
            client.connect(args[0]);
            long nymId = client.getNymId(args[1], args[2]);
            if (DBClient.NYM_ID_LOGIN_UNKNOWN == nymId)
                throw new RuntimeException("Unknown login");
            else if (DBClient.NYM_ID_PASSPHRASE_INVALID == nymId)
                throw new RuntimeException("Invalid passphrase");
            
            File file = new File(args[3]);
            if (!file.isFile())
                throw new RuntimeException("File does not exist");
            
            Importer imp = new Importer(client, args[2]);
            //imp.processMessage(new FileInputStream(file), nymId, args[2]);
        } catch (SQLException se) {
            throw new RuntimeException("Invalid database URL: " + se.getMessage(), se);
        } finally {
            if (client != null) client.close();
        }
    }
    
    /** 
     * process the message, importing it if possible.  If it was imported but
     * could not be decrypted (meaning that it is authentic and/or authorized),
     * it will fire ui.commandComplete with an exit value of 1.  if it was imported
     * and read, it will fire ui.commandComplete with an exit value of 0.  otherwise,
     * it will not fire an implicit ui.commandComplete.
     */
    public boolean processMessage(UI ui, InputStream source, long nymId, String pass, String bodyPassphrase, boolean forceReimport) throws IOException {
        if (bodyPassphrase != null)
            ui.debugMessage("Processing message with body passphrase " + bodyPassphrase);
        else
            ui.debugMessage("Processing message with no body passphrase");
        _wasPBE = false;
        boolean rv = true;
        boolean isMeta = false;
        Enclosure enc = new Enclosure(source);
        try {
            String format = enc.getEnclosureType();
            if (format == null) {
                throw new IOException("No enclosure type");
            } else if (!format.startsWith(Constants.TYPE_PREFIX)) {
                throw new IOException("Unsupported enclosure format: " + format);
            }
            _wasPBE = (enc.getHeaderString(Constants.MSG_HEADER_PBE_PROMPT) != null);
            
            String type = enc.getHeaderString(Constants.MSG_HEADER_TYPE);
            if (Constants.MSG_TYPE_META.equals(type)) { // validate and import metadata message
                rv = importMeta(ui, enc, nymId, bodyPassphrase);
                isMeta = true;
            } else if (Constants.MSG_TYPE_POST.equals(type)) { // validate and import content message
                rv = importPost(ui, enc, nymId, pass, bodyPassphrase, forceReimport);
            } else if (Constants.MSG_TYPE_REPLY.equals(type)) { // validate and import reply message
                rv = importPost(ui, enc, nymId, pass, bodyPassphrase, forceReimport);
            } else {
                throw new IOException("Invalid message type: " + type);
            }
        } finally {
            enc.discardData();
        }
        return rv;
    }
    /** was the last message processed encrypted with a passphrase? */
    public boolean wasPBE() { return _wasPBE; }
    
    protected boolean importMeta(UI ui, Enclosure enc, long nymId, String bodyPassphrase) {
        // first check that the metadata is signed by an authorized key
        if (verifyMeta(ui, enc)) {
            return ImportMeta.process(_client, ui, enc, nymId, _passphrase, bodyPassphrase);
        } else {
            ui.errorMessage("meta does not verify");
            return false;
        }
    }
    /**
     * The metadata message is ok if it is either signed by the channel's
     * identity itself or by one of the manager keys
     */
    private boolean verifyMeta(UI ui, Enclosure enc) {
        SigningPublicKey pubKey = enc.getHeaderSigningKey(Constants.MSG_META_HEADER_IDENTITY);
        Signature sig = enc.getAuthorizationSig();
        boolean ok = verifySig(_client, sig, enc.getAuthorizationHash(), pubKey);
        if (!ok) {
            ui.debugMessage("authorization hash does not match identity (authHash: " + enc.getAuthorizationHash().toBase64() + " sig: " + sig.toBase64() + ")");
            SigningPublicKey pubKeys[] = enc.getHeaderSigningKeys(Constants.MSG_META_HEADER_MANAGER_KEYS);
            if (pubKeys != null) {
                for (int i = 0; i < pubKeys.length; i++) {
                    if (verifySig(_client, sig, enc.getAuthorizationHash(), pubKeys[i])) {
                        ui.debugMessage("authorization hash matches a manager key");
                        ok = true;
                        break;
                    } else {
                        ui.debugMessage("authorization hash does not match manager key " + pubKeys[i].toBase64());
                    }
                }
            }
        } else {
            ui.debugMessage("authorization hash matches");
            boolean authenticated = verifySig(_client, enc.getAuthenticationSig(), enc.getAuthenticationHash(), pubKey);
            if (authenticated)
                ui.debugMessage("authentication hash matches");
            else
                ui.debugMessage("authentication hash does not match the identity, but that's alright");
        }
        return ok;
    }
    
    protected boolean importPost(UI ui, Enclosure enc, long nymId, String pass, String bodyPassphrase, boolean forceReimport) {
        return ImportPost.process(_client, ui, enc, nymId, pass, bodyPassphrase, forceReimport);
    }
}
