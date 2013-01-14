package syndie.db;

import java.io.*;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;

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

import syndie.Constants;
import syndie.data.NymKey;
import syndie.data.SyndieURI;

/**
 *CLI keyimport
 * --db $dbURL
 * --login $login
 * --pass $pass
 * --keyfile $keyFile      // keytype: (manage|reply|read)\nscope: $base64(channelHash)\nraw: $base64(data)\n
 * [--authentic $boolean]
 * [--expireExisting $boolean] // if true, expire all other existing keys of the same type for the scope
 */
public class KeyImport extends CommandImpl {

    public KeyImport() {}

    public DBClient runCommand(Opts args, UI ui, DBClient client) {
        if ( (client == null) || (!client.isLoggedIn()) ) {
            List missing = args.requireOpts(new String[] { "db", "login", "pass", "keyfile" });
            if (missing.size() > 0) {
                ui.errorMessage("Invalid options, missing " + missing);
                ui.commandComplete(-1, null);
                return client;
            }
        }
        
        String db = args.getOptValue("db");
        String login = args.getOptValue("login");
        String pass = args.getOptValue("pass");
        String keyFile = args.getOptValue("keyfile");
        boolean authentic = args.getOptBoolean("authentic", false);
        boolean expireExisting = args.getOptBoolean("expireExisting", false);
        
        return importKey(ui, client, db, login, pass, keyFile, authentic, expireExisting);
    }
    
    private DBClient importKey(UI ui, DBClient client, String db, String login, String pass, String keyFile, boolean authentic, boolean expireExisting) {
        File f = new File(keyFile);
        if (!f.exists()) {
            ui.errorMessage("Key file does not exist: " + keyFile);
            ui.commandComplete(-1, null);
            return client;
        }
        FileInputStream fin = null;
        try {
            fin = new FileInputStream(f);
            return importKey(ui, client, db, login, pass, fin, authentic, expireExisting);
        } catch (IOException ioe) {
            ui.errorMessage("Error importing the key", ioe);
            ui.commandComplete(-1, null);
            return client;
        } finally {
            if (fin != null) try { fin.close(); } catch (IOException ioe) {}
        }
    }

    public static DBClient importKey(UI ui, DBClient client, InputStream fin, boolean authentic, boolean expireExisting) throws IOException {
        return importKey(ui, client, null, null, null, fin, authentic, expireExisting);
    }

    public static DBClient importKey(UI ui, DBClient client, String db, String login, String pass, InputStream fin, boolean authentic, boolean expireExisting) throws IOException {
        String line = DataHelper.readLine(fin);
        if (!line.startsWith("keytype: ") || (line.length() < ("keytype: ".length() + 1)))
            throw new IOException("Invalid type line: " + line);
        String type = line.substring("keytype: ".length()).trim();

        line = DataHelper.readLine(fin);
        if (!line.startsWith("scope: ") || (line.length() < ("scope: ".length() + 1)))
            throw new IOException("Invalid scope line: " + line);
        String scope = line.substring("scope: ".length()).trim();

        line = DataHelper.readLine(fin);
        if (!line.startsWith("raw: ") || (line.length() < ("raw: ".length() + 1)))
            throw new IOException("Invalid raw line: " + line);
        String raw = line.substring("raw: ".length()).trim();

        byte scopeData[] = Base64.decode(scope);
        if ( (scopeData != null) && (scopeData.length != Hash.HASH_LENGTH) )
            scopeData = null;
        byte rawData[] = Base64.decode(raw);

        //ui.debugMessage("importing from " + f.getPath() +": type=" + type + " scope=" + scope + " raw=" + raw);
        client = importKey(ui, client, db, login, pass, type, new Hash(scopeData), rawData, authentic, expireExisting);
        fin = null;
        return client;
    }
    
    private static final String SQL_INSERT_KEY = "INSERT INTO nymKey " +
                                                 "(nymId, keyChannel, keyFunction, keyType, keyData, keySalt, authenticated, keyPeriodBegin, keyPeriodEnd)" +
                                                 " VALUES " +
                                                 "(?, ?, ?, ?, ?, ?, ?, NULL, NULL)";
    private static final String SQL_EXPIRE = "UPDATE nymKey SET keyPeriodEnd = NOW() WHERE nymId = ? AND keyChannel = ? and keyFunction = ?";

    public static DBClient importKey(UI ui, DBClient client, String type, Hash scope, byte[] raw, boolean authenticated, boolean expireExisting) {
        return importKey(ui, client, null, null, null, type, scope, raw, authenticated, expireExisting);
    }

    public static DBClient importKey(UI ui, DBClient client, String db, String login, String pass, String type, Hash scope, byte[] raw, boolean authenticated, boolean expireExisting) {
        client.clearNymChannelCache();
        PreparedStatement stmt = null;
        try {
            long nymId = -1;
            if ( (db != null) && (login != null) && (pass != null) ) {
                if (client == null)
                    client = new DBClient(I2PAppContext.getGlobalContext(), new File(TextEngine.getRootPath()));
                else
                    client.close();
                client.connect(db);
                nymId = client.getNymId(login, pass);
            } else if (client != null) {
                nymId = client.getLoggedInNymId();
                pass = client.getPass();
            }
            if (nymId == -1)
                throw new SQLException("Login unknown");
            else if (nymId == -2)
                throw new SQLException("Password invalid");

            List existing = client.getNymKeys(nymId, pass, scope, type);
            for (int i = 0; i < existing.size(); i++) {
                NymKey cur = (NymKey)existing.get(i);
                if (DataHelper.eq(cur.getData(), raw)) {
                    ui.statusMessage("Key already imported (type: " + type + ", " + cur.getFunction() + "/" 
                                     + cur.getType() + " raw.length=" + raw.length + ", " + cur.getData().length);
                    //ui.commandComplete(0, null);
                    return client;
                }
            }
            
            // wait until after the above already-exists check
            if (expireExisting) {
                Connection con = client.con();
                //"UPDATE nymKey SET keyPeriodEnd = NOW() WHERE nymId = ? AND keyChannel = ? and keyFunction = ?";
                stmt = con.prepareStatement(SQL_EXPIRE);
                stmt.setLong(1, nymId);
                stmt.setBytes(2, scope.getData());
                stmt.setString(3, type);
                stmt.executeUpdate();
                stmt.close();
                stmt = null;
            }
            
            if (Constants.KEY_FUNCTION_MANAGE.equals(type) || Constants.KEY_FUNCTION_POST.equals(type)) {
                SigningPrivateKey priv = new SigningPrivateKey(raw);
                SigningPublicKey pub = KeyGenerator.getSigningPublicKey(priv);
                if (pub.calculateHash().equals(scope)) {
                    ui.statusMessage("Importing an identity key for " + scope.toBase64());
                } else {
                    ui.debugMessage("Importing a key that is NOT an identity key for " + scope.toBase64() + "?");
                    ui.debugMessage("calculated pub: " + pub.calculateHash().toBase64());
                    ui.debugMessage("aka " + pub.toBase64());
                }
            }
            
            byte salt[] = new byte[16]; // overwritten by pbeEncrypt
            byte encrypted[] = client.pbeEncrypt(raw, salt);
            
            Connection con = client.con();
            stmt = con.prepareStatement(SQL_INSERT_KEY);
            stmt.setLong(1, nymId);
            stmt.setBytes(2, scope.getData());
            stmt.setString(3, type);
            if (Constants.KEY_FUNCTION_READ.equals(type))
                stmt.setString(4, Constants.KEY_TYPE_AES256);
            else if (Constants.KEY_FUNCTION_MANAGE.equals(type))
                stmt.setString(4, Constants.KEY_TYPE_DSA);
            else if (Constants.KEY_FUNCTION_POST.equals(type))
                stmt.setString(4, Constants.KEY_TYPE_DSA);
            else if (Constants.KEY_FUNCTION_REPLY.equals(type))
                stmt.setString(4, Constants.KEY_TYPE_ELGAMAL2048);
            
            stmt.setBytes(5, encrypted);
            stmt.setBytes(6, salt);
            stmt.setBoolean(7, authenticated);
            int rows = stmt.executeUpdate();
            if (rows == 1) {
                ui.statusMessage("Keys imported (type " + type + " scope " + scope.toBase64() + " hash:" + client.ctx().sha().calculateHash(raw).toBase64() + " rows " + rows + ")");
            } else {
                throw new SQLException("Error importing keys: row count of " + rows);
            }
            con.commit();
            stmt.close();
            stmt = null;
            
            ui.commandComplete(0, null);
        } catch (SQLException se) {
            ui.errorMessage("Error importing the key", se);
            ui.commandComplete(-1, null);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }

        return client;
    }
    
    public static void importKeys(UI ui, DBClient client, Hash keyScope, SyndieURI uri, List nymKeys) {
        if (uri == null)
            return;
        if (uri.getReadKey() != null)
            importReadKey(ui, client, keyScope, uri, nymKeys);
        if (uri.getReplyKey() != null)
            importReplyKey(ui, client, keyScope, uri, nymKeys);
        if (uri.getPostKey() != null)
            importPostKey(ui, client, keyScope, uri, nymKeys);
        if (uri.getManageKey() != null)
            importManageKey(ui, client, keyScope, uri, nymKeys);
    }
    
    private static void importReadKey(UI ui, DBClient client, Hash scope, SyndieURI uri, List nymKeys) {
        boolean authorized = getKeyAuthorized(client, scope, uri.getScope());
        if (authorized) {
            KeyImport.importKey(ui, client, Constants.KEY_FUNCTION_READ, scope, uri.getReadKey().getData(), true, false);
            nymKeys.add(new NymKey(Constants.KEY_TYPE_AES256, uri.getReadKey().getData(), true, Constants.KEY_FUNCTION_READ, client.getLoggedInNymId(), scope, false));
        }
    }
    
    private static void importReplyKey(UI ui, DBClient client, Hash scope, SyndieURI uri, List nymKeys) {
        boolean authorized = getKeyAuthorized(client, scope, uri.getScope());
        if (authorized) {
            KeyImport.importKey(ui, client, Constants.KEY_FUNCTION_REPLY, scope, uri.getReplyKey().getData(), true, false);
            nymKeys.add(new NymKey(Constants.KEY_TYPE_ELGAMAL2048, uri.getReplyKey().getData(), true, Constants.KEY_FUNCTION_REPLY, client.getLoggedInNymId(), scope, false));
        }
    }
    
    private static void importPostKey(UI ui, DBClient client, Hash scope, SyndieURI uri, List nymKeys) {
        SigningPrivateKey priv = uri.getPostKey();
        SigningPublicKey pub = priv.toPublic();
        importKey(ui, client, Constants.KEY_FUNCTION_MANAGE, pub.calculateHash(), priv.getData(), true, false);
        nymKeys.add(new NymKey(Constants.KEY_TYPE_DSA, priv.getData(), true, Constants.KEY_FUNCTION_MANAGE, client.getLoggedInNymId(), pub.calculateHash()));
        if (getKeyAuthorized(client, scope, uri.getScope())) {
            // if the author is authorized to manage the scope, also tag this key as a posting key for the given channel
            KeyImport.importKey(ui, client, Constants.KEY_FUNCTION_POST, scope, priv.getData(), true, false);
            nymKeys.add(new NymKey(Constants.KEY_TYPE_DSA, priv.getData(), true, Constants.KEY_FUNCTION_POST, client.getLoggedInNymId(), scope, false));
        }
    }
    
    private static void importManageKey(UI ui, DBClient client, Hash scope, SyndieURI uri, List nymKeys) {
        SigningPrivateKey priv = uri.getManageKey();
        SigningPublicKey pub = priv.toPublic();
        importKey(ui, client, Constants.KEY_FUNCTION_MANAGE, pub.calculateHash(), priv.getData(), true, false);
        nymKeys.add(new NymKey(Constants.KEY_TYPE_DSA, priv.getData(), true, Constants.KEY_FUNCTION_MANAGE, client.getLoggedInNymId(), pub.calculateHash()));
        if (getKeyAuthorized(client, scope, uri.getScope())) {
            // if the author is authorized to manage the scope, also tag this key as a managing key for the given channel
            KeyImport.importKey(ui, client, Constants.KEY_FUNCTION_MANAGE, scope, priv.getData(), true, false);
            nymKeys.add(new NymKey(Constants.KEY_TYPE_DSA, priv.getData(), true, Constants.KEY_FUNCTION_MANAGE, client.getLoggedInNymId(), scope, false));
        }
    }
    
    private static boolean getKeyAuthorized(DBClient client, Hash scope, Hash author) {
        // now lets make sure the author of the post is authorized to define reply keys
        boolean authorized = false;
        if (author.equals(scope)) {
            authorized = true;
        } else {
            List pubKeys = client.getAuthorizedPosters(client.getChannelId(scope), false, true, false);
            for (int i = 0; i < pubKeys.size(); i++) {
                SigningPublicKey key = (SigningPublicKey)pubKeys.get(i);
                Hash calc = key.calculateHash();
                if (calc.equals(author)) {
                    authorized = true;
                    break;
                }
            }
        }
        return authorized;
    }
    
    /**
     * we may have received keys that can decrypt previously undecryptable messages / metadata, 
     * so try to resolve those now.  these nymKeys can come from posts as part of their references,
     * from metadata as part of their references, or from metadata as published read keys.
     */
    public static void resolveWithNewKeys(UI ui, DBClient client, List nymKeys) {
        for (int i = 0; i < nymKeys.size(); i++) {
            NymKey key = (NymKey)nymKeys.get(i);
            if (Constants.KEY_FUNCTION_MANAGE.equals(key.getFunction())) {
                // noop.  manage keys don't decrypt anything
            } else if (Constants.KEY_FUNCTION_POST.equals(key.getFunction())) {
                // noop.  same with post keys
            } else if (Constants.KEY_FUNCTION_READ.equals(key.getFunction())) {
                resolveWithReadKey(ui, client, key.getChannel(), new SessionKey(key.getData()));
            } else if (Constants.KEY_FUNCTION_REPLY.equals(key.getFunction())) {
                resolveWithReplyKey(ui, client, key.getChannel(), new PrivateKey(key.getData()));
            }
        }
    }
    
    private static final String SQL_GET_UNDECRYPTABLE_READ = "SELECT msgId, messageId, channelHash FROM channelMessage JOIN channel ON channelId = scopeChannelId WHERE readKeyMissing = TRUE AND scopeChannelId = ?";

    private static void resolveWithReadKey(UI ui, DBClient client, Hash channel, SessionKey key) {
        Connection con = client.con();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        ArrayList uris = new ArrayList();
        long channelId = client.getChannelId(channel);
        try {
            stmt = con.prepareStatement(SQL_GET_UNDECRYPTABLE_READ);
            stmt.setLong(1, channelId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                long msgId = rs.getLong(1);
                if (rs.wasNull()) continue;
                long messageId = rs.getLong(2);
                if (rs.wasNull()) continue;
                byte chanHash[] = rs.getBytes(3);
                if ( (chanHash == null) || (chanHash.length != Hash.HASH_LENGTH) ) continue;
                SyndieURI uri = SyndieURI.createMessage(new Hash(chanHash), messageId);
                if (!uris.contains(uri))
                    uris.add(uri);
            }
        } catch (SQLException se) {
            ui.errorMessage("Internal error resolving imported keys", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        ui.debugMessage("Messages pending decryption in the newly updated channel: " + uris);
        if (uris.size() > 0) {
            for (int i = 0; i < uris.size(); i++) {
                SyndieURI uri = (SyndieURI)uris.get(i);
                File chanDir = new File(client.getArchiveDir(), uri.getScope().toBase64());
                File msgFile = new File(chanDir, uri.getMessageId().longValue() + Constants.FILENAME_SUFFIX);
                if (msgFile.exists()) {
                    // lets try to import 'er
                    Importer imp = new Importer(client);
                    FileInputStream fin = null;
                    try {
                        fin = new FileInputStream(msgFile);
                        boolean ok = imp.processMessage(ui, client, fin, null, true, null, null);
                        if (ok) {
                            if (imp.wasMissingKey())
                                ui.debugMessage("Still not able to decrypt " + uri.toString());
                            else
                                ui.debugMessage("Successful decryption with the new channel metadata: " + uri.toString());
                        } else {
                            ui.debugMessage("Still not able to decrypt " + uri.toString());
                        }
                    } catch (IOException ioe) {
                        ui.debugMessage("Still not able to decrypt " + uri.toString());
                    } finally {
                        if (fin != null) try { fin.close(); } catch (IOException ioe) {}
                    }
                } else {
                    ui.debugMessage("We don't have the message file, so not attempting to redecrypt: " + uri.toString());
                }
            }
        }
        
        // ok the new read key may relate to a metadata message for the forum
        String name = client.getChannelName(channelId);
        if (name == null) {
            // undecrypted meta... try to decrypt it
            File chanDir = new File(client.getArchiveDir(), channel.toBase64());
            File metaFile = new File(chanDir, "meta" + Constants.FILENAME_SUFFIX);
            if (metaFile.exists()) {
                // lets try to import 'er
                Importer imp = new Importer(client);
                FileInputStream fin = null;
                try {
                    fin = new FileInputStream(metaFile);
                    boolean ok = imp.processMessage(ui, client, fin, null, true, null, null);
                    if (ok) {
                        if (imp.wasMissingKey())
                            ui.debugMessage("Still not able to decrypt " + channel.toString());
                        else
                            ui.debugMessage("Successful decryption of the channel metadata: " + channel.toString());
                    } else {
                        ui.debugMessage("Still not able to decrypt metadata for " + channel.toString());
                    }
                } catch (IOException ioe) {
                    ui.debugMessage("Still not able to decrypt metadata for " + channel.toString());
                } finally {
                    if (fin != null) try { fin.close(); } catch (IOException ioe) {}
                }
            } else {
                ui.debugMessage("We don't have the metadata file, so not attempting to redecrypt: " + channel.toString());
            }            
        }
    }
    
    private static final String SQL_GET_UNDECRYPTABLE_REPLY = "SELECT msgId, messageId, channelHash FROM channelMessage JOIN channel ON channelId = scopeChannelId WHERE replyKeyMissing = TRUE AND (scopeChannelId = ? OR targetChannelId = ?)";

    private static void resolveWithReplyKey(UI ui, DBClient client, Hash channel, PrivateKey key) {
        Connection con = client.con();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        ArrayList uris = new ArrayList();
        long channelId = client.getChannelId(channel);
        try {
            stmt = con.prepareStatement(SQL_GET_UNDECRYPTABLE_REPLY);
            stmt.setLong(1, channelId);
            stmt.setLong(2, channelId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                long msgId = rs.getLong(1);
                if (rs.wasNull()) continue;
                long messageId = rs.getLong(2);
                if (rs.wasNull()) continue;
                byte chanHash[] = rs.getBytes(3);
                if ( (chanHash == null) || (chanHash.length != Hash.HASH_LENGTH) ) continue;
                SyndieURI uri = SyndieURI.createMessage(new Hash(chanHash), messageId);
                if (!uris.contains(uri))
                    uris.add(uri);
            }
        } catch (SQLException se) {
            ui.errorMessage("Internal error resolving imported keys", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        ui.debugMessage("Messages pending decryption in the newly updated channel: " + uris);
        if (uris.size() > 0) {
            for (int i = 0; i < uris.size(); i++) {
                SyndieURI uri = (SyndieURI)uris.get(i);
                File chanDir = new File(client.getArchiveDir(), uri.getScope().toBase64());
                File msgFile = new File(chanDir, uri.getMessageId().longValue() + Constants.FILENAME_SUFFIX);
                if (msgFile.exists()) {
                    // lets try to import 'er
                    Importer imp = new Importer(client);
                    FileInputStream fin = null;
                    try {
                        fin = new FileInputStream(msgFile);
                        boolean ok = imp.processMessage(ui, client, fin, null, true, null, null);
                        if (ok) {
                            if (imp.wasMissingKey())
                                ui.debugMessage("Still not able to decrypt " + uri.toString());
                            else
                                ui.debugMessage("Successful decryption with the new channel metadata: " + uri.toString());
                        } else {
                            ui.debugMessage("Still not able to decrypt " + uri.toString());
                        }
                    } catch (IOException ioe) {
                        ui.debugMessage("Still not able to decrypt " + uri.toString());
                    } finally {
                        if (fin != null) try { fin.close(); } catch (IOException ioe) {}
                    }
                } else {
                    ui.debugMessage("We don't have the message file, so not attempting to redecrypt: " + uri.toString());
                }
            }
        }    
    }
    
    public static void main(String args[]) {
        try {
        CLI.main(new String[] { "keyimport", 
                                "--db", "jdbc:hsqldb:file:/tmp/cli",
                                "--login", "j", "--pass", "j", 
                                "--keyfile", "/tmp/manageOut" });
        } catch (Exception e) { e.printStackTrace(); }
    }
}
