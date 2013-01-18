package syndie.db;

import java.io.*;
import java.sql.SQLException;
import java.util.*;

import net.i2p.I2PAppContext;
import net.i2p.data.Base64;
import net.i2p.data.DataFormatException;
import net.i2p.data.DataHelper;
import net.i2p.data.SessionKey;
import net.i2p.data.PrivateKey;
import net.i2p.data.PublicKey;
import net.i2p.data.SigningPrivateKey;
import net.i2p.data.SigningPublicKey;
import net.i2p.data.Signature;
import net.i2p.data.Hash;
import net.i2p.util.SecureFile;
import net.i2p.util.SecureFileOutputStream;

import syndie.Constants;
import syndie.data.Enclosure;
import syndie.data.EnclosureBody;
import syndie.data.SyndieURI;

/**
 *CLI messageextract
 * --db $dbURL
 * --login $login
 * --pass $pass
 * --in $filename       // data is read from the given snd file
 * --out $outDirectory  // data is extracted to the given dir
 * [--passphrase $passphrase] // use the passphrase for the PBE key derivation
 */
public class MessageExtract extends CommandImpl {

    public static String getHelp(String cmd) {
        return "--in $filename --out $outDirectory [--passphrase $passphrase]";
    }

    public DBClient runCommand(Opts args, UI ui, DBClient client) {
        if ( (client == null) || (!client.isLoggedIn()) ) {
            List missing = args.requireOpts(new String[] { "db", "login", "pass", "in", "out" });
            if (missing.size() > 0) {
                ui.errorMessage("Invalid options, missing " + missing);
                ui.commandComplete(-1, null);
                return client;
            }
        } else {
            List missing = args.requireOpts(new String[] { "in", "out" });
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
                nymId = client.connect(args.getOptValue("db"), args.getOptValue("login"), args.getOptValue("pass"));
            } else {
                nymId = client.getLoggedInNymId();
            }
            if (nymId < 0) {
                ui.errorMessage("Invalid login");
                ui.commandComplete(-1, null);
            } else {
                extract(client, ui, args, nymId);
            }
        } catch (SQLException se) {
            ui.errorMessage("Invalid database URL", se);
            ui.commandComplete(-1, null);
        } catch (IOException ioe) {
            ui.errorMessage("Error reading the message", ioe);
            ui.commandComplete(-1, null);
        //} finally {
        //    if (client != null) client.close();
        }
        return client;
    }
    
    private void extract(DBClient client, UI ui, Opts args, long nymId) throws SQLException, IOException {
        FileInputStream in = new FileInputStream(args.getOptValue("in"));
        Enclosure enc = new Enclosure(in);
        try {
            String format = enc.getEnclosureType();
            if (format == null) {
                throw new IOException("No enclosure type");
            } else if (!format.startsWith(Constants.TYPE_PREFIX)) {
                throw new IOException("Unsupported enclosure format: " + format);
            }
            
            String type = enc.getHeaderString(Constants.MSG_HEADER_TYPE);
            if (Constants.MSG_TYPE_POST.equals(type)) // validate and import content message
                extractPost(client, ui, enc, nymId, args);
            else if (Constants.MSG_TYPE_REPLY.equals(type)) // validate and import reply message
                extractReply(client, ui, enc, nymId, args);
            else
                throw new IOException("Invalid message type: " + type);
        } finally {
            enc.discardData();
        }
    }

    protected void extractPost(DBClient client, UI ui, Enclosure enc, long nymId, Opts args) throws IOException {
        if (verifyPost(client, enc)) {
            //ImportPost.process(_client, enc, nymId);
            
            EnclosureBody body = null;
            SigningPublicKey ident = enc.getHeaderSigningKey(Constants.MSG_META_HEADER_IDENTITY);
            SessionKey key = enc.getHeaderSessionKey(Constants.MSG_HEADER_BODYKEY);
            if (key != null) {
                try {
                    // decrypt it with that key
                    body = new EnclosureBody(client.ctx(), enc.getData(), enc.getDataSize(), key);
                } catch (DataFormatException dfe) {
                    ui.errorMessage("Error decrypting with the published key", dfe);
                    ui.commandComplete(-1, null);
                    return;
                } catch (IOException ioe) {
                    ui.debugMessage("Error decrypting with the published key", ioe);
                    return;
                }
            } else {
                String prompt = enc.getHeaderString(Constants.MSG_HEADER_PBE_PROMPT);
                byte promptSalt[] = enc.getHeaderBytes(Constants.MSG_HEADER_PBE_PROMPT_SALT);
                if ( (prompt != null) && (promptSalt != null) && (promptSalt.length != 0) ) {
                    String passphrase = args.getOptValue("passphrase");
                    if (passphrase == null) {
                        ui.errorMessage("Passphrase required to extract this message");
                        ui.errorMessage("Please use --passphrase 'passphrase value', where the passphrase value is the answer to:");
                        ui.errorMessage(strip(prompt));
                        ui.commandComplete(-1, null);
                        return;
                    } else {
                        key = client.ctx().keyGenerator().generateSessionKey(promptSalt, DataHelper.getUTF8(passphrase));
                        try {
                            // decrypt it with that key
                            body = new EnclosureBody(client.ctx(), enc.getData(), enc.getDataSize(), key);
                        } catch (DataFormatException dfe) {
                            ui.errorMessage("Invalid passphrase", dfe);
                            ui.commandComplete(-1, null);
                            return;
                        } catch (IOException ioe) {
                            ui.debugMessage("Invalid passphrase", ioe);
                            return;
                        }
                    }
                } else {
                    Hash identHash = ident.calculateHash();
                    List keys = client.getReadKeys(identHash, nymId, client.getPass(), false);
                    byte target[] = enc.getHeaderBytes(Constants.MSG_HEADER_TARGET_CHANNEL);
                    if ( (target != null) && (target.length == Hash.HASH_LENGTH) ) {
                        List targetKeys = client.getReadKeys(Hash.create(target), client.getLoggedInNymId(), client.getPass(), false);
                        keys.addAll(targetKeys);
                    }
                    
                    for (int i = 0; keys != null && i < keys.size(); i++) {
                        // try decrypting with that key
                        try {
                            body = new EnclosureBody(client.ctx(), enc.getData(), enc.getDataSize(), (SessionKey)keys.get(i));
                            break;
                        } catch (IOException ioe) {
                            ui.errorMessage("Error decrypting with the read key", ioe);
                            ui.commandComplete(-1, null);
                            return;
                        } catch (DataFormatException dfe) {
                            ui.debugMessage("Error decrypting with a read key", dfe);
                            continue;
                        }
                    }
                    if (body == null) {
                        ui.errorMessage("No read keys successful at decrypting the message");
                        ui.commandComplete(-1, null);
                        return;
                    }
                }
            }

            ui.debugMessage("enclosure: " + enc + "\nbody: " + body);
            extract(enc, ui, body, args);
        }
    }
    
    /**
     * The post message is ok if it is either signed by the channel's
     * identity itself, one of the manager keys, one of the authorized keys,
     * or the post's authentication key
     */
    private boolean verifyPost(DBClient client, Enclosure enc) {
        if (true) return true;
        SigningPublicKey pubKey = enc.getHeaderSigningKey(Constants.MSG_META_HEADER_IDENTITY);
        Signature sig = enc.getAuthorizationSig();
        boolean ok = verifySig(client, sig, enc.getAuthorizationHash(), pubKey);
        if (!ok) {
            SigningPublicKey pubKeys[] = enc.getHeaderSigningKeys(Constants.MSG_META_HEADER_MANAGER_KEYS);
            if (pubKeys != null) {
                for (int i = 0; i < pubKeys.length; i++) {
                    if (verifySig(client, sig, enc.getAuthorizationHash(), pubKeys[i])) {
                        ok = true;
                        break;
                    }
                }
            }
        }
        return ok;
    }
    
    private void extract(Enclosure enc, UI ui, EnclosureBody body, Opts args) throws IOException {
        File dir = new SecureFile(args.getOptValue("out"));
        if (dir.exists())
            throw new IOException("Output directory already exists: " + dir);
        dir.mkdirs();
        for (int i = 0; i < body.getPages(); i++) {
            File page = new File(dir, "page" + i + ".dat");
            FileOutputStream fos = null;
            try {
                fos = new SecureFileOutputStream(page);
                fos.write(body.getPage(i));
                fos.close();
                fos = null;
            } finally {
                if (fos != null) try { fos.close(); } catch (IOException ioe) {}
            }
            
            File cfg = new File(dir, "page" + i + ".cfg");
            try {
                fos = new SecureFileOutputStream(cfg);
                write(body.getPageConfig(i), fos);
                fos.close();
                fos = null;
            } finally {
                if (fos != null) try { fos.close(); } catch (IOException ioe) {}
            }
        }
        for (int i = 0; i < body.getAttachments(); i++) {
            File attach = new File(dir, "attach" + i + ".dat");
            FileOutputStream fos = null;
            try {
                fos = new SecureFileOutputStream(attach);
                fos.write(body.getAttachment(i));
                fos.close();
                fos = null;
            } finally {
                if (fos != null) try { fos.close(); } catch (IOException ioe) {}
            }
            
            File cfg = new File(dir, "attach" + i + ".cfg");
            try {
                fos = new SecureFileOutputStream(cfg);
                write(body.getAttachmentConfig(i), fos);
                fos.close();
                fos = null;
            } finally {
                if (fos != null) try { fos.close(); } catch (IOException ioe) {}
            }
        }
        File avatar = new File(dir, "avatar.png");
        InputStream in = body.getAvatar();
        if (in != null) {
            FileOutputStream fos = null;
            try {
                fos = new SecureFileOutputStream(avatar);
                byte buf[] = new byte[1024];
                int read = -1;
                while ( (read = in.read(buf)) != -1)
                    fos.write(buf, 0, read);
                fos.close();
                fos = null;
            } finally {
                if (fos != null) try { fos.close(); } catch (IOException ioe) {}
            }
        }
        
        FileOutputStream out = null;
        try {
            out = new SecureFileOutputStream(new File(dir, "privHeaders.txt"));
            write(body.getHeaders(), out);
            out.close();
            out = null;
        } finally {
            if (out != null) try { out.close(); } catch (IOException ioe) {}
        }
        try {
            out = new SecureFileOutputStream(new File(dir, "pubHeaders.txt"));
            write(enc.getHeaders(), out);
            out.close();
            out = null;
        } finally {
            if (out != null) try { out.close(); } catch (IOException ioe) {}
        }
        
        ui.commandComplete(0, null);
    }
    
    protected void extractReply(DBClient client, UI ui, Enclosure enc, long nymId, Opts args) throws IOException {
        if (verifyReply(client, enc)) {
            //ImportPost.process(_client, enc, nymId);
            
            EnclosureBody body = null;
            SessionKey key = enc.getHeaderSessionKey(Constants.MSG_HEADER_BODYKEY);
            if (key != null) {
                try {
                    // decrypt it with that key
                    body = new EnclosureBody(client.ctx(), enc.getData(), enc.getDataSize(), key);
                } catch (DataFormatException dfe) {
                    // ignore
                    ui.debugMessage("DFE decrypting with the published key", dfe);
                } catch (IOException ioe) {
                    // ignore
                    ui.debugMessage("IOE decrypting with the published key", ioe);
                } catch (ArrayIndexOutOfBoundsException e) {
                    // ignore
                    ui.debugMessage("Err decrypting with the published key", e);
                }
            }
            
            String prompt = enc.getHeaderString(Constants.MSG_HEADER_PBE_PROMPT);
            byte promptSalt[] = enc.getHeaderBytes(Constants.MSG_HEADER_PBE_PROMPT_SALT);
            if ( (prompt != null) && (promptSalt != null) && (promptSalt.length != 0) ) {
                String passphrase = args.getOptValue("passphrase");
                if (passphrase == null) {
                    ui.errorMessage("Passphrase required to extract this message");
                    ui.errorMessage("Please use --passphrase 'passphrase value', where the passphrase value is the answer to:");
                    ui.errorMessage(strip(prompt));
                    ui.commandComplete(-1, null);
                    return;
                } else {
                    key = client.ctx().keyGenerator().generateSessionKey(promptSalt, DataHelper.getUTF8(passphrase));
                    try {
                        // decrypt it with that key
                        body = new EnclosureBody(client.ctx(), enc.getData(), enc.getDataSize(), key);
                    } catch (DataFormatException dfe) {
                        ui.errorMessage("Invalid passphrase", dfe);
                        ui.commandComplete(-1, null);
                        return;
                    } catch (IOException ioe) {
                        ui.debugMessage("Invalid passphrase", ioe);
                        return;
                    }
                }
            }
            
            if (body == null) {
                SyndieURI uri = enc.getHeaderURI(Constants.MSG_HEADER_POST_URI);
                if (uri == null) {
                    ui.errorMessage("Cannot decrypt a reply if we don't know what channel it is on");
                    ui.commandComplete(-1, null);
                    return;
                }
                Hash channel = uri.getScope(); //Channel();
                if (channel == null) {
                    ui.errorMessage("Cannot decrypt a reply if the URI doesn't have a channel in it - " + uri);
                    ui.commandComplete(-1, null);
                    return;
                }
                List keys = client.getReplyKeys(channel, nymId, client.getPass());
                for (int i = 0; keys != null && i < keys.size(); i++) {
                    // try decrypting with that key
                    try {
                        body = new EnclosureBody(client.ctx(), enc.getData(), enc.getDataSize(), (PrivateKey)keys.get(i));
                        break;
                    } catch (IOException ioe) {
                        ui.errorMessage("Error decrypting with the reply key", ioe);
                        ui.commandComplete(-1, null);
                        return;
                    } catch (DataFormatException dfe) {
                        ui.debugMessage("Error decrypting with the reply key", dfe);
                        continue;
                    }
                }
            }
            if (body == null) {
                ui.errorMessage("No reply key was able to open the message");
                ui.commandComplete(-1, null);
                return;
            }

            ui.debugMessage("enclosure: " + enc + "\nbody: " + body);
            extract(enc, ui, body, args);
        }
    }
    
    /**
     * The post message is ok if it is either signed by the channel's
     * identity itself, one of the manager keys, one of the authorized keys,
     * or the post's authentication key
     */
    private boolean verifyReply(DBClient client, Enclosure enc) {
        if (true) return true;
        SigningPublicKey pubKey = enc.getHeaderSigningKey(Constants.MSG_META_HEADER_IDENTITY);
        Signature sig = enc.getAuthorizationSig();
        boolean ok = verifySig(client, sig, enc.getAuthorizationHash(), pubKey);
        if (!ok) {
            SigningPublicKey pubKeys[] = enc.getHeaderSigningKeys(Constants.MSG_META_HEADER_MANAGER_KEYS);
            if (pubKeys != null) {
                for (int i = 0; i < pubKeys.length; i++) {
                    if (verifySig(client, sig, enc.getAuthorizationHash(), pubKeys[i])) {
                        ok = true;
                        break;
                    }
                }
            }
        }
        return ok;
    }
    
    public static void main(String args[]) {
        try {
        CLI.main(new String[] { "messageextract", 
                                "--db", "jdbc:hsqldb:file:/tmp/cli",
                                "--login", "j",
                                "--pass", "j",
                                "--in", "/tmp/messageOut",
                                "--out", "/tmp/messageExtract" });
        } catch (Exception e) { e.printStackTrace(); }
    }
}
