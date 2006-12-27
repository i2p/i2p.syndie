package syndie.db;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import net.i2p.data.Base64;
import net.i2p.data.DataFormatException;
import net.i2p.data.DataHelper;
import net.i2p.data.Hash;
import net.i2p.data.PublicKey;
import net.i2p.data.SessionKey;
import net.i2p.data.SigningPublicKey;
import syndie.Constants;
import syndie.data.Enclosure;
import syndie.data.EnclosureBody;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;

/**
 *
 */
class ImportMeta {
    /**
     * The signature has been validated, so now import what we can
     */
    public static boolean process(DBClient client, UI ui, Enclosure enc, long nymId, String nymPassphrase, String bodyPassphrase) {
        boolean wasPublic = false;
        EnclosureBody body = null;
        SigningPublicKey ident = enc.getHeaderSigningKey(Constants.MSG_META_HEADER_IDENTITY);
        Hash identHash = ident.calculateHash();
        if (client.getBannedChannels().contains(identHash)) {
            ui.errorMessage("Not importing banned metadata for " + identHash.toBase64());
            ui.commandComplete(-1, null);
            return false;
        }
        SessionKey key = enc.getHeaderSessionKey(Constants.MSG_HEADER_BODYKEY);
        if (key != null) {
            try {
                // decrypt it with that key
                body = new EnclosureBody(client.ctx(), enc.getData(), enc.getDataSize(), key);
                wasPublic = true;
                ui.debugMessage("metadata was encrypted with a published bodyKey");
            } catch (DataFormatException dfe) {
                ui.errorMessage("Error processing with the body key (" + Base64.encode(key.getData()) + " len=" + key.getData().length + ")", dfe);
                ui.commandComplete(-1, null);
                return false;
            } catch (IOException ioe) {
                ui.errorMessage("Error processing with the body key", ioe);
                ui.commandComplete(-1, null);
                return false;
            }
        } else {
            String prompt = enc.getHeaderString(Constants.MSG_HEADER_PBE_PROMPT);
            byte promptSalt[] = enc.getHeaderBytes(Constants.MSG_HEADER_PBE_PROMPT_SALT);
            if ( (prompt != null) && (promptSalt != null) && (promptSalt.length != 0) ) {
                String passphrase = bodyPassphrase;
                if (passphrase == null) {
                    ui.errorMessage("Passphrase required to extract this message");
                    ui.errorMessage("Please use --passphrase 'passphrase value', where the passphrase value is the answer to:");
                    ui.errorMessage(CommandImpl.strip(prompt));
                    body = new UnreadableEnclosureBody(client.ctx());
                } else {
                    key = client.ctx().keyGenerator().generateSessionKey(promptSalt, DataHelper.getUTF8(passphrase));
                    try {
                        // decrypt it with that key
                        body = new EnclosureBody(client.ctx(), enc.getData(), enc.getDataSize(), key);
                        ui.debugMessage("metadata was encrypted with a passphrase");
                    } catch (DataFormatException dfe) {
                        ui.errorMessage("Invalid passphrase", dfe);
                        body = new UnreadableEnclosureBody(client.ctx());
                    } catch (IOException ioe) {
                        ui.debugMessage("Invalid passphrase", ioe);
                        body = new UnreadableEnclosureBody(client.ctx());
                    }
                }
            } else {
                List keys = client.getReadKeys(identHash, nymId, nymPassphrase, false);
                for (int i = 0; keys != null && i < keys.size(); i++) {
                    // try decrypting with that key
                    try {
                        body = new EnclosureBody(client.ctx(), enc.getData(), enc.getDataSize(), (SessionKey)keys.get(i));
                        wasPublic = client.getChannelReadKeyIsPublic(identHash, (SessionKey)keys.get(i));
                        ui.debugMessage("metadata was encrypted with an existing read key (was that public before? " + wasPublic + ")");
                        break;
                    } catch (IOException ioe) {
                        ui.debugMessage("Error processing with a read key", ioe);
                        continue;
                    } catch (DataFormatException dfe) {
                        ui.debugMessage("Error processing with a read key", dfe);
                        continue;
                    }
                }
                if (body == null) {
                    ui.errorMessage("No read keys were successful at decrypting the message");
                    body = new UnreadableEnclosureBody(client.ctx());
                }
            }
        }

        ui.debugMessage("enclosure: " + enc + "\nbody: " + body);
        boolean ok = importMeta(client, ui, nymId, nymPassphrase, enc, body, wasPublic);
        if (ok) {
            if (body instanceof UnreadableEnclosureBody)
                ui.commandComplete(1, null);
            else
                ui.commandComplete(0, null);
        } else {
            ui.commandComplete(-1, null);
        }
        return ok;
    }
    
    /**
     * interpret the bits in the enclosure body and headers, importing them
     * into the db
     */
    private static boolean importMeta(DBClient client, UI ui, long nymId, String passphrase, Enclosure enc, EnclosureBody body, boolean wasPublic) {
        SigningPublicKey identKey = enc.getHeaderSigningKey(Constants.MSG_META_HEADER_IDENTITY);
        Hash ident = identKey.calculateHash();
        Long edition = enc.getHeaderLong(Constants.MSG_META_HEADER_EDITION);
        if ( (edition == null) || (edition.longValue() < 0) )
            edition = new Long(0);
        // see if we have the info already (with the same or later edition),
        // since if we do, there's nothing to import.
        long knownEdition = client.getKnownEdition(ident);
        if (knownEdition > edition.longValue()) {
            ui.statusMessage("already known edition " + knownEdition);
            return true;
        }

        // if we don't...
        Connection con = client.con();
        boolean wasAuto = false;
        try {
            wasAuto = con.getAutoCommit();
            con.commit();
            con.setAutoCommit(false);
            long channelId = -1;
            if (knownEdition < 0) // brand new
                channelId = insertIntoChannel(client, ui, nymId, passphrase, enc, body, identKey, ident, edition.longValue());
            else
                channelId = updateChannel(client, ui, nymId, passphrase, enc, body, ident, edition.longValue());
            if (channelId < 0) { return false; }
            // clear out & insert into channelTag
            setTags(client, ui, channelId, enc, body);
            // clear out & insert into channelPostKey
            setPostKeys(client, ui, channelId, enc, body);
            // clear out & insert into channelManageKey
            setManageKeys(client, ui, channelId, enc, body);
            // clear out (recursively) and insert into channelArchive
            setChannelArchives(client, ui, channelId, enc, body);
            // insert into channelReadKey
            int newKeys = setChannelReadKeys(client, ui, channelId, enc, body, wasPublic);
            // insert into channelMetaHeader
            setChannelMetaHeaders(client, channelId, enc, body);
            // insert into channelReferenceGroup
            setChannelReferences(client, channelId, body);
            // (plus lots of 'insert into uriAttribute' interspersed)
            setChannelAvatar(client, channelId, body);
            con.commit();
            ui.statusMessage("committed as channel " + channelId);
            
            saveToArchive(client, ui, ident, enc);
            
            if (newKeys > 0)
                importUndecryptable(client, ui, channelId);
            return true;
        } catch (SQLException se) {
            ui.errorMessage("Error importing", se);
            try {
                con.rollback();
            } catch (SQLException ex) {
                ui.errorMessage("Unable to rollback on error", ex);
            }
            return false;
        } finally {
            try {
                con.setAutoCommit(wasAuto);
            } catch (SQLException ex) {
                // ignore
            }
        }
    }

    private static final String SQL_GET_UNDECRYPTABLE = "SELECT msgId, messageId, channelHash FROM channelMessage JOIN channel ON channelId = scopeChannelId WHERE readKeyMissing = TRUE AND scopeChannelId = ?";
    /**
     * the received channel read keys may be able to decrypt some previously-undecryptable
     * posts
     */
    private static void importUndecryptable(DBClient client, UI ui, long channelId) throws SQLException {
        Connection con = client.con();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        ArrayList uris = new ArrayList();
        try {
            stmt = con.prepareStatement(SQL_GET_UNDECRYPTABLE);
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
        } finally {
            if (rs != null) rs.close();
            if (stmt != null) stmt.close();
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
                        boolean ok = imp.processMessage(ui, client, fin, null, true);
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
    
    /*
     * CREATE CACHED TABLE channel (
     *  -- locally unique id
     *  channelId       BIGINT IDENTITY PRIMARY KEY
     *  , channelHash   VARBINARY(32)
     *  , identKey      VARBINARY(256)
     *  , encryptKey    VARBINARY(256)
     *  , edition       BIGINT
     *  , name          VARCHAR(256)
     *  , description   VARCHAR(1024)
     *  -- can unauthorized people post new topics?
     *  , allowPubPost  BOOLEAN
     *  -- can unauthorized people reply to existing topics?
     *  , allowPubReply BOOLEAN
     *  , UNIQUE (channelHash)
     * );
     */
    private static final String SQL_INSERT_CHANNEL = "INSERT INTO channel (channelId, channelHash, identKey, encryptKey, edition, name, description, allowPubPost, allowPubReply, importDate, readKeyMissing, pbePrompt) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, NOW(), ?, ?)";
    private static long insertIntoChannel(DBClient client, UI ui, long nymId, String passphrase, Enclosure enc, 
                                          EnclosureBody body, SigningPublicKey identKey, Hash ident, 
                                          long edition) throws SQLException {
        PublicKey encryptKey = body.getHeaderEncryptKey(Constants.MSG_META_HEADER_ENCRYPTKEY);
        if (encryptKey == null)
            encryptKey = enc.getHeaderEncryptKey(Constants.MSG_META_HEADER_ENCRYPTKEY);
        
        String name = body.getHeaderString(Constants.MSG_META_HEADER_NAME);
        if (name == null)
            name = enc.getHeaderString(Constants.MSG_META_HEADER_NAME);
        
        String desc = body.getHeaderString(Constants.MSG_META_HEADER_DESCRIPTION);
        if (desc == null)
            desc = enc.getHeaderString(Constants.MSG_META_HEADER_DESCRIPTION);
        
        Boolean pubPosting = body.getHeaderBoolean(Constants.MSG_META_HEADER_PUBLICPOSTING);
        if (pubPosting == null)
            pubPosting = enc.getHeaderBoolean(Constants.MSG_META_HEADER_PUBLICPOSTING);
        if (pubPosting == null)
            pubPosting = Constants.DEFAULT_ALLOW_PUBLIC_POSTS;
        
        Boolean pubReply = body.getHeaderBoolean(Constants.MSG_META_HEADER_PUBLICREPLY);
        if (pubReply == null)
            pubReply = enc.getHeaderBoolean(Constants.MSG_META_HEADER_PUBLICREPLY);
        if (pubReply == null)
            pubReply = Constants.DEFAULT_ALLOW_PUBLIC_REPLIES;
        
        long channelId = client.nextId("channelIdSequence");
        if (channelId < 0) {
            ui.errorMessage("Internal error with the database (GCJ/HSQLDB problem with sequences?)");
            return -1;
        }
        
        Connection con = client.con();
        PreparedStatement stmt = null;
        try {
            stmt = con.prepareStatement(SQL_INSERT_CHANNEL);
            //"INSERT INTO channel (channelId, channelHash, identKey, encryptKey, edition, name, 
            //                      description, allowPubPost, allowPubReply, readKeyMissing, pbePrompt)
            stmt.setLong(1, channelId);
            stmt.setBytes(2, ident.getData());
            stmt.setBytes(3, identKey.getData());
            if (encryptKey != null)
                stmt.setBytes(4, encryptKey.getData());
            else
                stmt.setNull(4, Types.VARBINARY);
            stmt.setLong(5, edition);
            if (name != null)
                stmt.setString(6, name);
            else
                stmt.setNull(6, Types.VARCHAR);
            if (desc != null)
                stmt.setString(7, desc);
            else
                stmt.setNull(7, Types.VARCHAR);
            stmt.setBoolean(8, pubPosting.booleanValue());
            stmt.setBoolean(9, pubReply.booleanValue());

            boolean readKeyMissing = false;
            String pbePrompt = null;
            
            // the metadata was authorized, but we couldn't decrypt the body.
            // that can happen if we either don't have the passphrase or if we
            // don't know the appropriate channel read key.
            if (body instanceof UnreadableEnclosureBody) {
                pbePrompt = enc.getHeaderString(Constants.MSG_HEADER_PBE_PROMPT);
                if (pbePrompt == null)
                    readKeyMissing = true;
            }
            
            stmt.setBoolean(10, readKeyMissing);
            if (pbePrompt != null)
                stmt.setString(11, pbePrompt);
            else
                stmt.setNull(11, Types.VARCHAR);
            
            int rows = stmt.executeUpdate();
            if (rows != 1)
                throw new SQLException("Unable to insert the new channel");
            return channelId;
        } finally {
            if (stmt != null) stmt.close();
        }
    }

    /*
     * CREATE CACHED TABLE channel (
     *  -- locally unique id
     *  channelId       BIGINT IDENTITY PRIMARY KEY
     *  , channelHash   VARBINARY(32)
     *  , identKey      VARBINARY(256)
     *  , encryptKey    VARBINARY(256)
     *  , edition       BIGINT
     *  , name          VARCHAR(256)
     *  , description   VARCHAR(1024)
     *  -- can unauthorized people post new topics?
     *  , allowPubPost  BOOLEAN
     *  -- can unauthorized people reply to existing topics?
     *  , allowPubReply BOOLEAN
     *  , UNIQUE (channelHash)
     * );
     */
    private static final String SQL_GET_CHANNEL_ID = "SELECT channelId FROM channel WHERE channelHash = ?";
    private static long getChannelId(DBClient client, UI ui, Hash identHash) throws SQLException {
        Connection con = client.con();
        PreparedStatement stmt = null;
        try {
            stmt = con.prepareStatement(SQL_GET_CHANNEL_ID);
            stmt.setBytes(1, identHash.getData());
            ResultSet rs = stmt.executeQuery();
            if (rs.next()) {
                long val = rs.getLong(1);
                if (!rs.wasNull())
                    return val;
            }
            return -1;
        } finally {
            if (stmt != null) stmt.close();
        }
    }
    private static final String SQL_UPDATE_CHANNEL = "UPDATE channel SET encryptKey = ?, edition = ?, name = ?, description = ?, allowPubPost = ?, allowPubReply = ?, readKeyMissing = ?, pbePrompt = ?, importDate = NOW() WHERE channelId = ?";
    private static long updateChannel(DBClient client, UI ui, long nymId, String passphrase, Enclosure enc, 
                                      EnclosureBody body, Hash ident, long edition) throws SQLException {
        long channelId = getChannelId(client, ui, ident);
        if (channelId < 0) throw new SQLException("Cannot update, as there is no existing channel for " + ident.toBase64());
        
        PublicKey encryptKey = body.getHeaderEncryptKey(Constants.MSG_META_HEADER_ENCRYPTKEY);
        if (encryptKey == null)
            encryptKey = enc.getHeaderEncryptKey(Constants.MSG_META_HEADER_ENCRYPTKEY);
        
        String name = body.getHeaderString(Constants.MSG_META_HEADER_NAME);
        if (name == null)
            name = enc.getHeaderString(Constants.MSG_META_HEADER_NAME);
        
        String desc = body.getHeaderString(Constants.MSG_META_HEADER_DESCRIPTION);
        if (desc == null)
            desc = enc.getHeaderString(Constants.MSG_META_HEADER_DESCRIPTION);
        
        Boolean pubPosting = body.getHeaderBoolean(Constants.MSG_META_HEADER_PUBLICPOSTING);
        if (pubPosting == null)
            pubPosting = enc.getHeaderBoolean(Constants.MSG_META_HEADER_PUBLICPOSTING);
        if (pubPosting == null)
            pubPosting = Constants.DEFAULT_ALLOW_PUBLIC_POSTS;
        
        Boolean pubReply = body.getHeaderBoolean(Constants.MSG_META_HEADER_PUBLICREPLY);
        if (pubReply == null)
            pubReply = enc.getHeaderBoolean(Constants.MSG_META_HEADER_PUBLICREPLY);
        if (pubReply == null)
            pubReply = Constants.DEFAULT_ALLOW_PUBLIC_REPLIES;
        
        Connection con = client.con();
        PreparedStatement stmt = null;
        try {
            stmt = con.prepareStatement(SQL_UPDATE_CHANNEL);
            //"UPDATE channel SET 
            // encryptKey = ?, edition = ?, name = ?, description = ?, allowPubPost = ?,
            // allowPubReply = ?, readKeyMissing = ?, pbePrompt = ? WHERE channelId = ?";
            if (encryptKey != null)
                stmt.setBytes(1, encryptKey.getData());
            else
                stmt.setNull(1, Types.VARBINARY);
            stmt.setLong(2, edition);
            if (name != null)
                stmt.setString(3, name);
            else
                stmt.setNull(3, Types.VARCHAR);
            if (desc != null)
                stmt.setString(4, desc);
            else
                stmt.setNull(4, Types.VARCHAR);
            stmt.setBoolean(5, pubPosting.booleanValue());
            stmt.setBoolean(6, pubReply.booleanValue());

            boolean readKeyMissing = false;
            String pbePrompt = null;
            
            // the metadata was authorized, but we couldn't decrypt the body.
            // that can happen if we either don't have the passphrase or if we
            // don't know the appropriate channel read key.
            if (body instanceof UnreadableEnclosureBody) {
                pbePrompt = enc.getHeaderString(Constants.MSG_HEADER_PBE_PROMPT);
                if (pbePrompt == null)
                    readKeyMissing = true;
            }
            
            stmt.setBoolean(7, readKeyMissing);
            if (pbePrompt != null)
                stmt.setString(8, pbePrompt);
            else
                stmt.setNull(8, Types.VARCHAR);
            
            stmt.setLong(9, channelId);

            if (stmt.executeUpdate() != 1) throw new SQLException("Unable to update the channel for " + ident.toBase64());
            return channelId;
        } finally {
            if (stmt != null) stmt.close();
        }
    }
    
    /*
     *  CREATE CACHED TABLE channelTag (
     *  channelId       BIGINT
     *  , tag           VARCHAR(64)
     *  , wasEncrypted  BOOLEAN
     *  , PRIMARY KEY (channelId, tag)
     * );
     */
    static final String SQL_DELETE_TAGS = "DELETE FROM channelTag WHERE channelId = ?";
    private static final String SQL_INSERT_TAG = "INSERT INTO channelTag (channelId, tag, wasEncrypted) VALUES (?, ?, ?)";
    private static void setTags(DBClient client, UI ui, long channelId, Enclosure enc, EnclosureBody body) throws SQLException {
        Connection con = client.con();
        PreparedStatement stmt = null;
        try {
            stmt = con.prepareStatement(SQL_DELETE_TAGS);
            //"DELETE FROM channelTag WHERE channelId = ?";
            stmt.setLong(1, channelId);
            stmt.execute();
        } finally {
            if (stmt != null) stmt.close();
        }
        
        String unencryptedTags[] = enc.getHeaderStrings(Constants.MSG_META_HEADER_TAGS, true);
        String encryptedTags[] = body.getHeaderStrings(Constants.MSG_META_HEADER_TAGS, true);
        try {
            stmt = con.prepareStatement(SQL_INSERT_TAG);
            if (unencryptedTags != null) {
                for (int i = 0; i < unencryptedTags.length; i++) {
                    stmt.setLong(1, channelId);
                    stmt.setString(2, unencryptedTags[i]);
                    stmt.setBoolean(3, false);
                    stmt.executeUpdate(); // ignore rv, since the tag may already be there
                }
            }
            if (encryptedTags != null) {
                for (int i = 0; i < encryptedTags.length; i++) {
                    stmt.setLong(1, channelId);
                    stmt.setString(2, encryptedTags[i]);
                    stmt.setBoolean(3, true);
                    stmt.executeUpdate(); // ignore rv, since the tag may already be there
                }
            }
        } finally {
            if (stmt != null) stmt.close();
        }
    }
    
    /*
     * CREATE CACHED TABLE channelPostKey (
     *  channelId       BIGINT
     *  , authPubKey    VARBINARY(256)
     *  , PRIMARY KEY (channelId, authPubKey)
     * );
     */
    static final String SQL_DELETE_POSTKEYS = "DELETE FROM channelPostKey WHERE channelId = ?";
    private static final String SQL_INSERT_POSTKEY = "INSERT INTO channelPostKey (channelId, authPubKey) VALUES (?, ?)";
    private static void setPostKeys(DBClient client, UI ui, long channelId, Enclosure enc, EnclosureBody body) throws SQLException {
        Connection con = client.con();
        PreparedStatement stmt = null;
        try {
            stmt = con.prepareStatement(SQL_DELETE_POSTKEYS);
            //"DELETE FROM channelPostKey WHERE channelId = ?";
            stmt.setLong(1, channelId);
            stmt.execute();
        } finally {
            if (stmt != null) stmt.close();
        }

        SigningPublicKey unencKeys[] = enc.getHeaderSigningKeys(Constants.MSG_META_HEADER_POST_KEYS);
        SigningPublicKey encKeys[] = body.getHeaderSigningKeys(Constants.MSG_META_HEADER_POST_KEYS);
        try {
            stmt = con.prepareStatement(SQL_INSERT_POSTKEY);
            if (unencKeys != null) {
                for (int i = 0; i < unencKeys.length; i++) {
                    stmt.setLong(1, channelId);
                    stmt.setBytes(2, unencKeys[i].getData());
                    stmt.executeUpdate(); // ignore rv, since the key may already be there
                }
            }
            if (encKeys != null) {
                for (int i = 0; i < encKeys.length; i++) {
                    stmt.setLong(1, channelId);
                    stmt.setBytes(2, encKeys[i].getData());
                    stmt.executeUpdate(); // ignore rv, since the key may already be there
                }
            }
        } finally {
            if (stmt != null) stmt.close();
        }
    }
    
    /*
     * CREATE CACHED TABLE channelManageKey (
     *  channelId       BIGINT
     *  , authPubKey    VARBINARY(256)
     *  , PRIMARY KEY (channelId, authPubKey)
     * );
     */
    static final String SQL_DELETE_MANAGEKEYS = "DELETE FROM channelManageKey WHERE channelId = ?";
    private static final String SQL_INSERT_MANAGEKEY = "INSERT INTO channelManageKey (channelId, authPubKey) VALUES (?, ?)";
    private static void setManageKeys(DBClient client, UI ui, long channelId, Enclosure enc, EnclosureBody body) throws SQLException {
        Connection con = client.con();
        PreparedStatement stmt = null;
        try {
            stmt = con.prepareStatement(SQL_DELETE_MANAGEKEYS);
            //"DELETE FROM channelManageKey WHERE channelId = ?";
            stmt.setLong(1, channelId);
            stmt.execute();
        } finally {
            if (stmt != null) stmt.close();
        }

        SigningPublicKey unencKeys[] = enc.getHeaderSigningKeys(Constants.MSG_META_HEADER_MANAGER_KEYS);
        SigningPublicKey encKeys[] = body.getHeaderSigningKeys(Constants.MSG_META_HEADER_MANAGER_KEYS);
        try {
            stmt = con.prepareStatement(SQL_INSERT_MANAGEKEY);
            if (unencKeys != null) {
                for (int i = 0; i < unencKeys.length; i++) {
                    stmt.setLong(1, channelId);
                    stmt.setBytes(2, unencKeys[i].getData());
                    stmt.executeUpdate(); // ignore rv, since the key may already be there
                }
            }
            if (encKeys != null) {
                for (int i = 0; i < encKeys.length; i++) {
                    stmt.setLong(1, channelId);
                    stmt.setBytes(2, encKeys[i].getData());
                    stmt.executeUpdate(); // ignore rv, since the key may already be there
                }
            }
        } finally {
            if (stmt != null) stmt.close();
        }
    }
    
    /*
     * CREATE CACHED TABLE channelArchive (
     *  channelId       BIGINT
     *  , archiveId     BIGINT
     *  , wasEncrypted  BOOLEAN
     *  , PRIMARY KEY (channelId, archiveId)
     *  );
     *  
     * CREATE CACHED TABLE archive (
     *  archiveId               BIGINT PRIMARY KEY
     *  -- are we allowed to post (with the auth we have)?
     *  , postAllowed           BOOLEAN
     *  -- are we allowed to pull messages (with the auth we have)?
     *  , readAllowed           BOOLEAN
     *  -- index into uris.uriId to access the archive
     *  , uriId                 BIGINT
     * );
     */
    static final String SQL_DELETE_ARCHIVE_URIS = "DELETE FROM uriAttribute WHERE uriId IN (SELECT uriId FROM archive WHERE archiveId IN (SELECT archiveId FROM channelArchive WHERE channelId = ?))";
    static final String SQL_DELETE_ARCHIVES = "DELETE FROM archive WHERE archiveId IN (SELECT archiveId FROM channelArchive WHERE channelId = ?)";
    static final String SQL_DELETE_CHAN_ARCHIVES = "DELETE FROM channelArchive WHERE channelId = ?";
    private static final String SQL_INSERT_ARCHIVE = "INSERT INTO archive (archiveId, postAllowed, readAllowed, uriId) VALUES (?, ?, ?, ?)";
    private static final String SQL_INSERT_CHAN_ARCHIVE = "INSERT INTO channelArchive (channelId, archiveId, wasEncrypted) VALUES (?, ?, ?)";

    private static void setChannelArchives(DBClient client, UI ui, long channelId, Enclosure enc, EnclosureBody body) throws SQLException {
        client.exec(SQL_DELETE_ARCHIVE_URIS, channelId);
        client.exec(SQL_DELETE_ARCHIVES, channelId);
        client.exec(SQL_DELETE_CHAN_ARCHIVES, channelId);
        
        addArchives(client, channelId, body.getHeaderURIs(Constants.MSG_META_HEADER_ARCHIVES), true);
        addArchives(client, channelId, enc.getHeaderURIs(Constants.MSG_META_HEADER_ARCHIVES), false);
    }
    private static void addArchives(DBClient client, long channelId, SyndieURI archiveURIs[], boolean encrypted) throws SQLException {
        if (archiveURIs == null) return;
        Connection con = client.con();
        PreparedStatement archStmt = null;
        PreparedStatement chanStmt = null;
        try {
            archStmt = con.prepareStatement(SQL_INSERT_ARCHIVE);
            chanStmt = con.prepareStatement(SQL_INSERT_CHAN_ARCHIVE);
            for (int i = 0; i < archiveURIs.length; i++) {
                long uriId = client.addURI(archiveURIs[i]);
                //"INSERT INTO archive (archiveId, postAllowed, readAllowed, uriId) VALUES (?, ?, ?, ?)";
                long archiveId = client.nextId("archiveIdSequence");
                archStmt.setLong(1, archiveId);
                archStmt.setBoolean(2, false);
                archStmt.setBoolean(3, true);
                archStmt.setLong(4, uriId);
                if (archStmt.executeUpdate() != 1)
                    throw new SQLException("Unable to insert the archive for uri " + uriId + "/" + channelId);
                
                //"INSERT INTO channelArchive (channelId, archiveId, wasEncrypted) VALUES (?, ?, ?)";
                chanStmt.setLong(1, channelId);
                chanStmt.setLong(2, archiveId);
                chanStmt.setBoolean(3, encrypted);
                if (chanStmt.executeUpdate() != 1)
                    throw new SQLException("Unable to insert the channelArchive for uri " + uriId + "/" + channelId);
            }
        } finally {
            if (archStmt != null) archStmt.close();
            if (chanStmt != null) chanStmt.close();
        }
    }
    
    static final String SQL_DEPRECATE_READ_KEYS = "UPDATE channelReadKey SET keyEnd = CURDATE() WHERE channelId = ? AND keyEnd IS NULL";
    private static int setChannelReadKeys(DBClient client, UI ui, long channelId, Enclosure enc, EnclosureBody body, boolean wasPublic) throws SQLException {
        int newKeys = 0;
        SessionKey priv[] = body.getHeaderSessionKeys(Constants.MSG_META_HEADER_READKEYS);
        SessionKey pub[] = enc.getHeaderSessionKeys(Constants.MSG_META_HEADER_READKEYS);
        if ( ( (priv != null) && (priv.length > 0) ) || ( (pub != null) && (pub.length > 0) ) ) {
            client.exec(SQL_DEPRECATE_READ_KEYS, channelId);
            if ( (priv != null) && (priv.length > 0) ) {
                ui.debugMessage("setting channel read keys to include " + priv.length + " private read keys (pub? " + wasPublic + ")");
                newKeys += addChannelReadKeys(client, ui, channelId, priv, wasPublic);
            }
            if ( (pub != null) && (pub.length > 0) ) {
                ui.debugMessage("setting channel read keys to include " + pub.length + " publicly displayed read keys");
                newKeys += addChannelReadKeys(client, ui, channelId, pub, true);
            }
        }
        return newKeys;
    }
    /*
     * CREATE CACHED TABLE channelReadKey (
     *  channelId       BIGINT
     *  , keyStart      DATE DEFAULT NULL
     *  , keyEnd        DATE DEFAULT NULL
     *  , keyData       VARBINARY(32)
     * );
     */
    private static final String SQL_INSERT_CHANNEL_READ_KEY = "INSERT INTO channelReadKey (channelId, keyData, wasPublic, keyStart) VALUES (?, ?, ?, CURDATE())";
    private static final String SQL_ENABLE_CHANNEL_READ_KEY = "UPDATE channelReadKey SET keyEnd = NULL, wasPublic = ? WHERE channelId = ? AND keyData = ?";
    private static final String SQL_CHANNEL_READ_KEY_EXISTS = "SELECT COUNT(*), wasPublic FROM channelReadKey WHERE channelId = ? AND keyData = ? GROUP BY wasPublic";
    private static int addChannelReadKeys(DBClient client, UI ui, long channelId, SessionKey keys[], boolean wasPublic) throws SQLException {
        if (keys == null) return 0;
        int newKeys = 0;
        Connection con = client.con();
        PreparedStatement insertStmt = null;
        PreparedStatement enableStmt = null;
        PreparedStatement existsStmt = null;
        ResultSet rs = null;
        try {
            insertStmt = con.prepareStatement(SQL_INSERT_CHANNEL_READ_KEY);
            enableStmt = con.prepareStatement(SQL_ENABLE_CHANNEL_READ_KEY);
            existsStmt = con.prepareStatement(SQL_CHANNEL_READ_KEY_EXISTS);
            for (int i = 0; i < keys.length; i++) {
                existsStmt.setLong(1, channelId);
                existsStmt.setBytes(2, keys[i].getData());
                rs = existsStmt.executeQuery();
                boolean exists = false;
                boolean curWasPublic = false;
                if (rs.next()) {
                    long cnt = rs.getLong(1);
                    if (rs.wasNull()) cnt = 0;
                    curWasPublic = rs.getBoolean(2);
                    if (rs.wasNull()) curWasPublic = false;
                    if (cnt > 0)
                        exists = true;
                }
                rs.close();
                rs = null;
                if (exists) {
                    // if the current key was public, don't let it become private,
                    // but if the current key was private, it may become public (if disclosed in
                    // a publicly readable metadata)
                    ui.debugMessage("key exists: " + keys[i].toBase64() + ", did it used to be public? " + curWasPublic);
                    enableStmt.setBoolean(1, wasPublic || curWasPublic);
                    enableStmt.setLong(2, channelId);
                    enableStmt.setBytes(3, keys[i].getData());
                    if (enableStmt.executeUpdate() < 1)
                        throw new SQLException("Unable to enable the channel read key");
                } else {
                    ui.debugMessage("key did not exist: " + keys[i].toBase64());
                    insertStmt.setLong(1, channelId);
                    insertStmt.setBytes(2, keys[i].getData());
                    insertStmt.setBoolean(3, wasPublic);
                    if (insertStmt.executeUpdate() != 1)
                        throw new SQLException("Unable to insert the channel read key");
                    newKeys++;
                }
            }
        } finally {
            if (rs != null) rs.close();
            if (enableStmt != null) enableStmt.close();
            if (enableStmt != null) insertStmt.close();
            if (existsStmt != null) existsStmt.close();
        }
        return newKeys;
    }
    /*
     * CREATE CACHED TABLE channelMetaHeader (
     *  channelId       BIGINT
     *  , headerName    VARCHAR(256)
     *  , headerValue   VARCHAR(4096)
     *  , wasEncrypted  BOOLEAN
     * );
     */
    
    static final String SQL_DELETE_CHANNEL_META_HEADER = "DELETE FROM channelMetaHeader WHERE channelId = ?";
    private static void setChannelMetaHeaders(DBClient client, long channelId, Enclosure enc, EnclosureBody body) throws SQLException {
        client.exec(SQL_DELETE_CHANNEL_META_HEADER, channelId);
        addChannelMetaHeaders(client, channelId, body.getHeaders(), true);
        addChannelMetaHeaders(client, channelId, enc.getHeaders(), false);
    }
    private static final String SQL_INSERT_CHANNEL_META_HEADER = "INSERT INTO channelMetaHeader (channelId, headerName, headerValue, wasEncrypted) VALUES (?, ?, ?, ?)";
    private static void addChannelMetaHeaders(DBClient client, long channelId, Properties headers, boolean encrypted) throws SQLException {
        if (headers == null) return;
        Connection con = client.con();
        PreparedStatement stmt = null;
        try {
            stmt = con.prepareStatement(SQL_INSERT_CHANNEL_META_HEADER);
            for (Iterator iter = headers.keySet().iterator(); iter.hasNext(); ) {
                String name = (String)iter.next();
                String val = headers.getProperty(name);
                //"INSERT INTO channelMetaHeader (channelId, headerName, headerValues, wasEncrypted) VALUES (?, ?, ?, ?)";
                stmt.setLong(1, channelId);
                stmt.setString(2, name);
                stmt.setString(3, val);
                stmt.setBoolean(4, encrypted);
                if (stmt.executeUpdate() != 1)
                    throw new SQLException("Unable to insert the channel meta header");
            }
        } finally {
            if (stmt != null) stmt.close();
        }
    }
    
    static final String SQL_DELETE_CHANNEL_REF_URIS = "DELETE FROM uriAttribute WHERE uriId IN (SELECT uriId FROM channelReferenceGroup WHERE channelId = ?)";
    static final String SQL_DELETE_CHANNEL_REFERENCES = "DELETE FROM channelReferenceGroup WHERE channelId = ?";
    private static void setChannelReferences(DBClient client, long channelId, EnclosureBody body) throws SQLException {
        client.exec(SQL_DELETE_CHANNEL_REF_URIS, channelId);
        client.exec(SQL_DELETE_CHANNEL_REFERENCES, channelId);
        RefWalker walker = new RefWalker(client, channelId);
        // 
        for (int i = 0; i < body.getReferenceRootCount(); i++) {
            ReferenceNode node = body.getReferenceRoot(i);
            walker.visitRoot(node, i);
        }
        walker.done();
    }
    
    /*
     * CREATE CACHED TABLE channelReferenceGroup (
     *  channelId       BIGINT
     *  , groupId       INTEGER NOT NULL
     *  , parentGroupId INTEGER
     *  , siblingOrder  INTEGER NOT NULL
     *  , name          VARCHAR(256)
     *  , description   VARCHAR(1024)
     *  , uriId         BIGINT
     *  -- allows for references of 'ban', 'recommend', 'trust', etc
     *  , referenceType INTEGER DEFAULT NULL
     *  , wasEncrypted  BOOLEAN
     *  , PRIMARY KEY (channelId, groupId)
     * );
     */
    private static final String SQL_INSERT_CHANNEL_REFERENCE = "INSERT INTO channelReferenceGroup (channelId, groupId, parentGroupId, siblingOrder, name, description, uriId, referenceType, wasEncrypted) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)";
    private static class RefWalker {
        private DBClient _client;
        private long _channelId;
        private long _nextId;
        private PreparedStatement _stmt;
        public RefWalker(DBClient client, long channelId) throws SQLException { 
            _client = client; 
            _channelId = channelId;
            _nextId = 0;
            _stmt = _client.con().prepareStatement(SQL_INSERT_CHANNEL_REFERENCE);
        }
        public void done() throws SQLException { _stmt.close(); }
        public void visitRoot(ReferenceNode node, int branch) throws SQLException { visit(node, branch, null); }
        private void visit(ReferenceNode node, int branch, Long parent) throws SQLException {
            insertRef(node, _nextId, parent, branch);
            Long cur  = new Long(_nextId);
            _nextId++;
            for (int i = 0; i < node.getChildCount(); i++)
                visit(node.getChild(i), i, cur);
        }
        //"INSERT INTO channelReferenceGroup 
        //  (channelId, groupId, parentGroupId, siblingOrder, name, 
        //   description, uriId, referenceType, wasEncrypted)";
        private void insertRef(ReferenceNode node, long groupId, Long parent, long branch) throws SQLException {
            SyndieURI uri = node.getURI();
            long uriId = -1;
            if (uri != null)
                uriId = _client.addURI(uri);
            _stmt.setLong(1, _channelId);
            _stmt.setLong(2, groupId);
            if (parent != null)
                _stmt.setLong(3, parent.longValue());
            else
                _stmt.setNull(3, Types.BIGINT);
            _stmt.setLong(4, branch);
            if (node.getName() != null)
                _stmt.setString(5, node.getName());
            else
                _stmt.setNull(5, Types.VARCHAR);
            if (node.getDescription() != null)
                _stmt.setString(6, node.getDescription());
            else
                _stmt.setNull(6, Types.VARCHAR);
            if (uriId != -1)
                _stmt.setLong(7, uriId);
            else
                _stmt.setNull(7, Types.BIGINT);
            if (node.getReferenceType() != null)
                _stmt.setString(8, node.getReferenceType());
            else
                _stmt.setNull(8, Types.VARCHAR);
            _stmt.setBoolean(9, true);
            if (_stmt.executeUpdate() != 1)
                throw new SQLException("Adding a channel reference did not go through");
        }
    }
    
    static final String SQL_DELETE_CHANNEL_AVATAR = "DELETE FROM channelAvatar WHERE channelId = ?";
    static final String SQL_SET_AVATAR = "INSERT INTO channelAvatar (channelId, avatarData) VALUES (?, ?)";
    private static void setChannelAvatar(DBClient client, long channelId, EnclosureBody body) throws SQLException {
        client.exec(SQL_DELETE_CHANNEL_AVATAR, channelId);
        byte avatar[] = body.getAvatarData();
        if (avatar != null) {
            PreparedStatement stmt = null;
            try {
                stmt = client.con().prepareStatement(SQL_SET_AVATAR);
                stmt.setLong(1, channelId);
                stmt.setBytes(2, avatar);
                stmt.executeUpdate();
                stmt.close();
                stmt = null;
            } finally {
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }
        }
    }
    
    private static void saveToArchive(DBClient client, UI ui, Hash ident, Enclosure enc) {
        File outDir = new File(client.getArchiveDir(), ident.toBase64());
        outDir.mkdirs();
        File outMeta = new File(outDir, "meta" + Constants.FILENAME_SUFFIX);
        try {
            enc.store(outMeta.getPath());
            ui.debugMessage("Metadata saved to the archive at " + outMeta.getPath());
        } catch (IOException ioe) {
            ui.errorMessage("Error saving the metadata to the archive", ioe);
        }
    }
}
