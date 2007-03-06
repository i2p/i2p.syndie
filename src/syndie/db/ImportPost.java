package syndie.db;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.util.*;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Types;
import net.i2p.crypto.KeyGenerator;
import net.i2p.data.*;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.Enclosure;
import syndie.data.EnclosureBody;
import syndie.data.MessageInfo;
import syndie.data.NymKey;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;

/**
 *
 */
public class ImportPost {
    private DBClient _client;
    private UI _ui;
    private long _nymId;
    private String _pass;
    private Enclosure _enc;
    private EnclosureBody _body;
    private SyndieURI _uri;
    private Hash _channel;
    private long _channelId;
    private boolean _publishedBodyKey;
    private boolean _privateMessage;
    private boolean _authenticated;
    private boolean _authorized;
    private boolean _pseudoauthorized;
    private String _bodyPassphrase;
    private boolean _forceReimport;
    private boolean _alreadyImported;
    private SessionKey _replySessionKey;
    private byte _replyIV[];
    
    public ImportPost(DBClient client, UI ui, Enclosure enc, long nymId, String pass, String bodyPassphrase, boolean forceReimport, byte replyIV[], SessionKey replySessionKey) {
        _client = client;
        _ui = ui;
        _enc = enc;
        _nymId = nymId;
        _pass = pass;
        _privateMessage = false;
        _bodyPassphrase = bodyPassphrase;
        _forceReimport = forceReimport;
        _alreadyImported = false;
        _replySessionKey = replySessionKey;
        _replyIV = replyIV;
    }
    
    public boolean getAlreadyImported() { return _alreadyImported; }
    public boolean getNoKey() { return (_body != null) && (_body instanceof UnreadableEnclosureBody); }
    public SyndieURI getURI() { return _uri; }
    
    /*
     * The post message is ok if it is either signed by the channel's
     * identity itself, one of the manager keys, one of the authorized keys,
     * or the post's authentication key.  the exit code in ui.commandComplete is
     * -1 if unimportable, 0 if imported fully, or 1 if imported but not decryptable
     */
    public static boolean process(DBClient client, UI ui, Enclosure enc, long nymId, String pass, String bodyPassphrase, boolean forceReimport, byte replyIV[], SessionKey replySessionKey) {
        ImportPost imp = new ImportPost(client, ui, enc, nymId, pass, bodyPassphrase, forceReimport, replyIV, replySessionKey);
        return imp.process();
    }
    public boolean process() {
        _uri = _enc.getHeaderURI(Constants.MSG_HEADER_POST_URI);
        if (_uri == null) {
            _ui.errorMessage("No URI in the post");
            _ui.commandComplete(-1, null);
            return false;
        }
        _channel = _uri.getScope();
        if (_channel == null) {
            _ui.errorMessage("No channel in the URI: " + _uri);
            _ui.commandComplete(-1, null);
            return false;
        }
        
        // first we check to ban posts by ANY author in a banned channel
        if (_client.getBannedChannels().contains(_channel)) {
            _ui.errorMessage("Not importing banned post in " + _channel.toBase64() + ": " + _uri);
            _ui.commandComplete(-1, null);
            return false;
        }
        /** was a published bodyKey used, rather than a secret readKey or replyKey? */
        _publishedBodyKey = false;
        _body = null;
        if (_enc.isReply()) {
            if (_replySessionKey != null) {
                _ui.debugMessage("post is a reply in scope " + _channel.toBase64() + " and we have an explicit reply session key");
                try {
                    _body = new EnclosureBody(_client.ctx(), _enc.getData(), _enc.getDataSize(), _replyIV, _replySessionKey);
                    _privateMessage = true;
                    _ui.debugMessage("Private decryption successful with explicit reply session key");
                } catch (IOException ioe) {
                    // ignore
                    _ui.debugMessage("IO error attempting decryption with explicit reply session key", ioe);
                } catch (DataFormatException dfe) {
                    // ignore
                    _ui.debugMessage("DFE attempting decryption with explicit reply session key", dfe);
                }
            }

            if (_body == null) {
                List privKeys = _client.getReplyKeys(_channel, _nymId, _pass);
                _ui.debugMessage("post is a reply in scope " + _channel.toBase64() + " and we have " + privKeys.size() + " keys");

                byte target[] = _enc.getHeaderBytes(Constants.MSG_HEADER_TARGET_CHANNEL);
                Hash targetHash = null;
                if (target != null) {
                    targetHash = new Hash(target);
                    List targetKeys = _client.getReplyKeys(targetHash, _nymId, _pass);
                    privKeys.addAll(targetKeys);
                    _ui.debugMessage("post is a reply targetting " + targetHash.toBase64() + " and we have " + targetKeys.size() + " keys");
                }
                if ( (privKeys != null) && (privKeys.size() > 0) ) {
                    for (int i = 0; i < privKeys.size(); i++) {
                        PrivateKey priv = (PrivateKey)privKeys.get(i);
                        _ui.debugMessage("Attempting decrypt with key " + KeyGenerator.getPublicKey(priv).calculateHash().toBase64());
                        try {
                            _body = new EnclosureBody(_client.ctx(), _enc.getData(), _enc.getDataSize(), priv);
                            _privateMessage = true;
                            _ui.debugMessage("Private decryption successful with key " + i);
                            break;
                        } catch (IOException ioe) {
                            // ignore
                            _ui.debugMessage("IO error attempting decryption " + i, ioe);
                        } catch (DataFormatException dfe) {
                            // ignore
                            _ui.debugMessage("DFE attempting decryption " + i, dfe);
                        }
                    }
                    if (_body == null)
                        _ui.debugMessage("None of the reply keys we have work for the message (we have " + privKeys.size() + " keys)");
                }

                if (_body == null) {
                    String prompt = _enc.getHeaderString(Constants.MSG_HEADER_PBE_PROMPT);
                    byte promptSalt[] = _enc.getHeaderBytes(Constants.MSG_HEADER_PBE_PROMPT_SALT);
                    if ( (prompt != null) && (promptSalt != null) && (promptSalt.length != 0) ) {
                        String passphrase = _bodyPassphrase; //args.getOptValue("passphrase");
                        if (passphrase == null) {
                            _ui.errorMessage("Passphrase required to extract this message");
                            _ui.errorMessage("Please use --passphrase 'passphrase value', where the passphrase value is the answer to:");
                            _ui.errorMessage(CommandImpl.strip(prompt));
                            _body = new UnreadableEnclosureBody(_client.ctx());
                        } else {
                            SessionKey key = _client.ctx().keyGenerator().generateSessionKey(promptSalt, DataHelper.getUTF8(passphrase));
                            try {
                                // decrypt it with that key
                                _body = new EnclosureBody(_client.ctx(), _enc.getData(), _enc.getDataSize(), key);
                            } catch (DataFormatException dfe) {
                                _ui.errorMessage("Invalid passphrase");
                                _ui.debugMessage("Invalid passphrase cause", dfe);
                                _body = new UnreadableEnclosureBody(_client.ctx());
                            } catch (IOException ioe) {
                                _ui.errorMessage("Invalid passphrase");
                                _ui.debugMessage("Invalid passphrase cause", ioe);
                                _body = new UnreadableEnclosureBody(_client.ctx());
                            }
                        }
                    }
                }

                if (_body == null) {
                    _ui.debugMessage("Cannot import a reply that we do not have the private key to read");
                    _body = new UnreadableEnclosureBody(_client.ctx());
                }
            }
        } else if (_enc.isPost()) {
            // it can either be encrypted with a key in the public header or encrypted
            // with one of the channel's read keys...
            
            SessionKey key = _enc.getHeaderSessionKey(Constants.MSG_HEADER_BODYKEY);
            if (key != null) {
                try {
                    // decrypt it with that key
                    _body = new EnclosureBody(_client.ctx(), _enc.getData(), _enc.getDataSize(), key);
                    _publishedBodyKey = true;
                    _ui.debugMessage("Published bodyKey was valid");
                } catch (DataFormatException dfe) {
                    _ui.errorMessage("Provided bodyKey is invalid", dfe);
                    _ui.commandComplete(-1, null);
                    return false;
                } catch (IOException ioe) {
                    _ui.errorMessage("Provided bodyKey is invalid", ioe);
                    _ui.commandComplete(-1, null);
                    return false;
                }
            } else {
                String prompt = _enc.getHeaderString(Constants.MSG_HEADER_PBE_PROMPT);
                byte promptSalt[] = _enc.getHeaderBytes(Constants.MSG_HEADER_PBE_PROMPT_SALT);
                if ( (prompt != null) && (promptSalt != null) && (promptSalt.length != 0) ) {
                    String passphrase = _bodyPassphrase; //args.getOptValue("passphrase");
                    if (passphrase == null) {
                        _ui.errorMessage("Passphrase required to extract this message");
                        _ui.errorMessage("Please use --passphrase 'passphrase value', where the passphrase value is the answer to:");
                        _ui.errorMessage(CommandImpl.strip(prompt));
                        _body = new UnreadableEnclosureBody(_client.ctx());
                    } else {
                        key = _client.ctx().keyGenerator().generateSessionKey(promptSalt, DataHelper.getUTF8(passphrase));
                        try {
                            // decrypt it with that key
                            _body = new EnclosureBody(_client.ctx(), _enc.getData(), _enc.getDataSize(), key);
                        } catch (DataFormatException dfe) {
                            _ui.errorMessage("Invalid passphrase");
                            _ui.debugMessage("Invalid passphrase [" + passphrase + "] salt [" + Base64.encode(promptSalt) + "]", dfe);
                            _body = new UnreadableEnclosureBody(_client.ctx());
                        } catch (IOException ioe) {
                            _ui.errorMessage("Invalid passphrase");
                            _ui.debugMessage("Invalid passphrase [" + passphrase + "] salt [" + Base64.encode(promptSalt) + "]", ioe);
                            _body = new UnreadableEnclosureBody(_client.ctx());
                        }
                    }
                } else {
                    List keys = _client.getReadKeys(_channel, _nymId, _pass, false);
                    if ( (keys == null) || (keys.size() <= 0) ) {
                        _ui.errorMessage("No read keys known for " + _channel.toBase64());
                        _body = new UnreadableEnclosureBody(_client.ctx());
                    }
                    byte target[] = _enc.getHeaderBytes(Constants.MSG_HEADER_TARGET_CHANNEL);
                    if ( (target != null) && (target.length == Hash.HASH_LENGTH) ) {
                        List targetKeys = _client.getReadKeys(new Hash(target), _nymId, _pass, false);
                        keys.addAll(targetKeys);
                    }
                    for (int i = 0; i < keys.size(); i++) {
                        // try decrypting with that key
                        try {
                            _body = new EnclosureBody(_client.ctx(), _enc.getData(), _enc.getDataSize(), (SessionKey)keys.get(i));
                            _ui.debugMessage("Known readKey was valid");
                            break;
                        } catch (IOException ioe) {
                            _ui.debugMessage("Read key attempt failed, continuing...", ioe);
                            continue;
                        } catch (DataFormatException dfe) {
                            //dfe.printStackTrace();
                            _ui.debugMessage("Read key " + i + "/" + keys.size() + " attempt failed, continuing...");//, dfe);
                            continue;
                        }
                    }
                    if (_body == null) {
                        _ui.debugMessage("Read keys were unable to decrypt the post to " + _channel.toBase64());
                        _body = new UnreadableEnclosureBody(_client.ctx());
                    }
                }
            }
        } else {
            _ui.errorMessage("Not a post or a reply... wtf? " + _enc.getEnclosureType());
            _ui.commandComplete(-1, null);
            return false;
        }
        
        // now the body has been decrypted... 
        _channelId = _client.getChannelId(_channel);
        if (_channelId == -1) {
            _ui.errorMessage("Channel is not known: " + _channel.toBase64());
            _ui.commandComplete(-1, null);
            return false;
        } else {
            _ui.debugMessage("Target channel is known: " + _channelId + "/" + _channel.toBase64());
        }
        
        _ui.debugMessage("private headers read: " + _body.getHeaders().toString());
        _ui.debugMessage("public headers read: " + _enc.getHeaders().toString());
        
        // check authentication/authorization
        _authenticated = false;
        _authorized = false;
        _pseudoauthorized = false;
        
        // posts do not need to include an identity in their headers (though if they are
        // neither identified nor authenticated, they'll be dropped)
        Signature authenticationSig = _enc.getAuthenticationSig();
        byte authorVal[] = _body.getHeaderBytes(Constants.MSG_HEADER_AUTHOR);
        if (authorVal == null) { // not a hidden author, maybe a publicly visible author?
            authorVal = _enc.getHeaderBytes(Constants.MSG_HEADER_AUTHOR);
            _ui.debugMessage("Not permuting the authentication signature (public)");
        } else { // hidden author, check to see if we need to permute authenticationSig
            byte mask[] = _body.getHeaderBytes(Constants.MSG_HEADER_AUTHENTICATION_MASK);
            if ( (mask != null) && (mask.length == Signature.SIGNATURE_BYTES) ) {
                _ui.debugMessage("Permuting the authentication signature");
                byte realSig[] = DataHelper.xor(authenticationSig.getData(), mask);
                authenticationSig.setData(realSig);
            } else {
                _ui.debugMessage("Not permuting the authentication signature (no mask)");
            }
        }
        
        if ( (authorVal == null) && (_body instanceof UnreadableEnclosureBody) ) {
            Hash authorHash = _uri.getScope();
            SigningPublicKey pub = _client.getIdentKey(authorHash);
            if (pub != null) {
                _authenticated = _client.ctx().dsa().verifySignature(authenticationSig, _enc.getAuthenticationHash(), pub);
                if (_authenticated) {
                    _ui.debugMessage("authenticated against the identity key for the unreadable authorHash (" + authorHash.toBase64() +"): " + pub.toBase64());
                    // now filter out banned authors who are posting in channels that
                    // aren't banned
                    if (_client.getBannedChannels().contains(authorHash)) {
                        _ui.errorMessage("Not importing unreadable post written by banned author " + authorHash.toBase64() + ": " + _uri);
                        _ui.commandComplete(-1, null);
                        return false;
                    }
                } else {
                    _ui.debugMessage("not authenticated against the identity key for the unreadable authorHash (" + authorHash.toBase64() + ")");
                }
            }
        }
        if ( (authorVal != null) && (authorVal.length == Hash.HASH_LENGTH) ) {
            Hash authorHash = new Hash(authorVal);
            SigningPublicKey pub = _client.getIdentKey(authorHash);
            if (pub != null) {
                _authenticated = _client.ctx().dsa().verifySignature(authenticationSig, _enc.getAuthenticationHash(), pub);
                if (_authenticated) {
                    _ui.debugMessage("authenticated against the identity key for the authorHash (" + authorHash.toBase64() +"): " + pub.toBase64());
                    // now filter out banned authors who are posting in channels that
                    // aren't banned
                    if (_client.getBannedChannels().contains(authorHash)) {
                        _ui.errorMessage("Not importing post written by banned author " + authorHash.toBase64() + ": " + _uri);
                        _ui.commandComplete(-1, null);
                        return false;
                    }
                }
            }
        }
                   
        
        byte target[] = _enc.getHeaderBytes(Constants.MSG_HEADER_TARGET_CHANNEL);
        Hash targetHash = null;
        if (target != null) {
            // may be separate from the author or scope
            targetHash = new Hash(target);
            if (_client.getBannedChannels().contains(targetHash)) {
                _ui.errorMessage("Not importing banned post in " + _channel.toBase64() + ": " + _uri);
                _ui.commandComplete(-1, null);
                return false;
            }
        }
        
        // includes managers, posters, and the owner
        List signingPubKeys = null;
        if (targetHash != null) {
            signingPubKeys = _client.getAuthorizedPosters(targetHash);
        } else {
            signingPubKeys = _client.getAuthorizedPosters(_channel);
        }
        if (signingPubKeys == null) {
            _ui.errorMessage("Internal error getting authorized posters for the channel");
            _ui.commandComplete(-1, null);
            return false;
        }
        
        Signature authorizationSig = _enc.getAuthorizationSig();
        Hash authorizationHash = _enc.getAuthorizationHash();
        _ui.debugMessage("attempting to authorize the post against " + signingPubKeys.size());
        for (int i = 0; i < signingPubKeys.size(); i++) {
            SigningPublicKey pubKey = (SigningPublicKey)signingPubKeys.get(i);
            boolean ok = _client.ctx().dsa().verifySignature(authorizationSig, authorizationHash, pubKey);
            if (ok) {
                _authorized = true;
                break;
            } else {
                _ui.debugMessage("not authorized for key " + i);
            }
        }
        
        if (!_authorized && !_authenticated && !targetHash.equals(_channel) && (_enc.getHeaderString(Constants.MSG_HEADER_PBE_PROMPT) != null) && (_body instanceof UnreadableEnclosureBody)) {
            // the post may be a PBE replying to a post in another channel (which could be authorized once
            // the PBE info is decrypted), so for now, lets try to import it as an unreadable post in the 
            // source channel
            signingPubKeys = _client.getAuthorizedPosters(_channel);
            _ui.debugMessage("attempting pseudoauthorization authorize the unreadable PBE'd post against " + signingPubKeys.size());
            for (int i = 0; i < signingPubKeys.size(); i++) {
                SigningPublicKey pubKey = (SigningPublicKey)signingPubKeys.get(i);
                boolean ok = _client.ctx().dsa().verifySignature(authorizationSig, authorizationHash, pubKey);
                if (ok) {
                    _pseudoauthorized = true;
                    _ui.debugMessage("pseudoauthorized unreadable PBE'd post");
                    break;
                } else {
                    _ui.debugMessage("not authorized for key " + i);
                }
            }
        }
        
        if (_authenticated || _authorized || _pseudoauthorized) {
            boolean ok = importMessage();
            if (ok) {
                if (_body instanceof UnreadableEnclosureBody)
                    _ui.commandComplete(1, null);
                else
                    _ui.commandComplete(0, null);
            } else {
                _ui.commandComplete(-1, null);
            }
            return ok;
        } else {
            _ui.errorMessage("Neither authenticated nor authorized.  bugger off.");
            _ui.commandComplete(-1, null);
            return false;
        }
    }
    
    private boolean importMessage() {
        _ui.debugMessage("Message is" + (_authenticated ? " authenticated" : " not authenticated") +
                          (_authorized ? " authorized" : _pseudoauthorized ? " pseudoauthorized" : " not authorized") + ": " + _body);
        long msgId = _client.nextId("msgIdSequence");
        if (msgId < 0) {
            _ui.errorMessage("Internal error with the database (GCJ/HSQLDB problem with sequences?)");
            return false;
        }
        _ui.debugMessage("importing new message with id " + msgId);
        
        try {
            boolean added = insertToChannel(msgId);
            if (!added) {
                _ui.statusMessage("Already imported");
                return false;
            }
            setMessageHierarchy(msgId);
            setMessageTags(msgId);
            setMessageAttachments(msgId);
            setMessagePages(msgId);
            setMessageReferences(msgId);
            setUnread(msgId);

            processControlActivity();
            
            saveToArchive(_client, _ui, _channel, _enc);
            return true;
        } catch (SQLException se) {
            _ui.errorMessage("Error importing the message", se);
            return false;
        }
    }
    
    /**
     * Cancel messages, overwrite messages, import channel keys, etc
     */
    private void processControlActivity() throws SQLException {
        //
    }

    private static boolean isAuth(Set authorizedKeys, Hash ident) {
        for (Iterator iter = authorizedKeys.iterator(); iter.hasNext(); ) {
            SigningPublicKey key = (SigningPublicKey)iter.next();
            if (key.calculateHash().equals(ident))
                return true;
        }
        return false;
    }
    
    private static final String SQL_INSERT_CHANNEL = "INSERT INTO channelMessage (" +
            "msgId, authorChannelId, messageId, targetChannelId, subject, overwriteScopeHash, " +
            "overwriteMessageId, forceNewThread, refuseReplies, wasEncrypted, wasPrivate, wasAuthorized, " +
            "wasAuthenticated, isCancelled, expiration, importDate, scopeChannelId, wasPBE, " +
            "readKeyMissing, replyKeyMissing, pbePrompt" +
            ") VALUES (" +
            "?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NOW(), ?, ?, ?, ?, ?" +
            ")";
    /**
     * returns true if the message was inserted into the channel properly, false 
     * if the message was already in there or there was a problem
     */
    private boolean insertToChannel(long msgId) throws SQLException {
        Hash author = null;
        if (_authenticated) {
            byte authorVal[] = _body.getHeaderBytes(Constants.MSG_HEADER_AUTHOR);
            if (authorVal == null) // not a hidden author, maybe a publicly visible author?
                authorVal = _enc.getHeaderBytes(Constants.MSG_HEADER_AUTHOR);
            if (authorVal == null) // we are authenticated, but implicitly, which means the channel's key was used
                author = _channel;
            else
                author = new Hash(authorVal);
        }
        
        Long messageId = _uri.getMessageId();
        
        Boolean forceNewThread = _body.getHeaderBoolean(Constants.MSG_HEADER_FORCE_NEW_THREAD);
        if (forceNewThread == null)
            forceNewThread = _enc.getHeaderBoolean(Constants.MSG_HEADER_FORCE_NEW_THREAD);
        
        Boolean refuseReplies = _body.getHeaderBoolean(Constants.MSG_HEADER_REFUSE_REPLIES);
        if (refuseReplies == null)
            refuseReplies = _enc.getHeaderBoolean(Constants.MSG_HEADER_REFUSE_REPLIES);
        
        long scopeChannelId = _client.getChannelId(_channel);
        long targetChannelId = scopeChannelId;
        byte target[] = _body.getHeaderBytes(Constants.MSG_HEADER_TARGET_CHANNEL);
        if (target != null) {
            Hash targetHash = new Hash(target);
            long targetId = _client.getChannelId(targetHash);
            if (_authorized) {
                targetChannelId = targetId;
            } else if (isAuthorizedFor(targetHash, targetId, author, forceNewThread)) {
                targetChannelId = targetId;
                _authorized = true;
            }
        } else if (!_authorized && isAuthorizedFor(_channel, targetChannelId, author, forceNewThread)) {
            _authorized = true;
        }
        
        if ( (!_authorized) && (_enc.isReply()) ) {
            Hash targetHash = new Hash(target);
            long targetId = _client.getChannelId(targetHash);
            targetChannelId = targetId;
            _ui.debugMessage("Unauthorized post to " + targetHash.toBase64() + ", but it is a private reply, so put it in the chan (as unauthorized)");
        }
        
        String subject = _body.getHeaderString(Constants.MSG_HEADER_SUBJECT);
        if (subject == null)
            subject = _enc.getHeaderString(Constants.MSG_HEADER_SUBJECT);
        
        SyndieURI overwrite = _body.getHeaderURI(Constants.MSG_HEADER_OVERWRITE);
        Hash overwriteHash = null;
        Long overwriteMsg = null;
        if (overwrite != null) {
            overwriteHash = overwrite.getScope();
            overwriteMsg = overwrite.getMessageId();
        }
        
        boolean wasEncrypted = !_publishedBodyKey;
        boolean wasPrivate = _privateMessage;
        boolean wasPBE = _enc.getHeaderString(Constants.MSG_HEADER_PBE_PROMPT) != null;
        
        Date expiration = _body.getHeaderDate(Constants.MSG_HEADER_EXPIRATION);
        if (expiration == null)
            expiration = _enc.getHeaderDate(Constants.MSG_HEADER_EXPIRATION);
        
        long channelId = _client.getChannelId(_channel);
        if (channelId < 0) {
            _ui.errorMessage("Cannot import the post, as it was made in a channel we don't know");
            return false;
        }
        MessageInfo msg = _client.getMessage(channelId, _uri.getMessageId());
        if (msg != null) {
            if (_forceReimport) {
                _ui.debugMessage("Message exists (" + msg.getInternalId() + ") but we want to force reimport, so drop it");
                _client.deleteFromDB(_uri, _ui);
                msg = null;
            } else if ( (msg.getPassphrasePrompt() == null) && (!msg.getReadKeyUnknown()) && (!msg.getReplyKeyUnknown()) ) {
                _ui.debugMessage("Existing message: " + msg.getInternalId());
                _alreadyImported = true;
                return false;
            } else if ( ( (msg.getPassphrasePrompt() != null) || msg.getReadKeyUnknown() || msg.getReplyKeyUnknown()) && 
                        (_body instanceof UnreadableEnclosureBody) ) {
                _ui.debugMessage("Existing message: " + msg.getInternalId() + " still cannot be decrypted.");
                _alreadyImported = true;
                return false;
            } else {
                _ui.debugMessage("Existing message: " + msg.getInternalId());
                // we have the post, but don't have the passphrase or keys.  So...
                // delete it, then import it again clean
                _ui.debugMessage("Known message was not decrypted, so lets drop it and try again...");
                _client.deleteFromDB(_uri, _ui);
                msg = null;
            }
        }
        _ui.debugMessage("No matching messages, continuing with insert.. (" + _uri.toString() + ")"); //author != null ? author.toBase64() : "no author") + ", for " + _uri + ", msgId=" + msgId + ")");
        
        if (scopeChannelId < 0) {
            _ui.errorMessage("The message's scope is not known");
            return false;
        }
        
        long authorChannelId = -1;
        if (author != null)
            authorChannelId = _client.getChannelId(author);
        
        PreparedStatement stmt = null;
        try {
            stmt = _client.con().prepareStatement(SQL_INSERT_CHANNEL);
            //"msgId, authorChannelId, messageId, targetChannelId, subject, overwriteScopeHash, " +
            //"overwriteMessageId, forceNewThread, refuseReplies, wasEncrypted, wasPrivate, wasAuthorized, " +
            //"wasAuthenticated, isCancelled, expiration, importDate, scopeChannelId, " +
            //"readKeyMissing, replyKeyMissing, pbePrompt"
            stmt.setLong(1, msgId);
            
            if (authorChannelId >= 0)
                stmt.setLong(2, authorChannelId);
            else
                stmt.setNull(2, Types.BIGINT);
            
            if (messageId != null)
                stmt.setLong(3, messageId.longValue());
            else
                stmt.setNull(3, Types.BIGINT);
            
            stmt.setLong(4, targetChannelId);
            
            if (subject != null)
                stmt.setString(5, subject);
            else
                stmt.setNull(5, Types.VARCHAR);
            
            if (overwriteHash != null)
                stmt.setBytes(6, overwriteHash.getData());
            else
                stmt.setNull(6, Types.VARBINARY);
            
            if (overwriteMsg != null)
                stmt.setLong(7, overwriteMsg.longValue());
            else
                stmt.setNull(7, Types.BIGINT);
            
            if (forceNewThread != null)
                stmt.setBoolean(8, forceNewThread.booleanValue());
            else
                stmt.setNull(8, Types.BOOLEAN);
            
            if (refuseReplies != null)
                stmt.setBoolean(9, refuseReplies.booleanValue());
            else
                stmt.setNull(9, Types.BOOLEAN);
            
            stmt.setBoolean(10, wasEncrypted);
            stmt.setBoolean(11, wasPrivate);
            stmt.setBoolean(12, _authorized);
            stmt.setBoolean(13, _authenticated);
            stmt.setBoolean(14, false); // cancelled
            if (expiration != null)
                stmt.setDate(15, new java.sql.Date(expiration.getTime()));
            else
                stmt.setNull(15, Types.DATE);
            stmt.setLong(16, scopeChannelId);
            stmt.setBoolean(17, wasPBE);
            
            //"readKeyMissing, replyKeyMissing, pbePrompt"
            boolean readKeyMissing = false;
            boolean replyKeyMissing = false;
            String pbePrompt = null;
            
            // the metadata was authorized, but we couldn't decrypt the body.
            // that can happen if we either don't have the passphrase or if we
            // don't know the appropriate channel read key.
            if (_body instanceof UnreadableEnclosureBody) {
                pbePrompt = _enc.getHeaderString(Constants.MSG_HEADER_PBE_PROMPT);
                if (pbePrompt == null) {
                    if (wasPrivate)
                        replyKeyMissing = true;
                    else
                        readKeyMissing = true;
                }
            }
            
            stmt.setBoolean(18, readKeyMissing);
            stmt.setBoolean(19, replyKeyMissing);
            if (pbePrompt != null)
                stmt.setString(20, pbePrompt);
            else
                stmt.setNull(20, Types.VARCHAR);
            
            
            int rows = stmt.executeUpdate();
            if (rows != 1) {
                _ui.debugMessage("Post NOT imported (" + rows + ")");
                _ui.errorMessage("Error importing the post");
                return false;
            } else {
                _ui.debugMessage("Post imported...");
                return true;
            }
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    /**
     * the message may not be directly authorized for the given scope, but the
     * target channel may either allow unauthorized posts (thereby authorizing it)
     * or may allow unauthorized replies (and if we are replying to an authorized
     * post, we are thereby authorized)
     */
    private boolean isAuthorizedFor(Hash targetHash, long targetId, Hash author, Boolean forceNewThread) {
        if (targetId >= 0) {
            ChannelInfo chanInfo = _client.getChannel(targetId);
            if (chanInfo != null) {
                if ( (author != null) && 
                     (isAuth(chanInfo.getAuthorizedManagers(), author) ||
                      isAuth(chanInfo.getAuthorizedPosters(), author) ||
                      chanInfo.getIdentKey().calculateHash().equals(author)) ) {
                    // explicitly allowed to post to this channel
                    _ui.debugMessage("Message is explicitly authorized");
                    return true;
                } else if (chanInfo.getAllowPublicPosts()) {
                    // implicitly allowed to start new threads
                    _ui.debugMessage("Message is an unauthorized post to a chan that doesnt require auth, so allow it");
                    return true;
                } else if (chanInfo.getAllowPublicReplies() && ( (forceNewThread == null) || (!forceNewThread.booleanValue()) )) {
                    SyndieURI parents[] = _body.getHeaderURIs(Constants.MSG_HEADER_REFERENCES);
                    SyndieURI pubParents[] = _enc.getHeaderURIs(Constants.MSG_HEADER_REFERENCES);
                    if ((parents == null) || (parents.length == 0)) {
                        parents = pubParents;
                        _ui.debugMessage("replacing private parent set with public one");
                    } else if ( (parents != null) && (pubParents != null) && (parents.length > 0) && (pubParents.length > 0) ) {
                        SyndieURI merged[] = new SyndieURI[parents.length + pubParents.length];
                        for (int i = 0; i < parents.length; i++)
                            merged[i] = parents[i];
                        for (int i = 0; i < pubParents.length; i++)
                            merged[i+parents.length] = pubParents[i];
                        parents = merged;
                        _ui.debugMessage("Merging parent sets (" + parents.length + "/" + pubParents.length + ")");
                    }
                    if ( (parents != null) && (parents.length > 0) ) {
                        for (int i = 0; i < parents.length; i++) {
                            Hash scope = parents[i].getScope();
                            // problem: if the parent refers to a post whose scope is authorized (implicitly or
                            // explicitly), how can we tell whether that parent exists and just isn't known locally
                            // vs. whether that parent does not exist?
                            // without this differentiation, anyone could create new threads in forums that allow
                            // public replies but do not allow public posts.  perhaps this could just be addressed
                            // in a UI fashion though - only show the threads if there is a locally known authorized
                            // ancestor.
                            if (scope == null) continue;
                            if (scope.equals(targetHash)) {
                                _ui.debugMessage("Message is an unauthorized reply to an implicitly authorized post (which we may not have, and which may not even exist...), so allow it");
                                return true;
                            } else if (isAuth(chanInfo.getAuthorizedManagers(), scope) ||
                                       isAuth(chanInfo.getAuthorizedPosters(), scope)) {
                                _ui.debugMessage("parent is explicitly authorized: " + parents[i].toString());
                                return true;
                            } else {
                                _ui.debugMessage("parent is neither implicitly nor explicitly authorized: " + parents[i].toString());
                            }
                        }
                        _ui.debugMessage("done iterating over the parents of " + _uri.toString());
                    } else {
                        // no parents, and !allowPublicPosts
                        _ui.debugMessage("no parents for " + _uri);
                    }
                } else {
                    // dont allow public replies or the post tried to force a new thread and the author isn't authorized
                    _ui.debugMessage("no public replies allowed for unauthorized: " + _uri);
                }
            } else {
                _ui.debugMessage("target channel is unknown: " + targetId);
            }
        } else {
            _ui.debugMessage("target channel is unspecified");
        }
        return false;
    }
    
    static final String SQL_DELETE_MESSAGE_HIERARCHY = "DELETE FROM messageHierarchy WHERE msgId = ?";
    private static final String SQL_INSERT_MESSAGE_PARENT = "INSERT INTO messageHierarchy (msgId, referencedChannelHash, referencedMessageId, referencedCloseness) VALUES (?, ?, ?, ?)";
    private void setMessageHierarchy(long msgId) throws SQLException {
        SyndieURI refs[] = _body.getHeaderURIs(Constants.MSG_HEADER_REFERENCES);
        if (refs == null)
            refs = _enc.getHeaderURIs(Constants.MSG_HEADER_REFERENCES);
        _client.exec(SQL_DELETE_MESSAGE_HIERARCHY, msgId);
        if ( (refs != null) && (refs.length > 0) ) {
            PreparedStatement stmt = null;
            try {
                stmt = _client.con().prepareStatement(SQL_INSERT_MESSAGE_PARENT);
                int closeness = 1;
                for (int i = 0; i < refs.length; i++) {
                    Hash chan = refs[i].getScope();
                    Long msg = refs[i].getMessageId();
                    if ( (chan != null) && (msg != null) ) {
                        //(msgId, referencedChannelHash, referencedMessageId, referencedCloseness)
                        stmt.setLong(1, msgId);
                        stmt.setBytes(2, chan.getData());
                        stmt.setLong(3, msg.longValue());
                        stmt.setInt(4, closeness);
                        stmt.executeUpdate();
                        closeness++;
                    }
                }
            } finally {
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }
        }
    }

    static final String SQL_DELETE_MESSAGE_TAGS = "DELETE FROM messageTag WHERE msgId = ?";
    private static final String SQL_INSERT_MESSAGE_TAG = "INSERT INTO messageTag (msgId, tag, isPublic) VALUES (?, ?, ?)";
    private void setMessageTags(long msgId) throws SQLException {
        String privTags[] = _body.getHeaderStrings(Constants.MSG_HEADER_TAGS);
        String pubTags [] = _enc.getHeaderStrings(Constants.MSG_HEADER_TAGS);
        _client.exec(SQL_DELETE_MESSAGE_TAGS, msgId);
        if ( ( (privTags != null) && (privTags.length > 0) ) ||
             ( (pubTags != null) && (pubTags.length > 0) ) ) {
            PreparedStatement stmt = null;
            try {
                stmt = _client.con().prepareStatement(SQL_INSERT_MESSAGE_TAG);
                insertTags(stmt, msgId, privTags, false);
                insertTags(stmt, msgId, pubTags, true);
            } finally {
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }
        }
    }
    private void insertTags(PreparedStatement stmt, long msgId, String tags[], boolean isPublic) throws SQLException {
        if (tags != null) {
            for (int i = 0; i < tags.length; i++) {
                stmt.setLong(1, msgId);
                stmt.setString(2, CommandImpl.strip(tags[i]));
                stmt.setBoolean(3, isPublic);
                stmt.executeUpdate();
            }
        }
    }

    static final String SQL_DELETE_MESSAGE_ATTACHMENTS = "DELETE FROM messageAttachment WHERE msgId = ?";
    static final String SQL_DELETE_MESSAGE_ATTACHMENT_DATA = "DELETE FROM messageAttachmentData WHERE msgId = ?";
    static final String SQL_DELETE_MESSAGE_ATTACHMENT_CONFIG = "DELETE FROM messageAttachmentConfig WHERE msgId = ?";
    private void setMessageAttachments(long msgId) throws SQLException {
        _client.exec(SQL_DELETE_MESSAGE_ATTACHMENTS, msgId);
        _client.exec(SQL_DELETE_MESSAGE_ATTACHMENT_DATA, msgId);
        _client.exec(SQL_DELETE_MESSAGE_ATTACHMENT_CONFIG, msgId);
        for (int i = 0; i < _body.getAttachments(); i++)
            insertAttachment(msgId, i);
    }
    private static final String SQL_INSERT_MESSAGE_ATTACHMENT = "INSERT INTO messageAttachment (msgId, attachmentNum, attachmentSize, contentType, name, description) VALUES (?, ?, ?, ?, ?, ?)";
    private static final String SQL_INSERT_MESSAGE_ATTACHMENT_DATA = "INSERT INTO messageAttachmentData (msgId, attachmentNum, dataBinary) VALUES (?, ?, ?)";
    private static final String SQL_INSERT_MESSAGE_ATTACHMENT_CONFIG = "INSERT INTO messageAttachmentConfig (msgId, attachmentNum, dataString) VALUES (?, ?, ?)";
    private void insertAttachment(long msgId, int attachmentId) throws SQLException {
        byte data[] = _body.getAttachment(attachmentId);
        Properties attachConfig = _body.getAttachmentConfig(attachmentId);
        String type = _body.getAttachmentConfigString(attachmentId, Constants.MSG_ATTACH_CONTENT_TYPE);
        String name = _body.getAttachmentConfigString(attachmentId, Constants.MSG_ATTACH_NAME);
        String desc = _body.getAttachmentConfigString(attachmentId, Constants.MSG_ATTACH_DESCRIPTION);

        PreparedStatement stmt = null;
        try {
            String cfg = formatConfig(attachConfig);
            
            stmt = _client.con().prepareStatement(SQL_INSERT_MESSAGE_ATTACHMENT);
            //(msgId, attachmentNum, attachmentSize, contentType, name, description)
            stmt.setLong(1, msgId);
            stmt.setInt(2, attachmentId);
            stmt.setLong(3, data.length);
            if (type != null)
                stmt.setString(4, CommandImpl.strip(type));
            else
                stmt.setNull(4, Types.VARCHAR);
            if (name != null)
                stmt.setString(5, CommandImpl.strip(name));
            else
                stmt.setNull(5, Types.VARCHAR);
            if (desc != null)
                stmt.setString(6, CommandImpl.strip(desc));
            else
                stmt.setNull(6, Types.VARCHAR);
            stmt.executeUpdate();
            
            stmt.close();
            
            stmt = _client.con().prepareStatement(SQL_INSERT_MESSAGE_ATTACHMENT_DATA);
            //(msgId, attachmentNum, dataBinary)
            stmt.setLong(1, msgId);
            stmt.setInt(2, attachmentId);
            stmt.setBytes(3, data);
            stmt.executeUpdate();
            stmt.close();
            
            stmt = _client.con().prepareStatement(SQL_INSERT_MESSAGE_ATTACHMENT_CONFIG);
            //(msgId, attachmentNum, dataBinary)
            stmt.setLong(1, msgId);
            stmt.setInt(2, attachmentId);
            stmt.setString(3, cfg);
            stmt.executeUpdate();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        if ( (type != null) && ("application/x-syndie".equals(type)) ) {
            // attachment is a .syndie file - try to import it automatically
            // (though of course still honoring the bans/etc)
            importMsg(data);
        }
    }
    
    private void importMsg(byte data[]) {
        _ui.debugMessage("Post had a .syndie file attached to it, attempting to import that file");
        Importer imp = new Importer(_client);
        try {
            boolean ok = imp.processMessage(_ui, new ByteArrayInputStream(data), _client.getLoggedInNymId(), _client.getPass(), null, false, null, null);
            _ui.debugMessage("Attachment import complete.  success? " + ok);
        } catch (IOException ioe) {
            _ui.debugMessage("Attachment was corrupt", ioe);
        }
    }
    
    static final String SQL_DELETE_MESSAGE_PAGES = "DELETE FROM messagePage WHERE msgId = ?";
    static final String SQL_DELETE_MESSAGE_PAGE_DATA = "DELETE FROM messagePageData WHERE msgId = ?";
    static final String SQL_DELETE_MESSAGE_PAGE_CONFIG = "DELETE FROM messagePageConfig WHERE msgId = ?";
    private void setMessagePages(long msgId) throws SQLException {
        _client.exec(SQL_DELETE_MESSAGE_PAGES, msgId);
        _client.exec(SQL_DELETE_MESSAGE_PAGE_DATA, msgId);
        _client.exec(SQL_DELETE_MESSAGE_PAGE_CONFIG, msgId);
        for (int i = 0; i < _body.getPages(); i++)
            insertPage(msgId, i);
    }
    private static final String SQL_INSERT_MESSAGE_PAGE = "INSERT INTO messagePage (msgId, pageNum, contentType) VALUES (?, ?, ?)";
    private static final String SQL_INSERT_MESSAGE_PAGE_DATA = "INSERT INTO messagePageData (msgId, pageNum, dataString) VALUES (?, ?, ?)";
    private static final String SQL_INSERT_MESSAGE_PAGE_CONFIG = "INSERT INTO messagePageConfig (msgId, pageNum, dataString) VALUES (?, ?, ?)";
    private void insertPage(long msgId, int pageId) throws SQLException {
        PreparedStatement stmt = null;
        try {
            byte data[] = _body.getPage(pageId);
            String type = _body.getPageConfigString(pageId, Constants.MSG_PAGE_CONTENT_TYPE);

            String cfg = formatConfig(_body.getPageConfig(pageId));
            
            stmt = _client.con().prepareStatement(SQL_INSERT_MESSAGE_PAGE);
            //(msgId, pageNum, contentType)
            stmt.setLong(1, msgId);
            stmt.setInt(2, pageId);
            if (type != null)
                stmt.setString(3, CommandImpl.strip(type));
            else
                stmt.setNull(3, Types.VARCHAR);
            stmt.executeUpdate();
            
            stmt.close();
            
            stmt = _client.con().prepareStatement(SQL_INSERT_MESSAGE_PAGE_DATA);
            //(msgId, pageNum, dataString)
            stmt.setLong(1, msgId);
            stmt.setInt(2, pageId);
            if (data != null)
                stmt.setString(3, DataHelper.getUTF8(data));
            else
                stmt.setNull(3, Types.VARCHAR);
            stmt.executeUpdate();
            stmt.close();
            
            stmt = _client.con().prepareStatement(SQL_INSERT_MESSAGE_PAGE_CONFIG);
            //(msgId, pageNum, dataString)
            stmt.setLong(1, msgId);
            stmt.setInt(2, pageId);
            stmt.setString(3, cfg);
            stmt.executeUpdate();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
  
    private String formatConfig(Properties props) {
        StringBuffer rv = new StringBuffer();
        for (Iterator iter = props.keySet().iterator(); iter.hasNext(); ) {
            String key = (String)iter.next();
            String val = props.getProperty(key);
            rv.append(CommandImpl.strip(key)).append('=').append(CommandImpl.strip(val)).append('\n');
        }
        return rv.toString();
    }

    private static final String SQL_DELETE_UNREAD = "DELETE FROM nymUnreadMessage WHERE msgId = ?";
    private static final String SQL_MARK_UNREAD = "INSERT INTO nymUnreadMessage (nymId, msgId) VALUES (?, ?)";
    private void setUnread(long msgId) throws SQLException {
        _client.exec(SQL_DELETE_UNREAD, msgId);
        List nymIds = _client.getNymIds();
        
        PreparedStatement stmt = null;
        try {
            stmt = _client.con().prepareStatement(SQL_MARK_UNREAD);
            for (int i = 0; i < nymIds.size(); i++) {
                stmt.setLong(1, ((Long)nymIds.get(i)).longValue());
                stmt.setLong(2, msgId);
                stmt.executeUpdate();
            }
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    static final String SQL_DELETE_MESSAGE_REF_URIS = "DELETE FROM uriAttribute WHERE uriId IN (SELECT uriId FROM messageReference WHERE msgId = ?)";
    static final String SQL_DELETE_MESSAGE_REFS = "DELETE FROM messageReference WHERE msgId = ?";
    private void setMessageReferences(long msgId) throws SQLException {
        _client.exec(SQL_DELETE_MESSAGE_REF_URIS, msgId);
        _client.exec(SQL_DELETE_MESSAGE_REFS, msgId);
        List refs = new ArrayList();
        for (int i = 0; i < _body.getReferenceRootCount(); i++)
            refs.add(_body.getReferenceRoot(i));
        _ui.debugMessage("Importing reference roots: " + refs.size());
        InsertRefVisitor visitor = new InsertRefVisitor(msgId);
        ReferenceNode.walk(refs, visitor);
        List imported = visitor.getImportedNymKeys();
        if (imported.size() > 0)
            KeyImport.resolveWithNewKeys(_ui, _client, imported);
        if (visitor.getError() != null) {
            _ui.errorMessage(visitor.getError());
            if (visitor.getException() != null)
                throw visitor.getException();
        }
    }
    
    private static final String SQL_INSERT_MESSAGE_REF = "INSERT INTO messageReference " +
            "(msgId, referenceId, parentReferenceId, siblingOrder, name, description, uriId, refType)" +
            " VALUES (?, ?, ?, ?, ?, ?, ?, ?)";
    private class InsertRefVisitor implements ReferenceNode.Visitor {
        private long _msgId;
        private int _node;
        private SQLException _exception;
        private String _err;
        private List _nymKeys;
        public InsertRefVisitor(long msgId) {
            _msgId = msgId;
            _node = 0;
            _exception = null;
            _err = null;
            _nymKeys = new ArrayList();
        }
        public SQLException getException() { return _exception; }
        public String getError() { return _err; }
        public List getImportedNymKeys() { return _nymKeys; }

        public void visit(ReferenceNode node, int depth, int siblingOrder) {
            if (_err == null) {
                int referenceId = node.getTreeIndexNum();
                if (referenceId < 0) {
                    referenceId = _node;
                    node.setTreeIndexNum(referenceId);
                }
                int parentReferenceId = -1;
                if (node.getParent() != null)
                    parentReferenceId = node.getParent().getTreeIndexNum();
                String name = node.getName();
                String desc = node.getDescription();
                String type = node.getReferenceType();
                if (type == null) type = "URL";
                long uriId = _client.addURI(node.getURI());
                _node++;

                PreparedStatement stmt = null;
                try {
                    _ui.debugMessage("Importing reference: " + referenceId + ", uri " + uriId + ", type: " + type);
                    stmt = _client.con().prepareStatement(SQL_INSERT_MESSAGE_REF);
                    // (msgId, referenceId, parentReferenceId, siblingOrder, name, description, uriId, refType)
                    stmt.setLong(1, _msgId);
                    stmt.setInt(2, referenceId);
                    stmt.setInt(3, parentReferenceId);
                    stmt.setInt(4, siblingOrder);
                    stmt.setString(5, CommandImpl.strip(name));
                    stmt.setString(6, CommandImpl.strip(desc));
                    stmt.setLong(7, uriId);
                    if (type != null)
                        stmt.setString(8, type);
                    else
                        stmt.setNull(8, Types.VARCHAR);
                    stmt.executeUpdate();
                } catch (SQLException se) {
                    _exception = se;
                    _err = "Error inserting the reference";
                } finally {
                    if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
                }
            }

            // import keys even if there is an error with earlier references
            importKeys(node.getURI(), _nymKeys);
        }
    }
    
    /**
     * import any keys bundled in the URI that are authenticated (e.g. a channel read key
     * bundled in an authenticated post from an authorized forum manager).  adds newly created
     * NymKey instances to the provided list
     */
    private void importKeys(SyndieURI uri, List nymKeys) {
        Hash keyScope = getKeyScope(uri);
        KeyImport.importKeys(_ui, _client, keyScope, uri, nymKeys);
    }
    
    private Hash getKeyScope(SyndieURI uri) {
        Hash scope = uri.getScope();
        if (scope == null) {
            byte target[] = _enc.getHeaderBytes(Constants.MSG_HEADER_TARGET_CHANNEL);
            if (target != null)
                scope = new Hash(target);
        }
        if (scope == null)
            scope = _uri.getScope();
        return scope;
    }
    
    private static void saveToArchive(DBClient client, UI ui, Hash ident, Enclosure enc) {
        SyndieURI uri = enc.getHeaderURI(Constants.MSG_HEADER_POST_URI);
        if ( (uri == null) || (uri.getScope() == null) || (uri.getMessageId() == null) ) {
            ui.errorMessage("Unable to save the post to the archive, as the uri was not ok: " + uri);
            return;
        }
        
        File outDir = new File(client.getArchiveDir(), ident.toBase64());
        outDir.mkdirs();
        File outMeta = new File(outDir, uri.getMessageId().longValue()+Constants.FILENAME_SUFFIX);
        try {
            enc.store(outMeta.getPath());
            ui.debugMessage("Post saved to the archive at " + outMeta.getPath());
        } catch (IOException ioe) {
            ui.errorMessage("Error saving the metadata to the archive", ioe);
        }
    }
}
