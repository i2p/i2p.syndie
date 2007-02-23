package syndie.db;

import gnu.crypto.hash.Sha256Standalone;
import java.io.*;
import java.net.URISyntaxException;
import java.sql.SQLException;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import net.i2p.I2PAppContext;
import net.i2p.data.*;
import syndie.Constants;
import syndie.data.EnclosureBody;
import syndie.data.NymKey;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;

/**
 *CLI messagegen
 * --db $dbURL
 * --login $login                  // keys/etc are pulled from the db, but the
 * --pass $pass                    // post itself is not imported via the CLI post
 //* [--channel $base64(channelHash)]// required, unless --simple
 * [--targetChannel $base64(channelHash)]
 * [--scopeChannel $base64(channelHash)]
 * (--page$n $filename --page$n-config $filename)*
 * (--attach$n $filename --attach$n-config $filename)*
 * [--authenticationKey $base64(privKey)] // what key to use to authenticate our post?
 * [--authorizationKey $base64(privKey)]  
 * [--messageId $id]               // if unspecified, randomize(trunc(now()))
 * [--subject $subject]            // short description of the post
 * [--postAsUnauthorized $boolean] // if true, encrypt with a random key and publicize it in the BodyKey public header
 * [--avatar $filename]            // overrides the avatar listed in the postAs channel metadata
 * [--encryptContent $boolean]     // if true, encrypt the content with a known read key for the channel
 * [--bodyPassphrase $passphrase --bodyPassphrasePrompt $prompt]
 *                                 // derive the body key from the passphrase, and include a publicly
 *                                 // visible hint to prompt it
 * [--postAsReply $boolean]        // if true, the post should be encrypted to the channel's reply key
 * [--pubTag $tag]*                // publicly visible tags
 * [--privTag $tag]*               // tags in the encrypted body
 * [--refs $channelRefGroupFile]   // ([\t]*$name\t$uri\t$refType\t$description\n)* lines
 * (--cancel $uri)*                // posts to be marked as cancelled (only honored if authorized to do so for those posts)
 * [--overwrite $uri]              // replace the $uri with the current post, if authorized to do so
 * [--references $uri[,$uri]*]     // ordered list of previous posts in the thread, newest first
 * [--expiration $yyyymmdd]        // date after which the post should be dropped
 * [--forceNewThread $boolean]     // if true, this post begins a new thread, even if there are references
 * [--refuseReplies $boolean]      // if true, only the author can reply to this post
 * [--authorHidden $boolean]       // if true, the author should be permuted and stored in the encrypted headers
 * [--author ($base64(pubKeyHash)|anon)]
 *                                 // if set, explicitly use the given author (or no author)
 * [--simple $boolean]             // if true, default the $channel and $authenticationKey to the nym's blog,
 *                                 // the $authorizationKey to the nym's blog (or if the nym has a post or manage key for the target channel,
 *                                 // one of those keys), default --encryptContent to true if a readKey is known
 * --out $filename
 */
public class MessageGen extends CommandImpl {
    private byte _replyIV[];
    public MessageGen() {}
    public byte[] getReplyIV() { return _replyIV; }
    public DBClient runCommand(Opts args, UI ui, DBClient client) {
        if ( (client == null) || (!client.isLoggedIn()) ) {
            List missing = args.requireOpts(new String[] { "db", "login", "pass", "out" });
            if (missing.size() > 0) {
                ui.errorMessage("Invalid options, missing " + missing);
                ui.commandComplete(-1, null);
                return client;
            }
        } else {
            List missing = args.requireOpts(new String[] { "out" });
            if (missing.size() > 0) {
                ui.errorMessage("Invalid options, missing " + missing);
                ui.commandComplete(-1, null);
                return client;
            }    
        }
        
        Hash targetChannel = null;
        Hash scopeChannel = null;
        
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
                return client;
            }
        
            if (args.getOptBoolean("simple", true)) {
                boolean ok = updateSimpleArgs(client, ui, nymId, args);
                if (!ok) {
                    ui.commandComplete(-1, null);
                    return client;
                }
            }

            byte val[] = args.getOptBytes("scopeChannel");
            if ( (val == null) || (val.length != Hash.HASH_LENGTH) ) {
                ui.errorMessage("Invalid scope channel");
                ui.commandComplete(-1, null);
                return client;
            } else {
                scopeChannel = new Hash(val);
            }
            
            long chanId = client.getChannelId(scopeChannel);
            if (chanId < 0) {
                ui.errorMessage("Cannot post to " + scopeChannel.toBase64() + ", as it isn't known locally");
                ui.commandComplete(-1, null);
                return client;
            }
            
            val = args.getOptBytes("targetChannel");
            if ( (val == null) || (val.length != Hash.HASH_LENGTH) ) {
                // ok, targetting the scope channel
                targetChannel = scopeChannel;
            } else {
                targetChannel = new Hash(val);
            }
            
            long targetChanId = client.getChannelId(targetChannel);
            if (targetChanId < 0) {
                ui.errorMessage("Cannot target " + targetChannel.toBase64() + ", as it isn't known locally");
                ui.commandComplete(-1, null);
                return client;
            }
            
            boolean signAsHidden = args.getOptBoolean("signAsHidden", false);
            byte signAsB[] = args.getOptBytes("signAs");
            Hash signAs = (signAsB != null ? new Hash(signAsB) : null);
            
            byte sessKey[] = args.getOptBytes("replySessionKey");
            SessionKey replySessionKey = null;
            if (sessKey != null)
                replySessionKey = new SessionKey(sessKey);
            
            boolean ok = false;
            if (args.getOptBoolean("postAsReply", false))
                ok = genMessage(client, ui, nymId, chanId, targetChanId, scopeChannel, targetChannel, args, client.getReplyKey(targetChanId), signAs, signAsHidden, replySessionKey);
            else
                ok = genMessage(client, ui, nymId, chanId, targetChanId, scopeChannel, targetChannel, args, null, signAs, signAsHidden, null);
            
            if (ok)
                ui.commandComplete(0, null);
            else
                ui.commandComplete(-1, null);
        } catch (SQLException se) {
            ui.errorMessage("Invalid database URL", se);
            ui.commandComplete(-1, null);
        //} finally {
        //    if (client != null) client.close();
        }
        return client;
    }
    
    private boolean genMessage(DBClient client, UI ui, long nymId, long scopeChannelId, long targetChannelId, Hash scopeChannel, Hash targetChannel, Opts args, PublicKey to, Hash signAs, boolean signAsHidden, SessionKey replySessionKey) throws SQLException {
        List readKeys = client.getReadKeys(targetChannel, nymId, client.getPass(), true);
        SessionKey bodyKey = null;
        boolean postAsUnauthorized = args.getOptBoolean("postAsUnauthorized", false);
        
        if ( (readKeys == null) || (readKeys.size() <= 0) ) {
            if (!postAsUnauthorized && (to == null)) {
                ui.errorMessage("We are not authorized to post (or don't have any keys to post with) and ");
                ui.errorMessage("we haven't been asked to --postAsUnauthorized.  aborting.");
                return false;
            }
        }
        
        SigningPrivateKey authorizationPrivate = null;
        SigningPrivateKey authenticationPrivate = null;
        
        List targetSignKeys = client.getSignKeys(targetChannel, nymId, client.getPass());
        Map signKeyHashes = new HashMap();
        for (Iterator iter = targetSignKeys.iterator(); iter.hasNext(); ) {
            SigningPrivateKey key = (SigningPrivateKey)iter.next();
            signKeyHashes.put(key.calculateHash(), key);
        }
        List scopeSignKeys = client.getSignKeys(scopeChannel, nymId, client.getPass());
        for (Iterator iter = scopeSignKeys.iterator(); iter.hasNext(); ) {
            SigningPrivateKey key = (SigningPrivateKey)iter.next();
            signKeyHashes.put(key.calculateHash(), key);
        }
        
        byte key[] = args.getOptBytes("authorizationKey");
        if ( (key != null) && (key.length == Hash.HASH_LENGTH) ) {
            authorizationPrivate = (SigningPrivateKey)signKeyHashes.get(new Hash(key));   
            if (authorizationPrivate == null) {
                ui.errorMessage("Authorization key w/ H()=" + Base64.encode(key) + " was not known for scope channel " + scopeChannel.toBase64() + " / " + targetChannel.toBase64() + " / " + nymId);
                ui.errorMessage("Known hashes: " + signKeyHashes.keySet());
                return false;
            }
        }
        
        boolean unauthorized = false;
        byte authenticationMask[] = null;
        key = args.getOptBytes("authenticationKey");
        if ( (key != null) && (key.length == Hash.HASH_LENGTH) ) {
            authenticationPrivate = (SigningPrivateKey)signKeyHashes.get(new Hash(key));
            if (authenticationPrivate == null) {
                List authOnly = client.getNymKeys(client.getLoggedInNymId(), client.getPass(), null, null);
                for (int i = 0; i < authOnly.size(); i++) {
                    NymKey nymKey = (NymKey)authOnly.get(i);
                    if (Constants.KEY_FUNCTION_POST.equals(nymKey.getFunction()) ||
                        Constants.KEY_FUNCTION_MANAGE.equals(nymKey.getFunction())) {
                        SigningPrivateKey authPriv = new SigningPrivateKey(nymKey.getData());
                        if (authPriv.calculateHash().equals(new Hash(key))) {
                            ui.debugMessage("Authenticating as a third party: " + client.ctx().keyGenerator().getSigningPublicKey(authPriv).calculateHash().toBase64().substring(0,6));
                            authenticationPrivate = authPriv;
                            unauthorized = true;
                            break;
                        }
                    }
                }
                if (authenticationPrivate == null) {
                    ui.errorMessage("Authentication key w/ H()=" + Base64.encode(key) + " was not known");
                    ui.errorMessage("Known hashes: " + signKeyHashes.keySet());
                    return false;
                }
            }
            if (!unauthorized && signAsHidden) {
                authenticationMask = new byte[Signature.SIGNATURE_BYTES];
                client.ctx().random().nextBytes(authenticationMask);
            }
        }

        Hash uriChannel = scopeChannel;
        boolean bodyKeyIsPublic = false;
        
        if (postAsUnauthorized || args.getOptBoolean("postAsReply", false)) {
            ui.debugMessage("creating a new body key (postAsUnaut? " + postAsUnauthorized + ", postAsReply? " + args.getOptBoolean("postAsReply", false) + ")");
            bodyKey = client.ctx().keyGenerator().generateSessionKey();
            if (!args.getOptBoolean("postAsReply", false))
                bodyKeyIsPublic = true;
            if (authenticationPrivate != null) {
                SigningPublicKey pub = client.ctx().keyGenerator().getSigningPublicKey(authenticationPrivate);
                uriChannel = pub.calculateHash();
            } else if (authorizationPrivate != null) {
                SigningPublicKey pub = client.ctx().keyGenerator().getSigningPublicKey(authorizationPrivate);
                uriChannel = pub.calculateHash();
            }
        } else {
            int index = client.ctx().random().nextInt(readKeys.size());
            bodyKey = (SessionKey)readKeys.get(index);
            bodyKeyIsPublic = false;
            ui.debugMessage("using a known read key");
        }
        
        byte salt[] = null;
        if ( (args.getOptValue("bodyPassphrase") != null) && (args.getOptValue("bodyPassphrasePrompt") != null) ) {
            salt = new byte[32];
            client.ctx().random().nextBytes(salt);
            SessionKey pbeKey = client.ctx().keyGenerator().generateSessionKey(salt, DataHelper.getUTF8(args.getOptValue("bodyPassphrase")));
            ui.debugMessage("Encrypting with PBE key " + Base64.encode(pbeKey.getData()) + " derived from " + args.getOptValue("bodyPassphrase") + " and salted with " + Base64.encode(salt));
            bodyKey = pbeKey;
        }
        
        Map publicHeaders = generatePublicHeaders(client, ui, args, uriChannel, targetChannel, bodyKey, bodyKeyIsPublic, salt, postAsUnauthorized);
        Map privateHeaders = generatePrivateHeaders(client, ui, args, targetChannel, authenticationPrivate, authenticationMask);
        
        String refStr = null;
        String filename = args.getOptValue("refs");
        if (filename != null) {
            refStr = readRefs(ui, filename);
            ui.debugMessage("Reading refs from " + filename + ", came up with " + (refStr != null ? refStr.length() + " chars": "no file"));;
        }
                
        String out = args.getOptValue("out");
        byte avatar[] = read(ui, args.getOptValue("avatar"), Constants.MAX_AVATAR_SIZE);
        try {
            byte zipped[] = prepareBody(args, ui, privateHeaders, refStr, avatar);
            boolean written = writeMessage(client, ui, out, authorizationPrivate, authenticationPrivate, authenticationMask, to, bodyKey, publicHeaders, avatar, zipped, replySessionKey);
            if (!written)
                return false;
            else
                return true;
        } catch (IOException ioe) {
            ui.errorMessage("Error writing the message", ioe);
            return false;
        }
    }
    
    private boolean writeMessage(DBClient client, UI ui, String out, SigningPrivateKey authorizationPrivate, SigningPrivateKey authenticationPrivate, byte[] authenticationMask, PublicKey to, SessionKey bodyKey, Map pubHeaders, byte[] avatar, byte[] zipped, SessionKey replySessionKey) throws IOException {
        FileOutputStream fos = null;
        try {
            byte encBody[] = null;
            if (to == null) {
                ui.debugMessage("Encrypting the message with the body key " + bodyKey.toBase64());
                encBody = encryptBody(client.ctx(), zipped, bodyKey);
            } else {
                ui.debugMessage("Encrypting the message to the reply key " + to.calculateHash().toBase64());
                byte iv[] = new byte[16];
                encBody = encryptBody(client.ctx(), zipped, to, iv, replySessionKey);
                _replyIV = iv;
            }
            fos = new FileOutputStream(out);
            Sha256Standalone hash = new Sha256Standalone();
            DataHelper.write(fos, DataHelper.getUTF8(Constants.TYPE_CURRENT+"\n"), hash);
            TreeSet ordered = new TreeSet(pubHeaders.keySet());
            for (Iterator iter = ordered.iterator(); iter.hasNext(); ) {
                String key = (String)iter.next();
                String val = (String)pubHeaders.get(key);
                DataHelper.write(fos, DataHelper.getUTF8(key + '=' + val + '\n'), hash);
            }
            DataHelper.write(fos, DataHelper.getUTF8("\nSize=" + encBody.length + "\n"), hash);
            DataHelper.write(fos, encBody, hash);
            
            byte authorizationHash[] = ((Sha256Standalone)hash.clone()).digest(); // digest() reset()s
            byte sig[] = null;
            if (authorizationPrivate != null) {
                sig = client.ctx().dsa().sign(new Hash(authorizationHash), authorizationPrivate).getData();
            } else {
                sig = new byte[Signature.SIGNATURE_BYTES];
                client.ctx().random().nextBytes(sig);
            }
            ui.debugMessage("Authorization hash: " + Base64.encode(authorizationHash) + " sig: " + Base64.encode(sig));
            DataHelper.write(fos, DataHelper.getUTF8("AuthorizationSig=" + Base64.encode(sig) + "\n"), hash);
            
            byte authenticationHash[] = hash.digest();
            sig = null;
            if (authenticationPrivate != null) {
                sig = client.ctx().dsa().sign(new Hash(authenticationHash), authenticationPrivate).getData();
                if ( (authenticationMask != null) && (authorizationPrivate != null) )
                    DataHelper.xor(sig, 0, authenticationMask, 0, sig, 0, sig.length);
            } else {
                sig = new byte[Signature.SIGNATURE_BYTES];
                client.ctx().random().nextBytes(sig);
            }
            ui.debugMessage("Authentication hash: " + Base64.encode(authenticationHash) + " sig: " + Base64.encode(sig));
            DataHelper.write(fos, DataHelper.getUTF8("AuthenticationSig=" + Base64.encode(sig) + "\n"), hash);
            
            fos.close();
            fos = null;
            return true;
        } catch (IOException ioe) {
            ui.errorMessage("Error writing the message", ioe);
            return false;
        } finally {
            if (fos != null) try { fos.close(); } catch (IOException ioe) {}
        }
    }

    /**
     * zip up all of the data expected to be in the encrypted body
     */
    private byte[] prepareBody(Opts args, UI ui, Map privateHeaders, String refsStr, byte avatar[]) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream(4*1024);
        ZipOutputStream zos = new ZipOutputStream(baos);
        if ( (privateHeaders != null) && (privateHeaders.size() > 0) ) {
            ZipEntry entry = new ZipEntry(EnclosureBody.ENTRY_HEADERS);
            entry.setTime(0);
            zos.putNextEntry(entry);
            write(privateHeaders, zos);
            zos.flush();
            zos.closeEntry();
            ui.debugMessage("Private headers included (size=" + privateHeaders.size() + ")");
        } else {
            ui.debugMessage("Private headers NOT included");
        }
        if ( (avatar != null) && (avatar.length > 0) ) {
            ZipEntry entry = new ZipEntry(EnclosureBody.ENTRY_AVATAR);
            entry.setTime(0);
            entry.setSize(avatar.length);
            zos.putNextEntry(entry);
            zos.write(avatar);
            zos.closeEntry();
        }
        if (refsStr != null) {
            ui.debugMessage("References string is " + refsStr.length() + " bytes long");
            ZipEntry entry = new ZipEntry(EnclosureBody.ENTRY_REFERENCES);
            entry.setTime(0);
            byte ref[] = DataHelper.getUTF8(refsStr);
            entry.setSize(ref.length);
            zos.putNextEntry(entry);
            zos.write(ref);
            zos.closeEntry();
        } else {
            ui.debugMessage("No references included");
        }
        
        int page = 0;
        while (true) {
            String dataFile = args.getOptValue("page" + page);
            String cfgFile = args.getOptValue("page" + page + "-config");
            if (dataFile != null) {
                byte data[] = read(ui, dataFile, 4*1024*1024); // align the max page size with the max post size (though still pretty absurd... 4MB of HTML?)
                if (data == null)
                    throw new IOException("Data for page " + page + " not found in " + dataFile);
                
                ZipEntry entry = new ZipEntry(EnclosureBody.ENTRY_PAGE_PREFIX + page + EnclosureBody.ENTRY_PAGE_DATA_SUFFIX);
                entry.setTime(0);
                entry.setSize(data.length);
                zos.putNextEntry(entry);
                zos.write(data);
                zos.closeEntry();

                if (cfgFile != null) {
                    data = read(ui, cfgFile, 32*1024);
                    if (data == null)
                        throw new IOException("Config for page " + page + " not found in " + cfgFile);

                    entry = new ZipEntry(EnclosureBody.ENTRY_PAGE_PREFIX + page + EnclosureBody.ENTRY_PAGE_CONFIG_SUFFIX);
                    entry.setTime(0);
                    entry.setSize(data.length);
                    zos.putNextEntry(entry);
                    zos.write(data);
                    zos.closeEntry();
                }
                
                page++;
            } else {
                break;
            }
        }
        
        int attachment = 0;
        while (true) {
            String dataFile = args.getOptValue("attach" + attachment);
            String cfgFile = args.getOptValue("attach" + attachment + "-config");
            if (dataFile != null) {
                byte data[] = read(ui, dataFile, 4*1024*1024);
                if (data == null)
                    throw new IOException("Data for attachment " + attachment + " not found in " + dataFile);
                
                ZipEntry entry = new ZipEntry(EnclosureBody.ENTRY_ATTACHMENT_PREFIX + attachment + EnclosureBody.ENTRY_ATTACHMENT_DATA_SUFFIX);
                entry.setTime(0);
                entry.setSize(data.length);
                zos.putNextEntry(entry);
                zos.write(data);
                zos.closeEntry();

                if (cfgFile != null) {
                    data = read(ui, cfgFile, 32*1024);
                    if (data == null)
                        throw new IOException("Config for attachment " + attachment + " not found in " + cfgFile);

                    entry = new ZipEntry(EnclosureBody.ENTRY_ATTACHMENT_PREFIX + attachment + EnclosureBody.ENTRY_ATTACHMENT_CONFIG_SUFFIX);
                    entry.setTime(0);
                    entry.setSize(data.length);
                    zos.putNextEntry(entry);
                    zos.write(data);
                    zos.closeEntry();
                }
                
                attachment++;
            } else {
                break;
            }
        }
        
        zos.close();
        
        byte raw[] = baos.toByteArray();
        return raw;
    }
    
    public static long createMessageId(DBClient client) {
        long now = client.ctx().clock().now();
        now = now - (now % 24*60*60*1000);
        now += client.ctx().random().nextLong(24*60*60*1000);
        return now;
    }
    private Map generatePublicHeaders(DBClient client, UI ui, Opts args, Hash channel, Hash targetChannel, SessionKey bodyKey, boolean bodyKeyIsPublic, byte salt[], boolean postAsUnauthorized) {
        Map rv = new HashMap();
        if (args.getOptBoolean("postAsReply", false)) {
            rv.put(Constants.MSG_HEADER_TYPE, Constants.MSG_TYPE_REPLY);
            //if (!targetChannel.equals(channel))
            rv.put(Constants.MSG_HEADER_TARGET_CHANNEL, targetChannel.toBase64());
        } else {
            rv.put(Constants.MSG_HEADER_TYPE, Constants.MSG_TYPE_POST);
            if (!targetChannel.equals(channel))
                rv.put(Constants.MSG_HEADER_TARGET_CHANNEL, targetChannel.toBase64());
        }
        
        // tags
        List tags = args.getOptValues("pubTag");
        if ( (tags != null) && (tags.size() > 0) ) {
            StringBuffer buf = new StringBuffer();
            for (int i = 0; i < tags.size(); i++) {
                String stripped = strip((String)tags.get(i));
                if (stripped.length() > 0)
                    buf.append(stripped).append('\t');
            }
            rv.put(Constants.MSG_META_HEADER_TAGS, buf.toString());
        }
        
        long msgId = args.getOptLong("messageId", -1);
        if (msgId < 0) { // YYYYMMDD+rand
            msgId = createMessageId(client);
        }
        rv.put(Constants.MSG_HEADER_POST_URI, strip(SyndieURI.createMessage(channel, msgId).toString()));
        
        //args.getOptBytes("author");
        
        if ( (args.getOptValue("bodyPassphrase") != null) && (args.getOptValue("bodyPassphrasePrompt") != null) ) {
            String prompt = strip(args.getOptValue("bodyPassphrasePrompt"));
            rv.put(Constants.MSG_HEADER_PBE_PROMPT, prompt);
            rv.put(Constants.MSG_HEADER_PBE_PROMPT_SALT, Base64.encode(salt));
        } else if ( (bodyKeyIsPublic) ||
                    (!args.getOptBoolean("encryptContent", false) || postAsUnauthorized) && 
                    (!args.getOptBoolean("postAsReply", false)) ) {
            // if we are NOT trying to privately encrypt the content (or if we are acting as if
            // we don't know the channel's read key(s)), then publicize the bodyKey in the public
            // headers (so anyone can open the zip content and read the private headers/refs/avatar/etc)
            rv.put(Constants.MSG_HEADER_BODYKEY, strip(bodyKey.toBase64()));
        }
        
        ui.debugMessage("public headers: " + rv);
        return rv;
    }
    private Map generatePrivateHeaders(DBClient client, UI ui, Opts args, Hash channel, SigningPrivateKey authenticationPrivate, byte authenticationMask[]) {
        Map rv = new HashMap();
        
        // tags
        List tags = args.getOptValues("privTag");
        if ( (tags != null) && (tags.size() > 0) ) {
            StringBuffer buf = new StringBuffer();
            for (int i = 0; i < tags.size(); i++) {
                String stripped = strip((String)tags.get(i));
                if (stripped.length() > 0)
                    buf.append(stripped).append('\t');
            }
            rv.put(Constants.MSG_META_HEADER_TAGS, buf.toString());
        }
        
        String referenceStrings = args.getOptValue("references");
        if (referenceStrings != null) {
            StringBuffer refs = new StringBuffer();
            String refList[] = Constants.split(',', referenceStrings);
            for (int i = 0; i < refList.length; i++) {
                try {
                    SyndieURI uri = new SyndieURI(refList[i]);
                    refs.append(strip(uri.toString()));
                    refs.append('\t');
                } catch (URISyntaxException use) {
                    // invalid
                    ui.errorMessage("URI reference is not valid: " + refList[i], use);
                }
            }
            rv.put(Constants.MSG_HEADER_REFERENCES, refs.toString());
        }
        
        String overwrite = args.getOptValue("overwrite");
        if (overwrite != null) {
            try {
                SyndieURI uri = new SyndieURI(overwrite);
                rv.put(Constants.MSG_HEADER_OVERWRITE, strip(uri.toString()));
            } catch (URISyntaxException use) {
                ui.debugMessage("Overwrite URI is not valid: " + overwrite, use);
            }
        }
        
        if (args.getOptBoolean("forceNewThread", false))
            rv.put(Constants.MSG_HEADER_FORCE_NEW_THREAD, Boolean.TRUE.toString());
    
        if (args.getOptBoolean("refuseReplies", false))
            rv.put(Constants.MSG_HEADER_REFUSE_REPLIES, Boolean.TRUE.toString());
        
        List cancel = args.getOptValues("cancel");
        if (cancel != null) {
            StringBuffer refs = new StringBuffer();
            for (int i = 0; i < cancel.size(); i++) {
                String ref = (String)cancel.get(i);
                try {
                    SyndieURI uri = new SyndieURI(ref);
                    refs.append(strip(uri.toString()));
                    refs.append('\t');
                } catch (URISyntaxException use) {
                    // invalid
                    ui.debugMessage("Cancelled URI reference is not valid: " + ref, use);
                }
            }
            rv.put(Constants.MSG_HEADER_CANCEL, refs.toString());
        }
        
        String val = args.getOptValue("subject");
        if (val != null)
            rv.put(Constants.MSG_HEADER_SUBJECT, strip(val));
        
        String expiration = args.getOptValue("expiration");
        if (val != null)
            rv.put(Constants.MSG_HEADER_EXPIRATION, strip(expiration));

        if (authenticationPrivate != null) {
            SigningPublicKey ident = client.ctx().keyGenerator().getSigningPublicKey(authenticationPrivate);
            rv.put(Constants.MSG_HEADER_AUTHOR, ident.calculateHash().toBase64());
            if (authenticationMask != null)
                rv.put(Constants.MSG_HEADER_AUTHENTICATION_MASK, Base64.encode(authenticationMask));
        }
        
        rv.put(Constants.MSG_HEADER_TARGET_CHANNEL, channel.toBase64());
        
        ui.debugMessage("private headers: " + rv);
        return rv;
    }

    /**
     * default the $channel and $authenticationKey to the nym's blog, the $authorizationKey
     * to the nym's blog (or if the nym has a post or manage key for the target channel,
     * one of those keys), default --encryptContent to true if a readKey is known
     */
    private boolean updateSimpleArgs(DBClient client, UI ui, long nymId, Opts args) {
        List keys = client.getNymKeys(nymId, client.getPass(), null, null);
        Hash channel = null;
        Hash authenticationKey = null;
        Hash authorizationKey = null;
        SessionKey readKey = null;
        
        byte chan[] = args.getOptBytes("scopeChannel");
        if (chan == null) {
            for (int i = 0; i < keys.size(); i++) {
                NymKey key = (NymKey)keys.get(i);
                if (Constants.KEY_FUNCTION_MANAGE.equals(key.getFunction())) {
                    if (channel == null) {
                        SigningPrivateKey k = new SigningPrivateKey(key.getData());
                        SigningPublicKey pub = client.ctx().keyGenerator().getSigningPublicKey(k);
                        channel = pub.calculateHash();
                    } else {
                        ui.errorMessage("Cannot use simple mode, as no channel was specified but multiple management keys are known");
                        channel = null;
                        return false;
                    }
                }
            }
        } else {
            channel = new Hash(chan);
        }
        
        if (channel == null) 
            return false;

        byte k[] = args.getOptBytes("authenticationKey");
        if (k != null)
            authenticationKey = new Hash(k);
        k = args.getOptBytes("authorizationKey");
        if (k != null)
            authorizationKey = new Hash(k);
        
        for (int i = 0; i < keys.size(); i++) {
            NymKey key = (NymKey)keys.get(i);
            if (Constants.KEY_FUNCTION_MANAGE.equals(key.getFunction())) {
                SigningPrivateKey priv = new SigningPrivateKey(key.getData());
                SigningPublicKey pub = client.ctx().keyGenerator().getSigningPublicKey(priv);
                Hash privChan = pub.calculateHash();
                // take the first authentication key found, as its probably our blog
                if (authenticationKey == null) 
                    authenticationKey = priv.calculateHash();
                // take the authorization key associated with the target chan
                if ((authorizationKey == null) && privChan.equals(channel))
                    authorizationKey = priv.calculateHash();
            } else if (Constants.KEY_FUNCTION_READ.equals(key.getFunction())) {
                if (key.getChannel().equals(channel))
                    readKey = new SessionKey(key.getData());
            }
        }
        //if ( (authenticationKey != null) && (authorizationKey == null) )
        //    authorizationKey = authenticationKey; // self-authorized, may not be sufficient
        
        if ( (readKey == null) && (channel != null) ) {
            List read = client.getReadKeys(channel, nymId, client.getPass(), true);
            if ( (read != null) && (read.size() > 0) ) {
                int index = client.ctx().random().nextInt(read.size());
                readKey = (SessionKey)read.get(index);
            }
        }
        
        if ( (authenticationKey != null) && 
             //(authorizationKey != null) && 
             (channel != null) ) {
            // ok, found what we need
            List chans = args.getOptValues("targetChannel");
            if ( (chans == null) || (chans.size() <= 0) )
                args.addOptValue("targetChannel", channel.toBase64());
            else
                chans.add(channel.toBase64());
            chans = args.getOptValues("scopeChannel");
            if ( (chans == null) || (chans.size() <= 0) )
                args.addOptValue("scopeChannel", channel.toBase64());
            else
                chans.add(channel.toBase64());
            
            keys = args.getOptValues("authenticationKey");
            if ( (keys == null) || (keys.size() <= 0) )
                args.addOptValue("authenticationKey", authenticationKey.toBase64());
            else
                keys.add(authenticationKey.toBase64());
            
            if (authorizationKey != null) {
                keys = args.getOptValues("authorizationKey");
                if ( (keys == null) || (keys.size() <= 0) ) 
                    args.addOptValue("authorizationKey", authorizationKey.toBase64());
                else
                    keys.add(authorizationKey.toBase64());
            }

            if ( (readKey != null) && (args.getOptValue("encryptContent") == null) )
                args.setOptValue("encryptContent", "true");
            return true;
        } else {
            ui.errorMessage("Auth keys not found, cant use simple mode");
            return false;
        }
    }
    
    public static void omain(String args[]) {
        try {
        CLI.main(new String[] { "messagegen", 
                                "--db", "jdbc:hsqldb:file:/tmp/cli",
                                "--login", "j",
                                "--pass", "j",
                                "--postAsReply", "true",
                                "--channel", "2klF2vDob7M82j8ZygZ-s9LmOHfaAdso5V0DzLvHISI=",
                                "--page0", "/etc/passwd", "--page0-config", "/dev/null",
                                "--attach0", "/etc/resolv.conf", "--attach0-config", "/dev/null",
                                "--authenticationKey", "bOdorbv8kVon7dEHHaFzuhz8qNMfX9Izcrh-rzZ0x6U=",
                                "--authorizationKey", "bOdorbv8kVon7dEHHaFzuhz8qNMfX9Izcrh-rzZ0x6U=",
                                "--simple", "false",
                                "--out", "/tmp/messageOut"
                                 });
        } catch (Exception e) { e.printStackTrace(); }
    }
    // example of the scriptability:
    /*
    public static void main(String args[]) {
        TextUI ui = new TextUI(true);
        ui.insertCommand("login --db jdbc:hsqldb:file:/tmp/textui --login j --pass j");
        ui.insertCommand("menu read");
        ui.insertCommand("channels");
        ui.insertCommand("messages --channel 0");
        ui.insertCommand("view --message 4");
        TextEngine engine = new TextEngine(ui);
        engine.run();
    }
     */
    public static void main(String args[]) {
        try {
        CLI.main(new String[] { "messagegen", 
                                "--db", "jdbc:hsqldb:file:/tmp/cli",
                                "--login", "bar",
                                "--pass", "bar",
                                "--page0", "/etc/passwd", "--page0-config", "/dev/null",
                                "--attach0", "/etc/resolv.conf", "--attach0-config", "/dev/null",
                                "--simple", "true",
                                "--out", "/tmp/simpleOut"
                                 });
        } catch (Exception e) { e.printStackTrace(); }
    }
}
