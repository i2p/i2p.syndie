package syndie.db;

import gnu.crypto.hash.Sha256Standalone;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import net.i2p.data.Base64;
import net.i2p.data.DataHelper;
import net.i2p.data.Hash;
import net.i2p.data.PublicKey;
import net.i2p.data.SessionKey;
import net.i2p.data.Signature;
import net.i2p.data.SigningPrivateKey;
import net.i2p.data.SigningPublicKey;
import syndie.Constants;
import syndie.data.EnclosureBody;
import syndie.data.NymKey;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;

/**
 * MessageCreator implementation that does not rely upon MessageGen
 */
public class MessageCreatorDirect extends MessageCreator {
    private SyndieURI _createdURI;
    private Hash _authorScope;
    private Hash _targetScope;
    private Hash _postScope;
    
    private SigningPrivateKey _authorPrivKey;
    private byte _authorMask[];
    private SigningPrivateKey _authorizationPrivKey;
    
    private SessionKey _bodyEncryptSessionKey;
    private byte _bodyEncryptIV[];
    
    private byte _pbeSalt[];
    private boolean _authorizedWithoutReadKeys;
    
    private Map _publicHeaders;
    private Map _privateHeaders;
    
    /** unencrypted zipped up body - pages/attachments/private headers/refs/etc */
    private byte[] _rawBody;
    private boolean _isPrivateMsg;
    
    private File _targetFile;
    
    private List _errors;
    
    public MessageCreatorDirect(MessageCreatorSource src) { 
        super(src);
        _errors = new ArrayList();
    }
    
    public void execute() {
        prepareAttributes(); // figure out the keys/scope/messageId/etc
        preparePublicHeaders(); // what everyone can see
        preparePrivateHeaders(); // what only authorized readers can see
        prepareBody(); // build the zip body
        writeMessage(); // encrypt the zip body and write the signed header+encrypted body to disk
        notifyCompletion(); // drink a beer
    }

    private void prepareAttributes() {
        Hash preferredTarget = _source.getTarget();
        Hash preferredAuthor = _source.getAuthor();
        Hash preferredSignAs = _source.getSignAs();
        _isPrivateMsg = _source.getPrivacyReply();
        
        Hash authorizeAs = (preferredSignAs != null ? preferredSignAs : preferredAuthor);
        
        _targetScope = preferredTarget;
        _authorScope = preferredAuthor;
        
        boolean explicitlyAuthorizedToPost = isExplicitlyAuthorizedToPost(_targetScope, authorizeAs);
        if (explicitlyAuthorizedToPost) {
            _postScope = _targetScope;
            if (_source.getAuthorHidden() && (preferredSignAs != null)) {
                _authorMask = new byte[Signature.SIGNATURE_BYTES];
                _source.getClient().ctx().random().nextBytes(_authorMask);
            }
        } else if (preferredSignAs != null) {
            _postScope = preferredSignAs;
            // might be implicitly authorized to post: 
            //  replying to a post in a forum that only authorized posters can post to 
            //  but you can read while still preventing those not authorized to read the
            //  forum from knowing who is posting
            if (_source.getAuthorHidden()) {
                _authorMask = new byte[Signature.SIGNATURE_BYTES];
                _source.getClient().ctx().random().nextBytes(_authorMask);
            }
        } else {
            _postScope = preferredAuthor;
        }

        _ui.debugMessage("preparing attributes: target=" + _targetScope + " author=" + _authorScope + " postScope=" + _postScope + " authorHidden? " + _source.getAuthorHidden() + " mask set? " + (_authorMask != null));
        
        prepareKeys();
        
        long messageId = MessageGen.createMessageId(_source.getClient());
        _createdURI = SyndieURI.createMessage(_postScope, messageId);
    }
    
    private void prepareKeys() {
        NymKey author = getIdent(_authorScope, true);
        NymKey postScope = getIdent(_postScope, false);
        
        if (author != null)
            _authorPrivKey = new SigningPrivateKey(author.getData());
        if (postScope != null)
            _authorizationPrivKey = new SigningPrivateKey(postScope.getData());
        
        _ui.debugMessage("preparing keys: authorPriv " + (_authorPrivKey != null ? "set " : "not set") +
                         " postPriv " + (_authorizationPrivKey != null ? "set" : "not set"));
        
        prepareEncryptionKey();
    }
    
    private void prepareEncryptionKey() {
        selectUsableEncryptionKey();
        if (_bodyEncryptSessionKey == null) {
            _ui.debugMessage("preparing keys: use a new encryption key");
            prepareNewEncryptionKey();
        }
        
        byte iv[] = new byte[16];
        Random r = _source.getClient().ctx().random();
        r.nextBytes(iv);
        _bodyEncryptIV = iv;
    }
    
    private void selectUsableEncryptionKey() {
        if (_isPrivateMsg) {
            // when sending a private message, we always use an asym key
            _ui.debugMessage("preparing keys: private message, no sym key reuse");
        } else if (_source.getPrivacyPublic()) {
            // when we want everyone to read the message, always create a new key
            // and publicize it in the headers - don't use a known read key
            _ui.debugMessage("preparing keys: public message, no sym key reuse");
        } else if (_source.getPrivacyPBE()) {
            _pbeSalt = new byte[32];
            Random r = _source.getClient().ctx().random();
            r.nextBytes(_pbeSalt);
            byte passphraseUTF8[] = DataHelper.getUTF8(_source.getPassphrase());
            SessionKey pbeKey = _source.getClient().ctx().keyGenerator().generateSessionKey(_pbeSalt, passphraseUTF8);
            _ui.debugMessage("Encrypting with derived PBE key");
            _bodyEncryptSessionKey = pbeKey;
        } else {
            // "read keys" are symmetric keys known only to authorized channel readers -
            // we encrypt a post using one of these keys (that hasn't yet expired) and anyone
            // authorized to read the channel (aka anyone with these keys) can decrypt it.
            List keys = _source.getClient().getReadKeys(_targetScope, true);
            if ( (keys != null) && (keys.size() > 0) ) {
                Random r = _source.getClient().ctx().random();
                int which = r.nextInt(keys.size());
                SessionKey key = (SessionKey)keys.get(which);
                _bodyEncryptSessionKey = key;
            }
            _ui.debugMessage("preparing keys: normal message, sym key reuse enabled: using " + (_bodyEncryptSessionKey != null ? "a key" : "no key") 
            + " out of " + (keys != null ? keys.size() : 0) + " known");
        }
    }
    
    private void prepareNewEncryptionKey() {
        Random r = _source.getClient().ctx().random();
        byte key[] = new byte[SessionKey.KEYSIZE_BYTES];
        r.nextBytes(key);
        _bodyEncryptSessionKey = new SessionKey(key);
    }
    
    private NymKey getIdent(Hash scope, boolean identOnly) {
        List manage = _source.getClient().getNymKeys(scope, Constants.KEY_FUNCTION_MANAGE);
        List post = _source.getClient().getNymKeys(scope, Constants.KEY_FUNCTION_POST);
        List keys = new ArrayList();
        keys.addAll(manage);
        keys.addAll(post);
        for (int i = 0; i < keys.size(); i++) {
            NymKey key = (NymKey)keys.get(i);
            SigningPrivateKey priv = new SigningPrivateKey(key.getData());
            SigningPublicKey pub = priv.toPublic();
            Hash pubScope = pub.calculateHash();
            boolean isIdent = pubScope.equals(scope);
            if (isIdent) {
                _ui.debugMessage("ident key for " + scope.toBase64().substring(0,6) + " found");
                return key;
            } else {
                _ui.debugMessage("authorized third party key targets " + scope.toBase64().substring(0,6) + ": " + pubScope.toBase64().substring(0,6));
                if (!identOnly)
                    return key;
            }
        }
        _ui.debugMessage("No matching key known for " + scope.toBase64().substring(0,6) + " out of known: " + post + " / " + manage);
        return null;
    }
    
    private boolean isExplicitlyAuthorizedToPost(Hash target, Hash postAs) {
        long channelId = _source.getClient().getChannelId(target);
        Set authorizedPosters = _source.getClient().getChannelAuthorizedPosters(channelId, true, true, true);
        boolean isAuth = authorizedPosters.contains(postAs);
        _ui.debugMessage(postAs.toBase64().substring(0,6) + (isAuth ? " IS " : " IS NOT ") + "authorized to post in " + target.toBase64().substring(0,6));
        return isAuth;
    }
    
    private void preparePublicHeaders() {
        Map headers = new HashMap();
        if (_isPrivateMsg)
            headers.put(Constants.MSG_HEADER_TYPE, Constants.MSG_TYPE_REPLY);
        else
            headers.put(Constants.MSG_HEADER_TYPE, Constants.MSG_TYPE_POST);
        
        if ( (_targetScope != null) && (!_targetScope.equals(_postScope)) )
            headers.put(Constants.MSG_HEADER_TARGET_CHANNEL, _targetScope.toBase64());
        
        headers.put(Constants.MSG_HEADER_POST_URI, _createdURI.toString());
        
        String tags[] = _source.getPublicTags();
        if ( (tags != null) && (tags.length > 0) )
            headers.put(Constants.MSG_HEADER_TAGS, formatTags(tags));
        
        if (_source.getPrivacyPBE()) {
            String prompt = CommandImpl.strip(_source.getPassphrasePrompt());
            headers.put(Constants.MSG_HEADER_PBE_PROMPT, prompt);
            headers.put(Constants.MSG_HEADER_PBE_PROMPT_SALT, Base64.encode(_pbeSalt));
        } else if (_source.getPrivacyPublic()) {
            headers.put(Constants.MSG_HEADER_BODYKEY, _bodyEncryptSessionKey.toBase64());
            // another scenario where we would want to publicize the encryption key is if we
            // are authorized to post but don't have any read keys.  however, since publicizing
            // the body key means making the message publicly readable, it'd be better to push
            // that choice up to the UI rather than "silently" making a publicly readable post.
            // so, the ui should detect whether the user has any read keys, and if not, don't
            // offer to make the post "authorized readers only".
        }
        _publicHeaders = headers;
    }
    
    private void preparePrivateHeaders() {
        Map headers = new HashMap();
        
        String tags[] = _source.getPrivateTags();
        if ( (tags != null) && (tags.length > 0) )
            headers.put(Constants.MSG_HEADER_TAGS, formatTags(tags));
        
        StringBuffer parentBuf = new StringBuffer();
        for (int i = 0; i < _source.getParentCount(); i++) {
            SyndieURI uri = _source.getParent(i);
            if (!uri.isChannel())
                continue;
            Hash parentScope = uri.getScope();
            if (parentScope == null)
                continue;
            Long messageId = uri.getMessageId();
            if (messageId == null)
                continue;
            // we rewrite it in case there are other attributes on the URI that are either
            // sensitive or contain unsafe chars
            parentBuf.append(SyndieURI.createMessage(parentScope, messageId.longValue()).toString());
            if (i + 1 < _source.getParentCount())
                parentBuf.append('\t');
        }
        if (parentBuf.length() > 0)
            headers.put(Constants.MSG_HEADER_REFERENCES, parentBuf.toString());
        
        if (_source.getForceNewThread())
            headers.put(Constants.MSG_HEADER_FORCE_NEW_THREAD, Boolean.TRUE.toString());
        
        if (_source.getRefuseReplies())
            headers.put(Constants.MSG_HEADER_REFUSE_REPLIES, Boolean.TRUE.toString());
        
        String subj = _source.getSubject();
        if (subj != null)
            headers.put(Constants.MSG_HEADER_SUBJECT, CommandImpl.strip(subj));
        
        String expire = _source.getExpiration();
        if (expire != null)
            headers.put(Constants.MSG_HEADER_EXPIRATION, CommandImpl.strip(expire));
        
        if (_authorPrivKey != null) {
            Hash author = _authorPrivKey.toPublic().calculateHash();
            headers.put(Constants.MSG_HEADER_AUTHOR, author.toBase64());
            if (_authorMask != null) {
                _ui.debugMessage("prep priv headers: author privkey set, as is author mask");
                headers.put(Constants.MSG_HEADER_AUTHENTICATION_MASK, Base64.encode(_authorMask));
            } else {
                _ui.debugMessage("prep priv headers: author privkey set, but author mask is not");
            }
        } else {
            _ui.debugMessage("prep priv headers: author privkey is not set");
        }
        
        if (_targetScope != null)
            headers.put(Constants.MSG_HEADER_TARGET_CHANNEL, _targetScope.toBase64());
        else
            headers.put(Constants.MSG_HEADER_TARGET_CHANNEL, _postScope.toBase64());
        
        _privateHeaders = headers;
    }
    
    private static final String formatTags(String tags[]) {
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < tags.length; i++) {
            String stripped = CommandImpl.strip(tags[i]);
            if (stripped.length() > 0)
                buf.append(stripped).append('\t');
        }
        return buf.toString();
    }
    
    private void prepareBody() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream(4*1024);
        ZipOutputStream zos = new ZipOutputStream(baos);
        try {
            if ( (_privateHeaders != null) && (_privateHeaders.size() > 0) ) {
                ZipEntry entry = new ZipEntry(EnclosureBody.ENTRY_HEADERS);
                entry.setTime(0);
                zos.putNextEntry(entry);
                CommandImpl.write(_privateHeaders, zos);
                zos.flush();
                zos.closeEntry();
                _ui.debugMessage("Private headers included (size=" + _privateHeaders.size() + ")");
            } else {
                _ui.debugMessage("Private headers NOT included");
            }

            byte avatar[] = getAvatar();
            if ( (avatar != null) && (avatar.length > 0) ) {
                ZipEntry entry = new ZipEntry(EnclosureBody.ENTRY_AVATAR);
                entry.setTime(0);
                entry.setSize(avatar.length);
                zos.putNextEntry(entry);
                zos.write(avatar);
                zos.closeEntry();
            }

            List refs = _source.getReferenceNodes();
            if ( (refs != null) && (refs.size() > 0) ) {
                String refsStr = ReferenceNode.walk(refs);
                _ui.debugMessage("References string is " + refsStr.length() + " bytes long");
                ZipEntry entry = new ZipEntry(EnclosureBody.ENTRY_REFERENCES);
                entry.setTime(0);
                byte ref[] = DataHelper.getUTF8(refsStr);
                entry.setSize(ref.length);
                zos.putNextEntry(entry);
                zos.write(ref);
                zos.closeEntry();
            } else {
                _ui.debugMessage("No references included");
            }

            int pages = _source.getPageCount();
            for (int page = 0; page < pages; page++) {
                byte data[] = DataHelper.getUTF8(_source.getPageContent(page));
                String contentType = _source.getPageType(page);
                byte cfgData[] = DataHelper.getUTF8(Constants.MSG_PAGE_CONTENT_TYPE + '=' + contentType + '\n');

                ZipEntry entry = new ZipEntry(EnclosureBody.ENTRY_PAGE_PREFIX + page + EnclosureBody.ENTRY_PAGE_DATA_SUFFIX);
                entry.setTime(0);
                entry.setSize(data.length);
                zos.putNextEntry(entry);
                zos.write(data);
                zos.closeEntry();

                entry = new ZipEntry(EnclosureBody.ENTRY_PAGE_PREFIX + page + EnclosureBody.ENTRY_PAGE_CONFIG_SUFFIX);
                entry.setTime(0);
                entry.setSize(cfgData.length);
                zos.putNextEntry(entry);
                zos.write(cfgData);
                zos.closeEntry();
            }

            List attachmentTypes = _source.getAttachmentTypes();
            List attachmentNames = _source.getAttachmentNames();
            if (attachmentTypes != null) {
                for (int i = 0; i < attachmentTypes.size(); i++) {
                    String type = (String)attachmentTypes.get(i);
                    String name = (String)attachmentNames.get(i);
                    byte data[] = _source.getAttachmentData(i+1);

                    ZipEntry entry = new ZipEntry(EnclosureBody.ENTRY_ATTACHMENT_PREFIX + i + EnclosureBody.ENTRY_ATTACHMENT_DATA_SUFFIX);
                    entry.setTime(0);
                    entry.setSize(data.length);
                    zos.putNextEntry(entry);
                    zos.write(data);
                    zos.closeEntry();

                    StringBuffer buf = new StringBuffer();
                    if (name != null)
                        buf.append(Constants.MSG_ATTACH_NAME).append('=').append(CommandImpl.strip(name)).append('\n');
                    if (type != null)
                        buf.append(Constants.MSG_ATTACH_CONTENT_TYPE).append('=').append(CommandImpl.strip(type.trim())).append('\n');
                    else
                        buf.append(Constants.MSG_ATTACH_CONTENT_TYPE).append('=').append("application/octet-stream").append('\n');

                    byte cfgData[] = DataHelper.getUTF8(buf.toString());

                    entry = new ZipEntry(EnclosureBody.ENTRY_ATTACHMENT_PREFIX + i + EnclosureBody.ENTRY_ATTACHMENT_CONFIG_SUFFIX);
                    entry.setTime(0);
                    entry.setSize(cfgData.length);
                    zos.putNextEntry(entry);
                    zos.write(cfgData);
                    zos.closeEntry();
                }
            }

            zos.close();
        } catch (IOException ioe) {
            _ui.errorMessage("Internal error serializing the message", ioe);
        }

        _rawBody = baos.toByteArray();
    }
    private byte[] getAvatar() {
        String avatarFilename = _source.getAvatarUnmodifiedFilename();
        if (avatarFilename == null) {
            byte avatar[] = _source.getAvatarModifiedData();
            if (avatar != null)
                return avatar;
        }
        
        if (avatarFilename != null)
            return CommandImpl.read(_ui, avatarFilename, Constants.MAX_AVATAR_SIZE);
        else
            return null;
    }
    private void writeMessage() {
        byte encryptedBody[] = null;
        
        DBClient client = _source.getClient();
        
        if (_isPrivateMsg) { // asym private message
            PublicKey targetPubReplyKey = client.getChannelReplyKey(_targetScope);
            encryptedBody = CommandImpl.encryptBody(client.ctx(), _rawBody, targetPubReplyKey, _bodyEncryptIV, _bodyEncryptSessionKey);
        } else { // sym post (possibly public)
            encryptedBody = CommandImpl.encryptBody(client.ctx(), _rawBody, _bodyEncryptSessionKey, getClass());
        }
        
        File out = pickTargetFile();
        FileOutputStream fos = null;
        try {
            fos = new FileOutputStream(out);
            Sha256Standalone hash = new Sha256Standalone();
            DataHelper.write(fos, DataHelper.getUTF8(Constants.TYPE_CURRENT+"\n"), hash);
            TreeSet ordered = new TreeSet(_publicHeaders.keySet());
            for (Iterator iter = ordered.iterator(); iter.hasNext(); ) {
                String key = (String)iter.next();
                String val = (String)_publicHeaders.get(key);
                DataHelper.write(fos, DataHelper.getUTF8(key + '=' + val + '\n'), hash);
            }
            DataHelper.write(fos, DataHelper.getUTF8("\nSize=" + encryptedBody.length + "\n"), hash);
            DataHelper.write(fos, encryptedBody, hash);
            
            byte authorizationHash[] = ((Sha256Standalone)hash.clone()).digest(); // digest() reset()s
            byte sig[] = null;
            if (_authorizationPrivKey != null) {
                sig = client.ctx().dsa().sign(new Hash(authorizationHash), _authorizationPrivKey).getData();
            } else {
                sig = new byte[Signature.SIGNATURE_BYTES];
                client.ctx().random().nextBytes(sig);
            }
            //_ui.debugMessage("Authorization hash: " + Base64.encode(authorizationHash) + " sig: " + Base64.encode(sig));
            DataHelper.write(fos, DataHelper.getUTF8("AuthorizationSig=" + Base64.encode(sig) + "\n"), hash);
            
            byte authenticationHash[] = hash.digest();
            sig = null;
            if (_authorPrivKey != null) {
                sig = client.ctx().dsa().sign(new Hash(authenticationHash), _authorPrivKey).getData();
                if (_authorMask != null) {
                    DataHelper.xor(sig, 0, _authorMask, 0, sig, 0, sig.length);
                }
            } else {
                sig = new byte[Signature.SIGNATURE_BYTES];
                client.ctx().random().nextBytes(sig);
            }
            //_ui.debugMessage("Authentication hash: " + Base64.encode(authenticationHash) + " sig: " + Base64.encode(sig));
            DataHelper.write(fos, DataHelper.getUTF8("AuthenticationSig=" + Base64.encode(sig) + "\n"), hash);
            
            fos.close();
            fos = null;
            _targetFile = out;
        } catch (IOException ioe) {
            _ui.errorMessage("Error writing the message", ioe);
            out.delete();
        } finally {
            if (fos != null) try { fos.close(); } catch (IOException ioe) {}
        }
    }

    private File pickTargetFile() {
        File outDir = _source.getClient().getOutboundDir();
        File scopeDir = new File(outDir, _postScope.toBase64());
        if (!scopeDir.exists())
            scopeDir.mkdirs();
        return new File(scopeDir, _createdURI.getMessageId().longValue() + Constants.FILENAME_SUFFIX);
    }
    
    private void notifyCompletion() {
        StringBuffer err = null;
        if (_errors.size() > 0) {
            err = new StringBuffer();
            for (int i = 0; i < _errors.size(); i++)
                err.append((String)_errors.get(i)).append("\n");
        }
        if (err != null)
            _source.getListener().creationComplete(this, null, err.toString(), false, null, null, null);
        else
            _source.getListener().creationComplete(this, _createdURI, null, true, _bodyEncryptSessionKey, _bodyEncryptIV, _targetFile);
    }
}
