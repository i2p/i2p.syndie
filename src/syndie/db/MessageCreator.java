package syndie.db;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Random;
import java.util.Set;

import net.i2p.I2PAppContext;
import net.i2p.data.Base64;
import net.i2p.data.DataHelper;
import net.i2p.data.Hash;
import net.i2p.data.SessionKey;
import net.i2p.data.SigningPrivateKey;
import net.i2p.data.SigningPublicKey;
import net.i2p.util.SecureFile;
import net.i2p.util.SecureFileOutputStream;

import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.NymKey;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;

/**
 *  See MessageCreatorDirect extension
 */
public class MessageCreator {
    //private MessageEditor _editor;
    protected final MessageCreatorSource _source;
    private SyndieURI _createdURI;
    private final StringBuilder _errorBuf;
    protected final UI _ui;
    
    private List _tempFiles;
    private File _refFile;

    public interface ExecutionListener {
        public void creationComplete(MessageCreator exec, SyndieURI uri, String errors, boolean successful, SessionKey replySessionKey, byte[] replyIV, File msg);
    }
    
    public MessageCreator(MessageCreatorSource source) {
        _source = source;
        _ui = source.getUI();
        _errorBuf = new StringBuilder();
    }
    
    /**
     * if we are authorized to post in the target channel, then do 
     * so.  otherwise, post in the author's channel
     */
    private Hash getScope(Hash author, Hash target, Hash signAs, boolean authorHidden) {
        if (signAs != null) {
            if (authorHidden || (author == null))
                return signAs;
            else
                return author;
        }
        //System.out.println("getScope [author = " + author + " target = " + target + "]");
        if (target == null) {
            return author;
        } else if (target.equals(author)) {
            return target;
        }
        
        if (!authorHidden)
            return author;
        
        DBClient client = _source.getClient();
        
        long chanId = client.getChannelId(target);
        ChannelInfo info = client.getChannel(chanId);
        //todo: handle this properly
        //if (info.getAllowPublicPosts())
        //    return target;
        for (Iterator iter = info.getAuthorizedManagers().iterator(); iter.hasNext(); ) {
            SigningPublicKey key = (SigningPublicKey)iter.next();
            if (key.calculateHash().equals(author))
                return target;
        }
        for (Iterator iter = info.getAuthorizedPosters().iterator(); iter.hasNext(); ) {
            SigningPublicKey key = (SigningPublicKey)iter.next();
            if (key.calculateHash().equals(author))
                return target;
        }
        
        // the target isn't going to let us, so use the author's
        // todo: (though maybe per reply it would?)
        return author;
    }
    
    public SyndieURI getCreatedURI() { return _createdURI; }
    public String getErrors() { return _errorBuf.toString(); }
    
    /**
     * a port of syndie.db.PostMenu.processExecute() to the GUI.
     * Overridden in MessageCreatorDirect
     */
    public void execute() {
        _errorBuf.setLength(0);
        DBClient client = _source.getClient();
        Hash author = _source.getAuthor();
        Hash target = _source.getTarget();
        Hash signAs = _source.getSignAs();
        boolean authorHidden = _source.getAuthorHidden();
        Hash scope = getScope(author, target, signAs, authorHidden);
        
        NestedUI ui = new NestedUI(_ui);
        
        long messageId = MessageGen.createMessageId(client, 0);
        
        String out = null;
        if (out == null) {
            File chanDir = new SecureFile(client.getOutboundDir(), scope.toBase64());
            chanDir.mkdirs();
            File msgFile = new File(chanDir, messageId + Constants.FILENAME_SUFFIX);
            out = msgFile.getPath();
        }
        
        File tmpDir = client.getTempDir();
        tmpDir.mkdirs();
        
        List tempFiles = new ArrayList();
        File refFile = null;
        
        MessageGen cmd = new MessageGen();
        Opts genOpts = new Opts();
        genOpts.setCommand("messagegen");
        if (target != null) {
            genOpts.setOptValue("targetChannel", target.toBase64());
        }
        genOpts.addOptValue("scopeChannel", scope.toBase64());
        if (signAs != null) {
            if (author != null)
                genOpts.addOptValue("author", author.toBase64());
            else
                genOpts.addOptValue("author", "anon");
            //genOpts.addOptValue("signAs", signAs.toBase64());
        }
        genOpts.addOptValue("authorHidden", authorHidden ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
        
        for (int i = 0; i < _source.getPageCount(); i++) {
            String content = _source.getPageContent(i);
            String contentType = _source.getPageType(i);
            String title = _source.getPageTitle(i);
            if (title != null) {
                title = title.replace('\n', ' ');
                title = title.replace('\r', ' ');
                title = title.replace('\t', ' ');
            }
            
            FileOutputStream fos = null;
            try {
                File pageFile = SecureFile.createTempFile("pageData", ""+i, tmpDir);
                fos = new SecureFileOutputStream(pageFile);
                fos.write(DataHelper.getUTF8(content));
                fos.close();
                fos = null;
                tempFiles.add(pageFile);
                
                String filename = pageFile.getAbsolutePath();
                
                File cfgFile = SecureFile.createTempFile("pageConfig", ""+ i, tmpDir);
                fos = new SecureFileOutputStream(cfgFile);
                fos.write(DataHelper.getUTF8(Constants.MSG_PAGE_CONTENT_TYPE + '=' + contentType + '\n'));
                if (title != null)
                    fos.write(DataHelper.getUTF8(Constants.MSG_PAGE_TITLE + '=' + title + '\n'));
                fos.close();
                fos = null;
                tempFiles.add(cfgFile);
                genOpts.setOptValue("page" + i, filename);
                genOpts.setOptValue("page" + i + "-config", cfgFile.getPath());
            } catch (IOException ioe) {
                _errorBuf.append("Error writing out the configuration for page " + i + ": " + ioe.getMessage());
                cleanup(tempFiles, refFile);
                _source.getListener().creationComplete(this, null, _errorBuf.toString(), false, null, null, null);
                return;
            }
        }

        List names = _source.getAttachmentNames();
        List types = _source.getAttachmentTypes();
        for (int i = 0; i < names.size(); i++) {
            String fname = (String)names.get(i);
            //String desc = null; // the ui doesn't have a way to specify the attachment description
            String type = (String)types.get(i);
            byte data[] = _source.getAttachmentData(i+1);
            
            FileOutputStream fos = null;
            try {
                File attachFile = SecureFile.createTempFile("attachData", ""+i, tmpDir);
                fos = new SecureFileOutputStream(attachFile);
                fos.write(data);
                fos.close();
                fos = null;
                data = null;
                tempFiles.add(attachFile);

                String filename = attachFile.getAbsolutePath();
            
                File cfgFile = SecureFile.createTempFile("attachConfig", ""+ i, tmpDir);
                fos = new SecureFileOutputStream(cfgFile);
                if (fname != null)
                    fos.write(DataHelper.getUTF8(Constants.MSG_ATTACH_NAME + '=' + CommandImpl.strip(fname.trim()) + '\n'));
                //if (desc != null)
                //    fos.write(DataHelper.getUTF8(Constants.MSG_ATTACH_DESCRIPTION + '=' + CommandImpl.strip(desc.trim()) + '\n'));
                if (type != null)
                    fos.write(DataHelper.getUTF8(Constants.MSG_ATTACH_CONTENT_TYPE + '=' + CommandImpl.strip(type.trim()) + '\n'));
                else
                    fos.write(DataHelper.getUTF8(Constants.MSG_ATTACH_CONTENT_TYPE + "=application/octet-stream\n"));
                fos.close();
                fos = null;
                tempFiles.add(cfgFile);
                genOpts.setOptValue("attach" + i, filename);
                genOpts.setOptValue("attach" + i + "-config", cfgFile.getPath());
            } catch (IOException ioe) {
                _errorBuf.append("Error writing out the configuration for attachment " + i + ": " + ioe.getMessage());
                cleanup(tempFiles, refFile);
                _source.getListener().creationComplete(this, null, _errorBuf.toString(), false, null, null, null);
                return;
            }
        }

        Hash authenticationKey = null;
        Hash authorizationKey = null;
        
        List targetKeys = client.getNymKeys(target, null);
        for (int i = 0; i < targetKeys.size(); i++) {
            NymKey key = (NymKey)targetKeys.get(i);
            if (signAs != null) {
                if (Constants.KEY_TYPE_DSA.equals(key.getType())) {
                    SigningPrivateKey priv = new SigningPrivateKey(key.getData());
                    SigningPublicKey pub = priv.toPublic();
                    if (pub.calculateHash().equals(signAs)) {
                        authorizationKey = priv.calculateHash();
                        break;
                    }
                }
            } else {
                if (Constants.KEY_FUNCTION_MANAGE.equals(key.getFunction()) ||
                    Constants.KEY_FUNCTION_POST.equals(key.getFunction())) {
                    authorizationKey = client.sha256(key.getData());
                    break;
                }
            }
        }
        
        if (author != null) {
            List authorKeys = client.getNymKeys(author, Constants.KEY_FUNCTION_MANAGE);
            for (int i = 0; i < authorKeys.size(); i++) {
                NymKey key = (NymKey)authorKeys.get(i);
                //if (author != null) {
                    SigningPrivateKey priv = new SigningPrivateKey(key.getData());
                    SigningPublicKey pub = priv.toPublic();
                    if (author.equals(pub.calculateHash())) {
                        authenticationKey = priv.calculateHash();
                    }
                //} else {
                //    if (key.isIdentity()) {
                //        authenticationKey = client.sha256(key.getData());
                //        break;
                //    }
                //}
            }
        } // author may be null if signAs is set
        
        long targetChanId = client.getChannelId(target);
        ChannelInfo targetChan = client.getChannel(targetChanId);
        
        if (authenticationKey != null)
            //genOpts.setOptValue("authenticationKey", Base64.encode(_authenticationKey.getData()));
            genOpts.setOptValue("authenticationKey", authenticationKey.toBase64());
        if (authorizationKey != null) {
            //genOpts.setOptValue("authorizationKey", Base64.encode(_authorizationKey.getData()));
            genOpts.setOptValue("authorizationKey", authorizationKey.toBase64());
        } else {
            boolean noAuthRequired = false;
            if (targetChan != null) {
                if (targetChan.getAllowPublicPosts()) {
                    noAuthRequired = true;
                } else if (targetChan.getAllowPublicReplies()) {
                    for (int i = 0; i < _source.getParentCount(); i++) {
                        SyndieURI parent = _source.getParent(i);
                        Set allowed = new HashSet();
                        for (Iterator iter = targetChan.getAuthorizedManagers().iterator(); iter.hasNext(); )
                            allowed.add(((SigningPublicKey)iter.next()).calculateHash());
                        for (Iterator iter = targetChan.getAuthorizedPosters().iterator(); iter.hasNext(); )
                            allowed.add(((SigningPublicKey)iter.next()).calculateHash());
                        allowed.add(targetChan.getChannelHash());
                        if (allowed.contains(parent.getScope())) {
                            noAuthRequired = true;
                            break;
                        }
                    }
                }
            }
            if (!noAuthRequired)
                genOpts.setOptValue("postAsUnauthorized", "true");
        }
        
        genOpts.setOptValue("messageId", Long.toString(messageId));
        genOpts.setOptValue("subject", _source.getSubject().trim());
 
        String passphrase = (_source.getPrivacyPBE() ? _source.getPassphrase() : null);
        String passphrasePrompt = _source.getPassphrasePrompt();
        if ( (_source.getPrivacyPBE()) && (passphrase != null) && (passphrasePrompt != null) ) {
            genOpts.setOptValue("bodyPassphrase", CommandImpl.strip(passphrase));
            genOpts.setOptValue("bodyPassphrasePrompt", CommandImpl.strip(passphrasePrompt));
        } else if (_source.getPrivacyPublic()) {
            genOpts.setOptValue("encryptContent", "false"); // if true, encrypt the content with a known read key for the channel
        } else {
            genOpts.setOptValue("encryptContent", "true"); // if true, encrypt the content with a known read key for the channel
        }
        
        String avatarFilename = _source.getAvatarUnmodifiedFilename();
        if (avatarFilename == null) {
            byte avatar[] = _source.getAvatarModifiedData();
            if (avatar != null) {
                try {
                    File avatarFile = SecureFile.createTempFile("avatar", ".png", tmpDir);
                    FileOutputStream fos = new SecureFileOutputStream(avatarFile);
                    fos.write(avatar);
                    fos.close();
                    fos = null;
                    tempFiles.add(avatarFile);
                    avatarFilename = avatarFile.getAbsolutePath();
                } catch (IOException ioe) {
                    _errorBuf.append("Error writing out the avatar: " + ioe.getMessage());
                    cleanup(tempFiles, refFile);
                    _source.getListener().creationComplete(this, null, _errorBuf.toString(), false, null, null, null);
                    return;
                }
            }
        }
        if (avatarFilename != null)
            genOpts.setOptValue("avatar", avatarFilename);
        byte[] rskData = new byte[SessionKey.KEYSIZE_BYTES];
		I2PAppContext.getGlobalContext().random().nextBytes(rskData);
        SessionKey replySessionKey = new SessionKey(rskData);
        if (_source.getPrivacyReply()) {
            genOpts.setOptValue("postAsReply", "true"); // if true, the post should be encrypted to the channel's reply key
            genOpts.setOptValue("replySessionKey", replySessionKey.toBase64());
        } else {
            replySessionKey = null;
        }
        
        String tags[] = _source.getPublicTags();
        for (int i = 0; i < tags.length; i++) {
            if (tags[i] != null) {
                String str = tags[i].trim();
                if (str.length() > 0)
                    genOpts.addOptValue("pubTag", str);
            }
        }
        tags = _source.getPrivateTags();
        for (int i = 0; i < tags.length; i++) {
            if (tags[i] != null) {
                String str = tags[i].trim();
                if (str.length() > 0)
                    genOpts.addOptValue("privTag", str);
            }
        }
        
        List referenceNodes = _source.getReferenceNodes();
        if (referenceNodes.size() > 0) {
            String refs = ReferenceNode.walk(referenceNodes);
            FileOutputStream fos = null;
            try {
                refFile = SecureFile.createTempFile("refs", "txt", tmpDir);
                fos = new SecureFileOutputStream(refFile);
                fos.write(DataHelper.getUTF8(refs));
                fos.close();
                tempFiles.add(refFile);
                genOpts.setOptValue("refs", refFile.getPath());
                ui.debugMessage("Pulling refs from " + refFile.getPath());
            } catch (IOException ioe) {
                _errorBuf.append("Error writing out the references: " + ioe.getMessage());
                cleanup(tempFiles, refFile);
                _source.getListener().creationComplete(this, null, _errorBuf.toString(), false, null, null, null);
                return;
            }
        }
        //* (--cancel $uri)*                // posts to be marked as cancelled (only honored if authorized to do so for those posts)
        
        // replace the $uri with the current post, if authorized to do so
        //if ( (_currentMessage.getOverwriteChannel() != null) && (_currentMessage.getOverwriteMessage() >= 0) )
        //    genOpts.setOptValue("overwrite", SyndieURI.createMessage(_currentMessage.getOverwriteChannel(), _currentMessage.getOverwriteMessage()).toString());

        StringBuilder parentBuf = new StringBuilder();
        for (int i = 0; i < _source.getParentCount(); i++) {
            SyndieURI uri = _source.getParent(i);
            parentBuf.append(uri.toString());
            if (i + 1 < _source.getParentCount())
                parentBuf.append(",");
        }
        if (parentBuf.length() > 0)
            genOpts.setOptValue("references", parentBuf.toString());

        String expiration = _source.getExpiration();
        if (expiration != null)
            genOpts.setOptValue("expiration", expiration);
        
        genOpts.setOptValue("forceNewThread", ""+_source.getForceNewThread());
        genOpts.setOptValue("refuseReplies", ""+_source.getRefuseReplies());
        
        genOpts.setOptValue("out", out);
        
        // we are explicit above regarding the keys to use
        genOpts.setOptValue("simple", Boolean.FALSE.toString());
        
        NestedUI nestedUI = new NestedUI(ui);
        ui.debugMessage("generating with opts: " + genOpts);
        cmd.runCommand(genOpts, nestedUI, client);
        byte replyIV[] = cmd.getReplyIV();
        
        _tempFiles = tempFiles;
        _refFile = refFile;

        _createdURI = SyndieURI.createMessage(scope, messageId);

        if (nestedUI.getExitCode() < 0)
            _source.getListener().creationComplete(this, null, _errorBuf.toString(), false, null, null, null);
        else
            _source.getListener().creationComplete(this, _createdURI, null, true, replySessionKey, replyIV, new File(out));
    }
    
    public boolean importCreated(DBClient client, UI ui, SyndieURI uri, File out, byte[] replyIV, SessionKey replySessionKey, String passphrase) {
        // generated fine, so lets import 'er
        ui.statusMessage("Message generated and written to " + out);

        Importer msgImp = new Importer();
        Opts msgImpOpts = new Opts();
        msgImpOpts.setOptValue("in", out.getAbsolutePath());
        if (passphrase != null)
            msgImpOpts.setOptValue("passphrase", CommandImpl.strip(passphrase));

        if ( (replySessionKey != null) && (replyIV != null) ) {
            msgImpOpts.setOptValue("replySessionKey", replySessionKey.toBase64());
            msgImpOpts.setOptValue("replyIV", Base64.encode(replyIV));
        }

        msgImpOpts.setCommand("import");
        NestedUI dataNestedUI = new NestedUI(ui);
        msgImp.runCommand(msgImpOpts, dataNestedUI, client);
        if (dataNestedUI.getExitCode() < 0) {
            _errorBuf.append("Failed in the nested import command");
            cleanup();
            return false;
        } else {
            ui.statusMessage("Post imported");
            ui.commandComplete(0, null);
            cleanup();

            long msgId = _source.getClient().getMessageId(uri.getScope(), uri.getMessageId());
            if (msgId >= 0)
                _source.getClient().markMessageRead(msgId);
            return true;
        }
    }

    public void cleanup() {
        cleanup(_tempFiles, _refFile);
    }
    
    private void cleanup(List tempFiles, File refFile) {
        if (tempFiles != null)
            for (int i = 0; i < tempFiles.size(); i++)
                ((File)tempFiles.get(i)).delete();
        if (refFile != null)
            refFile.delete();
    }
}
