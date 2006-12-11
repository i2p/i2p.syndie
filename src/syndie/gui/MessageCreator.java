package syndie.gui;

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
import net.i2p.data.DataHelper;
import net.i2p.data.Hash;
import net.i2p.data.SigningPublicKey;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.NymKey;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.CommandImpl;
import syndie.db.DBClient;
import syndie.db.Importer;
import syndie.db.MessageGen;
import syndie.db.NestedUI;
import syndie.db.NullUI;
import syndie.db.Opts;
import syndie.db.UI;

/**
 
 */
class MessageCreator {
    //private MessageEditor _editor;
    private MessageCreatorSource _editor;
    private SyndieURI _createdURI;
    private StringBuffer _errorBuf;
    private UI _ui;
    
    public MessageCreator(MessageCreatorSource editor) {
        _editor = editor;
        _ui = editor.getUI();
        _errorBuf = new StringBuffer();
    }
    
    /**
     * if we are authorized to post in the target channel, then do 
     * so.  otherwise, post in the author's channel
     */
    private Hash getScope(Hash author, Hash target) {
        //System.out.println("getScope [author = " + author + " target = " + target + "]");
        if (target == null) {
            return author;
        } else if (target.equals(author)) {
            return target;
        }
        DBClient client = _editor.getClient();
        // getAuthorizedPosters includes the managers and identities
        List authorized = client.getAuthorizedPosters(target);
        for (int i = 0; i < authorized.size(); i++) {
            SigningPublicKey key = (SigningPublicKey)authorized.get(i);
            if (key.calculateHash().equals(author))
                return target;
        }
        return author;
    }
    
    public SyndieURI getCreatedURI() { return _createdURI; }
    public String getErrors() { return _errorBuf.toString(); }
    
    /**
     * a port of syndie.db.PostMenu.processExecute() to the GUI.
     *
     * @return true if the post was created and imported successfully
     */
    public boolean execute() {
        _errorBuf.setLength(0);
        DBClient client = _editor.getClient();
        Hash author = _editor.getAuthor();
        Hash target = _editor.getTarget();
        Hash scope = getScope(author, target);
        
        NestedUI ui = new NestedUI(_ui);
        
        long msgId = MessageGen.createMessageId(client);
        
        String out = null;
        if (out == null) {
            File chanDir = new File(client.getOutboundDir(), scope.toBase64());
            chanDir.mkdirs();
            File msgFile = new File(chanDir, msgId + Constants.FILENAME_SUFFIX);
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
        
        for (int i = 0; i < _editor.getPageCount(); i++) {
            String content = _editor.getPageContent(i);
            String contentType = _editor.getPageType(i);
            
            FileOutputStream fos = null;
            try {
                File pageFile = File.createTempFile("pageData", ""+i, tmpDir);
                fos = new FileOutputStream(pageFile);
                fos.write(DataHelper.getUTF8(content));
                fos.close();
                fos = null;
                tempFiles.add(pageFile);
                
                String filename = pageFile.getAbsolutePath();
                
                File cfgFile = File.createTempFile("pageConfig", ""+ i, tmpDir);
                fos = new FileOutputStream(cfgFile);
                fos.write(DataHelper.getUTF8(Constants.MSG_PAGE_CONTENT_TYPE + '=' + contentType + '\n'));
                fos.close();
                fos = null;
                tempFiles.add(cfgFile);
                genOpts.setOptValue("page" + i, filename);
                genOpts.setOptValue("page" + i + "-config", cfgFile.getPath());
            } catch (IOException ioe) {
                _errorBuf.append("Error writing out the configuration for page " + i + ": " + ioe.getMessage());
                cleanup(tempFiles, refFile);
                return false;
            }
        }

        List names = _editor.getAttachmentNames();
        List types = _editor.getAttachmentTypes();
        for (int i = 0; i < names.size(); i++) {
            String fname = (String)names.get(i);
            String desc = null; // the ui doesn't have a way to specify the attachment description
            String type = (String)types.get(i);
            byte data[] = _editor.getAttachmentData(i+1);
            
            FileOutputStream fos = null;
            try {
                File attachFile = File.createTempFile("attachData", ""+i, tmpDir);
                fos = new FileOutputStream(attachFile);
                fos.write(data);
                fos.close();
                fos = null;
                data = null;
                tempFiles.add(attachFile);

                String filename = attachFile.getAbsolutePath();
            
                File cfgFile = File.createTempFile("attachConfig", ""+ i, tmpDir);
                fos = new FileOutputStream(cfgFile);
                if (fname != null)
                    fos.write(DataHelper.getUTF8(Constants.MSG_ATTACH_NAME + '=' + CommandImpl.strip(fname.trim()) + '\n'));
                if (desc != null)
                    fos.write(DataHelper.getUTF8(Constants.MSG_ATTACH_DESCRIPTION + '=' + CommandImpl.strip(desc.trim()) + '\n'));
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
                return false;
            }
        }

        Hash authenticationKey = null;
        Hash authorizationKey = null;
        
        List targetKeys = client.getNymKeys(target, null);
        for (int i = 0; i < targetKeys.size(); i++) {
            NymKey key = (NymKey)targetKeys.get(i);
            if (Constants.KEY_FUNCTION_MANAGE.equals(key.getFunction()) ||
                Constants.KEY_FUNCTION_POST.equals(key.getFunction())) {
                authorizationKey = client.sha256(key.getData());
                break;
            }
        }
        List authorKeys = client.getNymKeys(author, Constants.KEY_FUNCTION_MANAGE);
        for (int i = 0; i < authorKeys.size(); i++) {
            NymKey key = (NymKey)authorKeys.get(i);
            if (key.isIdentity()) {
                authenticationKey = client.sha256(key.getData());
                break;
            }
        }
        
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
                    for (int i = 0; i < _editor.getParentCount(); i++) {
                        SyndieURI parent = _editor.getParent(i);
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
        
        genOpts.setOptValue("messageId", Long.toString(msgId));
        genOpts.setOptValue("subject", _editor.getSubject().trim());
 
        String passphrase = (_editor.getPrivacyPBE() ? _editor.getPassphrase() : null);
        String passphrasePrompt = _editor.getPassphrasePrompt();
        if ( (_editor.getPrivacyPBE()) && (passphrase != null) && (passphrasePrompt != null) ) {
            genOpts.setOptValue("bodyPassphrase", CommandImpl.strip(passphrase));
            genOpts.setOptValue("bodyPassphrasePrompt", CommandImpl.strip(passphrasePrompt));
        } else if (_editor.getPrivacyPublic ()) {
            genOpts.setOptValue("encryptContent", "false"); // if true, encrypt the content with a known read key for the channel
        }
        
        String avatarFilename = _editor.getAvatarUnmodifiedFilename();
        if (avatarFilename == null) {
            byte avatar[] = _editor.getAvatarModifiedData();
            if (avatar != null) {
                try {
                    File avatarFile = File.createTempFile("avatar", ".png", tmpDir);
                    FileOutputStream fos = new FileOutputStream(avatarFile);
                    fos.write(avatar);
                    fos.close();
                    fos = null;
                    tempFiles.add(avatarFile);
                    avatarFilename = avatarFile.getAbsolutePath();
                } catch (IOException ioe) {
                    _errorBuf.append("Error writing out the avatar: " + ioe.getMessage());
                    cleanup(tempFiles, refFile);
                    return false;
                }
            }
        }
        if (avatarFilename != null)
            genOpts.setOptValue("avatar", avatarFilename);
        
        if (_editor.getPrivacyReply())
            genOpts.setOptValue("postAsReply", "true"); // if true, the post should be encrypted to the channel's reply key
        
        String tags[] = _editor.getPublicTags();
        for (int i = 0; i < tags.length; i++) {
            if (tags[i] != null) {
                String str = tags[i].trim();
                if (str.length() > 0)
                    genOpts.addOptValue("pubTag", str);
            }
        }
        tags = _editor.getPrivateTags();
        for (int i = 0; i < tags.length; i++) {
            if (tags[i] != null) {
                String str = tags[i].trim();
                if (str.length() > 0)
                    genOpts.addOptValue("privTag", str);
            }
        }
        
        List referenceNodes = _editor.getReferenceNodes();
        if (referenceNodes.size() > 0) {
            String refs = ReferenceNode.walk(referenceNodes);
            FileOutputStream fos = null;
            try {
                refFile = File.createTempFile("refs", "txt", tmpDir);
                fos = new FileOutputStream(refFile);
                fos.write(DataHelper.getUTF8(refs));
                fos.close();
                tempFiles.add(refFile);
                genOpts.setOptValue("refs", refFile.getPath());
                ui.debugMessage("Pulling refs from " + refFile.getPath());
            } catch (IOException ioe) {
                _errorBuf.append("Error writing out the references: " + ioe.getMessage());
                cleanup(tempFiles, refFile);
                return false;
            }
        }
        //* (--cancel $uri)*                // posts to be marked as cancelled (only honored if authorized to do so for those posts)
        
        // replace the $uri with the current post, if authorized to do so
        //if ( (_currentMessage.getOverwriteChannel() != null) && (_currentMessage.getOverwriteMessage() >= 0) )
        //    genOpts.setOptValue("overwrite", SyndieURI.createMessage(_currentMessage.getOverwriteChannel(), _currentMessage.getOverwriteMessage()).toString());

        StringBuffer parentBuf = new StringBuffer();
        // todo: rework this to include parents of parents (of parents of parents, etc), up to a certain
        // depth (so the structure can remain intact even with messages missing from the middle)
        for (int i = 0; i < _editor.getParentCount(); i++) {
            SyndieURI uri = _editor.getParent(i);
            parentBuf.append(uri.toString());
            if (i + 1 < _editor.getParentCount())
                parentBuf.append(",");
        }
        if (parentBuf.length() > 0)
            genOpts.setOptValue("references", parentBuf.toString());

        String expiration = _editor.getExpiration();
        if (expiration != null)
            genOpts.setOptValue("expiration", expiration);
        
        genOpts.setOptValue("forceNewThread", ""+_editor.getForceNewThread());
        genOpts.setOptValue("refuseReplies", ""+_editor.getRefuseReplies());
        
        genOpts.setOptValue("out", out);
        
        NestedUI nestedUI = new NestedUI(ui);
        ui.debugMessage("generating with opts: " + genOpts);
        cmd.runCommand(genOpts, nestedUI, client);
        if (nestedUI.getExitCode() >= 0) {
            // generated fine, so lets import 'er
            ui.statusMessage("Message generated and written to " + out);
            
            Importer msgImp = new Importer();
            Opts msgImpOpts = new Opts();
            msgImpOpts.setOptValue("in", out);
            if (passphrase != null)
                msgImpOpts.setOptValue("passphrase", CommandImpl.strip(passphrase));
            msgImpOpts.setCommand("import");
            NestedUI dataNestedUI = new NestedUI(ui);
            msgImp.runCommand(msgImpOpts, dataNestedUI, client);
            if (dataNestedUI.getExitCode() < 0) {
                _errorBuf.append("Failed in the nested import command");
                cleanup(tempFiles, refFile);
                return false;
            } else {
                _createdURI = SyndieURI.createMessage(scope, msgId);
                ui.statusMessage("Post imported");
                ui.commandComplete(0, null);
                cleanup(tempFiles, refFile);
                return true;
            }
        } else {
            _errorBuf.append("Error generating the message");
            cleanup(tempFiles, refFile);
            return false;
        }
    }
    
    private void cleanup(List tempFiles, File refFile) {
        for (int i = 0; i < tempFiles.size(); i++)
            ((File)tempFiles.get(i)).delete();
        if (refFile != null)
            refFile.delete();
    }
    
    public interface MessageCreatorSource {
        public DBClient getClient();
        public UI getUI();
        public Hash getAuthor();
        public Hash getTarget();
        public int getPageCount();
        public String getPageContent(int page);
        public String getPageType(int page);
        public List getAttachmentNames();
        public List getAttachmentTypes();
        /** @param attachmentIndex starts at 1 */
        public byte[] getAttachmentData(int attachmentIndex);
        public String getSubject();
        public boolean getPrivacyPBE();
        public String getPassphrase();
        public String getPassphrasePrompt();
        public boolean getPrivacyPublic();
        public String getAvatarUnmodifiedFilename();
        public byte[] getAvatarModifiedData();
        public boolean getPrivacyReply();
        public String[] getPublicTags();
        public String[] getPrivateTags();
        public List getReferenceNodes();
        public int getParentCount();
        public SyndieURI getParent(int depth);
        public String getExpiration();
        public boolean getForceNewThread();
        public boolean getRefuseReplies();
    }   
}
