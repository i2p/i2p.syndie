package syndie.gui;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.Set;
import net.i2p.data.Hash;
import net.i2p.data.SigningPublicKey;
import syndie.data.ArchiveInfo;
import syndie.data.Enclosure;
import syndie.data.SyndieURI;
import syndie.db.ChanGen;
import syndie.db.CommandImpl;
import syndie.db.DBClient;
import syndie.db.Importer;
import syndie.db.KeyImport;
import syndie.db.NestedUI;
import syndie.db.Opts;
import syndie.db.UI;
import syndie.Constants;

/**
 * actually perform the forum management (or at least gather the 
 * data from the ManageForum state and plug it into a ChanGen command,
 * as is done in the syndie.db.ManageMenu)
 */
class ManageForumExecutor {
    private DBClient _client;
    private UI _ui;
    private ManageForum _state;
    private StringBuffer _errors;
    private SyndieURI _forum;
    
    public ManageForumExecutor(DBClient client, UI ui, ManageForum state) {
        _client = client;
        _ui = ui;
        _state = state;
        _errors = new StringBuffer();
    }
    
    public String getErrors() { return _errors.toString(); }
    public SyndieURI getForum() { return _forum; }
    
    /**
     * actually save the state to a new syndie metadata message, persisting it to
     * the local database.  the URI to the (newly?) built forum is found at getForum(),
     * or if there were errors, getErrors().  this is a port of ManageMenu.processExecute
     */
    public void execute() {
        String out = null;
        File tmpDir = _client.getTempDir();
        tmpDir.mkdirs();
        File manageOut = null;
        File replyOut = null;
        File encPostOut = null;
        File encMetaOut = null;
        try {
            manageOut = File.createTempFile("syndieManage", "dat", tmpDir);
            replyOut = File.createTempFile("syndieReply", "dat", tmpDir);
            encPostOut = File.createTempFile("syndieEncPost", "dat", tmpDir);
            encMetaOut = File.createTempFile("syndieEncMeta", "dat", tmpDir);
            if (out == null) {
                out = File.createTempFile("syndieMetaOut", Constants.FILENAME_SUFFIX, tmpDir).getPath();
            }
        } catch (IOException ioe) {
            _errors.append("Unable to create temporary files: " + ioe.getMessage());
            _ui.commandComplete(-1, null);
            return;
        }
        
        Opts chanGenOpts = new Opts();
        chanGenOpts.setCommand("changen");
        chanGenOpts.setOptValue("name", _state.getName());
        chanGenOpts.setOptValue("description", _state.getDescription());
        chanGenOpts.setOptValue("avatar", ""); //_avatar);
        chanGenOpts.setOptValue("edition", Long.toString(_client.createEdition(_state.getLastEdition())));
        chanGenOpts.setOptValue("publicPosting", (_state.getAllowPublicPosts() ? Boolean.TRUE.toString() : Boolean.FALSE.toString()));
        chanGenOpts.setOptValue("publicReplies", (_state.getAllowPublicReplies() ? Boolean.TRUE.toString() : Boolean.FALSE.toString()));
        Set tags = _state.getPublicTags();
        if (tags != null) {
            for (Iterator iter = tags.iterator(); iter.hasNext(); )
                chanGenOpts.addOptValue("pubTag", iter.next().toString());
        }
        
        Set keys = _state.getAuthorizedPosters();
        if (keys != null) {
            for (Iterator iter = keys.iterator(); iter.hasNext(); ) {
                SigningPublicKey pub = (SigningPublicKey)iter.next();
                chanGenOpts.addOptValue("postKey", pub.toBase64());
            }
        }
        
        keys = _state.getAuthorizedManagers();
        if (keys != null) {
            for (Iterator iter = keys.iterator(); iter.hasNext(); ) {
                SigningPublicKey pub = (SigningPublicKey)iter.next();
                chanGenOpts.addOptValue("manageKey", pub.toBase64());
            }
        }
        
        chanGenOpts.setOptValue("refs", _state.getReferences());
        
        Set archives = _state.getPublicArchives();
        if (archives != null) {
            for (Iterator iter = archives.iterator(); iter.hasNext(); ) {
                ArchiveInfo archive = (ArchiveInfo)iter.next();
                chanGenOpts.addOptValue("pubArchive", archive.getURI().toString());
            }
        }
        archives = _state.getPrivateArchives();
        if (archives != null) {
            for (Iterator iter = archives.iterator(); iter.hasNext(); ) {
                ArchiveInfo archive = (ArchiveInfo)iter.next();
                chanGenOpts.addOptValue("privArchive", archive.getURI().toString());
            }
        }
        
        if (_state.getEncryptContent())
            chanGenOpts.setOptValue("encryptContent", Boolean.TRUE.toString());
        
        if (_state.getChannelId() >= 0)
            chanGenOpts.setOptValue("channelId", Long.toString(_state.getChannelId()));
        
        chanGenOpts.setOptValue("metaOut", out);
        chanGenOpts.setOptValue("keyManageOut", manageOut.getPath());
        chanGenOpts.setOptValue("keyReplyOut", replyOut.getPath());
        chanGenOpts.setOptValue("keyEncryptPostOut", encPostOut.getPath());
        chanGenOpts.setOptValue("keyEncryptMetaOut", encMetaOut.getPath());
        
        if (_state.getPBE()) {
            chanGenOpts.setOptValue("bodyPassphrase", CommandImpl.strip(_state.getPassphrase()));
            chanGenOpts.setOptValue("bodyPassphrasePrompt", CommandImpl.strip(_state.getPassphrasePrompt()));
        }
        
        ChanGen cmd = new ChanGen();
        _ui.debugMessage("Generating with options " + chanGenOpts);
        NestedUI nestedUI = new NestedUI(_ui);
        cmd.runCommand(chanGenOpts, nestedUI, _client);
        
        Hash identHash = null;
        
        if (nestedUI.getExitCode() >= 0) {
            // ok, used the default dir - migrate it
            FileInputStream fis = null;
            FileOutputStream fos = null;
            try {
                fis = new FileInputStream(out);
                Enclosure enc = new Enclosure(fis);
                SigningPublicKey pub = enc.getHeaderSigningKey(Constants.MSG_META_HEADER_IDENTITY);
                if (pub == null) {
                    _errors.append("Unable to pull the channel from the enclosure");
                    _ui.commandComplete(-1, null);
                    return;
                } else {
                    identHash = pub.calculateHash();
                    _ui.debugMessage("Channel identity: " +identHash.toBase64());
                }
                File chanDir = new File(_client.getOutboundDir(), identHash.toBase64());
                chanDir.mkdirs();
                File mdFile = new File(chanDir, "meta" + Constants.FILENAME_SUFFIX);
                fos = new FileOutputStream(mdFile);
                fis = new FileInputStream(out);
                byte buf[] = new byte[4096];
                int read = -1;
                while ( (read = fis.read(buf)) != -1)
                    fos.write(buf, 0, read);
                fis.close();
                fos.close();
                fis = null;
                fos = null;
                File outFile = new File(out);
                outFile.delete();
                out = mdFile.getPath();
                _ui.statusMessage("Sharable channel metadata saved to " + mdFile.getPath());
            } catch (IOException ioe) {
                _errors.append("Error migrating the channel metadata from " + out + ": " + ioe.getMessage());
                return;
            } finally {
                if (fis != null) try { fis.close(); } catch (IOException ioe) {}
                if (fos != null) try { fos.close(); } catch (IOException ioe) {}
            }
        }
        
        File outFile = new File(out);
        if ( (nestedUI.getExitCode() >= 0) && (outFile.exists() && outFile.length() > 0) ) {
            // channel created successfully, now import the metadata and keys, and delete
            // the temporary files
            _ui.statusMessage("Channel metadata created and stored in " + outFile.getPath());
            
            Importer msgImp = new Importer();
            Opts msgImpOpts = new Opts();
            msgImpOpts.setOptValue("in", out);
            if (_state.getPBE() && (_state.getPassphrase() != null))
                msgImpOpts.setOptValue("passphrase", CommandImpl.strip(_state.getPassphrase()));
            msgImpOpts.setCommand("import");
            NestedUI dataNestedUI = new NestedUI(_ui);
            _ui.debugMessage("Importing with options " + msgImpOpts);
            msgImp.runCommand(msgImpOpts, dataNestedUI, _client);
            if (dataNestedUI.getExitCode() < 0) {
                _errors.append("Failed in the nested import command");
                _ui.commandComplete(dataNestedUI.getExitCode(), null);
                return;
            }
            _ui.statusMessage("Channel metadata imported");

            KeyImport keyImp = new KeyImport();
            Opts keyOpts = new Opts();
            if (manageOut.length() > 0) {
                keyOpts.setOptValue("keyfile", manageOut.getPath());
                keyOpts.setOptValue("authentic", "true");
                dataNestedUI = new NestedUI(_ui);
                keyImp.runCommand(keyOpts, dataNestedUI, _client);
                if (dataNestedUI.getExitCode() < 0) {
                    _errors.append("Failed in the nested key import command");
                    _ui.commandComplete(dataNestedUI.getExitCode(), null);
                    return;
                }
                _ui.statusMessage("Channel management key imported");
            }
            if (replyOut.length() > 0) {
                keyOpts = new Opts();
                keyOpts.setOptValue("keyfile", replyOut.getPath());
                keyOpts.setOptValue("authentic", "true");
                dataNestedUI = new NestedUI(_ui);
                keyImp.runCommand(keyOpts, dataNestedUI, _client);
                if (dataNestedUI.getExitCode() < 0) {
                    _errors.append("Failed in the nested key import command");
                    _ui.commandComplete(dataNestedUI.getExitCode(), null);
                    return;
                }
                _ui.statusMessage("Channel reply key imported");
            }
            if (encPostOut.length() > 0) {
                keyOpts = new Opts();
                keyOpts.setOptValue("keyfile", encPostOut.getPath());
                keyOpts.setOptValue("authentic", "true");
                dataNestedUI = new NestedUI(_ui);
                keyImp.runCommand(keyOpts, dataNestedUI, _client);
                if (dataNestedUI.getExitCode() < 0) {
                    _errors.append("Failed in the nested key import command");
                    _ui.commandComplete(dataNestedUI.getExitCode(), null);
                    return;
                }
                _ui.statusMessage("Channel post read key imported");
            }
            if (encMetaOut.length() > 0) {
                keyOpts = new Opts();
                keyOpts.setOptValue("keyfile", encMetaOut.getPath());
                keyOpts.setOptValue("authentic", "true");
                dataNestedUI = new NestedUI(_ui);
                keyImp.runCommand(keyOpts, dataNestedUI, _client);
                if (dataNestedUI.getExitCode() < 0) {
                    _errors.append("Failed in the nested key import command");
                    _ui.commandComplete(dataNestedUI.getExitCode(), null);
                    return;
                }
                _ui.statusMessage("Channel metadata read key imported");
            }
            
            manageOut.delete();
            replyOut.delete();
            encPostOut.delete();
            encMetaOut.delete();
        }
        _forum = SyndieURI.createScope(identHash);
        _ui.commandComplete(nestedUI.getExitCode(), null);
    }
}
