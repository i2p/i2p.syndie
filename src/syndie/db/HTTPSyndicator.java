/*
 * HTTPSyndicator.java
 *
 * Created on September 19, 2006, 12:41 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package syndie.db;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.i2p.data.Base64;
import net.i2p.data.Hash;
import net.i2p.util.EepGet;
import net.i2p.util.EepGetScheduler;
import net.i2p.util.EepPost;
import syndie.Constants;
import syndie.data.*;

/**
 * request those files from the archive, saving them to client.getTempDir()
 * iterate across those files, attempting to import each one
 * if it fails due to PBE, add it to the pbefail list
 * if it fails for other reasons, add it to the unimported list (and delete the file?)
 * if it succeeds, delete the file
 * display the summary of the import process
 */
public class HTTPSyndicator {
    private String _archiveURL;
    private String _proxyHost;
    private int _proxyPort;
    private List _syndieURIs;
    private DBClient _client;
    private UI _ui;
    
    private List _fetchedFiles;
    private List _fetchedURIs;
    private List _pendingPBEFiles;
    private List _pendingPBEURIs;
    private List _pendingPBEPrompts;
    
    private List _postURIs;
    private String _postURLOverride;
    private String _postPassphrase;
    private boolean _postShouldDeleteOutbound;
    private ArchiveIndex _remoteIndex;
    private List _postToDelete;
    
    public HTTPSyndicator(String archiveURL, String proxyHost, int proxyPort, DBClient client, UI ui, ArchiveIndex index) {
        _archiveURL = archiveURL;
        _proxyHost = proxyHost;
        _proxyPort = proxyPort;
        _client = client;
        _ui = ui;
        _remoteIndex = index;
        
        _fetchedFiles = new ArrayList();
        _fetchedURIs = new ArrayList();
        _pendingPBEFiles = new ArrayList();
        _pendingPBEURIs = new ArrayList();
        _pendingPBEPrompts = new ArrayList();
        _postToDelete = new ArrayList();
        
        _postURIs = new ArrayList();
        _postShouldDeleteOutbound = false;
        _postURLOverride = null;
        _postPassphrase = null;
    }

    /**
     * fetch the posts/replies/metadata from the archive, saving them to disk
     * but not attempting to import them yet
     */
    public boolean fetch(List syndieURIs) {
        _syndieURIs = syndieURIs;
        if (_archiveURL.startsWith("https")) {
            fetchSSL();
        } else if (_archiveURL.startsWith("http")) {
            fetchHTTP();
        } else {
            fetchFiles();
        }
        return true;
    }
    
    private void fetchSSL() {
        // URL fetch
        _ui.errorMessage("SSL not yet supported");
    }
    private void fetchHTTP() {
        // eepget-driven, one at a time via EepGetScheduler
        if (!_archiveURL.endsWith("/"))
            _archiveURL = _archiveURL + "/";
        List urls = new ArrayList();
        List files = new ArrayList();
        Map httpURLToSyndieURI = new HashMap();
        
        File tmpDir = _client.getTempDir();
        int msgDirIndex = 0;
        File msgDir = new File(tmpDir, "httpsync"+msgDirIndex);
        while (msgDir.exists()) {
            msgDirIndex++;
            msgDir = new File(tmpDir, "httpsync"+msgDirIndex);
        }
        msgDir.mkdirs();
        
        for (int i = 0; i < _syndieURIs.size(); i++) {
            SyndieURI uri = (SyndieURI)_syndieURIs.get(i);
            String url = null;
            if (uri.getMessageId() == null)
                url = _archiveURL + uri.getScope().toBase64() + "/meta" + Constants.FILENAME_SUFFIX;
            else
                url = _archiveURL + uri.getScope().toBase64() + "/" + uri.getMessageId().longValue() + Constants.FILENAME_SUFFIX;
            
            File tmpFile = new File(msgDir, i + Constants.FILENAME_SUFFIX);
            httpURLToSyndieURI.put(url, uri);
            urls.add(url);
            files.add(tmpFile);
        }
        
        HTTPStatusListener lsnr = new HTTPStatusListener(httpURLToSyndieURI);
        EepGetScheduler sched = new EepGetScheduler(_client.ctx(), urls, files, _proxyHost, _proxyPort, lsnr);
        sched.fetch(true); // blocks until complete
        _ui.statusMessage("Fetch of selected URIs complete");
        //while (lsnr.transfersPending()) {
        //    try { Thread.sleep(1000); } catch (InterruptedException ie) {}
        //}
    }
    
    private class HTTPStatusListener implements EepGet.StatusListener {
        private Map _httpURLToSyndieURI;
        public HTTPStatusListener(Map httpURLToSyndieURI) {
            _httpURLToSyndieURI = httpURLToSyndieURI;
        }
        public void bytesTransferred(long alreadyTransferred, int currentWrite, long bytesTransferred, long bytesRemaining, String url) {
            _ui.debugMessage("Transferred: " + bytesTransferred);
        }
        public void transferComplete(long alreadyTransferred, long bytesTransferred, long bytesRemaining, String url, String outputFile, boolean notModified) {
            _ui.debugMessage("Transfer complete: " + bytesTransferred + " for " + url);
            _fetchedFiles.add(new File(outputFile));
            _fetchedURIs.add(_httpURLToSyndieURI.remove(url));
        }
        public void attemptFailed(String url, long bytesTransferred, long bytesRemaining, int currentAttempt, int numRetries, Exception cause) {
            _ui.debugMessage("Transfer attempt failed: " + bytesTransferred + " from " + url, cause);
        }
        public void transferFailed(String url, long bytesTransferred, long bytesRemaining, int currentAttempt)  {
            _ui.statusMessage("Transfer totally failed of " + url);
            _httpURLToSyndieURI.remove(url);
        }
        public void headerReceived(String url, int currentAttempt, String key, String val)  {
            _ui.debugMessage("Header received: " + key + "=" + val);
        }
        public void attempting(String url) {
            _ui.statusMessage("Fetching " + url + "...");
        }
        public boolean transfersPending() { return _httpURLToSyndieURI.size() > 0; }
    }
    
    private void fetchFiles() {
        File tmpDir = _client.getTempDir();
        int msgDirIndex = 0;
        File msgDir = new File(tmpDir, "httpsync"+msgDirIndex);
        while (msgDir.exists()) {
            msgDirIndex++;
            msgDir = new File(tmpDir, "httpsync"+msgDirIndex);
        }
        msgDir.mkdirs();
        int curFile = 0;
        File archiveDir = new File(_archiveURL);
        _ui.debugMessage("Fetching " + _syndieURIs);
        for (int i = 0; i < _syndieURIs.size(); i++) {
            SyndieURI uri = (SyndieURI)_syndieURIs.get(i);
            Hash scope = uri.getScope();
            if (scope == null) {
                _ui.errorMessage("Invalid fetch URI - has no scope: " + uri);
                continue;
            }
            
            File srcDir = new File(archiveDir, scope.toBase64());
            File srcFile = null;
            Long msgId = uri.getMessageId();
            if (msgId == null)
                srcFile = new File(srcDir, "meta" + Constants.FILENAME_SUFFIX);
            else
                srcFile = new File(srcDir, msgId.longValue() + Constants.FILENAME_SUFFIX);
            if (srcFile.exists()) {
                _ui.debugMessage("Fetching file from " + srcFile.getPath() + ": " + uri);
                File dest = new File(msgDir, curFile + Constants.FILENAME_SUFFIX);
                boolean ok = copy(srcFile, dest);
                if (!ok) {
                    dest.delete();
                    _ui.debugMessage(uri + " could not be fetched from " + srcFile.getPath());
                    return;
                } else {
                    _fetchedFiles.add(dest);
                    _fetchedURIs.add(uri);
                    _ui.debugMessage("URI fetched: " + uri);
                }
                curFile++;
            } else {
                _ui.errorMessage("Referenced URI is not in the archive: " + uri + " as " + srcFile.getPath());
            }
        }
    }
    
    private boolean copy(File src, File dest) {
        FileInputStream fis = null;
        FileOutputStream fos = null;
        try {
            fis = new FileInputStream(src);
            fos = new FileOutputStream(dest);
            byte buf[] = new byte[4096];
            int read = 0;
            while ( (read = fis.read(buf)) != -1)
                fos.write(buf, 0, read);
            fis.close();
            fos.close();
            fis = null;
            fos = null;
            return true;
        } catch (IOException ioe) {
            _ui.errorMessage("Error copying the file " + src.getPath() + " to " + dest.getPath());
            return false;
        } finally {
            if (fis != null) try { fis.close(); } catch (IOException ioe) {}
            if (fos != null) try { fos.close(); } catch (IOException ioe) {}
        }
    }
    
    public int importFetched() {
        int imported = 0;
        _ui.debugMessage("Attempting to import " + _fetchedFiles.size() + " messages");
        for (int i = 0; i < _fetchedFiles.size(); i++) {
            Importer imp = new Importer(_client, _client.getPass());
            File f = (File)_fetchedFiles.get(i);
            SyndieURI uri = (SyndieURI)_fetchedURIs.get(i);
            _ui.debugMessage("Importing " + uri + " from " + f.getPath());
            boolean ok;
            try {
                NestedUI nested = new NestedUI(_ui);
                ok = imp.processMessage(nested, new FileInputStream(f), _client.getLoggedInNymId(), _client.getPass(), null);
                if (ok && (nested.getExitCode() >= 0)) {
                    _ui.debugMessage("Import successful for " + uri);
                    f.delete();
                    imported++;
                } else {
                    _ui.debugMessage("Could not import " + f.getPath());
                    importFailed(uri, f);
                }
            } catch (IOException ioe) {
                _ui.errorMessage("Error importing the message for " + uri, ioe);
            }
        }
        return imported;
    }
    private void importFailed(SyndieURI uri, File localCopy) throws IOException {
        Enclosure enc = new Enclosure(new FileInputStream(localCopy));
        String prompt = enc.getHeaderString(Constants.MSG_HEADER_PBE_PROMPT);
        if (prompt != null) {
            _pendingPBEFiles.add(localCopy);
            _pendingPBEURIs.add(uri);
            _pendingPBEPrompts.add(prompt);
        } else {
            // record why the import failed in the db (missing readKey, missing replyKey, corrupt, unauthorized, etc)
        }
    }
    public int countMissingPassphrases() { return _pendingPBEPrompts.size(); }
    public String getMissingPrompt(int index) { return (String)_pendingPBEPrompts.get(index); }
    public SyndieURI getMissingURI(int index) { return (SyndieURI)_pendingPBEURIs.get(index); }
    public void importPBE(int index, String passphrase) {
        Importer imp = new Importer(_client, _client.getPass());
        File f = (File)_pendingPBEFiles.get(index);
        SyndieURI uri = (SyndieURI)_pendingPBEURIs.get(index);
        boolean ok;
        try {
            NestedUI nested = new NestedUI(_ui);
            ok = imp.processMessage(nested, new FileInputStream(f), _client.getLoggedInNymId(), _client.getPass(), passphrase);
            if (ok && (nested.getExitCode() >= 0) && (nested.getExitCode() != 1) ) {
                f.delete();
                _pendingPBEFiles.remove(index);
                _pendingPBEPrompts.remove(index);
                _pendingPBEURIs.remove(index);
                _ui.statusMessage("Passphrase correct.  Message imported: " + uri);
                _ui.commandComplete(0, null);
            } else {
                _ui.errorMessage("Passphrase incorrect");
                _ui.commandComplete(-1, null);
            }
        } catch (IOException ioe) {
            _ui.errorMessage("Error importing the message with a passphrase for " + uri, ioe);
            _ui.commandComplete(-1, null);
        }
    }
    
    public void post() {
        if (_postURIs.size() <= 0) {
            _ui.statusMessage("No messages to post");
            _ui.commandComplete(0, null);
        } else if (_archiveURL.startsWith("https")) {
            postSSL();
        } else if (_archiveURL.startsWith("http")) {
            postHTTP();
        } else {
            _ui.errorMessage("Only know how to post to HTTP or HTTPS");
            _ui.commandComplete(-1, null);
        }
    }
    public void setPostURLOverride(String url) { _postURLOverride = url; }
    public void setDeleteOutboundAfterSend(boolean shouldDelete) { _postShouldDeleteOutbound = shouldDelete; }
    public void setPostPassphrase(String passphrase) { _postPassphrase = passphrase; }

    private void postSSL() {
        _ui.errorMessage("Only know how to post to HTTP");
        _ui.commandComplete(-1, null);
    }
    
    private void postHTTP() {
        Map fields = new HashMap();
        int numMeta = 0;
        int numPosts = 0;
        for (int i = 0; i < _postURIs.size(); i++) {
            SyndieURI uri = (SyndieURI)_postURIs.get(i);
            File chanDir = new File(_client.getArchiveDir(), uri.getScope().toBase64());
            File f = null;
            String name = null;
            if (uri.getMessageId() == null) {
                name = "meta" + numMeta;
                f = new File(chanDir, "meta" + Constants.FILENAME_SUFFIX);
                numMeta++;
            } else {
                name = "post" + numPosts;
                f = new File(chanDir, uri.getMessageId().longValue() + Constants.FILENAME_SUFFIX);
                numPosts++;
            }
            fields.put(name, f);
            _ui.debugMessage("Posting " + f.getPath() + " as " + name);
        }
        _ui.statusMessage("Posting " + numMeta + " metadata messages and " + numPosts + " posts");

        if (_postPassphrase != null)
            fields.put("pass", _postPassphrase);
        
        EepPost post = new EepPost(_client.ctx());
        String url = null;
        if (_postURLOverride == null) {
            if (_archiveURL.endsWith("/"))
                url = _archiveURL + "import.cgi";
            else
                url = _archiveURL + "/import.cgi";
        } else {
            url = _postURLOverride;
        }
        
        Blocker onCompletion = new Blocker();
        _ui.debugMessage("About to post messages to " + url);
        post.postFiles(url, _proxyHost, _proxyPort, fields, onCompletion);
        while (onCompletion.notYetComplete()) {
            _ui.debugMessage("Post in progress...");
            try {
                synchronized (onCompletion) {
                    onCompletion.wait(1000);
                }
            } catch (InterruptedException ie) {}
        }
        _ui.statusMessage("Files posted");
        if (_postShouldDeleteOutbound) {
            for (int i = 0; i < _postToDelete.size(); i++) {
                File f = (File)_postToDelete.get(i);
                _ui.statusMessage("Removing " + f.getPath() + " from the outbound queue");
                f.delete();
                File parent = f.getParentFile();
                String siblings[] = parent.list();
                if ( (siblings == null) || (siblings.length == 0) ) {
                    parent.delete();
                    _ui.debugMessage("Removing empty queue dir " + parent.getPath());
                }
            }
        }
        _ui.commandComplete(0, null);
    }
    private class Blocker implements Runnable {
        private boolean _complete;
        public Blocker() { _complete = false; }
        public void run() {
            _complete = true;
            synchronized (Blocker.this) {
                Blocker.this.notifyAll();
            }
        }
        public boolean notYetComplete() { return !_complete; }
    }
    
    /**
     * Schedule a number of URIs to be sent to the remote archive.  The 
     * style has four valid values:
     *   outbound: send all posts and metadata in the local outbound queue
     *   outboundmeta: send all of the metadata in the local outbound queue
     *   archive: send all posts and metadata in the local archive or outbound queue
     *   archivemeta: send all of the metadata in the local archive or outbound queue
     *
     * @param knownChanOnly if true, only send posts or metadata where the remote archive knows about the channel
     */
    public void schedulePut(String style, boolean knownChanOnly) {
        _ui.debugMessage("Scheduling put of " + style);
        if ("outbound".equalsIgnoreCase(style)) {
            scheduleOutbound(knownChanOnly);
        } else if ("outboundmeta".equalsIgnoreCase(style)) {
            scheduleOutboundMeta(knownChanOnly);
        } else if ("archive".equalsIgnoreCase(style)) {
            scheduleArchive(knownChanOnly);
        } else if ("archivemeta".equalsIgnoreCase(style)) {
            scheduleArchiveMeta(knownChanOnly);
        } else {
            _ui.errorMessage("Schedule style is unsupported.  Valid values are 'outbound', 'outboundmeta', 'archive', and 'archivemeta'");
            _ui.commandComplete(-1, null);
        }
    }
    
    private void scheduleOutbound(boolean knownChanOnly) { schedule(_client.getOutboundDir(), false, true, knownChanOnly); }
    private void schedule(File rootDir, boolean metaOnly, boolean isOutbound, boolean knownChanOnly) {
        int numMeta = 0;
        int numPost = 0;
        long numBytes = 0;
        File chanDirs[] = rootDir.listFiles();
        _ui.debugMessage("Number of potential channel dirs: " + chanDirs.length + " in " + rootDir.getPath());
        for (int i = 0; i < chanDirs.length; i++) {
            if (!chanDirs[i].isDirectory())
                continue;
            File chanMessages[] = chanDirs[i].listFiles();
            byte chanHash[] = Base64.decode(chanDirs[i].getName());
            if ( (chanHash == null) || (chanHash.length != Hash.HASH_LENGTH) ) {
                _ui.debugMessage("Not scheduling the channel dir " + chanDirs[i].getName());
                continue;
            }
            Hash chan = new Hash(chanHash);
            ArchiveChannel remote = _remoteIndex.getChannel(SyndieURI.createScope(chan));
            if (knownChanOnly && (remote == null)) {
                _ui.debugMessage("Not scheduling the channel, since it isn't known remotely and we only send known");
                continue;
            }
            for (int j = 0; j < chanMessages.length; j++) {
                String name = chanMessages[j].getName();
                boolean isMeta = false;
                SyndieURI uri = null;
                if (("meta" + Constants.FILENAME_SUFFIX).equalsIgnoreCase(name)) {
                    isMeta = true;
                    uri = SyndieURI.createScope(chan);
                } else if (name.endsWith(Constants.FILENAME_SUFFIX) && (name.length() > Constants.FILENAME_SUFFIX.length())) {
                    if (!metaOnly) {
                        try {
                            String msgIdStr = name.substring(0, name.length()-Constants.FILENAME_SUFFIX.length());
                            Long msgId = Long.valueOf(msgIdStr);
                            uri = SyndieURI.createMessage(chan, msgId.longValue());
                        } catch (NumberFormatException nfe) {
                            // skip
                        }
                    } else {
                        _ui.debugMessage("Not scheduling the post, since we are only sending metadata");
                    }
                }
                boolean scheduled = false;
                if (uri != null) {
                    if (uri.getMessageId() != null) {
                        if (null == _remoteIndex.getMessage(uri)) {
                            if (!_postURIs.contains(uri)) {
                                _postURIs.add(uri);
                                if (isMeta)
                                    numMeta++;
                                else
                                    numPost++;
                                numBytes += chanMessages[j].length();
                            }
                            scheduled = true;
                        } else {
                            _ui.debugMessage("Not scheduling the post, since the remote site has it");
                        }
                    } else {
                        // check the version in the db
                        if (remote == null) {
                            if (!_postURIs.contains(uri)) {
                                _postURIs.add(uri);
                                if (isMeta)
                                    numMeta++;
                                else
                                    numPost++;
                                numBytes += chanMessages[j].length();
                            }
                            scheduled = true;
                        } else {
                            long chanId = _client.getChannelId(uri.getScope());
                            ChannelInfo info = _client.getChannel(chanId);
                            if (info.getEdition() > remote.getVersion()) {
                                if (!_postURIs.contains(uri)) {
                                    _postURIs.add(uri);
                                    if (isMeta)
                                        numMeta++;
                                    else
                                        numPost++;
                                    numBytes += chanMessages[j].length();
                                }
                            } else {
                                _ui.debugMessage("Not scheduling the metadata, since the remote site has that or a newer version");
                            }
                        }
                    }
                }
                if (scheduled && isOutbound && _postShouldDeleteOutbound) {
                    _ui.debugMessage("Scheduling " + chanMessages[j].getName() + " for deletion after post");
                    _postToDelete.add(chanMessages[j]);
                } else {
                    _ui.debugMessage("Not scheduling " + chanMessages[j].getName() + " for deletion after post (sched? " + scheduled + " out? " + isOutbound + " del? " + _postShouldDeleteOutbound + ")");
                }
            }
        }
        _ui.debugMessage("Scheduling post of " + _postURIs);
        _ui.statusMessage("Scheduled upload of " + numPost + " posts and " + numMeta + " channel metadata messages");
        _ui.statusMessage("Total size to be uploaded: " + ((numBytes+1023)/1024) + " kilobytes");
    }
    private void scheduleOutboundMeta(boolean knownChanOnly) { schedule(_client.getOutboundDir(), true, true, knownChanOnly); }
    private void scheduleArchive(boolean knownChanOnly) { schedule(_client.getArchiveDir(), false, false, knownChanOnly); }
    private void scheduleArchiveMeta(boolean knownChanOnly) { schedule(_client.getArchiveDir(), true, false, knownChanOnly); }
}
