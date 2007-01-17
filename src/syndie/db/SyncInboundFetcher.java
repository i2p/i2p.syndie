package syndie.db;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import net.i2p.I2PAppContext;
import net.i2p.util.EepGet;
import syndie.Constants;
import syndie.data.SyndieURI;

class SyncInboundFetcher {
    private SyncManager _manager;
    private static Map _runnerToArchive = new HashMap();
    
    public SyncInboundFetcher(SyncManager mgr) {
        _manager = mgr;
    }
    
    public void start() {
        for (int i = 0; i < 3; i++) {
            Thread t = new Thread(new Runner(), "InboundFetcher" + i);
            t.setDaemon(true);
            t.start();
        }
    }

    public void wakeUp() { synchronized (this) { notifyAll(); } }
        
    private class Runner implements Runnable {
        public void run() {
            while (true) {
                while (!_manager.isOnline()) {
                    try {
                        synchronized (SyncInboundFetcher.this) {
                            SyncInboundFetcher.this.wait(60*1000);
                        } 
                    } catch (InterruptedException ie) {}
                }
                
                SyncArchive archive = getNextToFetch(Runner.this);
                if (archive != null) {
                    try {
                        fetch(Runner.this, archive);
                    } catch (Exception e) {
                        synchronized (_runnerToArchive) { _runnerToArchive.remove(Runner.this); }
                        archive.indexFetchFail("Internal error fetching", e, true);
                    }
                } else {
                    try {
                        synchronized (SyncInboundFetcher.this) {
                            SyncInboundFetcher.this.wait(60*1000);
                        } 
                    } catch (InterruptedException ie) {}
                }
            }
        }
    }
    
    private SyncArchive getNextToFetch(Runner runner) {
        int count = _manager.getArchiveCount();
        long now = System.currentTimeMillis();
        for (int i = 0; i < count; i++) {
            SyncArchive archive = _manager.getArchive(i);
            synchronized (_runnerToArchive) {
                _runnerToArchive.remove(runner);
                if ( (archive.getNextPullTime() > 0) && (archive.getNextPullTime() <= now) ) {
                    if (archive.getIndexFetchComplete()) {
                        // there's stuff to be done
                        if (_runnerToArchive.containsValue(archive)) {
                            // but someone else is doing it
                            continue;
                        } else {
                            _runnerToArchive.put(runner, archive);
                            return archive;
                        }
                    } else {
                        // still fetching the index
                    }
                } else {
                    // not scheduled, or scheduled for the future
                }
            }
        }
        return null;
    }

    private void fetch(Runner runner, SyncArchive archive) {
        String url = archive.getURL();
        if ( (url == null) || (url.length() == 0) ) {
            archive.indexFetchFail("No URL", null, false);
            synchronized (_runnerToArchive) { _runnerToArchive.remove(runner); }
            return;
        }
        url = url.trim();
        if ( (url.startsWith("USK@")) || (url.startsWith("SSK@")) || (url.startsWith("KSK@")) ) {
            fetchFreenet(archive);
        } else if (url.startsWith("/") || url.startsWith("file://") || url.startsWith("C:\\")) {
            fetchFile(archive);
        } else { // use http as the fallthrough, for "http://foo/" as well as "foo/"
            fetchHTTP(archive);
        }
        archive.fetchActionComplete();
        synchronized (_runnerToArchive) { _runnerToArchive.remove(runner); }
    }
    
    private void fetchFreenet(SyncArchive archive) {
        int actions = archive.getIncomingActionCount();
        for (int i = 0; i < actions; i++) {
            while (!_manager.isOnline())
                try { Thread.sleep(1000); } catch (InterruptedException ie) {}
        
            SyncArchive.IncomingAction action = archive.getIncomingAction(i);
            if (action.getCompletionTime() > 0) continue; // already complete
            if (action.isPaused()) continue; // dont wanna do it
            if (!action.setIsExecuting(true)) continue; // someone else is doing it
            
            SyndieURI uri = action.getURI();
            if (isLocal(uri)) { // fetched concurrently from another archive
                action.importSuccessful();
                continue;
            }
        
            String url = getFreenetURL(archive, uri);
            if (url == null) {
                action.fetchFailed("Invalid freenet archive URL", null);
            } else {
                _manager.getUI().statusMessage("Fetching [" + url + "]");
                try {
                    File dataFile = File.createTempFile("freenetget", "dat", _manager.getClient().getTempDir());
                    EepGet get = new EepGet(I2PAppContext.getGlobalContext(), archive.getHTTPProxyHost(), archive.getHTTPProxyPort(), 3, dataFile.getAbsolutePath(), url);
                    GetListener lsnr = new GetListener(action, dataFile);
                    get.addStatusListener(lsnr);
                    get.fetch();
                } catch (IOException ioe) {
                    action.fetchFailed("Internal error writing temp file", ioe);
                }
            }
        }
    }
    
    private String getFreenetURL(SyncArchive archive, SyndieURI uri) {
        String archiveURL = archive.getURL();
        int keyStart = archiveURL.indexOf('@') - 3; // USK@/CHK@/SSK@ (fix if freenet ever gets other keys)
        if (keyStart < 0) return null;
        int end = archiveURL.indexOf('?', keyStart);
        String key = null;
        if (end < keyStart)
            key = archiveURL.substring(keyStart);
        else
            key = archiveURL.substring(keyStart, end);
        
        // ok, now we have SSK@foo/bar/baz
        // turn that into SSK@foo/bar/$scope/{$msgId,meta}.syndie
        if (key.indexOf('/') > 0) {
            if (!key.endsWith("/"))
                key = key.substring(0, key.lastIndexOf('/')+1);
        } else {
            key = key + '/';
        }
        if (uri.getMessageId() != null)
            key = key + uri.getMessageId().toString() + Constants.FILENAME_SUFFIX;
        else
            key = key + "meta" + Constants.FILENAME_SUFFIX;
        
        key = key + "?forcedownload"; // don't give us a content type warning
        
        return IndexFetcher.getFProxyURL(archive) + key;
    }
    
    private void fetchFile(SyncArchive archive) {
        String file = archive.getURL();
        if (file.startsWith("file://") && (file.length() > "file://".length()))
            file = file.substring("file://".length());
        File f = new File(file);
        File archiveDir = f;
        if (f.isDirectory()) {
            archiveDir = f;
        } else {
            archiveDir = f.getParentFile();
        }
        
        int actions = archive.getIncomingActionCount();
        for (int i = 0; i < actions; i++) {
            while (!_manager.isOnline())
                try { Thread.sleep(1000); } catch (InterruptedException ie) {}
        
            SyncArchive.IncomingAction action = archive.getIncomingAction(i);
            if (action.getCompletionTime() > 0) continue; // already complete
            if (action.isPaused()) continue; // dont wanna do it
            SyndieURI uri = action.getURI();
            
            File src = null;
            if (uri.getMessageId() == null)
                src = new File(new File(archiveDir, uri.getScope().toBase64()), "meta" + Constants.FILENAME_SUFFIX);
            else
                src = new File(new File(archiveDir, uri.getScope().toBase64()), uri.getMessageId().toString() + Constants.FILENAME_SUFFIX);
            
            importData(action, src, false);
        }
    }
    
    private void fetchHTTP(SyncArchive archive) {
        int actions = archive.getIncomingActionCount();
        for (int i = 0; i < actions; i++) {
            while (!_manager.isOnline())
                try { Thread.sleep(1000); } catch (InterruptedException ie) {}
        
            SyncArchive.IncomingAction action = archive.getIncomingAction(i);
            if (action.getCompletionTime() > 0) continue; // already complete
            if (action.isPaused()) continue; // dont wanna do it
            if (!action.setIsExecuting(true)) continue; // someone else is doing it
            
            SyndieURI uri = action.getURI();
        
            if (isLocal(uri)) { // fetched concurrently from another archive
                action.importSuccessful();
                continue;
            }
        
            String url = archive.getURL();
            if (url.indexOf("://") == -1)
                url = "http://" + url;

            int q = url.indexOf('?');
            String query = "";
            if (q != -1) {
                query = url.substring(q);
                url = url.substring(0, q);
            }
            int dir = url.lastIndexOf('/');
            if (dir <= "http://".length())
                url = url + '/';
            else
                url = url.substring(0, dir) + '/';

            url = url + uri.getScope().toBase64() + '/';
            if (uri.getMessageId() != null)
                url = url + uri.getMessageId().toString() + Constants.FILENAME_SUFFIX;
            else
                url = url + "meta" + Constants.FILENAME_SUFFIX;
        
            url = url + query;
            
            _manager.getUI().statusMessage("Fetching [" + url + "]");
            try {
                File dataFile = File.createTempFile("httpget", "dat", _manager.getClient().getTempDir());
                EepGet get = new EepGet(I2PAppContext.getGlobalContext(), archive.getHTTPProxyHost(), archive.getHTTPProxyPort(), 3, dataFile.getAbsolutePath(), url);
                GetListener lsnr = new GetListener(action, dataFile);
                get.addStatusListener(lsnr);
                get.fetch(60*1000);
            } catch (IOException ioe) {
                action.fetchFailed("Internal error writing temp file", ioe);
            }
        }
    }
    
    private boolean isLocal(SyndieURI uri) {
        if (uri.getMessageId() != null) {
            long msgId = _manager.getClient().getMessageId(uri.getScope(), uri.getMessageId());
            if (msgId >= 0)
                return true;
        }
        return false;
    }
    
    private class GetListener implements EepGet.StatusListener {
        private SyncArchive.IncomingAction _incomingAction;
        private File _dataFile;
        private Exception _err;
        public GetListener(SyncArchive.IncomingAction action, File dataFile) {
            _incomingAction = action;
            _dataFile = dataFile;
        }

        public void transferComplete(long alreadyTransferred, long bytesTransferred, long bytesRemaining, String url, String outputFile, boolean notModified) {
            _manager.getUI().debugMessage("Fetch data complete [" + url + "] after " + bytesTransferred);
            importData(_incomingAction, _dataFile, true);
        }
        
        public void attemptFailed(String url, long bytesTransferred, long bytesRemaining, int currentAttempt, int numRetries, Exception cause) {
            _manager.getUI().debugMessage("Fetch data attempt failed [" + url + "] after " + bytesTransferred);
            _err = cause;
        }
        public void transferFailed(String url, long bytesTransferred, long bytesRemaining, int currentAttempt) {
            _manager.getUI().debugMessage("Fetch data totally failed [" + url + "] after " + bytesTransferred + " and " + currentAttempt + " attempts");
            _incomingAction.fetchFailed("Unable to fetch", _err);
        }
        public void bytesTransferred(long alreadyTransferred, int currentWrite, long bytesTransferred, long bytesRemaining, String url) {}
        public void headerReceived(String url, int currentAttempt, String key, String val) {}
        public void attempting(String url) {
            _manager.getUI().debugMessage("Fetch data attempting [" + url + "]...");
        }
    }
    
    private void importData(SyncArchive.IncomingAction action, File datafile, boolean delete) {
        Importer imp = new Importer(_manager.getClient());
        InputStream src = null;
        try {
            src = new FileInputStream(datafile);
            boolean ok = imp.processMessage(_manager.getUI(), _manager.getClient(), src, null, false);
            if (!ok) {
                action.importCorrupt();
            } else {
                if (imp.wasPBE()) {
                    String prompt = imp.getPBEPrompt();
                    action.importPBE(prompt);
                } else if (imp.wasMissingKey()) {
                    if (imp.wasReply()) {
                        action.importMissingReplyKey();
                    } else {
                        action.importMissingReadKey();
                    }
                } else {
                    action.importSuccessful();
                }
            }
        } catch (IOException ioe) {
            action.importFailed("Error reading", ioe);
        } finally {
            if (delete)
                datafile.delete();
        }
    }
}
