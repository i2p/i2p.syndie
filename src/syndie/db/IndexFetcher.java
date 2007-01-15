package syndie.db;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import net.i2p.I2PAppContext;
import net.i2p.util.EepGet;

class IndexFetcher {
    private SyncManager _manager;
    
    public IndexFetcher(SyncManager mgr) {
        _manager = mgr;
    }
    
    public void start() {
        Thread t = new Thread(new Runner(), "IndexFetcher");
        t.setDaemon(true);
        t.start();
    }
    
    private class Runner implements Runnable {
        public void run() {
            try { Thread.sleep(30*1000); } catch (InterruptedException ie) {}
            while (true) {
                SyncArchive archive = getNextToFetch();
                if (archive != null)
                    fetch(archive);
                else
                    try { Thread.sleep(60*1000); } catch (InterruptedException ie) {}
            }
        }
    }
    
    private SyncArchive getNextToFetch() {
        int count = _manager.getArchiveCount();
        long now = System.currentTimeMillis();
        for (int i = 0; i < count; i++) {
            SyncArchive archive = _manager.getArchive(i);
            if ( ( (archive.getNextPullTime() > 0) && (archive.getNextPullTime() <= now) ) || 
                 ( (archive.getNextPushTime() > 0) && (archive.getNextPushTime() <= now) ) ) {
                if (archive.getIndexFetchInProgress() || archive.getIndexFetchComplete())
                    continue;
                archive.setIndexFetchInProgress(true);
                return archive;
            }
        }
        return null;
    }

    private void fetch(SyncArchive archive) {
        String url = archive.getURL();
        if ( (url == null) || (url.length() == 0) ) {
            archive.indexFetchFail("No URL", null, false);
            return;
        }
        if ( (url.indexOf("USK@") >= 0) || (url.indexOf("SSK@") >= 0) || (url.indexOf("KSK@") >= 0) ) {
            fetchFreenetIndex(archive);
        } else if (url.startsWith("/") || url.startsWith("file://") || url.startsWith("C:\\")) {
            fetchFileIndex(archive);
        } else { // use http as the fallthrough, for "http://foo/" as well as "foo/"
            fetchHTTPIndex(archive);
        }
    }
    
    private void fetchFreenetIndex(SyncArchive archive) {
        String url = getFreenetURL(archive);
        _manager.getUI().statusMessage("Fetching [" + url + "]");
        try {
            File indexFile = File.createTempFile("httpindex", "dat", _manager.getClient().getTempDir());
            EepGet get = new EepGet(I2PAppContext.getGlobalContext(), 3, indexFile.getAbsolutePath(), url);
            GetListener lsnr = new GetListener(archive, indexFile);
            get.addStatusListener(lsnr);
            get.fetch(); // no timeout
        } catch (IOException ioe) {
            archive.indexFetchFail("Internal error writing temp file", ioe, true);
        }
    }
    
    private String getFreenetURL(SyncArchive archive) {
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
        // turn that into SSK@foo/bar/shared-index.dat
        if (key.indexOf('/') > 0) {
            if (!key.endsWith("/"))
                key = key.substring(0, key.lastIndexOf('/')+1);
        } else {
            key = key + '/';
        }
        
        key = key + LocalArchiveManager.SHARED_INDEX_FILE;
        
        key = key + "?forcedownload"; // don't give us a content type warning
        
        return getFProxyURL(archive) + key;
    }
    
    /** http://localhost:8888/ or whatever */
    static String getFProxyURL(SyncArchive archive) {
        String host = archive.getHTTPProxyHost();
        if (host == null) host = "127.0.0.1";
        int port = archive.getHTTPProxyPort();
        if (port <= 0) port = 8888;
        return "http://" + host + ":" + port + "/";
    }
    
    private void fetchFileIndex(SyncArchive archive) {
        String file = archive.getURL();
        if (file.startsWith("file://") && (file.length() > "file://".length()))
            file = file.substring("file://".length());
        File f = new File(file);
        if (f.exists()) {
            if (f.isDirectory()) {
                f = new File(f, LocalArchiveManager.SHARED_INDEX_FILE);
                if (!f.exists()) {
                    archive.indexFetchFail("Shared index does not exist: " + f.getPath(), null, true);
                    return;
                }
            }
            
            SharedArchive index = new SharedArchive();
            FileInputStream fin = null;
            try {
                fin = new FileInputStream(f);
                index.read(fin);
                archive.indexFetched(_manager.getUI(), index);
            } catch (IOException ioe) {
                archive.indexFetchFail(ioe.getMessage(), ioe, true);
            } finally {
                if (fin != null) try { fin.close(); } catch (IOException ioe) {}
            }
        }
    }
    private void fetchHTTPIndex(SyncArchive archive) {
        String url = archive.getURL();
        if (url.indexOf("://") == -1)
            url = "http://" + url;
        
        if (url.indexOf(LocalArchiveManager.SHARED_INDEX_FILE) == -1) {
            int q = url.indexOf('?');
            String query = "";
            if (q != -1) {
                query = url.substring(q);
                url = url.substring(0, q);
            }
            int dir = url.lastIndexOf('/');
            if (dir <= "http://".length())
                url = url + '/' + LocalArchiveManager.SHARED_INDEX_FILE;
            else
                url = url.substring(0, dir) + '/' + LocalArchiveManager.SHARED_INDEX_FILE;
            
            url = url + query;
        } else {
            // already contains the shared-index.dat, so no need to rewrite it further
        }
        
        _manager.getUI().statusMessage("Fetching [" + url + "]");
        try {
            File indexFile = File.createTempFile("httpindex", "dat", _manager.getClient().getTempDir());
            EepGet get = new EepGet(I2PAppContext.getGlobalContext(), 3, indexFile.getAbsolutePath(), url);
            GetListener lsnr = new GetListener(archive, indexFile);
            get.addStatusListener(lsnr);
            get.fetch(60*1000);
        } catch (IOException ioe) {
            archive.indexFetchFail("Internal error writing temp file", ioe, true);
        }
    }
    
    private class GetListener implements EepGet.StatusListener {
        private SyncArchive _archive;
        private File _indexFile;
        private Exception _err;
        public GetListener(SyncArchive archive, File indexFile) {
            _archive = archive;
            _indexFile = indexFile;
        }

        public void transferComplete(long alreadyTransferred, long bytesTransferred, long bytesRemaining, String url, String outputFile, boolean notModified) {
            _manager.getUI().debugMessage("Fetch complete [" + url + "] after " + bytesTransferred);
            if (_indexFile.exists()) {
                SharedArchive index = new SharedArchive();
                FileInputStream fin = null;
                try {
                    fin = new FileInputStream(_indexFile);
                    index.read(fin);
                    _archive.indexFetched(_manager.getUI(), index);
                } catch (IOException ioe) {
                    _archive.indexFetchFail(ioe.getMessage(), ioe, true);
                } finally {
                    if (fin != null) try { fin.close(); } catch (IOException ioe) {}
                    _indexFile.delete();
                }
            } else {
                _manager.getUI().errorMessage("index file does not exist??" + _indexFile.getAbsolutePath());
            }
        }
        
        public void attemptFailed(String url, long bytesTransferred, long bytesRemaining, int currentAttempt, int numRetries, Exception cause) {
            _manager.getUI().debugMessage("Fetch attempt failed [" + url + "] after " + bytesTransferred);
            _err = cause;
        }
        public void transferFailed(String url, long bytesTransferred, long bytesRemaining, int currentAttempt) {
            _manager.getUI().debugMessage("Fetch totally failed [" + url + "] after " + bytesTransferred + " and " + currentAttempt + " attempts");
            _archive.indexFetchFail("Unable to fetch", _err, true);
        }
        public void bytesTransferred(long alreadyTransferred, int currentWrite, long bytesTransferred, long bytesRemaining, String url) {}
        public void headerReceived(String url, int currentAttempt, String key, String val) {}
        public void attempting(String url) {
            _manager.getUI().debugMessage("Fetch attempting [" + url + "]...");
        }
    }
}
