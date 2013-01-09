package syndie.db;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.i2p.I2PAppContext;
import net.i2p.util.EepGet;
import net.i2p.util.FileUtil;
import net.i2p.util.SecureFile;

import syndie.Constants;
import syndie.data.SyndieURI;
import syndie.util.RFC822Date;

/**
 *  Fetch the shared-index.dat file and process it
 */
class IndexFetcher {
    private SyncManager _manager;
    private boolean _die;
    
    private static final int FREENET_RETRIES = 0;
    private static final int CLEARNET_RETRIES = 0;
    private static final int I2P_RETRIES = 1;


    public IndexFetcher(SyncManager mgr) {
        _manager = mgr;
    }
    
    public void start() {
        Thread t = new Thread(new Runner(), "IndexFetcher");
        t.setDaemon(true);
        t.start();
    }
    
    public void wakeUp() { synchronized (this) { notifyAll(); } }
    public void kill() { _die = true; wakeUp(); }
    
    private class Runner implements Runnable {
        public void run() {
            while (true) {
                if (_die) return;
                
                while (!_manager.isOnline()) {
                    try {
                        synchronized (IndexFetcher.this) {
                            IndexFetcher.this.wait(60*1000);
                        } 
                    } catch (InterruptedException ie) {}
                    //_manager.getUI().debugMessage("not fetching indexes, as we aren't online");
                    if (_die) return;
                }
                
                SyncArchive archive = getNextToFetch();
                if (archive != null) {
                    _manager.getUI().debugMessage("next index to fetch: " + archive);
                    try {
                        fetch(archive);
                    } catch (Exception e) {
                        archive.indexFetchFail("Internal error fetching the index", e, true);
                    }
                } else {
                    //_manager.getUI().debugMessage("no next index to fetch, waiting 60s");
                    try {
                        synchronized (IndexFetcher.this) {
                            IndexFetcher.this.wait(60*1000);
                        } 
                    } catch (InterruptedException ie) {}
                }
            }
        }
    }
    
    private SyncArchive getNextToFetch() {
        int count = _manager.getArchiveCount();
        // shuffle the archives so we aren't always syncing with the first on the list
        List<SyncArchive> archives = new ArrayList(count);
        for (int i = 0; i < count; i++) {
            archives.add(_manager.getArchive(i));
        }
        Collections.shuffle(archives);
        long now = System.currentTimeMillis();
        for (SyncArchive archive : archives) {
            //_manager.getUI().debugMessage("indexFetch.getNextToFetch: " + archive + " nextSyncTime: " + archive.getNextSyncTime());
            if ( (archive.getNextSyncTime() > 0) && (archive.getNextSyncTime() <= now) ) {
                if (archive.getIndexFetchInProgress() || archive.getIndexFetchComplete()) {
                    _manager.getUI().debugMessage("archive fetch already in progress: " + archive.getName() + " inprogress?" + archive.getIndexFetchInProgress() + " complete? " + archive.getIndexFetchComplete());
                    continue;
                }
                archive.setIndexFetchInProgress(true);
                return archive;
            }
        }
        //_manager.getUI().debugMessage("no more archives to fetchIndex for");
        return null;
    }

    private void fetch(SyncArchive archive) {
        _manager.getUI().debugMessage("fetch index for " + archive.getName());
        String url = archive.getURL();
        if ( (url == null) || (url.length() == 0) ) {
            archive.indexFetchFail("No URL", null, false);
            return;
        }
        if ( (url.indexOf("USK@") >= 0) || (url.indexOf("SSK@") >= 0) || (url.indexOf("KSK@") >= 0) || (url.indexOf("CHK@") >= 0)) {
            if (archive.getPostKey() != null) {
                // fake it
                SharedArchive shared = new SharedArchive();
                SharedArchiveEngine.PullStrategy strategy = archive.getPullStrategy();
                if (strategy == null)
                    strategy = _manager.getDefaultPullStrategy();
                shared.setAbout(LocalArchiveManager.getLocalAbout(_manager.getClient(), strategy));
                shared.setChannels(new ArrayList());
                shared.setMessages(new ArrayList());
                archive.indexFetched(_manager.getUI(), shared);
            } else {
                fetchFreenetIndex(archive);
            }
        } else if (url.startsWith("/") || url.startsWith("file://") || url.startsWith("C:\\")) {
            fetchFileIndex(archive);
        } else { // use http as the fallthrough, for "http://foo/" as well as "foo/"
            fetchHTTPIndex(archive);
        }
    }
    
    private void fetchFreenetIndex(SyncArchive archive) {
        String url = getFreenetURL(archive);
        if (url == null) {
            //URL is not a freenet key
            archive.indexFetchFail("URL is not a valid freenet key", null, false);
        } else {
            //URL seems to be correct, go on
            if ( (archive.getHTTPProxyHost() != null) && (archive.getHTTPProxyHost().length() > 0) )
                _manager.getUI().statusMessage("Fetching [" + url + "] proxy " + archive.getHTTPProxyHost() + ":" + archive.getHTTPProxyPort());
            else
                _manager.getUI().statusMessage("Fetching [" + url + "]");
            try {
                File indexFile = SecureFile.createTempFile("httpindex", "dat", _manager.getClient().getTempDir());
                final EepGet get = new EepGet(I2PAppContext.getGlobalContext(), archive.getHTTPProxyHost(), archive.getHTTPProxyPort(),
                                              FREENET_RETRIES, indexFile.getAbsolutePath(), url);
                GetListener lsnr = new GetListener(get, url, archive, indexFile);
                get.addStatusListener(lsnr);
                Thread t = new Thread(new Runnable() { 
                    public void run() {
                        // 5 minutes for the headers, 10 minutes total, and up to 60s of inactivity
                        get.fetch(5*60*1000, 10*60*1000, 60*1000);
                    }
                }, "IndexFetch " + url);
                t.setDaemon(true);
                t.start();
            } catch (IOException ioe) {
                archive.indexFetchFail("Internal error writing temp file", ioe, true);
            }
        }
    }
    
    private String getFreenetURL(SyncArchive archive) { return getFreenetURL(archive, null); }
    static String getFreenetURL(SyncArchive archive, SyndieURI uri) {
        String finalURI = "";
        String archiveURL = archive.getURL();

        //split prefix and keytype from everything else
        int keyTypePos = archiveURL.indexOf("@");
        String keyType = archiveURL.substring(keyTypePos-3, keyTypePos+1); //Keytype, USK@/CHK@/...
        String prefix = archiveURL.substring(0, keyTypePos-3); //anything before the USK@/CHK@/SSK@/KSK@
        archiveURL = archiveURL.substring(keyTypePos+1);
	
        //strip any existing parameters from the end of the URL
        int end = archiveURL.indexOf("?");
        if (end >= 0) { 
            archiveURL = archiveURL.substring(0, end);
        } 

        if ( !keyType.equals("USK@") ) {
            //not a USK key, so further parsing is futile. Stick everything back together
            finalURI = keyType + archiveURL;
            if ( !finalURI.endsWith("/") ) {
                finalURI = finalURI + "/";
            }
        } else {
            //USK key, procede with getting the raw key and the name
            int firstSlash = archiveURL.indexOf("/");
            if ( firstSlash == -1 ) { 
                return null; //invalid url
            } else {
                String key = archiveURL.substring(0, firstSlash);
                archiveURL = archiveURL.substring(firstSlash+1);

                int secondSlash = archiveURL.indexOf("/");
                if ( secondSlash == -1 ) {
                    return null; //invalid url
                } else {
                    String name = archiveURL.substring(0, secondSlash);

                    //now build the (nearly) final URI depending on the fetch request
                    if ( uri == null ) {
                        //shared-index.dat fetch, use -1 as edition
                        finalURI = keyType + key + "/" + name + "/-1/";
                    } else {
                        //other fetch use the normal edition
                        finalURI = keyType + key + "/" + name + "/0/";
                    }
                }
            }
        }
        if (uri == null) {
            // turn the key into [http://foo.i2p/]SSK@foo/bar/shared-index.dat
            finalURI = finalURI + LocalArchiveManager.SHARED_INDEX_FILE;
        } else {
            finalURI = finalURI + uri.getScope().toBase64() + "/";
            if (uri.getMessageId() != null) {
                // turn the key into [http://foo.i2p/]SSK@foo/bar/$scope/$messageId.syndie
                finalURI = finalURI + uri.getMessageId().toString() + Constants.FILENAME_SUFFIX;
            } else {
                // turn the key into [http://foo.i2p/]SSK@foo/bar/$scope/meta.syndie
                finalURI = finalURI + "meta" + Constants.FILENAME_SUFFIX;
            }
        }
        
        finalURI = finalURI + "?forcedownload"; // don't give us a content type warning
        
        if ( prefix.equals("") )
            return getFProxyURL(archive) + finalURI;
        else // http://fproxy.tino.i2p/USK@foo/bar/shared-index.dat?forcedownload
            return prefix + finalURI;
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
        
        int retries;
        boolean shouldProxy = archive.getHTTPProxyHost() != null && archive.getHTTPProxyHost().length() > 0;
        if (shouldProxy) {
            retries = I2P_RETRIES;
            _manager.getUI().statusMessage("Fetching [" + url + "] proxy " + archive.getHTTPProxyHost() + ":" + archive.getHTTPProxyPort());
        } else {
            retries = CLEARNET_RETRIES;
            _manager.getUI().statusMessage("Fetching [" + url + "]");
        }
        try {
            long lastTime = archive.getLastSyncTime();
            String lastMod = lastTime > 0 ? RFC822Date.to822Date(lastTime) : null;
            File indexFile = SecureFile.createTempFile("httpindex", "dat", _manager.getClient().getTempDir());
            EepGet get = new EepGet(I2PAppContext.getGlobalContext(), shouldProxy, archive.getHTTPProxyHost(), archive.getHTTPProxyPort(),
                                    retries, indexFile.getAbsolutePath(), url, true, null, lastMod);
            GetListener lsnr = new GetListener(get, url, archive, indexFile);
            get.addStatusListener(lsnr);
            // 1 minute for the headers, 5 minutes total, and up to 60s of inactivity
            get.fetch(60*1000, 5*60*1000, 60*1000);
        } catch (IOException ioe) {
            archive.indexFetchFail("Internal error writing temp file", ioe, true);
        }
    }
    
    private class GetListener implements EepGet.StatusListener {
        private final EepGet _get;
        private final String _url;
        private final SyncArchive _archive;
        private final File _indexFile;
        private Exception _err;

        public GetListener(EepGet get, String url, SyncArchive archive, File indexFile) {
            _get = get;
            _url = url;
            _archive = archive;
            _indexFile = indexFile;
        }

        /** @return null on error */
        private File getSavedIndexFile() {
            String host;
            try {
                URI uri = new URI(_url);
                host = uri.getHost();
                if (host == null || host.length() <= 0)
                    return null;
            } catch (URISyntaxException use) {
                return null;
            }
            File dir = new File(_manager.getClient().getRootDir(), "indexes");
            String name = host + '-' + LocalArchiveManager.SHARED_INDEX_FILE;
            return new SecureFile(dir, name);
        }

        public void transferComplete(long alreadyTransferred, long bytesTransferred, long bytesRemaining, String url, String outputFile, boolean notModified) {
            _manager.getUI().debugMessage("Fetch complete [" + url + "] after " + bytesTransferred);
            int status = _get.getStatusCode();
            if (_indexFile.exists() && _indexFile.length() > 0) {
                FileInputStream fin = null;
                try {
                    fin = new FileInputStream(_indexFile);
                    SharedArchive index = new SharedArchive();
                    index.read(fin);
                    _archive.indexFetched(_manager.getUI(), index);
                    // we don't store the full index in the DB, only the about part,
                    // so save it for later
                    File to = getSavedIndexFile();
                    if (to != null)
                        FileUtil.rename(_indexFile, to);
                } catch (IOException ioe) {
                    _archive.indexFetchFail("Corrupt archive: " + ioe.getMessage(), ioe, true);
                } finally {
                    if (fin != null) try { fin.close(); } catch (IOException ioe) {}
                    _indexFile.delete();
                }
            } else if (status == 304) {
                // we don't store the full index in the DB, only the about part,
                // so read it back in here
                File old = getSavedIndexFile();
                if (old == null) {
                    _archive.indexNotModified(_manager.getUI(), null);
                    return;
                }
                FileInputStream fin = null;
                try {
                    SharedArchive index = new SharedArchive();
                    fin = new FileInputStream(old);
                    index.read(fin);
                    _archive.indexNotModified(_manager.getUI(), index);
                } catch (IOException ioe) {
                    _archive.indexNotModified(_manager.getUI(), null);
                    old.delete();
                } finally {
                    if (fin != null) try { fin.close(); } catch (IOException ioe) {}
                }
            } else if (status == 403) {
                _archive.indexFetchFail("Permission denied", null, false);
            } else {
                _archive.indexFetchFail("Response code " + status, null, true);
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

        public void bytesTransferred(long alreadyTransferred, int currentWrite,
                                     long bytesTransferred, long bytesRemaining, String url) {
            long rcvd = alreadyTransferred + currentWrite;
            long total = bytesRemaining >= 0 ? rcvd + bytesRemaining : -1;
            _archive.setIndexFetchProgress(rcvd, total);
        }

        public void headerReceived(String url, int currentAttempt, String key, String val) {}
        public void attempting(String url) {
            _manager.getUI().debugMessage("Fetch attempting [" + url + "]...");
        }
    }
}
