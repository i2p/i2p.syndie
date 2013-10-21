package syndie.db;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.i2p.I2PAppContext;
import net.i2p.util.EepGet;
import net.i2p.util.SecureFile;

import syndie.Constants;
import syndie.data.SyndieURI;
import static syndie.db.ImportResult.Detail.*;

/**
 *  Fetcher threads
 */
class SyncInboundFetcher {
    private final SyncManager _manager;
    private static final Map<Runner, SyncArchive> _runnerToArchive = new HashMap();
    private volatile boolean _die;

    private static final int THREADS = 3;
    private static final int I2P_RETRIES = 1;
    
    public SyncInboundFetcher(SyncManager mgr) {
        _manager = mgr;
    }
    
    public void start() {
        for (int i = 0; i < THREADS; i++) {
            Thread t = new Thread(new Runner(), "InboundFetcher" + (i+1) + '/' + THREADS);
            t.setDaemon(true);
            t.start();
        }
    }

    public void wakeUp() { synchronized (this) { notifyAll(); } }

    public void kill() { _die = true; wakeUp(); }
        
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
                    //_manager.getUI().debugMessage("next fetch inbound for " + archive);
                    try {
                        fetch(Runner.this, archive);
                    } catch (Exception e) {
                        synchronized (_runnerToArchive) { _runnerToArchive.remove(Runner.this); }
                        archive.indexFetchFail("Internal error fetching inbound", e, true);
                    }
                } else {
                    //_manager.getUI().debugMessage("no next to fetch inbound for, waiting 60s");
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
        // shuffle the archives so we aren't always syncing with the first on the list
        List<SyncArchive> archives = new ArrayList(count);
        for (int i = 0; i < count; i++) {
            archives.add(_manager.getArchive(i));
        }
        Collections.shuffle(archives);
        long now = System.currentTimeMillis();
        for (SyncArchive archive : archives) {
            synchronized (_runnerToArchive) {
                _runnerToArchive.remove(runner);
                if ( (archive.getNextSyncTime() > 0) && (archive.getNextSyncTime() <= now) ) {
                    if (archive.getIndexFetchComplete()) {
                        _manager.getUI().debugMessage("inbund fetch wanted for " + archive);
                        // there's stuff to be done
                        if (_runnerToArchive.containsValue(archive)) {
                            // but someone else is doing it
                            _manager.getUI().debugMessage("inbound fetch wanted for " + archive + " but its already in progress");
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
        long whitelistGroupId = archive.getWhitelistGroupId();
        Set whitelistScopes = _manager.getClient().getReferencedScopes(whitelistGroupId);

        DataImporter importer = new DataImporter(whitelistScopes);
        Thread t = new Thread(importer, "Data importer");
        t.start();
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
                action.importSuccessful(IMPORT_ALREADY);
                continue;
            }
        
            String url = IndexFetcher.getFreenetURL(archive, uri);
            if (url == null) {
                action.importFailed(IMPORT_BAD_FREENET_URL);
            } else {
                _manager.getUI().debugMessage("Fetching [" + url + "]");
                try {
                    File dataFile = SecureFile.createTempFile("freenetget", "dat", _manager.getClient().getTempDir());
                    EepGet get = new EepGet(I2PAppContext.getGlobalContext(), archive.getHTTPProxyHost(), archive.getHTTPProxyPort(), 0, dataFile.getAbsolutePath(), url);
                    // the index fetch runs async, but these run synchronously, since we don't want to fire up e.g. 500 threads to pull
                    // new messages.  much to optimize on this front though
                    GetListener lsnr = new GetListener(action, dataFile, importer, whitelistScopes);
                    get.addStatusListener(lsnr);
                    // 1 minute for the headers, 5 minutes total, and up to 60s of inactivity
                    get.fetch(60*1000, 5*60*1000, 60*1000);
                } catch (IOException ioe) {
                    action.importFailed(IMPORT_IOE, ioe);
                }
            }
        }
        importer.finishQueue();
        importer.complete();
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

        long whitelistGroupId = archive.getWhitelistGroupId();
        Set whitelistScopes = _manager.getClient().getReferencedScopes(whitelistGroupId);

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
        
            importData(action, src, false, whitelistScopes);
        }
    }
    
    private void fetchHTTP(SyncArchive archive) {
        List<SyncArchive.IncomingAction> pendingMeta = new ArrayList();
        List<SyncArchive.IncomingAction> pendingMsg = new ArrayList();
        int actions = archive.getIncomingActionCount();
        for (int i = 0; i < actions; i++) {
            SyncArchive.IncomingAction action = archive.getIncomingAction(i);
            if (action.getCompletionTime() > 0) continue; // already complete
            if (action.isPaused()) continue; // dont wanna do it
            if (!action.setIsExecuting(true)) continue; // someone else is doing it
            
            SyndieURI uri = action.getURI();
        
            if (isLocal(uri)) { // fetched concurrently from another archive
                action.importSuccessful(IMPORT_ALREADY);
                continue;
            }

            if (uri.getMessageId() != null)
                pendingMsg.add(action);
            else
                pendingMeta.add(action);
        }
        
        if ( (pendingMeta.size() == 0) &&  (pendingMsg.size() == 0) ) {
            _manager.getUI().debugMessage("nothing to fetch...");
            return;
        }
        
        String archiveURL = archive.getURL();
        if (archiveURL.indexOf("://") == -1)
            archiveURL = "http://" + archiveURL;

        int q = archiveURL.indexOf('?');
        String query = "";
        if (q != -1) {
            query = archiveURL.substring(q);
            archiveURL = archiveURL.substring(0, q);
        }
        int dir = archiveURL.lastIndexOf('/');
        if (dir <= "http://".length())
            archiveURL = archiveURL + '/';
        else
            archiveURL = archiveURL.substring(0, dir) + '/';

        long whitelistGroupId = archive.getWhitelistGroupId();
        Set whitelistScopes = _manager.getClient().getReferencedScopes(whitelistGroupId);
        
        // successful fetches are enqueued in the importer thread so we can import serially without
        // blocking the fetches
        DataImporter importer = new DataImporter(whitelistScopes);
        Thread t = new Thread(importer, "Data importer");
        t.start();
        
        // fetch all of the meta before any of the messages, as we need the meta for the channels
        // we are importing the messages with (to verify signatures).  within these fetches there
        // are 5 concurrent fetches running through the individual files to fetch
        if (!pendingMeta.isEmpty()) {
            fetchHTTPMeta(archive, pendingMeta, archiveURL, query, importer, whitelistScopes);
            _manager.getUI().debugMessage("meta fetches run, waiting for the queue to finish");
            importer.finishQueue();
            _manager.getUI().debugMessage("meta fetches imported, fetching msgs");
        }
        if (!pendingMsg.isEmpty()) {
            fetchHTTPMsgs(archive, pendingMsg, archiveURL, query, importer, whitelistScopes);
            _manager.getUI().debugMessage("msg fetches run, waiting for the queue to finish");
            importer.finishQueue();
            _manager.getUI().debugMessage("msgs imported, complete");
        }
        importer.complete();
    }
    
    private static final int CONCURRENT_FETCHES = 3;
    
    private void fetchHTTPMeta(SyncArchive archive, List<SyncArchive.IncomingAction> actions,
                               String archiveURL, String query, DataImporter importer, Set whitelistScopes) {
        int cnt = Math.min(actions.size(), CONCURRENT_FETCHES);
        List<Thread> fetchers = new ArrayList(cnt);
        for (int i = 0; i < cnt; i++) {
            Thread t = new Thread(new Fetch(archive, actions, archiveURL, query, importer, whitelistScopes), "MetaFetcher " + i);
            t.start();
            fetchers.add(t);
        }
        while (fetchers.size() > 0) {
            Thread t = fetchers.remove(0);
            try { t.join(); } catch (InterruptedException ie) {}
        }
    }
    
    private void fetchHTTPMsgs(SyncArchive archive, List<SyncArchive.IncomingAction> actions,
                               String archiveURL, String query, DataImporter importer, Set whitelistScopes) {
        int cnt = Math.min(actions.size(), CONCURRENT_FETCHES);
        List<Thread> fetchers = new ArrayList(cnt);
        for (int i = 0; i < cnt; i++) {
            Thread t = new Thread(new Fetch(archive, actions, archiveURL, query, importer, whitelistScopes), "MsgFetcher " + i);
            t.start();
            fetchers.add(t);
        }
        while (fetchers.size() > 0) {
            Thread t = fetchers.remove(0);
            try { t.join(); } catch (InterruptedException ie) {}
        }
    }
    
    private class Fetch implements Runnable {
        private SyncArchive _archive;
        private List<SyncArchive.IncomingAction> _actions;
        private String _archiveURL;
        private String _query;
        private DataImporter _importer;
        private Set _whitelistScopes;
        
        public Fetch(SyncArchive archive, List<SyncArchive.IncomingAction> actions,
                     String archiveURL, String query, DataImporter importer, Set whitelistScopes) {
            _archive = archive;
            _actions = actions;
            _archiveURL = archiveURL;
            _query = query;
            _importer = importer;
            _whitelistScopes = whitelistScopes;
        }
        public void run() {
            while (true) {
                while (!_manager.isOnline())
                    try { Thread.sleep(1000); } catch (InterruptedException ie) {}
                
                SyncArchive.IncomingAction action = null;
                synchronized (_actions) {
                    if (_actions.size() <= 0) return;
                    action = _actions.remove(0);
                }
                
                if (action.getCompletionTime() > 0) continue; // already complete
                if (action.isPaused()) continue; // dont wanna do it

                SyndieURI uri = action.getURI();

                if (isLocal(uri)) { // fetched concurrently from another archive
                    action.importSuccessful(IMPORT_ALREADY);
                    continue;
                }

                if (uri.getMessageId() == null)
                    action.setFetchingMeta();
                else
                    action.setFetchingBody();

                String url = _archiveURL;
                url = url + uri.getScope().toBase64() + '/';
                if (uri.getMessageId() == null)
                    url = url + "meta" + Constants.FILENAME_SUFFIX;
                else
                    url = url + uri.getMessageId().toString() + Constants.FILENAME_SUFFIX;
                url = url + _query;

                if ( (_archive.getHTTPProxyHost() != null) && (_archive.getHTTPProxyHost().length() > 0) )
                    _manager.getUI().debugMessage(Thread.currentThread().getName() + ": Fetching [" + url + "] proxy " + _archive.getHTTPProxyHost() + ":" + _archive.getHTTPProxyPort());
                else
                    _manager.getUI().debugMessage(Thread.currentThread().getName() + ": Fetching [" + url + "]");
                try {
                    File dataFile = SecureFile.createTempFile("httpget", "dat", _manager.getClient().getTempDir());
                    EepGet get = new EepGet(I2PAppContext.getGlobalContext(), _archive.getHTTPProxyHost(), _archive.getHTTPProxyPort(), I2P_RETRIES, dataFile.getAbsolutePath(), url);
                    GetListener lsnr = new GetListener(action, dataFile, _importer, _whitelistScopes);
                    get.addStatusListener(lsnr);
                    // 1m for headers, 10m total, 60s idle
                    get.fetch(60*1000, 10*60*1000, 60*1000);
                } catch (IOException ioe) {
                    action.importFailed(IMPORT_IOE, ioe);
                }
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
        private final SyncArchive.IncomingAction _incomingAction;
        private final DataImporter _importer;
        private final File _dataFile;
        private Exception _err;
        private final Set _whitelistScopes;

        public GetListener(SyncArchive.IncomingAction action, File dataFile, DataImporter importer, Set whitelistScopes) {
            _importer = importer;
            _incomingAction = action;
            _dataFile = dataFile;
            _whitelistScopes = whitelistScopes;
        }

        public void transferComplete(long alreadyTransferred, long bytesTransferred, long bytesRemaining, String url, String outputFile, boolean notModified) {
            _manager.getUI().debugMessage("Fetch data complete [" + url + "] after " + bytesTransferred);
            if (_importer != null)
                _importer.enqueueData(_incomingAction, _dataFile, true);
            else // only the http fetch uses the multithreaded importer (files are sequential, and freenet fetches are slow enough)
                importData(_incomingAction, _dataFile, true, _whitelistScopes);
        }
        
        public void attemptFailed(String url, long bytesTransferred, long bytesRemaining, int currentAttempt, int numRetries, Exception cause) {
            _manager.getUI().debugMessage("Fetch data attempt failed [" + url + "] after " + bytesTransferred);
            _err = cause;
        }
        public void transferFailed(String url, long bytesTransferred, long bytesRemaining, int currentAttempt) {
            _manager.getUI().debugMessage("Fetch data totally failed [" + url + "] after " + bytesTransferred + " and " + currentAttempt + " attempts");
            _incomingAction.importFailed(IMPORT_FETCH_FAIL, _err);
        }

        public void bytesTransferred(long alreadyTransferred, int currentWrite,
                                     long bytesTransferred, long bytesRemaining, String url) {
            long rcvd = alreadyTransferred + currentWrite;
            long total = bytesRemaining >= 0 ? rcvd + bytesRemaining : -1;
            _incomingAction.setSize(rcvd, total);
        }

        public void headerReceived(String url, int currentAttempt, String key, String val) {}
        public void attempting(String url) {
            //_manager.getUI().debugMessage("Fetch data attempting [" + url + "]...");
        }
    }
    
    private class DataImporter implements Runnable {
        private List _actions;
        private List _files;
        private List _toDelete;
        private boolean _complete;
        private Set _whitelistScopes;
        
        public DataImporter(Set whitelistScopes) { 
            _actions = new ArrayList();
            _files = new ArrayList();
            _toDelete = new ArrayList();
            _complete = false;
            _whitelistScopes = whitelistScopes;
        }

        public Set getWhitelistScopes() { return _whitelistScopes; }
        
        public void enqueueData(SyncArchive.IncomingAction action, File datafile, boolean delete) {
            _manager.getUI().debugMessage(Thread.currentThread().getName() + ": enqueueing import from " + datafile.toString());
            synchronized (DataImporter.this) {
                if (!_actions.contains(action)) {
                    _actions.add(action);
                    _files.add(datafile);
                    _toDelete.add(delete ? Boolean.TRUE : Boolean.FALSE);
                }
                DataImporter.this.notifyAll();
            }
        }
        
        public void complete() {
            _manager.getUI().debugMessage(Thread.currentThread().getName() + ": No more imports");
            synchronized (DataImporter.this) {
                _complete = true;
                DataImporter.this.notifyAll();
            }
        }
        
        public void finishQueue() { 
            while (true) {
                int remaining = 0;
                try {
                    synchronized (DataImporter.this) {
                        remaining = _actions.size();
                        if (remaining <= 0)
                            return;
                        else
                            DataImporter.this.wait(1000);
                    }
                } catch (InterruptedException ie) {}
                _manager.getUI().debugMessage(Thread.currentThread().getName() + ": Waiting for the pending " + remaining + " import action queue to clear...");
            }
        }
        
        public void run() {
            while (!_complete) {
                SyncArchive.IncomingAction action = null;
                File datafile = null;
                boolean delete = false;
                synchronized (DataImporter.this) {
                    if (_actions.size() <= 0)
                        try { DataImporter.this.wait(); } catch (InterruptedException ie) {}
                    if (_actions.size() > 0) {
                        action = (SyncArchive.IncomingAction)_actions.remove(0);
                        datafile = (File)_files.remove(0);
                        delete = ((Boolean)_toDelete.remove(0)).booleanValue();
                    }
                }
                
                if (action != null) {
                    _manager.getUI().debugMessage(Thread.currentThread().getName() + ": executing import from " + datafile.toString());
                    importData(action, datafile, delete, _whitelistScopes);
                }
            }
        }
    }
    
    private void importData(SyncArchive.IncomingAction action, File datafile, boolean delete, Set whitelistScopes) {
        Importer imp = new Importer(_manager.getClient());
        InputStream src = null;
        try {
            src = new FileInputStream(datafile);
            ImportResult.Result result = imp.processMessage(_manager.getUI(), _manager.getClient(), src, null, false, null, null);
            if (!result.ok()) {
                action.importFailed(result);
            } else {
                if (result == IMPORT_PASS_REQD) {
                    String prompt = imp.getPBEPrompt();
                    action.importPBE(prompt);
                } else if (result == IMPORT_NO_READ_KEY || result == IMPORT_NO_REPLY_KEY) {
                    action.importSuccessful(result);
                } else {
                    SyndieURI uri = imp.getURI();
                    boolean matchesWhitelist = false;
                    if ( (whitelistScopes.size() == 0) || (uri.getMessageId() == null) ) {
                        matchesWhitelist = true;
                    } else if (whitelistScopes.contains(uri.getScope())) {
                        matchesWhitelist = true;
                    } else {
                        long msgId = _manager.getClient().getMessageId(uri.getScope(), uri.getMessageId());
                        long targetId = _manager.getClient().getMessageTarget(msgId);
                        if (whitelistScopes.contains(_manager.getClient().getChannelHash(targetId))) {
                            matchesWhitelist = true;
                        }
                    }
                    if (!matchesWhitelist) {
                        action.importFailed("Message valid, but does not match the whitelist", null);
                        _manager.getUI().debugMessage("Message imported on fetch, but does not match the whitelist: " + uri);
                        delete = true;
                        _manager.getClient().deleteFromDB(uri, _manager.getUI());
                    }
                    action.importSuccessful(result);
                }
            }
        } catch (IOException ioe) {
            action.importFailed("Error reading", ioe);
        } catch (Exception e) {
            action.importFailed("Internal error importing", e);
        } catch (Throwable t) {
            t.printStackTrace();
            action.importFailed("Internal error importing: " + t.getMessage(), new Exception(t));
        } finally {
            if (delete)
                datafile.delete();
        }
    }
}
