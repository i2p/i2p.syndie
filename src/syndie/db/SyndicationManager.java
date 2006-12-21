package syndie.db;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import net.i2p.data.Base64;
import net.i2p.data.DataHelper;
import net.i2p.data.Hash;
import net.i2p.data.SessionKey;
import net.i2p.util.EepGet;
import syndie.Constants;
import syndie.data.NymKey;
import syndie.data.SyndieURI;

/**
 *
 */
public class SyndicationManager {
    private DBClient _client;
    private UI _ui;
    private List _archives;
    private List _listeners;
    private List _fetchRecords;
    private List _fetchMetaRecords;
    private int _concurrent;
    private String _httpProxyHost;
    private int _httpProxyPort;
    private String _fcpHost;
    private int _fcpPort;
    private SyndicationManagerScheduler _scheduler;
    /** map of archive name to Runnable that should be run when an archive's pulls are complete */
    private Map _onPullSuccess;
    
    private boolean _archivesLoaded;
    
    public static final int FETCH_SCHEDULED = 0;
    public static final int FETCH_STARTED = 1;
    public static final int FETCH_COMPLETE = 2;
    public static final int FETCH_FAILED = 3;
    public static final int FETCH_IMPORT_OK = 4;
    public static final int FETCH_IMPORT_PBE = 5;
    public static final int FETCH_IMPORT_NOKEY = 6;
    public static final int FETCH_IMPORT_CORRUPT = 7;
    public static final int FETCH_INDEX_LOAD_OK = 8;
    public static final int FETCH_INDEX_LOAD_ERROR = 9;
    public static final int FETCH_INDEX_DIFF_OK = 10;
    public static final int FETCH_STOPPED = 11;
    
    public static final int PUSH_SCHEDULED = 12;
    public static final int PUSH_STARTED = 13;
    public static final int PUSH_SENT = 14;
    public static final int PUSH_ERROR = 15;
    
    public static final int PULL_STRATEGY_DELTA = 0;
    public static final int PULL_STRATEGY_DELTABOOKMARKED = 1;
    public static final int PULL_STRATEGY_DELTAKNOWN = 2;
    public static final int PULL_STRATEGY_EXPLICIT = 3;
    public static final int PULL_STRATEGY_PIR = 4;
    public static final int PULL_STRATEGY_DEFAULT = PULL_STRATEGY_DELTA;
    
    public static final int PUSH_STRATEGY_DELTA = 0;
    public static final int PUSH_STRATEGY_DELTAKNOWN = 1;
    public static final int PUSH_STRATEGY_DEFAULT = PUSH_STRATEGY_DELTA;
    
    public SyndicationManager(DBClient client, UI ui) {
        _client = client;
        _ui = ui;
        _archives = new ArrayList();
        _listeners = new ArrayList();
        _fetchRecords = new ArrayList();
        _fetchMetaRecords = new ArrayList();
        _onPullSuccess = new HashMap();
        _archivesLoaded = false;
    }
    
    /** 
     * note that callbacks on this interface can be hit from any thread, so if they
     * touch an SWT resource, they should do so via display.asyncExec(runnable)
     */
    public interface SyndicationListener {
        public void archiveAdded(SyndicationManager mgr, String name);
        public void archiveRemoved(SyndicationManager mgr, String name);
        public void archiveUpdated(SyndicationManager mgr, String oldName, String newName);
        public void archivesLoaded(SyndicationManager mgr);

        /**
         * ideal status sequence: SCHEDULED, STARTEd, COMPLETE, INDEX_LOAD_OK, INDEX_DIFF_OK
         */
        public void archiveIndexStatus(SyndicationManager mgr, StatusRecord record);
        
        /**
         * ideal status sequence: SCHEDULED, STARTED, COMPLETE, ( IMPORT_OK | IMPORT_PBE )
         */
        public void fetchStatusUpdated(SyndicationManager mgr, StatusRecord record);
        
        /** all syndication tasks are terminal */
        public void syndicationComplete(SyndicationManager mgr);
    }
    
    public int getArchiveCount() { return _archives.size(); }
    public String getArchiveName(int index) {
        NymArchive archive = getArchive(index);
        if (archive != null)
            return archive.getName();
        else
            return null;
    }
    public SyndieURI getArchiveURI(int index) {
        NymArchive archive = getArchive(index);
        if (archive != null)
            return archive.getURI();
        else
            return null;
    }
    public long getLastSyncDate(int index) {
        NymArchive archive = getArchive(index);
        if (archive != null)
            return archive.getLastSyncDate();
        else
            return -1;
    }
    public long getNextSyncDate(int index) {
        NymArchive archive = getArchive(index);
        if (archive != null)
            return archive.getNextSyncDate();
        else
            return -1;
    }
    public SharedArchive getArchiveIndex(int index) {
        NymArchive archive = getArchive(index);
        if (archive != null)
            return archive.getIndex();
        else
            return null;
    }
    public String getCustomProxyHost(int index) {
        NymArchive archive = getArchive(index);
        if (archive != null)
            return archive.getCustomProxyHost();
        else
            return null;
    }
    public int getCustomProxyPort(int index) {
        NymArchive archive = getArchive(index);
        if (archive != null)
            return archive.getCustomProxyPort();
        else
            return -1;
    }
    
    public void setProxies(String httpHost, int httpPort, String fcpHost, int fcpPort) {
        _httpProxyHost = httpHost;
        _httpProxyPort = httpPort;
        _fcpHost = fcpHost;
        _fcpPort = fcpPort;
        
        _client.setDefaultHTTPProxyHost(httpHost);
        _client.setDefaultHTTPProxyPort(httpPort);
        _client.setDefaultFreenetHost(fcpHost);
        _client.setDefaultFreenetPort(fcpPort);
        _client.saveProxyConfig();
    }
    
    public void setPullStrategy(SharedArchiveEngine.PullStrategy strategy) {
        Properties prefs = _client.getNymPrefs();
        if (strategy != null)
            prefs.setProperty("syndicate.pullStrategy", strategy.serialize());
        else
            prefs.remove("syndicate.pullStrategy");
        _client.setNymPrefs(prefs);
    }
    public SharedArchiveEngine.PullStrategy getPullStrategy() {
        Properties prefs = _client.getNymPrefs();
        String strat = prefs.getProperty("syndicate.pullStrategy");
        SharedArchiveEngine.PullStrategy rv = new SharedArchiveEngine.PullStrategy(strat);
        String ser = rv.serialize();
        if (strat != null)
            _ui.debugMessage("db pull strategy: [" + strat + "] eq parsed? " + strat.equals(ser) + ": [" + ser + "]");
        return rv;
    }
    
    
    public void setPushStrategy(SharedArchiveEngine.PushStrategy strategy) {
        Properties prefs = _client.getNymPrefs();
        if (strategy != null)
            prefs.setProperty("syndicate.pushStrategy", strategy.serialize());
        else
            prefs.remove("syndicate.pushStrategy");
        _client.setNymPrefs(prefs);
    }
    public SharedArchiveEngine.PushStrategy getPushStrategy() {
        Properties prefs = _client.getNymPrefs();
        String strat = prefs.getProperty("syndicate.pushStrategy");
        SharedArchiveEngine.PushStrategy rv = new SharedArchiveEngine.PushStrategy(strat);
        String ser = rv.serialize();
        if (strat != null)
            _ui.debugMessage("db push strategy: [" + strat + "] eq parsed? " + strat.equals(ser) + ": [" + ser + "]");
        return rv;
    }

    /** rebuild our shared-index.dat every hour */
    public int getLocalRebuildDelayHours() { return 1; }
    
    /**
     * run this many concurrent http fetches/imports at a time
     */
    public void startFetching(int concurrentFetches) {
        for (int i = _concurrent; i < concurrentFetches; i++) {
            _concurrent++;
            // the first fetcher will (re)build the archive index
            Thread t = new Thread(new Fetcher(i), "Fetcher" + i);
            t.setDaemon(true);
            t.start();
        }
    }
    
    /** schedule a fetch of the particular message/metadata from the given archive */
    public void fetch(String archiveName, SyndieURI uri) {
        StatusRecord rec = new StatusRecord(archiveName, uri);
        fireFetchStatusUpdated(rec);
        synchronized (_fetchRecords) {
            _fetchRecords.add(rec);
            if (uri.isChannel() && (uri.getScope() != null) && (uri.getMessageId() == null))
                _fetchMetaRecords.add(rec);
            _fetchRecords.notifyAll();
        }
    }
    /** schedule a fetch of the archive's index */
    public void fetchIndex(String archiveName) {
        NymArchive archive = getArchive(archiveName);
        if (archive != null) {
            StatusRecord rec = new StatusRecord(archiveName, archive.getURI());
            rec.setStatus(FETCH_SCHEDULED);
            fireIndexStatus(rec);
            synchronized (_fetchRecords) {
                _fetchRecords.add(rec);
                _fetchRecords.notifyAll();
            }
        }
    }
    
    private void fetchIndex(StatusRecord record) {
        NymArchive archive = record.getArchive();
        if (archive == null) {
            synchronized (_onPullSuccess) { _onPullSuccess.remove(record.getSource()); }
            return;
        }
        
        String baseUrl = archive.getURI().getURL();
        if (baseUrl == null) {
            synchronized (_onPullSuccess) { _onPullSuccess.remove(record.getSource()); }
            return;
        }
        if (record.getStatus() == FETCH_STOPPED) {
            synchronized (_onPullSuccess) { _onPullSuccess.remove(record.getSource()); }
            return;
        }

        _ui.debugMessage("fetchIndex: " + record.getSource() + " started");

        String proxyHost = archive.getCustomProxyHost();
        int proxyPort = archive.getCustomProxyPort();
        if ( ( (proxyHost == null) || (proxyPort <= 0) ) &&
             ( (_httpProxyHost != null) && (_httpProxyPort > 0) ) ) {
            proxyHost = _httpProxyHost;
            proxyPort = _httpProxyPort;
        }
        
        int keyStart = -1;
        keyStart = baseUrl.indexOf("SSK@");
        if (keyStart < 0) {
            keyStart = baseUrl.indexOf("USK@");
            if (keyStart < 0) {
                keyStart = baseUrl.indexOf("CHK@");
            }
        }
	boolean includeForceDownload = false;
        if (keyStart >= 0) {
            String fproxyHost = proxyHost;
            int fproxyPort = proxyPort;
            if (fproxyHost == null)
                fproxyHost = "127.0.0.1";
            if (fproxyPort <= 0)
                fproxyPort = 8888;
            proxyHost = null;
            proxyPort = -1;
            baseUrl = "http://" + fproxyHost + ":" + fproxyPort + "/" + baseUrl.substring(keyStart);
	    includeForceDownload = true;
        }
                
        boolean unauth = false;
        String scope = "all"; //opts.getOptValue("scope");
        String url = null;
        if (scope == null)
            scope = "all";
        if (!baseUrl.endsWith("/"))
            baseUrl = baseUrl + "/";
        url = baseUrl + SHARED_INDEX_FILE;
        /*
        if ("new".equalsIgnoreCase(scope)) {
            url = baseUrl + "index-new.dat";
        } else if ("meta".equalsIgnoreCase(scope)) {
            url = baseUrl + "index-meta.dat";
        } else if ("unauth".equalsIgnoreCase(scope)) {
            unauth = true;
            String chan = null; //opts.getOptValue("channel");
            if (chan != null) {
                url = baseUrl + chan + "/index-unauthorized.dat";
            } else {
                url = baseUrl + "index-unauthorized.dat";
            }
        } else { //if ("all".equalsIgnoreCase(scope))
            url = baseUrl + SHARED_INDEX_FILE; //"index-all.dat";
        }
         */
	if (includeForceDownload) url = url + "?forcedownload";

        _ui.debugMessage("fetchIndex: " + record.getSource() + " - fetching: " + url);
        //fireIndexStatus(archive.getName(), INDEX_STATUS_FETCHING, null);
        
        if (record.getStatus() == FETCH_STOPPED) return;

        boolean shouldProxy = (proxyHost != null) && (proxyPort > 0);
        boolean archiveWasRemote = true;
        File out = null;
        if (baseUrl.startsWith("/")) {
            out = new File(url);
            _ui.debugMessage("fetchIndex: " + record.getSource() + " - fetch complete: " + url);
            //fireIndexStatus(archive.getName(), INDEX_STATUS_FETCH_COMPLETE, null);
            record.setStatus(FETCH_COMPLETE);
            fireIndexStatus(record);
            archiveWasRemote = false;
        } else if (baseUrl.startsWith("file://")) {
            out = new File(baseUrl.substring("file://".length()));
            _ui.debugMessage("fetchIndex: " + record.getSource() + " - fetch complete: " + url);
            //fireIndexStatus(archive.getName(), INDEX_STATUS_FETCH_COMPLETE, null);
            record.setStatus(FETCH_COMPLETE);
            fireIndexStatus(record);
            archiveWasRemote = false;
        } else {
            try {
                if (record.getStatus() == FETCH_STOPPED) {
                    synchronized (_onPullSuccess) { _onPullSuccess.remove(record.getSource()); }
                    return;
                }
                out = File.createTempFile("syndicate", ".index", _client.getTempDir());
                EepGet get = new EepGet(_client.ctx(), shouldProxy, proxyHost, (int)proxyPort, 0, out.getPath(), url, false, null, null);
                UIStatusListener lsnr = new UIStatusListener();
                get.addStatusListener(lsnr);
                boolean fetched = get.fetch();
                if (record.getStatus() == FETCH_STOPPED) {
                    if (archiveWasRemote) out.delete(); 
                    synchronized (_onPullSuccess) { _onPullSuccess.remove(record.getSource()); }
                    return;
                }
                if (!fetched) {
                    _ui.errorMessage("Fetch failed of " + url);
                    _ui.debugMessage("fetchIndex: " + record.getSource() + " - fetch error: " + url);
                    //fireIndexStatus(archive.getName(), INDEX_STATUS_FETCH_ERROR, "fetch failed");
                    record.setStatus(FETCH_FAILED);
                    record.setDetail(lsnr.getError());
                    //record.setDetail("");
                    fireIndexStatus(record);
                    if (archiveWasRemote)
                        out.delete();
                    synchronized (_onPullSuccess) { _onPullSuccess.remove(record.getSource()); }
                    return;
                }
                _ui.debugMessage("fetchIndex: " + record.getSource() + " - fetch complete: " + url);
                record.setStatus(FETCH_COMPLETE);
                fireIndexStatus(record);
                //fireIndexStatus(archive.getName(), INDEX_STATUS_FETCH_COMPLETE, null);
            } catch (IOException ioe) {
                _ui.errorMessage("Error pulling the index", ioe);
                record.setStatus(FETCH_FAILED);
                record.setDetail(ioe.getMessage());
                fireIndexStatus(record);
                if (archiveWasRemote && out != null)
                    out.delete();
                synchronized (_onPullSuccess) { _onPullSuccess.remove(record.getSource()); }
                return;
            }
        }
        try {
            if (record.getStatus() == FETCH_STOPPED) {
                if (archiveWasRemote && out != null) out.delete();
                synchronized (_onPullSuccess) { _onPullSuccess.remove(record.getSource()); }
                return;
            }
            SharedArchive index = new SharedArchive();
            FileInputStream fin = new FileInputStream(out);
            index.read(fin);
            fin.close();
            fin = null;
            //_ui.debugMessage("Read index: \n" + index.toString());
            //ArchiveIndex index = ArchiveIndex.loadIndex(out, _ui, unauth);
            archive.setIndex(index);
            HTTPSyndicator syndicator = new HTTPSyndicator(baseUrl, proxyHost, proxyPort, _client, _ui, index, false); //opts.getOptBoolean("reimport", false));
            archive.setSyndicator(syndicator);
            _ui.debugMessage("fetchIndex: " + record.getSource() + " - index loaded");
            record.setStatus(FETCH_INDEX_LOAD_OK);
            fireIndexStatus(record);
            //fireIndexStatus(archive.getName(), INDEX_STATUS_LOAD_OK, null);
            //archive.calculateDiffs(_client, _ui);
            //ArchiveDiff diff = index.diff(_client, _ui, new Opts());
            //archive.setDiff(diff);
            _ui.debugMessage("fetchIndex: " + record.getSource() + " - diff loaded");
            record.setStatus(FETCH_INDEX_DIFF_OK);
            fireIndexStatus(record);
            //fireIndexStatus(archive.getName(), INDEX_STATUS_DIFF_OK, null);
        } catch (IOException ioe) {
            _ui.errorMessage("fetchIndex: " + record.getSource() + " - Error loading the index", ioe);
            record.setStatus(FETCH_INDEX_LOAD_ERROR);
            record.setDetail(ioe.getMessage());
            synchronized (_onPullSuccess) { _onPullSuccess.remove(record.getSource()); }
            fireIndexStatus(record);
            //fireIndexStatus(archive.getName(), INDEX_STATUS_LOAD_ERROR, ioe.getMessage());
        }
        if (archiveWasRemote && out != null)
            out.delete();
    }
    
    //private void fireIndexStatus(String name, int status, String msg) {
    private void fireIndexStatus(StatusRecord record) {
        for (int i = 0; i < _listeners.size(); i++) {
            SyndicationListener lsnr = (SyndicationListener)_listeners.get(i);
            lsnr.archiveIndexStatus(this, record);
        }
    }
    
    /** 
     * queue up the fetch for entries matching the given strategy, as well as pushes
     * @param maxkb don't pull posts larger than this size
     * @param pullStrategy PULL_STRATEGY_* to determine which entries to pull
     * @param pushStrategy PUSH_STRATEGY_* to determine which entries to push
     * @param archiveNames set of archive names to sync with
     */
    public void sync(int maxkb, int pullStrategy, int pushStrategy, Set archiveNames) {
        sync(maxkb, pullStrategy, pushStrategy, archiveNames, null);
    }
    /**
     * @param explicit archive name to a Set of SyndieURIs that should be fetched from 
     *                 them (for the explicit pull strategy)
     */
    public void sync(int maxkb, int pullStrategy, int pushStrategy, Set archiveNames, Map explicit) {
        _ui.debugMessage("sync strategy: " + pullStrategy+"/"+pushStrategy + ": " + explicit);
        pull(maxkb, pullStrategy, archiveNames, explicit);
        push(maxkb, pushStrategy, archiveNames);
    }
    private void pull(int maxkb, int pullStrategy, Set archiveNames, Map explicit) {
        HashSet uris = new HashSet();
        switch (pullStrategy) {
            case PULL_STRATEGY_DELTAKNOWN:
            case PULL_STRATEGY_DELTABOOKMARKED: //todo: actually honor this
                for (int i = 0; i < _archives.size(); i++) {
                    NymArchive archive = (NymArchive)_archives.get(i);
                    if (!archiveNames.contains(archive.getName()))
                        continue;
                    if (archive.getIndex() != null) {
                        SharedArchiveEngine.PullStrategy strategy = new SharedArchiveEngine.PullStrategy();
                        strategy.includeDupForPIR = false;
                        strategy.includePBEMessages = true;
                        strategy.includePrivateMessages = true;
                        strategy.includeRecentMessagesOnly = false;
                        strategy.knownChannelsOnly = true;
                        SharedArchiveEngine engine = new SharedArchiveEngine();
                        List toFetch = engine.selectURIsToPull(_client, _ui, archive.getIndex(), strategy);
                        //List toFetch = archive.getIndex().selectURIsToPull(_client, _ui, strategy);
                        //List toFetch = diff.getFetchKnownURIs(true);
                        for (int j = 0; j < toFetch.size(); j++) {
                            SyndieURI uri = (SyndieURI)toFetch.get(j);
                            if (uris.add(uri)) {
                                fetch(archive.getName(), uri);
                            }
                        }
                    } else {
                        // need to fetch the index first
                    }
                }
                break;
            case PULL_STRATEGY_PIR:
                for (int i = 0; i < _archives.size(); i++) {
                    NymArchive archive = (NymArchive)_archives.get(i);
                    if (!archiveNames.contains(archive.getName()))
                        continue;
                    if (archive.getIndex() != null) {
                        SharedArchiveEngine.PullStrategy strategy = new SharedArchiveEngine.PullStrategy();
                        strategy.includeDupForPIR = true;
                        SharedArchiveEngine engine = new SharedArchiveEngine();
                        List toFetch = engine.selectURIsToPull(_client, _ui, archive.getIndex(), strategy);
                        //List toFetch = archive.getIndex().selectURIsToPull(_client, _ui, strategy);
                        for (int j = 0; j < toFetch.size(); j++) {
                            SyndieURI uri = (SyndieURI)toFetch.get(j);
                            if (uris.add(uri)) {
                                fetch(archive.getName(), uri);
                            }
                        }
                    }
                }
                break;
            case PULL_STRATEGY_DELTA:
                for (int i = 0; i < _archives.size(); i++) {
                    NymArchive archive = (NymArchive)_archives.get(i);
                    if (!archiveNames.contains(archive.getName()))
                        continue;
                    if (archive.getIndex() != null) {
                        SharedArchiveEngine.PullStrategy strategy = new SharedArchiveEngine.PullStrategy();
                        strategy.includeDupForPIR = false;
                        strategy.includePBEMessages = true;
                        strategy.includePrivateMessages = true;
                        strategy.includeRecentMessagesOnly = false;
                        strategy.knownChannelsOnly = false;
                        SharedArchiveEngine engine = new SharedArchiveEngine();
                        List toFetch = engine.selectURIsToPull(_client, _ui, archive.getIndex(), strategy);
                        //List toFetch = archive.getIndex().selectURIsToPull(_client, _ui, strategy);
                        for (int j = 0; j < toFetch.size(); j++) {
                            SyndieURI uri = (SyndieURI)toFetch.get(j);
                            if (uris.add(uri)) {
                                fetch(archive.getName(), uri);
                            }
                        }
                    }
                }
                break;
            case PULL_STRATEGY_EXPLICIT:
                for (Iterator iter = explicit.entrySet().iterator(); iter.hasNext(); ) {
                    Map.Entry entry = (Map.Entry)iter.next();
                    String archive = (String)entry.getKey();
                    Set curURIs = (Set)entry.getValue();
                    for (Iterator uriIter = curURIs.iterator(); uriIter.hasNext(); )
                        fetch(archive, (SyndieURI)uriIter.next());
                }
                break;
        }
    }
    private void push(int maxkb, int pushStrategy, Set archiveNames) {
        if (pushStrategy == -1) return;
        for (Iterator iter = archiveNames.iterator(); iter.hasNext(); )
            push((String)iter.next(), getPushStrategy());
    }
    private void push(String archiveName, SharedArchiveEngine.PushStrategy strategy) {
        NymArchive archive = getArchive(archiveName);
        if (archive == null) return;
        String url = archive.getURI().getURL();

        _ui.debugMessage("push to " + archive.getName() + " @ " + url);

        int keyStart = -1;
        keyStart = url.indexOf("CHK@");
        if (keyStart == -1)
            keyStart = url.indexOf("SSK@");
        if (keyStart == -1)
            keyStart = url.indexOf("USK@");

        if (keyStart != -1) {
            String fcpHost = archive.getCustomProxyHost();
            int fcpPort = archive.getCustomProxyPort();
            if ( (fcpHost == null) || (fcpHost.length() <= 0) || (fcpPort <= 0) ) {
                fcpHost = _fcpHost;
                fcpPort = _fcpPort;
            }
            FreenetArchivePusher pusher = new FreenetArchivePusher(_ui, fcpHost, fcpPort);

            String pubSSK = getPublicSSK(archive.getURI(), url, keyStart);
            String privSSK = getPrivateSSK(archive, pubSSK);
            if (privSSK == null) {
                // we don't have the private key
                if ("CHK@".equals(pubSSK)) {
                    // post it under a CHK
                    // todo: eventually support this
                    _ui.debugMessage("post under a CHK isn't yet implemented");
                    return;
                } else if ("USK@".equals(pubSSK) || "SSK@".equals(pubSSK)) {
                    // create a new SSK keypair, save our privkey, update the archive, and post it
                    pusher.generateSSK();
                    String error = pusher.getError();
                    if ( (error != null) && (error.length() > 0) ) {
                        _ui.errorMessage("Cannot create a new SSK: "+ error);
                        return;
                    }
                    pubSSK = pusher.getPublicSSK();
                    privSSK = pusher.getPrivateSSK();
                    if ( (pubSSK == null) || (privSSK == null) ) {
                        _ui.errorMessage("Error creating a new SSK");
                        return;
                    }
                    String type = "USK@".equals(pubSSK) ? "USK@" : "SSK@";
                    _ui.debugMessage("new SSK keypair created w/ pub=" + pubSSK);
                    savePrivateSSK(archive, pubSSK, privSSK, type);
                } else {
                    // cannot post
                    _ui.debugMessage("cannot post to the SSK, as no private key is known for " + pubSSK);
                    return;
                }
            }

            _ui.debugMessage("scheduling fcp post under " + pubSSK);

            pusher.setPrivateSSK(privSSK);
            pusher.setPublicSSK(pubSSK);

            // add a record so the putArchive can occur in one of the worker threads,
            // even though it just contacts the fcp host & sends them the data without
            // waiting for the data to be fully inserted into the network
            //pusher.putArchive(_client.getArchiveDir());
            StatusRecord rec = new StatusRecord(archive.getName(), archive.getURI(), pusher);
            synchronized (_fetchRecords) {
                _fetchRecords.add(rec);
                _fetchRecords.notifyAll();
            }
        } else {
            HTTPSyndicator template = archive.getSyndicator();
            // the syndicator was built with sequential operation in mind, not multithreaded/reused,
            // so just make another copy for our current sequence
            if (template == null) {
                // no index fetched yet. noop
                return;
            }
            HTTPSyndicator syndicator = (HTTPSyndicator)template.clone();
            syndicator.setDeleteOutboundAfterSend(false);
            //syndicator.setPostPassphrase(archive.getURI().getString(""))

            syndicator.schedulePut(strategy);
            /*
            _ui.debugMessage("http push strategy: " + pushStrategy);
            switch (pushStrategy) {
                case PUSH_STRATEGY_DELTAKNOWN:
                    syndicator.schedulePut("archive", true);
                    break;
                case PUSH_STRATEGY_DELTA:
                    syndicator.schedulePut("archive", false);
                    break;
                default:
                    _ui.debugMessage("http push strategy unknown: " + pushStrategy);
            }
             */
            // add a record so the post() can occur in one of the worker threads
            StatusRecord rec = new StatusRecord(archive.getName(), archive.getURI(), syndicator);
            synchronized (_fetchRecords) {
                _fetchRecords.add(rec);
                _fetchRecords.notifyAll();
            }
        } // end if(freenet) { } else (http) {}
    }
    

    void push(String archiveName) { push(archiveName, getPushStrategy()); }
    void pull(String archiveName, Runnable onSuccess) {
        NymArchive archive = getArchive(archiveName);
        if ( (archive == null) || (archive.getIndex() == null) ) return;
        SharedArchiveEngine engine = new SharedArchiveEngine();
        List toPull = engine.selectURIsToPull(_client, _ui, archive.getIndex(), getPullStrategy());
        if (toPull.size() > 0) {
            if (onSuccess != null)
                _onPullSuccess.put(archiveName, onSuccess);
            for (int i = 0; i < toPull.size(); i++)
                fetch(archiveName, (SyndieURI)toPull.get(i));
        } else {
            if (onSuccess != null)
                JobRunner.instance().enqueue(onSuccess);
        }
    }
    
    private String getPublicSSK(SyndieURI uri, String url, int keyStart) {
        String readKeyB64 = uri.getString("readKeyData");
        if (readKeyB64 != null) {
            byte[] readKeyData = Base64.decode(readKeyB64);
            if (readKeyData != null) {
                String key = DataHelper.getUTF8(readKeyData);
                if (key != null)
                    return key;
            }
        }
        return url.substring(keyStart);
    }
    private String getPrivateSSK(NymArchive archive, String pubSSK) {
        String postKeyB64 = archive.getURI().getString("postKeyData");
        if (postKeyB64 != null) {
            byte[] postKeyData = Base64.decode(postKeyB64);
            if (postKeyData != null) {
                String key = DataHelper.getUTF8(postKeyData);
                if (key != null)
                    return key;
            }
        }
        Hash pubSSKHash = _client.sha256(DataHelper.getUTF8(pubSSK));
        List keys = _client.getNymKeys(pubSSKHash, Constants.KEY_FUNCTION_SSKPRIV);
        for (int i = 0; i < keys.size(); i++) {
            NymKey key = (NymKey)keys.get(i);
            byte priv[] = key.getData();
            if (priv != null) {
                int end = 0;
                for (int j = 0; j < priv.length; j++) {
                    if (priv[j] == (byte)0xFF) {
                        end = j;
                        break;
                    }
                }
                return DataHelper.getUTF8(priv, 0, end);
            }
        }
        return null;
    }
    private void savePrivateSSK(NymArchive archive, String pubSSK, String privSSK, String type) {
        SyndieURI uri = archive.getURI();
        Map attr = uri.getAttributes();
        attr.put("url", pubSSK);
        archive.update(uri);
        update(archive.getName(), uri, archive.getCustomProxyHost(), archive.getCustomProxyPort(), null, null);
        
        Hash pubSSKHash = _client.sha256(DataHelper.getUTF8(pubSSK));
        byte padded[] = new byte[128];
        byte key[] = DataHelper.getUTF8(privSSK);
        // the ssk is plain ascii (largely base64 encoded ascii, even), so pad it with a nonascii so we
        // can strip the padding afterwards (in getPrivateSSK above).  necessary since KeyImport
        // transparently AES256 encrypts the key against the current nym's passphrase
        Arrays.fill(key, (byte)0xFF);
        System.arraycopy(key, 0, padded, 0, key.length);
        KeyImport.importKey(_ui, _client, Constants.KEY_FUNCTION_SSKPRIV, pubSSKHash, padded, true);
    }
    
    public static final String SHARED_INDEX_FILE = "shared-index.dat";
    
    public static void buildIndex(DBClient client, UI ui) {
        SharedArchiveBuilder builder = new SharedArchiveBuilder(client, ui);
        builder.setHideLocalHours(6); // don't advertize things we created locally until at least 6h have passed
        builder.setShareBanned(true); // just because we have banned something doesn't mean other people need to know that
        builder.setShareDelayHours(12); // tell people that we only build our index once every 12 hours, so dont bug us too much
        builder.setShareReceivedOnly(false); // sometimes
        SharedArchive archive = builder.buildSharedArchive();
        FileOutputStream fos = null;
        try {
            fos = new FileOutputStream(new File(client.getArchiveDir(), SHARED_INDEX_FILE));
            archive.write(fos);
            fos.close();
            fos = null;
        } catch (IOException ioe) {
            ui.errorMessage("Error writing the shared index", ioe);
        } finally {
            if (fos != null) try { fos.close(); } catch (IOException ioe) {}
        }
        return;
        /*
        File archiveDir = client.getArchiveDir();
        ArchiveIndex index;
        try {
            // load the whole index into memory
            index = ArchiveIndex.buildIndex(client, ui, archiveDir, maxSize);
            // iterate across each channel, building their index-all and index-new files
            // as well as pushing data into the overall index-all, index-new, and index-meta files
            FileOutputStream outFullAll = new FileOutputStream(new File(archiveDir, "index-all.dat"));
            FileOutputStream outFullNew = new FileOutputStream(new File(archiveDir, "index-new.dat"));
            FileOutputStream outFullMeta = new FileOutputStream(new File(archiveDir, "index-meta.dat"));
            FileOutputStream outFullUnauth = new FileOutputStream(new File(archiveDir, "index-unauthorized.dat"));
            for (int i = 0; i < index.getChannelCount(); i++) {
                ArchiveChannel chan = index.getChannel(i);
                File chanDir = new File(archiveDir, Base64.encode(chan.getScope()));
                FileOutputStream outAll = new FileOutputStream(new File(chanDir, "index-all.dat"));
                FileOutputStream outNew = new FileOutputStream(new File(chanDir, "index-new.dat"));
                FileOutputStream outUnauth = new FileOutputStream(new File(chanDir, "index-unauthorized.dat"));
                write(outAll, chan, false);
                write(outNew, chan, true);
                write(outFullAll, chan, false);
                write(outFullNew, chan, true);
                write(outFullMeta, chan);
                writeUnauth(outUnauth, chan);
                writeUnauth(outFullUnauth, chan);
                outAll.close();
                outNew.close();
                outUnauth.close();
            }
            outFullMeta.close();
            outFullNew.close();
            outFullAll.close();
            outFullUnauth.close();
            ui.statusMessage("Index rebuilt");
        } catch (IOException ioe) {
            ui.errorMessage("Error building the index", ioe);
        }
         */
    }
    
    /** returns a new list containing the actual fetch records (which can be updated asynchronously) */
    public List getFetchRecords() {
        synchronized (_fetchRecords) {
            return new ArrayList(_fetchRecords);
        }
    }
    /** returns a new list containing the actual fetch records (which can be updated asynchronously) */
    public List getFetchRecords(int status) {
        synchronized (_fetchRecords) {
            ArrayList rv = new ArrayList();
            for (int i = 0; i < _fetchRecords.size(); i++) {
                StatusRecord rec = (StatusRecord)_fetchRecords.get(i);
                if (rec.getStatus() == status)
                    rv.add(rec);
            }
            return rv;
        }
    }
    
    public void removeFetchRecord(StatusRecord rec) {
        synchronized (_fetchRecords) {
            _fetchRecords.remove(rec);
            _fetchMetaRecords.remove(rec);
            _fetchRecords.notifyAll();
        }
    }
    
    private void fireFetchStatusUpdated(StatusRecord record) {
        for (int i = 0; i < _listeners.size(); i++) {
            SyndicationListener lsnr = (SyndicationListener)_listeners.get(i);
            lsnr.fetchStatusUpdated(this, record);
        }
        synchronized (_fetchRecords) { _fetchRecords.notifyAll(); }
    }
    
    private void fireSyndicationComplete() {
        for (int i = 0; i < _listeners.size(); i++) {
            SyndicationListener lsnr = (SyndicationListener)_listeners.get(i);
            lsnr.syndicationComplete(this);
        }
        synchronized (_fetchRecords) { _fetchRecords.notifyAll(); }
        
        List toRun = new ArrayList();
        synchronized (_onPullSuccess) {
            toRun.addAll(_onPullSuccess.values());
            _onPullSuccess.clear();
        }
        for (int i = 0; i < toRun.size(); i++)
            JobRunner.instance().enqueue((Runnable)toRun.get(i));
    }
    
    public void addListener(SyndicationListener lsnr) { if (!_listeners.contains(lsnr)) _listeners.add(lsnr); }
    public void removeListener(SyndicationListener lsnr) { _listeners.remove(lsnr); }
    
    private static final String SQL_ADD_NYM_ARCHIVE = "INSERT INTO nymArchive (name, uriId, customProxyHost, customProxyPort, lastSyncDate, postKey, postKeySalt, readKey, readKeySalt, nymId) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
    public boolean add(SyndieURI uri, String customProxyHost, int customProxyPort, SessionKey readKey, SessionKey postKey) {
        return add(uri.getString("name"), uri, customProxyHost, customProxyPort, readKey, postKey);
    }
    public boolean add(String name, SyndieURI uri, String customProxyHost, int customProxyPort, SessionKey readKey, SessionKey postKey) {
        if ( (name == null) || (name.trim().length() <= 0) ) {
            _ui.errorMessage("Name has to be specified");
            return false;
        }
        if (uri == null) {
            _ui.errorMessage("URI has to be specified");
            return false;
        }
        NymArchive existing = getArchive(name);
        if (existing == null) {
            long uriId = _client.addURI(uri);
                
            byte postKeyEncr[] = null;
            byte postKeySalt[] = null;

            if (postKey != null) {
                postKeySalt = new byte[32];
                postKeyEncr = _client.pbeEncrypt(postKey.getData(), postKeySalt);
                //_client.ctx().random().nextBytes(postKeySalt);
                //SessionKey key = _client.ctx().keyGenerator().generateSessionKey(postKeySalt, DataHelper.getUTF8(_client.getPass()));
                //_client.ctx().aes().encrypt(postKey.getData(), 0, postKeyEncr, 0, key, postKeySalt, SessionKey.KEYSIZE_BYTES);
            }

            byte readKeyEncr[] = null;
            byte readKeySalt[] = null;

            if (readKey != null) {
                readKeySalt = new byte[32];
                postKeyEncr = _client.pbeEncrypt(readKey.getData(), readKeySalt);
                //_client.ctx().random().nextBytes(readKeySalt);
                //SessionKey key = _client.ctx().keyGenerator().generateSessionKey(readKeySalt, DataHelper.getUTF8(_client.getPass()));
                //_client.ctx().aes().encrypt(readKey.getData(), 0, readKeyEncr, 0, key, readKeySalt, SessionKey.KEYSIZE_BYTES);
            }
            
            PreparedStatement stmt = null;
            try {
                stmt = _client.con().prepareStatement(SQL_ADD_NYM_ARCHIVE);
                // (name, uriId, customProxyHost, customProxyPort, 
                //  lastSyncDate, postKey, postKeySalt, readKey, 
                //  readKeySalt, nymId)
                stmt.setString(1, name);
                stmt.setLong(2, uriId);
                if ( (customProxyHost != null) && (customProxyHost.trim().length() > 0) && (customProxyPort > 0) ) {
                    stmt.setString(3, customProxyHost);
                    stmt.setInt(4, customProxyPort);
                } else {
                    stmt.setNull(3, Types.VARCHAR);
                    stmt.setNull(4, Types.INTEGER);
                }
                stmt.setNull(5, Types.DATE);
                
                if ( (postKeyEncr != null) && (postKeySalt != null) ) {
                    stmt.setBytes(6, postKeyEncr);
                    stmt.setBytes(7, postKeySalt);
                } else {
                    stmt.setNull(6, Types.VARBINARY);
                    stmt.setNull(7, Types.VARBINARY);
                }
                
                if ( (readKeyEncr != null) && (readKeySalt != null) ) {
                    stmt.setBytes(8, readKeyEncr);
                    stmt.setBytes(9, readKeySalt);
                } else {
                    stmt.setNull(8, Types.VARBINARY);
                    stmt.setNull(9, Types.VARBINARY);
                }
                
                stmt.setLong(10, _client.getLoggedInNymId());
                
                stmt.executeUpdate();
                
                stmt.close();
                stmt = null;

                _archives.add(new NymArchive(name, uri, customProxyHost, customProxyPort, -1, postKey, readKey, -1));
                
                for (int i = 0; i < _listeners.size(); i++) {
                    SyndicationListener lsnr = (SyndicationListener)_listeners.get(i);
                    lsnr.archiveAdded(this, name);
                }
                return true;
            } catch (SQLException se) {
                _ui.errorMessage("Error inserting nym archive", se);
                return false;
            } finally {
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }
        } else {
            _ui.errorMessage("Cannot add the archive - name is already in use");
            return false;
        }
    }
    
    private static final String SQL_DELETE_OLD_NYM_ARCHIVE_URI = "DELETE FROM uriAttribute WHERE uriId IN (SELECT uriId FROM nymArchive WHERE nymId = ? AND name = ?)";
    private static final String SQL_UPDATE_NYM_ARCHIVE = "UPDATE nymArchive SET name = ?, uriId = ?, customProxyHost = ?, customProxyPort = ?, postKey = ?, postKeySalt = ?, readKey = ?, readKeySalt = ? WHERE name = ? AND nymId = ?";
    public boolean update(String oldName, SyndieURI uri, String customProxyHost, int customProxyPort, SessionKey readKey, SessionKey postKey) {
        return update(oldName, uri.getString("name"), uri, customProxyHost, customProxyPort, readKey, postKey);
    }
    public boolean update(String oldName, String newName, SyndieURI uri, String customProxyHost, int customProxyPort, SessionKey readKey, SessionKey postKey) {
        if ( (oldName == null) || (oldName.trim().length() <= 0) || (newName == null) || (newName.trim().length() <= 0) ) {
            _ui.errorMessage("Name has to be specified");
            return false;
        }
        if (uri == null) {
            _ui.errorMessage("URI has to be specified");
            return false;
        }
        NymArchive existing = getArchive(oldName);
        if (existing == null)
            return add(newName, uri, customProxyHost, customProxyPort, readKey, postKey);
        
        PreparedStatement stmt = null;
        try {
            stmt = _client.con().prepareStatement(SQL_DELETE_OLD_NYM_ARCHIVE_URI);
            stmt.setLong(1, _client.getLoggedInNymId());
            stmt.setString(2, oldName);
            stmt.executeUpdate();

            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            _ui.errorMessage("Error deleting old nym archive uri", se);
            return false;
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }

        
        long uriId = _client.addURI(uri);
                
        byte postKeyEncr[] = null;
        byte postKeySalt[] = null;

        if (postKey != null) {
            postKeySalt = new byte[32];
            postKeyEncr = _client.pbeEncrypt(postKey.getData(), postKeySalt);
            //_client.ctx().random().nextBytes(postKeySalt);
            //SessionKey key = _client.ctx().keyGenerator().generateSessionKey(postKeySalt, DataHelper.getUTF8(_client.getPass()));
            //_client.ctx().aes().encrypt(postKey.getData(), 0, postKeyEncr, 0, key, postKeySalt, SessionKey.KEYSIZE_BYTES);
        }

        byte readKeyEncr[] = null;
        byte readKeySalt[] = null;

        if (readKey != null) {
            readKeySalt = new byte[32];
            readKeyEncr = _client.pbeEncrypt(readKey.getData(), readKeySalt);
            //_client.ctx().random().nextBytes(readKeySalt);
            //SessionKey key = _client.ctx().keyGenerator().generateSessionKey(readKeySalt, DataHelper.getUTF8(_client.getPass()));
            //_client.ctx().aes().encrypt(readKey.getData(), 0, readKeyEncr, 0, key, readKeySalt, SessionKey.KEYSIZE_BYTES);
        }

        try {
            stmt = _client.con().prepareStatement(SQL_UPDATE_NYM_ARCHIVE);
            // name = ?, uriId = ?, customProxyHost = ?, customProxyPort = ?, lastSyncDate = ?,
            // postKey = ?, postKeySalt = ?, readKey = ?, readKeySalt = ?
            // WHERE name = ? AND nymId = ?
            stmt.setString(1, newName);
            stmt.setLong(2, uriId);
            if ( (customProxyHost != null) && (customProxyHost.trim().length() > 0) && (customProxyPort > 0) ) {
                stmt.setString(3, customProxyHost);
                stmt.setInt(4, customProxyPort);
            } else {
                stmt.setNull(3, Types.VARCHAR);
                stmt.setNull(4, Types.INTEGER);
            }

            if ( (postKeyEncr != null) && (postKeySalt != null) ) {
                stmt.setBytes(5, postKeyEncr);
                stmt.setBytes(6, postKeySalt);
            } else {
                stmt.setNull(5, Types.VARBINARY);
                stmt.setNull(6, Types.VARBINARY);
            }

            if ( (readKeyEncr != null) && (readKeySalt != null) ) {
                stmt.setBytes(7, readKeyEncr);
                stmt.setBytes(8, readKeySalt);
            } else {
                stmt.setNull(7, Types.VARBINARY);
                stmt.setNull(8, Types.VARBINARY);
            }

            stmt.setString(9, oldName);
            stmt.setLong(10, _client.getLoggedInNymId());

            stmt.executeUpdate();

            stmt.close();
            stmt = null;

            NymArchive old = getArchive(oldName);
            old.update(newName, uri, customProxyHost, customProxyPort, postKey, readKey);

            for (int i = 0; i < _listeners.size(); i++) {
                SyndicationListener lsnr = (SyndicationListener)_listeners.get(i);
                lsnr.archiveUpdated(this, oldName, newName);
            }
            return true;
        } catch (SQLException se) {
            _ui.errorMessage("Error inserting nym archive", se);
            return false;
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    private static final String SQL_DELETE_NYM_ARCHIVE = "DELETE FROM nymArchive WHERE nymId = ? AND name = ?";
    public void delete(String name) {
        if ( (name == null) || (name.trim().length() <= 0) ) {
            _ui.errorMessage("Name has to be specified");
            return;
        }

        PreparedStatement stmt = null;
        try {
            stmt = _client.con().prepareStatement(SQL_DELETE_NYM_ARCHIVE);
            stmt.setLong(1, _client.getLoggedInNymId());
            stmt.setString(2, name);
            stmt.executeUpdate();

            stmt.close();
            stmt = null;
            
            _archives.remove(getArchive(name));
            
            for (int i = 0; i < _listeners.size(); i++) {
                SyndicationListener lsnr = (SyndicationListener)_listeners.get(i);
                lsnr.archiveRemoved(this, name);
            }
        } catch (SQLException se) {
            _ui.errorMessage("Error deleting nym archive", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private NymArchive getArchive(int index) { return (NymArchive)_archives.get(index); }
    private NymArchive getArchive(String name) { 
        for (int i = 0; i < _archives.size(); i++) {
            NymArchive archive = (NymArchive)_archives.get(i);
            if (name.equals(archive.getName()))
                return archive;
        }
        return null;
    }
    public int getArchiveNum(String name) {
        for (int i = 0; i < _archives.size(); i++) {
            NymArchive archive = (NymArchive)_archives.get(i);
            if (name.equals(archive.getName()))
                return i;
        }
        return -1;
    }

    public void setNextSync(String name, long when) {
        if ( (name == null) || (name.length() <= 0) ) return;
        NymArchive archive = getArchive(name);
        archive.setNextSyncDate(when);
        if (when > 0)
            scheduleSync(name, when);
        else
            cancelSync(name);
    }
    private static final String SQL_SYNC_SCHEDULE = "UPDATE nymArchive SET nextSyncDate = ? WHERE nymId = ? AND name = ?";
    private void scheduleSync(String name, long when) {
        PreparedStatement stmt = null;
        try {
            stmt = _client.con().prepareStatement(SQL_SYNC_SCHEDULE);
            stmt.setTimestamp(1, new Timestamp(when));
            stmt.setLong(2, _client.getLoggedInNymId());
            stmt.setString(3, name);
            stmt.executeUpdate();
            stmt.close();
            stmt = null;
            
            startFetching(1);
        } catch (SQLException se) {
            _ui.errorMessage("Error scheduling sync", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        _scheduler.scheduleUpdated();
    }
    
    private static final String SQL_SYNC_CANCEL = "UPDATE nymArchive SET nextSyncDate = NULL WHERE nymId = ? AND name = ?";
    private void cancelSync(String name) {
        PreparedStatement stmt = null;
        try {
            stmt = _client.con().prepareStatement(SQL_SYNC_CANCEL);
            stmt.setLong(1, _client.getLoggedInNymId());
            stmt.setString(2, name);
            stmt.executeUpdate();
            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            _ui.errorMessage("Error cancelling sync", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    public void stopFetching(SyndieURI uri) {
        synchronized (_fetchRecords) {
            for (int i = 0; i < _fetchRecords.size(); i++) {
                StatusRecord record = (StatusRecord)_fetchRecords.get(i);
                if (record.getURI().equals(uri)) {
                    record.stop();
                    break;
                }
            }
            _fetchRecords.notifyAll();
        }
    }
    
    private static final String SQL_GET_NYM_ARCHIVES = "SELECT name, uriId, customProxyHost, customProxyPort, lastSyncDate, postKey, postKeySalt, readKey, readKeySalt, nextSyncDate FROM nymArchive WHERE nymId = ? ORDER BY name";
    public void loadArchives() {
        if (_archivesLoaded) {
            _ui.debugMessage("not loading archives, as they are already loaded");
            return;
        }
        //buildIndex(_client, _ui);
        _archivesLoaded = true;
        
        _ui.debugMessage("Loading archives");
        _archives.clear();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _client.con().prepareStatement(SQL_GET_NYM_ARCHIVES);
            stmt.setLong(1, _client.getLoggedInNymId());
            rs = stmt.executeQuery();
            while (rs.next()) {
                // name, uriId, customProxyHost, customProxyPort, lastSyncDate, postKey, postKeySalt, 
                // readKey, readKeySalt
                String name = rs.getString(1);
                long uriId = rs.getLong(2);
                if (rs.wasNull()) {
                    _ui.errorMessage("no URI for name = " + name);
                    continue;
                }
                _ui.debugMessage("archive name=" + name + " uriId = " + uriId);
                String host = rs.getString(3);
                int port = rs.getInt(4);
                if (rs.wasNull()) {
                    host = null;
                    port = -1;
                }
                Date when = rs.getDate(5);
                byte[] postKeyEncr = rs.getBytes(6);
                byte[] postKeySalt = rs.getBytes(7);
                byte[] readKeyEncr = rs.getBytes(8);
                byte[] readKeySalt = rs.getBytes(9);
                Timestamp nextSync = rs.getTimestamp(10);
                
                byte[] postKey = null;
                if ( (postKeyEncr != null) && (postKeySalt != null) ) {
                    SessionKey key = _client.ctx().keyGenerator().generateSessionKey(postKeySalt, DataHelper.getUTF8(_client.getPass()));
                    postKey = new byte[SessionKey.KEYSIZE_BYTES];
                    _client.ctx().aes().decrypt(postKeyEncr, 0, postKey, 0, key, postKeySalt, postKeyEncr.length);
                }
                
                byte[] readKey = null;
                if ( (readKeyEncr != null) && (readKeySalt != null) ) {
                    SessionKey key = _client.ctx().keyGenerator().generateSessionKey(readKeySalt, DataHelper.getUTF8(_client.getPass()));
                    readKey = new byte[SessionKey.KEYSIZE_BYTES];
                    _client.ctx().aes().decrypt(readKeyEncr, 0, readKey, 0, key, readKeySalt, readKeyEncr.length);
                }
                
                SyndieURI uri = _client.getURI(uriId);
                if (uri == null) {
                    _ui.errorMessage("uri not found [id = " + uriId + ", name = " + name + "]");
                    continue;
                }
                
                _archives.add(new NymArchive(name, uri, host, port, (when == null ? -1l : when.getTime()), postKey, readKey, (nextSync == null ? -1L : nextSync.getTime())));
            }
            rs.close();
            rs = null;
            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            _ui.errorMessage("Error fetching nym archives", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        _ui.debugMessage("archives loaded");
        for (int i = 0; i < _listeners.size(); i++)
            ((SyndicationManager.SyndicationListener)_listeners.get(i)).archivesLoaded(this);
        
        _scheduler = new SyndicationManagerScheduler(_client, _ui, this);
        _scheduler.startScheduling();
    }
    
    private static class NymArchive {
        private String _name;
        private SyndieURI _uri;
        private String _customProxyHost;
        private int _customProxyPort;
        private long _lastSyncDate;
        private long _nextSyncDate;
        private SessionKey _postKey;
        private SessionKey _readKey;
        private SharedArchive _index;
        private HTTPSyndicator _syndicator;
        
        public NymArchive(String name, SyndieURI uri, String host, int port, long when, byte[] post, byte[] read, long nextSync) {
            this(name, uri, host, port, when, (post != null ? new SessionKey(post) : null), (read != null ? new SessionKey(read) : null), nextSync);
        }
        public NymArchive(String name, SyndieURI uri, String host, int port, long when, SessionKey post, SessionKey read, long nextSync) {
            _name = name;
            _uri = uri;
            _customProxyHost = host;
            _customProxyPort = port;
            _lastSyncDate = when;
            _nextSyncDate = nextSync;
            _postKey = post;
            _readKey = read;
        }

        public void update(String newName, SyndieURI uri, String customProxyHost, int customProxyPort, SessionKey postKey, SessionKey readKey) {
            update(newName, uri, customProxyHost, customProxyPort, _lastSyncDate, postKey, readKey);
        }
        public void update(String newName, SyndieURI uri, String customProxyHost, int customProxyPort, long when, SessionKey postKey, SessionKey readKey) {
            _name = newName;
            _uri = uri;
            _customProxyHost = customProxyHost;
            _customProxyPort = customProxyPort;
            _lastSyncDate = when;
            _postKey = postKey;
            _readKey = readKey;
        }
        public void update(SyndieURI uri) { _uri = uri; }
        
        public String getName() { return _name; }
        public SyndieURI getURI() { return _uri; }
        public String getCustomProxyHost() { return _customProxyHost; }
        public int getCustomProxyPort() { return _customProxyPort; }
        public long getLastSyncDate() { return _lastSyncDate; }
        public long getNextSyncDate() { return _nextSyncDate; }
        void setNextSyncDate(long when) { _nextSyncDate = when; }
        public SessionKey getReadKey() { return _readKey; }
        public SessionKey getPostKey() { return _postKey; }
        public SharedArchive getIndex() { return _index; }
        public void setIndex(SharedArchive index) { _index = index; }
        public HTTPSyndicator getSyndicator() { return _syndicator; }
        public void setSyndicator(HTTPSyndicator syndicator) { _syndicator = syndicator; }
    }

    public class StatusRecord {
        private String _archiveName;
        private SyndieURI _uri;
        private int _status;
        private String _detail;
        private long _timestamp;
        private FreenetArchivePusher _freenetPusher;
        private HTTPSyndicator _httpSyndicator;
        
        public StatusRecord(String name, SyndieURI uri) {
            _archiveName = name;
            _uri = uri;
            _status = FETCH_SCHEDULED;
            _timestamp = System.currentTimeMillis();
        }
        public StatusRecord(String name, SyndieURI uri, FreenetArchivePusher pusher) {
            _archiveName = name;
            _uri = uri;
            _freenetPusher = pusher;
            _status = PUSH_SCHEDULED;
            _timestamp = System.currentTimeMillis();
        }
        public StatusRecord(String name, SyndieURI uri, HTTPSyndicator syndicator) {
            _archiveName = name;
            _uri = uri;
            _freenetPusher = null;
            _httpSyndicator = syndicator;
            _status = PUSH_SCHEDULED;
            _timestamp = System.currentTimeMillis();
        }
        
        NymArchive getArchive() { return SyndicationManager.this.getArchive(_archiveName); }
        FreenetArchivePusher getFreenetPusher() { return _freenetPusher; }
        HTTPSyndicator getHTTPSyndicator() { return _httpSyndicator; }
        public SyndieURI getURI() { return _uri; }
        public int getStatus() { return _status; }
        public String getSource() { return _archiveName; }
        public long getEventTime() { return _timestamp; }
        public boolean isTerminal() {
            switch (_status) {
                case FETCH_FAILED:
                case FETCH_IMPORT_OK:
                case FETCH_IMPORT_PBE:
                case FETCH_IMPORT_NOKEY:
                case FETCH_IMPORT_CORRUPT:
                case FETCH_INDEX_DIFF_OK:
                case FETCH_INDEX_LOAD_ERROR:
                case FETCH_STOPPED:
                    return true;
                case FETCH_COMPLETE:
                case FETCH_SCHEDULED:
                case FETCH_STARTED:
                default:
                    return false;
            }
        }
        public void setStatus(int status) {
            if (!isTerminal())
                _status = status;
        }
        /** status message detail */
        public String getDetail() { return _detail; }
        public void setDetail(String detail) { _detail = detail; }
        void stop() { setStatus(FETCH_STOPPED); }
    }
    
    private class Fetcher implements Runnable {
        private int _id;
        public Fetcher(int id) { _id = id; }
        public void run() {
            // make the index building an explict (or scheduled) task
            //if (_id == 0)
            //    buildIndex(ArchiveIndex.DEFAULT_MAX_SIZE);
            StatusRecord cur = null;
            for (;;) {
                int nonterminalRemaining = 0;
                synchronized (_fetchRecords) {
                    // fetch all metadata records before fetching any other records
                    int nonterminalMeta = 0;
                    for (int i = 0; i < _fetchMetaRecords.size(); i++) {
                        StatusRecord rec = (StatusRecord)_fetchRecords.get(i);
                        if (rec.getStatus() == FETCH_SCHEDULED) {
                            rec.setStatus(FETCH_STARTED);
                            cur = rec;
                            nonterminalMeta++;
                            break;
                        }
                        if (!rec.isTerminal()) {
                            nonterminalRemaining++;
                            nonterminalMeta++;
                        }
                    }
                    if ( (cur == null) && (nonterminalMeta == 0) ) {
                        for (int i = 0; i < _fetchRecords.size(); i++) {
                            StatusRecord rec = (StatusRecord)_fetchRecords.get(i);
                            if (rec.getStatus() == FETCH_SCHEDULED) {
                                rec.setStatus(FETCH_STARTED);
                                cur = rec;
                                break;
                            } else if (rec.getStatus() == PUSH_SCHEDULED) {
                                rec.setStatus(PUSH_STARTED);
                                cur = rec;
                                break;
                            }
                            if (!rec.isTerminal())
                                nonterminalRemaining++;
                        }
                    }
                    if (cur == null) {
                        try {
                            _fetchRecords.wait();
                        } catch (InterruptedException ie) {}
                    }
                }
                if (cur != null) {
                    fetch(cur);
                }
                if (nonterminalRemaining == 0) {
                    _ui.debugMessage("All of the records are terminal");
                    //buildIndex(ArchiveIndex.DEFAULT_MAX_SIZE);
                    fireSyndicationComplete();
                }
                cur = null;
            }
        }
        private void fetch(StatusRecord rec) {
            fireFetchStatusUpdated(rec); // scheduled-->start
            if (rec.getStatus() == FETCH_STOPPED) return;
            if (rec.getFreenetPusher() != null) {
                pushFreenet(rec);
            } else if (rec.getHTTPSyndicator() != null) {
                pushHTTP(rec);
            } else {
                if (rec.getURI().isArchive()) {
                    // fetching an archive's index itself
                    fetchIndex(rec);
                } else {
                    fetchData(rec);
                }
            }
        }
        private void pushFreenet(StatusRecord rec) {
            FreenetArchivePusher pusher = rec.getFreenetPusher();
            rec.setStatus(PUSH_STARTED);
            rec.setDetail(pusher.getPublicTarget());
            fireFetchStatusUpdated(rec);
            // this just contacts the fcp host & sends them the data without
            // waiting for the data to be fully inserted into the network
            pusher.putArchive(_client.getArchiveDir());
            String error = pusher.getError();
            if ( (error != null) && (error.length() > 0) ) {
                rec.setStatus(PUSH_ERROR);
                rec.setDetail(error);
            } else {
                rec.setStatus(PUSH_SENT);
                rec.setDetail(pusher.getPublicTarget());
            }
            fireFetchStatusUpdated(rec);
        }
        private void pushHTTP(StatusRecord rec) {
            rec.setStatus(PUSH_STARTED);
            fireFetchStatusUpdated(rec);
            HTTPSyndicator syndicator = rec.getHTTPSyndicator();
            syndicator.post();
            String error = syndicator.getError();
            if ( (error != null) && (error.length() > 0) ) {
                rec.setStatus(PUSH_ERROR);
                rec.setDetail(error);
            } else {
                rec.setStatus(PUSH_SENT);
                rec.setDetail("");
            }
            fireFetchStatusUpdated(rec);
        }
        private void fetchData(StatusRecord rec) {
            // fetching *FROM* an archive
            NymArchive archive = rec.getArchive();
            HTTPSyndicator template = archive.getSyndicator();
            if (template == null) {
                rec.setStatus(FETCH_FAILED);
                rec.setDetail("fetch index first");
                synchronized (_onPullSuccess) { _onPullSuccess.remove(rec.getSource()); }
                fireFetchStatusUpdated(rec);
                return;
            }
            // the syndicator was built with sequential operation in mind, not multithreaded/reused,
            // so just make another copy for our current sequence
            HTTPSyndicator syndicator = (HTTPSyndicator)template.clone();
            ArrayList uris = new ArrayList(1);
            uris.add(rec.getURI());
            if (rec.getStatus() == FETCH_STOPPED) {
                synchronized (_onPullSuccess) { _onPullSuccess.remove(rec.getSource()); }
                return;
            }
            boolean fetchComplete = syndicator.fetch(uris);
            if (rec.getStatus() == FETCH_STOPPED) {
                synchronized (_onPullSuccess) { _onPullSuccess.remove(rec.getSource()); }
                return;
            }
            if (fetchComplete) {
                rec.setStatus(FETCH_COMPLETE);
            } else {
                synchronized (_onPullSuccess) { _onPullSuccess.remove(rec.getSource()); }
                rec.setStatus(FETCH_FAILED);
                rec.setDetail(syndicator.getError());
            }
            fireFetchStatusUpdated(rec);

            if (!fetchComplete) {
                synchronized (_onPullSuccess) { _onPullSuccess.remove(rec.getSource()); }
                return;
            }

            if (rec.getStatus() == FETCH_STOPPED) {
                synchronized (_onPullSuccess) { _onPullSuccess.remove(rec.getSource()); }
                return;
            }
            int importCount = syndicator.importFetched();
            if (importCount == 1) {
                if (rec.getStatus() == FETCH_STOPPED) {
                    synchronized (_onPullSuccess) { _onPullSuccess.remove(rec.getSource()); }
                    return;
                }
                if (syndicator.countMissingKeys() >= 1) {
                    rec.setStatus(FETCH_IMPORT_NOKEY);
                } else {
                    rec.setStatus(FETCH_IMPORT_OK);
                }
            } else if (syndicator.countMissingPassphrases() == 1) {
                rec.setDetail(syndicator.getMissingPrompt(0));
                rec.setStatus(FETCH_IMPORT_PBE);
            } else {
                if (rec.getStatus() == FETCH_STOPPED) {
                    synchronized (_onPullSuccess) { _onPullSuccess.remove(rec.getSource()); }
                    return;
                }
                rec.setStatus(FETCH_IMPORT_CORRUPT);
            }
            fireFetchStatusUpdated(rec);
        }
    }
    
    private class UIStatusListener implements EepGet.StatusListener {
        private String _error;
        
        public String getError() { return _error; }
        public void bytesTransferred(long alreadyTransferred, int currentWrite, long bytesTransferred, long bytesRemaining, String url) {
            _ui.debugMessage("Transferred: " + bytesTransferred);
        }
        public void transferComplete(long alreadyTransferred, long bytesTransferred, long bytesRemaining, String url, String outputFile, boolean notModified) {
            _ui.debugMessage("Transfer complete: " + bytesTransferred);
        }
        public void attemptFailed(String url, long bytesTransferred, long bytesRemaining, int currentAttempt, int numRetries, Exception cause) {
            _ui.debugMessage("Transfer attempt failed: " + bytesTransferred, cause);
            if (cause != null)
                _error = cause.getMessage();
        }
        public void transferFailed(String url, long bytesTransferred, long bytesRemaining, int currentAttempt)  {
            _ui.debugMessage("Transfer totally failed of " + url);
        }
        public void headerReceived(String url, int currentAttempt, String key, String val)  {
            _ui.debugMessage("Header received: " + key + "=" + val);
        }
        public void attempting(String url) {
            _ui.debugMessage("Fetching " + url + "...");
        }
    }
}
