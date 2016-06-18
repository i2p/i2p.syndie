package syndie.db;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import net.i2p.util.SSLEepGet.SSLState;

import syndie.Constants;
import syndie.data.SyndieURI;

/**
 *  Control the archives, the pull and push strategies, the fetcher and pusher.
 */
public class SyncManager {
    private static DBClient _client;
    private static UI _ui;

    private static SyncManager _instance = new SyncManager();

    public static SyncManager getInstance(DBClient client, UI ui) {
        _client = client;
        _ui = ui;
        synchronized (SyncManager.class) {
            if (_instance == null) _instance = new SyncManager();
        }
        _instance.loadArchives();
        return _instance; 
    }
    private final List<SyncArchive> _archives;
    private volatile boolean _archivesLoaded;
    private final List<SyncListener> _listeners;
    private volatile boolean _online;
    private PullStrategy _defaultPullStrategy;
    private PushStrategy _defaultPushStrategy;
    
    private IndexFetcher _indexFetcher;
    private SyncInboundFetcher _inboundFetcher;
    private SyncOutboundPusher _outboundPusher;
    
    private SSLState _sslState;
    private final Object _sslStateLock = new Object();

    private SyncManager() {
        _archives = new ArrayList<SyncArchive>();
        _listeners = new ArrayList<SyncListener>();
    }
    
    public static void unloadAll() {
        SyncManager instance = null;
        synchronized (SyncManager.class) {
            instance = _instance;
            _instance = null;
        }
        if (instance != null) {
            instance._online = false;
            synchronized(instance._archives) {
                instance._archives.clear();
            }
            synchronized(instance._listeners) {
                instance._listeners.clear();
            }
            instance._archivesLoaded = false;
            instance._defaultPullStrategy = null;
            instance._defaultPushStrategy = null;
            if (instance._inboundFetcher != null)
                instance._inboundFetcher.kill();
            instance._inboundFetcher = null;
            if (instance._indexFetcher != null)
                instance._indexFetcher.kill();
            instance._indexFetcher = null;
            if (instance._outboundPusher != null)
                instance._outboundPusher.kill();
            instance._outboundPusher = null;
        }
        if (_ui != null)
            _ui.debugMessage("syncManager instance unloaded");
        _client = null;
        _ui = null;
    }
    
    public interface SyncListener {
        public void archiveAdded(SyncArchive archive);
        public void archiveRemoved(SyncArchive archive);
        public void archiveLoaded(SyncArchive archive);
        public void onlineStatusUpdated(boolean nowOnline);
    }
    
    UI getUI() { return _ui; }
    DBClient getClient() { return _client; }
    
    void deleted(SyncArchive archive) {
        synchronized(_listeners) {
            for (int i = 0; i < _listeners.size(); i++) {
                SyncListener lsnr = _listeners.get(i);
                lsnr.archiveRemoved(archive);
            }
        }
        synchronized(_archives) {
            _archives.remove(archive);
        }
    }

    void added(SyncArchive archive) {
        synchronized(_archives) {
            if (!_archives.contains(archive)) {
                _ui.debugMessage("Adding new archive " + archive  /*, new Exception() */ );
                _archives.add(archive);
            } else {
                archive = null;
            }
        }
        if (archive != null) {
            synchronized(_listeners) {
                for (int i = 0; i < _listeners.size(); i++) {
                    SyncListener lsnr = _listeners.get(i);
                    lsnr.archiveAdded(archive);
                }
            }
        }
    }
    
    void wakeUpEngine() {
        //_ui.debugMessage("wakeup sync engine");
        if (_indexFetcher != null) _indexFetcher.wakeUp();
        if (_inboundFetcher != null) _inboundFetcher.wakeUp();
        if (_outboundPusher != null) _outboundPusher.wakeUp();
    }
    
    public long getNextSyncDate() {
        long earliest = -1;
        synchronized(_archives) {
            for (SyncArchive archive : _archives) {
                long when = archive.getNextSyncTime();
                if (earliest <= 0) earliest = when;
                else if (when > 0) earliest = Math.min(earliest, when);
            }
        }
        return earliest;
    }
    
    public boolean isOnline() { return _online; }

    public void setIsOnline(boolean online) { 
        _online = online; 
        storeOnlineStatus(); 
        synchronized(_listeners) {
            for (int i = 0; i < _listeners.size(); i++)
                _listeners.get(i).onlineStatusUpdated(_online);
        }        
        synchronized(_archives) {
            for (int i = 0; i < _archives.size(); i++)
                getArchive(i).fireUpdated();
        }        
        wakeUpEngine();
    }
    private void storeOnlineStatus() {
        Properties prefs = _client.getNymPrefs();
        prefs.setProperty("syndication.online", _online ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
        _client.setNymPrefs(prefs);
    }
    private void loadOnlineStatus() {
        Properties prefs = _client.getNymPrefs();
        String val = prefs.getProperty("syndication.online", "true");
        _online = Boolean.parseBoolean(val);
    }
    
    /** @since 1.105b */
    SSLState getSSLState() {
        synchronized(_sslStateLock) {
            return _sslState;
        }
    }
    
    /** @since 1.105b */
    void setSSLState(SSLState state) {
        synchronized(_sslStateLock) {
            _sslState = state;
        }
    }

    public void addListener(SyncListener lsnr) {
        synchronized(_listeners) {
            if (!_listeners.contains(lsnr)) _listeners.add(lsnr);
        }
    }

    public void removeListener(SyncListener lsnr, SyncArchive.SyncArchiveListener alsnr) { 
        synchronized(_listeners) {
            _listeners.remove(lsnr);
        }
        synchronized(_archives) {
            for (int i = 0; i < _archives.size(); i++) {
                SyncArchive archive = _archives.get(i);
                archive.removeListener(alsnr);
            }
        }
    }
    
    /**
     *  @deprecated racy use getArchives()
     */
    @Deprecated
    public int getArchiveCount() { return _archives.size(); }

    /**
     *  @deprecated racy use getArchives()
     */
    @Deprecated
    public SyncArchive getArchive(int idx) { return _archives.get(idx); }
    
    /**
     *  Thread-safe replacement for getArchiveCount() / getArchive()
     *  @return a copy
     *  @since 1.106-2
     */
    public List<SyncArchive> getArchives() {
        synchronized(_archives) {
            return new ArrayList<SyncArchive>(_archives);
        }
    }
    
    public PullStrategy getDefaultPullStrategy() { return _defaultPullStrategy; }
    public PushStrategy getDefaultPushStrategy() { return _defaultPushStrategy; }
    public void setDefaultPullStrategy(PullStrategy strategy) { _defaultPullStrategy = strategy; saveDefaultStrategies(); }
    public void setDefaultPushStrategy(PushStrategy strategy) { _defaultPushStrategy = strategy; saveDefaultStrategies(); }
    
    private void loadDefaultStrategies() {
       if (!_client.isLoggedIn()) return;
        Properties prefs = _client.getNymPrefs();
        String strat = prefs.getProperty("syndicate.pullStrategy");
        PullStrategy pull = new PullStrategy(strat);
        _ui.debugMessage("db pull strategy: " + pull);
        _defaultPullStrategy = pull;
        
        strat = prefs.getProperty("syndicate.pushStrategy");
        PushStrategy push = new PushStrategy(strat);
        _ui.debugMessage("db push strategy: " + push);
        _defaultPushStrategy = push;
    }

    private void saveDefaultStrategies() {
        Properties prefs = _client.getNymPrefs();
        if (_defaultPushStrategy != null)
            prefs.setProperty("syndicate.pushStrategy", _defaultPushStrategy.serialize());
        else
            prefs.remove("syndicate.pushStrategy");
        
        if (_defaultPullStrategy != null)
            prefs.setProperty("syndicate.pullStrategy", _defaultPullStrategy.serialize());
        else
            prefs.remove("syndicate.pullStrategy");

        _client.setNymPrefs(prefs);
    }
    
    public synchronized void loadArchives() {
        if (_archivesLoaded) return;
        if (!_client.isLoggedIn()) return;
        loadOnlineStatus();
        loadDefaultStrategies();
        // .. load 'em up
        List<String> names = _client.getNymArchiveNames();
        if (names.size() == 0) {
            names = _client.getNymArchiveNames();
        }
        for (int i = 0; i < names.size(); i++) {
            String name = names.get(i);
            try {
                SyncArchive archive = new SyncArchive(this, _client, name);
                synchronized(_archives) {
                    _archives.add(archive);
                }
                synchronized(_listeners) {
                    for (int j = 0; j < _listeners.size(); j++)
                        _listeners.get(j).archiveLoaded(archive);
                }
            } catch (IllegalStateException ise) {
                _ui.errorMessage("Internal error loading the archive [" + name + "]", ise);
            }
        }
        synchronized(_listeners) {
            for (int i = 0; i < _listeners.size(); i++)
                _listeners.get(i).onlineStatusUpdated(_online);
        }
        
        _indexFetcher = new IndexFetcher(this);
        _indexFetcher.start();
        
        _inboundFetcher = new SyncInboundFetcher(this);
        _inboundFetcher.start();
        
        _outboundPusher = new SyncOutboundPusher(this);
        _outboundPusher.start();
        /*
        String name = "foo";
        String url = "http://blah";
        long lastPull = -1;
        long lastPush = -1;
        long nextPull = -1;
        long nextPush = System.currentTimeMillis() + 60*1000*5;
        String readKey = null;
        String postKey = null;
        PullStrategy pullStrategy = null;
        PushStrategy pushStrategy = null;
        
        SyncArchive archive = new SyncArchive(_client, name);
        archive.setConsecutiveFailures(0);
        archive.setFCPHost("localhost");
        archive.setFCPPort(8888);
        archive.setHTTPProxyHost("localhost");
        archive.setHTTPProxyPort(4444);
        archive.setIndexFetchInProgress(false);
        archive.setLastIndexFetchError(null);
        archive.setLastIndexFetchErrorMsg(null);
        archive.setLastPullTime(lastPull);
        archive.setLastPushTime(lastPush);
        archive.setName(name);
        archive.setNextPullOneOff(false);
        archive.setNextPullTime(nextPull);
        archive.setNextPushOneOff(false);
        archive.setNextPushTime(nextPush);
        archive.setPostKey(postKey);
        archive.setPullStrategy(pullStrategy);
        archive.setPushStrategy(pushStrategy);
        archive.setReadKey(readKey);
        archive.setURL(url);
        
        try {
            for (int i = 0; i < 20; i++) {
                SyndieURI uri = new SyndieURI("urn:syndie:channel:d7:channel44:fh6uCZvkXtmTVBqr4HF0pCfJITUUw-NG37sD1IBzaLc=9:messageIdi1167553527739e4:randi" + i + "ee");
                archive.createIncomingAction(uri);
            }
            for (int i = 0; i < 20; i++) {
                SyndieURI uri = new SyndieURI("urn:syndie:channel:d7:channel44:fh6uCZvkXtmTVBqr4HF0pCfJITUUw-NG37sD1IBzaLc=9:messageIdi1167553527739e4:randi" + i + "ee");
                archive.createOutgoingAction(uri);
            }
        } catch (URISyntaxException use) {}
        
        _archives.add(archive);
         */
        
        _archivesLoaded = true;
    }
}
