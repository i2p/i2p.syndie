package syndie.db;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import syndie.data.SyndieURI;

/**
 *
 */
public class SyncManager {
    private static DBClient _client;
    private static UI _ui;
    private static SyncManager _instance = new SyncManager();
    public static SyncManager getInstance(DBClient client, UI ui) {
        _client = client;
        _ui = ui;
        _instance.loadArchives();
        return _instance; 
    }
    private List _archives;
    private boolean _archivesLoaded;
    private List _listeners;
    private boolean _online;
    private SharedArchiveEngine.PullStrategy _defaultPullStrategy;
    private SharedArchiveEngine.PushStrategy _defaultPushStrategy;
    
    private IndexFetcher _indexFetcher;
    private SyncInboundFetcher _inboundFetcher;
    private SyncOutboundPusher _outboundPusher;
    
    private SyncManager() {
        _archives = new ArrayList();
        _archivesLoaded = false;
        _online = false;
        _listeners = new ArrayList();
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
        for (int i = 0; i < _listeners.size(); i++) {
            SyncListener lsnr = (SyncListener)_listeners.get(i);
            lsnr.archiveRemoved(archive);
        }
    }
    void added(SyncArchive archive) {
        for (int i = 0; i < _listeners.size(); i++) {
            SyncListener lsnr = (SyncListener)_listeners.get(i);
            lsnr.archiveAdded(archive);
        }
    }
    
    void wakeUpEngine() {
        if (_indexFetcher != null) _indexFetcher.wakeUp();
        if (_inboundFetcher != null) _inboundFetcher.wakeUp();
        if (_outboundPusher != null) _outboundPusher.wakeUp();
    }
    
    public long getNextSyncDate() {
        long earliest = -1;
        for (int i = 0; i < getArchiveCount(); i++) {
            SyncArchive archive = getArchive(i);
            long pull = archive.getNextPullTime();
            long push = archive.getNextPushTime();
            long when = -1;
            if (pull <= 0) when = push;
            else if (push <= 0) when = pull;
            else when = Math.min(pull, push);
            
            if (earliest <= 0) earliest = when;
            else if (when > 0) earliest = Math.min(earliest, when);
        }
        return earliest;
    }
    
    public boolean isOnline() { return _online; }
    public void setIsOnline(boolean online) { 
        _online = online; 
        storeOnlineStatus(); 
        for (int i = 0; i < _listeners.size(); i++)
            ((SyncListener)_listeners.get(i)).onlineStatusUpdated(_online);
        for (int i = 0; i < _archives.size(); i++)
            getArchive(i).fireUpdated();
        
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
        _online = Boolean.valueOf(val).booleanValue();
    }
    
    public void addListener(SyncListener lsnr) { if (!_listeners.contains(lsnr)) _listeners.add(lsnr); }
    public void removeListener(SyncListener lsnr) { _listeners.remove(lsnr); }
    
    public int getArchiveCount() { return _archives.size(); }
    public SyncArchive getArchive(int idx) { return (SyncArchive)_archives.get(idx); }
    
    public SharedArchiveEngine.PullStrategy getDefaultPullStrategy() { return _defaultPullStrategy; }
    public SharedArchiveEngine.PushStrategy getDefaultPushStrategy() { return _defaultPushStrategy; }
    public void setDefaultPullStrategy(SharedArchiveEngine.PullStrategy strategy) { _defaultPullStrategy = strategy; saveDefaultStrategies(); }
    public void setDefaultPushStrategy(SharedArchiveEngine.PushStrategy strategy) { _defaultPushStrategy = strategy; saveDefaultStrategies(); }
    
    private void loadDefaultStrategies() {
       if (!_client.isLoggedIn()) return;
        Properties prefs = _client.getNymPrefs();
        String strat = prefs.getProperty("syndicate.pullStrategy");
        SharedArchiveEngine.PullStrategy pull = new SharedArchiveEngine.PullStrategy(strat);
        _ui.debugMessage("db pull strategy: " + pull);
        _defaultPullStrategy = pull;
        
        strat = prefs.getProperty("syndicate.pushStrategy");
        SharedArchiveEngine.PushStrategy push = new SharedArchiveEngine.PushStrategy(strat);
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
        List names = _client.getNymArchiveNames();
        for (int i = 0; i < names.size(); i++) {
            String name = (String)names.get(i);
            SyncArchive archive = new SyncArchive(this, _client, name);
            _archives.add(archive);
            for (int j = 0; j < _listeners.size(); j++)
                ((SyncListener)_listeners.get(j)).archiveLoaded(archive);
        }
        for (int i = 0; i < _listeners.size(); i++)
            ((SyncListener)_listeners.get(i)).onlineStatusUpdated(_online);
        
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
        SharedArchiveEngine.PullStrategy pullStrategy = null;
        SharedArchiveEngine.PushStrategy pushStrategy = null;
        
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
