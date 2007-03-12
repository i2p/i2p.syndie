package syndie.db;

import java.io.File;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayList;
import java.util.List;
import net.i2p.data.Base64;
import net.i2p.data.DataHelper;
import syndie.Constants;
import syndie.data.SyndieURI;

/**
 *
 */
public class SyncArchive {
    private SyncManager _manager;
    private DBClient _client;
    private String _name;
    private String _oldName;
    private String _archiveURL;
    private String _postKey;
    private String _readKey;
    private String _httpProxyHost;
    private int _httpProxyPort;
    private String _fcpHost;
    private int _fcpPort;
    private int _consecutiveFailures;
    private long _nextSyncTime;
    private long _lastSyncTime;
    private long _uriId;
    private boolean _nextSyncOneOff;
    private List _incomingActions;
    private List _outgoingActions;
    private List _listeners;
    private SharedArchiveEngine.PullStrategy _pullStrategy;
    private SharedArchiveEngine.PushStrategy _pushStrategy;
    
    private boolean _indexFetching;
    private boolean _indexFetchComplete;
    private String _indexFetchErrorMsg;
    private Exception _indexFetchError;
    
    public SyncArchive(SyncManager mgr, DBClient client) { this(mgr, client, null); }
    public SyncArchive(SyncManager mgr, DBClient client, String name) {
        _manager = mgr;
        _client = client;
        _name = name;
        _oldName = name;
        _uriId = -1;
        _lastSyncTime = -1;
        _nextSyncTime = -1;
        _nextSyncOneOff = false;
        _consecutiveFailures = 0;
        _incomingActions = new ArrayList();
        _outgoingActions = new ArrayList();
        _listeners = new ArrayList();
        load();
    }
    
    /**
     * this creates the actions to be run as a result of fetching the specified archive index
     */
    public void indexFetched(UI ui, SharedArchive archive) {
        if (_nextSyncTime <= 0) {
            ui.debugMessage("cancel during fetch, so even though we got the index, don't build actions");
            setIndexFetchInProgress(false);
            setConsecutiveFailures(0);
            fireUpdated();
            return;
        }
        SharedArchiveEngine.PullStrategy pullStrategy = getPullStrategy();
        if (pullStrategy == null)
            pullStrategy = SyncManager.getInstance(_client, ui).getDefaultPullStrategy();
        ui.debugMessage("index fetched, pull strategy: " + pullStrategy);
        List pullURIs = new SharedArchiveEngine().selectURIsToPull(_client, ui, archive, pullStrategy);
        ui.debugMessage("index fetched, uris to push: " + pullURIs);
        
        SharedArchiveEngine.PushStrategy pushStrategy = getPushStrategy();
        if (pushStrategy == null)
            pushStrategy = SyncManager.getInstance(_client, ui).getDefaultPushStrategy();
        ui.debugMessage("index fetched, push strategy: " + pushStrategy);
        List pushURIs = new SharedArchiveEngine().selectURIsToPush(_client, ui, archive, pushStrategy);
        ui.debugMessage("index fetched, uris to push: " + pushURIs);
        
        SharedArchive.Message msgs[] = archive.getMessages();
        SharedArchive.Channel scopes[] = archive.getChannels();
        for (int i = 0; i < pullURIs.size(); i++) {
            SyndieURI uri = (SyndieURI)pullURIs.get(i);
            IncomingAction action = createIncomingAction(uri);
            int size = 0;
            if (uri.getMessageId() != null) {
                for (int j = 0; j < msgs.length; j++) {
                    if (msgs[j].getMessageId() == uri.getMessageId().longValue()) {
                        if (DataHelper.eq(uri.getScope().getData(), scopes[msgs[j].getScopeIndex()].getScope())) {
                            size = (int)msgs[j].getMaxSizeKB()*1024;
                            break;
                        }
                    }
                }
            }
            // metadata sizes aren't counted atm
            action.setSize(size);
        }
        
        for (int i = 0; i < pushURIs.size(); i++) {
            SyndieURI uri = (SyndieURI)pushURIs.get(i);
            OutgoingAction action = createOutgoingAction(uri);
            File src = null;
            if (uri.getMessageId() == null)
                src = new File(new File(_client.getArchiveDir(), uri.getScope().toBase64()), "meta" + Constants.FILENAME_SUFFIX);
            else
                src = new File(new File(_client.getArchiveDir(), uri.getScope().toBase64()), uri.getMessageId().toString() + Constants.FILENAME_SUFFIX);
            action.setSize((int)src.length());
        }
        
        ui.debugMessage("actions created, notify " + _listeners);
        
        for (int i = 0; i < _listeners.size(); i++) {
            SyncArchiveListener lsnr = (SyncArchiveListener)_listeners.get(i);
            lsnr.incomingUpdated(_incomingActions);
            for (int j = 0; j < _outgoingActions.size(); j++) {
                OutgoingAction action = (OutgoingAction)_outgoingActions.get(j);
                lsnr.outgoingUpdated(action);
            }
        }
        
        setIndexFetchInProgress(false);
        setConsecutiveFailures(0);
        setLastIndexFetchErrorMsg(null);
        setLastIndexFetchError(null);
        
        if ( (pushURIs.size() <= 0) && (pullURIs.size() <= 0) ) {
            // nothing to do, so reschedule the next sync
            updateSchedule(true);
        }
        
        ui.debugMessage("index fetch complete, notify " + _listeners);
        fireUpdated(false);
    }
    
    void fireUpdated() { fireUpdated(true); }
    void fireUpdated(boolean notifyActions) {
        for (int i = 0; i < _listeners.size(); i++) {
            SyncArchiveListener lsnr = (SyncArchiveListener)_listeners.get(i);
            lsnr.archiveUpdated(this);
        }
        if (notifyActions) {
            for (int i = 0; i < _incomingActions.size(); i++) {
                IncomingAction action = (IncomingAction)_incomingActions.get(i);
                notifyUpdate(action);
            }
            for (int i = 0; i < _outgoingActions.size(); i++) {
                OutgoingAction action = (OutgoingAction)_outgoingActions.get(i);
                notifyUpdate(action);
            }
        }
        _manager.wakeUpEngine();
    }
    
    public interface SyncArchiveListener {
        public void incomingUpdated(IncomingAction action);
        public void incomingUpdated(List actions);
        public void outgoingUpdated(OutgoingAction action);
        public void archiveUpdated(SyncArchive archive);
    }
    
    /** represents the pull of an archive element - either a message or metadata */
    public class IncomingAction {
        private SyndieURI _uri;
        private long _completionTime;
        private boolean _paused;
        private boolean _executing;
        private boolean _fetchOK;
        private boolean _importOK;
        private String _pbePrompt;
        private boolean _noReplyKey;
        private boolean _noReadKey;
        private String _fetchErrorMsg;
        private Exception _fetchError;
        private boolean _corrupt;
        private int _attempts;
        private int _size;
        private boolean _disposed;
        
        public IncomingAction(SyndieURI uri) {
            _uri = uri;
            _completionTime = -1;
            _paused = false;
            _executing = false;
            _fetchOK = false;
            _importOK = false;
            _pbePrompt = null;
            _noReplyKey = false;
            _noReadKey = false;
            _fetchErrorMsg = null;
            _fetchError = null;
            _corrupt = false;
            _attempts = 0;
            _size = 0;
            _disposed = false;
        }
        
        public SyndieURI getURI() { return _uri; }
        public SyncArchive getArchive() { return SyncArchive.this; }
        public boolean isScheduled() { return _completionTime == -1 && !_paused && !_executing; }
        public boolean isExecuting() { return _executing; }
        public boolean isComplete() { return _completionTime > 0; }
        public boolean isPaused() { return _paused; }
        public boolean isDisposed() { return _disposed; }
        public long getCompletionTime() { return _completionTime; }
        public String getPBEPrompt() { return _pbePrompt; }
        public boolean isReplyKeyUnknown() { return _noReplyKey; }
        public boolean isReadKeyUnknown() { return _noReadKey; }
        public String getFetchErrorMsg() { return _fetchErrorMsg; }
        public Exception getFetchError() { return _fetchError; }
        public boolean isCorrupt() { return _corrupt; }
        public int getFetchAttempts() { return _attempts; }
        public int getSize() { return _size; }
        
        void setSize(int bytes) { _size = bytes; }
        
        void importFailed(String msg, Exception cause) {
            _completionTime = System.currentTimeMillis();
            _fetchError = cause;
            _fetchErrorMsg = (msg != null ? msg : cause + "");
            _executing = false;
            notifyUpdate(this);
        }
        void importCorrupt() {
            _completionTime = System.currentTimeMillis();
            _corrupt = true;
            _executing = false;
            notifyUpdate(this);
        }
        void importMissingReplyKey() {
            _completionTime = System.currentTimeMillis();
            _noReplyKey = true;
            notifyUpdate(this);
        }
        void importMissingReadKey() {
            _completionTime = System.currentTimeMillis();
            _noReadKey = true;
            _executing = false;
            notifyUpdate(this);
        }
        void importPBE(String prompt) {
            _completionTime = System.currentTimeMillis();
            _pbePrompt = prompt;
            _executing = false;
            notifyUpdate(this);
        }
        void importSuccessful() {
            if (_completionTime <= 0) {
                _completionTime = System.currentTimeMillis();
                _importOK = true;
                _executing = false;
            }
            notifyUpdate(this);
        }
        
        void fetchFailed(String msg, Exception err) {
            _completionTime = System.currentTimeMillis();
            _fetchError = err;
            _fetchErrorMsg = msg;
            _executing = false;
            notifyUpdate(this);
        }
        
        boolean setIsExecuting(boolean executing) { 
            synchronized (IncomingAction.this) {
                if (executing && _executing) return false;
                _executing = executing;
            }
            if (executing) notifyUpdate(this);
            return true;
        }
        
        public void cancel(String reason) { if (!isComplete()) fetchFailed(reason, null); }
        
        public void dispose() {
            _disposed = true;
            _incomingActions.remove(IncomingAction.this);
            notifyUpdate(this);
        }
    }

    /** represents a push element, either scheduled, in process, or complete */
    public class OutgoingAction {
        private SyndieURI _uri;
        private long _completionTime;
        private boolean _paused;
        private boolean _executing;
        private int _size;
        private String _errMsg;
        private Exception _err;
        private boolean _disposed;
        
        public OutgoingAction(SyndieURI uri) {
            _uri = uri;
            _completionTime = -1;
            _paused = false;
            _executing = false;
            _size = 0;
            _disposed = false;
        }
        
        public SyndieURI getURI() { return _uri; }
        public SyncArchive getArchive() { return SyncArchive.this; }
        public boolean isScheduled() { return _completionTime == -1 && !_paused && !_executing; }
        public boolean isExecuting() { return _executing; }
        public boolean isPaused() { return _paused; }
        public boolean isComplete() { return _completionTime > 0; }
        public boolean isDisposed() { return _disposed; }
        public long getCompletionTime() { return _completionTime; }
        public int getSize() { return _size; }
        public String getErrorMsg() { return _errMsg; }
        public Exception getError() { return _err; }
        
        void setSize(int bytes) { _size = bytes; }
        
        void pushFailed(String msg, Exception err) {
            _errMsg = msg;
            _err = err;
            _completionTime = System.currentTimeMillis();
            setIsExecuting(false);
            notifyUpdate(this);
        }
        
        void pushOK() {
            _completionTime = System.currentTimeMillis();
            setIsExecuting(false);
            notifyUpdate(this);
        }
        
        boolean setIsExecuting(boolean executing) { 
            synchronized (OutgoingAction.this) {
                if (executing && _executing) return false;
                _executing = executing;
            }
            if (executing) notifyUpdate(this);
            return true;
        }
        public void cancel(String reason) { if (!isComplete()) pushFailed(reason, null); }
        public void dispose() {
            _disposed = true;
            _outgoingActions.remove(OutgoingAction.this);
            notifyUpdate(this);
        }
    }
    
    public void addListener(SyncArchiveListener lsnr) { if (!_listeners.contains(lsnr)) _listeners.add(lsnr); }
    public void removeListener(SyncArchiveListener lsnr) { _listeners.remove(lsnr); }
    
    public void clearCompletedActions(boolean incoming, boolean outgoing) {
        if (incoming) {
            List toRemove = new ArrayList();
            for (int i = 0; i < _incomingActions.size(); i++) {
                IncomingAction action = (IncomingAction)_incomingActions.get(i);
                if (action.getCompletionTime() > 0)
                    toRemove.add(action);
            }
            for (int i = 0; i < toRemove.size(); i++)
                ((IncomingAction)toRemove.get(i)).dispose(); // removes the element from _incomingActions
        }
        if (outgoing) {
            List toRemove = new ArrayList();
            for (int i = 0; i < _outgoingActions.size(); i++) {
                OutgoingAction action = (OutgoingAction)_outgoingActions.get(i);
                if (action.getCompletionTime() > 0)
                    toRemove.add(action);
            }
            for (int i = 0; i < toRemove.size(); i++)
                ((OutgoingAction)toRemove.get(i)).dispose(); // removes the element from _outgoingActions
        }
    }

    private void notifyUpdate(IncomingAction action) {
        for (int i = 0; i < _listeners.size(); i++) {
            SyncArchiveListener lsnr = (SyncArchiveListener)_listeners.get(i);
            lsnr.incomingUpdated(action);
        }
    }
    
    private void notifyUpdate(OutgoingAction action) {
        for (int i = 0; i < _listeners.size(); i++) {
            SyncArchiveListener lsnr = (SyncArchiveListener)_listeners.get(i);
            lsnr.outgoingUpdated(action);
        }
    }
    
    public void stop(String cancelReason) {
        _manager.getUI().debugMessage("stop " + toString() + ": " + cancelReason);
        if (!_indexFetchComplete) {
            _indexFetching = false;
            _indexFetchErrorMsg = cancelReason;
            _indexFetchError = null;
        }
        setNextSyncOneOff(false);
        setNextSyncTime(-1);
        for (int i = 0; i < _incomingActions.size(); i++) {
            IncomingAction action = (IncomingAction)_incomingActions.get(i);
            if (!action.isComplete())
                action.fetchFailed(cancelReason, null);
        }
        for (int i = 0; i < _outgoingActions.size(); i++) {
            OutgoingAction action = (OutgoingAction)_outgoingActions.get(i);
            if (!action.isComplete())
                action.pushFailed(cancelReason, null);
        }
        store(false);
        fireUpdated();
    }
    
    private static final String SQL_GET_ATTRIBUTES = "SELECT uriId, postKey, postKeySalt, readKey, readKeySalt, consecutiveFailures, customProxyHost, customProxyPort, customFCPHost, customFCPPort, nextPullDate, nextPushDate, lastPullDate, lastPushDate, customPullPolicy, customPushPolicy FROM nymArchive WHERE name = ? AND nymId = ?";
    /** (re)load all of the archive's attributes */
    private void load() {
        if (_name == null) return;
        
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            // uriId, postKey, postKeySalt, readKey, readKeySalt, consecutiveFailures, 
            // customProxyHost, customProxyPort, customFCPHost, customFCPPort, 
            // nextPullDate, nextPushDate, lastPullDate, lastPushDate, customPullPolicy, customPushPolicy
            // FROM nymArchive WHERE name = ? AND nymId = ?
            stmt = _client.con().prepareStatement(SQL_GET_ATTRIBUTES);
            stmt.setString(1, _name);
            stmt.setLong(2, _client.getLoggedInNymId());
            rs = stmt.executeQuery();
            if (rs.next()) {
                long uriId = rs.getLong(1);
                byte postKeyEncr[] = rs.getBytes(2);
                byte postKeySalt[] = rs.getBytes(3);
                byte readKeyEncr[] = rs.getBytes(4);
                byte readKeySalt[] = rs.getBytes(5);
                int consecFailures = rs.getInt(6);
                String proxyHost = rs.getString(7);
                int proxyPort = rs.getInt(8);
                String fcpHost = rs.getString(9);
                int fcpPort = rs.getInt(10);
                Timestamp nextPull = rs.getTimestamp(11);
                Timestamp nextPush = rs.getTimestamp(12);
                Timestamp lastPull = rs.getTimestamp(13);
                Timestamp lastPush = rs.getTimestamp(14);
                String pullPolicy = rs.getString(15);
                String pushPolicy = rs.getString(16);
                
                // now store 'em
                if (uriId >= 0) {
                    SyndieURI uri = _client.getURI(uriId);
                    if (uri != null) {
                        if (uri.isArchive())
                            _archiveURL = uri.getURL();
                        else if (uri.isURL())
                            _archiveURL = uri.getURL();
                    }
                }
                if (_archiveURL == null)
                    _archiveURL = "";
                
                byte postKey[] = null;
                if ( (postKeyEncr != null) && (postKeySalt != null) )
                    postKey = _client.pbeDecrypt(postKeyEncr, postKeySalt);
                byte readKey[] = null;
                if ( (readKeyEncr != null) && (readKeySalt != null) )
                    readKey = _client.pbeDecrypt(readKeyEncr, readKeySalt);
                if (postKey != null) _postKey = DataHelper.getUTF8(postKey); //Base64.encode(postKey);
                if (readKey != null) _readKey = DataHelper.getUTF8(postKey); //Base64.encode(readKey);
                
                if ( (proxyHost != null) && (proxyPort > 0) ) {
                    _httpProxyHost = proxyHost;
                    _httpProxyPort = proxyPort;
                }
                
                if ( (fcpHost != null) && (fcpPort > 0) ) {
                    _fcpHost = fcpHost;
                    _fcpPort = fcpPort;
                }
                
                _consecutiveFailures = consecFailures;
                
                long nxt = -1;
                if (nextPull != null)
                    nxt = nextPull.getTime();
                if ( (nextPush != null) && (nextPush.getTime() > 0) && ( (nextPush.getTime() < nxt) || (nxt <= 0) ) )
                    nxt = nextPush.getTime();
                _nextSyncTime = nxt;
                
                long last = -1;
                if (lastPull != null)
                    last = lastPull.getTime();
                if ( (lastPush != null) && (lastPush.getTime() > 0) && ( (lastPush.getTime() < last) || (last <= 0) ) )
                    last = lastPush.getTime();
                _lastSyncTime = last;
                
                _pullStrategy = new SharedArchiveEngine.PullStrategy(pullPolicy);
                _pushStrategy = new SharedArchiveEngine.PushStrategy(pushPolicy);
                
                _uriId = uriId;
            }
        } catch (SQLException se) {
            _client.logError("Error getting the nym archive details", se);
        } catch (RuntimeException re) {
            _client.logError("Internal error getting the nym archive details", re);
            throw new IllegalStateException("Internal error getting the archive details for " + _name + ": " + re.getMessage());
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String SQL_DELETE = "DELETE FROM nymArchive WHERE name = ? AND nymId = ?";
    private static final String SQL_DELETE_URI = "DELETE FROM uriAttribute WHERE uriId = ?";
    private static final String SQL_INSERT = "INSERT INTO nymArchive (uriId, postKey, postKeySalt, readKey, readKeySalt, " +
            "consecutiveFailures, customProxyHost, customProxyPort, customFCPHost, customFCPPort, " +
            "nextPullDate, nextPushDate, lastPullDate, lastPushDate, customPullPolicy, customPushPolicy, " +
            "name, nymId) " +
            "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
    /** persist all of the archive's attributes */
    public void store() { store(false); }
    public void store(boolean notifyListeners) {
        synchronized (this) {
            delete(false);

            PreparedStatement stmt = null;
            try {
                long uriId = _client.addURI(getArchiveURI());

                // uriId, postKey, postKeySalt, readKey, readKeySalt, " +
                // "consecutiveFailures, customProxyHost, customProxyPort, customFCPHost, customFCPPort, " +
                // "nextPullDate, nextPushDate, lastPullDate, lastPushDate, customPullPolicy, customPushPolicy, " +
                // "name, nymId)
                stmt = _client.con().prepareStatement(SQL_INSERT);
                stmt.setLong(1, uriId);
                if (_postKey != null) {
                    byte postKeySalt[] = new byte[16];
                    byte postKeyEncr[] = _client.pbeEncrypt(DataHelper.getUTF8(_postKey), postKeySalt);
                    stmt.setBytes(2, postKeyEncr);
                    stmt.setBytes(3, postKeySalt);
                } else {
                    stmt.setNull(2, Types.VARBINARY);
                    stmt.setNull(3, Types.VARBINARY);
                }
                if (_readKey != null) {
                    byte readKeySalt[] = new byte[16];
                    byte readKeyEncr[] = _client.pbeEncrypt(DataHelper.getUTF8(_readKey), readKeySalt);
                    stmt.setBytes(4, readKeyEncr);
                    stmt.setBytes(5, readKeySalt);
                } else {
                    stmt.setNull(4, Types.VARBINARY);
                    stmt.setNull(5, Types.VARBINARY);
                }
                stmt.setInt(6, _consecutiveFailures);
                if ( (_httpProxyHost != null) && (_httpProxyPort > 0) ) {
                    stmt.setString(7, _httpProxyHost);
                    stmt.setInt(8, _httpProxyPort);
                } else {
                    stmt.setNull(7, Types.VARCHAR);
                    stmt.setNull(8, Types.INTEGER);
                }
                if ( (_fcpHost != null) && (_fcpPort > 0) ) {
                    stmt.setString(9, _fcpHost);
                    stmt.setInt(10, _fcpPort);
                } else {
                    stmt.setNull(9, Types.VARCHAR);
                    stmt.setNull(10, Types.INTEGER);
                }

                if ( (_nextSyncTime > 0) && (!_nextSyncOneOff) )
                    stmt.setTimestamp(11, new Timestamp(_nextSyncTime ));
                else
                    stmt.setNull(11, Types.TIMESTAMP);
                if ( (_nextSyncTime > 0) && (!_nextSyncOneOff) )
                    stmt.setTimestamp(12, new Timestamp(_nextSyncTime));
                else
                    stmt.setNull(12, Types.TIMESTAMP);

                if (_lastSyncTime > 0)
                    stmt.setTimestamp(13, new Timestamp(_lastSyncTime));
                else
                    stmt.setNull(13, Types.TIMESTAMP);
                if (_lastSyncTime > 0)
                    stmt.setTimestamp(14, new Timestamp(_lastSyncTime ));
                else
                    stmt.setNull(14, Types.TIMESTAMP);

                if (_pullStrategy != null)
                    stmt.setString(15, _pullStrategy.serialize());
                else
                    stmt.setNull(15, Types.VARCHAR);

                if (_pushStrategy != null)
                    stmt.setString(16, _pushStrategy.serialize());
                else
                    stmt.setNull(16, Types.VARCHAR);

                stmt.setString(17, _name);
                stmt.setLong(18, _client.getLoggedInNymId());

                stmt.executeUpdate();
            } catch (SQLException se) {
                _client.logError("Error storing the nym archive details", se);
            } finally {
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }
        }
        
        if (_oldName == null)
            _manager.added(this);
        
        if (notifyListeners)
            fireUpdated();
        
        _manager.wakeUpEngine();
    }
    
    public void delete() { delete(true); }
    public void delete(boolean notifyListeners) {
        PreparedStatement stmt = null;
        try {
            if (_name != null) {
                stmt = _client.con().prepareStatement(SQL_DELETE);
                stmt.setString(1, _name);
                stmt.setLong(2, _client.getLoggedInNymId());
                stmt.executeUpdate();
                stmt.close();
                stmt = null;
            }
            
            if (_oldName != null) {
                stmt = _client.con().prepareStatement(SQL_DELETE);
                stmt.setString(1, _oldName);
                stmt.setLong(2, _client.getLoggedInNymId());
                stmt.executeUpdate();
                stmt.close();
                stmt = null;
            }
            
            if (_uriId >= 0) {
                _client.exec(SQL_DELETE_URI, _uriId);
                _uriId = -1;
            }
        } catch (SQLException se) {
            _client.logError("Error deleting the nym archive details", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        if (notifyListeners)
            _manager.deleted(this);
    }
    
    public String getName() { return _name; }
    public void setName(String name) { _name = name; }
    public String getURL() { return _archiveURL; }
    public void setURL(String url) { _archiveURL = url; }
    public String getPostKey() { return _postKey; }
    public void setPostKey(String key) { _postKey = key; }
    public String getReadKey() { return _readKey; }
    public void setReadKey(String key) { _readKey = key; }
    public String getHTTPProxyHost() { return _httpProxyHost; }
    public void setHTTPProxyHost(String host) { _httpProxyHost = host; }
    public int getHTTPProxyPort() { return _httpProxyPort; }
    public void setHTTPProxyPort(int port) { _httpProxyPort = port; }
    public String getFCPHost() { return _fcpHost; }
    public void setFCPHost(String host) { _fcpHost = host; }
    public int getFCPPort() { return _fcpPort; }
    public void setFCPPort(int port) { _fcpPort = port; }
    public int getConsecutiveFailures() { return _consecutiveFailures; }
    public void setConsecutiveFailures(int num) { _consecutiveFailures = num; }
    public long getNextSyncTime() { return _nextSyncTime; }
    public long getLastSyncTime() { return _lastSyncTime; }
    public void setNextSyncTime(long when) { _nextSyncTime = when; }
    public void setLastSyncTime(long when) { _lastSyncTime = when; }
    public boolean getNextSyncOneOff() { return _nextSyncOneOff; }
    public void setNextSyncOneOff(boolean oneOff) { _nextSyncOneOff = oneOff; }
    public SharedArchiveEngine.PullStrategy getPullStrategy() { return _pullStrategy; }
    public void setPullStrategy(SharedArchiveEngine.PullStrategy strategy) { _pullStrategy = strategy; }
    public SharedArchiveEngine.PushStrategy getPushStrategy() { return _pushStrategy; }
    public void setPushStrategy(SharedArchiveEngine.PushStrategy strategy) { _pushStrategy = strategy; }

    public boolean getIndexFetchInProgress() { return _indexFetching; }
    public void setIndexFetchInProgress(boolean now) {
        _indexFetching = now;
        if (!now) {
            _manager.getUI().debugMessage("SyncArchive: index complete for " + _name);
            _indexFetchComplete = true;
        } else {
            _manager.getUI().debugMessage("SyncArchive: index fetch beginning for " + _name);
        }
        
        fireUpdated();
    }
    public boolean getIndexFetchComplete() { return _indexFetchComplete; }
    
    public String getLastIndexFetchErrorMsg() { 
        String rv = _indexFetchErrorMsg;
        if (_indexFetchError != null)
            rv = rv + " - " + _indexFetchError.getMessage();
        return rv;
    }
    public void setLastIndexFetchErrorMsg(String msg) { _indexFetchErrorMsg = msg; }
    public Exception getLastIndexFetchError() { return _indexFetchError; }
    public void setLastIndexFetchError(Exception e) { _indexFetchError = e; }
    
    public SyndieURI getArchiveURI() { return SyndieURI.createArchive(getURL(), null); }
    
    public int getIncomingActionCount() { return _incomingActions.size(); }
    public IncomingAction getIncomingAction(int num) { return (IncomingAction)_incomingActions.get(num); }
    
    IncomingAction createIncomingAction(SyndieURI uri) { 
        for (int i = 0; i < _incomingActions.size(); i++) {
            IncomingAction cur = (IncomingAction)_incomingActions.get(i);
            // hmm, what if it was already complete?  etc
            if (cur.getURI().equals(uri))
                return cur;
        }
        IncomingAction action = new IncomingAction(uri); 
        _incomingActions.add(action);
        return action;
    }
    
    public int getOutgoingActionCount() { return _outgoingActions.size(); }
    public OutgoingAction getOutgoingAction(int num) { return (OutgoingAction)_outgoingActions.get(num); }
    public int getIncompleteOutgoingActionCount() {
        int rv = 0;
        for (int i = 0; i < _outgoingActions.size(); i++) {
            OutgoingAction action = (OutgoingAction)_outgoingActions.get(i);
            if (action.getCompletionTime() <= 0)
                rv++;
        }
        return rv;
    }
    
    OutgoingAction createOutgoingAction(SyndieURI uri) { 
        for (int i = 0; i < _outgoingActions.size(); i++) {
            OutgoingAction cur = (OutgoingAction)_outgoingActions.get(i);
            // hmm, what if it was already complete?  etc
            if (cur.getURI().equals(uri))
                return cur;
        }
        OutgoingAction action = new OutgoingAction(uri); 
        _outgoingActions.add(action);
        return action;
    }
    
    void indexFetchFail(String msg, Exception cause, boolean allowReschedule) {
        _manager.getUI().debugMessage("index fetch failed for " + _name + ": " + msg, cause);
        setLastIndexFetchErrorMsg(msg);
        setLastIndexFetchError(cause);
        setConsecutiveFailures(1 + getConsecutiveFailures());
        setIndexFetchInProgress(false);
        
        if (allowReschedule) {
            // which index fetch are we failing here?
            if (_nextSyncTime < System.currentTimeMillis())
                updateSchedule(false);
        } else {
            _nextSyncTime = -1;
            _nextSyncOneOff = false;
            store();
        }
        
        fireUpdated();
    }
    
    public void fetchActionComplete() {
        if (_incomingActions.size() > 0) {
            int ok = 0;
            int err = 0;
            int incomplete = 0;
            for (int i = 0; i < _incomingActions.size(); i++) {
                IncomingAction action = (IncomingAction)_incomingActions.get(i);
                if (!action.isComplete()) {
                    incomplete++;
                } else if ( (action.getFetchErrorMsg() != null) || (action.isCorrupt()) ) {
                    err++;
                } else {
                    ok++;
                }
            }
            if (incomplete == 0)
                updateSchedule(ok > 0 || err == 0);
        } else {
            updateSchedule(true);
        }
        fireUpdated();
    }
    public void pushActionComplete() {
        updateSchedule(true);
        fireUpdated();
    }
    
    private void updateSchedule(boolean success) {
        long delay = 0;
        int hours = 1;
        if (!success)
            hours = 1 << getConsecutiveFailures();
        
        _indexFetchComplete = false;
        _indexFetching = false;
        
        if (hours > 24) hours = 24;
        delay = hours*60*60*1000L + _client.ctx().random().nextInt(hours*60*60*1000);
        
        if (getNextSyncOneOff())
            setNextSyncTime(-1);
        else
            setNextSyncTime(System.currentTimeMillis() + delay);
        
        if (success)
            setLastSyncTime(System.currentTimeMillis());
        
        _manager.getUI().debugMessage("updateSchedule(" + success + "): next sync: " + Constants.getDateTime(getNextSyncTime()));
        store();
    }
}
