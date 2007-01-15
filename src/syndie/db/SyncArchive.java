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
    private long _nextPullTime;
    private long _nextPushTime;
    private long _lastPullTime;
    private long _lastPushTime;
    private long _uriId;
    private boolean _nextPullOneOff;
    private boolean _nextPushOneOff;
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
        _lastPullTime = -1;
        _lastPushTime = -1;
        _nextPullTime = -1;
        _nextPushTime = -1;
        _nextPullOneOff = false;
        _nextPushOneOff = false;
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
        SharedArchiveEngine.PullStrategy pullStrategy = getPullStrategy();
        if (pullStrategy == null)
            pullStrategy = SyncManager.getInstance(_client, ui).getDefaultPullStrategy();
        List pullURIs = new SharedArchiveEngine().selectURIsToPull(_client, ui, archive, pullStrategy);
        
        SharedArchiveEngine.PushStrategy pushStrategy = getPushStrategy();
        if (pushStrategy == null)
            pushStrategy = SyncManager.getInstance(_client, ui).getDefaultPushStrategy();
        List pushURIs = new SharedArchiveEngine().selectURIsToPush(_client, ui, archive, pushStrategy);
        
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
            for (int j = 0; j < _incomingActions.size(); j++) {
                IncomingAction action = (IncomingAction)_incomingActions.get(j);
                lsnr.incomingUpdated(action);
            }
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
            updateSchedule(true, true); // pull
            updateSchedule(true, false); // push
        }
        
        ui.debugMessage("index fetch complete, notify " + _listeners);
        for (int i = 0; i < _listeners.size(); i++) {
            SyncArchiveListener lsnr = (SyncArchiveListener)_listeners.get(i);
            lsnr.archiveUpdated(this);
        }
    }
    
    public interface SyncArchiveListener {
        public void incomingUpdated(IncomingAction action);
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
        }
        
        public SyndieURI getURI() { return _uri; }
        public SyncArchive getArchive() { return SyncArchive.this; }
        public boolean isScheduled() { return _completionTime == -1 && !_paused && !_executing; }
        public boolean isExecuting() { return _executing; }
        public boolean isComplete() { return _completionTime > 0; }
        public boolean isPaused() { return _paused; }
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
            _completionTime = System.currentTimeMillis();
            _importOK = true;
            _executing = false;
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
        
        public OutgoingAction(SyndieURI uri) {
            _uri = uri;
            _completionTime = -1;
            _paused = false;
            _executing = false;
            _size = 0;
        }
        
        public SyndieURI getURI() { return _uri; }
        public SyncArchive getArchive() { return SyncArchive.this; }
        public boolean isScheduled() { return _completionTime == -1 && !_paused && !_executing; }
        public boolean isExecuting() { return _executing; }
        public boolean isPaused() { return _paused; }
        public boolean isComplete() { return _completionTime > 0; }
        public long getCompletionTime() { return _completionTime; }
        public int getSize() { return _size; }
        
        void setSize(int bytes) { _size = bytes; }
        
        void pushFailed(String msg, Exception err) {
            _errMsg = msg;
            _err = err;
            _completionTime = System.currentTimeMillis();
            notifyUpdate(this);
        }
        
        void pushOK() {
            _completionTime = System.currentTimeMillis();
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
    }
    
    public void addListener(SyncArchiveListener lsnr) { if (!_listeners.contains(lsnr)) _listeners.add(lsnr); }
    public void removeListener(SyncArchiveListener lsnr) { _listeners.remove(lsnr); }

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
                SyndieURI uri = _client.getURI(uriId);
                if (uri != null) {
                    if (uri.isArchive())
                        _archiveURL = uri.getURL();
                    else if (uri.isURL())
                        _archiveURL = uri.getURL();
                }
                if (_archiveURL == null)
                    _archiveURL = "";
                
                byte postKey[] = null;
                if ( (postKeyEncr != null) && (postKeySalt != null) )
                    postKey = _client.pbeDecrypt(postKeyEncr, postKeySalt);
                byte readKey[] = null;
                if ( (readKeyEncr != null) && (readKeySalt != null) )
                    readKey = _client.pbeDecrypt(readKeyEncr, readKeySalt);
                if (postKey != null) _postKey = Base64.encode(postKey);
                if (readKey != null) _readKey = Base64.encode(readKey);
                
                if ( (proxyHost != null) && (proxyPort > 0) ) {
                    _httpProxyHost = proxyHost;
                    _httpProxyPort = proxyPort;
                }
                
                if ( (fcpHost != null) && (fcpPort > 0) ) {
                    _fcpHost = fcpHost;
                    _fcpPort = fcpPort;
                }
                
                _consecutiveFailures = consecFailures;
                
                _nextPullTime = (nextPull != null ? nextPull.getTime() : -1);
                _nextPushTime = (nextPush != null ? nextPush.getTime() : -1);
                _lastPullTime = (lastPull != null ? lastPull.getTime() : -1);
                _lastPushTime = (lastPush != null ? lastPush.getTime() : -1);
                
                _pullStrategy = new SharedArchiveEngine.PullStrategy(pullPolicy);
                _pushStrategy = new SharedArchiveEngine.PushStrategy(pushPolicy);
                
                _uriId = uriId;
            }
        } catch (SQLException se) {
            _client.logError("Error getting the nym archive details", se);
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
            
            if (_nextPullTime > 0)
                stmt.setTimestamp(11, new Timestamp(_nextPullTime));
            else
                stmt.setNull(11, Types.TIMESTAMP);
            if (_nextPushTime > 0)
                stmt.setTimestamp(12, new Timestamp(_nextPushTime));
            else
                stmt.setNull(12, Types.TIMESTAMP);
            
            if (_lastPullTime > 0)
                stmt.setTimestamp(13, new Timestamp(_lastPullTime));
            else
                stmt.setNull(13, Types.TIMESTAMP);
            if (_lastPushTime > 0)
                stmt.setTimestamp(14, new Timestamp(_lastPushTime));
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
        
        if (_oldName == null)
            _manager.added(this);
        
        if (notifyListeners) {
            for (int i = 0; i < _listeners.size(); i++) {
                SyncArchiveListener lsnr = (SyncArchiveListener)_listeners.get(i);
                lsnr.archiveUpdated(this);
            }
        }
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
    public long getNextPullTime() { return _nextPullTime; }
    public void setNextPullTime(long when) { _nextPullTime = when; }
    public long getNextPushTime() { return _nextPushTime; }
    public void setNextPushTime(long when) { _nextPushTime = when; }
    public long getLastPullTime() { return _lastPullTime; }
    public void setLastPullTime(long when) { _lastPullTime = when; }
    public long getLastPushTime() { return _lastPushTime; }
    public void setLastPushTime(long when) { _lastPushTime = when; }
    public boolean getNextPullOneOff() { return _nextPullOneOff; }
    public void setNextPullOneOff(boolean oneOff) { _nextPullOneOff = oneOff; }
    public boolean getNextPushOneOff() { return _nextPushOneOff; }
    public void setNextPushOneOff(boolean oneOff) { _nextPushOneOff = oneOff; }
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
        
        for (int i = 0; i < _listeners.size(); i++) {
            SyncArchiveListener lsnr = (SyncArchiveListener)_listeners.get(i);
            lsnr.archiveUpdated(this);
        }
    }
    public boolean getIndexFetchComplete() { return _indexFetchComplete; }
    
    public String getLastIndexFetchErrorMsg() { return _indexFetchErrorMsg; }
    public void setLastIndexFetchErrorMsg(String msg) { _indexFetchErrorMsg = msg; }
    public Exception getLastIndexFetchError() { return _indexFetchError; }
    public void setLastIndexFetchError(Exception e) { _indexFetchError = e; }
    
    public SyndieURI getArchiveURI() { return SyndieURI.createArchive(getURL(), null); }
    
    public int getIncomingActionCount() { return _incomingActions.size(); }
    public IncomingAction getIncomingAction(int num) { return (IncomingAction)_incomingActions.get(num); }
    
    IncomingAction createIncomingAction(SyndieURI uri) { 
        IncomingAction action = new IncomingAction(uri); 
        _incomingActions.add(action);
        return action;
    }
    
    public int getOutgoingActionCount() { return _outgoingActions.size(); }
    public OutgoingAction getOutgoingAction(int num) { return (OutgoingAction)_outgoingActions.get(num); }
    
    OutgoingAction createOutgoingAction(SyndieURI uri) { 
        OutgoingAction action = new OutgoingAction(uri); 
        _outgoingActions.add(action);
        return action;
    }
    
    void indexFetchFail(String msg, Exception cause, boolean allowReschedule) {
        _manager.getUI().debugMessage("index fetch failed for " + _name + ": " + msg, cause);
        setIndexFetchInProgress(false);
        setConsecutiveFailures(1 + getConsecutiveFailures());
        setLastIndexFetchErrorMsg(msg);
        setLastIndexFetchError(cause);
        
        if (allowReschedule) {
            // which index fetch are we failing here?
            if (_nextPullTime < System.currentTimeMillis())
                updateSchedule(false, true);
            if (_nextPushTime < System.currentTimeMillis())
                updateSchedule(false, false);
        } else {
            _nextPullTime = -1;
            _nextPushTime = -1;
            _nextPullOneOff = false;
            _nextPushOneOff = false;
            store();
        }
        
        for (int i = 0; i < _listeners.size(); i++) {
            SyncArchiveListener lsnr = (SyncArchiveListener)_listeners.get(i);
            lsnr.archiveUpdated(this);
        }
    }
    
    public void fetchActionComplete() {
        if (_incomingActions.size() > 0) {
            int ok = 0;
            int err = 0;
            for (int i = 0; i < _incomingActions.size(); i++) {
                IncomingAction action = (IncomingAction)_incomingActions.get(i);
                if (!action.isComplete()) continue;
                if ( (action.getFetchErrorMsg() != null) || (action.isCorrupt()) )
                    err++;
                else
                    ok++;
            }
            updateSchedule(ok > 0, true);
        } else {
            updateSchedule(true, true);
        }
        for (int i = 0; i < _listeners.size(); i++) {
            SyncArchiveListener lsnr = (SyncArchiveListener)_listeners.get(i);
            lsnr.archiveUpdated(this);
        }
    }
    public void pushActionComplete() {
        updateSchedule(true, false);
        for (int i = 0; i < _listeners.size(); i++) {
            SyncArchiveListener lsnr = (SyncArchiveListener)_listeners.get(i);
            lsnr.archiveUpdated(this);
        }
    }
    
    private void updateSchedule(boolean success, boolean inbound) {
        long delay = 0;
        int hours = 1;
        if (!success)
            hours = 1 << getConsecutiveFailures();
        
        if (hours > 24) hours = 24;
        delay = hours*60*60*1000L + _client.ctx().random().nextInt(hours*60*60*1000);
        
        if (inbound) {
            if ( (getNextPullTime() > 0) && (getNextPullTime() <= System.currentTimeMillis()) ) {
                if (getNextPullOneOff())
                    setNextPullTime(-1);
                else
                    setNextPullTime(System.currentTimeMillis() + delay);
                setNextPullOneOff(false);
            }
            if (success)
                setLastPullTime(System.currentTimeMillis());
        } else {
            if ( (getNextPushTime() > 0) && (getNextPushTime() <= System.currentTimeMillis()) ) {
                if (getNextPushOneOff())
                    setNextPushTime(-1);
                else
                    setNextPushTime(System.currentTimeMillis() + delay);
                setNextPushOneOff(false);
            }
            if (success)
                setLastPushTime(System.currentTimeMillis());
        }
        
        store();
    }
}
