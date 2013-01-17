package syndie.db;

import java.io.File;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.i2p.data.Base64;
import net.i2p.data.DataHelper;

import syndie.Constants;
import syndie.data.SyndieURI;

/**
 *  The state of a single remote archive
 *
 */
public class SyncArchive {
    private final SyncManager _manager;
    private final DBClient _client;
    private String _name;
    private final String _oldName;
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
    private int _nextSyncDelayHours;
    private long _uriId;
    private boolean _nextSyncOneOff;
    private final List<IncomingAction> _incomingActions;
    private final List<OutgoingAction> _outgoingActions;
    private final List<SyncArchiveListener> _listeners;
    private long _whitelistGroupId;
    private SharedArchiveEngine.PullStrategy _pullStrategy;
    private SharedArchiveEngine.PushStrategy _pushStrategy;
    
    private boolean _indexFetching;
    private boolean _indexFetchComplete;
    private String _indexFetchErrorMsg;
    private Exception _indexFetchError;
    private long _indexFetchRcvd;
    private long _indexFetchSize = -1;
    
    private int _incomingActionsInProgress;
    private int _outgoingActionsInProgress;
    
    /**
     *  This is the default master loop time.
     *  The individual archive default min is in
     *  SyndicatorDetailHTTPArchive.SYNC_DELAY_DEFAULT_INDEX
     */
    private static final int DEFAULT_DELAY_HOURS = 4;

    // TODO proper state transitions and tracking with an enum

    public SyncArchive(SyncManager mgr, DBClient client) { this(mgr, client, null); }

    public SyncArchive(SyncManager mgr, DBClient client, String name) {
        _manager = mgr;
        _client = client;
        _name = name;
        _oldName = name;
        _uriId = -1;
        _lastSyncTime = -1;
        _nextSyncTime = -1;
        _nextSyncDelayHours = DEFAULT_DELAY_HOURS;
        _whitelistGroupId = -1;
        _incomingActions = new ArrayList();
        _outgoingActions = new ArrayList();
        _listeners = new ArrayList();
        load();
    }
    
    /**
     *  The index fetch returned 304 not modified
     *  @param old the archive from the last successfull fetch, or null if not available
     */
    public void indexNotModified(UI ui, SharedArchive old) {
        if (old != null) {
            indexFetched(ui, old, false);
        } else {
            setIndexFetchInProgress(false);
            _indexFetchComplete = false;
            setConsecutiveFailures(0);
            setLastIndexFetchErrorMsg(null);
            setLastIndexFetchError(null);
            updateSchedule(true);
            fireUpdated();
        }
    }

    /**
     * this creates the actions to be run as a result of fetching the specified archive index
     * @param archive non-null
     */
    public void indexFetched(UI ui, SharedArchive archive) {
        indexFetched(ui, archive, true);
    }

    /**
     * this creates the actions to be run as a result of fetching the specified archive index
     * @param archive non-null
     * @param isNew false for notModofied
     * @since 1.102b-6
     */
    private void indexFetched(UI ui, SharedArchive archive, boolean isNew) {
        // TODO: set a "processing" indication

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
        List pullURIs = new SharedArchiveEngine().selectURIsToPull(_client, ui, archive, pullStrategy, _whitelistGroupId);
        ui.debugMessage("index fetched, uris to pull: " + pullURIs.size());
        
        List pushURIs;
        if (isNew) {
            SharedArchiveEngine.PushStrategy pushStrategy = getPushStrategy();
            if (pushStrategy == null)
                pushStrategy = SyncManager.getInstance(_client, ui).getDefaultPushStrategy();
            ui.debugMessage("index fetched, push strategy: " + pushStrategy);
            pushURIs = new SharedArchiveEngine().selectURIsToPush(_client, ui, archive, pushStrategy);
            ui.debugMessage("index fetched, uris to push: " + pushURIs.size());
        } else {
            // It doesn't appear that we remember what we pushed last time,
            // so if we're rescanning the old index, don't push anything.
            // TODO keep track of pushes per-archive
            pushURIs = Collections.EMPTY_LIST;
            ui.debugMessage("index not modified, not pushing");
        }
        
        SharedArchive.Message msgs[] = archive.getMessages();
        SharedArchive.Channel scopes[] = archive.getChannels();
        for (int i = 0; i < pullURIs.size(); i++) {
            SyndieURI uri = (SyndieURI)pullURIs.get(i);
            IncomingAction action = createIncomingAction(uri);
            //long size = 0;
            if (uri.getMessageId() != null) {
                for (int j = 0; j < msgs.length; j++) {
                    if (msgs[j].getMessageId() == uri.getMessageId().longValue()) {
                        if (DataHelper.eq(uri.getScope().getData(), scopes[msgs[j].getScopeIndex()].getScope())) {
                            //size = (int)msgs[j].getMaxSizeKB()*1024;
                            break;
                        }
                    }
                }
            }
            // metadata sizes aren't counted atm
            //action.setSize(size);
        }
        
        for (int i = 0; i < pushURIs.size(); i++) {
            SyndieURI uri = (SyndieURI)pushURIs.get(i);
            OutgoingAction action = createOutgoingAction(uri);
            File src = null;
            if (uri.getMessageId() == null)
                src = new File(new File(_client.getArchiveDir(), uri.getScope().toBase64()), "meta" + Constants.FILENAME_SUFFIX);
            else
                src = new File(new File(_client.getArchiveDir(), uri.getScope().toBase64()), uri.getMessageId().toString() + Constants.FILENAME_SUFFIX);
            action.setSize(src.length());
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
        public void incomingUpdated(SyncArchive.IncomingAction action);
        public void incomingUpdated(List actions);
        public void outgoingUpdated(SyncArchive.OutgoingAction action);
        public void archiveUpdated(SyncArchive archive);
    }
    
    /** represents the pull of an archive element - either a message or metadata */
    public class IncomingAction {
        private final SyndieURI _uri;
        private long _completionTime;
        private boolean _paused;
        private boolean _executing;
        private boolean _fetchingMeta;
        private boolean _fetchingBody;
        private boolean _fetchOK;
        private boolean _importOK;
        private String _pbePrompt;
        private boolean _noReplyKey;
        private boolean _noReadKey;
        private String _fetchErrorMsg;
        private Exception _fetchError;
        private boolean _corrupt;
        private int _attempts;
        private long _size, _rcvd;
        private boolean _disposed;
        
        public IncomingAction(SyndieURI uri) {
            _uri = uri;
            _completionTime = -1;
            _size = -1;
        }
        
        public SyndieURI getURI() { return _uri; }
        public SyncArchive getArchive() { return SyncArchive.this; }
        public boolean isScheduled() { return _completionTime == -1 && !_paused && !_executing; }
        /** this really just means queued */
        public boolean isExecuting() { return _executing; }
        public boolean isFetchingMeta() { return _fetchingMeta; }
        public boolean isFetchingBody() { return _fetchingBody; }
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
        /** @deprecated unused */
        public int getFetchAttempts() { return _attempts; }
        /** @return -1 if unknown */
        public long getSize() { return _size; }
        public long getReceived() { return _rcvd; }

        /** @param total -1 if unknown */
        void setSize(long rcvd, long total) {
            _size = total;
            _rcvd = rcvd;
            notifyUpdate(this);
        }
        
        void setFetchingMeta() { 
            _fetchingMeta = true;
            notifyUpdate(this);
        }

        void setFetchingBody() { 
            _fetchingMeta = false;
            _fetchingBody = true;
            notifyUpdate(this);
        }

        void importFailed(String msg, Exception cause) {
            _completionTime = System.currentTimeMillis();
            _fetchError = cause;
            _fetchErrorMsg = (msg != null ? msg : cause + "");
            setIsExecuting(false);
            _manager.getUI().debugMessage(msg, cause);
        }
        void importCorrupt() {
            _completionTime = System.currentTimeMillis();
            _corrupt = true;
            setIsExecuting(false);
        }
        void importMissingReplyKey() {
            _completionTime = System.currentTimeMillis();
            _noReplyKey = true;
            notifyUpdate(this);
        }
        void importMissingReadKey() {
            _completionTime = System.currentTimeMillis();
            _noReadKey = true;
            setIsExecuting(false);
        }
        void importPBE(String prompt) {
            _completionTime = System.currentTimeMillis();
            _pbePrompt = prompt;
            setIsExecuting(false);
        }
        void importSuccessful() {
            if (_completionTime <= 0) {
                _completionTime = System.currentTimeMillis();
                _importOK = true;
                setIsExecuting(false);
            } else
                notifyUpdate(this); // we didn't call to setIsExecuting(), so notify manually
        }
        
        void fetchFailed(String msg, Exception err) {
            _completionTime = System.currentTimeMillis();
            _fetchError = err;
            _fetchErrorMsg = msg;
            setIsExecuting(false);
            _manager.getUI().debugMessage(msg, err);
        }
        
        boolean setIsExecuting(boolean executing) {
            if (!executing) {
                _fetchingMeta = false;
                _fetchingBody = false;
            }
            boolean changed;
            synchronized (IncomingAction.this) {
                changed = _executing != executing;
                if (changed) {
                    _executing = executing;
                    _incomingActionsInProgress += executing ? 1 : -1;
                }
            }
            if (changed) notifyUpdate(this);
            return changed;
        }
        
        public void cancel(String reason) { if (!isComplete()) fetchFailed(reason, null); }
        
        public void clearFetchError() {
            if (isComplete() && _fetchErrorMsg != null) {
                _completionTime = -1;
                _fetchError = null;
                _fetchErrorMsg = null;
                notifyUpdate(this);
            }
        }
        
        public void dispose() {
            _disposed = true;
            _incomingActions.remove(IncomingAction.this);
            notifyUpdate(this);
        }
    }

    /** represents a push element, either scheduled, in process, or complete */
    public class OutgoingAction {
        private final SyndieURI _uri;
        private long _completionTime;
        private boolean _paused;
        private boolean _executing;
        private boolean _pushingMeta;
        private boolean _pushingBody;
        private long _size;
        private String _errMsg;
        private Exception _err;
        private boolean _disposed;
        
        public OutgoingAction(SyndieURI uri) {
            _uri = uri;
            _completionTime = -1;
        }
        
        public SyndieURI getURI() { return _uri; }
        public SyncArchive getArchive() { return SyncArchive.this; }
        public boolean isScheduled() { return _completionTime == -1 && !_paused && !_executing; }
        /** this really just means queued */
        public boolean isExecuting() { return _executing; }
        public boolean isPushingMeta() { return _pushingMeta; }
        public boolean isPushingBody() { return _pushingBody; }
        public boolean isPaused() { return _paused; }
        public boolean isComplete() { return _completionTime > 0; }
        public boolean isDisposed() { return _disposed; }
        public long getCompletionTime() { return _completionTime; }
        public long getSize() { return _size; }
        public String getErrorMsg() { return _errMsg; }
        public Exception getError() { return _err; }
        
        void setSize(long bytes) { _size = bytes; }
        
        void setPushingMeta() {
            _pushingMeta = true;
            notifyUpdate(this);
        }

        void setPushingBody() {
            _pushingMeta = false;
            _pushingBody = true;
            notifyUpdate(this);
        }

        void pushFailed(String msg, Exception err) {
            _errMsg = msg;
            _err = err;
            _completionTime = System.currentTimeMillis();
            setIsExecuting(false);
            _manager.getUI().debugMessage(msg, err);
        }
        
        void pushOK() {
            _completionTime = System.currentTimeMillis();
            setIsExecuting(false);
        }
        
        boolean setIsExecuting(boolean executing) { 
            if (!executing) {
                _pushingMeta = false;
                _pushingBody = false;
            }
            boolean changed;
            synchronized (OutgoingAction.this) {
                changed = _executing != executing;
                if (changed) {
                    _executing = executing;
                    _outgoingActionsInProgress += executing ? 1 : -1;
                }
            }
            if (changed) notifyUpdate(this);
            return changed;
        }
        
        public void cancel(String reason) { if (!isComplete()) pushFailed(reason, null); }
        
        public void clearError() {
            if (isComplete() && _errMsg != null) {
                _completionTime = -1;
                _errMsg = null;
                _err = null;
                notifyUpdate(this);
            }
        }
        
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
        // TODO this is really hard on the UI, can take several minutes,
        // have a bulk cancel call to the UI?
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
    
    private static final String SQL_GET_ATTRIBUTES = "SELECT uriId, postKey, postKeySalt, readKey, readKeySalt, consecutiveFailures, customProxyHost, customProxyPort, customFCPHost, customFCPPort, nextPullDate, nextPushDate, lastPullDate, lastPushDate, customPullPolicy, customPushPolicy, nextSyncDelayHours, whitelistGroupId FROM nymArchive WHERE name = ? AND nymId = ?";

    /** (re)load all of the archive's attributes */
    private void load() {
        if (_name == null) return;
        
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            // uriId, postKey, postKeySalt, readKey, readKeySalt, consecutiveFailures, 
            // customProxyHost, customProxyPort, customFCPHost, customFCPPort, 
            // nextPullDate, nextPushDate, lastPullDate, lastPushDate, customPullPolicy, customPushPolicy,
            // nextSyncDelayHours, whitelistGroupId
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
                int nextSyncDelayHours = rs.getInt(17);
                long whitelistGroupId = rs.getLong(18);
                
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
                
                _nextSyncDelayHours = nextSyncDelayHours;
                _whitelistGroupId = whitelistGroupId;
                
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
            "name, nymId, nextSyncDelayHours, whitelistGroupId) " +
            "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";

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
                stmt.setInt(19, _nextSyncDelayHours);
                stmt.setLong(20, _whitelistGroupId);

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

    /** must be called to complete fetch/push/pull cycle (success) */
    public void setNextSyncTime() { setNextSyncTime(true); }

    /** must be called to complete fetch/push/pull cycle */
    private void setNextSyncTime(boolean success) { setNextSyncTime(System.currentTimeMillis() + getDelay(success)); }

    /** does NOT clear fetch/push/pull cycle */
    public void setNextSyncTime(long when) { _nextSyncTime = when; }

    public void setLastSyncTime(long when) { _lastSyncTime = when; }

    /** if true, only sync once */
    public boolean getNextSyncOneOff() { return _nextSyncOneOff; }
    public void setNextSyncOneOff(boolean oneOff) { _nextSyncOneOff = oneOff; }

    /** TODO proper state tracking */
    public boolean getSyncInProgress() { return getIndexFetchInProgress() || _incomingActionsInProgress > 0 || _outgoingActionsInProgress > 0; }

    public SharedArchiveEngine.PullStrategy getPullStrategy() { return _pullStrategy; }
    public void setPullStrategy(SharedArchiveEngine.PullStrategy strategy) { _pullStrategy = strategy; }
    public SharedArchiveEngine.PushStrategy getPushStrategy() { return _pushStrategy; }
    public void setPushStrategy(SharedArchiveEngine.PushStrategy strategy) { _pushStrategy = strategy; }
    public void setNextSyncDelay(int hours) { _nextSyncDelayHours = hours; }
    public int getNextSyncDelay() { return _nextSyncDelayHours; }
    public long getWhitelistGroupId() { return _whitelistGroupId; }
    public void setWhitelistGroupId(long id) { _whitelistGroupId = id; }

    /**
     *  True if during index fetch only.
     *  False during pushes/pulls.
     */
    public boolean getIndexFetchInProgress() { return _indexFetching; }

    public void setIndexFetchInProgress(boolean now) {
        _indexFetching = now;
        if (!now) {
            _manager.getUI().debugMessage("SyncArchive: index fetch complete for " + _name);
            _indexFetchComplete = true;
        } else {
            _manager.getUI().debugMessage("SyncArchive: index fetch beginning for " + _name);
        }
        
        fireUpdated();
    }

    /** @param total -1 if unknown */
    public void setIndexFetchProgress(long rcvd, long total) {
        _indexFetchRcvd = rcvd;
        _indexFetchSize = total;
        fireUpdated();
    }

    public long getIndexFetchRcvdBytes() { return _indexFetchRcvd; }

    /** @return -1 if unknown */
    public long getIndexFetchSize() { return _indexFetchSize; }

    /**
     *  True if fetch was successful and pushes/pulls are in progress.
     *  False during fetch. False after fetch failure, and after pushes/pulls are complete.
     */
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

    /** @since 1.102b-9 */
    public int getIncomingActionsInProgress() { return _incomingActionsInProgress; }

    public IncomingAction getIncomingAction(int num) { return (IncomingAction)_incomingActions.get(num); }
    
    IncomingAction createIncomingAction(SyndieURI uri) { 
        // FIXME O(n**2)
        for (int i = 0; i < _incomingActions.size(); i++) {
            IncomingAction cur = (IncomingAction)_incomingActions.get(i);
            // hmm, what if it was already complete?  etc
            if (cur.getURI().equals(uri)) {
                if (cur.getFetchErrorMsg() != null)
                    cur.clearFetchError();
                
                return cur;
            }
        }
        IncomingAction action = new IncomingAction(uri); 
        _incomingActions.add(action);
        return action;
    }
    
    public int getOutgoingActionCount() { return _outgoingActions.size(); }

    /** @since 1.102b-9 */
    public int getOutgoingActionsInProgress() { return _outgoingActionsInProgress; }

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
        // FIXME O(n**2)
        for (int i = 0; i < _outgoingActions.size(); i++) {
            OutgoingAction cur = (OutgoingAction)_outgoingActions.get(i);
            // hmm, what if it was already complete?  etc
            if (cur.getURI().equals(uri)) {
                if (cur.getErrorMsg() != null)
                    cur.clearError();

                return cur;
            }
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
        _indexFetchComplete = false;
        
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

    /**
     *  Call after pushes are complete.
     *  Clears the index fetching and index fetch complete flags.
     *  Pushes must be after pulls.
     */
    public void pushActionComplete() {
        updateSchedule(true);
        fireUpdated();
    }
    
    /**
     *  Call after index fetch fails or pushes and pulls are complete.
     *  Clears the index fetching and index fetch complete flags.
     */
    private long getDelay(boolean success) {
        long delay = 0;
        int hours = 1;
        if (!success)
            hours = _nextSyncDelayHours * getConsecutiveFailures();
        
        _indexFetchComplete = false;
        _indexFetching = false;
        
        if (hours > 72) hours = 72;
        if (hours < _nextSyncDelayHours)
            hours = _nextSyncDelayHours;
        
        return hours*60*60*1000L + _client.ctx().random().nextInt(60*60*1000);
    }
    
    /**
     *  Call after index fetch fails or pushes and pulls are complete.
     *  Clears the index fetching and index fetch complete flags.
     */
    private void updateSchedule(boolean success) {
        if (getNextSyncOneOff()) {
            setNextSyncTime(-1);
            _indexFetchComplete = false;
            _indexFetching = false;
        } else
            setNextSyncTime(success);
        
        if (success)
            setLastSyncTime(System.currentTimeMillis());
        
        _manager.getUI().debugMessage("updateSchedule(" + success + "): next sync: " + Constants.getDateTime(getNextSyncTime()));
        _manager.getUI().debugMessage("incoming act/tot: " + _incomingActionsInProgress + " / " + _incomingActions.size());
        _manager.getUI().debugMessage("outgoing act/tot: " + _outgoingActionsInProgress + " / " + _outgoingActions.size());
        store();
    }

    public String toString() {
        return "Archive " + _archiveURL;
    }
}
