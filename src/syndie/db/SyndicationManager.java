package syndie.db;

import java.io.File;
import java.io.IOException;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import net.i2p.data.DataHelper;
import net.i2p.data.SessionKey;
import net.i2p.util.EepGet;
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
    private MergedArchiveIndex _mergedIndex;
    private ArchiveDiff _mergedDiff;
    private int _concurrent;
    private String _httpProxyHost;
    private int _httpProxyPort;
    private String _fcpHost;
    private int _fcpPort;
            
    public static final int INDEX_STATUS_START = 0;
    public static final int INDEX_STATUS_FETCHING = 1;
    public static final int INDEX_STATUS_FETCH_COMPLETE = 2;
    public static final int INDEX_STATUS_LOAD_OK = 3;
    public static final int INDEX_STATUS_LOAD_ERROR = 4;
    public static final int INDEX_STATUS_FETCH_ERROR = 5;
    public static final int INDEX_STATUS_DIFF_OK = 6;
    
    public static final int FETCH_SCHEDULED = 0;
    public static final int FETCH_STARTED = 1;
    public static final int FETCH_COMPLETE = 2;
    public static final int FETCH_FAILED = 3;
    public static final int FETCH_IMPORT_OK = 4;
    public static final int FETCH_IMPORT_PBE = 5;
    public static final int FETCH_IMPORT_NOKEY = 6;
    public static final int FETCH_IMPORT_CORRUPT = 7;
    public static final int FETCH_STOPPED = 8;
    
    public static final int STRATEGY_DELTA = 0;
    public static final int STRATEGY_DELTAKNOWN = 1;
    public static final int STRATEGY_PIR = 2;
    public static final int STRATEGY_DEFAULT = STRATEGY_DELTA;
    

    public SyndicationManager(DBClient client, UI ui) {
        _client = client;
        _ui = ui;
        _archives = new ArrayList();
        _listeners = new ArrayList();
        _fetchRecords = new ArrayList();
        _mergedIndex = null;
    }
    
    /** 
     * note that callbacks on this interface can be hit from any thread, so if they
     * touch an SWT resource, they should do so via display.asyncExec(runnable)
     */
    public interface SyndicationListener {
        public void archiveAdded(SyndicationManager mgr, String name);
        public void archiveRemoved(SyndicationManager mgr, String name);
        public void archiveUpdated(SyndicationManager mgr, String oldName, String newName);

        /**
         * ideal status sequence: START, FETCHING, FETCH_COMPLETE, LOAD_OK, DIFF_OK
         */
        public void archiveIndexStatus(SyndicationManager mgr, String archiveName, int status, String msg);
        
        /**
         * ideal status sequence: SCHEDULED, STARTED, COMPLETE, ( IMPORT_OK | IMPORT_PBE )
         */
        public void fetchStatusUpdated(SyndicationManager mgr, FetchRecord record);
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
    public ArchiveIndex getArchiveIndex(int index) {
        NymArchive archive = getArchive(index);
        if (archive != null)
            return archive.getIndex();
        else
            return null;
    }
    public ArchiveDiff getArchiveDiff(int index) {
        NymArchive archive = getArchive(index);
        if (archive != null)
            return archive.getDiff();
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
    /** get an index summarizing across all of the fetched archive indexes */
    public MergedArchiveIndex getMergedIndex(boolean forceRemerge) {
        if ((_mergedIndex == null) || (forceRemerge)) {
            MergedArchiveIndex merged = new MergedArchiveIndex();
            for (int i = 0; i < _archives.size(); i++) {
                ArchiveIndex index = getArchiveIndex(i);
                if (index != null)
                    merged.merge(index, getArchiveName(i));
            }
            _mergedDiff = null;
            _mergedIndex = merged;
        }
        return _mergedIndex;
    }
    /** diff of the local archive vs all of the fetched archives combined */
    public ArchiveDiff getMergedDiff(boolean forceRediff) {
        if ((_mergedDiff == null) || (forceRediff)) {
            ArchiveIndex index = getMergedIndex(false);
            ArchiveDiff diff = index.diff(_client, _ui, new Opts());
            _mergedDiff = diff;
        }
        return _mergedDiff;
    }
    
    public void setProxies(String httpHost, int httpPort, String fcpHost, int fcpPort) {
        _httpProxyHost = httpHost;
        _httpProxyPort = httpPort;
        _fcpHost = fcpHost;
        _fcpPort = fcpPort;
    }
    
    /**
     * run this many concurrent http fetches/imports at a time
     */
    public void startFetching(int concurrentFetches) {
        for (int i = _concurrent; i < concurrentFetches; i++) {
            _concurrent++;
            Thread t = new Thread(new Fetcher(), "Fetcher" + i);
            t.setDaemon(true);
            t.start();
        }
    }
    
    public void fetchIndex(int idx) {
        // port of syndicateMenu.getIndex
        NymArchive archive = getArchive(idx);
        if (archive == null) return;
        
        String baseUrl = archive.getURI().getURL();
        if (baseUrl == null) return;

        _ui.debugMessage("fetchIndex: " + idx + " started");
        fireIndexStatus(archive.getName(), INDEX_STATUS_START, null);
        
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
            url = baseUrl + "index-all.dat";
        }
	if (includeForceDownload) url = url + "?forcedownload";

        _ui.debugMessage("fetchIndex: " + idx + " fetching: " + url);
        fireIndexStatus(archive.getName(), INDEX_STATUS_FETCHING, null);

        boolean shouldProxy = (proxyHost != null) && (proxyPort > 0);
        boolean archiveWasRemote = true;
        File out = null;
        if (baseUrl.startsWith("/")) {
            out = new File(url);
            _ui.debugMessage("fetchIndex: " + idx + " fetch complete: " + url);
            fireIndexStatus(archive.getName(), INDEX_STATUS_FETCH_COMPLETE, null);
            archiveWasRemote = false;
        } else if (baseUrl.startsWith("file://")) {
            out = new File(baseUrl.substring("file://".length()));
            _ui.debugMessage("fetchIndex: " + idx + " fetch complete: " + url);
            fireIndexStatus(archive.getName(), INDEX_STATUS_FETCH_COMPLETE, null);
            archiveWasRemote = false;
        } else {
            try {
                out = File.createTempFile("syndicate", ".index", _client.getTempDir());
                EepGet get = new EepGet(_client.ctx(), shouldProxy, proxyHost, (int)proxyPort, 0, out.getPath(), url, false, null, null);
                get.addStatusListener(new UIStatusListener());
                boolean fetched = get.fetch();
                if (!fetched) {
                    _ui.errorMessage("Fetch failed of " + url);
                    _ui.debugMessage("fetchIndex: " + idx + " fetch error: " + url);
                    fireIndexStatus(archive.getName(), INDEX_STATUS_FETCH_ERROR, "fetch failed");
                    if (archiveWasRemote)
                        out.delete();
                    return;
                }
                _ui.debugMessage("fetchIndex: " + idx + " fetch complete: " + url);
                fireIndexStatus(archive.getName(), INDEX_STATUS_FETCH_COMPLETE, null);
            } catch (IOException ioe) {
                _ui.errorMessage("Error pulling the index", ioe);
                fireIndexStatus(archive.getName(), INDEX_STATUS_FETCH_ERROR, ioe.getMessage());
                if (archiveWasRemote)
                    out.delete();
                return;
            }
        }
        try {
            ArchiveIndex index = ArchiveIndex.loadIndex(out, _ui, unauth);
            if (index != null) {
                archive.setIndex(index);
                HTTPSyndicator syndicator = new HTTPSyndicator(baseUrl, proxyHost, proxyPort, _client, _ui, index, false); //opts.getOptBoolean("reimport", false));
                archive.setSyndicator(syndicator);
                _ui.debugMessage("fetchIndex: " + idx + " index loaded");
                fireIndexStatus(archive.getName(), INDEX_STATUS_LOAD_OK, null);
                ArchiveDiff diff = index.diff(_client, _ui, new Opts());
                archive.setDiff(diff);
                _ui.debugMessage("fetchIndex: " + idx + " diff loaded");
                fireIndexStatus(archive.getName(), INDEX_STATUS_DIFF_OK, null);
            } else {
                _ui.debugMessage("fetchIndex: " + idx + " load error");
                fireIndexStatus(archive.getName(), INDEX_STATUS_LOAD_ERROR, "index was not valid");
            }
        } catch (IOException ioe) {
            _ui.errorMessage("Error loading the index", ioe);
            fireIndexStatus(archive.getName(), INDEX_STATUS_LOAD_ERROR, ioe.getMessage());
        }
        if (archiveWasRemote && out != null)
            out.delete();
    }
    
    private void fireIndexStatus(String name, int status, String msg) {
        for (int i = 0; i < _listeners.size(); i++) {
            SyndicationListener lsnr = (SyndicationListener)_listeners.get(i);
            lsnr.archiveIndexStatus(this, name, status, msg);
        }
    }
    
    public void fetch(String archiveName, SyndieURI uri) {
        FetchRecord rec = new FetchRecord(archiveName, uri);
        fireFetchStatusUpdated(rec);
        synchronized (_fetchRecords) {
            _fetchRecords.add(rec);
            _fetchRecords.notifyAll();
        }
    }
    
    /** 
     * queue up the fetch for entries matching the given strategy, as well as pushes
     * @param maxkb don't pull posts larger than this size
     * @param strategy STRATEGY_* to determine which entries to pull
     * @param push should we push too
     */
    public void sync(int maxkb, int strategy, boolean push) {
        HashSet uris = new HashSet();
        switch (strategy) {
            case STRATEGY_DELTAKNOWN:
                for (int i = 0; i < _archives.size(); i++) {
                    NymArchive archive = (NymArchive)_archives.get(i);
                    ArchiveDiff diff = archive.getDiff();
                    if (diff != null) {
                        List toFetch = diff.getFetchKnownURIs(true);
                        for (int j = 0; j < toFetch.size(); j++) {
                            SyndieURI uri = (SyndieURI)toFetch.get(j);
                            if (uris.add(uri)) {
                                fetch(archive.getName(), uri);
                            }
                        }
                    }
                }
                break;
            case STRATEGY_PIR:
                for (int i = 0; i < _archives.size(); i++) {
                    NymArchive archive = (NymArchive)_archives.get(i);
                    ArchiveDiff diff = archive.getDiff();
                    if (diff != null) {
                        List toFetch = diff.getFetchPIRURIs();
                        for (int j = 0; j < toFetch.size(); j++) {
                            SyndieURI uri = (SyndieURI)toFetch.get(j);
                            if (uris.add(uri)) {
                                fetch(archive.getName(), uri);
                            }
                        }
                    }
                }
                break;
            case STRATEGY_DELTA:
            default:
                for (int i = 0; i < _archives.size(); i++) {
                    NymArchive archive = (NymArchive)_archives.get(i);
                    ArchiveDiff diff = archive.getDiff();
                    if (diff != null) {
                        List toFetch = diff.getFetchNewURIs(true);
                        for (int j = 0; j < toFetch.size(); j++) {
                            SyndieURI uri = (SyndieURI)toFetch.get(j);
                            if (uris.add(uri)) {
                                fetch(archive.getName(), uri);
                            }
                        }
                    }
                }
                break;
        }
        
        if (push) {
            // ...
        }
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
                FetchRecord rec = (FetchRecord)_fetchRecords.get(i);
                if (rec.getStatus() == status)
                    rv.add(rec);
            }
            return rv;
        }
    }
    
    public void removeFetchRecord(FetchRecord rec) {
        synchronized (_fetchRecords) {
            _fetchRecords.remove(rec);
        }
    }
    
    private void fireFetchStatusUpdated(FetchRecord record) {
        for (int i = 0; i < _listeners.size(); i++) {
            SyndicationListener lsnr = (SyndicationListener)_listeners.get(i);
            lsnr.fetchStatusUpdated(this, record);
        }
    }
    
    public void addListener(SyndicationListener lsnr) { if (!_listeners.contains(lsnr)) _listeners.add(lsnr); }
    public void removeListener(SyndicationListener lsnr) { _listeners.remove(lsnr); }
    
    private static final String SQL_ADD_NYM_ARCHIVE = "INSERT INTO nymArchive (name, uriId, customProxyHost, customProxyPort, lastSyncDate, postKey, postKeySalt, readKey, readKeySalt, nymId) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
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
                postKeyEncr = new byte[32];
                postKeySalt = new byte[32];
                _client.ctx().random().nextBytes(postKeySalt);
                SessionKey key = _client.ctx().keyGenerator().generateSessionKey(postKeySalt, DataHelper.getUTF8(_client.getPass()));
                _client.ctx().aes().encrypt(postKey.getData(), 0, postKeyEncr, 0, key, postKeySalt, SessionKey.KEYSIZE_BYTES);
            }

            byte readKeyEncr[] = null;
            byte readKeySalt[] = null;

            if (readKey != null) {
                readKeyEncr = new byte[32];
                readKeySalt = new byte[32];
                _client.ctx().random().nextBytes(readKeySalt);
                SessionKey key = _client.ctx().keyGenerator().generateSessionKey(readKeySalt, DataHelper.getUTF8(_client.getPass()));
                _client.ctx().aes().encrypt(readKey.getData(), 0, readKeyEncr, 0, key, readKeySalt, SessionKey.KEYSIZE_BYTES);
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

                _archives.add(new NymArchive(name, uri, customProxyHost, customProxyPort, -1, postKey, readKey));
                
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
            postKeyEncr = new byte[32];
            postKeySalt = new byte[32];
            _client.ctx().random().nextBytes(postKeySalt);
            SessionKey key = _client.ctx().keyGenerator().generateSessionKey(postKeySalt, DataHelper.getUTF8(_client.getPass()));
            _client.ctx().aes().encrypt(postKey.getData(), 0, postKeyEncr, 0, key, postKeySalt, SessionKey.KEYSIZE_BYTES);
        }

        byte readKeyEncr[] = null;
        byte readKeySalt[] = null;

        if (readKey != null) {
            readKeyEncr = new byte[32];
            readKeySalt = new byte[32];
            _client.ctx().random().nextBytes(readKeySalt);
            SessionKey key = _client.ctx().keyGenerator().generateSessionKey(readKeySalt, DataHelper.getUTF8(_client.getPass()));
            _client.ctx().aes().encrypt(readKey.getData(), 0, readKeyEncr, 0, key, readKeySalt, SessionKey.KEYSIZE_BYTES);
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
    
    public void stopFetching(SyndieURI uri) {
        synchronized (_fetchRecords) {
            for (int i = 0; i < _fetchRecords.size(); i++) {
                FetchRecord record = (FetchRecord)_fetchRecords.get(i);
                if (record.getURI().equals(uri)) {
                    record.stop();
                    break;
                }
            }
            _fetchRecords.notifyAll();
        }
    }
    
    private static final String SQL_GET_NYM_ARCHIVES = "SELECT name, uriId, customProxyHost, customProxyPort, lastSyncDate, postKey, postKeySalt, readKey, readKeySalt FROM nymArchive WHERE nymId = ? ORDER BY name";
    public void loadArchives() {
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
                
                _archives.add(new NymArchive(name, uri, host, port, (when == null ? -1l : when.getTime()), postKey, readKey));
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
    }
    
    private class NymArchive {
        private String _name;
        private SyndieURI _uri;
        private String _customProxyHost;
        private int _customProxyPort;
        private long _lastSyncDate;
        private SessionKey _postKey;
        private SessionKey _readKey;
        private ArchiveIndex _index;
        private ArchiveDiff _diff;
        private HTTPSyndicator _syndicator;
        
        public NymArchive(String name, SyndieURI uri, String host, int port, long when, byte[] post, byte[] read) {
            this(name, uri, host, port, when, (post != null ? new SessionKey(post) : null), (read != null ? new SessionKey(read) : null));
        }
        public NymArchive(String name, SyndieURI uri, String host, int port, long when, SessionKey post, SessionKey read) {
            _name = name;
            _uri = uri;
            _customProxyHost = host;
            _customProxyPort = port;
            _lastSyncDate = when;
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
        
        public String getName() { return _name; }
        public SyndieURI getURI() { return _uri; }
        public String getCustomProxyHost() { return _customProxyHost; }
        public int getCustomProxyPort() { return _customProxyPort; }
        public long getLastSyncDate() { return _lastSyncDate; }
        public SessionKey getReadKey() { return _readKey; }
        public SessionKey getPostKey() { return _postKey; }
        public ArchiveIndex getIndex() { return _index; }
        public ArchiveDiff getDiff() { return _diff; }
        public void setIndex(ArchiveIndex index) { _index = index; }
        public void setDiff(ArchiveDiff diff) { _diff = diff; }
        public HTTPSyndicator getSyndicator() { return _syndicator; }
        public void setSyndicator(HTTPSyndicator syndicator) { _syndicator = syndicator; }
    }

    public class FetchRecord {
        private String _archiveName;
        private SyndieURI _uri;
        private int _status;
        private String _detail;
        
        public FetchRecord(String name, SyndieURI uri) {
            _archiveName = name;
            _uri = uri;
            _status = FETCH_SCHEDULED;
        }
        
        NymArchive getArchive() { return SyndicationManager.this.getArchive(_archiveName); }
        public SyndieURI getURI() { return _uri; }
        public int getStatus() { return _status; }
        public String getSource() { return _archiveName; }
        void setStatus(int status) { 
            switch (_status) {
                case FETCH_FAILED:
                case FETCH_IMPORT_OK:
                case FETCH_IMPORT_PBE:
                case FETCH_IMPORT_NOKEY:
                case FETCH_IMPORT_CORRUPT:
                case FETCH_STOPPED:
                    return; // already done
                case FETCH_COMPLETE:
                case FETCH_SCHEDULED:
                case FETCH_STARTED:
                default:
                    _status = status;
            }
        }
        /** status message detail */
        public String getDetail() { return _detail; }
        void setDetail(String detail) { _detail = detail; }
        void stop() { 
            switch (_status) {
                case FETCH_FAILED:
                case FETCH_IMPORT_OK:
                case FETCH_IMPORT_PBE:
                case FETCH_IMPORT_NOKEY:
                case FETCH_IMPORT_CORRUPT:
                    return; // already done
                case FETCH_COMPLETE:
                case FETCH_SCHEDULED:
                case FETCH_STARTED:
                default:
                    _status = FETCH_STOPPED;
            }
        }
    }
    
    private class Fetcher implements Runnable {
        public void run() {
            FetchRecord cur = null;
            for (;;) {
                synchronized (_fetchRecords) {
                    for (int i = 0; i < _fetchRecords.size(); i++) {
                        FetchRecord rec = (FetchRecord)_fetchRecords.get(i);
                        if (rec.getStatus() == FETCH_SCHEDULED) {
                            rec.setStatus(FETCH_STARTED);
                            cur = rec;
                            break;
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
                cur = null;
            }
        }
        private void fetch(FetchRecord rec) {
            fireFetchStatusUpdated(rec); // scheduled-->start
            if (rec.getStatus() == FETCH_STOPPED) return;
            NymArchive archive = rec.getArchive();
            HTTPSyndicator template = archive.getSyndicator();
            // the syndicator was built with sequential operation in mind, not multithreaded/reused,
            // so just make another copy for our current sequence
            HTTPSyndicator syndicator = (HTTPSyndicator)template.clone();
            ArrayList uris = new ArrayList(1);
            uris.add(rec.getURI());
            if (rec.getStatus() == FETCH_STOPPED) return;
            boolean fetchComplete = syndicator.fetch(uris);
            if (rec.getStatus() == FETCH_STOPPED) return;
            if (fetchComplete)
                rec.setStatus(FETCH_COMPLETE);
            else
                rec.setStatus(FETCH_FAILED);
            fireFetchStatusUpdated(rec);
            
            if (!fetchComplete)
                return;
            
            if (rec.getStatus() == FETCH_STOPPED) return;
            int importCount = syndicator.importFetched();
            if (importCount == 1) {
                if (rec.getStatus() == FETCH_STOPPED) return;
                if (syndicator.countMissingKeys() >= 1) {
                    rec.setStatus(FETCH_IMPORT_NOKEY);
                } else {
                    rec.setStatus(FETCH_IMPORT_OK);
                }
            } else if (syndicator.countMissingPassphrases() == 1) {
                rec.setDetail(syndicator.getMissingPrompt(0));
                rec.setStatus(FETCH_IMPORT_PBE);
            } else {
                if (rec.getStatus() == FETCH_STOPPED) return;
                rec.setStatus(FETCH_IMPORT_CORRUPT);
            }
            fireFetchStatusUpdated(rec);
        }
    }
    
    private class UIStatusListener implements EepGet.StatusListener {
        public void bytesTransferred(long alreadyTransferred, int currentWrite, long bytesTransferred, long bytesRemaining, String url) {
            _ui.debugMessage("Transferred: " + bytesTransferred);
        }
        public void transferComplete(long alreadyTransferred, long bytesTransferred, long bytesRemaining, String url, String outputFile, boolean notModified) {
            _ui.debugMessage("Transfer complete: " + bytesTransferred);
        }
        public void attemptFailed(String url, long bytesTransferred, long bytesRemaining, int currentAttempt, int numRetries, Exception cause) {
            _ui.debugMessage("Transfer attempt failed: " + bytesTransferred, cause);
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
