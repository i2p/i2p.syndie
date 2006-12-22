package syndie.db;

import java.io.File;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import net.i2p.data.DataHelper;
import syndie.data.SyndieURI;

/**
 *
 */
public class SyndicationManagerScheduler implements SyndicationManager.SyndicationListener {
    private DBClient _client;
    private SyndicationManager _manager;
    private UI _ui;
    private boolean _alive;
    
    public SyndicationManagerScheduler(DBClient client, UI ui, SyndicationManager mgr) {
        _client = client;
        _ui = ui;
        _manager = mgr;
        _alive = false;
        mgr.addListener(this);
    }
    
    public void startScheduling() {
        _alive = true;
        Thread t = new Thread(new Runner());
        t.setDaemon(true);
        t.setName("Syndication scheduler");
        t.start();
    }
    
    public void stopScheduling() {
        _alive = false;
        synchronized (this) {
            notifyAll();
        }
    }
    public void scheduleUpdated() { synchronized (this) { notifyAll(); } }
    
    private class Runner implements Runnable {
        public void run() {
            while (_alive) {
                File sharedIndex = new File(_client.getArchiveDir(), SyndicationManager.SHARED_INDEX_FILE);
                if (!sharedIndex.exists()) {
                    _manager.buildIndex(_client, _ui);
                } else if (sharedIndex.lastModified() + _manager.getLocalRebuildDelayHours()*60*60*1000L < System.currentTimeMillis()) {
                    _manager.buildIndex(_client, _ui);
                }
                
                String archive = getNextArchive();
                if (archive != null) {
                    _ui.debugMessage("fireSync(" + archive + ")");
                    _manager.fetchIndex(archive);
                } else {
                    //_ui.debugMessage("No archives due for sync.  waiting...");
                    try { 
                        synchronized (SyndicationManagerScheduler.this) {
                            SyndicationManagerScheduler.this.wait(_client.ctx().random().nextLong(60*1000));
                        }
                    } catch (InterruptedException ie) {}
                }
            }
        }
    }
    
    private static final String SQL_GET_NEXT_ARCHIVE = "SELECT name FROM nymArchive WHERE nymId = ? AND nextSyncDate IS NOT NULL AND nextSyncDate <= NOW() ORDER BY nextSyncDate ASC";
    private String getNextArchive() {
        String rv = null;
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _client.con().prepareStatement(SQL_GET_NEXT_ARCHIVE);
            stmt.setLong(1, _client.getLoggedInNymId());
            rs = stmt.executeQuery();
            if (rs.next())
                rv = rs.getString(1);
            rs.close();
            rs = null;
            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            _ui.errorMessage("Error fetching next archive", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        if (rv != null) _manager.setNextSync(rv, -1); // so we dont try it again until sync completion
        return rv;
    }
    
    private void sync(final String archiveName) {
        int idx = _manager.getArchiveNum(archiveName);
        final SharedArchive archive = _manager.getArchiveIndex(idx);
        if (archive == null) return;
        _ui.debugMessage("attempting sync for " + archiveName);
        _manager.push(archiveName);
        _manager.pull(archiveName, new Runnable() { 
            public void run() { 
                _manager.setLastSync(archiveName, System.currentTimeMillis());
                _ui.debugMessage("Archive sync successful for " + archiveName);
                rescheduleSync(archiveName, archive.getAbout().getPublishRebuildFrequencyHours());
            }
        }, new Runnable() {
            public void run() {
                int failures = _manager.getConsecutiveFailures(archiveName);
                int delayHours = (failures + 1) * archive.getAbout().getPublishRebuildFrequencyHours();
                _ui.debugMessage("Archive sync failed for " + archiveName);
                rescheduleSync(archiveName, delayHours);
            }
        });
    }
    
    private void rescheduleSync(String archiveName, int minDelayHours) {
        long delay = minDelayHours*60*60*1000L;
        delay += _client.ctx().random().nextLong(delay*2);
        if (delay > 36*60*60*1000L)
            delay = 12*60*60*1000L + _client.ctx().random().nextLong(24*60*60*1000L);
        _ui.debugMessage("Rescheduling sync for " + archiveName + ": time to next sync: " + DataHelper.formatDuration(delay));
        _manager.setNextSync(archiveName, delay+System.currentTimeMillis());
    }
    
    public void archiveIndexStatus(SyndicationManager mgr, final SyndicationManager.StatusRecord record) {
        if (record.isTerminal()) {
            if (record.getStatus() == SyndicationManager.FETCH_INDEX_DIFF_OK) {
                JobRunner.instance().enqueue(new Runnable() {
                    public void run() { sync(record.getSource()); }
                });
            } else if (record.isError()) {
                int failures = _manager.incrementConsecutiveFailures(record.getSource());
                int freqHours = 24;
                SharedArchive archive = _manager.getArchiveIndex(_manager.getArchiveNum(record.getSource()));
                if (archive != null)
                    freqHours = archive.getAbout().getPublishRebuildFrequencyHours();
                int numHours = (failures+1) * freqHours;
                _ui.statusMessage("Error fetching the index from " + record.getSource() + " (" + failures + " consecutive failure)");
                if (failures > 12)
                    _manager.setNextSync(record.getSource(), -1);
                else
                    rescheduleSync(record.getSource(), numHours);
            }
        }
    }

    public void archiveAdded(SyndicationManager mgr, String name) { synchronized (this) { notifyAll(); } }
    public void archiveRemoved(SyndicationManager mgr, String name) { synchronized (this) { notifyAll(); } }
    public void archiveUpdated(SyndicationManager mgr, String oldName, String newName) { synchronized (this) { notifyAll(); } }
    public void archivesLoaded(SyndicationManager mgr) { synchronized (this) { notifyAll(); } }
    public void fetchStatusUpdated(SyndicationManager mgr, SyndicationManager.StatusRecord record) {
        if (record.isTerminal() && record.isError() && record.getURI().isArchive()) {
            _ui.statusMessage("fetch status error / " + record.getSource() + " / " + record.getDetail() + " / " + record.getStatus());    
        }
    }
    public void syndicationComplete(SyndicationManager mgr) {}
}
