package syndie.db;

import java.io.File;
import java.io.FilenameFilter;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import net.i2p.data.Base64;
import net.i2p.data.Hash;
import syndie.Constants;
import syndie.data.ExpirationPolicy;
import syndie.data.SyndieURI;
import syndie.data.WatchedChannel;

/**
 * actually execute the expiration policies
 */
public class Expirer {
    private DBClient _client;
    private UI _ui;
    
    private ExpirationPolicy _defaultDBPolicy;
    private ExpirationPolicy _defaultDataFilePolicy;
    private ExpirationPolicy _watchedDBPolicy;
    private ExpirationPolicy _watchedDataFilePolicy;
    private Map _chanIdToDBPolicy;
    private Map _chanIdToDataFilePolicy;
    private SharedArchive _sharedArchive;

    private static final boolean LOG_EXPIRE_CAUSES = false;
    private static final boolean ACTUALLY_DELETE = true;
    
    public Expirer(DBClient client, UI ui) {
        _client = client;
        _ui = ui;
        _chanIdToDBPolicy = new HashMap();
        _chanIdToDataFilePolicy = new HashMap();
    }
    
    public void expireMessages() {
        populateDBMessageSizes();
        loadPolicies();
        loadDataFileSummaries(); // expire by *target* channel, not scope channel, so do some crunching
        Set channelsWithCustomPolicies = _chanIdToDBPolicy.keySet();
        Set watchedChannelIds = getWatchedChannelIds();
        Map chanIdToHash = _client.getChannelIds();
        Set otherChannelIds = new HashSet(chanIdToHash.keySet());
        otherChannelIds.removeAll(watchedChannelIds);
        otherChannelIds.removeAll(channelsWithCustomPolicies);
        
        // now we know the channels to apply the default policy to (otherChannelIds), the
        // watched policy to (watchedChannelIds), and the custom policies to (channelsWithCustomPolicies)
        
        // execute the custom policies
        for (Iterator iter = (new TreeSet(channelsWithCustomPolicies)).iterator(); iter.hasNext(); ) {
            long chanId = ((Long)iter.next()).longValue();
            Hash chan = (Hash)chanIdToHash.get(new Long(chanId));
            ExpirationPolicy dbPolicy = (ExpirationPolicy)_chanIdToDBPolicy.get(new Long(chanId));
            ExpirationPolicy dataFilePolicy = (ExpirationPolicy)_chanIdToDataFilePolicy.get(new Long(chanId));
            if (dbPolicy.getMimicDefault())
                dbPolicy = _defaultDBPolicy;
            if (dataFilePolicy.getMimicDefault())
                dataFilePolicy = _defaultDataFilePolicy;

            executeDBPolicy("custom", chanId, chan, dbPolicy);
            executeDataFilePolicy("custom", chan, chanId, dataFilePolicy);
        }
        
        // execute the watched policies
        for (Iterator iter = (new TreeSet(watchedChannelIds)).iterator(); iter.hasNext(); ) {
            long chanId = ((Long)iter.next()).longValue();
            Hash chan = (Hash)chanIdToHash.get(new Long(chanId));
            ExpirationPolicy dbPolicy = _watchedDBPolicy;
            ExpirationPolicy dataFilePolicy = _watchedDataFilePolicy;
            if (dbPolicy.getMimicDefault())
                dbPolicy = _defaultDBPolicy;
            if (dataFilePolicy.getMimicDefault())
                dataFilePolicy = _defaultDataFilePolicy;

            executeDBPolicy("watched", chanId, chan, dbPolicy);
            executeDataFilePolicy("watched", chan, chanId, dataFilePolicy);
        }
        
        // execute the default policies (for channels still in the database)
        for (Iterator iter = (new TreeSet(otherChannelIds)).iterator(); iter.hasNext(); ) {
            long chanId = ((Long)iter.next()).longValue();
            Hash chan = (Hash)chanIdToHash.get(new Long(chanId));
            ExpirationPolicy dbPolicy = _defaultDBPolicy;
            ExpirationPolicy dataFilePolicy = _defaultDataFilePolicy;
            
            executeDBPolicy("default", chanId, chan, dbPolicy);
            executeDataFilePolicy("default", chan, chanId, dataFilePolicy);
        }
        
        // now we need to deal with channels that are no longer in the database but are in the
        // file system
        Set channelHashesNotInDB = getChannelHashesNotInDB(chanIdToHash.values());
        for (Iterator iter = channelHashesNotInDB.iterator(); iter.hasNext(); ) {
            Hash chan = (Hash)iter.next();
            executeDataFilePolicy("removed", chan, -1, _defaultDataFilePolicy);
        }
        
        _ui.debugMessage("Done expiring messages");
    }
    
    private void loadPolicies() {
        Set policies = _client.getExpirationPolicies();
        for (Iterator iter = policies.iterator(); iter.hasNext(); ) {
            ExpirationPolicy policy = (ExpirationPolicy)iter.next();
            if (policy.isDefaultPolicy()) {
                if (policy.isDBPolicy())
                    _defaultDBPolicy = policy;
                else
                    _defaultDataFilePolicy = policy;
            } else if (policy.isWatchedPolicy()) {
                if (policy.isDBPolicy())
                    _watchedDBPolicy = policy;
                else
                    _watchedDataFilePolicy = policy;
            } else if (policy.getPolicyChannelId() >= 0) { // should always be true if !default and !watched
                if (policy.isDBPolicy())
                    _chanIdToDBPolicy.put(new Long(policy.getPolicyChannelId()), policy);
                else
                    _chanIdToDataFilePolicy.put(new Long(policy.getPolicyChannelId()), policy);
            }
        }
        
        if (_defaultDBPolicy == null) {
            _ui.debugMessage("Warning - no default DB policy");
            ExpirationPolicy ep = new ExpirationPolicy();
            ep.setIsDefaultPolicy();
            ep.setMimicDefault(true);
            ep.setIsDBPolicy();
           _defaultDBPolicy = ep;
        }
        if (_defaultDataFilePolicy == null) {
            _ui.debugMessage("Warning - no default data file policy");
            ExpirationPolicy ep = new ExpirationPolicy();
            ep.setIsDefaultPolicy();
            ep.setMimicDefault(true);
            ep.setIsDataFilePolicy();
           _defaultDataFilePolicy = ep;
        }

        if (_watchedDBPolicy == null)
            _watchedDBPolicy = _defaultDBPolicy;
        if (_watchedDataFilePolicy == null)
            _watchedDataFilePolicy = _defaultDataFilePolicy;
    }
    
    private void loadDataFileSummaries() {
        SharedArchiveBuilder builder = new SharedArchiveBuilder(_client, _ui, new SharedArchive.About());
        builder.setPeriodNew(-1);
        builder.setHideLocalHours(0);
        _sharedArchive = builder.buildSharedArchive();
    }
    
    class ChannelData {
        ArrayList ids = new ArrayList();
        ArrayList dates = new ArrayList();
        ArrayList sizes = new ArrayList();
        ArrayList uris = new ArrayList();
        long totalSizeKB = 0;
    }
    
    private static final String SQL_GET_TO_EXPIRE = "SELECT msgId, importDate, totalMessageSizeKB FROM channelMessage WHERE deletionCause IS NULL AND isCancelled = FALSE AND targetChannelId = ?";
    /** get the db channel summary */
    private ChannelData getChannelData(long chanId, String chan) {
        ChannelData data = new ChannelData();
        
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _client.con().prepareStatement(SQL_GET_TO_EXPIRE);
            stmt.setLong(1, chanId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                long msgId = rs.getLong(1);
                Date importDate = rs.getDate(2);
                int size = rs.getInt(3);
                
                data.totalSizeKB += size;
                
                data.ids.add(new Long(msgId));
                data.dates.add(new Long(importDate.getTime()));
                data.sizes.add(new Integer(size));
                data.uris.add(new Long(msgId));
            }
        } catch (SQLException se) {
            _ui.errorMessage("Error listing messages to expire", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        if (data.totalSizeKB > 0)
            _ui.debugMessage("db channel data for " + chan + "/" + chanId + ": total size: " + data.totalSizeKB + "KB, messages: " + data.ids.size());
        return data;
    }
    
    /** get the data file channel summary */
    private ChannelData getChannelData(Hash chan, String id) {
        ChannelData data = new ChannelData();
        if (chan == null) return data;
        
        ArrayList msgSummaries = _sharedArchive.getTargetMessages(chan);
        for (int i = 0; i < msgSummaries.size(); i++) {
            SharedArchive.Message msg = (SharedArchive.Message)msgSummaries.get(i);
            
            long messageId = msg.getMessageId();
            long date = msg.getLocalMessageDate();
            int size = msg.getExactSizeKB();
            
            SharedArchive.Channel scope = _sharedArchive.getChannels()[msg.getScopeIndex()];
            SyndieURI uri = SyndieURI.createMessage(Hash.create(scope.getScope()), messageId);
            
            data.totalSizeKB += size;
            
            data.ids.add(new Long(messageId));
            data.dates.add(new Long(date));
            data.sizes.add(new Integer(size));
            data.uris.add(uri);
        }
        
        if (data.totalSizeKB > 0)
            _ui.debugMessage("df channel data for " + chan + "/" + id + ": total size: " + data.totalSizeKB + "KB, messages: " + data.ids.size());

        return data;
    }
    
    /** @param policy non-null */
    private void executeDBPolicy(String policyType, long chanId, Hash chan, ExpirationPolicy policy) {
        if (policy == null) {
            // policies set in loadPolicies() with fallbacks if DB doesn't return them,
            // shouldn't happen any more
            _ui.errorMessage("Null policy", new Exception("I did it"));
            return;
        }
        ChannelData data = getChannelData(chanId, chan+"");
        Map idsToExpire = selectToExpire(data, policy);
        if (idsToExpire.size() > 0) {
            //_ui.debugMessage("Executing " + policyType + " db policy for channel " + chanId + "/" + chan);
            deleteDBMessages(chanId, chan, new TreeSet(idsToExpire.keySet()));
        }
    }

    /** @param policy non-null */
    private void executeDataFilePolicy(String policyType, Hash chan, long chanId, ExpirationPolicy policy) {
        if (policy == null) {
            // policies set in loadPolicies() with fallbacks if DB doesn't return them,
            // shouldn't happen any more
            _ui.errorMessage("Null policy", new Exception("I did it"));
            return;
        }
        ChannelData data = getChannelData(chan, chanId + "");
        Map idsToExpire = selectToExpire(data, policy);
        if (idsToExpire.size() > 0) {
            //_ui.debugMessage("Executing " + policyType + " data file policy for channel " + chan);
            deleteDataFileMessages(chan, chanId, idsToExpire);
        }
    }
    
    /** @param policy non-null */
    private Map selectToExpire(ChannelData data, ExpirationPolicy policy) {
        int maxDays = policy.getMaxAgeDays();
        long maxMsgs = policy.getMaxNumMessages();
        int maxSizeKB = policy.getMaxSizeKB();
        
        HashMap idsToExpire = new HashMap();
        
        // expire by date first (since that applies regardless of the other filters), then by
        // the max number of messages (since that isn't affected by the maxSize attribute), and then
        // finally by the max size (deleting the oldest remaining ones first until the size limit is met)
        
        if (maxDays > 0) {
            long now = System.currentTimeMillis();
            long dayBegin = now - (now % (24*60*60*1000l));
            long earliestImportDate = dayBegin - (24*60*60*1000l)*maxDays;
            
            for (int i = 0; i < data.ids.size(); i++) {
                Long id = (Long)data.ids.get(i);
                Long impDate = (Long)data.dates.get(i);
                
                if (impDate.longValue() < earliestImportDate) {
                    data.ids.remove(i);
                    data.dates.remove(i);
                    Object uri = data.uris.remove(i);
                    Integer sizeKB = (Integer)data.sizes.remove(i);
                    data.totalSizeKB -= sizeKB.intValue();
                    
                    int importDays = (int)((dayBegin - impDate.longValue()) / (24*60*60*1000l));

                    if (LOG_EXPIRE_CAUSES)
                        _ui.debugMessage("Expiring message " + id + " because it was imported too long ago (" + importDays + " days) vs the limit (" + maxDays + " days)");

                    idsToExpire.put(id, uri);
                    i--;
                }
            }
        }
        
        if (maxMsgs > 0) {
            while (data.ids.size() > maxMsgs) {
                Long id = (Long)data.ids.remove(0);
                Long impDate = (Long)data.dates.remove(0);
                Object uri = data.uris.remove(0);
                Integer size = (Integer)data.sizes.remove(0);
                data.totalSizeKB -= size.intValue();
                
                if (LOG_EXPIRE_CAUSES)
                    _ui.debugMessage("Expiring message " + id + " because the total number of messages (" + data.ids.size() + ") still exceedsthe max number of messages (" + maxMsgs + ")");
                
                idsToExpire.put(id, uri);
            }
        }

        if (maxSizeKB > 0) {
            while (data.totalSizeKB > maxSizeKB) {
                Long id = (Long)data.ids.remove(0);
                Long impDate = (Long)data.dates.remove(0);
                Integer size = (Integer)data.sizes.remove(0);
                Object uri = data.uris.remove(0);

                if (LOG_EXPIRE_CAUSES)
                    _ui.debugMessage("Expiring message " + id + " because the total size (" + data.totalSizeKB + "KB) still exceeds the max size (" + maxSizeKB + "KB)");
                
                data.totalSizeKB -= size.intValue();
                idsToExpire.put(id, uri);
            }
        }
        
        return idsToExpire;
    }
    
    private void deleteDBMessages(long chanId, Hash chan, Collection msgIds) {
        if (msgIds.size() > 0) {
            _ui.debugMessage("Messages to expire in " + chan + "/" + chanId + " for the db policy: " + msgIds);
            if (ACTUALLY_DELETE) {
                for (Iterator iter = msgIds.iterator(); iter.hasNext(); ) {
                    Long msgId = (Long)iter.next();
                    Exception err = _client.expireMessageFromDB(msgId.longValue());
                    if (err != null)
                        _ui.errorMessage("Error deleting expired message from the db: " + msgId, err);
                }
            }
        }
    }
    
    private void deleteDataFileMessages(Hash chan, long chanId, Map ids) {
        if (ids.size() > 0) {
            _ui.debugMessage("Messages to expire in " + chan + "/" + chanId + " for the data file policy: " + ids.values());
            if (ACTUALLY_DELETE) {
                // we figure out what to delete by target channel id, but the files are stored by
                // scope channel id (which is why we keep track of the uris so we can pick the right dir)
                for (Iterator iter = ids.values().iterator(); iter.hasNext(); ) {
                    SyndieURI uri = (SyndieURI)iter.next();
                    File chanDir = new File(_client.getArchiveDir(), uri.getScope().toBase64());
                    Long messageId = uri.getMessageId();
                    File msgFile = new File(chanDir, messageId.toString() + Constants.FILENAME_SUFFIX);
                    if (msgFile.exists()) {
                        msgFile.delete();
                        long scopeId = _client.getChannelId(uri.getScope());
                        long msgId = _client.getMessageId(scopeId, messageId.longValue());
                        if (msgId >= 0) {
                            boolean decrypted = _client.getMessageDecrypted(msgId);
                            if (!decrypted) {
                                // since we haven't yet decrypted it, the db doesn't really hold any
                                // data, and since we are expiring the data file necessary to decrypt it,
                                // lets mark the undecrypted message as expired too
                                _client.expireMessageFromDB(msgId);
                                _ui.debugMessage("Running extra db expiration for " + msgId + "/" + uri + " since it wasn't decrypted yet");
                            }
                        }
                    }
                }
            }
        }
    }
    
    private Set getChannelHashesNotInDB(Collection dbHashes) {
        final Set dbHashStrings = new HashSet();
        for (Iterator iter = dbHashes.iterator(); iter.hasNext(); )
            dbHashStrings.add(((Hash)iter.next()).toBase64());
        
        File archiveDir = _client.getArchiveDir();
        String otherDirs[] = archiveDir.list(new FilenameFilter() {
            public boolean accept(File dir, String name) {
                if (".".equals(name) || "..".equals(name))
                    return false;
                return !dbHashStrings.contains(name);
            }
        });
        
        Set rv = new HashSet();
        for (int i = 0; i < otherDirs.length; i++) {
            byte val[] = Base64.decode(otherDirs[i]);
            if ( (val != null) && (val.length == Hash.HASH_LENGTH) )
                rv.add(Hash.create(val));
        }
        return rv;
    }
    
    private Set getWatchedChannelIds() {
        Set rv = new HashSet();
        List channels = _client.getWatchedChannels();
        for (int i = 0; i < channels.size(); i++) {
            WatchedChannel chan = (WatchedChannel)channels.get(i);
            rv.add(new Long(chan.getChannelId()));
        }
        return rv;
    }
    
    /** 
     * old messages may not yet have their totalMessageSizeKB fields set, so lets populate those
     * ones before continuing on
     */
    private void populateDBMessageSizes() {
        List msgIds = getSizelessMessages();
        for (int i = 0; i < msgIds.size(); i++) {
            long msgId = ((Long)msgIds.get(i)).longValue();
            setMessageSize(msgId, calculateMessageSizeKB(msgId));
        }
    }
    
    private static final String SQL_GET_SIZELESS_MSGS = "SELECT msgId FROM channelMessage WHERE totalMessageSizeKB is NULL AND isCancelled = FALSE AND deletionCause IS NULL";
    private List getSizelessMessages() {
        ArrayList rv = new ArrayList();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _client.con().prepareStatement(SQL_GET_SIZELESS_MSGS);
            rs = stmt.executeQuery();
            while (rs.next()) {
                long id = rs.getLong(1);
                if ( (!rs.wasNull()) && (id >= 0) )
                    rv.add(new Long(id));
            }
        } catch (SQLException se) {
            _ui.errorMessage("Error listing sizeless messages", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return rv;
    }
    
    private static final String SQL_GET_SIZE_ATTACHMENTS = "SELECT SUM(attachmentSize) FROM messageAttachment WHERE msgId = ?";
    private static final String SQL_GET_SIZE_PAGES = "SELECT SUM(LENGTH(dataString)) FROM messagePageData WHERE msgId = ?";
    private static final String SQL_GET_SIZE_AVATAR = "SELECT LENGTH(avatarData) FROM messageAvatar WHERE msgId = ?";
    private int calculateMessageSizeKB(long msgId) {
        long attachmentSize = calculateSize(SQL_GET_SIZE_ATTACHMENTS, msgId);
        long pageSize = calculateSize(SQL_GET_SIZE_PAGES, msgId);
        long msgAvatarSize = calculateSize(SQL_GET_SIZE_AVATAR, msgId);
        long otherSize = 1024;
        
        return (int)((attachmentSize + pageSize + msgAvatarSize + otherSize + 1023)/1024);
    }
    
    private long calculateSize(String query, long msgId) {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _client.con().prepareStatement(query);
            stmt.setLong(1, msgId);
            rs = stmt.executeQuery();
            if (rs.next()) {
                long val = rs.getLong(1);
                if (!rs.wasNull())
                    return val;
            }
        } catch (SQLException se) {
            _ui.errorMessage("Error calculating size [" + query + "]", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return 0;
    }
    
    private static final String SQL_SET_MSG_SIZE = "UPDATE channelMessage SET totalMessageSizeKB = ? WHERE msgId = ?";
    private void setMessageSize(long msgId, int sizeKB) {
        try {
            _client.exec(SQL_SET_MSG_SIZE, sizeKB, msgId);
        } catch (SQLException se) {
            _ui.errorMessage("error setting the message size", se);
        }
    }
}
