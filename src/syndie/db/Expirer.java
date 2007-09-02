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
import net.i2p.data.Base64;
import net.i2p.data.Hash;
import syndie.Constants;
import syndie.data.ExpirationPolicy;
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

    private static final boolean LOG_EXPIRE_CAUSES = false;
    private static final boolean ACTUALLY_DELETE = false;
    
    public Expirer(DBClient client, UI ui) {
        _client = client;
        _ui = ui;
        _chanIdToDBPolicy = new HashMap();
        _chanIdToDataFilePolicy = new HashMap();
    }
    
    public void expireMessages() {
        populateDBMessageSizes();
        loadPolicies();
        Set channelsWithCustomPolicies = _chanIdToDBPolicy.keySet();
        Set watchedChannelIds = getWatchedChannelIds();
        Map chanIdToHash = _client.getChannelIds();
        Set otherChannelIds = new HashSet(chanIdToHash.keySet());
        otherChannelIds.removeAll(watchedChannelIds);
        otherChannelIds.removeAll(channelsWithCustomPolicies);
        
        // now we know the channels to apply the default policy to (otherChannelIds), the
        // watched policy to (watchedChannelIds), and the custom policies to (channelsWithCustomPolicies)
        
        // execute the custom policies
        for (Iterator iter = channelsWithCustomPolicies.iterator(); iter.hasNext(); ) {
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
        for (Iterator iter = watchedChannelIds.iterator(); iter.hasNext(); ) {
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
        for (Iterator iter = otherChannelIds.iterator(); iter.hasNext(); ) {
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
    }
    
    class ChannelData {
        ArrayList ids = new ArrayList();
        ArrayList dates = new ArrayList();
        ArrayList sizes = new ArrayList();
        long totalSizeKB = 0;
    }
    
    private static final String SQL_GET_TO_EXPIRE = "SELECT msgId, importDate, totalMessageSizeKB FROM channelMessage WHERE deletionCause IS NULL AND pbePrompt IS NULL AND replyKeyMissing = FALSE AND readKeyMissing = FALSE AND isCancelled = FALSE AND targetChannelId = ?";
    /** get the db channel summary */
    private ChannelData getChannelData(long chanId) {
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
            }
        } catch (SQLException se) {
            _ui.errorMessage("Error listing messages to expire", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        _ui.debugMessage("channel data for " + chanId + ": total size: " + data.totalSizeKB + "KB, messages: " + data.ids.size());
        return data;
    }
    
    /** get the data file channel summary */
    private ChannelData getChannelData(Hash chan) {
        ChannelData data = new ChannelData();
        if (chan == null) return data;
        if (true) {
            _ui.errorMessage("ugh, the data file expiration is wrong.");
            _ui.errorMessage("we want to expire based on the target channel, not the scope channel, so");
            _ui.errorMessage("we need to read in the enclosure headers of ALL channels first, then sort them by target channel");
            _ui.errorMessage("so, for now, this just pretends that no data files are expirable");
            return data;
        }
        
        File chanDir = new File(_client.getArchiveDir(), chan.toBase64());
        if (!chanDir.exists() || !chanDir.isDirectory())
            return data;
        
        File msgFiles[] = chanDir.listFiles(new FilenameFilter() {
            public boolean accept(File dir, String name) {
                return (!name.equals("meta" + Constants.FILENAME_SUFFIX)) && name.endsWith(Constants.FILENAME_SUFFIX);
            }
        });
        
        for (int i = 0; i < msgFiles.length; i++) {
            long messageId = SharedArchiveBuilder.getMessageId(msgFiles[i]);
            long date = msgFiles[i].lastModified();
            int size = (int)((msgFiles[i].length()+1023)/1024);
            
            data.totalSizeKB += size;
            
            data.ids.add(new Long(messageId));
            data.dates.add(new Long(date));
            data.sizes.add(new Integer(size));
        }

        _ui.debugMessage("channel data for " + chan + ": total size: " + data.totalSizeKB + "KB, messages: " + data.ids.size());

        return data;
    }
    
    private void executeDBPolicy(String policyType, long chanId, Hash chan, ExpirationPolicy policy) {
        ChannelData data = getChannelData(chanId);
        ArrayList idsToExpire = selectToExpire(data, policy);
        if (idsToExpire.size() > 0) {
            //_ui.debugMessage("Executing " + policyType + " db policy for channel " + chanId + "/" + chan);
            deleteDBMessages(chanId, chan, idsToExpire);
        }
    }
    private void executeDataFilePolicy(String policyType, Hash chan, long chanId, ExpirationPolicy policy) {
        ChannelData data = getChannelData(chan);
        ArrayList idsToExpire = selectToExpire(data, policy);
        if (idsToExpire.size() > 0) {
            //_ui.debugMessage("Executing " + policyType + " data file policy for channel " + chan);
            deleteDataFileMessages(chan, chanId, idsToExpire);
        }
    }
    
    private ArrayList selectToExpire(ChannelData data, ExpirationPolicy policy) {
        int maxDays = policy.getMaxAgeDays();
        long maxMsgs = policy.getMaxNumMessages();
        int maxSizeKB = policy.getMaxSizeKB();
        
        ArrayList idsToExpire = new ArrayList();
        
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
                    Integer sizeKB = (Integer)data.sizes.remove(i);
                    data.totalSizeKB -= sizeKB.intValue();
                    
                    int importDays = (int)((dayBegin - impDate.longValue()) / (24*60*60*1000l));

                    if (LOG_EXPIRE_CAUSES)
                        _ui.debugMessage("Expiring message " + id + " because it was imported too long ago (" + importDays + " days) vs the limit (" + maxDays + " days)");

                    idsToExpire.add(id);
                    i--;
                }
            }
        }
        
        if (maxMsgs > 0) {
            while (data.ids.size() > maxMsgs) {
                Long id = (Long)data.ids.remove(0);
                Long impDate = (Long)data.dates.remove(0);
                Integer size = (Integer)data.sizes.remove(0);
                data.totalSizeKB -= size.intValue();
                
                if (LOG_EXPIRE_CAUSES)
                    _ui.debugMessage("Expiring message " + id + " because the total number of messages (" + data.ids.size() + ") still exceedsthe max number of messages (" + maxMsgs + ")");
                
                idsToExpire.add(id);
            }
        }

        if (maxSizeKB > 0) {
            while (data.totalSizeKB > maxSizeKB) {
                Long id = (Long)data.ids.remove(0);
                Long impDate = (Long)data.dates.remove(0);
                Integer size = (Integer)data.sizes.remove(0);

                if (LOG_EXPIRE_CAUSES)
                    _ui.debugMessage("Expiring message " + id + " because the total size (" + data.totalSizeKB + "KB) still exceeds the max size (" + maxSizeKB + "KB)");
                
                data.totalSizeKB -= size.intValue();
                idsToExpire.add(id);
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
    
    private void deleteDataFileMessages(Hash chan, long chanId, Collection ids) {
        if (ids.size() > 0) {
            _ui.debugMessage("Messages to expire in " + chan + "/" + chanId + " for the data file policy: " + ids);
            if (ACTUALLY_DELETE) {
                File chanDir = new File(_client.getArchiveDir(), chan.toBase64());
                for (Iterator iter = ids.iterator(); iter.hasNext(); ) {
                    Long messageId = (Long)iter.next();
                    File msgFile = new File(chanDir, messageId.toString() + Constants.FILENAME_SUFFIX);
                    if (msgFile.exists())
                        msgFile.delete();
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
                rv.add(new Hash(val));
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
    
    private static final String SQL_GET_SIZELESS_MSGS = "SELECT msgId FROM channelMessage WHERE totalMessageSizeKB is NULL AND isCancelled = FALSE AND readKeyMissing = FALSE AND replyKeyMissing = FALSE AND pbePrompt IS NULL AND deletionCause IS NULL";
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
