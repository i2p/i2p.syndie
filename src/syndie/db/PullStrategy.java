package syndie.db;

/**
 *  The policy for pulling from a particular shared archive
 *
 *  @since 1.102b-11 refactored out of SharedArchiveEngine
 */
public class PullStrategy {

    public PullStrategy() {
        maxKBPerMessage = SharedArchive.DEFAULT_MAX_SIZE_KB;
        maxKBTotal = -1;
        includePrivateMessages = true;
        includePBEMessages = true;
        includeRecentMessagesOnly = SharedArchive.DEFAULT_RECENT_ONLY;
        discoverArchives = true;
        newAgeDays = SharedArchive.DEFAULT_NEWAGE_DAYS;
    }

    public PullStrategy(String serialized) {
        this();
        if (serialized != null) {
            includeDupForPIR = (serialized.indexOf("PIR") != -1);
            discoverArchives = (serialized.indexOf("DontDiscoverArchives") == -1);
            if (!includeDupForPIR) {
                pullNothing = (serialized.indexOf("PullNothing") != -1);
                includeRecentMessagesOnly = (serialized.indexOf("RecentMessagesOnly") != -1);
                includePBEMessages = (serialized.indexOf("DontIncludePBE") == -1);
                includePrivateMessages = (serialized.indexOf("DontIncludePrivate") == -1);
                knownChannelsOnly = (serialized.indexOf("KnownChannelsOnly") != -1);
                pullWhitelistOnly = (serialized.indexOf("PullWhitelistOnly") != -1);
                int maxPerIdx = serialized.indexOf("MaxPerMsg");
                if (maxPerIdx >= 0) {
                    int end = serialized.indexOf(' ', maxPerIdx);
                    if (end > 0) {
                        try {
                            maxKBPerMessage = Integer.parseInt(serialized.substring(maxPerIdx + "MaxPerMsg".length(), end));
                        } catch (NumberFormatException nfe) {}
                    }
                }
                int maxTotIdx = serialized.indexOf("MaxTotal");
                if (maxTotIdx >= 0) {
                    int end = serialized.indexOf(' ', maxTotIdx);
                    if (end > 0) {
                        try {
                            maxKBTotal = Integer.parseInt(serialized.substring(maxTotIdx + "MaxTotal".length(), end));
                        } catch (NumberFormatException nfe) {}
                    }
                }
                int newAgeDaysIdx = serialized.indexOf("NewAgeDays");
                if (newAgeDaysIdx >= 0) {
                    int end = serialized.indexOf(' ', newAgeDaysIdx);
                    if (end > 0) {
                        try {
                            newAgeDays = Integer.parseInt(serialized.substring(newAgeDaysIdx + "NewAgeDays".length(), end));
                        } catch (NumberFormatException nfe) {}
                    }
                }
            }
        }
    }
    /**
     * if a message exceeds this size, do not pull it
     */
    public int maxKBPerMessage;
    /**
     * how much data we will pull across all of the messages (ignoring metadata size
     * and transmission overhead)
     */
    public int maxKBTotal;
    /** 
     * if true, only pull down messages and/or metadata for channels we already
     * know locally
     */
    public boolean knownChannelsOnly;
    /**
     * if true, pull down messages encrypted to forum owner(s) (even if that
     * isn't us)
     */
    public boolean includePrivateMessages;
    /**
     * if true, pull down messages that are encrypted with a passphrase 
     */
    public boolean includePBEMessages;
    /**
     * should we only fetch messages the archive advertises as 'recent', even if they
     * have messages we would otherwise want but are flagged as 'old'?
     */
    public boolean includeRecentMessagesOnly;
    /**
     * if true, we want to use trivial single-database private information retrieval:
     * get everything the archive advertises as "new", and their dependencies, even if
     * we already have some of them.  This way the archive can't tell what we have or
     * what we want (and hence will have a much harder time profiling the user).  If
     * this flag is set, all of the other flags in the strategy are ignored.
     */
    public boolean includeDupForPIR;
    
    /** noop strategy - dont pull anything */
    public boolean pullNothing;
    
    /** 
     * when we talk to an archive, they may tell us about other archives, and if this
     * flag is set, we will add those other archives to our list (though we will NOT
     * schedule them up for syndication, and WILL track what archive told us about it)
     */
    public boolean discoverArchives;
    
    /**
     * when pulling "new" messages, we can either go by what the archive advertises as new
     * or we can expand back the "new" period further (without falling back on !recentMessagesOnly).
     * this specifies the number of days back we want to treat as "new", but if it is less than 1,
     * use the archive's advertised "new" flag
     */
    public int newAgeDays;
    
    /**
     * if there is a whitelist for the archive, try to only pull messages that will match the list,
     * even though this exposes the whitelist to the remote archive.  if false, the messages will be
     * pulled as otherwise determined, but any whitelist will be applied before importing the fetched
     * messages.
     */
    public boolean pullWhitelistOnly;
    
    public String toString() {
        StringBuilder buf = new StringBuilder();
        if (includeDupForPIR) {
            buf.append("PIR ");
        } else if (pullNothing) {
            buf.append("PullNothing ");
        } else {
            if (includeRecentMessagesOnly) buf.append("RecentMessagesOnly ");
            if (includePBEMessages)
                buf.append("IncludePBE ");
            else
                buf.append("DontIncludePBE ");
            if (includePrivateMessages)
                buf.append("IncludePrivate ");
            else
                buf.append("DontIncludePrivate ");
            if (knownChannelsOnly)
                buf.append("KnownChannelsOnly ");
            else
                buf.append("AllChannels ");
            if (maxKBPerMessage >= 0)
                buf.append("MaxPerMsg").append(maxKBPerMessage).append(" ");
            if (maxKBTotal >= 0)
                buf.append("MaxTotal").append(maxKBTotal).append(" ");
            if (newAgeDays > 0)
                buf.append("NewAgeDays").append(newAgeDays).append(" ");
            if (pullWhitelistOnly)
                buf.append("PullWhitelistOnly ");
        }
        if (discoverArchives)
            buf.append("DiscoverArchives ");
        else
            buf.append("DontDiscoverArchives ");
        return buf.toString();
    }
    public String serialize() { return toString(); }
}
