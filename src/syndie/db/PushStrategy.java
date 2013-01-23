package syndie.db;

/**
 *  The policy for pushing to a particular shared archive
 *
 *  @since 1.102b-11 refactored out of SharedArchiveEngine
 */
public class PushStrategy {

    public PushStrategy() {
        maxKBPerMessage = SharedArchive.DEFAULT_MAX_SIZE_KB;
        maxKBTotal = -1;
        sendMaxAge = 7;
    }

    public PushStrategy(String serialized) {
        this();
        if (serialized != null) {
            sendNothing = (serialized.indexOf("SendNothing") != -1);
            sendLocalNewOnly = !sendNothing && (serialized.indexOf("LocalNewOnly") != -1);
            sendHashcashForAll = !sendNothing && (serialized.indexOf("HCForAll") != -1);
            sendHashcashForLocal = !sendNothing && (serialized.indexOf("HCForLocal") != -1);
            
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
            int maxAgeIdx = serialized.indexOf("MaxAge");
            if (maxAgeIdx >= 0) {
                int end = serialized.indexOf(' ', maxAgeIdx);
                if (end > 0) {
                    try {
                        sendMaxAge = Integer.parseInt(serialized.substring(maxAgeIdx + "MaxAge".length(), end));
                    } catch (NumberFormatException nfe) {}
                }
            }
        }
    }
    /**
     * if a message exceeds this size, do not push it
     */
    public int maxKBPerMessage;
    /**
     * how much data we will push across all of the messages (ignoring metadata size
     * and transmission overhead)
     */
    public int maxKBTotal;
    /**
     * if true, generate hashcash certs for locally created messages that
     * the remote archive wants certs for
     */
    public boolean sendHashcashForLocal;
    /**
     * if true, generate hashcash certs for all messages that
     * the remote archive wants certs for
     */
    public boolean sendHashcashForAll;
    /**
     * only send messages we generated locally in the last few days that
     * the archive doesnt have yet.  this has obvious anonymity attributes
     */
    public boolean sendLocalNewOnly;
    /** noop strategy - dont send anything */
    public boolean sendNothing;
    /** max age in days of things to send - less than 1 means infinite */
    public int sendMaxAge;
    
    public String toString() {
        StringBuilder buf = new StringBuilder();
        if (sendNothing) {
            buf.append("SendNothing ");
        } else {
            if (sendHashcashForLocal) buf.append("HCForLocal ");
            if (sendHashcashForAll) buf.append("HCForAll ");
            if (sendLocalNewOnly)
                buf.append("LocalNewOnly ");
            else
                buf.append("AllDiff ");
            if (maxKBPerMessage >= 0)
                    buf.append("MaxPerMsg").append(maxKBPerMessage).append(" ");
            if (maxKBTotal >= 0)
                buf.append("MaxTotal").append(maxKBTotal).append(" ");
            if (sendMaxAge >= 0)
                buf.append("MaxAge").append(sendMaxAge).append(" ");
        }
        return buf.toString();
    }
    public String serialize() { return toString(); }
}
