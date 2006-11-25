package syndie.db;

import net.i2p.data.Hash;

/**
 *
 */
public class MergedMessage extends ArchiveMessage {
    private ArchiveMessage _msg;
    private String _source;
    public MergedMessage(ArchiveMessage msg, String source) {
        super();
        _msg = msg;
        _source = source;
    }

    public long getMessageId() { return _msg.getMessageId(); }
    public long getReceiveDate() { return _msg.getReceiveDate(); }
    public long getEntrySize() { return _msg.getEntrySize(); }
    public boolean getIsNew() { return _msg.getIsNew(); }
    public boolean getIsAuthorized() { return _msg.getIsAuthorized(); }
    public boolean getIsReply() { return _msg.getIsReply(); }
    public boolean getIsPasswordProtected() { return _msg.getIsPasswordProtected(); }
    public int getFlags() { return _msg.getFlags(); }
    public Hash getPrimaryScope() { return _msg.getPrimaryScope(); }
    
    public String getSource() { return _source; }
}
