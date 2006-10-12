package syndie.db;

import net.i2p.data.*;

/**
* describes a message in an archive index
*/
public class ArchiveMessage {
    private long _messageId;
    private long _recvDate;
    private long _entrySize;
    private int _flags;
    private boolean _isNew;
    private Hash _primaryScope;
    
    /** is the post authorized */
    static final int MASK_AUTHORIZED = 1 << 7;
    /** is the post a privately encrypted reply */
    static final int MASK_REPLY = 1 << 6;
    /** is the post encrypted with password based encryption */
    static final int MASK_PBE = 1 << 5;
    /** the archive considers the post 'new' */
    static final int MASK_NEW = 1 << 4;

    public long getMessageId() { return _messageId; }
    public long getReceiveDate() { return _recvDate; }
    public long getEntrySize() { return _entrySize; }
    public boolean getIsNew() { return ((_flags & MASK_NEW) != 0); }
    public boolean getIsAuthorized() { return ((_flags & MASK_AUTHORIZED) != 0); }
    public boolean getIsReply() { return ((_flags & MASK_REPLY) != 0); }
    public boolean getIsPasswordProtected() { return ((_flags & MASK_PBE) != 0); }
    public int getFlags() { return _flags; }
    /** channel that 'owns' the message (not necessary for authorized posts) */
    public Hash getPrimaryScope() { return _primaryScope; }

    void setMessageId(long id) { _messageId = id; }
    void setReceiveDate(long when) { _recvDate = when; }
    void setEntrySize(long size) { _entrySize = size; }
    void setFlags(int flags) { _flags = flags; }
    void setPrimaryScope(Hash channel) { _primaryScope = channel; }
}
