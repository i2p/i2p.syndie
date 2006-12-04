package syndie.db;

import java.io.*;
import java.util.*;
import net.i2p.data.*;

/**
 * describes a channel and all its messages as viewed from one particular index
 */
public class ArchiveChannel {
    private byte[] _scope;
    private long _metaVersion;
    private long _receiveDate;
    private List _messageEntries;
    private List _pseudoAuthorizedMessages;
    private List _unauthMessageEntries;
    private long _knownMessageCount;
    private long _entrySize;
    protected UI _ui;

    public ArchiveChannel(UI ui) {
        _ui = ui;
        _messageEntries = null;
        _pseudoAuthorizedMessages = null;
        _unauthMessageEntries = null;
        _knownMessageCount = -1;
    }
    
    public byte[] getScope() { return _scope; }
    public long getVersion() { return _metaVersion; }
    public long getReceiveDate() { return _receiveDate; }
    public long getEntrySize() { return _entrySize; }
    /** how many messages do we have MessageEntry values for */
    public int getMessageCount() { return (_messageEntries != null ? _messageEntries.size() : 0); }
    /** how many messages does the archive know in the channel in total, even if its not referenced here */
    public long getKnownMessageCount() {
        if ( (_knownMessageCount < 0) && (_messageEntries != null) )
            _knownMessageCount = _messageEntries.size();
        return _knownMessageCount;
    }
    public ArchiveMessage getMessage(int index) { return (ArchiveMessage)_messageEntries.get(index); }
    /**
     * messages that are not authorized at all, not even though channel specific criteria
     */
    public int getUnauthorizedMessageCount() { return (_unauthMessageEntries != null ? _unauthMessageEntries.size() : 0); }
    public ArchiveMessage getUnauthorizedMessage(int index) { return (ArchiveMessage)_unauthMessageEntries.get(index); }
    /**
     * messages that wouldn't typically be authorized, but met some channel specific criteria allowing
     * it to be included, such as "allow replies" and the post is a reply to a normally authorized message
     */
    public int getPseudoAuthorizedMessageCount() { return (_pseudoAuthorizedMessages != null ? _pseudoAuthorizedMessages.size() : 0); }
    public ArchiveMessage getPseudoAuthorizedMessage(int index) { return (ArchiveMessage)_pseudoAuthorizedMessages.get(index); }
    
    void setScope(byte scope[]) { _scope = scope; }
    void setVersion(long version) { _metaVersion = version; }
    void setReceiveDate(long when) { _receiveDate = when; }
    void setMessages(List messages) { _messageEntries = messages; }
    void setPseudoAuthorizedMessages(List messages) { _pseudoAuthorizedMessages = messages; }
    void setUnauthorizedMessages(List messages) { _unauthMessageEntries = messages; }
    void setEntrySize(long size) { _entrySize = size; }

    public void write(OutputStream out, boolean newOnly, boolean chanOnly, boolean includeUnauthorized) throws IOException {            
        try {
            //_ui.debugMessage("Writing channel " + Base64.encode(getScope()) + " (new? " + newOnly + " meta? " + chanOnly + " unauthorized? " + includeUnauthorized + ")");
            //$scopeHash
            out.write(getScope());
            //$metaVersion
            DataHelper.writeLong(out, 4, getVersion());
            //$recvDate
            DataHelper.writeLong(out, 4, getReceiveDate()/24*60*60*1000l);
            //$metadataEntrySize
            DataHelper.writeLong(out, 4, getEntrySize());
            //$numMessages
            DataHelper.writeLong(out, 4, getMessageCount());

            if (chanOnly) {
                // subsequent messages
                DataHelper.writeLong(out, 4, 0);
                // unauthorized/pseudoauthorized messages
                DataHelper.writeLong(out, 4, 0);
            } else {
                //foreach (message)
                int numToWrite = getMessageCount();
                if (includeUnauthorized) {
                    DataHelper.writeLong(out, 4, 0);
                } else {
                    if (newOnly) {
                        numToWrite = 0;
                        for (int j = 0; j < getMessageCount(); j++) {
                            ArchiveMessage msg = getMessage(j);
                            if (msg.getIsNew())
                                numToWrite++;
                        }
                    }
                    DataHelper.writeLong(out, 4, numToWrite);
                    //_ui.debugMessage("Including fully authorized messages: " + numToWrite);
                    for (int j = 0; !includeUnauthorized && j < getMessageCount(); j++) {
                        ArchiveMessage msg = getMessage(j);
                        //    $messageId
                        //    $recvDate
                        //    $entrySize
                        //    $flags {authorized|isReply|isPBE}
                        if (msg.getIsNew() || !newOnly) {
                            DataHelper.writeLong(out, 8, msg.getMessageId());
                            DataHelper.writeLong(out, 4, msg.getReceiveDate()/24*60*60*1000l);
                            DataHelper.writeLong(out, 4, msg.getEntrySize());
                            DataHelper.writeLong(out, 1, msg.getFlags());
                            //_ui.debugMessage("\t" + msg.getPrimaryScope().toBase64() + ":" + msg.getMessageId());
                        }
                    }
                }
                
                // the index either includes unauthorized posts or pseudoauthorized
                // posts
                Map thirdParty = new HashMap();
                if (includeUnauthorized) {
                    //_ui.debugMessage("Including unauthorized messages: " + getUnauthorizedMessageCount());
                    for (int i = 0; i < getUnauthorizedMessageCount(); i++) {
                        ArchiveMessage msg = getUnauthorizedMessage(i);
                        if (!msg.getIsNew() && newOnly)
                            continue;
                        List msgs = (List)thirdParty.get(msg.getPrimaryScope());
                        if (msgs == null) {
                            msgs = new ArrayList();
                            thirdParty.put(msg.getPrimaryScope(), msgs);
                        }
                        msgs.add(msg);
                    }
                } else {
                    //_ui.debugMessage("Including pseudoauthorized messages: " + getPseudoAuthorizedMessageCount());
                    for (int i = 0; i < getPseudoAuthorizedMessageCount(); i++) {
                        ArchiveMessage msg = getPseudoAuthorizedMessage(i);
                        if (!msg.getIsNew() && newOnly)
                            continue;
                        List msgs = (List)thirdParty.get(msg.getPrimaryScope());
                        if (msgs == null) {
                            msgs = new ArrayList();
                            thirdParty.put(msg.getPrimaryScope(), msgs);
                        }
                        msgs.add(msg);
                    }
                }
                DataHelper.writeLong(out, 4, thirdParty.size());
                for (Iterator iter = thirdParty.entrySet().iterator(); iter.hasNext(); ) {
                    Map.Entry entry = (Map.Entry)iter.next();
                    Hash scope = (Hash)entry.getKey();
                    List msgs = (List)entry.getValue();
                    out.write(scope.getData());
                    DataHelper.writeLong(out, 4, msgs.size());
                    for (int i = 0; i < msgs.size(); i++) {
                        ArchiveMessage msg = (ArchiveMessage)msgs.get(i);
                        DataHelper.writeLong(out, 8, msg.getMessageId());
                        DataHelper.writeLong(out, 4, msg.getReceiveDate()/24*60*60*1000L);
                        DataHelper.writeLong(out, 4, msg.getEntrySize());
                        DataHelper.writeLong(out, 1, msg.getFlags());
                        //_ui.debugMessage("\t" + msg.getPrimaryScope().toBase64() + ":" + msg.getMessageId());
                    }
                }
            }
        } catch (DataFormatException dfe) {
            throw new IOException("Invalid number: " + dfe.getMessage());
        }
    }

    public boolean read(InputStream in, boolean includesUnauthorized) throws IOException {
        try {
            byte scope[] = new byte[32];
            int read = DataHelper.read(in, scope);
            if (read <= 0)
                return false;
            if (read != scope.length)
                throw new IOException("Not enough data for the scope (read=" + read + ")");
            Hash scopeHash = new Hash(scope);
            long version = DataHelper.readLong(in, 4);
            long recvDate = DataHelper.readLong(in, 4)*24*60*60*1000l;
            long entrySize = DataHelper.readLong(in, 4);

            long numMsgs = DataHelper.readLong(in, 4);
            int subsequent = (int)DataHelper.readLong(in, 4);
            //_ui.debugMessage("scope: " + scopeHash.toBase64() + " #msgs: " + numMsgs + " included? " + subsequent);
            for (int i = 0; i < subsequent; i++) {
                ArchiveMessage msg = new ArchiveMessage();
                long msgId = DataHelper.readLong(in, 8);
                long msgRecv = DataHelper.readLong(in, 4)*24*60*60*1000l;
                long msgSize = DataHelper.readLong(in, 4);
                int msgFlags = (int)DataHelper.readLong(in, 1);
                msg.setPrimaryScope(scopeHash);
                msg.setMessageId(msgId);
                msg.setReceiveDate(msgRecv);
                msg.setEntrySize(msgSize);
                msg.setFlags(msgFlags);
                if (_messageEntries == null)
                    _messageEntries = new ArrayList();
                _messageEntries.add(msg);
            }

            List thirdParty = new ArrayList();
            int thirdPartyMsgs = (int)DataHelper.readLong(in, 4);
            for (int i = 0; i < thirdPartyMsgs; i++) {
                byte origScope[] = new byte[32];
                if (32 != DataHelper.read(in, origScope))
                    throw new IOException("Not enough data to read the orig scope");
                Hash thirdPartyChan = new Hash(origScope);
                int msgs = (int)DataHelper.readLong(in, 4);
                for (int j = 0; j < msgs; j++) {
                    long curMsgId = DataHelper.readLong(in, 8);
                    long curRecvDate = DataHelper.readLong(in, 4)*24*60*60*1000L;
                    int curEntrySize = (int)DataHelper.readLong(in, 4);
                    int curFlags = (int)DataHelper.readLong(in, 1);
                    ArchiveMessage curMsg = new ArchiveMessage();
                    curMsg.setMessageId(curMsgId);
                    curMsg.setReceiveDate(curRecvDate);
                    curMsg.setEntrySize(curEntrySize);
                    curMsg.setFlags(curFlags);
                    curMsg.setPrimaryScope(thirdPartyChan);
                    thirdParty.add(curMsg);
                }
            }
            if (includesUnauthorized)
                _unauthMessageEntries = thirdParty;
            else
                _pseudoAuthorizedMessages = thirdParty;
            
            _scope = scope;
            _knownMessageCount = numMsgs;
            _metaVersion = version;
            _receiveDate = recvDate;
            _entrySize = entrySize;
            return true;
        } catch (DataFormatException dfe) {
            throw new IOException("Invalid number: " + dfe.getMessage());
        }
    }

    public boolean isKnownMessage(ArchiveMessage msg) { return _messageEntries.contains(msg); }
    public void addMessage(ArchiveMessage msg) { _messageEntries.add(msg); }
    public boolean isKnownPseudoAuthorizedMessage(ArchiveMessage msg) { return _pseudoAuthorizedMessages.contains(msg); }
    public void addPseudoAuthorizedMessage(ArchiveMessage msg) { _pseudoAuthorizedMessages.add(msg); }
    public boolean isKnownUnauthorizedMessage(ArchiveMessage msg) { return _unauthMessageEntries.contains(msg); }
    public void addUnauthorizedMessage(ArchiveMessage msg) { _unauthMessageEntries.add(msg); }
    
    public boolean equals(Object o) {
        if (o instanceof ArchiveChannel) {
            ArchiveChannel chan = (ArchiveChannel)o;
            return DataHelper.eq(chan.getScope(), getScope()) && (chan.getVersion() == getVersion());
        } else {
            return false;
        }
    }
    public int hashCode() { return DataHelper.hashCode(getScope()) ^ (int)getVersion(); }
}
