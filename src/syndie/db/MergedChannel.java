package syndie.db;

/**
 *
 */
public class MergedChannel extends ArchiveChannel {
    private ArchiveChannel _channel;
    private String _source;
    public MergedChannel(ArchiveChannel chan, String source) {
        super(chan._ui);
        _channel = chan;
        _source = source;
    }
    
    public String getSource() { return _source; }
    public void setSource(String source) { _source = source; }
    
    public byte[] getScope() { return _channel.getScope(); }
    public long getVersion() { return _channel.getVersion(); }
    public long getReceiveDate() { return _channel.getReceiveDate(); }
    public long getEntrySize() { return _channel.getEntrySize(); }
    public int getMessageCount() { return _channel.getMessageCount(); }
    public long getKnownMessageCount() { return _channel.getKnownMessageCount(); }
    public ArchiveMessage getMessage(int index) { return new MergedMessage(_channel.getMessage(index), _source); }
    public int getUnauthorizedMessageCount() { return _channel.getUnauthorizedMessageCount(); }
    public ArchiveMessage getUnauthorizedMessage(int index) { return new MergedMessage(_channel.getUnauthorizedMessage(index), _source); }
    public int getPseudoAuthorizedMessageCount() { return _channel.getPseudoAuthorizedMessageCount(); }
    public ArchiveMessage getPseudoAuthorizedMessage(int index) { return new MergedMessage(_channel.getPseudoAuthorizedMessage(index), _source); }
}
