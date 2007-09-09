package syndie.data;

public class ExpirationPolicy {
    private boolean _isDataFilePolicy;
    private boolean _isWatched;
    private boolean _isDefault;
    private long _channelId;
    private long _maxNumMessages;
    private int _maxSizeKB;
    private int _maxAgeDays;
    private boolean _mimicDefault;
    private boolean _isNew;

    public ExpirationPolicy() {
        _isDataFilePolicy = false;
        _isWatched = false;
        _isDefault = true;
        _channelId = -1;
        _maxNumMessages = -1;
        _maxSizeKB = -1;
        _maxAgeDays = -1;
        _mimicDefault = false;
        _isNew = false;
    }
    
    public ExpirationPolicy(ExpirationPolicy src) {
        this();
        load(src);
    }
    
    public void load(ExpirationPolicy src) {
        _isDataFilePolicy = src.isDataFilePolicy();
        _isWatched = src.isWatchedPolicy();
        _isDefault = src.isDefaultPolicy();
        _channelId = src.getPolicyChannelId();
        _maxNumMessages = src.getMaxNumMessages();
        _maxSizeKB = src.getMaxSizeKB();
        _maxAgeDays = src.getMaxAgeDays();
        _mimicDefault = src.getMimicDefault();
        _isNew = src.getIsNew();
    }
    
    public boolean isDataFilePolicy() { return _isDataFilePolicy; }
    public boolean isDBPolicy() { return !_isDataFilePolicy; }
    public boolean isDefaultPolicy() { return _isDefault; }
    public boolean isWatchedPolicy() { return _isWatched; }
    public long getPolicyChannelId() { return _channelId; }
    public long getMaxNumMessages() { return _maxNumMessages; }
    public int getMaxSizeKB() { return _maxSizeKB; }
    public int getMaxAgeDays() { return _maxAgeDays; }
    public boolean getMimicDefault() { return _mimicDefault; }
    /** not a persisted field */
    public boolean getIsNew() { return _isNew; }
    
    public void setIsDataFilePolicy() { _isDataFilePolicy = true; }
    public void setIsDBPolicy() { _isDataFilePolicy = false; }
    public void setIsDefaultPolicy() {
        _isDefault = true;
        _isWatched = false;
        _channelId = -1;
    }
    public void setIsWatchedPolicy() { 
        _isDefault = false;
        _isWatched = true;
        _channelId = -1;
    }
    public void setPolicyChannelId(long id) { 
        _isDefault = false;
        _isWatched = false;
        _channelId = id;
    }
    public void setMaxNumMessages(long msgs) { _maxNumMessages = msgs; }
    public void setMaxSizeKB(int KB) { _maxSizeKB = KB; }
    public void setMaxAgeDays(int days) { _maxAgeDays = days; }
    public void setMimicDefault(boolean mimic) { _mimicDefault = mimic; }
    
    public void setIsNew(boolean isNew) { _isNew = isNew; }
}
