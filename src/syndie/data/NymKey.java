package syndie.data;

import net.i2p.data.*;

public class NymKey {
    private Hash _channel;
    private byte _data[];
    private String _dataHash;
    private boolean _authenticated;
    private String _function;
    private String _type;
    private long _nymId;
    public NymKey(String type, byte data[], boolean authenticated, String function, long nymId, Hash channel) {
        this(type, data, null, authenticated, function, nymId, channel);
    }
    public NymKey(String type, byte data[], String dataHash, boolean authenticated, String function, long nymId, Hash channel) {
        _channel = channel;
        _data = data;
        _dataHash = dataHash;
        _authenticated = authenticated;
        _function = function;
        _type = type;
        _nymId = nymId;
    }
    public byte[] getData() { return _data; }
    /** DSA/ElGamal2048/AES256, etc */
    public String getType() { return _type; }
    /** do we know it is a valid key for the channel? */
    public boolean getAuthenticated() { return _authenticated; }
    /** read/post/manage/reply, etc */
    public String getFunction() { return _function; }
    /** nym that knows this key */
    public long getNymId() { return _nymId; }
    public Hash getChannel() { return _channel; }
    public String toString() {
        return _function + " for " + _channel.toBase64() + " " + Base64.encode(_data) 
               + (_dataHash != null ? " / " + _dataHash : "") + " (" + _authenticated + ")";
    }
}
