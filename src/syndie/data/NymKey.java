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
    private boolean _expired;
    public NymKey(String type, byte data[], boolean authenticated, String function, long nymId, Hash channel) {
        this(type, data, null, authenticated, function, nymId, channel, false);
    }
    public NymKey(String type, byte data[], boolean authenticated, String function, long nymId, Hash channel, boolean expired) {
        this(type, data, null, authenticated, function, nymId, channel, expired);
    }
    public NymKey(String type, byte data[], String dataHash, boolean authenticated, String function, long nymId, Hash channel) {
        this(type, data, dataHash, authenticated, function, nymId, channel, false);
    }
    public NymKey(String type, byte data[], String dataHash, boolean authenticated, String function, long nymId, Hash channel, boolean expired) {
        _channel = channel;
        _data = data;
        _dataHash = dataHash;
        _authenticated = authenticated;
        _function = function;
        _type = type;
        _nymId = nymId;
        _expired = expired;
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
    public boolean getIsExpired() { return _expired; }
    public Hash getChannel() { return _channel; }
    public String toString() {
        return _function + " for " + _channel.toBase64() + " " + Base64.encode(_data) 
               + (_dataHash != null ? " / " + _dataHash : "") + " (" + _authenticated + ")";
    }

    public boolean isIdentity() {
        if ( (_data == null) || (_data.length != SigningPrivateKey.KEYSIZE_BYTES) || (_channel == null) )
            return false;
        SigningPrivateKey priv = new SigningPrivateKey(_data);
        SigningPublicKey pub = priv.toPublic();
        if (pub == null)
            return false;
        return _channel.equals(pub.calculateHash());
    }
}
