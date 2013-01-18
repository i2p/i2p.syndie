package syndie.data;

import net.i2p.data.Base64;
import net.i2p.data.DataHelper;
import net.i2p.data.Hash;
import net.i2p.data.PrivateKey;
import net.i2p.data.PublicKey;
import net.i2p.data.SessionKey;
import net.i2p.data.Signature;
import net.i2p.data.SigningPublicKey;
import net.i2p.data.SigningPrivateKey;

/**
 *  Stores the private key data and associated info
 */
public class NymKey {
    private final Hash _channel;
    private final byte _data[];
    private final String _dataHash;
    private final boolean _authenticated;
    private final String _function;
    private final String _type;
    private final long _nymId;
    private final boolean _expired;

    public NymKey(String type, byte data[], boolean authenticated, String function, long nymId, Hash channel) {
        this(type, data, null, authenticated, function, nymId, channel, false);
    }

    public NymKey(String type, byte data[], boolean authenticated, String function, long nymId, Hash channel, boolean expired) {
        this(type, data, null, authenticated, function, nymId, channel, expired);
    }

    public NymKey(String type, byte data[], String dataHash, boolean authenticated, String function, long nymId, Hash channel) {
        this(type, data, dataHash, authenticated, function, nymId, channel, false);
    }

    /**
     * @param type see Constants.KEY_TYPE_*
     * @param data either 256 bytes (PrivateKey) or 20 bytes (SigningPrivateKey)
     */
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

    /** @return either 256 bytes (PrivateKey) or 20 bytes (SigningPrivateKey) */
    public byte[] getData() { return _data; }

    /** DSA/ElGamal2048/AES256, etc - See Constants */
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
        String auth = _authenticated ? "Authenticated " : "Unauthenticated ";
        return auth + _type + ' ' + _function + " private key for [" + _channel.toBase64() + "] (" +
               _data.length + "bytes) is: " + Base64.encode(_data);
               //+ (_dataHash != null ? " / " + _dataHash : "");
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

    @Override
    public int hashCode() {
        return DataHelper.hashCode(_data);
    }

    /**
     *  Just for printing in KeyList for now.
     *  May want to add channel if using elsewhere?
     */
    @Override
    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (o == null || !(o instanceof NymKey))
            return false;
        NymKey nk = (NymKey) o;
        return
            DataHelper.eq(_type, nk._type) &&
            DataHelper.eq(_function, nk._function) &&
            DataHelper.eq(_data, nk._data);
    }
}
