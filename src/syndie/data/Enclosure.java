package syndie.data;

import java.io.*;
import java.net.URISyntaxException;
import java.security.MessageDigest;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

import net.i2p.I2PAppContext;
import net.i2p.crypto.SHA256Generator;
import net.i2p.data.Base64;
import net.i2p.data.DataHelper;
import net.i2p.data.Hash;
import net.i2p.data.PublicKey;
import net.i2p.data.SessionKey;
import net.i2p.data.Signature;
import net.i2p.data.SigningPublicKey;
import net.i2p.util.SecureFileOutputStream;

import syndie.Constants;
import syndie.util.StringUtil;

/**
 *  Handle the parsing of a raw message, i.e. a meta.syndie or xxx.syndie file.
 *  Somewhat misnamed, but perhaps because it's one element in a POST message.
 *
 *  An Enclosure contains:
 *   - unencrypted headerrs
 *   - optionally a zipped and encrypted EnclosureBody
 *   - an authorization Signature
 *   - an authentication Signature
 *
 *  See spec.html for specification.
 */
public class Enclosure {
    /** full enclosure formatting version */
    private final String _enclosureType;
    /** headers visible to all */
    private final Properties _publicHeaders;
    /** cached unparsed public headers, as they must go out in the same order */
    private final byte _publicHeaderData[];
    /** encrypted/padded/zipped/etc data */
    private byte[] _data;
    /** hash from the beginning of the enclosure through the data */
    private final Hash _authorizationHash;
    /**
     * signature of the enclosure up through the data by an authorized key
     * (or just random junk if unauthorized)
     */
    private final Signature _authorizationSig;
    /** hash from the beginning of the enclosure through the authorization signature */
    private final Hash _authenticationHash;
    /**
     * signature of the enclosure up through the authorization signature
     * by the nym.  the nym may not be known prior to unencrypting the data
     */
    private final Signature _authenticationSig;
    /** original signature data as stored in the enclosure, while the authenticationSig itself
     * may be adjusted as controlled by a private header value */
    private final byte _authenticationSigOrig[];
    private final int _rawSize;
    
    // see below for constructor
    
    public boolean getLoaded() { return _authorizationSig != null; }
    public String getEnclosureType() { return _enclosureType; }
    public boolean isReply() { return msgType(Constants.MSG_TYPE_REPLY); }
    public boolean isPost() { return msgType(Constants.MSG_TYPE_POST); }
    public boolean isMeta() { return msgType(Constants.MSG_TYPE_META); }
    private boolean msgType(String queryType) {
        String type = getHeaderString(Constants.MSG_HEADER_TYPE);
        if (type != null)
            return type.equals(queryType);
        else
            return false;
    }
    public Properties getHeaders() { return _publicHeaders; }
    public String getHeaderString(String key) { return _publicHeaders.getProperty(key); }
    public byte[] getHeaderBytes(String key) {
        return toBytes(_publicHeaders.getProperty(key));
    }
    public static byte[] toBytes(String val) {
        if (val == null)
            return null;
        else
            return Base64.decode(val);
    }
    public SyndieURI getHeaderURI(String key) {
        return toURI(_publicHeaders.getProperty(key));
    }
    public static SyndieURI toURI(String val) {
        if (val == null) {
            return null;
        } else {
            try {
                return new SyndieURI(val);
            } catch (URISyntaxException ex) {
                return null;
            }
        }
    }
    public SyndieURI[] getHeaderURIs(String key) {
        return toURIs(_publicHeaders.getProperty(key));
    }
    public static SyndieURI[] toURIs(String val) {
        if (val == null) {
            return null;
        } else {
            String str[] = StringUtil.split('\t', val); // val.split("\t");
            if (str != null) {
                SyndieURI uris[] = new SyndieURI[str.length];
                int invalid = 0;
                for (int i = 0; i < str.length; i++) {
                    try {
                        uris[i] = new SyndieURI(str[i]);
                    }  catch (URISyntaxException ex) {
                        invalid++;
                        uris[i] = null;
                    }
                }
                if (invalid > 0) {
                    SyndieURI rv[] = new SyndieURI[str.length - invalid];
                    int cur = 0;
                    for (int i = 0; i < str.length; i++) {
                        if (uris[i] != null) {
                            rv[cur] = uris[i];
                            cur++;
                        }
                    }
                    return rv;
                } else {
                    return uris;
                }
            } else {
                return null;
            }
        }
    }
    
    /** split by tabs */
    public String[] getHeaderStrings(String key) { return getHeaderStrings(key, false); }

    /** split by tabs and optionally commas too */
    public String[] getHeaderStrings(String key, boolean splitByCommaToo) { 
        return toStrings(_publicHeaders.getProperty(key), splitByCommaToo);
    }

    /** split by tabs */
    public static String[] toStrings(String val) { return toStrings(val, false); }

    /** split by tabs and optionally commas too */
    public static String[] toStrings(String val, boolean splitByCommaToo) {
        if (val == null)
            return null;
        else if (!splitByCommaToo)
            return StringUtil.split("\t", val); //val.split("\t");
        else {
            return StringUtil.split("\t,", val);
        }
    }

    /** @return null if not found */
    public Boolean getHeaderBoolean(String key) {
        return toBoolean(_publicHeaders.getProperty(key));
    }

    /** @return null if val is null */
    public static Boolean toBoolean(String val) {
        if (val == null)
            return null;
        else
            return Boolean.valueOf(val);
    }

    /** @return null if not found */
    public Long getHeaderLong(String key) {
        return toLong(_publicHeaders.getProperty(key));
    }

    /** @return null if val is null */
    public static Long toLong(String val) {
        if (val == null) {
            return null;
        } else {
            try {
                return Long.valueOf(val);
            } catch (NumberFormatException nfe) {
                return null;
            }
        }
    }

    /** @return null if not found */
    public Date getHeaderDate(String key) {
        return toDate(_publicHeaders.getProperty(key));
    }
    private static final SimpleDateFormat _dateFormat = new SimpleDateFormat("yyyyMMdd");

    /** @return null if val is null */
    public static Date toDate(String val) {
        if (val == null) {
            return null;
        } else {
            try {
                synchronized (_dateFormat) {
                    return _dateFormat.parse(val);
                }
            } catch (ParseException pe) {
                return null;
            }
        }
    }
    public SessionKey getHeaderSessionKey(String key) {
        return toSessionKey(_publicHeaders.getProperty(key));
    }
    public static SessionKey toSessionKey(String val) {
        if (val == null) {
            return null;
        } else {
            byte b[] = Base64.decode(val);
            if ( (b != null) && (b.length == SessionKey.KEYSIZE_BYTES) )
                return new SessionKey(b);
            else
                return null;
        }
    }
    public SessionKey[] getHeaderSessionKeys(String key) {
        return toSessionKeys(_publicHeaders.getProperty(key));
    }
    public static SessionKey[] toSessionKeys(String val) {
        if (val == null) {
            return null;
        } else {
            String str[] = StringUtil.split('\t', val); //val.split("\t");
            if (str != null) {
                SessionKey keys[] = new SessionKey[str.length];
                int invalid = 0;
                for (int i = 0; i < keys.length; i++) {
                    byte key[] = Base64.decode(str[i]);
                    if ( (key != null) && (key.length == SessionKey.KEYSIZE_BYTES) )
                        keys[i] = new SessionKey(key);
                    else
                        invalid++;
                }
                if (invalid > 0) {
                    SessionKey rv[] = new SessionKey[str.length - invalid];
                    int cur = 0;
                    for (int i = 0; i < str.length; i++) {
                        if (keys[i] != null) {
                            rv[cur] = keys[i];
                            cur++;
                        }
                    }
                    return rv;
                } else {
                    return keys;
                }
            } else {
                return null;
            }
        }
    }
    public SigningPublicKey getHeaderSigningKey(String key) {
        return toSigningKey(_publicHeaders.getProperty(key));
    }
    public static SigningPublicKey toSigningKey(String str) {
        if (str == null) {
            return null;
        } else {
            byte val[] = Base64.decode(str);
            if ( (val != null) && (val.length == SigningPublicKey.KEYSIZE_BYTES) )
                return SigningPublicKey.create(val, 0);
            else
                return null;
        }
    }
    public SigningPublicKey[] getHeaderSigningKeys(String key) {
        return toSigningKeys(toStrings(_publicHeaders.getProperty(key)));
    }
    public static SigningPublicKey[] toSigningKeys(String vals[]) {
        if (vals == null) {
            return null;
        } else {
            SigningPublicKey keys[] = new SigningPublicKey[vals.length];
            int invalid = 0;
            for (int i = 0; i < vals.length; i++) {
                    byte val[] = Base64.decode(vals[i]);
                    if ( (val != null) && (val.length == SigningPublicKey.KEYSIZE_BYTES) )
                        keys[i] = SigningPublicKey.create(val, 0);
                    else
                        invalid++;
            }
            if (invalid > 0) {
                SigningPublicKey rv[] = new SigningPublicKey[vals.length - invalid];
                int cur = 0;
                for (int i = 0; i < vals.length; i++) {
                    if (keys[i] != null) {
                        rv[cur] = keys[i];
                        cur++;
                    }
                }
                return rv;
            } else {
                return keys;
            }
        }
    }
    public PublicKey getHeaderEncryptKey(String key) {
        return toEncryptKey(_publicHeaders.getProperty(key));
    }
    public static PublicKey toEncryptKey(String str) {
        if (str == null) {
            return null;
        } else {
            byte val[] = Base64.decode(str);
            if ( (val != null) && (val.length == PublicKey.KEYSIZE_BYTES) )
                return PublicKey.create(val, 0);
            else
                return null;
        }
    }
    
    public int getDataSize() { return (_data != null ? _data.length : 0); }
    public int getRawSize() { return _rawSize; }
    public InputStream getData() { return new ByteArrayInputStream(_data); }
    public void discardData() { _data = null; }
    
    public Hash getAuthorizationHash() { return _authorizationHash; }
    public Signature getAuthorizationSig() { return _authorizationSig; }
    public Hash getAuthenticationHash() { return _authenticationHash; }
    public Signature getAuthenticationSig() { return _authenticationSig; }
    
    public String toString() {
        StringBuilder rv = new StringBuilder();
        rv.append("Enclosure: ").append(_enclosureType).append(" with headers:\n");
        dumpProps(rv, _publicHeaders);
        if (_data != null)
            rv.append("Enclosure body: ").append(_data.length).append(" bytes");
        else
            rv.append("Without enclosure body");
        SigningPublicKey identKey = getHeaderSigningKey(Constants.MSG_META_HEADER_IDENTITY);
        if (identKey != null)
            rv.append("\nIdentity Hash: ").append(identKey.calculateHash().toBase64());
        rv.append("\nAuthorization Sig: ").append(_authorizationSig.toBase64());
        rv.append("\nAuthentication Sig: ").append(_authenticationSig.toBase64());
        rv.append("\nTotal size: ").append(_rawSize).append(" bytes");
        return rv.toString();
    }
    
    /** for debug only */
    static void dumpProps(StringBuilder buf, Properties props) {
        if (props == null || props.isEmpty()) {
            buf.append("  (none)\n");
            return;
        }
        List keys = new ArrayList(props.keySet());
        Collections.sort(keys);
        for (Iterator iter = keys.iterator(); iter.hasNext(); ) {
            String key = (String)iter.next();
            String val = props.getProperty(key);
            buf.append("  '").append(key).append("' => '").append(val).append("\'");
            buf.append("\n");
        }
    }

    /**
     * Caller must close the InputStream
     */
    public Enclosure(InputStream raw) throws IOException {
        _publicHeaders = new Properties();
        if (raw instanceof FileInputStream)
            raw = new BufferedInputStream(raw);

        MessageDigest hash = SHA256Generator.getDigestInstance();
        _enclosureType = DataHelper.readLine(raw, hash);
        if (_enclosureType == null) throw new IOException("Corrupt enclosure, no type line");
        int rawSize = _enclosureType.length();
        
        // read the headers
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        StringBuilder buf = new StringBuilder(512);
        while (DataHelper.readLine(raw, buf, hash)) {
            int len = buf.length();
            if (len <= 0) break;
            rawSize += len;
            baos.write(DataHelper.getUTF8(buf.toString()+"\n"));
            int split = buf.indexOf("=");
            if (split <= 0) throw new IOException("Invalid header: " + buf.toString());
            String key = buf.substring(0, split).trim();
            String val = null;
            if (split+1 < len)
                val = buf.substring(split+1).trim();
            else
                val = "";
            
            _publicHeaders.setProperty(key, val);
            buf.setLength(0);
        }
        _publicHeaderData = baos.toByteArray();
        
        // now comes the size header
        String sz = DataHelper.readLine(raw, hash);
        if (sz == null) throw new IOException("Missing size header");
        rawSize += sz.length();
        int split = sz.indexOf('=');
        if ( (split <= 0) || (split + 1 >= sz.length()) ) throw new IOException("Invalid size header: " + sz);
        String key = sz.substring(0, split);
        String val = sz.substring(split+1);
        if (!Constants.MSG_HEADER_SIZE.equals(key.trim())) throw new IOException("Size header expected instead of " + sz);
        int bytes = -1;
        try {
            bytes = Integer.parseInt(val.trim());
        } catch (NumberFormatException nfe) {
            throw new IOException("Invalid size header: " + bytes);
        }
        if (bytes < 0) throw new IOException("Invalid size header: " + bytes);
        rawSize += bytes;
        
        // load the data into _data
        loadData(raw, bytes, hash);
        
        try {
            _authorizationHash = new Hash(((MessageDigest)hash.clone()).digest());
        } catch (CloneNotSupportedException cnse) {
            // too hard to support non-cloneable
            throw new IOException("Internal error", cnse);
        }
        _authorizationSig = readSig(raw, hash);
        
        _authenticationHash = new Hash(hash.digest());
        _authenticationSig = readSig(raw, hash);
        _authenticationSigOrig = _authenticationSig.getData();
        _rawSize = rawSize + Signature.SIGNATURE_BYTES*2;
    }
    
    /**
     *  This must be an exact inverse of load(), as the ImportMeta and ImportPost tasks
     *  load() and then store() to the archive rather than copying the file.
     *  That's why we save the raw _publicHeaderData, so it doesn't get reordered.
     *
     *  Changing those classes to just copy the file may be safer.
     */
    public void store(String filename) throws IOException {
        File out = new File(filename);
        //if (out.exists()) throw new IOException("File already exists");
        OutputStream raw = null;
        boolean good = false;
        try {
            raw = new BufferedOutputStream(new SecureFileOutputStream(out));
            raw.write(DataHelper.getUTF8(_enclosureType+"\n"));
            raw.write(_publicHeaderData);
            raw.write(DataHelper.getUTF8("\n"));
            raw.write(DataHelper.getUTF8(Constants.MSG_HEADER_SIZE + "=" + _data.length + "\n"));
            raw.write(_data);
            raw.write(DataHelper.getUTF8("AuthorizationSig=" + Base64.encode(_authorizationSig.getData())+"\n"));
            raw.write(DataHelper.getUTF8("AuthenticationSig=" + Base64.encode(_authenticationSigOrig)+"\n"));
            good = true;
        } finally {
            if (!good)
                out.delete();
            if (raw != null) 
                try {raw.close();} catch (IOException ignore){}
        }
    }
    
    private void loadData(InputStream raw, int numBytes, MessageDigest hash) throws IOException {
        /*
        File bufDir = new File("./syndb_temp");
        bufDir.mkdir();
        File tmp = File.createTempFile("enclosure", "dat", bufDir);
        FileOutputStream fos = new SecureFileOutputStream(tmp);
        byte buf[] = new byte[4096];
        int remaining = numBytes;
        while (remaining > 0) {
            int toRead = Math.min(remaining, buf.length);
            int read = raw.read(buf, 0, toRead);
            if (read == -1)
                throw new IOException("End of the data reached with " + remaining + " bytes remaining");
            fos.write(buf, 0, read);
            hash.update(buf, 0, read);
            remaining -= read;
        }
        fos.close();
        _dataFile = tmp;
        _data = new FileInputStream(tmp);
        _dataSize = numBytes;
        tmp.deleteOnExit();
         */
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        byte buf[] = new byte[4096];
        int remaining = numBytes;
        while (remaining > 0) {
            int toRead = Math.min(remaining, buf.length);
            int read = raw.read(buf, 0, toRead);
            if (read == -1)
                throw new IOException("End of the data reached with " + remaining + " bytes remaining");
            baos.write(buf, 0, read);
            hash.update(buf, 0, read);
            remaining -= read;
        }
        _data = baos.toByteArray();
    }
    
    private Signature readSig(InputStream raw, MessageDigest hash) throws IOException {
        String rem = DataHelper.readLine(raw, hash);
        if (rem != null) {
            int start = rem.indexOf('=');
            if ( (start < 0) || (start+1 >= rem.length()) )
                throw new IOException("No signature");
            rem = rem.substring(start+1);
        }
        byte val[] = Base64.decode(rem);
        if ( (val == null) || (val.length != Signature.SIGNATURE_BYTES) )
            throw new IOException("Not enough data for the signature (" + rem + "/" + (val != null ? val.length : 0) + ")");
        return new Signature(val);
    }

    /**
     *  Dump out the contents of a shared-index.dat file.
     *  Useful for debugging.
     *
     *  @since 1.102b-7
     */
    public static void main(String[] args) {
        if (args.length == 0) {
            System.out.println("Usage: Enclosure files....");
            System.exit(1);
        }
        if (args[0].equals("copytest") && args.length == 3) {
            copytest(args);
            return;
        }
        for (int i = 0; i < args.length; i++) {
            try {
                System.out.println("File: " + args[i]);
                InputStream in = new BufferedInputStream(new FileInputStream(args[i]));
                Enclosure enc = new Enclosure(in);
                System.out.println(enc.toString());
                if (enc.getDataSize() == 0)
                    return;
                SessionKey key = enc.getHeaderSessionKey(Constants.MSG_HEADER_BODYKEY);
                if (key == null) {
                    System.out.println("No body key for decryption");
                    SessionKey[] keys = enc.getHeaderSessionKeys(Constants.MSG_META_HEADER_READKEYS);
                    if (keys != null && keys.length > 0) {
                        System.out.println("Trying first alt. read key");
                        key = keys[0];
                    } else {
                        System.out.println("No alt. read keys for decryption");
                    }
                }
                if (key == null) {
                    System.out.println("Cannot decrypt body");
                } else {
                    EnclosureBody body = new EnclosureBody(I2PAppContext.getGlobalContext(),
                                                           enc.getData(), enc.getDataSize(), key);
                    System.out.println(body.toString());
                }
            } catch (Exception ioe) {
                ioe.printStackTrace();
            }
        }
    }

    private static void copytest(String[] args) {
        InputStream in = null;
        try {
            in = new BufferedInputStream(new FileInputStream(args[1]));
            Enclosure enc = new Enclosure(in);
            enc.store(args[2]);
        } catch (Exception ioe) {
            ioe.printStackTrace();
        } finally {
            if (in != null) try { in.close(); } catch (IOException ioe) {}
        }
    }
}
