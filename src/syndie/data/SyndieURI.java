package syndie.data;

import java.lang.reflect.Array;
import java.net.URISyntaxException;
import java.util.*;
import net.i2p.data.*;
import syndie.Constants;

/**
 * Maintain a reference within syndie per the syndie URN spec, including canonical
 * encoding and decoding
 *
 */
public class SyndieURI {
    private TreeMap _attributes;
    private String _type;
    private transient String _stringified;
    
    public SyndieURI(String encoded) throws URISyntaxException {
        fromString(encoded);
    }
    public SyndieURI(String type, TreeMap attributes) {
        if ( (type == null) || (type.trim().length() <= 0) || (attributes == null) ) 
            throw new IllegalArgumentException("Invalid attributes or type");
        _type = type;
        _attributes = attributes;
    }
    public SyndieURI(String type, Map attributes) {
        this(type, new TreeMap(attributes));
    }
    /** 
     * populate the new URI with the given primary URI's settings, falling back on the given
     * default values for attriutes that aren't in the primary URI
     */
    public SyndieURI(SyndieURI primary, SyndieURI defaultValues) {
        if (defaultValues != null) {
            _attributes = new TreeMap(defaultValues.getAttributes());
            _type = defaultValues.getType();
        }
        if (primary != null) {
            if (primary.getType() != null)
                _type = primary.getType();
            if (primary.getAttributes() != null)
                _attributes.putAll(primary.getAttributes());
        }
    }

    public static SyndieURI createRelativePage(int pageNum) {
        String uri = "urn:syndie:channel:d4:pagei" + pageNum + "ee";
        try {
            return new SyndieURI(uri);
        } catch (URISyntaxException use) {
            throw new RuntimeException("Hmm, encoded URI is not valid: " + use.getMessage() + " [" + uri + "]");
        }
    }
    public static SyndieURI createRelativeAttachment(int attachmentNum) {
        String uri = "urn:syndie:channel:d10:attachmenti" + attachmentNum + "ee";
        try {
            return new SyndieURI(uri);
        } catch (URISyntaxException use) {
            throw new RuntimeException("Hmm, encoded URI is not valid: " + use.getMessage() + " [" + uri + "]");
        }
    }
    public static SyndieURI createSearch(String searchString) {
        String searchURI = "urn:syndie:search:d7:keyword" + searchString.length() + ":" + searchString + "e";
        try {
            return new SyndieURI(searchURI);
        } catch (URISyntaxException use) {
            throw new RuntimeException("Hmm, encoded search URI is not valid: " + use.getMessage() + " [" + searchURI + "]");
        }
    }

    public SyndieURI createSearch() { return createSearch(getScope()); }
    
    public static final SyndieURI DEFAULT_SEARCH_URI = SyndieURI.createSearch((Hash)null);
    public static SyndieURI createSearch(Hash channel) {
        String scopes[] = null;
        if (channel != null)
            scopes = new String[] { channel.toBase64() };
        return createSearch(scopes, "authorized", null, new Long(7), null, null, null, false, 
                            null, null, null, null, null, null, null, null, false, false, false, true, false);
    }
    public static SyndieURI createSearch(Hash channel, boolean unreadOnly, boolean threaded, boolean useImportDate) {
        String scopes[] = null;
        if (channel != null)
            scopes = new String[] { channel.toBase64() };
        Long postDays = null;
        Long importDays = null;
        if (useImportDate)
            importDays = new Long(7);
        else
            postDays = new Long(7);
        return createSearch(scopes, "authorized", postDays, importDays, null, null, null, false, 
                            null, null, null, null, null, null, null, null, false, false, false, threaded, unreadOnly);
    }
    
    public static SyndieURI createBookmarked(List scopeHashes) {
        String scopes[] = new String[scopeHashes.size()];
        for (int i = 0; i < scopes.length; i++)
            scopes[i] = ((Hash)scopeHashes.get(i)).toBase64();
        Long postDays = null;
        Long importDays = null;
        if (true) //useImportDate)
            importDays = new Long(7);
        else
            postDays = new Long(7);
        SyndieURI uri = createSearch(scopes, "authorized", postDays, importDays, null, null, null, false, 
                                     null, null, null, null, null, null, null, null, false, false, false, true, false);
        Map attr = uri.getAttributes();
        attr.put("byforum", "true");
        return uri;
    }
    
    /**
     * parameters here map to the fields @ doc/web/spec.html#uri_search
     */
    public static SyndieURI createSearch(String scopes[], String author, Long postDays, Long recvDays,
                                         String inc[], String req[], String excl[], boolean msgs,
                                         Long pageMin, Long pageMax, Long attachMin, Long attachMax,
                                         Long refMin, Long refMax, Long keyMin, Long keyMax,
                                         boolean encrypted, boolean pbe, boolean priv, boolean threaded, boolean unreadOnly) {
        HashMap attributes = new HashMap();
        if ( (scopes != null) && (scopes.length > 0) )
            attributes.put("scope", scopes);
        if (author != null)
            attributes.put("author", author);
        if (recvDays != null)
            attributes.put("agelocal", recvDays);
        if (postDays != null)
            attributes.put("age", postDays);
        if ( (inc != null) && (inc.length > 0) )
            attributes.put("taginclude", inc);
        if ( (excl != null) && (excl.length > 0) )
            attributes.put("tagexclude", excl);
        if ( (req != null) && (req.length > 0) )
            attributes.put("tagrequire", req);
        if ( (pageMin != null) && (pageMin.intValue() >= 0) )
            attributes.put("pagemin", pageMin);
        if ( (pageMax != null) && (pageMax.intValue() >= 0) )
            attributes.put("pagemax", pageMax);
        if ( (attachMin != null) && (attachMin.intValue() >= 0) )
            attributes.put("attachmin", attachMin);
        if ( (attachMax != null) && (attachMax.intValue() >= 0) )
            attributes.put("attachmax", attachMax);
        if ( (refMin != null) && (refMin.intValue() >= 0) )
            attributes.put("refmin", refMin);
        if ( (refMax != null) && (refMax.intValue() >= 0) )
            attributes.put("refmax", refMax);
        if ( (keyMin != null) && (keyMin.intValue() >= 0) )
            attributes.put("keymin", keyMin);
        if ( (keyMax != null) && (keyMax.intValue() >= 0) )
            attributes.put("keymax", keyMax);
        
        attributes.put("tagmessages", msgs ? Boolean.TRUE : Boolean.FALSE);
        attributes.put("encrypted", encrypted ? Boolean.TRUE : Boolean.FALSE);
        attributes.put("pbe", pbe ? Boolean.TRUE : Boolean.FALSE);
        attributes.put("private", priv ? Boolean.TRUE : Boolean.FALSE);
        attributes.put("threaded", threaded ? Boolean.TRUE : Boolean.FALSE);
        attributes.put("unreadonly", unreadOnly ? Boolean.TRUE : Boolean.FALSE);
        
        return new SyndieURI("search", attributes);
    }
    
    public static SyndieURI createURL(String url) {
        StringBuffer buf = new StringBuffer();
        buf.append("urn:syndie:url:d");
        if (url != null)
            buf.append("3:url").append(url.length()).append(":").append(url);
        buf.append("e");
        try {
            return new SyndieURI(buf.toString());
        } catch (URISyntaxException use) {
            System.err.println("attempted: " + buf.toString());
            use.printStackTrace();
            return null;
        }
    }
    public static SyndieURI createArchive(String url, String pass) {
        StringBuffer buf = new StringBuffer();
        buf.append("urn:syndie:archive:d");
        if (url != null)
            buf.append("3:url").append(url.length()).append(':').append(url);
        if (pass != null) {
            buf.append("11:postKeyType4:pass11:postKeyData");
            String base64Pass = Base64.encode(DataHelper.getUTF8(pass));
            buf.append(base64Pass.length()).append(':').append(base64Pass);
        }
        buf.append("e");
        try {
            return new SyndieURI(buf.toString());
        } catch (URISyntaxException use) {
            System.err.println("attempted: " + buf.toString());
            use.printStackTrace();
            return null;
        }
    }
    public static SyndieURI createArchive(String url, String name, String description) {
        StringBuffer buf = new StringBuffer();
        buf.append("urn:syndie:archive:d");
        if (url != null)
            buf.append("3:url").append(url.length()).append(':').append(url);
        if (name != null)
            buf.append("4:name").append(name.length()).append(':').append(name);
        if (description != null)
            buf.append("4:desc").append(description.length()).append(':').append(description);
        buf.append("e");
        try {
            return new SyndieURI(buf.toString());
        } catch (URISyntaxException use) {
            System.err.println("attempted: " + buf.toString());
            use.printStackTrace();
            return null;
        }
    }
    public static SyndieURI createScope(Hash scope) { return createMessage(scope, -1, -1); }
    public static SyndieURI createMessage(Hash scope, long msgId) { return createMessage(scope, msgId, -1); }
    public static SyndieURI createMessage(Hash scope, long msgId, int pageNum) {
        StringBuffer buf = new StringBuffer();
        buf.append("urn:syndie:channel:d");
        if (scope != null) {
            buf.append("7:channel");
            String ch = scope.toBase64();
            buf.append(ch.length()).append(':').append(ch);
            if (msgId >= 0) {
                buf.append("9:messageIdi").append(msgId).append("e");
                if (pageNum >= 0)
                    buf.append("4:pagei").append(pageNum).append("e");
            }
        }
        buf.append('e');
        try {
            return new SyndieURI(buf.toString());
        } catch (URISyntaxException use) {
            System.err.println("attempted: " + buf.toString());
            use.printStackTrace();
            return null;
        }
    }
    public static SyndieURI createAttachment(Hash scope, long msgId, int attachmentNum) {
        StringBuffer buf = new StringBuffer();
        buf.append("urn:syndie:channel:d");
        if (scope != null) {
            buf.append("7:channel");
            String ch = scope.toBase64();
            buf.append(ch.length()).append(':').append(ch);
            if (msgId >= 0) {
                buf.append("9:messageIdi").append(msgId).append("e");
                if (attachmentNum >= 0)
                    buf.append("10:attachmenti").append(attachmentNum).append("e");
            }
        }
        buf.append('e');
        try {
            return new SyndieURI(buf.toString());
        } catch (URISyntaxException use) {
            System.err.println("attempted: " + buf.toString());
            use.printStackTrace();
            return null;
        }
    }
    

    /**
     * Create a URI that includes the given read key for the specified channel
     */
    public static SyndieURI createKey(Hash scope, SessionKey sessionKey) {
        StringBuffer buf = new StringBuffer();
        buf.append("urn:syndie:channel:d");
        if (scope != null) {
            buf.append("7:channel");
            String ch = scope.toBase64();
            buf.append(ch.length()).append(':').append(ch);
            buf.append("7:readKey");
            ch = Base64.encode(sessionKey.getData());
            buf.append(ch.length()).append(':').append(ch);
        }
        buf.append('e');
        try {
            return new SyndieURI(buf.toString());
        } catch (URISyntaxException use) {
            System.err.println("attempted: " + buf.toString());
            use.printStackTrace();
            return null;
        }
    }

    /**
     * Create a URI that includes the given post or manage key for the specified channel
     */    
    public static SyndieURI createKey(Hash scope, String function, SigningPrivateKey priv) {
        StringBuffer buf = new StringBuffer();
        buf.append("urn:syndie:channel:d");
        if (scope != null) {
            buf.append("7:channel");
            String ch = scope.toBase64();
            buf.append(ch.length()).append(':').append(ch);
            if (function.equalsIgnoreCase(Constants.KEY_FUNCTION_POST))
                buf.append("7:postKey");
            else if (function.equalsIgnoreCase(Constants.KEY_FUNCTION_MANAGE))
                buf.append("9:manageKey");
            ch = Base64.encode(priv.getData());
            buf.append(ch.length()).append(':').append(ch);
        }
        buf.append('e');
        try {
            return new SyndieURI(buf.toString());
        } catch (URISyntaxException use) {
            System.err.println("attempted: " + buf.toString());
            use.printStackTrace();
            return null;
        }
    }
    
    /**
     * Create a URI that includes the private key to decrypt replies for the channel
     */
    public static SyndieURI createKey(Hash scope, PrivateKey priv) {
        StringBuffer buf = new StringBuffer();
        buf.append("urn:syndie:channel:d");
        if (scope != null) {
            buf.append("7:channel");
            String ch = scope.toBase64();
            buf.append(ch.length()).append(':').append(ch);
            buf.append("8:replyKey");
            ch = Base64.encode(priv.getData());
            buf.append(ch.length()).append(':').append(ch);
        }
        buf.append('e');
        try {
            return new SyndieURI(buf.toString());
        } catch (URISyntaxException use) {
            System.err.println("attempted: " + buf.toString());
            use.printStackTrace();
            return null;
        }
    }
    
    private static final String TYPE_URL = "url";
    private static final String TYPE_CHANNEL = "channel";
    private static final String TYPE_ARCHIVE = "archive";
    private static final String TYPE_TEXT = "text";
    private static final String TYPE_SEARCH = "search";
    
    /** does this this URI maintain a reference to a URL? */
    public boolean isURL() { return TYPE_URL.equals(_type); }
    /** does this this URI maintain a reference to a syndie channel/message/page/attachment? */
    public boolean isChannel() { return TYPE_CHANNEL.equals(_type); }
    /** does this this URI maintain a reference to a syndie archive? */
    public boolean isArchive() { return TYPE_ARCHIVE.equals(_type); }
    /** does this this URI maintain a reference to a URL? */
    public boolean isText() { return TYPE_TEXT.equals(_type); }
    public boolean isSearch() { return TYPE_SEARCH.equals(_type); }
    
    public String getType() { return _type; }
    public Map getAttributes() { return _attributes; }
    
    public String getString(String key) { 
        Object o = _attributes.get(key);
        if (o == null)
            return null;
        else
            return o.toString();
    }
    public Long getLong(String key) { return (Long)_attributes.get(key); }
    public String[] getStringArray(String key) { return (String[])_attributes.get(key); }
    public boolean getBoolean(String key, boolean defaultVal) {
        Object o = _attributes.get(key);
        if (o == null) return defaultVal;
        if (o instanceof Boolean)
            return ((Boolean)o).booleanValue();
        String str = o.toString();
        if (str == null)
            return defaultVal;
        else
            return Boolean.valueOf(str).booleanValue();
    }
    public String getURL() { return getString("url"); }
    public Hash getScope() { return getHash("channel"); }
    public Hash getHash(String key) {
        Object obj = _attributes.get(key);
        if (obj == null) return null;
        String val = null;
        if (obj.getClass().isArray()) {
            String vals[] = (String[])obj;
            if (vals.length == 0) return null;
            val = vals[0];
        } else {
            val = obj.toString();
        }
        if (val != null) {
            byte b[] = Base64.decode(val);
            if ( (b != null) && (b.length == Hash.HASH_LENGTH) )
                return new Hash(b);
        }
        return null;
    }
    public SessionKey getReadKey() {
        byte val[] = decodeKey(getString("readKey"), SessionKey.KEYSIZE_BYTES);
        if ( (val != null) && (val.length == SessionKey.KEYSIZE_BYTES) )
            return new SessionKey(val);
        else
            return null;
    }
    public SessionKey getArchiveKey() {
        byte val[] = decodeKey(getString(Constants.URI_ARCHIVE_PASSPHRASE), SessionKey.KEYSIZE_BYTES);
        if ( (val != null) && (val.length == SessionKey.KEYSIZE_BYTES) )
            return new SessionKey(val);
        else
            return null;
    }
    public SigningPrivateKey getPostKey() {
        byte val[] = decodeKey(getString("postKey"), SigningPrivateKey.KEYSIZE_BYTES);
        if ( (val != null) && (val.length == SigningPrivateKey.KEYSIZE_BYTES) )
            return new SigningPrivateKey(val);
        else
            return null;
    }
    public byte[] getPostKeyData() {
        String str = getString("postKeyData");
        if (str != null)
            return Base64.decode(str);
        else
            return null;
    }
    public String getArchivePassphrase() {
        String str = getString("postKeyData");
        if (str != null) {
            byte data[] = Base64.decode(str);
            if (data != null)
                return DataHelper.getUTF8(data);
        }
        return null;
    }
    public SigningPrivateKey getManageKey() {
        byte val[] = decodeKey(getString("manageKey"), SigningPrivateKey.KEYSIZE_BYTES);
        if ( (val != null) && (val.length == SigningPrivateKey.KEYSIZE_BYTES) )
            return new SigningPrivateKey(val);
        else
            return null;
    }
    public PrivateKey getReplyKey() {
        byte val[] = decodeKey(getString("replyKey"), PrivateKey.KEYSIZE_BYTES);
        if ( (val != null) && (val.length == PrivateKey.KEYSIZE_BYTES) )
            return new PrivateKey(val);
        else
            return null;
    }
    public Long getMessageId() { return getLong("messageId"); }
    public Long getAttachment() { return getLong("attachment"); }
    public Long getPage() { return getLong("page"); }
    public Hash[] getSearchScopes() { 
        String scopes[] = getStringArray("scope");
        if (scopes == null) return null;
        Hash rv[] = new Hash[scopes.length];
        for (int i = 0; i < scopes.length; i++) {
            byte val[] = Base64.decode(scopes[i]);
            if ( (val != null) && (val.length == Hash.HASH_LENGTH) )
                rv[i] = new Hash(val);
        }
        return rv;
    }
    
    public void fromString(String bencodedURI) throws URISyntaxException {
        if (bencodedURI == null) throw new URISyntaxException("null URI", "no uri");
        if (bencodedURI.startsWith("urn:syndie:"))
            bencodedURI = bencodedURI.substring("urn:syndie:".length());
        int endType = bencodedURI.indexOf(':');
        if (endType <= 0)
            throw new URISyntaxException(bencodedURI, "Missing type");
        if (endType >= bencodedURI.length())
            throw new URISyntaxException(bencodedURI, "No bencoded attributes");
        _type = bencodedURI.substring(0, endType);
        bencodedURI = bencodedURI.substring(endType+1);
        try { 
            _attributes = bdecode(bencodedURI);
        } catch (IllegalArgumentException iae) {
            throw new URISyntaxException(bencodedURI, "Error bencoding: " + iae.getMessage());
        } catch (IndexOutOfBoundsException ioobe) {
            throw new URISyntaxException(bencodedURI, "Error bencoding: " + ioobe.getMessage());
        }
        if (_attributes == null) {
            throw new URISyntaxException(bencodedURI, "Invalid bencoded attributes");
        }
    }
    public String toString() {
        if (_stringified == null)
            _stringified = "urn:syndie:" + _type + ":" + bencode(_attributes);
        return _stringified;
    }
    
    private static final Set SENSITIVE_ATTRIBUTES = new HashSet();
    static {
        SENSITIVE_ATTRIBUTES.add("readKey");
        SENSITIVE_ATTRIBUTES.add("postKey");
        SENSITIVE_ATTRIBUTES.add("replyKey");
        SENSITIVE_ATTRIBUTES.add("manageKey");
        SENSITIVE_ATTRIBUTES.add("identKey");
    }
    /** true if the given uri attribute is one carrying private key information */
    public static boolean isSensitiveAttribute(String name) {
        return (name != null) && SENSITIVE_ATTRIBUTES.contains(name);
    }

    /**
     * the target URI could be a normal URI, or it could be a relative URI scoped
     * within the given source - for instance, the target may refer to "page 2".
     * this method returns the absolute SyndieURI to the target (which is just the
     * target itself, in most situations, except those that are e.g. "page 2")
     */
    public static SyndieURI resolveRelative(SyndieURI source, SyndieURI target) {
       if ( (source != null) && (target != null) && (target.isChannel()) && (target.getScope() == null) ) {
           if ( (source.getScope() != null) && (source.getMessageId() != null) ) {
               Long page = target.getPage();
               if (page != null)
                   return SyndieURI.createMessage(source.getScope(), source.getMessageId().longValue(), page.intValue());
               Long attachment = target.getAttachment();
               if (attachment != null)
                   return SyndieURI.createAttachment(source.getScope(), source.getMessageId().longValue(), attachment.intValue());
           }
       }
       return target;
    }
    
    public boolean equals(Object obj) { return (obj != null) && toString().equals(obj.toString()); }
    public int hashCode() { return toString().hashCode(); }

    public static String encodeKey(byte orig[]) {
        int remaining = orig.length;
        int start = 0;
        while ( (remaining > 0) && (orig[start] == 0x00) ) {
            remaining--;
            start++;
        }
        return Base64.encode(orig, start, remaining);
    }
    public static byte[] decodeKey(String orig, int size) {
        byte rv[] = new byte[size];
        byte decoded[] = Base64.decode(orig);
        if (decoded == null) return null;
        if (decoded.length > size) return null;
        for (int i = 0; i < decoded.length; i++)
            rv[size-i-1] = decoded[decoded.length-1-i];
        return rv;
    }
    
    public static void main(String args[]) { test(); }
    private static void test() {
        try {
            new SyndieURI("urn:syndie:channel:d7:channel40:12345678901234567890123456789012345678908:showRefs4:truee");
        } catch (Exception e) {
            e.printStackTrace();
            return;
        }
        if (!test(new TreeMap()))
            throw new RuntimeException("failed on empty");
        if (!test(createStrings()))
            throw new RuntimeException("failed on strings");
        if (!test(createList()))
            throw new RuntimeException("failed on list");
        if (!test(createEmptyList()))
            throw new RuntimeException("failed on empty list");
        if (!test(createMixed()))
            throw new RuntimeException("failed on mixed");
        if (!test(createMultiMixed()))
            throw new RuntimeException("failed on multimixed");
        System.out.println("Passed all tests");
    }
    private static TreeMap createStrings() {
        TreeMap m = new TreeMap();
        for (int i = 0; i < 64; i++)
            m.put("key" + i, "val" + i);
        return m;
    }
    private static TreeMap createList() {
        TreeMap m = new TreeMap();
        for (int i = 0; i < 8; i++)
            m.put("key" + i, "val" + i);
        String str[] = new String[] { "stringElement1", "stringElement2", "stringElement3" };
        m.put("stringList", str);
        return m;
    }
    private static TreeMap createEmptyList() {
        TreeMap m = new TreeMap();
        for (int i = 0; i < 8; i++)
            m.put("key" + i, "val" + i);
        String str[] = new String[0];
        m.put("stringList", str);
        return m;
    }
    private static TreeMap createMixed() {
        TreeMap m = new TreeMap();
        for (int i = 0; i < 8; i++)
            m.put("key" + i, "val" + i);
        String str[] = new String[] { "stringElement1", "stringElement2", "stringElement3" };
        m.put("stringList", str);
        for (int i = 8; i < 16; i++)
            m.put("intKey" + i, (i%2==0?(Number)(new Long(i)):(Number)(new Integer(i))));
        return m;
    }
    private static TreeMap createMultiMixed() {
        TreeMap m = new TreeMap();
        for (int i = 0; i < 8; i++)
            m.put("key" + i, "val" + i);
        for (int i = 0; i < 10; i++) {
            String str[] = new String[] { "stringElement1", "stringElement2", "stringElement3" };
            m.put("stringList" + i, str);
        }
        for (int i = 8; i < 16; i++)
            m.put("intKey" + i, (i%2==0?(Number)(new Long(i)):(Number)(new Integer(i))));
        return m;
    }
    private static boolean test(TreeMap orig) {
        String enc = bencode(orig);
        System.out.println("bencoded: " + enc);
        TreeMap decoded = null;
        try {
            decoded = bdecode(enc);
        } catch (URISyntaxException use) {
            use.printStackTrace();
        }
        if (decoded == null) return false;
        Set origKeys = new HashSet(orig.keySet());
        Set decKeys = new HashSet(decoded.keySet());
        if (origKeys.equals(decKeys)) {
            for (Iterator iter = origKeys.iterator(); iter.hasNext(); ) {
                String k = (String)iter.next();
                Object origVal = orig.get(k);
                Object decVal = decoded.get(k);
                if (origVal.getClass().isArray()) {
                    boolean ok = Arrays.equals((String[])origVal, (String[])decVal);
                    if (!ok) {
                        System.out.println("key " + k + " is an unequal array");
                        return false;
                    }
                } else if (origVal instanceof Number) {
                    long o = ((Number)origVal).longValue();
                    long d = ((Number)decVal).longValue();
                    if (d != o) {
                        System.out.println("key " + k + " is an unequal number: " + d + ", " + o);
                    }
                } else if (!origVal.equals(decVal)) {
                    System.out.println("key " + k + " does not match (" + origVal + ", " + decVal + ")/(" + origVal.getClass().getName() + ", " + decVal.getClass().getName() + ")");
                    return false;
                }
            }
            return true;
        } else {
            return false;
        }
    }
    
    /////
    // remaining is a trivial bencode/bdecode impl, capable only of handling
    // what the SyndieURI needs
    /////
    
    private static final String bencode(TreeMap attributes) {
        StringBuffer buf = new StringBuffer(64);
        buf.append('d');
        for (Iterator iter = attributes.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry)iter.next();
            String key = (String)entry.getKey();
            buf.append(key.length()).append(':').append(key);
            buf.append(bencode(entry.getValue()));
        }
        buf.append('e');
        return buf.toString();
    }
    
    private static final String bencode(Object val) {
        if ( (val instanceof Integer) || (val instanceof Long) ) {
            return "i" + val.toString() + "e";
        } else if (val.getClass().isArray()) {
            StringBuffer buf = new StringBuffer();
            buf.append("l");
            Object vals[] = (Object[])val;
            for (int i = 0; i < vals.length; i++)
                buf.append(bencode(vals[i]));
            buf.append("e");
            return buf.toString();
        } else {
            String str = val.toString();
            return String.valueOf(str.length()) + ":" + val;
        }
    }
    
    private static final void bdecodeNext(StringBuffer remaining, TreeMap target) throws URISyntaxException {
        String key = null;
        while (true) {
            switch (remaining.charAt(0)) {
                case 'l':
                    List l = new ArrayList();
                    boolean ok = true;
                    remaining.deleteCharAt(0);
                    if (remaining.charAt(0) == 'e') {
                        // 0 element list
                        remaining.deleteCharAt(0);
                        target.put(key, new String[0]);
                        key = null;
                        return;
                    } else {
                        while (bdecodeNext(remaining, l)) {
                            if (remaining.charAt(0) == 'e') {
                                String str[] = new String[l.size()];
                                for (int i = 0; i < str.length; i++)
                                    str[i] = (String)l.get(i);
                                target.put(key, str);
                                key = null;
                                remaining.deleteCharAt(0);
                                return;
                            }
                        }
                        // decode failed
                        throw new URISyntaxException(remaining.toString(), "Unterminated list");
                    }
                case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
                    String str = bdecodeNext(remaining);
                    if (str == null) {
                        throw new URISyntaxException(remaining.toString(), "Undecoded string");
                    } else if (key == null) {
                        key = str;
                    } else {
                        target.put(key, str);
                        key = null;
                        return;
                    }
                    break;
                case 'i':
                    remaining.deleteCharAt(0);
                    int idx = remaining.indexOf("e");
                    if (idx < 0)
                        throw new URISyntaxException(remaining.toString(), "No remaining 'e'");
                    try {
                        String lstr = remaining.substring(0, idx);
                        long val = Long.parseLong(lstr);
                        if (key == null)
                            throw new URISyntaxException(remaining.toString(), "Numbers cannot be syndie uri keys");
                        target.put(key, new Long(val));
                        key = null;
                        remaining.delete(0, idx+1);
                        return;
                    } catch (NumberFormatException nfe) {
                        throw new URISyntaxException(remaining.toString(), "Invalid number format: " + nfe.getMessage());
                    }
                default:
                    throw new URISyntaxException(remaining.toString(), "Unsupported bencoding type");
            }
        }
    }
    private static final boolean bdecodeNext(StringBuffer remaining, List target) {
        String str = bdecodeNext(remaining);
        if (str == null) return false;
        target.add(str);
        return true;
    }
    private static final String bdecodeNext(StringBuffer remaining) {
        int br = remaining.indexOf(":");
        if (br <= 0)
            return null;
        if (br >= remaining.length())
            return null;
        String len = remaining.substring(0, br);
        try {
            int sz = Integer.parseInt(len);
            remaining.delete(0, br+1);
            if (sz > remaining.length())
                throw new IllegalArgumentException("bad length (" + len + ") with " + remaining.length() + " left");;
            String val = remaining.substring(0, sz);
            remaining.delete(0, sz);
            return val;
        } catch (NumberFormatException nfe) {
            return null;
        }
    }
    /**
     * bdecode the subset of bencoded data we require.  The bencoded string must
     * be a single dictionary and contain either strings, integers, or lists of
     * strings.
     */
    private static final TreeMap bdecode(String bencoded) throws URISyntaxException {
        if ( (bencoded.charAt(0) != 'd') || (bencoded.charAt(bencoded.length()-1) != 'e') )
            throw new URISyntaxException(bencoded, "Not bencoded properly");
        StringBuffer buf = new StringBuffer(bencoded);
        buf.deleteCharAt(0);
        buf.deleteCharAt(buf.length()-1);
        TreeMap rv = new TreeMap();
        while (buf.length() > 0)
            bdecodeNext(buf, rv);
        return rv;
    }
}
