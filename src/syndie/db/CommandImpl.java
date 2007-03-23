package syndie.db;

import gnu.crypto.hash.Sha256Standalone;
import java.io.*;
import java.util.*;
import net.i2p.I2PAppContext;
import net.i2p.util.Log;
import syndie.Constants;
import net.i2p.data.*;
import syndie.data.ReferenceNode;

public abstract class CommandImpl implements CLI.Command {
    boolean writeKey(UI ui, String filename, PrivateKey key, Hash scope) {
        return writeKey(ui, filename, Constants.KEY_FUNCTION_REPLY, scope, key.toBase64());
    }
    boolean writeKey(UI ui, String filename, SigningPrivateKey key, Hash scope) {
        return writeKey(ui, filename, Constants.KEY_FUNCTION_MANAGE, scope, key.toBase64());
    }
    boolean writeKey(UI ui, String filename, SessionKey key, Hash scope) {
        return writeKey(ui, filename, Constants.KEY_FUNCTION_READ, scope, key.toBase64());
    }
    public static boolean writeKey(UI ui, String filename, String type, Hash scope, String data) {
        if (filename == null) {
            ui.errorMessage("Filename is null for writing?");
            return false;
        }
        FileOutputStream fos = null;
        try {
            fos = new FileOutputStream(filename);
            writeKey(fos, type, scope, data);
            fos.close();
            fos = null;
            return true;
        } catch (IOException ioe) {
            ui.errorMessage("Error writing the key", ioe);
            return false;
        } finally {
            if (fos != null) try { fos.close(); } catch (IOException ioe) {}
        }
    }
    public static void writeKey(OutputStream out, String type, Hash scope, String data) throws IOException {
        out.write(DataHelper.getUTF8("keytype: " + type + "\n"));
        if (scope != null)
            out.write(DataHelper.getUTF8("scope: " + scope.toBase64() + "\n"));
        out.write(DataHelper.getUTF8("raw: " + data + "\n"));
    }
    
    public static byte[] read(UI ui, String filename, int maxSize) {
        if (filename == null) return null;
        FileInputStream fis = null;
        try {
            File f = new File(filename);
            if (!f.exists())
                return null;
            if (f.length() > maxSize)
                return null;
            fis = new FileInputStream(f);
            byte data[] = new byte[(int)f.length()];
            if (data.length != DataHelper.read(fis, data))
                return null;
            fis.close();
            fis = null;
            return data;
        } catch (IOException ioe) {
            ui.debugMessage("Error reading the file", ioe);
            return null;
        } finally {
            if (fis != null) try { fis.close(); } catch (IOException ioe) {}
        }
    }
    
    String readRefs(UI ui, String filename) {
        FileInputStream fin = null;
        File f = new File(filename);
        if (f.exists()) {
            ui.debugMessage("References file exists: " + f.getPath());
            try {
                fin = new FileInputStream(f);
                List refNodes = ReferenceNode.buildTree(fin);
                ui.debugMessage("Reference nodes: " + refNodes.size());
                return ReferenceNode.walk(refNodes);
            } catch (IOException ioe) {
                ui.errorMessage("Error pulling in the refs", ioe);
                return null;
            } finally {
                if (fin != null) try { fin.close(); } catch (IOException ioe) {}
            }
        } else {
            ui.debugMessage("References file does not exist: " + f.getPath());
            return null;
        }
    }
        
    public static void write(Map headers, OutputStream out) throws IOException {
        TreeSet ordered = new TreeSet(headers.keySet());
        for (Iterator iter = ordered.iterator(); iter.hasNext(); ) {
            String key = (String)iter.next();
            String val = (String)headers.get(key);
            out.write(DataHelper.getUTF8(key + '=' + val + '\n'));
        }
    }
    
    /**
     * symmetrically encrypt the raw data to the given key by prepending an
     * IV followed by the AES/256/CBC encrypted raw data
     */
    byte[] encryptBody(I2PAppContext ctx, byte raw[], SessionKey bodyKey) {
        return encryptBody(ctx, raw, bodyKey, getClass());
    }
    public static byte[] encryptBody(I2PAppContext ctx, byte raw[], SessionKey bodyKey, Class cls) {
        byte iv[] = new byte[16];
        byte hmac[] = new byte[Hash.HASH_LENGTH];
        int pad = ctx.random().nextInt(256);
        // IV + AES-CBC(rand(nonzero) padding + 0 + internalSize + totalSize + data + rand, IV, bodyKey)+HMAC(bodySection, H(bodyKey+IV))
        int internalSize = pad + 1 + 4 + 4 + raw.length;
        int remainder = 16 - (internalSize % 16);
        internalSize += remainder;
        
        byte prep[] = new byte[internalSize];
        int off = 0;
        while (off < pad) {
            byte b = (byte)(0xFF & ctx.random().nextInt());
            if (b != 0) {
                prep[off] = b;
                off++;
            }
        }
        prep[off] = 0;
        off++;
        DataHelper.toLong(prep, off, 4, raw.length);
        off += 4;
        DataHelper.toLong(prep, off, 4, prep.length+hmac.length);
        off += 4;
        System.arraycopy(raw, 0, prep, off, raw.length);
        off += raw.length;
        int tail = (prep.length-off);
        while (off < prep.length) {
            byte b = (byte)(0xFF & ctx.random().nextInt());
            prep[off] = b;
            off++;
        }
        
        // ok, prepared.  now lets encrypt
        ctx.random().nextBytes(iv);
        byte rv[] = new byte[iv.length+prep.length+hmac.length];
        System.arraycopy(iv, 0, rv, 0, iv.length);
        ctx.aes().encrypt(prep, 0, rv, 16, bodyKey, rv, 0, prep.length);
        
        // append HMAC(bodySection, H(bodyKey+IV))
        byte hmacPreKey[] = new byte[SessionKey.KEYSIZE_BYTES+iv.length];
        System.arraycopy(bodyKey.getData(), 0, hmacPreKey, 0, SessionKey.KEYSIZE_BYTES);
        System.arraycopy(iv, 0, hmacPreKey, SessionKey.KEYSIZE_BYTES, iv.length);
        byte hmacKey[] = ctx.sha().calculateHash(hmacPreKey).getData();
        ctx.hmac256().calculate(new SessionKey(hmacKey), rv, 16, prep.length, hmac, 0);
        System.arraycopy(hmac, 0, rv, iv.length+prep.length, hmac.length);
        
        if (true) {
            Log log = ctx.logManager().getLog(cls);
            Sha256Standalone dbg = new Sha256Standalone();
            dbg.update(rv);
            byte h[] = dbg.digest();
            log.debug("Encrypted body hashes to " + Base64.encode(h));
            log.debug("key used: " + Base64.encode(bodyKey.getData()));
            log.debug("IV used: " + Base64.encode(iv));
            log.debug("pad: " + pad);
            log.debug("remainder: " + remainder);
            log.debug("internalSize: " + internalSize);
            log.debug("raw.length: " + raw.length);
            log.debug("tail: " + tail);
            log.debug("hmac: " + Base64.encode(hmac));
        }
        return rv;
    }
    
    /**
     * asymmetrically encrypt the raw data to the given key by prepending an
     * ElGamal/2048 encrypted AES/256 key and IV block, followed by the 
     * AES/256/CBC encrypted raw data
     */
    public static byte[] encryptBody(I2PAppContext ctx, byte raw[], PublicKey encryptTo, byte ivOut[], SessionKey replySessionKey) {
        byte data[] = new byte[32+16];
        SessionKey key = null;
        if (replySessionKey != null)
            key = replySessionKey;
        else
            key = ctx.keyGenerator().generateSessionKey();
        byte preIV[] = new byte[16];
        ctx.random().nextBytes(preIV);
        System.arraycopy(preIV, 0, data, 0, preIV.length);
        System.arraycopy(key.getData(), 0, data, preIV.length, SessionKey.KEYSIZE_BYTES);
        byte enc[] = ctx.elGamalEngine().encrypt(data, encryptTo);
        //System.out.println("Asym block [" + enc.length + "]:\n" + Base64.encode(enc) + "\npubKey:\n" + Base64.encode(encryptTo.getData()));
        
        
        byte iv[] = new byte[16];
        Hash ivH = ctx.sha().calculateHash(preIV);
        System.arraycopy(ivH.getData(), 0, iv, 0, iv.length);
        
        System.arraycopy(iv, 0, ivOut, 0, iv.length);

        byte hmac[] = new byte[Hash.HASH_LENGTH];
        
        int pad = ctx.random().nextInt(256);
        // IV + AES-CBC(rand(nonzero) padding + 0 + internalSize + totalSize + data + rand, IV, bodyKey)+HMAC(bodySection, H(bodyKey+IV))
        int internalSize = pad + 1 + 4 + 4 + raw.length;
        int remainder = 16 - (internalSize % 16);
        internalSize += remainder;
        
        byte prep[] = new byte[internalSize];
        int off = 0;
        while (off < pad) {
            byte b = (byte)(0xFF & ctx.random().nextInt());
            if (b != 0) {
                prep[off] = b;
                off++;
            }
        }
        prep[off] = 0;
        off++;
        DataHelper.toLong(prep, off, 4, raw.length);
        off += 4;
        DataHelper.toLong(prep, off, 4, prep.length+hmac.length);
        off += 4;
        System.arraycopy(raw, 0, prep, off, raw.length);
        off += raw.length;
        while (off < prep.length) {
            byte b = (byte)(0xFF & ctx.random().nextInt());
            prep[off] = b;
            off++;
        }
        
        // ok, prepared.  now lets encrypt
        byte rv[] = new byte[enc.length+prep.length+hmac.length];
        System.arraycopy(enc, 0, rv, 0, enc.length);
        ctx.aes().encrypt(prep, 0, rv, enc.length, key, iv, prep.length);
        
        // append HMAC(bodySection, H(bodyKey+IV))
        byte hmacPreKey[] = new byte[SessionKey.KEYSIZE_BYTES+iv.length];
        System.arraycopy(key.getData(), 0, hmacPreKey, 0, SessionKey.KEYSIZE_BYTES);
        System.arraycopy(iv, 0, hmacPreKey, SessionKey.KEYSIZE_BYTES, iv.length);
        byte hmacKey[] = ctx.sha().calculateHash(hmacPreKey).getData();
        ctx.hmac256().calculate(new SessionKey(hmacKey), rv, enc.length, prep.length, hmac, 0);
        System.arraycopy(hmac, 0, rv, enc.length+prep.length, hmac.length);
        
        return rv;
    }
    
    public static final String strip(String orig) { return strip(orig, "\t\n\r\f", ' '); }
    public static final String strip(String orig, String charsToRemove, char replacement) {
        boolean changed = false;
        if (orig == null) return "";
        char buf[] = orig.toCharArray();
        for (int i = 0; i < buf.length; i++) {
            if (charsToRemove.indexOf(buf[i]) != -1) {
                buf[i] = replacement;
                changed = true;
            }
        }
        if (changed)
            return new String(buf);
        else
            return orig;
    }
    
    boolean verifySig(DBClient client, Signature sig, Hash hash, SigningPublicKey pubKey) {
        return client.ctx().dsa().verifySignature(sig, hash, pubKey);
    }

    public static void parseProps(String data, Properties rv) { parseProps(DataHelper.getUTF8(data), rv); }
    public static void parseProps(byte data[], Properties rv) {
        //System.out.println("parsing props: " + new String(data));
        int off = 0;
        int dataStart = off;
        int valStart = -1;
        while (off < data.length) {
            if (data[off] == '\n') {
                try {
                    String key = new String(data, dataStart, valStart-1-dataStart, "UTF-8");
                    String val = new String(data, valStart, off-valStart, "UTF-8");
                    //System.out.println("Prop parsed: [" + key + "] = [" + val + "] (dataStart=" + dataStart + " valStart " + valStart + " off " + off + ")");
                    rv.setProperty(key, val);
                } catch (UnsupportedEncodingException uee) {
                    //
                } catch (RuntimeException re) {
                    //re.printStackTrace();
                }
                dataStart = off+1;
                valStart = -1;
            } else if ( (data[off] == '=') && (valStart == -1) ) {
                valStart = off+1;
            } else if (off + 1 >= data.length) {
                if ( ( (valStart-1-dataStart) > 0) && ( (off+1-valStart) > 0) ) {
                    try {
                        String key = new String(data, dataStart, valStart-1-dataStart, "UTF-8");
                        String val = new String(data, valStart, off+1-valStart, "UTF-8");
                        //System.out.println("End prop parsed: [" + key + "] = [" + val + "] (dataStart=" + dataStart + " valStart " + valStart + " off " + off + ")");
                        rv.setProperty(key, val);
                    } catch (UnsupportedEncodingException uee) {
                        //
                    } catch (RuntimeException re) {
                        //re.printStackTrace();
                    }
                }
            }
            off++;
        }
    }
}
