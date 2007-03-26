package net.i2p.crypto;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;
import net.i2p.I2PAppContext;
import net.i2p.data.Base64;
import net.i2p.data.DataHelper;
import net.i2p.data.Hash;

import gnu.crypto.hash.Sha256Standalone;

/** 
 * Defines a wrapper for SHA-256 operation.  All the good stuff occurs
 * in the GNU-Crypto {@link gnu.crypto.hash.Sha256Standalone}
 * 
 */
public final class SHA256Generator {
    private List _digests;
    private List _digestsGnu;
    public SHA256Generator(I2PAppContext context) {
        _digests = new ArrayList(32);
        _digestsGnu = new ArrayList(32);
    }
    
    public static final SHA256Generator getInstance() {
        return I2PAppContext.getGlobalContext().sha();
    }
    
    /** Calculate the SHA-256 has of the source
     * @param source what to hash
     * @return hash of the source
     */
    public final Hash calculateHash(byte[] source) {
        return calculateHash(source, 0, source.length);
    }
    public final Hash calculateHash(byte[] source, int start, int len) {
        Sha256Standalone digest = acquireGnu();
        digest.update(source, start, len);
        byte rv[] = digest.digest();
        releaseGnu(digest);
        return new Hash(rv);
    }
    
    public final void calculateHash(byte[] source, int start, int len, byte out[], int outOffset) {
        Sha256Standalone digest = acquireGnu();
        digest.update(source, start, len);
        byte rv[] = digest.digest();
        releaseGnu(digest);
        System.arraycopy(rv, 0, out, outOffset, rv.length);
    }
    
    public final void calculateHash(byte[] source, int start, int len, byte out[], int outOffset, int rounds) {
        Sha256Standalone digest = new Sha256Standalone();
        for (int i = 0; i < rounds; i++) {
            digest.update(source, start, len);
            source = digest.digest();
            digest.reset();
        }
        System.arraycopy(source, 0, out, outOffset, source.length);
    }
    
    public static boolean NO_CACHE = false;
    
    private Sha256Standalone acquireGnu() {
        if (NO_CACHE) return new Sha256Standalone();
        Sha256Standalone rv = null;
        synchronized (_digestsGnu) {
            if (_digestsGnu.size() > 0)
                rv = (Sha256Standalone)_digestsGnu.remove(0);
        }
        if (rv != null)
            rv.reset();
        else
            rv = new Sha256Standalone();
        return rv;
    }
    
    private void releaseGnu(Sha256Standalone digest) {
        if (NO_CACHE) return;
        synchronized (_digestsGnu) {
            if (_digestsGnu.size() < 32) {
                _digestsGnu.add(digest);
            }
        }
    }
    
    public static void main(String args[]) {
        I2PAppContext ctx = I2PAppContext.getGlobalContext();
        for (int i = 0; i < args.length; i++)
            System.out.println("SHA256 [" + args[i] + "] = [" + Base64.encode(ctx.sha().calculateHash(args[i].getBytes()).getData()) + "]");
        Hash base = ctx.sha().calculateHash(new byte[] { 0x01, 0x02, 0x03, 0x04 });
        
        byte h1[] = new byte[Hash.HASH_LENGTH];
        byte h2[] = new byte[Hash.HASH_LENGTH];
        System.arraycopy(base.getData(), 0, h1, 0, h1.length);
        System.arraycopy(base.getData(), 0, h2, 0, h2.length);
        long t1 = System.currentTimeMillis();
        for (int i = 0; i < 1000; i++)
            ctx.sha().calculateHash(h1, 0, Hash.HASH_LENGTH, h1, 0);
        long t2 = System.currentTimeMillis();
        ctx.sha().calculateHash(h2, 0, Hash.HASH_LENGTH, h2, 0, 1000);
        long t3 = System.currentTimeMillis();
        System.out.println("eq: " + DataHelper.eq(h1, h2) + " time: " + (t2-t1) + "/" + (t3-t2));
    }
}