package syndie.gui;

import net.i2p.data.Hash;

/**
 *  Misc. stuff
 *  @since 1.102b-5
 */
public class UIUtil {

    /**
     *  Consistent display of forums and authors
     *
     *  @param name may be null
     *  @param hash may be null
     */
    public static String displayName(String name, Hash hash) {
        StringBuilder buf = new StringBuilder(64);
        if (name != null)
            buf.append(name).append(' ');
        buf.append('[');
        if (hash != null)
            buf.append(hash.toBase64().substring(0, 6));
        else
            buf.append("??????");
        buf.append(']');
        return buf.toString();
    }
}
