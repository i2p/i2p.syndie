package syndie.gui;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintWriter;

import net.i2p.data.Hash;

/**
 *  Misc. stuff
 *  @since 1.102b-5
 */
public class UIUtil {

    public static final int MAX_NAME_LEN = 28;
    public static final int MAX_DESCRIPTION_LEN = 100;
    public static final int MAX_SUBJECT_LEN = 50;

    /**
     *  Consistent display of forums and authors
     *
     *  @param name may be null
     *  @param hash may be null
     */
    public static String displayName(String name, Hash hash) {
        StringBuilder buf = new StringBuilder(64);
        if (name != null)
            buf.append(truncate(name, MAX_NAME_LEN)).append(' ');
        buf.append('[');
        if (hash != null)
            buf.append(hash.toBase64().substring(0, 6));
        else
            buf.append("??????");
        buf.append(']');
        return buf.toString();
    }

    /**
     *  Consistent display hashes
     *
     *  @param hash may be null
     *  @since 1.102b-7
     */
    public static String display(Hash hash) {
        StringBuilder buf = new StringBuilder(8);
        buf.append('[');
        if (hash != null)
            buf.append(hash.toBase64().substring(0, 6));
        else
            buf.append("??????");
        buf.append(']');
        return buf.toString();
    }

    /**
     *  From i2p LogRecordFormatter
     *
     *  @since 1.102b-8
     */
    public static void display(StringBuilder buf, Throwable t) {
        ByteArrayOutputStream baos = new ByteArrayOutputStream(512);
        PrintWriter pw = new PrintWriter(baos, true);
        t.printStackTrace(pw);
        try {
            pw.flush();
            baos.flush();
        } catch (IOException ioe) {}
        byte tb[] = baos.toByteArray();
        buf.append(new String(tb));
    }

    /**
     *  Truncate and add ... if truncated
     *
     *  @since 1.102b-8
     */
    public static String truncate(String s, int max) {
        if (s == null)
            return  "";
        if (s.length() <= max)
            return s;
        return s.substring(0, max - 3) + "...";
    }
}
