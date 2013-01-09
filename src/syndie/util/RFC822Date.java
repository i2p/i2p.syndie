package syndie.util;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/**
 *  Modded from net.i2p.router.util
 *  @since 1.102b-3
 */
public class RFC822Date {

    // SimpleDateFormat is not thread-safe, methods must be synchronized

    private static final SimpleDateFormat OUTPUT_FORMAT = new SimpleDateFormat("d MMM yyyy HH:mm:ss Z", Locale.US);

    /**
     * http://jimyjoshi.com/blog/2007/08/rfc822dateparsinginjava.html
     * Apparently public domain
     * Probably don't need all of these...
     */
    private static final SimpleDateFormat rfc822DateFormats[] = new SimpleDateFormat[] {
                 OUTPUT_FORMAT,
                 new SimpleDateFormat("EEE, d MMM yy HH:mm:ss Z", Locale.US),
                 new SimpleDateFormat("EEE, d MMM yy HH:mm Z", Locale.US),
                 new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z", Locale.US),
                 new SimpleDateFormat("EEE, d MMM yyyy HH:mm Z", Locale.US),
                 new SimpleDateFormat("d MMM yy HH:mm Z", Locale.US),
                 new SimpleDateFormat("d MMM yy HH:mm:ss Z", Locale.US),
                 new SimpleDateFormat("d MMM yyyy HH:mm Z", Locale.US)
    };

    //
    // The router JVM is forced to UTC but we are not so do it here
    //
    static {
        TimeZone utc = TimeZone.getTimeZone("GMT");
        for (int i = 0; i < rfc822DateFormats.length; i++) {
            rfc822DateFormats[i].setTimeZone(utc);
        }
    }

    /**
     * new Date(String foo) is deprecated, so let's do this the hard way
     *
     * @param s non-null
     * @return -1 on failure
     */
    public synchronized static long parse822Date(String s) {
        for (int i = 0; i < rfc822DateFormats.length; i++) {
            try {
                Date date = rfc822DateFormats[i].parse(s);
                if (date != null)
                    return date.getTime();
            } catch (ParseException pe) {}
        }
        return -1;
    }

    /**
     * Format is "d MMM yyyy HH:mm:ss Z"
     *
     * @since 0.8.2
     */
    public synchronized static String to822Date(long t) {
        return OUTPUT_FORMAT.format(new Date(t));
    }

    public static void main(String[] args) {
        if (args.length == 1) {
            try {
                System.out.println(to822Date(Long.parseLong(args[0])));
            } catch (NumberFormatException nfe) {
                System.out.println(nfe.toString());
            }
        } else {
            System.out.println("Usage: RFC822Date numericDate");
        }
    }
}
