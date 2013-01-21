package syndie.util;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;

/**
 *  Date / time utilities
 *  @since 1.102b-11 moved from Constants
 */
public class DateTime {

    /** one day in the future */
    private static final long LATEST_DATE = 24*60*60*1000l;
    /** 1/1/2000 */
    private static final long EARLIEST_DATE = 946700000 * 1000l;
    // todo translate
    private static final String INVALID_DATE = "Invalid date";
    private static final String UNSET_DATE = "Date not set";

    private static final DateFormat _dayFmt = DateFormat.getDateInstance(DateFormat.SHORT);

    /**
     *  Current locale.
     *  Returns error string if too old or too far in future.
     */
    public static final String getDate(long when) { 
        if (when <= 0)
            return UNSET_DATE;
        if (when < EARLIEST_DATE || when > System.currentTimeMillis() + LATEST_DATE)
            return INVALID_DATE;
        synchronized (_dayFmt) { 
            return _dayFmt.format(new Date(when)); 
        } 
    }

    private static final DateFormat _dayFmtMedium = DateFormat.getDateInstance(DateFormat.MEDIUM);
    private static final DateFormat _dateTimeFmt = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT);

    /**
     *  Current locale.
     *  Displays date only if older than a week.
     *  Returns error string if too old or too far in future.
     */
    public static final String getDateTime(long when) {
        if (when <= 0)
            return UNSET_DATE;
        long now = System.currentTimeMillis();
        if (when < EARLIEST_DATE || when > now + LATEST_DATE)
            return INVALID_DATE;
        DateFormat fmt;
        if (when > now - 7*24*60*60*1000l)
            fmt = _dateTimeFmt;
        else
            fmt = _dayFmtMedium;
        synchronized (fmt) { 
            return fmt.format(new Date(when)); 
        }
    }

    /**
     *  Current locale. Tries several variants.
     *  @since 1.102b-5
     */
    public static final Date parseDateTime(String when) throws ParseException {
        synchronized (_dateTimeFmt) { 
            try {
                return _dateTimeFmt.parse(when); 
            } catch (ParseException pe) {}
        }
        synchronized (_dayFmtMedium) { 
            try {
                return _dayFmtMedium.parse(when); 
            } catch (ParseException pe) {}
        }
        synchronized (_dayFmt) { 
            return _dayFmt.parse(when); 
        }
    }
}
