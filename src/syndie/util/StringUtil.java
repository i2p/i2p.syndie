package syndie.util;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 *  String utilities
 *  @since 1.102b-12 moved from Constants
 */
public class StringUtil {

    /**
     * split on the given character, with the resulting tokens not including that character
     * @return non-null
     */
    public static final String[] split(char elem, String orig) { return split(""+elem, orig); }

    /**
     * split on all of the characters in splitElements, with the resulting tokens not including that character
     * @return non-null
     */
    public static final String[] split(String splitElements, String orig) { return split(splitElements, orig, true); }

    /**
     * @return non-null
     */
    public static final String[] split(String splitElements, String orig, boolean includeZeroSizedElem) {
        List vals = new ArrayList();
        int off = 0;
        int start = 0;
        char str[] = orig.toCharArray();
        while (off < str.length) {
            if (splitElements.indexOf(str[off]) != -1) {
                String val = null;
                if (off-start > 0) {
                    val = new String(str, start, off-start);
                } else {
                    val = "";
                }
                if ( (val.trim().length() > 0) || (includeZeroSizedElem) )
                    vals.add(val);
                start = off+1;
            }
            off++;
        }
        String val;
        if (off-start > 0)
            val = new String(str, start, off-start);
        else 
            val = "";
        if ( (val.trim().length() > 0) || (includeZeroSizedElem) )
            vals.add(val);
        String rv[] = new String[vals.size()];
        for (int i = 0; i < rv.length; i++)
            rv[i] = (String)vals.get(i);
        return rv;
    }
    
    private static final String ALLOWED_FILENAME = "abcdefghijklmnopqrstuvwxyz" +
                                                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
                                                   "0123456789" +
                                                   "._-+";
    
    /** incredibly conservative filter, only allowing ascii alphanum and a few symbols */
    public static String stripFilename(String name, boolean allowPaths) {
        char rv[] = name.toCharArray();
        boolean mod = false;
        for (int i = 0; i < rv.length; i++) {
            if (allowPaths && ( (rv[i] == '/' || rv[i] == '\\') )) {
                rv[i] = File.separatorChar;
                mod = true;
            } else if (ALLOWED_FILENAME.indexOf(rv[i]) == -1) {
                rv[i] = '_';
                mod = true;
            }
        }
        if (!mod) return name;
        return new String(rv);
    }

    /** lowercase that is not dependent upon the user's locale */
    public static final String lowercase(String orig) {
        if (orig == null) return null;
        // for HTML data, we need to ignore the user's locale, otherwise things
        // could get b0rked.  the canonical case here is "TITLE".toLowerCase() with
        // a turkish locale returns "T\u0131tle" (with unicode 0131 being the turkish
        // dotless-i)
        return orig.toLowerCase(Locale.UK);
    }
    
    
    public static final String replace(String orig, String oldval, String newval) { return replace(orig, oldval, newval, 0); }

    public static final String replace(String orig, String oldval, String newval, int howManyReplacements) {
        if ( (orig == null) || (oldval == null) || (oldval.length() <= 0) ) return orig;
        
        StringBuilder rv = new StringBuilder();
        char origChars[] = orig.toCharArray();
        char search[] = oldval.toCharArray();
        int numReplaced = 0;
        for (int i = 0; i < origChars.length; i++) {
            boolean match = true;
            if ((howManyReplacements > 0) && (howManyReplacements <= numReplaced))
                match = false; // matched enough, stop
            for (int j = 0; match && j < search.length && (j + i < origChars.length); j++) {
                if (search[j] != origChars[i+j])
                    match = false;
            }
            if (match) {
                if (newval != null)
                    rv.append(newval);
                i += search.length-1;
                numReplaced++;
            } else {
                rv.append(origChars[i]);
            }
        }
        return rv.toString();
    }
    
/****
    public static void main(String args[]) {
        String split[] = split('\n', "hi\nhow are you?\n\nw3wt\n\nthe above is a blank line");
        for (int i = 0; i < split.length; i++)
            System.out.println(split[i]);
    }
****/
}
