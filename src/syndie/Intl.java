package syndie;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;

/**
 * Internationalization helper
 */
public class Intl {
    private static Map _loaded = new HashMap(4);
    private static Intl _default = new Intl("EN", "GB");
    public static Intl getDefault() { return _default; }
    public static Intl get(String lang, String region) {
        Intl rv = (Intl)_loaded.get(lang + "_" + region);
        if (rv == null)
            rv = _default;
        return rv;
    }
    
    private String _lang;
    private String _region;
    private Properties _props;
    private Intl(String lang, String region) {
        _region = region;
        _lang = lang;
        _props = new Properties();
        load();
    }
    private void load() {
        try {
            String name = "intl-" + _lang + "_" + _region + ".properties";
            File f = new File("resources", name);
            InputStream in = null;
            if (f.exists()) {
                System.out.println("Loading " + f.getAbsolutePath());
                in = new FileInputStream(f);
            } else {
                System.out.println("Could not load " + f.getAbsolutePath());
                in = getClass().getResourceAsStream(name);
            }
            if (in != null)
                _props.load(in);
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
    }
    
    public String getString(String key) { 
        String rv = _props.getProperty(key);
        if ( (rv == null) && (this != _default) )
            rv = _default.getString(key);
        if (rv == null) {
            System.out.println("internationalized key not found [" + key + "]");
            rv = key; //rv = "";
        }
        return rv;
    }
}
