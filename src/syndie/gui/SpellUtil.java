package syndie.gui;

import com.swabunga.spell.engine.SpellDictionary;
import com.swabunga.spell.engine.SpellDictionaryHashMap;
import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import net.i2p.util.SimpleTimer2;
import syndie.db.JobRunner;

/**
 *
 */
public class SpellUtil {
    private static SpellDictionary _dictionary;
    private static boolean _isEnabled;

    private static final boolean _isWin = System.getProperty("os.name").startsWith("Win");

    public static SpellDictionary getDictionary() { 
        synchronized (SpellUtil.class) {
            if (_dictionary == null) buildDictionary();
            return _dictionary; 
        }
    }
    
    public static boolean isEnabled() {
        return _isEnabled;
    }

    /*
     * initialize the dictionary, shared across all page editors.  this runs
     * asynchronously and will wait 10 seconds before attempting to init the dictionary
     * (to reduce load on startup, but to prepopulate the dictionary before its required
     * for spellchecking)
     */
    public static void init() {
    	InitEvent evt = new InitEvent();
    	evt.schedule(10 *1000);
    }
    
    private static class InitEvent extends SimpleTimer2.TimedEvent {
    	InitEvent(){
    		super(SimpleTimer2.getInstance());
    	}
    	public void timeReached() {
    		JobRunner.instance().enqueue(new Runnable() {
    			public void run() { getDictionary(); }
    		});
    	}
    }
    private static void buildDictionary() {
        try {
            _dictionary = new SpellDictionaryHashMap(getDictionaryReader());
            System.out.println("Dictionary loaded");
        } catch (IOException ioe) {
            // use an empty one
            try { _dictionary = new SpellDictionaryHashMap(); } catch (IOException ioe2) {}
            System.out.println("Dictionary could not be loaded: " + ioe);
            _isEnabled = false;
        }
    }
    private static Reader getDictionaryReader() {
        if (_isWin) {
            System.err.println("Spellchecker disabled");
            return new InputStreamReader(new ByteArrayInputStream(new byte[0]));
        }
        // read from the db/etc
        String dictLocation = System.getProperty("syndie.dict", "/usr/share/dict/words");
        try {
            Reader rv = new InputStreamReader(new FileInputStream(dictLocation), "UTF-8");
            _isEnabled = true;
            return rv;

        } catch (IOException ioe) {
            System.out.println("Dictionary could not be loaded: " + dictLocation);
        }
        return new InputStreamReader(new ByteArrayInputStream(new byte[0]));
    }    
}
