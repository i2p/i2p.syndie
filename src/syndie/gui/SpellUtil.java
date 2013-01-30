package syndie.gui;

import com.swabunga.spell.engine.SpellDictionary;
import com.swabunga.spell.engine.SpellDictionaryHashMap;

import java.io.BufferedInputStream;
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
    private static SpellDictionaryHashMap _dictionary;
    private static volatile boolean _isEnabled;

    private static final boolean _isWin = System.getProperty("os.name").startsWith("Win");

    private static final String PROP_STANDARD_WORDS = "syndie.dict";
    private static final String STANDARD_WORDS = "/usr/share/dict/words";
    private static final String LOCAL_WORDS = "/syndie/gui/localwords.txt";

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
        Reader r = null;
        try {
            r = getDictionaryReader();
            _dictionary = new SpellDictionaryHashMap(r);
            if (_isEnabled) {
                Reader r2 = null;
                try {
                    r2 = getLocalWords();
                    _dictionary.addDictionary(r2);
                } catch (IOException ioe) {
                    System.out.println("Local dictionary could not be loaded: " + ioe);
                } finally {
                    //SpellDictionaryHashMap does not close()
                    if (r2 != null) try { r2.close(); } catch (IOException ioe) {}
                }
            }
            //System.out.println("Dictionary loaded");
        } catch (IOException ioe) {
            // use an empty one
            try { _dictionary = new SpellDictionaryHashMap(); } catch (IOException ioe2) {}
            System.out.println("Dictionary could not be loaded: " + ioe);
            _isEnabled = false;
        } finally {
            //SpellDictionaryHashMap does not close()
            if (r != null) try { r.close(); } catch (IOException ioe) {}
        }
    }

    private static Reader getLocalWords() throws IOException {
        return new InputStreamReader(
                        new BufferedInputStream(SpellUtil.class.getResourceAsStream(LOCAL_WORDS)),
                        "UTF-8");
    }

    private static Reader getDictionaryReader() {
        if (_isWin) {
            System.err.println("Spellchecker disabled");
            return new InputStreamReader(new ByteArrayInputStream(new byte[0]));
        }
        // read from the db/etc
        String dictLocation = System.getProperty(PROP_STANDARD_WORDS, STANDARD_WORDS);
        try {
            Reader rv = new InputStreamReader(
                    new BufferedInputStream(new FileInputStream(dictLocation)),
                    "UTF-8");
            _isEnabled = true;
            return rv;

        } catch (IOException ioe) {
            System.out.println("Dictionary could not be loaded: " + dictLocation);
        }
        return new InputStreamReader(new ByteArrayInputStream(new byte[0]));
    }    
}
