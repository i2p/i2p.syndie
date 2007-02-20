package syndie.gui;

import com.swabunga.spell.engine.SpellDictionary;
import com.swabunga.spell.engine.SpellDictionaryHashMap;
import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import net.i2p.util.SimpleTimer;
import syndie.db.JobRunner;

/**
 *
 */
public class SpellUtil {
    private static SpellDictionary _dictionary;
    public static SpellDictionary getDictionary() { 
        synchronized (SpellUtil.class) {
            if (_dictionary == null) buildDictionary();
            return _dictionary; 
        }
    }
    
    /*
     * initialize the dictionary, shared across all page editors.  this runs
     * asynchronously and will wait 10 seconds before attempting to init the dictionary
     * (to reduce load on startup, but to prepopulate the dictionary before its required
     * for spellchecking)
     */
    public static void init() {
        SimpleTimer.getInstance().addEvent(new SimpleTimer.TimedEvent() {
            public void timeReached() {
                JobRunner.instance().enqueue(new Runnable() {
                    public void run() { getDictionary(); }
                });
            }
        }, 10*1000);
    }
    private static void buildDictionary() {
        try {
            _dictionary = new SpellDictionaryHashMap(getDictionaryReader());
        } catch (IOException ioe) {
            // use an empty one
            try { _dictionary = new SpellDictionaryHashMap(); } catch (IOException ioe2) {}
        }
    }
    private static Reader getDictionaryReader() {
        // read from the db/etc
        String dictLocation = System.getProperty("syndie.dict", "/usr/share/dict/words");
        try {
            return new InputStreamReader(new FileInputStream(dictLocation), "UTF-8");
        } catch (IOException ioe) {
            System.out.println("Dictionary could not be loaded: " + dictLocation);
        }
        return new InputStreamReader(new ByteArrayInputStream(new byte[0]));
    }    
}
