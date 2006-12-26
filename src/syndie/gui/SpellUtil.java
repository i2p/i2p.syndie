package syndie.gui;

import com.swabunga.spell.engine.SpellDictionary;
import com.swabunga.spell.engine.SpellDictionaryHashMap;
import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import syndie.db.JobRunner;

/**
 *
 */
public class SpellUtil {
    private static SpellDictionary _dictionary;
    public static SpellDictionary getDictionary() { return _dictionary; }
    
    /*
     * initialize the dictionary, shared across all page editors
     */
    public static void init() {
        JobRunner.instance().enqueue(new Runnable() {
            public void run() {
                try {
                    _dictionary = new SpellDictionaryHashMap(getDictionaryReader());
                } catch (IOException ioe) {
                    // use an empty one
                    try { _dictionary = new SpellDictionaryHashMap(); } catch (IOException ioe2) {}
                }
            }
        });
    }
    private static Reader getDictionaryReader() {
        // read from the db/etc
        try {
            return new InputStreamReader(new FileInputStream("/usr/share/dict/words"), "UTF-8");
        } catch (IOException ioe) {}
        return new InputStreamReader(new ByteArrayInputStream(new byte[0]));
    }    
}
