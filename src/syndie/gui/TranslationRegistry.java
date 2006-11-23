package syndie.gui;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import org.eclipse.swt.graphics.Image;

/**
 *
 */
public class TranslationRegistry {
    private Set _translatable;
    private String _lang;
    private Properties _text;
    private Map _images;
    
    public TranslationRegistry() {
        _translatable = Collections.synchronizedSet(new HashSet());
        _lang = "default";
        _text = new Properties();
        _images = new HashMap();
    }
    
    public void register(Translatable entry) { _translatable.add(entry); entry.translate(this); }
    public void unregister(Translatable entry) { _translatable.remove(entry); }
    
    public String getTranslation() { return _lang; }
    public String getText(String key, String defaultVal) { return _text.getProperty(key, defaultVal); }
    public Image getImage(String key) { return (Image)_images.get(key); }
    
    // todo: how does newText/newImages get populated
    private void switchTranslation(String newLang, Properties newText, Map newImages) {
        _lang = newLang;
        _text = newText;
        _images = newImages;
        for (Iterator iter = _translatable.iterator(); iter.hasNext(); )
            ((Translatable)iter.next()).translate(this);
    }
}
