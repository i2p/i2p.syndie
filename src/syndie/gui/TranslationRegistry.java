package syndie.gui;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import net.i2p.I2PAppContext;
import net.i2p.util.ConcurrentHashSet;
import net.i2p.util.Translate;

import org.eclipse.swt.graphics.Image;

import syndie.db.UI;

/**
 *  Formerly used property files inside the jar and in the directory.
 *  Now using I2P's Messages API and .po files.
 */
public class TranslationRegistry {
    private final I2PAppContext _context;
    private final UI _ui;
    private final File _rootDir;
    private final Set<Translatable> _translatable;
    private Map<String, Image> _images;
    private Map<String, Image> _baseImages;
    
    private static final String BUNDLE = "syndie.locale.messages";

    public TranslationRegistry(UI ui, File rootDir) {
        _context = I2PAppContext.getGlobalContext();
        _ui = ui;
        _rootDir = rootDir;
        _translatable = new ConcurrentHashSet();
        // images are unused
        _images = new HashMap();
        _baseImages = new HashMap();
    }
    
    public void register(Translatable entry) { _translatable.add(entry); entry.translate(this); }

    public void unregister(Translatable entry) { _translatable.remove(entry); }
    
    public String getTranslation() { return Translate.getLanguage(_context); }

    /**
     * retrieve the translation's value for the given key, or the default value if no translated
     * value is known.  the returned string is postprocessed as well, turning any newlines into
     * the current platform's newline character
     *
     * @param key ignored, to be removed
     */
    public String getText(String key, String defaultVal) { 
        return getText(defaultVal);
    }
    
    private static final String NL = System.getProperty("line.separator");

    private static final String postprocess(String val) {
        if (val.indexOf('\n') == -1)
            return val;
        StringBuilder buf = new StringBuilder();
        int len = val.length();
        for (int i = 0; i < len; i++) {
            char c = val.charAt(i);
            if (c == '\n')
                buf.append(NL);
            else
                buf.append(c);
        }
        return buf.toString();
    }
    
    /** 
     * get the translation for the given key, using the current language selected and falling
     * back on the base language if it isn't known
     */
    public String getText(String key) {
        String val = Translate.getString(key, _context, BUNDLE);
        return postprocess(val);
    }

    public String ngettext(String s, String p, int n) {
        return Translate.getString(n, s, p, _context, BUNDLE);
    }

    /** @deprecated unused */
    public Image getImage(String key) {
        Image img = (Image)_images.get(key);
        if (img == null)
            img = (Image)_baseImages.get(key);
        return img;
    }
    
    /**
     * @param newText ignored, to be removed
     */
    private void switchTranslation(String newLang, Properties newText, Map newImages) {
        if (_context.isRouterContext()) {
            _ui.errorMessage("use the router console to change languages");
            return;
        }
        // TODO store lang in database???
        _images = newImages;
        _ui.debugMessage("switching translation to " + newLang);
        // context falls back to system properties
        System.setProperty(Translate.PROP_LANG, newLang);
        for (Translatable cur : _translatable) {
            _ui.debugMessage("switching translation to " + newLang + " for " + cur.getClass().getName() + "/" + System.identityHashCode(cur));
            cur.translate(this);
        }
    }
    
    public void switchTranslation(String newLang) {
        String lang = getTranslation();
        if (newLang.equals(lang)) {
            _ui.debugMessage("language already in use (" + newLang + ")");
            return; // noop
        }
        switchTranslation(newLang, null, new HashMap());
    }
    
    public List<String> getTranslations() {
        // remove this, or look for class files, or add to Translate API?
        ArrayList<String> rv = new ArrayList();
        rv.add("en");
        rv.add("de");
        rv.add("ru");
        rv.add("xx");
        return rv;
    }
    
    public void loadTranslations() {}
}
