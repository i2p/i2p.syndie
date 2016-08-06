package syndie.gui;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import net.i2p.I2PAppContext;
import net.i2p.util.ConcurrentHashSet;
import net.i2p.util.Translate;

import org.eclipse.swt.SWTException;
import org.eclipse.swt.graphics.Image;

import syndie.db.DBClient;
import syndie.db.UI;

/**
 *  Formerly used property files inside the jar and in the directory.
 *  Now using I2P's Messages API and .po files.
 */
public class TranslationRegistry {
    private final I2PAppContext _context;
    private final DBClient _client;
    private final UI _ui;
    private final File _rootDir;
    private final Set<Translatable> _translatable;
    private Map<String, Image> _images;
    private Map<String, Image> _baseImages;
    private static final String PROP_LANG = "gui.lang";
    
    /**
     * Two- or three-letter lower-case language codes, with optional _XX upper-case country codes,
     * order not important, UI will sort
     */
    private static final String[] SUPPORTED_TRANSLATIONS = {
        "en", "de", "ru", "sv", "es", "fr", "pt", "pl", "ro", "el", "it", "nl", "pt_BR", "tr", "uk", "zh",
        "fi", "nb"
    };

    private static final List<String> AVAILABLE_TRANSLATIONS;

    static {
        List<String> avail = new ArrayList(24);
        avail.addAll(Arrays.asList(SUPPORTED_TRANSLATIONS));
        String cur = Locale.getDefault().getLanguage();
        if (!avail.contains(cur))
            avail.add(cur);
        AVAILABLE_TRANSLATIONS = Collections.unmodifiableList(avail);
    }

    private static final String BUNDLE = "syndie.locale.messages";

    public TranslationRegistry(DBClient client, UI ui, File rootDir) {
        _context = I2PAppContext.getGlobalContext();
        _client = client;
        _ui = ui;
        _rootDir = rootDir;
        _translatable = new ConcurrentHashSet();
        // images are unused
        _images = new HashMap();
        _baseImages = new HashMap();
    }
    
    public void register(Translatable entry) { _translatable.add(entry); entry.translate(this); }

    public void unregister(Translatable entry) { _translatable.remove(entry); }
    
    /**
     * @return 2- or 3-letter lower-case ISO-639 codes, with optional _XX upper case country code
     */
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

    public String getText(String key, Object o) {
        String val = Translate.getString(key, o, _context, BUNDLE);
        return val;
    }

    public String getText(String key, Object o1, Object o2) {
        String val = Translate.getString(key, o1, o2, _context, BUNDLE);
        return val;
    }

    public String ngettext(String s, String p, int n) {
        return Translate.getString(n, s, p, _context, BUNDLE);
    }

    /** @deprecated unused */
    public Image getImage(String key) {
        Image img = _images.get(key);
        if (img == null)
            img = _baseImages.get(key);
        return img;
    }
    
    /**
     * Does not save to the database
     * @param newLang 2-or 3-letter lower-case ISO-639 code, and optional _XX upper case country code
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
        int under = newLang.indexOf("_");
        if (under <= 0) {
            System.setProperty(Translate.PROP_LANG, newLang);
            System.clearProperty(Translate.PROP_COUNTRY);
        } else {
            System.setProperty(Translate.PROP_LANG, newLang.substring(0, under));
            System.setProperty(Translate.PROP_COUNTRY, newLang.substring(under + 1));
        }
        for (Translatable cur : _translatable) {
            try {
                _ui.debugMessage("switching translation to " + newLang + " for " + cur.getClass().getName() + "/" + System.identityHashCode(cur));
                cur.translate(this);
            } catch (SWTException se) {
                // don't let disposed widget break all registered Translatables
                _ui.debugMessage("Translation switch failed for " + cur.getClass().getName(), se);
            }
        }
    }
    
    /**
     * Saves to the database, if logged in
     * @param newLang 2- or 3-letter lower-case ISO-639 code, with optional _XX upper case country code
     */
    public void switchTranslation(String newLang) {
        String lang = getTranslation();
        if (newLang.equals(lang)) {
            _ui.debugMessage("language already in use (" + newLang + ")");
            return; // noop
        }
        switchTranslation(newLang, null, new HashMap());
        saveLang(newLang);
    }
    
    /**
     * @return 2- or 3-letter lower-case ISO-639 codes, with optional _XX upper case country code, unsorted
     */
    public List<String> getTranslations() {
        return AVAILABLE_TRANSLATIONS;
    }
    
    /** NOOP */
    public void loadTranslations() {}
    
    /**
     * Switch to the preference stored in the database.
     * DBClient must be logged in
     *
     * @since 1.106b-2
     */
    public void switchTranslations() {
        if (_client != null && _client.isLoggedIn()) {
            Properties prefs = _client.getNymPrefs();
            String pref = prefs.getProperty(PROP_LANG);
            if (pref != null && !pref.equals(getTranslation())) {
                //_translatable is empty here so this won't do any calbacks
                switchTranslation(pref, null, _images);
            }
        }
    }

    /**
     * Saves to the database, if logged in
     * @param newLang 2- or 3-letter lower-case ISO-639 code, with optional _XX upper case country code
     * @since 1.106b-2
     */
    private void saveLang(String newLang) {
        if (_client == null || !_client.isLoggedIn()) return;
        Properties prefs = _client.getNymPrefs();
        prefs.setProperty(PROP_LANG, newLang);
        _client.setNymPrefs(prefs);
    }
}
