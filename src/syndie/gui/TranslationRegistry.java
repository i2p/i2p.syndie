package syndie.gui;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;
import org.eclipse.swt.graphics.Image;

/**
 *
 */
public class TranslationRegistry {
    private BrowserControl _browser;
    private Set _translatable;
    private String _lang;
    private Properties _text;
    private Map _images;
    private Properties _baseText;
    private Map _baseImages;
    private Map _embeddedTranslations;
    private Map _fileTranslations;
    
    public TranslationRegistry(BrowserControl browser) {
        _browser = browser;
        _translatable = Collections.synchronizedSet(new HashSet());
        _lang = "default";
        _text = new Properties();
        _images = new HashMap();
        _baseText = new Properties();
        _baseImages = new HashMap();
    }
    
    public void register(Translatable entry) { _translatable.add(entry); entry.translate(this); }
    public void unregister(Translatable entry) { _translatable.remove(entry); }
    
    public String getTranslation() { return _lang; }
    public String getText(String key, String defaultVal) { return _text.getProperty(key, defaultVal); }
    /** 
     * get the translation for the given key, using the current language selected and falling
     * back on the base language if it isn't known
     */
    public String getText(String key) {
        String val = _text.getProperty(key);
        if (val == null)
            val = _baseText.getProperty(key);
        return val;
    }
    public Image getImage(String key) {
        Image img = (Image)_images.get(key);
        if (img == null)
            img = (Image)_baseImages.get(key);
        return img;
    }
    
    private void switchTranslation(String newLang, Properties newText, Map newImages) {
        _lang = newLang;
        _text = newText;
        _images = newImages;
        _browser.getUI().debugMessage("switching translation to " + newLang);
        for (Iterator iter = _translatable.iterator(); iter.hasNext(); ) {
            Translatable cur = (Translatable)iter.next();
            _browser.getUI().debugMessage("switching translation to " + newLang + " for " + cur.getClass().getName() + "/" + System.identityHashCode(cur));
            cur.translate(this);
        }
    }
    
    public void switchTranslation(String newLang) {
        if ( (_lang != null) && (newLang.equals(_lang)) ) {
            _browser.getUI().debugMessage("language already in use (" + newLang + ")");
            return; // noop
        }
        
        if ("default".equals(newLang)) {
            switchTranslation(newLang, new Properties(), new HashMap());
        } else {
            Properties txt = (Properties)_fileTranslations.get(newLang);
            if (txt != null) {
                switchTranslation(newLang, txt, new HashMap());
            } else {
                txt = (Properties)_embeddedTranslations.get(newLang);
                if (txt != null) {
                    switchTranslation(newLang, txt, new HashMap());
                }
            }
        }
    }
    
    private static final String KEY_LANG = "LANG";
    private static final String KEY_ISBASE = "ISBASE";
    
    private void refreshEmbeddedTranslations() {
        Properties baseText = null;
        Map translations = new HashMap();
        int translation = 0;
        while (true) {
            BufferedReader reader = null;
            try {
                InputStream in = getClass().getResourceAsStream("/translation_ " + translation + ".txt");
                if (in != null) {
                    reader = new BufferedReader(new InputStreamReader(in, "UTF-8"));
                    Properties props = new Properties();
                    String line = null;
                    String lang = null;
                    while ( (line = reader.readLine()) != null) {
                        int split = line.indexOf(':');
                        if (split <= 0)
                            split = line.indexOf('=');
                        if (split <= 0)
                            continue;
                        if (line.startsWith("/") || line.startsWith("#") || line.startsWith("--")) 
                            continue;
                        String key = line.substring(0, split).trim();
                        String val = line.substring(split+1).trim();
                        if (key.equals(KEY_LANG))
                            lang = val;
                        else if (key.equals(KEY_ISBASE))
                            baseText = props;
                        else
                            props.setProperty(key, val);
                    }
                    reader.close();
                    reader = null;
                    if (lang != null)
                        translations.put(lang, props);
                    translation++;
                } else {
                    break;
                }
            } catch (IOException ioe) {
                _browser.getUI().errorMessage("problem getting the embedded translations", ioe);
            } finally {
                if (reader != null) try { reader.close(); } catch (IOException ioe) {}
            }
        }
        if (baseText != null)
            _baseText = baseText;
        _embeddedTranslations = translations;
    }
    private void refreshFileTranslations() {
        Map translations = new HashMap();
        refreshFileTranslations(translations, _browser.getClient().getRootDir());
        _fileTranslations = translations;
    }
    private void refreshFileTranslations(Map translations, File dir) {
        File files[] = dir.listFiles(new FilenameFilter() {
            public boolean accept(File dir, String name) {
                return name.startsWith("translation_");
            }
        });
        Properties defaultText = null;
        for (int i = 0; i < files.length; i++) {
            BufferedReader reader = null;
            try {
                InputStream in = new FileInputStream(files[i]);
                if (in != null) {
                    reader = new BufferedReader(new InputStreamReader(in, "UTF-8"));
                    Properties props = new Properties();
                    String line = null;
                    String lang = null;
                    while ( (line = reader.readLine()) != null) {
                        int split = line.indexOf(':');
                        if (split <= 0)
                            split = line.indexOf('=');
                        if (split <= 0)
                            continue;
                        if (line.startsWith("/") || line.startsWith("#") || line.startsWith("--")) 
                            continue;
                        String key = line.substring(0, split).trim();
                        String val = line.substring(split+1).trim();
                        if (key.equals(KEY_LANG))
                            lang = val;
                        else if (key.equals(KEY_ISBASE))
                            defaultText = props;
                        else
                            props.setProperty(key, val);
                    }
                    reader.close();
                    reader = null;
                    if (lang != null)
                        translations.put(lang, props);
                }
            } catch (IOException ioe) {
                _browser.getUI().errorMessage("problem getting the file translations", ioe);
            } finally {
                if (reader != null) try { reader.close(); } catch (IOException ioe) {}
            }
        }
        
        if (defaultText != null)
            _baseText = defaultText;
    }
    
    public List getTranslations() {
        refreshEmbeddedTranslations();
        refreshFileTranslations();

        ArrayList rv = new ArrayList();
        rv.add("default");
        TreeSet ordered = new TreeSet();
        ordered.addAll(_embeddedTranslations.keySet());
        ordered.addAll(_fileTranslations.keySet());
        for (Iterator iter = ordered.iterator(); iter.hasNext(); )
            rv.add(iter.next());
        
        return rv;
    }
    
    public void loadTranslations() {
        refreshEmbeddedTranslations();
        refreshFileTranslations();
        
        if (_baseImages == null)
            _baseImages = new HashMap();
        if (_baseText == null)
            _baseText = new Properties();
        
        _text = _baseText;
        _images = _baseImages;
    }
}
