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
    private Map _embeddedTranslations;
    private Map _fileTranslations;
    
    public TranslationRegistry(BrowserControl browser) {
        _browser = browser;
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
    
    private void refreshEmbeddedTranslations() {
        Map translations = new HashMap();
        int translation = 0;
        while (true) {
            try {
                InputStream in = getClass().getResourceAsStream("/translation_ " + translation + ".txt");
                if (in != null) {
                    BufferedReader reader = new BufferedReader(new InputStreamReader(in, "UTF-8"));
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
                        else
                            props.setProperty(key, val);
                    }
                    in.close();
                    if (lang != null)
                        translations.put(lang, props);
                    translation++;
                } else {
                    break;
                }
            } catch (IOException ioe) {
                _browser.getUI().errorMessage("problem getting the embedded translations", ioe);
            }
        }
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
        for (int i = 0; i < files.length; i++) {
            try {
                InputStream in = new FileInputStream(files[i]);
                if (in != null) {
                    BufferedReader reader = new BufferedReader(new InputStreamReader(in, "UTF-8"));
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
                        else
                            props.setProperty(key, val);
                    }
                    in.close();
                    if (lang != null)
                        translations.put(lang, props);
                }
            } catch (IOException ioe) {
                _browser.getUI().errorMessage("problem getting the file translations", ioe);
            }
        }
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
}
