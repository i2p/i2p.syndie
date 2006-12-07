package syndie.gui;

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

/**
 *
 */
public class ThemeRegistry {
    private BrowserControl _browser;
    private Set _listeners = Collections.synchronizedSet(new HashSet());
    private Theme _cur;
    
    public ThemeRegistry(BrowserControl browser) {
        _browser = browser;
        _cur = Theme.getDefault();
        //loadTheme();
        // the SWT.Settings event is fired when the user adjust their OSes system
        // theme (adjusting fonts, colors, etc).  we may want to revamp our fonts
        // based on that
        Display.getDefault().addListener(SWT.Settings, new Listener() {
            public void handleEvent(Event event) {
                ThemeRegistry.this.notifyAll(_cur);
            }
        });
    }
    
    public void register(Themeable lsnr) { 
        _browser.getUI().debugMessage("register & apply theme to " + lsnr.getClass().getName() + "/" + System.identityHashCode(lsnr));
        _listeners.add(lsnr);
        lsnr.applyTheme(_cur); 
    }
    public void unregister(Themeable lsnr) { 
        _browser.getUI().debugMessage("unregister " + lsnr.getClass().getName() + "/" + System.identityHashCode(lsnr));
        _listeners.remove(lsnr); 
    }
    
    public Theme getTheme() { return _cur; }
    
    private void notifyAll(Theme theme) {
        for (Iterator iter = _listeners.iterator(); iter.hasNext(); ) {
            Themeable cur = (Themeable)iter.next();
            String err = theme.validate();
            if (err == null) {
                long before = System.currentTimeMillis();
                cur.applyTheme(theme);
                long after = System.currentTimeMillis();
                _browser.getUI().debugMessage("apply theme to " + cur.getClass().getName() + "/" + System.identityHashCode(cur) + " took " + (after-before));
            } else {
                _browser.getUI().errorMessage("cannot apply theme: " + err);
            }
        }
    }

    public void increaseFont() {
        _browser.getUI().debugMessage("increasing font size");
        long before = System.currentTimeMillis();
        _cur.increaseFont();
        long t1 = System.currentTimeMillis();
        notifyAll(_cur);
        long t2 = System.currentTimeMillis();
        saveTheme();
        long t3 = System.currentTimeMillis();
        _browser.getUI().debugMessage("font adjust time: " + (t1-before) + ", notify: " + (t2-t1) + ", save: " + (t3-t2));
    }
    public void decreaseFont() {
        _browser.getUI().debugMessage("decreasing font size");
        long before = System.currentTimeMillis();
        _cur.decreaseFont();
        long t1 = System.currentTimeMillis();
        notifyAll(_cur);
        long t2 = System.currentTimeMillis();
        saveTheme();
        long t3 = System.currentTimeMillis();
        _browser.getUI().debugMessage("font adjust time: " + (t1-before) + ", notify: " + (t2-t1) + ", save: " + (t3-t2));
    }
    
    private void saveTheme() {
        Properties prefs = _browser.getClient().getNymPrefs(_browser.getClient().getLoggedInNymId());
        _cur.store(prefs);
        _browser.getClient().setNymPrefs(_browser.getClient().getLoggedInNymId(), prefs);
    }
    public void loadTheme() {
        Properties prefs = _browser.getClient().getNymPrefs(_browser.getClient().getLoggedInNymId());
        if (_cur != null) {
            _browser.getUI().debugMessage("disposing old theme");
            _cur.dispose();
        }
        _cur = Theme.getTheme(prefs);
        notifyAll(_cur);
    }
    public void resetTheme() {
        Properties prefs = _browser.getClient().getNymPrefs(_browser.getClient().getLoggedInNymId());
        for (Iterator iter = prefs.keySet().iterator(); iter.hasNext(); ) {
            String key = (String)iter.next();
            if (key.startsWith("theme."))
                iter.remove();
        }
        _browser.getClient().setNymPrefs(_browser.getClient().getLoggedInNymId(), prefs);
        if (_cur != null) {
            _browser.getUI().debugMessage("disposing old theme");
            _cur.dispose();
        }
        _cur = Theme.getTheme(prefs);
        notifyAll(_cur);
    }
}
