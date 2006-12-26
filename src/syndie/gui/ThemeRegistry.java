package syndie.gui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.Properties;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

/**
 *
 */
public class ThemeRegistry {
    private Browser _browser;
    private ArrayList _listeners;
    private Theme _cur;
    
    public ThemeRegistry(Browser browser) {
        _browser = browser;
        _listeners = new ArrayList();
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
        synchronized (_listeners) {
            _listeners.add(lsnr);
        }
        lsnr.applyTheme(_cur); 
    }
    public void unregister(Themeable lsnr) { 
        _browser.getUI().debugMessage("unregister " + lsnr.getClass().getName() + "/" + System.identityHashCode(lsnr));
        synchronized (_listeners) {
            _listeners.remove(lsnr);
        }
    }
    
    public Theme getTheme() { return _cur; }
    
    private void notifyAll(Theme theme) {
        // we need to make sure the browser is themed last, so that when it 
        // calls a recursive layout, it uses rethemed component info.
        Object lsnrs[] = null;
        synchronized (_listeners) {
            lsnrs = _listeners.toArray();
        }
        for (int i = 0; i < lsnrs.length; i++) {
            Themeable cur = (Themeable)lsnrs[i];
            if (cur == _browser) continue;
            String err = null; //theme.validate();
            if (err == null) {
                long before = System.currentTimeMillis();
                cur.applyTheme(theme);
                long after = System.currentTimeMillis();
                _browser.getUI().debugMessage("apply theme to " + cur.getClass().getName() + "/" + System.identityHashCode(cur) + " took " + (after-before));
            } else {
                _browser.getUI().errorMessage("cannot apply theme: " + err);
            }
        }
        
        long before = System.currentTimeMillis();
        _browser.getUI().debugMessage("beginning applyTheme to the browser");
        CustomStyledText.IGNORE_FORCE = true;
        _browser.applyTheme(theme);
        CustomStyledText.IGNORE_FORCE = false;
        long after = System.currentTimeMillis();
        _browser.getUI().debugMessage("finally, apply theme to the browser: " + (after-before));
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
