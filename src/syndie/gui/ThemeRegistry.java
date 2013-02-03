package syndie.gui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.Properties;

import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

import syndie.db.DBClient;
import syndie.db.UI;

/**
 *  All changes save to database.
 */
public class ThemeRegistry {
    private final ArrayList<Themeable> _listeners;
    private Theme _cur;
    private boolean _themeLoaded;
    private final DBClient _client;
    private final UI _ui;
    private Themeable _toThemeLast;
    
    public ThemeRegistry(DBClient client, UI ui, Themeable toThemeLast) {
        _client = client;
        _ui = ui;
        _toThemeLast = toThemeLast;
        _listeners = new ArrayList();
        _cur = Theme.getDefault();
        _themeLoaded = false;
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
    
    public void setToThemeLast(Themeable themeable) { _toThemeLast = themeable; }
    
    public void register(Themeable lsnr) { 
        //if (_ui != null)
        //    _ui.debugMessage("register & apply theme to " + lsnr.getClass().getName() + "/" + System.identityHashCode(lsnr));
        if (lsnr != _toThemeLast) {
            synchronized (_listeners) {
                _listeners.add(lsnr);
            }
        }
        lsnr.applyTheme(_cur); 
    }

    public void unregister(Themeable lsnr) { 
        //if (_ui != null)
        //    _ui.debugMessage("unregister " + lsnr.getClass().getName() + "/" + System.identityHashCode(lsnr));
        synchronized (_listeners) {
            _listeners.remove(lsnr);
        }
    }
    
    public Theme getTheme() { return _cur; }
    
    private void notifyAll(Theme theme) {
        // we need to make sure the browser is themed last, so that when it 
        // calls a recursive layout, it uses rethemed component info.
        Themeable lsnrs[];
        synchronized (_listeners) {
            lsnrs = _listeners.toArray(new Themeable[_listeners.size()]);
        }
        for (int i = 0; i < lsnrs.length; i++) {
            Themeable cur = lsnrs[i];
            if (cur == _toThemeLast) continue;
            String err = null; //theme.validate();
            if (err == null) {
                try {
                    long before = System.currentTimeMillis();
                    cur.applyTheme(theme);
                    long after = System.currentTimeMillis();
                    if (_ui != null)
                        _ui.debugMessage("apply theme to " + cur.getClass().getName() + "/" + System.identityHashCode(cur) + " took " + (after-before));
                } catch (SWTException se) {
                    // don't let disposed widget break all registered Themeables
                    if (_ui != null)
                        _ui.debugMessage("Theme switch failed for " + cur.getClass().getName(), se);
                }
            } else {
                if (_ui != null)
                    _ui.errorMessage("cannot apply theme: " + err);
            }
        }
        
        long before = System.currentTimeMillis();
        if (_ui != null)
            _ui.debugMessage("beginning applyTheme to the browser");
        CustomStyledText.IGNORE_FORCE = true;
        if (_toThemeLast != null)
            _toThemeLast.applyTheme(theme);
        CustomStyledText.IGNORE_FORCE = false;
        long after = System.currentTimeMillis();
        if (_ui != null)
            _ui.debugMessage("finally, apply theme to the browser: " + (after-before));
    }

    public void increaseFont() {
        if (_ui != null)
            _ui.debugMessage("increasing font size");
        long before = System.currentTimeMillis();
        _cur.increaseFont();
        long t1 = System.currentTimeMillis();
        notifyAll(_cur);
        long t2 = System.currentTimeMillis();
        saveTheme();
        long t3 = System.currentTimeMillis();
        if (_ui != null)
            _ui.debugMessage("font adjust time: " + (t1-before) + ", notify: " + (t2-t1) + ", save: " + (t3-t2));
    }

    public void decreaseFont() {
        if (_ui != null)
            _ui.debugMessage("decreasing font size");
        long before = System.currentTimeMillis();
        _cur.decreaseFont();
        long t1 = System.currentTimeMillis();
        notifyAll(_cur);
        long t2 = System.currentTimeMillis();
        saveTheme();
        long t3 = System.currentTimeMillis();
        if (_ui != null)
            _ui.debugMessage("font adjust time: " + (t1-before) + ", notify: " + (t2-t1) + ", save: " + (t3-t2));
    }
    
    private void saveTheme() {
        if (_client == null) return;
        Properties prefs = _client.getNymPrefs();
        _cur.store(prefs);
        _client.setNymPrefs(prefs);
    }

    public void loadTheme() {
        Properties prefs = null;
        if (_client == null) {
            prefs = new Properties();
        } else {
            prefs = _client.getNymPrefs();
        }
        if (_cur != null) {
            if (_ui != null)
                _ui.debugMessage("disposing old theme");
            _cur.dispose();
        }
        _cur = Theme.getTheme(prefs);
        notifyAll(_cur);
        _themeLoaded = true;
    }

    /** @since 1.102b-5 */
    public void loadTheme(Theme newTheme) {
        if (_cur != null) {
            if (_ui != null)
                _ui.debugMessage("disposing old theme");
            _cur.dispose();
        }
        _cur = newTheme;
        notifyAll(_cur);
        saveTheme();
        _themeLoaded = true;
    }

    public boolean themeLoaded() { return _themeLoaded; }

    public void resetTheme() {
        Properties prefs = null;
        if (_client == null) {
            prefs = new Properties();
        } else {
            prefs = _client.getNymPrefs();
        }
        for (Iterator iter = prefs.keySet().iterator(); iter.hasNext(); ) {
            String key = (String)iter.next();
            if (key.startsWith("theme."))
                iter.remove();
        }
        if (_client != null)
            _client.setNymPrefs(prefs);
        if (_cur != null) {
            if (_ui != null)
                _ui.debugMessage("disposing old theme");
            _cur.dispose();
        }
        _cur = Theme.getTheme(prefs);
        notifyAll(_cur);
    }
}
