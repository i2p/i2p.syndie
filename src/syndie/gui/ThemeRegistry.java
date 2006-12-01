package syndie.gui;

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
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
    
    private void notifyAll(Theme theme) {
        for (Iterator iter = _listeners.iterator(); iter.hasNext(); ) {
            Themeable cur = (Themeable)iter.next();
            String err = theme.validate();
            if (err == null) {
                _browser.getUI().debugMessage("apply theme to " + cur.getClass().getName() + "/" + System.identityHashCode(cur));
                cur.applyTheme(theme);
            } else {
                _browser.getUI().errorMessage("cannot apply theme: " + err);
            }
        }
    }
    
    public void increaseFont() {
        _browser.getUI().debugMessage("increasing font size");
        _cur.increaseFont();
        notifyAll(_cur);
    }
    public void decreaseFont() {
        _browser.getUI().debugMessage("decreasing font size");
        _cur.decreaseFont();
        notifyAll(_cur);
    }
}
