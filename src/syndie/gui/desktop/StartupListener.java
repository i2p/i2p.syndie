package syndie.gui.desktop;

import java.util.HashSet;
import java.util.Set;

import syndie.data.Timer;
import syndie.db.TextEngine;

/**
 *  Moved out of StartupPanel.java
 */
class StartupListener implements TextEngine.ScriptListener {
    private Set _complete;
    private boolean _alreadyRunning;
    private Exception _loginFailedCause;
    private Timer _timer;
    private static final Exception BAD_LOGIN = new Exception();
    private static final Exception BAD_PASS = new Exception();

    public StartupListener(Timer timer) { 
        _complete = new HashSet(); 
        _alreadyRunning = false;
        _timer = timer;
    }
    public void scriptComplete(String script) {
        _timer.addEvent("script complete: " + script);
        synchronized (_complete) { _complete.add(script); _complete.notifyAll(); }
    }
    public void alreadyRunning() { 
        _alreadyRunning = true; 
        synchronized (_complete) { _complete.notifyAll(); } 
    }
    public void loginFailed(Exception cause) {
        _loginFailedCause = cause;
        synchronized (_complete) { _complete.notifyAll(); }
    }
    public void loginFailedBadPassphrase() {
        _loginFailedCause = BAD_PASS;
        synchronized (_complete) { _complete.notifyAll(); }
    }
    public void loginFailedBadLogin() {
        _loginFailedCause = BAD_LOGIN;
        synchronized (_complete) { _complete.notifyAll(); }
    }
    public void clearLoginState() {
        synchronized (_complete) {
            _complete.remove("login");
            _complete.notifyAll();
        }
    }
    public boolean getAlreadyRunning() { return _alreadyRunning; }
    public Exception getLoginFailedCause() { return _loginFailedCause; }
    public boolean getLoginFailedBadPassphrase() { return _loginFailedCause == BAD_PASS; }
    public boolean getLoginFailedBadLogin() { return _loginFailedCause == BAD_LOGIN; }
    public boolean waitFor(String scriptName, long maxPeriod) {
        _alreadyRunning = false;
        _loginFailedCause = null;
        long endAt = System.currentTimeMillis() + maxPeriod;
        for (;;) {
            if (_alreadyRunning) return false;
            if (_loginFailedCause != null) return false;
            long remaining = -1;
            if (maxPeriod > 0) {
                remaining = endAt - System.currentTimeMillis();
                if (remaining <= 0) return false;
            }
            try {
                synchronized (_complete) {
                    if (_complete.contains(scriptName))
                        return true;
                    else if (maxPeriod > 0)
                        _complete.wait(remaining);
                    else
                        _complete.wait();
                }
            } catch (InterruptedException ie) {}
        }
    }
}
