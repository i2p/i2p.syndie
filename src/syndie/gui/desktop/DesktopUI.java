package syndie.gui.desktop;

import java.util.ArrayList;
import java.util.List;
import syndie.db.Opts;
import syndie.db.UI;
import syndie.gui.*;

class DesktopUI implements Runnable, UI {
    private List _uiListeners;
    private List _commands;
    private List _errMsgs;
    private List _errCauses;
    private List _statusMsgs;
    private List _debugMsgs;
    private List _debugCauses;
    private List _completeStatus;
    private List _completeLocations;
    private List _typeOrder;

    private static final Exception NO_CAUSE = new Exception();
    private static final List NO_LOCATIONS = new ArrayList(0);

    private static final Integer TYPE_ORDER_ERROR = Integer.valueOf(0);
    private static final Integer TYPE_ORDER_STATUS = Integer.valueOf(1);
    private static final Integer TYPE_ORDER_DEBUG = Integer.valueOf(2);
    private static final Integer TYPE_ORDER_COMMAND_COMPLETE = Integer.valueOf(3);


    public DesktopUI() {
        _uiListeners = new ArrayList();
        _commands = new ArrayList();
        _errMsgs = new ArrayList(4);
        _errCauses = new ArrayList(4);
        _statusMsgs = new ArrayList(4);
        _debugMsgs = new ArrayList(4);
        _debugCauses = new ArrayList(4);
        _completeStatus = new ArrayList(4);
        _completeLocations = new ArrayList(4);
        _typeOrder = new ArrayList();

        Thread t = new Thread(DesktopUI.this, "Desktop UI");
        t.setDaemon(true);
        t.start();
    }

    public void addUI(UI lsnr) { synchronized (_uiListeners) { _uiListeners.add(lsnr); } }
    public void removeUI(UI lsnr) { synchronized (_uiListeners) { _uiListeners.remove(lsnr); } }

    public void run() {
        while (true) {
            String errMsg = null;
            Exception errCause = null;
            String statusMsg = null;
            String debugMsg = null;
            Exception debugCause = null;
            Integer completeStatus = null;
            List completeLocation = null;

            try {
                synchronized (DesktopUI.this) {
                    if (_typeOrder.size() <= 0) {
                        DesktopUI.this.wait();
                    } else {
                        Object type = _typeOrder.remove(0);
                        if (type.equals(TYPE_ORDER_ERROR)) {
                            errMsg = (String)_errMsgs.remove(0);
                            errCause = (Exception)_errCauses.remove(0);
                            if (errCause == NO_CAUSE)
                                errCause = null;
                        } else if (type.equals(TYPE_ORDER_STATUS)) {
                            statusMsg = (String)_statusMsgs.remove(0);
                        } else if (type.equals(TYPE_ORDER_DEBUG)) {
                            debugMsg = (String)_debugMsgs.remove(0);
                            debugCause = (Exception)_debugCauses.remove(0);
                            if (debugCause == NO_CAUSE)
                                debugCause = null;
                        } else if (type.equals(TYPE_ORDER_COMMAND_COMPLETE)) {
                            completeStatus = (Integer)_completeStatus.remove(0);
                            completeLocation = (List)_completeLocations.remove(0);
                            if (completeLocation == NO_LOCATIONS)
                                completeLocation = null;
                        }
                    }
                }
                if (errMsg != null) {
                    for (int i = 0; i < _uiListeners.size(); i++)
                        ((UI)_uiListeners.get(i)).errorMessage(errMsg, errCause);
                } else if (statusMsg != null) {
                    for (int i = 0; i < _uiListeners.size(); i++)
                        ((UI)_uiListeners.get(i)).statusMessage(statusMsg);
                } else if (debugMsg != null) {
                    for (int i = 0; i < _uiListeners.size(); i++)
                        ((UI)_uiListeners.get(i)).debugMessage(debugMsg, debugCause);
                } else if (completeStatus != null) {
                    for (int i = 0; i < _uiListeners.size(); i++)
                        ((UI)_uiListeners.get(i)).commandComplete(completeStatus.intValue(), completeLocation);
                }
            } catch (InterruptedException ie) {}
        }
    }

    public void errorMessage(String msg, Exception cause) {
        synchronized (DesktopUI.this) {
            _typeOrder.add(TYPE_ORDER_ERROR);
            _errMsgs.add(msg);
            if (cause == null)
                _errCauses.add(NO_CAUSE);
            else
                _errCauses.add(cause);
            DesktopUI.this.notifyAll();
        }
    }

    public void statusMessage(String msg) {
        synchronized (DesktopUI.this) {
            _typeOrder.add(TYPE_ORDER_STATUS);
            _statusMsgs.add(msg);
            DesktopUI.this.notifyAll();
        }
    }

    public void debugMessage(String msg, Exception cause) {
        synchronized (DesktopUI.this) {
            _typeOrder.add(TYPE_ORDER_DEBUG);
            _debugMsgs.add(msg);
            if (cause == null)
                _debugCauses.add(NO_CAUSE);
            else
                _debugCauses.add(cause);
            DesktopUI.this.notifyAll();
        }
    }

    public void commandComplete(int status, List location) {
        synchronized (DesktopUI.this) {
            _typeOrder.add(TYPE_ORDER_COMMAND_COMPLETE);
            _completeStatus.add(Integer.valueOf(status));
            if (location == null)
                _completeLocations.add(NO_LOCATIONS);
            else
                _completeLocations.add(location);
            DesktopUI.this.notifyAll();
        }
    }

    // now for the rest of the UI methods

    public void insertCommand(String cmd) { 
        synchronized (_commands) { _commands.add(cmd); _commands.notifyAll(); }
    }
    public Opts readCommand() {
        while (true) {
            synchronized (_commands) {
                try {
                    if (_commands.size() <= 0)
                        _commands.wait();
                } catch (InterruptedException ie) {}
                if (_commands.size() > 0)
                    return new Opts((String)_commands.remove(0));
            }
        }
    }
    public Opts readCommand(boolean displayPrompt) { return readCommand(); }

    public void errorMessage(String msg) { errorMessage(msg, null); }
    public void debugMessage(String msg) { debugMessage(msg, null); }
    public boolean toggleDebug() { return false; }
    public boolean togglePaginate() { return false; }
    public String readStdIn() { return null; }
}

