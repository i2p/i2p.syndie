package syndie.db;

import java.util.ArrayList;

/**
 *
 */
public final class JobRunner {
    private ArrayList _pending;
    private boolean _alive;
    private UI _ui;
    private static JobRunner _instance = new JobRunner();
    public static JobRunner instance() { return _instance; }
    
    private JobRunner() {
        _alive = true;
        _pending = new ArrayList();
        for (int i = 0; i < 3; i++) {
            Thread t = new Thread(new Runner(), "Job runner " + i);
            t.setDaemon(true);
            t.setPriority(Thread.MIN_PRIORITY);
            t.start();
        }
    }
    
    public void enqueue(Runnable r) {
        synchronized (_pending) { _pending.add(r); _pending.notifyAll(); }
    }
    public void stop() { _alive = false; synchronized (_pending) { _pending.notifyAll(); } }
    public void setUI(UI ui) { _ui = ui; }
    
    private class Runner implements Runnable {
        public void run() {
            Runnable cur = null;
            while (_alive) {
                try {
                    synchronized (_pending) {
                        if (_pending.size() <= 0)
                            _pending.wait();
                        else
                            cur = (Runnable)_pending.remove(0);
                    }
                } catch (InterruptedException ie) {}
                if (cur != null) {
                    //_ui.debugMessage("Job queue: Start running " + cur);
                    try { cur.run(); } catch (Exception e) { 
                        _ui.errorMessage("internal error with the job: " + cur + ": " + e, e); }
                    //_ui.debugMessage("Job queue: Finish running " + cur);
                }
                cur = null;
            }
        }
    }
}
