package syndie.db;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

/**
 *  Background threads
 */
public final class JobRunner {
    private final BlockingQueue<Runnable> _pending;
    private volatile boolean _alive;
    private volatile UI _ui;

    /** need this many during startup */
    private static final int THREADS = 4;

    private static final JobRunner _instance = new JobRunner();

    public static JobRunner instance() { return _instance; }
    
    private JobRunner() {
        _alive = true;
        _pending = new LinkedBlockingQueue();
        for (int i = 0; i < THREADS; i++) {
            Thread t = new Thread(new Runner(), "Job runner " + (i+1) + '/' + THREADS);
            t.setDaemon(true);
            t.setPriority(Thread.MIN_PRIORITY);
            t.start();
        }
    }
    
    public void enqueue(Runnable r) {
        _pending.offer(r);
    }

    public void stop() { _alive = false; synchronized (_pending) { _pending.notifyAll(); } }

    public void setUI(UI ui) { _ui = ui; }
    
    private class Runner implements Runnable {
        public void run() {
            Runnable cur = null;
            while (_alive) {
                try {
                    cur = _pending.take();
                } catch (InterruptedException ie) {}
                if (cur != null) {
                    long start = System.currentTimeMillis();
                    if (_ui != null)
                        _ui.debugMessage("Job queue: Start running " + cur);
                    try { cur.run(); } catch (Exception e) { 
                        if (_ui != null)
                            _ui.errorMessage("internal error with the job: " + cur + ": " + e, e);
                    }
                    if (_ui != null)
                        _ui.debugMessage("Job queue: Finish running " + cur + " after " + (System.currentTimeMillis() - start));
                }
                cur = null;
            }
        }
    }
}

