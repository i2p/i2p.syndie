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
    private static final Runnable POISON = new Poison();

    private static class Poison implements Runnable {
        public void run() {}
    }

    /** need this many during startup */
    private static final int THREADS = 4;

    private static final JobRunner _instance;
    static {
        _instance = new JobRunner();
        _instance.start();
    }

    public static JobRunner instance() { return _instance; }
    
    /** caller MUST call start() */
    private JobRunner() {
        _pending = new LinkedBlockingQueue();
    }

    private synchronized void start() {
        _alive = true;
        _pending.clear();
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

    /** cannot be restarted */
    public synchronized void stop() {
        _alive = false;
        _pending.clear();
        for (int i = 0; i < THREADS; i++) {
            enqueue(POISON);
        }
    }

    public void setUI(UI ui) { _ui = ui; }
    
    private class Runner implements Runnable {
        public void run() {
            Runnable cur = null;
            while (_alive) {
                try {
                    cur = _pending.take();
                    if (cur == POISON)
                        break;
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

