package syndie.gui;

import java.util.ArrayList;

/**
 *
 */
public class PageRendererThread implements Runnable {
    private static final ArrayList _enqueued = new ArrayList();
    private static final PageRendererThread _instance = new PageRendererThread();
    
    private PageRendererThread() {
        Thread t = new Thread(this, "Page renderer");
        t.setDaemon(true);
        t.start();
    }
    
    public static void enqueue(PageRenderer renderer) {
        synchronized (_enqueued) {
            if (!_enqueued.contains(renderer))
                _enqueued.add(renderer);
            _enqueued.notifyAll();
        }
    }
    public void run() {
        PageRenderer cur = null;
        while (true) {
            synchronized (_enqueued) {
                try {
                    if (_enqueued.isEmpty())
                        _enqueued.wait();
                    else
                        cur = (PageRenderer)_enqueued.remove(0);
                } catch (InterruptedException ie) {
                    if (!_enqueued.isEmpty())
                        cur = (PageRenderer)_enqueued.remove(0);
                }
            }
            if (cur != null) {
                try {
                    long before = System.currentTimeMillis();
                    cur.threadedRender();
                    long renderTime = System.currentTimeMillis() - before;
                    System.out.println("async render time: " + renderTime);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            cur = null;
        }
    }
}
