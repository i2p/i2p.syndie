package syndie.gui;

import java.io.File;
import java.util.HashSet;
import java.util.Set;
import net.i2p.I2PAppContext;
import org.eclipse.swt.graphics.DeviceData;
import org.eclipse.swt.widgets.Display;
import syndie.db.DBClient;
import syndie.db.TextEngine;
import syndie.db.TextUI;

/** swt's readAndDispatch needs to be in the main thread */
public class SWTUI {
    private static final boolean trackResources(String args[]) {
        // see browser.dumpResources
        if (true) return true;
        if (args != null)
            for (int i = 0; i < args.length; i++)
                if ("--trackresources".equalsIgnoreCase(args[i]))
                    return true;
        return false;
    }
    
    public static void main(final String args[]) {
        if (args != null) {
            for (int i = 0; i < args.length; i++) {
                if ("--cli".equals(args[i])) {
                    TextUI.main(args);
                    return;
                }
            }
        }
        System.setProperty("jbigi.dontLog", "true");
        System.setProperty("jcpuid.dontLog", "true");
        long start = System.currentTimeMillis();
        boolean trackResources = trackResources(args);
   
        Display d = null;
        if (trackResources) {
            DeviceData data = new DeviceData();
            data.tracking = trackResources;
            d = new Display(data);
        } else {
            d = new Display();
        }
        
        long t1 = System.currentTimeMillis();
        Splash.show(d);
        
        /*
        ColorUtil.init();
        ImageUtil.init();
        SpellUtil.init();
         */
        
        long t2 = System.currentTimeMillis();
        
        String root = TextEngine.getRootPath();
        if (args.length > 0)
            root = args[0];
        
        File rootFile = new File(root);
        if (rootFile.exists() && !rootFile.isDirectory()) {
            System.err.println("Syndie data directory is not a directory: " + rootFile);
            System.exit(-1);
        }
        // this way the logs won't go to ./logs/log-#.txt (i2p's default)
        // (this has to be set before the I2PAppContext instantiates the LogManager)
        System.setProperty("loggerFilenameOverride", root + "/logs/syndie-log-#.txt");
        StartupListener lsnr = new StartupListener();
        long t3 = System.currentTimeMillis();
        DBClient client = new DBClient(I2PAppContext.getGlobalContext(), new File(root));
        long t4 = System.currentTimeMillis();
        final Browser browser = new Browser(client);
        long t5 = System.currentTimeMillis();
        browser.debugMessage("constructing engine");
        final TextEngine engine = new TextEngine(client, browser, lsnr);
        long t6 = System.currentTimeMillis();
        browser.debugMessage("engine constructed");
        browser.setEngine(engine);
        client.setDefaultUI(browser.getUI());
        
        Thread t = new Thread(new Runnable() {
            public void run() {
                browser.debugMessage("starting the engine");
                try {
                    engine.run();
                } catch (Exception e) {
                    browser.errorMessage("error running the engine", e);
                }
                browser.debugMessage("engine stopped");
            }
        }, "text ui");
        t.setPriority(Thread.MIN_PRIORITY);
        t.start();
        long t7 = System.currentTimeMillis();
        
        browser.debugMessage("waiting for login completion...");
        
        // to allow the startup scripts to run, which may include 'login',
        // so we dont have to show a login prompt.  perhaps toss up a splash screen
        boolean ok = lsnr.waitFor("login", 30*1000);
        if (!ok) {
            browser.errorMessage("Timed out trying to start syndie up.  Please review the logs");
            System.exit(0);
            return;
        }
        long t8 = System.currentTimeMillis();
        if (engine.newNymCreated()) {
            WelcomeScreen screen = new WelcomeScreen(d, browser, new WelcomeScreen.CompleteListener() {
                public void complete() {
                    browser.startup();
                }
            });
            screen.open();
        } else {
            browser.debugMessage("db login complete, starting browser...");
            browser.startup();
            browser.debugMessage("browser started");
        }
        long t9 = System.currentTimeMillis();
        
        System.out.println("Startup times: total=" + (t9-start) + ", " + 
                           (t1-start) + "/" + (t2-t1) + "/" + (t3-t2) + "/" + (t4-t3) + "/" +
                           (t5-t4) + "/" + (t6-t5) + "/"+ (t7-t6) + "/" + (t8-t7) + "/" +
                           (t9-t8));
        while (!d.isDisposed()) {
            try { 
                if (!d.readAndDispatch()) d.sleep(); 
            } catch (RuntimeException e) {
                browser.errorMessage("Internal error", e);
            }
        }
    }
    
    private static class StartupListener implements TextEngine.ScriptListener {
        private Set _complete;
        
        public StartupListener() { _complete = new HashSet(); }
        public void scriptComplete(String script) {
            synchronized (_complete) { _complete.add(script); _complete.notifyAll(); }
        }
        public boolean waitFor(String scriptName, long maxPeriod) {
            long endAt = System.currentTimeMillis() + maxPeriod;
            for (;;) {
                long remaining = endAt - System.currentTimeMillis();
                if (remaining <= 0) return false;
                try {
                    synchronized (_complete) {
                        if (_complete.contains(scriptName))
                            return true;
                        else
                            _complete.wait(remaining);
                    }
                } catch (InterruptedException ie) {}
            }
        }
    }
}
