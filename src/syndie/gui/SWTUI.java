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
        System.setProperty("jbigi.dontLog", "true");
        System.setProperty("jcpuid.dontLog", "true");
        boolean trackResources = trackResources(args);
   
        Display d = null;
        if (trackResources) {
            DeviceData data = new DeviceData();
            data.tracking = trackResources;
            d = new Display(data);
        } else {
            d = new Display();
        }
        
        ColorUtil.init();
        ImageUtil.init();
        SpellUtil.init();
        
        String root = TextEngine.getRootPath();
        if (args.length > 0)
            root = args[0];
        StartupListener lsnr = new StartupListener();
        DBClient client = new DBClient(I2PAppContext.getGlobalContext(), new File(root));
        final Browser browser = new Browser(client);
        browser.debugMessage("constructing engine");
        final TextEngine engine = new TextEngine(client, browser, lsnr);
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
        
        // to allow the startup scripts to run, which may include 'login',
        // so we dont have to show a login prompt.  perhaps toss up a splash screen
        lsnr.waitFor("login");
        browser.debugMessage("db login complete, starting browser...");
        browser.startup();
        browser.debugMessage("browser started");
        
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
        public void waitFor(String scriptName) {
            for (;;) {
                try {
                    synchronized (_complete) {
                        if (_complete.contains(scriptName))
                            return;
                        else
                            _complete.wait();
                    }
                } catch (InterruptedException ie) {}
            }
        }
    }
}
