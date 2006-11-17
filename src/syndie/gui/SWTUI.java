package syndie.gui;

import java.io.File;
import net.i2p.I2PAppContext;
import org.eclipse.swt.widgets.Display;
import syndie.db.DBClient;
import syndie.db.TextEngine;
import syndie.db.TextUI;

/** swt's readAndDispatch needs to be in the main thread */
public class SWTUI {
    public static void main(final String args[]) {
        System.setProperty("jbigi.dontLog", "true");
        System.setProperty("jcpuid.dontLog", "true");
        
        Display d = Display.getDefault();
        ColorUtil.init();
        ImageUtil.init();
        
        String root = TextEngine.getRootPath();
        if (args.length > 0)
            root = args[0];
        DBClient client = new DBClient(I2PAppContext.getGlobalContext(), new File(root));
        final Browser browser = new Browser(client);
        final TextEngine engine = new TextEngine(client, browser);
        browser.setEngine(engine);
        
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
        try { Thread.sleep(2000); } catch (InterruptedException ie) {}
        browser.debugMessage("starting the browser");
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
}
