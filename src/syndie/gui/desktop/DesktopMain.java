package syndie.gui.desktop;

import java.io.File;
import org.eclipse.swt.graphics.DeviceData;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import syndie.data.Timer;
import syndie.db.JobRunner;
import syndie.db.TextEngine;
import syndie.db.TextUI;
import syndie.db.UI;
import syndie.gui.*;

/** swt's readAndDispatch needs to be in the main thread */
public class DesktopMain {
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
        // we don't need I2P's 4MB of strong PRNG data.  4KB will do
        System.setProperty("prng.bufsize", "1024");
        System.setProperty("prng.buffers", "4");
        
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
   
        DesktopUI ui = new DesktopUI();
        Timer timer = new Timer("startup", ui);
        JobRunner.instance().setUI(ui);
        Desktop desktop = new Desktop(rootFile, ui, d, timer);
        
        while (!d.isDisposed()) {
            try { 
                if (!d.readAndDispatch()) d.sleep(); 
            } catch (RuntimeException e) {
                e.printStackTrace();
                ui.errorMessage("Internal error: " + e.getMessage(), e);
            }
        }
    }

    private static final boolean trackResources(String args[]) {
        // see browser.dumpResources
        //if (true) return true;
        if (args != null)
            for (int i = 0; i < args.length; i++)
                if ("--trackresources".equalsIgnoreCase(args[i]))
                    return true;
        return false;
    }
}
