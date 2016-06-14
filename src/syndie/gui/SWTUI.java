package syndie.gui;

import java.io.File;
import java.util.HashSet;
import java.util.Set;

import net.i2p.I2PAppContext;
import net.i2p.util.SecureFile;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.DeviceData;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import syndie.util.Timer;
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
    
    /**
     * If true, remove the 30 second timeout when connecting to the database,
     * as slow computers w/ lots in their redo logs could take that long.  The
     * reason the timeout is here in the first place is to deal with those 
     * running Syndie off pre-1.0 installs that didn't automatically log in
     */
    private static final boolean ALLOW_SLOW_STARTUP = true;
    
    public static void main(final String args[]) {
        long start = System.currentTimeMillis();
        for (int i = 0; i < args.length; i++) {
            if ("--cli".equals(args[i])) {
                TextUI.main(args);
                return;
            }
        }
        System.setProperty("jbigi.dontLog", "true");
        System.setProperty("jcpuid.dontLog", "true");
        // we don't need I2P's 4MB of strong PRNG data.  4KB will do
        System.setProperty("prng.bufsize", "1024");
        System.setProperty("prng.buffers", "4");
        
        boolean trackResources = trackResources(args);
   
        Display d = null;
        Display.setAppName("Syndie");
        if (trackResources) {
            DeviceData data = new DeviceData();
            data.tracking = trackResources;
            d = new Display(data);
        } else {
            d = new Display();
        }
        
        String root = TextEngine.getRootPath();
        if (args.length > 0 && args[0] != null && !args[0].startsWith("-")) 
            root = args[0];
        
        File rootFile = new SecureFile(root);
        // create it here, as Splash will do mkdirs() and it won't set the
        // permissions right
        rootFile.mkdirs();
        if (!rootFile.exists()) {
            System.err.println("Cannot create Syndie data directory: " + rootFile);
            System.exit(-1);
        }
        if (!rootFile.isDirectory()) {
            System.err.println("Syndie data directory is not a directory: " + rootFile);
            System.exit(-1);
        }
        
        // Debug, wait a few seconds
        String wait = System.getProperty("SWT_WAIT");
        if (wait != null) {
            try {
                int secs = Integer.parseInt(wait);
                System.out.println("Waiting " + secs + " for debugger...");
                Thread.sleep(secs * 1000);
                System.out.println("Proceeding...");
            } catch (NumberFormatException nfe) {
                System.out.println("Bad time " + wait);
            } catch (InterruptedException ie) {
            }
        }

        // this way the logs won't go to ./logs/log-#.txt (i2p's default)
        // (this has to be set before the I2PAppContext instantiates the LogManager)
        System.setProperty("loggerFilenameOverride", root + "/logs/syndie-log-#.txt");
        StartupListener lsnr = new StartupListener();
        DBClient client = new DBClient(I2PAppContext.getGlobalContext(), rootFile);
        Splash.show(d, client.getTempDir());
        final Browser browser = new Browser(client);
        long now = System.currentTimeMillis();
        final Timer timer = new Timer("swtUI startup", browser.getUI());
        timer.addEvent("time between main(..) and timer: " + (now-start));
        final TextEngine engine = new TextEngine(client, browser, lsnr);
        timer.addEvent("text engine instantiated");
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
        timer.addEvent("text engine started");
        
        browser.debugMessage("waiting for login completion...");
        
        // to allow the startup scripts to run, which may include 'login',
        // so we dont have to show a login prompt.  perhaps toss up a splash screen
        boolean ok = lsnr.waitFor("login", ALLOW_SLOW_STARTUP ? -1 : 30*1000);
        if (lsnr.getAlreadyRunning()) {
            // show a special warning/error screen
            final Shell s = new Shell(d, SWT.DIALOG_TRIM);
            s.setText(browser.getTranslationRegistry().getText("Already running"));
            s.setLayout(new GridLayout(1, true));
            Label l = new Label(s, SWT.SINGLE | SWT.WRAP);
            l.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            l.setText(browser.getTranslationRegistry().getText("Syndie is already running - please use the existing Syndie window"));
            Button b = new Button(s, SWT.PUSH);
            b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            b.setText(browser.getTranslationRegistry().getText("Exit"));
            b.addSelectionListener(new FireSelectionListener() { 
                public void fire() {
                    s.dispose();
                    System.exit(-1);
                }
            });
            s.addShellListener(new ShellListener() {
                public void shellActivated(ShellEvent shellEvent) {}
                public void shellClosed(ShellEvent shellEvent) {
                    s.dispose();
                    System.exit(-1);
                }
                public void shellDeactivated(ShellEvent shellEvent) {}
                public void shellDeiconified(ShellEvent shellEvent) {}
                public void shellIconified(ShellEvent shellEvent) {}
            });
            Splash.dispose();
            s.pack();
            Rectangle sSize = s.getBounds();
            Rectangle screenSize = Splash.getScreenSize(s);
            int x = screenSize.width/2-sSize.width/2;
            int y = screenSize.height/2-sSize.height/2;
            s.setBounds(x, y, sSize.width, sSize.height);
            s.open();
        } else if (lsnr.getLoginFailedCause() != null) {
            // show a special warning/error screen
            final Shell s = new Shell(d, SWT.DIALOG_TRIM);
            s.setText(browser.getTranslationRegistry().getText("Internal error"));
            s.setLayout(new GridLayout(1, true));
            Label l = new Label(s, SWT.SINGLE | SWT.WRAP);
            GridData gd = new GridData(GridData.FILL, GridData.FILL, true, false);
            gd.widthHint = 500;
            l.setLayoutData(gd);
            l.setText(browser.getTranslationRegistry().getText("Syndie ran into an internal error trying to start up - please see the logs") +
                                                               ":\n\n" + lsnr.getLoginFailedCause().getMessage() + '\n');
            Button b = new Button(s, SWT.PUSH);
            b.setLayoutData(new GridData(GridData.CENTER, GridData.FILL, false, false));
            b.setText(browser.getTranslationRegistry().getText("Exit"));
            b.addSelectionListener(new FireSelectionListener() { 
                public void fire() {
                    s.dispose();
                    System.exit(-1);
                }
            });
            s.addShellListener(new ShellListener() {
                public void shellActivated(ShellEvent shellEvent) {}
                public void shellClosed(ShellEvent shellEvent) {
                    s.dispose();
                    System.exit(-1);
                }
                public void shellDeactivated(ShellEvent shellEvent) {}
                public void shellDeiconified(ShellEvent shellEvent) {}
                public void shellIconified(ShellEvent shellEvent) {}
            });
            Splash.dispose();
            s.pack();
            Rectangle sSize = s.getBounds();
            Rectangle screenSize = Splash.getScreenSize(s);
            int x = screenSize.width/2-sSize.width/2;
            int y = screenSize.height/2-sSize.height/2;
            s.setBounds(x, y, sSize.width, sSize.height);
            s.open();
        } else {
            if (!ok) {
                browser.errorMessage("Timed out trying to start syndie up.  Please review the logs");
                System.exit(0);
                return;
            }
            timer.addEvent("login complete");
            if (engine.newNymCreated()) {
                ThemeRegistry themes = browser.getThemeRegistry();
                if (!themes.themeLoaded())
                    themes.loadTheme();
                
                WelcomeScreen screen = new WelcomeScreen(d, browser, new WelcomeScreen.CompleteListener() {
                    public void complete(boolean success) {
                        if (success) {
                            browser.startup(timer);
                        } else {
                            // doesn't work
                            //browser.exit();
                            // TODO something nicer
                            System.exit(0);
                        }
                    }
                }, timer);
                screen.open();
            } else {
                browser.debugMessage("db login complete, starting browser...");
                browser.startup(timer);
                Splash.dispose();
                browser.debugMessage("browser started");
            }
            timer.addEvent("swtUI startup complete");
            timer.complete();
        }
        
        while (!d.isDisposed()) {
            try { 
                if (!d.readAndDispatch()) d.sleep(); 
            } catch (RuntimeException e) {
                browser.errorMessage("Internal error", e);
            }
        }
    }
    

    private static final Exception BAD_LOGIN = new Exception();
    private static final Exception BAD_PASS = new Exception();
    
    private static class StartupListener implements TextEngine.ScriptListener {
        private Set _complete;
        private boolean _alreadyRunning;
        private Exception _loginFailedCause;
        
        public StartupListener() { 
            _complete = new HashSet(); 
            _alreadyRunning = false;
        }
        public void scriptComplete(String script) {
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
        public boolean getAlreadyRunning() { return _alreadyRunning; }
        public Exception getLoginFailedCause() { return _loginFailedCause; }
        public boolean waitFor(String scriptName, long maxPeriod) {
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
}
