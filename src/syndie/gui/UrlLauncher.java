/*
 * I2P - An anonymous, secure, and fully-distributed communication network.
 * 
 * UrlLauncher.java
 * 2004 The I2P Project
 * http://www.i2p.net
 * This code is public domain.
 */

package syndie.gui;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.net.InetSocketAddress;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.Socket;
import java.net.SocketAddress;
import java.net.URL;
import java.util.Locale;

import net.i2p.I2PAppContext;
import net.i2p.util.I2PAppThread;
import net.i2p.util.ShellCommand;
import net.i2p.util.SystemVersion;

/**
 * A quick and simple multi-platform URL launcher. It attempts to launch the
 * default browser for the host platform first, then popular third-party
 * browsers if that was not successful.
 * <p>
 * Handles Galeon, Internet Explorer, Konqueror, Links, Lynx, Mozilla, Mozilla
 * Firefox, Netscape, Opera, and Safari.    
 * 
 * @author hypercubus
 * @since 1.102b-12 adapted from I2P app/systray
 */
class UrlLauncher implements Runnable {

    private final ShellCommand _shellCommand = new ShellCommand();
    private final String _url;

    /**
     *  Browsers to try IN-ORDER
     */
    private static final String[] BROWSERS = {
            // This debian script tries everything in $BROWSER, then gnome-www-browser and x-www-browser
            // if X is running and www-browser otherwise. Those point to the user's preferred
            // browser using the update-alternatives system.
            "sensible-browser",
            // another one that opens a preferred browser
            "xdg-open",
            // Try x-www-browser directly
            "x-www-browser",
            // general graphical browsers
            "defaultbrowser",  // puppy linux
            "opera -newpage",
            "firefox",
            "mozilla",
            "netscape",
            "konqueror",
            "galeon",
            // Text Mode Browsers only below here
            "www-browser",
            "links",
            "lynx"
    };
            
    /**
     * @param  url The URL to open.
     */ 
    public UrlLauncher(String url) throws MalformedURLException {
        // just to check validity
        new URL(url);
        _url = url;
    }

    public void run() {
        try {
            openUrl();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static void backgroundOpenUrl(String url) throws MalformedURLException {
        UrlLauncher u = new UrlLauncher(url);
        (new I2PAppThread(u, "launcher", true)).start();
    }

    /**
     * Discovers the operating system the installer is running under and tries
     * to launch the given URL using the default browser for that platform; if
     * unsuccessful, an attempt is made to launch the URL using the most common
     * browsers.
     *
     * @return     <code>true</code> if the operation was successful, otherwise
     *             <code>false</code>.
     *
     * @throws Exception
     */
    public boolean openUrl() throws Exception {
        String url = _url;
        String osName = System.getProperty("os.name");

        if (osName.toLowerCase(Locale.US).indexOf("mac") > -1) {
            if (osName.toLowerCase(Locale.US).startsWith("mac os x")) {

                if (_shellCommand.executeSilentAndWaitTimed("open " + url, 5))
                    return true;

            } else {
                return false;
            }

            if (_shellCommand.executeSilentAndWaitTimed("iexplore " + url, 5))
                return true;

        } else if (SystemVersion.isWindows()) {

            String         browserString  = "\"C:\\Program Files\\Internet Explorer\\iexplore.exe\" -nohome";
            BufferedReader bufferedReader = null;

            File foo = new File(I2PAppContext.getGlobalContext().getTempDir(), "browser.reg");
            _shellCommand.executeSilentAndWait("regedit /E \"" + foo.getAbsolutePath() + "\" \"HKEY_CLASSES_ROOT\\http\\shell\\open\\command\"");

            try {
                bufferedReader = new BufferedReader(new InputStreamReader(new FileInputStream(foo), "UTF-16"));
                for (String line; (line = bufferedReader.readLine()) != null; ) {
                    if (line.startsWith("@=")) {
                        // we should really use the whole line and replace %1 with the url
                        browserString = line.substring(3, line.toLowerCase(Locale.US).indexOf(".exe") + 4);
                        if (browserString.startsWith("\\\""))
                            browserString = browserString.substring(2);
                        browserString = "\"" + browserString + "\"";
                    }
                }
                try {
                    bufferedReader.close();
                } catch (IOException e) {
                    // No worries.
                }
                foo.delete();
            } catch (Exception e) {
                // Defaults to IE.
            } finally {
                if (bufferedReader != null)
                    try { bufferedReader.close(); } catch (IOException ioe) {}
            }
            if (_shellCommand.executeSilentAndWaitTimed(browserString + ' ' + url, 5))
                return true;

        } else {

            // fall through
        }

        for (int i = 0; i < BROWSERS.length; i++) {
            if (_shellCommand.executeSilentAndWaitTimed(BROWSERS[i] + ' ' + url, 5))
                return true;
        }
        return false;
    }

    /**
     * Opens the given URL with the given browser.
     *
     * @param  browser The browser to use.
     * @return         <code>true</code> if the operation was successful,
     *                 otherwise <code>false</code>.
     * 
     * @throws Exception
     */
    public boolean openUrl(String browser) throws Exception {
        return _shellCommand.executeSilentAndWaitTimed(browser + " " + _url, 5);
    }
}
