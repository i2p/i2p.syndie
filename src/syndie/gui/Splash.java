package syndie.gui;

import java.io.IOException;
import java.io.InputStream;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;

/**
 *
 */
public class Splash {
    private static Shell _shell;
    private static Image _img;
    private static final boolean DISABLED = false;
    public static void show(Display display) {
        if (DISABLED) return;
        _shell = new Shell(display, SWT.NO_TRIM | SWT.APPLICATION_MODAL | SWT.ON_TOP | SWT.NO_FOCUS | SWT.NO_BACKGROUND);
        _img = getImage();
        _shell.setLayout(new FillLayout());
        Label l = new Label(_shell,  SWT.NO_BACKGROUND);
        l.setImage(_img);
        _shell.pack();
        Rectangle imgSize = _img.getBounds();
        Rectangle screenSize = getScreenSize(_shell);
        int x = screenSize.width/2-imgSize.width/2;
        int y = screenSize.height/2-imgSize.height/2;
        _shell.setBounds(x, y, imgSize.width, imgSize.height);
        _shell.open();
    }
    public static void dispose() {
        if (DISABLED) return;
        if (!_shell.isDisposed())
            _shell.dispose();
        ImageUtil.dispose(_img);
    }
    
    static Rectangle getScreenSize(Shell shell) {
        Monitor monitors[] = shell.getDisplay().getMonitors();
        if ( (monitors == null) || (monitors.length <= 1) ) {
            return shell.getDisplay().getBounds();
        } else {
            // just throw it up on the first monitor (we may not be able to
            // query shell to see what its coordinates will be (and hence
            // determine what monitor its on) since its not yet open)
            return monitors[0].getBounds();
        }
    }
    
    private static Image getImage() { 
        int splashCount = getSplashCount();
        // many clocks have only 10ms granularity
        long which = (System.currentTimeMillis()/10) % splashCount;
        return createImageFromResource("splash" + which + ".png");
    }
    
    private static int getSplashCount() {
        int splashes = 0;
        while (true) {
            InputStream in = Splash.class.getResourceAsStream("splash" + splashes + ".png");
            if (in != null) {
                splashes++;
                try { in.close(); } catch (IOException ioe) {}
            } else {
                return splashes;
            }
        }
    }
    
    /** copied from ImageUtil to avoid invoking ImageUtil's statics */
    private static Image createImageFromResource(String resource) {
        InputStream in = ImageUtil.class.getResourceAsStream(resource);
        if (in != null) {
            try {
                return new Image(Display.getDefault(), in);
            } catch (IllegalArgumentException iae) {
                return null;
            }
        } else {
            return null;
        }
    }

}
