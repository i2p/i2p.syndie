package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 *
 */
public class Splash {
    private static Shell _shell;
    private static Image _img;
    public static void show(Display display) {
        _shell = new Shell(display, SWT.NO_TRIM | SWT.APPLICATION_MODAL | SWT.ON_TOP | SWT.NO_FOCUS | SWT.NO_BACKGROUND);
        _img = ImageUtil.createImageFromResource("splash.png");
        _shell.setLayout(new FillLayout());
        Label l = new Label(_shell,  SWT.NO_BACKGROUND);
        l.setImage(_img);
        _shell.pack();
        Rectangle imgSize = _img.getBounds();
        Rectangle screenSize = display.getBounds();
        int x = screenSize.width/2-imgSize.width/2;
        int y = screenSize.height/2-imgSize.height/2;
        _shell.setBounds(x, y, imgSize.width, imgSize.height);
        _shell.open();
    }
    public static void dispose() {
        if (!_shell.isDisposed())
            _shell.dispose();
        ImageUtil.dispose(_img);
    }
}
