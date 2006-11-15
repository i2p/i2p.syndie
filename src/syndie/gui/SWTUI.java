package syndie.gui;

import org.eclipse.swt.widgets.Display;
import syndie.db.TextUI;

/** swt's readAndDispatch needs to be in the main thread */
public class SWTUI {
    public static void main(final String args[]) {
        new Thread(new Runnable() {
            public void run() {
                TextUI.main(args);
            }
        }, "text ui").start();
        Display d = Display.getDefault();
        ColorUtil.init();
        ImageUtil.init();
        while (!d.isDisposed()) {
            try { 
                if (!d.readAndDispatch()) d.sleep(); 
            } catch (RuntimeException e) {
                e.printStackTrace();
            }
        }
    }
}
