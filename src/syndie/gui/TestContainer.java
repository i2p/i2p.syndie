package syndie.gui;

import org.eclipse.swt.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.swt.custom.*;
import org.eclipse.swt.layout.*;
import syndie.db.*;

public class TestContainer {
    public static void main(String args[]) {
        DBClient client = startTextEngine(args);
        Display display = Display.getDefault();
        Shell shell = new Shell(display, SWT.SHELL_TRIM);
        Composite composite = new Composite(shell, SWT.NONE);
        composite.setLayout(new GridLayout(1, true));
        shell.setLayout(new FillLayout());
        CTabFolder parent = new CTabFolder(composite, SWT.BORDER | SWT.CLOSE | SWT.V_SCROLL | SWT.H_SCROLL);

        MessageTab tab = new MessageTab(client, parent, "");

        parent.setSelection(tab.getTab());
        tab.setFocus();
        parent.setLayoutData(new GridData(700, 500));
        shell.pack();
        shell.open();
        while (!shell.isDisposed()) {
            try {
                if (!display.readAndDispatch()) display.sleep();
            } catch (RuntimeException e) {
                e.printStackTrace();
            }
        }
        System.out.println("Done");
        System.exit(0);
    }

    /** ~= TextUI.main, except run in its own thread */
    private static DBClient startTextEngine(String args[]) {
        final TextUI ui = new TextUI(args);
        new Thread(new Runnable() { public void run() { ui.run(); } }).start();
	return ui.getEngine().getClient();
    }
}
