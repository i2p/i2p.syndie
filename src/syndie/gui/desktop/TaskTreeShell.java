package syndie.gui.desktop;

import java.util.List;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;

class TaskTreeShell implements TaskTree.TaskTreeListener {
    private Desktop _desktop;
    private Shell _shell;
    private TaskTree _tree;
    
    public TaskTreeShell(Desktop desktop) {
        _desktop = desktop;
        _shell = new Shell(desktop.getCenter().getShell(), SWT.NO_TRIM | SWT.BORDER);
        _shell.setLayout(new FillLayout());
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) {
                evt.doit = false;
                hide();
            }
            public void shellDeactivated(ShellEvent evt) {
                evt.doit = false;
                hide();
            }
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        _tree = new TaskTree(desktop, _shell);
        _tree.addListener(this);
    }
    
    private static final int SHELL_WIDTH = 600;
    private static final int SHELL_HEIGHT = 400;
    
    public void show() { 
        _tree.show(); 
        //_shell.setSize(_tree.getRoot().computeSize(400, 400));
        Display d = Display.getDefault();
        Rectangle bounds = d.getBounds();
        Monitor m[] = d.getMonitors();
        if (m != null)
            bounds = m[0].getBounds();
        //Point pt = Display.getDefault().getCursorLocation();
        //_shell.setBounds(pt.x, pt.y, 400, 400);
        _shell.setBounds(bounds.width/2-SHELL_WIDTH/2, bounds.height/2-SHELL_HEIGHT/2, SHELL_WIDTH, SHELL_HEIGHT);
        _shell.open();
    }
    public void dispose() { _tree.dispose(); }
    
    private void hide() {
        _shell.setVisible(false);
    }

    public void cancelSelected() {
        hide();
    }

    public void exitSelected() { 
        hide();
        _desktop.exit();
    }

    public void viewSelected(DesktopPanel panel) {
        hide();
        _desktop.show(panel);
    }

    public void closeSelected(DesktopPanel panel) {
        hide();
        panel.close();
    }

    public void closeSelected(List panels) {
        hide();
        for (int i = 0; i < panels.size(); i++) {
            DesktopPanel panel = (DesktopPanel)panels.get(i);
            panel.close();
        }
    }
}
