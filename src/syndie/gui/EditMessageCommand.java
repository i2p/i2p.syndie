package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import syndie.data.SyndieURI;
import syndie.db.*;

/**
 * pop up and display an swt message editor
 */
public class EditMessageCommand implements CLI.Command {
    public EditMessageCommand() {}
    public DBClient runCommand(Opts opts, UI ui, DBClient client) {
        edit(opts, ui, client);
        return client;
    }
    
    private void edit(final Opts opts, final UI ui, final DBClient client) {
        final Display display = Display.getDefault();
        display.asyncExec(new Runnable() {
            public void run() {
                Shell shell = new Shell(display, SWT.SHELL_TRIM);
                ScrolledComposite scroll = new ScrolledComposite(shell, SWT.H_SCROLL | SWT.V_SCROLL);
                scroll.setExpandHorizontal(true);
                scroll.setExpandVertical(true);
                MessageEditor editor = new MessageEditor(client, scroll, null, null);
                scroll.setContent(editor.getControl());
                shell.setLayout(new FillLayout());
                scroll.setLayout(new FillLayout());

                editor.addPage();
                Point setSize = editor.getControl().getSize();
                Point preferredSize = editor.getControl().computeSize(SWT.DEFAULT, SWT.DEFAULT);
                Point minSize = new Point(Math.min(setSize.x, preferredSize.x),
                                          Math.min(setSize.y, preferredSize.y));
                Point size = new Point(Math.max(minSize.x, 400),
                                       Math.max(minSize.y, 400));
                //System.out.println("setSize: " + setSize + " pref: " + preferredSize + " min: " + minSize + " sz: " + size);
                editor.getControl().setSize(size);
                scroll.setMinSize(size);
                shell.pack();
                shell.open();
            }
        });
    }
}
