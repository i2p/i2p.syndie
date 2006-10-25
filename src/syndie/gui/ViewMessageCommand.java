package syndie.gui;

import java.net.URISyntaxException;
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
 * pop up and display the given message in an swt window
 */
public class ViewMessageCommand implements CLI.Command {
    public ViewMessageCommand() {}
    public DBClient runCommand(Opts opts, UI ui, DBClient client) {
        String uri = opts.getOptValue("uri");
        if (uri != null) {
            try {
                final SyndieURI msg = new SyndieURI(uri);
                view(client, msg);
            } catch (URISyntaxException use) {
                final SyndieURI msg = SyndieURI.createSearch(uri);
                view(client, msg);
            }
        }
        
        return client;
    }
    
    private void view(final DBClient client, final SyndieURI msg) {
        final Display display = Display.getDefault();
        display.asyncExec(new Runnable() {
            public void run() {
                Shell shell = new Shell(display, SWT.SHELL_TRIM);
                ScrolledComposite scroll = new ScrolledComposite(shell, SWT.H_SCROLL | SWT.V_SCROLL);
                scroll.setExpandHorizontal(true);
                scroll.setExpandVertical(true);
                PageRenderer renderer = new PageRenderer(scroll);
                scroll.setContent(renderer.getComposite());
                shell.setLayout(new FillLayout());
                scroll.setLayout(new FillLayout());
                Point sz = shell.computeSize(600, 800);
                shell.setSize(sz);
                sz = renderer.getComposite().computeSize(sz.x-20, sz.y-20); //SWT.DEFAULT, SWT.DEFAULT); //400, 600);
                renderer.renderPage(client, msg);
                scroll.setMinSize(sz);
                shell.open();
                
            }
        });
    }
}
