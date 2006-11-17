package syndie.gui;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.HashSet;
import net.i2p.data.Base64;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.*;

/**
 */
public class BrowseForumCommand implements CLI.Command {
    public BrowseForumCommand() {}
    public DBClient runCommand(Opts opts, UI ui, DBClient client) {
        view(opts, ui, client);
        return client;
    }
    
    private void view(final Opts opts, final UI ui, final DBClient client) {
        final byte chan[] = opts.getOptBytes("channel");
        final Display display = Display.getDefault();
        display.asyncExec(new Runnable() {
            public void run() {
                Shell shell = new Shell(display, SWT.SHELL_TRIM);
                shell.setLayout(new FillLayout());
                BrowseForum browse = new BrowseForum(shell, client, new Listener(), new NullUI());

                if ( (chan != null) && (chan.length == Hash.HASH_LENGTH) )
                    browse.setFilter(SyndieURI.createSearch(new Hash(chan)));
                else
                    browse.setFilter(SyndieURI.DEFAULT_SEARCH_URI);
                Point setSize = browse.getControl().getSize();
                Point preferredSize = browse.getControl().computeSize(SWT.DEFAULT, SWT.DEFAULT);
                Point minSize = new Point(Math.min(setSize.x, preferredSize.x),
                                          Math.min(setSize.y, preferredSize.y));
                Point size = new Point(Math.max(minSize.x, 200),
                                       Math.max(minSize.y, 200));
                //System.out.println("setSize: " + setSize + " pref: " + preferredSize + " min: " + minSize + " sz: " + size);
                browse.getControl().setSize(size);
       
                shell.pack();
                shell.open();
            }
        });
    }
    
    private class Listener implements MessageTree.MessageTreeListener {
        public void messageSelected(MessageTree tree, SyndieURI uri, boolean toView) {
            if ( (uri != null) && (uri.getScope() != null) ) {
                if (toView)
                    System.out.println("view " + uri.getScope().toBase64().substring(0,6) + ":" + uri.getMessageId());
                else
                    System.out.println("hover over " + uri.getScope().toBase64().substring(0,6) + ":" + uri.getMessageId());
            }
        }
        public void filterApplied(MessageTree tree, SyndieURI filter) {
            System.out.println("filter applied [" + filter + "]");
        }
    }
}
