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
import syndie.db.NullUI;

/**
 * pop up and display an swt message editor
 */
public class MessageTreeCommand implements CLI.Command {
    public MessageTreeCommand() {}
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
                MessageTree tree = new MessageTree(new DummyBrowserControl(client, new NullUI()), shell, new Listener());
                shell.setLayout(new FillLayout());
                
                List msgs = getThreads(client, chan);
                //if (chan != null)
                //    tree.showChannel(false);
                tree.sortDate(true);
                tree.setMessages(msgs);
                Point setSize = tree.getControl().getSize();
                Point preferredSize = tree.getControl().computeSize(SWT.DEFAULT, SWT.DEFAULT);
                Point minSize = new Point(Math.min(setSize.x, preferredSize.x),
                                          Math.min(setSize.y, preferredSize.y));
                Point size = new Point(Math.max(minSize.x, 200),
                                       Math.max(minSize.y, 200));
                //System.out.println("setSize: " + setSize + " pref: " + preferredSize + " min: " + minSize + " sz: " + size);
                tree.getControl().setSize(size);
       
                shell.pack();
                shell.open();
            }
        });
    }
    
    private static List getThreads(DBClient client, byte chan[]) {
        HashSet channels = null;
        if (chan != null) {
            channels = new HashSet();
            channels.add(new Hash(chan));
        }
        System.out.println("getting threads in channels: " + channels);
        ThreadAccumulator acc = new ThreadAccumulator(client, new NullUI());
        acc.setScope(channels);
        acc.gatherThreads();
        List threads = new ArrayList();
        for (int i = 0; i < acc.getThreadCount(); i++)
            threads.add(acc.getRootThread(i));
        return threads;
    }
    private static List getThreads(DBClient client) {
        List rv = new ArrayList();
        Hash h = new Hash(Base64.decode("QU7SmeHqW1etRmXNEfCR6jP7CC0Tyln5V7YalYq1jnY="));
        SyndieURI uri = SyndieURI.createMessage(h, 1161059215163L);
        rv.add(new ReferenceNode(null, uri, null, null));
        rv.add(new ReferenceNode(null, uri, null, null));
        ReferenceNode node = new ReferenceNode(null, uri, null, null);
        node.addChild(null, uri, null, null);
        node.addChild(null, uri, null, null);
        ReferenceNode sub = node.addChild(null, uri, null, null);
        sub.addChild(null, uri, null, null);
        sub.addChild(null, uri, null, null);
        node.addChild(null, uri, null, null);
        SyndieURI nonexistantURI = SyndieURI.createMessage(h, 1161059215162L);
        sub = node.addChild(null, nonexistantURI, null, null);
        sub.addChild(null, uri, null, null);
        sub.addChild(null, nonexistantURI, null, null);
        rv.add(node);
        rv.add(new ReferenceNode(null, uri, null, null));
        rv.add(new ReferenceNode(null, uri, null, null));
        rv.add(new ReferenceNode(null, uri, null, null));
        return rv;
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
