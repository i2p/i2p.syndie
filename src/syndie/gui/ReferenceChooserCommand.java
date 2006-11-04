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
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.ChannelInfo;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.*;

/**
 * pop up and display the reference chooser
 */
public class ReferenceChooserCommand implements CLI.Command {
    public ReferenceChooserCommand() {}
    public DBClient runCommand(Opts opts, UI ui, DBClient client) {
        showRefChooser(opts, ui, client);
        return client;
    }
    
    private void showRefChooser(final Opts opts, final UI ui, final DBClient client) {
        final Display display = Display.getDefault();
        display.asyncExec(new Runnable() {
            public void run() {
                ReferenceChooserTree tree = showTree(display, opts, ui, client);
                showSearch(display, opts, ui, client, tree);
            }
        });
    }
    
    private ReferenceChooserTree showTree(Display display, Opts opts, UI ui, DBClient client) {
        Shell shell = new Shell(display, SWT.SHELL_TRIM);
        ScrolledComposite scroll = new ScrolledComposite(shell, SWT.H_SCROLL | SWT.V_SCROLL);
        scroll.setExpandHorizontal(true);
        scroll.setExpandVertical(true);
        ReferenceChooserTree chooser = new ReferenceChooserTree(client, scroll, new RefListener(ui));
        scroll.setContent(chooser.getControl());
        shell.setLayout(new FillLayout());
        scroll.setLayout(new FillLayout());

        Point setSize = chooser.getControl().getSize();
        Point preferredSize = chooser.getControl().computeSize(SWT.DEFAULT, SWT.DEFAULT);
        Point minSize = new Point(Math.min(setSize.x, preferredSize.x),
                                  Math.min(setSize.y, preferredSize.y));
        Point size = new Point(Math.max(minSize.x, 200),
                               Math.max(minSize.y, 200));
        //System.out.println("setSize: " + setSize + " pref: " + preferredSize + " min: " + minSize + " sz: " + size);
        chooser.getControl().setSize(size);
        scroll.setMinSize(size);
        shell.pack();
        shell.open();
        return chooser;
    }
    
    private void showSearch(Display display, Opts opts, UI ui, DBClient client, ReferenceChooserTree tree) {
        Shell shell = new Shell(display, SWT.SHELL_TRIM);
        ScrolledComposite scroll = new ScrolledComposite(shell, SWT.H_SCROLL | SWT.V_SCROLL);
        scroll.setExpandHorizontal(true);
        scroll.setExpandVertical(true);
        ReferenceChooserSearch search = new ReferenceChooserSearch(scroll, tree);
        scroll.setContent(search.getControl());
        shell.setLayout(new FillLayout());
        scroll.setLayout(new FillLayout());

        shell.pack();
        shell.open();
    }
    
    private class RefListener implements ReferenceChooserTree.ChoiceListener {
        private UI _ui;
        public RefListener(UI ui) { _ui = ui; }
        public void bookmarkSelected(TreeItem item, NymReferenceNode node) {
            _ui.statusMessage("bookmark selected [" + item.getText() + "]: " + node);
        }
        public void manageChannelSelected(TreeItem item, ChannelInfo channel) {
            _ui.statusMessage("manage channel selected [" + item.getText() + "]: " + channel.getChannelHash().toBase64());
        }
        public void postChannelSelected(TreeItem item, ChannelInfo channel) {
            _ui.statusMessage("post channel selected [" + item.getText() + "]: " + channel.getChannelHash().toBase64());
        }
        public void searchResultSelected(TreeItem item, ReferenceNode node) {
            _ui.statusMessage("search result selected [" + item.getText() + "]: " + node);
        }
        public void otherSelected(TreeItem item) {
            _ui.statusMessage("other item selected [" + item.getText() + "]");
        }
    }
}
