package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.ChannelInfo;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.WatchedChannel;
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
                AcceptListener lsnr = new AcceptListener(ui);
                if (true) {
                    ReferenceChooserPopup popup = new ReferenceChooserPopup(null, new DummyBrowserControl(client, ui), lsnr);
                    popup.show();
                } else {
                    ReferenceChooserTree tree = showTree(display, opts, ui, client, lsnr);
                    ReferenceChooserSearch search = showSearch(display, opts, ui, client, tree);
                    showInfo(display, opts, ui, client, tree, search, lsnr);
                }
            }
        });
    }
    
    private ReferenceChooserTree showTree(Display display, Opts opts, UI ui, DBClient client, AcceptListener lsnr) {
        Shell shell = new Shell(display, SWT.SHELL_TRIM);
        ScrolledComposite scroll = new ScrolledComposite(shell, SWT.H_SCROLL | SWT.V_SCROLL);
        scroll.setExpandHorizontal(true);
        scroll.setExpandVertical(true);
        ReferenceChooserTree chooser = new ReferenceChooserTree(new DummyBrowserControl(client, ui), scroll, new RefListener(ui), lsnr);
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
    
    private ReferenceChooserSearch showSearch(Display display, Opts opts, UI ui, DBClient client, ReferenceChooserTree tree) {
        Shell shell = new Shell(display, SWT.SHELL_TRIM);
        ScrolledComposite scroll = new ScrolledComposite(shell, SWT.H_SCROLL | SWT.V_SCROLL);
        scroll.setExpandHorizontal(true);
        scroll.setExpandVertical(true);
        ReferenceChooserSearch search = new ReferenceChooserSearch(scroll, tree, new DummyBrowserControl(client, ui));
        scroll.setContent(search.getControl());
        shell.setLayout(new FillLayout());
        scroll.setLayout(new FillLayout());

        shell.pack();
        shell.open();
        return search;
    }
    
    private void showInfo(Display display, Opts opts, UI ui, DBClient client, ReferenceChooserTree tree, ReferenceChooserSearch search, AcceptListener lsnr) {
        Shell shell = new Shell(display, SWT.SHELL_TRIM);
        ScrolledComposite scroll = new ScrolledComposite(shell, SWT.H_SCROLL | SWT.V_SCROLL);
        scroll.setExpandHorizontal(true);
        scroll.setExpandVertical(true);
        ReferenceChooserInfo info = new ReferenceChooserInfo(scroll, tree, lsnr, new DummyBrowserControl(client, ui));
        lsnr.setChooser(tree);
        lsnr.setInfo(info);
        lsnr.setSearch(search);
        scroll.setContent(info.getControl());
        shell.setLayout(new FillLayout());
        scroll.setLayout(new FillLayout());

        Point setSize = info.getControl().getSize();
        Point preferredSize = info.getControl().computeSize(SWT.DEFAULT, SWT.DEFAULT);
        Point minSize = new Point(Math.min(setSize.x, preferredSize.x),
                                  Math.min(setSize.y, preferredSize.y));
        Point size = new Point(Math.max(minSize.x, 200),
                               Math.max(minSize.y, 100));
        //System.out.println("setSize: " + setSize + " pref: " + preferredSize + " min: " + minSize + " sz: " + size);
        info.getControl().setSize(size);
        scroll.setMinSize(size);

        shell.pack();
        shell.open();
    }
    
    private static class RefListener implements ReferenceChooserTree.ChoiceListener {
        private UI _ui;
        public RefListener(UI ui) { _ui = ui; }
        public void watchedChannelSelected(TreeItem item, WatchedChannel watched) {}
        public void bookmarkSelected(TreeItem item, NymReferenceNode node) {
            _ui.statusMessage("bookmark selected [" + item.getText() + "]: " + node);
        }
        public void manageChannelSelected(TreeItem item, ChannelInfo channel) {
            _ui.statusMessage("manage channel selected [" + item.getText() + "]: " + channel.getChannelHash().toBase64());
        }
        public void postChannelSelected(TreeItem item, ChannelInfo channel) {
            _ui.statusMessage("post channel selected [" + item.getText() + "]: " + channel.getChannelHash().toBase64());
        }
        public void searchResultSelected(String name, ReferenceNode node) {
            _ui.statusMessage("search result selected [" + name + "]: " + node);
        }
        public void otherSelected(TreeItem item) {
            _ui.statusMessage("other item selected [" + item.getText() + "]");
        }
    }
    private static class AcceptListener implements ReferenceChooserTree.AcceptanceListener {
        private UI _ui;
        private ReferenceChooserTree _chooser;
        private ReferenceChooserSearch _search;
        private ReferenceChooserInfo _info;
        public AcceptListener(UI ui) { _ui = ui; }
        public void referenceAccepted(SyndieURI uri) {
            _ui.statusMessage("reference accepted: " + uri.toString());
            close();
        }
        public void referenceChoiceAborted() { close(); }
        private void close() {
            hideShell(_chooser.getControl());
            hideShell(_search.getControl());
            hideShell(_info.getControl());
        }
        private void hideShell(Control control) {
            if (control == null) return;
            Shell shell = control.getShell();
            if ( (shell == null) || (shell.isDisposed()) || (!shell.getVisible()) ) 
                return;
            shell.setVisible(false);
        }

        private void setChooser(ReferenceChooserTree chooser) { _chooser = chooser; }
        private void setSearch(ReferenceChooserSearch search) { _search = search; }
        private void setInfo(ReferenceChooserInfo info) { _info = info; }
    }
}
