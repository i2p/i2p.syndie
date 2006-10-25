package syndie.gui;

import java.net.URISyntaxException;
import java.util.Map;
import net.i2p.data.Hash;
import net.i2p.data.PrivateKey;
import net.i2p.data.SessionKey;
import net.i2p.data.SigningPrivateKey;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.graphics.Image;
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
                view(client, msg, ui);
            } catch (URISyntaxException use) {
                final SyndieURI msg = SyndieURI.createSearch(uri);
                view(client, msg, ui);
            }
        }
        
        return client;
    }
    
    private void view(final DBClient client, final SyndieURI msg, final UI ui) {
        final Display display = Display.getDefault();
        display.asyncExec(new Runnable() {
            public void run() {
                Shell shell = new Shell(display, SWT.SHELL_TRIM);
                ScrolledComposite scroll = new ScrolledComposite(shell, SWT.H_SCROLL | SWT.V_SCROLL);
                scroll.setExpandHorizontal(true);
                scroll.setExpandVertical(true);
                PageRenderer renderer = new PageRenderer(scroll);
                renderer.setListener(new DummyPageActionListener(ui));
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
    
    private class DummyPageActionListener implements PageRenderer.PageActionListener {
        private UI _ui;
        public DummyPageActionListener(UI ui) { _ui = ui; }

        public void viewScopeMessages(PageRenderer renderer, Hash scope) {
            _ui.statusMessage("view messages in " + scope.toBase64());
        }

        public void viewScopeMetadata(PageRenderer renderer, Hash scope) {
            _ui.statusMessage("view metadata for " + scope.toBase64());
        }

        public void view(PageRenderer renderer, SyndieURI uri) {
            _ui.statusMessage("view " + uri);
        }

        public void bookmark(PageRenderer renderer, SyndieURI uri) {
            _ui.statusMessage("bookmark " + uri);
        }

        public void banScope(PageRenderer renderer, Hash scope) {
            _ui.statusMessage("ban " + scope.toBase64());
        }

        public void viewImage(PageRenderer renderer, Image img) {
            _ui.statusMessage("view image: " + img);
        }

        public void ignoreImageScope(PageRenderer renderer, Hash scope) {
            _ui.statusMessage("ignore images from " + scope.toBase64());
        }

        public void importReadKey(PageRenderer renderer, Hash referencedBy, Hash keyScope, SessionKey key) {
            _ui.statusMessage("import read key " + keyScope.toBase64() + ": " + key.toBase64() + " (given to us by " + referencedBy.toBase64() + ")");
        }

        public void importPostKey(PageRenderer renderer, Hash referencedBy, Hash keyScope, SigningPrivateKey key) {
            _ui.statusMessage("import post key " + keyScope.toBase64() + ": " + key.toBase64() + " (given to us by " + referencedBy.toBase64() + ")");
        }

        public void importManageKey(PageRenderer renderer, Hash referencedBy, Hash keyScope, SigningPrivateKey key) {
            _ui.statusMessage("import manage key " + keyScope.toBase64() + ": " + key.toBase64() + " (given to us by " + referencedBy.toBase64() + ")");
        }

        public void importReplyKey(PageRenderer renderer, Hash referencedBy, Hash keyScope, PrivateKey key) {
            _ui.statusMessage("import reply key " + keyScope.toBase64() + ": " + key.toBase64() + " (given to us by " + referencedBy.toBase64() + ")");
        }

        public void importArchiveKey(PageRenderer renderer, Hash referencedBy, SyndieURI archiveURI, SessionKey key) {
            _ui.statusMessage("import archive key for " + archiveURI + ": " + key.toBase64() + " (given to us by " + referencedBy.toBase64() + ")");
        }        

        public void saveAllImages(PageRenderer renderer, Map images) {
            _ui.statusMessage("save images: " + images);
        }

        public void saveImage(PageRenderer renderer, String suggestedName, Image img) {
            _ui.statusMessage("save image: " + suggestedName);
        }
    }
}
