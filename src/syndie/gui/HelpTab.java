package syndie.gui;

import java.io.File;

import net.i2p.util.FileUtil;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.MessageBox;

import syndie.data.SyndieURI;

/**
 *  Simple display of HTML(?) text at the top level of the browser.
 *  Only for "help" URIs.
 *
 *  The URI params "file" supplies the location of the file, relative to the doc/ directory.
 *  @since 1.102b-8
 */
public class HelpTab extends  PageRendererTab {
    
    private static final int MAX_LINES = 2000;

    private String _body;
    private boolean _success;

    public HelpTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri);
    }
    
    @Override
    protected void initComponents() {
        getRoot().setLayout(new FillLayout());
        _renderer = ComponentBuilder.instance().createPageRenderer(getRoot(), true, false);
        _renderer.setListener(this);
        
        getBrowser().getThemeRegistry().register(this);
        getBrowser().getTranslationRegistry().register(this);
    }

    // prevent gettext
    private static final String FILE = "file";

    @Override
    public void show(SyndieURI uri) {
        String path = getURI().getString(FILE);
        if (path == null)
            path = "index.html";
        if (path.endsWith(".html")) {
            File f = new File(path);
            if (!f.isAbsolute() && !path.contains("..")) {
                f = new File(getClient().getRootDir(), "help");
                f = new File(f, path);
                // TODO use WebRipRunner to fixup links on-the-fly?
                // TODO PageRenderer expects images to be message attachments?
                _body = FileUtil.readTextFile(f.toString(), MAX_LINES, true);
            }
        }
        // TODO get title from <title> ?
        if (_body != null)
            show(_body, "Help", "tooltip");
        super.show(uri);
    }

    @Override
    public void tabShown() {
        if (_body == null) {
            MessageBox box = new MessageBox(getRoot().getShell(), SWT.ICON_INFORMATION | SWT.OK);
            box.setText(getBrowser().getTranslationRegistry().getText("File not found"));
            box.setMessage(getBrowser().getTranslationRegistry().getText("File not found"));
            box.open();
            getBrowser().getNavControl().unview(getURI());
            return;
        }    
        super.tabShown();
    }    
    
    @Override
    public Image getIcon() { return ImageUtil.ICON_HM_ABOUT; }
}
