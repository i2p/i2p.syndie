package syndie.gui;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import syndie.data.SyndieURI;

/**
 *
 */
public class MessageEditorNewTab extends BrowserTab {
    private MessageEditorNew _editor;
    
    public MessageEditorNewTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri);
    }
    
    protected void initComponents() {
        getRoot().setLayout(new FillLayout());
        _editor = new MessageEditorNew(getBrowser(), getRoot());
    }
    
    protected void disposeDetails() { _editor.dispose(); }
    
    public Image getIcon() { return ImageUtil.ICON_TAB_PAGE; }
    public String getName() { return "x editor"; }
    public String getDescription() { return "x desc"; }
}
