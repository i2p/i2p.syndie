package syndie.gui;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import net.i2p.data.Hash;
import net.i2p.data.PrivateKey;
import net.i2p.data.SessionKey;
import net.i2p.data.SigningPrivateKey;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.MessageBox;
import syndie.data.MessageInfo;
import syndie.data.SyndieURI;

/**
 *
 */
public class MessageViewTab extends BrowserTab implements Translatable, Themeable {
    private MessageView _view;
    private String _desc;
    
    public MessageViewTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri);
    }
    
    protected void initComponents() {
        getRoot().setLayout(new FillLayout());
        _view = new MessageView(getBrowser(), getRoot(), getURI());
        
        getBrowser().getThemeRegistry().register(this);
        getBrowser().getTranslationRegistry().register(this);
    }
    
    protected void tabShown() {
        if (!_view.isKnownLocally()) {
            MessageBox box = new MessageBox(getRoot().getShell(), SWT.ICON_INFORMATION | SWT.OK);
            box.setText(getBrowser().getTranslationRegistry().getText(T_NOMSG_TITLE, "Message unknown"));
            box.setMessage(getBrowser().getTranslationRegistry().getText(T_NOMSG, "The selected message is not known locally"));
            getBrowser().unview(getURI());
            box.open();
            return;
        }    
        _view.enable();
    }
    
    private static final String T_NOMSG_TITLE = "syndie.gui.messageview.nomsg.title";
    private static final String T_NOMSG = "syndie.gui.messageview.nomsg";
    
    protected void disposeDetails() { 
        _view.dispose();
        getBrowser().getTranslationRegistry().unregister(this);
        getBrowser().getThemeRegistry().unregister(this);
    }

    
    public boolean canShow(SyndieURI uri) { 
        if (super.canShow(uri)) return true;
        if (uri == null) return false;
        if (uri.isChannel() && (uri.getScope() != null) && (uri.getMessageId() != null))
            return getURI().getScope().equals(uri.getScope()) && (getURI().getMessageId().equals(uri.getMessageId()));
        return false;
    }
    
    public void show(SyndieURI uri) {
        if (uri.getPage() != null)
            _view.viewPage(uri.getPage().intValue());
    }
    
    
    public Image getIcon() { return ImageUtil.ICON_TAB_MSG; }
    public String getName() { return _view.getTitle(); }
    public String getDescription() { return getURI().toString(); }

    public void translate(TranslationRegistry registry) {
        // nothing translatable
    }
    public void applyTheme(Theme theme) {
        // nothing themeable
    }
}
