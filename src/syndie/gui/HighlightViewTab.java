package syndie.gui;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import net.i2p.data.Hash;
import net.i2p.data.PrivateKey;
import net.i2p.data.SessionKey;
import net.i2p.data.SigningPrivateKey;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import syndie.data.MessageInfo;
import syndie.data.SyndieURI;

/**
 *
 */
public class HighlightViewTab extends BrowserTab implements Translatable, Themeable {
    private HighlightView _view;
    
    public HighlightViewTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri);
    }
    
    protected void initComponents() {
        getRoot().setLayout(new FillLayout());
        _view = new HighlightView(getBrowser(), getRoot());
        
        getBrowser().getThemeRegistry().register(this);
        getBrowser().getTranslationRegistry().register(this);
    }
    
    protected void disposeDetails() { 
        getBrowser().getTranslationRegistry().unregister(this);
        getBrowser().getThemeRegistry().unregister(this);
    }
    
    private static final String T_NAME = "syndie.gui.highlightviewtab.name";
    private static final String T_DESC = "syndie.gui.highlightviewtab.desc";
    
    public Image getIcon() { return ImageUtil.ICON_TAB_HIGHLIGHTS; }
    public String getName() { return getBrowser().getTranslationRegistry().getText(T_NAME, "Highlights"); }
    public String getDescription() { return getBrowser().getTranslationRegistry().getText(T_DESC, "Quick summary of local Syndie activity"); }

    public void translate(TranslationRegistry registry) {}
    public void applyTheme(Theme theme) {}
}
