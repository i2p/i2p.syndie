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
public class BugReportTab extends BrowserTab implements Translatable, Themeable {
    private BugReport _report;
    
    public BugReportTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri);
    }
    
    protected void initComponents() {
        getRoot().setLayout(new FillLayout());
        _report = new BugReport(_client, _ui, _themeRegistry, _translationRegistry, getBrowser(), getBrowser().getNavControl(), getRoot(), getURI());
        
        getBrowser().getThemeRegistry().register(this);
        getBrowser().getTranslationRegistry().register(this);
    }
    
    protected void disposeDetails() { 
        _report.dispose();
        getBrowser().getTranslationRegistry().unregister(this);
        getBrowser().getThemeRegistry().unregister(this);
    }
    
    
    public Image getIcon() { return ImageUtil.ICON_TAB_HIGHLIGHTS; }
    public String getName() { return getBrowser().getTranslationRegistry().getText("Bug report"); }
    public String getDescription() { return getBrowser().getTranslationRegistry().getText("File a new bug report"); }

    public void translate(TranslationRegistry registry) { reconfigItem(); }
    public void applyTheme(Theme theme) {}
}
