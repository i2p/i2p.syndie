package syndie.gui;

import java.io.File;

import net.i2p.util.SystemVersion;

import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.TitleEvent;
import org.eclipse.swt.browser.TitleListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.MessageBox;

import syndie.data.SyndieURI;

/**
 *  Experiment to try out the SWT Browser
 *
 *  @since 1.102b-12
 */
public class RealBrowserTab extends BrowserTab implements Translatable, Themeable {
    
    private org.eclipse.swt.browser.Browser _browser;
    private String _name;

    /*
     *   http://www.eclipse.org/swt/faq.php#browserproxy

       Q: How do I set a proxy for the Browser to use?
       A:

        Windows: All Browser instances, regardless of native renderer, automatically use Windows' global proxy settings.
        These settings can be changed at any time in the Windows Control Panel.

        OS X: All Browser instances, regardless of native renderer, automatically use OS X's global proxy settings.
        These settings can be changed at any time in the OS X System Preferences.

        Linux/Solaris: Proxy information must be explicitly specified by setting values for java properties
        network.proxy_host and network.proxy_port (@since 3.4). These properties are checked the first time a Browser
        is created, and if set, will be used for all non-local HTTP, HTTPS and FTP requests in all Browser instances.
        A user wishing to set these values should do so by passing -D... VM arguments to the JRE at startup.
     */
     static {
         System.setProperty("network.proxy_host", "127.0.0.1");
         System.setProperty("network.proxy_port", "4444");
     }


    public RealBrowserTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri);
    }
    
    @Override
    protected void initComponents() {
        getRoot().setLayout(new FillLayout());
        _name = "Browser Test";
        if (SystemVersion.isWindows() || SystemVersion.isMac()) {
            // sorry
        } else {
            _browser = new org.eclipse.swt.browser.Browser(getRoot(), SWT.WEBKIT);
            _browser.setJavascriptEnabled(false);
            _browser.addTitleListener(new TitleListener() {
                public void changed(TitleEvent event) {
                    _name = event.title;
                    reconfigItem();
                }
            });
        }
        getBrowser().getThemeRegistry().register(this);
        getBrowser().getTranslationRegistry().register(this);
    }

    @Override
    public void show(SyndieURI uri) {
        if (SystemVersion.isWindows() || SystemVersion.isMac()) {
            MessageBox box = new MessageBox(getRoot().getShell(), SWT.ICON_INFORMATION | SWT.OK);
            box.setText("Sorry, Linux only");
            box.setMessage("Not secure on Windows or Mac");
            getBrowser().getNavControl().unview(getURI());
            box.open();
            return;
        }    
        String url = uri.getURL();
        if (url != null) {
            _browser.setUrl(url);
        } else {
            MessageBox box = new MessageBox(getRoot().getShell(), SWT.ICON_INFORMATION | SWT.OK);
            box.setText("Bad URL");
            box.setMessage("Bad URL");
            getBrowser().getNavControl().unview(getURI());
            box.open();
            return;
        }    
        super.tabShown();
    }
    
    protected void disposeDetails() { 
        getBrowser().getTranslationRegistry().unregister(this);
        getBrowser().getThemeRegistry().unregister(this);
    }
    
    @Override
    public Image getIcon() { return ImageUtil.ICON_ARCHIVE_TYPE_URL; }
    @Override
    public String getName() { return _name; }
    @Override
    public String getDescription() { return "TESTING - MAY NOT BE SECURE"; }

    public void translate(TranslationRegistry registry) {
        // TODO
    }
    public void applyTheme(Theme theme) {
        // TODO
    }
}
