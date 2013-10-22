package syndie.gui;

import java.io.File;

import net.i2p.util.SystemVersion;

import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTError;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.LocationEvent;
import org.eclipse.swt.browser.LocationListener;
import org.eclipse.swt.browser.ProgressEvent;
import org.eclipse.swt.browser.ProgressListener;
import org.eclipse.swt.browser.StatusTextEvent;
import org.eclipse.swt.browser.StatusTextListener;
import org.eclipse.swt.browser.TitleEvent;
import org.eclipse.swt.browser.TitleListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.swt.widgets.Text;

import syndie.data.SyndieURI;

/**
 *  Experiment to try out the SWT Browser
 *
 *  @since 1.102b-12
 */
class RealBrowserTab extends BrowserTab implements Translatable, Themeable {
    
    private org.eclipse.swt.browser.Browser _browser;
    private String _name;
    private Button _back, _next, _stop, _go, _reload;
    private ProgressBar _bar;
    private Text _loc;
    private boolean _inProgress;
    private boolean _badURLPopupShown;

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
            Composite all = new Composite(getRoot(), SWT.NONE);
            all.setLayout(new GridLayout(1, false));
            GridData gd = new GridData(GridData.FILL, GridData.FILL, true, true);
            all.setLayoutData(gd);
            Composite top = new Composite(all, SWT.NONE);
            top.setLayout(new GridLayout(7, false));
            gd = new GridData(GridData.FILL, GridData.BEGINNING, true, false);
            top.setLayoutData(gd);

            _back = new Button(top, SWT.PUSH);
            _back.setLayoutData(new GridData(GridData.BEGINNING, GridData.FILL, false, false));
            _back.addSelectionListener(new FireSelectionListener() {
                public void fire() { back(); }
            });
            _next = new Button(top, SWT.PUSH);
            _next.setLayoutData(new GridData(GridData.BEGINNING, GridData.FILL, false, false));
            _next.addSelectionListener(new FireSelectionListener() {
                public void fire() { next(); }
            });
            _loc = new Text(top, SWT.SINGLE | SWT.BORDER);
            gd = new GridData(GridData.FILL, GridData.FILL, true, false);
            gd.widthHint = 400;
            _loc.setLayoutData(gd);
            _loc.addTraverseListener(new TraverseListener() {
                public void keyTraversed(TraverseEvent evt) {
                    if (evt.detail == SWT.TRAVERSE_RETURN)
                        go(_loc.getText());
                }
            });
            _go = new Button(top, SWT.PUSH);
            _go.setLayoutData(new GridData(GridData.BEGINNING, GridData.FILL, false, false));
            _go.setImage(ImageUtil.ICON_ARCHIVE_TYPE_URL);
            _go.addSelectionListener(new FireSelectionListener() {
                public void fire() { go(_loc.getText()); }
            });
            _stop = new Button(top, SWT.PUSH);
            _stop.setLayoutData(new GridData(GridData.BEGINNING, GridData.FILL, false, false));
            _stop.setImage(ImageUtil.ICON_CANCEL);
            _stop.addSelectionListener(new FireSelectionListener() {
                public void fire() { stop(); }
            });
            _reload = new Button(top, SWT.PUSH);
            _reload.setLayoutData(new GridData(GridData.BEGINNING, GridData.FILL, false, false));
            _reload.addSelectionListener(new FireSelectionListener() {
                public void fire() { reload(); }
            });
            _bar = new ProgressBar(top, SWT.HORIZONTAL);
            _bar.setLayoutData(new GridData(GridData.END, GridData.FILL, false, false));

            Composite bottom = new Composite(all, SWT.NONE);
            bottom.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
            bottom.setLayout(new FillLayout());

            //bottom.setLayout(new GridLayout(1, false));
            //gd = new GridData(GridData.FILL, GridData.FILL, true, true);
            //bottom.setLayoutData(gd);

            try {
                _browser = new org.eclipse.swt.browser.Browser(bottom, SWT.NONE);
                _ui.debugMessage("Browser using default");
            } catch (SWTError e1) {
                try {
                    _browser = new org.eclipse.swt.browser.Browser(bottom, SWT.WEBKIT);
                    _ui.debugMessage("Browser using webkit");
                } catch (SWTError e2) {
                    try {
                        _browser = new org.eclipse.swt.browser.Browser(bottom, SWT.MOZILLA);
                        _ui.debugMessage("Browser using mozilla");
                    } catch (SWTError e3) {
                        _ui.debugMessage("No browser available");
                    }
                }
            }
            if (_browser != null) {
                // TODO save/restore cookies
                _browser.setJavascriptEnabled(false);
                _browser.addLocationListener(new LocationListener() {
                    public void changing(LocationEvent event) {
                        _name = event.location;
                        _loc.setText(event.location);
                        _inProgress = true;
                        _ui.debugMessage("LL changing " + event.location);
                        reconfigItem();
                    }
                    public void changed(LocationEvent event) {
                        _name = event.location;
                        _loc.setText(event.location);
                        _inProgress = false;
                        //_ui.debugMessage("LL changed " + event.location);
                        reconfigItem();
                    }
                });
                _browser.addProgressListener(new ProgressListener() {
                    public void changed(ProgressEvent event) {
                        _bar.setMaximum(event.total);
                        _bar.setSelection(event.current);
                        _inProgress = event.current < event.total;
                        //_ui.debugMessage("PL changed " + event.current + "/" + event.total);
                        reconfigItem();
                    }
                    public void completed(ProgressEvent event) {
                        _inProgress = false;
                        //_ui.debugMessage("PL completed");
                        reconfigItem();
                    }
                });
                _browser.addStatusTextListener(new StatusTextListener() {
                    public void changed(StatusTextEvent event) {
                        _inProgress = false;
                        //_ui.debugMessage("STL changed");
                        reconfigItem();
                    }
                });
                _browser.addTitleListener(new TitleListener() {
                    public void changed(TitleEvent event) {
                        _name = event.title;
                        //_ui.debugMessage("TL changed");
                        reconfigItem();
                    }
                });
            }
        }
        getBrowser().getThemeRegistry().register(this);
        getBrowser().getTranslationRegistry().register(this);
    }

    private void back() {
        if (_browser != null)
            _browser.back();
    }

    private void next() {
        if (_browser != null)
            _browser.forward();
    }

    private void stop() {
        if (_browser != null)
            _browser.stop();
    }

    private void reload() {
        if (_browser != null)
            _browser.refresh();
    }

    private void go(String url) {
        _loc.setText(url);
        _bar.setSelection(0);
        _inProgress = true;
        updateButtons();
        _browser.setUrl(url);
    }

    @Override
    protected void reconfigItem() {
        super.reconfigItem();
        updateButtons();
    }

    private void updateButtons() {
        _back.setEnabled(_browser != null && _browser.isBackEnabled());
        _next.setEnabled(_browser != null && _browser.isForwardEnabled());
        _stop.setEnabled(_browser != null && _inProgress);
        _go.setEnabled(_browser != null && !_inProgress);
        _reload.setEnabled(_browser != null && !_inProgress);
        _loc.setEnabled(_browser != null && !_inProgress);
        _bar.setEnabled(_browser != null && _inProgress);
        showWaitCursor(_browser != null && _inProgress);
    }

    private void showWaitCursor(boolean show) {
        if (show)
            getRoot().getShell().setCursor(ImageUtil.CURSOR_WAIT);
        else
            getRoot().getShell().setCursor(null);
    
    }

    @Override
    public void show(SyndieURI uri) {
        String url = uri.getURL();
        if (url != null && url.startsWith("http://"))
            go(url);
        super.show(uri);
    }

    @Override
    public void tabShown() {
        updateButtons();
        if (_browser == null) {
            MessageBox box = new MessageBox(getRoot().getShell(), SWT.ICON_INFORMATION | SWT.OK);
            box.setText("Not supported");
            box.setMessage("Sorry, the Syndie web browser is not supported on your system");
            box.open();
            closeTab();
            return;
        }    
        String url = getURI().getURL();
        if ((!_badURLPopupShown) && (url == null || !url.startsWith("http://"))) {
            _badURLPopupShown = true;
            MessageBox box = new MessageBox(getRoot().getShell(), SWT.ICON_INFORMATION | SWT.OK);
            box.setText("Bad URL");
            box.setMessage("Bad URL");
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
        _back.setText(getText("Back"));
        _next.setText(getText("Forward"));
        _go.setText(getText("Go"));
        _stop.setText(getText("Stop"));
        _reload.setText(getText("Reload"));
    }
    public void applyTheme(Theme theme) {
        // TODO
    }
}
