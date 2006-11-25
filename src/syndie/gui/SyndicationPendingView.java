package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

/**
 *
 */
public class SyndicationPendingView implements Translatable {
    private BrowserControl _browser;
    private Composite _parent;
    private Composite _root;
    
    public SyndicationPendingView(BrowserControl browser, Composite parent) {
        _browser = browser;
        _parent = parent;
        initComponents();
    }
    
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(1, true));
        
        _browser.getTranslationRegistry().register(this);
    }

    public void translate(TranslationRegistry registry) {}
}
