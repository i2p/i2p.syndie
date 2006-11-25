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
public class SyndicationView implements Translatable {
    private BrowserControl _browser;
    private Composite _parent;
    private Composite _root;
    private Group _archiveGroup;
    private SyndicationArchiveView _archives;
    private Group _pendingGroup;
    private SyndicationPendingView _pending;
    private Group _statusGroup;
    private SyndicationStatusView _status;
    
    public SyndicationView(BrowserControl browser, Composite parent) {
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
        
        _archiveGroup = new Group(_root, SWT.NONE);
        _archiveGroup.setLayout(new FillLayout());
        _archiveGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _archives = new SyndicationArchiveView(_browser, _archiveGroup);
        
        _pendingGroup = new Group(_root, SWT.NONE);
        _pendingGroup.setLayout(new FillLayout());
        _pendingGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _pending = new SyndicationPendingView(_browser, _pendingGroup);
        
        _statusGroup = new Group(_root, SWT.NONE);
        _statusGroup.setLayout(new FillLayout());
        _statusGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _status = new SyndicationStatusView(_browser, _statusGroup);
        
        _browser.getTranslationRegistry().register(this);
    }

    private static final String T_ARCHIVE = "syndie.gui.syndicationview.archive";
    private static final String T_PENDING = "syndie.gui.syndicationview.pending";
    private static final String T_STATUS = "syndie.gui.syndicationview.status";
    
    public void translate(TranslationRegistry registry) {
        _archiveGroup.setText(registry.getText(T_ARCHIVE, "Archives"));
        _pendingGroup.setText(registry.getText(T_PENDING, "Pending"));
        _statusGroup.setText(registry.getText(T_STATUS, "Status"));
    }
    
}
