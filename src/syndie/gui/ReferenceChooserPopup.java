package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.ChannelInfo;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 *
 */
class ReferenceChooserPopup implements ReferenceChooserTree.ChoiceListener, ReferenceChooserTree.AcceptanceListener, Translatable {
    private Shell _parent;
    private BrowserControl _browser;
    private UI _ui;
    private DBClient _client;
    private Shell _shell;
    private ReferenceChooserTree.AcceptanceListener _lsnr;
    private ReferenceChooserTree _tree;
    private ReferenceChooserSearch _search;
    private ReferenceChooserInfo _info;
    private String _titleKey;
    private String _titleVal;
    
    public ReferenceChooserPopup(Shell parent, BrowserControl browser, String titleKey, String titleVal) { this(parent, browser, null, titleKey, titleVal); }
    public ReferenceChooserPopup(Shell parent, BrowserControl browser) { this(parent, browser, null); }
    public ReferenceChooserPopup(Shell parent, BrowserControl browser, ReferenceChooserTree.AcceptanceListener lsnr) {
        this(parent, browser, lsnr, T_TITLE, "Reference chooser");
    }
    public ReferenceChooserPopup(Shell parent, BrowserControl browser, ReferenceChooserTree.AcceptanceListener lsnr, String titleKey, String titleVal) {
        _parent = parent;
        _browser = browser;
        _ui = browser.getUI();
        _client = browser.getClient();
        _titleKey = titleKey;
        _titleVal = titleVal;
        _lsnr = lsnr;
        initComponents();
    }
    
    public void setListener(ReferenceChooserTree.AcceptanceListener lsnr) { _lsnr = lsnr; }
    
    public void show() { _shell.open(); }
    public void hide() { _shell.setVisible(false); }
    
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
        _tree.dispose();
        _search.dispose();
        _info.dispose();
        _shell.dispose();
    }
    
    private void initComponents() {
        if (_parent == null)
            _shell = new Shell(Display.getDefault(), SWT.SHELL_TRIM | SWT.PRIMARY_MODAL);
        else
            _shell = new Shell(_parent, SWT.SHELL_TRIM | SWT.PRIMARY_MODAL);
        _shell.setLayout(new FillLayout());
        
        SashForm sash = new SashForm(_shell, SWT.HORIZONTAL);
        _tree = new ReferenceChooserTree(_browser, sash, this, this);
        
        Composite right = new Composite(sash, SWT.NONE);
        right.setLayout(new GridLayout(1, true));
        _search = new ReferenceChooserSearch(right, _tree, _browser);
        GridData gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = false;
        _search.getControl().setLayoutData(gd);
        _info = new ReferenceChooserInfo(right, _tree, this, _browser);
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        _info.getControl().setLayoutData(gd);
        
        _shell.pack();
        
        // intercept the shell closing, since that'd cause the shell to be disposed rather than just hidden
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; referenceChoiceAborted(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        _browser.getTranslationRegistry().register(this);
    }

    public void bookmarkSelected(TreeItem item, NymReferenceNode node) { _info.bookmarkSelected(item, node); }
    public void manageChannelSelected(TreeItem item, ChannelInfo channel) { _info.manageChannelSelected(item, channel); }
    public void postChannelSelected(TreeItem item, ChannelInfo channel) { _info.postChannelSelected(item, channel); }
    public void searchResultSelected(String name, ReferenceNode node) { _info.searchResultSelected(name, node); }
    public void otherSelected(TreeItem item) { _info.otherSelected(item); }

    public void referenceAccepted(SyndieURI uri) {
        _shell.setVisible(false);
        _lsnr.referenceAccepted(uri);
    }
    public void referenceChoiceAborted() {
        _shell.setVisible(false);
        _lsnr.referenceChoiceAborted();
    }
    
    private static final String T_TITLE = "syndie.gui.referencechooser.title";
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText(_titleKey, _titleVal));
    }
}
