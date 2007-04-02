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
import syndie.data.WatchedChannel;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 * the old tree-based reference chooser
 */
class ReferenceChooserPopupImpl extends BaseComponent implements ReferenceChooserPopup, ReferenceChooserTree.ChoiceListener, ReferenceChooserTree.AcceptanceListener, Translatable {
    private Shell _parent;
    private NavigationControl _navControl;
    private URIControl _uriControl;
    private Shell _shell;
    private ReferenceChooserTree.AcceptanceListener _lsnr;
    private ReferenceChooserTree _tree;
    private ReferenceChooserSearch _search;
    private ReferenceChooserInfo _info;
    private String _titleKey;
    private String _titleVal;
    
    public ReferenceChooserPopupImpl(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Shell parent, NavigationControl navControl, URIControl uriControl, String titleKey, String titleVal) { this(client, ui, themes, trans, parent, navControl, uriControl, null, titleKey, titleVal); }
    public ReferenceChooserPopupImpl(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Shell parent, NavigationControl navControl, URIControl uriControl) { this(client, ui, themes, trans, parent, navControl, uriControl, null); }
    public ReferenceChooserPopupImpl(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Shell parent, NavigationControl navControl, URIControl uriControl, ReferenceChooserTree.AcceptanceListener lsnr) {
        this(client, ui, themes, trans, parent, navControl, uriControl, lsnr, T_TITLE, "Reference chooser");
    }
    public ReferenceChooserPopupImpl(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Shell parent, NavigationControl navControl, URIControl uriControl, ReferenceChooserTree.AcceptanceListener lsnr, String titleKey, String titleVal) {
        super(client, ui, themes, trans);
        _parent = parent;
        _navControl = navControl;
        _uriControl = uriControl;
        _titleKey = titleKey;
        _titleVal = titleVal;
        _lsnr = lsnr;
        initComponents();
    }
    
    public void setListener(ReferenceChooserTree.AcceptanceListener lsnr) { _lsnr = lsnr; }
    
    public void show() { _shell.open(); }
    public void hide() { _shell.setVisible(false); }
    
    public void dispose() {
        _translationRegistry.unregister(this);
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
        _tree = new ReferenceChooserTree(_client, _ui, _themeRegistry, _translationRegistry, _navControl, _uriControl, sash, this, this);
        
        Composite right = new Composite(sash, SWT.NONE);
        right.setLayout(new GridLayout(1, true));
        _search = new ReferenceChooserSearch(_client, _ui, _themeRegistry, _translationRegistry, right, _tree);
        GridData gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = false;
        _search.getControl().setLayoutData(gd);
        _info = new ReferenceChooserInfo(_client, _ui, _themeRegistry, _translationRegistry, right, _tree, this);
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
        _translationRegistry.register(this);
    }

    
    public void watchedChannelSelected(TreeItem item, WatchedChannel channel) { _info.watchedChannelSelected(item, channel); }
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
