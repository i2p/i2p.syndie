package syndie.gui;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import syndie.data.SyndieURI;
import syndie.db.SyndicationManager;

/**
 *
 */
public class SyndicationView implements Translatable {
    private BrowserControl _browser;
    private Composite _parent;
    //private Composite _root;
    private Group _archiveGroup;
    private SashForm _sash;
    private SyndicationArchiveView _archives;
    private Button _syndicate;
    private Combo _detailChooser;
    private StackLayout _stack;
    private SyndicationConfigView _config;
    private SyndicationStatusView _status;
    private SyndicationDiff _diff;
    private SyndieURI _archive;
    
    public SyndicationView(BrowserControl browser, Composite parent) { this(browser, parent, null); }
    public SyndicationView(BrowserControl browser, Composite parent, SyndieURI archive) {
        _browser = browser;
        _parent = parent;
        _archive = archive;
        browser.getSyndicationManager().loadArchives();
        initComponents();
    }
    
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
        _status.dispose();
        _archives.dispose();
        _config.dispose();
    }
    public void shown() { _archives.shown(); }
    
    private void initComponents() {
        _sash = new SashForm(_parent, SWT.VERTICAL);
        
        Composite top = new Composite(_sash, SWT.NONE);
        top.setLayout(new GridLayout(2, false));
        _archiveGroup = new Group(top, SWT.NONE);
        _archiveGroup.setLayout(new FillLayout());
        _archiveGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
        
        _archives = new SyndicationArchiveView(_browser, _archiveGroup);
        if (_archive != null)
            _archives.highlight(_archive);
        
        
        _syndicate = new Button(top, SWT.PUSH);
        _syndicate.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _syndicate.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { syndicate(); }
            public void widgetSelected(SelectionEvent selectionEvent) { syndicate(); }
        });
        
        _detailChooser = new Combo(top, SWT.DROP_DOWN | SWT.READ_ONLY);
        _detailChooser.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _detailChooser.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { choose(); }
            public void widgetSelected(SelectionEvent selectionEvent) { choose(); }
        });
        
        
        ScrolledComposite detailScroll = new ScrolledComposite(_sash, SWT.H_SCROLL | SWT.V_SCROLL);
        detailScroll.setExpandHorizontal(true);
        detailScroll.setExpandVertical(true);
        
        _stack = new StackLayout();
        Composite stacked = new Composite(detailScroll, SWT.NONE);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1);
        stacked.setLayoutData(gd);
        stacked.setLayout(_stack);
        
        _config = new SyndicationConfigView(_browser, stacked, this);
        
        _status = new SyndicationStatusView(_browser, stacked);
        
        _diff = new SyndicationDiff(_browser, stacked, this);

        _browser.getTranslationRegistry().register(this);
        // register will populate the combo via translate
        choose();
        
        detailScroll.setContent(stacked);
        detailScroll.setMinSize(stacked.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        _sash.setWeights(new int[] { 50, 50 });
    }
    
    private void syndicate() {
        int proxyPort = _config.getProxyPort();
        String proxyHost = _config.getProxyHost();
        int fcpPort = _config.getFCPPort();
        String fcpHost = _config.getFCPHost();
        _browser.getSyndicationManager().setProxies(proxyHost, proxyPort, fcpHost, fcpPort);
        Set names = new HashSet(_archives.getSelectedNames());

        Map explicit = null;
        int action = _config.getAction();
        switch (action) {
            case SyndicationConfigView.ACTION_INDEXES:
                for (Iterator iter = names.iterator(); iter.hasNext(); )
                    _browser.getSyndicationManager().fetchIndex((String)iter.next());
                break;
            case SyndicationConfigView.ACTION_PUSH_ONLY:
                _browser.getSyndicationManager().sync(_config.getMaxKB(), -1, _config.getPushStrategy(), names);
                break;
            case SyndicationConfigView.ACTION_PULL_PUSH:
                if (_config.getPullStrategy() == SyndicationManager.PULL_STRATEGY_EXPLICIT)
                    explicit = _diff.getExplicit();
                _browser.getSyndicationManager().sync(_config.getMaxKB(), _config.getPullStrategy(), _config.getPushStrategy(), names, explicit);
                break;
            case SyndicationConfigView.ACTION_PULL_ONLY:
                if (_config.getPullStrategy() == SyndicationManager.PULL_STRATEGY_EXPLICIT)
                    explicit = _diff.getExplicit();
                _browser.getSyndicationManager().sync(_config.getMaxKB(), _config.getPullStrategy(), -1, names, explicit);
                break;
        }
        int concurrency = _config.getConcurrency();
        // if enough runners are already started, no new ones are started
        _browser.getSyndicationManager().startFetching(concurrency);

        if (_detailChooser.getSelectionIndex() != CHOICE_STATUS) {
            _detailChooser.select(CHOICE_STATUS);
            choose();
        }
    }
    
    public void showDiff() {
        _detailChooser.select(CHOICE_DIFF);
        choose();
    }
    /** the explicit selection view was updated, so pick that as the pull strategy */
    public void explicitSelectionUpdated() { _config.pickExplicit(); }
    
    private static final int CHOICE_CONFIG = 0;
    private static final int CHOICE_STATUS = 1;
    private static final int CHOICE_DIFF = 2;
    
    private void choose() {
        if (_detailChooser.getSelectionIndex() == CHOICE_CONFIG) {
            _stack.topControl = _config.getControl();
        } else if (_detailChooser.getSelectionIndex() == CHOICE_DIFF) {
            _stack.topControl = _diff.getControl();
        } else {
            _stack.topControl = _status.getControl();
        }
        _config.getControl().getParent().layout();
    }
    private void populateChooser() {
        int idx = CHOICE_CONFIG;
        _detailChooser.setRedraw(false);
        if (_detailChooser.getItemCount() > 0)
            idx = _detailChooser.getItemCount();
        _detailChooser.removeAll();
        _detailChooser.add(_browser.getTranslationRegistry().getText(T_CHOICE_CONFIG, "Syndication options"));
        _detailChooser.add(_browser.getTranslationRegistry().getText(T_CHOICE_STATUS, "Syndication status"));
        _detailChooser.add(_browser.getTranslationRegistry().getText(T_CHOICE_DIFF, "Explicit message selection"));
        _detailChooser.select(idx);
        _detailChooser.setRedraw(true);
    }

    private static final String T_ARCHIVE = "syndie.gui.syndicationview.archive";
    private static final String T_SYNDICATE = "syndie.gui.syndicationview.syndicate";
    private static final String T_CHOICE_CONFIG = "syndie.gui.syndicationview.choice.config";
    private static final String T_CHOICE_STATUS = "syndie.gui.syndicationview.choice.status";
    private static final String T_CHOICE_DIFF = "syndie.gui.syndicationview.choice.diff";
    
    public void translate(TranslationRegistry registry) {
        _archiveGroup.setText(registry.getText(T_ARCHIVE, "Archives"));
        _syndicate.setText(registry.getText(T_SYNDICATE, "Syndicate"));
        populateChooser();
    }
}
