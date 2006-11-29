package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import syndie.db.ArchiveIndex;
import syndie.db.SyndicationManager;

/**
 *
 */
public class SyndicationConfigView implements Translatable, SyndicationManager.SyndicationListener {
    private BrowserControl _browser;
    private Composite _parent;
    private Composite _root;
    private Label _sizeLabel;
    private Spinner _size;
    private Label _strategyLabel;
    private Combo _strategy;
    private Label _concurrencyLabel;
    private Spinner _concurrency;
    private Button _execute;
    
    public SyndicationConfigView(BrowserControl browser, Composite parent) {
        _browser = browser;
        _parent = parent;
        initComponents();
        _browser.getSyndicationManager().addListener(this);
    }
    
    public Control getControl() { return _root; }
    
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
    }
    
    public int getProxyPort() { return -1; }
    public String getProxyHost() { return null; }
    
    public int getFCPPort() { return -1; }
    public String getFCPHost() { return null; }
    
    public static final int ACTION_INDEXES = 0;
    public static final int ACTION_PULL_PUSH = 1;
    public static final int ACTION_PULL_ONLY = 2;
    
    public int getAction() { return ACTION_INDEXES; }
    public int getConcurrency() { return 1; }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(7, false));
        
        _execute = new Button(_root, SWT.PUSH);
        _execute.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _execute.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { sync(); }
            public void widgetSelected(SelectionEvent selectionEvent) { sync(); }
        });
        _concurrencyLabel = new Label(_root, SWT.NONE);
        _concurrencyLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _concurrency = new Spinner(_root, SWT.READ_ONLY | SWT.BORDER);
        _concurrency.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _concurrency.setIncrement(1);
        _concurrency.setDigits(0);
        _concurrency.setMinimum(1);
        _concurrency.setMaximum(5);
        
        _sizeLabel = new Label(_root, SWT.NONE);
        _sizeLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _size = new Spinner(_root, SWT.BORDER);
        _size.setIncrement(1);
        _size.setDigits(0);
        _size.setMinimum(4);
        _size.setSelection((int)(ArchiveIndex.DEFAULT_MAX_SIZE/1024));
        _size.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _strategyLabel = new Label(_root, SWT.NONE);
        _strategyLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _strategy = new Combo(_root, SWT.DROP_DOWN | SWT.READ_ONLY);
        _strategy.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _strategy.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { strategyUpdated(); }
            public void widgetSelected(SelectionEvent selectionEvent) { strategyUpdated(); }
        });
        
        _browser.getTranslationRegistry().register(this);
    }
    
    private void sync() {
        _browser.getSyndicationManager().startFetching(_concurrency.getSelection());
        _browser.getSyndicationManager().sync(_size.getSelection(), _strategy.getSelectionIndex(), true);
    }
    
    private void updateStrategy(TranslationRegistry registry) {
        int idx = SyndicationManager.STRATEGY_DEFAULT;
        if (_strategy.getItemCount() > 0)
            idx = _strategy.getSelectionIndex();
        _strategy.setRedraw(false);
        _strategy.removeAll();
        // tied to the SyndicationManager.STRATEGY_* values
        _strategy.add(registry.getText(T_STRATEGY_DELTA, "All differences"));
        _strategy.add(registry.getText(T_STRATEGY_DELTAKNOWN, "Differences in locally known forums"));
        _strategy.add(registry.getText(T_STRATEGY_PIR, "PIR (wastes bandwidth, more anonymity)"));
        _strategy.select(idx);
        _strategy.setRedraw(true);
    }
    
    private void strategyUpdated() {
        // update the syndicationManager to use a different strategy
    }
    
    private static final String T_STRATEGY_DELTA = "syndie.gui.syndicationpendingview.strategy.delta";
    private static final String T_STRATEGY_DELTAKNOWN = "syndie.gui.syndicationpendingview.strategy.deltaknown";
    private static final String T_STRATEGY_PIR = "syndie.gui.syndicationpendingview.strategy.pir";
    
    private static final String T_SIZE = "syndie.gui.syndicationpendingview.size";
    private static final String T_SIZE_TOOLTIP = "syndie.gui.syndicationpendingview.size_tooltip";
    private static final String T_STRATEGY = "syndie.gui.syndicationpendingview.strategy";
    private static final String T_CONCURRENCY = "syndie.gui.syndicationpendingview.concurrency";
    private static final String T_CONCURRENCY_TOOLTIP = "syndie.gui.syndicationpendingview.concurrency_tooltip";
    private static final String T_EXECUTE = "syndie.gui.syndicationpendingview.execute";

    public void translate(TranslationRegistry registry) {
        _sizeLabel.setText(registry.getText(T_SIZE, "Max size: "));
        _size.setToolTipText(registry.getText(T_SIZE_TOOLTIP, "kilobytes"));
        
        _strategyLabel.setText(registry.getText(T_STRATEGY, "Pull strategy: "));
        updateStrategy(registry);
        
        _concurrencyLabel.setText(registry.getText(T_CONCURRENCY, "Concurrency: "));
        _concurrency.setToolTipText(registry.getText(T_CONCURRENCY_TOOLTIP, "Number of locations fetched at a time"));
        
        _execute.setText(registry.getText(T_EXECUTE, "Execute"));
    }

    public void archiveAdded(SyndicationManager mgr, String name) {}
    public void archiveRemoved(SyndicationManager mgr, String name) {}
    public void archiveUpdated(SyndicationManager mgr, String oldName, String newName) {}
    public void archiveIndexStatus(SyndicationManager mgr, SyndicationManager.StatusRecord record) {
        if (record.getStatus() == SyndicationManager.FETCH_INDEX_DIFF_OK) {
            // update summary
        }
    }

    public void fetchStatusUpdated(SyndicationManager mgr, SyndicationManager.StatusRecord record) {}
}
