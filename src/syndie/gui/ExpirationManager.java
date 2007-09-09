package syndie.gui;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabFolder2Listener;
import org.eclipse.swt.custom.CTabFolderEvent;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import syndie.data.ExpirationPolicy;
import syndie.db.DBClient;
import syndie.db.Expirer;
import syndie.db.JobRunner;
import syndie.db.UI;

public class ExpirationManager extends BaseComponent implements Themeable, Translatable {
    private Composite _parent;
    private Composite _root;
    private CTabFolder _tabs;
    private Button _execute;
    
    private ExpirationPolicy _defaultDB;
    private ExpirationPolicy _defaultDataFile;
    private ExpirationPolicy _watchedDB;
    private ExpirationPolicy _watchedDataFile;
    private ArrayList _channelDB;
    private ArrayList _channelDataFile;
    private ArrayList _channelNames;
    
    public ExpirationManager(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite parent) {
        super(client, ui, themes, trans);
        _parent = parent;
        _channelDB = new ArrayList();
        _channelDataFile = new ArrayList();
        _channelNames = new ArrayList();
        initComponents();
    }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
    }
    
    public void setLayoutData(GridData data) { _root.setLayoutData(data); }

    public void editPolicy(Hash scope) {
        if (scope == null) {
            _tabs.setSelection(0);
        } else {
            long chanId = _client.getChannelId(scope);
            for (int i = 0; i < _channelDB.size(); i++) {
                ExpirationPolicy db = (ExpirationPolicy)_channelDB.get(i);
                if (db.getPolicyChannelId() == chanId) {
                    _tabs.setSelection(i+2);
                    return;
                }
            }
            
            // its a new one
            ExpirationPolicy db = createChannelPolicy(chanId);
            db.setIsDBPolicy();
            ExpirationPolicy dataFile = createChannelPolicy(chanId);
            dataFile.setIsDataFilePolicy();
            
            String chanName = _client.getChannelName(chanId);
            String name = chanName;
            if (name == null) {
                name = chanId + ": " + chanId;
                Hash chan = _client.getChannelHash(chanId);
                if (chan != null)
                    name = chan.toBase64().substring(0,6);
                else
                    name = chanId + "";
                chanName = name;
            }
            
            _channelDB.add(db);
            _channelDataFile.add(dataFile);
            _channelNames.add(name);
            
            buildChannelTab(db, dataFile);
            translate(_translationRegistry);
            _tabs.setSelection(_tabs.getItemCount()-1);
        }
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        _root.setLayout(gl);
        _tabs = new CTabFolder(_root, SWT.MULTI | SWT.TOP);
        _tabs.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _execute = new Button(_root, SWT.PUSH);
        _execute.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _execute.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                _execute.setEnabled(false);
                JobRunner.instance().enqueue(new Runnable() {
                    public void run() {
                        Expirer expirer = new Expirer(_client, _ui);
                        expirer.expireMessages();
                        _root.getDisplay().asyncExec(new Runnable() { public void run() { _execute.setEnabled(true); } });
                    }
                });
            }
        });
        
        loadPolicies();
        _ui.debugMessage("policies loaded");
        buildDefaultTab();
        _ui.debugMessage("default tab built");
        buildWatchedTab();
        _ui.debugMessage("watched tab built");
        for (int i = 0; i < _channelDB.size(); i++) {
            buildChannelTab((ExpirationPolicy)_channelDB.get(i), (ExpirationPolicy)_channelDataFile.get(i));
            _ui.debugMessage("channel[" + i + "] tab built");
        }
        
        _ui.debugMessage("default tab: " + _tabs.getItem(0));
        _ui.debugMessage("watched tab: " + _tabs.getItem(1));
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    private void buildDefaultTab() { buildTab(_defaultDB, _defaultDataFile); }
    private void buildWatchedTab() { buildTab(_watchedDB, _watchedDataFile); }
    private void buildChannelTab(ExpirationPolicy db, ExpirationPolicy dataFile) { buildTab(db, dataFile); }
    private void buildTab(ExpirationPolicy db, ExpirationPolicy dataFile) {
        CTabItem tab = new CTabItem(_tabs, SWT.NONE);
        Composite root = new Composite(_tabs, SWT.NONE);
        root.setLayout(new FillLayout());
        new PolicyGroup(tab, root, db, dataFile);
        tab.setControl(root);
    }
    
    private void loadPolicies() {
        Set policies = _client.getExpirationPolicies();
        
        HashSet channelDBPolicies = new HashSet();
        HashSet channelDataFilePolicies = new HashSet();
        
        for (Iterator iter = policies.iterator(); iter.hasNext(); ) {
            ExpirationPolicy policy = (ExpirationPolicy)iter.next();
            if (policy.isDefaultPolicy()) {
                if (policy.isDBPolicy())
                    _defaultDB = policy;
                else
                    _defaultDataFile = policy;
            } else if (policy.isWatchedPolicy()) {
                if (policy.isDBPolicy())
                    _watchedDB = policy;
                else
                    _watchedDataFile = policy;
            } else {
                if (policy.isDBPolicy())
                    channelDBPolicies.add(policy);
                else
                    channelDataFilePolicies.add(policy);
            }
        }
        
        sortChannelPolicies(channelDBPolicies, channelDataFilePolicies);
        
        if (_defaultDB == null) {
            _defaultDB = createDefaultPolicy();
            _defaultDB.setIsDBPolicy();
        }
        if (_defaultDataFile == null) {
            _defaultDataFile = createDefaultPolicy();
            _defaultDataFile.setIsDataFilePolicy();
        }
        
        if (_watchedDB == null) {
            _watchedDB = createWatchedPolicy();
            _watchedDB.setIsDBPolicy();
            _watchedDB.setMimicDefault(true);
        }
        if (_watchedDataFile == null) {
            _watchedDataFile = createWatchedPolicy();
            _watchedDataFile.setIsDataFilePolicy();
            _watchedDataFile.setMimicDefault(true);
        }
    }
    
    private ExpirationPolicy createDefaultPolicy() {
        ExpirationPolicy policy = new ExpirationPolicy();
        policy.setIsDefaultPolicy();
        policy.setMaxAgeDays(180);
        policy.setMaxNumMessages(-1);
        policy.setMaxSizeKB(-1);
        policy.setIsNew(true);
        return policy;
    }
    private ExpirationPolicy createWatchedPolicy() {
        ExpirationPolicy policy = new ExpirationPolicy();
        policy.setIsWatchedPolicy();
        policy.setMaxAgeDays(-1);
        policy.setMaxNumMessages(-1);
        policy.setMaxSizeKB(-1);
        policy.setIsNew(true);
        return policy;
    }
    private ExpirationPolicy createChannelPolicy(long chanId) {
        ExpirationPolicy policy = new ExpirationPolicy();
        policy.setPolicyChannelId(chanId);
        policy.setMaxAgeDays(-1);
        policy.setMaxNumMessages(-1);
        policy.setMaxSizeKB(-1);
        policy.setIsNew(true);
        policy.setMimicDefault(true);
        return policy;
    }
    
    private void sortChannelPolicies(Set dbPolicies, Set dataFilePolicies) {
        if (dbPolicies.size() != dataFilePolicies.size()) {
            _ui.errorMessage("channel policy inconsistency: dbPolicies: " + dbPolicies.size() + " dataFile: " + dataFilePolicies.size());
        }
        TreeMap nameToScopeId = new TreeMap();
        TreeMap scopeIdToName = new TreeMap();
        for (Iterator iter = dbPolicies.iterator(); iter.hasNext(); ) {
            ExpirationPolicy policy = (ExpirationPolicy)iter.next();
            long chanId = policy.getPolicyChannelId();
            String chanName = _client.getChannelName(chanId);
            String name = chanName;
            if (name == null) {
                name = chanId + ": " + chanId;
                Hash chan = _client.getChannelHash(chanId);
                if (chan != null)
                    name = chan.toBase64().substring(0,6);
                else
                    name = chanId + "";
                chanName = name;
            }
            
            name = name + ": " + chanId;
            nameToScopeId.put(name, new Long(chanId));
            scopeIdToName.put(new Long(chanId), chanName);
        }
        
        for (Iterator iter = nameToScopeId.values().iterator(); iter.hasNext(); ) {
            Long scopeId = (Long)iter.next();
            ExpirationPolicy db = getPolicy(scopeId.longValue(), dbPolicies);
            ExpirationPolicy dataFile = getPolicy(scopeId.longValue(), dataFilePolicies);
            if ( (db != null) && (dataFile != null) ) {
                _channelDB.add(db);
                _channelDataFile.add(dataFile);
                _channelNames.add(scopeIdToName.get(scopeId));
            }
        }
    }
    
    private ExpirationPolicy getPolicy(long scopeId, Set policies) {
        for (Iterator iter = policies.iterator(); iter.hasNext(); ) {
            ExpirationPolicy policy = (ExpirationPolicy)iter.next();
            if (policy.getPolicyChannelId() == scopeId)
                return policy;
        }
        return null;
    }
    
    public void applyTheme(Theme theme) {
        _tabs.setFont(theme.TAB_FONT);
        _execute.setFont(theme.BUTTON_FONT);
    }
    public void translate(TranslationRegistry registry) {
        _execute.setText(registry.getText(T_EXECUTE, "Execute expiration policies"));
        
        CTabItem items[] = _tabs.getItems();
        for (int i = 0; i < items.length; i++) {
            switch (i) {
                case 0:
                    items[i].setText(registry.getText(T_TABTITLE_DEFAULT, "Default policy"));
                    break;
                case 1:
                    items[i].setText(registry.getText(T_TABTITLE_WATCHED, "Watched forums"));
                    break;
                default:
                    String name = (String)_channelNames.get(i-2);
                    items[i].setText(name);
                    break;
            }
        }
    }
    
    private class PolicyGroup {
        private CTabItem _tab;
        private Composite _tabRoot;
        private Composite _detailRoot;
        private PolicyDetail _db;
        private PolicyDetail _dataFile;
        
        private Button _save;
        private Button _revert;
        private Button _delete;
        
        public PolicyGroup(CTabItem tab, Composite root, ExpirationPolicy db, ExpirationPolicy dataFile) {
            _tab = tab;
            _tabRoot = root;
            initGroupComponents(db, dataFile);
        }
        private void initGroupComponents(ExpirationPolicy db, ExpirationPolicy dataFile) {
            _detailRoot = new Composite(_tabRoot, SWT.NONE);
            GridLayout gl = new GridLayout(2, true);
            gl.horizontalSpacing = 0;
            gl.verticalSpacing = 0;
            gl.marginHeight = 0;
            gl.marginWidth = 0;
            _detailRoot.setLayout(gl);
            //_detailRoot.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));foo
            _db = new PolicyDetail(_detailRoot, db, PolicyGroup.this);
            _db.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
            _dataFile = new PolicyDetail(_detailRoot, dataFile, PolicyGroup.this);
            _dataFile.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
            
            Composite buttons = new Composite(_detailRoot, SWT.NONE);
            buttons.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false, 2, 1));
            buttons.setLayout(new FillLayout(SWT.HORIZONTAL));
            _save = new Button(buttons, SWT.PUSH);
            _save.addSelectionListener(new FireSelectionListener() { public void fire() { saveChanges(); } });
            _revert = new Button(buttons, SWT.PUSH);
            _save.addSelectionListener(new FireSelectionListener() { public void fire() { revertChanges(); } });
            
            _save.setText(_translationRegistry.getText(T_SAVE, "Save changes"));
            _revert.setText(_translationRegistry.getText(T_REVERT, "Revert changes"));
            
            _save.setFont(_themeRegistry.getTheme().BUTTON_FONT);
            _revert.setFont(_themeRegistry.getTheme().BUTTON_FONT);
            
            boolean isNew = db.getIsNew() || dataFile.getIsNew();
            _save.setEnabled(isNew);
            _revert.setEnabled(isNew);
            
            if (db.isDefaultPolicy() || db.isWatchedPolicy()) {
                // standard policies, not deletable
            } else {
                _delete = new Button(buttons, SWT.PUSH);
                _delete.setText(_translationRegistry.getText(T_DELETE, "Delete policy"));
                _delete.setFont(_themeRegistry.getTheme().BUTTON_FONT);
                _delete.addSelectionListener(new FireSelectionListener() { public void fire() { deletePolicy(); } });
            }
        }
        
        void changeMade() {
            _save.setEnabled(true);
            _revert.setEnabled(true);
        }
        
        private void saveChanges() {
            _dataFile.save();
            _db.save();
            _save.setEnabled(false);
            _revert.setEnabled(false);
        }
        private void revertChanges() {
            _dataFile.revert();
            _db.revert();
            _save.setEnabled(false);
            _revert.setEnabled(false);
        }
        private void deletePolicy() {
            _dataFile.delete();
            _db.delete();
            _tabRoot.dispose();
            _tab.dispose();
        }
    }
  
    private class PolicyDetail {
        private PolicyGroup _group;
        private Composite _groupRoot;
        private Composite _detailRoot;
        private ExpirationPolicy _policy;
        private ExpirationPolicy _policyOrig;
        
        private Label _type;
        
        private Button _mimicDefault;
        
        private Button _maxNumMsgsEnable;
        private Label _maxNumMsgsLabel;
        private Text _maxNumMsgsVal;
        
        private Button _maxSizeKBEnable;
        private Label _maxSizeKBLabel;
        private Text _maxSizeKBVal;
        
        private Button _maxAgeDaysEnable;
        private Label _maxAgeDaysLabel;
        private Text _maxAgeDaysVal;
        
        public PolicyDetail(Composite root, ExpirationPolicy policy, PolicyGroup group) {
            _group = group;
            _groupRoot = root;
            _policy = policy;
            _policyOrig = new ExpirationPolicy(policy);
            initDetailComponents();
        }
        
        void save() {
            _client.saveExpirationPolicy(_policy);
            _policy.setIsNew(false);
            if (_policy.getMimicDefault()) {
                _policy.setMaxAgeDays(-1);
                _policy.setMaxNumMessages(-1);
                _policy.setMaxSizeKB(-1);
            }
            _policyOrig.load(_policy);
            render();
        }
        void revert() {
            _policy.load(_policyOrig);
            render();
        }
        void delete() {
            _client.deleteExpirationPolicy(_policy);
        }
        
        public void setLayoutData(GridData data) { _detailRoot.setLayoutData(data); }
        private void initDetailComponents() {
            _detailRoot = new Composite(_groupRoot, SWT.NONE);
            GridLayout gl = new GridLayout(1, true);
            gl.marginWidth = 0;
            gl.marginHeight = 0;
            gl.verticalSpacing = 0;
            gl.horizontalSpacing = 0;
            _detailRoot.setLayout(gl);

            _type = new Label(_detailRoot, SWT.NONE);
            _type.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
            _type.setLayoutData(new GridData(GridData.CENTER, GridData.FILL, true, false));
            
            Composite attributeRoot = _detailRoot;
            if (_policy.isDefaultPolicy()) {
                // no "mimic default" row
            } else {
                _mimicDefault = new Button(_detailRoot, SWT.CHECK);
                _mimicDefault.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
                _mimicDefault.setSelection(_policy.getMimicDefault());
                _mimicDefault.setText(_translationRegistry.getText(T_MIMICDEFAULT, "Same as the default policy"));
                _mimicDefault.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
                attributeRoot = new Composite(_detailRoot, SWT.NONE);
                attributeRoot.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
                gl = new GridLayout(1, true);
                gl.marginWidth = 0;
                gl.marginHeight = 0;
                gl.verticalSpacing = 0;
                gl.horizontalSpacing = 0;
                attributeRoot.setLayout(gl);
                final Composite subRoot = attributeRoot;
                _mimicDefault.addSelectionListener(new FireSelectionListener() {
                    public void fire() {
                        setMimic(_mimicDefault.getSelection());
                        _group.changeMade();
                    }
                });
            }
            
            initMaxNumMsgs(attributeRoot);
            initMaxSizeKB(attributeRoot);
            initMaxAgeDays(attributeRoot);
            
            setMimic(_policy.getMimicDefault());
            
            render();
            
            _ui.debugMessage("policy detail created for " + _policy);
        }
        
        private void render() {
            if (_policy.isDBPolicy())
                _type.setText(_translationRegistry.getText(T_TYPEDB, "Internal message data"));
            else
                _type.setText(_translationRegistry.getText(T_TYPEDATAFILE, "Sharable external message data"));
            
            if (_policy.getMaxNumMessages() > 0) {
                _maxNumMsgsVal.setText(_policy.getMaxNumMessages() + "");
                _maxNumMsgsEnable.setSelection(true);
                _maxNumMsgsLabel.setEnabled(true);
                _maxNumMsgsVal.setEnabled(true);
            } else {
                _maxNumMsgsVal.setText(0 + "");
                _maxNumMsgsEnable.setSelection(false);
                _maxNumMsgsLabel.setEnabled(false);
                _maxNumMsgsVal.setEnabled(false);
            }
        
            if (_policy.getMaxSizeKB() > 0) {
                _maxSizeKBVal.setText(_policy.getMaxSizeKB() + "");
                _maxSizeKBEnable.setSelection(true);
                _maxSizeKBLabel.setEnabled(true);
                _maxSizeKBVal.setEnabled(true);
            } else {
                _maxSizeKBVal.setText(0 + "");
                _maxSizeKBEnable.setSelection(false);
                _maxSizeKBLabel.setEnabled(false);
                _maxSizeKBVal.setEnabled(false);
            }
            
            if (_policy.getMaxAgeDays() > 0) {
                _maxAgeDaysVal.setText(_policy.getMaxAgeDays() + "");
                _maxAgeDaysEnable.setSelection(true);
                _maxAgeDaysLabel.setEnabled(true);
                _maxAgeDaysVal.setEnabled(true);
            } else {
                _maxAgeDaysVal.setText(0 + "");
                _maxAgeDaysEnable.setSelection(false);
                _maxAgeDaysLabel.setEnabled(false);
                _maxAgeDaysVal.setEnabled(false);
            }
            
            renderMimic(_policy.getMimicDefault());
        }
        
        private void renderMimic(boolean mimic) {
            _maxNumMsgsEnable.setEnabled(!mimic);
            _maxNumMsgsLabel.setEnabled(!mimic && _maxNumMsgsEnable.getSelection());
            _maxNumMsgsVal.setEnabled(!mimic && _maxNumMsgsEnable.getSelection());

            _maxSizeKBEnable.setEnabled(!mimic);
            _maxSizeKBLabel.setEnabled(!mimic && _maxSizeKBEnable.getSelection());
            _maxSizeKBVal.setEnabled(!mimic && _maxSizeKBEnable.getSelection());

            _maxAgeDaysEnable.setEnabled(!mimic);
            _maxAgeDaysLabel.setEnabled(!mimic && _maxAgeDaysEnable.getSelection());
            _maxAgeDaysVal.setEnabled(!mimic && _maxAgeDaysEnable.getSelection());
        }
            
        private void setMimic(boolean mimic) {
            _policy.setMimicDefault(mimic);

            renderMimic(mimic);
            
            if ( (_mimicDefault != null) && (mimic != _mimicDefault.getSelection()) )
                _mimicDefault.setSelection(mimic);
        }
        
        private void initMaxNumMsgs(Composite attributeRoot) {
            Composite row = new Composite(attributeRoot, SWT.NONE);
            row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            
            _maxNumMsgsEnable = new Button(row, SWT.CHECK);
            _maxNumMsgsLabel = new Label(row, SWT.NONE);
            _maxNumMsgsLabel.setText(_translationRegistry.getText(T_MAXNUMMSGS_LABEL, "Max number of messages: "));
            _maxNumMsgsVal = new Text(row, SWT.SINGLE | SWT.BORDER);
            _maxNumMsgsVal.setTextLimit(7);
            
            GridLayout gl = new GridLayout(3, false);
            gl.horizontalSpacing = 0;
            gl.verticalSpacing = 0;
            gl.marginHeight = 0;
            gl.marginWidth = 0;
            row.setLayout(gl);
            
            _maxNumMsgsEnable.setLayoutData(new GridData(GridData.BEGINNING, GridData.FILL, false, false));
            _maxNumMsgsLabel.setLayoutData(new GridData(GridData.END, GridData.FILL, false, false));
            GridData gd = new GridData(GridData.FILL, GridData.FILL, true, true);
            gd.widthHint = 100;
            _maxNumMsgsVal.setLayoutData(gd);
            
            _maxNumMsgsEnable.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    boolean enabled = _maxNumMsgsEnable.getSelection();
                    _maxNumMsgsLabel.setEnabled(enabled);
                    _maxNumMsgsVal.setEnabled(enabled);
                    if (enabled)
                        _policy.setMaxNumMessages(getLong(_maxNumMsgsVal.getText(), 0));
                    else
                        _policy.setMaxNumMessages(-1);
                    setMimic(false);
                    _group.changeMade();
                }
            });
            _maxNumMsgsVal.addFocusListener(new FocusListener() {
                public void focusGained(FocusEvent focusEvent) {}
                public void focusLost(FocusEvent focusEvent) {
                    if (_maxNumMsgsEnable.getSelection()) {
                        _policy.setMaxNumMessages(getLong(_maxNumMsgsVal.getText(), 0));
                        setMimic(false);
                        _group.changeMade();
                    }
                }
            });
            
            _maxNumMsgsEnable.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
            _maxNumMsgsLabel.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
            _maxNumMsgsVal.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        }
        
        private void initMaxSizeKB(Composite attributeRoot) {
            Composite row = new Composite(attributeRoot, SWT.NONE);
            row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            _maxSizeKBEnable = new Button(row, SWT.CHECK);
            _maxSizeKBLabel = new Label(row, SWT.NONE);
            _maxSizeKBLabel.setText(_translationRegistry.getText(T_MAXSIZEKB_LABEL, "Max total size (KBytes): "));
            _maxSizeKBVal  = new Text(row, SWT.SINGLE | SWT.BORDER);
            _maxSizeKBVal.setTextLimit(7);

            GridLayout gl = new GridLayout(3, false);
            gl.horizontalSpacing = 0;
            gl.verticalSpacing = 0;
            gl.marginHeight = 0;
            gl.marginWidth = 0;
            row.setLayout(gl);
            
            _maxSizeKBEnable.setLayoutData(new GridData(GridData.BEGINNING, GridData.FILL, false, false));
            _maxSizeKBLabel.setLayoutData(new GridData(GridData.END, GridData.FILL, false, false));
            GridData gd = new GridData(GridData.FILL, GridData.FILL, true, true);
            gd.widthHint = 100;
            _maxSizeKBVal.setLayoutData(gd);
            
            _maxSizeKBEnable.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    boolean enabled = _maxSizeKBEnable.getSelection();
                    _maxSizeKBLabel.setEnabled(enabled);
                    _maxSizeKBVal.setEnabled(enabled);
                    if (enabled)
                        _policy.setMaxSizeKB((int)getLong(_maxSizeKBVal.getText(), 0));
                    else
                        _policy.setMaxSizeKB(-1);
                    setMimic(false);
                    _group.changeMade();
                }
            });
            _maxSizeKBVal.addFocusListener(new FocusListener() {
                public void focusGained(FocusEvent focusEvent) {}
                public void focusLost(FocusEvent focusEvent) {
                    if (_maxSizeKBEnable.getSelection()) {
                        _policy.setMaxSizeKB((int)getLong(_maxSizeKBVal.getText(), 0));
                        setMimic(false);
                        _group.changeMade();
                    }
                }
            });
            
            _maxSizeKBEnable.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
            _maxSizeKBLabel.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
            _maxSizeKBVal.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        }
        
        private void initMaxAgeDays(Composite attributeRoot) {
            Composite row = new Composite(attributeRoot, SWT.NONE);
            row.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            _maxAgeDaysEnable = new Button(row, SWT.CHECK);
            _maxAgeDaysLabel = new Label(row, SWT.NONE);
            _maxAgeDaysLabel.setText(_translationRegistry.getText(T_MAXAGEDAYS_LABEL, "Max age (days): "));
            _maxAgeDaysVal = new Text(row, SWT.SINGLE | SWT.BORDER);
            _maxAgeDaysVal.setTextLimit(4);
            
            GridLayout gl = new GridLayout(3, false);
            gl.horizontalSpacing = 0;
            gl.verticalSpacing = 0;
            gl.marginHeight = 0;
            gl.marginWidth = 0;
            row.setLayout(gl);
            
            _maxAgeDaysEnable.setLayoutData(new GridData(GridData.BEGINNING, GridData.FILL, false, false));
            _maxAgeDaysLabel.setLayoutData(new GridData(GridData.END, GridData.FILL, false, false));
            GridData gd = new GridData(GridData.FILL, GridData.FILL, true, true);
            gd.widthHint = 100;
            _maxAgeDaysVal.setLayoutData(gd);
            
            _maxAgeDaysEnable.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    boolean enabled = _maxAgeDaysEnable.getSelection();
                    _maxAgeDaysLabel.setEnabled(enabled);
                    _maxAgeDaysVal.setEnabled(enabled);
                    if (enabled)
                        _policy.setMaxAgeDays((int)getLong(_maxAgeDaysVal.getText(), 0));
                    else
                        _policy.setMaxAgeDays(-1);
                    setMimic(false);
                    _group.changeMade();
                }
            });
            _maxAgeDaysVal.addFocusListener(new FocusListener() {
                public void focusGained(FocusEvent focusEvent) {}
                public void focusLost(FocusEvent focusEvent) {
                    if (_maxAgeDaysEnable.getSelection()) {
                        _policy.setMaxAgeDays((int)getLong(_maxAgeDaysVal.getText(), 0));
                        setMimic(false);
                        _group.changeMade();
                    }
                }
            });
            
            _maxAgeDaysEnable.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
            _maxAgeDaysLabel.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
            _maxAgeDaysVal.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        }
    }
    
    private static final long getLong(String str, long def) {
        if (str == null) return def;
        try { 
            long val = Long.parseLong(str);
            return val;
        } catch (NumberFormatException nfe) {
            return def;
        }
    }
    
    private static final String T_SAVE = "syndie.gui.expirationmanager.save";
    private static final String T_REVERT = "syndie.gui.expirationmanager.revert";
    private static final String T_DELETE = "syndie.gui.expirationmanager.delete";
    private static final String T_EXECUTE = "syndie.gui.expirationmanager.execute";
    private static final String T_MIMICDEFAULT = "syndie.gui.expirationmanager.mimicdefault";
    private static final String T_TYPEDB = "syndie.gui.expirationmanager.typedb";
    private static final String T_TYPEDATAFILE = "syndie.gui.expirationmanager.typedatafile";
    private static final String T_MAXNUMMSGS_LABEL = "syndie.gui.expirationmanager.maxnummsgs.label";
    private static final String T_MAXSIZEKB_LABEL = "syndie.gui.expirationmanager.maxsizekb.label";
    private static final String T_MAXAGEDAYS_LABEL = "syndie.gui.expirationmanager.maxagedays.label";
    private static final String T_TABTITLE_DEFAULT = "syndie.gui.expirationmanager.tabtitle.default";
    private static final String T_TABTITLE_WATCHED = "syndie.gui.expirationmanager.tabtitle.watched";
}
