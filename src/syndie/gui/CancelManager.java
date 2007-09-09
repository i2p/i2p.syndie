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
import syndie.data.CancelPolicy;
import syndie.db.DBClient;
import syndie.db.Expirer;
import syndie.db.JobRunner;
import syndie.db.UI;

public class CancelManager extends BaseComponent implements Themeable, Translatable {
    private Composite _parent;
    private Composite _root;
    private CTabFolder _tabs;
    
    private CancelPolicy _defaultPolicy;
    private CancelPolicy _managedPolicy;
    private ArrayList _channelPolicies;
    private ArrayList _channelNames;
    
    public CancelManager(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite parent) {
        super(client, ui, themes, trans);
        _parent = parent;
        _channelPolicies = new ArrayList();
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
            for (int i = 0; i < _channelPolicies.size(); i++) {
                CancelPolicy policy = (CancelPolicy)_channelPolicies.get(i);
                if (policy.getScopeApplyToChannelId() == chanId) {
                    _tabs.setSelection(i+2);
                    return;
                }
            }
            
            // its a new one
            CancelPolicy policy = createChannelPolicy(chanId);
            
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
            
            _channelPolicies.add(policy);
            _channelNames.add(name);
            
            buildChannelTab(policy);
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
        
        loadPolicies();
        _ui.debugMessage("policies loaded");
        buildDefaultTab();
        _ui.debugMessage("default tab built");
        buildManagedTab();
        _ui.debugMessage("managed tab built");
        for (int i = 0; i < _channelPolicies.size(); i++) {
            buildChannelTab((CancelPolicy)_channelPolicies.get(i));
            _ui.debugMessage("channel[" + i + "] tab built");
        }
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    private void buildDefaultTab() { buildTab(_defaultPolicy); }
    private void buildManagedTab() { buildTab(_managedPolicy); }
    private void buildChannelTab(CancelPolicy policy) { buildTab(policy); }
    private void buildTab(CancelPolicy policy) {
        CTabItem tab = new CTabItem(_tabs, SWT.NONE);
        Composite root = new Composite(_tabs, SWT.NONE);
        root.setLayout(new FillLayout());
        new PolicyGroup(tab, root, policy);
        tab.setControl(root);
    }
    
    private void loadPolicies() {
        Set policies = _client.getCancelPolicies();
        
        HashSet channelPolicies = new HashSet();
        
        for (Iterator iter = policies.iterator(); iter.hasNext(); ) {
            CancelPolicy policy = (CancelPolicy)iter.next();
            if (policy.getScopeApplyToAll())
                _defaultPolicy = policy;
            else if (policy.getScopeApplyToLocallyManaged())
                _managedPolicy = policy;
            else
                channelPolicies.add(policy);
        }
        
        sortChannelPolicies(channelPolicies);
        
        if (_defaultPolicy == null)
            _defaultPolicy = createDefaultPolicy();
        
        if (_managedPolicy == null)
            _managedPolicy = createManagedPolicy();
    }
    
    private CancelPolicy createDefaultPolicy() {
        CancelPolicy policy = new CancelPolicy(true);
        policy.setHonorFromAuthor(true);
        policy.setHonorFromForumOwner(true);
        policy.setHonorFromForumManager(true);
        policy.setHonorFromForumAuthorizedPoster(false);
        policy.setIsNew(true);
        return policy;
    }
    private CancelPolicy createManagedPolicy() {
        CancelPolicy policy = new CancelPolicy(false);
        policy.setHonorFromAuthor(true);
        policy.setHonorFromForumOwner(true);
        policy.setHonorFromForumManager(true);
        policy.setHonorFromForumAuthorizedPoster(false);
        policy.setIsNew(true);
        return policy;
    }
    private CancelPolicy createChannelPolicy(long chanId) {
        CancelPolicy policy = new CancelPolicy(chanId);
        policy.setHonorFromAuthor(true);
        policy.setHonorFromForumOwner(true);
        policy.setHonorFromForumManager(true);
        policy.setHonorFromForumAuthorizedPoster(false);
        policy.setIsNew(true);
        return policy;
    }
    
    private void sortChannelPolicies(Set policies) {
        TreeMap nameToScopeId = new TreeMap();
        TreeMap scopeIdToName = new TreeMap();
        for (Iterator iter = policies.iterator(); iter.hasNext(); ) {
            CancelPolicy policy = (CancelPolicy)iter.next();
            long chanId = policy.getScopeApplyToChannelId();
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
            CancelPolicy policy = getPolicy(scopeId.longValue(), policies);
            if (policy != null) {
                _channelPolicies.add(policy);
                _channelNames.add(scopeIdToName.get(scopeId));
            }
        }
    }
    
    private CancelPolicy getPolicy(long scopeId, Set policies) {
        for (Iterator iter = policies.iterator(); iter.hasNext(); ) {
            CancelPolicy policy = (CancelPolicy)iter.next();
            if (policy.getScopeApplyToChannelId() == scopeId)
                return policy;
        }
        return null;
    }
    
    public void applyTheme(Theme theme) {
        _tabs.setFont(theme.TAB_FONT);
    }
    public void translate(TranslationRegistry registry) {
        CTabItem items[] = _tabs.getItems();
        for (int i = 0; i < items.length; i++) {
            switch (i) {
                case 0:
                    items[i].setText(registry.getText(T_TABTITLE_DEFAULT, "Default policy"));
                    break;
                case 1:
                    items[i].setText(registry.getText(T_TABTITLE_MANAGED, "Managed forums"));
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
        private PolicyDetail _detail;
        
        private Button _save;
        private Button _revert;
        private Button _delete;
        
        public PolicyGroup(CTabItem tab, Composite root, CancelPolicy policy) {
            _tab = tab;
            _tabRoot = root;
            initGroupComponents(policy);
        }
        private void initGroupComponents(CancelPolicy policy) {
            _detailRoot = new Composite(_tabRoot, SWT.NONE);
            GridLayout gl = new GridLayout(2, true);
            gl.horizontalSpacing = 0;
            gl.verticalSpacing = 0;
            gl.marginHeight = 0;
            gl.marginWidth = 0;
            _detailRoot.setLayout(gl);
            _detail = new PolicyDetail(_detailRoot, policy, PolicyGroup.this);
            _detail.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
            
            Composite buttons = new Composite(_detailRoot, SWT.NONE);
            buttons.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false, 2, 1));
            buttons.setLayout(new FillLayout(SWT.HORIZONTAL));
            _save = new Button(buttons, SWT.PUSH);
            _save.addSelectionListener(new FireSelectionListener() { public void fire() { saveChanges(); } });
            _revert = new Button(buttons, SWT.PUSH);
            _revert.addSelectionListener(new FireSelectionListener() { public void fire() { revertChanges(); } });
            
            _save.setText(_translationRegistry.getText(T_SAVE, "Save changes"));
            _revert.setText(_translationRegistry.getText(T_REVERT, "Revert changes"));
            
            _save.setFont(_themeRegistry.getTheme().BUTTON_FONT);
            _revert.setFont(_themeRegistry.getTheme().BUTTON_FONT);
            
            boolean isNew = policy.getIsNew();
            _save.setEnabled(isNew);
            _revert.setEnabled(isNew);
            
            if (policy.getScopeApplyToAll() || policy.getScopeApplyToLocallyManaged()) {
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
            _detail.save();
            _save.setEnabled(false);
            _revert.setEnabled(false);
        }
        private void revertChanges() {
            _ui.debugMessage("reverting changes to the detail");
            _detail.revert();
            _save.setEnabled(false);
            _revert.setEnabled(false);
        }
        private void deletePolicy() {
            _detail.delete();
            _tabRoot.dispose();
            _tab.dispose();
        }
    }
  
    private static final String T_HONOR_AUTHOR = "syndie.gui.cancelmanager.honorauthor";
    private static final String T_HONOR_OWNER = "syndie.gui.cancelmanager.honorowner";
    private static final String T_HONOR_MANAGER = "syndie.gui.cancelmanager.honormanager";
    private static final String T_HONOR_POSTER = "syndie.gui.cancelmanager.honorposter";
    
    private class PolicyDetail {
        private PolicyGroup _group;
        private Composite _groupRoot;
        private Composite _detailRoot;
        private CancelPolicy _policy;
        private CancelPolicy _policyOrig;
        
        private Button _honorFromAuthor;
        private Button _honorFromForumOwner;
        private Button _honorFromForumManager;
        private Button _honorFromForumAuthorizedPoster;
        
        public PolicyDetail(Composite root, CancelPolicy policy, PolicyGroup group) {
            _group = group;
            _groupRoot = root;
            _policy = policy;
            _policyOrig = new CancelPolicy(policy);
            initDetailComponents();
        }
        
        void save() {
            _client.saveCancelPolicy(_policy);
            _policy.setIsNew(false);
            _policyOrig.load(_policy);
            render();
        }
        void revert() {
            _ui.debugMessage("reverting policy: [" + _policy + "] to [" + _policyOrig + "]");
            _policy.load(_policyOrig);
            render();
        }
        void delete() {
            _client.deleteCancelPolicy(_policy);
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

            _honorFromAuthor = new Button(_detailRoot, SWT.CHECK);
            _honorFromAuthor.setText(_translationRegistry.getText(T_HONOR_AUTHOR, "Honor cancel messages from the message author"));
            _honorFromAuthor.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            _honorFromAuthor.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    boolean enabled = _honorFromAuthor.getSelection();
                    _policy.setHonorFromAuthor(enabled);
                    _group.changeMade();
                }
            });
            
            _honorFromForumOwner = new Button(_detailRoot, SWT.CHECK);
            _honorFromForumOwner.setText(_translationRegistry.getText(T_HONOR_OWNER, "Honor cancel messages from the forum owner"));
            _honorFromForumOwner.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            _honorFromForumOwner.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    boolean enabled = _honorFromForumOwner.getSelection();
                    _policy.setHonorFromForumOwner(enabled);
                    _group.changeMade();
                }
            });
            
            _honorFromForumManager = new Button(_detailRoot, SWT.CHECK);
            _honorFromForumManager.setText(_translationRegistry.getText(T_HONOR_MANAGER, "Honor cancel messages from any forum manager(s)"));
            _honorFromForumManager.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            _honorFromForumManager.setEnabled(false);
            _honorFromForumManager.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    boolean enabled = _honorFromForumManager.getSelection();
                    _policy.setHonorFromForumManager(enabled);
                    _group.changeMade();
                }
            });
            
            _honorFromForumAuthorizedPoster = new Button(_detailRoot, SWT.CHECK);
            _honorFromForumAuthorizedPoster.setText(_translationRegistry.getText(T_HONOR_POSTER, "Honor cancel messages from any explicitly authorized forum posters"));
            _honorFromForumAuthorizedPoster.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            _honorFromForumAuthorizedPoster.setEnabled(false);
            _honorFromForumAuthorizedPoster.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    boolean enabled = _honorFromForumAuthorizedPoster.getSelection();
                    _policy.setHonorFromForumAuthorizedPoster(enabled);
                    _group.changeMade();
                }
            });
            
            _honorFromAuthor.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
            _honorFromForumOwner.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
            _honorFromForumManager.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
            _honorFromForumAuthorizedPoster.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
            
            render();
            
            _ui.debugMessage("policy detail created for " + _policy);
        }
        
        private void render() {
            _ui.debugMessage("rendering policy: [" + _policy + "]");
            _honorFromAuthor.setSelection(_policy.getHonorFromAuthor());
            _honorFromForumOwner.setSelection(_policy.getHonorFromForumOwner());
            _honorFromForumManager.setSelection(_policy.getHonorFromForumManager());
            _honorFromForumAuthorizedPoster.setSelection(_policy.getHonorFromForumAuthorizedPoster());
        }
    }
    
    private static final String T_SAVE = "syndie.gui.cancelmanager.save";
    private static final String T_REVERT = "syndie.gui.cancelmanager.revert";
    private static final String T_DELETE = "syndie.gui.cancelmanager.delete";
    private static final String T_TABTITLE_DEFAULT = "syndie.gui.cancelmanager.tabtitle.default";
    private static final String T_TABTITLE_MANAGED = "syndie.gui.cancelmanager.tabtitle.managed";
}
