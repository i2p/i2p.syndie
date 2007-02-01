package syndie.gui;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Properties;
import net.i2p.data.Base64;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.Version;
import syndie.data.BugConfig;
import syndie.data.ChannelInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.WebRipRunner;
import syndie.db.CommandImpl;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 *
 */
public class BugReport implements Themeable, Translatable {
    private Composite _parent;
    private BrowserControl _browser;
    private ScrolledComposite _scroll;
    private Composite _root;
    private SyndieURI _uri;
    
    private Label _componentLabel;
    private Button _component;
    private Menu _componentMenu;
    private Label _typeLabel;
    private Combo _type;
    private ArrayList _types;
    private Label _severityLabel;
    private Combo _severity;
    private ArrayList _severities;
    private Label _osLabel;
    private Text _os;
    private Label _jvmLabel;
    private Text _jvm;
    private Label _swtLabel;
    private Text _swt;
    private Label _syndieLabel;
    private Text _syndie;
    private Label _attachmentsLabel;
    private Combo _attachments;
    private Button _attachmentsAdd;
    private Button _attachmentsRemove;
    private Label _summaryLabel;
    private Text _summary;
    private Group _logGroup;
    private Text _log;
    
    private Button _post;
    private Label _targetLabel;
    private Combo _target;
    private Button _asPrivate;
    private Label _signAsLabel;
    private Combo _signAs;
    
    private String _selectedComponentId;
    private String _selectedTypeId;
    private String _selectedSeverityId;
    private ArrayList _attachmentFiles;
    private ArrayList _targetChans;
    private ArrayList _signAsChans;
    
    /** 
     * if the user knows this forum (and its one they can post to), it'll be their default 
     * target (which is good, since this is the standard bug report forum)
     */
    private static final String STANDARD_BUGREPORT_FORUM = "eu61~moznLTNsOizxDjAsJpxBIm1WC1s4b1hWDy8gYQ=";
    
    public BugReport(BrowserControl browser, Composite parent, SyndieURI uri) {
        _browser = browser;
        _parent = parent;
        _uri = uri;
        _attachmentFiles = new ArrayList();
        _targetChans = new ArrayList();
        _signAsChans = new ArrayList();
        _severities = new ArrayList();
        _types = new ArrayList();
        initComponents();
    }
    
    private void initComponents() {
        _scroll = new ScrolledComposite(_parent, SWT.H_SCROLL | SWT.V_SCROLL);
        _scroll.setAlwaysShowScrollBars(false);
        _scroll.setExpandHorizontal(true);
        _scroll.setExpandVertical(true);
        _root = new Composite(_scroll, SWT.NONE);
        _root.setLayout(new GridLayout(4, false));
        _scroll.setContent(_root);
        
        _componentLabel = new Label(_root, SWT.NONE);
        _componentLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _component = new Button(_root, SWT.DROP_DOWN | SWT.READ_ONLY);
        _component.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        _componentMenu = new Menu(_component);
        _component.setMenu(_componentMenu);
        _component.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _componentMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _componentMenu.setVisible(true); }
        });
        
        _typeLabel = new Label(_root, SWT.NONE);
        _typeLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _type = new Combo(_root, SWT.DROP_DOWN | SWT.READ_ONLY);
        _type.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _type.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _selectedTypeId = (String)_types.get(_type.getSelectionIndex()); }
            public void widgetSelected(SelectionEvent selectionEvent) { _selectedTypeId = (String)_types.get(_type.getSelectionIndex()); }
        });
        
        _severityLabel = new Label(_root, SWT.NONE);
        _severityLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _severity = new Combo(_root, SWT.DROP_DOWN | SWT.READ_ONLY);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        gd.widthHint = 100;
        _severity.setLayoutData(gd);
        _severity.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _selectedSeverityId = (String)_severities.get(_severity.getSelectionIndex()); }
            public void widgetSelected(SelectionEvent selectionEvent) { _selectedSeverityId = (String)_severities.get(_severity.getSelectionIndex()); }
        });
        
        _signAsLabel = new Label(_root, SWT.NONE);
        _signAsLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _signAs = new Combo(_root, SWT.DROP_DOWN | SWT.READ_ONLY);
        gd = new GridData(GridData.FILL, GridData.CENTER, false, false);
        gd.widthHint = 100;
        _signAs.setLayoutData(gd);
        
        _attachmentsLabel = new Label(_root, SWT.NONE);
        _attachmentsLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        Composite attach = new Composite(_root, SWT.NONE);
        attach.setLayout(new GridLayout(3, false));
        attach.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _attachments = new Combo(attach, SWT.DROP_DOWN | SWT.READ_ONLY);
        _attachments.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));
        
        _attachmentsAdd = new Button(attach, SWT.PUSH);
        _attachmentsAdd.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        _attachmentsRemove = new Button(attach, SWT.PUSH);
        _attachmentsRemove.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        
        _attachmentsAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addAttachment(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addAttachment(); }
        });
        _attachmentsRemove.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { removeAttachment(); }
            public void widgetSelected(SelectionEvent selectionEvent) { removeAttachment(); }
        });
        
        _summaryLabel = new Label(_root, SWT.NONE);
        _summaryLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _summary = new Text(_root, SWT.SINGLE | SWT.BORDER);
        _summary.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        
        _asPrivate = new Button(_root, SWT.CHECK);
        _asPrivate.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false, 4, 1));
        
        _logGroup = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _logGroup.setLayout(new FillLayout());
        _logGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 4, 1));
        _log = new Text(_logGroup, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER | SWT.WRAP);

        Composite revRow = new Composite(_root, SWT.NONE);
        revRow.setLayout(new GridLayout(8, false));
        revRow.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        
        _syndieLabel = new Label(revRow, SWT.NONE);
        _syndieLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _syndie = new Text(revRow, SWT.SINGLE | SWT.BORDER);
        gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        gd.widthHint = 50;
        _syndie.setLayoutData(gd);
        
        _osLabel = new Label(revRow, SWT.NONE);
        _osLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _os = new Text(revRow, SWT.SINGLE | SWT.BORDER);
        gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        gd.widthHint = 50;
        _os.setLayoutData(gd);
        
        _jvmLabel = new Label(revRow, SWT.NONE);
        _jvmLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _jvm = new Text(revRow, SWT.SINGLE | SWT.BORDER);
        gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        gd.widthHint = 50;
        _jvm.setLayoutData(gd);
        
        _swtLabel = new Label(revRow, SWT.NONE);
        _swtLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _swt = new Text(revRow, SWT.SINGLE | SWT.BORDER);
        gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        gd.widthHint = 50;
        _swt.setLayoutData(gd);

        Composite action = new Composite(_root, SWT.NONE);
        action.setLayout(new GridLayout(3, false));
        action.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 4, 1));
        
        _post = new Button(action, SWT.PUSH);
        _post.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _post.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { postReport(); }
            public void widgetSelected(SelectionEvent selectionEvent) { postReport(); }
        });
        
        _targetLabel = new Label(action, SWT.NONE);
        _targetLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _target = new Combo(action, SWT.DROP_DOWN | SWT.READ_ONLY);
        gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        gd.widthHint = 100;
        _target.setLayoutData(gd);
        _target.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { targetSelected(); }
            public void widgetSelected(SelectionEvent selectionEvent) { targetSelected(); }
        });
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
        
        loadConfig();
        _root.layout(true, true);
        _scroll.setMinSize(_root.computeSize(SWT.DEFAULT, SWT.DEFAULT));
    }
    
    private void savePrefs(String os, String jvm, String swt) {
        Properties prefs = _browser.getClient().getNymPrefs();
        prefs.setProperty("bugreport.os", os);
        prefs.setProperty("bugreport.jvm", jvm);
        prefs.setProperty("bugreport.swt", swt);
        _browser.getClient().setNymPrefs(prefs);
    }
    private void loadPrefs() {
        String defOS = System.getProperty("os.name") + " " + System.getProperty("os.version") + " " + System.getProperty("os.arch");
        String defJVM = System.getProperty("java.vm.name") + " " + System.getProperty("java.vm.version");
        String defSWT = SWT.getPlatform() + "-" + SWT.getVersion();
        
        Properties prefs = _browser.getClient().getNymPrefs();
        _os.setText(prefs.getProperty("bugreport.os", defOS));
        _jvm.setText(prefs.getProperty("bugreport.jvm", defJVM));
        _swt.setText(prefs.getProperty("bugreport.swt", defSWT));
    }
    
    private void postReport() {
        final String os = _os.getText().trim();
        final String jvm = _jvm.getText().trim();
        final String swt = _swt.getText().trim();
        
        savePrefs(os, jvm, swt);
                
        MessageCreator creator = new MessageCreator(new MessageCreator.MessageCreatorSource() {
            public BrowserControl getBrowser() { return _browser; }
            public DBClient getClient() { return _browser.getClient(); }
            public UI getUI() { return _browser.getUI(); }
            public Hash getAuthor() {
                int idx = _signAs.getSelectionIndex();
                if (idx >= _signAsChans.size()) {
                    // create a new keypair?
                    return null;
                } else {
                    return (Hash)_signAsChans.get(idx);
                }
            }
            public Hash getSignAs() { return null; }
            public boolean getAuthorHidden() { return false; }

            public Hash getTarget() {
                int idx = _target.getSelectionIndex();
                if (idx < _targetChans.size())
                    return (Hash)_targetChans.get(idx);
                else
                    return null;
            }

            public int getPageCount() { return 1; }

            public String getPageContent(int page) { 
                StringBuffer rv = new StringBuffer();
                rv.append("OS: " + os + "\n");
                rv.append("JVM: " + jvm + "\n");
                rv.append("SWT: " + swt + "\n");
        
                rv.append("\n");
                rv.append(_log.getText());
                return rv.toString();
            }
            public String getPageType(int page) { return "text/plain"; }
            public java.util.List getAttachmentNames() {
                ArrayList rv = new ArrayList();
                for (int i = 0; i < _attachmentFiles.size(); i++)
                    rv.add(((File)_attachmentFiles.get(i)).getName());
                return rv;
            }

            public java.util.List getAttachmentTypes() {
                ArrayList rv = new ArrayList();
                for (int i = 0; i < _attachmentFiles.size(); i++) {
                    String name = ((File)_attachmentFiles.get(i)).getName();
                    String type = WebRipRunner.guessContentType(name);
                    rv.add(type);
                }
                return rv;
            }

            public byte[] getAttachmentData(int attachmentIndex) {
                File f = (File)_attachmentFiles.get(attachmentIndex-1); // 1-indexed
                byte buf[] = new byte[(int)f.length()];
                int off = 0;
                FileInputStream fis = null;
                try {
                    fis = new FileInputStream(f);
                    int remaining = buf.length;
                    int read = 0;
                    while ( (remaining > 0) && ((read = fis.read(buf, off, remaining)) != -1) ) {
                        off += read;
                        remaining -= read;
                    }
                    return buf;
                } catch (IOException ioe) {
                    _browser.getUI().errorMessage("Error reading attachment", ioe);
                    return null;
                } finally {
                    if (fis != null) try { fis.close(); } catch (IOException ioe) {}
                }
            }

            public String getSubject() { return _summary.getText(); }
            public boolean getPrivacyPBE() { return false; }
            public String getPassphrase() { return null; }
            public String getPassphrasePrompt() { return null; }
            public boolean getPrivacyPublic() { return !_asPrivate.getSelection(); }
            public String getAvatarUnmodifiedFilename() { return null; }
            public byte[] getAvatarModifiedData() { return null; }
            public boolean getPrivacyReply() { return _asPrivate.getSelection(); }
            public String[] getPublicTags() {
                ArrayList tags = new ArrayList();
                tags.add("bugreport");
                tags.add("syndie." + CommandImpl.strip(_syndie.getText()));
                tags.add("severity." + _severities.get(_severity.getSelectionIndex()));
                if (_selectedComponentId != null)
                    tags.add("component." + _selectedComponentId);
                tags.add("type." + _types.get(_type.getSelectionIndex()));
                return (String[])tags.toArray(new String[0]);
            }

            public String[] getPrivateTags() { return new String[0]; }
            public java.util.List getReferenceNodes() { return new ArrayList(); }
            public int getParentCount() { return 0; }
            public SyndieURI getParent(int depth) { return null; }
            public String getExpiration() { return null; }
            public boolean getForceNewThread() { return false; }
            public boolean getRefuseReplies() { return false; }
        });
        boolean posted = creator.execute();
        if (posted) {
            SyndieURI uri = creator.getCreatedURI();
            _browser.view(uri);
            _browser.unview(_uri);
        } else {
            String err = creator.getErrors();
            MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_ERROR | SWT.OK);
            box.setMessage(err);
            box.setText(_browser.getTranslationRegistry().getText(T_POST_ERR, "Error posting report"));
            box.open();
        }
    }
    
    private static final String T_POST_OK = "syndie.gui.bugreport.postok";
    private static final String T_POST_OK_MSG = "syndie.gui.bugreport.postokmsg";
    private static final String T_POST_ERR = "syndie.gui.bugreport.posterr";
    
    private void targetSelected() {
        /*
        SyndieURI target = _browser.getClient().getBugConfig().getTargetScope();
        long targetId = _browser.getClient().getChannelId(target.getScope());
        
        if ( (targetId >= 0) && (_target.getSelectionIndex() == 0) ) {
            _asPrivate.setEnabled(true);
        } else {
            _asPrivate.setEnabled(false);
            _asPrivate.setSelection(false);
        }
         */
    }
    
    private static final String T_FILE_TEXT = "syndie.gui.bugreport.file";
    private FileDialog _fileDialog;
    private void addAttachment() {
        if (_fileDialog == null) {
            _fileDialog = new FileDialog(_root.getShell(), SWT.OPEN | SWT.MULTI);
            _fileDialog.setText(_browser.getTranslationRegistry().getText(T_FILE_TEXT, "File to attach"));
        }
        if (_fileDialog.open() == null) return;
        String selected[] = _fileDialog.getFileNames();
        String base = _fileDialog.getFilterPath();
        for (int i = 0; i < selected.length; i++) {
            File cur = null;
            if (base == null)
                cur = new File(selected[i]);
            else
                cur = new File(base, selected[i]);
            if (cur.exists() && cur.isFile() && cur.canRead()) {
                if (!_attachmentFiles.contains(cur)) {
                    _attachmentFiles.add(cur);
                    _attachments.add(cur.getName());
                    _attachments.select(_attachments.getItemCount()-1);
                }
            }
        }
    }
    private void removeAttachment() {
        int idx = _attachments.getSelectionIndex();
        if (idx < 0) return;
        _attachmentFiles.remove(idx);
        _attachments.remove(idx);
        if (_attachments.getItemCount() > 0)
            _attachments.select(_attachments.getItemCount()-1);
    }
    
    private void loadConfig() {
        BugConfig cfg = _browser.getClient().getBugConfig();
        rebuildComponentMenu(cfg);
        ReferenceNode def = cfg.getComponentDefault();
        if (def != null)
            _component.setText(_browser.getTranslationRegistry().getText(def.getName(), def.getDescription()));
        
        _severity.setRedraw(false);
        for (int i = 0; i < cfg.getSeverityCount(); i++) {
            String id = cfg.getSeverityId(i);
            String name = cfg.getSeverityName(i);
            _severity.add(_browser.getTranslationRegistry().getText(id, name));
            _severities.add(id);
        }
        if (cfg.getSeverityDefaultIndex() >= 0) {
            _severity.select(cfg.getSeverityDefaultIndex());
            _selectedSeverityId = (String)_severities.get(cfg.getSeverityDefaultIndex());
        }
        _severity.setRedraw(true);
        
        _type.setRedraw(false);
        for (int i = 0; i < cfg.getTypeCount(); i++) {
            String id = cfg.getTypeId(i);
            String name = cfg.getTypeName(i);
            _type.add(_browser.getTranslationRegistry().getText(id, name));
            _types.add(id);
        }
        if (cfg.getTypeDefaultIndex() >= 0) {
            _type.select(cfg.getTypeDefaultIndex());
            _selectedTypeId = (String)_types.get(cfg.getTypeDefaultIndex());
        }
        _type.setRedraw(true);
        
        loadPrefs();
        _syndie.setText(Version.VERSION);
        
        SyndieURI target = cfg.getTargetScope();
        if (target != null) {
            long targetId = _browser.getClient().getChannelId(target.getScope());
            if (targetId >= 0) {
                String name = _browser.getClient().getChannelName(targetId);
                _target.add(target.getScope().toBase64().substring(0,6) + ": " + name);
            }
        }
        
        DBClient.ChannelCollector chans = _browser.getClient().getChannels(true, true, true, true);
        for (int i = 0; i < chans.getIdentityChannelCount(); i++) {
            ChannelInfo info = chans.getIdentityChannel(i);
            _targetChans.add(info.getChannelHash());
            _signAsChans.add(info.getChannelHash());
            _target.add(info.getChannelHash().toBase64().substring(0,6) + ": " + info.getName());
            _signAs.add(info.getChannelHash().toBase64().substring(0,6) + ": " + info.getName());
        }
        for (int i = 0; i < chans.getManagedChannelCount(); i++) {
            ChannelInfo info = chans.getManagedChannel(i);
            _targetChans.add(info.getChannelHash());
            _target.add(info.getChannelHash().toBase64().substring(0,6) + ": " + info.getName());
        }
        for (int i = 0; i < chans.getPostChannelCount(); i++) {
            ChannelInfo info = chans.getPostChannel(i);
            _targetChans.add(info.getChannelHash());
            _target.add(info.getChannelHash().toBase64().substring(0,6) + ": " + info.getName());
        }
        for (int i = 0; i < chans.getPublicPostChannelCount(); i++) {
            ChannelInfo info = chans.getPublicPostChannel(i);
            _targetChans.add(info.getChannelHash());
            _target.add(info.getChannelHash().toBase64().substring(0,6) + ": " + info.getName());
        }
        
        // todo: enable this (need to modify MessageGen to create authorized unauthenticated posts)
        // _signAs.add(_browser.getTranslationRegistry().getText(T_SIGN_AS_ANON, "Anonymous"));
        
        int idx = _targetChans.indexOf(new Hash(Base64.decode(STANDARD_BUGREPORT_FORUM)));
        if (idx >= 0)
            _target.select(idx);
        else
            _target.select(0);
        targetSelected();
        _signAs.select(0);
    }
    
    private void rebuildComponentMenu(BugConfig cfg) {
        MenuItem items[] = _componentMenu.getItems();
        for (int i = 0; i < items.length; i++) items[i].dispose();
        
        for (int i = 0; i < cfg.getComponentCount(); i++)
            addComponent(cfg.getComponent(i), _componentMenu);
    }
    private void addComponent(final ReferenceNode node, Menu parent) {
        if (node == null) return;
        MenuItem item = null;
        if (node.getChildCount() > 0) {
            item = new MenuItem(parent, SWT.CASCADE);
            Menu sub = new Menu(item);
            item.setMenu(sub);
            item.setText(_browser.getTranslationRegistry().getText(node.getName(), node.getDescription()));
            
            MenuItem subcur = new MenuItem(sub, SWT.PUSH);
            subcur.setText(_browser.getTranslationRegistry().getText(node.getName(), node.getDescription()));
            subcur.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { selectComponent(node); }
                public void widgetSelected(SelectionEvent selectionEvent) { selectComponent(node); }
            });
            new MenuItem(sub, SWT.SEPARATOR);
            for (int i = 0; i < node.getChildCount(); i++)
                addComponent(node.getChild(i), sub);
        } else {
            item = new MenuItem(parent, SWT.PUSH);
            item.setText(_browser.getTranslationRegistry().getText(node.getName(), node.getDescription()));
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { selectComponent(node); }
                public void widgetSelected(SelectionEvent selectionEvent) { selectComponent(node); }
            });
        }
    }
    
    private void selectComponent(ReferenceNode node) {
        _component.setText(_browser.getTranslationRegistry().getText(node.getName(), node.getDescription()));
        _selectedComponentId = node.getName();
    }
    
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
    }
    
    public void applyTheme(Theme theme) {
        _componentLabel.setFont(theme.DEFAULT_FONT);
        _component.setFont(theme.DEFAULT_FONT);
        _typeLabel.setFont(theme.DEFAULT_FONT);
        _type.setFont(theme.DEFAULT_FONT);
        _severityLabel.setFont(theme.DEFAULT_FONT);
        _severity.setFont(theme.DEFAULT_FONT);
        _osLabel.setFont(theme.DEFAULT_FONT);
        _os.setFont(theme.DEFAULT_FONT);
        _jvmLabel.setFont(theme.DEFAULT_FONT);
        _jvm.setFont(theme.DEFAULT_FONT);
        _swtLabel.setFont(theme.DEFAULT_FONT);
        _swt.setFont(theme.DEFAULT_FONT);
        _syndieLabel.setFont(theme.DEFAULT_FONT);
        _syndie.setFont(theme.DEFAULT_FONT);
        _summaryLabel.setFont(theme.DEFAULT_FONT);
        _summary.setFont(theme.DEFAULT_FONT);
        _logGroup.setFont(theme.DEFAULT_FONT);
        _log.setFont(theme.DEFAULT_FONT);
        _attachmentsLabel.setFont(theme.DEFAULT_FONT);
        _attachments.setFont(theme.DEFAULT_FONT);
        _attachmentsAdd.setFont(theme.BUTTON_FONT);
        _attachmentsRemove.setFont(theme.BUTTON_FONT);
        _post.setFont(theme.BUTTON_FONT);
        _targetLabel.setFont(theme.DEFAULT_FONT);
        _target.setFont(theme.DEFAULT_FONT);
        _asPrivate.setFont(theme.DEFAULT_FONT);
        _signAsLabel.setFont(theme.DEFAULT_FONT);
        _signAs.setFont(theme.DEFAULT_FONT);
        
        _scroll.setMinSize(_root.computeSize(SWT.DEFAULT, SWT.DEFAULT));
    }
    
    private static final String T_COMPONENT = "syndie.gui.bugreport.component";
    private static final String T_TYPE = "syndie.gui.bugreport.type";
    private static final String T_SEVERITY = "syndie.gui.bugreport.severity";
    private static final String T_OS = "syndie.gui.bugreport.os";
    private static final String T_JVM = "syndie.gui.bugreport.jvm";
    private static final String T_SWT = "syndie.gui.bugreport.swt";
    private static final String T_SUMMARY = "syndie.gui.bugreport.summary";
    private static final String T_LOG = "syndie.gui.bugreport.log";
    private static final String T_ATTACHMENTS = "syndie.gui.bugreport.attachments";
    private static final String T_ATTACHMENTS_ADD = "syndie.gui.bugreport.attachments.add";
    private static final String T_ATTACHMENTS_REMOVE = "syndie.gui.bugreport.attachments.remove";
    private static final String T_POST = "syndie.gui.bugreport.post";
    private static final String T_TARGET = "syndie.gui.bugreport.target";
    private static final String T_ASPRIVATE = "syndie.gui.bugreport.asprivate";
    
    private static final String T_SIGN_AS = "syndie.gui.bugreport.signas";
    private static final String T_SIGN_AS_ANON = "syndie.gui.bugreport.signas.anon";

    public void translate(TranslationRegistry registry) {
        _componentLabel.setText(registry.getText(T_COMPONENT, "Component:"));
        _typeLabel.setText(registry.getText(T_TYPE, "Bug type:"));
        _severityLabel.setText(registry.getText(T_SEVERITY, "Severity:"));
        _osLabel.setText(registry.getText(T_OS, "OS:"));
        _jvmLabel.setText(registry.getText(T_JVM, "JVM:"));
        _swtLabel.setText(registry.getText(T_SWT, "SWT:"));
        _syndieLabel.setText(registry.getText(T_SWT, "Syndie:"));
        _summaryLabel.setText(registry.getText(T_SUMMARY, "Issue summary:"));
        _logGroup.setText(registry.getText(T_LOG, "Details:"));
        _attachmentsLabel.setText(registry.getText(T_ATTACHMENTS, "Attachments:"));
        _attachmentsAdd.setText(registry.getText(T_ATTACHMENTS_ADD, "Add"));
        _attachmentsRemove.setText(registry.getText(T_ATTACHMENTS_REMOVE, "Remove"));
        
        _post.setText(registry.getText(T_POST, "Post bug report"));
        _targetLabel.setText(registry.getText(T_TARGET, "Post to:"));
        _asPrivate.setText(registry.getText(T_ASPRIVATE, "Report includes sensitive data (so only let the admins read it)"));
        _signAsLabel.setText(registry.getText(T_SIGN_AS, "Sign report as:"));
        if (_signAs.getItemCount() > _signAsChans.size()) {
            _signAs.remove(_signAs.getItemCount()-1);
            _signAs.add(registry.getText(T_SIGN_AS_ANON, "Anonymous"));
        }
        
        rebuildComponentMenu(_browser.getClient().getBugConfig());
    }
}
