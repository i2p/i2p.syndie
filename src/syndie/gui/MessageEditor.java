package syndie.gui;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import net.i2p.data.DataHelper;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Label;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.Enclosure;
import syndie.data.EnclosureBody;
import syndie.data.NymKey;
import syndie.data.SyndieURI;
import syndie.db.CommandImpl;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 *
 */
public class MessageEditor implements ReferenceChooserTree.AcceptanceListener, Translatable {
    private BrowserControl _browser;
    private DBClient _client;
    /** list of (byte[]) instances */
    private ArrayList _attachments;
    /** list of Properties associated with each attachment */
    private ArrayList _attachmentConfig;
    /** list of PageEditor instances */
    private ArrayList _pages;
    private Hash _author;
    private Hash _target;
    private List _targetList;
    /**
     * ordered list of earlier messages (SyndieURI) this follows in the thread 
     * of (most recent parent first)
     */
    private List _parents;
    /** if using PBE, this is the required passphrase */
    private String _passphrase;
    /** if using PBE, this is the prompt for the passphrase */
    private String _passphrasePrompt;
    /** list of ReferenceNode roots */
    private List _referenceNodes;
    private DBClient.ChannelCollector _nymChannels;

    private ReferenceChooserPopup _refChooser;
    
    private Composite _parent;
    private Composite _root;
    private Composite _pageRoot;
    private StackLayout _pageRootLayout;
    private Composite _actions;
    private Button _ok;
    private Button _save;
    private Button _cancel;
    
    private Composite _msgControl;
    private Label _controlAvatar;
    private Image _controlAvatarImage;
    /** 
     * file the avatar was loaded from, or null if either the avatar hasn't
     * been selected or if it was scaled/updated in-process (so we should use _controlAvatarImage as
     * the source)
     */
    private String _controlAvatarImageSource;
    private FileDialog _controlAvatarDialog;
    private Text _controlAuthor;
    private Menu _controlAuthorMenu;
    private Label _controlSubject;
    private Text _controlSubjectText;
    private Label _controlForum;
    private Combo _controlForumCombo;
    private Label _controlTags;
    private Text _controlTagsText;
    private Label _controlPrivacy;
    private Combo _controlPrivacyCombo;
    private Label _controlPage;
    private Combo _controlPageCombo;
    private Menu _controlPageMenu;
    private Button _controlPageAction;
    private Label _controlAttachment;
    private Combo _controlAttachmentCombo;
    private Menu _controlAttachmentMenu;
    private Button _controlAttachmentAction;
    private Label _controlRef;
    private Button _controlRefAction;
    private Label _controlExpiration;
    private Text _controlExpirationText;
    
    private MessageEditorListener _listener;
    private UI _ui;

    private MenuItem _pageAddHTML;
    private MenuItem _pageAddText;
    private MenuItem _pageRemove;
    private MenuItem _attachmentAdd;
    private MenuItem _attachmentRemove;
    
    /** Creates a new instance of MessageEditor */
    public MessageEditor(BrowserControl browser, Composite parent, MessageEditorListener lsnr) {
        _browser = browser;
        _client = browser.getClient();
        _parent = parent;
        _listener = lsnr;
        _ui = browser.getUI();
        _pages = new ArrayList();
        _attachments = new ArrayList();
        _attachmentConfig = new ArrayList();
        _targetList = new ArrayList();
        _parents = new ArrayList();
        _referenceNodes = new ArrayList();
    
        _browser.getUI().debugMessage("editor: fetching channels");
        _nymChannels = _client.getChannels(true, true, true, true);
        _browser.getUI().debugMessage("editor: channels fetched.  init components");
        initComponents();
        _browser.getUI().debugMessage("editor: components initialized");
        _refChooser = new ReferenceChooserPopup(_root.getShell(), _browser, this);
    }
    
    public interface MessageEditorListener {
        public void messageCreated(MessageEditor editor, SyndieURI postedURI);
        public void messagePostponed(MessageEditor editor, long postponementId);
        public void messageCancelled(MessageEditor editor);
    }
    
    public void setParentMessage(SyndieURI uri) {
        _parents.clear();
        if (uri != null) {
            _parents.add(uri);
        }
    }
    
    public void setScope(Hash scope) {
        _target = scope;
        updateAuthor();
    }
    
    UI getUI() { return _ui; }
    BrowserControl getBrowser() { return _browser; }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(1, true));
        initControlBar();
        
        _pageRoot = new Composite(_root, SWT.NONE);
        _pageRootLayout = new StackLayout();
        _pageRoot.setLayout(_pageRootLayout);
        _pageRoot.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _actions = new Composite(_root, SWT.NONE);
        _actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        _actions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _ok = new Button(_actions, SWT.PUSH);
        _ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { postMessage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { postMessage(); }
        });
        _save = new Button(_actions, SWT.PUSH);
        _save.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { postponeMessage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { postponeMessage(); }
        });
        _cancel = new Button(_actions, SWT.PUSH);
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { cancelMessage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { cancelMessage(); }
        });
        
        getBrowser().getTranslationRegistry().register(this);
    }
    
    public void setAsReply(boolean asReply) {
        if (asReply)
            _controlPrivacyCombo.select(PRIVACY_REPLY);
    }

    private static final int PRIVACY_PUBLIC = 0;
    private static final int PRIVACY_AUTHORIZED = 1;
    private static final int PRIVACY_PBE = 2;
    private static final int PRIVACY_REPLY = 3;

    private void initControlBar() {
        _msgControl = new Composite(_root, SWT.BORDER);
        _msgControl.setLayout(new GridLayout(3, false));
        _msgControl.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _controlAvatarImage = ImageUtil.ICON_QUESTION;
        _controlAvatar = new Label(_msgControl, SWT.BORDER);
        _controlAvatar.setImage(_controlAvatarImage);
        _controlAvatar.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false, 1, 2));
        _controlAvatar.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) {}
            public void mouseDown(MouseEvent mouseEvent) { pickAvatar(); }
            public void mouseUp(MouseEvent mouseEvent) {}
        });
        
        _controlAvatarDialog = new FileDialog(_root.getShell(), SWT.OPEN | SWT.SINGLE);
        _controlAvatarDialog.setFilterExtensions(new String[] { "*.png; *.jpeg; *.jpg; *.gif; *.ico", "*.*" });
        _controlAvatarDialog.setFilterNames(new String[] { "Images", "All files" });
        
        _controlSubject = new Label(_msgControl, SWT.NONE);
        _controlSubject.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _controlSubjectText = new Text(_msgControl, SWT.SINGLE | SWT.BORDER);
        _controlSubjectText.setText("");
        _controlSubjectText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _controlForum = new Label(_msgControl, SWT.NONE);
        _controlForum.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        Composite line2 = new Composite(_msgControl, SWT.NONE);
        line2.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        GridLayout gl = new GridLayout(5, false);
        //gl.horizontalSpacing = 0;
        gl.marginWidth = 0;
        gl.verticalSpacing = 0;
        line2.setLayout(gl);
        
        _controlForumCombo = new Combo(line2, SWT.DROP_DOWN);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        gd.widthHint = 200;
        _controlForumCombo.setLayoutData(gd);
        _controlForumCombo.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { forumSelected(); }
            public void widgetSelected(SelectionEvent selectionEvent) { forumSelected(); }
        });
        
        _controlTags = new Label(line2, SWT.NONE);
        _controlTags.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _controlTagsText = new Text(line2, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        gd.widthHint = 50;
        _controlTagsText.setLayoutData(gd);
        
        _controlPrivacy = new Label(line2, SWT.NONE);
        _controlPrivacy.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _controlPrivacyCombo = new Combo(line2, SWT.DROP_DOWN);
        _controlPrivacyCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        // author is under the avatar
        _controlAuthor = new Text(_msgControl, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        gd = new GridData(GridData.FILL, GridData.BEGINNING, false, false);
        gd.widthHint = Constants.MAX_AVATAR_WIDTH;
        _controlAuthor.setLayoutData(gd);
        
        _controlAuthorMenu = new Menu(_controlAuthor);
        _controlAuthor.setMenu(_controlAuthorMenu);
        _controlAuthor.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _controlAuthorMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _controlAuthorMenu.setVisible(true); }
        });
        
        _controlPage = new Label(_msgControl, SWT.NONE);
        _controlPage.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        Composite line3 = new Composite(_msgControl, SWT.NONE);
        gl = new GridLayout(9, false);
        //gl.horizontalSpacing = 0;
        gl.marginWidth = 0;
        gl.verticalSpacing = 0;
        line3.setLayout(gl);
        line3.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _controlPageCombo = new Combo(line3, SWT.DROP_DOWN);
        _controlPageCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _controlPageCombo.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { if (_pages.size() > 0) showPage(_controlPageCombo.getSelectionIndex()); }
            public void widgetSelected(SelectionEvent selectionEvent) { if (_pages.size() > 0) showPage(_controlPageCombo.getSelectionIndex()); }
        });
        
        _controlPageAction = new Button(line3, SWT.PUSH);
        _controlPageAction.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _controlPageMenu = new Menu(_controlPageAction);
        _pageAddHTML = new MenuItem(_controlPageMenu, SWT.PUSH);
        _pageAddHTML.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addPage("text/html"); }
            public void widgetSelected(SelectionEvent selectionEvent) { addPage("text/html"); }
        });
        _pageAddText = new MenuItem(_controlPageMenu, SWT.PUSH);
        _pageAddText.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addPage("text/plain"); }
            public void widgetSelected(SelectionEvent selectionEvent) { addPage("text/plain"); }
        });
        new MenuItem(_controlPageMenu, SWT.SEPARATOR);
        _pageRemove = new MenuItem(_controlPageMenu, SWT.PUSH);
        _pageRemove.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { removePage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { removePage(); }
        });
        _controlPageAction.setMenu(_controlPageMenu);
        _controlPageAction.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _controlPageMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _controlPageMenu.setVisible(true); }
        });
        
        _controlAttachment = new Label(line3, SWT.NONE);
        _controlAttachment.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _controlAttachmentCombo = new Combo(line3, SWT.DROP_DOWN);
        gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        gd.widthHint = 100;
        _controlAttachmentCombo.setLayoutData(gd);
        
        _controlAttachmentAction = new Button(line3, SWT.PUSH);
        _controlAttachmentAction.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _controlAttachmentMenu = new Menu(_controlAttachmentAction);
        _controlAttachmentAction.setMenu(_controlAttachmentMenu);
        _controlAttachmentAction.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _controlAttachmentMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _controlAttachmentMenu.setVisible(true); }
        });
        _attachmentAdd = new MenuItem(_controlAttachmentMenu, SWT.PUSH);
        _attachmentAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addAttachment(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addAttachment(); }
        });
        new MenuItem(_controlAttachmentMenu, SWT.SEPARATOR);
        _attachmentRemove = new MenuItem(_controlAttachmentMenu, SWT.PUSH);
        _attachmentRemove.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { removeAttachment(); }
            public void widgetSelected(SelectionEvent selectionEvent) { removeAttachment(); }
        });
        
        _controlRef = new Label(line3, SWT.NONE);
        _controlRef.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _controlRefAction = new Button(line3, SWT.PUSH);
        _controlRefAction.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _controlExpiration = new Label(line3, SWT.NONE);
        _controlExpiration.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _controlExpirationText = new Text(line3, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        gd.widthHint = 50;
        _controlExpirationText.setLayoutData(gd);
        
        updateAuthor();
    }
    
    public Control getControl() { return _root; }
    
    public void showPage(int pageNum) {
        PageEditor editor = (PageEditor)_pages.get(pageNum);
        if (editor != null) {
            _pageRootLayout.topControl = editor.getControl();
            _pageRoot.layout();
        } else {
            _pageRootLayout.topControl = null;
            _pageRoot.layout();
        }
    }
    public void addPage() { addPage("text/html"); }
    public void addPage(String type) {
        _browser.getUI().debugMessage("addPage: creating editor");
        PageEditor page = new PageEditor(_client, _pageRoot, this, type, _browser);
        _browser.getUI().debugMessage("addPage: editor created");
        _pages.add(page);
        showPage(_pages.size()-1);
        _browser.getUI().debugMessage("addPage: page shown");
        rebuildPagesCombo();
    }
    public void removePage() {
        if (_pages.size() > 0) {
            int idx = _controlPageCombo.getSelectionIndex();
            PageEditor editor = (PageEditor)_pages.remove(idx);
            editor.dispose();
        }
        if (_pages.size() > 0) {
            showPage(_pages.size()-1);
        }
        rebuildPagesCombo();
    }
    private void rebuildPagesCombo() {
        _controlPageCombo.setRedraw(false);
        _controlPageCombo.removeAll();
        if (_pages.size() > 0) {
            for (int i = 0; i < _pages.size(); i++)
                _controlPageCombo.add((i+1)+"");
            Control ctl = _pageRootLayout.topControl;
            int page = 0;
            for (int i = 0; i < _pages.size(); i++) {
                if (((PageEditor)_pages.get(i)).getControl() == ctl) {
                    page = i;
                    break;
                }
            }
            _controlPageCombo.select(page);
        } else {
            _controlPageCombo.add(_browser.getTranslationRegistry().getText(T_PAGE_NONE, "none"));
            _controlPageCombo.select(0);
        }
         
        _controlPageCombo.setRedraw(true);
    }
    
    private void addAttachment() {
        FileDialog dialog = new FileDialog(_root.getShell(), SWT.MULTI | SWT.OPEN);
        if (dialog.open() == null) return; // cancelled
        String selected[] = dialog.getFileNames();
        String base = dialog.getFilterPath();
        File baseFile = null;
        if ( (base != null) && (base.trim().length() > 0) )
            baseFile = new File(base);
        for (int i = 0; i < selected.length; i++) {
            File cur = null;
            if (base == null)
                cur = new File(selected[i]);
            else
                cur = new File(base, selected[i]);
            if (cur.exists() && cur.isFile() && cur.canRead()) {
                addAttachment(cur);
            }
        }
    }

    private static final Map _extensionToType;
    static {
        _extensionToType = new HashMap();
        _extensionToType.put("png", "image/png");
        _extensionToType.put("jpg", "image/jpg");
        _extensionToType.put("jpeg", "image/jpg");
        _extensionToType.put("gif", "image/gif");
        _extensionToType.put("html", "text/html");
        _extensionToType.put("htm", "text/html");
        _extensionToType.put("txt", "text/plain");
        _extensionToType.put("syndie", "application/x-syndie");
    }
    public static final String guessContentType(String filename) {
        filename = HTMLTag.lowercase(filename);
        int split = filename.lastIndexOf('.');
        if ( (split >= 0) && (split + 1 < filename.length()) ) {
            String type = (String)_extensionToType.get(filename.substring(split+1));
            if (type != null)
                return type;
        }
        return "application/octet-stream";
    } 
    
    private void addAttachment(File file) {
        String fname = file.getName();
        String name = Constants.stripFilename(fname, false);
        String type = guessContentType(fname);
        fname = HTMLTag.lowercase(fname);
        
        if (file.length() > Constants.MAX_ATTACHMENT_SIZE)
            return;
        
        byte data[] = new byte[(int)file.length()];
        try {
            int read = DataHelper.read(new FileInputStream(file), data);
            if (read != data.length) return;
        } catch (IOException ioe) {
            return;
        }
        Properties cfg = new Properties();
        cfg.setProperty(Constants.MSG_ATTACH_CONTENT_TYPE, type);
        cfg.setProperty(Constants.MSG_ATTACH_NAME, name);
        _attachmentConfig.add(cfg);
        _attachments.add(data);
        rebuildAttachmentsCombo();
    }
    int addAttachment(String contentType, String name, byte[] data) {
        int rv = -1;
        Properties cfg = new Properties();
        cfg.setProperty(Constants.MSG_ATTACH_CONTENT_TYPE, contentType);
        cfg.setProperty(Constants.MSG_ATTACH_NAME, name);
        _attachmentConfig.add(cfg);
        _attachments.add(data);
        rv = _attachments.size();
        rebuildAttachmentsCombo();
        return rv;
    }
    private void removeAttachment() {
        if (_attachments.size() > 0) {
            // should this check to make sure there aren't any pages referencing
            // this attachment first?
            int idx = _controlAttachmentCombo.getSelectionIndex();
            _attachments.remove(idx);
            _attachmentConfig.remove(idx);
        }
        rebuildAttachmentsCombo();
    }
    
    private void rebuildAttachmentsCombo() {
        _controlAttachmentCombo.setRedraw(false);
        _controlAttachmentCombo.removeAll();
        if (_attachments.size() > 0) {
            for (int i = 0; i < _attachments.size(); i++) {
                byte data[] = (byte[])_attachments.get(i);
                Properties cfg = (Properties)_attachmentConfig.get(i);
                StringBuffer buf = new StringBuffer();
                buf.append((i+1) + ": ");
                String name = cfg.getProperty(Constants.MSG_ATTACH_NAME);
                if (name != null)
                    buf.append(name).append(" ");
                String type = cfg.getProperty(Constants.MSG_ATTACH_CONTENT_TYPE);
                if (type != null)
                    buf.append('(').append(type).append(") ");
                buf.append("[" + data.length + " bytes]");
                _controlAttachmentCombo.add(buf.toString());
            }
        } else {
            _controlAttachmentCombo.add(_browser.getTranslationRegistry().getText(T_ATTACHMENTS_NONE, "none"));
        }
        _controlAttachmentCombo.select(0);
         
        _controlAttachmentCombo.setRedraw(true);
    }

    int getPageCount() { return _pages.size(); }
    String getPageContent(int page) {
        PageEditor editor = (PageEditor)_pages.get(page);
        return editor.getContent();
    }
    String getPageType(int page) {
        PageEditor editor = (PageEditor)_pages.get(page);
        return editor.getContentType();
    }
    
    List getAttachmentTypes() {
        ArrayList rv = new ArrayList();
        for (int i = 0; i < _attachmentConfig.size(); i++) {
            Properties cfg = (Properties)_attachmentConfig.get(i);
            String type = cfg.getProperty(Constants.MSG_ATTACH_CONTENT_TYPE);
            if (type == null)
                rv.add("application/octet-stream");
            else
                rv.add(type);
        }
        return rv;
    }

    List getAttachmentDescriptions() { return getAttachmentDescriptions(false); }
    List getAttachmentDescriptions(boolean imagesOnly) {
        ArrayList rv = new ArrayList();
        for (int i = 0; i < _attachmentConfig.size(); i++) {
            if (imagesOnly) {
                Properties cfg = (Properties)_attachmentConfig.get(i);
                String type = cfg.getProperty(Constants.MSG_ATTACH_CONTENT_TYPE);
                if ( (type == null) || (!type.startsWith("image")) )
                    continue;
            }
            String item = (String)_controlAttachmentCombo.getItem(i);
            rv.add(item);
        }
        return rv;
    }
    List getAttachmentNames() {
        ArrayList rv = new ArrayList();
        for (int i = 0; i < _attachmentConfig.size(); i++) {
            Properties cfg = (Properties)_attachmentConfig.get(i);
            rv.add(cfg.getProperty(Constants.MSG_ATTACH_NAME));
        }
        return rv;
    }
    byte[] getAttachmentData(int attachment) {
        if ( (attachment <= 0) || (attachment > _attachments.size()) ) return null;
        return (byte[])_attachments.get(attachment-1);
    }
    byte[] getImageAttachment(int idx) {
        int cur = 0;
        for (int i = 0; i < _attachmentConfig.size(); i++) {
            Properties cfg = (Properties)_attachmentConfig.get(i);
            String type = cfg.getProperty(Constants.MSG_ATTACH_CONTENT_TYPE);
            if ( (type == null) || (!type.startsWith("image")) )
                continue;
            if (cur + 1 == idx)
                return (byte[])_attachments.get(i);
            cur++;
        }
        return null;
    }
    int getImageAttachmentNum(int imageNum) { 
        int cur = 0;
        for (int i = 0; i < _attachmentConfig.size(); i++) {
            Properties cfg = (Properties)_attachmentConfig.get(i);
            String type = cfg.getProperty(Constants.MSG_ATTACH_CONTENT_TYPE);
            if ( (type == null) || (!type.startsWith("image")) )
                continue;
            if (cur == imageNum)
                return cur+1;
            cur++;
        }
        return -1;
    }
    void updateImageAttachment(int imageNum, String contentType, byte data[]) { 
        int cur = 0;
        for (int i = 0; i < _attachmentConfig.size(); i++) {
            Properties cfg = (Properties)_attachmentConfig.get(i);
            String type = cfg.getProperty(Constants.MSG_ATTACH_CONTENT_TYPE);
            if ( (type == null) || (!type.startsWith("image")) )
                continue;
            if (cur == imageNum) {
                cfg.setProperty(Constants.MSG_ATTACH_CONTENT_TYPE, contentType);
                _attachments.set(cur, data);
                rebuildAttachmentsCombo();
                return;
            }
            cur++;
        }
    }

    private void forumSelected() {
        int idx = _controlForumCombo.getSelectionIndex();
        if ( (idx < 0) || (idx > _targetList.size()) )
            return;
        if (idx == _targetList.size()) {
            // "other...""
            _refChooser.show();
            return;
        }
        _target = (Hash)_targetList.get(idx);
        getBrowser().getUI().debugMessage("forum selected, setting target to " + _target + " / " + idx);
        updateAuthor();
    }
    private void updateAuthor() {
        _controlAuthor.setRedraw(false);
        _controlForumCombo.setRedraw(false);
        _controlForumCombo.removeAll();
        _targetList.clear();
        while (_controlAuthorMenu.getItemCount() > 0)
            _controlAuthorMenu.getItem(0).dispose();
        
        boolean targetFound = false;
        
        for (int i = 0; i < _nymChannels.getIdentityChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getIdentityChannel(i);
            MenuItem pickAuthor = new MenuItem(_controlAuthorMenu, SWT.PUSH);
            pickAuthor.setText(info.getName() + ": " + info.getChannelHash().toBase64().substring(0,6));
            if (_author == null) {
                _author = info.getChannelHash();
                _controlAuthor.setText(info.getName());
                _controlAuthor.setToolTipText(info.getName() + ": " + info.getDescription());
                pickAuthor.setSelection(true);
            } else if (_author.equals(info.getChannelHash())) {
                _controlAuthor.setText(info.getName());
                _controlAuthor.setToolTipText(info.getName() + ": " + info.getDescription());
                pickAuthor.setSelection(true);
            }
            _targetList.add(info.getChannelHash());
            
            if (_target == null)
                _target = info.getChannelHash();
            
            _controlForumCombo.add("! " + info.getName() + " [" + info.getChannelHash().toBase64().substring(0,6) + "]");
            if ( (_target != null) && (_target.equals(info.getChannelHash()))) {
                _controlForumCombo.select(_controlForumCombo.getItemCount()-1);
                targetFound = true;
            }
        }
        for (int i = 0; i < _nymChannels.getManagedChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getManagedChannel(i);
            _targetList.add(info.getChannelHash());
            _controlForumCombo.add("* " + info.getName() + " [" + info.getChannelHash().toBase64().substring(0,6) + "]");
            if (_target == null)
                _target = info.getChannelHash();
            if ( (_target != null) && (_target.equals(info.getChannelHash()))) {
                _controlForumCombo.select(_controlForumCombo.getItemCount()-1);
                targetFound = true;
            }
        }
        for (int i = 0; i < _nymChannels.getPostChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getPostChannel(i);
            _targetList.add(info.getChannelHash());
            _controlForumCombo.add("= " + info.getName() + " [" + info.getChannelHash().toBase64().substring(0,6) + "]");
            if (_target == null)
                _target = info.getChannelHash();
            if ( (_target != null) && (_target.equals(info.getChannelHash()))) {
                _controlForumCombo.select(_controlForumCombo.getItemCount()-1);
                targetFound = true;
            }
        }
        for (int i = 0; i < _nymChannels.getPublicPostChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getPublicPostChannel(i);
            _targetList.add(info.getChannelHash());
            _controlForumCombo.add("- " + info.getName() + " [" + info.getChannelHash().toBase64().substring(0,6) + "]");
            if (_target == null)
                _target = info.getChannelHash();
            if ( (_target != null) && (_target.equals(info.getChannelHash()))) {
                _controlForumCombo.select(_controlForumCombo.getItemCount()-1);
                targetFound = true;
            }
        }
        
        if (!targetFound && (_target != null)) {
            // other forum chosen
            long id = _client.getChannelId(_target);
            if (id >= 0) {
                ChannelInfo info = _client.getChannel(id);
                _targetList.add(info.getChannelHash());
                _controlForumCombo.add(info.getName() + " [" + info.getChannelHash().toBase64().substring(0,6) + "]");
                _controlForumCombo.select(_controlForumCombo.getItemCount()-1);
            }
        }
        
        _controlForumCombo.add(getBrowser().getTranslationRegistry().getText(T_FORUM_OTHER, "other..."));
                
        _controlAuthor.setRedraw(true);
        _controlForumCombo.setRedraw(true);
    }

    public void referenceAccepted(SyndieURI uri) {
        _refChooser.hide();
        if (uri == null) return;
        _target = uri.getScope();
        getBrowser().getUI().debugMessage("reference accepted, setting target to " + _target);
        updateAuthor();
    }
    
    public void referenceChoiceAborted() {
        _refChooser.hide();
        updateAuthor();
    }
    
    private void pickAvatar() {
        String file = _controlAvatarDialog.open();
        if (file != null) {
            File f = new File(file);
            //ignoring it here, since we scale it down later
            //if (f.length() > Constants.MAX_AVATAR_SIZE)
            //    return;
            _controlAvatar.setRedraw(false);
            try {
                Image img = ImageUtil.createImageFromFile(file);
                Rectangle bounds = img.getBounds();
                if ( (bounds.width != Constants.MAX_AVATAR_WIDTH) || (bounds.height != Constants.MAX_AVATAR_HEIGHT) ) {
                    img = ImageUtil.resize(img, Constants.MAX_AVATAR_WIDTH, Constants.MAX_AVATAR_HEIGHT, true);
                    _controlAvatarImageSource = null;
                } else {
                    _controlAvatarImageSource = file;
                }
                ImageUtil.dispose(_controlAvatarImage);
                _controlAvatarImage = img;
                _controlAvatar.setImage(img);
            } catch (IllegalArgumentException iae) {
                // deal with invalid image
            }
            _controlAvatar.setRedraw(true);
        }
    }

    Hash getAuthor() { return _author; }
    Hash getTarget() { return _target; }
    DBClient getClient() { return _client; }

    private void postMessage() {
        MessageCreator creator = new MessageCreator(this);
        boolean ok = creator.execute();
        if (ok) {
            MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_INFORMATION | SWT.OK);
            box.setMessage(getBrowser().getTranslationRegistry().getText(T_POSTED_MESSAGE, "Message created and imported successfully!  Please be sure to syndicate it to others so they can read it"));
            box.setText(getBrowser().getTranslationRegistry().getText(T_POSTED_TITLE, "Message created!"));
            box.open();
            if (_listener != null)
                _listener.messageCreated(this, creator.getCreatedURI());
        } else {
            MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_ERROR | SWT.OK);
            box.setMessage(getBrowser().getTranslationRegistry().getText(T_POST_ERROR_MESSAGE_PREFIX, "There was an error creating the message.  Please view the log for more information: ") + creator.getErrors());
            box.setText(getBrowser().getTranslationRegistry().getText(T_POST_ERROR_TITLE, "Error creating the message"));
            box.open();
        }
    }
    void postponeMessage() {
        MessageCreator creator = new MessageCreator(this);
        long postponementId = creator.postpone();
        if (_listener != null)
            _listener.messagePostponed(this, postponementId);
    }
    private void cancelMessage() {
        // confirm
        MessageBox dialog = new MessageBox(_root.getShell(), SWT.ICON_WARNING | SWT.YES | SWT.NO);
        dialog.setMessage(getBrowser().getTranslationRegistry().getText(T_CANCEL_MESSAGE, "Are you sure you want to cancel this message?"));
        dialog.setText(getBrowser().getTranslationRegistry().getText(T_CANCEL_TITLE, "Confirm message cancellation"));
        int rv = dialog.open();
        if (rv == SWT.YES) {
            if (_listener != null)
                _listener.messageCancelled(this);
        }
    }

    public void dispose() { 
        editorComplete();
    }
    
    private void editorComplete() {
        _root.setVisible(false);
        // dispose of all of the pages
        while (_pages.size() > 0) {
            PageEditor editor = (PageEditor)_pages.remove(0);
            editor.dispose();
        }
        _attachments.clear();
        _attachmentConfig.clear();
        _root.dispose();
        _refChooser.dispose();
        getBrowser().getTranslationRegistry().unregister(this);
    }
    
    public int getParentCount() { return _parents.size(); }
    public SyndieURI getParent(int depth) { return (SyndieURI)_parents.get(depth); }
    public String getSubject() { return _controlSubjectText.getText(); }
    
    public boolean getPrivacyPublic() { return _controlPrivacyCombo.getSelectionIndex() == PRIVACY_PUBLIC; }
    public boolean getPrivacyPBE() { return _controlPrivacyCombo.getSelectionIndex() == PRIVACY_PBE; }
    public boolean getPrivacyAuthorizedOnly() { return _controlPrivacyCombo.getSelectionIndex() == PRIVACY_AUTHORIZED; }
    public boolean getPrivacyReply() { return _controlPrivacyCombo.getSelectionIndex() == PRIVACY_REPLY; }
    public String getPassphrase() { return _passphrase; }
    public String getPassphrasePrompt() { return _passphrasePrompt; }
    
    public String getAvatarUnmodifiedFilename() { return _controlAvatarImageSource; }
    public byte[] getAvatarModifiedData() {
        if (_controlAvatarImageSource == null) {
            Image img = _controlAvatarImage;
            if ( (img != null) && (!img.isDisposed()) && (img != ImageUtil.ICON_QUESTION) ) {
                byte rv[] = ImageUtil.serializeImage(img);
                getBrowser().getUI().debugMessage("avatar size: " + rv.length + " bytes");
                if (rv.length > Constants.MAX_AVATAR_SIZE)
                    return null;
                else
                    return rv;
            }
        }
        return null;
    }
    public String[] getPublicTags() { 
        String src = _controlTagsText.getText().trim();
        return Constants.split(" \t\r\n", src, false);
    }
    public String[] getPrivateTags() { return new String[0]; }
    public List getReferenceNodes() { return _referenceNodes; }
    
    private static final SimpleDateFormat _dayFmt = new SimpleDateFormat("yyyy/MM/dd");
    public String getExpiration() {
        try {
            synchronized (_dayFmt) {
                // roundtrip, to verify
                Date when = _dayFmt.parse(_controlExpirationText.getText().trim());
                return _dayFmt.format(when);
            }
        } catch (ParseException pe) {
            return null;
        }
    }
    public boolean getForceNewThread() { return false; }
    public boolean getRefuseReplies() { return false; }
    
    private static final String T_TAGS = "syndie.gui.messageeditor.tags";
    private static final String T_FORUM = "syndie.gui.messageeditor.forum";
    private static final String T_CANCEL = "syndie.gui.messageeditor.cancel";
    private static final String T_POSTPONE = "syndie.gui.messageeditor.postpone";
    private static final String T_POST = "syndie.gui.messageeditor.post";
    private static final String T_SUBJECT = "syndie.gui.messageeditor.subject";
    private static final String T_PRIVACY = "syndie.gui.messageeditor.privacy";
    private static final String T_PRIVACY_PUBLIC = "syndie.gui.messageeditor.privacy.public";
    private static final String T_PRIVACY_PBE = "syndie.gui.messageeditor.privacy.pbe";
    private static final String T_PRIVACY_AUTHORIZED = "syndie.gui.messageeditor.privacy.authorized";
    private static final String T_PRIVACY_REPLY = "syndie.gui.messageeditor.privacy.reply";
    private static final String T_PAGE = "syndie.gui.messageeditor.page";
    private static final String T_PAGE_NONE = "syndie.gui.messageeditor.page.none";
    private static final String T_PAGE_ACTION = "syndie.gui.messageeditor.page.action";
    private static final String T_PAGE_ADD_HTML = "syndie.gui.messageeditor.page.addhtml";
    private static final String T_PAGE_ADD_TEXT = "syndie.gui.messageeditor.page.addtext";
    private static final String T_PAGE_REMOVE = "syndie.gui.messageeditor.page.remove";
    private static final String T_ATTACHMENTS = "syndie.gui.messageeditor.attachments";
    private static final String T_ATTACHMENTS_NONE = "syndie.gui.messageeditor.attachments.none";
    private static final String T_ATTACHMENT_ACTION = "syndie.gui.messageeditor.attachments.action";
    private static final String T_ATTACHMENT_ADD = "syndie.gui.messageeditor.attachments.add";
    private static final String T_ATTACHMENT_REMOVE = "syndie.gui.messageeditor.attachments.remove";
    private static final String T_REFERENCES = "syndie.gui.messageeditor.references";
    private static final String T_REFERENCES_NONE = "syndie.gui.messageeditor.references.none";
    private static final String T_EXPIRATION = "syndie.gui.messageeditor.expiration";
    private static final String T_EXPIRATION_NONE = "syndie.gui.messageeditor.expiration.none";
    private static final String T_AUTHOR_UNKNOWN = "syndie.gui.messageeditor.author";
    private static final String T_FORUM_OTHER = "syndie.gui.messageeditor.forum.other";
    private static final String T_POSTED_MESSAGE = "syndie.gui.messageeditor.post.message";
    private static final String T_POSTED_TITLE = "syndie.gui.messageeditor.post.title";
    private static final String T_POST_ERROR_MESSAGE_PREFIX = "syndie.gui.messageeditor.post.errormsg";
    private static final String T_POST_ERROR_TITLE = "syndie.gui.messageeditor.post.errortitle";
    private static final String T_CANCEL_MESSAGE = "syndie.gui.messageeditor.cancel.message";
    private static final String T_CANCEL_TITLE = "syndie.gui.messageeditor.cancel.title";
    
    public void translate(TranslationRegistry registry) {
        _controlTags.setText(registry.getText(T_TAGS, "Tags:"));
        _controlForum.setText(registry.getText(T_FORUM, "Forum:"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel"));
        _save.setText(registry.getText(T_POSTPONE, "Save for later"));
        _ok.setText(registry.getText(T_POST, "Post message!"));
        _controlSubject.setText(registry.getText(T_SUBJECT, "Subject:"));
        _controlPrivacy.setText(registry.getText(T_PRIVACY, "Privacy:"));
        
        // values indexed per:
        //int PRIVACY_PUBLIC = 0;
        //int PRIVACY_AUTHORIZED = 1;
        //int PRIVACY_PBE = 2;
        //int PRIVACY_REPLY = 3;
        int privSelected = PRIVACY_AUTHORIZED;
        if (_controlPrivacyCombo.getItemCount() > 0)
            privSelected = _controlPrivacyCombo.getSelectionIndex();
        _controlPrivacyCombo.setRedraw(false);
        _controlPrivacyCombo.removeAll();
        _controlPrivacyCombo.add(registry.getText(T_PRIVACY_PUBLIC, "Publicly readable"));
        _controlPrivacyCombo.add(registry.getText(T_PRIVACY_AUTHORIZED, "Authorized readers only"));
        _controlPrivacyCombo.add(registry.getText(T_PRIVACY_PBE, "Passphrase protected..."));
        _controlPrivacyCombo.add(registry.getText(T_PRIVACY_REPLY, "Private reply to forum owner"));
        _controlPrivacyCombo.select(privSelected);
        _controlPrivacyCombo.setRedraw(true);
        
        _controlPage.setText(registry.getText(T_PAGE, "Pages:"));
        
        rebuildPagesCombo();
        
        _controlPageAction.setText(registry.getText(T_PAGE_ACTION, "+/-"));
        _pageAddHTML.setText(registry.getText(T_PAGE_ADD_HTML, "Add HTML page"));
        _pageAddText.setText(registry.getText(T_PAGE_ADD_TEXT, "Add text page"));
        _pageRemove.setText(registry.getText(T_PAGE_REMOVE, "Remove current page"));
        _controlAttachment.setText(registry.getText(T_ATTACHMENTS, "Attachments:"));

        rebuildAttachmentsCombo();
        
        _controlAttachmentAction.setText(registry.getText(T_ATTACHMENT_ACTION, "+/-"));
        _attachmentAdd.setText(registry.getText(T_ATTACHMENT_ADD, "Add new file..."));
        _attachmentRemove.setText(registry.getText(T_ATTACHMENT_REMOVE, "Remove current attachment"));
        _controlRef.setText(registry.getText(T_REFERENCES, "References:"));
        _controlRefAction.setText(registry.getText(T_REFERENCES_NONE, "none")); //todo: fix when supported
        _controlExpiration.setText(registry.getText(T_EXPIRATION, "Expiration:"));
        
        if (getExpiration() == null)
            _controlExpirationText.setText(registry.getText(T_EXPIRATION_NONE, "none"));
        
        if (_author == null)
            _controlAuthor.setText(registry.getText(T_AUTHOR_UNKNOWN, "author..."));
        
        _root.pack(true);
    }
}
