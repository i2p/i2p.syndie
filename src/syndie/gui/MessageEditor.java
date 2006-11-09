package syndie.gui;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import net.i2p.data.DataHelper;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
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
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Label;
import syndie.Constants;
import syndie.data.Enclosure;
import syndie.data.EnclosureBody;
import syndie.db.CommandImpl;
import syndie.db.DBClient;

/**
 *
 */
public class MessageEditor {
    private DBClient _client;
    /** list of (byte[]) instances */
    private ArrayList _attachments;
    /** list of Properties associated with each attachment */
    private ArrayList _attachmentConfig;
    /** list of PageEditor instances */
    private ArrayList _pages;

    private Composite _parent;
    private Composite _root;
    private Composite _pageRoot;
    private StackLayout _pageRootLayout;
    private Composite _actions;
    
    private Composite _msgControl;
    private Label _controlAvatar;
    private Image _controlAvatarImage;
    private Text _controlAuthor;
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
    
    /** Creates a new instance of MessageEditor */
    public MessageEditor(DBClient client, Composite parent) {
        _client = client;
        _parent = parent;
        _pages = new ArrayList();
        _attachments = new ArrayList();
        _attachmentConfig = new ArrayList();
        initComponents();
    }
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
        new Button(_actions, SWT.PUSH).setText("Post message!");
        new Button(_actions, SWT.PUSH).setText("Save for later");
        new Button(_actions, SWT.PUSH).setText("Cancel");
    }
    
    private void initControlBar() {
        _msgControl = new Composite(_root, SWT.BORDER);
        _msgControl.setLayout(new GridLayout(3, false));
        _msgControl.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _controlAvatarImage = Display.getDefault().getSystemImage(SWT.ICON_QUESTION);
        _controlAvatar = new Label(_msgControl, SWT.BORDER);
        _controlAvatar.setImage(_controlAvatarImage);
        _controlAvatar.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false, 1, 2));
        
        _controlSubject = new Label(_msgControl, SWT.NONE);
        _controlSubject.setText("Subject:");
        _controlSubject.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _controlSubjectText = new Text(_msgControl, SWT.SINGLE | SWT.BORDER);
        _controlSubjectText.setText("");
        _controlSubjectText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _controlForum = new Label(_msgControl, SWT.NONE);
        _controlForum.setText("Forum:");
        _controlForum.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        Composite line2 = new Composite(_msgControl, SWT.NONE);
        line2.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        GridLayout gl = new GridLayout(5, false);
        //gl.horizontalSpacing = 0;
        gl.marginWidth = 0;
        gl.verticalSpacing = 0;
        line2.setLayout(gl);
        
        _controlForumCombo = new Combo(line2, SWT.SIMPLE);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        gd.widthHint = 100;
        _controlForumCombo.setLayoutData(gd);
        
        _controlTags = new Label(line2, SWT.NONE);
        _controlTags.setText("Tags:");
        _controlTags.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _controlTagsText = new Text(line2, SWT.BORDER | SWT.SINGLE);
        _controlTagsText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _controlPrivacy = new Label(line2, SWT.NONE);
        _controlPrivacy.setText("Privacy:");
        _controlPrivacy.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _controlPrivacyCombo = new Combo(line2, SWT.SIMPLE);
        _controlPrivacyCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _controlPrivacyCombo.add("Publicly readable");
        _controlPrivacyCombo.add("Authorized readers only");
        _controlPrivacyCombo.add("Passphrase protected...");
        _controlPrivacyCombo.add("Private reply to forum owner");
        _controlPrivacyCombo.select(0);
        
        // author is under the avatar
        _controlAuthor = new Text(_msgControl, SWT.SINGLE | SWT.BORDER);
        _controlAuthor.setText("Author...");
        _controlAuthor.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        
        _controlPage = new Label(_msgControl, SWT.NONE);
        _controlPage.setText("Pages:");
        _controlPage.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        Composite line3 = new Composite(_msgControl, SWT.NONE);
        gl = new GridLayout(9, false);
        //gl.horizontalSpacing = 0;
        gl.marginWidth = 0;
        gl.verticalSpacing = 0;
        line3.setLayout(gl);
        line3.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _controlPageCombo = new Combo(line3, SWT.SIMPLE);
        _controlPageCombo.add("none");
        _controlPageCombo.select(0);
        _controlPageCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _controlPageCombo.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { if (_pages.size() > 0) showPage(_controlPageCombo.getSelectionIndex()); }
            public void widgetSelected(SelectionEvent selectionEvent) { if (_pages.size() > 0) showPage(_controlPageCombo.getSelectionIndex()); }
        });
        
        _controlPageAction = new Button(line3, SWT.PUSH);
        _controlPageAction.setText("+/-");
        _controlPageAction.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _controlPageMenu = new Menu(_controlPageAction);
        MenuItem pageAddHTML = new MenuItem(_controlPageMenu, SWT.PUSH);
        pageAddHTML.setText("Add HTML page");
        pageAddHTML.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addPage("text/html"); }
            public void widgetSelected(SelectionEvent selectionEvent) { addPage("text/html"); }
        });
        MenuItem pageAddText = new MenuItem(_controlPageMenu, SWT.PUSH);
        pageAddText.setText("Add text page");
        pageAddText.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addPage("text/plain"); }
            public void widgetSelected(SelectionEvent selectionEvent) { addPage("text/plain"); }
        });
        new MenuItem(_controlPageMenu, SWT.SEPARATOR);
        MenuItem pageRemove = new MenuItem(_controlPageMenu, SWT.PUSH);
        pageRemove.setText("Remove current page");
        pageRemove.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { removePage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { removePage(); }
        });
        _controlPageAction.setMenu(_controlPageMenu);
        _controlPageAction.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _controlPageMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _controlPageMenu.setVisible(true); }
        });
        
        _controlAttachment = new Label(line3, SWT.NONE);
        _controlAttachment.setText("Attachments:");
        _controlAttachment.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _controlAttachmentCombo = new Combo(line3, SWT.SIMPLE);
        _controlAttachmentCombo.add("none");
        _controlAttachmentCombo.select(0);
        _controlAttachmentCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _controlAttachmentAction = new Button(line3, SWT.PUSH);
        _controlAttachmentAction.setText("+/-");
        _controlAttachmentAction.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _controlAttachmentMenu = new Menu(_controlAttachmentAction);
        _controlAttachmentAction.setMenu(_controlAttachmentMenu);
        _controlAttachmentAction.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _controlAttachmentMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _controlAttachmentMenu.setVisible(true); }
        });
        MenuItem attachmentAdd = new MenuItem(_controlAttachmentMenu, SWT.PUSH);
        attachmentAdd.setText("Add new file...");
        attachmentAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addAttachment(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addAttachment(); }
        });
        new MenuItem(_controlAttachmentMenu, SWT.SEPARATOR);
        MenuItem attachmentRemove = new MenuItem(_controlAttachmentMenu, SWT.PUSH);
        attachmentRemove.setText("Remove current attachment");
        attachmentRemove.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { removeAttachment(); }
            public void widgetSelected(SelectionEvent selectionEvent) { removeAttachment(); }
        });
        
        _controlRef = new Label(line3, SWT.NONE);
        _controlRef.setText("References:");
        _controlRef.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _controlRefAction = new Button(line3, SWT.PUSH);
        _controlRefAction.setText("none");
        _controlRefAction.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _controlExpiration = new Label(line3, SWT.NONE);
        _controlExpiration.setText("Expiration:");
        _controlExpiration.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _controlExpirationText = new Text(line3, SWT.BORDER | SWT.SINGLE);
        _controlExpirationText.setText("none");
        gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        gd.widthHint = 50;
        _controlExpirationText.setLayoutData(gd);
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
        _pages.add(new PageEditor(_client, _pageRoot, this, type));
        showPage(_pages.size()-1);
        rebuildPagesCombo();
    }
    public void removePage() {
        if (_pages.size() > 0) {
            int idx = _controlPageCombo.getSelectionIndex();
            _pages.remove(idx);
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
            _controlPageCombo.add("none");
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
    
    private void addAttachment(File file) {
        String fname = file.getName();
        String name = Constants.stripFilename(fname, false);
        String type = "application/octet-stream";
        fname = HTMLTag.lowercase(fname);
        int split = fname.lastIndexOf('.');
        if ( (split >= 0) && (split + 1 < fname.length()) ) {
            String ntype = (String)_extensionToType.get(fname.substring(split+1));
            if (ntype != null)
                type = ntype;
        }
        
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
            _controlAttachmentCombo.add("none");
        }
        _controlAttachmentCombo.select(0);
         
        _controlAttachmentCombo.setRedraw(true);
    }
}
