package syndie.gui;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

/**
 *
 */
class AttachmentPreviewPopup implements Translatable {
    private BrowserControl _browser;
    private DBClient _client;
    private Shell _parent;
    private Shell _shell;
    private Label _nameLabel;
    private Text _name;
    private Label _descLabel;
    private Text _desc;
    private Label _sizeLabel;
    private Text _size;
    private Label _typeLabel;
    private Text _type;
    private ImageCanvas _preview;
    private Label _saveAsLabel;
    private Text _saveAs;
    private Button _saveAsBrowse;
    private Button _saveAsOk;
    private Button _cancel;
    
    private FileDialog _dialog;

    private byte _data[];

    public AttachmentPreviewPopup(BrowserControl browser, Shell parent) {
        _browser = browser;
        _client = browser.getClient();
        _parent = parent;
        initComponents();
    }
    
    private void initComponents() {
        _shell = new Shell(_parent, SWT.SHELL_TRIM | SWT.APPLICATION_MODAL);
        _shell.setLayout(new GridLayout(4, false));
        
        _nameLabel = new Label(_shell, SWT.NONE);
        _nameLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _name = new Text(_shell, SWT.SINGLE | SWT.READ_ONLY | SWT.BORDER);
        _name.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _preview = new ImageCanvas(_shell, false);
        _preview.setLayoutData(new GridData(GridData.BEGINNING, GridData.BEGINNING, false, false, 2, 4));
        _preview.forceSize(64, 64);
        
        _descLabel = new Label(_shell, SWT.NONE);
        _descLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _desc = new Text(_shell, SWT.MULTI | SWT.READ_ONLY | SWT.WRAP | SWT.BORDER);
        _desc.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _sizeLabel = new Label(_shell, SWT.NONE);
        _sizeLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _size = new Text(_shell, SWT.SINGLE | SWT.READ_ONLY | SWT.BORDER);
        _size.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _typeLabel = new Label(_shell, SWT.NONE);
        _typeLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _type = new Text(_shell, SWT.SINGLE | SWT.READ_ONLY | SWT.BORDER);
        _type.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _saveAsLabel = new Label(_shell, SWT.NONE);
        _saveAsLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _saveAs = new Text(_shell, SWT.SINGLE | SWT.BORDER);
        _saveAs.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _saveAsBrowse = new Button(_shell, SWT.PUSH);
        _saveAsBrowse.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _saveAsBrowse.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { browse(); }
            public void widgetSelected(SelectionEvent selectionEvent) { browse(); }
        });
        _saveAsOk = new Button(_shell, SWT.PUSH);
        _saveAsOk.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _saveAsOk.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { save(); }
            public void widgetSelected(SelectionEvent selectionEvent) { save(); }
        });
        
        _cancel = new Button(_shell, SWT.PUSH);
        _cancel.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 4, 1));
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { cancel(); }
            public void widgetSelected(SelectionEvent selectionEvent) { cancel(); }
        });
        
        // intercept the shell closing, since that'd cause the shell to be disposed rather than just hidden
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; cancel(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        _dialog = new FileDialog(_shell, SWT.SAVE);
        _browser.getTranslationRegistry().register(this);
    }
    
    public void showURI(SyndieURI uri) {
        long scope = _client.getChannelId(uri.getScope());
        long msgId = _client.getMessageId(scope, uri.getMessageId().longValue());
        int internalAttachmentNum = uri.getAttachment().intValue()-1;
        Properties cfg = _client.getMessageAttachmentConfig(msgId, internalAttachmentNum);
        int bytes = _client.getMessageAttachmentSize(msgId, internalAttachmentNum);
        
        if (cfg.containsKey(Constants.MSG_ATTACH_NAME))
            _name.setText(cfg.getProperty(Constants.MSG_ATTACH_NAME));
        else
            _name.setText("");
        if (cfg.containsKey(Constants.MSG_ATTACH_DESCRIPTION))
            _desc.setText(cfg.getProperty(Constants.MSG_ATTACH_DESCRIPTION));
        else
            _desc.setText("");
        
        String type = cfg.getProperty(Constants.MSG_ATTACH_CONTENT_TYPE);
        if (type == null)
            type = "application/octet-stream";
        _type.setText(type);
        
        _size.setText((bytes+1023)/1024 + " KB");
        
        _data = _client.getMessageAttachmentData(msgId, internalAttachmentNum);
        showPreviewIfPossible(type, _data);
        
        _saveAs.setText(_name.getText());
        
        _shell.pack();
        _shell.open();
    }
    
    private void showPreviewIfPossible(String contentType, byte data[]) {
        Image old = _preview.getImage();
        ImageUtil.dispose(old);
        boolean show = false;
        if (contentType.startsWith("image/") && (data != null)) {
            Image img = ImageUtil.createImage(data);
            if (img != null) {
                _preview.setImage(img);
                show = true;
            } else {
                show = false;
            }
        }
        GridData gd = (GridData)_preview.getLayoutData();
        if (show) {
            _preview.setVisible(true);
            _browser.getUI().debugMessage("preview size: " + _preview.getSize() + " computed: " + _preview.computeSize(SWT.DEFAULT, SWT.DEFAULT));
            //gd.exclude = false;
        } else {
            _preview.setImage(null);
            _preview.setVisible(false);
            //gd.exclude = true;
        }
    }
    
    private void cancel() {
        _shell.setVisible(false);
    }
    private void browse() {
        _dialog.setFileName(_saveAs.getText());
        String filename = _dialog.open();
        if (filename != null)
            _saveAs.setText(filename);
    }
    private void save() {
        FileOutputStream fos = null;
        String fname = _saveAs.getText().trim();
        File out = new File(fname);
        try {
            fos = new FileOutputStream(out);
            fos.write(_data);
            fos.close();
            fos = null;
            MessageBox box = new MessageBox(_shell, SWT.OK | SWT.ICON_INFORMATION);
            box.setText(_browser.getTranslationRegistry().getText(T_SAVE_OK_TITLE, "Attachment saved"));
            box.setMessage(_browser.getTranslationRegistry().getText(T_SAVE_OK_MSG, "Attachment saved to:") + out.getAbsolutePath());
            box.open();
            _shell.setVisible(false);
        } catch (IOException ioe) {
            // hrm
            MessageBox box = new MessageBox(_shell, SWT.OK | SWT.ICON_ERROR);
            box.setText(_browser.getTranslationRegistry().getText(T_SAVE_ERROR_TITLE, "Error saving attachment"));
            box.setMessage(_browser.getTranslationRegistry().getText(T_SAVE_ERROR_MSG, "Attachment could not be saved: ") + ioe.getMessage());
            box.open();
        }
    }
    
    public void dispose() { _browser.getTranslationRegistry().unregister(this); }
    
    private static final String T_SAVE_OK_TITLE = "syndie.gui.attachmentpreviewpopup.save.ok.title";
    private static final String T_SAVE_OK_MSG = "syndie.gui.attachmentpreviewpopup.save.ok.msg";
    private static final String T_SAVE_ERROR_TITLE = "syndie.gui.attachmentpreviewpopup.save.error.title";
    private static final String T_SAVE_ERROR_MSG = "syndie.gui.attachmentpreviewpopup.save.error.msg";
    private static final String T_TITLE = "syndie.gui.attachmentpreviewpopup.title";
    private static final String T_NAME = "syndie.gui.attachmentpreviewpopup.name";
    private static final String T_DESC = "syndie.gui.attachmentpreviewpopup.desc";
    private static final String T_SIZE = "syndie.gui.attachmentpreviewpopup.size";
    private static final String T_TYPE = "syndie.gui.attachmentpreviewpopup.type";
    private static final String T_SAVEAS = "syndie.gui.attachmentpreviewpopup.saveas";
    private static final String T_SAVEAS_BROWSE = "syndie.gui.attachmentpreviewpopup.saveas.browse";
    private static final String T_SAVE = "syndie.gui.attachmentpreviewpopup.save";
    private static final String T_CANCEL = "syndie.gui.attachmentpreviewpopup.cancel";
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(_browser.getTranslationRegistry().getText(T_TITLE, "Preview attachment"));
        _nameLabel.setText(_browser.getTranslationRegistry().getText(T_NAME, "Name:"));
        _descLabel.setText(_browser.getTranslationRegistry().getText(T_DESC, "Description:"));
        _sizeLabel.setText(_browser.getTranslationRegistry().getText(T_SIZE, "Size:"));
        _typeLabel.setText(_browser.getTranslationRegistry().getText(T_TYPE, "Type:"));
        _saveAsLabel.setText(_browser.getTranslationRegistry().getText(T_SAVEAS, "Save as:"));
        _saveAsBrowse.setText(_browser.getTranslationRegistry().getText(T_SAVEAS_BROWSE, "Browse..."));
        _saveAsOk.setText(_browser.getTranslationRegistry().getText(T_SAVE, "Save"));
        _cancel.setText(_browser.getTranslationRegistry().getText(T_CANCEL, "Cancel"));
    }
}
