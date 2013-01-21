package syndie.gui;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;

import net.i2p.util.SecureFileOutputStream;
import net.i2p.data.DataHelper;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.SyndieURI;
import syndie.data.Timer;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 *  The tab with the details on the left 20% and the image on the right 80%.
 *
 *  Parent is a MessageViewBody CTabFolder.
 */
class AttachmentPreview extends BaseComponent implements Translatable, Themeable {
    private final Composite _parent;
    private Composite _root;
    private Label _message;
    private Label _error;
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
    
    private FileDialog _dialog;

    /** If non-null, showURI() has been called. Hold onto the source so we can save data later
     *  even if we didn't load it.
     */
    private AttachmentSource _source;
    private SyndieURI _uri;

    private static final long MAX_SIZE_TO_DISPLAY = 512 * 1024;

    public AttachmentPreview(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite parent) {
        super(client, ui, themes, trans);
        _parent = parent;
        initComponents();
    }
    
    /** NOTE - params are zero-based */
    public interface AttachmentSource {
        public Properties getAttachmentConfig(int attachmentNum);
        /** @since 1.102b-11 */
        public long getAttachmentSize(int attachmentNum);
        public byte[] getAttachmentData(int attachmentNum);
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        // why does 2 not work?
        _root.setLayout(new GridLayout(3, false));
        
        Composite left = new Composite(_root, SWT.NONE);
        left.setLayout(new GridLayout(1, false));
        GridData gd = new GridData(GridData.BEGINNING, GridData.FILL, false, false);
        gd.widthHint = 240;
        left.setLayoutData(gd);

        // top
        _nameLabel = new Label(left, SWT.NONE);
        _nameLabel.setLayoutData(new GridData(GridData.BEGINNING, GridData.BEGINNING, false, false));

        _name = new Text(left, SWT.SINGLE | SWT.READ_ONLY | SWT.BORDER);
        _name.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        
        _descLabel = new Label(left, SWT.NONE);
        _descLabel.setLayoutData(new GridData(GridData.BEGINNING, GridData.BEGINNING, false, false));
        _desc = new Text(left, SWT.MULTI | SWT.READ_ONLY | SWT.WRAP | SWT.BORDER);
        _desc.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        
        _sizeLabel = new Label(left, SWT.NONE);
        _sizeLabel.setLayoutData(new GridData(GridData.BEGINNING, GridData.BEGINNING, false, false));
        _size = new Text(left, SWT.SINGLE | SWT.READ_ONLY | SWT.BORDER);
        _size.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        
        _typeLabel = new Label(left, SWT.NONE);
        _typeLabel.setLayoutData(new GridData(GridData.BEGINNING, GridData.BEGINNING, false, false));
        _type = new Text(left, SWT.SINGLE | SWT.READ_ONLY | SWT.BORDER);
        _type.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false));
        
        // spacer
        Label dummy = new Label(left, SWT.NONE);
        dummy.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        dummy.setText("");

        // middle
        _message = new Label(left, SWT.NONE);
        _message.setLayoutData(new GridData(GridData.BEGINNING, GridData.FILL, true, true));
        _message.setText("");
        _error = new Label(left, SWT.NONE);
        _error.setLayoutData(new GridData(GridData.BEGINNING, GridData.FILL, true, true));
        _error.setText("");

        // spacer
        dummy = new Label(left, SWT.NONE);
        dummy.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        dummy.setText("");

        // bottom
        _saveAsLabel = new Label(left, SWT.NONE);
        _saveAsLabel.setLayoutData(new GridData(GridData.BEGINNING, GridData.END, true, true));
        _saveAs = new Text(left, SWT.SINGLE | SWT.BORDER);
        _saveAs.setLayoutData(new GridData(GridData.FILL, GridData.END, false, false));

        Composite buttons = new Composite(left, SWT.NONE);
        buttons.setLayout(new GridLayout(2, true));
        buttons.setLayoutData(new GridData(GridData.FILL, GridData.END, false, false));

        _saveAsBrowse = new Button(buttons, SWT.PUSH);
        _saveAsBrowse.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _saveAsBrowse.addSelectionListener(new FireSelectionListener() {
            public void fire() { browse(); }
        });
        _saveAsOk = new Button(buttons, SWT.PUSH);
        _saveAsOk.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _saveAsOk.addSelectionListener(new FireSelectionListener() {
            public void fire() { save(); }
        });

        // right
        _preview = new ImageCanvas(_root, true);
        _preview.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 4));
        //_preview.forceSize(64, 64);
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    private Shell _maxShell;
    private ImageCanvas _maxImage;
    
    public void maximize() {
        if ( (_preview == null) || (_preview.getImage() == null) ) return;
        if (_maxShell != null) {
            unmax();
            return;
        }
        
        _maxShell = new Shell(_root.getShell(), SWT.NO_TRIM | SWT.PRIMARY_MODAL);
        _maxShell.setLayout(new GridLayout(1, true));
        Button unmax = new Button(_maxShell, SWT.PUSH);
        unmax.setText(_translationRegistry.getText("Restore normal size"));
        unmax.setFont(_themeRegistry.getTheme().BUTTON_FONT);
        unmax.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));

        _maxImage = new ImageCanvas(_maxShell, true);
        _maxImage.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 1, 1));
        _maxImage.setImage(_preview.getImage());

        Monitor mon[] = _root.getDisplay().getMonitors();
        Rectangle rect = null;
        if ( (mon != null) && (mon.length > 1) )
            rect = mon[0].getClientArea();
        else
            rect = _parent.getDisplay().getClientArea();
        _maxShell.setSize(rect.width, rect.height);
        _maxShell.setMaximized(true);

        _maxShell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) {
                evt.doit = false;
                unmax();
            }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });

        unmax.addSelectionListener(new FireSelectionListener() {
            public void fire() { unmax(); }
        });

        _maxShell.open();
    }

    private void unmax() {
        if ( (_maxShell != null) && (!_maxShell.isDisposed()) )
            _maxShell.dispose();
        _maxShell = null;
        _maxImage = null; // leave the image disposal to the _preview
    }

    public SyndieURI getURI() { return _uri; }
    
    //public void showURI(SyndieURI uri) {
    
    public void showURI(AttachmentSource source, SyndieURI uri) {
        if (_source != null) return;
        if (_root.isDisposed()) return;
        _source = source;
        _uri = uri;
        _ui.debugMessage("show URI: " + uri + " source=" + source);
        Timer timer = new Timer("show attachment", _ui);
        Long id = uri.getAttachment();
        if (id == null) return;
        int internalAttachmentNum = id.intValue()-1;
        Properties cfg = source.getAttachmentConfig(internalAttachmentNum);
        long bytes = source.getAttachmentSize(internalAttachmentNum);
        timer.addEvent("size fetched");
        
        String name = cfg.getProperty(Constants.MSG_ATTACH_NAME);
        if (name != null && name.length() > 0) {
            _name.setText(name);
        } else {
            _nameLabel.setVisible(false);
            _name.setVisible(false);
        }

        String desc = cfg.getProperty(Constants.MSG_ATTACH_DESCRIPTION);
        if (desc != null && desc.length() > 0) {
            _desc.setText(desc);
        } else {
            _descLabel.setVisible(false);
            _desc.setVisible(false);
        }
        
        String type = cfg.getProperty(Constants.MSG_ATTACH_CONTENT_TYPE);
        if (type != null && type.length() > 0) {
            _type.setText(type);
        } else {
            _typeLabel.setVisible(false);
            _type.setVisible(false);
        }
        
        _size.setText(DataHelper.formatSize2(bytes).replace("&nbsp;", " ") + 'B');
        
        timer.addEvent("options displayed");
        timer.addEvent("data fetched");
        showPreviewIfPossible(type, bytes, source, internalAttachmentNum, timer);
        timer.addEvent("data displayed");
        
        _saveAs.setText(_name.getText());
        timer.complete();
        
        //_shell.pack();
        //_shell.open();
    }
    
    /**
     *  Don't load the data into memory until here
     */
    private void showPreviewIfPossible(String contentType, long size, AttachmentSource source,
                                       int internalAttachmentNum, Timer timer) {
        Image old = _preview.getImage();
        ImageUtil.dispose(old);
        timer.addEvent("old disposed");
        boolean show = false;
        if (contentType.startsWith("image/") && size <= MAX_SIZE_TO_DISPLAY) {
            // NOW we get the data
            // TODO streams/blobs
            byte[] data = source.getAttachmentData(internalAttachmentNum);
            if (data != null) {
                if (data.length == size) {
                    // we don't save the raw data; must load again to save-as.
                    Image img = ImageUtil.createImage(data, _client.getTempDir());
                    timer.addEvent("new image created");
                    if (img != null) {
                        _preview.setImage(img);
                        timer.addEvent("new image set on preview");
                        show = true;
                    } else {
                        _error.setText("Unable to display image");
                        _ui.errorMessage("Bad image");
                    }
                } else {
                    _error.setText("Bad attachment length " + data.length + " expected " + size);
                    _ui.errorMessage("Bad attachment length " + data.length + " expected " + size);
                }
            } else {
                _error.setText(_translationRegistry.getText("Unable to load attachment"));
                _ui.errorMessage("unable to load attachment");
            }
        }
        if (show) {
            _preview.setVisible(true);
            timer.addEvent("new image preview shown");
            _ui.debugMessage("preview size: " + _preview.getSize() + " computed: " + _preview.computeSize(SWT.DEFAULT, SWT.DEFAULT));
            //gd.exclude = false;
        } else {
            _ui.debugMessage("not loading preview");
            // FIXME not being displayed
            _message.setText(_translationRegistry.getText("No preview available"));
            _preview.setImage(null);
            _preview.setVisible(false);
            //gd.exclude = true;
        }
    }
    
    private void browse() {
        if (_dialog == null)
            _dialog = new FileDialog(_root.getShell(), SWT.SAVE);
        _dialog.setFileName(_saveAs.getText());
        String filename = _dialog.open();
        if (filename != null)
            _saveAs.setText(filename);
    }

    /**
     *  Loads or reloads the data into memory here
     */
    private void save() {
        FileOutputStream fos = null;
        String fname = _saveAs.getText().trim();
        File out = new File(fname);
        try {
            Long id = _uri.getAttachment();
            if (id == null)
                throw new IOException("Unable to get attachment from URI");
            // TODO streams/blobs
            byte[] data = _source.getAttachmentData(id.intValue() - 1);
            if (data == null)
                throw new IOException("Unable to load attachment from database");
            fos = new SecureFileOutputStream(out);
            fos.write(data);
            fos.close();
            fos = null;
            MessageBox box = new MessageBox(_root.getShell(), SWT.OK | SWT.ICON_INFORMATION);
            box.setText(_translationRegistry.getText("Attachment saved"));
            box.setMessage(_translationRegistry.getText("Attachment saved to") + ':' + out.getAbsolutePath());
            box.open();
        } catch (IOException ioe) {
            // hrm
            MessageBox box = new MessageBox(_root.getShell(), SWT.OK | SWT.ICON_ERROR);
            box.setText(_translationRegistry.getText("Error saving attachment"));
            box.setMessage(_translationRegistry.getText("Attachment could not be saved") + ": " + ioe.getMessage());
            box.open();
        } finally {
            if (fos != null) try { fos.close(); } catch (IOException ioe) {}
        }
    }
    
    public void dispose() { 
        _translationRegistry.unregister(this); 
        _themeRegistry.unregister(this);
        unmax();
        _preview.disposeImage();
    }
    
    
    public void translate(TranslationRegistry registry) {
        _nameLabel.setText(registry.getText("Name") + ':');
        _descLabel.setText(registry.getText("Description") + ':');
        _sizeLabel.setText(registry.getText("Size") + ':');
        _typeLabel.setText(registry.getText("Type") + ':');
        _saveAsLabel.setText(registry.getText("Save as") + ':');
        _saveAsBrowse.setText(registry.getText("Browse") + "...");
        _saveAsOk.setText(registry.getText("Save"));
    }
    
    public void applyTheme(Theme theme) {
        _desc.setFont(theme.CONTENT_FONT);
        _descLabel.setFont(theme.DEFAULT_FONT);
        _name.setFont(theme.CONTENT_FONT);
        _nameLabel.setFont(theme.DEFAULT_FONT);
        _saveAs.setFont(theme.CONTENT_FONT);
        _saveAsBrowse.setFont(theme.BUTTON_FONT);
        _saveAsLabel.setFont(theme.DEFAULT_FONT);
        _saveAsOk.setFont(theme.BUTTON_FONT);
        _size.setFont(theme.CONTENT_FONT);
        _sizeLabel.setFont(theme.DEFAULT_FONT);
        _type.setFont(theme.CONTENT_FONT);
        _typeLabel.setFont(theme.DEFAULT_FONT);
    }
}
