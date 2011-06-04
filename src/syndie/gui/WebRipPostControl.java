package syndie.gui;

import java.util.ArrayList;
import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Text;
import syndie.data.ChannelInfo;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 * rip a web page to create a brand new post
 */
public class WebRipPostControl extends WebRipPageControl {
    private MenuItem _privacyPublic;
    private MenuItem _privacyPBE;
    private MenuItem _privacyAuthorized;
    private MenuItem _tagWithURL;
    
    private Label _tagLabel;
    private Text _tag;
    private Label _targetLabel;
    private Combo _target;
    private Label _authorLabel;
    private Combo _author;
    
    private List _targetList;
    private List _authorList;
    
    private String _passphrase;
    private String _passphrasePrompt;
    
    public WebRipPostControl(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite parent) {
        super(client, ui, themes, trans, parent);
    }
    
    /** anyone can read it */
    public static final int PRIV_PUBLIC = 1;
    /** passphrase encrypted */
    public static final int PRIV_PBE = 2;
    /** authorized readers only */
    public static final int PRIV_AUTH = 3;
    public int getPrivacy() {
        if (_privacyPublic.getSelection())
            return PRIV_PUBLIC;
        else if (_privacyPBE.getSelection())
            return PRIV_PBE;
        else
            return PRIV_AUTH;
    }
    
    public String getPassphrase() { return _passphrase; }
    public String getPassphrasePrompt() { return _passphrasePrompt; }
    
    protected void createOptions(Menu optionMenu) {
        super.createOptions(optionMenu);
        new MenuItem(optionMenu, SWT.SEPARATOR);
        _tagWithURL = new MenuItem(optionMenu, SWT.CHECK);
        _tagWithURL.setSelection(true);
        new MenuItem(optionMenu, SWT.SEPARATOR);
        _privacyPublic = new MenuItem(optionMenu, SWT.RADIO);
        _privacyPBE = new MenuItem(optionMenu, SWT.RADIO);
        _privacyPBE.setEnabled(false);
        _privacyAuthorized = new MenuItem(optionMenu, SWT.RADIO);
        
        _privacyAuthorized.setSelection(true);
    }
    
    protected void createAttributeFields(Composite root) {
        super.createAttributeFields(root);
        
        _tagLabel = new Label(root, SWT.NONE);
        _tagLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _tag = new Text(root, SWT.SINGLE | SWT.WRAP | SWT.BORDER);
        _tag.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _targetLabel = new Label(root, SWT.NONE);
        _targetLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _target = new Combo(root, SWT.DROP_DOWN | SWT.READ_ONLY);
        _target.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _authorLabel = new Label(root, SWT.NONE);
        _authorLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _author = new Combo(root, SWT.DROP_DOWN | SWT.READ_ONLY);
        _author.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        populateCombos();
    }
    
    public Hash getTarget() {
        int idx = _target.getSelectionIndex();
        if (idx < 0)
            idx = 0;
        if ( (_targetList.size() > 0) && (_targetList.size() > idx) ) {
            Hash rv = (Hash)_targetList.get(idx);
            return rv;
        } else {
            return null;
        }
    }
    public Hash getAuthor() {
        int idx = _author.getSelectionIndex();
        if (idx < 0)
            idx = 0;
        if ( (_authorList.size() > 0) && (_authorList.size() > idx) ) {
            Hash rv = (Hash)_authorList.get(idx);
            return rv;
        } else {
            return null;
        }
    }
    public String getTags() { return _tag.getText() + (_tagWithURL.getSelection() ? " webrip:" + getURL() : ""); }
    
    private void populateCombos() {
        _target.setRedraw(false);
        _author.setRedraw(false);
        _target.removeAll();
        _author.removeAll();
        if (_targetList != null)
            _targetList.clear();
        else
            _targetList = new ArrayList();
        if (_authorList != null)
            _authorList.clear();
        else
            _authorList = new ArrayList();
        
        DBClient.ChannelCollector channels = _client.getChannels(true, true, true, true);
        
        for (int i = 0; i < channels.getIdentityChannelCount(); i++) {
            ChannelInfo info = channels.getIdentityChannel(i);
            Hash h = info.getChannelHash();
            _targetList.add(h);
            _authorList.add(h);
            StringBuilder buf = new StringBuilder();
            buf.append("! "); // identity row
            if (info.getName() != null)
                buf.append(info.getName()).append(" ");
            buf.append("[").append(h.toBase64().substring(0,6)).append("]");
            _target.add(buf.toString());
            _author.add(buf.toString());
        }
        for (int i = 0; i < channels.getManagedChannelCount(); i++) {
            ChannelInfo info = channels.getManagedChannel(i);
            _targetList.add(info.getChannelHash());
            StringBuilder buf = new StringBuilder();
            buf.append("* "); // managed row
            if (info.getName() != null)
                buf.append(info.getName()).append(" ");
            buf.append("[").append(info.getChannelHash().toBase64().substring(0,6)).append("]");
            _target.add(buf.toString());
        }
        for (int i = 0; i < channels.getPostChannelCount(); i++) {
            ChannelInfo info = channels.getPostChannel(i);
            _targetList.add(info.getChannelHash());
            StringBuilder buf = new StringBuilder();
            buf.append("= "); // postable row
            if (info.getName() != null)
                buf.append(info.getName()).append(" ");
            buf.append("[").append(info.getChannelHash().toBase64().substring(0,6)).append("]");
            _target.add(buf.toString());
        }
        for (int i = 0; i < channels.getPublicPostChannelCount(); i++) {
            ChannelInfo info = channels.getPublicPostChannel(i);
            _targetList.add(info.getChannelHash());
            StringBuilder buf = new StringBuilder();
            buf.append("- "); // publicly postable row
            if (info.getName() != null)
                buf.append(info.getName()).append(" ");
            buf.append("[").append(info.getChannelHash().toBase64().substring(0,6)).append("]");
            _target.add(buf.toString());
        }
        
        if (_target.getItemCount() > 0)
            _target.select(0);
        if (_author.getItemCount() > 0)
            _author.select(0);
        
        _target.setRedraw(true);
        _author.setRedraw(true);
    }
    
    protected void rip() {
        _author.setEnabled(false);
        _authorLabel.setEnabled(false);
        _targetLabel.setEnabled(false);
        _target.setEnabled(false);
        _tag.setEnabled(false);
        _tagLabel.setEnabled(false);
        super.rip();
    }
    
    private static final String T_PRIV_PUBLIC = "syndie.gui.webrippostcontrol.privpublic";
    private static final String T_PRIV_PBE = "syndie.gui.webrippostcontrol.privpbe";
    private static final String T_PRIV_AUTHORIZED = "syndie.gui.webrippostcontrol.privauthorized";
    
    private static final String T_TAGWITHURL = "syndie.gui.webrippostcontrol.tagwithurl";
    private static final String T_TAG = "syndie.gui.webrippostcontrol.tag";
    private static final String T_TARGET = "syndie.gui.webrippostcontrol.target";
    private static final String T_AUTHOR = "syndie.gui.webrippostcontrol.author";
    
    public void translate(TranslationRegistry registry) {
        super.translate(registry);
        _privacyPublic.setText(registry.getText(T_PRIV_PUBLIC, "Anyone can read the rip"));
        _privacyPBE.setText(registry.getText(T_PRIV_PBE, "Passphrase required to read the rip..."));
        _privacyAuthorized.setText(registry.getText(T_PRIV_AUTHORIZED, "Only authorized people can read the rip"));
        _tagWithURL.setText(registry.getText(T_TAGWITHURL, "Tag the post with the URL?"));
        
        _tagLabel.setText(registry.getText(T_TAG, "Tags:"));
        _targetLabel.setText(registry.getText(T_TARGET, "Post to:"));
        _authorLabel.setText(registry.getText(T_AUTHOR, "Post from:"));
    }
    
    public void applyTheme(Theme theme) {
        super.applyTheme(theme);
        _tagLabel.setFont(theme.DEFAULT_FONT);
        _tag.setFont(theme.DEFAULT_FONT);
        _target.setFont(theme.DEFAULT_FONT);
        _targetLabel.setFont(theme.DEFAULT_FONT);
        _author.setFont(theme.DEFAULT_FONT);
        _authorLabel.setFont(theme.DEFAULT_FONT);
    }
}
