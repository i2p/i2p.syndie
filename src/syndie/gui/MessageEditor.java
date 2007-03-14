package syndie.gui;

import com.swabunga.spell.engine.Word;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.net.URISyntaxException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;
import net.i2p.data.Base64;
import net.i2p.data.DataHelper;
import net.i2p.data.Hash;
import net.i2p.data.SigningPrivateKey;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.NymKey;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.WebRipRunner;
import syndie.db.CommandImpl;
import syndie.db.DBClient;
import syndie.db.ThreadAccumulatorJWZ;
import syndie.db.ThreadBuilder;
import syndie.db.ThreadMsgId;
import syndie.db.UI;

/**
 *
 */
public class MessageEditor implements Themeable, Translatable, ImageBuilderPopup.ImageBuilderSource {
    private DataControl _dataControl;
    private DataCallback _dataCallback;
    private NavigationControl _navControl;
    private BookmarkControl _bookmarkControl;
    private URIControl _uriControl;
    private Composite _parent;
    private Composite _root;
    private Composite _toolbar;
    private Label _authorLabel;
    private Combo _authorCombo;
    // sometimes _signAs is not _authorCombo
    private Label _signAsLabel;
    private Combo _signAs;
    private List _signAsHashes;
    private Button _authorHidden;
    private Label _toLabel;
    private Combo _to;
    private Label _subjectLabel;
    private Text _subject;
    private Label _tagLabel;
    private Text _tag;
    private Label _privacyLabel;
    private Combo _privacy;
    private CTabFolder _pageTabs;
    private Button _post;
    private Button _postpone;
    private Button _cancel;
    
    private CTabItem _refEditorTab;
    private Composite _refEditorTabRoot;
    private MessageReferencesEditor _refEditor;
    private CTabItem _threadTab;
    private Composite _threadTabRoot;
    private MessageTree _threadTree;
    
    // now for the toolbar...
    
    // forum control
    private Group _forumGroup;
    private Button _forumButton;
    private Image _forumAvatar;
    private Menu _forumMenu;
    // author control
    private Group _authorGroup;
    private Button _authorButton;
    private Image _authorAvatar;
    private Menu _authorMenu;
    private MenuItem _authorMenuOther;
    // privacy control
    private Group _privGroup;
    private Button _privButton;
    private Menu _privMenu;
    private MenuItem _privPublic;
    private MenuItem _privAuthorized;
    private MenuItem _privPBE;
    private MenuItem _privReply;
    // page control
    private Group _pageGroup;
    private Button _pageButton;
    private Menu _pageMenu;
    private MenuItem _pageAdd;
    private MenuItem _pageAddWebRip;
    private MenuItem _pageRemove;
    // page type control
    private Group _pageTypeGroup;
    private Button _pageType;
    // attachment control
    private Group _attachGroup;
    private Button _attachButton;
    private Menu _attachMenu;
    private MenuItem _attachAdd;
    private MenuItem _attachAddImage;
    // link control
    private Group _linkGroup;
    private Button _linkButton;
    private Menu _linkMenu;
    private MenuItem _linkWeb;
    private MenuItem _linkPage;
    private MenuItem _linkAttach;
    private MenuItem _linkForum;
    private MenuItem _linkMsg;
    private MenuItem _linkEepsite;
    private MenuItem _linkI2P;
    private MenuItem _linkFreenet;
    private MenuItem _linkArchive;
    private MenuItem _linkOther;
    // style control
    private Group _styleGroup;
    private Button _styleButton;
    private Menu _styleMenu;
    private MenuItem _styleText;
    private MenuItem _styleImage;
    private MenuItem _styleBGColor;
    private Menu _styleBGColorMenu;
    private MenuItem _styleBGColorDefault;
    private MenuItem _styleBGImage;
    private MenuItem _styleListOrdered;
    private MenuItem _styleListUnordered;
    private MenuItem _styleHeading;
    private Menu _styleHeadingMenu;
    private MenuItem _styleHeading1;
    private MenuItem _styleHeading2;
    private MenuItem _styleHeading3;
    private MenuItem _styleHeading4;
    private MenuItem _styleHeading5;
    private MenuItem _stylePre;
    // spellcheck control
    private Group _spellGroup;
    private Button _spellButton;
    // search control
    private Group _searchGroup;
    private Button _searchButton;
    // quote control
    private Group _quoteGroup;
    private Button _quoteButton;
    
    // state info
    private List _pageEditors;
    private List _pageTypes;
    
    private List _attachmentConfig;
    private List _attachmentData;
    private List _attachmentSummary;
    private String _selectedPageBGColor;
    private String _selectedPageBGImage;
    /** has it been modified since it was last persisted */
    private boolean _modifiedSinceSave;
    /** has it been modified since opening the editor */
    private boolean _modifiedSinceOpen;
    /** set to false to disable temporary save points (during automated updates) */
    private boolean _enableSave;
    
    /** cache some details for who we have keys to write to / manage / etc */
    private DBClient.ChannelCollector _nymChannels;
    /** set of MessageEditorListener */
    private List _listeners;
    
    /** forum the post is targetting */
    private Hash _forum;
    /** who the post should be signed by */
    private Hash _author;
    /**
     * ordered list of earlier messages (SyndieURI) this follows in the thread 
     * of (most recent parent first)
     */
    private List _parents;
    /** if using PBE, this is the required passphrase */
    private String _passphrase;
    /** if using PBE, this is the prompt for the passphrase */
    private String _passphrasePrompt;
    
    /** postponeId is -1 if not yet saved, otherwise its the entry in nymMsgPostpone */
    private long _postponeId;
    /** version is incremented each time the state is saved */
    private int _postponeVersion;

    private MessageEditorFind _finder;
    private MessageEditorSpell _spellchecker;
    //private MessageEditorStyler _styler;
    private ImageBuilderPopup _imagePopup;
    private ReferenceChooserPopup _refChooser;
    private LinkBuilderPopup _linkPopup;
    private LinkBuilderPopup _refAddPopup;
    
    /** Creates a new instance of MessageEditorNew */
    public MessageEditor(DataControl dataControl, DataCallback callback, NavigationControl navControl, BookmarkControl bookmarkControl, URIControl uriControl, Composite parent, LocalMessageCallback lsnr) {
        _dataControl = dataControl;
        _dataCallback = callback;
        _navControl = navControl;
        _bookmarkControl = bookmarkControl;
        _uriControl = uriControl;
        _parent = parent;
        _pageEditors = new ArrayList(1);
        _pageTypes = new ArrayList();
        _attachmentConfig = new ArrayList();
        _attachmentData = new ArrayList();
        _attachmentSummary = new ArrayList();
        _parents = new ArrayList();
        _signAsHashes = new ArrayList();
        _modifiedSinceOpen = false;
        _modifiedSinceSave = false;
        _enableSave = false;
        _postponeId = -1;
        _postponeVersion = -1;
        Properties prefs = _dataControl.getClient().getNymPrefs();
        String val = prefs.getProperty("editor.defaultAuthor");
        if (val != null) {
            byte hash[] = Base64.decode(val);
            if ( (hash != null) && (hash.length == Hash.HASH_LENGTH) ) {
                _author = new Hash(hash);
                _forum = _author;
            }
        }
        _listeners = new ArrayList();
        if (lsnr != null) _listeners.add(lsnr);
        initComponents();
        _modifiedSinceOpen = false;
        _modifiedSinceSave = false;
    }
    
    public void addListener(LocalMessageCallback lsnr) { _listeners.add(lsnr); }
    
    public void dispose() {
        if (_refAddPopup != null) _refAddPopup.dispose();
        if (_linkPopup != null) _linkPopup.dispose();
        if (_refChooser != null) _refChooser.dispose();
        if (_imagePopup != null) _imagePopup.dispose();
        //if (_styler != null) _styler.dispose();
        if (_spellchecker != null) _spellchecker.dispose();
        if (_finder != null) _finder.dispose();
        while (_pageEditors.size() > 0)
            ((PageEditor)_pageEditors.remove(0)).dispose();
        
        ImageUtil.dispose(_forumAvatar);
        ImageUtil.dispose(_authorAvatar);
        _dataControl.getTranslationRegistry().unregister(this);
        _dataControl.getThemeRegistry().unregister(this);
    }
    
    // PageEditors ask for these:
    CTabFolder getPageRoot() { return _pageTabs; }//_pageRoot; }
    void modified() {
        _modifiedSinceSave = true; 
        _modifiedSinceOpen = true; 
    }
    void enableAutoSave() { _enableSave = true; }
    void disableAutoSave() { _enableSave = false;}
    
    /** save the state of the message so if there is a crash / exit / etc, it is resumeable */
    private static final String SQL_POSTPONE = "INSERT INTO nymMsgPostpone (nymId, postponeId, postponeVersion, encryptedData)  VALUES(?, ?, ?, ?)";
    private static final String SQL_POSTPONE_CLEANUP = "DELETE FROm nymMsgPostpone WHERE nymId = ? AND postponeId = ? AND postponeVersion < ?";
    void saveState() {
        if (!_modifiedSinceSave || !_enableSave) return;
        long stateId = _postponeId;
        if (stateId < 0)
            stateId = System.currentTimeMillis();
        _dataControl.getUI().debugMessage("saving state for postponeId " + _postponeId + "/" + stateId);
        String state = serializeStateToB64(stateId); // increments the version too
        _dataControl.getUI().debugMessage("serialized state for postponeId " + stateId + " / " + _postponeVersion);
        if (state == null) {
            _dataControl.getUI().errorMessage("Internal error serializing message state");
            return;
        }
        int version = _postponeVersion;
        Connection con = _dataControl.getClient().con();
        PreparedStatement stmt = null;
        try {
            stmt = con.prepareStatement(SQL_POSTPONE);
            stmt.setLong(1, _dataControl.getClient().getLoggedInNymId());
            stmt.setLong(2, stateId);
            stmt.setInt(3, version);
            stmt.setString(4, state);
            stmt.executeUpdate();
            stmt.close();
            stmt = null;

            
            // if that didn't fail, delete all of the older versions
            stmt = con.prepareStatement(SQL_POSTPONE_CLEANUP);
            stmt.setLong(1, _dataControl.getClient().getLoggedInNymId());
            stmt.setLong(2, stateId);
            stmt.setInt(3, version);
            stmt.executeUpdate();
            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            _dataControl.getUI().errorMessage("Internal error postponing", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        _dataControl.getUI().debugMessage("done saving state.  " + _postponeId + "/" + _postponeVersion);
        _modifiedSinceSave = false;
    }
    
    private static final String SQL_RESUME = "SELECT encryptedData FROM nymMsgPostpone WHERE nymId = ? AND postponeId = ? AND postponeVersion = ?";
    public boolean loadState(long postponeId, int version) {
        Connection con = _dataControl.getClient().con();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        String state = null;
        try {
            stmt = con.prepareStatement(SQL_RESUME);
            stmt.setLong(1, _dataControl.getClient().getLoggedInNymId());
            stmt.setLong(2, postponeId);
            stmt.setInt(3, version);
            rs = stmt.executeQuery();
            if (rs.next()) {
                state = rs.getString(1);
            } else {
                return false;
            }
            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            _dataControl.getUI().errorMessage("Internal error resuming", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        if (state != null) {
            deserializeStateFromB64(state, postponeId, version);
            _modifiedSinceSave = false;
            _modifiedSinceOpen = false;
            return true;
        } else {
            return false;
        }
    }
    
    private static final String SQL_DROP = "DELETE FROM nymMsgPostpone WHERE nymId = ? AND postponeId = ?";
    void dropSavedState() {
        _dataControl.getUI().debugMessage("dropping saved state for postponeId " + _postponeId);
        Connection con = _dataControl.getClient().con();
        PreparedStatement stmt = null;
        try {
            stmt = con.prepareStatement(SQL_DROP);
            stmt.setLong(1, _dataControl.getClient().getLoggedInNymId());
            stmt.setLong(2, _postponeId);
            stmt.executeUpdate();
            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            _dataControl.getUI().errorMessage("Internal error dropping saved state", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String T_POSTED_MESSAGE = "syndie.gui.messageeditor.post.message";
    private static final String T_POSTED_TITLE = "syndie.gui.messageeditor.post.title";
    private static final String T_POST_ERROR_MESSAGE_PREFIX = "syndie.gui.messageeditor.post.errormsg";
    private static final String T_POST_ERROR_TITLE = "syndie.gui.messageeditor.post.errortitle";
    
    private void postMessage() {
        if (!validateAuthorForum()) {
            showUnauthorizedWarning();
            return;
        }
        MessageCreator creator = new MessageCreator(new CreatorSource());
        boolean ok = creator.execute();
        if (ok) {
            dropSavedState();
            MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_INFORMATION | SWT.OK);
            box.setMessage(getDataControl().getTranslationRegistry().getText(T_POSTED_MESSAGE, "Message created and imported successfully!  Please be sure to syndicate it to others so they can read it"));
            box.setText(getDataControl().getTranslationRegistry().getText(T_POSTED_TITLE, "Message created!"));
            box.open();
            for (Iterator iter = _listeners.iterator(); iter.hasNext(); ) 
                ((LocalMessageCallback)iter.next()).messageCreated(creator.getCreatedURI());
        } else {
            MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_ERROR | SWT.OK);
            box.setMessage(getDataControl().getTranslationRegistry().getText(T_POST_ERROR_MESSAGE_PREFIX, "There was an error creating the message.  Please view the log for more information: ") + creator.getErrors());
            box.setText(getDataControl().getTranslationRegistry().getText(T_POST_ERROR_TITLE, "Error creating the message"));
            box.open();
        }
    }
    
    private class CreatorSource implements MessageCreator.MessageCreatorSource {
        public DataCallback getDataCallback() { return _dataCallback; }
        public DBClient getClient() { return _dataControl.getClient(); }
        public UI getUI() { return _dataControl.getUI(); }
        public Hash getAuthor() { return _author; }
        public Hash getSignAs() {
            if (_signAsHashes.size() > 0) {
                int idx = _signAs.getSelectionIndex();
                if ( (idx >= 0) && (idx < _signAsHashes.size()) ) {
                    return (Hash)_signAsHashes.get(idx);
                }
            }
            return null;
        }
        public boolean getAuthorHidden() {
            return (_signAsHashes.size() > 0) && (_authorHidden.getSelection());
        }
        public Hash getTarget() { return _forum; }
        public int getPageCount() { return _pageEditors.size(); }
        public String getPageContent(int page) { return ((PageEditor)_pageEditors.get(page)).getContent(); }
        /** 0-indexed page type */
        public String getPageType(int page) { return ((PageEditor)_pageEditors.get(page)).getContentType(); }
        public List getAttachmentNames() {             
            ArrayList rv = new ArrayList();
            for (int i = 0; i < _attachmentConfig.size(); i++) {
                Properties cfg = (Properties)_attachmentConfig.get(i);
                rv.add(cfg.getProperty(Constants.MSG_ATTACH_NAME));
            }
            return rv;
        }
        public List getAttachmentTypes() { 
            List rv = new ArrayList(_attachmentConfig.size());
            for (int i = 0; i < _attachmentConfig.size(); i++) {
                Properties cfg = (Properties)_attachmentConfig.get(i);
                rv.add(cfg.getProperty(Constants.MSG_ATTACH_CONTENT_TYPE));
            }
            return rv;
        }
        /** @param attachmentIndex starts at 1 */
        public byte[] getAttachmentData(int attachmentIndex) { return MessageEditor.this.getAttachmentData(attachmentIndex); }
        public String getSubject() { return _subject.getText(); }
        public boolean getPrivacyPBE() { return (_privacy.getSelectionIndex() == PRIVACY_PBE) && (_passphrase != null) && (_passphrasePrompt != null); }
        public String getPassphrase() { return (_privacy.getSelectionIndex() == PRIVACY_PBE) ? _passphrase : null; }
        public String getPassphrasePrompt() { return (_privacy.getSelectionIndex() == PRIVACY_PBE) ? _passphrasePrompt : null; }
        public boolean getPrivacyPublic() { return _privacy.getSelectionIndex() == PRIVACY_PUBLIC; }
        public String getAvatarUnmodifiedFilename() { return null; }
        public byte[] getAvatarModifiedData() { return null; }
        public boolean getPrivacyReply() { return _privacy.getSelectionIndex() == PRIVACY_REPLY; }
        public String[] getPublicTags() { return new String[0]; }
        public String[] getPrivateTags() {
            String src = _tag.getText().trim();
            return Constants.split(" \t\r\n", src, false);
        }
        public List getReferenceNodes() { return _refEditor.getReferenceNodes(); }
        public int getParentCount() { return _parents.size(); }
        public SyndieURI getParent(int depth) { return (SyndieURI)_parents.get(depth); }
        public String getExpiration() { return null; }
        public boolean getForceNewThread() { return false; }
        public boolean getRefuseReplies() { return false; }
    }   
    
    public void postponeMessage() {
        saveState();
        for (Iterator iter = _listeners.iterator(); iter.hasNext(); ) 
                ((LocalMessageCallback)iter.next()).messagePostponed(_postponeId);
    }
    
    private static final String T_CANCEL_MESSAGE = "syndie.gui.messageeditor.cancel.message";
    private static final String T_CANCEL_TITLE = "syndie.gui.messageeditor.cancel.title";
    
    public void cancelMessage() { cancelMessage(true); }
    public void cancelMessage(boolean requireConfirm) {
        if (requireConfirm) {
            // confirm
            MessageBox dialog = new MessageBox(_root.getShell(), SWT.ICON_WARNING | SWT.YES | SWT.NO);
            dialog.setMessage(getDataControl().getTranslationRegistry().getText(T_CANCEL_MESSAGE, "Are you sure you want to cancel this message?"));
            dialog.setText(getDataControl().getTranslationRegistry().getText(T_CANCEL_TITLE, "Confirm message cancellation"));
            int rv = dialog.open();
            if (rv == SWT.YES) {
                cancelMessage(false);
            }
        } else {
            dropSavedState();
            for (Iterator iter = _listeners.iterator(); iter.hasNext(); ) 
                ((LocalMessageCallback)iter.next()).messageCancelled();
        }
    }

    /**
     * serialize a deep copy of the editor state (including pages, attachments, config),
     * PBE encrypted with the currently logged in nym's passphrase, and return that after
     * base64 encoding.  the first 16 bytes (24 characters) make up the salt for PBE decryption,
     * and there is no substantial padding on the body (only up to the next 16 byte boundary)
     */
    public String serializeStateToB64(long postponementId) {
        try {
            byte data[] = null;
            try {
                data = serializeState();
            } catch (IOException ioe) {
                // this is writing to memory...
                _dataControl.getUI().errorMessage("Internal error serializing message state", ioe);
                return null;
            }
            byte salt[] = new byte[16];
            byte encr[] = _dataControl.getClient().pbeEncrypt(data, salt);
            String rv = Base64.encode(salt) + Base64.encode(encr);
            _postponeId = postponementId;
            _postponeVersion++;
            _dataControl.getUI().debugMessage("serialized state to " + encr.length + " bytes (" + rv.length() + " base64 encoded...)");
            return rv;
        } catch (OutOfMemoryError oom) {
            _dataControl.getUI().errorMessage("Ran out of memory serializing the state.  page buffers: " + countPageUndoBuffers() + " bytes");
            return null;
        }
    }
    
    private long countPageUndoBuffers() {
        long rv = 0;
        for (int i = 0; i < _pageEditors.size(); i++) {
            PageEditor ed = (PageEditor)_pageEditors.get(i);
            rv += ed.getUndoBufferSize();
        }
        return rv;
    }
    
    public long getPostponementId() { return _postponeId; }
    public int getPostponementVersion() { return _postponeVersion; }
    
    public void deserializeStateFromB64(String state, long postponeId, int version) {
        String salt = state.substring(0, 24);
        String body = state.substring(24);
        byte decr[] = _dataControl.getClient().pbeDecrypt(Base64.decode(body), Base64.decode(salt));
        
        if (decr == null) {
            _dataControl.getUI().errorMessage("Error pbe decrypting " + postponeId + "." + version + ": state: " + state);
            dispose();
            return;
        }

        _dataControl.getUI().debugMessage("deserialized state to " + decr.length + " bytes (" + state.length() + " base64 encoded...)");
        state = null;

        ZipInputStream zin = null;
        try {
            zin = new ZipInputStream(new ByteArrayInputStream(decr));
            deserializeState(zin);
        } catch (IOException ioe) {
            _dataControl.getUI().errorMessage("Internal error deserializing message state", ioe);
            return;
        } finally {
            if (zin != null) try { zin.close(); } catch (IOException ioe) {}
        }
        _postponeId = postponeId;
        _postponeVersion = version;
    }
    
    private static final String SER_ENTRY_CONFIG = "config.txt";
    private static final String SER_ENTRY_PAGE_PREFIX = "page";
    private static final String SER_ENTRY_PAGE_CFG_PREFIX = "pageCfg";
    private static final String SER_ENTRY_ATTACH_PREFIX = "attach";
    private static final String SER_ENTRY_ATTACH_CFG_PREFIX = "attachCfg";
    private static final String SER_ENTRY_REFS = "refs.txt";
    private static final String SER_ENTRY_AVATAR = "avatar.png";
    
    private byte[] serializeState() throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ZipOutputStream zos = new ZipOutputStream(baos);
        
        Properties cfg = serializeConfig();
        zos.putNextEntry(new ZipEntry(SER_ENTRY_CONFIG));
        for (Iterator iter = cfg.keySet().iterator(); iter.hasNext(); ) {
            String key = (String)iter.next();
            String val = cfg.getProperty(key);
            String line = CommandImpl.strip(key, "=:\r\t\n", '_') + "=" + CommandImpl.strip(val, "\r\t\n", '_') + "\n";
            zos.write(DataHelper.getUTF8(line));
        }
        zos.closeEntry();
        
        List nodes = _refEditor.getReferenceNodes();
        //List nodes = _refs.getReferenceNodes();
        if ( (nodes != null) && (nodes.size() > 0) ) {
            zos.putNextEntry(new ZipEntry(SER_ENTRY_REFS));
            String str = ReferenceNode.walk(nodes);
            zos.write(DataHelper.getUTF8(str));
            zos.closeEntry();
        }
        
        for (int i = 0; i < _pageEditors.size(); i++) {
            PageEditor editor = (PageEditor)_pageEditors.get(i);
            String type = editor.getContentType();
            String data = editor.getContent();
            zos.putNextEntry(new ZipEntry(SER_ENTRY_PAGE_PREFIX + i));
            zos.write(DataHelper.getUTF8(data));
            zos.closeEntry();
            
            zos.putNextEntry(new ZipEntry(SER_ENTRY_PAGE_CFG_PREFIX + i));
            zos.write(DataHelper.getUTF8(type));
            zos.closeEntry();
        }
        
        for (int i = 0; i < _attachmentData.size(); i++) {
            byte data[] = (byte[])_attachmentData.get(i);
            Properties attCfg = (Properties)_attachmentConfig.get(i);
            zos.putNextEntry(new ZipEntry(SER_ENTRY_ATTACH_PREFIX + i));
            zos.write(data);
            zos.closeEntry();
            
            zos.putNextEntry(new ZipEntry(SER_ENTRY_ATTACH_CFG_PREFIX + i));
            for (Iterator iter = attCfg.keySet().iterator(); iter.hasNext(); ) {
                String key = (String)iter.next();
                String val = attCfg.getProperty(key);
                String line = CommandImpl.strip(key, "=:\r\t\n", '_') + "=" + CommandImpl.strip(val, "\r\t\n", '_') + "\n";
                zos.write(DataHelper.getUTF8(line));
            }
            zos.closeEntry();
        }
        
        byte avatar[] = null;
        /*
        if (_controlAvatarImageSource == null)
            avatar = getAvatarModifiedData();
        else
            avatar = getAvatarUnmodifiedData();
        if (avatar != null) {
            zos.putNextEntry(new ZipEntry(SER_ENTRY_AVATAR));
            zos.write(avatar);
            zos.closeEntry();        
        }
         */
        
        zos.finish();
        return baos.toByteArray();
    }
    
    private void deserializeState(ZipInputStream zin) throws IOException {
        ZipEntry entry = null;
        Map pages = new TreeMap();
        Map pageCfgs = new TreeMap();
        Map attachments = new TreeMap();
        Map attachmentCfgs = new TreeMap();
        byte avatar[] = null;
        while ( (entry = zin.getNextEntry()) != null) {
            String name = entry.getName();
            _dataControl.getUI().debugMessage("Deserializing state: entry = " + name);
            if (name.equals(SER_ENTRY_CONFIG)) {
                deserializeConfig(readCfg(read(zin)));
            } else if (name.equals(SER_ENTRY_REFS)) {
                _refEditor.setReferenceNodes(ReferenceNode.buildTree(zin));
            } else if (name.startsWith(SER_ENTRY_PAGE_CFG_PREFIX)) {
                pageCfgs.put(name, read(zin));
            } else if (name.startsWith(SER_ENTRY_PAGE_PREFIX)) {
                pages.put(name, read(zin));
            } else if (name.startsWith(SER_ENTRY_ATTACH_CFG_PREFIX)) {
                attachmentCfgs.put(name, readCfg(read(zin)));
            } else if (name.startsWith(SER_ENTRY_ATTACH_PREFIX)) {
                attachments.put(name, readBytes(zin));
            } else if (name.startsWith(SER_ENTRY_AVATAR)) {
                avatar = readBytes(zin);
            }
            zin.closeEntry();
        }

        while (_pageEditors.size() > 0) {
            PageEditor editor = (PageEditor)_pageEditors.remove(0);
            editor.dispose();
        }
        int pageCount = pages.size();
        
        _pageTypes.clear();
        while (_pageEditors.size() > 0)
            ((PageEditor)_pageEditors.remove(0)).dispose();
        
        for (int i = 0; i < pageCount; i++) {
            String body = (String)pages.get(SER_ENTRY_PAGE_PREFIX + i);
            String type = (String)pageCfgs.get(SER_ENTRY_PAGE_CFG_PREFIX + i);
            
            _dataControl.getUI().debugMessage("Deserializing state: adding page: " + i + " [" + type + "]");
            boolean isHTML = TYPE_HTML.equals(type);
            PageEditor editor = new PageEditor(_dataControl, this, isHTML, i);
            _pageEditors.add(editor);
            _pageTypes.add(type);
            editor.setContent(body);
            editor.getItem().setText(_dataControl.getTranslationRegistry().getText(T_PAGE_PREFIX, "Page ") + (i+1));
            if (isHTML)
                _pageType.setImage(ImageUtil.ICON_EDITOR_PAGETYPE_HTML);
            else
                _pageType.setImage(ImageUtil.ICON_EDITOR_PAGETYPE_TEXT);
        }
        
        _attachmentData.clear();
        _attachmentConfig.clear();
        int attachmentCount = attachments.size();
        for (int i = 0; i < attachmentCount; i++) {
            byte data[] = (byte[])attachments.get(SER_ENTRY_ATTACH_PREFIX + i);
            Properties cfg = (Properties)attachmentCfgs.get(SER_ENTRY_ATTACH_CFG_PREFIX + i);
            if ( (cfg == null) || (data == null) ) 
                break;
            _dataControl.getUI().debugMessage("Deserializing state: adding attachment: " + i);
            _attachmentData.add(data);
            _attachmentConfig.add(cfg);
        }
        
        /*
        ImageUtil.dispose(_controlAvatarImage);
        _controlAvatarImageSource = null;
        Image img = null;
        if (avatar != null) {
            img = ImageUtil.createImage(avatar);
            Rectangle bounds = img.getBounds();
            if ( (bounds.width != Constants.MAX_AVATAR_WIDTH) || (bounds.height != Constants.MAX_AVATAR_HEIGHT) ) {
                img = ImageUtil.resize(img, Constants.MAX_AVATAR_WIDTH, Constants.MAX_AVATAR_HEIGHT, true);
            }
        }
        _controlAvatarImageSource = null;
        if (img == null)
            _controlAvatarImage = ImageUtil.ICON_QUESTION;
        else
            _controlAvatarImage = img;
        _controlAvatar.setImage(_controlAvatarImage);
         */
        
        rebuildAttachmentSummaries();
        if (_pageEditors.size() > 0)
            viewPage(0);
        updateAuthor();
        updateForum();
        refreshAuthors();
        updateToolbar();
    }
    
    private static final String T_PAGE_PREFIX = "syndie.gui.messageeditor.pageprefix";
    
    private static final String SER_AUTHOR = "author";
    private static final String SER_TARGET = "target";
    private static final String SER_PARENTS = "parents";
    private static final String SER_PARENTS_PREFIX = "parents_";
    private static final String SER_PASS = "passphrase";
    private static final String SER_PASSPROMPT = "passphraseprompt";
    private static final String SER_SUBJECT = "subject";
    private static final String SER_TAGS = "tags";
    private static final String SER_PRIV = "privacy";
    private static final String SER_EXPIRATION = "expiration";
    private Properties serializeConfig() {
        Properties rv = new Properties();
        if (_author == null)
            rv.setProperty(SER_AUTHOR, "");
        else
            rv.setProperty(SER_AUTHOR, _author.toBase64());
        
        if (_forum == null)
            rv.setProperty(SER_TARGET, "");
        else
            rv.setProperty(SER_TARGET, _forum.toBase64());
        
        if (_parents == null) {
            rv.setProperty(SER_PARENTS, "0");
        } else {
            rv.setProperty(SER_PARENTS, _parents.size() + "");
            for (int i = 0; i < _parents.size(); i++)
                rv.setProperty(SER_PARENTS_PREFIX + i, ((SyndieURI)_parents.get(i)).toString());
        }
        
        if (_passphrase != null)
            rv.setProperty(SER_PASS, _passphrase);
        if (_passphrasePrompt != null)
            rv.setProperty(SER_PASSPROMPT, _passphrasePrompt);
        
        rv.setProperty(SER_SUBJECT, _subject.getText());
        rv.setProperty(SER_TAGS, _tag.getText());
        
        int privacy = _privacy.getSelectionIndex();
        if (privacy < 0) privacy = 1;
        
        rv.setProperty(SER_PRIV, privacy + "");
        //String exp = getExpiration();
        //if (exp != null)
        //    rv.setProperty(SER_EXPIRATION, exp);
        
        return rv;
    }
    
    private Hash getHash(Properties cfg, String prop) {
        String t = cfg.getProperty(prop);
        if (t == null) return null;
        byte d[] = Base64.decode(t);
        if ( (d == null) || (d.length != Hash.HASH_LENGTH) ) {
            _dataControl.getUI().debugMessage("serialized prop (" + prop + ") [" + t + "] could not be decoded");
            return null;
        } else {
            return new Hash(d);
        }
    }
    
    private void deserializeConfig(Properties cfg) {
        _dataControl.getUI().debugMessage("deserializing config: \n" + cfg.toString());
        _author = getHash(cfg, SER_AUTHOR);
        _forum = getHash(cfg, SER_TARGET);
        
        int parents = 0;
        if ( (cfg.getProperty(SER_PARENTS) != null) && (cfg.getProperty(SER_PARENTS).length() > 0) )
            try { parents = Integer.parseInt(cfg.getProperty(SER_PARENTS)); } catch (NumberFormatException nfe) {}

        if (parents <= 0)
            _parents = new ArrayList();
        else
            _parents = new ArrayList(parents);
        for (int i = 0; i < parents; i++) {
            String uriStr = cfg.getProperty(SER_PARENTS_PREFIX + i);
            try {
                SyndieURI uri = new SyndieURI(uriStr);
                _parents.add(uri);
            } catch (URISyntaxException use) {
                //
            }
        }
        if (_parents.size() > 0) {
            ThreadBuilder builder = new ThreadBuilder(_dataControl.getClient(), _dataControl.getUI());
            HashSet msgIds = new HashSet();
            for (int i = 0; i < _parents.size(); i++) {
                SyndieURI uri = (SyndieURI)_parents.get(i);
                if ( (uri.getScope() != null) && (uri.getMessageId() != null) ) {
                    long msgId = _dataControl.getClient().getMessageId(uri.getScope(), uri.getMessageId());
                    ThreadMsgId id = new ThreadMsgId(msgId); // may be -1
                    id.messageId = uri.getMessageId().longValue();
                    id.scope = uri.getScope();
                    msgIds.add(id);
                }
            }
            List roots = builder.buildThread(msgIds);
            _dataControl.getUI().debugMessage("setting message ancestry tree to: \n" + roots);
            _threadTree.setMessages(roots);
            _threadTree.select((SyndieURI)_parents.get(0));
        } else {
            _threadTree.dispose();
            _threadTab.dispose();
        }
        
        _passphrase = cfg.getProperty(SER_PASS);
        _passphrasePrompt = cfg.getProperty(SER_PASSPROMPT);
        if (cfg.containsKey(SER_SUBJECT)) {
            _subject.setText(cfg.getProperty(SER_SUBJECT));
        } else if ( (_parents != null) && (_parents.size() > 0) ) {
            SyndieURI parent = (SyndieURI)_parents.get(0);
            String parentSubject = MessageView.calculateSubject(_dataControl, parent).trim();
            if ( (parentSubject.length() > 0) && (!Constants.lowercase(parentSubject).startsWith("re:")) ) {
                _subject.setText("re: " + parentSubject);
            } else {
                _subject.setText(parentSubject);
            }
        } else {
            _subject.setText("");
        }
        if (cfg.containsKey(SER_TAGS))
            _tag.setText(cfg.getProperty(SER_TAGS));
        else
            _tag.setText("");
        
        String privStr = cfg.getProperty(SER_PRIV);
        if (privStr != null) {
            try {
                int priv = Integer.parseInt(privStr);
                
                switch (priv) {
                    case 0: pickPrivacy(0); break;
                    case 2: pickPrivacy(2); break;
                    case 3: pickPrivacy(3, false); break;
                    case 1: 
                    default: pickPrivacy(1); break;
                }
            } catch (NumberFormatException nfe) {}
        }
        /*
        if (cfg.containsKey(SER_EXPIRATION))
            _controlExpirationText.setText(cfg.getProperty(SER_EXPIRATION));
        else
            _controlExpirationText.setText(_browser.getTranslationRegistry().getText(T_EXPIRATION_NONE, "none"));
         */
    }
    
    
    private byte[] readBytes(InputStream in) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        byte buf[] = new byte[4096];
        int read = -1;
        while ( (read = in.read(buf)) != -1)
            baos.write(buf, 0, read);
        return baos.toByteArray();
    }
    private String read(InputStream in) throws IOException {
        return DataHelper.getUTF8(readBytes(in));
    }
    private Properties readCfg(String str) throws IOException {
        Properties cfg = new Properties();
        
        BufferedReader in = new BufferedReader(new StringReader(str));
        String line = null;
        while ( (line = in.readLine()) != null) {
            int split = line.indexOf('=');
            if (split > 0) {
                String key = line.substring(0, split);
                String val = null;
                if (split >= line.length())
                    val = "";
                else
                    val = line.substring(split+1);
                cfg.setProperty(key, val);
            }
        }
        return cfg;
    }
    
    DataControl getDataControl() { return _dataControl; }
    
    /** current search term used */
    String getSearchTerm() { return _finder.getSearchTerm(); }
    /** current replacement for the search term used */
    String getSearchReplacement() { return _finder.getSearchReplacement(); }
    /** are searches case sensitive? */
    boolean getSearchCaseSensitive() { return _finder.getSearchCaseSensitive(); }
    /** do we want to search backwards? */
    boolean getSearchBackwards() { return _finder.getSearchBackwards(); }
    /** do we want to search around the end/beginning of the page? */
    boolean getSearchWrap() { return _finder.getSearchWrap(); }
    /** fire up the search/replace dialog w/ empty values */
    void search() { 
        // don't open unless there's a page to search...
        if (getPageEditor() != null)
            _finder.open();
    }
    
    void quote() {
        if (_parents.size() > 0) {
            SyndieURI parent = (SyndieURI)_parents.get(0);
            PageEditor editor = getPageEditor();
            if (editor != null)
                editor.quote(parent);
        }
    }
    
    // these four are proxies from the finder to the current page editor */
    void findNext() {
        PageEditor editor = getPageEditor();
        if (editor != null)
            editor.findNext();
    }
    void findReplace() {
        PageEditor editor = getPageEditor();
        if (editor != null)
            editor.findReplace();
    }
    void findReplaceAll() {
        PageEditor editor = getPageEditor();
        if (editor != null)
            editor.findReplaceAll();
    }
    void cancelFind() {
        _finder.hide();
        PageEditor editor = getPageEditor();
        if (editor != null)
            editor.cancelFind();
    }

    // spellcheck proxies
    void spellIgnore() {
        PageEditor editor = getPageEditor();
        if (editor != null)
            editor.spellIgnore();
    } 
    void resetSpellcheck() {
        PageEditor editor = getPageEditor();
        if (editor != null)
            editor.resetSpellcheck();
    }
    void spellReplaceWord(boolean allOccurrences) {
        PageEditor editor = getPageEditor();
        if (editor != null)
            editor.spellReplaceWord(allOccurrences);
    }
    void spellNext() {
        PageEditor editor = getPageEditor();
        if (editor != null)
            editor.spellNext();
    }
    // from editor to spellchecker
    String getSpellWordOrig() { return _spellchecker.getSpellWordOrig(); }
    String getSpellWordSuggestion() { return _spellchecker.getSuggestion(); }
    List getSpellIgnoreAllList() { return _spellchecker.getIgnoreAllList(); }
    /** tuning parameter for how close a word has to be to serve as a suggestion.  5 was arbitrary */
    private static final int SEARCH_CLOSENESS = 5;
    ArrayList getSuggestions(String word, String lcword, String lineText) {
        if (!SpellUtil.getDictionary().isCorrect(lcword)) {
            ArrayList rv = new ArrayList();
            for (Iterator iter = SpellUtil.getDictionary().getSuggestions(word, SEARCH_CLOSENESS).iterator(); iter.hasNext(); ) {
                Word suggestedWord = (Word)iter.next();
                rv.add(suggestedWord.getWord());
            }
            
            _spellchecker.updateSuggestions(rv, lineText, word);
            return rv;
        }
        return null;
        
    }
    void showSpell(boolean wordSet) { _spellchecker.showSpell(wordSet); }
    
    void styleText() {
        if ((getPageType() != null) && (TYPE_HTML.equals(getPageType()))) {
            new MessageEditorStyler(this).open();
            //_styler.open();
        }
    }
    void cancelStyle() {
        PageEditor editor = getPageEditor();
        if (editor != null)
            editor.cancelStyle();
    }
    String getSelectedText() {
        PageEditor editor = getPageEditor();
        if (editor != null)
            return editor.getSelectedText();
        else
            return null;
    }
    void insertStyle(String buf, boolean insert, int begin, int end) {
        PageEditor editor = getPageEditor();
        if (editor != null)
            editor.insertStyle(buf, insert, begin, end);
    }
    
    public void toggleMaxView() {
        PageEditor ed = getPageEditor();
        if (ed != null) {
            ed.toggleMaxView();
        }
    }
    public void toggleMaxEditor() { 
        PageEditor ed = getPageEditor();
        if (ed != null) {
            ed.toggleMaxEditor();
        } else {
            _dataControl.getUI().debugMessage("messageEditor.toggleMaxEditor()");
        }
    }
    
    // gui stuff..
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(1, true));
        
        initHeader();
        initToolbar();
        initPage();
        initFooter();
        initPrivacyCombo();
        
        pickPrivacy(1);
        
        _finder = new MessageEditorFind(this);
        _spellchecker = new MessageEditorSpell(this);
        //_styler = new MessageEditorStyler(this);
        
        _dataControl.getTranslationRegistry().register(this);
        _dataControl.getThemeRegistry().register(this);
                
        addPage();
        
        _nymChannels = _dataControl.getClient().getChannels(true, true, true, true);
        updateForum();
        updateAuthor();

        _pageTabs.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { switchPage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { switchPage(); }
            private void switchPage() {
                int idx = _pageTabs.getSelectionIndex();
                if ( (idx >= 0) && (idx < _pageEditors.size()) ) {
                    PageEditor ed = getPageEditor();
                    String type = getPageType(idx);
                    _dataControl.getUI().debugMessage("switching to page " + idx + " [" + type + "]");
                    ed.setContentType(type);
                }
                updateToolbar(); 
            }
        });
    }
    
    private void initPrivacyCombo() {
        _privacy.setRedraw(false);
        int idx = -1;
        if (_privacy.getItemCount() > 0)
            idx = _privacy.getSelectionIndex();
        else
            idx = 1;
        _privacy.removeAll();
        _privacy.add(_dataControl.getTranslationRegistry().getText(T_PRIV_PUBLIC, "Anyone can read the post"));
        _privacy.add(_dataControl.getTranslationRegistry().getText(T_PRIV_AUTHORIZED, "Authorized readers of the forum can read the post"));
        _privacy.add(_dataControl.getTranslationRegistry().getText(T_PRIV_PBE, "Passphrase required to read the post..."));
        _privacy.add(_dataControl.getTranslationRegistry().getText(T_PRIV_REPLY, "Only forum administrators can read the post"));
        _privacy.setRedraw(true);
    }
    
    private static final String T_PRIV_PUBLIC = "syndie.gui.messageeditor.priv.public";
    private static final String T_PRIV_AUTHORIZED = "syndie.gui.messageeditor.priv.auth";
    private static final String T_PRIV_PBE = "syndie.gui.messageeditor.priv.pbe";
    private static final String T_PRIV_REPLY = "syndie.gui.messageeditor.priv.reply";
    
    private static final int PRIVACY_PUBLIC = 0;
    private static final int PRIVACY_AUTHORIZED = 1;
    private static final int PRIVACY_PBE = 2;
    private static final int PRIVACY_REPLY = 3;
    
    private void pickPrivacy(int privacyIndex) { pickPrivacy(privacyIndex, true); }
    private void pickPrivacy(int privacyIndex, boolean promptForPassphrase) {
        modified();
        switch (privacyIndex) {
            case 0: // public 
                _privacy.select(privacyIndex);
                _privButton.setImage(ImageUtil.ICON_EDITOR_PRIVACY_PUBLIC);
                _privPublic.setSelection(true);
                break;
            case 2: //pbe
                _privacy.select(privacyIndex);
                _privButton.setImage(ImageUtil.ICON_EDITOR_PRIVACY_PBE); 
                _privPBE.setSelection(true);
                if (promptForPassphrase) { // false when deserializing state
                    final PassphrasePrompt dialog = new PassphrasePrompt(_dataControl, _root.getShell(), true);
                    dialog.setPassphrase(_passphrase);
                    dialog.setPassphrasePrompt(_passphrasePrompt);
                    dialog.setPassphraseListener(new PassphrasePrompt.PassphraseListener() { 
                        public void promptComplete(String passphraseEntered, String promptEntered) {
                            _dataControl.getUI().debugMessage("passphrase set [" + passphraseEntered + "] / [" + promptEntered + "]");
                            _passphrase = passphraseEntered;
                            _passphrasePrompt = promptEntered;
                        }
                        public void promptAborted() {}
                    });
                    dialog.open();
                }
                break;
            case 3: // private reply
                _privacy.select(privacyIndex);
                _privButton.setImage(ImageUtil.ICON_EDITOR_PRIVACY_REPLY);
                _privReply.setSelection(true);
                break;
            case 1: // authorized only
            default:
                _privacy.select(privacyIndex);
                _privButton.setImage(ImageUtil.ICON_EDITOR_PRIVACY_AUTHORIZED);
                _privAuthorized.setSelection(true);
                break;
        }
    }
    
    private void initFooter() {
        Composite c = new Composite(_root, SWT.NONE);
        c.setLayout(new FillLayout(SWT.HORIZONTAL));
        c.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _post = new Button(c, SWT.PUSH);
        _post.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { postMessage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { postMessage(); }
        });
        _postpone = new Button(c, SWT.PUSH);
        _postpone.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { postponeMessage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { postponeMessage(); }
        });
        _cancel = new Button(c, SWT.PUSH);
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { cancelMessage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { cancelMessage(); }
        });
        
        _post.setText("Post the message");
        _postpone.setText("Save the message for later");
        _cancel.setText("Cancel the message");
    }
    
    static final String TYPE_HTML = "text/html";
    static final String TYPE_TEXT = "text/plain";
    private void setDefaultPageType(String type) {
        Properties prefs = _dataControl.getClient().getNymPrefs();
        prefs.setProperty("editor.defaultFormat", type);
        _dataControl.getClient().setNymPrefs(prefs);
    }
    
    private PageEditor addPage() {
        Properties prefs = _dataControl.getClient().getNymPrefs();
        boolean html = true;
        String pref = prefs.getProperty("editor.defaultFormat", TYPE_TEXT);
        if (TYPE_HTML.equals(pref))
            html = true;
        else
            html = false;
        if (html)
            return addPage(TYPE_HTML);
        else
            return addPage(TYPE_TEXT);
    }
    private PageEditor addPage(String type) {
        saveState();
        modified();
        PageEditor ed = new PageEditor(_dataControl, this, TYPE_HTML.equals(type), _pageEditors.size());
        _pageEditors.add(ed);
        _pageTypes.add(type);
        int pageNum = _pageEditors.size();
        ed.getItem().setText(_dataControl.getTranslationRegistry().getText(T_PAGE_PREFIX, "Page ") + pageNum);
        
        viewPage(_pageEditors.size()-1);
        if (type.equals(TYPE_HTML))
            _pageType.setImage(ImageUtil.ICON_EDITOR_PAGETYPE_HTML);
        else
            _pageType.setImage(ImageUtil.ICON_EDITOR_PAGETYPE_TEXT);
        saveState();
        return ed;
    }
    public void removePage() {
        int cur = _pageTabs.getSelectionIndex();
        if ( (cur >= 0) && (cur < _pageEditors.size()) ) {
            removePage(cur);
        }
    }
    public void removePage(int pageNum) {
        if ( (pageNum >= 0) && (pageNum < _pageEditors.size()) ) {
            _dataControl.getUI().debugMessage("saving stte, pages: " + _pageEditors.size());
            saveState();
            modified();
            _dataControl.getUI().debugMessage("remove page " + pageNum + "/" + _pageEditors.size());
            PageEditor editor = (PageEditor)_pageEditors.remove(pageNum);
            _pageTypes.remove(pageNum);
            editor.dispose();
            
            for (int i = 0; i < _pageEditors.size(); i++) {
                PageEditor cur = (PageEditor)_pageEditors.get(i);
                cur.getItem().setText(_dataControl.getTranslationRegistry().getText(T_PAGE_PREFIX, "Page ") + (i+1));
            }
            viewPage(_pageEditors.size()-1);
            saveState();
        } else {
            _dataControl.getUI().debugMessage("remove page " + pageNum + " is out of range");
        }
    }
    private void togglePageType() {
        int page = getCurrentPage();
        if (page >= 0) {
            String type = getPageType(page);
            if (TYPE_HTML.equals(type))
                type = TYPE_TEXT;
            else
                type = TYPE_HTML;
            PageEditor ed = getPageEditor(page);
            ed.setContentType(type);
            _pageTypes.set(page, type);
            if (type.equals(TYPE_HTML))
                _pageType.setImage(ImageUtil.ICON_EDITOR_PAGETYPE_HTML);
            else
                _pageType.setImage(ImageUtil.ICON_EDITOR_PAGETYPE_TEXT);
            setDefaultPageType(type);
            updateToolbar();
        }
    }
    
    private static final String T_WEBRIP_TITLE = "syndie.gui.messageeditor.webrip";
    private static final String T_WEBRIP_FAIL = "syndie.gui.messageeditor.webrip.fail";
    
    private void addWebRip() {
        Shell shell = new Shell(_root.getShell(), SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        shell.setLayout(new FillLayout());
        final WebRipPageControl ctl = new WebRipPageControl(_dataControl, shell);
        ctl.setListener(new WebRipListener(shell, ctl));
        ctl.setExistingAttachments(_attachmentData.size());
        shell.pack();
        shell.setText(getDataControl().getTranslationRegistry().getText(T_WEBRIP_TITLE, "Add web rip"));
        shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { ctl.dispose(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        shell.open();
    }
    
    private class WebRipListener implements WebRipPageControl.RipControlListener {
        private Shell _shell;
        private WebRipPageControl _ctl;
        public WebRipListener(Shell shell, WebRipPageControl ctl) {
            _shell = shell;
            _ctl = ctl;
        }
        public void ripComplete(boolean successful, WebRipRunner runner) {
            _dataControl.getUI().debugMessage("rip complete: ok?" + successful);
            if (successful) {
                disableAutoSave();
                PageEditor editor = addPage("text/html");
                String content = runner.getRewrittenHTML();
                if (content != null)
                    editor.setContent(content);
                List files = runner.getAttachmentFiles();
                for (int i = 0; i < files.size(); i++) {
                    File f = (File)files.get(i);
                    addAttachment(f);
                }
                enableAutoSave();
                saveState();
                _shell.dispose();
                _ctl.dispose();
            } else {
                ripFailed(_shell, _ctl);
            }
        }
    }
    
    private void ripFailed(Shell shell, WebRipPageControl ctl) {
        shell.dispose();
        List msgs = ctl.getErrorMessages();
        ctl.dispose();        
        if (msgs.size() > 0) {
            MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_ERROR | SWT.OK);
            box.setText(getDataControl().getTranslationRegistry().getText(T_WEBRIP_FAIL, "Rip failed"));
            StringBuffer err = new StringBuffer();
            for (int i = 0; i < msgs.size(); i++)
                err.append((String)msgs.get(i)).append('\n');
            box.setMessage(err.toString());
            box.open();
        } else {
            _dataControl.getUI().debugMessage("rip failed, but no messages, so it must have been cancelled");
        }
    }
    
    /** 0-indexed page being shown, or -1 if not a page */
    private int getCurrentPage() { 
        int idx = _pageTabs.getSelectionIndex();
        if ( (idx >= 0) && (idx < _pageEditors.size()) )
            return idx;
        else
            return -1;
    } 
    
    /** current page */
    private PageEditor getPageEditor() { return getPageEditor(getCurrentPage()); }
    /** grab the given (0-indexed) page */
    private PageEditor getPageEditor(int pageNum) {
        return (PageEditor)_pageEditors.get(pageNum);
    }
    /** current page */
    private String getPageType() { return getPageType(getCurrentPage()); }
    /** grab the content type of the given (0-indexed) page */
    private String getPageType(int pageNum) {
        if (_pageTypes.size() > pageNum)
            return (String)_pageTypes.get(pageNum);
        else
            return "";
    }
    
    /** view the given (0-indexed) page */
    private void viewPage(int pageNum) {
        _pageTabs.setSelection(pageNum);
        updateToolbar();
    }
    
    /**
     * go through the toolbar to adjust the available options for the current page
     */
    private void updateToolbar() {
        int page = getCurrentPage();
        int pages = _pageEditors.size();
        int attachments = _attachmentData.size();
        String type = (page >= 0) ? getPageType(page) : null;
        boolean pageLoaded = (page >= 0) && (pages > 0);
        boolean isHTML = pageLoaded && TYPE_HTML.equals(type);
        
        _dataControl.getUI().debugMessage("updateToolbar: pages=" + pages + " (" + page + "/" + (pages-1) + ") attachments=" + attachments + " isHTML? " + isHTML + "/" + type + " pageLoaded? " + pageLoaded + " types: " + _pageTypes);
       
        _attachAddImage.setEnabled(isHTML);
        _linkMenu.setEnabled(isHTML);
        _linkArchive.setEnabled(isHTML);
        _linkAttach.setEnabled(isHTML);
        _linkButton.setEnabled(isHTML);
        _linkEepsite.setEnabled(isHTML);
        _linkForum.setEnabled(isHTML);
        _linkFreenet.setEnabled(isHTML);
        _linkGroup.setEnabled(isHTML);
        _linkI2P.setEnabled(isHTML);
        _linkMsg.setEnabled(isHTML);
        _linkOther.setEnabled(isHTML);
        _linkPage.setEnabled(isHTML);
        _linkWeb.setEnabled(isHTML);
        _styleMenu.setEnabled(isHTML);
        _styleBGColor.setEnabled(isHTML);
        _styleBGColorDefault.setEnabled(isHTML);
        _styleBGColorMenu.setEnabled(isHTML);
        _styleBGImage.setEnabled(isHTML);
        _styleButton.setEnabled(isHTML);
        _styleGroup.setEnabled(isHTML);
        _styleHeading.setEnabled(isHTML);
        _styleHeading1.setEnabled(isHTML);
        _styleHeading2.setEnabled(isHTML);
        _styleHeading3.setEnabled(isHTML);
        _styleHeading4.setEnabled(isHTML);
        _styleHeading5.setEnabled(isHTML);
        _styleHeadingMenu.setEnabled(isHTML);
        _styleImage.setEnabled(isHTML);
        _styleListOrdered.setEnabled(isHTML);
        _styleListUnordered.setEnabled(isHTML);
        _stylePre.setEnabled(isHTML);
        _styleText.setEnabled(isHTML);

        if (isHTML) {
            _linkPage.setEnabled(pages > 0);
            _linkAttach.setEnabled(attachments > 0);
        }
        
        if (page >= 0) {
            if (isHTML)
                _pageType.setImage(ImageUtil.ICON_EDITOR_PAGETYPE_HTML);
            else
                _pageType.setImage(ImageUtil.ICON_EDITOR_PAGETYPE_TEXT);
            _pageType.setEnabled(true);
            _pageTypeGroup.setEnabled(true);
        } else {
            _pageType.setEnabled(false);
            _pageTypeGroup.setEnabled(false);
        }
        
        _spellButton.setEnabled(pageLoaded);
        _spellGroup.setEnabled(pageLoaded);
        _searchButton.setEnabled(pageLoaded);
        _searchGroup.setEnabled(pageLoaded);
        _quoteButton.setEnabled(pageLoaded && _parents.size() > 0);
        _quoteGroup.setEnabled(pageLoaded && _parents.size() > 0);
    }
    
    void setBodyTags() { setBodyTags(null); }
    public void setBodyTags(String bgImageURL) {
        _selectedPageBGImage = bgImageURL;
        PageEditor editor = getPageEditor();
        if (editor != null)
            editor.setBodyTags(_selectedPageBGImage, _selectedPageBGColor);
    }
    void setPageBGColor(String name) {
        _selectedPageBGColor = name;
        setBodyTags(_selectedPageBGImage);
    }
    private void showImagePopup(boolean forBodyBackground) { 
        if (_imagePopup == null)
            _imagePopup = new ImageBuilderPopup(_root.getShell(), this);
        _imagePopup.showPopup(forBodyBackground); 
    }
    private void showLinkPopup(boolean web, boolean page, boolean attach, boolean forum, boolean message, boolean submessage, boolean eepsite, boolean i2p, boolean freenet, boolean archive) { 
        if (_linkPopup == null)
            _linkPopup = new LinkBuilderPopup(getDataControl(), _parent.getShell(), new LinkBuilderPopup.LinkBuilderSource () {
                public void uriBuilt(SyndieURI uri, String text) {
                    insertAtCaret("<a href=\"" + uri.toString() + "\">" + text + "</a>");
                }
                public int getPageCount() { return _pageEditors.size(); }
                public List getAttachmentDescriptions() { return MessageEditor.this.getAttachmentDescriptions(); }
            });
        _linkPopup.limitOptions(web, page, attach, forum, message, submessage, eepsite, i2p, freenet, archive, true);
        _linkPopup.showPopup();
    }
    
    public Hash getForum() { return _forum; }
    public int getParentCount() { return _parents.size(); }
    public SyndieURI getParent(int depth) { return (SyndieURI)_parents.get(depth); }
    public boolean getPrivacyReply() { return _privReply.getSelection(); }
    public void setParentMessage(SyndieURI uri) {
        _parents.clear();
        if ( (uri != null) && (uri.getScope() != null) && (uri.getMessageId() != null) ) {
            _parents.add(uri);
            
            long msgId = _dataControl.getClient().getMessageId(uri.getScope(), uri.getMessageId());
            if (msgId >= 0) {
                ThreadMsgId tmi = new ThreadMsgId(msgId);
                tmi.messageId = uri.getMessageId().longValue();
                tmi.scope = uri.getScope();
                Map tmiToList = new HashMap();
                ThreadAccumulatorJWZ.buildAncestors(_dataControl.getClient(), _dataControl.getUI(), tmi, tmiToList);
                List ancestors = (List)tmiToList.get(tmi);
                if ( (ancestors != null) && (ancestors.size() > 0) ) {
                    _dataControl.getUI().debugMessage("parentMessage is " + uri + ", but its ancestors are " + ancestors);
                    for (int i = 0; i < ancestors.size(); i++) {
                        ThreadMsgId ancestor = (ThreadMsgId)ancestors.get(i);
                        _parents.add(SyndieURI.createMessage(ancestor.scope, ancestor.messageId));
                    }
                } else {
                    _dataControl.getUI().debugMessage("parentMessage is " + uri + ", and it has no ancestors");
                }

                ThreadBuilder builder = new ThreadBuilder(_dataControl.getClient(), _dataControl.getUI());
                HashSet msgIds = new HashSet(1);
                msgIds.add(tmi);
                List roots = builder.buildThread(msgIds);
                _dataControl.getUI().debugMessage("thread: " + roots);
                _threadTree.setMessages(roots);
                _threadTree.select(uri);
                
                if (_subject.getText().trim().length() <= 0) {
                    String parentSubject = MessageView.calculateSubject(_dataControl, uri).trim();
                    if ( (parentSubject.length() > 0) && (!Constants.lowercase(parentSubject).startsWith("re:")) ) {
                        _subject.setText("re: " + parentSubject);
                    } else {
                        _subject.setText(parentSubject);
                    }
                }
            } else {
                _dataControl.getUI().debugMessage("parentMessage is " + uri + ", but we don't know it, so don't know its ancestors");

                ThreadBuilder builder = new ThreadBuilder(_dataControl.getClient(), _dataControl.getUI());
                HashSet msgIds = new HashSet(1);
                ThreadMsgId tmi = new ThreadMsgId(-1);
                tmi.messageId = uri.getMessageId().longValue();
                tmi.scope = uri.getScope();
                msgIds.add(tmi);
                List roots = builder.buildThread(msgIds);
                _threadTree.setMessages(roots);
                _threadTree.select(uri);
            }
            
            modified();
        } else {
            _threadTree.dispose();
            _threadTab.dispose();
        }
    }
    public void setForum(Hash forum) { _forum = forum; }
    public void setAsReply(boolean reply) {
        if (reply)
            pickPrivacy(PRIVACY_REPLY);
    }
    public void configurationComplete(SyndieURI uri) {
        String pbePass = uri.getString("pbePass");
        String pbePrompt = uri.getString("pbePrompt");
        if ( (pbePass != null) && (pbePrompt != null) ) {
            // a passphrase is provided in the ViewForum tab via Browser.createPostURI
            pickPrivacy(PRIVACY_PBE, false);
            _passphrase = pbePass;
            _passphrasePrompt = pbePrompt;
        }
        String refs = uri.getString("refs");
        if (refs != null) {
            // refs may include private read/post/manage/reply keys for various forums
            List refNodes = ReferenceNode.buildTree(new ByteArrayInputStream(DataHelper.getUTF8(refs)));
            _refEditor.setReferenceNodes(refNodes);
        }
        
        updateAuthor();
        updateForum();
        rebuildAttachmentSummaries();
        updateToolbar();
        if (_pageEditors.size() > 0)
            viewPage(0);
        
        Long attach = uri.getLong("attachments");
        _dataControl.getUI().debugMessage("configuration complete, with attachments: " + attach);
        if (attach != null) {
            for (int i = 0; i < attach.intValue(); i++) {
                String filename = uri.getString("attachment" + i);
                if (filename == null) break;
                File f = new File(filename);
                if (!f.exists()) break;
                addAttachment(f);
            }
        }
        
        enableAutoSave();
        if (!validateAuthorForum()) {
            // ugly, yet it lets us delay long enough to show the tab (assuming an unauthorized reply)
            _root.getDisplay().timerExec(500, new Runnable() { public void run() { showUnauthorizedWarning(); } });
        }
        _modifiedSinceOpen = false;
    }
    
    private void initPage() {
        _pageTabs = new CTabFolder(_root, SWT.MULTI | SWT.TOP | SWT.BORDER);
        _pageTabs.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _refEditorTab = new CTabItem(_pageTabs, SWT.NONE);
        _refEditorTabRoot = new Composite(_pageTabs, SWT.NONE);
        _refEditorTabRoot.setLayout(new FillLayout());
        _refEditorTab.setControl(_refEditorTabRoot);
        _refEditor = ComponentBuilder.instance().createMessageReferencesEditor(_refEditorTabRoot);
        
        _threadTab = new CTabItem(_pageTabs, SWT.NONE);
        _threadTabRoot = new Composite(_pageTabs, SWT.NONE);
        _threadTabRoot.setLayout(new FillLayout());
        _threadTab.setControl(_threadTabRoot);
        _threadTree = ComponentBuilder.instance().createMessageTree(_threadTabRoot, new MessageTree.MessageTreeListener() {
            public void messageSelected(MessageTree tree, SyndieURI uri, boolean toView, boolean nodelay) {
                if (toView)
                    _navControl.view(uri);
            }
            public void filterApplied(MessageTree tree, SyndieURI searchURI) {}
        }, true);
        _threadTree.setFilterable(false);
    }
    
    private static final String T_REFTAB = "syndie.gui.messageeditor.reftab";
    private static final String T_THREADTAB = "syndie.gui.messageeditor.threadtab";
    
    private void initHeader() {
        Composite header = new Composite(_root, SWT.NONE);
        header.setLayout(new GridLayout(5, false));
        header.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _authorLabel = new Label(header, SWT.NONE);
        _authorLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _authorCombo = new Combo(header, SWT.DROP_DOWN | SWT.READ_ONLY);
        _authorCombo.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false, 4, 1));
        _authorCombo.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickFrom(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickFrom(); }
            private void pickFrom() {
                int idx = _authorCombo.getSelectionIndex();
                if ( (idx >= 0) && (idx < _authorHashes.size()) )
                    pickAuthor((Hash)_authorHashes.get(idx));
            }
        });
        
        _signAsLabel = new Label(header, SWT.NONE);
        _signAsLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _signAs = new Combo(header, SWT.DROP_DOWN | SWT.READ_ONLY);
        _signAs.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));
        _authorHidden = new Button(header, SWT.CHECK);
        _authorHidden.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _signAsLabel.setVisible(false);
        ((GridData)_signAsLabel.getLayoutData()).exclude = true;
        _signAs.setVisible(false);
        ((GridData)_signAs.getLayoutData()).exclude = true;
        _authorHidden.setVisible(false);
        ((GridData)_authorHidden.getLayoutData()).exclude = true;
        
        _toLabel = new Label(header, SWT.NONE);
        _toLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _to = new Combo(header, SWT.DROP_DOWN | SWT.READ_ONLY);
        _to.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        _to.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickTo(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickTo(); }
            private void pickTo() {
                int idx = _to.getSelectionIndex();
                if (idx >= _forumHashes.size())
                    pickOtherForum();
                else if (idx >= 0)
                    pickForum((Hash)_forumHashes.get(idx));
            }
        });
        
        _subjectLabel = new Label(header, SWT.NONE);
        _subjectLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _subject = new Text(header, SWT.BORDER | SWT.SINGLE);
        _subject.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        
        _tagLabel = new Label(header, SWT.NONE);
        _tagLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _tag = new Text(header, SWT.BORDER | SWT.SINGLE);
        _tag.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _privacyLabel = new Label(header, SWT.NONE);
        _privacyLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _privacy = new Combo(header, SWT.DROP_DOWN | SWT.READ_ONLY);
        _privacy.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        _privacy.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickPrivacy(_privacy.getSelectionIndex()); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickPrivacy(_privacy.getSelectionIndex()); }
        });
        
        _subjectLabel.setText("Subject:");
        _tagLabel.setText("Tags:");
        _authorLabel.setText("Author:");
        _signAsLabel.setText("Signed by:");
        _authorHidden.setText("Hidden?");
        _to.setText("Forum:");
        _privacyLabel.setText("Privacy:");
    }
    
    private void initToolbar() {
        _toolbar = new Composite(_root, SWT.NONE);
        RowLayout rl = new RowLayout(SWT.HORIZONTAL);
        rl.wrap = false;
        rl.fill = true;
        _toolbar.setLayout(rl);
        _toolbar.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        initForumControl();
        initAuthorControl();
        initPrivacyControl();
        initPageControl();
        initAttachControl();
        initLinkControl();
        initStyleControl();
        initSpellControl();
        initSearchControl();
        initQuoteControl();
    }
    
    private List _forumHashes = new ArrayList();
    
    private void updateForum() {
        MenuItem items[] = _forumMenu.getItems();
        for (int i = 0; i < items.length; i++)
            items[i].dispose();
        
        _forumHashes.clear();
        _to.removeAll();
        
        boolean targetFound = false;
        
        long forumId = -1;
        String forumSummary = "";
        boolean managed = false;
        _dataControl.getUI().debugMessage("updateForum: " + _forum);
        
        boolean itemsSinceSep = false;
        
        for (int i = 0; i < _nymChannels.getIdentityChannelCount(); i++) {
            itemsSinceSep = true;
            final ChannelInfo info = _nymChannels.getIdentityChannel(i);
            
            StringBuffer buf = new StringBuffer();
            if ( (info.getName() != null) && (info.getName().length() > 0) )
                buf.append(info.getName());
            if (buf.length() > 0)
                buf.append(": ");
            if ( (info.getDescription() != null) && (info.getDescription().length() > 0) )
                buf.append(info.getDescription());
            if (buf.length() > 0)
                buf.append(": ");
            buf.append(info.getChannelHash().toBase64().substring(0,6));
            
            final String summary = buf.toString();
            _dataControl.getUI().debugMessage("summary: " + summary);
            
            if (_forum == null) {
                _forum = info.getChannelHash();
                forumId = info.getChannelId();
                forumSummary = summary;
                managed = true;
            } else if (_forum.equals(info.getChannelHash())) {
                forumId = info.getChannelId();
                forumSummary = summary;
                managed = true;
            }
            
            _forumHashes.add(info.getChannelHash());
            _to.add(summary);
            
            MenuItem item = new MenuItem(_forumMenu, SWT.PUSH);
            item.setText(summary);
            item.setData("channel.hash", info.getChannelHash());
            item.setData("channel.managed", Boolean.TRUE);
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickForum(info.getChannelHash(), info.getChannelId(), summary, true); }
                public void widgetSelected(SelectionEvent selectionEvent) { pickForum(info.getChannelHash(), info.getChannelId(), summary, true); }
            });
            if ( (_forum != null) && (_forum.equals(info.getChannelHash())))
                targetFound = true;
        }
        if (itemsSinceSep) {
            new MenuItem(_forumMenu, SWT.SEPARATOR);
            itemsSinceSep = false;
        }
        for (int i = 0; i < _nymChannels.getManagedChannelCount(); i++) {
            itemsSinceSep = true;
            final ChannelInfo info = _nymChannels.getManagedChannel(i);

            StringBuffer buf = new StringBuffer();
            if ( (info.getName() != null) && (info.getName().length() > 0) )
                buf.append(info.getName());
            if (buf.length() > 0)
                buf.append(": ");
            if ( (info.getDescription() != null) && (info.getDescription().length() > 0) )
                buf.append(info.getDescription());
            if (buf.length() > 0)
                buf.append(": ");
            buf.append(info.getChannelHash().toBase64().substring(0,6));
            
            final String summary = buf.toString();
            _dataControl.getUI().debugMessage("summary: " + summary);

            if (_forum == null) {
                _forum = info.getChannelHash();
                forumId = info.getChannelId();
                forumSummary = summary;
                managed = true;
            } else if (_forum.equals(info.getChannelHash())) {
                forumId = info.getChannelId();
                forumSummary = summary;
                managed = true;
            }

            _forumHashes.add(info.getChannelHash());
            _to.add(summary);
            
            MenuItem item = new MenuItem(_forumMenu, SWT.PUSH);
            item.setText(summary);
            item.setData("channel.hash", info.getChannelHash());
            item.setData("channel.managed", Boolean.TRUE);
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickForum(info.getChannelHash(), info.getChannelId(), summary, true); }
                public void widgetSelected(SelectionEvent selectionEvent) { pickForum(info.getChannelHash(), info.getChannelId(), summary, true); }
            });
            if ( (_forum != null) && (_forum.equals(info.getChannelHash())))
                targetFound = true;            
        }
        if (itemsSinceSep) {
            new MenuItem(_forumMenu, SWT.SEPARATOR);
            itemsSinceSep = false;
        }
        for (int i = 0; i < _nymChannels.getPostChannelCount(); i++) {
            itemsSinceSep = true;
            final ChannelInfo info = _nymChannels.getPostChannel(i);
            
            StringBuffer buf = new StringBuffer();
            if ( (info.getName() != null) && (info.getName().length() > 0) )
                buf.append(info.getName());
            if (buf.length() > 0)
                buf.append(": ");
            if ( (info.getDescription() != null) && (info.getDescription().length() > 0) )
                buf.append(info.getDescription());
            if (buf.length() > 0)
                buf.append(": ");
            buf.append(info.getChannelHash().toBase64().substring(0,6));
            
            final String summary = buf.toString();
            _dataControl.getUI().debugMessage("summary: " + summary);
            
            if (_forum == null) {
                _forum = info.getChannelHash();
                forumId = info.getChannelId();
                forumSummary = summary;
                managed = true;
            } else if (_forum.equals(info.getChannelHash())) {
                forumId = info.getChannelId();
                forumSummary = summary;
                managed = true;
            }
            
            _forumHashes.add(info.getChannelHash());
            _to.add(summary);
            
            MenuItem item = new MenuItem(_forumMenu, SWT.PUSH);
            item.setText(summary);
            item.setData("channel.hash", info.getChannelHash());
            item.setData("channel.managed", Boolean.TRUE);
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickForum(info.getChannelHash(), info.getChannelId(), summary, true); }
                public void widgetSelected(SelectionEvent selectionEvent) { pickForum(info.getChannelHash(), info.getChannelId(), summary, true); }
            });
            if ( (_forum != null) && (_forum.equals(info.getChannelHash())))
                targetFound = true;
        }
        if (itemsSinceSep) {
            new MenuItem(_forumMenu, SWT.SEPARATOR);
            itemsSinceSep = false;
        }
        for (int i = 0; i < _nymChannels.getPublicPostChannelCount(); i++) {
            itemsSinceSep = true;
            final ChannelInfo info = _nymChannels.getPublicPostChannel(i);
            
            StringBuffer buf = new StringBuffer();
            if ( (info.getName() != null) && (info.getName().length() > 0) )
                buf.append(info.getName());
            if (buf.length() > 0)
                buf.append(": ");
            if ( (info.getDescription() != null) && (info.getDescription().length() > 0) )
                buf.append(info.getDescription());
            if (buf.length() > 0)
                buf.append(": ");
            buf.append(info.getChannelHash().toBase64().substring(0,6));
            
            final String summary = buf.toString();
            _dataControl.getUI().debugMessage("summary: " + summary);
            
            if (_forum == null) {
                _forum = info.getChannelHash();
                forumId = info.getChannelId();
                forumSummary = summary;
                managed = false;
            } else if (_forum.equals(info.getChannelHash())) {
                forumId = info.getChannelId();
                forumSummary = summary;
                managed = false;
            }
            
            _forumHashes.add(info.getChannelHash());
            _to.add(summary);
            
            MenuItem item = new MenuItem(_forumMenu, SWT.PUSH);
            item.setText(summary);
            item.setData("channel.hash", info.getChannelHash());
            item.setData("channel.managed", Boolean.FALSE);
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickForum(info.getChannelHash(), info.getChannelId(), summary, false); }
                public void widgetSelected(SelectionEvent selectionEvent) { pickForum(info.getChannelHash(), info.getChannelId(), summary, false); }
            });
            if ( (_forum != null) && (_forum.equals(info.getChannelHash())))
                targetFound = true;
        }
        
        if (!targetFound && (_forum != null)) {
            // other forum chosen
            long id = _dataControl.getClient().getChannelId(_forum);
            if (id >= 0) {
                if (itemsSinceSep)
                    new MenuItem(_forumMenu, SWT.SEPARATOR);
                final ChannelInfo info = _dataControl.getClient().getChannel(id);

                StringBuffer buf = new StringBuffer();
                if ( (info.getName() != null) && (info.getName().length() > 0) )
                    buf.append(info.getName());
                if (buf.length() > 0)
                    buf.append(": ");
                if ( (info.getDescription() != null) && (info.getDescription().length() > 0) )
                    buf.append(info.getDescription());
                if (buf.length() > 0)
                    buf.append(": ");
                buf.append(info.getChannelHash().toBase64().substring(0,6));

                final String summary = buf.toString();
                _dataControl.getUI().debugMessage("summary: " + summary);

                forumId = info.getChannelId();
                forumSummary = summary;
            
                _forumHashes.add(info.getChannelHash());
                _to.add(summary);

                MenuItem item = new MenuItem(_forumMenu, SWT.PUSH);
                item.setText(summary);
                item.setData("channel.hash", info.getChannelHash());
                item.setData("channel.managed", Boolean.FALSE);
                item.addSelectionListener(new SelectionListener() {
                    public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickForum(info.getChannelHash(), info.getChannelId(), summary, false); }
                    public void widgetSelected(SelectionEvent selectionEvent) { pickForum(info.getChannelHash(), info.getChannelId(), summary, false); }
                });
                redrawForumAvatar(_forum, info.getChannelId(), summary, false);
            }
        } else if (!targetFound) {
            if (itemsSinceSep)
                new MenuItem(_forumMenu, SWT.SEPARATOR);
            
            _to.add("other...");
            
            MenuItem item = new MenuItem(_forumMenu, SWT.PUSH);
            item.setText("other...");
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickOtherForum(); }
                public void widgetSelected(SelectionEvent selectionEvent) { pickOtherForum(); }
            });
        } else {
            if (itemsSinceSep)
                new MenuItem(_forumMenu, SWT.SEPARATOR);
            
            _to.add("other...");
            
            MenuItem item = new MenuItem(_forumMenu, SWT.PUSH);
            item.setText("other...");
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickOtherForum(); }
                public void widgetSelected(SelectionEvent selectionEvent) { pickOtherForum(); }
            });
            redrawForumAvatar(_forum, forumId, forumSummary, managed);
        }
    }
    
    private static final String T_PICK_FORUM_KEY = "syndie.gui.messageeditor.pickotherforum";
    private void pickOtherForum() {
        if (_refChooser == null) {
            String transKey = T_PICK_FORUM_KEY;
            String transDefaultVal = "Forum chooser";
            _refChooser = ComponentBuilder.instance().createReferenceChooserPopup(_root.getShell(), new ReferenceChooserTree.AcceptanceListener() {
                public void referenceAccepted(SyndieURI uri) {
                    if (uri.isSearch()) {
                        Hash scopes[] = uri.getSearchScopes();
                        if ( (scopes != null) && (scopes.length > 0) )
                            _forum = scopes[0];
                    } else if (uri.isChannel()) {
                        _forum = uri.getScope();
                    }
                    _dataControl.getUI().debugMessage("other forum picked: " + uri);
                    updateForum();
                    refreshAuthors();
                    if (!validateAuthorForum())
                        showUnauthorizedWarning();
                }

                public void referenceChoiceAborted() {
                    _dataControl.getUI().debugMessage("other forum selection aborted");
                }
            }, transKey, transDefaultVal);
        }
        _dataControl.getUI().debugMessage("picking other forum...");
        _refChooser.show();
    }

    private void pickForum(Hash forum) {
        modified();
        _forum = forum;
        MenuItem items[] = _forumMenu.getItems();
        for (int i = 0; i < items.length; i++) {
            Hash cur = (Hash)items[i].getData("channel.hash");
            Boolean managed = (Boolean)items[i].getData("channel.managed");
            if (managed == null) 
                managed = Boolean.FALSE;
            if ( (cur != null) && (cur.equals(forum)) ) {
                redrawForumAvatar(cur, _dataControl.getClient().getChannelId(cur), items[i].getText(), managed.booleanValue());
                break;
            }
        }
        refreshAuthors();
        if (!validateAuthorForum())
            showUnauthorizedWarning();
    }
    private void pickForum(Hash forum, long channelId, String summary, boolean isManaged) {
        _dataControl.getUI().debugMessage("pick forum " + forum + " / " + summary);
        _forum = forum;
        redrawForumAvatar(forum, channelId, summary, isManaged);
        refreshAuthors();
        if (!validateAuthorForum())
            showUnauthorizedWarning();
    }
    private void redrawForumAvatar(Hash forum, long channelId, String summary, boolean isManaged) {
        _forumButton.setRedraw(false);
        for (int i = 0; i < _forumHashes.size(); i++) {
            Hash h = (Hash)_forumHashes.get(i);
            if (h.equals(forum)) {
                _to.select(i);
                break;
            }
        }
        ImageUtil.dispose(_forumAvatar);
        _forumButton.setImage(null);
        if (channelId >= 0) {
            // don't show the forum avatar unless the forum is bookmarked or we own the channel -
            // this should help fight phishing attacks (to prevent spoofing w/ same 
            // icon & link <a href=...>send me your password</a>)
            if (isManaged || _bookmarkControl.isBookmarked(SyndieURI.createScope(forum))) {
                byte avatar[] = _dataControl.getClient().getChannelAvatar(channelId);
                if (avatar != null) {
                    _forumAvatar = ImageUtil.createImage(avatar);
                    _forumButton.setImage(_forumAvatar);
                } else {
                    _forumButton.setImage(ImageUtil.ICON_EDITOR_BOOKMARKED_NOAVATAR);
                }
            } else {
                _forumButton.setImage(ImageUtil.ICON_EDITOR_NOT_BOOKMARKED);
            }
            _forumButton.setToolTipText(summary);
        } else {
            _forumButton.setImage(ImageUtil.ICON_EDITOR_NOT_BOOKMARKED);
            _forumButton.setToolTipText("");
        }
        _forumButton.setRedraw(true);
    }
    
    private void initForumControl() {
        _forumGroup = new Group(_toolbar, SWT.SHADOW_ETCHED_IN);
        //ctl.setLayoutData(new RowData(48, 48));
        _forumGroup.setLayoutData(new RowData(50, 50));
        _forumGroup.setLayout(new FillLayout());
        
        _forumButton = new Button(_forumGroup, SWT.PUSH);
        
        _forumMenu = new Menu(_forumButton);
        _forumGroup.setMenu(_forumMenu);
        _forumButton.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _forumMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _forumMenu.setVisible(true); }
        });
        
        _forumGroup.setText("Post to:");
        _forumGroup.setToolTipText("Select the forum to post in");
    }
    
    private void refreshAuthors() {
        List signAsKeys = null;
        boolean explicitKey = false;
        if (_forum != null) {
            List nymKeys = _dataControl.getClient().getNymKeys(_forum, null);
            for (int i = 0; i < nymKeys.size(); i++) {
                NymKey key = (NymKey)nymKeys.get(i);
                if (!key.getAuthenticated()) {
                    _dataControl.getUI().debugMessage("key is not authenticated: " + key);
                    continue;
                }
                if (key.getIsExpired()) {
                    _dataControl.getUI().debugMessage("key is expired: " + key);
                    continue;
                }
                if (Constants.KEY_FUNCTION_MANAGE.equals(key.getFunction()) ||
                    Constants.KEY_FUNCTION_POST.equals(key.getFunction())) {
                    if (signAsKeys == null) signAsKeys = new ArrayList();
                    signAsKeys.add(key);
                    if (!key.isIdentity())
                        explicitKey = true;
                }
            }
        }
        _dataControl.getUI().debugMessage("refreshing authors: forum=" + _forum + " signAs keys: " + signAsKeys);
        
        GridData authorGD = (GridData)_authorCombo.getLayoutData();
        GridData signAsGD = (GridData)_signAs.getLayoutData();
        GridData signAsLabelGD = (GridData)_signAsLabel.getLayoutData();
        GridData authorHiddenGD = (GridData)_authorHidden.getLayoutData();
        if ( (signAsKeys != null) && ( (signAsKeys.size() > 1) || explicitKey) ) {
            // multiple possible authors, so we need to populate and show the _signAs* fields
            signAsGD.exclude = false;
            signAsLabelGD.exclude = false;
            authorHiddenGD.exclude = false;
            _signAs.setVisible(true);
            _signAsLabel.setVisible(true);
            _authorHidden.setVisible(true);
            authorGD.horizontalSpan = 1;
            
            _signAs.removeAll();
            _signAsHashes.clear();
            boolean selected = false;
            for (int i = 0; i < signAsKeys.size(); i++) {
                NymKey key = (NymKey)signAsKeys.get(i);
                SigningPrivateKey priv = new SigningPrivateKey(key.getData());
                Hash pubHash = priv.toPublic().calculateHash();
                String name = _dataControl.getClient().getChannelName(pubHash);
                if (name != null)
                    _signAs.add(name + " (" + pubHash.toBase64().substring(0,6) + ")");
                else
                    _signAs.add("(" + pubHash.toBase64().substring(0,6) + ")");
                _signAsHashes.add(pubHash);
                if (pubHash.equals(_forum)) {
                    _signAs.select(i);
                    selected = true;
                }
            }
            if (!selected)
                _signAs.select(0);
        } else {
            // only one (or zero) possible authors, so hide the _from* fields and make sure
            // _signBy contains all of the known authors
            signAsGD.exclude = true;
            signAsLabelGD.exclude = true;
            authorHiddenGD.exclude = true;
            _signAs.setVisible(false);
            _signAsLabel.setVisible(false);
            _authorHidden.setVisible(false);
            authorGD.horizontalSpan = 4;
        }
        // relayout the header
        _signAs.getParent().layout(true, true);
    }
    
    private List _authorHashes = new ArrayList();
    private void updateAuthor() {
        MenuItem items[] = _authorMenu.getItems();
        for (int i = 0; i < items.length; i++)
            items[i].dispose();
        
        _authorHashes.clear();
        _authorCombo.removeAll();
        boolean authorFound = false;
        
        long authorId = -1;
        String authorSummary = "";
        _dataControl.getUI().debugMessage("updateAuthor: " + _author);
        
        boolean itemsSinceSep = false;
        
        for (int i = 0; i < _nymChannels.getIdentityChannelCount(); i++) {
            itemsSinceSep = true;
            final ChannelInfo info = _nymChannels.getIdentityChannel(i);
            
            StringBuffer buf = new StringBuffer();
            if ( (info.getName() != null) && (info.getName().length() > 0) )
                buf.append(info.getName());
            if (buf.length() > 0)
                buf.append(": ");
            if ( (info.getDescription() != null) && (info.getDescription().length() > 0) )
                buf.append(info.getDescription());
            if (buf.length() > 0)
                buf.append(": ");
            buf.append(info.getChannelHash().toBase64().substring(0,6));
            
            final String summary = buf.toString();
            _dataControl.getUI().debugMessage("summary: " + summary);
            
            if (_author == null) {
                _author = info.getChannelHash();
                authorId = info.getChannelId();
                authorSummary = summary;
            } else if (_author.equals(info.getChannelHash())) {
                authorId = info.getChannelId();
                authorSummary = summary;
            }
            
            _authorHashes.add(info.getChannelHash());
            _authorCombo.add(summary);
                    
            MenuItem item = new MenuItem(_authorMenu, SWT.PUSH);
            item.setText(summary);
            item.setData("channel.hash", info.getChannelHash());
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickAuthor(info.getChannelHash(), info.getChannelId(), summary); }
                public void widgetSelected(SelectionEvent selectionEvent) { pickAuthor(info.getChannelHash(), info.getChannelId(), summary); }
            });
            if ( (_author != null) && (_author.equals(info.getChannelHash())))
                authorFound = true;
        }
        if (itemsSinceSep) {
            new MenuItem(_authorMenu, SWT.SEPARATOR);
            itemsSinceSep = false;
        }
        for (int i = 0; i < _nymChannels.getManagedChannelCount(); i++) {
            itemsSinceSep = true;
            final ChannelInfo info = _nymChannels.getManagedChannel(i);

            StringBuffer buf = new StringBuffer();
            if ( (info.getName() != null) && (info.getName().length() > 0) )
                buf.append(info.getName());
            if (buf.length() > 0)
                buf.append(": ");
            if ( (info.getDescription() != null) && (info.getDescription().length() > 0) )
                buf.append(info.getDescription());
            if (buf.length() > 0)
                buf.append(": ");
            buf.append(info.getChannelHash().toBase64().substring(0,6));
            
            final String summary = buf.toString();
            _dataControl.getUI().debugMessage("summary: " + summary);

            if (_author == null) {
                _author = info.getChannelHash();
                authorId = info.getChannelId();
                authorSummary = summary;
            } else if (_author.equals(info.getChannelHash())) {
                authorId = info.getChannelId();
                authorSummary = summary;
            }

            _authorHashes.add(info.getChannelHash());
            _authorCombo.add(summary);
            
            MenuItem item = new MenuItem(_authorMenu, SWT.PUSH);
            item.setText(summary);
            item.setData("channel.hash", info.getChannelHash());
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickAuthor(info.getChannelHash(), info.getChannelId(), summary); }
                public void widgetSelected(SelectionEvent selectionEvent) { pickAuthor(info.getChannelHash(), info.getChannelId(), summary); }
            });
            if ( (_author != null) && (_author.equals(info.getChannelHash())))
                authorFound = true;            
        }
        
        if (_author != null) {
            Properties prefs = _dataControl.getClient().getNymPrefs();
            prefs.setProperty("editor.defaultAuthor", _author.toBase64());
            _dataControl.getClient().setNymPrefs(prefs);
        }
        
        redrawAuthorAvatar(_author, authorId, authorSummary);
    }
    
    private void pickAuthor(Hash author) {
        modified();
        _author = author;
        if (_author != null) {
            Properties prefs = _dataControl.getClient().getNymPrefs();
            prefs.setProperty("editor.defaultAuthor", _author.toBase64());
            _dataControl.getClient().setNymPrefs(prefs);
        }
        MenuItem items[] = _authorMenu.getItems();
        for (int i = 0; i < items.length; i++) {
            Hash cur = (Hash)items[i].getData("channel.hash");
            if ( (cur != null) && (cur.equals(author)) ) {
                redrawAuthorAvatar(cur, _dataControl.getClient().getChannelId(cur), items[i].getText());
                break;
            }
        }
        if (!validateAuthorForum())
            showUnauthorizedWarning();
    }
    private void pickAuthor(Hash author, long channelId, String summary) {
        _dataControl.getUI().debugMessage("pick author " + author + " / " + summary);
        _author = author;
        if (_author != null) {
            Properties prefs = _dataControl.getClient().getNymPrefs();
            prefs.setProperty("editor.defaultAuthor", _author.toBase64());
            _dataControl.getClient().setNymPrefs(prefs);
        }
        redrawAuthorAvatar(author, channelId, summary);
        if (!validateAuthorForum())
            showUnauthorizedWarning();
    }
    private void redrawAuthorAvatar(Hash author, long channelId, String summary) {
        _authorButton.setRedraw(false);
        for (int i = 0; i < _authorHashes.size(); i++) {
            Hash h = (Hash)_authorHashes.get(i);
            if (h.equals(author)) {
                _authorCombo.select(i);
                break;
            }
        }
        ImageUtil.dispose(_authorAvatar);
        _authorButton.setImage(null);
        byte avatar[] = _dataControl.getClient().getChannelAvatar(channelId);
        if (avatar != null) {
            _authorAvatar = ImageUtil.createImage(avatar);
            _authorButton.setImage(_authorAvatar);
        } else {
            _authorButton.setImage(ImageUtil.ICON_EDITOR_BOOKMARKED_NOAVATAR);
        }
        _authorButton.setToolTipText(summary);
        _authorButton.setRedraw(true);
    }    
    
    private void initAuthorControl() {
        _authorGroup = new Group(_toolbar, SWT.SHADOW_ETCHED_IN);
        _authorGroup.setLayoutData(new RowData(50, 50));
        _authorGroup.setLayout(new FillLayout());
        
        _authorButton = new Button(_authorGroup, SWT.PUSH);
        _authorButton.setSize(48, 48);
        //_authorButton.setImage(ImageUtil.resize(ImageUtil.ICON_QUESTION, 48, 48, false));
        
        _authorMenu = new Menu(_authorButton);
        _authorGroup.setMenu(_authorMenu);
        _authorButton.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _authorMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _authorMenu.setVisible(true); }
        });
        
        _authorGroup.setText("Author:");
        _authorGroup.setToolTipText("Who do you want to sign the post as?");
    }
    
    /** 
     * make sure the author selected has the authority to post to the forum selected (or to
     * reply to an existing message, if we are replying)
     */
    private boolean validateAuthorForum() {
        Hash author = _author;
        ChannelInfo forum = null;
        if (_forum != null)
            forum = _dataControl.getClient().getChannel(_dataControl.getClient().getChannelId(_forum));
        
        boolean ok = true;
        
        _dataControl.getUI().debugMessage("validating author forum: author=" + _author + " forum=" + _forum);
        
        if ( (author != null) && (forum != null) ) {
            if (author.equals(forum.getChannelHash())) {
                // ok
                _dataControl.getUI().debugMessage("forum == author");
            } else if (forum.getAllowPublicPosts()) {
                // ok too
                _dataControl.getUI().debugMessage("forum allows public posts");
            } else if (forum.getAuthorizedManagerHashes().contains(author)) {
                // yep
                _dataControl.getUI().debugMessage("forum explicitly allowes the author to manage the forum");
            } else if (forum.getAuthorizedPosterHashes().contains(author)) {
                // again
                _dataControl.getUI().debugMessage("forum explicitly allows the author to post in the forum");
            } else if (_privacy.getSelectionIndex() == PRIVACY_REPLY) {
                // sure... though it won't go in the forum's scope
                _dataControl.getUI().debugMessage("post is a private reply");
            } else if (forum.getAllowPublicReplies() && (_parents.size() > 0) ) {
                // maybe... check to make sure the parent is allowed
                _dataControl.getUI().debugMessage("forum allows public replies, and our parents: " + _parents);
                boolean allowed = false;
                for (int i = _parents.size()-1; !allowed && i >= 0; i--) {
                    SyndieURI uri = (SyndieURI)_parents.get(i);
                    Hash scope = uri.getScope();
                    if (forum.getChannelHash().equals(scope) ||
                        forum.getAuthorizedManagerHashes().contains(scope) ||
                        forum.getAuthorizedPosterHashes().contains(scope)) {
                        // the scope is authorized, but make sure the uri is actually pointing to
                        // a post in the targetted forum!
                        long msgId = _dataControl.getClient().getMessageId(scope, uri.getMessageId());
                        if (msgId >= 0) {
                            long targetChanId = _dataControl.getClient().getMessageTarget(msgId);
                            if (forum.getChannelId() == targetChanId) {
                                allowed = true;
                            } else {
                                _dataControl.getUI().debugMessage("ancestor would be authorized, but they are targetting a different forum: " + targetChanId + ": " + uri);
                            }
                        } else {
                            _dataControl.getUI().debugMessage("ancestor would be authorized, but isn't known, so we don't know whether they're actually targetting the right forum: " + uri);
                        }
                    }
                }
                if (!allowed) {
                    // none of the ancestors were allowed, so reject
                    _dataControl.getUI().debugMessage("forum allows public replies but the parents are not authorized");
                    ok = false;
                }
            } else {
                // not allowed
                _dataControl.getUI().debugMessage("forum not allowed");
                ok = false;
            }
        }
        
        if (!ok && (forum != null) && (_signAsHashes.size() > 0)) {
            int idx = _signAs.getSelectionIndex();
            if ( (idx >= 0) && (idx < _signAsHashes.size()) ) {
                Hash signAs = (Hash)_signAsHashes.get(idx);
            
                // the author may not be allowed, but the nym has an explicitly authorized private key
                // for posting or managing the forum.  note that the *nym* may have the key, but where they got
                // the key may only be possible for one or more of the nym's channels, and using another channel
                // as the author would link the channel that was authorized to receive the private key and the
                // channel that posted with the key.  the safe way to behave would be to run different unlinkable
                // nyms in their own Syndie instance, syncing between the instances without sharing any secrets
                List nymKeys = _dataControl.getClient().getNymKeys(forum.getChannelHash(), null);
                for (int i = 0; i < nymKeys.size(); i++) {
                    NymKey key = (NymKey)nymKeys.get(i);
                    if (!key.getAuthenticated()) continue;
                    if (key.getIsExpired()) continue;
                    if (Constants.KEY_TYPE_DSA.equals(key.getType())) {
                        SigningPrivateKey priv = new SigningPrivateKey(key.getData());
                        if (priv.toPublic().calculateHash().equals(signAs)) {
                            _dataControl.getUI().debugMessage("Explicitly authorized 'sign as' key selected: " + signAs);
                            ok = true;
                            break;
                        }
                    }
                }
            }
        }
        
        return ok;
    }
    private void showUnauthorizedWarning() {
        MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_ERROR | SWT.OK);
        box.setMessage(_dataControl.getTranslationRegistry().getText(T_NOT_AUTHORIZED_MSG, "The selected author does not have permission to write in the selected forum - please adjust your selection"));
        box.setText(_dataControl.getTranslationRegistry().getText(T_NOT_AUTHORIZED_TITLE, "Not authorized"));
        box.open();
    }
    
    private static final String T_NOT_AUTHORIZED_MSG = "syndie.gui.messageeditor.notauthorized.msg";
    private static final String T_NOT_AUTHORIZED_TITLE = "syndie.gui.messageeditor.notauthorized.title";
    
    private void initPrivacyControl() {
        _privGroup = new Group(_toolbar, SWT.SHADOW_ETCHED_IN);
        _privGroup.setLayout(new FillLayout());
        
        _privButton = new Button(_privGroup, SWT.PUSH);
        
        _privMenu = new Menu(_privButton);
        _privGroup.setMenu(_privMenu);
        _privButton.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _privMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _privMenu.setVisible(true); }
        });
        
        _privPublic = new MenuItem(_privMenu, SWT.PUSH);
        _privAuthorized = new MenuItem(_privMenu, SWT.PUSH);
        _privPBE = new MenuItem(_privMenu, SWT.PUSH);
        _privReply = new MenuItem(_privMenu, SWT.PUSH);
        
        _privPublic.setImage(ImageUtil.ICON_EDITOR_PRIVACY_PUBLIC);
        _privAuthorized.setImage(ImageUtil.ICON_EDITOR_PRIVACY_AUTHORIZED);
        _privPBE.setImage(ImageUtil.ICON_EDITOR_PRIVACY_PBE);
        _privReply.setImage(ImageUtil.ICON_EDITOR_PRIVACY_REPLY);
        
        _privPublic.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { pickPrivacy(0); }
            public void widgetSelected(SelectionEvent evt) { pickPrivacy(0); }
        });
        _privAuthorized.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { pickPrivacy(1);}
            public void widgetSelected(SelectionEvent evt) { pickPrivacy(1); }
        });
        _privPBE.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { pickPrivacy(2); }
            public void widgetSelected(SelectionEvent evt) { pickPrivacy(2); }
        });
        _privReply.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { pickPrivacy(3);}
            public void widgetSelected(SelectionEvent evt) { pickPrivacy(3); }
        });
        
        _privAuthorized.setSelection(true);
        
        _privPublic.setText("Anyone can read the post");
        _privAuthorized.setText("Authorized readers of the forum can read the post");
        _privPBE.setText("Passphrase required to read the post...");
        _privReply.setText("Only forum administrators can read the post");
        _privAuthorized.setSelection(true);
        
        _privGroup.setText("Privacy:");
        _privGroup.setToolTipText("Who is allowed to read the post?");
    }
    
    private void initPageControl() {
        _pageGroup = new Group(_toolbar, SWT.SHADOW_ETCHED_IN);
        _pageGroup.setLayout(new FillLayout());
        
        _pageButton = new Button(_pageGroup, SWT.PUSH);
        _pageButton.setImage(ImageUtil.ICON_EDITOR_PAGEADD);
        
        _pageMenu = new Menu(_pageButton);
        _pageGroup.setMenu(_pageMenu);
        _pageButton.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _pageMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _pageMenu.setVisible(true); }
        });
        
        _pageAdd = new MenuItem(_pageMenu, SWT.PUSH);
        _pageAddWebRip = new MenuItem(_pageMenu, SWT.PUSH);
        
        _pageAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addPage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addPage(); }
        });
        _pageAddWebRip.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addWebRip(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addWebRip(); }
        });
        
        _pageRemove = new MenuItem(_pageMenu, SWT.PUSH);
        _pageRemove.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { removePage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { removePage(); }
        });
        
        _pageGroup.setText("Page:");
        _pageGroup.setToolTipText("Manage pages in this post");
        _pageAdd.setText("Add a new page");
        _pageAddWebRip.setText("Add a new web rip");
        _pageRemove.setText("Remove the current page");
        
        _pageTypeGroup = new Group(_toolbar, SWT.SHADOW_ETCHED_IN);
        _pageTypeGroup.setLayout(new FillLayout());
        _pageType = new Button(_pageTypeGroup, SWT.PUSH);
        _pageType.setImage(ImageUtil.ICON_EDITOR_PAGETYPE_HTML);
        _pageType.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { togglePageType(); }
            public void widgetSelected(SelectionEvent selectionEvent) { togglePageType(); }
        });
        
        _pageTypeGroup.setText("Type:");
    }
    
    private void initAttachControl() {
        _attachGroup = new Group(_toolbar, SWT.SHADOW_ETCHED_IN);
        _attachGroup.setLayout(new FillLayout());
        
        _attachButton = new Button(_attachGroup, SWT.PUSH);
        _attachButton.setImage(ImageUtil.ICON_EDITOR_ATTACH);
        
        _attachMenu = new Menu(_attachButton);
        _attachGroup.setMenu(_attachMenu);
        _attachButton.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _attachMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _attachMenu.setVisible(true); }
        });
        
        _attachAddImage = new MenuItem(_attachMenu, SWT.PUSH);
        _attachAddImage.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showImagePopup(false); }
            public void widgetSelected(SelectionEvent selectionEvent) { showImagePopup(false); }
        });
        _attachAdd = new MenuItem(_attachMenu, SWT.PUSH);
        _attachAdd.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { addAttachment(); }
            public void widgetSelected(SelectionEvent selectionEvent) { addAttachment(); }
        });
        
        _attachGroup.setText("Attach:");
        _attachGroup.setToolTipText("Manage attachments to this post");
        _attachAddImage.setText("Insert a new image");
        _attachAdd.setText("Add a new attachment");
    }
    
    private void initLinkControl() {
        _linkGroup = new Group(_toolbar, SWT.SHADOW_ETCHED_IN);
        _linkGroup.setLayout(new FillLayout());
        
        _linkButton = new Button(_linkGroup, SWT.PUSH);
        _linkButton.setImage(ImageUtil.ICON_EDITOR_LINK);
        
        _linkMenu = new Menu(_linkButton);
        _linkGroup.setMenu(_linkMenu);
        _linkButton.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _linkMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _linkMenu.setVisible(true); }
        });
        
        _linkWeb = new MenuItem(_linkMenu, SWT.PUSH);
        new MenuItem(_linkMenu, SWT.SEPARATOR);
        _linkPage = new MenuItem(_linkMenu, SWT.PUSH);
        _linkAttach = new MenuItem(_linkMenu, SWT.PUSH);
        _linkForum = new MenuItem(_linkMenu, SWT.PUSH);
        _linkMsg = new MenuItem(_linkMenu, SWT.PUSH);
        _linkArchive = new MenuItem(_linkMenu, SWT.PUSH);
        new MenuItem(_linkMenu, SWT.SEPARATOR);
        _linkEepsite = new MenuItem(_linkMenu, SWT.PUSH);
        _linkI2P = new MenuItem(_linkMenu, SWT.PUSH);
        new MenuItem(_linkMenu, SWT.SEPARATOR);
        _linkFreenet = new MenuItem(_linkMenu, SWT.PUSH);
        new MenuItem(_linkMenu, SWT.SEPARATOR);
        _linkOther = new MenuItem(_linkMenu, SWT.PUSH);
        
        _linkPage.setEnabled(false);
        _linkAttach.setEnabled(false);
        
        _linkWeb.setImage(ImageUtil.ICON_REF_URL);
        _linkWeb.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showLinkPopup(true, false, false, false, false, false, false, false, false, false); }
            public void widgetSelected(SelectionEvent selectionEvent) { showLinkPopup(true, false, false, false, false, false, false, false, false, false); }
        });
        _linkPage.setImage(ImageUtil.ICON_REF_SYNDIE);
        _linkPage.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showLinkPopup(false, true, false, false, false, false, false, false, false, false); }
            public void widgetSelected(SelectionEvent selectionEvent) { showLinkPopup(false, true, false, false, false, false, false, false, false, false); }
        });
        _linkAttach.setImage(ImageUtil.ICON_REF_SYNDIE);
        _linkAttach.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showLinkPopup(false, false, true, false, false, false, false, false, false, false); }
            public void widgetSelected(SelectionEvent selectionEvent) { showLinkPopup(false, false, true, false, false, false, false, false, false, false); }
        });
        _linkForum.setImage(ImageUtil.ICON_REF_FORUM);
        _linkForum.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showLinkPopup(false, false, false, true, false, false, false, false, false, false); }
            public void widgetSelected(SelectionEvent selectionEvent) { showLinkPopup(false, false, false, true, false, false, false, false, false, false); }
        });
        _linkMsg.setImage(ImageUtil.ICON_REF_MSG);
        _linkMsg.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showLinkPopup(false, false, false, false, true, false, false, false, false, false); }
            public void widgetSelected(SelectionEvent selectionEvent) { showLinkPopup(false, false, false, false, true, false, false, false, false, false); }
        });
        _linkEepsite.setImage(ImageUtil.ICON_REF_SYNDIE);
        _linkEepsite.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showLinkPopup(false, false, false, false, false, false, true, false, false, false); }
            public void widgetSelected(SelectionEvent selectionEvent) { showLinkPopup(false, false, false, false, false, false, true, false, false, false); }
        });
        _linkI2P.setImage(ImageUtil.ICON_REF_SYNDIE);
        _linkI2P.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showLinkPopup(false, false, false, false, false, false, false, true, false, false); }
            public void widgetSelected(SelectionEvent selectionEvent) { showLinkPopup(false, false, false, false, false, false, false, true, false, false); }
        });
        _linkFreenet.setImage(ImageUtil.ICON_REF_FREENET);
        _linkFreenet.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showLinkPopup(false, false, false, false, false, false, false, false, true, false); }
            public void widgetSelected(SelectionEvent selectionEvent) { showLinkPopup(false, false, false, false, false, false, false, false, true, false); }
        });
        _linkArchive.setImage(ImageUtil.ICON_REF_ARCHIVE);
        _linkArchive.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showLinkPopup(false, false, false, false, false, false, false, false, false, true); }
            public void widgetSelected(SelectionEvent selectionEvent) { showLinkPopup(false, false, false, false, false, false, false, false, false, true); }
        });
        _linkOther.setImage(ImageUtil.ICON_REF_SYNDIE);
        _linkOther.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showLinkPopup(false, false, false, true, false, false, false, false, false, false); }
            public void widgetSelected(SelectionEvent selectionEvent) { showLinkPopup(false, false, false, true, false, false, false, false, false, false); }
        });
        
        _linkWeb.setText("Link to a website");
        _linkPage.setText("Link to a page in this message");
        _linkAttach.setText("Link to an attachment in this message");
        _linkForum.setText("Link to a forum");
        _linkMsg.setText("Link to a particular Syndie message");
        _linkEepsite.setText("Link to an I2P eepsite");
        _linkI2P.setText("Link to an I2P destination");
        _linkFreenet.setText("Link to a Freenet freesite");
        _linkArchive.setText("Link to a Syndie archive");
        _linkOther.setText("Link to another Syndie URI");
        
        _linkGroup.setText("Link:");
        _linkGroup.setToolTipText("Add a new link");
    }
    
    private void initStyleControl() {
        _dataControl.getUI().debugMessage("init styleControl");
        _styleGroup = new Group(_toolbar, SWT.SHADOW_ETCHED_IN);
        _styleGroup.setLayout(new FillLayout());
        
        _styleButton = new Button(_styleGroup, SWT.PUSH);
        _styleButton.setImage(ImageUtil.ICON_EDITOR_STYLE);
        
        _styleMenu = new Menu(_styleButton);
        _styleGroup.setMenu(_styleMenu);
        _styleButton.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _styleMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _styleMenu.setVisible(true); }
        });
        
        _styleText = new MenuItem(_styleMenu, SWT.PUSH);
        _styleText.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { styleText(); }
            public void widgetSelected(SelectionEvent selectionEvent) { styleText(); }
        });
        
        _styleImage = new MenuItem(_styleMenu, SWT.PUSH);
        _styleImage.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showImagePopup(false); }
            public void widgetSelected(SelectionEvent selectionEvent) { showImagePopup(false); }
        });
        new MenuItem(_styleMenu, SWT.SEPARATOR);
        
        _styleBGColor = new MenuItem(_styleMenu, SWT.CASCADE);
        _styleBGColorMenu = new Menu(_styleBGColor);
        _styleBGColor.setMenu(_styleBGColorMenu);
        _styleBGColorDefault = new MenuItem(_styleBGColorMenu, SWT.PUSH);
        _styleBGColorDefault .addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { setPageBGColor(null); }
            public void widgetSelected(SelectionEvent selectionEvent) { setPageBGColor(null); }
        });
        ColorUtil.init();
        List names = ColorUtil.getSystemColorNames();
        _dataControl.getUI().debugMessage("color names: " + names);
        for (int i = 0; i < names.size(); i++) {
            final String name = (String)names.get(i);
            Color color = ColorUtil.getColor(name);
            MenuItem item = new MenuItem(_styleBGColorMenu, SWT.PUSH);
            item.setImage(ColorUtil.getSystemColorSwatch(color));
            item.setText(name);
            item.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { setPageBGColor(name); }
                public void widgetSelected(SelectionEvent selectionEvent) { setPageBGColor(name); }
            });
        }
        _styleBGImage = new MenuItem(_styleMenu, SWT.PUSH);
        _styleBGImage.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showImagePopup(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { showImagePopup(true); }
        });
        new MenuItem(_styleMenu, SWT.SEPARATOR);
        
        _styleListOrdered = new MenuItem(_styleMenu, SWT.PUSH);
        _styleListOrdered.addSelectionListener(new InsertListener("<ol>\n\t<li>first list item</li>\n</ol>\n", true));
        _styleListUnordered = new MenuItem(_styleMenu, SWT.PUSH);
        _styleListUnordered.addSelectionListener(new InsertListener("<ul>\n\t<li>first list item</li>\n</ul>\n", true));
        
        new MenuItem(_styleMenu, SWT.SEPARATOR);
        
        _styleHeading = new MenuItem(_styleMenu, SWT.CASCADE);
        _styleHeadingMenu = new Menu(_styleHeading);
        _styleHeading.setMenu(_styleHeadingMenu);
        _styleHeading1 = new MenuItem(_styleHeadingMenu, SWT.PUSH);
        _styleHeading1.addSelectionListener(new InsertListener("<h1>TEXT</h1>", true));
        _styleHeading2 = new MenuItem(_styleHeadingMenu, SWT.PUSH);
        _styleHeading2.addSelectionListener(new InsertListener("<h2>TEXT</h2>", true));
        _styleHeading3 = new MenuItem(_styleHeadingMenu, SWT.PUSH);
        _styleHeading3.addSelectionListener(new InsertListener("<h3>TEXT</h3>", true));
        _styleHeading4 = new MenuItem(_styleHeadingMenu, SWT.PUSH);
        _styleHeading4.addSelectionListener(new InsertListener("<h4>TEXT</h4>", true));
        _styleHeading5 = new MenuItem(_styleHeadingMenu, SWT.PUSH);
        _styleHeading5.addSelectionListener(new InsertListener("<h5>TEXT</h5>", true));
        _stylePre = new MenuItem(_styleMenu, SWT.PUSH);
        _stylePre.addSelectionListener(new InsertListener("<pre>first line\n\tindented line</pre>", true));
        
        _styleGroup.setText("Style:");
        _styleButton.setToolTipText("Insert style elements");
        
        _styleText.setText("Styled text...");
        _styleImage.setText("Image...");
        _styleBGColor.setText("Page background color");
        _styleBGColorDefault.setText("standard");
        _styleBGImage.setText("Page background image...");
        _styleListOrdered.setText("List (ordered)");
        _styleListUnordered.setText("List (unordered)");
        _styleHeading.setText("Heading");
        _styleHeading1.setText("Heading 1 (largest)");
        _styleHeading2.setText("Heading 2");
        _styleHeading3.setText("Heading 3");
        _styleHeading4.setText("Heading 4");
        _styleHeading5.setText("Heading 5 (smallest)");
        _stylePre.setText("Preformatted text");
    }

    private void initSpellControl() {
        _spellGroup = new Group(_toolbar, SWT.SHADOW_ETCHED_IN);
        _spellGroup.setLayout(new FillLayout());
        
        _spellButton = new Button(_spellGroup, SWT.PUSH);
        _spellButton.setImage(ImageUtil.ICON_EDITOR_SPELL);
        _spellButton.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { spellNext(); }
            public void widgetSelected(SelectionEvent selectionEvent) { spellNext(); }
        });
        
        _spellGroup.setText("Spell:");
        _spellButton.setToolTipText("Check the spelling in the current page");
    }
    
    private void initSearchControl() {
        _searchGroup = new Group(_toolbar, SWT.SHADOW_ETCHED_IN);
        _searchGroup.setLayout(new FillLayout());
        
        _searchButton = new Button(_searchGroup, SWT.PUSH);
        _searchButton.setImage(ImageUtil.ICON_EDITOR_SEARCH);
        _searchButton.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { search(); }
            public void widgetSelected(SelectionEvent selectionEvent) { search(); }
        });
        
        _searchGroup.setText("Find:");
        _searchButton.setToolTipText("Find or replace text in the current page");
    }
    
    private void initQuoteControl() {
        _quoteGroup = new Group(_toolbar, SWT.SHADOW_ETCHED_IN);
        _quoteGroup.setLayout(new FillLayout());
        
        _quoteButton = new Button(_quoteGroup, SWT.PUSH);
        _quoteButton.setImage(ImageUtil.ICON_EDITOR_SEARCH);
        _quoteButton.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { quote(); }
            public void widgetSelected(SelectionEvent selectionEvent) { quote(); }
        });
        
        _quoteGroup.setText("Quote:");
        _quoteButton.setToolTipText("Quote a section of the previous message");
    }
    
    public void applyTheme(Theme theme) {
        _authorLabel.setFont(theme.DEFAULT_FONT);
        _authorCombo.setFont(theme.DEFAULT_FONT);
        _signAs.setFont(theme.DEFAULT_FONT);
        _authorHidden.setFont(theme.DEFAULT_FONT);
        _signAsLabel.setFont(theme.DEFAULT_FONT);
        _toLabel.setFont(theme.DEFAULT_FONT);
        _to.setFont(theme.DEFAULT_FONT);
        _subjectLabel.setFont(theme.DEFAULT_FONT);
        _subject.setFont(theme.DEFAULT_FONT);
        _tagLabel.setFont(theme.DEFAULT_FONT);
        _tag.setFont(theme.DEFAULT_FONT);
        _post.setFont(theme.BUTTON_FONT);
        _postpone.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
        _privacyLabel.setFont(theme.DEFAULT_FONT);
        _privacy.setFont(theme.DEFAULT_FONT);
        _pageTabs.setFont(theme.TAB_FONT);
    
        _forumGroup.setFont(theme.DEFAULT_FONT);
        _authorGroup.setFont(theme.DEFAULT_FONT);
        _privGroup.setFont(theme.DEFAULT_FONT);
        _pageGroup.setFont(theme.DEFAULT_FONT);
        _pageTypeGroup.setFont(theme.DEFAULT_FONT);
        _attachGroup.setFont(theme.DEFAULT_FONT);
        _linkGroup.setFont(theme.DEFAULT_FONT);
        _styleGroup.setFont(theme.DEFAULT_FONT);
        _spellGroup.setFont(theme.DEFAULT_FONT);
        _searchGroup.setFont(theme.DEFAULT_FONT);
        _quoteGroup.setFont(theme.DEFAULT_FONT);
        
        _root.layout(true);
    }
    
    private static final String T_ATTACHMENTS_NONE = "syndie.gui.messageeditor.attachments.none";
    
    private static final String T_FROM_LINE = "syndie.gui.messageeditor.fromline";
    private static final String T_TO_LINE = "syndie.gui.messageeditor.toline";
    
    public void translate(TranslationRegistry registry) {
        _authorLabel.setText(registry.getText(T_FROM_LINE, "Author:"));
        _toLabel.setText(registry.getText(T_TO_LINE, "Post to:"));
        
        _refEditorTab.setText(_dataControl.getTranslationRegistry().getText(T_REFTAB, "References"));
        _threadTab.setText(_dataControl.getTranslationRegistry().getText(T_THREADTAB, "Thread"));
    }

    // image popup stuff
    private void addAttachment() {
        FileDialog dialog = new FileDialog(_root.getShell(), SWT.MULTI | SWT.OPEN);
        if (dialog.open() == null) return; // cancelled
        String selected[] = dialog.getFileNames();
        String base = dialog.getFilterPath();
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

    private static final String T_TOOLARGE_MSG = "syndie.gui.messageeditor.toolarge.msg";
    private static final String T_TOOLARGE_TITLE = "syndie.gui.messageeditor.toolarge.title";
    private static final String T_LARGE_MSG = "syndie.gui.messageeditor.large.msg";
    private static final String T_LARGE_TITLE = "syndie.gui.messageeditor.large.title";
    
    private boolean isValidSize(long length) {
        if (length > Constants.MAX_ATTACHMENT_SIZE) {
            MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_ERROR | SWT.OK);
            box.setMessage(_dataControl.getTranslationRegistry().getText(T_TOOLARGE_MSG, "The attachment could not be added, as it exceeds the maximum attachment size (" + Constants.MAX_ATTACHMENT_SIZE/1024 + "KB)"));
            box.setText(_dataControl.getTranslationRegistry().getText(T_TOOLARGE_TITLE, "Too large"));
            box.open();
            return false;
        //} else if (length > _browser.getSyndicationManager().getPushStrategy().maxKBPerMessage*1024) {
        //    MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_ERROR | SWT.YES | SWT.NO);
        //    box.setMessage(_browser.getTranslationRegistry().getText(T_LARGE_MSG, "The attachment exceeds your maximum syndication size, so you will not be able to push this post to others.  Are you sure you want to include this attachment?"));
        //    box.setText(_browser.getTranslationRegistry().getText(T_LARGE_TITLE, "Large attachment"));
        //    int rc = box.open();
        //    if (rc != SWT.YES)
        //        return false;
        }
        return true;
    }
    
    private void addAttachment(File file) {
        saveState();
        modified();
        String fname = file.getName();
        String name = Constants.stripFilename(fname, false);
        String type = WebRipRunner.guessContentType(fname);

        _dataControl.getUI().debugMessage("add attachment(" + fname + ") sz= " + file.length());
        
        if (!isValidSize(file.length()))
            return;
        
        byte data[] = new byte[(int)file.length()];
        try {
            int read = DataHelper.read(new FileInputStream(file), data);
            if (read != data.length) {
                _dataControl.getUI().debugMessage("attachment was the wrong size (" + read + "/" + data.length + ")");
                return;
            }
        } catch (IOException ioe) {
            _dataControl.getUI().debugMessage("Unable to read the attachment", ioe);
            return;
        }
        Properties cfg = new Properties();
        cfg.setProperty(Constants.MSG_ATTACH_CONTENT_TYPE, type);
        cfg.setProperty(Constants.MSG_ATTACH_NAME, name);
        _attachmentConfig.add(cfg);
        _attachmentData.add(data);
        rebuildAttachmentSummaries();
        updateToolbar();
        _dataControl.getUI().debugMessage("Attachment read and added");
    }
    public int addAttachment(String contentType, String name, byte[] data) {
        if (data == null) return -1;
        if (!isValidSize(data.length)) return -1;
        saveState();
        modified();
        int rv = -1;
        Properties cfg = new Properties();
        cfg.setProperty(Constants.MSG_ATTACH_CONTENT_TYPE, contentType);
        cfg.setProperty(Constants.MSG_ATTACH_NAME, name);
        _attachmentConfig.add(cfg);
        _attachmentData.add(data);
        rv = _attachmentData.size();
        rebuildAttachmentSummaries();
        updateToolbar();
        return rv;
    }
    private void removeAttachment(int idx) {
        saveState();
        modified();
        if (_attachmentData.size() > 0) {
            // should this check to make sure there aren't any pages referencing
            // this attachment first?
            _attachmentConfig.remove(idx);
            _attachmentData.remove(idx);
            _attachmentSummary.remove(idx);
        }
        rebuildAttachmentSummaries();
        for (int i = 0; i < _pageEditors.size(); i++) {
            PageEditor ed = (PageEditor)_pageEditors.get(i);
            ed.updated();
        }
        updateToolbar();
    }

    public List getAttachmentDescriptions() { return getAttachmentDescriptions(false); }
    public List getAttachmentDescriptions(boolean imagesOnly) {
        ArrayList rv = new ArrayList();
        for (int i = 0; i < _attachmentConfig.size(); i++) {
            if (imagesOnly) {
                Properties cfg = (Properties)_attachmentConfig.get(i);
                String type = cfg.getProperty(Constants.MSG_ATTACH_CONTENT_TYPE);
                if ( (type == null) || (!type.startsWith("image")) )
                    continue;
            }
            String item = (String)_attachmentSummary.get(i);
            rv.add(item);
        }
        return rv;
    }
    public List getAttachmentNames() {
        ArrayList rv = new ArrayList();
        for (int i = 0; i < _attachmentConfig.size(); i++) {
            Properties cfg = (Properties)_attachmentConfig.get(i);
            rv.add(cfg.getProperty(Constants.MSG_ATTACH_NAME));
        }
        return rv;
    }
    public byte[] getAttachmentData(int attachment) {
        if ( (attachment <= 0) || (attachment > _attachmentData.size()) ) return null;
        return (byte[])_attachmentData.get(attachment-1);
    }
    public byte[] getImageAttachment(int idx) {
        int cur = 0;
        for (int i = 0; i < _attachmentConfig.size(); i++) {
            Properties cfg = (Properties)_attachmentConfig.get(i);
            String type = cfg.getProperty(Constants.MSG_ATTACH_CONTENT_TYPE);
            if ( (type == null) || (!type.startsWith("image")) )
                continue;
            if (cur + 1 == idx)
                return (byte[])_attachmentData.get(i);
            cur++;
        }
        return null;
    }
    public int getImageAttachmentNum(int imageNum) { 
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
    public void updateImageAttachment(int imageNum, String contentType, byte data[]) { 
        modified();
        int cur = 0;
        for (int i = 0; i < _attachmentConfig.size(); i++) {
            Properties cfg = (Properties)_attachmentConfig.get(i);
            String type = cfg.getProperty(Constants.MSG_ATTACH_CONTENT_TYPE);
            if ( (type == null) || (!type.startsWith("image")) )
                continue;
            if (cur == imageNum) {
                cfg.setProperty(Constants.MSG_ATTACH_CONTENT_TYPE, contentType);
                _attachmentConfig.set(cur, data);
                rebuildAttachmentSummaries();
                updateToolbar();
                return;
            }
            cur++;
        }
    }
    private static final String T_ATTACHMENT_VIEW = "syndie.gui.messageeditornew.attachview";
    private static final String T_ATTACHMENT_DELETE = "syndie.gui.messageeditornew.attachdelete";
    private void rebuildAttachmentSummaries() {
        _attachmentSummary.clear();
        if (_attachmentData.size() > 0) {
            for (int i = 0; i < _attachmentData.size(); i++) {
                byte data[] = (byte[])_attachmentData.get(i);
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
                _attachmentSummary.add(buf.toString());
            }
        } else {
            _attachmentSummary.add(_dataControl.getTranslationRegistry().getText(T_ATTACHMENTS_NONE, "none"));
        }
        
        
        MenuItem items[] = _attachMenu.getItems();
        for (int i = 0; i < items.length; i++)
            if ( (items[i] != _attachAdd) && (items[i] != _attachAddImage) )
                items[i].dispose();
        for (int i = 0; i < _attachmentData.size(); i++) {
            MenuItem attachItem = new MenuItem(_attachMenu, SWT.CASCADE);
            attachItem.setText((String)_attachmentSummary.get(i));
            Menu sub = new Menu(attachItem);
            attachItem.setMenu(sub);
            MenuItem view = new MenuItem(sub, SWT.PUSH);
            view.setEnabled(false);
            view.setText(_dataControl.getTranslationRegistry().getText(T_ATTACHMENT_VIEW, "View"));
            MenuItem delete = new MenuItem(sub, SWT.PUSH);
            delete.setText(_dataControl.getTranslationRegistry().getText(T_ATTACHMENT_DELETE, "Delete"));
            final int attachNum = i;
            delete.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { removeAttachment(attachNum); }
                public void widgetSelected(SelectionEvent selectionEvent) { removeAttachment(attachNum); }
            });
        }
    }
    
    private static final String T_PAGE_VIEW = "syndie.gui.messageeditor.pageview";
    private static final String T_PAGE_DELETE = "syndie.gui.messageeditor.pagedelete";
    
    public void insertAtCaret(String html) {
        PageEditor editor = getPageEditor();
        if (editor != null)
            editor.insertAtCaret(html);
    }

    boolean isModifiedSinceOpen() { return _modifiedSinceOpen; }
    SyndieURI getURI() {
        long prevVersion = _postponeVersion;
        saveState();
        SyndieURI rv = null;
        if (_postponeId >= 0) {
            rv = _uriControl.createPostURI(_postponeId, _postponeVersion);
        } else {
            rv = null;
        }
        return rv;
    }
    
    /** simple hook to inert a buffer at the caret */
    private class InsertListener implements SelectionListener {
        private boolean _onNewline;
        private String _toInsert;
        public InsertListener(String toInsert, boolean onNewline) {
            _onNewline = onNewline;
            _toInsert = toInsert;
        }
        public void widgetSelected(SelectionEvent selectionEvent) { insert(); }
        public void widgetDefaultSelected(SelectionEvent selectionEvent) { insert(); }
        private void insert() {
            PageEditor editor = getPageEditor();
            if (editor != null)
                editor.insert(_toInsert, _onNewline);
        }
    }
}

class MessageEditorFind implements Translatable, Themeable {
    private MessageEditor _editor;
    private Shell _findShell;
    private Label _findTextLabel;
    private Text _findText;
    private Label _findReplaceLabel;
    private Text _findReplace;
    private Button _findMatchCase;
    private Button _findWrapAround;
    private Button _findBackwards;
    private Button _findNext;
    private Button _close;
    private Button _replace;
    private Button _replaceAll;
    
    public MessageEditorFind(MessageEditor editor) {
        _editor = editor;
        initComponents();
    }
    
    public void hide() { _findShell.setVisible(false); }
    public void open() {
        _findText.setText("");
        _findReplace.setText("");
        _findBackwards.setSelection(false);
        _findMatchCase.setSelection(false);
        _findWrapAround.setSelection(false);
        _findShell.open();
        _findText.forceFocus();
    }
    public void dispose() {
        _editor.getDataControl().getTranslationRegistry().unregister(this);
        _editor.getDataControl().getThemeRegistry().unregister(this);
        _findShell.dispose();
        
    }
    
    /** current search term used */
    public String getSearchTerm() { return _findText.getText(); }
    /** current replacement for the search term used */
    public String getSearchReplacement() { return _findReplace.getText(); }
    /** are searches case sensitive? */
    public boolean getSearchCaseSensitive() { return _findMatchCase.getSelection(); }
    /** do we want to search backwards? */
    public boolean getSearchBackwards() { return _findBackwards.getSelection(); }
    /** do we want to search around the end/beginning of the page? */
    public boolean getSearchWrap() { return _findWrapAround.getSelection(); }
    
    private void initComponents() {
        _findShell = new Shell(_editor.getPageRoot().getShell(), SWT.DIALOG_TRIM);
        GridLayout gl = new GridLayout(2, false);
        _findShell.setLayout(gl);
    
        _findTextLabel = new Label(_findShell, SWT.NONE);
        _findTextLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _findText = new Text(_findShell, SWT.BORDER | SWT.SINGLE);
        _findText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _findReplaceLabel = new Label(_findShell, SWT.NONE);
        _findReplaceLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _findReplace = new Text(_findShell, SWT.BORDER | SWT.SINGLE);
        _findReplace.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _findMatchCase = new Button(_findShell, SWT.CHECK | SWT.LEFT);
        _findMatchCase.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _findWrapAround = new Button(_findShell, SWT.CHECK | SWT.LEFT);
        _findWrapAround.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _findBackwards = new Button(_findShell, SWT.CHECK | SWT.LEFT);
        _findBackwards.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        Composite actionRow = new Composite(_findShell, SWT.NONE);
        actionRow.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        actionRow.setLayout(new FillLayout(SWT.HORIZONTAL));
        
        _findNext = new Button(actionRow, SWT.PUSH);
        _findNext.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _editor.findNext(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _editor.findNext(); }
        });
        
        _close = new Button(actionRow, SWT.PUSH);
        _close.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _editor.cancelFind(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _editor.cancelFind(); }
        });
        
        _replace = new Button(actionRow, SWT.PUSH);
        _replace.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _editor.findReplace(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _editor.findReplace(); }
        });
        
        _replaceAll = new Button(actionRow, SWT.PUSH);
        _replaceAll.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _editor.findReplaceAll(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _editor.findReplaceAll(); }
        });

        _findText.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) {
                    _findNext.forceFocus();
                    _editor.findNext();
                }
            }
        });
        _findReplace.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) {
                    _replace.forceFocus();
                    _editor.findReplace();
                }
            }
        });
        
        _findShell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; _editor.cancelFind(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        _editor.getDataControl().getTranslationRegistry().register(this);
        _editor.getDataControl().getThemeRegistry().register(this);
    }
 
    public void applyTheme(Theme theme) {
        _findShell.setFont(theme.SHELL_FONT);
        _findTextLabel.setFont(theme.DEFAULT_FONT);
        _findText.setFont(theme.DEFAULT_FONT);
        _findReplaceLabel.setFont(theme.DEFAULT_FONT);
        _findReplace.setFont(theme.DEFAULT_FONT);
        _findMatchCase.setFont(theme.DEFAULT_FONT);
        _findWrapAround.setFont(theme.DEFAULT_FONT);
        _findBackwards.setFont(theme.DEFAULT_FONT);
        _findNext.setFont(theme.BUTTON_FONT);
        _close.setFont(theme.BUTTON_FONT);
        _replace.setFont(theme.BUTTON_FONT);
        _replaceAll.setFont(theme.BUTTON_FONT);
        
        _findShell.pack();
    }
    
    private static final String T_FIND_ROOT = "syndie.gui.messageeditorfind.root";
    private static final String T_FIND_TEXT = "syndie.gui.messageeditorfind.text";
    private static final String T_FIND_REPLACE = "syndie.gui.messageeditorfind.replace";
    private static final String T_FIND_MATCH = "syndie.gui.messageeditorfind.match";
    private static final String T_FIND_WRAP = "syndie.gui.messageeditorfind.wrap";
    private static final String T_FIND_BACKWARDS = "syndie.gui.messageeditorfind.backwards";
    private static final String T_FIND_NEXT = "syndie.gui.messageeditorfind.next";
    private static final String T_FIND_NEXT_TOOLTIP = "syndie.gui.messageeditorfind.nexttooltip";
    private static final String T_FIND_CLOSE = "syndie.gui.messageeditorfind.close";
    private static final String T_FIND_CLOSE_TOOLTIP = "syndie.gui.messageeditorfind.closetooltip";
    private static final String T_FIND_REPLACE_ACTION = "syndie.gui.messageeditorfind.replace.action";
    private static final String T_FIND_REPLACE_ACTION_TOOLTIP = "syndie.gui.messageeditorfind.replace.actiontooltip";
    private static final String T_FIND_REPLACE_ALL_ACTION = "syndie.gui.messageeditorfind.replaceall.action";
    private static final String T_FIND_REPLACE_ALL_ACTION_TOOLTIP = "syndie.gui.messageeditorfind.replaceall.actiontooltip";
    
    public void translate(TranslationRegistry registry) {
        _findShell.setText(registry.getText(T_FIND_ROOT, "Find"));
        _findTextLabel.setText(registry.getText(T_FIND_TEXT, "Find what: "));
        _findReplaceLabel.setText(registry.getText(T_FIND_REPLACE, "Replace with: "));
        _findMatchCase.setText(registry.getText(T_FIND_MATCH, "match case"));
        _findWrapAround.setText(registry.getText(T_FIND_WRAP, "wrap around"));
        _findBackwards.setText(registry.getText(T_FIND_BACKWARDS, "backwards"));
        _findNext.setText(registry.getText(T_FIND_NEXT, "Find next"));
        _findNext.setToolTipText(registry.getText(T_FIND_NEXT_TOOLTIP, "Find the next occurrence of the word"));
        _close.setText(registry.getText(T_FIND_CLOSE, "Close"));
        _close.setToolTipText(registry.getText(T_FIND_CLOSE_TOOLTIP, "Finish searching"));
        _replace.setText(registry.getText(T_FIND_REPLACE_ACTION, "Replace"));
        _replace.setToolTipText(registry.getText(T_FIND_REPLACE_ACTION_TOOLTIP, "Replace the current occurrence of the word"));
        _replaceAll.setText(registry.getText(T_FIND_REPLACE_ALL_ACTION, "Replace all"));
        _replaceAll.setToolTipText(registry.getText(T_FIND_REPLACE_ALL_ACTION_TOOLTIP, "Replace all remaining occurrences of the word"));
    }
}

class MessageEditorSpell implements Themeable, Translatable {
    private MessageEditor _editor;
    private Shell _spellShell;
    private StyledText _spellContext;
    private Label _spellWordLabel;
    private Text _spellWord;
    private Label _spellSuggestionsLabel;
    private Combo _spellSuggestions;
    private Button _spellReplace;
    private Button _spellReplaceAll;
    private Button _spellIgnore;
    private Button _spellIgnoreAll;
    private Button _spellAdd;
    private Button _spellCancel;
    /** list of words we are ignoring for the current spellcheck iteration */
    private ArrayList _spellIgnoreAllList;
    
    public MessageEditorSpell(MessageEditor editor) {
        _editor = editor;
        initComponents();
    }
 
    public void dispose() {
        _editor.getDataControl().getTranslationRegistry().unregister(this);
        _editor.getDataControl().getThemeRegistry().unregister(this);
        _spellShell.dispose();
    }
    
    public String getSpellWordOrig() {
        if (_spellShell.isDisposed()) return null;
        return _spellWord.getText().trim();
    }
    public String getSuggestion() { 
        if (_spellShell.isDisposed()) return null;
        return _spellSuggestions.getText().trim();
    }
    public List getIgnoreAllList() { return _spellIgnoreAllList; }
    public void updateSuggestions(ArrayList suggestions, String lineText, String word) {
        if (_spellShell.isDisposed()) return;
        _spellWord.setText(word);
        _spellSuggestions.removeAll();
        for (int i = 0; i < suggestions.size(); i++)
            _spellSuggestions.add((String)suggestions.get(i));
        _spellSuggestions.select(0);
        _spellContext.setText(lineText);
    }
    public void showSpell(boolean wordSet) {
        if (_spellShell.isDisposed()) return;
        if (wordSet) {
            _spellContext.setLineBackground(0, 1, null);
            _spellWord.setEnabled(true);
            _spellSuggestions.setEnabled(true);
            _spellAdd.setEnabled(false); // todo: user-specific dictionary
            _spellCancel.setEnabled(true);
            _spellCancel.setText(_editor.getDataControl().getTranslationRegistry().getText(T_SPELL_CANCEL, "cancel"));
            _spellIgnore.setEnabled(true);
            _spellIgnoreAll.setEnabled(true);
            _spellReplace.setEnabled(true);
            _spellReplaceAll.setEnabled(true);
        } else {
            _spellContext.setText(_editor.getDataControl().getTranslationRegistry().getText(T_SPELL_END, "End of content reached"));
            _spellContext.setLineBackground(0, 1, ColorUtil.getColor("red", null));
            _spellWord.setText("");
            _spellWord.setEnabled(false);
            _spellSuggestions.removeAll();
            _spellSuggestions.setEnabled(false);
            _spellAdd.setEnabled(false);
            _spellCancel.setEnabled(true);
            _spellCancel.setText(_editor.getDataControl().getTranslationRegistry().getText(T_SPELL_END_OK, "ok"));
            _spellIgnore.setEnabled(false);
            _spellIgnoreAll.setEnabled(false);
            _spellReplace.setEnabled(false);
            _spellReplaceAll.setEnabled(false);
        }
        _spellShell.open();
    }
    
    private void initComponents() {
        _spellShell = new Shell(_editor.getPageRoot().getShell(), SWT.DIALOG_TRIM);
        GridLayout gl = new GridLayout(2, false);
        _spellShell.setLayout(gl);
        
        _spellIgnoreAllList = new ArrayList();
        
        _spellContext = new StyledText(_spellShell, SWT.MULTI | SWT.WRAP | SWT.BORDER | SWT.READ_ONLY);
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalSpan = 2;
        _spellContext.setLayoutData(gd);
        
        _spellWordLabel = new Label(_spellShell, SWT.NONE);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = false;
        _spellWordLabel.setLayoutData(gd);
        _spellWord = new Text(_spellShell, SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE | SWT.LEFT);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        _spellWord.setLayoutData(gd);

        _spellSuggestionsLabel = new Label(_spellShell, SWT.NONE);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = false;
        _spellSuggestionsLabel.setLayoutData(gd);
        _spellSuggestions = new Combo(_spellShell, SWT.DROP_DOWN);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        _spellSuggestions.setLayoutData(gd);
        
        Composite actionLine = new Composite(_spellShell, SWT.NONE);
        actionLine.setLayout(new FillLayout(SWT.HORIZONTAL));
        _spellReplace = new Button(actionLine, SWT.PUSH);
        _spellReplaceAll = new Button(actionLine, SWT.PUSH);
        _spellIgnore = new Button(actionLine, SWT.PUSH);
        _spellIgnoreAll = new Button(actionLine, SWT.PUSH);
        _spellAdd = new Button(actionLine, SWT.PUSH);
        _spellCancel = new Button(actionLine, SWT.PUSH);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalSpan = 2;
        actionLine.setLayoutData(gd);
        
        _spellIgnore.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _editor.spellIgnore(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _editor.spellIgnore(); }
        });
        _spellIgnoreAll.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _spellIgnoreAllList.add(_spellWord.getText().trim().toLowerCase()); _editor.spellIgnore(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _spellIgnoreAllList.add(_spellWord.getText().trim().toLowerCase()); _editor.spellIgnore(); }
        });
        _spellReplace.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _editor.spellReplaceWord(false); _editor.spellNext(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _editor.spellReplaceWord(false); _editor.spellNext(); }
        });
        _spellReplaceAll.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _editor.spellReplaceWord(true); _editor.spellNext(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _editor.spellReplaceWord(true); _editor.spellNext(); }
        });

        _spellCancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { cancelSpell(); }
            public void widgetSelected(SelectionEvent selectionEvent) { cancelSpell(); }
        });
        
        _spellShell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) {
                evt.doit = false;
                cancelSpell();
            }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        _editor.getDataControl().getTranslationRegistry().register(this);
        _editor.getDataControl().getThemeRegistry().register(this);
    }
    
    void resetSpellcheck() {
        _spellIgnoreAllList.clear();
        _editor.resetSpellcheck();
    }
    void cancelSpell() {
        resetSpellcheck();
        if (_spellShell.isDisposed()) return;
        _spellShell.setVisible(false); 
    }
    
    public void applyTheme(Theme theme) {
        _spellShell.setFont(theme.SHELL_FONT);
        _spellWordLabel.setFont(theme.DEFAULT_FONT);
        _spellWord.setFont(theme.DEFAULT_FONT);
        _spellSuggestionsLabel.setFont(theme.DEFAULT_FONT);
        _spellSuggestions.setFont(theme.DEFAULT_FONT);
        _spellReplace.setFont(theme.BUTTON_FONT);
        _spellReplaceAll.setFont(theme.BUTTON_FONT);
        _spellIgnore.setFont(theme.BUTTON_FONT);
        _spellIgnoreAll.setFont(theme.BUTTON_FONT);
        _spellAdd.setFont(theme.BUTTON_FONT);
        _spellCancel.setFont(theme.BUTTON_FONT);
        _spellShell.pack();
    }
    
    private static final String T_SPELL_ROOT = "syndie.gui.messageeditorspell.root";
    private static final String T_SPELL_WORD = "syndie.gui.messageeditorspell.word";
    private static final String T_SPELL_SUGGESTION = "syndie.gui.messageeditorspell.suggestion";
    private static final String T_SPELL_REPLACE = "syndie.gui.messageeditorspell.replace";
    private static final String T_SPELL_REPLACE_ALL = "syndie.gui.messageeditorspell.replaceall";
    private static final String T_SPELL_IGNORE = "syndie.gui.messageeditorspell.ignore";
    private static final String T_SPELL_IGNORE_ALL = "syndie.gui.messageeditorspell.ignoreall";
    private static final String T_SPELL_ADD = "syndie.gui.messageeditorspell.add";
    private static final String T_SPELL_CANCEL = "syndie.gui.messageeditorspell.cancel";
 
    private static final String T_SPELL_END = "syndie.gui.messageeditorspell.end";
    private static final String T_SPELL_END_OK = "syndie.gui.messageeditorspell.end.ok";
    
    public void translate(TranslationRegistry registry) {
        _spellShell.setText(registry.getText(T_SPELL_ROOT, "Spell checker"));
        _spellWordLabel.setText(registry.getText(T_SPELL_WORD, "Word: "));
        _spellSuggestionsLabel.setText(registry.getText(T_SPELL_SUGGESTION, "Suggestions: "));
        _spellReplace.setText(registry.getText(T_SPELL_REPLACE, "replace"));
        _spellReplaceAll.setText(registry.getText(T_SPELL_REPLACE_ALL, "replace all"));
        _spellIgnore.setText(registry.getText(T_SPELL_IGNORE, "ignore"));
        _spellIgnoreAll.setText(registry.getText(T_SPELL_IGNORE_ALL, "ignore all"));
        _spellAdd.setText(registry.getText(T_SPELL_ADD, "add"));
        _spellCancel.setText(registry.getText(T_SPELL_CANCEL, "cancel"));
    }
}
