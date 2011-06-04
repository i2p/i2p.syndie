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
import net.i2p.data.SessionKey;
import net.i2p.data.SigningPrivateKey;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
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
import org.eclipse.swt.widgets.Monitor;
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
import syndie.db.JobRunner;
import syndie.db.MessageCreator;
import syndie.db.MessageCreatorDirect;
import syndie.db.MessageCreatorSource;
import syndie.db.ThreadAccumulatorJWZ;
import syndie.db.ThreadBuilder;
import syndie.db.ThreadMsgId;
import syndie.db.UI;

/**
 *
 */
public class MessageEditor extends BaseComponent implements Themeable, Translatable, ImageBuilderPopup.ImageBuilderSource {
    private DataCallback _dataCallback;
    private NavigationControl _navControl;
    private BanControl _banControl;
    private BookmarkControl _bookmarkControl;
    private URIControl _uriControl;
    private Composite _parent;
    private Composite _root;
    private Composite _toolbar;
    private Composite _headers;
    private Button _hideHeaderButton;
    private Label _authorLabel;
    ////private Combo _authorCombo;
    private Label _authorCurrentLabel;
    private Button _authorChangeButton;
    // sometimes _signAs is not _authorCombo
    private Label _signAsLabel;
    //private Combo _signAs;
    private Composite _signAsGroup;
    private Label _signAsCurrentLabel;
    private Button _signAsChangeButton;
    //private List _signAsHashes;
    private Hash _signAsChannel;
    private Button _authorHidden;
    private Label _toLabel;
    //private Combo _to;
    private Label _toCurrentLabel;
    private Button _toChangeButton;
    private Label _subjectLabel;
    private Text _subject;
    private Label _tagLabel;
    private Text _tag;
    private Label _privacyLabel;
    private Combo _privacy;

    private Composite _abbrHeaders;
    private Button _showHeaderButton;
    private Label _abbrSubjectLabel;
    private Text _abbrSubject;
    
    private CTabFolder _pageTabs;
    private Button _preview;
    private Button _post;
    private Button _postpone;
    private Button _cancel;
    
    private CTabItem _refEditorTab;
    private Composite _refEditorTabRoot;
    private MessageReferencesEditor _refEditor;
    private CTabItem _threadTab;
    private Composite _threadTabRoot;
    private MessageTree _threadTree;
    
    // state info
    private List _pageEditors;
    private List _pageTypes;
    
    private List _attachmentRoots;
    private List _attachmentPreviews;
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
    
    private List _editorStatusListeners;
    
    private boolean _buildToolbar;
    private boolean _allowPreview;
    private boolean _showActions;
    
    private MessageEditorToolbar _bar;

    private Menu _titleMenu;
    private List _pageTitles;
    
    private MessageEditorFind _finder;
    private MessageEditorSpell _spellchecker;
    //private MessageEditorStyler _styler;
    private ImageBuilderPopup _imagePopup;
    private ReferenceChooserPopup _refChooser;
    private LinkBuilderPopup _linkPopup;
    private LinkBuilderPopup _refAddPopup;
    
    /** Creates a new instance of MessageEditorNew */
    public MessageEditor(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, DataCallback callback, NavigationControl navControl, BookmarkControl bookmarkControl, BanControl banControl, URIControl uriControl, Composite parent, LocalMessageCallback lsnr, boolean buildToolbar, boolean allowPreview, boolean showActions) {
        super(client, ui, themes, trans);
        _dataCallback = callback;
        _navControl = navControl;
        _banControl = banControl;
        _bookmarkControl = bookmarkControl;
        _uriControl = uriControl;
        _parent = parent;
        _pageEditors = new ArrayList(1);
        _pageTypes = new ArrayList();
        _pageTitles = new ArrayList();
        _attachmentRoots = new ArrayList();
        _attachmentPreviews = new ArrayList();
        _attachmentConfig = new ArrayList();
        _attachmentData = new ArrayList();
        _attachmentSummary = new ArrayList();
        _parents = new ArrayList();
        //_signAsHashes = new ArrayList();
        _editorStatusListeners = new ArrayList();
        _modifiedSinceOpen = false;
        _modifiedSinceSave = false;
        _buildToolbar = buildToolbar;
        _allowPreview = allowPreview;
        _showActions = showActions;
        _enableSave = false;
        _postponeId = -1;
        _postponeVersion = -1;
        Properties prefs = _client.getNymPrefs();
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
    
    public void addStatusListener(EditorStatusListener lsnr) { _editorStatusListeners.add(lsnr); }
    public void removeStatusListener(EditorStatusListener lsnr) { _editorStatusListeners.remove(lsnr); }
    
    public interface EditorStatusListener {
        public void pickPrivacyPublic();
        public void pickPrivacyPBE();
        public void pickPrivacyPrivate();
        public void pickPrivacyAuthorized();
        public void pickPageTypeHTML(boolean isHTML);
        public void statusUpdated(int page, int pages, int attachment, int attachments, String type, boolean pageLoaded, boolean isHTML, boolean hasAncestors);
        public void forumSelected(Hash forum, long channelId, String summary, boolean isManaged);
        public void authorSelected(Hash author, long channelId, String summary);
    }
    
    public void dispose() {
        if (_refAddPopup != null) _refAddPopup.dispose();
        if (_linkPopup != null) _linkPopup.dispose();
        if (_refChooser != null) _refChooser.dispose();
        if (_imagePopup != null) {
            Properties prefs = _client.getNymPrefs();
            prefs.setProperty("editor.defaultImagePath", _imagePopup.getFilterPath());
            _client.setNymPrefs(prefs);
            _imagePopup.dispose();
        }
        if (_refEditor != null) _refEditor.dispose();
        //if (_styler != null) _styler.dispose();
        if (_spellchecker != null) _spellchecker.dispose();
        if (_finder != null) _finder.dispose();
        while (_pageEditors.size() > 0)
            ((PageEditor)_pageEditors.remove(0)).dispose();
        while (_attachmentPreviews.size() > 0)
            ((AttachmentPreview)_attachmentPreviews.remove(0)).dispose();
        while (_attachmentRoots.size() > 0)
            ((Composite)_attachmentRoots.remove(0)).dispose();
        _attachmentConfig.clear();
        _attachmentData.clear();
        _attachmentSummary.clear();
            
        
        if (_threadTree != null)
            _threadTree.dispose();
        if (_bar != null) _bar.dispose();
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
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
        _ui.debugMessage("saving state for postponeId " + _postponeId + "/" + stateId);
        String state = serializeStateToB64(stateId); // increments the version too
        _ui.debugMessage("serialized state for postponeId " + stateId + " / " + _postponeVersion);
        if (state == null) {
            _ui.errorMessage("Internal error serializing message state");
            return;
        }
        int version = _postponeVersion;
        Connection con = _client.con();
        PreparedStatement stmt = null;
        try {
            stmt = con.prepareStatement(SQL_POSTPONE);
            stmt.setLong(1, _client.getLoggedInNymId());
            stmt.setLong(2, stateId);
            stmt.setInt(3, version);
            stmt.setString(4, state);
            stmt.executeUpdate();
            stmt.close();
            stmt = null;

            
            // if that didn't fail, delete all of the older versions
            stmt = con.prepareStatement(SQL_POSTPONE_CLEANUP);
            stmt.setLong(1, _client.getLoggedInNymId());
            stmt.setLong(2, stateId);
            stmt.setInt(3, version);
            stmt.executeUpdate();
            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            _ui.errorMessage("Internal error postponing", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        _ui.debugMessage("done saving state.  " + _postponeId + "/" + _postponeVersion);
        _modifiedSinceSave = false;
    }
    
    private static final String SQL_RESUME = "SELECT encryptedData FROM nymMsgPostpone WHERE nymId = ? AND postponeId = ? AND postponeVersion = ?";
    public boolean loadState(long postponeId, int version) {
        String state = readState(_client, _ui, postponeId, version);
        
        if (state != null) {
            deserializeStateFromB64(state, postponeId, version);
            _modifiedSinceSave = false;
            _modifiedSinceOpen = false;
            return true;
        } else {
            return false;
        }
    }
    
    private static String readState(DBClient client, UI ui, long postponeId, int version) {
        Connection con = client.con();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        String state = null;
        try {
            stmt = con.prepareStatement(SQL_RESUME);
            stmt.setLong(1, client.getLoggedInNymId());
            stmt.setLong(2, postponeId);
            stmt.setInt(3, version);
            rs = stmt.executeQuery();
            if (rs.next()) {
                state = rs.getString(1);
            } else {
                return null;
            }
            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            ui.errorMessage("Internal error resuming", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return state;
    }
    
    public static class MessageSummary {
        public String subject;
        public Hash author;
        public Hash forum;
        public long postponeId;
        public int version;
    }
    public static MessageSummary loadSummary(DBClient client, UI ui, TranslationRegistry trans, long postponeId, int version) {
        MessageSummary summary = new MessageSummary();
        summary.postponeId = postponeId;
        summary.version = version;
        
        String state = readState(client, ui, postponeId, version);
        if (state == null) return null;
        
        byte decr[] = decryptState(client, state);
        if (decr == null) return null;
        
        ZipInputStream zin = null;
        try {
            zin = new ZipInputStream(new ByteArrayInputStream(decr));
        
            ZipEntry entry = null;
            while ( (entry = zin.getNextEntry()) != null) {
                String name = entry.getName();
                if (name.equals(SER_ENTRY_CONFIG)) {
                    Properties cfg = readCfg(read(zin));

                    summary.author = getHash(ui, cfg, SER_AUTHOR);
                    summary.forum = getHash(ui, cfg, SER_TARGET);
        
                    int parents = 0;
                    if ( (cfg.getProperty(SER_PARENTS) != null) && (cfg.getProperty(SER_PARENTS).length() > 0) )
                        try { parents = Integer.parseInt(cfg.getProperty(SER_PARENTS)); } catch (NumberFormatException nfe) {}

                    List parentURIs = null;
                    if (parents <= 0)
                        parentURIs = new ArrayList();
                    else
                        parentURIs = new ArrayList(parents);
                    
                    for (int i = 0; i < parents; i++) {
                        String uriStr = cfg.getProperty(SER_PARENTS_PREFIX + i);
                        try {
                            SyndieURI uri = new SyndieURI(uriStr);
                            parentURIs.add(uri);
                        } catch (URISyntaxException use) {
                            //
                        }
                    }
                    
                    if (cfg.containsKey(SER_SUBJECT)) {
                        summary.subject = cfg.getProperty(SER_SUBJECT);
                    } else if ( (parentURIs != null) && (parentURIs.size() > 0) ) {
                        SyndieURI parent = (SyndieURI)parentURIs.get(0);
                        String parentSubject = MessageView.calculateSubject(client, ui, trans, parent).trim();
                        if ( (parentSubject.length() > 0) && (!Constants.lowercase(parentSubject).startsWith("re:")) ) {
                            summary.subject = "re: " + parentSubject;
                        } else {
                            summary.subject = parentSubject;
                        }
                    } else {
                        summary.subject = "";
                    }
                    
                    break;
                } // end if (isCFG)
            } // end while
        } catch (IOException ioe) {
            ui.errorMessage("Internal error deserializing message state", ioe);
            return null;
        } finally {
            if (zin != null) try { zin.close(); } catch (IOException ioe) {}
        }
        
        return summary;
    }
    
    private static final String SQL_DROP = "DELETE FROM nymMsgPostpone WHERE nymId = ? AND postponeId = ?";
    void dropSavedState() { dropSavedState(_client, _ui, _postponeId); }
    public static void dropSavedState(DBClient client, UI ui, long postponeId) {
        ui.debugMessage("dropping saved state for postponeId " + postponeId);
        Connection con = client.con();
        PreparedStatement stmt = null;
        try {
            stmt = con.prepareStatement(SQL_DROP);
            stmt.setLong(1, client.getLoggedInNymId());
            stmt.setLong(2, postponeId);
            stmt.executeUpdate();
            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            ui.errorMessage("Internal error dropping saved state", se);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private static final String T_POSTED_MESSAGE = "syndie.gui.messageeditor.post.message";
    private static final String T_POSTED_TITLE = "syndie.gui.messageeditor.post.title";
    private static final String T_POSTED_CHECKBOX = "syndie.gui.messageeditor.post.checkbox";
    private static final String T_POSTED_OK = "syndie.gui.messageeditor.post.ok";
    private static final String T_POST_ERROR_MESSAGE_PREFIX = "syndie.gui.messageeditor.post.errormsg";
    private static final String T_POST_ERROR_TITLE = "syndie.gui.messageeditor.post.errortitle";
    private static final String T_CONFIRM_EMPTY_TITLE = "syndie.gui.messageeditor.post.confirmempty.title";
    private static final String T_CONFIRM_EMPTY_MESSAGE = "syndie.gui.messageeditor.post.confirmempty.message";
    
    public void postMessage() {
        if (!isModifiedSinceOpen()) {
            MessageBox confirm = new MessageBox(_root.getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO);
            confirm.setText(_translationRegistry.getText(T_CONFIRM_EMPTY_TITLE, "Post empty message?"));
            confirm.setMessage(_translationRegistry.getText(T_CONFIRM_EMPTY_MESSAGE, "Do you really want to post this empty message?"));
            
            if (confirm.open() == SWT.NO)
                return;
        }
        if (!validateAuthorForum()) {
            showUnauthorizedWarning();
            return;
        }
        new MessageCreatorDirect(new CreatorSource()).execute();
    }
    
    private class CreatorSource implements MessageCreatorSource {
        public MessageCreator.ExecutionListener getListener() {
            return new MessageCreator.ExecutionListener() {
                public void creationComplete(MessageCreator exec, SyndieURI uri, String errors, boolean successful, SessionKey replySessionKey, byte[] replyIV, File msg) {
                    if (successful) {
                        boolean ok = exec.importCreated(_client, _ui, uri, msg, replyIV, replySessionKey, getPassphrase());
                        if (ok) {
                            dropSavedState();
                            _dataCallback.messageImported();
                            messageCreatedBox();
                            for (Iterator iter = _listeners.iterator(); iter.hasNext(); ) 
                                ((LocalMessageCallback)iter.next()).messageCreated(uri);
                        } else {
                            MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_ERROR | SWT.OK);
                            box.setMessage(_translationRegistry.getText(T_POST_ERROR_MESSAGE_PREFIX, "There was an error creating the message.  Please view the log for more information: ") + errors);
                            box.setText(_translationRegistry.getText(T_POST_ERROR_TITLE, "Error creating the message"));
                            box.open();
                        }
                    } else {
                        MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_ERROR | SWT.OK);
                        box.setMessage(_translationRegistry.getText(T_POST_ERROR_MESSAGE_PREFIX, "There was an error creating the message.  Please view the log for more information: ") + errors);
                        box.setText(_translationRegistry.getText(T_POST_ERROR_TITLE, "Error creating the message"));
                        box.open();
                    }
                    exec.cleanup();
                }
                
                void messageCreatedBox() {
                    Properties prefs = _client.getNymPrefs();
                    if (Boolean.valueOf(prefs.getProperty("editor.showMessageCreatedBox")).booleanValue()) {
                        final Shell shell = new Shell(_root.getShell(), SWT.APPLICATION_MODAL | SWT.DIALOG_TRIM);
                        shell.setFont(_themeRegistry.getTheme().SHELL_FONT);
                        shell.setText(_translationRegistry.getText(T_POSTED_TITLE, "Message created!"));
                        
                        GridLayout gl = new GridLayout(2, false);
                        shell.setLayout(gl);
                        
                        Label message = new Label(shell, SWT.WRAP);
                        GridData messageLayoutData = new GridData(GridData.FILL, GridData.BEGINNING, false, false);
                        messageLayoutData.horizontalSpan = 2;
                        messageLayoutData.heightHint = 75;
                        message.setLayoutData(messageLayoutData);
                        message.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
                        message.setText(_translationRegistry.getText(T_POSTED_MESSAGE, "Message created and imported successfully!  Please be sure to syndicate it to others so they can read it"));
                        
                        final Button checkbox = new Button(shell, SWT.CHECK);
                        checkbox.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
                        checkbox.setText(_translationRegistry.getText(T_POSTED_CHECKBOX, "Display this message next time"));
                        checkbox.setSelection(true);
                        
                        Button ok = new Button(shell, SWT.PUSH);
                        ok.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
                        ok.setFont(_themeRegistry.getTheme().BUTTON_FONT);
                        ok.setText(_translationRegistry.getText(T_POSTED_OK, "OK"));
                        ok.addSelectionListener(new FireSelectionListener() {
                            public void fire() {
                                if (!checkbox.getSelection()) {
                                    Properties prefs = _client.getNymPrefs();
                                    prefs.setProperty("editor.showMessageCreatedBox", Boolean.FALSE.toString());
                                    _client.setNymPrefs(prefs);
                                }
                                shell.close();
                            }
                        });
                        
                        shell.addShellListener(new ShellListener() {
                            public void shellActivated(ShellEvent e) {}
                            public void shellClosed(ShellEvent e) { shell.dispose(); }
                            public void shellDeactivated(ShellEvent e) {}
                            public void shellDeiconified(ShellEvent e) {}
                            public void shellIconified(ShellEvent e) {}
                        });
                        
                        shell.pack();
                        
                        Monitor monitor = _root.getDisplay().getPrimaryMonitor();
                        Rectangle bounds = monitor.getBounds();
                        Rectangle rect = shell.getBounds();
                        int x = bounds.x + (bounds.width - rect.width) / 2;
                        int y = bounds.y + (bounds.height - rect.height) / 2;
                        shell.setLocation(x, y);
                        
                        shell.open();
                    }
                }
            };
        }
        
        public DBClient getClient() { return _client; }
        public UI getUI() { return _ui; }
        public Hash getAuthor() { return _author; }
        public Hash getSignAs() { return _signAsChannel; }
        public boolean getAuthorHidden() {
            return (_signAsChannel != null) && (_authorHidden.getSelection()); // _signAsHashes.size() > 0) && (_authorHidden.getSelection());
        }
        public Hash getTarget() { return _forum; }
        public int getPageCount() { return _pageEditors.size(); }
        public String getPageContent(int page) { return ((PageEditor)_pageEditors.get(page)).getContent(); }
        /** 0-indexed page type */
        public String getPageType(int page) { return ((PageEditor)_pageEditors.get(page)).getContentType(); }
        public String getPageTitle(int page) { 
            String title = (String)_pageTitles.get(page); 
            if ( (title != null) && (title.trim().length() > 0) )
                return title;
            else
                return null;
        }
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
        public List getCancelURIs() { return new ArrayList(); }
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
            dialog.setMessage(_translationRegistry.getText(T_CANCEL_MESSAGE, "Are you sure you want to cancel this message?"));
            dialog.setText(_translationRegistry.getText(T_CANCEL_TITLE, "Confirm message cancellation"));
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
                _ui.errorMessage("Internal error serializing message state", ioe);
                return null;
            }
            byte salt[] = new byte[16];
            byte encr[] = _client.pbeEncrypt(data, salt);
            String rv = Base64.encode(salt) + Base64.encode(encr);
            _postponeId = postponementId;
            _postponeVersion++;
            _ui.debugMessage("serialized state to " + encr.length + " bytes (" + rv.length() + " base64 encoded...)");
            return rv;
        } catch (OutOfMemoryError oom) {
            _ui.errorMessage("Ran out of memory serializing the state.  page buffers: " + countPageUndoBuffers() + " bytes");
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
    
    private static byte[] decryptState(DBClient client, String state) {
        String salt = state.substring(0, 24);
        String body = state.substring(24);
        return client.pbeDecrypt(Base64.decode(body), Base64.decode(salt));
    }
    
    public void deserializeStateFromB64(String state, long postponeId, int version) {
        byte decr[] = decryptState(_client, state);
        
        if (decr == null) {
            _ui.errorMessage("Error pbe decrypting " + postponeId + "." + version + ": state: " + state);
            dispose();
            return;
        }

        _ui.debugMessage("deserialized state to " + decr.length + " bytes (" + state.length() + " base64 encoded...)");
        state = null;

        ZipInputStream zin = null;
        try {
            zin = new ZipInputStream(new ByteArrayInputStream(decr));
            deserializeState(zin);
        } catch (IOException ioe) {
            _ui.errorMessage("Internal error deserializing message state", ioe);
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
    private static final String SER_ENTRY_PAGE_TITLE_PREFIX = "pageTitle";
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
            String title = getTitle(i);
            
            zos.putNextEntry(new ZipEntry(SER_ENTRY_PAGE_PREFIX + i));
            zos.write(DataHelper.getUTF8(data));
            zos.closeEntry();
            
            zos.putNextEntry(new ZipEntry(SER_ENTRY_PAGE_CFG_PREFIX + i));
            zos.write(DataHelper.getUTF8(type));
            zos.closeEntry();
            
            zos.putNextEntry(new ZipEntry(SER_ENTRY_PAGE_TITLE_PREFIX + i));
            zos.write(DataHelper.getUTF8(title));
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
        Map pageTitles = new TreeMap();
        Map attachments = new TreeMap();
        Map attachmentCfgs = new TreeMap();
        byte avatar[] = null;
        while ( (entry = zin.getNextEntry()) != null) {
            String name = entry.getName();
            _ui.debugMessage("Deserializing state: entry = " + name);
            if (name.equals(SER_ENTRY_CONFIG)) {
                deserializeConfig(readCfg(read(zin)));
            } else if (name.equals(SER_ENTRY_REFS)) {
                _refEditor.setReferenceNodes(ReferenceNode.buildTree(zin));
            } else if (name.startsWith(SER_ENTRY_PAGE_CFG_PREFIX)) {
                pageCfgs.put(name, read(zin));
            } else if (name.startsWith(SER_ENTRY_PAGE_TITLE_PREFIX)) {
                pageTitles.put(name, read(zin));
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
        
        _pageTitles.clear();
        
        for (int i = 0; i < pageCount; i++) {
            String body = (String)pages.get(SER_ENTRY_PAGE_PREFIX + i);
            String type = (String)pageCfgs.get(SER_ENTRY_PAGE_CFG_PREFIX + i);
            String title = (String)pageTitles.get(SER_ENTRY_PAGE_TITLE_PREFIX + i);
            
            _ui.debugMessage("Deserializing state: adding page: " + i + " [" + type + "]");
            boolean isHTML = TYPE_HTML.equals(type);
            PageEditor editor = new PageEditor(_client, _ui, _themeRegistry, _translationRegistry, this, _allowPreview, isHTML, i);
            _pageEditors.add(editor);
            _pageTypes.add(type);
            _pageTitles.add(title);
            editor.setContent(body);
            if ( (title != null) && (title.trim().length() > 0) )
                editor.getItem().setText(title);
            else
                editor.getItem().setText(_translationRegistry.getText(T_PAGE_PREFIX, "Page ") + (i+1));
            //if (isHTML)
            //    _pageType.setImage(ImageUtil.ICON_EDITOR_PAGETYPE_HTML);
            //else
            //    _pageType.setImage(ImageUtil.ICON_EDITOR_PAGETYPE_TEXT);
        }
        _pageTabs.setMenu(_titleMenu);
        
        _attachmentData.clear();
        _attachmentConfig.clear();
        int attachmentCount = attachments.size();
        for (int i = 0; i < attachmentCount; i++) {
            byte data[] = (byte[])attachments.get(SER_ENTRY_ATTACH_PREFIX + i);
            Properties cfg = (Properties)attachmentCfgs.get(SER_ENTRY_ATTACH_CFG_PREFIX + i);
            if ( (cfg == null) || (data == null) ) 
                break;
            _ui.debugMessage("Deserializing state: adding attachment: " + i);
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
    
    private Hash getHash(Properties cfg, String prop) { return getHash(_ui, cfg, prop); }
    private static Hash getHash(UI ui, Properties cfg, String prop) {
        String t = cfg.getProperty(prop);
        if (t == null) return null;
        byte d[] = Base64.decode(t);
        if ( (d == null) || (d.length != Hash.HASH_LENGTH) ) {
            ui.debugMessage("serialized prop (" + prop + ") [" + t + "] could not be decoded");
            return null;
        } else {
            return new Hash(d);
        }
    }
    
    private void deserializeConfig(Properties cfg) {
        _ui.debugMessage("deserializing config: \n" + cfg.toString());
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
            ThreadBuilder builder = new ThreadBuilder(_client, _ui);
            HashSet msgIds = new HashSet();
            for (int i = 0; i < _parents.size(); i++) {
                SyndieURI uri = (SyndieURI)_parents.get(i);
                if ( (uri.getScope() != null) && (uri.getMessageId() != null) ) {
                    long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                    ThreadMsgId id = new ThreadMsgId(msgId); // may be -1
                    id.messageId = uri.getMessageId().longValue();
                    id.scope = uri.getScope();
                    msgIds.add(id);
                }
            }
            List roots = builder.buildThread(msgIds);
            _ui.debugMessage("setting message ancestry tree to: \n" + roots);
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
            _abbrSubject.setText(_subject.getText());
        } else if ( (_parents != null) && (_parents.size() > 0) ) {
            SyndieURI parent = (SyndieURI)_parents.get(0);
            String parentSubject = MessageView.calculateSubject(_client, _ui, _translationRegistry, parent).trim();
            if ( (parentSubject.length() > 0) && (!Constants.lowercase(parentSubject).startsWith("re:")) ) {
                _subject.setText("re: " + parentSubject);
            } else {
                _subject.setText(parentSubject);
            }
            _abbrSubject.setText(_subject.getText());
        } else {
            _subject.setText("");
            _abbrSubject.setText(_subject.getText());
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
    
    
    private static byte[] readBytes(InputStream in) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        byte buf[] = new byte[4096];
        int read = -1;
        while ( (read = in.read(buf)) != -1)
            baos.write(buf, 0, read);
        return baos.toByteArray();
    }
    private static String read(InputStream in) throws IOException {
        return DataHelper.getUTF8(readBytes(in));
    }
    private static Properties readCfg(String str) throws IOException {
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
    public void search() { 
        // don't open unless there's a page to search...
        if (getPageEditor() != null)
            _finder.open();
    }
    
    public void quote() {
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
    public void spellNext() {
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
    
    public void styleText() {
        if ((getPageType() != null) && (TYPE_HTML.equals(getPageType()))) {
            new MessageEditorStyler(_client, _ui, _themeRegistry, _translationRegistry, this).open();
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
            _ui.debugMessage("messageEditor.toggleMaxEditor()");
        }
    }
    
    // gui stuff..
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.verticalSpacing = 0;
        gl.horizontalSpacing = 0;
        _root.setLayout(gl);
        
        initHeader();
        initAbbrHeader();
        hideHeaders(); // default to the abbreviated headers
        initToolbar();
        initPage();
        initFooter();
        initPrivacyCombo();
        
        pickPrivacy(1);
        
        _finder = new MessageEditorFind(_themeRegistry, _translationRegistry, this);
        _spellchecker = new MessageEditorSpell(_themeRegistry, _translationRegistry, this);
        //_styler = new MessageEditorStyler(this);
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
                
        addPage();
        
        _nymChannels = _client.getNymChannels(); //_client.getChannels(true, true, true, true);
        
        updateForum();
        updateAuthor();

        _titleMenu = new Menu(_pageTabs);
        MenuItem item = new MenuItem(_titleMenu, SWT.PUSH);
        item.setText("Set page title");
        item.addSelectionListener(new FireSelectionListener() { public void fire() { setTitle(); } });

        _pageTabs.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { switchPage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { switchPage(); }
            private void switchPage() {
                int idx = _pageTabs.getSelectionIndex();
                if ( (idx >= 0) && (idx < _pageEditors.size()) ) {
                    PageEditor ed = getPageEditor();
                    String type = getPageType(idx);
                    _ui.debugMessage("switching to page " + idx + " [" + type + "]");
                    ed.setContentType(type);
                    _pageTabs.setMenu(_titleMenu);
                } else {
                    _pageTabs.setMenu(null);
                }
                updateToolbar();
            }
        });
    }
    
    private static final String T_TITLE = "syndie.gui.messageeditor.title";
    private static final String T_TITLE_LABEL = "syndie.gui.messageeditor.title.label";
    private static final String T_TITLE_DONE = "syndie.gui.messageeditor.title.done";
    
    private void setTitle() {
        final int page = _pageTabs.getSelectionIndex();
        if ( (page >= 0) && (page < _pageTitles.size()) ) {
            String title = (String)_pageTitles.get(page);
            
            final Shell shell = new Shell(_root.getShell(), SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
            GridLayout gl = new GridLayout(3, false);
            gl.marginHeight = 0;
            gl.marginWidth = 0;
            gl.horizontalSpacing = 0;
            gl.verticalSpacing = 0;
            shell.setLayout(gl);
            shell.addShellListener(new ShellListener() {
                public void shellActivated(ShellEvent shellEvent) {}
                public void shellClosed(ShellEvent shellEvent) { shell.dispose(); }
                public void shellDeactivated(ShellEvent shellEvent) {}
                public void shellDeiconified(ShellEvent shellEvent) {}
                public void shellIconified(ShellEvent shellEvent) {}
            });
            shell.setText(_translationRegistry.getText(T_TITLE, "Page title"));
            
            Label l = new Label(shell, SWT.NONE);
            l.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
            l.setText(_translationRegistry.getText(T_TITLE_LABEL, "Page title:"));
            l.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
            
            final Text titleField = new Text(shell, SWT.SINGLE | SWT.BORDER);
            titleField.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            titleField.setText(title != null ? title : "");
            titleField.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
            titleField.setTextLimit(40);
            titleField.addTraverseListener(new TraverseListener() {
                public void keyTraversed(TraverseEvent evt) {
                    if (evt.detail == SWT.TRAVERSE_RETURN) {
                        _pageTitles.set(page, titleField.getText());
                        CTabItem item = _pageTabs.getItem(page);
                        if (item != null)
                            item.setText(titleField.getText());
                        shell.dispose();
                    }
                }
            });
            
            Button done = new Button(shell, SWT.PUSH);
            done.setText(_translationRegistry.getText(T_TITLE_DONE, "OK"));
            done.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
            done.setFont(_themeRegistry.getTheme().BUTTON_FONT);
            done.addSelectionListener(new FireSelectionListener() { 
                public void fire() {
                    _pageTitles.set(page, titleField.getText());
                    CTabItem item = _pageTabs.getItem(page);
                    if (item != null)
                        item.setText(titleField.getText());
                    shell.dispose();
                }
            });
            
            shell.pack();
            //shell.setSize(300, shell.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
            shell.open();
        }
    }
    private String getTitle(int page) {
        if ( (page >= 0) && (page < _pageTitles.size()) )
            return (String)_pageTitles.get(page);
        else
            return null;
    }
    
    private void initPrivacyCombo() {
        _privacy.setRedraw(false);
        int idx = -1;
        if (_privacy.getItemCount() > 0)
            idx = _privacy.getSelectionIndex();
        else
            idx = 1;
        _privacy.removeAll();
        _privacy.add(_translationRegistry.getText(T_PRIV_PUBLIC, "Anyone can read the post"));
        _privacy.add(_translationRegistry.getText(T_PRIV_AUTHORIZED, "Authorized readers of the forum can read the post"));
        _privacy.add(_translationRegistry.getText(T_PRIV_PBE, "Passphrase required to read the post..."));
        _privacy.add(_translationRegistry.getText(T_PRIV_REPLY, "Only forum administrators can read the post"));
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
    
    public void pickPrivacy(int privacyIndex) { pickPrivacy(privacyIndex, true); }
    public void pickPrivacy(int privacyIndex, boolean promptForPassphrase) {
        modified();
        switch (privacyIndex) {
            case 0: // public 
                _privacy.select(privacyIndex);
                for (int i = 0; i < _editorStatusListeners.size(); i++)
                    ((EditorStatusListener)_editorStatusListeners.get(i)).pickPrivacyPublic();
                break;
            case 2: //pbe
                _privacy.select(privacyIndex);
                for (int i = 0; i < _editorStatusListeners.size(); i++)
                    ((EditorStatusListener)_editorStatusListeners.get(i)).pickPrivacyPBE();
                if (promptForPassphrase) { // false when deserializing state
                    final PassphrasePrompt dialog = new PassphrasePrompt(_client, _ui, _themeRegistry, _translationRegistry, _root.getShell(), true);
                    dialog.setPassphrase(_passphrase);
                    dialog.setPassphrasePrompt(_passphrasePrompt);
                    dialog.setPassphraseListener(new PassphrasePrompt.PassphraseListener() { 
                        public void promptComplete(String passphraseEntered, String promptEntered) {
                            _ui.debugMessage("passphrase set [" + passphraseEntered + "] / [" + promptEntered + "]");
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
                for (int i = 0; i < _editorStatusListeners.size(); i++)
                    ((EditorStatusListener)_editorStatusListeners.get(i)).pickPrivacyPrivate();
                break;
            case 1: // authorized only
            default:
                _privacy.select(privacyIndex);
                for (int i = 0; i < _editorStatusListeners.size(); i++)
                    ((EditorStatusListener)_editorStatusListeners.get(i)).pickPrivacyAuthorized();
                break;
        }
    }
    
    private static final String T_PREVIEW = "syndie.gui.messageeditor.preview";
    private static final String T_EDIT = "syndie.gui.messageeditor.edit";
    private static final String T_POST = "syndie.gui.messageeditor.post";
    private static final String T_POSTPONE = "syndie.gui.messageeditor.postpone";
    private static final String T_CANCEL = "syndie.gui.messageeditor.cancel";
    
    private void initFooter() {
        if (!_showActions) return;
        Composite c = new Composite(_root, SWT.NONE);
        c.setLayout(new FillLayout(SWT.HORIZONTAL));
        c.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _preview = new Button(c, SWT.PUSH);
        _preview.setEnabled(TYPE_HTML.equals(getDefaultPageType()));
        _preview.addSelectionListener(new FireSelectionListener() {
            public void fire(SelectionEvent selectionEvent) {
                getPageEditor().toggleFullPreview();
                updateToolbar();
            }
        });
        _post = new Button(c, SWT.PUSH);
        _post.addSelectionListener(new FireSelectionListener() {
            public void fire(SelectionEvent selectionEvent) { postMessage(); }
        });
        _postpone = new Button(c, SWT.PUSH);
        _postpone.addSelectionListener(new FireSelectionListener() {
            public void fire(SelectionEvent selectionEvent) { postponeMessage(); }
        });
        _cancel = new Button(c, SWT.PUSH);
        _cancel.addSelectionListener(new FireSelectionListener() {
            public void fire(SelectionEvent selectionEvent) { cancelMessage(); }
        });
        
        _post.setText(_translationRegistry.getText(T_POST, "Post the message"));
        _postpone.setText(_translationRegistry.getText(T_POSTPONE, "Save the message for later"));
        _cancel.setText(_translationRegistry.getText(T_CANCEL, "Cancel the message"));
    }
    
    public static final String TYPE_HTML = "text/html";
    public static final String TYPE_TEXT = "text/plain";
    private void setDefaultPageType(String type) {
        Properties prefs = _client.getNymPrefs();
        prefs.setProperty("editor.defaultFormat", type);
        _client.setNymPrefs(prefs);
    }
    
    private String getDefaultPageType() {
        Properties prefs = _client.getNymPrefs();
        String type = prefs.getProperty("editor.defaultFormat");
        
        if (TYPE_HTML.equals(type))
            return TYPE_HTML;
        else
            return TYPE_TEXT;
    }
    
    public PageEditor addPage() {
        return addPage(getDefaultPageType());
    }
    private PageEditor addPage(String type) {
        saveState();
        modified();
        PageEditor ed = new PageEditor(_client, _ui, _themeRegistry, _translationRegistry, this, _allowPreview, TYPE_HTML.equals(type), _pageEditors.size());
        _pageEditors.add(ed);
        _pageTypes.add(type);
        _pageTitles.add("");
        int pageNum = _pageEditors.size();
        ed.getItem().setText(_translationRegistry.getText(T_PAGE_PREFIX, "Page ") + pageNum);
        
        viewPage(_pageEditors.size()-1);
        for (int i = 0; i < _editorStatusListeners.size(); i++)
            ((EditorStatusListener)_editorStatusListeners.get(i)).pickPageTypeHTML(type.equals(TYPE_HTML));
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
            _ui.debugMessage("saving stte, pages: " + _pageEditors.size());
            saveState();
            modified();
            _ui.debugMessage("remove page " + pageNum + "/" + _pageEditors.size());
            PageEditor editor = (PageEditor)_pageEditors.remove(pageNum);
            _pageTypes.remove(pageNum);
            _pageTitles.remove(pageNum);
            editor.dispose();
            
            for (int i = 0; i < _pageEditors.size(); i++) {
                PageEditor cur = (PageEditor)_pageEditors.get(i);
                cur.getItem().setText(_translationRegistry.getText(T_PAGE_PREFIX, "Page ") + (i+1));
            }
            viewPage(_pageEditors.size()-1);
            saveState();
        } else {
            _ui.debugMessage("remove page " + pageNum + " is out of range");
        }
    }
    public void togglePageType() {
        int page = getCurrentPage();
        if (page >= 0) {
            PageEditor ed = getPageEditor(page);
            if (ed.isPreviewShowing()) ed.toggleFullPreview();
            String type = getPageType(page);
            if (TYPE_HTML.equals(type))
                type = TYPE_TEXT;
            else
                type = TYPE_HTML;
            ed.setContentType(type);
            _pageTypes.set(page, type);
            for (int i = 0; i < _editorStatusListeners.size(); i++)
                ((EditorStatusListener)_editorStatusListeners.get(i)).pickPageTypeHTML(type.equals(TYPE_HTML));
            setDefaultPageType(type);
            updateToolbar();
            modified();
        }
    }
    
    private static final String T_WEBRIP_TITLE = "syndie.gui.messageeditor.webrip";
    private static final String T_WEBRIP_FAIL = "syndie.gui.messageeditor.webrip.fail";
    
    public void addWebRip() {
        Shell shell = new Shell(_root.getShell(), SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        shell.setLayout(new FillLayout());
        final WebRipPageControl ctl = new WebRipPageControl(_client, _ui, _themeRegistry, _translationRegistry, shell);
        ctl.setListener(new WebRipListener(shell, ctl));
        ctl.setExistingAttachments(_attachmentData.size());
        shell.pack();
        shell.setText(_translationRegistry.getText(T_WEBRIP_TITLE, "Add web rip"));
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
            _ui.debugMessage("rip complete: ok?" + successful);
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
            box.setText(_translationRegistry.getText(T_WEBRIP_FAIL, "Rip failed"));
            StringBuilder err = new StringBuilder();
            for (int i = 0; i < msgs.size(); i++)
                err.append((String)msgs.get(i)).append('\n');
            box.setMessage(err.toString());
            box.open();
        } else {
            _ui.debugMessage("rip failed, but no messages, so it must have been cancelled");
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
    
    /** 0-indexed attachment being shown, or -1 if not an attachment */
    private int getCurrentAttachment() {
        int idx = _pageTabs.getSelectionIndex();
        int pages = _pageEditors.size();
        int attachments = _attachmentData.size();
        if ( (idx >= 0) && (idx >= pages) && (idx < pages+attachments) )
            return idx-pages;
        else
            return -1;
    }
    
    /** current page */
    public PageEditor getPageEditor() { return getPageEditor(getCurrentPage()); }
    /** grab the given (0-indexed) page */
    private PageEditor getPageEditor(int pageNum) {
        if ( (pageNum >= 0) && (pageNum < _pageEditors.size()) )
            return (PageEditor)_pageEditors.get(pageNum);
        else
            return null;
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
        _pageTabs.setMenu(_titleMenu);
        updateToolbar();
    }
    
    /**
     * go through the toolbar to adjust the available options for the current page
     */
    private void updateToolbar() {
        int page = getCurrentPage();
        int pages = _pageEditors.size();
        int attachment = getCurrentAttachment();
        int attachments = _attachmentData.size();
        String type = (page >= 0) ? getPageType(page) : null;
        boolean pageLoaded = (page >= 0) && (pages > 0);
        boolean isHTML = pageLoaded && TYPE_HTML.equals(type);
        boolean hasAncestors = pageLoaded && _parents.size() > 0;
        
        _ui.debugMessage("updateToolbar: pages=" + pages + " (" + page + "/" + (pages-1) + ") attachments=" + attachments + " isHTML? " + isHTML + "/" + type + " pageLoaded? " + pageLoaded + " types: " + _pageTypes);
       
        for (int i = 0; i < _editorStatusListeners.size(); i++)
            ((EditorStatusListener)_editorStatusListeners.get(i)).statusUpdated(page, pages, attachment, attachments, type, pageLoaded, isHTML, hasAncestors);
        
        _preview.setEnabled(isHTML);
        if (isHTML && getPageEditor(page).isPreviewShowing())
            _preview.setText(_translationRegistry.getText(T_EDIT, "Edit the message"));
        else
            _preview.setText(_translationRegistry.getText(T_PREVIEW, "Preview the message"));
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
    public void showImagePopup(boolean forBodyBackground) {
        if (_imagePopup == null) {
            Properties prefs = _client.getNymPrefs();
            _imagePopup = new ImageBuilderPopup(_root.getShell(), this);
            _imagePopup.setFilterPath(prefs.getProperty("editor.defaultImagePath"));
        }
        _imagePopup.showPopup(forBodyBackground); 
    }
    public void showLinkPopup() {
        buildLinkPopup();
        _linkPopup.showPopup();
    }
    public void showLinkPopup(boolean web, boolean page, boolean attach, boolean forum, boolean message, boolean submessage, boolean eepsite, boolean i2p, boolean freenet, boolean archive) { 
        buildLinkPopup();
        _linkPopup.limitOptions(web, page, attach, forum, message, submessage, eepsite, i2p, freenet, archive, true);
        _linkPopup.showPopup();
    }
    private void buildLinkPopup() {
        if (_linkPopup == null)
            _linkPopup = new LinkBuilderPopup(_client, _ui, _themeRegistry, _translationRegistry, _navControl, _banControl, _bookmarkControl, _parent.getShell(), new LinkBuilderPopup.LinkBuilderSource () {
                public void uriBuildingCancelled() {}
                public void uriBuilt(SyndieURI uri, String text) {
                    insertAtCaret("<a href=\"" + uri.toString() + "\">" + text + "</a>");
                }
                public int getPageCount() { return _pageEditors.size(); }
                public List getAttachmentDescriptions() { return MessageEditor.this.getAttachmentDescriptions(); }
            });
    }
    
    public Hash getForum() { return _forum; }
    public Hash getAuthor() { return _author; }
    public int getParentCount() { return _parents.size(); }
    public SyndieURI getParent(int depth) { return (SyndieURI)_parents.get(depth); }
    public boolean getPrivacyReply() { return _privacy.getSelectionIndex() == PRIVACY_REPLY; }
    public void setParentMessage(SyndieURI uri) {
        _parents.clear();
        if ( (uri != null) && (uri.getScope() != null) && (uri.getMessageId() != null) ) {
            _parents.add(uri);
            
            long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
            if (msgId >= 0) {
                ThreadMsgId tmi = new ThreadMsgId(msgId);
                tmi.messageId = uri.getMessageId().longValue();
                tmi.scope = uri.getScope();
                Map tmiToList = new HashMap();
                ThreadAccumulatorJWZ.buildAncestors(_client, _ui, tmi, tmiToList);
                List ancestors = (List)tmiToList.get(tmi);
                if ( (ancestors != null) && (ancestors.size() > 0) ) {
                    _ui.debugMessage("parentMessage is " + uri + ", but its ancestors are " + ancestors);
                    for (int i = 0; i < ancestors.size(); i++) {
                        ThreadMsgId ancestor = (ThreadMsgId)ancestors.get(i);
                        _parents.add(SyndieURI.createMessage(ancestor.scope, ancestor.messageId));
                    }
                } else {
                    _ui.debugMessage("parentMessage is " + uri + ", and it has no ancestors");
                }

                ThreadBuilder builder = new ThreadBuilder(_client, _ui);
                HashSet msgIds = new HashSet(1);
                msgIds.add(tmi);
                List roots = builder.buildThread(msgIds);
                _ui.debugMessage("thread: " + roots);
                _threadTree.setMessages(roots);
                _threadTree.select(uri);
                
                if (_subject.getText().trim().length() <= 0) {
                    String parentSubject = MessageView.calculateSubject(_client, _ui, _translationRegistry, uri).trim();
                    if ( (parentSubject.length() > 0) && (!Constants.lowercase(parentSubject).startsWith("re:")) ) {
                        _subject.setText("re: " + parentSubject);
                        _abbrSubject.setText(_subject.getText());
                    } else {
                        _subject.setText(parentSubject);
                        _abbrSubject.setText(_subject.getText());
                    }
                }
            } else {
                _ui.debugMessage("parentMessage is " + uri + ", but we don't know it, so don't know its ancestors");

                ThreadBuilder builder = new ThreadBuilder(_client, _ui);
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
        _ui.debugMessage("configuration complete, with attachments: " + attach);
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
        _modifiedSinceSave = false;
        _modifiedSinceOpen = uri.getLong("postponeid") != null;
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
        GridLayout gl = new GridLayout(6, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        header.setLayout(gl);
        header.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _headers = header;
        
        _authorLabel = new Label(header, SWT.NONE);
        _authorLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        Composite authorGroup = new Composite(header, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.verticalSpacing = 0;
        gl.horizontalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        authorGroup.setLayout(gl);
        authorGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        
        _authorCurrentLabel = new Label(authorGroup, SWT.NONE);
        _authorCurrentLabel.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, true, false));
        
        _authorChangeButton = new Button(authorGroup, SWT.PUSH);
        _authorChangeButton.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _authorChangeButton.addSelectionListener(new FireSelectionListener() {
            public void fire() { pickAuthor(); }
        });

        _signAsLabel = new Label(header, SWT.NONE);
        _signAsLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _signAsGroup = new Composite(header, SWT.NONE);
        _signAsGroup.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));
        gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.verticalSpacing = 0;
        gl.horizontalSpacing = 0;
        _signAsGroup.setLayout(gl);
        
        _signAsCurrentLabel = new Label(_signAsGroup, SWT.NONE);
        _signAsCurrentLabel.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));
        _signAsChangeButton = new Button(_signAsGroup, SWT.PUSH);
        _signAsChangeButton.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _signAsChangeButton.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                final ForumReferenceChooserPopup popup = new ForumReferenceChooserPopup(_client, _ui, _themeRegistry, _translationRegistry, _navControl, _banControl, _bookmarkControl, _root, null, new IdentityChannelSource());
                popup.setListener(new ReferenceChooserTree.AcceptanceListener() {
                    public void referenceAccepted(SyndieURI uri) {
                        popup.dispose();
                        if ( (uri != null) && (uri.isChannel()) && (uri.getScope() != null) ) {
                            _signAsChannel = uri.getScope();
                            long chanId = _client.getChannelId(_signAsChannel);
                            _signAsCurrentLabel.setText(getSummary(chanId));
                        }
                    }
                    public void referenceChoiceAborted() { popup.dispose(); }
    
                });
                popup.open();
            }
        });
        
        _authorHidden = new Button(header, SWT.CHECK);
        _authorHidden.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _signAsLabel.setVisible(false);
        ((GridData)_signAsLabel.getLayoutData()).exclude = true;
        _signAsGroup.setVisible(false);
        ((GridData)_signAsGroup.getLayoutData()).exclude = true;
        _authorHidden.setVisible(false);
        ((GridData)_authorHidden.getLayoutData()).exclude = true;
        
        _hideHeaderButton = new Button(header, SWT.PUSH);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, false, false, 1, 4);
        _hideHeaderButton.setLayoutData(gd);
        _hideHeaderButton.addSelectionListener(new FireSelectionListener() { public void fire() { hideHeaders(); } });
        
        _toLabel = new Label(header, SWT.NONE);
        _toLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        Composite toGroup = new Composite(header, SWT.NONE);
        toGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 4, 1));
        gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.verticalSpacing = 0;
        gl.horizontalSpacing = 0;
        toGroup.setLayout(gl);
        
        _toCurrentLabel = new Label(toGroup, SWT.NONE);
        _toCurrentLabel.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));
        _toChangeButton = new Button(toGroup, SWT.PUSH);
        _toChangeButton.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _toChangeButton.addSelectionListener(new FireSelectionListener() {
            public void fire() { pickForum(); }
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
        
        _hideHeaderButton.setText("Hide");
        _subjectLabel.setText("Subject:");
        _tagLabel.setText("Tags:");
        _authorLabel.setText("Author:");
        _signAsLabel.setText("Signed by:");
        _authorHidden.setText("Hidden?");
        _privacyLabel.setText("Privacy:");
    }
    
    private void initAbbrHeader() {
        _abbrHeaders = new Composite(_root, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        _abbrHeaders.setLayout(gl);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        gd.exclude = true;
        _abbrHeaders.setLayoutData(gd);
        
        _abbrSubjectLabel = new Label(_abbrHeaders, SWT.NONE);
        _abbrSubjectLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _abbrSubject = new Text(_abbrHeaders, SWT.BORDER | SWT.SINGLE);
        _abbrSubject.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        // keep the _subject as the authoritative subject source
        _abbrSubject.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent modifyEvent) {
                if (_abbrHeaders.isVisible()) _subject.setText(_abbrSubject.getText()); 
            }
        });
        
        _showHeaderButton = new Button(_abbrHeaders, SWT.PUSH);
        gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        _showHeaderButton.setLayoutData(gd);
        _showHeaderButton.addSelectionListener(new FireSelectionListener() { public void fire() { showHeaders(); } });
        
        _showHeaderButton.setText("Headers");
        _abbrSubjectLabel.setText("Subject:");
        _abbrHeaders.setVisible(false);
    }
    
    private void hideHeaders() {
        _root.setRedraw(false);
        _abbrSubject.setText(_subject.getText());
        ((GridData)_headers.getLayoutData()).exclude = true;
        _headers.setVisible(false);
        ((GridData)_abbrHeaders.getLayoutData()).exclude = false;
        _abbrHeaders.setVisible(true);
        _root.setRedraw(true);
        _root.layout(true);
        _showHeaderButton.forceFocus();
    }
    public void showHeaders() {
        _root.setRedraw(false);
        ((GridData)_headers.getLayoutData()).exclude = false;
        _headers.setVisible(true);
        ((GridData)_abbrHeaders.getLayoutData()).exclude = true;
        _abbrHeaders.setVisible(false);
        _root.setRedraw(true);
        _root.layout(true);
        _hideHeaderButton.forceFocus();
    }
    
    private void initToolbar() {
        if (!_buildToolbar) return;
        _bar = new MessageEditorToolbar(_root, this, _client, _bookmarkControl, _translationRegistry);
        _bar.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        addStatusListener(_bar);
    }
    
    private boolean isManaged(long chanId) {
        DBClient.ChannelCollector chans = _client.getNymChannels();
        return chans.getIdentityChannelIds().contains(new Long(chanId)) ||
               chans.getManagedChannelIds().contains(new Long(chanId));
    }
    
    private String getSummary(long chanId) {
        StringBuilder buf = new StringBuilder();
        String name = _client.getChannelName(chanId);
        String desc = _client.getChannelDescription(chanId);
        Hash chan = _client.getChannelHash(chanId);
        
        if ( (name != null) && (name.length() > 0) )
            buf.append(name);
        if (buf.length() > 0)
            buf.append(": ");
        if ( (desc != null) && (desc.length() > 0) ) {
            buf.append(desc);
            if (buf.length() > 0)
                buf.append(": ");
        }
        buf.append(chan.toBase64().substring(0,6));
        return buf.toString();
    }
    
    private void updateForum() {
        long forumId = -1;
        DBClient.ChannelCollector chans = _client.getNymChannels();
        
        if (_forum != null)
            forumId = _client.getChannelId(_forum);
        else
            forumId = ((Long)chans.getIdentityChannelIds().get(0)).longValue();
        
        redrawForumAvatar(_forum, forumId, getSummary(forumId), isManaged(forumId));
    }
    
    public void pickForum() {
        final ForumReferenceChooserPopup popup = new ForumReferenceChooserPopup(_client, _ui, _themeRegistry, _translationRegistry, _navControl, _banControl, _bookmarkControl, _root, null, new ForumChannelSource());
        popup.setListener(new ReferenceChooserTree.AcceptanceListener() {
            public void referenceAccepted(SyndieURI uri) {
                popup.dispose();
                if ( (uri != null) && (uri.isChannel()) && (uri.getScope() != null) )
                    pickForum(uri.getScope());
            }
            public void referenceChoiceAborted() { popup.dispose(); }
        });
        popup.open();
    }
    
    private void pickForum(Hash forum) {
        modified();
        _forum = forum;
        
        long channelId = _client.getChannelId(forum);
        String summary = getSummary(channelId);
        boolean managed = isManaged(channelId);
        
        if (channelId >= 0) {
            redrawForumAvatar(forum, channelId, summary, managed);
        }
        refreshAuthors();
        if (!validateAuthorForum())
            showUnauthorizedWarning();
    }
    public void pickForum(Hash forum, long channelId, String summary, boolean isManaged) {
        _ui.debugMessage("pick forum " + forum + " / " + summary);
        _forum = forum;
        redrawForumAvatar(forum, channelId, summary, isManaged);
        refreshAuthors();
        if (!validateAuthorForum())
            showUnauthorizedWarning();
    }
    private void redrawForumAvatar(Hash forum, long channelId, String summary, boolean isManaged) {
        if (summary != null)
            _toCurrentLabel.setText(summary);
        else
            _toCurrentLabel.setText("");
        _headers.layout(true, true);
        for (int i = 0; i < _editorStatusListeners.size(); i++)
            ((EditorStatusListener)_editorStatusListeners.get(i)).forumSelected(forum, channelId, summary, isManaged);
    }
    
    private void refreshAuthors() {
        List signAsKeys = null;
        boolean explicitKey = false;
        if (_forum != null) {
            List nymKeys = _client.getNymKeys(_forum, null);
            for (int i = 0; i < nymKeys.size(); i++) {
                NymKey key = (NymKey)nymKeys.get(i);
                if (!key.getAuthenticated()) {
                    _ui.debugMessage("key is not authenticated: " + key);
                    continue;
                }
                if (key.getIsExpired()) {
                    _ui.debugMessage("key is expired: " + key);
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
        _ui.debugMessage("refreshing authors: forum=" + _forum + " signAs keys: " + signAsKeys);
        
        GridData authorGD = (GridData)_authorCurrentLabel.getParent().getLayoutData();
        GridData signAsGD = (GridData)_signAsGroup.getLayoutData();
        GridData signAsLabelGD = (GridData)_signAsLabel.getLayoutData();
        GridData authorHiddenGD = (GridData)_authorHidden.getLayoutData();
        if ( (signAsKeys != null) && ( (signAsKeys.size() > 1) || explicitKey) ) {
            // multiple possible authors, so we need to populate and show the _signAs* fields
            signAsGD.exclude = false;
            signAsLabelGD.exclude = false;
            authorHiddenGD.exclude = false;
            _signAsGroup.setVisible(true);
            _signAsLabel.setVisible(true);
            _authorHidden.setVisible(true);
            authorGD.horizontalSpan = 1;
            
            boolean selected = false;
            for (int i = 0; i < signAsKeys.size(); i++) {
                NymKey key = (NymKey)signAsKeys.get(i);
                SigningPrivateKey priv = new SigningPrivateKey(key.getData());
                Hash pubHash = priv.toPublic().calculateHash();
                String name = _client.getChannelName(pubHash);
                if (name != null)
                    name = name + " (" + pubHash.toBase64().substring(0,6) + ")";
                else
                    name = "(" + pubHash.toBase64().substring(0,6) + ")";
                if (pubHash.equals(_forum)) {
                    _signAsChannel = pubHash;
                    _signAsCurrentLabel.setText(name);
                    selected = true;
                }
            }
            if (!selected)
                _signAsCurrentLabel.setText("");
        } else {
            // only one (or zero) possible authors, so hide the _from* fields and make sure
            // _signBy contains all of the known authors
            signAsGD.exclude = true;
            signAsLabelGD.exclude = true;
            authorHiddenGD.exclude = true;
            _signAsGroup.setVisible(false);
            _signAsLabel.setVisible(false);
            _authorHidden.setVisible(false);
            authorGD.horizontalSpan = 4;
        }
        // relayout the header
        _signAsLabel.getParent().layout(true, true);
    }
    
    private void updateAuthor() {
        boolean authorFound = false;
        
        long authorId = -1;
        if (_author == null) {
            authorId = ((Long)_client.getNymChannels().getIdentityChannelIds().get(0)).longValue();
            _author = _client.getChannelHash(authorId);
        } else {
            authorId = _client.getChannelId(_author);
        }
        _ui.debugMessage("updateAuthor: " + _author);

        redrawAuthorAvatar(_author, authorId, getSummary(authorId));
    }
    
    public void pickAuthor() {
        final ForumReferenceChooserPopup popup = new ForumReferenceChooserPopup(_client, _ui, _themeRegistry, _translationRegistry, _navControl, _banControl, _bookmarkControl, _root, null, new IdentityChannelSource());
        popup.setListener(new ReferenceChooserTree.AcceptanceListener() {
            public void referenceAccepted(SyndieURI uri) {
                popup.dispose();
                if ( (uri != null) && (uri.isChannel()) && (uri.getScope() != null) )
                    pickAuthor(uri.getScope());
            }
            public void referenceChoiceAborted() { popup.dispose(); }

        });
        popup.open();
    }
    
    private void pickAuthor(Hash author) {
        modified();
        _author = author;
        if (_author != null) {
            Properties prefs = _client.getNymPrefs();
            prefs.setProperty("editor.defaultAuthor", _author.toBase64());
            _client.setNymPrefs(prefs);
        }
        if (author != null) {
            long authorId = _client.getChannelId(author);
            String summary = getSummary(authorId);

            if ( (summary != null) && (authorId >= 0) )
                redrawAuthorAvatar(author, authorId, summary);
        }
        if (!validateAuthorForum())
            showUnauthorizedWarning();
    }
    public void pickAuthor(Hash author, long channelId, String summary) {
        _ui.debugMessage("pick author " + author + " / " + summary);
        _author = author;
        if (_author != null) {
            Properties prefs = _client.getNymPrefs();
            prefs.setProperty("editor.defaultAuthor", _author.toBase64());
            _client.setNymPrefs(prefs);
        }
        redrawAuthorAvatar(author, channelId, summary);
        if (!validateAuthorForum())
            showUnauthorizedWarning();
    }
    private void redrawAuthorAvatar(Hash author, long channelId, String summary) {
        if (summary != null)
            _authorCurrentLabel.setText(summary);
        else
            _authorCurrentLabel.setText("");
        _headers.layout(true, true);
        
        for (int i = 0; i < _editorStatusListeners.size(); i++)
            ((EditorStatusListener)_editorStatusListeners.get(i)).authorSelected(author, channelId, summary);
    }    
    
    
    /** 
     * make sure the author selected has the authority to post to the forum selected (or to
     * reply to an existing message, if we are replying)
     */
    private boolean validateAuthorForum() {
        Hash author = _author;
        ChannelInfo forum = null;
        if (_forum != null)
            forum = _client.getChannel(_client.getChannelId(_forum));
        
        boolean ok = true;
        
        _ui.debugMessage("validating author forum: author=" + _author + " forum=" + _forum);
        
        if ( (author != null) && (forum != null) ) {
            if (author.equals(forum.getChannelHash())) {
                // ok
                _ui.debugMessage("forum == author");
            } else if (forum.getAllowPublicPosts()) {
                // ok too
                _ui.debugMessage("forum allows public posts");
            } else if (forum.getAuthorizedManagerHashes().contains(author)) {
                // yep
                _ui.debugMessage("forum explicitly allowes the author to manage the forum");
            } else if (forum.getAuthorizedPosterHashes().contains(author)) {
                // again
                _ui.debugMessage("forum explicitly allows the author to post in the forum");
            } else if (_privacy.getSelectionIndex() == PRIVACY_REPLY) {
                // sure... though it won't go in the forum's scope
                _ui.debugMessage("post is a private reply");
            } else if (forum.getAllowPublicReplies() && (_parents.size() > 0) ) {
                // maybe... check to make sure the parent is allowed
                _ui.debugMessage("forum allows public replies, and our parents: " + _parents);
                boolean allowed = false;
                for (int i = _parents.size()-1; !allowed && i >= 0; i--) {
                    SyndieURI uri = (SyndieURI)_parents.get(i);
                    Hash scope = uri.getScope();
                    if (forum.getChannelHash().equals(scope) ||
                        forum.getAuthorizedManagerHashes().contains(scope) ||
                        forum.getAuthorizedPosterHashes().contains(scope)) {
                        // the scope is authorized, but make sure the uri is actually pointing to
                        // a post in the targetted forum!
                        long msgId = _client.getMessageId(scope, uri.getMessageId());
                        if (msgId >= 0) {
                            long targetChanId = _client.getMessageTarget(msgId);
                            if (forum.getChannelId() == targetChanId) {
                                allowed = true;
                            } else {
                                _ui.debugMessage("ancestor would be authorized, but they are targetting a different forum: " + targetChanId + ": " + uri);
                            }
                        } else {
                            _ui.debugMessage("ancestor would be authorized, but isn't known, so we don't know whether they're actually targetting the right forum: " + uri);
                        }
                    }
                }
                if (!allowed) {
                    // none of the ancestors were allowed, so reject
                    _ui.debugMessage("forum allows public replies but the parents are not authorized");
                    ok = false;
                }
            } else {
                // not allowed
                _ui.debugMessage("forum not allowed");
                ok = false;
            }
        }
        
        if (!ok && (forum != null) && (_signAsChannel != null)) { //_signAsHashes.size() > 0)) {
                Hash signAs = _signAsChannel;

            // the author may not be allowed, but the nym has an explicitly authorized private key
            // for posting or managing the forum.  note that the *nym* may have the key, but where they got
            // the key may only be possible for one or more of the nym's channels, and using another channel
            // as the author would link the channel that was authorized to receive the private key and the
            // channel that posted with the key.  the safe way to behave would be to run different unlinkable
            // nyms in their own Syndie instance, syncing between the instances without sharing any secrets
            List nymKeys = _client.getNymKeys(forum.getChannelHash(), null);
            for (int i = 0; i < nymKeys.size(); i++) {
                NymKey key = (NymKey)nymKeys.get(i);
                if (!key.getAuthenticated()) continue;
                if (key.getIsExpired()) continue;
                if (Constants.KEY_TYPE_DSA.equals(key.getType())) {
                    SigningPrivateKey priv = new SigningPrivateKey(key.getData());
                    if (priv.toPublic().calculateHash().equals(signAs)) {
                        _ui.debugMessage("Explicitly authorized 'sign as' key selected: " + signAs);
                        ok = true;
                        break;
                    }
                }
            }
        }
        
        return ok;
    }
    private void showUnauthorizedWarning() {
        MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_ERROR | SWT.OK);
        box.setMessage(_translationRegistry.getText(T_NOT_AUTHORIZED_MSG, "The selected author does not have permission to write in the selected forum - please adjust your selection"));
        box.setText(_translationRegistry.getText(T_NOT_AUTHORIZED_TITLE, "Not authorized"));
        box.open();
    }
    
    private static final String T_NOT_AUTHORIZED_MSG = "syndie.gui.messageeditor.notauthorized.msg";
    private static final String T_NOT_AUTHORIZED_TITLE = "syndie.gui.messageeditor.notauthorized.title";
    
    public void applyTheme(Theme theme) {
        _authorLabel.setFont(theme.DEFAULT_FONT);
        _authorChangeButton.setFont(theme.BUTTON_FONT);
        _authorCurrentLabel.setFont(theme.DEFAULT_FONT);
        _signAsChangeButton.setFont(theme.BUTTON_FONT);
        _signAsCurrentLabel.setFont(theme.DEFAULT_FONT);
        _authorHidden.setFont(theme.DEFAULT_FONT);
        _signAsLabel.setFont(theme.DEFAULT_FONT);
        _toLabel.setFont(theme.DEFAULT_FONT);
        _toCurrentLabel.setFont(theme.DEFAULT_FONT);
        _toChangeButton.setFont(theme.BUTTON_FONT);
        _subjectLabel.setFont(theme.DEFAULT_FONT);
        _subject.setFont(theme.DEFAULT_FONT);
        _tagLabel.setFont(theme.DEFAULT_FONT);
        _tag.setFont(theme.DEFAULT_FONT);
        if (_showActions) {
            _post.setFont(theme.BUTTON_FONT);
            _postpone.setFont(theme.BUTTON_FONT);
            _cancel.setFont(theme.BUTTON_FONT);
        }
        _privacyLabel.setFont(theme.DEFAULT_FONT);
        _privacy.setFont(theme.DEFAULT_FONT);
        _pageTabs.setFont(theme.TAB_FONT);
        _abbrSubjectLabel.setFont(theme.DEFAULT_FONT);
        _abbrSubject.setFont(theme.DEFAULT_FONT);
        _hideHeaderButton.setFont(theme.BUTTON_FONT);
        _showHeaderButton.setFont(theme.BUTTON_FONT);
    
        if (_bar != null) _bar.applyTheme(theme);
        
        _root.layout(true);
    }
    
    private static final String T_ATTACHMENTS_NONE = "syndie.gui.messageeditor.attachments.none";
    
    private static final String T_FROM_LINE = "syndie.gui.messageeditor.fromline";
    private static final String T_TO_LINE = "syndie.gui.messageeditor.toline";
    private static final String T_FROM_CHANGE = "syndie.gui.messageeditor.fromchange";
    private static final String T_TO_CHANGE = "syndie.gui.messageeditor.tochange";
    private static final String T_SIGNAS_CHANGE = "syndie.gui.messageeditor.signaschange";
    
    public void translate(TranslationRegistry registry) {
        _authorLabel.setText(registry.getText(T_FROM_LINE, "Author:"));
        _toLabel.setText(registry.getText(T_TO_LINE, "Post to:"));
        _authorChangeButton.setText(registry.getText(T_FROM_CHANGE, "Change author"));
        _toChangeButton.setText(registry.getText(T_TO_CHANGE, "Change forum"));
        _signAsChangeButton.setText(registry.getText(T_SIGNAS_CHANGE, "Change signed by"));
        
        _refEditorTab.setText(_translationRegistry.getText(T_REFTAB, "References"));
        _threadTab.setText(_translationRegistry.getText(T_THREADTAB, "Thread"));
    }

    // image popup stuff
    public void addAttachment() {
        FileDialog dialog = new FileDialog(_root.getShell(), SWT.MULTI | SWT.OPEN);
        Properties prefs = _client.getNymPrefs();
        dialog.setFilterPath(prefs.getProperty("editor.defaultAttachmentPath"));
        dialog.setText(_translationRegistry.getText(T_ADDATTACH, "Attach file"));
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
        prefs.setProperty("editor.defaultAttachmentPath", base);
        _client.setNymPrefs(prefs);
    }
    private static final String T_ADDATTACH = "syndie.gui.messageeditor.addattach";

    private static final String T_TOOLARGE_MSG = "syndie.gui.messageeditor.toolarge.msg";
    private static final String T_TOOLARGE_TITLE = "syndie.gui.messageeditor.toolarge.title";
    private static final String T_LARGE_MSG = "syndie.gui.messageeditor.large.msg";
    private static final String T_LARGE_TITLE = "syndie.gui.messageeditor.large.title";
    
    private boolean isValidSize(long length) {
        if (length > Constants.MAX_ATTACHMENT_SIZE) {
            MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_ERROR | SWT.OK);
            box.setMessage(_translationRegistry.getText(T_TOOLARGE_MSG, "The attachment could not be added, as it exceeds the maximum attachment size (" + Constants.MAX_ATTACHMENT_SIZE/1024 + "KB)"));
            box.setText(_translationRegistry.getText(T_TOOLARGE_TITLE, "Too large"));
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

        _ui.debugMessage("add attachment(" + fname + ") sz= " + file.length());
        
        if (!isValidSize(file.length()))
            return;
        
        final byte data[] = new byte[(int)file.length()];
        try {
            int read = DataHelper.read(new FileInputStream(file), data);
            if (read != data.length) {
                _ui.debugMessage("attachment was the wrong size (" + read + "/" + data.length + ")");
                return;
            }
        } catch (IOException ioe) {
            _ui.debugMessage("Unable to read the attachment", ioe);
            return;
        }
        final Properties cfg = new Properties();
        cfg.setProperty(Constants.MSG_ATTACH_CONTENT_TYPE, type);
        cfg.setProperty(Constants.MSG_ATTACH_NAME, name);
        _attachmentConfig.add(cfg);
        _attachmentData.add(data);
        
        addAttachmentTab(cfg, data);
        
        rebuildAttachmentSummaries();
        updateToolbar();
        _ui.debugMessage("Attachment read and added");
    }
    
    private void addAttachmentTab(final Properties cfg, final byte data[]) {
        int pages = _pageEditors.size();
        CTabItem item = new CTabItem(_pageTabs, SWT.NONE, pages + _attachmentData.size() - 1);
        //item.setText("attachment " + _attachmentData.size());
        Composite root = new Composite(_pageTabs, SWT.NONE);
        root.setLayout(new FillLayout());
        _attachmentRoots.add(root);
        item.setControl(root);
        //DBClient client
        AttachmentPreview preview = new AttachmentPreview(_client, _ui, _themeRegistry, _translationRegistry, root);
        preview.showURI(new AttachmentPreview.AttachmentSource() {
            public Properties getAttachmentConfig(int attachmentNum) { return cfg; }
            public byte[] getAttachmentData(int attachmentNum) { return data; }
        }, SyndieURI.createAttachment(null, -1, _attachmentData.size()));
        _attachmentPreviews.add(preview);
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
        addAttachmentTab(cfg, data);
        rebuildAttachmentSummaries();
        updateToolbar();
        return rv;
    }
    public void removeAttachment(int idx) {
        saveState();
        modified();
        if ( (_attachmentData.size() > 0) && (idx < _attachmentData.size()) ) {
            // should this check to make sure there aren't any pages referencing
            // this attachment first?
            _attachmentConfig.remove(idx);
            _attachmentData.remove(idx);
            _attachmentSummary.remove(idx);
            AttachmentPreview preview = (AttachmentPreview)_attachmentPreviews.remove(idx);
            preview.dispose();
            Composite root = (Composite)_attachmentRoots.remove(idx);
            root.dispose();
            CTabItem item = (CTabItem)_pageTabs.getItem(idx + _pageEditors.size());
            item.dispose();
        }
        rebuildAttachmentSummaries();
        for (int i = 0; i < _pageEditors.size(); i++) {
            PageEditor ed = (PageEditor)_pageEditors.get(i);
            ed.updated();
        }
        updateToolbar();
    }
    public void removeAttachment() {
        int attach = getCurrentAttachment();
        if (attach >= 0) removeAttachment(attach);
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
    public void updateImageAttachment(int imageNum, String contentType, final byte data[]) { 
        modified();
        int cur = 0;
        for (int i = 0; i < _attachmentConfig.size(); i++) {
            final Properties cfg = (Properties)_attachmentConfig.get(i);
            String type = cfg.getProperty(Constants.MSG_ATTACH_CONTENT_TYPE);
            if ( (type == null) || (!type.startsWith("image")) )
                continue;
            if (cur == imageNum) {
                cfg.setProperty(Constants.MSG_ATTACH_CONTENT_TYPE, contentType);
                _attachmentConfig.set(cur, data);
                AttachmentPreview preview = (AttachmentPreview)_attachmentPreviews.get(i);
                final int num = i;
                preview.showURI(new AttachmentPreview.AttachmentSource() {
                    public Properties getAttachmentConfig(int attachmentNum) { return cfg; }
                    public byte[] getAttachmentData(int attachmentNum) { return data; }
                }, SyndieURI.createAttachment(null, -1, num));
                rebuildAttachmentSummaries();
                updateToolbar();
                return;
            }
            cur++;
        }
    }
    private void rebuildAttachmentSummaries() {
        _attachmentSummary.clear();
        if (_attachmentData.size() > 0) {
            for (int i = 0; i < _attachmentData.size(); i++) {
                byte data[] = (byte[])_attachmentData.get(i);
                Properties cfg = (Properties)_attachmentConfig.get(i);
                StringBuilder buf = new StringBuilder();
                buf.append((i+1) + ": ");
                String name = cfg.getProperty(Constants.MSG_ATTACH_NAME);
                if (name != null)
                    buf.append(name).append(" ");
                String type = cfg.getProperty(Constants.MSG_ATTACH_CONTENT_TYPE);
                if (type != null)
                    buf.append('(').append(type).append(") ");
                buf.append("[" + data.length + " bytes]");
                String summary = buf.toString();
                _attachmentSummary.add(summary);
                
                CTabItem item = _pageTabs.getItem(_pageEditors.size()+i);
                item.setText(_translationRegistry.getText(T_ATTACHMENT_PREFIX, "Attachment ") + (i+1));
            }
        } else {
            _attachmentSummary.add(_translationRegistry.getText(T_ATTACHMENTS_NONE, "none"));
        }
        
/*        for (int i = 0; i < _editorStatusListeners.size(); i++)
            ((EditorStatusListener)_editorStatusListeners.get(i)).attachmentsRebuilt(_attachmentData, _attachmentSummary);*/
    }
    
    private static final String T_ATTACHMENT_PREFIX = "syndie.gui.messageeditor.attachment.prefix";
    
    private static final String T_PAGE_VIEW = "syndie.gui.messageeditor.pageview";
    private static final String T_PAGE_DELETE = "syndie.gui.messageeditor.pagedelete";
    
    public void insertAtCaret(String html) {
        PageEditor editor = getPageEditor();
        if (editor != null)
            editor.insertAtCaret(html);
    }
    
    public boolean isModifiedSinceOpen() { return _modifiedSinceOpen; }
    public SyndieURI getURI() {
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

    private class IdentityChannelSource implements NymChannelTree.ChannelSource {
        private List _nodes;
        public List getReferenceNodes() { return _nodes; }
        public boolean isManageable(long chanId) { return false; }
        public boolean isPostable(long chanId) { return false; }
        public boolean isWatched(long chanId) { return false; }
        public boolean isDeletable(long chanId) { return false; }
        public void loadSource() {
            if (_nodes != null)
                return;
            
            _nodes = new ArrayList();
            
            DBClient.ChannelCollector chans = _client.getNymChannels();
            List ids = chans.getIdentityChannelIds();
            for (int i = 0; i < ids.size(); i++) {
                Long id = (Long)ids.get(i);
                if (id.longValue() < 0)
                    continue;
                String name = _client.getChannelName(id.longValue());
                if (name == null) name = "";
                Hash scope = _client.getChannelHash(id.longValue());
                name = name + " [" + scope.toBase64().substring(0,6) + "]";
                SyndieURI uri = SyndieURI.createScope(scope);
                String desc = _client.getChannelDescription(id.longValue());
                if (desc == null) desc = "";
                ReferenceNode node = new ReferenceNode(name, uri, desc, "");
                node.setUniqueId(id.longValue());
                _nodes.add(node);
            }
        }
    }

    private class ForumChannelSource extends NymChannelSource {
        public ForumChannelSource() {
            super(MessageEditor.this._client, MessageEditor.this._translationRegistry, false, true, true, true, true);
        }
    }
}
