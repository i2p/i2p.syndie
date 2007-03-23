package syndie.gui.desktop;

import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.MessageBox;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;
import syndie.gui.LocalMessageCallback;
import syndie.gui.MessageEditor;
import syndie.gui.ThemeRegistry;
import syndie.gui.TranslationRegistry;
import syndie.gui.URIHelper;

/**
 *
 */
public class MessageEditorPanel extends DesktopPanel implements LocalMessageCallback {
    private MessageEditor _editor;
    
    // if resuming
    private long _postponeId;
    private int _postponeVersion;
    // if creating a new post
    private Hash _targetForum;
    private SyndieURI _parentURI;
    private boolean _asReply;

    public MessageEditorPanel(Desktop desktop, DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite parent, long postponeId, int postponeVersion, SyndieURI origURI) {
        super(desktop, client, themes, trans, parent, ui, origURI);
        _postponeId = postponeId;
        _postponeVersion = postponeVersion;
        initComponents();
    }
    public MessageEditorPanel(Desktop desktop, DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite parent, Hash forum, SyndieURI parentMsg, boolean asReply, SyndieURI origURI) {
        super(desktop, client, themes, trans, parent, ui, origURI);
        _targetForum = forum;
        _parentURI = parentMsg;
        ui.debugMessage("Editing message replying to " + parentMsg + " in forum " + forum);
        _asReply = asReply;
        initComponents();
    }
    
    public void dispose() {
        _editor.dispose();
        super.dispose();
    }
    
    Hash getTargetScope() { return _editor.getForum(); }
    
    private void initComponents() {
        Composite root = getRoot();
        root.setLayout(new FillLayout());
        _editor = new MessageEditor(_client, _ui, _themeRegistry, _translationRegistry, _desktop.getDataCallback(), _desktop.getNavControl(), _desktop.getBookmarkControl(), URIHelper.instance(), root, this);
        
        if ( (_postponeId > 0) && (_postponeVersion > 0) ) {
            //_editor.addListener(getBrowser());
            _editor.loadState(_postponeId, _postponeVersion);
            _editor.addListener(_desktop.getLocalMessageCallback());
            _editor.configurationComplete(getOriginalURI());
            //_forum = _editor.getForum();
            //if (_editor.getParentCount() > 0)
            //    _parent = _editor.getParent(0);
            //else
            //    _parent = null;
            //_asReply = _editor.getPrivacyReply();
        } else {
            //_editor.addListener(getBrowser());
            _ui.debugMessage("message editor initialized");
            
            _editor.setParentMessage(_parentURI);
            if (_targetForum != null)
                _editor.setForum(_targetForum);
            if (_asReply)
                _editor.setAsReply(true);
            //updateTabInfo(scope, null);
            _editor.addListener(_desktop.getLocalMessageCallback());
            _editor.configurationComplete(getOriginalURI());
            //_editor.addPage();
            //getBrowser().getUI().debugMessage("page added");
        }
        
        root.layout(true, true);
    }
    
    private static final String T_CONFIRM_CLOSE_TITLE = "syndie.gui.desktop.messageeditorpanel.confirm.title";
    private static final String T_CONFIRM_CLOSE_MESSAGE = "syndie.gui.desktop.messageeditorpanel.confirm.message";
    
    protected boolean allowClose() {
        if (!_editor.isModifiedSinceOpen()) return true;
        MessageBox confirm = new MessageBox(getRoot().getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO | SWT.CANCEL);
        confirm.setText(_translationRegistry.getText(T_CONFIRM_CLOSE_TITLE, "Postpone message?"));
        confirm.setMessage(_translationRegistry.getText(T_CONFIRM_CLOSE_MESSAGE, "Do you want to postpone this message to resume it later?"));
        int rc = confirm.open();
        if (rc == SWT.YES) {
            _editor.postponeMessage();
            return true;
        } else if (rc == SWT.CANCEL) {
            return false;
        } else if (rc == SWT.NO) {
            _editor.cancelMessage(false);
            return true;
        } else {
            return false;
        }
    }

    public void messageCreated(SyndieURI postedURI) {
        _desktop.getNavControl().view(postedURI);
        close();
    }

    public void messagePostponed(long postponementId) {
        close();
    }

    public void messageCancelled() {
        close();
    }
}
