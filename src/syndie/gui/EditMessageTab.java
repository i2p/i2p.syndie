package syndie.gui;

import net.i2p.data.Hash;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import syndie.data.ChannelInfo;
import syndie.data.SyndieURI;

/**
 *
 */
public class EditMessageTab extends BrowserTab implements Translatable {
    private MessageEditor _editor;
    private String _name;
    private String _description;
    private Hash _scope;
    private SyndieURI _parent;
    private boolean _asReply;
    
    public EditMessageTab(BrowserControl browser, SyndieURI uri, Hash scope, SyndieURI parent, boolean asReply) {
        super(browser, uri);
        _parent = parent;
        browser.getUI().debugMessage("Editing message replying to " + parent + " in scope " + scope);
        _asReply = asReply;
        _editor.setParentMessage(_parent);
        _editor.setScope(_scope);
        if (_asReply)
            _editor.setAsReply(true);
        updateTabInfo(scope, null);
    }
    public EditMessageTab(BrowserControl browser, SyndieURI uri) { 
        super(browser, uri);
        updateTabInfo(_scope, null);
    }
    
    protected void initComponents() {
        getBrowser().getUI().debugMessage("Initializing message editor");
        SyndieURI uri = getURI();
        Long postponeId = uri.getLong("postponeid");
        Long postponeVer = uri.getLong("postponever");
        if ( (postponeId != null) && (postponeVer != null) ) {
            _editor = new MessageEditor(getBrowser(), getRoot(), new EditorListener());
            _editor.addListener(getBrowser().getMessageEditorListener());
            _editor.loadState(postponeId.longValue(), postponeVer.intValue());
            _scope = _editor.getTarget();
            if (_editor.getParentCount() > 0)
                _parent = _editor.getParent(0);
            else
                _parent = null;
            _asReply = _editor.getPrivacyReply();
        } else {
            _editor = new MessageEditor(getBrowser(), getRoot(), new EditorListener());
            _editor.addListener(getBrowser().getMessageEditorListener());
            getBrowser().getUI().debugMessage("message editor initialized.  adding page");
            _editor.addPage();
            getBrowser().getUI().debugMessage("page added");
        }
        getRoot().setLayout(new FillLayout());
        getBrowser().getTranslationRegistry().register(this);
    }
    
    private class EditorListener implements MessageEditor.MessageEditorListener {
        public void messageCreated(MessageEditor editor, SyndieURI postedURI) {
            closeTab();
            getBrowser().view(postedURI);
        }
        public void messagePostponed(MessageEditor editor, long postponementId) { closeTab(); }
        public void messageCancelled(MessageEditor editor) { closeTab(); }
    }
        
    public Image getIcon() { return ImageUtil.ICON_TAB_EDIT; }
    public String getName() { return _name; }
    public String getDescription() { return _description; }
    
    public boolean close() {
        if (allowClose()) {
            return super.close();
        } else {
            return false;
        }
    }
    
    protected boolean allowClose() {
        MessageBox confirm = new MessageBox(getRoot().getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO | SWT.CANCEL);
        confirm.setText(getBrowser().getTranslationRegistry().getText(T_CONFIRM_CLOSE_TITLE, "Postpone message?"));
        confirm.setMessage(getBrowser().getTranslationRegistry().getText(T_CONFIRM_CLOSE_MESSAGE, "Do you want to postpone this message to resume it later?"));
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
    
    protected void disposeDetails() {
        _editor.dispose();
        getBrowser().getTranslationRegistry().unregister(this);
    }

    private void updateTabInfo(Hash scope, TranslationRegistry registry) {
        if (scope != null) {
            long chanId = getClient().getChannelId(scope);
            if (chanId >= 0) {
                ChannelInfo chan = getClient().getChannel(chanId);
                if (chan != null) {
                    _name = chan.getName();
                    _description = chan.getDescription();
                    //_icon = createAvatar(chan);
                }
            }
            if (_name == null) {
                _name = scope.toBase64().substring(0,6);
                _description = (registry == null ? "forum: " : registry.getText(T_FORUM_DESC_PREFIX, "forum: ")) + scope.toBase64();
                //_icon = ImageUtil.ICON_QUESTION;
            }
        } else {
            _name = (registry == null ? "post" : registry.getText(T_FORUM_NAME_NEW, "post"));
            _description = (registry == null ? "post a new message" : registry.getText(T_FORUM_DESC_NEW, "post a new message"));
            //_icon = ImageUtil.ICON_QUESTION;
        }
        reconfigItem();
    }

    private static final String T_FORUM_DESC_PREFIX = "syndie.gui.editmessagetab.forumdesc.prefix";
    private static final String T_FORUM_DESC_NEW = "syndie.gui.editmessagetab.forumdesc.new";
    private static final String T_FORUM_NAME_NEW = "syndie.gui.editmessagetab.forumname.new";
    private static final String T_CONFIRM_CLOSE_TITLE = "syndie.gui.editmessagetab.confirm.title";
    private static final String T_CONFIRM_CLOSE_MESSAGE = "syndie.gui.editmessagetab.confirm.message";
    
    public void translate(TranslationRegistry registry) {
        // todo: translate
        updateTabInfo(_editor.getTarget(), registry);
    }
}
