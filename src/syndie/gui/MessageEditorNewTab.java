package syndie.gui;

import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.MessageBox;
import syndie.data.SyndieURI;

/**
 *
 */
public class MessageEditorNewTab extends BrowserTab implements MessageEditor.MessageEditorListener {
    private MessageEditorNew _editor;
    private Hash _forum;
    private SyndieURI _parent;
    private boolean _asReply;
    
    public MessageEditorNewTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri);
        _editor.configurationComplete();
    }
    public MessageEditorNewTab(BrowserControl browser, SyndieURI uri, Hash forum, SyndieURI parent, boolean asReply) {
        super(browser, uri);
        _forum = forum;
        _parent = parent;
        browser.getUI().debugMessage("Editing message replying to " + parent + " in forum " + forum);
        _asReply = asReply;
        _editor.setParentMessage(_parent);
        _editor.setForum(_forum);
        if (_asReply)
            _editor.setAsReply(true);
        //updateTabInfo(scope, null);
        _editor.configurationComplete();
    }
    
    protected void initComponents() {
        getRoot().setLayout(new FillLayout());
        _editor = new MessageEditorNew(getBrowser(), getRoot(), this);
        
        SyndieURI uri = getURI();
        Long postponeId = uri.getLong("postponeid");
        Long postponeVer = uri.getLong("postponever");
        if ( (postponeId != null) && (postponeVer != null) ) {
            _editor.addListener(getBrowser().getMessageEditorListener());
            _editor.loadState(postponeId.longValue(), postponeVer.intValue());
            _forum = _editor.getForum();
            if (_editor.getParentCount() > 0)
                _parent = _editor.getParent(0);
            else
                _parent = null;
            _asReply = _editor.getPrivacyReply();
        } else {
            _editor.addListener(getBrowser().getMessageEditorListener());
            getBrowser().getUI().debugMessage("message editor initialized.  adding page");
            //_editor.addPage();
            //getBrowser().getUI().debugMessage("page added");
        }
    
    }
    
    protected void disposeDetails() { _editor.dispose(); }

    public boolean close() {
        if (allowClose()) {
            return super.close();
        } else {
            return false;
        }
    }

    private static final String T_CONFIRM_CLOSE_TITLE = "syndie.gui.editmessagetab.confirm.title";
    private static final String T_CONFIRM_CLOSE_MESSAGE = "syndie.gui.editmessagetab.confirm.message";
    
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
    
    public void messageCreated(SyndieURI postedURI) {
        closeTab();
        getBrowser().view(postedURI);
    }
    public void messagePostponed(long postponementId) { closeTab(); }
    public void messageCancelled() { closeTab(); }

    public Image getIcon() { return ImageUtil.ICON_TAB_PAGE; }
    public String getName() { return "x editor"; }
    public String getDescription() { return "x desc"; }
}
