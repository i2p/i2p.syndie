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
public class EditMessageTab extends BrowserTab {
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
                _description = "forum: " + scope.toBase64();
                //_icon = ImageUtil.ICON_QUESTION;
            }
        } else {
            _name = "post";
            _description = "post a new message";
            //_icon = ImageUtil.ICON_QUESTION;
        }
        _editor.setParentMessage(_parent);
        _editor.setScope(_scope);
        if (_asReply)
            _editor.setAsReply(true);
        reconfigItem();
    }
    
    protected void initComponents() {
        _editor = new MessageEditor(getClient(), getRoot(), new EditorListener(), getBrowser().getUI());
        _editor.addPage();
        getRoot().setLayout(new FillLayout());
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
    
    protected boolean allowClose() {
        MessageBox confirm = new MessageBox(getRoot().getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO | SWT.CANCEL);
        confirm.setText("Postpone message?");
        confirm.setMessage("Do you want to postpone this message to resume it later?");
        int rc = confirm.open();
        if (rc == SWT.YES) {
            _editor.postponeMessage();
            return true;
        } else if (rc == SWT.CANCEL) {
            return false;
        } else if (rc == SWT.NO) {
            return true;
        } else {
            return false;
        }
    }
    
    protected void disposeDetails() {}
}
