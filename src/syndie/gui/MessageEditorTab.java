package syndie.gui;

import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.MessageBox;
import syndie.data.SyndieURI;

/**
 *  Contains a MessageEditor
 */
public class MessageEditorTab extends BrowserTab implements LocalMessageCallback, Translatable {
    private MessageEditor _editor;
    private Hash _forum;
    private SyndieURI _parent;
    private boolean _asReply;
    private String _name;
    
    public MessageEditorTab(BrowserControl browser, SyndieURI uri) {
        super(browser, uri);
        _editor.configurationComplete(getURI());
    }

    public MessageEditorTab(BrowserControl browser, SyndieURI uri, Hash forum, SyndieURI parent, boolean asReply) {
        super(browser, uri);
        _forum = forum;
        _parent = parent;
        browser.getUI().debugMessage("Editing message replying to " + parent + " in forum " + forum);
        _asReply = asReply;
        _editor.setParentMessage(_parent);
        if (forum != null)
            _editor.setForum(_forum);
        if (_asReply)
            _editor.setAsReply(true);
        //updateTabInfo(scope, null);
        _editor.configurationComplete(getURI());
    }
    
    protected void initComponents() {
        getRoot().setLayout(new FillLayout());
        _editor = new MessageEditor(getBrowser().getClient(), getBrowser().getUI(), getBrowser().getThemeRegistry(),
                                    getBrowser().getTranslationRegistry(), getBrowser(), getBrowser().getNavControl(),
                                    getBrowser(), getBrowser(), URIHelper.instance(),
                                    getRoot(), this, true,
                                    true, true, this);
        
        SyndieURI uri = getURI();
        Long postponeId = uri.getLong("postponeid");
        Long postponeVer = uri.getLong("postponever");
        if ( (postponeId != null) && (postponeVer != null) ) {
            _editor.addListener(getBrowser());
            _editor.loadState(postponeId.longValue(), postponeVer.intValue());
            _forum = _editor.getForum();
            if (_editor.getParentCount() > 0)
                _parent = _editor.getParent(0);
            else
                _parent = null;
            _asReply = _editor.getPrivacyReply();
        } else {
            _editor.addListener(getBrowser());
            getBrowser().getUI().debugMessage("message editor initialized.  adding page");
            //_editor.addPage();
            //getBrowser().getUI().debugMessage("page added");
        }
    
        getBrowser().getTranslationRegistry().register(this);
    }
    
    protected void disposeDetails() { 
        _editor.dispose();
        getBrowser().getTranslationRegistry().unregister(this);
    }

    public boolean close() {
        if (allowClose()) {
            return super.close();
        } else {
            return false;
        }
    }

    
    protected boolean allowClose() {
        if (!_editor.isModifiedSinceOpen()) return true;
        MessageBox confirm = new MessageBox(getRoot().getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO | SWT.CANCEL);
        confirm.setText(getBrowser().getTranslationRegistry().getText("Save message?"));
        confirm.setMessage(getBrowser().getTranslationRegistry().getText("Message has not been sent. Do you want to save the message as a draft?"));
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
        getBrowser().getNavControl().view(postedURI);
    }
    public void messagePostponed(long postponementId) { closeTab(); }
    public void messageCancelled() { closeTab(); }

    public void closeTab() {
        SyndieURI uri = super.getURI();
        getBrowser().getNavControl().unview(uri);
    }
    
    
    public Image getIcon() { return ImageUtil.ICON_TAB_PAGE; }

    public String getName() {
        String post = getText("Post");
        if (_name != null && _name.length() > 0)
            post = post + ": " + _name;
        return post;
    }

    /** 
     *  For the MessageEditor to set the subject to the tab
     *
     *  @since 1.102b-10
     */
    @Override
    public void setName(String name) {
        _name = name;
        reconfigItem();
    }

    public String getDescription() {
        String post = getText("Post a new message");
        if (_name != null && _name.length() > 0)
            post = post + ": " + _name;
        return post;
    }
    
    public boolean canShow(SyndieURI uri) {
        if (uri == null) return false;
        boolean rv = super.canShow(uri);
        if (!rv) {
            SyndieURI curURI = getURI();
            Long curPostponeId = curURI.getLong("postponeid");
            Long postponeId = uri.getLong("postponeid");
            if ( (postponeId != null) && (curPostponeId != null) && (curPostponeId.longValue() == postponeId.longValue()) )
                return true; // only resume once per postponeId (even if they are different versions
        }
        return rv;
    }
    
    public SyndieURI getURI() { 
        SyndieURI uri = _editor.getURI();
        if (uri != null) {
            return uri;
        } else {
            return super.getURI();
        }
    }
    
    public void toggleMaxView() { _editor.toggleMaxView(); }
    public void toggleMaxEditor() { _editor.toggleMaxEditor(); }
    
    public void translate(TranslationRegistry registry) {
        reconfigItem(); // queries getName/getDescription
    }
}
