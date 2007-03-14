package syndie.gui;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import syndie.data.Timer;

public class ComponentBuilder {
    private DataControl _dataControl;
    private NavigationControl _navControl;
    private URIControl _uriControl;
    private BookmarkControl _bookmarkControl;
    private DataCallback _dataCallback;
    private LocalMessageCallback _localMsgCallback;
    
    private static final ComponentBuilder _instance = new ComponentBuilder();
    public static final ComponentBuilder instance() { return _instance; }
    
    public void setDataControl(DataControl ctl) { _dataControl = ctl; }
    public void setNavigationControl(NavigationControl ctl) { _navControl = ctl; }
    public void setURIControl(URIControl ctl) { _uriControl = ctl; }
    public void setBookmarkControl(BookmarkControl ctl) { _bookmarkControl = ctl; }
    public void setDataCallback(DataCallback dc) { _dataCallback = dc; }
    public void setLocalMessageCallback(LocalMessageCallback lmc) { _localMsgCallback = lmc; }
    
    public MessageTree createMessageTree(Composite parent, MessageTree.MessageTreeListener lsnr) {
        return new MessageTree(_dataControl, _navControl, _uriControl, _bookmarkControl, _dataCallback, parent, lsnr);
    }
    
    public MessageTree createMessageTree(Composite parent, MessageTree.MessageTreeListener lsnr, boolean hideFilter) {
        return new MessageTree(_dataControl, _navControl, _uriControl, _bookmarkControl, _dataCallback, parent, lsnr, hideFilter);
    }
    
    public WatchedMessageTree createWatchedMessageTree(Composite parent, MessageTree.MessageTreeListener lsnr, boolean hideFilter) {
        return new WatchedMessageTree(_dataControl, _navControl, _uriControl, _bookmarkControl, _dataCallback, parent, lsnr, hideFilter);
    }
    
    public ReferenceChooserPopup createReferenceChooserPopup(Shell parent, ReferenceChooserTree.AcceptanceListener lsnr) {
        return new ReferenceChooserPopup(parent, _dataControl, _navControl, _uriControl, lsnr);
    }
    public ReferenceChooserPopup createReferenceChooserPopup(Shell parent, ReferenceChooserTree.AcceptanceListener lsnr, String titleKey, String titleVal) {
        return new ReferenceChooserPopup(parent, _dataControl, _navControl, _uriControl, lsnr, titleKey, titleVal);
    }
    public ReferenceChooserPopup createReferenceChooserPopup(Shell parent, String titleKey, String titleVal) {
        return new ReferenceChooserPopup(parent, _dataControl, _navControl, _uriControl, titleKey, titleVal);
    }
    public ReferenceChooserPopup createReferenceChooserPopup(Shell parent) {
        return new ReferenceChooserPopup(parent, _dataControl, _navControl, _uriControl);
    }
    
    public BookmarkEditorPopup createBookmarkEditorPopup(Shell parent) {
        return new BookmarkEditorPopup(_dataControl, _bookmarkControl, parent);
    }
    
    public BrowserTree createBrowserTree(Browser browser, Timer timer, Composite parent, ReferenceChooserTree.ChoiceListener choiceListener, ReferenceChooserTree.AcceptanceListener acceptListener) {
        return new BrowserTree(browser, _dataControl, _navControl, _uriControl, _bookmarkControl, parent, choiceListener, acceptListener, timer);
    }
    
    public StatusBar createStatusBar(Browser browser, Composite parent, Timer timer) {
        return new StatusBar(_dataControl, _bookmarkControl, _navControl, _uriControl, browser, _dataCallback, parent, timer);
    }
    
    public PageRenderer createPageRenderer(Composite parent, boolean scrollbars) {
        return new PageRenderer(parent, scrollbars, _dataControl, _dataCallback);
    }
    
    public MessagePreview createMessagePreview(Composite parent) {
        return new MessagePreview(_dataControl, _navControl, _bookmarkControl, _uriControl, parent);
    }
    
    public BrowseForum createBrowseForum(Composite parent, MessageTree.MessageTreeListener lsnr, boolean viewOnly, boolean byForum) {
        return new BrowseForum(parent, _dataControl, _navControl, _bookmarkControl, _uriControl, _dataCallback, lsnr, viewOnly, byForum);
    }
    
    public RefTree createRefTree(Composite parent) {
        return new RefTree(_dataControl, _navControl, _uriControl, parent);
    }
    
    public MessageFlagBar createMessageFlagBar(Composite parent, boolean includeTooltips) {
        return new MessageFlagBar(_dataControl, _bookmarkControl, parent, includeTooltips);
    }
    
    public MessageReferencesEditor createMessageReferencesEditor(Composite parent) {
        return new MessageReferencesEditor(parent, _dataControl, _navControl);
    }
    
    public ManageReferenceChooser createManageReferenceChooser(Composite parent, boolean editable) {
        return new ManageReferenceChooser(parent, _dataControl, _navControl, _bookmarkControl, editable);
    }
}
