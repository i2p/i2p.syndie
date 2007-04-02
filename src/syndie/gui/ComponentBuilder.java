package syndie.gui;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import syndie.data.Timer;
import syndie.db.DBClient;
import syndie.db.UI;

public class ComponentBuilder {
    private DBClient _client;
    private UI _ui;
    private ThemeRegistry _themes;
    private TranslationRegistry _trans;
    private BanControl _banControl;
    private NavigationControl _navControl;
    private URIControl _uriControl;
    private BookmarkControl _bookmarkControl;
    private DataCallback _dataCallback;
    private LocalMessageCallback _localMsgCallback;
    
    private static final ComponentBuilder _instance = new ComponentBuilder();

    public static final ComponentBuilder instance() { return _instance; }
    
    public void setBanControl(BanControl ctl) { _banControl = ctl; }
    public void setNavigationControl(NavigationControl ctl) { _navControl = ctl; }
    public void setURIControl(URIControl ctl) { _uriControl = ctl; }
    public void setBookmarkControl(BookmarkControl ctl) { _bookmarkControl = ctl; }
    public void setDataCallback(DataCallback dc) { _dataCallback = dc; }
    public void setLocalMessageCallback(LocalMessageCallback lmc) { _localMsgCallback = lmc; }
    public void setDBClient(DBClient client) { _client = client; }
    public void setUI(UI ui) { _ui = ui; }
    public void setThemeRegistry(ThemeRegistry themes) { _themes = themes; }
    public void setTranslationRegistry(TranslationRegistry trans) { _trans = trans; }
    
    public MessageTree createMessageTree(Composite parent, MessageTree.MessageTreeListener lsnr) {
        return new MessageTree(_client, _ui, _themes, _trans, _navControl, _uriControl, _bookmarkControl, _dataCallback, parent, lsnr);
    }
    
    public MessageTree createMessageTree(Composite parent, MessageTree.MessageTreeListener lsnr, boolean hideFilter) {
        return new MessageTree(_client, _ui, _themes, _trans, _navControl, _uriControl, _bookmarkControl, _dataCallback, parent, lsnr, hideFilter);
    }
    
    public WatchedMessageTree createWatchedMessageTree(Composite parent, MessageTree.MessageTreeListener lsnr, boolean hideFilter) {
        return new WatchedMessageTree(_client, _ui, _themes, _trans, _navControl, _uriControl, _bookmarkControl, _dataCallback, parent, lsnr, hideFilter);
    }
    
    public ReferenceChooserPopup createReferenceChooserPopup(Shell parent, ReferenceChooserTree.AcceptanceListener lsnr) {
        //return new ReferenceChooserPopupImpl(_client, _ui, _themes, _trans, parent, _navControl, _uriControl, lsnr);
        return new ForumReferenceChooserPopup(_client, _ui, _themes, _trans, parent, lsnr);
    }
    public ReferenceChooserPopup createReferenceChooserPopup(Shell parent, ReferenceChooserTree.AcceptanceListener lsnr, String titleKey, String titleVal) {
        //return new ReferenceChooserPopupImpl(_client, _ui, _themes, _trans, parent, _navControl, _uriControl, lsnr, titleKey, titleVal);
        return new ForumReferenceChooserPopup(_client, _ui, _themes, _trans, parent, lsnr);
    }
    public ReferenceChooserPopup createReferenceChooserPopup(Shell parent, String titleKey, String titleVal) {
        //return new ReferenceChooserPopupImpl(_client, _ui, _themes, _trans, parent, _navControl, _uriControl, titleKey, titleVal);
        return new ForumReferenceChooserPopup(_client, _ui, _themes, _trans, parent, null);
    }
    public ReferenceChooserPopup createReferenceChooserPopup(Shell parent) {
        //return new ReferenceChooserPopupImpl(_client, _ui, _themes, _trans, parent, _navControl, _uriControl);
        return new ForumReferenceChooserPopup(_client, _ui, _themes, _trans, parent, null);
    }
    
    public BrowserTree createBrowserTree(Browser browser, Timer timer, Composite parent, ReferenceChooserTree.ChoiceListener choiceListener, ReferenceChooserTree.AcceptanceListener acceptListener) {
        return new BrowserTree(browser, _client, _ui, _themes, _trans, _navControl, _uriControl, _bookmarkControl, parent, choiceListener, acceptListener, timer);
    }
    
    public StatusBar createStatusBar(Browser browser, Composite parent, Timer timer) {
        return new StatusBar(_client, _ui, _themes, _trans, _bookmarkControl, _navControl, _uriControl, browser, _dataCallback, parent, timer);
    }
    
    public PageRenderer createPageRenderer(Composite parent, boolean scrollbars) {
        return new PageRenderer(_client, _ui, _themes, _trans, parent, scrollbars, _dataCallback);
    }
    
    public MessagePreview createMessagePreview(Composite parent) {
        return new MessagePreview(_client, _ui, _themes, _trans, _navControl, _bookmarkControl, _uriControl, parent);
    }
    
    public BrowseForum createBrowseForum(Composite parent, MessageTree.MessageTreeListener lsnr, boolean viewOnly, boolean byForum) {
        return new BrowseForum(_client, _ui, _themes, _trans, parent, _navControl, _bookmarkControl, _uriControl, _dataCallback, _banControl, lsnr, viewOnly, byForum);
    }
    
    public RefTree createRefTree(Composite parent) {
        return new RefTree(_client, _ui, _themes, _trans, _navControl, _uriControl, parent);
    }
    
    public MessageFlagBar createMessageFlagBar(Composite parent, boolean includeTooltips) {
        return new MessageFlagBar(_client, _ui, _themes, _trans, _bookmarkControl, parent, includeTooltips);
    }
    
    public MessageReferencesEditor createMessageReferencesEditor(Composite parent) {
        return new MessageReferencesEditor(_client, _ui, _themes, _trans, parent, _navControl);
    }
    
    public ManageReferenceChooser createManageReferenceChooser(Composite parent, boolean editable) {
        return new ManageReferenceChooser(_client, _ui, _themes, _trans, parent, _navControl, _bookmarkControl, editable);
    }
}
