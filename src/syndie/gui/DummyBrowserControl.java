package syndie.gui;

import net.i2p.data.Hash;
import org.eclipse.swt.custom.CTabFolder;
import syndie.data.NymReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 *
 */
public class DummyBrowserControl implements BrowserControl {
    private DBClient _client;
    private UI _ui;
    
    public DummyBrowserControl(DBClient client, UI ui) {
        _client = client;
        _ui = ui;
    }

    public UI getUI() { return _ui; }
    public DBClient getClient() { return _client; }
    
    public void view(SyndieURI uri) {}
    public void unview(SyndieURI uri) {}
    public SyndieURI createPostURI(Hash forum, SyndieURI parent) { return null; }
    public SyndieURI createPostURI(Hash forum, SyndieURI parent, boolean asPrivateReply) { return null; }
    public SyndieURI createTextUIURI() { return null; }
    public SyndieURI createLogsURI() { return null; }
    public SyndieURI createManageURI(Hash forum) { return null; }
    public CTabFolder getTabFolder() { return null; }
    public void addUIListener(Browser.UIListener lsnr) {}
    public void removeUIListener(Browser.UIListener lsnr) {}

    public void bookmark(SyndieURI uri) {}
    public void deleteBookmark(long bookmarkGroupId) {}
    public void updateBookmark(NymReferenceNode bookmark) {}
    public void bookmark(NymReferenceNode node) {}
    
    public void showWaitCursor(boolean show) {}
    public TranslationRegistry getTranslationRegistry() { return null; }
}
