package syndie.gui;

import syndie.data.NymReferenceNode;
import syndie.db.DBClient;
import syndie.db.UI;
import syndie.data.SyndieURI;
import net.i2p.data.Hash;
import org.eclipse.swt.custom.CTabFolder;

/** enable browser components to tell the browser to do things */
public interface BrowserControl {
    public void view(SyndieURI uri);
    public void unview(SyndieURI uri);
    /** show a popup to bookmark the given uri in the user's set of bookmarked references */
    public void bookmark(SyndieURI uri);
    /** just add the given bookmark.  the node's groupId, siblingOrder, and uriId will be populated */
    public void bookmark(NymReferenceNode node);
    public void deleteBookmark(long bookmarkGroupId);
    public void updateBookmark(NymReferenceNode bookmark);

    public SyndieURI createPostURI(Hash forum, SyndieURI parent);
    public SyndieURI createPostURI(Hash forum, SyndieURI parent, boolean asPrivateReply);
    public SyndieURI createTextUIURI();
    public SyndieURI createLogsURI();
    public SyndieURI createManageURI(Hash forum);

    public void showWaitCursor(boolean wait);
    
    public UI getUI();
    public DBClient getClient();

    public CTabFolder getTabFolder();

    public void addUIListener(Browser.UIListener lsnr);
    public void removeUIListener(Browser.UIListener lsnr);
}
