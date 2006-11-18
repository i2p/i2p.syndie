package syndie.gui;

import syndie.db.DBClient;
import syndie.db.UI;
import syndie.data.SyndieURI;
import net.i2p.data.Hash;
import org.eclipse.swt.custom.CTabFolder;

/** enable browser components to tell the browser to do things */
public interface BrowserControl {
    public void view(SyndieURI uri);
    public void unview(SyndieURI uri);

    public SyndieURI createPostURI(Hash forum, SyndieURI parent);
    public SyndieURI createPostURI(Hash forum, SyndieURI parent, boolean asPrivateReply);
    public SyndieURI createTextUIURI();
    public SyndieURI createLogsURI();
    public SyndieURI createMetaURI(Hash forum);

    public UI getUI();
    public DBClient getClient();

    public CTabFolder getTabFolder();

    public void addUIListener(Browser.UIListener lsnr);
    public void removeUIListener(Browser.UIListener lsnr);
}
