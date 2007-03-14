package syndie.gui;

import java.io.File;
import java.util.List;
import java.util.TreeMap;
import syndie.data.NymReferenceNode;
import syndie.db.DBClient;
import syndie.db.UI;
import syndie.data.SyndieURI;
import net.i2p.data.Hash;
import org.eclipse.swt.custom.CTabFolder;

/** enable browser components to tell the browser to do things */
public interface BrowserControl extends NavigationControl, BookmarkControl, URIControl, BanControl, DataCallback, LocalMessageCallback {
    public CTabFolder getTabFolder();
    public DBClient getClient();
    public UI getUI();
    public TranslationRegistry getTranslationRegistry();
    public ThemeRegistry getThemeRegistry();
}
