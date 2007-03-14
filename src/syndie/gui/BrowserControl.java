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
public interface BrowserControl extends NavigationControl, BookmarkControl, URIControl, DataControl, DataCallback, LocalMessageCallback {
    public CTabFolder getTabFolder();
}
