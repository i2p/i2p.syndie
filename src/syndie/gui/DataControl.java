package syndie.gui;

import java.util.List;
import java.util.TreeMap;
import net.i2p.data.Hash;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;

public interface DataControl {
    public List getPrivateMsgIds(boolean alreadyRead);
    public boolean ban(Hash scope);
    public boolean reimport(SyndieURI uri, String passphrase);

    public UI getUI();
    public void addUIListener(Browser.UIListener lsnr);
    public void removeUIListener(Browser.UIListener lsnr);
    
    public TranslationRegistry getTranslationRegistry();
    public ThemeRegistry getThemeRegistry();
    public DBClient getClient();

    /**
     * ordered map of postponeId (Long) to the most recent version (Integer),
     * with the most recent messages first 
     */
    public TreeMap getResumeable();
}
