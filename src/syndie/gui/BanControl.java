package syndie.gui;

import java.util.List;
import java.util.TreeMap;
import net.i2p.data.Hash;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;

public interface BanControl {
    /** confirm before actually banning the scope */
    public boolean ban(Hash scope);
    /** confirm before actually cancelling the message */
    public boolean cancelMessage(SyndieURI uri);
    /** confirm before actually banning the scope */
    public boolean deleteMessage(SyndieURI uri);
}
