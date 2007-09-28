package syndie.gui;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.i2p.data.Hash;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

public class BookmarksChannelSource implements NymChannelTree.ChannelSource {
    private DBClient _client;
    private TranslationRegistry _translationRegistry;
    private List _refNodes;
    
    public BookmarksChannelSource(DBClient client, TranslationRegistry trans) {
        _client = client;
        _translationRegistry = trans;
        _refNodes = new ArrayList();
    }
    
    public List getReferenceNodes() { return _refNodes; }
    public boolean isManageable(long chanId) { return false; }
    public boolean isPostable(long chanId) { return false; }
    
    public void loadSource() {
        if (_refNodes.size() > 0)
            return;

        _refNodes.addAll(_client.getNymReferences(_client.getLoggedInNymId()));
    }
    
    public void clearSource() { _refNodes.clear(); }

    public boolean isWatched(long chanId) { return false; }
    public boolean isDeletable(long chanId) { return true; }
}
