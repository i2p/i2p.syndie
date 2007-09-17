package syndie.gui;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.i2p.data.Hash;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

public class SearchChannelSource implements NymChannelTree.ChannelSource {
    private DBClient _client;
    private TranslationRegistry _translationRegistry;
    private String _term;
    private List _refNodes;
    
    public SearchChannelSource(String term, DBClient client, TranslationRegistry trans) {
        _client = client;
        _translationRegistry = trans;
        _term = term;
        _refNodes = new ArrayList();
    }
    
    public List getReferenceNodes() { return _refNodes; }
    public boolean isManageable(long chanId) { return false; }
    public boolean isPostable(long chanId) { return false; }
    
    public void loadSource() {
        if (_refNodes.size() > 0)
            return;
        
        List ids = _client.getChannelIds(_term);
        for (int i = 0; i < ids.size(); i++) {
            Long chanId = (Long)ids.get(i);
            if ( (chanId == null) || (chanId.longValue() < 0) )
                continue;
            Hash scope = _client.getChannelHash(chanId.longValue());
            SyndieURI uri = SyndieURI.createScope(scope);
            ReferenceNode node = new ReferenceNode("", uri, "", "");
            node.setUniqueId(chanId.longValue());
            _refNodes.add(node);
        }
    }

    public boolean isWatched(long chanId) { return false; }
    public boolean isDeletable(long chanId) { return false; }
}
