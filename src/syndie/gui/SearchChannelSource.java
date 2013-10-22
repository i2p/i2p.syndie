package syndie.gui;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import net.i2p.data.Hash;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

class SearchChannelSource implements NymChannelTree.ChannelSource {
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
        Map nameToNode = new TreeMap();
        for (int i = 0; i < ids.size(); i++) {
            Long chanId = (Long)ids.get(i);
            if ( (chanId == null) || (chanId.longValue() < 0) )
                continue;
            Hash scope = _client.getChannelHash(chanId.longValue());
            SyndieURI uri = SyndieURI.createScope(scope);
            String name = _client.getChannelName(chanId.longValue());
            if (name == null) name = "";
            name = name + ": " + scope.toBase64();
            ReferenceNode node = new ReferenceNode(name, uri, "", "");
            node.setUniqueId(chanId.longValue());
            nameToNode.put(name, node);
        }
        for (Iterator iter = nameToNode.values().iterator(); iter.hasNext(); )
            _refNodes.add(iter.next());
    }

    public boolean isWatched(long chanId) { return false; }
    public boolean isDeletable(long chanId) { return false; }
}
