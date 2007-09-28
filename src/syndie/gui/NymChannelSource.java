package syndie.gui;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.i2p.data.Hash;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.WatchedChannel;
import syndie.db.DBClient;

public class NymChannelSource implements NymChannelTree.ChannelSource {
    private DBClient _client;
    private TranslationRegistry _translationRegistry;
    private List _refNodes;
    private Set _manageable;
    private Set _postable;
    private List _watchedIds;
    
    private boolean _includeWatched;
    private boolean _includeIdent;
    private boolean _includeManage;
    private boolean _includePost;
    private boolean _includePubPost;
    
    public NymChannelSource(DBClient client, TranslationRegistry trans) {
        this(client, trans, true, true, true, true, true);
    }
    public NymChannelSource(DBClient client, TranslationRegistry trans, boolean includeWatched, boolean includeIdent, boolean includeManage, boolean includePost, boolean includePubPost) {
        _client = client;
        _translationRegistry = trans;
        _includeWatched = includeWatched;
        _includeIdent = includeIdent;
        _includeManage = includeManage;
        _includePost = includePost;
        _includePubPost = includePubPost;
        _refNodes = new ArrayList();
        _manageable = new HashSet();
        _postable = new HashSet();
        _watchedIds = null;
    }
    
    public List getReferenceNodes() { return _refNodes; }
    public boolean isManageable(long chanId) { return _manageable.contains(new Long(chanId)); }
    public boolean isPostable(long chanId) { return _postable.contains(new Long(chanId)); }
    
    public void clearSource() {
        _refNodes.clear();
        _manageable.clear();
        _postable.clear();
        _watchedIds.clear();
    }
    
    public void loadSource() {
        if (_refNodes.size() > 0)
            return;
        
        DBClient.ChannelCollector chans = _client.getNymChannels();
        List watchedIds = getWatchedIds();
        _watchedIds = watchedIds;

        ReferenceNode root = new ReferenceNode(_translationRegistry.getText(T_NYM_ITEM_ROOT, "Forums"), null, "", "");
        ReferenceNode watched = new ReferenceNode(_translationRegistry.getText(T_NYM_ITEM_WATCHED, "Watched forums"), null, "", "");
        ReferenceNode owned = new ReferenceNode(_translationRegistry.getText(T_NYM_ITEM_OWNED, "Locally owned forums"), null, "", "");
        ReferenceNode managed = new ReferenceNode(_translationRegistry.getText(T_NYM_ITEM_MANAGED, "Locally managed forums"), null, "", "");
        ReferenceNode authpost = new ReferenceNode(_translationRegistry.getText(T_NYM_ITEM_AUTHPOST, "Explicitly postable forums"), null, "", "");
        ReferenceNode pubpost = new ReferenceNode(_translationRegistry.getText(T_NYM_ITEM_PUBLICPOST, "Publicly postable forums"), null, "", "");

        if (_includeWatched)
            root.addChild(watched);
        if (_includeIdent)
            root.addChild(owned);
        if (_includeManage)
            root.addChild(managed);
        if (_includePost)
            root.addChild(authpost);
        if (_includePubPost)
            root.addChild(pubpost);

        if (_includeWatched)
            loadSource(watched, watchedIds, false, false);
        if (_includeIdent)
            loadSource(owned, chans.getIdentityChannelIds(), true, true);
        if (_includeManage)
            loadSource(managed, chans.getManagedChannelIds(), true, true);
        if (_includePost)
            loadSource(authpost, chans.getPostChannelIds(), false, true);
        if (_includePubPost)
            loadSource(pubpost, chans.getPublicPostChannelIds(), false, true);

        _refNodes.add(root);
    }

    private void loadSource(ReferenceNode parent, List chanIds, boolean manageable, boolean postable) {
        for (int i = 0; i < chanIds.size(); i++)
            loadSource(parent, (Long)chanIds.get(i), manageable, postable);
    }
    private void loadSource(ReferenceNode parent, Long chanId, boolean manageable, boolean postable) {
        if ( (chanId == null) || (chanId.longValue() < 0) )
            return;
        Hash scope = _client.getChannelHash(chanId.longValue());
        SyndieURI uri = SyndieURI.createScope(scope);
        ReferenceNode node = new ReferenceNode("", uri, "", "");
        node.setUniqueId(chanId.longValue());
        parent.addChild(node);
        if (manageable)
            _manageable.add(chanId);
        if (postable)
            _postable.add(chanId);
    }

    private List getWatchedIds() {
        List chans = _client.getWatchedChannels();
        ArrayList rv = new ArrayList();
        for (int i = 0; i < chans.size(); i++) {
            WatchedChannel chan = (WatchedChannel)chans.get(i);
            rv.add(new Long(chan.getChannelId()));
        }
        return rv;
    }

    public boolean isWatched(long chanId) { return _watchedIds.contains(new Long(chanId)); }
    public boolean isDeletable(long chanId) { return false; }

    private static final String T_NYM_ITEM_ROOT = "syndie.gui.nymchanneltree.item.root";
    private static final String T_NYM_ITEM_WATCHED = "syndie.gui.nymchanneltree.item.watched";
    private static final String T_NYM_ITEM_OWNED = "syndie.gui.nymchanneltree.item.owned";
    private static final String T_NYM_ITEM_MANAGED = "syndie.gui.nymchanneltree.item.managed";
    private static final String T_NYM_ITEM_AUTHPOST = "syndie.gui.nymchanneltree.item.authpost";
    private static final String T_NYM_ITEM_PUBLICPOST = "syndie.gui.nymchanneltree.item.publicpost";
}