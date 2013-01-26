package syndie.db;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.i2p.data.SigningPublicKey;
import syndie.data.CancelPolicy;
import syndie.data.SyndieURI;

public class CancelEngine {
    private DBClient _client;
    private UI _ui;
    private CancelPolicy _defaultPolicy;
    private CancelPolicy _locallyManagedPolicy;
    private Map _channelIdToPolicy;
    private Set _locallyManagedIds;
    
    public CancelEngine(DBClient client, UI ui) {
        _client = client;
        _ui = ui;
        _channelIdToPolicy = new HashMap();
        _locallyManagedIds = new HashSet();
    }
    
    /**
     * @param requestByChannelId metadata channel that sent in the cancel requests
     * @param cancelledURIs uris to cancel
     */
    public void processCancelRequests(long requestByChannelId, List cancelledURIs) {
        if ( (cancelledURIs == null) || (cancelledURIs.size() == 0) )
            return;
        _ui.debugMessage("received cancel requests from " + requestByChannelId + "/" + _client.getChannelHash(requestByChannelId) + ": " + cancelledURIs);
        _client.recordCancelRequests(requestByChannelId, cancelledURIs);

        loadPolicies();
        
        for (int i = 0; i < cancelledURIs.size(); i++) {
            SyndieURI uri = (SyndieURI)cancelledURIs.get(i);
            processCancelRequest(requestByChannelId, uri);
        }
    }

    public boolean processCancelRequest(long requestByChannelId, SyndieURI uri) {
        loadPolicies(); // noop if already loaded
        long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
        if (msgId >= 0) {
            long author = _client.getMessageAuthor(msgId);
            long scope = _client.getChannelId(uri.getScope());
            long target = _client.getMessageTarget(msgId);

            boolean honor = false;
            CancelPolicy policy = getCancelPolicy(target);

            if ( ( (author == requestByChannelId) || (scope == requestByChannelId) ) &&
                 policy.getHonorFromAuthor()) {
                honor = true;
            } else if ( (target == requestByChannelId) && (policy.getHonorFromForumOwner()) ) {
                honor = true;
            } else {
                if (policy.getHonorFromForumManager()) {
                    Set managerKeys = _client.getChannelManageKeys(target);
                    if (managerKeys != null) {
                        for (Iterator iter = managerKeys.iterator(); !honor && iter.hasNext(); ) {
                            SigningPublicKey pub = (SigningPublicKey)iter.next();
                            long managerId = _client.getChannelId(pub.calculateHash());
                            if (managerId == requestByChannelId) {
                                honor = true;
                            }
                        }
                    }
                }
                if (!honor && policy.getHonorFromForumAuthorizedPoster()) {
                    Set postKeys = _client.getChannelPostKeys(target);
                    if (postKeys != null) {
                        for (Iterator iter = postKeys.iterator(); !honor && iter.hasNext(); ) {
                            SigningPublicKey pub = (SigningPublicKey)iter.next();
                            long posterId = _client.getChannelId(pub.calculateHash());
                            if (posterId == requestByChannelId) {
                                honor = true;
                            }
                        }
                    }
                }
            }

            if (honor) {
                _ui.debugMessage("honoring cancel message of " + msgId + "/" + uri + " from " + requestByChannelId + " [policy: " + policy + "]");
                _client.honorCancel(uri, msgId);
                return true;
            } else {
                // ignore cancel
                _ui.debugMessage("ignoring cancel message of " + msgId + "/" + uri + " from " + requestByChannelId + " [policy: " + policy + "]");
            }
        }
        return false;
    }
    
    private void loadPolicies() {
        if (_defaultPolicy != null) return;

        _locallyManagedIds.clear();
        _locallyManagedIds.add(_client.getNymChannels().getIdentityChannelIds());
        _locallyManagedIds.add(_client.getNymChannels().getManagedChannelIds());
        
        Set policies = _client.getCancelPolicies();
        for (Iterator iter = policies.iterator(); iter.hasNext(); ) {
            CancelPolicy policy = (CancelPolicy)iter.next();
            if (policy.getScopeApplyToAll())
                _defaultPolicy = policy;
            else if (policy.getScopeApplyToLocallyManaged())
                _locallyManagedPolicy = policy;
            else
                _channelIdToPolicy.put(Long.valueOf(policy.getScopeApplyToChannelId()), policy);
        }
        
        if (_defaultPolicy == null) {
            _defaultPolicy = createDefaultPolicy();
            _client.saveCancelPolicy(_defaultPolicy);
        }
        if (_locallyManagedPolicy == null) {
            _locallyManagedPolicy = createLocallyManagedPolicy();
            _client.saveCancelPolicy(_locallyManagedPolicy);
        }
    }
    
    private CancelPolicy createDefaultPolicy() { return new CancelPolicy(true); }
    private CancelPolicy createLocallyManagedPolicy() { return new CancelPolicy(false); }
    
    private CancelPolicy getCancelPolicy(long msgTarget) {
        CancelPolicy policy = (CancelPolicy)_channelIdToPolicy.get(Long.valueOf(msgTarget));
        if (policy == null) {
            if (_locallyManagedIds.contains(Long.valueOf(msgTarget)))
                policy = _locallyManagedPolicy;
            else
                policy = _defaultPolicy;
        }
        return policy;
    }
}
