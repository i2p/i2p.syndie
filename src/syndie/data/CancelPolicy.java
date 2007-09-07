package syndie.data;

/**
 *
 */
public class CancelPolicy {
    private boolean _scopeAll;
    private boolean _scopeLocallyManaged;
    private long _scopeChannelId;
    
    private boolean _honorFromAuthor;
    private boolean _honorFromForumOwner;
    private boolean _honorFromForumManager;
    private boolean _honorFromForumAuthorizedPoster;

    private boolean _isNew;
    
    private CancelPolicy() {
        _scopeAll = false;
        _scopeLocallyManaged = false;
        _scopeChannelId = -1;
        
        _honorFromAuthor = true;
        _honorFromForumOwner = true;
        _honorFromForumManager = false;
        _honorFromForumAuthorizedPoster = false;
        
        _isNew = false;
    }
    
    /**
     * @param forAll if true, this cancel policy applies to all forums in general,
     *               otherwise it applies to forums manageable by the local user
     */
    public CancelPolicy(boolean forAll) {
        this();
        if (forAll) {
            _scopeAll = true;
            _scopeLocallyManaged = false;
            _scopeChannelId = -1;
        } else {
            _scopeAll = false;
            _scopeLocallyManaged = true;
            _scopeChannelId = -1;
        }
    }
    
    /**
     * @param channelId the cancel policy applies to posts in the specific forum
     */
    public CancelPolicy(long channelId) {
        this();
        _scopeAll = false;
        _scopeLocallyManaged = false;
        _scopeChannelId = channelId;
    }
    
    public void load(CancelPolicy src) {
        _scopeAll = src.getScopeApplyToAll();
        _scopeLocallyManaged = src.getScopeApplyToLocallyManaged();
        _scopeChannelId = src.getScopeApplyToChannelId();
        
        _honorFromAuthor = src.getHonorFromAuthor();
        _honorFromForumOwner = src.getHonorFromForumOwner();
        _honorFromForumManager = src.getHonorFromForumManager();
        _honorFromForumAuthorizedPoster = src.getHonorFromForumAuthorizedPoster();
        
        _isNew = src.getIsNew();
    }

    
    public boolean getHonorFromAuthor() { return _honorFromAuthor; }
    public boolean getHonorFromForumOwner() { return _honorFromForumOwner; }
    public boolean getHonorFromForumManager() { return _honorFromForumManager; }
    public boolean getHonorFromForumAuthorizedPoster() { return _honorFromForumAuthorizedPoster; }

    public boolean getScopeApplyToAll() { return _scopeAll; }
    public boolean getScopeApplyToLocallyManaged() { return _scopeLocallyManaged; }
    public long getScopeApplyToChannelId() { return _scopeChannelId; }
    
    public void setHonorFromAuthor(boolean shouldHonor) { _honorFromAuthor = shouldHonor; }
    public void setHonorFromForumOwner(boolean shouldHonor) { _honorFromForumOwner = shouldHonor; }
    public void setHonorFromForumManager(boolean shouldHonor) { _honorFromForumManager = shouldHonor; }
    public void setHonorFromForumAuthorizedPoster(boolean shouldHonor) { _honorFromForumAuthorizedPoster = shouldHonor; }
    
    public boolean getIsNew() { return _isNew; }
    public void setIsNew(boolean isNew) { _isNew = isNew; }
    
    public String toString() {
        return _scopeChannelId + "/" + _scopeAll + "/" + _scopeLocallyManaged + ": " + _honorFromAuthor + "/" + _honorFromForumOwner + "/" + _honorFromForumManager + "/" + _honorFromForumAuthorizedPoster;
    }
}
