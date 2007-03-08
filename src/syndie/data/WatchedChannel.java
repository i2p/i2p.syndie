package syndie.data;

public class WatchedChannel {
    private long _channelId;
    private boolean _highlight;
    private boolean _importKeys;
    private boolean _importBookmarks;
    private boolean _importBans;
    private boolean _importArchives;
    
    public WatchedChannel(long channelId, boolean highlight, boolean impKeys, boolean impBookmarks, boolean impBans, boolean impArchives) {
        _channelId = channelId;
        _highlight = highlight;
        _importKeys = impKeys;
        _importBookmarks = impBookmarks;
        _importBans = impBans;
        _importArchives = impArchives;
    }
    
    /** channel being watched */
    public long getChannelId() { return _channelId; }
    /** if true, highlight the forum by watching it for unread messages */
    public boolean getHighlight() { return _highlight; }
    /** 
     * if true, when the forum offers us some keys, import them as authentic even
     * if they aren't implicitly authentic.
     */
    public boolean getImportKeys() { return _importKeys; }
    /** if true, import any bookmarks they offer to us */
    public boolean getImportBookmarks() { return _importBookmarks; }
    /** if true, import and honor any bans they advertise */
    public boolean getImportBans() { return _importBans; }
    /** if true, import any archives they advertise, but do not automatically sync with them */
    public boolean getImportArchives() { return _importArchives; }

    /** channel being watched */
    public void setChannelId(long id) { _channelId = id; }
    /** if true, highlight the forum by watching it for unread messages */
    public void setHighlight(boolean highlight) { _highlight = highlight; }
    /** 
     * if true, when the forum offers us some keys, import them as authentic even
     * if they aren't implicitly authentic.
     */
    public void setImportKeys(boolean imp) { _importKeys = imp; }
    /** if true, import any bookmarks they offer to us */
    public void setImportBookmarks(boolean imp) { _importBookmarks = imp; }
    /** if true, import and honor any bans they advertise */
    public void setImportBans(boolean imp) { _importBans = imp; }
    /** if true, import any archives they advertise, but do not automatically sync with them */
    public void setImportArchives(boolean imp) { _importArchives = imp; }
}
