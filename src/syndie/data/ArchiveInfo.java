package syndie.data;

/**
 *
 */
public class ArchiveInfo {
    private long _archiveId;
    private boolean _postAllowed;
    private boolean _readAllowed;
    private SyndieURI _uri;
    
    public ArchiveInfo() {
        _archiveId = -1;
        _postAllowed = false;
        _readAllowed = false;
        _uri = null;
    }
    public ArchiveInfo(SyndieURI uri) {
        this();
        _uri = uri;
    }
    
    public long getArchiveId() { return _archiveId; }
    public void setArchiveId(long id) { _archiveId = id; }
    public boolean getPostAllowed() { return _postAllowed; }
    public void setPostAllowed(boolean ok) { _postAllowed = ok; }
    public boolean getReadAllowed() { return _readAllowed; }
    public void setReadAllowed(boolean ok) { _readAllowed = ok; }
    public SyndieURI getURI() { return _uri; }
    public void setURI(SyndieURI uri) { _uri = uri; }
    
    public boolean equals(Object o) { return (o instanceof ArchiveInfo) ? ((ArchiveInfo)o)._archiveId == _archiveId : false; }
    public int hashCode() { return (int)_archiveId; }
    public String toString() { return "Archive " + _archiveId + ": " + _uri; }
}
