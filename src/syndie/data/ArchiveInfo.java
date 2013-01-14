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
    
    public boolean equals(Object o) { 
        if (o instanceof ArchiveInfo) {
            ArchiveInfo info = (ArchiveInfo)o;
            if (info._archiveId != _archiveId)
                return false;
            else if (!("" + info._uri).equals("" + _uri) ) // "null" is never a valid URI, so there's no conflict
                return false;
            else
                return true;
        } else {
            return false;
        }
    }
    public int hashCode() { return (int)_archiveId; }
    public String toString() { return "Archive " + _archiveId + ": " + _uri; }
}
