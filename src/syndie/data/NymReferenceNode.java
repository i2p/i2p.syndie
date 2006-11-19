package syndie.data;

import java.util.Iterator;
import java.util.TreeMap;

/**
 * organizes the nym's set of references to forums/messages/etc
 */
public class NymReferenceNode extends ReferenceNode {
    private long _uriId;
    private long _groupId;
    private long _parentGroupId;
    private int _siblingOrder;
    private boolean _isIgnored;
    private boolean _isBanned;
    private boolean _loadOnStart;

    public NymReferenceNode(String name, SyndieURI uri, String description, long uriId, long groupId, long parentGroupId, int siblingOrder, boolean isIgnored, boolean isBanned, boolean loadOnStart) {
        super(name, uri, description, null);
        _uriId = uriId;
        _groupId = groupId;
        _parentGroupId = parentGroupId;
        _siblingOrder = siblingOrder;
        _isIgnored = isIgnored;
        _isBanned = isBanned;
        _loadOnStart = loadOnStart;
    }
    public long getURIId() { return _uriId; }
    public long getGroupId() { return _groupId; }
    public long getParentGroupId() { return _parentGroupId; }
    public int getSiblingOrder() { return _siblingOrder; }
    public boolean getIsIgnored() { return _isIgnored; }
    public void setIsIgnored(boolean ignore) { _isIgnored = ignore; }
    public boolean getIsBanned() { return _isBanned; }
    public void setIsBanned(boolean banned) { _isBanned = banned; }
    public boolean getLoadOnStart() { return _loadOnStart; }
    public void setLoadOnStart(boolean load) { _loadOnStart = load; }
    
    public void updateData(long groupId, int siblingOrder, long uriId) {
        _groupId = groupId;
        _siblingOrder = siblingOrder;
        _uriId = uriId;
    }
    
    public long getUniqueId() { return _groupId; }
    
    public void sortChildren() {
        TreeMap sorted = new TreeMap();
        for (int i = 0; i < _children.size(); i++) {
            NymReferenceNode child = (NymReferenceNode)_children.get(i);
            int off = 0;
            while (sorted.containsKey(new Long(child.getSiblingOrder()+off)))
                off++;
            sorted.put(new Long(child.getSiblingOrder()+off), child);
        }
        _children.clear();
        for (Iterator iter = sorted.values().iterator(); iter.hasNext(); ) {
            NymReferenceNode child = (NymReferenceNode)iter.next();
            _children.add(child);
        }
    }
}
