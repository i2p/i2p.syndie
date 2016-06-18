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
    private final boolean _isIgnored;
    private final boolean _isBanned;
    private final boolean _loadOnStart;

    public NymReferenceNode(String name, SyndieURI uri, String description, long uriId, long groupId, long parentGroupId,
                            int siblingOrder, boolean isIgnored, boolean isBanned, boolean loadOnStart) {
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

    //public void setIsIgnored(boolean ignore) { _isIgnored = ignore; }
    public boolean getIsBanned() { return _isBanned; }
    //public void setIsBanned(boolean banned) { _isBanned = banned; }
    public boolean getLoadOnStart() { return _loadOnStart; }
    //public void setLoadOnStart(boolean load) { _loadOnStart = load; }
    
    //public void setURIId(long id) { _uriId = id; }
    public void setGroupId(long id) { _groupId = id; }
    public void setParentGroupId(long id) { _parentGroupId = id; }
    public void setSiblingOrder(int order) { _siblingOrder = order; }
    
    public void updateData(long groupId, int siblingOrder, long uriId) {
        _groupId = groupId;
        _siblingOrder = siblingOrder;
        _uriId = uriId;
    }
    
    public long getUniqueId() { return _groupId; }
    
    public void sortChildren() {
        TreeMap<Long, NymReferenceNode> sorted = new TreeMap<Long, NymReferenceNode>();
        for (int i = 0; i < _children.size(); i++) {
            NymReferenceNode child = (NymReferenceNode)_children.get(i);
            int off = 0;
            while (sorted.containsKey(Long.valueOf(child.getSiblingOrder()+off)))
                off++;
            sorted.put(Long.valueOf(child.getSiblingOrder()+off), child);
        }
        _children.clear();
        for (NymReferenceNode child : sorted.values()) {
            _children.add(child);
        }
    }
    
    public void addChild(ReferenceNode ref) {
        // remove it and readd it in the new position
        for (int i = 0; i < _children.size(); i++) {
            ReferenceNode child = _children.get(i);
            if ( (child.getURI() != null) && (child.getURI().equals(ref.getURI())) ) {
                // its a link to the same URL in the same category
                _children.remove(i);
                i--;
            } else if ( (ref.getURI() == null) && (child.getURI() == null) && (ref.getName() != null) && (child.getName() != null) && (ref.getName().equals(child.getName())) ) {
                // its a folder with the same name
                return;
                /*
                if (ref._children != null)
                    ref._children.addAll(child._children);
                else
                    ref._children = child._children;
                _children.remove(i);
                i--;
                 */
            }
        }
        //if (_children.contains(ref))
        //    _children.remove(ref); // readd in a new position
        
        ref._parent = this;
        if (ref instanceof NymReferenceNode) {
            NymReferenceNode r = (NymReferenceNode)ref;
            r.setParentGroupId(_groupId);
            int order = r.getSiblingOrder();
            if ( (order < 0) || (order > _children.size()) ) {
                order = _children.size();
                r.setSiblingOrder(order);
                _children.add(r);
            } else {
                for (int i = order; i < _children.size(); i++)
                    ((NymReferenceNode)_children.get(i)).setSiblingOrder(i+1);
                _children.add(order, r);
            }
            r._treeIndex = _treeIndex + "." + order;
        } else {
            _children.add(ref);
            ref._treeIndex = _treeIndex + "." + _children.size();
        }
    }
    
    public static NymReferenceNode deepCopyNym(NymReferenceNode node) {
        NymReferenceNode copy = new NymReferenceNode(node.getName(), node.getURI(), node.getDescription(), 
                node.getURIId(), node.getGroupId(), node.getParentGroupId(), node.getSiblingOrder(),
                node.getIsBanned(), node.getIsIgnored(), node.getLoadOnStart());
        copy.setReferenceType(node.getReferenceType());
        for (int i = 0; i < node.getChildCount(); i++)
            copy.addChild(NymReferenceNode.deepCopyNym(node.getChild(i)));
        return copy;
    }

    public static NymReferenceNode deepCopyNym(ReferenceNode node) {
        long parentId = -1;
        if (node.getParent() != null)
            parentId = node.getParent().getUniqueId();
        NymReferenceNode copy = new NymReferenceNode(node.getName(), node.getURI(), node.getDescription(), 
                -1, -1, parentId, 0, false, false, false);
        copy.setReferenceType(node.getReferenceType());
        for (int i = 0; i < node.getChildCount(); i++)
            copy.addChild(NymReferenceNode.deepCopyNym(node.getChild(i)));
        return copy;
    }
}
