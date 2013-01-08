package syndie.data;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import net.i2p.data.DataHelper;

/**
 * tree structure referencing resources
 */
public class ReferenceNode {
    protected String _name;
    protected SyndieURI _uri;
    protected String _description;
    protected String _refType;
    protected List<ReferenceNode> _children;
    protected ReferenceNode _parent;
    /**
     * contains the node's index in a tree of nodes.  For instance, "1.3.2.15"
     * means this is the 15th child of the node "1.3.2", which is the 2nd child
     * of the node "1.3", which is the 3rd child of the root node ("1")
     */
    protected String _treeIndex;
    /** sequential index in the walk (unique within the tree, but not a descriptive location) */
    private int _treeIndexNum;
    private long _uniqueId;
    
    public ReferenceNode(String name, SyndieURI uri, String description, String type) {
        _name = name;
        _uri = uri;
        _description = description;
        _refType = type;
        _children = new ArrayList();
        _parent = null;
        _treeIndex = "1";
        _treeIndexNum = -1;
        _uniqueId = -1;
    }
    
    public String getName() { return _name; }
    public SyndieURI getURI() { return _uri; }
    public String getDescription() { return _description; }
    public String getReferenceType() { return _refType; }
    public int getChildCount() { return _children.size(); }
    public ReferenceNode getChild(int index) { return (ReferenceNode)_children.get(index); }
    public ReferenceNode getParent() { return _parent; }
    public String getTreeIndex() { return _treeIndex; }
    public int getTreeIndexNum() { return _treeIndexNum; }
    public long getUniqueId() { return _uniqueId >= 0 ? _uniqueId : hashCode(); }
    
    public void setName(String name) { _name = name; }
    public void setURI(SyndieURI uri) { _uri = uri; }
    public void setDescription(String desc) { _description = desc; }
    public void setReferenceType(String type) { _refType = type; }
    public void setTreeIndexNum(int num) { _treeIndexNum = num; }
    public void setUniqueId(long id) { _uniqueId = id; }
    
    public int hashCode() {
        return (int)(_uniqueId + (_uri != null ? _uri.hashCode() : 0) +
                     (_name != null ? _name.hashCode() : 0));
    }
    public boolean equals(Object obj) {
        if (obj == null) return false;
        ReferenceNode node = (ReferenceNode)obj;
        if (node.getUniqueId() != getUniqueId())
            return false;
        if (node.getURI() != getURI())
            return false;
        if (node.getTreeIndexNum() != getTreeIndexNum())
            return false;
        return ( ( (getName() != null) && (getName().equals(node.getName())) ) || 
                 ( (getName() == null) && (getName() == null) ) );
    }
    
    public ReferenceNode addChild(String name, SyndieURI uri, String description, String type) {
        ReferenceNode rv = new ReferenceNode(name, uri, description, type);
        rv._parent = this;
        rv._treeIndex = _treeIndex + "." + (_children.size()+1);
        //System.out.println("Add new child [" + rv._treeIndex + "/" + name + "] to " + _treeIndex + "/" + name);
        _children.add(rv);
        return rv;
    }
    
    public void addChild(ReferenceNode ref) {
        ref._parent = this;
        if (!_children.contains(ref)) {
            ref._treeIndex = _treeIndex + "." + (_children.size()+1);
            //System.out.println("Add child [" + ref._treeIndex + "/" + ref.getName() + "] to " + _treeIndex + "/" + _name + " (#kids: " + _children.size() + " instance: " + System.identityHashCode(this));
            _children.add(ref);
        } else {
            //System.out.println("child already added: " + ref._treeIndex + "/" + ref.getName() + " to " + _treeIndex + "/" + _name);
        }
    }
    public void removeChild(ReferenceNode child) {
        _children.remove(child);
        // does not reindex!
        child._parent = null;
    }
    public void clearChildren() { _children.clear(); }
    
    public ReferenceNode getByUniqueId(long id) {
        if (getUniqueId() == id) {
            return this;
        } else {
            for (int i = 0; i < getChildCount(); i++) {
                ReferenceNode child = getChild(i);
                ReferenceNode found = child.getByUniqueId(id);
                if (found != null)
                    return found;
            }
        }
        return null;
    }
    
    /** 
     * return the roots of the tree, as parsed from the given input stream.  The format is
     * simple:
     * "[\t]*$name\t$uri\t$refType\t$description\n"
     * the tab indentation at the beginning of the line determines the tree structure, such as
     *
     * rootName\t\t\tfirst grouping
     * \tchildName\t\t\t
     * \tsecondChild\t\t\t
     * \t\tchildOfSecondChild\t\t\t
     * secondRoot\t\t\t
     * thirdRoot\t\t\t
     * \tchildOfThirdRoot\t\t\t
     *
     * etc
     */
    public static List<ReferenceNode> buildTree(InputStream treeData) {
        int index = 0;
        List rv = new ArrayList();
        ReferenceNode prevNode = null;
        try {
            StringBuilder buf = new StringBuilder(256);
            while (DataHelper.readLine(treeData, buf)) {
                int indentation = 0;
                int nameEnd = -1;
                int uriEnd = -1;
                int refTypeEnd = -1;
                
                for (int i = 0; i < buf.length(); i++) {
                    if (buf.charAt(i) == '\t')
                        indentation++;
                    else
                        break;
                }
                for (int i = indentation; i < buf.length(); i++) {
                    if (buf.charAt(i) == '\t') {
                        if (nameEnd == -1)
                            nameEnd = i;
                        else if (uriEnd == -1)
                            uriEnd = i;
                        else if (refTypeEnd == -1)
                            refTypeEnd = i;
                    }
                }
                String name = null;
                if ((nameEnd)-(indentation) > 0)
                    name = buf.substring(indentation, nameEnd);
                String uri = null;
                if ((uriEnd)-(nameEnd+1) > 0)
                    uri = buf.substring(nameEnd+1, uriEnd);
                SyndieURI suri = null;
                if (uri != null) {
                    try {
                        suri = new SyndieURI(uri);
                    } catch (URISyntaxException use) {
                        suri = null;
                    }
                }
                String refType = null;
                if ((refTypeEnd)-(uriEnd+1) > 0)
                    refType = buf.substring(uriEnd+1, refTypeEnd);
                String desc = null;
                if ((buf.length())-(refTypeEnd+1) > 0)
                    desc = buf.substring(refTypeEnd+1).trim();
                
                // ok, now to interpret
                if ( (indentation == 0) || (prevNode == null) ) {
                    ReferenceNode node = new ReferenceNode(name, suri, desc, refType);
                    prevNode = node;
                    node._treeIndex = (""+rv.size() + 1);
                    //System.out.println("Create new [" + node._treeIndex + "/" + name + "]");
                    node._treeIndexNum = index++;
                    rv.add(node);
                } else {
                    int height = -1;
                    ReferenceNode cur = prevNode;
                    while (cur != null) {
                        cur = cur.getParent();
                        height++;
                    }
                    if (indentation > height) { // child of the prev node
                        prevNode = prevNode.addChild(name, suri, desc, refType);
                        prevNode._treeIndexNum = index++;
                    } else if (indentation == height) { // sibling of the prev node
                        prevNode = prevNode.getParent().addChild(name, suri, desc, refType);
                        prevNode._treeIndexNum = index++;
                    } else { // uncle/great-uncle/etc
                        int diff = height-indentation;
                        for (int i = 0; i < diff; i++)
                            prevNode = prevNode.getParent();
                        prevNode = prevNode.addChild(name, suri, desc, refType);
                        prevNode._treeIndexNum = index++;
                    }
                }
                buf.setLength(0);
            }
        } catch (IOException ioe) {
            // ignore
        }
        return rv;
    }
    
    public String toString() {
        StringBuilder buf = new StringBuilder(); 
        append(buf, this, 0); 
        return buf.toString();
    }
    
    /** stringify a forest of nodes into a format that can be parsed with buildTree() */
    public static String walk(List roots) {
        StringBuilder walked = new StringBuilder();
        for (int i = 0; i < roots.size(); i++) {
            ReferenceNode node = (ReferenceNode)roots.get(i);
            append(walked, node, 0);
        }
        return walked.toString();
    }
    
    /** depth first traversal */
    public static void walk(List roots, Visitor visitor) {
        for (int i = 0; i < roots.size(); i++) {
            ReferenceNode node = (ReferenceNode)roots.get(i);
            node.walk(visitor, 0, i);
        }
    }
    private void walk(Visitor visitor, int depth, int siblingOrder) {
        visitor.visit(this, depth, siblingOrder);
        for (int i = 0; i < _children.size(); i++) {
            ReferenceNode child = (ReferenceNode)_children.get(i);
            child.walk(visitor, depth+1, i);
        }
    }
    
    public static ArrayList<ReferenceNode> deepCopy(List<ReferenceNode> orig) {
        if (orig == null) return new ArrayList();;
        ArrayList rv = new ArrayList(orig.size());
        for (int i = 0; i < orig.size(); i++) {
            ReferenceNode node = (ReferenceNode)orig.get(i);
            rv.add(deepCopy(node));
        }
        return rv;
    }
    public static ReferenceNode deepCopy(ReferenceNode node) {
        if (node == null) return null;
        ReferenceNode copy = new ReferenceNode(node.getName(), node.getURI(), node.getDescription(), node.getReferenceType());
        for (int i = 0; i < node.getChildCount(); i++)
            copy.addChild(deepCopy(node.getChild(i)));
        return copy;
    }
    
    public interface Visitor {
        public void visit(ReferenceNode node, int depth, int siblingOrder);
    }
    
    public static void main(String args[]) {
        test(TEST_TREE1);
        test(TEST_TREE2);
        test(TEST_TREE3);
    }
    
    private static void test(String treeContent) {
        List tree = ReferenceNode.buildTree(new ByteArrayInputStream(DataHelper.getUTF8(treeContent)));
        StringBuilder walked = new StringBuilder(treeContent.length());
        for (int i = 0; i < tree.size(); i++) {
            ReferenceNode node = (ReferenceNode)tree.get(i);
            append(walked, node, 0);
        }
        if (walked.toString().equals(treeContent))
            System.out.println("Trees match: \n" + treeContent);
        else
            System.out.println("Trees do not match: tree content = \n" + treeContent + "\n\nwalked = \n" + walked.toString());
    }
    
    private static void append(StringBuilder walked, ReferenceNode node, int indent) {
        for (int i = 0; i < indent; i++)
            walked.append('\t');
        if (node.getName() != null)
            walked.append(node.getName());
        walked.append('\t');
        if (node.getURI() != null)
            walked.append(node.getURI().toString());
        walked.append('\t');
        if (node.getReferenceType() != null)
            walked.append(node.getReferenceType());
        walked.append('\t');
        if (node.getDescription() != null)
            walked.append(node.getDescription());
        walked.append('\n');
        for (int i = 0; i < node.getChildCount(); i++)
            append(walked, node.getChild(i), indent+1);
    }

    private static final String TEST_TREE1 = "rootName\t\t\tfirst grouping\n" +
                                             "\tchildName\t\t\t\n" +
                                             "\tsecondChild\t\t\t\n" +
                                             "\t\tchildOfSecondChild\t\t\t\n" +
                                             "secondRoot\t\t\t\n" +
                                             "thirdRoot\t\t\t\n" +
                                             "\tchildOfThirdRoot\t\t\t\n";
    
    private static final String TEST_TREE2 = "rootName\t\tfirstType\tfirst grouping\n" +
                                             "\tchildName\t\tsecondType\t\n" +
                                             "\tsecondChild\t\tthirdType\t\n" +
                                             "\t\tchildOfSecondChild\t\tfourthType\t\n" +
                                             "s\t\ta\td\n" +
                                             "thirdRoot\t\t\t\n" +
                                             "\tchildOfThirdRoot\t\t\t\n";
    
    private static final String TEST_TREE3 = "rootName\t\tfirstType\tfirst grouping\n" +
                                             "\tchildName\t\tsecondType\t\n" +
                                             "\tsecondChild\t\tthirdType\t\n" +
                                             "\t\tchildOfSecondChild\t\tfourthType\t\n" +
                                             "s\turn:syndie:dummy:de\ta\td\n" +
                                             "thirdRoot\t\t\t\n" +
                                             "\tchildOfThirdRoot\t\t\t\n\t\t\t\t\t\n";
}
