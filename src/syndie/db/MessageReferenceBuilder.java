package syndie.db;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;
import net.i2p.data.Hash;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;

/**
 * Walk through the database and build a tree of references from a given message
 */
class MessageReferenceBuilder {
    private DBClient _client;
    private Map _referenceIdToReferenceNode;
    
    public MessageReferenceBuilder(DBClient client) {
        _client = client;
        _referenceIdToReferenceNode = new TreeMap();
    }
    
    /** get the reference trees from the given message
     */
    public List loadReferences(long internalMsgId) throws SQLException {
        buildReferences(internalMsgId);
        resolveTree();
        List rv = new ArrayList();
        for (Iterator iter = _referenceIdToReferenceNode.values().iterator(); iter.hasNext(); ) {
            ReferenceNode node = (ReferenceNode)iter.next();
            if (node.getParent() == null)
                rv.add(node);
        }
        _referenceIdToReferenceNode.clear();
        return rv;
    }

    private static final String SQL_GET_MESSAGE_REFERENCE = "SELECT referenceId, parentReferenceId, siblingOrder, name, description, uriId, refType FROM messageReference WHERE msgId = ? ORDER BY referenceId ASC";
    /*          
    CREATE CACHED TABLE messageReference (
            msgId                   BIGINT NOT NULL
            -- referenceId is unique within the msgId scope
            , referenceId           INTEGER NOT NULL
            , parentReferenceId     INTEGER NOT NULL
            , siblingOrder          INTEGER NOT NULL
            , name                  VARCHAR(128)
            , description           VARCHAR(512)
            , uriId                 BIGINT
            , refType               VARCHAR(64)
            , PRIMARY KEY (msgId, referenceId)
            , UNIQUE (msgId, parentReferenceId, siblingOrder)
    );
     */
    private void buildReferences(long msgId) throws SQLException {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _client.con().prepareStatement(SQL_GET_MESSAGE_REFERENCE);
            stmt.setLong(1, msgId);
            rs = stmt.executeQuery();
            while (rs.next()) {
                // referenceId, parentReferenceId, siblingOrder, name, description, uriId, refType
                int refId = rs.getInt(1);
                if (rs.wasNull()) continue;
                int parentId = rs.getInt(2);
                if (rs.wasNull()) parentId = -1;
                int order = rs.getInt(3);
                if (rs.wasNull()) order = 0;
                String name = rs.getString(4);
                String desc = rs.getString(5);
                long uriId = rs.getLong(6);
                if (rs.wasNull()) uriId = -1;
                String refType = rs.getString(7);
                
                SyndieURI uri = _client.getURI(uriId);
                MsgReferenceNode node = new MsgReferenceNode(name, uri, desc, refType, refId, parentId, order);
                _referenceIdToReferenceNode.put(new Integer(refId), node);
            }
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private void resolveTree() {
        setParents();
        orderChildren();
    }
    
    private void setParents() {
        for (Iterator iter = _referenceIdToReferenceNode.values().iterator(); iter.hasNext(); ) {
            MsgReferenceNode node = (MsgReferenceNode)iter.next();
            if (node.getParentReferenceId() >= 0) {
                MsgReferenceNode parent = (MsgReferenceNode)_referenceIdToReferenceNode.get(new Integer(node.getParentReferenceId()));
                if (parent != null) {
                    node.setParent(parent);
                    parent.addChild(node);
                }
            }
        }
    }
    private void orderChildren() {
        for (Iterator iter = _referenceIdToReferenceNode.values().iterator(); iter.hasNext(); ) {
            MsgReferenceNode node = (MsgReferenceNode)iter.next();
            node.orderChildren();
        }
    }
    
    private static class MsgReferenceNode extends ReferenceNode {
        private int _referenceId;
        private int _parentReferenceId;
        private int _siblingOrder;
        public MsgReferenceNode(String name, SyndieURI uri, String description, String type, int refId, int parentId, int order) {
            super(name, uri, description, type);
            _referenceId = refId;
            _parentReferenceId = parentId;
            _siblingOrder = order;
        }
        public int getReferenceId() { return _referenceId; }
        public int getParentReferenceId() { return _parentReferenceId; }
        public int getSiblingOrder() { return _siblingOrder; }
        public void setParent(MsgReferenceNode node) { _parent = node; }
        public void orderChildren() {
            TreeMap ordered = new TreeMap();
            for (int i = 0; i < _children.size(); i++) {
                MsgReferenceNode child = (MsgReferenceNode)_children.get(i);
                ordered.put(new Integer(child.getSiblingOrder()), child);
            }
            _children.clear();
            for (Iterator iter = ordered.values().iterator(); iter.hasNext(); ) {
                MsgReferenceNode child = (MsgReferenceNode)iter.next();
                addChild(child); // adjusts the child's tree index too
            }
        }
    }
}
