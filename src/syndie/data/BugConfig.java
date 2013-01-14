package syndie.data;

import java.net.URISyntaxException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * contains the general configuration attributes necessary for filing bugs
 */
public class BugConfig {
    private List _componentNodes;
    private ReferenceNode _componentDefault;
    
    private List _typeIds;
    private List _typeNames;
    private int _typeDefault;
    
    private List _severityIds;
    private List _severityNames;
    private int _severityDefault;
    
    private SyndieURI _targetScope;
    
    public BugConfig() {
        _componentNodes = new ArrayList();
        
        _typeIds = new ArrayList();
        _typeNames = new ArrayList();
        _typeDefault = -1;
        
        _severityIds = new ArrayList();
        _severityNames = new ArrayList();
        _severityDefault = -1;
    }
    
    public int getComponentCount() { return _componentNodes.size(); }
    public ReferenceNode getComponent(int idx) { return (ReferenceNode)_componentNodes.get(idx); }
    public ReferenceNode getComponentDefault() { return _componentDefault; }
    
    public int getTypeCount() { return _typeIds.size(); }
    public String getTypeId(int idx) { return (String)_typeIds.get(idx); }
    public String getTypeName(int idx) { return (String)_typeNames.get(idx); }
    public int getTypeDefaultIndex() { return _typeDefault; }
    
    public int getSeverityCount() { return _severityIds.size(); }
    public String getSeverityId(int idx) { return (String)_severityIds.get(idx); }
    public String getSeverityName(int idx) { return (String)_severityNames.get(idx); }
    public int getSeverityDefaultIndex() { return _severityDefault; }
    
    public SyndieURI getTargetScope() { return _targetScope; }
    
    public void load(Connection con) throws SQLException {
        loadComponents(con);
        loadTypes(con);
        loadSeverities(con);
        loadTargetScope(con);
    }
    private static final String SQL_GET_COMPONENTS = "SELECT componentId, defaultDisplayName, isDefault, parentId FROM bugTrackComponent ORDER BY sortOrder ASC";
    private void loadComponents(Connection con) throws SQLException {
        LinkedHashMap refNodes = new LinkedHashMap();
        HashMap parents = new HashMap();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = con.prepareStatement(SQL_GET_COMPONENTS);
            rs = stmt.executeQuery();
            while (rs.next()) {
                String id = rs.getString(1);
                String name = rs.getString(2);
                boolean isDefault = rs.getBoolean(3);
                if (rs.wasNull()) isDefault = false;
                String parentId = rs.getString(4);
                
                ReferenceNode node = new ReferenceNode(id, null, name, null);
                refNodes.put(id, node);
                if (parentId != null)
                    parents.put(id, parentId);
                if (isDefault) _componentDefault = node;
            }
        } finally {
            if (rs != null) rs.close();
            if (stmt != null) stmt.close();
        }
        
        // now turn the parents & refNodes into a tree
        List roots = new ArrayList();
        for (Iterator iter = refNodes.keySet().iterator(); iter.hasNext(); ) {
            String id = (String)iter.next();
            ReferenceNode node = (ReferenceNode)refNodes.get(id);
            String parentId = (String)parents.get(id);
            if (parentId == null) {
                roots.add(node);
            } else {
                ReferenceNode parent = (ReferenceNode)refNodes.get(parentId);
                parent.addChild(node);
            }
        }
        _componentNodes = roots;
    }
    
    private static final String SQL_GET_TYPES = "SELECT typeId, defaultDisplayName, isDefault FROM bugTrackType ORDER BY sortOrder ASC";
    private void loadTypes(Connection con) throws SQLException {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = con.prepareStatement(SQL_GET_TYPES);
            rs = stmt.executeQuery();
            int idx = 0;
            while (rs.next()) {
                String id = rs.getString(1);
                String name = rs.getString(2);
                boolean isDefault = rs.getBoolean(3);
                if (rs.wasNull()) isDefault = false;
                
                _typeIds.add(id);
                _typeNames.add(name);
                if (isDefault) _typeDefault = idx;
                idx++;
            }
        } finally {
            if (rs != null) rs.close();
            if (stmt != null) stmt.close();
        }
    }
    
    private static final String SQL_GET_SEVERITIES = "SELECT severityId, defaultDisplayName, isDefault FROM bugTrackSeverity ORDER BY sortOrder ASC";
    private void loadSeverities(Connection con) throws SQLException {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = con.prepareStatement(SQL_GET_SEVERITIES);
            rs = stmt.executeQuery();
            int idx = 0;
            while (rs.next()) {
                String id = rs.getString(1);
                String name = rs.getString(2);
                boolean isDefault = rs.getBoolean(3);
                if (rs.wasNull()) isDefault = false;
                
                _severityIds.add(id);
                _severityNames.add(name);
                if (isDefault) _severityDefault = idx;
                idx++;
            }
        } finally {
            if (rs != null) rs.close();
            if (stmt != null) stmt.close();
        }
    }
    
    private static final String SQL_GET_TARGET_SCOPE = "SELECT prefValue FROM nymPref WHERE prefName = 'bugtrack.targetChannel'";
    private void loadTargetScope(Connection con) throws SQLException {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = con.prepareStatement(SQL_GET_TARGET_SCOPE);
            rs = stmt.executeQuery();
            if (rs.next()) {
                String uri = rs.getString(1);
                if (uri != null) {
                    try {
                        SyndieURI suri = new SyndieURI(uri);
                        _targetScope = suri;
                    } catch (URISyntaxException use) {
                        //
                    }
                }
            }
        } finally {
            if (rs != null) rs.close();
            if (stmt != null) stmt.close();
        }
    }
}
