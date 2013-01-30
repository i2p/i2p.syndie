package syndie.db;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.i2p.data.Hash;

import syndie.data.SyndieURI;
import syndie.util.Timer;

/**
 * direct implementation of the jwz threading algorithm without any filtering
 */
public class ThreadBuilder {
    private final DBClient _client;
    private final UI _ui;
    /** ThreadMsgId to Container */
    private final Map<ThreadMsgId, Container> _idTable;
    
    private static final boolean DEBUG = false;
    
    public ThreadBuilder(DBClient client, UI ui) {
        _client = client;
        _ui = ui;
        _idTable = new HashMap();
    }
    
    public List buildThread(Set threadMsgIds) { 
        Timer timer = new Timer("JWZ threader", _ui);
        List rv = buildThread(threadMsgIds, timer);
        timer.complete();
        return rv;
    }
    public List buildThread(Set threadMsgIds, Timer timer) {
        Map tmiToAncestors = new HashMap();
        Set newMsgIds = new HashSet();
        
        // find all children of the messages, in case they weren't included in the threadMsgIds
        ThreadAccumulatorJWZ.buildChildren(_client, _ui, newMsgIds, threadMsgIds, timer);
        timer.addEvent("children built");
        if (newMsgIds.size() > 0) {
            if (DEBUG) _ui.debugMessage("children exposed under existing messages: " + newMsgIds);
            threadMsgIds.addAll(newMsgIds);
        }
        
        // step 1: foreach msg
        for (Iterator iter = threadMsgIds.iterator(); iter.hasNext(); ) {
            ThreadMsgId tmi = (ThreadMsgId)iter.next();
            // step 1.A: build a container for each msg
            Container c = (Container)_idTable.get(tmi);
            if (c == null) {
                c = new Container();
                _idTable.put(tmi, c);
            }
            if (c.msg == null) {
                c.msg = new Message();
                c.msg.id = tmi;
                //timer.addEvent("building ancestor");
                int rc = ThreadAccumulatorJWZ.buildAncestors(_client, _ui, tmi, tmiToAncestors);
                //timer.addEvent("ancestor built");
                c.msg.references = (List)tmiToAncestors.get(tmi);
                if (DEBUG) _ui.debugMessage("ancestors for " + tmi + ": " + c.msg.references);
            }
        }
        
        // build all ancestors first, otherwise the loop check will be incomplete
        for (Iterator iter = threadMsgIds.iterator(); iter.hasNext(); ) {
            ThreadMsgId tmi = (ThreadMsgId)iter.next();
            Container c = (Container)_idTable.get(tmi);
            // step 1.B: fetch & link the ancestors of the containers
            if ( (c != null) && (c.msg != null) && (c.msg.references != null) ) {
                Container childContainer = c;
                if (childContainer.parent != null) {
                    if (DEBUG) _ui.debugMessage("existing thread container for " + tmi + ", already has a parent: " + c.parent);
                    continue;
                }

                if (DEBUG) _ui.debugMessage("existing thread container for " + tmi + ", building through refs: " + c.msg.references);

                for (int j = 0; j < c.msg.references.size(); j++) {
                    ThreadMsgId ref = (ThreadMsgId)c.msg.references.get(j);
                    Container refContainer = (Container)_idTable.get(ref);
                    if (refContainer == null) {
                        refContainer = new Container();
                        // lets prefill...
                        refContainer.msg = new Message();
                        refContainer.msg.id = ref;
                        
                        //timer.addEvent("building expanded ancestor");
                        int rc = ThreadAccumulatorJWZ.buildAncestors(_client, _ui, ref, tmiToAncestors);
                        //timer.addEvent("expanded ancestor built");
                        refContainer.msg.references = (List)tmiToAncestors.get(ref);
                        if (DEBUG) _ui.debugMessage("ancestors for " + ref + ": " + refContainer.msg.references);
                        
                        _idTable.put(ref, refContainer);
                    }
                    ThreadMsgId childId = null;
                    if (childContainer.msg != null) childId = childContainer.msg.id;
                    
                    // now insert it into the tree if it isn't a loop
                    if (childContainer.parent != null) {
                        if (DEBUG) _ui.debugMessage("not updating the parent of " + childContainer.toString().trim());
                    } else if ( (childContainer.msg == null) || (!isLoop(ref, childContainer.msg.id)) ) {
                        // step 1.C (kind of): set the parent
                        childContainer.parent = refContainer;
                        if (refContainer.child == null) {
                            if (DEBUG) _ui.debugMessage("setting parent of " + childId + " to " + ref + ": parent has no kids");
                            refContainer.child = childContainer;
                            childContainer.nextSibling = null;
                        } else {
                            // parent already has kids, so make us the last sibling
                            Container sibling = refContainer.child;
                            while (sibling.nextSibling != null)
                                sibling = sibling.nextSibling;
                            sibling.nextSibling = childContainer;
                            childContainer.nextSibling = null;
                            if (DEBUG) _ui.debugMessage("setting parent of " + childId + " to " + ref);
                        }
                    } else {
                        if (DEBUG) _ui.debugMessage("not setting the parent of " + childId + " to " + ref + ", loop: " + childContainer + " / " + _idTable.get(ref));
                    }
                    childContainer = refContainer;
                } // end looping over references
            } else {
                if (DEBUG) _ui.debugMessage("thread container for " + tmi + ": " + c);
            }
        }
        
        timer.addEvent("all ancestors built");
        List roots = new ArrayList();
        // step 2: find the root set
        for (Iterator iter = _idTable.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry)iter.next();
            ThreadMsgId id = (ThreadMsgId)entry.getKey();
            Container c = (Container)entry.getValue();
            if (c.parent == null)
                roots.add(c);
        }
        timer.addEvent("roots found");
        dumpTable();
        // step 3: eh. pretty useless, but doesn't hurt to do this early
        _idTable.clear();
        // step 4
        // skipped, because we do filtering elsewhere, and need to prune there
        //prune(roots);
        // skip step 5, because syndie uses real references, not "Re: " stuff
        // step 6: persist the tree to our own structure
        List rv = containerToRefNode(roots);
        timer.addEvent("refnodes built");
        // skip step 7, because we sort elsewhere
        return rv;
    }
    
    private void dumpTable() {
        if (true) return;
        StringBuilder buf = new StringBuilder();
        buf.append("================ Begin thread build dump\n");
        for (Iterator iter = _idTable.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry)iter.next();
            ThreadMsgId id = (ThreadMsgId)entry.getKey();
            Container c = (Container)entry.getValue();
            if (c.parent == null)
                buf.append("ROOT: ").append(c.toString());
            else {
                buf.append("NONROOT: ").append(c.toString());
                buf.append("PARENT IS: ").append(c.parent.msg).append('\n');
            }
        }
        buf.append("\n================ End thread build dump");
        _ui.debugMessage(buf.toString());
    }
    
    private List containerToRefNode(List containers) {
        List rv = new ArrayList();
        for (int i = 0; i < containers.size(); i++) {
            ThreadReferenceNode node = containerToRefNode((Container)containers.get(i), null);
            if (node != null)
                rv.add(node);
        }
        return rv;
    }
    
    private static final String SQL_POPULATE_NODE = "SELECT " +
            "msg.authorChannelId, authorchan.name, authorchan.channelHash, " +
            "msg.scopeChannelId, scopechan.name, scopechan.channelHash, " +
            "msg.targetChannelId, targetchan.name, targetchan.channelHash, " +
            "msg.subject, msg.importDate " +
            "FROM channelMessage msg " +
            "JOIN channel AS authorchan ON authorchan.channelId = msg.authorChannelId " +
            "JOIN channel AS scopechan ON scopechan.channelId = msg.scopeChannelId " +
            "JOIN channel AS targetchan ON targetchan.channelId = msg.targetChannelId " +
            "WHERE msg.msgId = ?";
    
    static void populateNode(DBClient client, ThreadReferenceNode node, ThreadMsgId tmi) {
        node.setURI(SyndieURI.createMessage(tmi.scope, tmi.messageId));
        if ( (tmi.msgId >= 0) && (!tmi.unreadable) ) {
            node.setIsDummy(false);
            
            PreparedStatement stmt = null;
            ResultSet rs = null;
            try {
                stmt = client.con().prepareStatement(SQL_POPULATE_NODE);
                stmt.setLong(1, tmi.msgId);
                rs = stmt.executeQuery();
                if (rs.next()) {
                    // "authorChannelId, authorchan.name, authorchan.channelHash, " +
                    // "scopeChannelId, scopechan.name, scopechan.channelHash, " +
                    // "targetChannelId, targetchan.name, targetchan.channelHash, " +
                    // "subject, msg.importDate "
                    long authorId = rs.getLong(1);
                    String authorName = rs.getString(2);
                    byte authorHash[] = rs.getBytes(3);
                    long scopeId = rs.getLong(4);
                    String scopeName = rs.getString(5);
                    byte scopeHash[] = rs.getBytes(6);
                    long targetId = rs.getLong(7);
                    String targetName = rs.getString(8);
                    byte targetHash[] = rs.getBytes(9);
                    String subject = rs.getString(10);
                    Timestamp when = rs.getTimestamp(11);
                    
                    node.setAuthorId(authorId);
                    node.setAuthorName(authorName);
                    node.setAuthorHash(Hash.create(authorHash));
                    node.setScopeId(scopeId);
                    node.setScopeName(scopeName);
                    node.setScopeHash(Hash.create(scopeHash));
                    node.setTargetId(targetId);
                    node.setTargetName(targetName);
                    node.setTargetHash(Hash.create(targetHash));
                    if (when != null)
                        node.setImportDate(when.getTime());
                    
                    node.setSubject(subject);
                    
                    node.setName(authorName);
                    // separate sql query
                    node.setAttachmentCount(client.getMessageAttachmentCount(tmi.msgId));
                }
            } catch (SQLException se) {
                client.logError("Internal error populating the node", se);
                throw new RuntimeException("Internal error populating the node: " + se.getMessage());
            } finally {
                if (rs != null) try { rs.close(); } catch (SQLException se) {}
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }
            /*
            long authorId = client.getMessageAuthor(tmi.msgId);
            String subject = client.getMessageSubject(tmi.msgId);
            long target = client.getMessageTarget(tmi.msgId);
            String authorName = client.getChannelName(authorId);
            int status = client.getMessageStatus(tmi.msgId);
            long scopeId = client.getChannelId(tmi.scope);
            node.setScopeId(scopeId);
            node.setAuthorId(authorId);
            node.setSubject(subject);
            node.setThreadTarget(target);
            node.setMessageStatus(status);
             */

            node.setMessageStatus(client.getMessageStatus(tmi.msgId));
            
            //List tags = new ArrayList(); node.getThreadTags(tags);
            //_ui.debugMessage("buildThread: msg: " + tmi + " authorId: " + authorId + " target: " + target + " authorName: " + authorName);// + " tags: " + tags);

            // to mirror the MessageThreadBuilder, fill the node in per:
            //
            // * each node has the author's preferred name stored in node.getName()
            // * and the message subject in node.getDescription(), with the message URI in
            // * node.getURI().
            //
            ////node.setName(authorName);
            node.setDescription(node.getThreadSubject());
        } else {
            //_ui.debugMessage("node is a dummy: " + tmi);
            node.setIsDummy(true);
        }
    }

    private ThreadReferenceNode containerToRefNode(Container container, ThreadReferenceNode parent) {
        ThreadMsgId tmi = null;
        if (container.msg != null)
            tmi = container.msg.id;
        ThreadReferenceNode node = new ThreadReferenceNode(tmi);
        
        if (tmi != null) {
            populateNode(_client, node, tmi);
        } else {
            node.setIsDummy(true);
            //_ui.debugMessage("tmi is null: " + container);
        }
        
        if (parent != null)
            parent.addChild(node);
        
        if (container.nextSibling != null) // recurses across the siblings one at a time
            containerToRefNode(container.nextSibling, parent);
        
        if (container.child != null)
            containerToRefNode(container.child, node);
        
        if (node.isDummy() && (node.getChildCount() <= 0) )
            return null;
        
        return node;
    }
    
    private boolean isLoop(ThreadMsgId ancestorId, ThreadMsgId childId) {
        if ( (ancestorId == null) || (childId == null) ) return false;
        if ( (ancestorId == childId) || (ancestorId.equals(childId)) ) return true;
        Set ids = walk(ancestorId);
        if (ids.contains(childId)) {
            if (DEBUG) _ui.debugMessage("loop detected.  ancestor=" + ancestorId + " child=" + childId + " ids=" + ids);
            return true;
        }
        //ids = walk(childId);
        //if (ids.contains(ancestorId)) 
        //    return true;
        return false;
    }
    
    private Set walk(ThreadMsgId id) {
        Set rv = new HashSet();
        Set remaining = new HashSet();
        remaining.add(id);
        while (remaining.size() > 0) {
            Iterator iter = remaining.iterator();
            ThreadMsgId cur = (ThreadMsgId)iter.next();
            iter.remove();
            rv.add(cur);
            Container c = (Container)_idTable.get(cur);
            if ( (c != null) && (c.msg != null) ) {
                for (int i = 0; i < c.msg.references.size(); i++) {
                    ThreadMsgId ancestor = (ThreadMsgId)c.msg.references.get(i);
                    if (!rv.contains(ancestor))
                        remaining.add(ancestor);
                }
            }
        }
        return rv;
    }

    /** build the thread that contains the given message */
    public ThreadReferenceNode buildThread(ThreadMsgId id) {
        Timer timer = new Timer("JWZ threader ID " + id.msgId, _ui);
        ThreadReferenceNode rv = buildThread(id, timer);
        timer.complete();
        return rv;
    }
    public ThreadReferenceNode buildThread(ThreadMsgId id, Timer timer) {
        Set msgIds = new HashSet();
        msgIds.add(id);
        // we can't just feed this to buildThread(msgIds), because that assumes
        // the leaves are all included
        Map ancestors = new HashMap();
        ThreadAccumulatorJWZ.buildAncestors(_client, _ui, id, ancestors);
        timer.addEvent("ancestors built");
        for (Iterator iter = ancestors.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry)iter.next();
            msgIds.add(entry.getKey());
            List parents = (List)entry.getValue();
            msgIds.addAll(parents);
        }
        addChildren(msgIds);
        timer.addEvent("children added");
        List threads = buildThread(msgIds, timer);
        timer.addEvent("thread built");
        if (threads.size() > 0)
            return (ThreadReferenceNode)threads.get(0);
        else
            return null;
    }
    private static final String SQL_GET_CHILDREN = "SELECT cm.msgId, messageId, channelHash, cm.wasAuthorized, cm.authorChannelId FROM channelMessage cm JOIN channel ON scopeChannelId = channelId JOIN messageHierarchy mh ON cm.msgId = mh.msgId WHERE referencedMessageId = ? AND referencedChannelHash = ?";
    private void addChildren(Set msgIds) {
        List toQuery = new ArrayList(msgIds);
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _client.con().prepareStatement(SQL_GET_CHILDREN);
            while (toQuery.size() > 0) {
                ThreadMsgId id = (ThreadMsgId)toQuery.remove(0);
                stmt.setLong(1, id.messageId);
                stmt.setBytes(2, id.scope.getData());
                rs = stmt.executeQuery();
                while (rs.next()) {
                    long msgId = rs.getLong(1);
                    if (rs.wasNull()) continue;
                    long messageId = rs.getLong(2);
                    if (rs.wasNull()) continue;
                    byte chan[] = rs.getBytes(3);
                    if ( (chan == null) && (chan.length != Hash.HASH_LENGTH) ) continue;
                    Boolean wasAuth = rs.getBoolean(4) ? Boolean.TRUE : Boolean.FALSE;
                    if (rs.wasNull()) wasAuth = null;
                    long author = rs.getLong(5);
                    if (rs.wasNull()) author = -1;
                    ThreadMsgId child = new ThreadMsgId(msgId);
                    child.messageId = messageId;
                    child.scope = Hash.create(chan);
                    child.authorized = wasAuth;
                    child.authorScopeId = author;
                    if (!msgIds.contains(child)) {
                        msgIds.add(child);
                        if (!toQuery.contains(child))
                            toQuery.add(child);
                    }
                }
                rs.close();
                rs = null;
            }
            stmt.close();
            stmt = null;
        } catch (SQLException se) {
            _ui.errorMessage("Error fetching children", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    private class Container {
        Message msg;
        Container parent;
        Container child;
        Container nextSibling;
        public String toString() { return toString(0); }
        String toString(int indent) {
            StringBuilder buf = new StringBuilder();
            for (int i = 0; i < indent; i++)
                buf.append('\t');
            if (msg != null)
                buf.append(msg.id);
            else
                buf.append("no msg");
            buf.append('\n');
        
            if (nextSibling != null)
                buf.append(nextSibling.toString(indent));
            if (child != null)
                buf.append(child.toString(indent+1));
            return buf.toString();
        }
    }
    private static class Message {
        ThreadMsgId id;
        /** list of ThreadMsgId, most recent first */
        List references;
        
        public String toString() { return (id != null ? id.toString() : "nomsg"); }
        public boolean equals(Object o) {
            if (id == null)
                return id.equals(((Message)o).id);
            else
                return super.equals(o);
        }
    }
}
