package syndie.thread;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.i2p.data.Hash;

import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

public class ThreadReferenceNode extends ReferenceNode {
    private long _scopeId;
    private long _authorId;
    private String _subject;
    private long _targetChannelId;
    private boolean _dummy;
    private final ThreadMsgId _msg;
    private long _importDate;
    private Set<String> _tags;
    private int _messageStatus;
    private String _authorName;
    private Hash _authorHash;
    private String _scopeName;
    private Hash _scopeHash;
    private long _targetId;
    private String _targetName;
    private Hash _targetHash;
    private int _attachmentCount;
    
    public ThreadReferenceNode() { this(null); }
    public ThreadReferenceNode(ThreadMsgId id) {
        super(null, null, null, null);
        _msg = id;
        _authorId = -1;
        _targetChannelId = -1;
        _importDate = -1;
        _messageStatus = -1;
        _scopeId = -1;
    }
    public ThreadMsgId getMsgId() { return _msg; }
    public void setAuthorId(long authorId) { _authorId = authorId; }
    public void setSubject(String subject) { _subject = subject; }
    public String getSubject() { return _subject; }
    public Set<String> getTags() { return _tags; }
    public void setThreadTarget(long channelId) { _targetChannelId = channelId; }
    /** this node represents something we do not have locally, or is filtered */
    public boolean isDummy() { return _dummy || getUniqueId() < 0; }
    public void setIsDummy(boolean dummy) { _dummy = dummy; }
    public int getMessageStatus() { return _messageStatus; }
    public void setMessageStatus(int status) { _messageStatus = status; }
    public long getImportDate() { return _importDate; }
    public void setScopeId(long id) { _scopeId = id; }
    public long getScopeId() { return _scopeId; }
    
    public void setAuthorName(String name) { _authorName = name; }
    public void setAuthorHash(Hash scope) { _authorHash = scope; }
    public void setScopeName(String name) { _scopeName = name; }
    public void setScopeHash(Hash scope) { _scopeHash = scope; }
    public void setTargetId(long id) { _targetChannelId = id; }
    public void setTargetName(String name) { _targetName = name; }
    public void setTargetHash(Hash scope) { _targetHash = scope; }
    public void setImportDate(long when) { _importDate = when; }
    
    public String getAuthorName() { return _authorName; }
    public Hash getAuthorHash() { return _authorHash; }
    public String getScopeName() { return _scopeName; }
    public Hash getScopeHash() { return _scopeHash; }
    public long getTargetId() { return _targetChannelId; }
    public String getTargetName() { return _targetName; }
    public Hash getTargetHash() { return _targetHash; }

    /** @since 1.102b-11 */
    public void setAttachmentCount(int cnt) { _attachmentCount = cnt; }
    /** @since 1.102b-11 */
    public int getAttachmentCount() { return _attachmentCount; }
    
    public long getThreadTarget() {
        if (_targetChannelId >= 0)
            return _targetChannelId;
        for (int i = 0; i < getChildCount(); i++) {
            long id = ((ThreadReferenceNode)getChild(i)).getThreadTarget();
            if (id >= 0)
                return id;
        }
        return -1;
    }

    void getThreadTags(List<String> rv, Map<Long, Set<String>> msgIdToTagSet) { 
        if (_msg != null) {
            _tags = msgIdToTagSet.get(Long.valueOf(_msg.msgId));
            if (_tags != null)
                rv.addAll(_tags);
        }
        for (int i = 0; i < getChildCount(); i++)
            ((ThreadReferenceNode)getChild(i)).getThreadTags(rv, msgIdToTagSet);
    }

    long getLatestMessageId() {
        long latestMessageId = -1;
        SyndieURI uri = getURI();
        if ( !isDummy() && (uri != null) && (uri.getMessageId() != null) )
            latestMessageId = uri.getMessageId().longValue();
        for (int i = 0; i < getChildCount(); i++)
            latestMessageId = Math.max(latestMessageId, ((ThreadReferenceNode)getChild(i)).getLatestMessageId());
        return latestMessageId;
    }

    long getLatestImportDate(DBClient client) {
        long latest = _importDate;
        if (!isDummy() && (_msg != null) && (_msg.msgId >= 0) && (latest < 0))
            latest = _importDate = client.getMessageImportDate(_msg.msgId);
        for (int i = 0; i < getChildCount(); i++)
            latest = Math.max(latest, ((ThreadReferenceNode)getChild(i)).getLatestImportDate(client));
        return latest;
    }
    long getLatestAuthorId() { return getLatestAuthorId(getLatestMessageId()); }
    private long getLatestAuthorId(long latestMessageId) {
        SyndieURI uri = getURI();
        if ( !isDummy() && (uri != null) && (uri.getMessageId() != null) )
            if (latestMessageId == uri.getMessageId().longValue())
                return getAuthorId();
        for (int i = 0; i < getChildCount(); i++) {
            long authorId = ((ThreadReferenceNode)getChild(i)).getLatestAuthorId(latestMessageId);
            if (authorId >= 0)
                return authorId;
        }
        return -1;
    }
    long getLatestPostDate() { return getLatestMessageId(); }
    /** count of actual messages, not including any dummy nodes */
    public int getMessageCount() {
        int rv = isDummy() ? 1 : 0;
        for (int i = 0; i < getChildCount(); i++)
            rv += ((ThreadReferenceNode)getChild(i)).getMessageCount();
        return rv;
    }
    public long getAuthorId() { return isDummy() ? -1 : _authorId; }
    public String getThreadSubject() {
        if ( !isDummy() && (_subject != null) && (_subject.length() > 0) )
            return _subject;
        for (int i = 0; i < getChildCount(); i++) {
            String subject = ((ThreadReferenceNode)getChild(i)).getThreadSubject();
            if ( (subject != null) && (subject.length() > 0) )
                return subject;
        }
        return "";
    }

    public void setChildren(ThreadReferenceNode children[]) {
        clearChildren();
        if (children != null)
            for (int i = 0; i < children.length; i++)
                addChild(children[i]);
    }

    public ThreadReferenceNode[] getChildren() {
        ThreadReferenceNode rv[] = new ThreadReferenceNode[getChildCount()];
        for (int i = 0; i < rv.length; i++)
            rv[i] = (ThreadReferenceNode)getChild(i);
        return rv;
    }

    public void clearParent() { _parent = null; }
    public long getUniqueId() { return (_msg != null ? _msg.msgId : 0-super.getUniqueId()); }
    
    public static ArrayList<ThreadReferenceNode> deepThreadCopy(List<ThreadReferenceNode> orig) {
        if (orig == null) return new ArrayList();
        ArrayList<ThreadReferenceNode> rv = new ArrayList<ThreadReferenceNode>(orig.size());
        for (int i = 0; i < orig.size(); i++) {
            ThreadReferenceNode node = orig.get(i);
            rv.add(deepThreadCopy(node));
        }
        return rv;
    }

    public static ThreadReferenceNode deepThreadCopy(ThreadReferenceNode node) {
        if (node == null) return null;
        ThreadReferenceNode copy = new ThreadReferenceNode(node._msg);
        copy._name = node._name;
        copy._uri = node._uri;
        copy._description = node._description;
        copy._refType = node._refType;
        
        copy._authorHash = node._authorHash;
        copy._authorId = node._authorId;
        copy._authorName = node._authorName;
        copy._dummy = node._dummy;
        copy._importDate = node._importDate;
        copy._messageStatus = node._messageStatus;
        copy._scopeHash = node._scopeHash;
        copy._scopeId = node._scopeId;
        copy._scopeName = node._scopeName;
        copy._subject = node._subject;
        copy._tags = node._tags != null ? new HashSet(node._tags) : null;
        copy._targetChannelId = node._targetChannelId;
        copy._targetHash = node._targetHash;
        copy._targetId = node._targetId;
        copy._targetName = node._targetName;
        copy._treeIndex = node._treeIndex;
        for (int i = 0; i < node.getChildCount(); i++)
            copy.addChild(deepThreadCopy((ThreadReferenceNode)node.getChild(i)));
        return copy;
    }
}
