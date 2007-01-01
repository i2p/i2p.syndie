package syndie.db;

import java.util.List;
import java.util.Map;
import java.util.Set;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;

class ThreadReferenceNode extends ReferenceNode {
    private long _authorId;
    private String _subject;
    private long _targetChannelId;
    private boolean _dummy;
    private ThreadMsgId _msg;
    private long _importDate;
    public ThreadReferenceNode() { this(null); }
    public ThreadReferenceNode(ThreadMsgId id) {
        super(null, null, null, null);
        _msg = id;
        _authorId = -1;
        _subject = null;
        _targetChannelId = -1;
        _dummy = false;
        _importDate = -1;
    }
    public ThreadMsgId getMsgId() { return _msg; }
    public void setAuthorId(long authorId) { _authorId = authorId; }
    public void setSubject(String subject) { _subject = subject; }
    public String getSubject() { return _subject; }
    public void setThreadTarget(long channelId) { _targetChannelId = channelId; }
    /** this node represents something we do not have locally, or is filtered */
    public boolean isDummy() { return _dummy || getUniqueId() < 0; }
    public void setIsDummy(boolean dummy) { _dummy = dummy; }
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
    public void getThreadTags(List rv, Map msgIdToTagSet) { 
        if (_msg != null) {
            Set tags = (Set)msgIdToTagSet.get(new Long(_msg.msgId));
            if (tags != null)
                rv.addAll(tags);
        }
        for (int i = 0; i < getChildCount(); i++)
            ((ThreadReferenceNode)getChild(i)).getThreadTags(rv, msgIdToTagSet);
    }
    public long getLatestMessageId() {
        long latestMessageId = -1;
        SyndieURI uri = getURI();
        if ( !isDummy() && (uri != null) && (uri.getMessageId() != null) )
            latestMessageId = uri.getMessageId().longValue();
        for (int i = 0; i < getChildCount(); i++)
            latestMessageId = Math.max(latestMessageId, ((ThreadReferenceNode)getChild(i)).getLatestMessageId());
        return latestMessageId;
    }
    public long getLatestImportDate(DBClient client) {
        long latest = _importDate;
        if (!isDummy() && (_msg != null) && (_msg.msgId >= 0) && (latest < 0))
            latest = _importDate = client.getMessageImportDate(_msg.msgId);
        for (int i = 0; i < getChildCount(); i++)
            latest = Math.max(latest, ((ThreadReferenceNode)getChild(i)).getLatestImportDate(client));
        return latest;
    }
    public long getLatestAuthorId() { return getLatestAuthorId(getLatestMessageId()); }
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
    public long getLatestPostDate() { return getLatestMessageId(); }
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
    public long getUniqueId() { return (_msg != null ? _msg.msgId : 0-super.getUniqueId()); }
}