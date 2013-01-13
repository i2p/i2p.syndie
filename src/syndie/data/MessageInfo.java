package syndie.data;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Set;
import net.i2p.data.Hash;

/**
 *
 */
public class MessageInfo {
    private long _internalId;
    private SyndieURI _uri;
    private long _authorChannelId;
    private long _messageId;
    private long _scopeChannelId;
    private long _targetChannelId;
    private Hash _targetChannel;
    private String _subject;
    private Hash _overwriteChannel;
    private long _overwriteMessage;
    private boolean _forceNewThread;
    private boolean _refuseReplies;
    private boolean _wasEncrypted;
    private boolean _wasPBEncrypted;
    private boolean _wasPrivate;
    private boolean _wasAuthorized;
    private boolean _wasAuthenticated;
    /** prompt is only listed if the message could not be decrypted */
    private String _passphrasePrompt;
    /** readKeyUnknown is only set if the message could not be decrypted and no prompt was specified */
    private boolean _readKeyUnknown;
    private boolean _replyKeyUnknown;
    private boolean _isCancelled;
    private long _expiration;
    private long _receiveDate;
    /** list of SyndieURI instances this message replies to, most recent first */
    private List _hierarchy;
    /** set of tags (String) that are hidden in the message */
    private Set _privateTags;
    /** set of tags (String) that are publicly visible */
    private Set _publicTags;
    private int _attachmentCount;
    private int _pageCount;
    /** list of ReferenceNode roots attached to the message (does not include parsed data from pages or attachments) */
    private List _references;
    
    /** Creates a new instance of MessageInfo */
    public MessageInfo() {
        _internalId = -1;
        _authorChannelId = -1;
        _messageId = -1;
        _scopeChannelId = -1;
        _targetChannelId = -1;
        _overwriteMessage = -1;
        _expiration = -1;
        _receiveDate = -1;
        _hierarchy = Collections.EMPTY_LIST;
        _privateTags = Collections.EMPTY_SET;
        _publicTags = Collections.EMPTY_SET;
        _references = Collections.EMPTY_LIST;
    }

    public long getInternalId() { return _internalId; }
    public void setInternalId(long internalId) { _internalId = internalId; }
    public SyndieURI getURI() { return _uri; }
    public void setURI(SyndieURI uri) { _uri = uri; }
    public long getAuthorChannelId() { return _authorChannelId; }
    public void setAuthorChannelId(long id) { _authorChannelId = id; }
    public long getMessageId() { return _messageId; }
    public void setMessageId(long messageId) { _messageId = messageId; }
    /** channel that the messageId is unique within */
    public long getScopeChannelId() { return _scopeChannelId; }
    public void setScopeChannelId(long scopeChannelId) { _scopeChannelId = scopeChannelId; }
    public Hash getScopeChannel() { return _uri.getScope(); }
    public long getTargetChannelId() { return _targetChannelId; }
    public void setTargetChannelId(long targetChannelId) { _targetChannelId = targetChannelId; }
    public Hash getTargetChannel() { return _targetChannel; }
    public void setTargetChannel(Hash targetChannel) { _targetChannel = targetChannel; }
    public String getSubject() { return _subject; }
    public void setSubject(String subject) { _subject = subject; }
    public Hash getOverwriteChannel() { return _overwriteChannel; }
    public void setOverwriteChannel(Hash overwriteChannel) { _overwriteChannel = overwriteChannel; }
    public long getOverwriteMessage() { return _overwriteMessage; }
    public void setOverwriteMessage(long overwriteMessage) { _overwriteMessage = overwriteMessage; }
    public boolean getForceNewThread() { return _forceNewThread; }
    public void setForceNewThread(boolean forceNewThread) { _forceNewThread = forceNewThread; }
    public boolean getRefuseReplies() { return _refuseReplies; }
    public void setRefuseReplies(boolean refuseReplies) { _refuseReplies = refuseReplies; }
    /**
     * was this post normally encrypted (true) or was the body encryption key 
     * publicized (false) - effectively making it unencrypted
     */
    public boolean getWasEncrypted() { return _wasEncrypted; }
    public void setWasEncrypted(boolean wasEncrypted) { _wasEncrypted = wasEncrypted; }

    public boolean getWasPassphraseProtected() { return _wasPBEncrypted; }
    public void setWasPassphraseProtected(boolean pbe) { _wasPBEncrypted = pbe; }
    /**
     * was this post encrypted to the channel's reply encryption key (true), as opposed to
     * a normal post on the channel encrypted with the channel read key (false)
     */
    public boolean getWasPrivate() { return _wasPrivate; }
    public void setWasPrivate(boolean wasPrivate) { _wasPrivate = wasPrivate; }
    /** was the post signed by an authorized key */
    public boolean getWasAuthorized() { return _wasAuthorized; }
    public void setWasAuthorized(boolean wasAuthorized) { _wasAuthorized = wasAuthorized; }
    /** was the post's author specified (or implied) and did they authenticate that identity */
    public boolean getWasAuthenticated() { return _wasAuthenticated;}
    public void setWasAuthenticated(boolean wasAuthenticated) { _wasAuthenticated = wasAuthenticated; }
    /** has the post been cancelled by an authorized person (the original author or managers on the channel it was posted to) */
    public boolean getIsCancelled() { return _isCancelled; }
    public void setIsCancelled(boolean isCancelled) { _isCancelled = isCancelled; }
    /** when the post should be discarded (or -1 if never) */
    public long getExpiration() { return _expiration; }
    public void setExpiration(long expiration) { _expiration = expiration; }
    public long getReceiveDate() { return _receiveDate; }
    public void setReceiveDate(long when) { _receiveDate = when; }
    /** list of SyndieURI instances this message replies to, most recent first */
    public List getHierarchy() { return _hierarchy; }
    public void setHierarchy(List hierarchy) { _hierarchy = hierarchy; }
    /** set of tags (String) */
    public Set getPrivateTags() { return _privateTags; }
    public void setPrivateTags(Set privateTags) { _privateTags = privateTags; }
    /** set of tags (String) */
    public Set getPublicTags() { return _publicTags; }
    public void setPublicTags(Set publicTags) { _publicTags = publicTags; }
    public int getAttachmentCount() { return _attachmentCount; }
    public void setAttachmentCount(int attachmentCount) { _attachmentCount = attachmentCount; }
    public int getPageCount() { return _pageCount; }
    public void setPageCount(int pageCount) { _pageCount = pageCount; }
    /** list of ReferenceNode roots attached to the message (does not include parsed data from pages or attachments) */
    public List getReferences() { return _references; }
    public void setReferences(List refs) { _references = refs; }

    /** if specified, the post was imported, but we didn't have the passphrase */
    public String getPassphrasePrompt() { return _passphrasePrompt; }
    public void setPassphrasePrompt(String prompt) { _passphrasePrompt = prompt; }

    /** readKeyUnknown is only set if the message could not be decrypted and no prompt was specified */
    public boolean getReadKeyUnknown() { return _readKeyUnknown; }
    public void setReadKeyUnknown(boolean isUnknown) { _readKeyUnknown = isUnknown; }

    public boolean getReplyKeyUnknown() { return _replyKeyUnknown; }
    public void setReplyKeyUnknown(boolean isUnknown) { _replyKeyUnknown = isUnknown; }
    
    public boolean equals(Object o) { return (o instanceof MessageInfo) ? ((MessageInfo)o)._internalId == _internalId : false; }
    public int hashCode() { return (int)_internalId; }
    public String toString() {
        StringBuilder buf = new StringBuilder();
        buf.append("Message ").append(_internalId).append(":\n");
        buf.append("Posted on ");
        if (_targetChannel != null)
            buf.append(_targetChannel.toBase64()).append(" ");
        buf.append("(internal channel id: ").append(_targetChannelId).append(")\n");
        buf.append("Channel messageId: ").append(_messageId).append("\n");
        if ( (_overwriteChannel != null) && (_overwriteMessage >= 0) )
            buf.append("Overwriting ").append(_overwriteChannel.toBase64()).append(":").append(_overwriteMessage).append("\n");
        if (_authorChannelId >= 0)
            buf.append("Author: ").append(_authorChannelId).append("\n");
        if (_subject != null)
            buf.append("Subject: ").append(_subject).append("\n");
        buf.append("Force this message onto a new thread? ").append(_forceNewThread).append("\n");
        buf.append("Force replies to use their own thread? ").append(_refuseReplies).append("\n");
        buf.append("Was the post readable to anyone? ").append(!_wasEncrypted && !_wasPBEncrypted).append("\n");
        buf.append("Was the post passphrase protected? ").append(_wasPBEncrypted).append("\n");
        buf.append("Was the message encrypted to the channel's reply key? ").append(_wasPrivate).append("\n");
        buf.append("Was the message signed by an authorized user? ").append(_wasAuthorized).append("\n");
        buf.append("Was the author specified and authenticated? ").append(_wasAuthenticated).append("\n");
        buf.append("Was the message (subsequently) cancelled by an authorized user? ").append(_isCancelled).append("\n");
        if (_expiration <= 0)
            buf.append("Message expiration: never\n");
        else
            buf.append("Message expiration: ").append(new Date(_expiration)).append("\n");
        if ( (_hierarchy != null) && (_hierarchy.size() > 0) ) {
            buf.append("This message replies to: ");
            for (int i = 0; i < _hierarchy.size(); i++) {
                SyndieURI uri = (SyndieURI)_hierarchy.get(i);
                buf.append(uri.toString());
                if (i + 1 < _hierarchy.size())
                    buf.append(", ");
                else
                    buf.append("\n");
            }
        }
        if ( (_publicTags != null) && (_publicTags.size() > 0) )
            buf.append("Publicly visible tags on the message: ").append(_publicTags).append("\n");
        if ( (_privateTags != null) && (_privateTags.size() > 0) )
            buf.append("Hidden tags on the message: ").append(_privateTags).append("\n");
        buf.append("Pages in the message: ").append(_pageCount).append("\n");
        buf.append("Attachments in the message: ").append(_attachmentCount).append("\n");
        buf.append("References in the message: ").append(_references.size()).append("\n");
        return buf.toString();
    }
}
