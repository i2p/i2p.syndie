package syndie.gui;

import java.util.*;
import net.i2p.data.Hash;
import syndie.Constants;
import syndie.data.MessageInfo;
import syndie.db.CommandImpl;
import syndie.db.DBClient;

/**
 * dummy page source so we can view a particular message that isn't in the
 * database.  however, this falls back on the database when referring to
 * elements not in the given message
 */
public class PageRendererSourceMem extends PageRendererSource {
    /** contents of each page (as String) */
    private List _pageData;
    /** map of attachment name (String) to attachment data (byte[]) */
    private Map _attachments;
    /** ordered list of attachment names */
    private List _attachmentOrder;
    /** contains the message's general config data */
    private MessageInfo _msg;
    
    public PageRendererSourceMem(DBClient client, MessageInfo msg, List pageData, Map attachments, List attachmentOrder) {
        super(client);
        _msg = msg;
        _pageData = pageData;
        _attachments = attachments;
        _attachmentOrder = attachmentOrder;
    }
    public MessageInfo getMessage(long chanId, Long msgId) {
        if ( (_msg.getScopeChannelId() == chanId) && (msgId != null) && (msgId.longValue() == _msg.getMessageId()) )
            return _msg;
        else
            return super.getMessage(chanId, msgId);
    }
    public Hash getChannelHash(long authorId) { 
        if (authorId == _msg.getTargetChannelId())
            return _msg.getTargetChannel();
        else if (authorId == _msg.getScopeChannelId())
            return _msg.getScopeChannel();
        else
            return super.getChannelHash(authorId);
    }
    public long getMessageId(long scopeId, long msgId) {
        if ( (_msg.getScopeChannelId() == scopeId) && (_msg.getMessageId() == msgId) )
            return _msg.getInternalId();
        return super.getMessageId(scopeId, msgId);
    }
    public long getChannelId(Hash scope) {
        if (_msg.getTargetChannel().equals(scope))
            return _msg.getTargetChannelId();
        else if (_msg.getScopeChannel().equals(scope))
            return _msg.getScopeChannelId();
        else
            return super.getChannelId(scope);
    }
    public String getMessagePageConfig(long internalMsgId, int pageNum) {
        if (_msg.getInternalId() == internalMsgId)
            return PAGE_CONFIG;
        else
            return super.getMessagePageConfig(internalMsgId, pageNum);
    }
    public String getMessagePageData(long internalMsgId, int pageNum) {
        if (_msg.getInternalId() == internalMsgId)
            return (String)_pageData.get(pageNum);
        else
            return super.getMessagePageData(internalMsgId, pageNum);
    }
    public Properties getMessageAttachmentConfig(long internalMsgId, int attachmentNum) {
        if (_msg.getInternalId() != internalMsgId)
            return super.getMessageAttachmentConfig(internalMsgId, attachmentNum);
        Properties rv = new Properties();
        String name = (String)_attachmentOrder.get(attachmentNum);
        rv.setProperty(Constants.MSG_ATTACH_CONTENT_TYPE, "application/octet-stream");
        rv.setProperty(Constants.MSG_ATTACH_DESCRIPTION, CommandImpl.strip(name));
        rv.setProperty(Constants.MSG_ATTACH_NAME, CommandImpl.strip(name));
        return rv;
    }
    public byte[] getMessageAttachmentData(long internalMsgId, int attachmentNum) {
        if (_msg.getInternalId() != internalMsgId)
            return super.getMessageAttachmentData(internalMsgId, attachmentNum);
        return (byte[])_attachments.get((String)_attachmentOrder.get(attachmentNum));
    }
    
    /** just treat all pages as html */
    private static final String PAGE_CONFIG = Constants.MSG_PAGE_CONTENT_TYPE + "=text/html\n";
}
