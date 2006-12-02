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
    /** ordered attachment data (byte[]) */
    private List _attachments;
    /** ordered list of attachment names */
    private List _attachmentOrder;
    /** contains the message's general config data */
    private MessageInfo _msg;
    
    public PageRendererSourceMem(BrowserControl browser, DBClient client, MessageInfo msg, List pageData, List attachments, List attachmentOrder) {
        super(browser, client);
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
            return (String)_pageData.get(pageNum-1);
        else
            return super.getMessagePageData(internalMsgId, pageNum);
    }
    public Properties getMessageAttachmentConfig(long internalMsgId, int attachmentNum) {
        if (_msg.getInternalId() != internalMsgId)
            return super.getMessageAttachmentConfig(internalMsgId, attachmentNum);
        Properties rv = new Properties();
        if ( (attachmentNum <= 0) || (attachmentNum > _attachmentOrder.size()) ) return null;
        String name = (String)_attachmentOrder.get(attachmentNum-1);
        rv.setProperty(Constants.MSG_ATTACH_CONTENT_TYPE, "application/octet-stream");
        rv.setProperty(Constants.MSG_ATTACH_DESCRIPTION, CommandImpl.strip(name));
        rv.setProperty(Constants.MSG_ATTACH_NAME, CommandImpl.strip(name));
        return rv;
    }
    public byte[] getMessageAttachmentData(long internalMsgId, int attachmentNum) {
        if (_msg.getInternalId() != internalMsgId) {
            System.out.println("not the current message... fetch other attachment");
            return super.getMessageAttachmentData(internalMsgId, attachmentNum);
        }
        if ( (attachmentNum <= 0) || (attachmentNum > _attachmentOrder.size()) ) return null;
        String name = (String)_attachmentOrder.get(attachmentNum-1);
        byte data[] = (byte[])_attachments.get(attachmentNum-1);
        System.out.println("current message... attachment count " + _attachmentOrder.size() + " (" + name + "): " + (data != null ? data.length+"" : "null"));
        return data;
    }
    
    /** just treat all pages as html */
    private static final String PAGE_CONFIG = Constants.MSG_PAGE_CONTENT_TYPE + "=text/html\n";
}
