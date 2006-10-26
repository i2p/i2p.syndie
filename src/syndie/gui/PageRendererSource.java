package syndie.gui;

import java.util.Properties;
import net.i2p.data.Hash;
import syndie.data.MessageInfo;
import syndie.db.DBClient;

/**
 * wrapper to limit the page rendering dependencies on the dbclient,
 * exposing only the functionality that the renderer needs (so as to
 * allow simple and safe subclassing in the PageRendererSourceMem,
 * for instance)
 */
public class PageRendererSource {
    private DBClient _client;
    protected PageRendererSource() {}
    public PageRendererSource(DBClient client) { _client = client; }
    
    public MessageInfo getMessage(long chanId, Long msgId) { return _client.getMessage(chanId, msgId); }
    public Hash getChannelHash(long authorId) { return _client.getChannelHash(authorId); }
    public long getMessageId(long scopeId, long msgId) { return _client.getMessageId(scopeId, msgId); }
    public long getChannelId(Hash scope) { return _client.getChannelId(scope); }
    public String getMessagePageConfig(long internalMsgId, int pageNum) {
        return _client.getMessagePageConfig(internalMsgId, pageNum);
    }
    public String getMessagePageData(long internalMsgId, int pageNum) {
        return _client.getMessagePageData(internalMsgId, pageNum);
    }
    public Properties getMessageAttachmentConfig(long internalMsgId, int attachmentNum) {
        return _client.getMessageAttachmentConfig(internalMsgId, attachmentNum);
    }
    public byte[] getMessageAttachmentData(long internalMsgId, int attachmentId) {
        return _client.getMessageAttachmentData(internalMsgId, attachmentId);
    }
}
