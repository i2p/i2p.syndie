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
    private ThemeRegistry _themes;
    private PageRendererSource() {}
    public PageRendererSource(DBClient client, ThemeRegistry themes) { 
        _client = client;
        _themes = themes;
    }
    
    public MessageInfo getMessage(long chanId, Long msgId) { return _client.getMessage(chanId, msgId); }
    public Hash getChannelHash(long authorId) { return _client.getChannelHash(authorId); }
    public long getMessageId(long scopeId, long msgId) { return _client.getMessageId(scopeId, msgId); }
    public long getChannelId(Hash scope) { return _client.getChannelId(scope); }
    /** page number starts at 1 */
    public String getMessagePageConfig(long internalMsgId, int pageNum) {
        return _client.getMessagePageConfig(internalMsgId, pageNum-1);
    }
    /** page number starts at 1 */
    public String getMessagePageData(long internalMsgId, int pageNum) {
        return _client.getMessagePageData(internalMsgId, pageNum-1);
    }
    /** attachment number starts at 1 */
    public Properties getMessageAttachmentConfig(long internalMsgId, int attachmentNum) {
        return _client.getMessageAttachmentConfig(internalMsgId, attachmentNum-1);
    }
    /** attachment number starts at 1 */
    public byte[] getMessageAttachmentData(long internalMsgId, int attachmentId) {
        return _client.getMessageAttachmentData(internalMsgId, attachmentId-1);
    }

    public Theme getTheme() { return _themes.getTheme(); }
}
