package syndie.gui;

import java.io.File;
import java.util.List;
import java.util.TreeMap;
import net.i2p.data.Hash;
import org.eclipse.swt.custom.CTabFolder;
import syndie.data.NymReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 *
 */
public class DummyBrowserControl implements BrowserControl {
    private DBClient _client;
    private UI _ui;
    
    public DummyBrowserControl(DBClient client, UI ui) {
        _client = client;
        _ui = ui;
    }

    public UI getUI() { return _ui; }
    public DBClient getClient() { return _client; }
    
    public void view(SyndieURI uri) {}
    public void unview(SyndieURI uri) {}
    public SyndieURI createPostURI(Hash forum, SyndieURI parent) { return null; }
    public SyndieURI createPostURI(Hash forum, SyndieURI parent, boolean asPrivateReply) { return null; }
    public SyndieURI createPostURI(Hash forum, SyndieURI parent, boolean asPrivateReply, List references, File attachments[]) { return null; }
    public SyndieURI createPostURI(Hash forum, SyndieURI parent, String pbePass, String pbePrompt, List references, File attachments[]) { return null; }
    public SyndieURI createPostURI(long postponeId, int postponeVersion) { return null; }
    public SyndieURI createTextUIURI() { return null; }
    public SyndieURI createLogsURI() { return null; }
    public SyndieURI createManageURI(Hash forum) { return null; }
    public CTabFolder getTabFolder() { return null; }
    public void addUIListener(Browser.UIListener lsnr) {}
    public void removeUIListener(Browser.UIListener lsnr) {}

    public void bookmark(SyndieURI uri) {}
    public void bookmark(SyndieURI uri, long parentGroupId) {}
    public void deleteBookmark(long bookmarkGroupId) {}
    public void deleteBookmarks(List bookmarkGroupIds) {}
    public void updateBookmark(NymReferenceNode bookmark) {}
    public void bookmark(NymReferenceNode node, boolean done) {}
    public void bookmarkCurrentTab() {}
    public List getBookmarks() { return null; }

    public List getPrivateMsgIds(boolean alreadyRead) { return null; }
    
    public void showWaitCursor(boolean show) {}
    public TranslationRegistry getTranslationRegistry() { return null; }

    public SyndieURI createSyndicationArchiveURI() { return null; }
    public SyndieURI createSyndicationConfigURI() { return null; }
    public SyndieURI createSyndicationDiffURI() { return null; }
    public SyndieURI createSyndicationStatusURI() { return null; }
    
    public SyndieURI createMetaURI(Hash forum) { return null; }
    public SyndieURI createMetaRefsURI(Hash forum) { return createMetaURI(forum); }
    public SyndieURI createMetaArchivesURI(Hash forum) { return createMetaURI(forum); }
    public SyndieURI createMetaPostersURI(Hash forum) { return createMetaURI(forum); }
    public SyndieURI createMetaManagersURI(Hash forum) { return createMetaURI(forum); }
    public SyndieURI createBugReportURI() { return null; }

    public SyndieURI createSQLURI() { return null; }
    public ThemeRegistry getThemeRegistry() { return null; } 

    public MessageEditor.MessageEditorListener getMessageEditorListener() { return null; }
    public void addMessageEditorListener(MessageEditor.MessageEditorListener lsnr) {}
    public void removeMessageEditorListener(MessageEditor.MessageEditorListener lsnr) {}

    public boolean isBookmarked(SyndieURI syndieURI) { return false; }

    public TreeMap getResumeable() { return null; }
    public void resumePost(long postponeId, int postponeVersion) {}

    public boolean reimport(SyndieURI uri, String passphrase) { return false; }
    
    public boolean ban(Hash scope) { return false; }

    public SyndieURI createHighlightWatchedURI(boolean threaded, boolean unreadOnly, boolean useImportDate) { return null; }
    public void messageImported() {}
    public void metaImported() {}
    public void readStatusUpdated() {}
    public void forumCreated() {}
}