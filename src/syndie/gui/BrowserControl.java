package syndie.gui;

import java.util.List;
import java.util.TreeMap;
import syndie.data.NymReferenceNode;
import syndie.db.DBClient;
import syndie.db.SyndicationManager;
import syndie.db.UI;
import syndie.data.SyndieURI;
import net.i2p.data.Hash;
import org.eclipse.swt.custom.CTabFolder;

/** enable browser components to tell the browser to do things */
public interface BrowserControl {
    public void view(SyndieURI uri);
    public void unview(SyndieURI uri);
    /** show a popup to bookmark the given uri in the user's set of bookmarked references */
    public void bookmark(SyndieURI uri);
    /** just add the given bookmark.  the node's groupId, siblingOrder, and uriId will be populated */
    public void bookmark(NymReferenceNode node);
    public void deleteBookmark(long bookmarkGroupId);
    public void updateBookmark(NymReferenceNode bookmark);
    /** get the bookmarks (NymReferenceNode) currently loaded */
    public List getBookmarks();
    

    public SyndieURI createPostURI(Hash forum, SyndieURI parent);
    public SyndieURI createPostURI(Hash forum, SyndieURI parent, boolean asPrivateReply);
    public SyndieURI createTextUIURI();
    public SyndieURI createLogsURI();
    public SyndieURI createSQLURI();
    /** manage the forum */
    public SyndieURI createManageURI(Hash forum);
    /** view the details of the forum, not the messages */
    public SyndieURI createMetaURI(Hash forum);
    /** view the details, but zoom in on the published references */
    public SyndieURI createMetaRefsURI(Hash forum);
    /** view the details, but zoom in on the published archives */
    public SyndieURI createMetaArchivesURI(Hash forum);
    /** view the details, but zoom in on the published authorized posters */
    public SyndieURI createMetaPostersURI(Hash forum);
    /** view the details, but zoom in on the published authorized managers */
    public SyndieURI createMetaManagersURI(Hash forum);
    
    public SyndieURI createSyndicationArchiveURI();
    public SyndieURI createSyndicationConfigURI();
    public SyndieURI createSyndicationDiffURI();
    public SyndieURI createSyndicationStatusURI();

    public void showWaitCursor(boolean wait);
    
    public UI getUI();
    public TranslationRegistry getTranslationRegistry();
    public ThemeRegistry getThemeRegistry();
    public DBClient getClient();

    public CTabFolder getTabFolder();
    
    public SyndicationManager getSyndicationManager();

    public void addUIListener(Browser.UIListener lsnr);
    public void removeUIListener(Browser.UIListener lsnr);
    
    public MessageEditor.MessageEditorListener getMessageEditorListener();

    public boolean isBookmarked(SyndieURI syndieURI);
    
    /**
     * ordered map of postponeId (Long) to the most recent version (Integer),
     * with the most recent messages first 
     */
    public TreeMap getResumeable();
    public void resumePost(long postponeId, int postponeVersion);
}
