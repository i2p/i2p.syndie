package syndie.gui.desktop;

import java.io.File;
import java.util.List;
import java.util.TreeMap;
import net.i2p.data.Hash;
import org.eclipse.swt.custom.CTabFolder;
import syndie.data.NymReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;
import syndie.gui.Browser;
import syndie.gui.BrowserControl;
import syndie.gui.MessageEditor;
import syndie.gui.ThemeRegistry;
import syndie.gui.TranslationRegistry;

abstract class DesktopBrowserControl implements BrowserControl {
    private Desktop _desktop;
    
    public DesktopBrowserControl(Desktop desktop) { _desktop = desktop; }

    /*
    public void view(SyndieURI uri) { view(uri, null, null); }
    public void view(SyndieURI uri, String suggestedName, String suggestedDescription) {
        _desktop.view(uri, suggestedName, suggestedDescription);
    }

    public void unview(SyndieURI uri) { _desktop.unview(uri); }
    
    public void bookmark(SyndieURI uri) {}
    public void bookmark(SyndieURI uri, long parentGroupId) {}
    public void bookmark(NymReferenceNode node, boolean doneBookmarking) {}
    public void deleteBookmark(long bookmarkGroupId) {}
    public void deleteBookmarks(List bookmarkGroupIds) {}
    public void updateBookmark(NymReferenceNode bookmark) {}
    public void bookmarkCurrentTab() {}
    public List getBookmarks() {}
    
    public List getPrivateMsgIds(boolean alreadyRead) { return new ArrayList(); }
    
    public boolean ban(Hash scope) { return false; }
    
    public SyndieURI createPostURI(Hash forum, SyndieURI parent) {
    }

    public SyndieURI createPostURI(Hash forum, SyndieURI parent, boolean asPrivateReply) {
    }

    public SyndieURI createPostURI(Hash forum, SyndieURI parent, boolean asPrivateReply, List references, File[] attachments) {
    }

    public SyndieURI createPostURI(Hash forum, SyndieURI parent, String pbePass, String pbePrompt, List references, File[] attachments) {
    }

    public SyndieURI createPostURI(long postponeId, int postponeVersion) {
    }

    public SyndieURI createTextUIURI() {
    }

    public SyndieURI createLogsURI() {
    }

    public SyndieURI createSQLURI() {
    }

    public SyndieURI createManageURI(Hash forum) {
    }

    public SyndieURI createMetaURI(Hash forum) {
    }

    public SyndieURI createMetaRefsURI(Hash forum) {
    }

    public SyndieURI createMetaArchivesURI(Hash forum) {
    }

    public SyndieURI createMetaPostersURI(Hash forum) {
    }

    public SyndieURI createMetaManagersURI(Hash forum) {
    }

    public SyndieURI createSyndicationArchiveURI() {
    }

    public SyndieURI createSyndicationConfigURI() {
    }

    public SyndieURI createSyndicationDiffURI() {
    }

    public SyndieURI createSyndicationStatusURI() {
    }

    public SyndieURI createBugReportURI() {
    }

    public SyndieURI createHighlightWatchedURI(boolean threaded, boolean unreadOnly, boolean useImportDate) {
    }

    public void showWaitCursor(boolean wait) {
    }

    public UI getUI() {
    }

    public TranslationRegistry getTranslationRegistry() { return _desktop.
    }

    public ThemeRegistry getThemeRegistry() {
    }

    public DBClient getClient() { return _desktop.getDBClient(); }
    
    public CTabFolder getTabFolder() {
    }

    public void addUIListener(Browser.UIListener lsnr) {
    }

    public void removeUIListener(Browser.UIListener lsnr) {
    }

    public MessageEditor.MessageEditorListener getMessageEditorListener() {
    }

    public void addMessageEditorListener(MessageEditor.MessageEditorListener lsnr) {
    }

    public void removeMessageEditorListener(MessageEditor.MessageEditorListener lsnr) {
    }

    public boolean isBookmarked(SyndieURI syndieURI) {
    }

    public TreeMap getResumeable() {
    }

    public void resumePost(long postponeId, int postponeVersion) {
    }

    public boolean reimport(SyndieURI uri, String passphrase) {
    }

    public void messageImported() {
    }

    public void metaImported() {
    }

    public void readStatusUpdated() {
    }

    public void forumCreated() {
    }
     */
}
