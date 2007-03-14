package syndie.gui;

import java.util.List;
import syndie.data.NymReferenceNode;
import syndie.data.SyndieURI;

public interface BookmarkControl {
    /** show a popup to bookmark the given uri in the user's set of bookmarked references */
    public void bookmark(SyndieURI uri);
    /** show a popup to bookmark the given uri in the user's set of bookmarked references */
    public void bookmark(SyndieURI uri, long parentGroupId);
    /** just add the given bookmark.  the node's groupId, siblingOrder, and uriId will be populated */
    public void bookmark(NymReferenceNode node, boolean doneBookmarking);
    public void deleteBookmark(long bookmarkGroupId);
    public void deleteBookmarks(List bookmarkGroupIds);
    public void updateBookmark(NymReferenceNode bookmark);
    public void bookmarkCurrentTab();
    /** get the bookmarks (NymReferenceNode) currently loaded */
    public List getBookmarks();

    public boolean isBookmarked(SyndieURI syndieURI);    
}
