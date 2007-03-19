package syndie.gui;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import net.i2p.data.Hash;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.ThreadAccumulator;
import syndie.db.ThreadAccumulatorJWZ;
import syndie.db.ThreadReferenceNode;
import syndie.db.UI;

/**
 * message tree that organizes threads first by forum, then by thread
 */
public class WatchedMessageTree extends MessageTree {
    private boolean _multiforum;
    
    public WatchedMessageTree(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl navControl, URIControl uriControl, BookmarkControl bookmarkControl, DataCallback dataCallback, Composite parent, MessageTreeListener lsnr) { this(client, ui, themes, trans, navControl, uriControl, bookmarkControl, dataCallback, parent, lsnr, false); }
    public WatchedMessageTree(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl navControl, URIControl uriControl, BookmarkControl bookmarkControl, DataCallback dataCallback, Composite parent, MessageTreeListener lsnr, boolean hideFilter) {
        this(client, ui, themes, trans, navControl, uriControl, bookmarkControl, dataCallback, parent, lsnr, true, true, true, true, hideFilter);
    }
    public WatchedMessageTree(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl navControl, URIControl uriControl, BookmarkControl bookmarkControl, DataCallback dataCallback, Composite parent, MessageTreeListener lsnr, boolean showAuthor, boolean showChannel, boolean showDate, boolean showTags, boolean hideFilter) {
        // don't show the forum column, don't show the flags column, don't expand anything by default
        super(client, ui, themes, trans, navControl, uriControl, bookmarkControl, dataCallback, parent, lsnr, showAuthor, false, showDate, showTags, hideFilter, false, false, false);
        _multiforum = true;
    }

    public void applyFilter(String filter) {
        try { 
            SyndieURI uri = new SyndieURI(filter);
            if (uri.isSearch()) {
                Hash scopes[] = uri.getSearchScopes();
                _multiforum = ( (scopes != null) && (scopes.length != 1) );
            } else {
                _multiforum = false;
            }
        } catch (URISyntaxException use) {
            _multiforum = false;
        }
        //System.out.println("apply filter: " + _multiforum + ": " + filter);
        super.applyFilter(filter);
    }
    
    /** given the list of thread roots, munge them into forums w/ threads underneath */
    void setMessages(List referenceNodes) {
        if (!_multiforum) { super.setMessages(referenceNodes); return; }
        Map forumToNodeList = new HashMap();
        Map forumNameToForum = new TreeMap();
        for (int i = 0; i < referenceNodes.size(); i++) {
            ThreadReferenceNode node = (ThreadReferenceNode)referenceNodes.get(i);
            Hash forum = getForum(node);
            if (forum == null) continue; // all dummies?
            List nodes = (List)forumToNodeList.get(forum);
            if (nodes == null) {
                nodes = new ArrayList();
                forumToNodeList.put(forum, nodes);
            }
            String name = getForumName(node);
            if ( (name == null) || (name.trim().length() <= 0) )
                name = forum.toBase64().substring(0,6);
            else
                name = name + " [" + forum.toBase64().substring(0,6) + "]";
            forumNameToForum.put(name, forum);
            nodes.add(node);
        }
        List forumNodes = new ArrayList();
        // sorted by forum name
        for (Iterator iter = forumNameToForum.keySet().iterator(); iter.hasNext(); ) {
            String name = (String)iter.next();
            Hash forum = (Hash)forumNameToForum.get(name);
            List nodes = (List)forumToNodeList.get(forum);
            //String name = _browser.getClient().getChannelName(forum);
            String type = nodes.size() + "";
            if (name == null)
                name = "";
            name = "(" + nodes.size() + ") " + name;// + " [" + forum.toBase64().substring(0,6) + "]";
            //ReferenceNode node = new ReferenceNode(name, SyndieURI.createScope(forum), "", "");
            ThreadReferenceNode node = new ThreadReferenceNode();
            node.setName(name);
            node.setURI(SyndieURI.createScope(forum));
            node.setDescription("");
            for (int i = 0; i < nodes.size(); i++)
                node.addChild((ReferenceNode)nodes.get(i));
            forumNodes.add(node);
        }
        super.setMessages(forumNodes);
    }
    
    private Hash getForum(ThreadReferenceNode node) {
        if (node.getTargetHash() != null)
            return node.getTargetHash();
        for (int i = 0; i < node.getChildCount(); i++) {
            Hash forum = getForum((ThreadReferenceNode)node.getChild(i));
            if (forum != null)
                return forum;
        }
        return null;
    }
    private String getForumName(ThreadReferenceNode node) {
        if (node.getTargetName() != null)
            return node.getTargetName();
        for (int i = 0; i < node.getChildCount(); i++) {
            String name = getForumName((ThreadReferenceNode)node.getChild(i));
            if (name != null)
                return name;
        }
        return null;
    }
    
    protected void markUnreadChild(TreeItem item) {
        if (!_multiforum) { super.markUnreadChild(item); return; }
        if (item.getParentItem() != null) // don't mark the forum nodes
            super.markUnreadChild(item);
    }
    
    protected long markAllRead(TreeItem item) {
        if (!_multiforum) { return super.markAllRead(item); }
        if (item.getParentItem() == null) {
            ReferenceNode node = (ReferenceNode)_itemToNode.get(item);
            if ( (node != null) && (node.getURI() != null) ) {
                Hash scope = node.getURI().getScope();
                if (scope != null) {
                    long target = _client.getChannelId(scope);
                    _client.markChannelRead(target);
                    return target;
                }
            }
            return -1;
        } else {
            return super.markAllRead(item);
        }
    }
    
    protected int getMessageSelectedCount() {
        if (!_multiforum) { return super.getMessageSelectedCount(); }
        TreeItem sel[] = _tree.getSelection();
        int rv = 0;
        for (int i = 0; i < sel.length; i++) {
            if (sel[i].getParentItem() != null)
                rv++;
        }
        return rv;
    }
    
    protected TreeItem getThreadRoot(TreeItem item) {
        if (!_multiforum) { return super.getThreadRoot(item); }
        TreeItem root = item;
        while ( (root.getParentItem() != null) && (root.getParentItem().getParentItem() != null) )
            root = root.getParentItem();
        return root;
    }
    
    protected long renderNode(ThreadReferenceNode node, TreeItem item) {
        if (!_multiforum) { return super.renderNode(node, item); }
        if ( (node != null) && (node.getURI() != null) && (node.getURI().getMessageId() == null) )
            return renderForumNode(node, item);
        else
            return super.renderNode(node, item);
    }
    
    private long renderForumNode(ThreadReferenceNode node, TreeItem item) {
        String title = node.getName();
        item.setText(0, title);
        setMinWidth(_colSubject, title, 0, 100);
        return 0;
    }
       
    protected BookmarkDnD getBookmark(TreeItem item, ReferenceNode node) {
        if (!_multiforum) { return super.getBookmark(item, node); }
        if ( (node != null) && (node.getURI() != null) && (node.getURI().getMessageId() == null) ) {
            Hash forum = getForum((ThreadReferenceNode)node);
            if (forum == null) return null;
            String name = getForumName((ThreadReferenceNode)node); //_browser.getClient().getChannelName(forum);
            if (name == null) name = "";
            
            SyndieURI uri = SyndieURI.createScope(forum);
            BookmarkDnD bookmark = new BookmarkDnD();
            bookmark.uri = uri;
            bookmark.name = name;
            bookmark.desc = forum.toBase64();
            return bookmark;
        } else {
            return super.getBookmark(item, node);
        }
    }    
}
