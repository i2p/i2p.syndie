package syndie.gui;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import net.i2p.data.Hash;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.ChannelInfo;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.WatchedChannel;

class RefTree extends ReferenceChooserTree {
    private ReferenceChooserTreeDnDSource _dndSrc;
    public RefTree(DataControl dataControl, NavigationControl navControl, URIControl uriControl, Composite parent) {
        super(dataControl, navControl, uriControl, parent, new ReferenceChooserTree.ChoiceListener() {
            public void watchedChannelSelected(TreeItem item, WatchedChannel channel) {}
            public void bookmarkSelected(TreeItem item, NymReferenceNode node) {}
            public void manageChannelSelected(TreeItem item, ChannelInfo channel) {}
            public void postChannelSelected(TreeItem item, ChannelInfo channel) {}
            public void searchResultSelected(String name, ReferenceNode node) {}
            public void otherSelected(TreeItem item) {}
        }, new ReferenceChooserTree.AcceptanceListener() {
            public void referenceAccepted(SyndieURI uri) {}
            public void referenceChoiceAborted() {}
        }, false, false);
    }

    protected void initComponents(boolean register, boolean multipleSelections) {
        super.initComponents(register, true);
        _dndSrc = new ReferenceChooserTreeDnDSource(_dataControl, RefTree.this, true);
    }
    public ReferenceNode getDragged() { 
        TreeItem[] item = super.getTree().getSelection();
        _dataControl.getUI().debugMessage("getDragged(): " + (item != null ? item.length : 0) + " items");
        if ( (item == null) || (item.length == 0) ) return null;
        if (item.length == 1) {
            return getDragged(item[0]);
        } else {
            ReferenceNode parent = new ReferenceNode("", null, "", "");
            for (int i = 0; i < item.length; i++) {
                ReferenceNode child = getDragged(item[i]);
                if (child != null)
                    parent.addChild(child);
            }
            if (parent.getChildCount() > 1)
                return parent;
            else if (parent.getChildCount() == 1)
                return parent.getChild(0);
            else
                return null;
        }
    }
    private ReferenceNode getDragged(TreeItem item) {
        NymReferenceNode bookmark = getBookmark(item);
        if (bookmark != null)
            return ReferenceNode.deepCopy(bookmark);

        ChannelInfo chan = getPostChannel(item);
        if (chan == null)
            chan = getManageChannel(item);
        if (chan != null) {
            String name = chan.getName();
            String desc = chan.getDescription();
            if (name == null) name = chan.getChannelHash().toBase64().substring(0,6);
            if (desc == null) desc = "";
            return new ReferenceNode(name, SyndieURI.createScope(chan.getChannelHash()), desc, null);
        }
        WatchedChannel watched = getWatchedChannel(item);
        if (watched != null) {
            Hash scope = _dataControl.getClient().getChannelHash(watched.getChannelId());
            String name = _dataControl.getClient().getChannelName(watched.getChannelId());
            if (name == null)
                name = scope.toBase64().substring(0,6);
            return new ReferenceNode(name, SyndieURI.createScope(scope), "", null);
        }

        if (item == getWatchedRoot()) {
            ReferenceNode rv = new ReferenceNode(_dataControl.getTranslationRegistry().getText(T_SRCGROUP, "Group ") + (System.currentTimeMillis()%1000), null, "", null);
            Collection chans = _dataControl.getClient().getWatchedChannels();
            for (Iterator iter = chans.iterator(); iter.hasNext(); ) {
                WatchedChannel cur = (WatchedChannel)iter.next();
                Hash scope = _dataControl.getClient().getChannelHash(cur.getChannelId());
                String name = _dataControl.getClient().getChannelName(cur.getChannelId());
                if (name == null)
                    name = scope.toBase64().substring(0,6);
                rv.addChild(new ReferenceNode(name, SyndieURI.createScope(scope), "", null));
            }
            if (rv.getChildCount() > 1)
                return rv;
            else if (rv.getChildCount() == 1)
                return rv.getChild(0);
            else
                return null;
        } else if (item == getBookmarkRoot()) {
            ReferenceNode rv = new ReferenceNode(_dataControl.getTranslationRegistry().getText(T_SRCGROUP, "Group ") + (System.currentTimeMillis()%1000), null, "", null);
            Collection chans = getBookmarks();
            for (Iterator iter = chans.iterator(); iter.hasNext(); ) {
                NymReferenceNode cur = (NymReferenceNode)iter.next();
                ReferenceNode add = ReferenceNode.deepCopy(cur); // not just one level
                rv.addChild(add);
            }
            if (rv.getChildCount() > 1)
                return rv;
            else if (rv.getChildCount() == 1)
                return rv.getChild(0);
            else
                return null;
        } else if (item == getManageRoot()) {
            ReferenceNode rv = new ReferenceNode(_dataControl.getTranslationRegistry().getText(T_SRCGROUP, "Group ") + (System.currentTimeMillis()%1000), null, "", null);
            ArrayList chans = getManageableChannels();
            for (int i = 0; i < chans.size(); i++) {
                chan = (ChannelInfo)chans.get(i);
                String name = chan.getName();
                String desc = chan.getDescription();
                if (name == null) name = chan.getChannelHash().toBase64().substring(0,6);
                if (desc == null) desc = "";
                rv.addChild(new ReferenceNode(name, SyndieURI.createScope(chan.getChannelHash()), desc, null));
            }
            if (rv.getChildCount() > 1)
                return rv;
            else if (rv.getChildCount() == 1)
                return rv.getChild(0);
            else
                return null;
        } else if (item == getPostRoot()) {
            ReferenceNode rv = new ReferenceNode(_dataControl.getTranslationRegistry().getText(T_SRCGROUP, "Group ") + (System.currentTimeMillis()%1000), null, "", null);
            ArrayList chans = getPostableChannels();
            for (int i = 0; i < chans.size(); i++) {
                chan = (ChannelInfo)chans.get(i);
                String name = chan.getName();
                String desc = chan.getDescription();
                if (name == null) name = chan.getChannelHash().toBase64().substring(0,6);
                if (desc == null) desc = "";
                rv.addChild(new ReferenceNode(name, SyndieURI.createScope(chan.getChannelHash()), desc, null));
            }
            if (rv.getChildCount() > 1)
                return rv;
            else if (rv.getChildCount() == 1)
                return rv.getChild(0);
            else
                return null;
        }

        return null;
    }    
    private static final String T_SRCGROUP = "syndie.gui.reftree.srcgroup";
}