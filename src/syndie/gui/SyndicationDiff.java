package syndie.gui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TreeEvent;
import org.eclipse.swt.events.TreeListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.SyndieURI;
import syndie.db.SharedArchive;
import syndie.db.SyndicationManager;

/**
 * view and pick particular messages/forums to pull from the archives
 *
 */
public class SyndicationDiff implements Translatable, Themeable, SyndicationManager.SyndicationListener {
    private BrowserControl _browser;
    private Composite _parent;
    private Composite _root;
    private Group _treeGroup;
    private Tree _tree;
    private TreeColumn _colName;
    private TreeColumn _colType;
    private TreeColumn _colSize;
    private TreeColumn _colRecvDate;
    private TreeColumn _colIsNewScope;
    private TreeColumn _colIsBookmarked;
    
    private MenuItem _menuBookmarkedOnly;
    private MenuItem _menuNewScope;
    private MenuItem _menuKnownScope;
    
    /** uri to TreeItem */
    private Map _uriToItem;
    /** archive name to Set of SyndieURIs that are included in the diff */
    private Map _archiveURIs;
    
    private boolean _displayUnbookmarked;
    private boolean _displayNewScope;
    private boolean _displayKnownScope;
    
    public SyndicationDiff(BrowserControl browser, Composite parent) {
        _browser = browser;
        _parent = parent;
        _uriToItem = new HashMap();
        _archiveURIs = new HashMap();
        _displayUnbookmarked = true;
        _displayNewScope = true;
        _displayKnownScope = true;
        initComponents();
    }
    
    public Control getControl() { return _root; }

    public void dispose() {
        _browser.getSyndicationManager().removeListener(this);
        _browser.getThemeRegistry().unregister(this);
        _browser.getTranslationRegistry().unregister(this);
    }
    
    /** set of URIs that were explicitly selected */
    public Set getSelectedURIs() {
        Set rv = new HashSet();
        for (Iterator iter = _uriToItem.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry)iter.next();
            SyndieURI uri = (SyndieURI)entry.getKey();
            TreeItem item = (TreeItem)entry.getValue();
            if ( (uri.getMessageId() == null) && ("".equals(item.getText(4))) ) {
                // its a scope we already have.  dont fetch it, even if selected
            } else {
                if (item.getChecked())
                    rv.add(uri);
            }
        }
        return rv;
    }
    /** set of URIs that were expanded */
    public Set getExpandedURIs() {
        Set rv = new HashSet();
        for (Iterator iter = _uriToItem.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry)iter.next();
            SyndieURI uri = (SyndieURI)entry.getKey();
            TreeItem item = (TreeItem)entry.getValue();
            if (item.getExpanded())
                rv.add(uri);
        }
        return rv;
    }
    /** set of archive names that the URI could be fetched from */
    private List getSources(SyndieURI uri) {
        ArrayList rv = new ArrayList(1);
        for (Iterator iter = _archiveURIs.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry)iter.next();
            String archive = (String)entry.getKey();
            Set uris = (Set)entry.getValue();
            if (uris.contains(uri))
                rv.add(archive);
        }
        return rv;
    }
    /** 
     * map of archive name to SyndieURI that should be retrieved from it.  for resources
     * that can be fetched from multiple archives, a random one is selected
     */
    public Map getExplicit() {
       Map rv = new HashMap();
       Set uris = getSelectedURIs();
       for (Iterator iter = uris.iterator(); iter.hasNext(); ) {
           SyndieURI uri = (SyndieURI)iter.next();
           List sources = getSources(uri);
           Collections.shuffle(sources);
           if (sources.size() > 0) {
               String archive = (String)sources.get(0);
               Set toFetch = (Set)rv.get(archive);
               if (toFetch == null) {
                   toFetch = new HashSet();
                   rv.put(archive, toFetch);
               }
               _browser.getUI().debugMessage("select explicit: " + uri + " from " + archive);
               toFetch.add(uri);
           } else {
               _browser.getUI().debugMessage("select explicit no source for: " + uri);
           }
       }
       return rv;
    }
    
    private void displayDiff() {
        Set prevSelected = getSelectedURIs();
        Set expanded = getExpandedURIs();
        
        _browser.getUI().debugMessage("displaying diff");
        _tree.setRedraw(false);
        // first remove all the old stuff
        _archiveURIs.clear();
        for (Iterator iter = _uriToItem.values().iterator(); iter.hasNext(); )
            ((TreeItem)iter.next()).dispose();
        _uriToItem.clear();
        
        // now build up the diffs
        List banned = _browser.getClient().getBannedChannels();
        SyndicationManager mgr = _browser.getSyndicationManager();
        int count = mgr.getArchiveCount();
        for (int i = 0; i < mgr.getArchiveCount(); i++) {
            String name = mgr.getArchiveName(i);
            SharedArchive index = mgr.getArchiveIndex(i);
            _browser.getUI().debugMessage("displaying archive " + name + (index == null ? " [no index]" : ""));
            if (index == null) continue;
            
            _archiveURIs.put(name, new HashSet());
            
            // todo: build this tree of channel-->message out of the data
            /*
            for (int j = 0; j < index.getChannelCount(); j++) {
                ArchiveChannel chan = index.getChannel(j);
                if (!banned.contains(new Hash(chan.getScope())))
                    displayDiff(index.getChannel(j), name, banned, prevSelected, expanded);
            }
             */
        }
        _tree.setEnabled(_tree.getItemCount() > 0);
        _treeGroup.setEnabled(_tree.getItemCount() > 0);
        _tree.setRedraw(true);
    }

    /*
    private void displayDiff(ArchiveChannel channel, String archive, List banned, Set toSelectURIs, Set expanded) {
        Hash scope = new Hash(channel.getScope());
        long channelId = _browser.getClient().getChannelId(scope);
        long remVersion = channel.getVersion();
        long localVersion = -1;
        if (channelId >= 0)
            localVersion = _browser.getClient().getChannelVersion(channelId);
        
        if (!_displayKnownScope && (channelId >= 0))
            return;
        else if (!_displayNewScope && (channelId < 0))
            return;
        
        List wantedMessages = new ArrayList();
        for (int i = 0; i < channel.getMessageCount(); i++) {
            ArchiveMessage msg = channel.getMessage(i);
            if (banned.contains(msg.getPrimaryScope()))
                continue;
            if (_browser.getClient().getMessageId(msg.getPrimaryScope(), msg.getMessageId()) == -1) {
                wantedMessages.add(msg);
                _browser.getUI().debugMessage("want message " + msg.getMessageId() + " from " + scope.toBase64() + " from " + archive);
            }
        }
        
        if ( (wantedMessages.size() > 0) || (remVersion > localVersion) ) {
            SyndieURI scopeURI = SyndieURI.createScope(scope);
            boolean isBookmarked = ((channelId >= 0) && _browser.isBookmarked(scopeURI));
            if (!_displayUnbookmarked && !isBookmarked)
                return;
            
            TreeItem scopeItem = (TreeItem)_uriToItem.get(scopeURI);
            if (scopeItem == null) {
                scopeItem = new TreeItem(_tree, SWT.NONE);
                _uriToItem.put(scopeURI, scopeItem);
                long msgSize = 0;
                for (int i = 0; i < wantedMessages.size(); i++)
                    msgSize += ((ArchiveMessage)wantedMessages.get(i)).getEntrySize();
                displayScope(scopeItem, scope, channelId, localVersion < remVersion, channel, msgSize, toSelectURIs, isBookmarked);
            }

            if (expanded.contains(scopeURI))
                scopeItem.setExpanded(true);
            
            Set archiveURIs = (Set)_archiveURIs.get(archive);
            if (remVersion > localVersion)
                archiveURIs.add(scopeURI);
            
            for (int i = 0; i < wantedMessages.size(); i++) {
                ArchiveMessage msg = (ArchiveMessage)wantedMessages.get(i);
                SyndieURI msgURI = SyndieURI.createMessage(scope, msg.getMessageId());
                TreeItem msgItem = (TreeItem)_uriToItem.get(msgURI);
                if (msgItem == null) {
                    msgItem = new TreeItem(scopeItem, SWT.NONE);
                    _uriToItem.put(msgURI, msgItem);
                    displayMsg(msgItem, scopeItem, msg, toSelectURIs);
                }
                if (expanded.contains(msgURI))
                    msgItem.setExpanded(true);
                archiveURIs.add(msgURI);
            }
        }
    }
    private void displayScope(TreeItem item, Hash scope, long channelId, boolean remoteIsNewer, ArchiveChannel channel, long msgSize, Set toSelectURIs, boolean isBookmarked) {
        String name = null;
        //int size = channel.get
        long size = channel.getEntrySize();
        long recv = channel.getReceiveDate();
        boolean isNew = (channelId < 0);
        SyndieURI uri = SyndieURI.createScope(scope);
        
        if (channelId >= 0) {
            ChannelInfo info = _browser.getClient().getChannel(channelId);
            name = scope.toBase64().substring(0, 6) + ": " + info.getName();
        } else {
            name = scope.toBase64().substring(0, 6);
        }
        
        item.setText(0, name);
        item.setImage(1, ImageUtil.ICON_MSG_TYPE_META);
        item.setText(2, Long.toString((size+1023)/1024) + "/" + Long.toString((msgSize+1023)/1024));
        item.setText(3, Constants.getDate(recv));
        item.setText(4, isNew ? "X" : "");
        item.setText(5, isBookmarked ? "X" : "");
        
        item.setChecked(toSelectURIs.contains(uri));
        
        setMinWidth(_colName, name, 20);
        setMinWidth(_colSize, item.getText(2));
        setMinWidth(_colRecvDate, item.getText(3));
    }
    
    private void displayMsg(TreeItem item, TreeItem parent, ArchiveMessage msg, Set toSelectURIs) {
        String name = Long.toString(msg.getMessageId());
        long size = msg.getEntrySize();
        long recv = msg.getReceiveDate();
        
        item.setText(0, name);
        if (msg.getIsReply())
            item.setImage(1, ImageUtil.ICON_MSG_TYPE_PRIVATE);
        else if (msg.getIsPasswordProtected())
            item.setImage(1, ImageUtil.ICON_SYNDICATE_STATUS_PBE);
        else
            item.setImage(1, ImageUtil.ICON_MSG_TYPE_NORMAL);
        item.setText(2, Long.toString((size+1023)/1024));
        item.setText(3, Constants.getDate(recv));
        item.setText(4, parent.getText(4)); // is in a new scope
        item.setText(5, parent.getText(5)); // is in a bookmarked scope
        
        SyndieURI uri = SyndieURI.createMessage(msg.getPrimaryScope(), msg.getMessageId());
        item.setChecked(toSelectURIs.contains(uri));
        
        setMinWidth(_colName, name, 20);
        setMinWidth(_colSize, item.getText(2));
        setMinWidth(_colRecvDate, item.getText(3));
    }
     */
    
    private void setMinWidth(TreeColumn col, String text) { setMinWidth(col, text, 0); }
    private void setMinWidth(TreeColumn col, String text, int extraWidth) {
        int width = ImageUtil.getWidth(text + "  ", _tree) + _tree.getGridLineWidth()*2 + extraWidth;
        int existing = col.getWidth();
        if (width > existing) {
            _browser.getUI().debugMessage("Increasing the width on " + col.getText() + " from " + existing + " to " + width);
            col.setWidth(width);
        } else {
            _browser.getUI().debugMessage("Keeping the width on " + col.getText() + " at " + existing + " (new width would be " + width + ")");
        }
    }
    
    private void selectionUpdated() {
        //if (_view != null)
        //    _view.explicitSelectionUpdated();
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new FillLayout());
        
        _treeGroup = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _treeGroup.setLayout(new FillLayout());
        
        _tree = new Tree(_treeGroup, SWT.MULTI | SWT.CHECK | SWT.H_SCROLL | SWT.V_SCROLL);
        _tree.setHeaderVisible(true);
        _tree.setLinesVisible(true);
        
        SyndieTreeListener lsnr = new SyndieTreeListener(_tree) {
            public void widgetDefaultSelected(SelectionEvent evt) { 
                super.widgetDefaultSelected(evt);
                if (evt.detail == SWT.CHECK)
                    selectionUpdated();
            }
            public void widgetSelected(SelectionEvent evt) { 
                super.widgetSelected(evt);
                if (evt.detail == SWT.CHECK)
                    selectionUpdated();
            }
            public void doubleclick() {
                super.doubleclick();
                toggleSubtree();
            }
        };
        _tree.addControlListener(lsnr);
        _tree.addKeyListener(lsnr);
        _tree.addTraverseListener(lsnr);
        _tree.addSelectionListener(lsnr);
        _tree.addMouseListener(lsnr);
        
        _colName = new TreeColumn(_tree, SWT.LEFT);
        _colType = new TreeColumn(_tree, SWT.CENTER);
        _colSize = new TreeColumn(_tree, SWT.RIGHT);
        _colRecvDate = new TreeColumn(_tree, SWT.LEFT);
        _colIsNewScope = new TreeColumn(_tree, SWT.CENTER);
        _colIsBookmarked = new TreeColumn(_tree, SWT.CENTER);
    
        Menu menu = new Menu(_tree);
        _tree.setMenu(menu);
        
        _menuBookmarkedOnly = new MenuItem(menu, SWT.CHECK);
        _menuBookmarkedOnly.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                _displayUnbookmarked = !_menuBookmarkedOnly.getSelection();
                displayDiff();
            }
            public void widgetSelected(SelectionEvent selectionEvent) {
                _displayUnbookmarked = !_menuBookmarkedOnly.getSelection();
                displayDiff();
            }
        });
        _menuNewScope = new MenuItem(menu, SWT.CHECK);
        _menuNewScope.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                _displayNewScope = _menuNewScope.getSelection();
                displayDiff();
            }
            public void widgetSelected(SelectionEvent selectionEvent) {
                _displayNewScope = _menuNewScope.getSelection();
                displayDiff();
            }
        });
        _menuKnownScope = new MenuItem(menu, SWT.CHECK);
        _menuKnownScope.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                _displayKnownScope = _menuKnownScope.getSelection();
                displayDiff();
            }
            public void widgetSelected(SelectionEvent selectionEvent) {
                _displayKnownScope = _menuKnownScope.getSelection();
                displayDiff();
            }
        });
        
        _menuBookmarkedOnly.setSelection(!_displayUnbookmarked);
        _menuNewScope.setSelection(_displayNewScope);
        _menuKnownScope.setSelection(_displayKnownScope);
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
        _browser.getSyndicationManager().addListener(this);
        
        displayDiff();
    }
    
    private void toggleSubtree() {
        TreeItem items[] = _tree.getSelection();
        if (items != null) {
            for (int i = 0; i < items.length; i++) {
                boolean checked = items[i].getChecked();
                items[i].setChecked(!checked);
                for (int j = 0; j < items[i].getItemCount(); j++)
                    items[i].getItem(j).setChecked(!checked);
            }
        }
    }
    
    private static final String T_TREE_GROUP = "syndie.gui.syndicationdiff.treegroup";
    private static final String T_COLNAME = "syndie.gui.syndicationdiff.colname";
    private static final String T_COLTYPE = "syndie.gui.syndicationdiff.coltype";
    private static final String T_COLSIZE = "syndie.gui.syndicationdiff.colsize";
    private static final String T_COLRECVDATE = "syndie.gui.syndicationdiff.colrecvdate";
    private static final String T_COLISNEW = "syndie.gui.syndicationdiff.colisnew";
    private static final String T_COLBOOKMARKED = "syndie.gui.syndicationdiff.colisbookmarked";
    private static final String T_COLSIZE_TOOLTIP = "syndie.gui.syndicationdiff.colsizetooltip";
    private static final String T_COLRECVDATE_TOOLTIP = "syndie.gui.syndicationdiff.colrecvdatetooltip";
    private static final String T_COLISNEW_TOOLTIP = "syndie.gui.syndicationdiff.colisnewtooltip";
    private static final String T_COLBOOKMARKED_TOOLTIP = "syndie.gui.syndicationdiff.colisbookmarkedtooltip";
    private static final String T_BOOKMARKEDONLY = "syndie.gui.syndicationdiff.bookmarkedonly";
    private static final String T_NEWSCOPE = "syndie.gui.syndicationdiff.newscope";
    private static final String T_KNOWNSCOPE = "syndie.gui.syndicationdiff.knownscope";
    
    public void translate(TranslationRegistry registry) {
        _treeGroup.setText(registry.getText(T_TREE_GROUP, "Differences:"));
        
        _colName.setText(registry.getText(T_COLNAME, "Resource"));
        _colType.setText(registry.getText(T_COLTYPE, ""));
        _colSize.setText(registry.getText(T_COLSIZE, "Size"));
        _colRecvDate.setText(registry.getText(T_COLRECVDATE, "Date"));
        _colIsNewScope.setText(registry.getText(T_COLISNEW, "New scope?"));
        _colIsBookmarked.setText(registry.getText(T_COLBOOKMARKED, "Bookmarked?"));

        _colSize.setToolTipText(registry.getText(T_COLSIZE_TOOLTIP, "Size in kilobytes"));
        _colRecvDate.setToolTipText(registry.getText(T_COLRECVDATE_TOOLTIP, "When the remote archive says they received the message"));
        _colIsNewScope.setToolTipText(registry.getText(T_COLISNEW_TOOLTIP, "Is this forum one that we do not yet know locally?"));
        _colIsBookmarked.setToolTipText(registry.getText(T_COLBOOKMARKED_TOOLTIP, "Is this forum bookmarked?"));

        _menuBookmarkedOnly.setText(registry.getText(T_BOOKMARKEDONLY, "Only show posts in bookmarked forums?"));
        _menuKnownScope.setText(registry.getText(T_KNOWNSCOPE, "Show posts in forums already known locally?"));
        _menuNewScope.setText(registry.getText(T_NEWSCOPE, "Show posts in forums not known locally?"));
    }
    
    public void applyTheme(Theme theme) {
        _tree.setFont(theme.TREE_FONT);
        _treeGroup.setFont(theme.DEFAULT_FONT);
        _colName.pack();
        _colType.setWidth(24);
        _colSize.pack();
        _colRecvDate.pack();
        _colIsNewScope.pack();
        _colIsBookmarked.pack();
    }

    public void archivesLoaded(SyndicationManager mgr) {
        Display.getDefault().asyncExec(new Runnable() { 
            public void run() { displayDiff(); }
        });
    }
    public void archiveAdded(SyndicationManager mgr, String name) {}
    public void archiveRemoved(SyndicationManager mgr, String name) {}
    public void archiveUpdated(SyndicationManager mgr, String oldName, String newName) {}
    public void archiveIndexStatus(SyndicationManager mgr, SyndicationManager.StatusRecord record) {
        _browser.getUI().debugMessage("index status for " + record.getSource() + ":" + record.getStatus());
        if (record.getStatus() == SyndicationManager.FETCH_INDEX_DIFF_OK) {
            // called from a fetcher thread
            Display.getDefault().asyncExec(new Runnable() { 
                public void run() { displayDiff(); }
            });
        }
    }
    public void fetchStatusUpdated(SyndicationManager mgr, SyndicationManager.StatusRecord record) {}
    public void syndicationComplete(SyndicationManager mgr) {
        Display.getDefault().asyncExec(new Runnable() { 
            public void run() { displayDiff(); }
        });
    }
}
