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
import org.eclipse.swt.events.TreeEvent;
import org.eclipse.swt.events.TreeListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.SyndieURI;
import syndie.db.ArchiveChannel;
import syndie.db.ArchiveDiff;
import syndie.db.ArchiveIndex;
import syndie.db.ArchiveMessage;
import syndie.db.SyndicationManager;

/**
 * view and pick particular messages/forums to pull from the archives
 *
 */
public class SyndicationDiff implements Translatable, Themeable, SyndicationManager.SyndicationListener {
    private BrowserControl _browser;
    private Composite _parent;
    private SyndicationView _view;
    private Composite _root;
    private Group _treeGroup;
    private Tree _tree;
    private TreeColumn _colName;
    private TreeColumn _colType;
    private TreeColumn _colSize;
    private TreeColumn _colRecvDate;
    private TreeColumn _colIsNewScope;
    private TreeColumn _colIsBookmarked;
    
    /** uri to TreeItem */
    private Map _uriToItem;
    /** archive name to Set of SyndieURIs that are included in the diff */
    private Map _archiveURIs;
    /** set of SyndieURIs that should be fetched */
    private Set _selectedURIs;
    
    public SyndicationDiff(BrowserControl browser, Composite parent, SyndicationView view) {
        _browser = browser;
        _parent = parent;
        _view = view;
        _uriToItem = new HashMap();
        _archiveURIs = new HashMap();
        _selectedURIs = new HashSet();
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
        // we should save selection state before clearing everything, since 
        // displayDiff is called by the SyndicationManager whenever an archive's index
        // is fully loaded
        Set prevSelected = getSelectedURIs();
        
        _browser.getUI().debugMessage("displaying diff");
        _tree.setRedraw(false);
        // first remove all the old stuff
        _archiveURIs.clear();
        _selectedURIs.clear();
        for (Iterator iter = _uriToItem.values().iterator(); iter.hasNext(); )
            ((TreeItem)iter.next()).dispose();
        _uriToItem.clear();
        
        // now build up the diffs
        List banned = _browser.getClient().getBannedChannels();
        SyndicationManager mgr = _browser.getSyndicationManager();
        int count = mgr.getArchiveCount();
        for (int i = 0; i < mgr.getArchiveCount(); i++) {
            String name = mgr.getArchiveName(i);
            ArchiveIndex index = mgr.getArchiveIndex(i);
            _browser.getUI().debugMessage("displaying archive " + name + (index == null ? " [no index]" : ""));
            if (index == null) continue;
            
            _archiveURIs.put(name, new HashSet());
            
            for (int j = 0; j < index.getChannelCount(); j++) {
                ArchiveChannel chan = index.getChannel(j);
                if (!banned.contains(new Hash(chan.getScope())))
                    displayDiff(index.getChannel(j), name, banned, prevSelected);
            }
        }
        _tree.setRedraw(true);
    }
    
    private void displayDiff(ArchiveChannel channel, String archive, List banned, Set toSelectURIs) {
        Hash scope = new Hash(channel.getScope());
        long channelId = _browser.getClient().getChannelId(scope);
        long remVersion = channel.getVersion();
        long localVersion = -1;
        if (channelId >= 0)
            localVersion = _browser.getClient().getChannelVersion(channelId);
        List wantedMessages = new ArrayList();
        for (int i = 0; i < channel.getMessageCount(); i++) {
            ArchiveMessage msg = channel.getMessage(i);
            if (banned.contains(msg.getPrimaryScope()))
                continue;
            //if (_browser.getClient().getMessageId(msg.getPrimaryScope(), msg.getMessageId()) == -1) {
                wantedMessages.add(msg);
                _browser.getUI().debugMessage("want message " + msg.getMessageId() + " from " + scope.toBase64() + " from " + archive);
            //}
        }
        
        if ( (wantedMessages.size() > 0) || (remVersion > localVersion) ) {
            SyndieURI scopeURI = SyndieURI.createScope(scope);
            TreeItem scopeItem = (TreeItem)_uriToItem.get(scopeURI);
            if (scopeItem == null) {
                scopeItem = new TreeItem(_tree, SWT.NONE);
                _uriToItem.put(scopeURI, scopeItem);
                long msgSize = 0;
                for (int i = 0; i < wantedMessages.size(); i++)
                    msgSize += ((ArchiveMessage)wantedMessages.get(i)).getEntrySize();
                displayScope(scopeItem, scope, channelId, localVersion < remVersion, channel, msgSize, toSelectURIs);
            }
            
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
                archiveURIs.add(msgURI);
            }
        }
    }
    private void displayScope(TreeItem item, Hash scope, long channelId, boolean remoteIsNewer, ArchiveChannel channel, long msgSize, Set toSelectURIs) {
        String name = null;
        //int size = channel.get
        long size = channel.getEntrySize();
        long recv = channel.getReceiveDate();
        boolean isNew = (channelId < 0);
        SyndieURI uri = SyndieURI.createScope(scope);
        boolean isBookmarked = (!isNew && _browser.isBookmarked(uri));
        
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
        _view.explicitSelectionUpdated();
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
        
        _browser.getThemeRegistry().register(this);
        _browser.getTranslationRegistry().register(this);
        _browser.getSyndicationManager().addListener(this);
    }
    
    private static final String T_TREE_GROUP = "syndie.gui.syndicationdiff.treegroup";
    private static final String T_COLNAME = "syndie.gui.syndicationdiff.colname";
    private static final String T_COLTYPE = "syndie.gui.syndicationdiff.coltype";
    private static final String T_COLSIZE = "syndie.gui.syndicationdiff.colsize";
    private static final String T_COLRECVDATE = "syndie.gui.syndicationdiff.colrecvdate";
    private static final String T_COLISNEW = "syndie.gui.syndicationdiff.colisnew";
    private static final String T_COLBOOKMARKED = "syndie.gui.syndicationdiff.colisbookmarked";
    
    public void translate(TranslationRegistry registry) {
        _treeGroup.setText(registry.getText(T_TREE_GROUP, "Differences:"));
        
        _colName.setText(registry.getText(T_COLNAME, "Resource"));
        _colType.setText(registry.getText(T_COLTYPE, ""));
        _colSize.setText(registry.getText(T_COLSIZE, "Size"));
        _colRecvDate.setText(registry.getText(T_COLRECVDATE, "Date"));
        _colIsNewScope.setText(registry.getText(T_COLISNEW, "New scope?"));
        _colIsBookmarked.setText(registry.getText(T_COLBOOKMARKED, "Bookmarked?"));
                
        _colName.pack();
        _colType.setWidth(24);
        _colSize.pack();
        _colRecvDate.pack();
        _colIsNewScope.pack();
        _colIsBookmarked.pack();
    }
    
    public void applyTheme(Theme theme) {
        _tree.setFont(theme.TREE_FONT);
        _treeGroup.setFont(theme.DEFAULT_FONT);
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
}
