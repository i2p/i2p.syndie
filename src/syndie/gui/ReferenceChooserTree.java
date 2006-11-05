package syndie.gui;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.events.TreeEvent;
import org.eclipse.swt.events.TreeListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.ChannelInfo;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

/**
 * The reference chooser tree has four roots:
 *  - the nym's bookmarks
 *  - the channels the nym can post to
 *  - the channels the nym can manage
 *  - search results
 */
public class ReferenceChooserTree {
    private DBClient _client;
    private Composite _parent;
    /** list of NymReferenceNode instances for the roots of the reference trees */
    private ArrayList _nymRefs;
    /** organizes the channels the nym can post to or manage */
    private DBClient.ChannelCollector _nymChannels;
    /** list of ReferenceNode instances matching the search criteria */
    private ArrayList _searchResults;
    
    private Tree _tree;
    private TreeItem _bookmarkRoot;
    /** map of TreeItem to NymReferenceNode */
    private Map _bookmarkNodes;
    private TreeItem _postRoot;
    /** map of TreeItem to ChannelInfo */
    private Map _postChannels;
    private TreeItem _manageRoot;
    /** map of TreeItem to ChannelInfo */
    private Map _manageChannels;
    private TreeItem _searchRoot;
    /** map of TreeItem to ReferenceNode */
    private Map _searchNodes;
    
    private ChoiceListener _choiceListener;
    private AcceptanceListener _acceptanceListener;
    
    public ReferenceChooserTree(DBClient client, Composite parent, ChoiceListener lsnr, AcceptanceListener accept) {
        _client = client;
        _parent = parent;
        _choiceListener = lsnr;
        _acceptanceListener = accept;
        _nymRefs = new ArrayList();
        _bookmarkNodes = new HashMap();
        _postChannels = new HashMap();
        _manageChannels = new HashMap();
        _searchNodes = new HashMap();
        _searchResults = new ArrayList();
        initComponents();
    }

    DBClient getClient() { return _client; }
    public Control getControl() { return _tree; }
    /**
     * update the search results to display.  this then redraws the seach result entry in the
     * swt display thread.
     * @param resultNodes ReferenceNode instances to display as search results
     */
    public void setSearchResults(Collection resultNodes) {
        _searchResults.clear();
        for (Iterator iter = resultNodes.iterator(); iter.hasNext(); ) {
            ReferenceNode node = (ReferenceNode)iter.next();
            _searchResults.add(node);
        }
        _tree.getDisplay().asyncExec(new Runnable() { public void run() { redrawSearchResults(); } });
    }
    public void setListener(ChoiceListener lsnr) { _choiceListener = lsnr; }
    public void setAcceptanceListener(AcceptanceListener lsnr) { _acceptanceListener = lsnr; }

    public interface ChoiceListener {
        public void bookmarkSelected(TreeItem item, NymReferenceNode node);
        public void manageChannelSelected(TreeItem item, ChannelInfo channel);
        public void postChannelSelected(TreeItem item, ChannelInfo channel);
        public void searchResultSelected(TreeItem item, ReferenceNode node);
        public void otherSelected(TreeItem item);
    }
    
    public interface AcceptanceListener {
        public void referenceAccepted(SyndieURI uri);
        public void referenceChoiceAborted();
    }
    
    
    private void initComponents() {
        _tree = new Tree(_parent, SWT.BORDER | SWT.SINGLE);
        _bookmarkRoot = new TreeItem(_tree, SWT.NONE);
        _postRoot = new TreeItem(_tree, SWT.NONE);
        _manageRoot = new TreeItem(_tree, SWT.NONE);
        _searchRoot = new TreeItem(_tree, SWT.NONE);
        
        _bookmarkRoot.setText("Bookmarked references");
        _postRoot.setText("Writable forums");
        _manageRoot.setText("Manageable forums");
        _searchRoot.setText("Search results...");
        
        _tree.addKeyListener(new KeyListener() {
            public void keyPressed(KeyEvent keyEvent) {}
            public void keyReleased(KeyEvent evt) {
                if (evt.keyCode == SWT.ARROW_LEFT) {
                    TreeItem selected = getSelected();
                    if (selected.getExpanded()) {
                        selected.setExpanded(false);
                    } else {
                        TreeItem parent = selected.getParentItem();
                        if (parent != null) {
                            parent.setExpanded(false);
                            _tree.setSelection(parent);
                        }
                    }
                } else if (evt.keyCode == SWT.ARROW_RIGHT) {
                    TreeItem selected = getSelected();
                    selected.setExpanded(true);
                } else if (evt.character == ' ') {
                    TreeItem selected = getSelected();
                    selected.setExpanded(!selected.getExpanded());
                }
            }
        });
        _tree.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) {
                    TreeItem selected = getSelected();
                    selected.setExpanded(!selected.getExpanded());        }
            }
        });
        _tree.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { fireSelectionEvents(); }
            public void widgetSelected(SelectionEvent selectionEvent) { fireSelectionEvents(); }
        });
        
        rebuildBookmarks();
        refetchNymChannels();
        redrawPostable();
        redrawManageable();
        redrawSearchResults();
    }
    private TreeItem getSelected() { if (_tree.getSelectionCount() > 0) return _tree.getSelection()[0]; return null; }
    private void rebuildBookmarks() {
        _nymRefs.clear();
        _nymRefs.addAll(_client.getNymReferences(_client.getLoggedInNymId()));
        redrawBookmarks();
    }
    private void refetchNymChannels() {
        _nymChannels = _client.getChannels(true, true, true, true);
    }
    private void redrawManageable() {
        _manageRoot.removeAll();
        _manageChannels.clear();
        for (int i = 0; i < _nymChannels.getIdentityChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getIdentityChannel(i);
            TreeItem item = new TreeItem(_manageRoot, SWT.NONE);
            item.setText("ident: " + info.getName());
            _manageChannels.put(item, info);
        }
        for (int i = 0; i < _nymChannels.getManagedChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getManagedChannel(i);
            TreeItem item = new TreeItem(_manageRoot, SWT.NONE);
            item.setText("manage: " + info.getName());
            _manageChannels.put(item, info);
        }
    }
    private void redrawPostable() {
        _postRoot.removeAll();
        _postChannels.clear();
        for (int i = 0; i < _nymChannels.getIdentityChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getIdentityChannel(i);
            TreeItem item = new TreeItem(_postRoot, SWT.NONE);
            item.setText("ident: " + info.getName());
            _postChannels.put(item, info);
        }
        for (int i = 0; i < _nymChannels.getManagedChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getManagedChannel(i);
            TreeItem item = new TreeItem(_postRoot, SWT.NONE);
            item.setText("manage: " + info.getName());
            _postChannels.put(item, info);
        }
        for (int i = 0; i < _nymChannels.getPostChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getPostChannel(i);
            TreeItem item = new TreeItem(_postRoot, SWT.NONE);
            item.setText("post: " + info.getName());
            _postChannels.put(item, info);
        }
        for (int i = 0; i < _nymChannels.getPublicPostChannelCount(); i++) {
            ChannelInfo info = _nymChannels.getPublicPostChannel(i);
            TreeItem item = new TreeItem(_postRoot, SWT.NONE);
            item.setText("public: " + info.getName());
            _postChannels.put(item, info);
        }
    }
    private void redrawBookmarks() {
        _bookmarkRoot.removeAll();
        _bookmarkNodes.clear();
        for (int i = 0; i < _nymRefs.size(); i++) {
            NymReferenceNode ref = (NymReferenceNode)_nymRefs.get(i);
            add(_bookmarkRoot, ref);
        }
    }
    private void add(TreeItem parent, NymReferenceNode child) {
        TreeItem childItem = new TreeItem(parent, SWT.NONE);
        childItem.setText(child.getName() + "-" + child.getDescription());
        _bookmarkNodes.put(childItem, child);
        for (int i = 0; i < child.getChildCount(); i++)
            add(childItem, (NymReferenceNode)child.getChild(i));
    }
    private void redrawSearchResults() {
        _searchRoot.removeAll();
        _searchNodes.clear();
        for (int i = 0; i < _searchResults.size(); i++) {
            ReferenceNode node = (ReferenceNode)_searchResults.get(i);
            add(_searchRoot, node);
        }
    }
    private void add(TreeItem parent, ReferenceNode child) {
        TreeItem childItem = new TreeItem(parent, SWT.NONE);
        if (child.getDescription() != null)
            childItem.setText(child.getDescription());
        else 
            childItem.setText(child.getName());
        _searchNodes.put(childItem, child);
        for (int i = 0; i < child.getChildCount(); i++)
            add(childItem, child.getChild(i));
    }

    /** tell the listener about each selected item */
    private void fireSelectionEvents() {
        ChoiceListener lsnr = _choiceListener;
        if (lsnr == null) return;
        
        TreeItem items[] = _tree.getSelection();
        if (items != null) {
            for (int i = 0; i < items.length; i++) {
                NymReferenceNode nref = (NymReferenceNode)_bookmarkNodes.get(items[i]);
                if (nref != null) {
                    lsnr.bookmarkSelected(items[i], nref);
                    continue;
                }
                ReferenceNode ref = (ReferenceNode)_searchNodes.get(items[i]);
                if (ref != null) {
                    lsnr.searchResultSelected(items[i], ref);
                    continue;
                }
                ChannelInfo info = (ChannelInfo)_manageChannels.get(items[i]);
                if (info != null) {
                    lsnr.manageChannelSelected(items[i], info);
                    continue;
                }
                info = (ChannelInfo)_postChannels.get(items[i]);
                if (info != null) {
                    lsnr.postChannelSelected(items[i], info);
                    continue;
                }
                // if reached, its one of the meta-entries (roots, etc)
                lsnr.otherSelected(items[i]);
            }
        }
    }
}
