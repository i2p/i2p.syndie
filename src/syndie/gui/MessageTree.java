package syndie.gui;

import java.net.URISyntaxException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.NullUI;
import syndie.db.ThreadAccumulator;

/**
 * 
 */
public class MessageTree implements Translatable, Themeable {
    private BrowserControl _browser;
    private DBClient _client;
    private Composite _parent;
    private Composite _root;
    private Tree _tree;
    private TreeColumn _colType;
    private TreeColumn _colSubject;
    private TreeColumn _colAuthor;
    private TreeColumn _colChannel;
    private TreeColumn _colDate;
    private TreeColumn _colTags;

    private Label _filterLabel;
    private Combo _filterAge;
    private Label _filterTagLabel;
    private Combo _filterTag;
    private Button _filterAdvanced;
    private String _filter;
    //private Button _filterApply;
    //private Button _filterEdit;
    private SyndieURI _appliedFilter;
    private MessageTreeFilter _filterEditor;
    private Shell _filterEditorShell;
    
    private Menu _menu;
    private MenuItem _view;
    private MenuItem _markRead;
    private MenuItem _markAllRead;
    
    private boolean _showAuthor;
    private boolean _showChannel;
    private boolean _showDate;
    private boolean _showTags;
    
    /** if > 0, the custom date being filtered */
    private long _customDate;
    /** tags applied to the messages being displayed */
    private Set _tags;

    private MessageTreeListener _listener;
    private Map _itemToURI;
    /** items for messages that are old */
    private Set _itemsOld;
    /** items for messages that are new but read */
    private Set _itemsNewRead;
    /** items for messages that are new and unread */
    private Set _itemsNewUnread;
    /** item to MessageInfo */
    private Map _itemToMsg;
    
    public MessageTree(BrowserControl browser, Composite parent, MessageTreeListener lsnr) { this(browser, parent, lsnr, true, true, true, true); }        
    public MessageTree(BrowserControl browser, Composite parent, MessageTreeListener lsnr, boolean showAuthor, boolean showChannel, boolean showDate, boolean showTags) {
        _browser = browser;
        _client = browser.getClient();
        _parent = parent;
        _listener = lsnr;
        _showAuthor = showAuthor;
        _showChannel = showChannel;
        _showDate = showDate;
        _showTags = showTags;
        _itemToURI = new HashMap();
        _itemsOld = new HashSet();
        _itemsNewRead = new HashSet();
        _itemsNewUnread = new HashSet();
        _itemToMsg = new HashMap();
        _tags = new HashSet();
        _customDate = -1;
        initComponents();
    }
    
    public Control getControl() { return _root; } //return _tree; }
    public SyndieURI getSelected() {
        TreeItem items[] = _tree.getSelection();
        if (items != null) {
            for (int i = 0; i < items.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(items[i]);
                if (uri != null)
                    return uri;
            }
        }
        // nothing selected (or at least no uris)
        return null;
    }
    
    public void select(SyndieURI uri) {
        if (uri == null) return;
        for (Iterator iter = _itemToURI.keySet().iterator(); iter.hasNext(); ) {
            TreeItem item = (TreeItem)iter.next();
            SyndieURI cur = (SyndieURI)_itemToURI.get(item);
            if (cur.equals(uri)) {
                _tree.setSelection(item);
                _tree.setTopItem(item);
                return;
            }
        }
    }

    public interface MessageTreeListener {
        /** 
         * @param toView if true, the user wants to fully select the message.
         *        if false, they are just traversing the tree, browsing through messages,
         *        though they'd probably be ok with some contextual info regarding the
         *        selected message
         */
        public void messageSelected(MessageTree tree, SyndieURI uri, boolean toView);
        /** the new filter was applied */
        public void filterApplied(MessageTree tree, SyndieURI searchURI);
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(1, true));
        _tree = new Tree(_root, SWT.BORDER | SWT.SINGLE);
        _tree.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _colSubject = new TreeColumn(_tree, SWT.LEFT);
        _colType = new TreeColumn(_tree, SWT.LEFT);
        _colAuthor = new TreeColumn(_tree, SWT.LEFT);
        _colChannel = new TreeColumn(_tree, SWT.LEFT);
        _colDate = new TreeColumn(_tree, SWT.LEFT);
        _colTags = new TreeColumn(_tree, SWT.LEFT);
        
        _tree.setHeaderVisible(true);
        _tree.setLinesVisible(true);
    
        _menu = new Menu(_tree);
        _tree.setMenu(_menu);
        _view = new MenuItem(_menu, SWT.PUSH);
        _view.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewSelected(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewSelected(); }
        });
        _markRead = new MenuItem(_menu, SWT.PUSH);
        _markRead.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { markRead(); }
            public void widgetSelected(SelectionEvent selectionEvent) { markRead(); }
        });
        _markAllRead = new MenuItem(_menu, SWT.PUSH);
        _markAllRead.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { markAllRead(); }
            public void widgetSelected(SelectionEvent selectionEvent) { markAllRead(); }
        });
        
        createFilterEditor();
        
        Composite filterRow = new Composite(_root, SWT.BORDER);
        filterRow.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        filterRow.setLayout(new GridLayout(5, false));
        _filterLabel = new Label(filterRow, SWT.NONE);
        _filterLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _filterAge = new Combo(filterRow, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.BORDER);
        _filterAge.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _filterAge.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                if (_filterAge.getSelectionIndex() == AGE_CUSTOM)
                    pickDate();
                else
                    applyFilter();
            }
            public void widgetSelected(SelectionEvent selectionEvent) {
                if (_filterAge.getSelectionIndex() == AGE_CUSTOM)
                    pickDate();
                else
                    applyFilter();
            }
        });
        
        populateAgeCombo();
        
        _filterTagLabel = new Label(filterRow, SWT.NONE);
        _filterTagLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _filterTag = new Combo(filterRow, SWT.DROP_DOWN | SWT.BORDER);
        _filterTag.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _filterTag.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) applyFilter();
            }
        });
        _filterTag.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { applyFilter(); }
            public void widgetSelected(SelectionEvent selectionEvent) { applyFilter(); }
        });
        
        populateTagCombo();
        
        _filterAdvanced = new Button(filterRow, SWT.PUSH);
        _filterAdvanced.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _filterAdvanced.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { editFilter(); }
            public void widgetSelected(SelectionEvent selectionEvent) { editFilter(); }
        });
        
        SyndieTreeListener lsnr = new SyndieTreeListener(_tree) {
            public void resized() { resizeCols(); }
            public void selected(boolean rightClick) { /*fireSelected(false);*/ }
            public void returnHit() { fireSelected(true); }
            public void doubleclick() { fireSelected(true); }
            public boolean collapseOnReturn() { return false; }
        };
        _tree.addSelectionListener(lsnr);
        _tree.addControlListener(lsnr);
        _tree.addTraverseListener(lsnr);
        _tree.addKeyListener(lsnr);
        _tree.addMouseListener(lsnr);
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
    }
    
    private void createFilterEditor() {
        _filterEditorShell = new Shell(_parent.getShell(), SWT.SHELL_TRIM | SWT.PRIMARY_MODAL);
        _filterEditorShell.setLayout(new FillLayout());
        // intercept the shell closing, since that'd cause the shell to be disposed rather than just hidden
        _filterEditorShell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; hideFilterEditor(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        _filterEditor = new MessageTreeFilter(_browser, _filterEditorShell, this);
        _filterEditorShell.pack();
        _filterEditorShell.setSize(_filterEditor.getControl().computeSize(400, 400));
    }
    
    public void sortDate(boolean newestFirst) { _tree.setSortColumn(_colDate); _tree.setSortDirection(newestFirst ? SWT.DOWN : SWT.UP); }
    public void sortAuthor(boolean aToZ) { _tree.setSortColumn(_colAuthor); _tree.setSortDirection(aToZ ? SWT.UP : SWT.DOWN); }
    public void sortChannel(boolean aToZ) { _tree.setSortColumn(_colChannel); _tree.setSortDirection(aToZ ? SWT.UP : SWT.DOWN); }
    public void sortSubject(boolean aToZ) { _tree.setSortColumn(_colSubject); _tree.setSortDirection(aToZ ? SWT.UP : SWT.DOWN); }
    public void sortTags(boolean aToZ) { _tree.setSortColumn(_colTags); _tree.setSortDirection(aToZ ? SWT.UP : SWT.DOWN); }
    
    public void showChannel(boolean show) { _showChannel = show; }
    public void showDate(boolean show) { _showDate = show; }
    public void showAuthor(boolean show) { _showAuthor = show; }
    public void showTags(boolean show) { _showTags = show; }
    
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
        _filterEditor.dispose();
    }
    
    public void setFilter(SyndieURI searchURI) {
        if (searchURI != null)
            setFilter(searchURI.toString()); //_filter = searchURI.toString();
        else
            setFilter(""); //_filter = "";
    }
    void setFilter(String filter) {
        SyndieURI uri = null;
        if ( (filter != null) && (filter.trim().length() > 0) ) {
            try {
                uri = new SyndieURI(filter);
                _filter = uri.toString();
            } catch (URISyntaxException use) {
                uri = new SyndieURI("search", new HashMap());
                _filter = "";
            }
        } else {
            _filter = "";
        }
    
        Long days = uri.getLong("age");
        if (days == null)
            days = uri.getLong("agelocal");
        if (days != null) {
            switch (days.intValue()) {
                case 60: _filterAge.select(AGE_LASTMONTH); break;
                case 30: _filterAge.select(AGE_THISMONTH); break;
                case 14: _filterAge.select(AGE_LASTWEEK); break;
                case 7: _filterAge.select(AGE_THISWEEK); break;
                case 2: _filterAge.select(AGE_YESTERDAY); break;
                case 1: _filterAge.select(AGE_TODAY); break;
                default: 
                    _filterAge.setItem(AGE_CUSTOM, Constants.getDate(System.currentTimeMillis()-days.longValue()) + "...");
                    _filterAge.select(AGE_CUSTOM);
                    break;
            }
        }
        String tags[] = uri.getStringArray("tagrequire");
        if ( (tags == null) || (tags.length == 0) ) {
            _filterTag.select(0);
        } else if (_filterTag.indexOf(tags[0]) > 0) {
            _filterTag.select(_filterTag.indexOf(tags[0]));
        } else {
            _filterTag.setText(tags[0]);
        }
    }
    
    private static final int AGE_TODAY = 0;
    private static final int AGE_YESTERDAY = 1;
    private static final int AGE_THISWEEK = 2;
    private static final int AGE_LASTWEEK = 3;
    private static final int AGE_THISMONTH = 4;
    private static final int AGE_LASTMONTH = 5;
    private static final int AGE_CUSTOM = 6;
    private static final int AGE_DEFAULT = AGE_THISWEEK;
    
    private static final String T_AGE_TODAY = "syndie.gui.messagetree.age.today";
    private static final String T_AGE_YESTERDAY = "syndie.gui.messagetree.age.yesterday";
    private static final String T_AGE_THISWEEK = "syndie.gui.messagetree.age.thisweek";
    private static final String T_AGE_LASTWEEK = "syndie.gui.messagetree.age.lastweek";
    private static final String T_AGE_THISMONTH = "syndie.gui.messagetree.age.thismonth";
    private static final String T_AGE_LASTMONTH = "syndie.gui.messagetree.age.lastmonth";
    private static final String T_AGE_CUSTOM = "syndie.gui.messagetree.age.custom";
    
    private void populateAgeCombo() {
        int selected = AGE_DEFAULT;
        if (_filterAge.getItemCount() > 0)
            selected = _filterAge.getSelectionIndex();
        _filterAge.setRedraw(false);
        _filterAge.removeAll();
        _filterAge.add(_browser.getTranslationRegistry().getText(T_AGE_TODAY, "Today"));
        _filterAge.add(_browser.getTranslationRegistry().getText(T_AGE_YESTERDAY, "Since yesterday"));
        _filterAge.add(_browser.getTranslationRegistry().getText(T_AGE_THISWEEK, "This week"));
        _filterAge.add(_browser.getTranslationRegistry().getText(T_AGE_LASTWEEK, "Since last week"));
        _filterAge.add(_browser.getTranslationRegistry().getText(T_AGE_THISMONTH, "This month"));
        _filterAge.add(_browser.getTranslationRegistry().getText(T_AGE_LASTMONTH, "Since last month"));
        if (_customDate > 0)
            _filterAge.add(Constants.getDate(_customDate) + "...");
        else
            _filterAge.add(_browser.getTranslationRegistry().getText(T_AGE_CUSTOM, "Custom date..."));
        _filterAge.select(selected);
        _filterAge.setRedraw(true);
    }
    
    private static final String T_TAG_ALL = "syndie.gui.messagetree.tag.all";
    
    private void populateTagCombo() {
        String txt = _filterTag.getText();
        int selected = -1;
        if ( (_filterTag.getItemCount() > 0) && (txt.trim().length() == 0) )
            selected = _filterTag.getSelectionIndex();
        _browser.getUI().debugMessage("populateTagCombo text=[" + txt + "] selected [" + selected + "]");
        _filterTag.setRedraw(false);
        _filterTag.removeAll();
        TreeSet tags = new TreeSet(_tags);
        _filterTag.add(_browser.getTranslationRegistry().getText(T_TAG_ALL, "Any tags"));
        for (Iterator iter = tags.iterator(); iter.hasNext(); ) {
            String tag = (String)iter.next();
            _filterTag.add(tag);
        }
        if (selected > 0) {
            _filterTag.select(selected);
        } else {
            _filterTag.setText(txt);
            //_filterTag.select(0);
        }
        _filterTag.setRedraw(true);
    }
    
    private String buildFilter() {
        SyndieURI uri = null;
        try {
            if ( (_filter == null) || (_filter.length() <= 0) )
                _filter = SyndieURI.DEFAULT_SEARCH_URI.toString();
            uri = new SyndieURI(_filter);
        } catch (URISyntaxException use) {
            uri = SyndieURI.DEFAULT_SEARCH_URI;
        }
        
        Map attributes = uri.getAttributes();
        int days = 1;
        switch (_filterAge.getSelectionIndex()) {
            case AGE_CUSTOM:
                if (_customDate > 0) {
                    long diff = System.currentTimeMillis() - _customDate;
                    days = (int)((diff+24*60*60*1000l-1) / (24*60*60*1000l));
                }
                break;
            case AGE_LASTMONTH: days = 60; break;
            case AGE_THISMONTH: days = 30; break;
            case AGE_LASTWEEK: days = 14; break;
            case AGE_THISWEEK: days = 7; break;
            case AGE_YESTERDAY: days = 2; break;
            case AGE_TODAY: 
            default:
                days = 1; 
                break;
        }
        attributes.put("age", new Integer(days));
        attributes.put("agelocal", new Integer(days));
        
        String tag = _filterTag.getText().trim();
        if ( (_filterTag.getSelectionIndex() == 0) || 
             ( (_filterTag.getItemCount() > 0) && _filterTag.getItem(0).equals(tag)) ) {
            tag = "";
            attributes.remove("tagrequire");
            attributes.remove("taginclude");
            attributes.remove("tagexclude");
        }
        if (tag.length() > 0) {
            // should this be subsequent to the applied filters, or additive? 
            // the following is additive, but may be confusing...
            if (false) {
                String require[] = (String[])attributes.get("tagrequire");
                if (require == null) {
                    require = new String[] { tag };
                } else {
                    boolean found = false;
                    for (int i = 0; !found && i < require.length; i++)
                        if (require[i].equals(tag))
                            found = true;
                    if (!found) {
                        String req[] = new String[require.length + 1];
                        System.arraycopy(require, 0, req, 0, require.length);
                        req[req.length-1] = tag;
                        require = req;
                    }
                }
                attributes.put("tagrequire", require);
                attributes.remove("taginclude");
                attributes.remove("tagexclude");
            } else {
                attributes.put("tagrequire", new String[] { tag });
                attributes.remove("taginclude");
                attributes.remove("tagexclude");
            }
        }
        
        int idx = _filterTag.indexOf(tag);
        if (idx >= 0)
            _filterTag.select(idx);
        else
            _filterTag.setText(tag);
        String rv = new SyndieURI(uri.getType(), attributes).toString();
        _browser.getUI().debugMessage("building filter w/ new tag [" + tag + "] and age [" + days + "]: " + rv);
        return rv;
    }
    
    public void pickDate() {
        _customDate = 1; // erm, do a popup
        String when = Constants.getDate(_customDate) + "...";
        _filterAge.setItem(AGE_CUSTOM, when);
        _filterAge.setText(when);
        _filterAge.select(AGE_CUSTOM);
        applyFilter();
    }
    
    public void applyFilter() {
        _filter = buildFilter();
        final String txt = _filter;
        if (txt.trim().length() > 0) {
            try {
                final SyndieURI uri = new SyndieURI(txt);
                JobRunner.instance().enqueue(new Runnable() {
                    public void run() {
                        _browser.getUI().debugMessage("begin async calculating nodes in the tree");
                        //try { Thread.sleep(200); } catch (InterruptedException ie) {}
                        applyFilter(txt, uri, calculateNodes(uri)); 
                        _browser.getUI().debugMessage("end async calculating nodes in the tree");
                    }
                });
            } catch (URISyntaxException use) {
                // noop
                //System.out.println("filter applied was not valid, noop [" + use.getMessage() + "]");
            }
        }
    }
    private void applyFilter(final String txt, final SyndieURI uri, final List nodes) {
        _tree.getDisplay().asyncExec(new Runnable() {
            public void run() { 
                _browser.getUI().debugMessage("nodes calculated, setting them");
                setMessages(nodes);
                _browser.getUI().debugMessage("nodes set");
                _appliedFilter = uri;
                _filter = uri.toString(); // normalize manually edited uris
                if (_listener != null)
                    _listener.filterApplied(MessageTree.this, uri);
            }
        });     
    }
    
    /**
     * actually generate the sorted list of matches
     *
     * @return list of ReferenceNode for the root of each thread, or if it isn't
     *         threaded, simply one per message
     */
    private List calculateNodes(SyndieURI uri) {
        ThreadAccumulator acc = new ThreadAccumulator(_client, _browser.getUI());
        _browser.getUI().debugMessage("setting the filter: " + uri.toString());
        acc.setFilter(uri);
        _browser.getUI().debugMessage("gathering the threads");
        acc.gatherThreads();
        // now sort it...
        _browser.getUI().debugMessage("sorting the threads");
        List threads = new ArrayList();
        for (int i = 0; i < acc.getThreadCount(); i++)
            threads.add(acc.getRootThread(i));
        
        // now sort...
        // (or not...)
        
        return threads;
    }
    
    void setMessages(List referenceNodes) {
        _tree.setRedraw(false);
        _tree.removeAll();
        _itemToURI.clear();
        _itemToMsg.clear();
        _itemsOld.clear();
        _itemsNewRead.clear();
        _itemsNewUnread.clear();
        _tags.clear();
        for (int i = 0; i < referenceNodes.size(); i++) {
            ReferenceNode node = (ReferenceNode)referenceNodes.get(i);
            add(node, null);
        }
        _colType.pack();
        _colSubject.pack();
        _colDate.pack();
        _colChannel.pack();
        _colAuthor.pack();
        _colTags.pack();
        
        populateTagCombo();
        
        resizeCols();
        _tree.setRedraw(true);
    }
    
    private void add(ReferenceNode node, TreeItem parent) {
        TreeItem item = null;
        SyndieURI uri = node.getURI();
        if ( (uri != null) && (uri.getScope() != null) && (uri.getMessageId() != null) ) {
            if (parent == null)
                item = new TreeItem(_tree, SWT.NONE);
            else
                item = new TreeItem(parent, SWT.NONE);

            _itemToURI.put(item, uri);
            
            String subj = "";
            String auth = "";
            String chan = "";
            String date = "";
            String tags = "";
            int status = DBClient.MSG_STATUS_NEW_UNREAD;
            
            long chanId = _client.getChannelId(uri.getScope());
            ChannelInfo scopeInfo = _client.getChannel(chanId);
            MessageInfo msg = _client.getMessage(chanId, uri.getMessageId());
            if (msg != null) {
                _itemToMsg.put(item, msg);
                if (msg.getSubject() != null)
                    subj = msg.getSubject();
                long authorId = msg.getAuthorChannelId();
                if (authorId != chanId) {
                    ChannelInfo authInfo = _client.getChannel(authorId);
                    if (authInfo != null) {
                        auth = authInfo.getName() + " [" + authInfo.getChannelHash().toBase64().substring(0,6) + "]";
                    } else {
                        auth = "";
                    }
                    //System.out.println("author is NOT the scope chan for " + uri.toString() + ": " + auth);
                } else {
                    //System.out.println("author is the scope chan for " + uri.toString());
                    auth = scopeInfo.getName() + " [" + scopeInfo.getChannelHash().toBase64().substring(0,6) + "]";
                }
                ChannelInfo chanInfo = scopeInfo;
                if (msg.getTargetChannelId() != scopeInfo.getChannelId()) {
                    //System.out.println("target chan != scope chan: " + msg.getTargetChannel().toBase64() + "/" + msg.getTargetChannelId() + " vs " + scopeInfo.getChannelHash().toBase64() + "/" + scopeInfo.getChannelId());
                    //System.out.println("msg: " + uri.toString());
                    chanInfo = _client.getChannel(msg.getTargetChannelId());
                    if (chanInfo == null) {
                        chan = "[" + msg.getTargetChannel().toBase64().substring(0,6) + "]";
                    } else {
                        chan = chanInfo.getName() + " [" + chanInfo.getChannelHash().toBase64().substring(0,6) + "]";
                    }
                } else {
                    //System.out.println("target chan == scope chan: " + msg.getTargetChannel().toBase64() + "/" + msg.getTargetChannelId() + "/" + msg.getInternalId() + "/" + msg.getScopeChannelId() + "/" + msg.getAuthorChannelId());
                    //System.out.println("msg: " + uri.toString());
                    chan = chanInfo.getName() + " [" + chanInfo.getChannelHash().toBase64().substring(0,6) + "]";
                }
                
                if (auth.length() <= 0) {
                     auth = chan;
                }
                Set msgTags = new TreeSet();
                msgTags.addAll(msg.getPublicTags());
                msgTags.addAll(msg.getPrivateTags());
                StringBuffer buf = new StringBuffer();
                for (Iterator iter = msgTags.iterator(); iter.hasNext(); ) {
                    String tag = (String)iter.next();
                    tag = tag.trim();
                    buf.append(tag).append(" ");
                    _tags.add(tag);
                }
                tags = buf.toString().trim();
                date = Constants.getDate(msg.getMessageId());
                item.setGrayed(false);
                
                status = _client.getMessageStatus(_client.getLoggedInNymId(), msg.getInternalId(), msg.getTargetChannelId());
            } else {
                // message is not locally known
                subj = "";
                if (scopeInfo != null)
                    auth = scopeInfo.getName() + " [" + scopeInfo.getChannelHash().toBase64().substring(0,6) + "]";
                else
                    auth = "[" + uri.getScope().toBase64().substring(0,6) + "]";
                chan = "";
                date = Constants.getDate(uri.getMessageId().longValue());
                tags = "";
            }
            item.setText(0, subj);
            if (msg.getWasPrivate())
                item.setImage(1, ImageUtil.ICON_MSG_TYPE_PRIVATE);
            else
                item.setImage(1, ImageUtil.ICON_MSG_TYPE_NORMAL);
            item.setText(2, auth);
            item.setText(3, chan);
            item.setText(4, date);
            item.setText(5, tags);
            if (status == DBClient.MSG_STATUS_OLD) {
                _itemsOld.add(item);
                item.setFont(_browser.getThemeRegistry().getTheme().MSG_OLD_FONT);
            } else if (status == DBClient.MSG_STATUS_NEW_READ) {
                _itemsNewRead.add(item);
                item.setFont(_browser.getThemeRegistry().getTheme().MSG_NEW_READ_FONT);
            } else {
                _itemsNewUnread.add(item);
                item.setFont(_browser.getThemeRegistry().getTheme().MSG_NEW_UNREAD_FONT);
            }
            _browser.getUI().debugMessage("message status: " + status);
        } else {
            // reference node does not point to a uri, so don't build a row
            item = parent;
        }
        for (int i = 0; i < node.getChildCount(); i++)
            add(node.getChild(i), item);
    }
    
    private void resizeCols() {
        int total = _tree.getClientArea().width;
        int subjWidth = 100;
        
        int chanWidth = 100;
        if (!_showChannel) chanWidth = 0;
        int authWidth = 100;
        if (!_showAuthor) authWidth = 0;
        int dateWidth = 100;
        if (!_showDate) dateWidth = 0;
        int tagsWidth = 100;
        if (!_showTags) tagsWidth = 0;
        
        if (total > subjWidth+chanWidth+authWidth+dateWidth+tagsWidth+24)
            subjWidth = total-chanWidth-authWidth-dateWidth-tagsWidth-24;
        
        _colType.setWidth(24);
        _colSubject.setWidth(subjWidth);
        _colChannel.setWidth(chanWidth);
        _colAuthor.setWidth(authWidth);
        _colDate.setWidth(dateWidth);
        _colTags.setWidth(tagsWidth);
        
        //System.out.println("resize w/ total=" + total + " colSubject=" + _colSubject.getWidth());
    }

    private void fireSelected(boolean toView) {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                if (_listener != null)
                    _listener.messageSelected(this, uri, toView);
            }
        }
    }
    private void viewSelected() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                _browser.view(uri);
            }
        }
    }
    
    private void markRead() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                MessageInfo msg = (MessageInfo)_itemToMsg.get(selected[i]);
                if (msg != null) {
                    if (_itemsOld.contains(selected[i])) {
                        // noop
                    } else {
                        _browser.getClient().markMessageRead(msg.getInternalId());
                        selected[i].setFont(_browser.getThemeRegistry().getTheme().MSG_NEW_READ_FONT);
                        _itemsNewUnread.remove(selected[i]);
                        _itemsOld.remove(selected[i]);
                        _itemsNewRead.add(selected[i]);
                    }
                }
            }
        }
    }
    private void markAllRead() {
        TreeItem selected[] = _tree.getSelection();
        if ( (selected != null) && (selected.length > 0) ) {
            for (int i = 0; i < selected.length; i++) {
                MessageInfo msg = (MessageInfo)_itemToMsg.get(selected[i]);
                if (msg != null) {
                    _browser.getClient().markChannelRead(msg.getTargetChannelId());
                }
            }
            _itemsNewRead.clear();
            _itemsNewUnread.clear();
            for (Iterator iter = _itemToURI.keySet().iterator(); iter.hasNext(); ) {
                TreeItem item = (TreeItem)iter.next();
                _itemsOld.add(item);
                item.setFont(_browser.getThemeRegistry().getTheme().MSG_OLD_FONT);
            }
        }
    }

    private void editFilter() { 
        String txt = _filter;
        //System.out.println("showing edit for [" + txt + "]");
        _filterEditor.setFilter(txt);
        _filterEditorShell.open();
    }
    void hideFilterEditor() { _filterEditorShell.setVisible(false); }

    private static final String T_SUBJECT = "syndie.gui.messagetree.subject";
    private static final String T_AUTHOR = "syndie.gui.messagetree.author";
    private static final String T_FORUM = "syndie.gui.messagetree.forum";
    private static final String T_DATE = "syndie.gui.messagetree.date";
    private static final String T_TAGS = "syndie.gui.messagetree.tags";
    private static final String T_FILTER_LABEL = "syndie.gui.messagetree.filter.label";
    private static final String T_FILTER_ADVANCED = "syndie.gui.messagetree.filter.advanced";
    private static final String T_FILTER_EDIT_SHELL = "syndie.gui.messagetree.filter.edit.shell";
    private static final String T_FILTER_TAG = "syndie.gui.messagetree.filter.tag";
    
    private static final String T_VIEW = "syndie.gui.messagetree.view";
    private static final String T_MARKREAD = "syndie.gui.messagetree.markread";
    private static final String T_MARKALLREAD = "syndie.gui.messagetree.markallread";
    
    public void translate(TranslationRegistry registry) {
        _colSubject.setText(registry.getText(T_SUBJECT, "Subject"));
        _colAuthor.setText(registry.getText(T_AUTHOR, "Author"));
        _colChannel.setText(registry.getText(T_FORUM, "Forum"));
        _colDate.setText(registry.getText(T_DATE, "Date"));
        _colTags.setText(registry.getText(T_TAGS, "Tags"));
        
        _filterLabel.setText(registry.getText(T_FILTER_LABEL, "Filters:"));
        
        _filterAdvanced.setText(registry.getText(T_FILTER_ADVANCED, "Advanced..."));
        _filterTagLabel.setText(registry.getText(T_FILTER_TAG, "Tag:"));
        _filterEditorShell.setText(registry.getText(T_FILTER_EDIT_SHELL, "Message filter"));

        _view.setText(registry.getText(T_VIEW, "View the messages"));
        _markRead.setText(registry.getText(T_MARKREAD, "Mark the message as read"));
        _markAllRead.setText(registry.getText(T_MARKALLREAD, "Mark all messages as read"));

        populateAgeCombo();
        populateTagCombo();
    }
    
    public void applyTheme(Theme theme) {
        _tree.setFont(theme.TREE_FONT);
        _filterAdvanced.setFont(theme.BUTTON_FONT);
        _filterLabel.setFont(theme.DEFAULT_FONT);
        _filterAge.setFont(theme.DEFAULT_FONT);
        _filterTagLabel.setFont(theme.DEFAULT_FONT);
        _filterTag.setFont(theme.DEFAULT_FONT);
        _filterEditorShell.setFont(theme.SHELL_FONT);
        _root.layout(true, true);
        _filterLabel.getParent().layout(true);
        _filterEditorShell.layout(true, true);
        for (Iterator iter = _itemsOld.iterator(); iter.hasNext(); ) {
            TreeItem item = (TreeItem)iter.next();
            item.setFont(_browser.getThemeRegistry().getTheme().MSG_OLD_FONT);
        }
        for (Iterator iter = _itemsNewRead.iterator(); iter.hasNext(); ) {
            TreeItem item = (TreeItem)iter.next();
            item.setFont(_browser.getThemeRegistry().getTheme().MSG_NEW_READ_FONT);
        }
        for (Iterator iter = _itemsNewUnread.iterator(); iter.hasNext(); ) {
            TreeItem item = (TreeItem)iter.next();
            item.setFont(_browser.getThemeRegistry().getTheme().MSG_NEW_UNREAD_FONT);
        }
    }
}
