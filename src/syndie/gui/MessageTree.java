package syndie.gui;

import java.net.URISyntaxException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
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
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
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
public class MessageTree implements Translatable {
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
    private Text _filter;
    private Button _filterApply;
    private Button _filterEdit;
    private SyndieURI _appliedFilter;
    private MessageTreeFilter _filterEditor;
    private Shell _filterEditorShell;
    
    private boolean _showAuthor;
    private boolean _showChannel;
    private boolean _showDate;
    private boolean _showTags;

    private MessageTreeListener _listener;
    private Map _itemToURI;
    
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
        
        createFilterEditor();
        
        Composite filterRow = new Composite(_root, SWT.BORDER);
        filterRow.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        filterRow.setLayout(new GridLayout(4, false));
        _filterLabel = new Label(filterRow, SWT.NONE);
        _filterLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _filter = new Text(filterRow, SWT.SINGLE | SWT.BORDER);
        _filter.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _filter.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) applyFilter();
            }
        });
        
        _filterApply = new Button(filterRow, SWT.PUSH);
        _filterApply.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _filterApply.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { applyFilter(); }
            public void widgetSelected(SelectionEvent selectionEvent) { applyFilter(); }
        });
        _filterEdit = new Button(filterRow, SWT.PUSH);
        _filterEdit.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _filterEdit.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { editFilter(); }
            public void widgetSelected(SelectionEvent selectionEvent) { editFilter(); }
        });
        
        SyndieTreeListener lsnr = new SyndieTreeListener(_tree) {
            public void resized() { resizeCols(); }
            public void selected() { fireSelected(false); }
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
        _filterEditor.dispose();
    }
    
    public void setFilter(SyndieURI searchURI) {
        if (searchURI != null)
            _filter.setText(searchURI.toString());
        else
            _filter.setText("");
    }
    public void setFilter(String filter) {
        if ( (filter != null) && (filter.trim().length() > 0) ) {
            try {
                SyndieURI uri = new SyndieURI(filter);
                _filter.setText(uri.toString());
            } catch (URISyntaxException use) {
                _filter.setText("");
            }
        } else {
            _filter.setText("");
        }
    }
    
    public void applyFilter() {
        final String txt = _filter.getText();
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
                _filter.setText(uri.toString()); // normalize manually edited uris
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
        _browser.getUI().debugMessage("setting the filter");
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

            long chanId = _client.getChannelId(uri.getScope());
            ChannelInfo scopeInfo = _client.getChannel(chanId);
            MessageInfo msg = _client.getMessage(chanId, uri.getMessageId());
            if (msg != null) {
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
                    buf.append(((String)iter.next()).trim()).append(" ");
                }
                tags = buf.toString().trim();
                date = Constants.getDate(msg.getMessageId());
                item.setGrayed(false);
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

    private void editFilter() { 
        String txt = _filter.getText();
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
    private static final String T_FILTER_APPLY = "syndie.gui.messagetree.filter.apply";
    private static final String T_FILTER_EDIT = "syndie.gui.messagetree.filter.edit";
    private static final String T_FILTER_EDIT_SHELL = "syndie.gui.messagetree.filter.edit.shell";
    
    public void translate(TranslationRegistry registry) {
        _colSubject.setText(registry.getText(T_SUBJECT, "Subject"));
        _colAuthor.setText(registry.getText(T_AUTHOR, "Author"));
        _colChannel.setText(registry.getText(T_FORUM, "Forum"));
        _colDate.setText(registry.getText(T_DATE, "Date"));
        _colTags.setText(registry.getText(T_TAGS, "Tags"));
        
        _filterLabel.setText(registry.getText(T_FILTER_LABEL, "Filters: "));
        
        _filterApply.setText(registry.getText(T_FILTER_APPLY, "Apply"));
        _filterEdit.setText(registry.getText(T_FILTER_EDIT, "Edit..."));
        _filterEditorShell.setText(registry.getText(T_FILTER_EDIT_SHELL, "Message filter"));
    }
}
