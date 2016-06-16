package syndie.gui;

import java.net.URISyntaxException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;

import net.i2p.data.Hash;

import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.MenuListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;

import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.NullUI;
import syndie.db.UI;
import syndie.thread.MessageIterator;
import syndie.thread.ThreadAccumulator;
import syndie.thread.ThreadAccumulatorJWZ;
import syndie.thread.TreeMessageIterator;
import syndie.thread.ThreadReferenceNode;
import syndie.util.DateTime;

/**
 *  Includes page nav buttons at the top, the Tree in the middle, and the message/thread buttons at the bottom.
 *
 *  See also WatchedMessageTree extension
 */
public class MessageTree extends BaseComponent implements Translatable, Themeable, DBClient.MessageStatusListener {
    private final NavigationControl _navControl;
    private final DataCallback _dataCallback;
    private final BookmarkControl _bookmarkControl;
    private final Composite _parent;
    private Composite _root;
    private Composite _top;
    private Button _navStart;
    private Button _navPrev;
    private Label _navState;
    private Label _navPageSizeLabel;
    private Spinner _navPageSize;
    private Button _navNext;
    private Button _navEnd;
    protected Tree _tree;
    protected TreeColumn _colSubject;
    private TreeColumn _colAuthor;
    private TreeColumn _colChannel;
    private TreeColumn _colDate;
    private TreeColumn _colTags;
    private Composite _bottom;
    private Button _markMessageReadButton;
    private Button _markThreadReadButton;
    private Button _refreshButton;
    private Button _expandThreadButton;
    private Button _collapseThreadButton;
    private Button _expandAllThreadButton;
    private Button _collapseAllThreadButton;

    //private FilterBar _filterBar;
    /*
    private Label _filterLabel;
    private Combo _filterAge;
    private Label _filterTagLabel;
    private Combo _filterTag;
    private Button _filterUnreadOnly;
    private Button _filterAdvanced;
     */
    private String _filter;
    //private Button _filterApply;
    //private Button _filterEdit;
    private SyndieURI _appliedFilter;
    private Shell _filterEditorShell;
    
    private Menu _menu;
    private MenuItem _view;
    private MenuItem _reply;
    private MenuItem _viewForum;
    private MenuItem _viewForumMeta;
    private MenuItem _bookmarkForum;
    private MenuItem _viewAuthor;
    private MenuItem _viewAuthorMeta;
    private MenuItem _bookmarkAuthor;
    private MenuItem _expandThread;
    private MenuItem _collapseThread;
    private MenuItem _expandAllThread;
    private MenuItem _collapseAllThread;
    private MenuItem _markRead;
    private MenuItem _markUnread;
    private MenuItem _markThreadRead;
    private MenuItem _markAllRead;
    private MenuItem _delete;
    private MenuItem _cancel;
    
    private boolean _showAuthor;
    private boolean _showChannel;
    private boolean _showDate;
    private boolean _showTags;
    private boolean _showFlags;
    private final boolean _expandRoots;
    private final boolean _expandAll;
    
    private SyndieURI _highlight;
    
    /** tags applied to the messages being displayed */
    private final Set _tags;

    private final MessageTreeListener _listener;
    private final Map<TreeItem, SyndieURI> _itemToURI;
    /** items for messages that are new and unread */
    private final Set<TreeItem> _itemsNewUnread;
    /** item to msgId (Long) */
    private final Map<TreeItem, Long> _itemToMsgId;
    /** msgId (Long) to item */
    private final Map<Long, TreeItem> _msgIdToItem;
    /** ordered list of ReferenceNode instances describing the tree */
    private List<ReferenceNode> _threadReferenceNodes;
    /** TreeItem to ReferenceNode */
    protected final Map<TreeItem, ReferenceNode> _itemToNode;
    
    /** orered list of ReferenceNodes matching the filter - aka all pages of the tree */
    private List<ReferenceNode> _fullNodes;
    private int _currentPage;
    
    /** column we are sorting on */
    private TreeColumn _currentSortColumn;
    /** SWT.UP or SWT.DOWN */
    private int _currentSortDirection;
    
    /** list of FilterBar instances added to this tree */
    private final List<FilterBar> _bars;
    private boolean _hideFilter;
    
    private boolean _filterable;
    
    /** cached avg char width for the text in the tree */
    private int _avgCharWidth;
    
    private MessageTreePreview _preview;
    
    final private BanControl _banControl;
    
    public MessageTree(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, BanControl ban,
                       NavigationControl navControl, URIControl uriControl, BookmarkControl bookmarkControl,
                       DataCallback callback, Composite parent, MessageTreeListener lsnr) {
        this(client, ui, themes, trans, ban, navControl, uriControl, bookmarkControl, callback, parent, lsnr, false);
    }

    public MessageTree(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, BanControl ban,
                       NavigationControl navControl, URIControl uriControl, BookmarkControl bookmarkControl,
                       DataCallback callback, Composite parent, MessageTreeListener lsnr, boolean hideFilter) {
        this(client, ui, themes, trans, ban, navControl, uriControl, bookmarkControl, callback, parent, lsnr, true,
             true, true, true, hideFilter, true, true, true);
    }

    public MessageTree(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, BanControl ban,
                       NavigationControl navControl, URIControl uriControl, BookmarkControl bookmarkControl,
                       DataCallback callback, Composite parent, MessageTreeListener lsnr, boolean showAuthor,
                       boolean showChannel, boolean showDate, boolean showTags, boolean hideFilter,
                       boolean showFlags, boolean expandRoots, boolean expandAll) {
        super(client, ui, themes, trans);
        _banControl = ban;
        _navControl = navControl;
        _bookmarkControl = bookmarkControl;
        _dataCallback = callback;
        if (callback == null)
            throw new IllegalArgumentException("callback is required");
        _parent = parent;
        _listener = lsnr;
        _showAuthor = showAuthor;
        _showChannel = showChannel;
        _showDate = showDate;
        _showTags = showTags;
        _showFlags = false; //showFlags; // disabled for now, until we need them
        _hideFilter = hideFilter;
        _expandRoots = expandRoots;
        _expandAll = expandAll;
        _itemToURI = new HashMap();
        _itemsNewUnread = new HashSet();
        _itemToMsgId = new HashMap();
        _msgIdToItem = new HashMap();
        _itemToNode = new HashMap();
        _tags = new HashSet();
        _bars = new ArrayList();
        _filterable = true;
        _avgCharWidth = -1;
        initComponents();
    }
    
    /** currently applied filter */
    public SyndieURI getCurrentFilter() { return _appliedFilter; }

    public Control getControl() { return _root; } //return _tree; }

    Tree getTree() { return _tree; }

    /** uri to the currently selected message/item */
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
    public SyndieURI getMouseoverURI() {
        Point cur = Display.getDefault().getCursorLocation();
        Point local = _tree.toControl(cur);
        TreeItem item = _tree.getItem(local);
        _ui.debugMessage("getMouseoverURI: cur= " + cur + " local=" + local + " item=" + item);
        if (item != null)
            return (SyndieURI)_itemToURI.get(item);
        else
            return null;
    }
    public Point getSelectedLocation() {
        TreeItem items[] = _tree.getSelection();
        if (items != null) {
            for (int i = 0; i < items.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(items[i]);
                if (uri != null) {
                    Rectangle rect = items[i].getBounds();
                    Point rv = new Point(rect.x, rect.y + rect.height); // underneath it 
                    return _tree.toDisplay(rv);
                }
            }
        }
        return Display.getDefault().getCursorLocation();
    }
    
    void focusOnMessages() { _tree.forceFocus(); }
    
    public void selectNext() {
        // todo
    }
    public void selectPrev() {
        // todo
    }
    
    public void select(SyndieURI uri) {
        _highlight = uri;
        renderHighlight();
        show(uri);
    }
    private void renderHighlight() {
        if (_highlight == null) return;
        for (Iterator iter = _itemToURI.keySet().iterator(); iter.hasNext(); ) {
            TreeItem item = (TreeItem)iter.next();
            SyndieURI cur = (SyndieURI)_itemToURI.get(item);
            if ( (cur != null) && (cur.equals(_highlight)) ) {
                renderHighlight(item);
                return;
            }
        }
    }
    private TreeItem _prevHighlight;
    private void renderHighlight(TreeItem item) {
        if ( (_prevHighlight != null) && (!_prevHighlight.isDisposed()) ) {
            _prevHighlight.setBackground(null);
            _prevHighlight.setForeground(null);
        }
        if (item != null) {
            ColorUtil.init();
            item.setBackground(ColorUtil.getColor("yellow"));
            item.setForeground(ColorUtil.getColor("black"));
        }
        _prevHighlight = item;
    }
    
    private void show(SyndieURI uri) {
        if (uri == null) return;
        for (Iterator iter = _itemToURI.keySet().iterator(); iter.hasNext(); ) {
            TreeItem item = (TreeItem)iter.next();
            SyndieURI cur = (SyndieURI)_itemToURI.get(item);
            if ( (cur != null) && (cur.equals(uri)) ) {
                _tree.setSelection(item);
                _tree.showItem(item);
                //_tree.setTopItem(item);
                return;
            }
        }
    }
    
    public void createFilterBar(Composite row, PreviewControlListener lsnr) {
        FilterBar bar = new FilterBar(_client, _ui, _themeRegistry, _translationRegistry, _navControl, URIHelper.instance(), this, row, lsnr);
        _bars.add(bar);
    }

    public interface MessageTreeListener {
        /** 
         * @param toView if true, the user wants to fully select the message.
         *        if false, they are just traversing the tree, browsing through messages,
         *        though they'd probably be ok with some contextual info regarding the
         *        selected message
         */
        public void messageSelected(MessageTree tree, SyndieURI uri, boolean toView, boolean nodelay);
        /** the new filter was applied */
        public void filterApplied(MessageTree tree, SyndieURI searchURI);
    }
    
    public static interface PreviewControlListener {
        public void togglePreview(boolean shouldShow);
    }
    
    public static boolean shouldShowPreview(DBClient client) {
        Properties prefs = client.getNymPrefs();
        return ( (prefs != null) && (prefs.containsKey("showPreview")) && (Boolean.valueOf(prefs.getProperty("showPreview")).booleanValue()));
    }
    public static void setShouldShowPreview(DBClient client, boolean shouldShow) {
        Properties prefs = client.getNymPrefs();
        prefs.setProperty("showPreview", shouldShow ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
        client.setNymPrefs(prefs);
    }

    /** default false */
    public static boolean shouldUseImportDate(DBClient client) {
        Properties prefs = client.getNymPrefs();
        return (prefs != null &&  Boolean.parseBoolean(prefs.getProperty("browse.useImportDate")));
    }

    /** default false */
    static void setShouldUseImportDate(DBClient client, boolean useImportDate) {
        Properties prefs = client.getNymPrefs();
        prefs.setProperty("browse.useImportDate", useImportDate ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
        client.setNymPrefs(prefs);
    }
 
    /** default true */
    public static boolean shouldMarkReadOnView(DBClient client) {
        Properties prefs = client.getNymPrefs();
        return ( (prefs == null) || (!prefs.containsKey("browse.markReadOnView")) || (Boolean.valueOf(prefs.getProperty("browse.markReadOnView")).booleanValue()));
    }

    /** default true */
    static void setShouldMarkReadOnView(DBClient client, boolean markReadOnView) {
        Properties prefs = client.getNymPrefs();
        prefs.setProperty("browse.markReadOnView", markReadOnView ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
        client.setNymPrefs(prefs);
    }
    
    static boolean shouldMarkReadOnPreview(DBClient client) {
        Properties prefs = client.getNymPrefs();
        return ( (prefs != null) && (prefs.containsKey("browse.markReadOnPreview")) && (Boolean.valueOf(prefs.getProperty("browse.markReadOnPreview")).booleanValue()));
    }
    static void setShouldMarkReadOnPreview(DBClient client, boolean markReadOnPreview) {
        Properties prefs = client.getNymPrefs();
        prefs.setProperty("browse.markReadOnPreview", markReadOnPreview ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
        client.setNymPrefs(prefs);
    }
    
    private static class FilterBar extends BaseComponent implements Translatable, Themeable {
        private NavigationControl _barNavControl;
        private URIControl _barURIControl;
        private Composite _filterRow;
        private MessageTree _msgTree;
        private Label _filterLabel;
        private Combo _filterAge;
        private Label _filterKeywordLabel;
        private Text _filterKeyword;
        private Label _filterTagLabel;
        private Combo _filterTag;
        private Button _filterUnreadOnly;
        private Button _filterAdvanced;
        private Menu _advancedMenu;
        private MenuItem _advancedScopeAll;
        private MenuItem _advancedScopeBookmarked;
        private MenuItem _advancedScopeOther;
        private MenuItem _advancedPrivacyPublic;
        private MenuItem _advancedPrivacyAuthorized;
        private MenuItem _advancedPrivacyPBE;
        private MenuItem _advancedPrivacyPrivate;
        private MenuItem _advancedDateImport;
        private MenuItem _advancedThreadResults;
        private MenuItem _advancedPreview;
        private MenuItem _advancedMarkReadOnView;
        private MenuItem _advancedMarkReadOnPreview;
        private MenuItem _advancedPassphraseRequired;
        private ReferenceChooserPopup _forumChooser;
        private int _age; /* max age, in days, to display */
        private Hash _forumScopeOther[];
        private PreviewControlListener _listener;
        
        private static final int DEFAULT_AGE_DAYS = 3653;

        public FilterBar(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl navControl, URIControl uriControl, MessageTree msgTree, Composite bar, PreviewControlListener lsnr) {
            super(client, ui, themes, trans);
            _barNavControl = navControl;
            _barURIControl = uriControl;
            _msgTree = msgTree;
            _filterRow = bar;
            _age = DEFAULT_AGE_DAYS;
            _listener = lsnr;
            initBar();
        }
        private void initBar() {
            _filterLabel = new Label(_filterRow, SWT.NONE);
            _filterLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
            
            _filterAge = new Combo(_filterRow, SWT.DROP_DOWN | SWT.BORDER);
            _filterAge.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
            _filterAge.addSelectionListener(new SelectionListener() {
                public void widgetSelected (SelectionEvent e) {
                    switch (_filterAge.getSelectionIndex()) {
                        case AGE_10YEAR: _age = 3653; break;
                        case AGE_5YEAR: _age = 1826; break;
                        case AGE_3YEAR: _age = 1095; break;
                        case AGE_YEAR: _age = 365; break;
                        case AGE_6MONTH: _age = 183; break;
                        case AGE_LASTMONTH: _age = 60; break;
                        case AGE_THISMONTH: _age = 30; break;
                        case AGE_LASTWEEK: _age = 14; break;
                        case AGE_THISWEEK: _age = 7; break;
                        case AGE_YESTERDAY: _age = 2; break;
                        case AGE_TODAY: _age = 1; break;
                        default:
                            _age = DEFAULT_AGE_DAYS;
                            break;
                    }
                    
                    //_ui.debugMessage("filter age selected: " + _age + " custom date: " + DateTime.getDate(System.currentTimeMillis()-(_age-1)*24l*60l*60l*1000l));
                    
                    _msgTree.applyFilter();
                }
                
                public void widgetDefaultSelected (SelectionEvent e) {
                    datePicked(_filterAge);
                }
            });
            _filterAge.addTraverseListener(new TraverseListener() {
                public void keyTraversed(TraverseEvent e) {
                    if (e.detail == SWT.TRAVERSE_RETURN) {
                        e.doit = false;
                        e.detail = SWT.TRAVERSE_NONE;
                    }
                }
            });
            populateAgeCombo();
            
            _filterKeywordLabel = new Label(_filterRow, SWT.NONE);
            _filterKeywordLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));

            _filterKeyword = new Text(_filterRow, SWT.BORDER | SWT.SINGLE);
            _filterKeyword.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));
            _filterKeyword.addTraverseListener(new TraverseListener() {
                public void keyTraversed(TraverseEvent evt) { 
                    if (evt.detail == SWT.TRAVERSE_RETURN)
                        _msgTree.applyFilter();
                }
            });
            
            _filterTagLabel = new Label(_filterRow, SWT.NONE);
            _filterTagLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));

            _filterTag = new Combo(_filterRow, SWT.DROP_DOWN | SWT.BORDER);
            _filterTag.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));
            _filterTag.addTraverseListener(new TraverseListener() {
                public void keyTraversed(TraverseEvent evt) {
                    if (evt.detail == SWT.TRAVERSE_RETURN)
                        _msgTree.applyFilter();
                }
            });
            _filterTag.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { _msgTree.applyFilter(); }
                public void widgetSelected(SelectionEvent selectionEvent) { _msgTree.applyFilter(); }
            });

            _filterUnreadOnly = new Button(_filterRow, SWT.CHECK);
            _filterUnreadOnly.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { _msgTree.applyFilter(); }
                public void widgetSelected(SelectionEvent selectionEvent) { _msgTree.applyFilter(); }
            });

            _filterAdvanced = new Button(_filterRow, SWT.PUSH);
            _filterAdvanced.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
            _filterAdvanced.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { _advancedMenu.setVisible(true); }
                public void widgetSelected(SelectionEvent selectionEvent) { _advancedMenu.setVisible(true); }
            });

            _advancedMenu = new Menu(_filterAdvanced);
            _filterAdvanced.setMenu(_advancedMenu);
            
            _advancedScopeAll = new MenuItem(_advancedMenu, SWT.RADIO);
            _advancedScopeBookmarked = new MenuItem(_advancedMenu, SWT.RADIO);
            _advancedScopeOther = new MenuItem(_advancedMenu, SWT.RADIO);
            new MenuItem(_advancedMenu, SWT.SEPARATOR);
            _advancedPrivacyPublic = new MenuItem(_advancedMenu, SWT.CHECK);
            _advancedPrivacyAuthorized = new MenuItem(_advancedMenu, SWT.CHECK);
            _advancedPrivacyPBE = new MenuItem(_advancedMenu, SWT.CHECK);
            _advancedPrivacyPrivate = new MenuItem(_advancedMenu, SWT.CHECK);
            new MenuItem(_advancedMenu, SWT.SEPARATOR);
            _advancedThreadResults = new MenuItem(_advancedMenu, SWT.CHECK);
            _advancedPreview = new MenuItem(_advancedMenu, SWT.CHECK);
            _advancedMarkReadOnView = new MenuItem(_advancedMenu, SWT.CHECK);
            _advancedMarkReadOnPreview = new MenuItem(_advancedMenu, SWT.CHECK);
            _advancedPreview.setEnabled(true);
            if (_listener == null) {
                _advancedMarkReadOnPreview.setEnabled(false);
            }
            new MenuItem(_advancedMenu, SWT.SEPARATOR);
            _advancedDateImport = new MenuItem(_advancedMenu, SWT.CHECK);
            new MenuItem(_advancedMenu, SWT.SEPARATOR);
            _advancedPassphraseRequired = new MenuItem(_advancedMenu, SWT.CHECK);
            
            _advancedScopeAll.setSelection(true);
            _advancedPrivacyPublic.setSelection(true);
            _advancedPrivacyAuthorized.setSelection(true);
            _advancedPrivacyPBE.setSelection(true);
            _advancedPrivacyPrivate.setSelection(true);
            _advancedThreadResults.setSelection(true);
            _advancedPreview.setSelection(shouldShowPreview(_client));
            _advancedMarkReadOnView.setSelection(shouldMarkReadOnView(_client));
            _advancedMarkReadOnPreview.setSelection(shouldMarkReadOnPreview(_client));
            _advancedDateImport.setSelection(shouldUseImportDate(_client));
            
            SelectionListener lsnr = new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { _msgTree.applyFilter(); }
                public void widgetSelected(SelectionEvent selectionEvent) { _msgTree.applyFilter(); }
            };
            
            _advancedScopeAll.addSelectionListener(lsnr);
            _advancedScopeBookmarked.addSelectionListener(lsnr);
            _advancedPrivacyPublic.addSelectionListener(lsnr);
            _advancedPrivacyAuthorized.addSelectionListener(lsnr);
            _advancedPrivacyPBE.addSelectionListener(lsnr);
            _advancedPrivacyPrivate.addSelectionListener(lsnr);
            _advancedThreadResults.addSelectionListener(lsnr);
            //_advancedDateImport.addSelectionListener(lsnr);
            
            _advancedPreview.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    setShouldShowPreview(_client, _advancedPreview.getSelection());
                    if (_listener != null)
                        _listener.togglePreview(_advancedPreview.getSelection());
                }
            });
            _advancedMarkReadOnView.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    setShouldMarkReadOnView(_client, _advancedMarkReadOnView.getSelection());
                }
            });
            _advancedMarkReadOnPreview.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    setShouldMarkReadOnPreview(_client, _advancedMarkReadOnPreview.getSelection());
                }
            });
            
            _advancedDateImport.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    setShouldUseImportDate(_client, _advancedDateImport.getSelection());
                    _msgTree.applyFilter();
                }
            });
            
            _advancedPassphraseRequired.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    if (_advancedPassphraseRequired.getSelection()) {
                        _advancedPrivacyPublic.setSelection(false);
                        _advancedPrivacyAuthorized.setSelection(false);
                        _advancedPrivacyPrivate.setSelection(false);
                        _advancedPrivacyPBE.setSelection(true);
                    }
                    _ui.debugMessage("advanced pbe selected - no more private");
                    _msgTree.applyFilter();
                }
            });
            
            _advancedScopeOther.addSelectionListener(new FireSelectionListener() {
                public void fire() { pickForum(); }
            });
            
            _translationRegistry.register(this);
            _themeRegistry.register(this);
        }
        
        public void dispose() {
            _translationRegistry.unregister(this);
            _themeRegistry.unregister(this);
        }
        
        private void pickForum() {
            if (_forumChooser == null) {
                _forumChooser = ComponentBuilder.instance().createReferenceChooserPopup(_filterRow.getShell());
                //   new ReferenceChooserPopup(_client, _ui, _themeRegistry, _translationRegistry, _filterRow.getShell(), _barNavControl, _barURIControl, 
                _forumChooser.setListener(new ReferenceChooserTree.AcceptanceListener () {
                    public void referenceAccepted(SyndieURI uri) {
                        Hash scope = uri.getScope();
                        _forumScopeOther = new Hash[] { scope };
                        _msgTree.applyFilter();
                    }
                    public void referenceChoiceAborted() {}
                });
            }
            _forumChooser.show();
        }

        
        
        private void datePicked(final Combo date) {
            String when = date.getText();
            try {
                synchronized (_fmt) {
                    Date val = DateTime.parseDateTime(when);
                    long customDate = val.getTime();
                    
                    // calculate age in days from customDate
                    long diff = System.currentTimeMillis() - customDate;
                    _age = (int)((diff+24*60*60*1000l-1) / (24*60*60*1000l));
                    
                    /* String reparsed = */ DateTime.getDate(customDate);
                    _msgTree.applyFilter();
                    /* String afterApply = */ DateTime.getDate(customDate);
                }
            } catch (ParseException pe) {
                MessageBox box = new MessageBox(_filterRow.getShell(), SWT.ICON_ERROR | SWT.OK);
                box.setText(getText("Bad date"));
                box.setMessage(getText("Error parsing date") + ": " + pe.getMessage());
                box.open();
                date.forceFocus();
            }
        }
        
        private static final SimpleDateFormat _fmt = new SimpleDateFormat("yyyy/MM/dd", Locale.UK);

        private static final String PRIVATE = "private";
        
        public void setFilter(SyndieURI uri) {
            Long days = null;
            if (uri != null) { 
                days = uri.getLong("age");
                if (days == null)
                    days = uri.getLong("agelocal");
            }
            if (days != null)
                _filterAge.setText(DateTime.getDateTime(System.currentTimeMillis()-(days.longValue())*24*60*60*1000l));
            String tags[] = null;
            if (uri != null)
                tags = uri.getStringArray("tagrequire");
            if ( (tags == null) || (tags.length == 0) ) {
                _filterTag.select(0);
            } else if (_filterTag.indexOf(tags[0]) > 0) {
                _filterTag.select(_filterTag.indexOf(tags[0]));
            } else {
                _filterTag.setText(tags[0]);
            }
            
            if (uri != null) { 
                if (uri.isChannel() && (uri.getScope() != null) )
                    _forumScopeOther = new Hash[] { uri.getScope() };
                else if (uri.isSearch())
                     _forumScopeOther = uri.getSearchScopes();
                _filterUnreadOnly.setSelection(uri.getBoolean("unreadonly", false));
                _ui.debugMessage("filterBar.setFilter has private? (" + uri.getString(PRIVATE) + "): "+ uri + ")");
                _advancedPrivacyPBE.setSelection(uri.getBoolean("pbe", true));
                _advancedPrivacyAuthorized.setSelection(uri.getBoolean("authorized", true));
                _advancedPrivacyPublic.setSelection(uri.getBoolean("public", true));
            } else {
                _filterUnreadOnly.setSelection(false);
                _advancedPrivacyPBE.setSelection(true);
                _advancedPrivacyAuthorized.setSelection(true);
                _advancedPrivacyPublic.setSelection(true);
            }
            _advancedScopeAll.setSelection(false);
            _advancedScopeBookmarked.setSelection(false);
            _advancedScopeOther.setSelection(false);
            if (_forumScopeOther != null)
                _advancedScopeOther.setSelection(true);
            else
                _advancedScopeAll.setSelection(true);
            
            _advancedPrivacyPrivate.setSelection(true); //uri.getBoolean("private", true));
        }
        
        
        private static final int AGE_TODAY = 0;
        private static final int AGE_YESTERDAY = 1;
        private static final int AGE_THISWEEK = 2;
        private static final int AGE_LASTWEEK = 3;
        private static final int AGE_THISMONTH = 4;
        private static final int AGE_LASTMONTH = 5;
        private static final int AGE_6MONTH = 6;
        private static final int AGE_YEAR = 7;
        private static final int AGE_3YEAR = 8;
        private static final int AGE_5YEAR = 9;
        private static final int AGE_10YEAR = 10;
        private static final int AGE_CUSTOM = 11;

        
        private void populateAgeCombo() {
            String [] items = {
                // translators: you may use "Last {0} day" for the singular if that's better
                _translationRegistry.ngettext("Last day", "Last {0} days", 1),
                _translationRegistry.ngettext("Last day", "Last {0} days", 2),
                // translators: you may use "Last {0} week" for the singular if that's better
                _translationRegistry.ngettext("Last week", "Last {0} weeks", 1),
                _translationRegistry.ngettext("Last week", "Last {0} weeks", 2),
                // translators: you may use "Last {0} month" for the singular if that's better
                _translationRegistry.ngettext("Last month", "Last {0} months", 1),
                _translationRegistry.ngettext("Last month", "Last {0} months", 2),
                _translationRegistry.ngettext("Last month", "Last {0} months", 6),
                // translators: you may use "Last {0} year" for the singular if that's better
                _translationRegistry.ngettext("Last year", "Last {0} years", 1),
                _translationRegistry.ngettext("Last year", "Last {0} years", 3),
                _translationRegistry.ngettext("Last year", "Last {0} years", 5),
                _translationRegistry.ngettext("Last year", "Last {0} years", 10),
            };
            
            _filterAge.setItems(items);
        }
        

        private void populateTagCombo() {
            String txt = _filterTag.getText().trim();
            int selected = -1;
            //if ( (_filterTag.getItemCount() > 0) && (txt.trim().length() == 0) )
            //    selected = _filterTag.getSelectionIndex();
            if ( (_filterTag.getItemCount() > 0) && (txt.length() > 0) )
                selected = _filterTag.indexOf(txt);
            //_ui.debugMessage("populateTagCombo text=[" + txt + "] selected [" + selected + "]");
            _filterTag.setRedraw(false);
            _filterTag.removeAll();
            TreeSet tags = new TreeSet(_msgTree.getTags());
            _filterTag.add(getText("Any tags"));
            for (Iterator iter = tags.iterator(); iter.hasNext(); ) {
                String tag = ((String)iter.next()).trim();
                if (tag.length() > 0)
                    _filterTag.add(tag);
            }
            if (selected >= 0) {
                _filterTag.select(selected);
                _filterTag.setText(txt);
            } else {
                _filterTag.setText(txt);
                //_filterTag.select(0);
            }
            _filterTag.setRedraw(true);
        }

        private String buildFilter() {
            SyndieURI uri = null;
            String filter = _msgTree.getFilter();
            //_ui.debugMessage("build filter, tree has [" + filter + "]");
            try {
                if ( (filter == null) || (filter.trim().length() <= 0) )
                    filter = SyndieURI.DEFAULT_SEARCH_URI.toString();
                uri = new SyndieURI(filter);
            } catch (URISyntaxException use) {
                //_ui.debugMessage("build filter, tree is invalid [" + filter + "]", use);
                uri = SyndieURI.DEFAULT_SEARCH_URI;
            }

            Map attributes = uri.getAttributes();
            
            //_ui.debugMessage("build filter w/ base attributes: " + attributes);
            
            if (!uri.getBoolean("keepdate", false)) {
                if (_advancedDateImport.getSelection()) {
                    attributes.put("agelocal", Integer.valueOf(_age));
                    attributes.remove("age");
                } else {
                    attributes.put("age", Integer.valueOf(_age));
                    attributes.remove("agelocal");
                }
            }

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

            boolean unreadOnly = _filterUnreadOnly.getSelection();
            if (unreadOnly)
                attributes.put("unreadonly", Boolean.TRUE.toString());
            else
                attributes.remove("unreadonly");
            
            String keyword = _filterKeyword.getText();
            keyword = keyword.trim();
            if (keyword.length() > 0)
                attributes.put("keyword", keyword);
            else
                attributes.remove("keyword");
            
            if (_advancedScopeAll.getSelection()) {
                attributes.put("scope", new String[] { "all" });
            } else if (_advancedScopeOther.getSelection() && (_forumScopeOther != null)) {
                String val[] = new String[_forumScopeOther.length];
                for (int i = 0; i < val.length; i++)
                    val[i] = _forumScopeOther[i].toBase64();
                attributes.put("scope", val);
            } else {
                //attributes.put("scope", getBookmarkedScopes());
                attributes.put("scope", new String[] { "all" });
            }
            
            //_ui.debugMessage("buildFilter scope: " + attributes.get("scope") + " otherScope: " + _forumScopeOther);
            
            if (_advancedPrivacyPBE.getSelection())
                attributes.put("pbe", Boolean.TRUE.toString());
            else
                attributes.put("pbe", Boolean.FALSE.toString());
            if (_advancedPrivacyPrivate.getSelection())
                attributes.put("private", Boolean.TRUE.toString());
            else
                attributes.put("private", Boolean.FALSE.toString());
            _ui.debugMessage("buildFilter: private? (" + _advancedPrivacyPrivate.getSelection() + "): "+ attributes+ ")");
            if (_advancedPrivacyPublic.getSelection())
                attributes.put("public", Boolean.TRUE.toString());
            else
                attributes.put("public", Boolean.FALSE.toString());
            if (_advancedPrivacyAuthorized.getSelection())
                attributes.put("authorized", Boolean.TRUE.toString());
            else
                attributes.put("authorized", Boolean.FALSE.toString());

            if (_advancedPassphraseRequired.getSelection()) {
                attributes.put("encrypted", Boolean.TRUE.toString());
                attributes.put("pbe", Boolean.TRUE.toString());
            } else {
                attributes.remove("encrypted");
            }
            
            if (_advancedThreadResults.getSelection())
                attributes.put("threaded", Boolean.TRUE.toString());
            else
                attributes.put("threaded", Boolean.FALSE.toString());
            
            String rv = new SyndieURI(uri.getType(), attributes).toString();
            //_ui.debugMessage("building filter w/ new tag [" + tag + "] and age [" + days + "]: " + rv);
            return rv;
        }
        
        /*
        private String[] getBookmarkedScopes() {
            List scopes = new ArrayList();
            List nodes = _ctl.getBookmarks();
            for (int i = 0; i < nodes.size(); i++)
                getBookmarkedScopes((NymReferenceNode)nodes.get(i), scopes);
            _ctl.getUI().debugMessage("bookmarked scopes: " + scopes);
            return (String[])scopes.toArray(new String[0]);
        }
        private void getBookmarkedScopes(NymReferenceNode node, List scopes) {
            if (!node.getIsBanned() && !node.getIsIgnored()) {
                SyndieURI uri = node.getURI();
                Hash scope = null;
                if (uri.isChannel())
                    scope = uri.getScope();
                else if (uri.isSearch())
                    scope = uri.getHash("scope");
                if ( (scope != null) && (!scopes.contains(scope.toBase64())) )
                    scopes.add(scope.toBase64());
            } else {
               _ctl.getUI().debugMessage("scope isbanned/ignored: " + node.getIsBanned() +"/"+ node.getIsIgnored() + " " + node.getURI());
            }
            for (int i = 0; i < node.getChildCount(); i++)
                getBookmarkedScopes((NymReferenceNode)node.getChild(i), scopes);
        }
         */
        
        public void translate(TranslationRegistry registry) {
            _filterLabel.setText(getText("Filter") + ":  " + getText("Since") + ':');

            _filterAdvanced.setText(registry.getText("Advanced") + "...");
            _filterKeywordLabel.setText(registry.getText("Text") + ':');
            _filterTagLabel.setText(registry.getText("Tag") + ':');
            _filterUnreadOnly.setText(registry.getText("Unread only"));
            _filterKeyword.setToolTipText(registry.getText("Search pages and subjects for the phrase"));
            
            _advancedScopeAll.setText(registry.getText("All forums"));
            _advancedScopeBookmarked.setText(registry.getText("Bookmarked forums"));
            _advancedScopeOther.setText(registry.getText("Specific forum") + "...");
            
            _advancedPrivacyPublic.setText(registry.getText("Readable by: anyone"));
            _advancedPrivacyAuthorized.setText(registry.getText("Readable by: authorized readers"));
            _advancedPrivacyPBE.setText(registry.getText("Readable by: those with a passphrase"));
            _advancedPrivacyPrivate.setText(registry.getText("Readable by: forum administrators"));
            _advancedDateImport.setText(registry.getText("Use local import date instead of (unreliable) message creation date"));
            _advancedThreadResults.setText(registry.getText("Organize results in threads"));
            _advancedPreview.setText(registry.getText("Show a preview of the selected message"));
            _advancedMarkReadOnView.setText(registry.getText("Mark messages read when opened"));
            _advancedMarkReadOnPreview.setText(registry.getText("Mark messages read when previewed"));
            _advancedPassphraseRequired.setText(registry.getText("Messages requiring a new passphrase"));
            
            populateAgeCombo();
            populateTagCombo();
        }
        
        public void applyTheme(Theme theme) {
            _filterAdvanced.setFont(theme.BUTTON_FONT);
            _filterLabel.setFont(theme.DEFAULT_FONT);
            _filterAge.setFont(theme.DEFAULT_FONT);
            _filterKeywordLabel.setFont(theme.DEFAULT_FONT);
            _filterKeyword.setFont(theme.DEFAULT_FONT);
            _filterTagLabel.setFont(theme.DEFAULT_FONT);
            _filterTag.setFont(theme.DEFAULT_FONT);
            _filterUnreadOnly.setFont(theme.DEFAULT_FONT);
            _filterRow.layout(true);
        }
    }
    
    private Set getTags() { return _tags; }
    private String getFilter() { return _filter; }
    
    private static final int FLAG_SPACING = 2;
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.verticalSpacing = 0;
        _root.setLayout(gl);
        
        _top = new Composite(_root, SWT.NONE);
        _top.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _top.setLayout(new GridLayout(7, false));
        
        _navStart = new Button(_top, SWT.PUSH);
        _navStart.setText("<<");
        _navStart.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _navStart.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                _currentPage = 0;
                setMessages(_fullNodes, false);
            }
        });
        _navPrev = new Button(_top, SWT.PUSH);
        _navPrev.setText("<");
        _navPrev.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _navPrev.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                _currentPage = Math.max(0, _currentPage-1);
                setMessages(_fullNodes, false);
            }
        });
        
        _navState = new Label(_top, SWT.NONE);
        _navState.setLayoutData(new GridData(GridData.CENTER, GridData.CENTER, true, false));
        
        _navPageSizeLabel = new Label(_top, SWT.NONE);
        _navPageSizeLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _navPageSize = new Spinner(_top, SWT.DROP_DOWN | SWT.BORDER); // editable
        _navPageSize.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _navPageSize.setDigits(0);
        _navPageSize.setIncrement(5);
        _navPageSize.setMaximum(200);
        _navPageSize.setMinimum(0);
        _navPageSize.setSelection(20);
        _navPageSize.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) {
                    _currentPage = 0;
                    setMessages(_fullNodes, false);
                }
            }
        });
        _navPageSize.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                _currentPage = 0;
                setMessages(_fullNodes, false);
            }
        });
        
        _navNext = new Button(_top, SWT.PUSH);
        _navNext.setText(">");
        _navNext.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _navNext.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                _currentPage = Math.max(0, _currentPage+1);
                setMessages(_fullNodes, false);
            }
        });
        _navEnd = new Button(_top, SWT.PUSH);
        _navEnd.setText(">>");
        _navEnd.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _navEnd.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                int sz = _navPageSize.getSelection();
                if (sz > 0) {
                    int pages = (_fullNodes.size() + sz-1)/sz;
                    _currentPage = Math.max(0, pages-1);
                }
                setMessages(_fullNodes, false);
            }
        });
        
        _tree = new Tree(_root, SWT.BORDER | SWT.SINGLE | SWT.FULL_SELECTION);
        _tree.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));

        /*
        _tree.addListener(SWT.SetData, new Listener() {
            public void handleEvent(Event evt) {
                TreeItem item = (TreeItem)evt.item;
                TreeItem parent = item.getParentItem();
                ReferenceNode itemNode = null;
                if (parent != null) {
                    ReferenceNode parentNode = (ReferenceNode)_itemToNode.get(parent);
                    int childNum = parent.indexOf(item);
                    itemNode = parentNode.getChild(childNum);
                } else {
                    itemNode = (ReferenceNode)_threadReferenceNodes.get(_tree.indexOf(item));
                }
                _itemToNode.put(item, itemNode);
                _itemToURI.put(item, itemNode.getURI());
                _itemToMsgId.put(item, Long.valueOf(itemNode.getUniqueId()));
                renderNode(itemNode, item);
                item.setItemCount(itemNode.getChildCount());
                if ( _expandAll || ( (parent == null) && (_expandRoots) ) ) {
                    if (itemNode.getChildCount() > 0)
                        item.setExpanded(true);
                }
                if ( (_highlight != null) && (itemNode.getURI() != null) )
                    if (_highlight.equals(itemNode.getURI()))
                        renderHighlight(item);
            }
        });
         */
        
        _colSubject = new TreeColumn(_tree, SWT.LEFT);
        _colAuthor = new TreeColumn(_tree, SWT.LEFT);
        _colChannel = new TreeColumn(_tree, SWT.LEFT);
        _colDate = new TreeColumn(_tree, SWT.LEFT);
        _colTags = new TreeColumn(_tree, SWT.LEFT);
        
        _tree.setHeaderVisible(true);
        _tree.setLinesVisible(true);
        
        _currentSortColumn = _colDate;
        _currentSortDirection = SWT.DOWN;
        _tree.setSortColumn(_colDate);
        _tree.setSortDirection(SWT.DOWN);
        
        _colSubject.addListener(SWT.Selection, new Listener() { public void handleEvent(Event evt) { toggleSort(_colSubject); } });
        _colDate.addListener(SWT.Selection, new Listener() { public void handleEvent(Event evt) { toggleSort(_colDate); } });
        _colAuthor.addListener(SWT.Selection, new Listener() { public void handleEvent(Event evt) { toggleSort(_colAuthor); } });
        _colChannel.addListener(SWT.Selection, new Listener() { public void handleEvent(Event evt) { toggleSort(_colChannel); } });
    
        _menu = new Menu(_tree);
        _tree.setMenu(_menu);
        
        _menu.addMenuListener(new MenuListener() {
            public void menuHidden(MenuEvent menuEvent) {}
            public void menuShown(MenuEvent menuEvent) {
                boolean enable = _tree.getSelectionCount() > 0;
                boolean enableMsg = enable && (getMessageSelectedCount() > 0);
                boolean cancellable = enableMsg && getCancellable();
                _bookmarkAuthor.setEnabled(enableMsg);
                _bookmarkForum.setEnabled(enable);
                
                _expandThread.setEnabled(enableMsg && isExpandable(_tree.getSelection()));
                _collapseThread.setEnabled(enableMsg && isCollapsible(_tree.getSelection()));
                _expandAllThread.setEnabled(enable && isExpandable());
                _collapseAllThread.setEnabled(enable && isCollapsible());
                _markAllRead.setEnabled(enable);
                _delete.setEnabled(enableMsg);
                _cancel.setEnabled(cancellable);
                _markRead.setEnabled(enableMsg);
                _markThreadRead.setEnabled(enableMsg);
                _markUnread.setEnabled(enableMsg);
                _view.setEnabled(enableMsg);
                _viewAuthor.setEnabled(enableMsg);
                _viewAuthorMeta.setEnabled(enableMsg);
                
                _viewForum.setEnabled(enable);
                _viewForumMeta.setEnabled(enable);
                if (enableMsg) {
                    SyndieURI uri = getSelected();
                    if (uri != null) {
                        long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                        _reply.setEnabled(MessagePreview.allowedToReply(_client, msgId));
                    } else {
                        _reply.setEnabled(false);
                    }
                } else {
                    _reply.setEnabled(false);
                }
            }
        });
        
        _view = new MenuItem(_menu, SWT.PUSH);
        _view.setImage(ImageUtil.ICON_VIEWMESSAGE);
        _view.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewSelected(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewSelected(); }
        });
        _reply = new MenuItem(_menu, SWT.PUSH);
        _reply.setImage(ImageUtil.ICON_REPLYMESSAGE);
        _reply.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { replySelected(); }
            public void widgetSelected(SelectionEvent selectionEvent) { replySelected(); }
        });
        
        new MenuItem(_menu, SWT.SEPARATOR);
        _viewForum = new MenuItem(_menu, SWT.PUSH);
        _viewForum.setImage(ImageUtil.ICON_FORUMMESSAGES);
        _viewForum.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewSelectedForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewSelectedForum(); }
        });
        _viewForumMeta = new MenuItem(_menu, SWT.PUSH);
        _viewForumMeta.setImage(ImageUtil.ICON_FORUMPROFILE);
        _viewForumMeta.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewSelectedForumMeta(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewSelectedForumMeta(); }
        });
        _bookmarkForum = new MenuItem(_menu, SWT.PUSH);
        _bookmarkForum.setImage(ImageUtil.ICON_ADDBOOKMARK);
        _bookmarkForum.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { bookmarkSelectedForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { bookmarkSelectedForum(); }
        });
        
        new MenuItem(_menu, SWT.SEPARATOR);
        _viewAuthor = new MenuItem(_menu, SWT.PUSH);
        _viewAuthor.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewSelectedAuthor(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewSelectedAuthor(); }
        });
        _viewAuthorMeta = new MenuItem(_menu, SWT.PUSH);
        _viewAuthorMeta.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewSelectedAuthorMeta(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewSelectedAuthorMeta(); }
        });
        _bookmarkAuthor = new MenuItem(_menu, SWT.PUSH);
        _bookmarkAuthor.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { bookmarkSelectedAuthor(); }
            public void widgetSelected(SelectionEvent selectionEvent) { bookmarkSelectedAuthor(); }
        });
        
        new MenuItem(_menu, SWT.SEPARATOR);
        _expandThread = new MenuItem(_menu, SWT.PUSH);
        _expandThread.setImage(ImageUtil.ICON_EXPAND);
        _expandThread.addSelectionListener(new FireSelectionListener() { public void fire() { expandThread(); } });
        _collapseThread = new MenuItem(_menu, SWT.PUSH);
        _collapseThread.setImage(ImageUtil.ICON_COLLAPSE);
        _collapseThread.addSelectionListener(new FireSelectionListener() { public void fire() { collapseThread(); } });

        new MenuItem(_menu, SWT.SEPARATOR);
        _expandAllThread = new MenuItem(_menu, SWT.PUSH);
        _expandAllThread.setImage(ImageUtil.ICON_EXPAND);
        _expandAllThread.addSelectionListener(new FireSelectionListener() { public void fire() { expandAll(); } });
        _collapseAllThread = new MenuItem(_menu, SWT.PUSH);
        _collapseAllThread.setImage(ImageUtil.ICON_COLLAPSE);
        _collapseAllThread.addSelectionListener(new FireSelectionListener() { public void fire() { collapseAll(); } });
        
        new MenuItem(_menu, SWT.SEPARATOR);
        _markRead = new MenuItem(_menu, SWT.PUSH);
        _markRead.setImage(ImageUtil.ICON_MSG);
        _markRead.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { markRead(); }
            public void widgetSelected(SelectionEvent selectionEvent) { markRead(); }
        });
        _markUnread = new MenuItem(_menu, SWT.PUSH);
        _markUnread.setImage(ImageUtil.ICON_UNREADMESSAGE);
        _markUnread.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { markUnread(); }
            public void widgetSelected(SelectionEvent selectionEvent) { markUnread(); }
        });
        _markThreadRead = new MenuItem(_menu, SWT.PUSH);
        _markThreadRead.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { markThreadRead(); }
            public void widgetSelected(SelectionEvent selectionEvent) { markThreadRead(); }
        });
        _markAllRead = new MenuItem(_menu, SWT.PUSH);
        _markAllRead.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { markAllRead(); }
            public void widgetSelected(SelectionEvent selectionEvent) { markAllRead(); }
        });
        
        new MenuItem(_menu, SWT.SEPARATOR);
        _delete = new MenuItem(_menu, SWT.PUSH);
        _delete.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { delete(); }
            public void widgetSelected(SelectionEvent selectionEvent) { delete(); }
        });
        
        _cancel = new MenuItem(_menu, SWT.PUSH);
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { cancel(); }
            public void widgetSelected(SelectionEvent selectionEvent) { cancel(); }
        });
        
        if (!_hideFilter) {
            Composite filterRow = new Composite(_root, SWT.BORDER);
            filterRow.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            filterRow.setLayout(new GridLayout(8, false));
            createFilterBar(filterRow, null);
        }
        
        SyndieTreeListener lsnr = new SyndieTreeListener(_tree) {
            public void selected() { fireSelected(false, false); }
            public void returnHit() { fireSelected(true, true); }
            public void doubleclick() { fireSelected(true, true); }
            public boolean collapseOnReturn() { return false; }
            public void keyReleased(KeyEvent evt) {
                // refresh the pane on ^R
                if ( (_filterable) && (evt.character == 0x12) & ((evt.stateMask & SWT.MOD1) != 0) ) {
                    applyFilter();
                    return;
                }
                super.keyReleased(evt);
            }

            /** FIXME the selection isn't always highlighted, and you may expand/collapse without
             *  moving the selection to that item. Confusing, can we force selection on expand/collapse?
             */
            @Override
            public void collapsed() { updateECButtons(); }
            @Override
            public void expanded() { updateECButtons(); }
        };

        _tree.addSelectionListener(lsnr);
        _tree.addControlListener(lsnr);
        _tree.addTraverseListener(lsnr);
        _tree.addKeyListener(lsnr);
        _tree.addMouseListener(lsnr);
        _tree.addTreeListener(lsnr);
        
        _bottom = new Composite(_root, SWT.NONE);
        _bottom.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _bottom.setLayout(new GridLayout(9, false));
        

        _markMessageReadButton = new Button(_bottom, SWT.PUSH);
        _markMessageReadButton.addSelectionListener(new FireSelectionListener() { public void fire() { markRead(); } });
        
        _markThreadReadButton = new Button(_bottom, SWT.PUSH);
        _markThreadReadButton.addSelectionListener(new FireSelectionListener() { public void fire() { markThreadRead(); } });
        
        Label dummy = new Label(_bottom, SWT.NONE);
        dummy.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _refreshButton = new Button(_bottom, SWT.PUSH);
        _refreshButton.addSelectionListener(new FireSelectionListener() { public void fire() { applyFilter(); } });
        
        Label dummy2 = new Label(_bottom, SWT.NONE);
        dummy2.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));

        _expandThreadButton = new Button(_bottom, SWT.PUSH);
        _expandThreadButton.addSelectionListener(new FireSelectionListener() { public void fire() { expandThread(); } });
        _expandThreadButton.setImage(ImageUtil.ICON_EXPAND);
        
        _collapseThreadButton = new Button(_bottom, SWT.PUSH);
        _collapseThreadButton.addSelectionListener(new FireSelectionListener() { public void fire() { collapseThread(); } });
        _collapseThreadButton.setImage(ImageUtil.ICON_COLLAPSE);

        _expandAllThreadButton = new Button(_bottom, SWT.PUSH);
        _expandAllThreadButton.addSelectionListener(new FireSelectionListener() { public void fire() { expandAll(); } });
        _expandAllThreadButton.setImage(ImageUtil.ICON_EXPAND);
        
        _collapseAllThreadButton = new Button(_bottom, SWT.PUSH);
        _collapseAllThreadButton.addSelectionListener(new FireSelectionListener() { public void fire() { collapseAll(); } });
        _collapseAllThreadButton.setImage(ImageUtil.ICON_COLLAPSE);

        // disable all the buttons to start, refreshPageNodes() will enable them
        _markMessageReadButton.setEnabled(false);
        _markThreadReadButton.setEnabled(false);
        _refreshButton.setEnabled(false);
        _expandThreadButton.setEnabled(false);
        _collapseThreadButton.setEnabled(false);
        _expandAllThreadButton.setEnabled(false);
        _collapseAllThreadButton.setEnabled(false);
        
        _preview = new MessageTreePreview(_client, _ui, _themeRegistry, _translationRegistry, _navControl, _bookmarkControl, URIHelper.instance(), this);

        _client.addMessageStatusListener(this);
        _translationRegistry.register(this);
        _themeRegistry.register(this);
        
        initDnD();
    }
    
    public void messageStatusUpdated(final long msgId, final int status) {
        ThreadReferenceNode node = null;
        TreeItem item = null;
        synchronized (_msgIdToItem) {
            item = (TreeItem)_msgIdToItem.get(Long.valueOf(msgId));
        }
        _ui.debugMessage("tree: messageStatusUpdated(" + msgId + "): item=" + item);
        if (item != null) {
            synchronized (_itemToNode) {
                node = (ThreadReferenceNode)_itemToNode.get(item);
            }
            _ui.debugMessage("tree: messageStatusUpdated(" + msgId + ", " + status + "): node is known? " + (node != null));
            if (node != null) {
                node.setMessageStatus(status);
                final ThreadReferenceNode updateNode = node;
                final TreeItem updateItem = item;
                _root.getDisplay().asyncExec(new Runnable() {
                    public void run() {
                        _ui.debugMessage("rendering updated status node");
                        renderNode(updateNode, updateItem);
                    }
                });
            }
        }
    }
    
    protected int getMessageSelectedCount() { return _tree.getSelectionCount(); }

    /** 
     * we can generate a cancel message if we created the message, own the channel 
     * its in, manage the channel its in, or are explicitly allowed to post in the
     * channel its in.  that doesn't mean people will /honor/ the cancel message, but
     * if none of those conditions are true, they definitely won't.
     */
    protected boolean getCancellable() {
        TreeItem selected[] = _tree.getSelection();
        for (int i = 0; i < selected.length; i++) {
            SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
            if ( (uri != null) && (uri.getMessageId() != null) ) {
                long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                if (msgId < 0)
                    return false;
                long chanId = _client.getMessageTarget(msgId);
                if (chanId < 0)
                    return false;
                DBClient.ChannelCollector chans = _client.getNymChannels();
                Long ownerId = Long.valueOf(_client.getChannelId(uri.getScope()));
                Long id = Long.valueOf(chanId);
                return chans.getIdentityChannelIds().contains(ownerId) || // we wrote it
                       chans.getIdentityChannelIds().contains(id) || // we own the forum
                       chans.getManagedChannelIds().contains(id) || // we manage the forum
                       chans.getPostChannelIds().contains(id); // we are trusted posters in the forum
            }
        }
        return false;
    }
    
    private void initDnD() {
        Transfer transfer[] = new Transfer[] { TextTransfer.getInstance() };
        int ops = DND.DROP_COPY;
        DragSource source = new DragSource(_tree, ops);
        source.setTransfer(transfer);
        source.addDragListener(new DragSourceListener() {
            public void dragFinished(DragSourceEvent evt) {}
            public void dragSetData(DragSourceEvent evt) {
                SyndieURI uri = getSelected();
                if (uri != null) {
                    TreeItem sel[] = _tree.getSelection();
                    BookmarkDnD bookmark = getBookmark(sel[0], (ReferenceNode)_itemToNode.get(sel[0]));
                    if (bookmark != null)
                        evt.data = bookmark.toString();
                }
            }
            public void dragStart(DragSourceEvent evt) {
                SyndieURI uri = getSelected();
                if (uri == null) {
                    evt.doit = false; // don't drag when nothing is selected
                }
            }
        });
    }
    
    protected BookmarkDnD getBookmark(TreeItem item, ReferenceNode node) {
        SyndieURI uri = (SyndieURI)_itemToURI.get(item);
        BookmarkDnD bookmark = new BookmarkDnD();
        bookmark.uri = uri;
        bookmark.name = item.getText(0);
        bookmark.desc = "";
        return bookmark;
    }
    
    private void toggleSort(TreeColumn column) {
        if (!_filterable) return;
        if (column == _currentSortColumn) {
            _currentSortDirection = (_currentSortDirection == SWT.UP ? SWT.DOWN : SWT.UP);
            //_ui.debugMessage("toggleSort direction on " + column.getText());
        } else {
            //_ui.debugMessage("toggleSort column on " + column.getText());
            _currentSortColumn = column;
            _currentSortDirection = SWT.DOWN;
        }
        
        _tree.setSortColumn(_currentSortColumn);
        _tree.setSortDirection(_currentSortDirection);
        applyFilter();
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
        _preview.dispose();
        _client.removeMessageStatusListener(this);
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        for (int i = 0; i < _bars.size(); i++)
            ((FilterBar)_bars.get(i)).dispose();
    }
    
    public void setFilterable(boolean filterable) { _filterable = filterable; }
    
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
                //_ui.debugMessage("Good filter set [" + filter + "]");
            } catch (URISyntaxException use) {
                //_ui.debugMessage("Bad filter set [" + filter + "]", use);
                uri = new SyndieURI("search", new HashMap());
                _filter = "";
            }
        } else {
            //_ui.debugMessage("Blank filter set");
            _filter = "";
        }
    
        for (int i = 0; i < _bars.size(); i++)
            ((FilterBar)_bars.get(i)).setFilter(uri);
        //_filterBar.setFilter(uri);
    }
    
    public void applyFilter() {
        String filter = null;
        for (int i = 0; i < _bars.size() && filter == null; i++) {
            String newfilter = ((FilterBar)_bars.get(i)).buildFilter();
            if (newfilter != null)
                _ui.debugMessage("overwriting filter [" + filter + "] with [" + newfilter + "]");
            filter = newfilter;
        }
        applyFilter(filter);
    }

    private transient boolean _filtering;

    public void applyFilter(String filter) {
        //_ui.debugMessage("\n\n\n\napplying filter: " + filter + " (old: " + _appliedFilter + ")");
        if (!_filterable) return;
        boolean alreadyFiltering = false;
        synchronized (MessageTree.this) {
            if (_filtering)
                alreadyFiltering = true;
            _filtering = true;
        }
        if (alreadyFiltering) {
            _ui.debugMessage("filter already in progress, not applying...");
            return;
        }
        final Shell s = showFilteringWidget();
        _tree.setEnabled(false);
        
        SyndieURI uri = null;
        try {
            if (filter != null)
                uri = new SyndieURI(filter);
        } catch (URISyntaxException use) {
            s.dispose();
            _tree.setEnabled(true);
        }
        //_filter = filter;
        setFilter(filter);
        if (uri != null) {
            final SyndieURI filteredURI = uri;
            JobRunner.instance().enqueue(new Runnable() {
                public void run() {
                    try {
                        //_ui.debugMessage("begin async calculating nodes in the tree: " + filteredURI.toString());
                        //try { Thread.sleep(200); } catch (InterruptedException ie) {}
                        applyFilter(filteredURI.toString(), filteredURI, calculateNodes(filteredURI)); 
                        //_ui.debugMessage("end async calculating nodes in the tree");
                    } finally {
                        synchronized (MessageTree.this) {
                            _filtering = false;
                        }
                        s.getDisplay().asyncExec(new Runnable() { 
                            public void run() { 
                                s.dispose(); 
                                _tree.setEnabled(true);
                            } });
                    }   
                }
            });
        }
    }
    
    private Shell showFilteringWidget() {
        Shell parent = _tree.getShell();  // doesnt help, why? not _root.getShell(), we want it to stay with syndie
        Shell s = new Shell(parent, SWT.NO_TRIM | SWT.APPLICATION_MODAL); // | SWT.ON_TOP);
        s.setLayout(new FillLayout());
        Composite c = new Composite(s, SWT.BORDER);
        c.setLayout(new RowLayout(SWT.HORIZONTAL));
        Label label = new Label(c, SWT.NONE);
        label.setText(getText("Message filtering in progress") + "   ");
        final Label filler = new Label(c, SWT.NONE);
        label.setFont(_themeRegistry.getTheme().SHELL_FONT);
        filler.setFont(_themeRegistry.getTheme().SHELL_FONT);
        filler.setText("\\   ");
        LivelinessIndicator liv = new LivelinessIndicator(filler);
        liv.run();
        s.pack();
        Point sz = s.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        Point src = parent.getSize();
        //if (src.x + src.y <= 0)
        //    src = _root.getParent().computeSize(SWT.DEFAULT, SWT.DEFAULT);
        //_ui.debugMessage("show liveliness: sz=" + sz + " src=" + src);
        Rectangle rect = Display.getDefault().map(parent, null, src.x/2-sz.x/2, src.y/2-sz.y/2, sz.x, sz.y);
        s.setBounds(rect);
        s.open();
        return s;
    }

    private static class LivelinessIndicator implements Runnable {
        private final Label _label;
        private int _index;

        public LivelinessIndicator(Label label) { _label = label; }

        public void run() {
            char c = ' ';
            switch (_index % 4) {
                case 0: c = '\\'; break;
                case 1: c = '|'; break;
                case 2: c = '/'; break;
                case 3: c = '-'; break;
            }
            _index++;
            String str = c + "   ";
            if (!_label.isDisposed()) {
                // race condition between that and this
                _label.setText(str);
                Display.getDefault().timerExec(100, LivelinessIndicator.this);
            }
        }
    }
    
    private void applyFilter(final String txt, final SyndieURI uri, final List nodes) {
        _tree.getDisplay().asyncExec(new Runnable() {
            public void run() { 
                if (uri.isChannel()) {
                    _showChannel = false;
                } else if (uri.isSearch()) {
                    Hash scopes[] = uri.getSearchScopes();
                    if ( (scopes != null) && (scopes.length == 1) )
                        _showChannel = false;
                    else
                        _showChannel = true;
                } else {
                    _showChannel = true;
                }
                if (!_showChannel) _colChannel.setWidth(1);
                
                //_ui.debugMessage("nodes calculated, setting them");
                setMessages(nodes);
                //_ui.debugMessage("nodes set w/ " + uri);
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
     * @return list of ThreadReferenceNode for the root of each thread, or if it isn't
     *         threaded, simply one per message
     */
    private List calculateNodes(SyndieURI uri) {
        ThreadAccumulator acc = new ThreadAccumulatorJWZ(_client, _ui);
        //_ui.debugMessage("setting the filter: " + uri.toString());
        acc.setFilter(uri);
        int sort = ThreadAccumulator.SORT_DEFAULT;
        if (_currentSortColumn == _colSubject)
            sort = ThreadAccumulator.SORT_SUBJECT;
        else if (_currentSortColumn == _colChannel)
            sort = ThreadAccumulator.SORT_FORUM;
        else if (_currentSortColumn == _colDate)
            sort = ThreadAccumulator.SORT_DATE;
        else if (_currentSortColumn == _colAuthor)
            sort = ThreadAccumulator.SORT_AUTHOR;
        acc.setSort(sort, _currentSortDirection == SWT.UP);
        //_ui.debugMessage("gathering the threads");
        acc.gatherThreads();
        List threads = new ArrayList();
        for (int i = 0; i < acc.getThreadCount(); i++)
            threads.add(acc.getRootThread(i));
        return threads;
    }
    
    /**
     *  Enables/disables all the buttons
     */
    private List getCurrentPageNodes(List referenceNodes) {
        if (_root.isDisposed()) return referenceNodes;
        int sz = _navPageSize.getSelection();
        _fullNodes = referenceNodes;
        _refreshButton.setEnabled(true);
        if (sz <= 0 || referenceNodes.isEmpty()) {
            String msg = getText("No items to display");
            _navState.setText(msg);
            _navNext.setEnabled(false);
            _navEnd.setEnabled(false);
            _navPrev.setEnabled(false);
            _navStart.setEnabled(false);
            _navPageSize.setEnabled(false);
            _markMessageReadButton.setEnabled(false);
            _markThreadReadButton.setEnabled(false);
            _expandThreadButton.setEnabled(false);
            _collapseThreadButton.setEnabled(false);
            _expandAllThreadButton.setEnabled(false);
            _collapseAllThreadButton.setEnabled(false);
          /****
            // TODO no method. Have to disable or remove the listeners?
            _colSubject.setEnabled(false);
            _colAuthor.setEnabled(false);
            _colChannel.setEnabled(false);
            _colDate.setEnabled(false);
            _colTags.setEnabled(false);
           ****/

            return referenceNodes;
        }
        _navPageSize.setEnabled(true);
        
        int start = _currentPage * sz;
        int end = (_currentPage+1) * sz;
        int max = referenceNodes.size();
        if (end >= max)
            end = max;
        
        int pages = (max + sz)/sz;
        
        boolean atEnd = (end >= referenceNodes.size());
        boolean atBeginning = (start == 0);
        _navNext.setEnabled(!atEnd);
        _navEnd.setEnabled(!atEnd);
        _navPrev.setEnabled(!atBeginning);
        _navStart.setEnabled(!atBeginning);
        // TODO enable/disable based on selection
        _markMessageReadButton.setEnabled(true);
        _markThreadReadButton.setEnabled(true);
        updateECButtons();

      /****
        // TODO enable/disable based if > 1 row
        _colSubject.setEnabled(true);
        _colAuthor.setEnabled(true);
        _colChannel.setEnabled(true);
        _colDate.setEnabled(true);
        _colTags.setEnabled(true);
      ****/
        
        StringBuilder buf = new StringBuilder();
        buf.append(getText("Page") + ": ");
        buf.append(_currentPage+1);
        buf.append(" / ");
        buf.append(pages);
        _navState.setText(buf.toString());
        
        _top.layout(new Control[] { _navState }); //true, true);
        //_ui.debugMessage("currentPage[" + _currentPage + "/" + pages + "]Nodes("+start +","+end+"): all nodes=" + referenceNodes.size());
        if (start > end) start = end;
        return referenceNodes.subList(start, end);
    }

    
    public MessageIterator getIterator(SyndieURI uri) {
        ReferenceNode node = getNode(uri);
        if (node == null)
            return null;
        TreeMessageIterator iter = new TreeMessageIterator(_client, _ui, _fullNodes, getCurrentFilter());
        iter.recenter(uri);
        return iter;
    }
    
    private Map _uriToNode;

    private ReferenceNode getNode(final SyndieURI uri) {
        if (_uriToNode == null) {
            final Map uriToNode = new HashMap();
            ReferenceNode.walk(_fullNodes, new ReferenceNode.Visitor() {
                public void visit(ReferenceNode node, int depth, int siblingOrder) {
                    SyndieURI nuri = node.getURI();
                    if (nuri != null)
                        uriToNode.put(nuri, node);
                }
            });
            _uriToNode = uriToNode;
        }
        ReferenceNode node = (ReferenceNode)_uriToNode.get(uri);
        return node;
    }
    
    List getMessages() { return _threadReferenceNodes; }

    /**
     *  this method is overridden in WatchedMessageTree to rewrite allNodes, injecting parents.
     *  on page traversals, we don't call this again though, but instead call w/ recalcTags param,
     *  so it won't inject the (already injected) parents
     */
    void setMessages(List allNodes) { 
        setMessages(allNodes, true); 
    }

    void setMessages(List allNodes, boolean recalcTags) {
        if (allNodes == null) return;
        if (_root.isDisposed()) return;
        _uriToNode = null;
        List referenceNodes = getCurrentPageNodes(allNodes);
        _tree.setRedraw(false);
        _tree.removeAll();
        // pack so the width will be set right in setMinWidth(),
        // otherwize 'apparent' width of subject column is the whole thing,
        // and it won't be set correctly.
        _tree.pack();
        _itemToURI.clear();
        _itemToMsgId.clear();
        _msgIdToItem.clear();
        _itemsNewUnread.clear();
        if (recalcTags)
            _tags.clear();
        _itemToNode.clear();
        _threadReferenceNodes = referenceNodes;
        long totalDBTime = 0;
        //long before = System.currentTimeMillis();
        
        //_tree.setItemCount(referenceNodes != null ? referenceNodes.size() : 0);
        // done on-demand via the virtual tree
        
        if (referenceNodes.isEmpty()) {

        }

        for (int i = 0; i < referenceNodes.size(); i++) {
            ThreadReferenceNode node = (ThreadReferenceNode)referenceNodes.get(i);
            totalDBTime += add(node, null);
        }
        
        //long after = System.currentTimeMillis();
        //_ui.debugMessage("setting messages: db time: " + totalDBTime + " for " + referenceNodes.size() + ", total add time: " + (after-before));
        
        //long beforeGetTags = System.currentTimeMillis();
        if (recalcTags)
            _tags.addAll(getTags(allNodes));
        //long afterGetTags = System.currentTimeMillis();
        //_ui.debugMessage("get all tags took " + (afterGetTags-beforeGetTags) + " for " + _tags.size() + " tags");
        
        if (recalcTags) {
            for (int i = 0; i < _bars.size(); i++)
                ((FilterBar)_bars.get(i)).populateTagCombo();
        }
        
        _tree.setSortColumn(_currentSortColumn);
        _tree.setSortDirection(_currentSortDirection);
        _tree.setRedraw(true);

        updateECButtons();
    }
    
    /**
     *  Enable/disable the expand/collapse-thread/all buttons
     *  @since 1.102b-10
     */
    private void updateECButtons() {
        TreeItem selected[] = _tree.getSelection();
        _expandThreadButton.setEnabled(isExpandable(selected));
        _collapseThreadButton.setEnabled(isCollapsible(selected));
        _expandAllThreadButton.setEnabled(isExpandable());
        _collapseAllThreadButton.setEnabled(isCollapsible());
    }

    /**
     *  Is there anything to expand?
     *  @since 1.102b-10
     */
    private boolean isExpandable() {
        return isExpandable(_tree.getItems());
    }
    
    /**
     *  Is there anything to expand?
     *  @since 1.102b-10
     */
    private boolean isExpandable(TreeItem[] items) {
        if (items == null)
            return false;
        for (int i = 0; i < items.length; i++) {
            if (isExpandable(items[i]))
                return true;
        }
        return false;
    }
    
    /**
     *  Is there anything to expand?
     *  @since 1.102b-10
     */
    private boolean isExpandable(TreeItem item) {
        if (item.getItemCount() <= 0)
            return false;
        if (!item.getExpanded())
            return true;
        TreeItem items[] = item.getItems();
        for (int i = 0; i < items.length; i++) {
            if (isExpandable(items[i]))
                return true;
        }
        return false;
    }
    
    /**
     *  Is there anything to collapse?
     *  @since 1.102b-10
     */
    private boolean isCollapsible() {
        return isCollapsible(_tree.getItems());    
    }

    /**
     *  Is there anything to collapse?
     *  @since 1.102b-10
     */
    private boolean isCollapsible(TreeItem[] items) {
        if (items == null)
            return false;
        for (int i = 0; i < items.length; i++) {
            if (isCollapsible(items[i]))
                return true;
        }
        return false;
    }
    
    /**
     *  Is there anything to collapse?
     *  @since 1.102b-10
     */
    private boolean isCollapsible(TreeItem item) {
        if (item.getItemCount() <= 0)
            return false;
        if (item.getExpanded())
            return true;
        TreeItem items[] = item.getItems();
        for (int i = 0; i < items.length; i++) {
            if (isCollapsible(items[i]))
                return true;
        }
        return false;
    }

    public void expandAll() {
        TreeItem items[] = _tree.getItems();
        for (int i = 0; i < items.length; i++)
            expandAll(items[i]);
        boolean enableCollapse = _tree.getSelectionCount() > 0;
        _expandThreadButton.setEnabled(false);
        _collapseThreadButton.setEnabled(isExpandable());
        _expandAllThreadButton.setEnabled(false);
        _collapseAllThreadButton.setEnabled(true);
    }

    private void expandAll(TreeItem item) {
        if (item.getItemCount() <= 0) return;
        item.setExpanded(true);
        TreeItem items[] = item.getItems();
        for (int i = 0; i < items.length; i++)
            expandAll(items[i]);
    }

    public void collapseAll() {
        TreeItem items[] = _tree.getItems();
        for (int i = 0; i < items.length; i++)
            collapseAll(items[i]);
        _expandThreadButton.setEnabled(isCollapsible());
        _collapseThreadButton.setEnabled(false);
        _expandAllThreadButton.setEnabled(true);
        _collapseAllThreadButton.setEnabled(false);
    }

    private void collapseAll(TreeItem item) {
        if (item.getItemCount() <= 0) return;
        item.setExpanded(false);
        TreeItem items[] = item.getItems();
        for (int i = 0; i < items.length; i++)
            collapseAll(items[i]);
    }
    
    /** build up the thread in a nonvirtual tree */
    private long add(ThreadReferenceNode node, TreeItem parent) {
        //_browser.getUI().debugMessage("Add: " + node.getURI() + " [" + System.identityHashCode(node) + "]");
        long dbTime = 0;
        TreeItem item = createItem(parent, _tree);
        dbTime = renderNode(node, item);
        
        _itemToNode.put(item, node);
        _itemToURI.put(item, node.getURI());
        _itemToMsgId.put(item, Long.valueOf(node.getUniqueId()));
        _msgIdToItem.put(Long.valueOf(node.getUniqueId()), item);
        
        if ( _expandAll || ( (parent == null) && (_expandRoots) ) ) {
            if (node.getChildCount() > 0)
                item.setExpanded(true);
        }
        
        for (int i = 0; i < node.getChildCount(); i++)
            dbTime += add((ThreadReferenceNode)node.getChild(i), item);
        return dbTime;
    }
    
    private Set getTags(List nodes) {
        IDGatherer idGatherer = new IDGatherer();
        ReferenceNode.walk(nodes, idGatherer);
        return idGatherer.getTags();
    }

    private static class IDGatherer implements ReferenceNode.Visitor {
        private final Set _tags;
        public IDGatherer() { _tags = new HashSet(); }
        public Set getTags() { return _tags; }
        public void visit(ReferenceNode node, int depth, int siblingOrder) {
            Set tags = ((ThreadReferenceNode)node).getTags();
            if (tags != null)
                _tags.addAll(tags);
        }
    }
    
    /**
     *  @return database access time
     */
    protected long renderNode(ThreadReferenceNode node, TreeItem item) {
        if (node == null)
            return 0;
        SyndieURI uri = node.getURI();
        String subj = "";
        String auth = "";
        String chan = "";
        String date = "";
        String tags = "";
        int status = DBClient.MSG_STATUS_UNREAD;

        long dbStart = System.currentTimeMillis();

        long msgId = -1;
        if (uri != null) {
            long chanId = node.getScopeId();
            String scopeName = node.getScopeName();
            ////long chanId = _client.getChannelId(uri.getScope());
            ////String scopeName = _client.getChannelName(chanId);
            //ChannelInfo scopeInfo = _client.getChannel(chanId);
            //MessageInfo msg = _client.getMessage(chanId, uri.getMessageId());

            // simple optimization: use the fact that these ReferenceNode instances are really
            // ThreadReferenceNode instances, which contain subject, msgId, target, etc.

            long messageId = -1;
            //if (uri.getMessageId() != null)
            //    messageId = uri.getMessageId().longValue();
            messageId = node.getMsgId().messageId;
            msgId = node.getMsgId().msgId; // _client.getMessageId(chanId, messageId);
            
            //_browser.getUI().debugMessage("renderNode: " + uri + ": msgId=" + msgId);
            
            if (msgId >= 0) {
                _itemToMsgId.put(item, Long.valueOf(msgId));
                _msgIdToItem.put(Long.valueOf(msgId), item);
                subj = node.getSubject(); //_client.getMessageSubject(msgId);
                long authorId = node.getAuthorId(); //_client.getMessageAuthor(msgId);//msg.getAuthorChannelId();
                if (authorId != chanId) {
                    String authorName = node.getAuthorName(); // _client.getChannelName(authorId);
                    Hash authorHash = node.getAuthorHash(); //_client.getChannelHash(authorId);
                    //ChannelInfo authInfo = _client.getChannel(authorId);
                    //System.out.println("author is NOT the scope chan for " + uri.toString() + ": " + auth);
                } else {
                    //System.out.println("author is the scope chan for " + uri.toString());
                    auth = UIUtil.displayName(scopeName, uri.getScope());
                }
                //ChannelInfo chanInfo = scopeInfo;
                long targetChanId = node.getTargetId(); // _client.getMessageTarget(msgId);
                if (targetChanId != chanId) {
                    //System.out.println("target chan != scope chan: " + msg.getTargetChannel().toBase64() + "/" + msg.getTargetChannelId() + " vs " + scopeInfo.getChannelHash().toBase64() + "/" + scopeInfo.getChannelId());
                    //System.out.println("msg: " + uri.toString());
                    String targetName = node.getTargetName(); //_client.getChannelName(targetChanId);
                    Hash targetHash = node.getTargetHash(); //_client.getChannelHash(targetChanId);
                    //chanInfo = _client.getChannel(msg.getTargetChannelId());
                    chan = UIUtil.displayName(targetName, targetHash);
                    //if (chanInfo == null) {
                    //    chan = "[" + msg.getTargetChannel().toBase64().substring(0,6) + "]";
                    //} else {
                    //    chan = chanInfo.getName() + " [" + chanInfo.getChannelHash().toBase64().substring(0,6) + "]";
                    //}
                } else {
                    //System.out.println("target chan == scope chan: " + msg.getTargetChannel().toBase64() + "/" + msg.getTargetChannelId() + "/" + msg.getInternalId() + "/" + msg.getScopeChannelId() + "/" + msg.getAuthorChannelId());
                    //System.out.println("msg: " + uri.toString());
                    chan = UIUtil.displayName(scopeName, uri.getScope());
                }

                if (auth.length() <= 0) {
                     auth = chan;
                }
                Set msgTags = node.getTags(); //_client.getMessageTags(msgId, true, true);
                StringBuilder buf = new StringBuilder();
                if (msgTags != null) {
                    for (Iterator iter = msgTags.iterator(); iter.hasNext(); ) {
                        String tag = (String)iter.next();
                        tag = tag.trim();
                        buf.append(tag).append(" ");
                        //_tags.add(tag);
                    }
                }
                tags = buf.toString().trim();
                item.setGrayed(false);
                long importDate = node.getImportDate(); //_client.getMessageImportDate(msgId);
                long postDate = messageId; //uri.getMessageId().longValue();
                if (_appliedFilter == null) {
                    if (MessageTree.shouldUseImportDate(_client))
                        date = DateTime.getDateTime(importDate);
                    else
                        date = DateTime.getDateTime(postDate);
                } else if (_appliedFilter.getString("agelocal") != null) {
                    date = DateTime.getDateTime(importDate);
                    //_browser.getUI().debugMessage("using local import date for " + msgId + ": " + date + " (instead of " + DateTime.getDate(postDate) + ")");
                } else {
                    date = DateTime.getDateTime(postDate);
                    //_browser.getUI().debugMessage("using post date for " + msgId + ": " + date + " (instead of " + DateTime.getDate(importDate) + ")");
                }
                status = node.getMessageStatus(); //_client.getMessageStatus(_client.getLoggedInNymId(), msgId, targetChanId);
            } else {
                // message is not locally known
                subj = getText("Message is not known locally");
                auth = UIUtil.displayName(scopeName, uri.getScope());
                chan = "";
                if (messageId >= 0)
                    date = DateTime.getDateTime(uri.getMessageId().longValue());
                else
                    date = "";
                tags = "";
            }
        }

        if ( (subj == null) || (subj.trim().length() <= 0) ) {
            subj = calculateSubject(node, uri);
        }
        
        long dbEnd = System.currentTimeMillis();

        item.setText(0, subj);
        if (node.getAttachmentCount() > 0)
            item.setImage(0, ImageUtil.ICON_MSG_FLAG_HASATTACHMENTS);
        item.setText(1, auth);
        item.setText(2, chan);
        item.setText(3, date);
        item.setText(4, tags);
        Font f = null;
        boolean isNew = false;
        if ( (status == DBClient.MSG_STATUS_READ) || (uri == null) ) {
            f = _themeRegistry.getTheme().MSG_OLD_FONT;
        } else if (msgId < 0) {
            f = _themeRegistry.getTheme().MSG_UNKNOWN_FONT;
        } else {
            isNew = true;
            _itemsNewUnread.add(item);
            f = _themeRegistry.getTheme().MSG_NEW_UNREAD_FONT;
        }
        
        synchronized (this) {
            item.setFont(f);
        }
        
        if (isNew) {
            // note: this will only retheme the ancestors of unread messages /that have been rendered/
            rethemeAncestorsOfUnread(item);
        }
        
        setMinWidth(_colSubject, subj, 0, 200);
        setMinWidth(_colAuthor, auth, 0, 150);
        setMinWidth(_colChannel, chan, 0, 150);
        setMinWidth(_colDate, date, 20, 150);
        setMinWidth(_colTags, tags, 0, 50);
        if (!_showChannel) _colChannel.setWidth(1);
        //_browser.getUI().debugMessage("message status: " + status);
        return dbEnd-dbStart;
    }
    
    private String calculateSubject(ThreadReferenceNode node, SyndieURI uri) {
        ThreadReferenceNode cur = node;
        while (cur != null) {
            String subj = cur.getSubject();
            if ( (subj != null) && (subj.length() > 0) )
                return subj;
            else
                cur = (ThreadReferenceNode)cur.getParent();
        }
        return MessageView.calculateSubject(_client, _ui, _translationRegistry, node.getUniqueId(), node.getScopeHash(), Long.valueOf(node.getMsgId().messageId), false);
    }
    
    
    protected void setMinWidth(TreeColumn col, String txt, int extra, int min) {
        int width = getWidth(txt) + _tree.getGridLineWidth()*2 + extra;
        if (width < min)
            width = min;
        else if (width > 400)
            width = 400;
        int existing = col.getWidth();
        //System.out.println("setMinWidth() " + txt + " extra " + extra + " min " + min + " width " + width + " existing " + existing);
        if (width > existing)
            col.setWidth(width);
    }
    
    protected int getWidth(String text) {
        if (_avgCharWidth <= 0) {
            GC gc = new GC(_tree);
            FontMetrics fm = gc.getFontMetrics();
            _avgCharWidth = fm.getAverageCharWidth();
            gc.dispose();
        }
        return _avgCharWidth * (text == null ? 1 : text.length());
    }
    
    private void fireSelected(boolean toView, boolean nodelay) {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                if (_listener != null)
                    _listener.messageSelected(this, uri, toView, nodelay);
            }
        }
        updateECButtons();
    }

    private void viewSelected() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                _navControl.view(uri);
            }
        }
    }
    private void replySelected() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                if (uri.getMessageId() == null) continue;
                long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                long targetId = _client.getMessageTarget(msgId);
                Hash target = _client.getChannelHash(targetId);
                _navControl.view(URIHelper.instance().createPostURI(target, uri));
            }
        }
    }
    private void viewSelectedForum() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                if (uri.getMessageId() == null) {
                    _navControl.view(uri);
                } else {
                    long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                    long targetId = _client.getMessageTarget(msgId);
                    Hash scope = _client.getChannelHash(targetId);
                    _navControl.view(SyndieURI.createScope(scope));
                }
            }
        }
    }
    private void viewSelectedForumMeta() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                if (uri.getMessageId() == null) {
                    _navControl.view(URIHelper.instance().createMetaURI(uri.getScope()));
                } else {
                    long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                    long targetId = _client.getMessageTarget(msgId);
                    Hash scope = _client.getChannelHash(targetId);
                    _navControl.view(URIHelper.instance().createMetaURI(scope));
                }
            }
        }
    }
    private void bookmarkSelectedForum() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                if (uri.getMessageId() == null) {
                    _bookmarkControl.bookmark(uri);
                } else {
                    long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                    long targetId = _client.getMessageTarget(msgId);
                    Hash scope = _client.getChannelHash(targetId);
                    _bookmarkControl.bookmark(SyndieURI.createScope(scope));
                }
            }
        }
    }
    private void viewSelectedAuthor() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                if (uri.getMessageId() == null) {
                    _navControl.view(uri);
                } else {
                    long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                    long authorId = _client.getMessageAuthor(msgId);
                    Hash scope = _client.getChannelHash(authorId);
                    _navControl.view(SyndieURI.createScope(scope));
                }
            }
        }
    }
    private void viewSelectedAuthorMeta() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                if (uri.getMessageId() == null) {
                    _navControl.view(URIHelper.instance().createMetaURI(uri.getScope()));
                } else {
                    long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                    long authorId = _client.getMessageAuthor(msgId);
                    Hash scope = _client.getChannelHash(authorId);
                    _navControl.view(URIHelper.instance().createMetaURI(scope));
                }
            }
        }
    }
    private void bookmarkSelectedAuthor() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                if (uri.getMessageId() == null) {
                    _bookmarkControl.bookmark(SyndieURI.createScope(uri.getScope()));
                } else {
                    long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                    long authorId = _client.getMessageAuthor(msgId);
                    Hash scope = _client.getChannelHash(authorId);
                    _bookmarkControl.bookmark(SyndieURI.createScope(scope));
                }
            }
        }
    }
    
    private void expandThread() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                TreeItem root = getThreadRoot(selected[i]);
                if (root != null) {
                    List remaining = new ArrayList();
                    remaining.add(root);
                    while (remaining.size() > 0) {
                        TreeItem cur = (TreeItem)remaining.remove(0);
                        cur.setExpanded(true);
                        for (int j = 0; j < cur.getItemCount(); j++)
                            remaining.add(cur.getItem(j));
                    }
                }
            }
        }
        _expandThreadButton.setEnabled(false);
        _collapseThreadButton.setEnabled(true);
        _expandAllThreadButton.setEnabled(isExpandable());
        _collapseAllThreadButton.setEnabled(isCollapsible());
    }

    private void collapseThread() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                TreeItem root = getThreadRoot(selected[i]);
                if (root != null)
                    root.setExpanded(false);
            }
        }
        _expandThreadButton.setEnabled(true);
        _collapseThreadButton.setEnabled(false);
        _expandAllThreadButton.setEnabled(isExpandable());
        _collapseAllThreadButton.setEnabled(isCollapsible());
    }
    
    private void markRead() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                Long msgId = (Long)_itemToMsgId.get(selected[i]);
                if (msgId != null) {
                    if (_itemsNewUnread.contains(selected[i])) {
                        //_ui.debugMessage("mark an unread message as read (" + msgId + ")");
                        _client.markMessageRead(msgId.longValue());
                        selected[i].setFont(_themeRegistry.getTheme().MSG_NEW_READ_FONT);
                        _itemsNewUnread.remove(selected[i]);
                    }
                }
            }
            _dataCallback.readStatusUpdated();
        }
    }
    private void markUnread() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                Long msgId = (Long)_itemToMsgId.get(selected[i]);
                if (msgId != null) {
                    //_ui.debugMessage("mark as unread (" + msgId + ")");
                    _client.markMessageUnread(msgId.longValue());
                    selected[i].setFont(_themeRegistry.getTheme().MSG_NEW_UNREAD_FONT);
                    _itemsNewUnread.add(selected[i]);
                }
            }
            _dataCallback.readStatusUpdated();
        }
    }
    
    private void markThreadRead() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                TreeItem root = getThreadRoot(selected[i]);
                markThreadRead(root);
            }
            if (selected.length > 0)
                _dataCallback.readStatusUpdated();
        }
    }

    protected TreeItem getThreadRoot(TreeItem item) {
        TreeItem root = item;
        while (getParentItem(root) != null)
            root = getParentItem(root);
        return root;
    }

    private void markThreadRead(TreeItem item) {
        Long msgId = (Long)_itemToMsgId.get(item);
        if (msgId != null) {
            if (_itemsNewUnread.contains(item)) {
                _ui.debugMessage("marking message read: " + msgId);
                _client.markMessageRead(msgId.longValue());
                item.setFont(_themeRegistry.getTheme().MSG_NEW_READ_FONT);
                _itemsNewUnread.remove(item);
            } else {
                _ui.debugMessage("message was already unread: " + msgId);
            }
        }
        TreeItem children[] = item.getItems();
        for (int i = 0; i < children.length; i++)
            markThreadRead(children[i]);
    }

    private void markThreadUnread(TreeItem item) {
        Long msgId = (Long)_itemToMsgId.get(item);
        if (msgId != null) {
            if (!_itemsNewUnread.contains(item)) {
                _ui.debugMessage("marking message unread: " + msgId);
                _client.markMessageUnread(msgId.longValue());
                item.setFont(_themeRegistry.getTheme().MSG_NEW_UNREAD_FONT);
                _itemsNewUnread.add(item);
            } else {
                _ui.debugMessage("message was already read: " + msgId);
            }
        }
        TreeItem children[] = item.getItems();
        for (int i = 0; i < children.length; i++)
            markThreadUnread(children[i]);
    }

    private void markAllRead() {
        TreeItem selected[] = _tree.getSelection();
        if ( (selected != null) && (selected.length > 0) ) {
            HashSet channelIds = new HashSet();
            for (int i = 0; i < selected.length; i++) {
                long channelId = markAllRead(selected[i]);
                if (channelId >= 0)
                    channelIds.add(Long.valueOf(channelId));
            }
            if (channelIds.size() > 0)
                _dataCallback.readStatusUpdated();
            
            for (Iterator iter = _itemToURI.entrySet().iterator(); iter.hasNext(); ) {
                Map.Entry cur = (Map.Entry)iter.next();
                TreeItem item = (TreeItem)cur.getKey();
                SyndieURI uri = (SyndieURI)cur.getValue();
                
                if (uri != null) {
                    long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                    if (msgId >= 0) {
                        long target = _client.getMessageTarget(msgId);
                        if (channelIds.contains(Long.valueOf(target))) {
                            _itemsNewUnread.remove(item);
                            item.setFont(_themeRegistry.getTheme().MSG_OLD_FONT);
                        }
                    }
                }
            }
            // we aren't marking *all* read, just all messages in a particular forum */
            /*
            _itemsNewRead.clear();
            _itemsNewUnread.clear();
            for (Iterator iter = _itemToURI.keySet().iterator(); iter.hasNext(); ) {
                TreeItem item = (TreeItem)iter.next();
                _itemsOld.add(item);
                item.setFont(_browser.getThemeRegistry().getTheme().MSG_OLD_FONT);
            }
             */
        }
    }

    protected long markAllRead(TreeItem item) {
        Long msgId = (Long)_itemToMsgId.get(item);
        if (msgId != null) {
            long target = _client.getMessageTarget(msgId.longValue());
            _client.markChannelRead(target);
            return target;
        } else {
            return -1;
        }
    }

    private void delete() {
        TreeItem selected[] = _tree.getSelection();
        int deleted = 0;
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                if ( (uri != null) && (uri.getMessageId() != null) ) {
                    if (_banControl.deleteMessage(uri)) // _client.deleteMessage(uri, _ui, true);
                        deleted++;
                }
            }
            _dataCallback.readStatusUpdated();
        }
        if (deleted > 0)
            applyFilter();
    }

    private void cancel() {
        TreeItem selected[] = _tree.getSelection();
        int cancelled = 0;
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                if ( (uri != null) && (uri.getMessageId() != null) ) {
                    if (_banControl.cancelMessage(uri)) // _client.cancelMessage(uri, _ui);
                        cancelled++;
                }
            }
            _dataCallback.readStatusUpdated();
        }
        if (cancelled > 0)
            applyFilter();
    }

    

    
    
    
    
    public void translate(TranslationRegistry registry) {
        _colSubject.setText(registry.getText("Subject"));
        _colAuthor.setText(registry.getText("Author"));
        _colChannel.setText(registry.getText("Forum"));
        _colDate.setText(registry.getText("Date"));
        _colTags.setText(registry.getText("Tags"));
        
        _colSubject.pack();
        _colAuthor.pack();
        _colChannel.pack();
        _colDate.pack();
        _colTags.pack();
        
        _view.setText(registry.getText("View the message"));
        _reply.setText(registry.getText("Reply to the message"));
        _viewForum.setText(registry.getText("View the forum's messages"));
        _viewForumMeta.setText(registry.getText("View the forum's profile"));
        _viewAuthor.setText(registry.getText("View the author's blog"));
        _viewAuthorMeta.setText(registry.getText("View the author's profile"));
        _bookmarkForum.setText(registry.getText("Bookmark the forum"));
        _bookmarkAuthor.setText(registry.getText("Bookmark the author"));
        _expandThread.setText(registry.getText("Expand the thread fully"));
        _collapseThread.setText(registry.getText("Collapse the thread fully"));
        _expandAllThread.setText(registry.getText("Expand all threads fully"));
        _collapseAllThread.setText(registry.getText("Collapse all threads fully"));
        _markRead.setText(registry.getText("Mark the message as read"));
        _markThreadRead.setText(registry.getText("Mark the thread as read"));
        _markUnread.setText(registry.getText("Mark the message as unread"));
        _markAllRead.setText(registry.getText("Mark the forum as read"));
        _delete.setText(registry.getText("Delete the message locally"));
        _cancel.setText(registry.getText("Cancel the message (tell others to delete it)"));
        
        _markMessageReadButton.setText(registry.getText("Mark Read"));
        _markThreadReadButton.setText(registry.getText("Mark Thread Read"));
        _refreshButton.setText(registry.getText("Refresh"));
        _expandThreadButton.setText(registry.getText("Expand"));
        _collapseThreadButton.setText(registry.getText("Collapse"));
        _expandAllThreadButton.setText(registry.getText("Expand All"));
        _collapseAllThreadButton.setText(registry.getText("Collapse All"));
        
        _navPageSizeLabel.setText(registry.getText("Page size") + ':');
    }
    
    public void applyTheme(Theme theme) {
        _navStart.setFont(theme.BUTTON_FONT);
        _navPrev.setFont(theme.BUTTON_FONT);
        _navState.setFont(theme.DEFAULT_FONT);
        _navPageSizeLabel.setFont(theme.DEFAULT_FONT);
        _navPageSize.setFont(theme.DEFAULT_FONT);
        _navNext.setFont(theme.BUTTON_FONT);
        _navEnd.setFont(theme.BUTTON_FONT);
        
        _markMessageReadButton.setFont(theme.BUTTON_FONT);
        _markThreadReadButton.setFont(theme.BUTTON_FONT);
        _refreshButton.setFont(theme.BUTTON_FONT);
        _expandThreadButton.setFont(theme.BUTTON_FONT);
        _collapseThreadButton.setFont(theme.BUTTON_FONT);
        
        _avgCharWidth = -1;
        
        synchronized (this) {
            _tree.setFont(theme.TREE_FONT);
            if (_filterEditorShell != null) {
                _filterEditorShell.setFont(theme.SHELL_FONT);
                _filterEditorShell.layout(true, true);
            }
            //_root.layout(true, true);
            for (Iterator iter = _itemToURI.keySet().iterator(); iter.hasNext(); ) {
                TreeItem item = (TreeItem)iter.next();
                if (_itemsNewUnread.contains(item))
                    item.setFont(theme.MSG_NEW_UNREAD_FONT);
                else
                    item.setFont(theme.MSG_OLD_FONT);
            }
            rethemeAncestorsOfUnread();
        }
    }

    private void rethemeAncestorsOfUnread() {
        //if (true) return;
        // now readjust the font of read/old message ancestors of unread messages
        synchronized (this) {
            for (Iterator iter = _itemsNewUnread.iterator(); iter.hasNext(); ) {
                TreeItem item = (TreeItem)iter.next();
                TreeItem parent = getParentItem(item);
                while (parent != null) {
                    if (!_itemsNewUnread.contains(parent))
                        markUnreadChild(parent);
                    parent = getParentItem(parent);
                }
            }
        }
    }

    private void rethemeAncestorsOfUnread(TreeItem item) {
        //if (true) return;
        // now readjust the font of read/old message ancestors of unread messages
        TreeItem parent = getParentItem(item);
        while (parent != null) {
            if (!_itemsNewUnread.contains(parent))
                markUnreadChild(parent);
            parent = getParentItem(parent);
        }
    }
    protected void markUnreadChild(TreeItem item) {
        item.setFont(_themeRegistry.getTheme().MSG_UNREAD_CHILD_FONT);
    }
    
    /**
     * gtk and perhaps other platforms have to do some muckiness to get the parent,
     * probably due to the virtual trees.  however, since we dont use virtual trees
     * in this class, lets just keep a pointer to the parent item in the item itself
     */
    protected TreeItem getParentItem(TreeItem item) {
        if (item == null) return null;
        return (TreeItem)item.getData("parent");
    }

    protected TreeItem createItem(TreeItem parent, Tree tree) {
        TreeItem item = null;
        if (parent == null)
            item = new TreeItem(tree, SWT.NONE);
        else
            item = new TreeItem(parent, SWT.NONE);
        item.setData("parent", parent); // see getParentItem(item)
        return item;
    }
    
    public void expandSelected(boolean all) { 
        if (all)
            expandAll();
        else
            expandThread(); 
    }

    public void collapseSelected(boolean all) {
        if (all)
            collapseAll();
        else
            collapseThread(); 
    }

    public void toggleRead() { 
        TreeItem selected[] = _tree.getSelection();
        boolean statusWasRead = false;
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                Long msgId = (Long)_itemToMsgId.get(selected[i]);
                if (msgId != null) {
                    if (_itemsNewUnread.contains(selected[i])) {
                        statusWasRead = false;
                    } else {
                        statusWasRead = true;
                    }
                }
                _ui.debugMessage("toggleRead: item was read? " + statusWasRead);
                if (statusWasRead)
                    markUnread();
                else
                    markRead();
            }
        }
    }
}
