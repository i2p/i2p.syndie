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
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
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
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.NullUI;
import syndie.db.ThreadAccumulator;
import syndie.db.ThreadAccumulatorJWZ;

/**
 * 
 */
public class MessageTree implements Translatable, Themeable {
    protected BrowserControl _browser;
    private DBClient _client;
    private Composite _parent;
    private Composite _root;
    private Tree _tree;
    private TreeColumn _colType;
    protected TreeColumn _colSubject;
    private TreeColumn _colAuthor;
    private TreeColumn _colChannel;
    private TreeColumn _colDate;
    private TreeColumn _colTags;

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
    private MenuItem _markRead;
    private MenuItem _markUnread;
    private MenuItem _markThreadRead;
    private MenuItem _markAllRead;
    
    private boolean _showAuthor;
    private boolean _showChannel;
    private boolean _showDate;
    private boolean _showTags;
    private boolean _showFlags;
    private boolean _expandRoots;
    private boolean _expandAll;
    
    private SyndieURI _highlight;
    
    /** tags applied to the messages being displayed */
    private Set _tags;

    private MessageTreeListener _listener;
    private Map _itemToURI;
    /** items for messages that are new and unread */
    private Set _itemsNewUnread;
    /** item to msgId (Long) */
    private Map _itemToMsgId;
    /** item to MessageFlagBar */
    private Map _itemToMsgFlags;
    /** ordered list of ReferenceNode instances describing the tree */
    private List _threadReferenceNodes;
    /** TreeItem to ReferenceNode */
    private Map _itemToNode;
    
    /** column we are sorting on */
    private TreeColumn _currentSortColumn;
    /** SWT.UP or SWT.DOWN */
    private int _currentSortDirection;
    
    /** list of FilterBar instances added to this tree */
    private List _bars;
    private boolean _hideFilter;
    
    private boolean _filterable;
    
    public MessageTree(BrowserControl browser, Composite parent, MessageTreeListener lsnr) { this(browser, parent, lsnr, false); }
    public MessageTree(BrowserControl browser, Composite parent, MessageTreeListener lsnr, boolean hideFilter) {
        this(browser, parent, lsnr, true, true, true, true, hideFilter, true, true, true);
    }
    public MessageTree(BrowserControl browser, Composite parent, MessageTreeListener lsnr, boolean showAuthor, boolean showChannel, boolean showDate, boolean showTags, boolean hideFilter, boolean showFlags, boolean expandRoots, boolean expandAll) {
        _browser = browser;
        _client = browser.getClient();
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
        _itemToMsgFlags = new HashMap();
        _itemToNode = new HashMap();
        _tags = new HashSet();
        _bars = new ArrayList();
        _filterable = true;
        initComponents();
    }
    
    public SyndieURI getCurrentFilter() { return _appliedFilter; }
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
        FilterBar bar = new FilterBar(_browser, this, row, lsnr);
        _bars.add(bar);
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
    
    public static interface PreviewControlListener {
        public void togglePreview(boolean shouldShow);
    }
    
    static boolean shouldShowPreview(BrowserControl ctl) {
        Properties prefs = ctl.getClient().getNymPrefs();
        return ( (prefs == null) || (!prefs.containsKey("showPreview")) || (Boolean.valueOf(prefs.getProperty("showPreview")).booleanValue()));
    }
    static void setShouldShowPreview(BrowserControl ctl, boolean shouldShow) {
        Properties prefs = ctl.getClient().getNymPrefs();
        prefs.setProperty("showPreview", shouldShow ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
        ctl.getClient().setNymPrefs(prefs);
    }
    static boolean shouldUseImportDate(BrowserControl ctl) {
        Properties prefs = ctl.getClient().getNymPrefs();
        return ( (prefs == null) || (!prefs.containsKey("browse.useImportDate")) || (Boolean.valueOf(prefs.getProperty("browse.useImportDate")).booleanValue()));
    }
    static void setShouldUseImportDate(BrowserControl ctl, boolean useImportDate) {
        Properties prefs = ctl.getClient().getNymPrefs();
        prefs.setProperty("browse.useImportDate", useImportDate ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
        ctl.getClient().setNymPrefs(prefs);
    }
 
    static boolean shouldMarkReadOnView(BrowserControl ctl) {
        Properties prefs = ctl.getClient().getNymPrefs();
        return ( (prefs != null) && (prefs.containsKey("browse.markReadOnView")) && (Boolean.valueOf(prefs.getProperty("browse.markReadOnView")).booleanValue()));
    }
    static void setShouldMarkReadOnView(BrowserControl ctl, boolean markReadOnView) {
        Properties prefs = ctl.getClient().getNymPrefs();
        prefs.setProperty("browse.markReadOnView", markReadOnView ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
        ctl.getClient().setNymPrefs(prefs);
    }
    
    static boolean shouldMarkReadOnPreview(BrowserControl ctl) {
        Properties prefs = ctl.getClient().getNymPrefs();
        return ( (prefs != null) && (prefs.containsKey("browse.markReadOnPreview")) && (Boolean.valueOf(prefs.getProperty("browse.markReadOnPreview")).booleanValue()));
    }
    static void setShouldMarkReadOnPreview(BrowserControl ctl, boolean markReadOnPreview) {
        Properties prefs = ctl.getClient().getNymPrefs();
        prefs.setProperty("browse.markReadOnPreview", markReadOnPreview ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
        ctl.getClient().setNymPrefs(prefs);
    }
    
    private static class FilterBar implements Translatable, Themeable {
        private BrowserControl _ctl;
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
        /** if > 0, the custom date being filtered */
        private long _customDate;
        private Hash _forumScopeOther[];
        private PreviewControlListener _listener;
        
        public FilterBar(BrowserControl browser, MessageTree msgTree, Composite bar, PreviewControlListener lsnr) {
            _ctl = browser;
            _msgTree = msgTree;
            _filterRow = bar;
            _customDate = -1;
            _listener = lsnr;
            initBar();
        }
        private void initBar() {
            _filterLabel = new Label(_filterRow, SWT.NONE);
            _filterLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));

            _filterAge = new Combo(_filterRow, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.BORDER);
            _filterAge.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
            _filterAge.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    if (_filterAge.getSelectionIndex() == AGE_CUSTOM)
                        pickDate();
                    else
                        _msgTree.applyFilter();
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    if (_filterAge.getSelectionIndex() == AGE_CUSTOM)
                        pickDate();
                    else
                        _msgTree.applyFilter();
                }
            });

            
            _filterKeywordLabel = new Label(_filterRow, SWT.NONE);
            _filterKeywordLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));

            _filterKeyword = new Text(_filterRow, SWT.BORDER | SWT.SINGLE);
            _filterKeyword.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));
            _filterKeyword.addTraverseListener(new TraverseListener() {
                public void keyTraversed(TraverseEvent evt) { _msgTree.applyFilter(); }
            });
            
            _filterTagLabel = new Label(_filterRow, SWT.NONE);
            _filterTagLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));

            _filterTag = new Combo(_filterRow, SWT.DROP_DOWN | SWT.BORDER);
            _filterTag.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));
            _filterTag.addTraverseListener(new TraverseListener() {
                public void keyTraversed(TraverseEvent evt) {
                    if (evt.detail == SWT.TRAVERSE_RETURN) _msgTree.applyFilter();
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
            if (_listener == null) {
                _advancedPreview.setEnabled(false);
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
            _advancedPreview.setSelection(shouldShowPreview(_ctl));
            _advancedMarkReadOnView.setSelection(shouldMarkReadOnView(_ctl));
            _advancedMarkReadOnPreview.setSelection(shouldMarkReadOnPreview(_ctl));
            _advancedDateImport.setSelection(shouldUseImportDate(_ctl));
            
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
            
            _advancedPreview.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                private void fire() {
                    setShouldShowPreview(_ctl, _advancedPreview.getSelection());
                    if (_listener != null)
                        _listener.togglePreview(_advancedPreview.getSelection());
                }
            });
            _advancedMarkReadOnView.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                private void fire() {
                    setShouldMarkReadOnView(_ctl, _advancedMarkReadOnView.getSelection());
                }
            });
            _advancedMarkReadOnPreview.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
                public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
                private void fire() {
                    setShouldMarkReadOnPreview(_ctl, _advancedMarkReadOnPreview.getSelection());
                }
            });
            
            _advancedDateImport.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) {
                    setShouldUseImportDate(_ctl, _advancedDateImport.getSelection());
                    _msgTree.applyFilter();
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    setShouldUseImportDate(_ctl, _advancedDateImport.getSelection());
                    _msgTree.applyFilter();
                }
            });
            
            _advancedPassphraseRequired.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent evt) {
                    if (_advancedPassphraseRequired.getSelection()) {
                        _advancedPrivacyPublic.setSelection(false);
                        _advancedPrivacyAuthorized.setSelection(false);
                        _advancedPrivacyPrivate.setSelection(false);
                        _advancedPrivacyPBE.setSelection(true);
                    }
                    _msgTree.applyFilter();
                }
                public void widgetSelected(SelectionEvent selectionEvent) {
                    if (_advancedPassphraseRequired.getSelection()) {
                        _advancedPrivacyPublic.setSelection(false);
                        _advancedPrivacyAuthorized.setSelection(false);
                        _advancedPrivacyPrivate.setSelection(false);
                        _advancedPrivacyPBE.setSelection(true);
                    }
                    _msgTree.applyFilter();
                }
            });
            
            _advancedScopeOther.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickForum(); }
                public void widgetSelected(SelectionEvent selectionEvent) { pickForum(); }
            });
            
            _ctl.getTranslationRegistry().register(this);
            _ctl.getThemeRegistry().register(this);
        }
        
        public void dispose() {
            _ctl.getTranslationRegistry().unregister(this);
            _ctl.getThemeRegistry().unregister(this);
        }
        
        private void pickForum() {
            if (_forumChooser == null) {
                _forumChooser = new ReferenceChooserPopup(_filterRow.getShell(), _ctl, new ReferenceChooserTree.AcceptanceListener () {
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
        
        public void pickDate() {
            _customDate = 1; // erm, do a popup
            String when = Constants.getDate(_customDate) + "...";
            _filterAge.setItem(AGE_CUSTOM, when);
            _filterAge.setText(when);
            _filterAge.select(AGE_CUSTOM);
            _msgTree.applyFilter();
        }
        public void setFilter(SyndieURI uri) {
            Long days = null;
            if (uri != null) { 
                days = uri.getLong("age");
                if (days == null)
                    days = uri.getLong("agelocal");
            }
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
            
            if (uri.isChannel() && (uri.getScope() != null) )
                _forumScopeOther = new Hash[] { uri.getScope() };
            else if (uri.isSearch()) {
                _forumScopeOther = uri.getSearchScopes();
            }
            _filterUnreadOnly.setSelection(uri.getBoolean("unreadonly", false));
            _advancedScopeAll.setSelection(false);
            _advancedScopeBookmarked.setSelection(false);
            _advancedScopeOther.setSelection(false);
            if (_forumScopeOther != null)
                _advancedScopeOther.setSelection(true);
            else
                _advancedScopeAll.setSelection(true);
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
            _filterAge.add(_ctl.getTranslationRegistry().getText(T_AGE_TODAY, "Today"));
            _filterAge.add(_ctl.getTranslationRegistry().getText(T_AGE_YESTERDAY, "Since yesterday"));
            _filterAge.add(_ctl.getTranslationRegistry().getText(T_AGE_THISWEEK, "This week"));
            _filterAge.add(_ctl.getTranslationRegistry().getText(T_AGE_LASTWEEK, "Since last week"));
            _filterAge.add(_ctl.getTranslationRegistry().getText(T_AGE_THISMONTH, "This month"));
            _filterAge.add(_ctl.getTranslationRegistry().getText(T_AGE_LASTMONTH, "Since last month"));
            if (_customDate > 0)
                _filterAge.add(Constants.getDate(_customDate) + "...");
            else
                _filterAge.add(_ctl.getTranslationRegistry().getText(T_AGE_CUSTOM, "Custom date..."));
            _filterAge.select(selected);
            _filterAge.setRedraw(true);
        }

        private static final String T_TAG_ALL = "syndie.gui.messagetree.tag.all";

        private void populateTagCombo() {
            String txt = _filterTag.getText().trim();
            int selected = -1;
            //if ( (_filterTag.getItemCount() > 0) && (txt.trim().length() == 0) )
            //    selected = _filterTag.getSelectionIndex();
            if ( (_filterTag.getItemCount() > 0) && (txt.length() > 0) )
                selected = _filterTag.indexOf(txt);
            _ctl.getUI().debugMessage("populateTagCombo text=[" + txt + "] selected [" + selected + "]");
            _filterTag.setRedraw(false);
            _filterTag.removeAll();
            TreeSet tags = new TreeSet(_msgTree.getTags());
            _filterTag.add(_ctl.getTranslationRegistry().getText(T_TAG_ALL, "Any tags"));
            for (Iterator iter = tags.iterator(); iter.hasNext(); ) {
                String tag = (String)iter.next();
                if (tag.trim().length() > 0)
                    _filterTag.add(tag.trim());
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
            _ctl.getUI().debugMessage("build filter, tree has [" + filter + "]");
            try {
                if ( (filter == null) || (filter.trim().length() <= 0) )
                    filter = SyndieURI.DEFAULT_SEARCH_URI.toString();
                uri = new SyndieURI(filter);
            } catch (URISyntaxException use) {
                _ctl.getUI().debugMessage("build filter, tree is invalid [" + filter + "]", use);
                uri = SyndieURI.DEFAULT_SEARCH_URI;
            }

            Map attributes = uri.getAttributes();
            
            _ctl.getUI().debugMessage("build filter w/ base attributes: " + attributes);
            
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
            if (_advancedDateImport.getSelection()) {
                attributes.put("agelocal", new Integer(days));
                attributes.remove("age");
            } else {
                attributes.put("age", new Integer(days));
                attributes.remove("agelocal");
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
                attributes.put("scope", getBookmarkedScopes());
            }
            
            _ctl.getUI().debugMessage("buildFilter scope: " + attributes.get("scope") + " otherScope: " + _forumScopeOther);
            
            if (_advancedPrivacyPBE.getSelection())
                attributes.put("pbe", Boolean.TRUE.toString());
            else
                attributes.put("pbe", Boolean.FALSE.toString());
            if (_advancedPrivacyPrivate.getSelection())
                attributes.put("private", Boolean.TRUE.toString());
            else
                attributes.put("private", Boolean.FALSE.toString());
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
            _ctl.getUI().debugMessage("building filter w/ new tag [" + tag + "] and age [" + days + "]: " + rv);
            return rv;
        }
        
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
        
        public void translate(TranslationRegistry registry) {
            _filterLabel.setText(registry.getText(T_FILTER_LABEL, "Filters:"));

            _filterAdvanced.setText(registry.getText(T_FILTER_ADVANCED, "Advanced..."));
            _filterKeywordLabel.setText(registry.getText(T_FILTER_KEYWORD, "Text:"));
            _filterTagLabel.setText(registry.getText(T_FILTER_TAG, "Tag:"));
            _filterUnreadOnly.setText(registry.getText(T_FILTER_UNREAD, "Unread only"));
            _filterKeyword.setToolTipText(registry.getText(T_FILTER_KEYWORD_TOOLTIP, "Search pages and subjects for the phrase"));
            
            _advancedScopeAll.setText(registry.getText(T_ADVANCED_SCOPE_ALL, "All forums"));
            _advancedScopeBookmarked.setText(registry.getText(T_ADVANCED_SCOPE_BOOKMARKED, "Bookmarked forums"));
            _advancedScopeOther.setText(registry.getText(T_ADVANCED_SCOPE_OTHER, "Specific forum..."));
            
            _advancedPrivacyPublic.setText(registry.getText(T_ADVANCED_PRIVACY_PUBLIC, "Readable by: anyone"));
            _advancedPrivacyAuthorized.setText(registry.getText(T_ADVANCED_PRIVACY_AUTHORIZED, "Readable by: authorized readers"));
            _advancedPrivacyPBE.setText(registry.getText(T_ADVANCED_PRIVACY_PBE, "Readable by: those with a passphrase"));
            _advancedPrivacyPrivate.setText(registry.getText(T_ADVANCED_PRIVACY_PRIVATE, "Readable by: forum administrators"));
            _advancedDateImport.setText(registry.getText(T_ADVANCED_DATEIMPORT, "Use local import date instead of (unreliable) message creation date"));
            _advancedThreadResults.setText(registry.getText(T_ADVANCED_THREAD, "Organize results in threads"));
            _advancedPreview.setText(registry.getText(T_ADVANCED_PREVIEW, "Show a preview of the selected message"));
            _advancedMarkReadOnView.setText(registry.getText(T_ADVANCED_MARKREADVIEW, "Mark messages read when opened"));
            _advancedMarkReadOnPreview.setText(registry.getText(T_ADVANCED_MARKREADPREVIEW, "Mark messages read when previewed"));
            _advancedPassphraseRequired.setText(registry.getText(T_ADVANCED_PASSPHRASE_REQUIRED, "Messages requiring a new passphrase"));
            
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
        _tree = new Tree(_root, SWT.BORDER | SWT.SINGLE | SWT.FULL_SELECTION | SWT.VIRTUAL);
        _tree.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
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
                _itemToMsgId.put(item, new Long(itemNode.getUniqueId()));
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
        
        _colSubject = new TreeColumn(_tree, SWT.LEFT);
        _colType = new TreeColumn(_tree, SWT.LEFT);
        _colAuthor = new TreeColumn(_tree, SWT.LEFT);
        _colChannel = new TreeColumn(_tree, SWT.LEFT);
        _colDate = new TreeColumn(_tree, SWT.LEFT);
        _colTags = new TreeColumn(_tree, SWT.LEFT);
        
        if (_showFlags)
            _colType.setWidth(getMessageFlagBarWidth(_tree));
        else
            _colType.setWidth(1);
        //_tree.addListener(SWT.MeasureItem, new Listener() {
        //    public void handleEvent(Event evt) {
        //        if (evt.index == 1) evt.width = getMessageFlagBarWidth(_tree);
        //    }
        //});
        _tree.addListener(SWT.PaintItem, new Listener() {
            public void handleEvent(Event evt) {
                if (!_showFlags) return;
                if (evt.index == 1) {
                    MessageFlagBar bar = (MessageFlagBar)_itemToMsgFlags.get(evt.item);
                    if (bar == null) {
                        Long msgId = (Long)_itemToMsgId.get(evt.item);
                        MessageInfo msg = _client.getMessage(msgId.longValue());
                        bar = new MessageFlagBar(_browser, _tree, false);
                        bar.setMessage(msg);
                        _itemToMsgFlags.put(evt.item, bar);
                    }
                    Image imgs[] = bar.getFlags();
                    //String tt = bar.getTooltip();
                    int off = evt.x;
                    //_browser.getUI().debugMessage("paint height:" + evt.height + " y:" + evt.y + " x:" + evt.x);
                    for (int i = 0; i < imgs.length; i++) {
                        Rectangle sz = imgs[i].getBounds();
                        int excess = evt.height-sz.height;
                        if (excess > 1)
                            evt.gc.drawImage(imgs[i], off, evt.y + excess/2);
                        else
                            evt.gc.drawImage(imgs[i], off, evt.y + excess/2);
                        off += imgs[i].getBounds().width + FLAG_SPACING;
                    }
                }
            }
        });
        
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
                _bookmarkAuthor.setEnabled(enable);
                _bookmarkForum.setEnabled(enable);
                _markAllRead.setEnabled(enable);
                _markRead.setEnabled(enable);
                _markThreadRead.setEnabled(enable);
                _markUnread.setEnabled(enable);
                _view.setEnabled(enable);
                _reply.setEnabled(enable);
                _viewAuthor.setEnabled(enable);
                _viewAuthorMeta.setEnabled(enable);
                _viewForum.setEnabled(enable);
                _viewForumMeta.setEnabled(enable);
            }
        });
        
        _view = new MenuItem(_menu, SWT.PUSH);
        _view.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewSelected(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewSelected(); }
        });
        _reply = new MenuItem(_menu, SWT.PUSH);
        _reply.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { replySelected(); }
            public void widgetSelected(SelectionEvent selectionEvent) { replySelected(); }
        });
        new MenuItem(_menu, SWT.SEPARATOR);
        _viewForum = new MenuItem(_menu, SWT.PUSH);
        _viewForum.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewSelectedForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewSelectedForum(); }
        });
        _viewForumMeta = new MenuItem(_menu, SWT.PUSH);
        _viewForumMeta.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewSelectedForumMeta(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewSelectedForumMeta(); }
        });
        _bookmarkForum = new MenuItem(_menu, SWT.PUSH);
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
        _markRead = new MenuItem(_menu, SWT.PUSH);
        _markRead.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { markRead(); }
            public void widgetSelected(SelectionEvent selectionEvent) { markRead(); }
        });
        _markUnread = new MenuItem(_menu, SWT.PUSH);
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
        
        if (!_hideFilter) {
            Composite filterRow = new Composite(_root, SWT.BORDER);
            filterRow.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            filterRow.setLayout(new GridLayout(8, false));
            createFilterBar(filterRow, null);
        }
        
        SyndieTreeListener lsnr = new SyndieTreeListener(_tree) {
            public void selected() { fireSelected(false); }
            public void returnHit() { fireSelected(true); }
            public void doubleclick() { fireSelected(true); }
            public boolean collapseOnReturn() { return false; }
            public void keyReleased(KeyEvent evt) {
                // refresh the pane on ^R
                if ( (_filterable) && (evt.character == 0x12) & ((evt.stateMask & SWT.MOD1) != 0) ) {
                    applyFilter();
                    return;
                }
                super.keyReleased(evt);
            }
        };
        _tree.addSelectionListener(lsnr);
        _tree.addControlListener(lsnr);
        _tree.addTraverseListener(lsnr);
        _tree.addKeyListener(lsnr);
        _tree.addMouseListener(lsnr);
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
        
        initDnD();
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
                if (uri == null)
                    evt.doit = false; // don't drag when nothing is selected
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

    private static final int getMessageFlagBarWidth(Tree tree) {
        // to avoid a bunch of messageflagbar calcs, lets just assume 8 flags
        int width = tree.getGridLineWidth() * 2;
        width += 8 * (ImageUtil.ICON_MSG_FLAG_AUTHENTICATED.getBounds().width + FLAG_SPACING);
        return width;
    }
    
    private void toggleSort(TreeColumn column) {
        if (!_filterable) return;
        if (column == _currentSortColumn) {
            _currentSortDirection = (_currentSortDirection == SWT.UP ? SWT.DOWN : SWT.UP);
            _browser.getUI().debugMessage("toggleSort direction on " + column.getText());
        } else {
            _browser.getUI().debugMessage("toggleSort column on " + column.getText());
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
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
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
                _browser.getUI().debugMessage("Good filter set [" + filter + "]");
            } catch (URISyntaxException use) {
                _browser.getUI().debugMessage("Bad filter set [" + filter + "]", use);
                uri = new SyndieURI("search", new HashMap());
                _filter = "";
            }
        } else {
            _browser.getUI().debugMessage("Blank filter set");
            _filter = "";
        }
    
        for (int i = 0; i < _bars.size(); i++)
            ((FilterBar)_bars.get(i)).setFilter(uri);
        //_filterBar.setFilter(uri);
    }
    
    public void applyFilter() {
        String filter = null;
        for (int i = 0; i < _bars.size() && filter == null; i++)
            filter = ((FilterBar)_bars.get(i)).buildFilter();
        applyFilter(filter);
    }
    private transient boolean _filtering;
    public void applyFilter(String filter) {
        if (!_filterable) return;
        boolean alreadyFiltering = false;
        synchronized (MessageTree.this) {
            if (_filtering)
                alreadyFiltering = true;
            _filtering = true;
        }
        if (alreadyFiltering) {
            _browser.getUI().debugMessage("filter already in progress, not applying...");
            return;
        }
        final Shell s = showFilteringWidget();
        _tree.setEnabled(false);
        _filter = filter;
        final String txt = _filter;
        if (txt.trim().length() > 0) {
            try {
                final SyndieURI uri = new SyndieURI(txt);
                JobRunner.instance().enqueue(new Runnable() {
                    public void run() {
                        try {
                            _browser.getUI().debugMessage("begin async calculating nodes in the tree: " + uri.toString());
                            //try { Thread.sleep(200); } catch (InterruptedException ie) {}
                            applyFilter(txt, uri, calculateNodes(uri)); 
                            _browser.getUI().debugMessage("end async calculating nodes in the tree");
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
            } catch (URISyntaxException use) {
                // noop
                //System.out.println("filter applied was not valid, noop [" + use.getMessage() + "]");
                s.dispose();
                _tree.setEnabled(true);
            }
        }
    }
    
    private static final String T_FILTERING_LABEL = "syndie.gui.messagetree.filtering";
    private Shell showFilteringWidget() {
        Shell s = new Shell(_root.getShell(), SWT.NO_TRIM | SWT.APPLICATION_MODAL | SWT.ON_TOP);
        s.setLayout(new FillLayout());
        Composite c = new Composite(s, SWT.BORDER);
        c.setLayout(new RowLayout(SWT.HORIZONTAL));
        Label label = new Label(c, SWT.NONE);
        label.setText(_browser.getTranslationRegistry().getText(T_FILTERING_LABEL, "Message filtering in progress"));
        final Label filler = new Label(c, SWT.NONE);
        label.setFont(_browser.getThemeRegistry().getTheme().SHELL_FONT);
        filler.setFont(_browser.getThemeRegistry().getTheme().SHELL_FONT);
        filler.setText("\\");
        LivelinessIndicator liv = new LivelinessIndicator(filler);
        liv.run();
        s.pack();
        Point sz = s.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        Point src = _root.getShell().getSize();
        //if (src.x + src.y <= 0)
        //    src = _root.getParent().computeSize(SWT.DEFAULT, SWT.DEFAULT);
        _browser.getUI().debugMessage("show liveliness: sz=" + sz + " src=" + src);
        Rectangle rect = Display.getDefault().map(_root.getShell(), null, src.x/2-sz.x/2, src.y/2-sz.y/2, sz.x, sz.y);
        s.setBounds(rect);
        s.open();
        return s;
    }
    private class LivelinessIndicator implements Runnable {
        private Label _label;
        private int _index;
        public LivelinessIndicator(Label label) { _label = label; _index = 0; }
        public void run() {
            char c = ' ';
            switch (_index % 4) {
                case 0: c = '\\'; break;
                case 1: c = '|'; break;
                case 2: c = '/'; break;
                case 3: c = '-'; break;
            }
            _index++;
            String str = "" + c;
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
                _browser.getUI().debugMessage("nodes calculated, setting them");
                setMessages(nodes);
                _browser.getUI().debugMessage("nodes set w/ " + uri);
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
        //ThreadAccumulator acc = new ThreadAccumulator(_client, _browser.getUI());
        ThreadAccumulator acc = new ThreadAccumulatorJWZ(_client, _browser.getUI());
        _browser.getUI().debugMessage("setting the filter: " + uri.toString());
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
        _browser.getUI().debugMessage("gathering the threads");
        acc.gatherThreads();
        List threads = new ArrayList();
        for (int i = 0; i < acc.getThreadCount(); i++)
            threads.add(acc.getRootThread(i));
        return threads;
    }
    
    void setMessages(List referenceNodes) {
        _tree.setRedraw(false);
        _tree.removeAll();
        _itemToURI.clear();
        _itemToMsgId.clear();
        // MessageFlagBar instances may allocate icons
        for (Iterator iter = _itemToMsgFlags.values().iterator(); iter.hasNext(); )
            ((MessageFlagBar)iter.next()).dispose();
        _itemToMsgFlags.clear();
        _itemsNewUnread.clear();
        _tags.clear();
        _itemToNode.clear();
        _threadReferenceNodes = referenceNodes;
        long totalDBTime = 0;
        long before = System.currentTimeMillis();
        _tree.setItemCount(referenceNodes != null ? referenceNodes.size() : 0);
        // done on-demand via the virtual tree
        /*
        for (int i = 0; i < referenceNodes.size(); i++) {
            ReferenceNode node = (ReferenceNode)referenceNodes.get(i);
            totalDBTime += add(node, null);
        }
         */
        long after = System.currentTimeMillis();
        _browser.getUI().debugMessage("setting messages: db time: " + totalDBTime + " for " + referenceNodes.size() + ", total add time: " + (after-before));
        
        long beforeGetTags = System.currentTimeMillis();
        _tags.addAll(getTags(referenceNodes));
        long afterGetTags = System.currentTimeMillis();
        _browser.getUI().debugMessage("get all tags took " + (afterGetTags-beforeGetTags) + " for " + _tags.size() + " tags");
        
        for (int i = 0; i < _bars.size(); i++)
            ((FilterBar)_bars.get(i)).populateTagCombo();
        
        _tree.setSortColumn(_currentSortColumn);
        _tree.setSortDirection(_currentSortDirection);
        _tree.setRedraw(true);
    }
    
    private Set getTags(List nodes) {
        IDGatherer idGatherer = new IDGatherer();
        ReferenceNode.walk(nodes, idGatherer);
        return _client.getMessageTags(idGatherer.getIds(), true, true);
    }
    private static class IDGatherer implements ReferenceNode.Visitor {
        private Set _ids;
        public IDGatherer() { _ids = new HashSet(); }
        public Set getIds() { return _ids; }
        public void visit(ReferenceNode node, int depth, int siblingOrder) {
            _ids.add(new Long(node.getUniqueId()));
        }
    }
    
    protected long renderNode(ReferenceNode node, TreeItem item) {
        SyndieURI uri = node.getURI();
        String subj = "";
        String auth = "";
        String chan = "";
        String date = "";
        String tags = "";
        int status = DBClient.MSG_STATUS_UNREAD;

        long dbStart = System.currentTimeMillis();

        if (uri != null) {
            long chanId = _client.getChannelId(uri.getScope());
            String scopeName = _client.getChannelName(chanId);
            //ChannelInfo scopeInfo = _client.getChannel(chanId);
            //MessageInfo msg = _client.getMessage(chanId, uri.getMessageId());

            // simple optimization: use the fact that these ReferenceNode instances are really
            // ThreadReferenceNode instances, which contain subject, msgId, target, etc.

            long msgId = _client.getMessageId(chanId, uri.getMessageId().longValue());
            if (msgId >= 0) {
                _itemToMsgId.put(item, new Long(msgId));
                subj = _client.getMessageSubject(msgId);
                long authorId = _client.getMessageAuthor(msgId);//msg.getAuthorChannelId();
                if (authorId != chanId) {
                    String authorName = _client.getChannelName(authorId);
                    Hash authorHash = _client.getChannelHash(authorId);
                    //ChannelInfo authInfo = _client.getChannel(authorId);
                    if (authorName != null) {
                        auth = authorName + " [" + authorHash.toBase64().substring(0,6) + "]";
                    } else {
                        auth = "";
                    }
                    //System.out.println("author is NOT the scope chan for " + uri.toString() + ": " + auth);
                } else {
                    //System.out.println("author is the scope chan for " + uri.toString());
                    auth = scopeName + " [" + uri.getScope().toBase64().substring(0,6) + "]";
                }
                //ChannelInfo chanInfo = scopeInfo;
                long targetChanId = _client.getMessageTarget(msgId);
                if (targetChanId != chanId) {
                    //System.out.println("target chan != scope chan: " + msg.getTargetChannel().toBase64() + "/" + msg.getTargetChannelId() + " vs " + scopeInfo.getChannelHash().toBase64() + "/" + scopeInfo.getChannelId());
                    //System.out.println("msg: " + uri.toString());
                    String targetName = _client.getChannelName(targetChanId);
                    Hash targetHash = _client.getChannelHash(targetChanId);
                    //chanInfo = _client.getChannel(msg.getTargetChannelId());
                    chan = targetName + " [" + targetHash.toBase64().substring(0,6) + "]";
                    //if (chanInfo == null) {
                    //    chan = "[" + msg.getTargetChannel().toBase64().substring(0,6) + "]";
                    //} else {
                    //    chan = chanInfo.getName() + " [" + chanInfo.getChannelHash().toBase64().substring(0,6) + "]";
                    //}
                } else {
                    //System.out.println("target chan == scope chan: " + msg.getTargetChannel().toBase64() + "/" + msg.getTargetChannelId() + "/" + msg.getInternalId() + "/" + msg.getScopeChannelId() + "/" + msg.getAuthorChannelId());
                    //System.out.println("msg: " + uri.toString());
                    chan = scopeName  + " [" + uri.getScope().toBase64().substring(0,6) + "]";
                }

                if (auth.length() <= 0) {
                     auth = chan;
                }
                Set msgTags = _client.getMessageTags(msgId, true, true);
                StringBuffer buf = new StringBuffer();
                for (Iterator iter = msgTags.iterator(); iter.hasNext(); ) {
                    String tag = (String)iter.next();
                    tag = tag.trim();
                    buf.append(tag).append(" ");
                    //_tags.add(tag);
                }
                tags = buf.toString().trim();
                item.setGrayed(false);
                long importDate = _client.getMessageImportDate(msgId);
                long postDate = uri.getMessageId().longValue();
                if ( (_appliedFilter == null) || (_appliedFilter.getString("agelocal") != null) ) {
                    date = Constants.getDate(importDate);
                    _browser.getUI().debugMessage("using local import date for " + msgId + ": " + date + " (instead of " + Constants.getDate(postDate) + ")");
                } else {
                    date = Constants.getDate(postDate);
                    _browser.getUI().debugMessage("using post date for " + msgId + ": " + date + " (instead of " + Constants.getDate(importDate) + ")");
                }
                status = _client.getMessageStatus(_client.getLoggedInNymId(), msgId, targetChanId);
            } else {
                // message is not locally known
                subj = "";
                if (scopeName != null)
                    auth = scopeName + " [" + uri.getScope().toBase64().substring(0,6) + "]";
                else
                    auth = "[" + uri.getScope().toBase64().substring(0,6) + "]";
                chan = "";
                date = Constants.getDate(uri.getMessageId().longValue());
                tags = "";
            }
        }

        long dbEnd = System.currentTimeMillis();

        if ( (subj == null) || (subj.trim().length() <= 0) )
            item.setText(0, MessageView.calculateSubject(_browser, uri));
        else
            item.setText(0, subj);
        // msgbar stuff
        // defer this to the paint() - we only paint the rows we need (which may be << total rows, expanded)
        //MessageFlagBar bar = new MessageFlagBar(_browser, _tree, false);
        //bar.setMessage(msg);
        //_itemToMsgFlags.put(item, bar);
        //if ( (msg != null) && (msg.getWasPrivate()) )
        //    item.setImage(1, ImageUtil.ICON_MSG_TYPE_PRIVATE);
        //else
        //    item.setImage(1, ImageUtil.ICON_MSG_TYPE_NORMAL);
        item.setText(2, auth);
        item.setText(3, chan);
        item.setText(4, date);
        item.setText(5, tags);
        Font f = null;
        if ( (status == DBClient.MSG_STATUS_READ) || (uri == null) ) {
            f = _browser.getThemeRegistry().getTheme().MSG_OLD_FONT;
        } else {
            _itemsNewUnread.add(item);
            f = _browser.getThemeRegistry().getTheme().MSG_NEW_UNREAD_FONT;
        }
        
        synchronized (this) {
            item.setFont(f);
        }
        
        // note: this will only retheme the ancestors of unread messages /that have been rendered/
        rethemeAncestorsOfUnread();
        
        setMinWidth(_colSubject, subj, 0, 100);
        setMinWidth(_colAuthor, auth, 0, 50);
        setMinWidth(_colChannel, chan, 0, 50);
        setMinWidth(_colDate, date, 20, 50);
        setMinWidth(_colTags, tags, 0, 50);
        if (!_showChannel) _colChannel.setWidth(1);
        //_browser.getUI().debugMessage("message status: " + status);
        return dbEnd-dbStart;
    }
    
    private static final String T_NO_SUBJECT = "syndie.gui.messagetree.nosubject";
    
    protected void setMinWidth(TreeColumn col, String txt, int extra, int min) {
        int width = ImageUtil.getWidth(txt, _tree) + _tree.getGridLineWidth()*2 + extra;
        if (width < min)
            width = min;
        int existing = col.getWidth();
        if (width > existing)
            col.setWidth(width);
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
    private void replySelected() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                long msgId = _browser.getClient().getMessageId(uri.getScope(), uri.getMessageId());
                long targetId = _browser.getClient().getMessageTarget(msgId);
                Hash target = _browser.getClient().getChannelHash(targetId);
                _browser.view(_browser.createPostURI(target, uri));
            }
        }
    }
    private void viewSelectedForum() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                long targetId = _client.getMessageTarget(msgId);
                Hash scope = _client.getChannelHash(targetId);
                _browser.view(SyndieURI.createScope(scope));
            }
        }
    }
    private void viewSelectedForumMeta() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                long targetId = _client.getMessageTarget(msgId);
                Hash scope = _client.getChannelHash(targetId);
                _browser.view(_browser.createMetaURI(scope));
            }
        }
    }
    private void bookmarkSelectedForum() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                long targetId = _client.getMessageTarget(msgId);
                Hash scope = _client.getChannelHash(targetId);
                _browser.bookmark(SyndieURI.createScope(scope));
            }
        }
    }
    private void viewSelectedAuthor() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                long authorId = _client.getMessageAuthor(msgId);
                Hash scope = _client.getChannelHash(authorId);
                _browser.view(SyndieURI.createScope(scope));
            }
        }
    }
    private void viewSelectedAuthorMeta() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                long authorId = _client.getMessageAuthor(msgId);
                Hash scope = _client.getChannelHash(authorId);
                _browser.view(_browser.createMetaURI(scope));
            }
        }
    }
    private void bookmarkSelectedAuthor() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                long authorId = _client.getMessageAuthor(msgId);
                Hash scope = _client.getChannelHash(authorId);
                _browser.bookmark(SyndieURI.createScope(scope));
            }
        }
    }
    
    private void markRead() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                Long msgId = (Long)_itemToMsgId.get(selected[i]);
                if (msgId != null) {
                    if (_itemsNewUnread.contains(selected[i])) {
                        _browser.getUI().debugMessage("mark an unread message as read (" + msgId + ")");
                        _browser.getClient().markMessageRead(msgId.longValue());
                        selected[i].setFont(_browser.getThemeRegistry().getTheme().MSG_NEW_READ_FONT);
                        _itemsNewUnread.remove(selected[i]);
                    }
                }
            }
            _browser.readStatusUpdated();
        }
    }
    private void markUnread() {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                Long msgId = (Long)_itemToMsgId.get(selected[i]);
                if (msgId != null) {
                    _browser.getUI().debugMessage("mark as unread (" + msgId + ")");
                    _browser.getClient().markMessageUnread(msgId.longValue());
                    selected[i].setFont(_browser.getThemeRegistry().getTheme().MSG_NEW_UNREAD_FONT);
                    _itemsNewUnread.add(selected[i]);
                }
            }
            _browser.readStatusUpdated();
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
                _browser.readStatusUpdated();
        }
    }
    protected TreeItem getThreadRoot(TreeItem item) {
        TreeItem root = item;
        while (root.getParentItem() != null)
            root = root.getParentItem();
        return root;
    }
    private void markThreadRead(TreeItem item) {
        Long msgId = (Long)_itemToMsgId.get(item);
        if (msgId != null) {
            if (_itemsNewUnread.contains(item)) {
                _browser.getClient().markMessageRead(msgId.longValue());
                item.setFont(_browser.getThemeRegistry().getTheme().MSG_NEW_READ_FONT);
                _itemsNewUnread.remove(item);
            }
        }
        TreeItem children[] = item.getItems();
        for (int i = 0; i < children.length; i++)
            markThreadRead(children[i]);
    }
    private void markAllRead() {
        TreeItem selected[] = _tree.getSelection();
        if ( (selected != null) && (selected.length > 0) ) {
            HashSet channelIds = new HashSet();
            for (int i = 0; i < selected.length; i++) {
                Long msgId = (Long)_itemToMsgId.get(selected[i]);
                if (msgId != null) {
                    long target = _client.getMessageTarget(msgId.longValue());
                    channelIds.add(new Long(target));
                    _browser.getClient().markChannelRead(target);
                }
            }
            if (channelIds.size() > 0)
                _browser.readStatusUpdated();
            
            for (Iterator iter = _itemToURI.entrySet().iterator(); iter.hasNext(); ) {
                Map.Entry cur = (Map.Entry)iter.next();
                TreeItem item = (TreeItem)cur.getKey();
                SyndieURI uri = (SyndieURI)cur.getValue();
                
                if (uri != null) {
                    long msgId = _browser.getClient().getMessageId(uri.getScope(), uri.getMessageId());
                    if (msgId >= 0) {
                        long target = _browser.getClient().getMessageTarget(msgId);
                        if (channelIds.contains(new Long(target))) {
                            _itemsNewUnread.remove(item);
                            item.setFont(_browser.getThemeRegistry().getTheme().MSG_OLD_FONT);
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

    private static final String T_SUBJECT = "syndie.gui.messagetree.subject";
    private static final String T_AUTHOR = "syndie.gui.messagetree.author";
    private static final String T_FORUM = "syndie.gui.messagetree.forum";
    private static final String T_DATE = "syndie.gui.messagetree.date";
    private static final String T_TAGS = "syndie.gui.messagetree.tags";
    private static final String T_FILTER_LABEL = "syndie.gui.messagetree.filter.label";
    private static final String T_FILTER_UNREAD = "syndie.gui.messagetree.filter.unread";
    private static final String T_FILTER_ADVANCED = "syndie.gui.messagetree.filter.advanced";
    private static final String T_FILTER_EDIT_SHELL = "syndie.gui.messagetree.filter.edit.shell";
    private static final String T_FILTER_TAG = "syndie.gui.messagetree.filter.tag";
    private static final String T_FILTER_KEYWORD = "syndie.gui.messagetree.filter.keyword";
    private static final String T_FILTER_KEYWORD_TOOLTIP = "syndie.gui.messagetree.filter.keywordtooltip";
    
    private static final String T_ADVANCED_SCOPE_ALL = "syndie.gui.messagetree.filteradvanced.scope.all";
    private static final String T_ADVANCED_SCOPE_BOOKMARKED = "syndie.gui.messagetree.filteradvanced.scope.bookmarked";
    private static final String T_ADVANCED_SCOPE_OTHER = "syndie.gui.messagetree.filteradvanced.scope.other";
    private static final String T_ADVANCED_PRIVACY_PUBLIC = "syndie.gui.messagetree.filteradvanced.priv.public";
    private static final String T_ADVANCED_PRIVACY_AUTHORIZED = "syndie.gui.messagetree.filteradvanced.priv.auth";
    private static final String T_ADVANCED_PRIVACY_PBE = "syndie.gui.messagetree.filteradvanced.priv.pbe";
    private static final String T_ADVANCED_PRIVACY_PRIVATE = "syndie.gui.messagetree.filteradvanced.priv.private";
    private static final String T_ADVANCED_DATEIMPORT = "syndie.gui.messagetree.filteradvanced.dateimport";
    private static final String T_ADVANCED_THREAD = "syndie.gui.messagetree.filteradvanced.thread";
    private static final String T_ADVANCED_PREVIEW = "syndie.gui.messagetree.filteradvanced.preview";
    private static final String T_ADVANCED_MARKREADVIEW = "syndie.gui.messagetree.filteradvanced.markreadview";
    private static final String T_ADVANCED_MARKREADPREVIEW = "syndie.gui.messagetree.filteradvanced.markreadpreview";

    private static final String T_ADVANCED_PASSPHRASE_REQUIRED = "syndie.gui.messagetree.filteradvanced.passrequired";
    
    private static final String T_VIEW = "syndie.gui.messagetree.view";
    private static final String T_VIEWFORUM = "syndie.gui.messagetree.viewforum";
    private static final String T_VIEWFORUMMETA = "syndie.gui.messagetree.viewforummeta";
    private static final String T_VIEWAUTHOR = "syndie.gui.messagetree.viewauthor";
    private static final String T_VIEWAUTHORMETA = "syndie.gui.messagetree.viewauthormeta";
    private static final String T_MARKREAD = "syndie.gui.messagetree.markread";
    private static final String T_MARKTHREADREAD = "syndie.gui.messagetree.markthreadread";
    private static final String T_MARKUNREAD = "syndie.gui.messagetree.markunread";
    private static final String T_MARKALLREAD = "syndie.gui.messagetree.markallread";
    private static final String T_BOOKMARKFORUM = "syndie.gui.messagetree.bookmarkforum";
    private static final String T_BOOKMARKAUTHOR = "syndie.gui.messagetree.bookmarkauthor";
    
    public void translate(TranslationRegistry registry) {
        _colSubject.setText(registry.getText(T_SUBJECT, "Subject"));
        _colAuthor.setText(registry.getText(T_AUTHOR, "Author"));
        _colChannel.setText(registry.getText(T_FORUM, "Forum"));
        _colDate.setText(registry.getText(T_DATE, "Date"));
        _colTags.setText(registry.getText(T_TAGS, "Tags"));
        
        _view.setText(registry.getText(T_VIEW, "View the message"));
        _reply.setText(registry.getText(T_VIEW, "Reply to the message"));
        _viewForum.setText(registry.getText(T_VIEWFORUM, "View the forum's messages"));
        _viewForumMeta.setText(registry.getText(T_VIEWFORUMMETA, "View the forum's profile"));
        _viewAuthor.setText(registry.getText(T_VIEWAUTHOR, "View the author's blog"));
        _viewAuthorMeta.setText(registry.getText(T_VIEWAUTHORMETA, "View the author's profile"));
        _bookmarkForum.setText(registry.getText(T_BOOKMARKFORUM, "Bookmark the forum"));
        _bookmarkAuthor.setText(registry.getText(T_BOOKMARKAUTHOR, "Bookmark the author"));
        _markRead.setText(registry.getText(T_MARKREAD, "Mark the message as read"));
        _markThreadRead.setText(registry.getText(T_MARKTHREADREAD, "Mark the thread as read"));
        _markUnread.setText(registry.getText(T_MARKUNREAD, "Mark the message as unread"));
        _markAllRead.setText(registry.getText(T_MARKALLREAD, "Mark the forum as read"));
    }
    
    public void applyTheme(Theme theme) {
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
                    item.setFont(_browser.getThemeRegistry().getTheme().MSG_NEW_UNREAD_FONT);
                else
                    item.setFont(_browser.getThemeRegistry().getTheme().MSG_OLD_FONT);
            }
            rethemeAncestorsOfUnread();
        }
    }
    private void rethemeAncestorsOfUnread() {
        // now readjust the font of read/old message ancestors of unread messages
        synchronized (this) {
            for (Iterator iter = _itemsNewUnread.iterator(); iter.hasNext(); ) {
                TreeItem item = (TreeItem)iter.next();
                TreeItem parent = item.getParentItem();
                while (parent != null) {
                    if (!_itemsNewUnread.contains(parent))
                        markUnreadChild(parent);
                    parent = parent.getParentItem();
                }
            }
        }
    }
    protected void markUnreadChild(TreeItem item) {
        item.setFont(_browser.getThemeRegistry().getTheme().MSG_UNREAD_CHILD_FONT);
    }
}
