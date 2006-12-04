package syndie.gui;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.i2p.data.Base64;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ExpandEvent;
import org.eclipse.swt.events.ExpandListener;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.ExpandBar;
import org.eclipse.swt.widgets.ExpandItem;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.SyndieURI;
import syndie.db.CommandImpl;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 *
 */
public class MessageTreeFilter implements ReferenceChooserTree.AcceptanceListener, Translatable {
    private BrowserControl _browser;
    private DBClient _client;
    private Composite _parent;
    private MessageTree _tree;
    private Hash _channels[];
    private Composite _root;
    private Text _filterText;
    private ExpandBar _bar;
    // filter by scope
    private ExpandItem _itemScope;
    private Composite _scope;
    private Label _forumLabel;
    private Text _forumName;
    private Button _forumSelect;
    private Label _authorLabel;
    private Group _authorGroup;
    private Button _authorAuthorized;
    private Button _authorManager;
    private Button _authorOwner;
    private Button _authorAny;
    // filter by age
    private ExpandItem _itemAge;
    private Group _age;
    private Button _ageDay;
    private Button _age2Days;
    private Button _ageWeek;
    private Button _age2Weeks;
    private Button _ageMonth;
    private Button _age2Months;
    private Button _ageCustom;
    private Text _ageCustomText;
    // filter by tags
    private ExpandItem _itemTags;
    private Composite _tags;
    private Label _tagsInclude;
    private Text _tagsRequireText;
    private Label _tagsRequire;
    private Text _tagsIncludeText;
    private Label _tagsExclude;
    private Text _tagsExcludeText;
    private Button _tagsApplyToMessages;
    // filter by content
    private ExpandItem _itemContent;
    private Composite _content;
    private Group _contentPageGroup;
    private Button _contentPageYes;
    private Button _contentPageNo;
    private Button _contentPageIgnore;
    private Group _contentAttachGroup;
    private Button _contentAttachYes;
    private Button _contentAttachNo;
    private Button _contentAttachIgnore;
    private Group _contentRefGroup;
    private Button _contentRefYes;
    private Button _contentRefNo;
    private Button _contentRefIgnore;
    private Group _contentKeyGroup;
    private Button _contentKeyYes;
    private Button _contentKeyNo;
    private Button _contentKeyIgnore;
    // filter by status
    private ExpandItem _itemStatus;
    private Composite _status;
    private Button _statusDecrypted;
    private Button _statusPBE;
    private Button _statusPrivate;
    // display options
    private ExpandItem _itemDisplay;
    private Composite _display;
    private Button _displayThreaded;
    
    private Button _ok;
    private Button _cancel;
    
    private FilterModifyListener _modListener;
    
    private ReferenceChooserPopup _refChooser;
    
    public MessageTreeFilter(BrowserControl browser, Composite parent, MessageTree tree) {
        _browser = browser;
        _client = browser.getClient();
        _parent = parent;
        _tree = tree;
        _modListener = new FilterModifyListener();
        _refChooser = new ReferenceChooserPopup(_parent.getShell(), browser, this);
        initComponents();
    }

    public Control getControl() { return _root; }
    public void setFilter(String filter) { 
        _filterText.setText(filter); 
        parseFilter();
        _itemAge.setExpanded(true);
    }
    
    private void initComponents() {
        _browser.getUI().debugMessage("** init msgTreeFilter");
        _root = new Composite(_parent, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        //gl.marginLeft = 10;
        //gl.marginRight = 0;
        _root.setLayout(gl);
        
        _filterText = new Text(_root, SWT.SINGLE | SWT.BORDER);
        _filterText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _filterText.addFocusListener(new FocusListener() {
            public void focusGained(FocusEvent focusEvent) {}
            public void focusLost(FocusEvent evt) { parseFilter(); }
        });
        
        _bar = new ExpandBar(_root, SWT.V_SCROLL);
        _bar.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _browser.getUI().debugMessage("** init msgTreeFilter: build filters");
        
        buildScopeFilter();
        buildAgeFilter();
        buildTagsFilter();
        buildContentFilter();
        buildStatusFilter();
        buildDisplayFilter();
        
        _browser.getUI().debugMessage("** init msgTreeFilter: filters built");
        Composite actions = new Composite(_root, SWT.NONE);
        actions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        _ok = new Button(actions, SWT.PUSH);
        _ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { updateFilter(); apply(); }
            public void widgetSelected(SelectionEvent selectionEvent) { updateFilter(); apply(); }
        });
        
        _cancel = new Button(actions, SWT.PUSH);
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { cancel(); }
            public void widgetSelected(SelectionEvent selectionEvent) { cancel(); }
        });
        
        _browser.getTranslationRegistry().register(this);
    }
    
    /** interpret the _filterText and update the gui components to display the correct state */
    private void parseFilter() {
        _browser.getUI().debugMessage("** parsing filters");
        SyndieURI uri = null;
        String txt = _filterText.getText().trim();
        if (txt.length() > 0) {
            try {
                uri = new SyndieURI(_filterText.getText().trim());
                uri = new SyndieURI(uri, SyndieURI.DEFAULT_SEARCH_URI);
            } catch (URISyntaxException use) {
                // invalid
                //use.printStackTrace();
                uri = SyndieURI.DEFAULT_SEARCH_URI;
            }
        } else {
            uri = SyndieURI.DEFAULT_SEARCH_URI;
        }
        // see doc/web/spec.html#uri_search for the attributes of the uri
        
        // now go through the gui components and pick the right value to display, 
        // using the attributes from SyndieURI.DEFAULT_SEARCH_URI to fill in for unspecified
        // values
        parseChannels(uri.getStringArray("scope"), uri);
        parseAuthor(uri.getString("author"));
        parseAge(uri.getLong("age"), uri.getLong("ageLocal"));
        parseTags(uri.getStringArray("taginclude"), uri.getStringArray("tagrequire"), 
                  uri.getStringArray("tagexclude"), uri.getBoolean("tagmessages", false));
        parseContent(uri.getLong("pagemin"), uri.getLong("pagemax"),
                     uri.getLong("attachmin"), uri.getLong("attachmax"),
                     uri.getLong("refmin"), uri.getLong("refmax"),
                     uri.getLong("keymin"), uri.getLong("keymax"));
        parseStatus(uri.getBoolean("encrypted", false), uri.getBoolean("pbe", false), 
                    uri.getBoolean("private", false));
        parseDisplay(uri.getBoolean("threaded", true));
    }
    
    private void parseChannels(String channelHashes[], SyndieURI uri) {
        synchronized (this) {
            _channels = null;
        }
        if ( (channelHashes == null) || (channelHashes.length == 0) || ("all".equals(channelHashes[0])) ) {
            _forumName.setText("all");
            synchronized (this) {
                _channels = null;
            }
        } else {
            StringBuffer buf = new StringBuffer();
            Hash channels[] = new Hash[channelHashes.length];
            int curChan = 0;
            for (int i = 0; i < channelHashes.length; i++) {
                byte h[] = Base64.decode(channelHashes[i]);
                if ( (h != null) && (h.length == Hash.HASH_LENGTH) ) {
                    channels[curChan] = new Hash(h);
                    long id = _client.getChannelId(channels[curChan]);
                    if (id >= 0) {
                        ChannelInfo info = _client.getChannel(id);
                        if ( (info != null) && (info.getName() != null) ) {
                            buf.append(CommandImpl.strip(info.getName(), "[]\r\n", ' '));
                            buf.append(' ');
                        }
                    }
                    buf.append('[').append(channels[curChan].toBase64().substring(0,6)).append(']');
                    if (i + 1 < channelHashes.length)
                        buf.append(", ");
                    curChan++;
                } else {
                    _browser.getUI().errorMessage("invalid channel hash: " + channelHashes[i]);
                    _browser.getUI().errorMessage("uri attributes: " + uri.getAttributes());
                    Hash nchannels[] = new Hash[channels.length-1];
                    System.arraycopy(channels, 0, nchannels, 0, i-1);
                    channels = nchannels;
                    //return;
                }
            }
            synchronized (this) {
                _channels = channels;
            }
            _forumName.setText(buf.toString());
        }
    }
    private void parseAuthor(String author) {
        _authorAny.setSelection(false);
        _authorAuthorized.setSelection(false);
        _authorManager.setSelection(false);
        _authorOwner.setSelection(false);
        
        if ("any".equalsIgnoreCase(author))
            _authorAny.setSelection(true);
        else if ("manager".equalsIgnoreCase(author))
            _authorManager.setSelection(true);
        else if ("owner".equalsIgnoreCase(author))
            _authorOwner.setSelection(true);
        else // if ("authorized".equalsIgnoreCase(author))
            _authorAuthorized.setSelection(true);
    }
    private void parseAge(Long postDays, Long recvDays) {
        _ageDay.setSelection(false);
        _age2Days.setSelection(false);
        _ageWeek.setSelection(false);
        _age2Weeks.setSelection(false);
        _ageMonth.setSelection(false);
        _age2Months.setSelection(false);
        _ageCustom.setSelection(false);
        _ageCustomText.setText("");
        
        int days = Integer.MAX_VALUE;
        if (recvDays != null)
            days = Math.min(days, recvDays.intValue());
        if (postDays != null)
            days = Math.min(days, postDays.intValue());
        if ( (recvDays == null) && (postDays == null) )
            days = 2;
        
        if (days <= 1)
            _ageDay.setSelection(true);
        else if (days == 2)
            _age2Days.setSelection(true);
        else if (days <= 7)
            _ageWeek.setSelection(true);
        else if (days <= 14)
            _age2Weeks.setSelection(true);
        else if (days <= 31)
            _ageMonth.setSelection(true);
        else if (days <= 62)
            _age2Months.setSelection(true);
        else {
            _ageCustom.setSelection(true);
            _ageCustomText.setText(days+"");
        }
    }
    private void parseTags(String inc[], String req[], String excl[], boolean msgs) {
        _tagsRequireText.setText(parseTags(req));
        _tagsIncludeText.setText(parseTags(inc));
        _tagsExcludeText.setText(parseTags(excl));
        _tagsApplyToMessages.setSelection(msgs);
    }
    private static final String parseTags(String tags[]) {
        if ( (tags == null) || (tags.length == 0) ) return "";
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < tags.length; i++) {
            String str = tags[i].trim();
            if (str.length() > 0)
                buf.append(str).append(' ');
        }
        return buf.toString().trim();
    }
    private void parseContent(Long pageMin, Long pageMax, Long attachMin, Long attachMax,
                              Long refMin, Long refMax, Long keyMin, Long keyMax) {
        if ( (pageMin != null) && (pageMin.intValue() > 0) )
            _contentPageYes.setSelection(true);
        else if ( (pageMax != null) && (pageMax.intValue() <= 0) )
            _contentPageNo.setSelection(true);
        else
            _contentPageIgnore.setSelection(true);
        
        if ( (attachMin != null) && (attachMin.intValue() > 0) )
            _contentAttachYes.setSelection(true);
        else if ( (attachMax != null) && (attachMax.intValue() <= 0) )
            _contentAttachNo.setSelection(true);
        else
            _contentAttachIgnore.setSelection(true);
        
        if ( (refMin != null) && (refMin.intValue() > 0) )
            _contentRefYes.setSelection(true);
        else if ( (refMax != null) && (refMax.intValue() <= 0) )
            _contentRefNo.setSelection(true);
        else
            _contentRefIgnore.setSelection(true);
        
        if ( (keyMin != null) && (keyMin.intValue() > 0) )
            _contentKeyYes.setSelection(true);
        else if ( (keyMax != null) && (keyMax.intValue() <= 0) )
            _contentKeyNo.setSelection(true);
        else
            _contentKeyIgnore.setSelection(true);
    }
    private void parseStatus(boolean encrypted, boolean pbe, boolean priv) {
        _statusDecrypted.setSelection(!encrypted);
        _statusPBE.setSelection(pbe);
        _statusPrivate.setSelection(priv);
    }
    private void parseDisplay(boolean threaded) {
        _displayThreaded.setSelection(threaded);
    }
    
    /** update the filter text to reflect the gui state */
    private void updateFilter() {
        // get all the attributes together and call buildSearchURI(...)
        
        String scopes[] = null;
        synchronized (this) {
            if (_channels == null) {
                scopes = new String[] { "all" };
            } else {
                scopes = new String[_channels.length];
                for (int i = 0; i < _channels.length; i++)
                    scopes[i] = _channels[i].toBase64();
            }
        }
        String author = null;
        if (_authorAny.getSelection()) author = "any";
        else if (_authorManager.getSelection()) author = "manager";
        else if (_authorOwner.getSelection()) author = "owner";
        else author = "authorized";
        Long postDays = null;
        if (_ageDay.getSelection()) postDays = new Long(1);
        else if (_age2Days.getSelection()) postDays = new Long(2);
        else if (_ageWeek.getSelection()) postDays = new Long(7);
        else if (_age2Weeks.getSelection()) postDays = new Long(14);
        else if (_ageMonth.getSelection()) postDays = new Long(31);
        else if (_age2Months.getSelection()) postDays = new Long(62);
        else if (_ageCustom.getSelection()) postDays = getAge(_ageCustomText.getText());
        Long recvDays = null;
        String inc[] = Constants.split(" \t\r\n,", _tagsIncludeText.getText(), false);
        String req[] = Constants.split(" \t\r\n,", _tagsRequireText.getText(), false);
        String excl[] = Constants.split(" \t\r\n,", _tagsExcludeText.getText(), false);
        boolean msgs = _tagsApplyToMessages.getSelection();
        Long pageMin = (_contentPageYes.getSelection() ? new Long(1) : null);
        Long pageMax = (_contentPageNo.getSelection() ? new Long(0) : null);
        Long attachMin = (_contentAttachYes.getSelection() ? new Long(1) : null);
        Long attachMax = (_contentAttachNo.getSelection() ? new Long(0) : null);
        Long refMin = (_contentRefYes.getSelection() ? new Long(1) : null);
        Long refMax = (_contentRefNo.getSelection() ? new Long(0) : null);
        Long keyMin = (_contentKeyYes.getSelection() ? new Long(1) : null);
        Long keyMax = (_contentKeyNo.getSelection() ? new Long(0) : null);
        boolean encrypted = !_statusDecrypted.getSelection();
        boolean pbe = _statusPBE.getSelection();
        boolean priv = _statusPrivate.getSelection();
        boolean threaded = _displayThreaded.getSelection();
        
        SyndieURI uri = SyndieURI.createSearch(scopes, author, postDays, recvDays, inc, req, excl, msgs, pageMin,
                                               pageMax, attachMin, attachMax, refMin, refMax, keyMin, keyMax,
                                               encrypted, pbe, priv, threaded);
        _filterText.setText(uri.toString());
    }
    private static final Long getAge(String age) {
        try {
            int split = age.indexOf('d');
            if (split > 0)
                return new Long(age.substring(0, split).trim());
            split = age.indexOf('w');
            if (split > 0)
                return new Long(7 * Long.parseLong(age.substring(0,split).trim()));
            split = age.indexOf('m');
            if (split > 0)
                return new Long(31 * Long.parseLong(age.substring(0,split).trim()));
            return new Long(Long.parseLong(age));
        } catch (NumberFormatException nfe) {
            return null;
        }
    }
    
    private void apply() { 
        _tree.setFilter(_filterText.getText()); 
        _tree.applyFilter();
        _tree.hideFilterEditor();
    }
    private void cancel() { _tree.hideFilterEditor(); }
    
    private void buildScopeFilter() {
        _itemScope = new ExpandItem(_bar, SWT.NONE);
        _scope = new Composite(_bar, SWT.BORDER);
        _scope.setLayout(new GridLayout(3, false));
        _forumLabel = new Label(_scope, SWT.NONE);
        _forumLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _forumName = new Text(_scope, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        _forumName.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _forumSelect = new Button(_scope, SWT.PUSH);
        _forumSelect.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _forumSelect.addSelectionListener(new SelectionListener() {
            public void widgetSelected(SelectionEvent selectionEvent) { pickScope(); }
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickScope(); }
        });
        
        _authorLabel = new Label(_scope, SWT.NONE);
        _authorLabel.setLayoutData(new GridData(GridData.END, GridData.BEGINNING, false, false));
        _authorGroup = new Group(_scope, SWT.NONE);
        _authorGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
        _authorGroup.setLayout(new FillLayout(SWT.VERTICAL));
        _authorAuthorized = new Button(_authorGroup, SWT.RADIO);
        _authorManager = new Button(_authorGroup, SWT.RADIO);
        _authorOwner = new Button(_authorGroup, SWT.RADIO);
        _authorAny = new Button(_authorGroup, SWT.RADIO);
        _authorAuthorized.setSelection(true);
        
        _authorAuthorized.addSelectionListener(_modListener);
        _authorManager.addSelectionListener(_modListener);
        _authorOwner.addSelectionListener(_modListener);
        _authorAny.addSelectionListener(_modListener);
        
        _itemScope.setControl(_scope);
        _itemScope.setHeight(_scope.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
    }
    
    private void buildAgeFilter() {
        _itemAge = new ExpandItem(_bar, SWT.NONE);
        _age = new Group(_bar, SWT.BORDER);
        _age.setLayout(new GridLayout(2, false));
        _ageDay = new Button(_age, SWT.RADIO);
        _ageDay.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 1, 1));
        _age2Days = new Button(_age, SWT.RADIO);
        _age2Days.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 1, 1));
        _ageWeek = new Button(_age, SWT.RADIO);
        _ageWeek.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 1, 1));
        _age2Weeks = new Button(_age, SWT.RADIO);
        _age2Weeks.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 1, 1));
        _ageMonth = new Button(_age, SWT.RADIO);
        _ageMonth.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 1, 1));
        _age2Months = new Button(_age, SWT.RADIO);
        _age2Months.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 1, 1));
        _ageCustom = new Button(_age, SWT.RADIO);
        _ageCustom.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _ageCustomText = new Text(_age, SWT.BORDER | SWT.SINGLE);
        _ageCustomText.setText("yyyy/MM/dd");
        _ageCustomText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _ageDay.setSelection(true);
        
        _ageDay.addSelectionListener(_modListener);
        _age2Days.addSelectionListener(_modListener);
        _ageWeek.addSelectionListener(_modListener);
        _age2Weeks.addSelectionListener(_modListener);
        _ageMonth.addSelectionListener(_modListener);
        _age2Months.addSelectionListener(_modListener);
        _ageCustom.addSelectionListener(_modListener);
        _ageCustomText.addFocusListener(_modListener);
        
        _itemAge.setControl(_age);
        _itemAge.setHeight(_age.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
    }
    private void buildTagsFilter() {
        _itemTags = new ExpandItem(_bar, SWT.NONE);
        
        _tags = new Composite(_bar, SWT.BORDER);
        _tags.setLayout(new GridLayout(2, false));
        _tagsInclude = new Label(_tags, SWT.NONE);
        _tagsInclude.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        _tagsIncludeText = new Text(_tags, SWT.SINGLE | SWT.BORDER);
        _tagsIncludeText.setText("");
        _tagsIncludeText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _tagsRequire = new Label(_tags, SWT.NONE);
        _tagsRequire.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        _tagsRequireText = new Text(_tags, SWT.SINGLE | SWT.BORDER);
        _tagsRequireText.setText("");
        _tagsRequireText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _tagsExclude = new Label(_tags, SWT.NONE);
        _tagsExclude.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        _tagsExcludeText = new Text(_tags, SWT.SINGLE | SWT.BORDER);
        _tagsExcludeText.setText("");
        _tagsExcludeText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _tagsApplyToMessages = new Button(_tags, SWT.CHECK);
        _tagsApplyToMessages.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, true, false, 2, 1));
        
        _tagsExcludeText.addFocusListener(_modListener);
        _tagsIncludeText.addFocusListener(_modListener);
        _tagsRequireText.addFocusListener(_modListener);
        _tagsApplyToMessages.addSelectionListener(_modListener);
        
        _itemTags.setControl(_tags);
        _itemTags.setHeight(_tags.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
    }
    private void buildContentFilter() {
        _itemContent = new ExpandItem(_bar, SWT.NONE);
        
        _content = new Composite(_bar, SWT.NONE);
        _content.setLayout(new FillLayout(SWT.VERTICAL));
        
        _contentPageGroup = new Group(_content, SWT.HORIZONTAL);
        _contentPageGroup.setLayout(new FillLayout(SWT.HORIZONTAL));
        _contentPageYes = new Button(_contentPageGroup, SWT.RADIO);
        _contentPageNo = new Button(_contentPageGroup, SWT.RADIO);
        _contentPageIgnore = new Button(_contentPageGroup, SWT.RADIO);
        _contentPageIgnore.setSelection(true);
        
        _contentAttachGroup = new Group(_content, SWT.HORIZONTAL);
        _contentAttachGroup.setLayout(new FillLayout(SWT.HORIZONTAL));
        _contentAttachYes = new Button(_contentAttachGroup, SWT.RADIO);
        _contentAttachNo = new Button(_contentAttachGroup, SWT.RADIO);
        _contentAttachIgnore = new Button(_contentAttachGroup, SWT.RADIO);
        _contentAttachIgnore.setSelection(true);
        
        _contentRefGroup = new Group(_content, SWT.HORIZONTAL);
        _contentRefGroup.setLayout(new FillLayout(SWT.HORIZONTAL));
        _contentRefYes = new Button(_contentRefGroup, SWT.RADIO);
        _contentRefNo = new Button(_contentRefGroup, SWT.RADIO);
        _contentRefIgnore = new Button(_contentRefGroup, SWT.RADIO);
        _contentRefIgnore.setSelection(true);
        
        _contentKeyGroup = new Group(_content, SWT.HORIZONTAL);
        _contentKeyGroup.setLayout(new FillLayout(SWT.HORIZONTAL));
        _contentKeyYes = new Button(_contentKeyGroup, SWT.RADIO);
        _contentKeyNo = new Button(_contentKeyGroup, SWT.RADIO);
        _contentKeyIgnore = new Button(_contentKeyGroup, SWT.RADIO);
        _contentKeyIgnore.setSelection(true);
        
        _contentAttachIgnore.addSelectionListener(_modListener);
        _contentAttachNo.addSelectionListener(_modListener);
        _contentAttachYes.addSelectionListener(_modListener);
        _contentKeyIgnore.addSelectionListener(_modListener);
        _contentKeyNo.addSelectionListener(_modListener);
        _contentKeyYes.addSelectionListener(_modListener);
        _contentPageIgnore.addSelectionListener(_modListener);
        _contentPageNo.addSelectionListener(_modListener);
        _contentPageYes.addSelectionListener(_modListener);
        _contentRefIgnore.addSelectionListener(_modListener);
        _contentRefNo.addSelectionListener(_modListener);
        _contentRefYes.addSelectionListener(_modListener);
        
        _itemContent.setControl(_content);
        _itemContent.setHeight(_content.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
    }
    private void buildStatusFilter() {
        _itemStatus = new ExpandItem(_bar, SWT.NONE);
        
        _status = new Composite(_bar, SWT.BORDER);
        _status.setLayout(new GridLayout(1, true));
        
        _statusDecrypted = new Button(_status, SWT.CHECK);
        _statusDecrypted.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _statusPBE = new Button(_status, SWT.CHECK);
        _statusPBE.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _statusPrivate = new Button(_status, SWT.CHECK);
        _statusPrivate.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _statusDecrypted.setSelection(true);
        
        _statusDecrypted.addSelectionListener(_modListener);
        _statusPBE.addSelectionListener(_modListener);
        _statusPrivate.addSelectionListener(_modListener);
        
        _itemStatus.setControl(_status);
        _itemStatus.setHeight(_status.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
    }
    private void buildDisplayFilter() {
        _itemDisplay = new ExpandItem(_bar, SWT.NONE);
        
        _display = new Composite(_bar, SWT.BORDER);
        _display.setLayout(new GridLayout(1, true));
        
        _displayThreaded = new Button(_display, SWT.CHECK);
        _displayThreaded.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        /*
        _displaySort = new Group(display, SWT.SHADOW_ETCHED_IN);
        _displaySort.setLayout(...
         */
        
        _displayThreaded.setSelection(true);
        
        _displayThreaded.addSelectionListener(_modListener);
        
        _itemDisplay.setControl(_display);
        _itemDisplay.setHeight(_display.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
    }
    
    private void pickScope() { _refChooser.show(); }

    public void referenceAccepted(SyndieURI uri) {
        Hash scope = null;
        if (uri != null) {
            scope = uri.getScope();
            if ( (scope == null) && (uri.isSearch()) )
                scope = uri.getHash("scope");
        }
        if (scope != null) {
            synchronized (this) {
                _channels = new Hash[1];
                _channels[0] = scope;
            }

            long id = _client.getChannelId(scope);
            if (id >= 0) {
                ChannelInfo info = _client.getChannel(id);
                if ( (info != null) && (info.getName() != null) ) {
                    _forumName.setText(CommandImpl.strip(info.getName(), "[]\r\n", ' '));
                } else {
                    _forumName.setText(scope.toBase64().substring(0,6));
                }
            } else {
                _forumName.setText(scope.toBase64().substring(0,6));
            }
            
            updateFilter();
        }
        _refChooser.hide();
    }

    public void referenceChoiceAborted() { _refChooser.hide(); }
    
    private class FilterModifyListener implements SelectionListener, TraverseListener, FocusListener {
        public void widgetSelected(SelectionEvent selectionEvent) { updateFilter(); }
        public void widgetDefaultSelected(SelectionEvent selectionEvent) { updateFilter(); }
        public void keyTraversed(TraverseEvent traverseEvent) { updateFilter(); }
        public void focusGained(FocusEvent focusEvent) {}
        public void focusLost(FocusEvent focusEvent) { updateFilter(); }
    }
    
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
        _refChooser.dispose();
        //_tree.dispose(); // message tree disposes us
    }
    
    private static final String T_OK = "syndie.gui.messagetreefilter.ok";
    private static final String T_CANCEL = "syndie.gui.messagetreefilter.cancel";
        
    private static final String T_SCOPE = "syndie.gui.messagetreefilter.scope";
    private static final String T_FORUMLABEL = "syndie.gui.messagetreefilter.forumlabel";
    private static final String T_FORUMNAME = "syndie.gui.messagetreefilter.forumname";
    private static final String T_FORUMSELECT = "syndie.gui.messagetreefilter.forumselect";
    private static final String T_AUTHORLABEL = "syndie.gui.messagetreefilter.authorlabel";
    private static final String T_AUTHORAUTHORIZED = "syndie.gui.messagetreefilter.authorauthorized";
    private static final String T_AUTHORMANAGER = "syndie.gui.messagetreefilter.authormanager";
    private static final String T_AUTHOROWNER = "syndie.gui.messagetreefilter.authorowner";
    private static final String T_AUTHORANY = "syndie.gui.messagetreefilter.authorany";
    private static final String T_AGE = "syndie.gui.messagetreefilter.age";
    private static final String T_AGEDAY = "syndie.gui.messagetreefilter.ageday";
    private static final String T_AGE2DAY = "syndie.gui.messagetreefilter.age2day";
    private static final String T_AGEWEEK = "syndie.gui.messagetreefilter.ageweek";
    private static final String T_AGE2WEEK = "syndie.gui.messagetreefilter.age2week";
    private static final String T_AGEMONTH = "syndie.gui.messagetreefilter.agemonth";
    private static final String T_AGE2MONTH = "syndie.gui.messagetreefilter.age2month";
    private static final String T_AGECUSTOM = "syndie.gui.messagetreefilter.agecustom";
    private static final String T_TAGS = "syndie.gui.messagetreefilter.tags";
    private static final String T_TAGSINCLUDE = "syndie.gui.messagetreefilter.tagsinclude";
    private static final String T_TAGSREQUIRE = "syndie.gui.messagetreefilter.tagsrequire";
    private static final String T_TAGSEXCLUDE = "syndie.gui.messagetreefilter.tagsexclude";
    private static final String T_APPLYTOMESSAGES = "syndie.gui.messagetreefilter.applytomessages";
    private static final String T_CONTENT = "syndie.gui.messagetreefilter.content";
    private static final String T_CONTENT_PAGES = "syndie.gui.messagetreefilter.content.pages";
    private static final String T_CONTENT_PAGES_YES = "syndie.gui.messagetreefilter.content.pages.yes";
    private static final String T_CONTENT_PAGES_NO = "syndie.gui.messagetreefilter.content.pages.no";
    private static final String T_CONTENT_PAGES_DONTCARE = "syndie.gui.messagetreefilter.content.pages.dontcare";
    private static final String T_CONTENT_ATTACH = "syndie.gui.messagetreefilter.content.attach";
    private static final String T_CONTENT_ATTACH_YES = "syndie.gui.messagetreefilter.content.attach.yes";
    private static final String T_CONTENT_ATTACH_NO = "syndie.gui.messagetreefilter.content.attach.no";
    private static final String T_CONTENT_ATTACH_DONTCARE = "syndie.gui.messagetreefilter.content.attach.dontcare";
    private static final String T_CONTENT_REFS = "syndie.gui.messagetreefilter.content.refs";
    private static final String T_CONTENT_REFS_YES = "syndie.gui.messagetreefilter.content.refs.yes";
    private static final String T_CONTENT_REFS_NO = "syndie.gui.messagetreefilter.content.refs.no";
    private static final String T_CONTENT_REFS_DONTCARE = "syndie.gui.messagetreefilter.content.refs.dontcare";
    private static final String T_CONTENT_KEYS = "syndie.gui.messagetreefilter.content.keys";
    private static final String T_CONTENT_KEYS_YES = "syndie.gui.messagetreefilter.content.keys.yes";
    private static final String T_CONTENT_KEYS_NO = "syndie.gui.messagetreefilter.content.keys.no";
    private static final String T_CONTENT_KEYS_DONTCARE = "syndie.gui.messagetreefilter.content.keys.dontcare";
    private static final String T_STATUS = "syndie.gui.messagetreefilter.status";
    private static final String T_STATUS_DECRYPTED = "syndie.gui.messagetreefilter.status.decrypted";
    private static final String T_STATUS_PBE = "syndie.gui.messagetreefilter.status.pbe";
    private static final String T_STATUS_PRIV = "syndie.gui.messagetreefilter.status.priv";
    private static final String T_DISPLAY = "syndie.gui.messagetreefilter.display";
    private static final String T_DISPLAY_THREADED = "syndie.gui.messagetreefilter.display.threaded";
        
    public void translate(TranslationRegistry registry) {
        _ok.setText(registry.getText(T_OK, "Apply"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel"));
        
        _itemScope.setText(registry.getText(T_SCOPE, "Filter scope"));
        _forumLabel.setText(registry.getText(T_FORUMLABEL, "Forum: "));
        _forumSelect.setText(registry.getText(T_FORUMSELECT, "Select..."));
        _authorLabel.setText(registry.getText(T_AUTHORLABEL, "Author: "));
        _authorAuthorized.setText(registry.getText(T_AUTHORAUTHORIZED, "any authorized user"));
        _authorManager.setText(registry.getText(T_AUTHORMANAGER, "forum managers only"));
        _authorOwner.setText(registry.getText(T_AUTHOROWNER, "forum owner only"));
        _authorAny.setText(registry.getText(T_AUTHORANY, "anyone, even unauthorized users"));
        _itemAge.setText(registry.getText(T_AGE, "Filter age"));
        _ageDay.setText(registry.getText(T_AGEDAY, "posted today"));
        _age2Days.setText(registry.getText(T_AGE2DAY, "posted in the last 2 days"));
        _ageWeek.setText(registry.getText(T_AGEWEEK, "posted in the last week"));
        _age2Weeks.setText(registry.getText(T_AGE2WEEK, "posted in the last 2 weeks"));
        _ageMonth.setText(registry.getText(T_AGEMONTH, "posted in the last month"));
        _age2Months.setText(registry.getText(T_AGE2MONTH, "posted in the last 2 months"));
        _ageCustom.setText(registry.getText(T_AGECUSTOM, "posted since: "));
        _itemTags.setText(registry.getText(T_TAGS, "Filter tags"));
        _tagsInclude.setText(registry.getText(T_TAGSINCLUDE, "include tags: "));
        _tagsRequire.setText(registry.getText(T_TAGSREQUIRE, "require tags: "));
        _tagsExclude.setText(registry.getText(T_TAGSEXCLUDE, "exclude tags: "));
        _tagsApplyToMessages.setText(registry.getText(T_APPLYTOMESSAGES, "apply to messages individually, not just to threads"));
        _itemContent.setText(registry.getText(T_CONTENT, "Filter content"));
        _contentPageGroup.setText(registry.getText(T_CONTENT_PAGES, "Pages:"));
        _contentPageYes.setText(registry.getText(T_CONTENT_PAGES_YES, "included"));
        _contentPageNo.setText(registry.getText(T_CONTENT_PAGES_NO, "not included"));
        _contentPageIgnore.setText(registry.getText(T_CONTENT_PAGES_DONTCARE, "don't care"));
        _contentAttachGroup.setText(registry.getText(T_CONTENT_ATTACH, "Attachments:"));
        _contentAttachYes.setText(registry.getText(T_CONTENT_ATTACH_YES, "included"));
        _contentAttachNo.setText(registry.getText(T_CONTENT_ATTACH_NO, "not included"));
        _contentAttachIgnore.setText(registry.getText(T_CONTENT_ATTACH_DONTCARE, "don't care"));
        _contentRefGroup.setText(registry.getText(T_CONTENT_REFS, "References:"));
        _contentRefYes.setText(registry.getText(T_CONTENT_REFS_YES, "included"));
        _contentRefNo.setText(registry.getText(T_CONTENT_REFS_NO, "not included"));
        _contentRefIgnore.setText(registry.getText(T_CONTENT_REFS_DONTCARE, "don't care"));
        _contentKeyGroup.setText(registry.getText(T_CONTENT_KEYS, "Forum keys:"));
        _contentKeyYes.setText(registry.getText(T_CONTENT_KEYS_YES, "included"));
        _contentKeyNo.setText(registry.getText(T_CONTENT_KEYS_NO, "not included"));
        _contentKeyIgnore.setText(registry.getText(T_CONTENT_KEYS_DONTCARE, "don't care"));
        _itemStatus.setText(registry.getText(T_STATUS, "Filter status"));
        _statusDecrypted.setText(registry.getText(T_STATUS_DECRYPTED, "already decrypted"));
        _statusPBE.setText(registry.getText(T_STATUS_PBE, "encrypted with a passphrase"));
        _statusPrivate.setText(registry.getText(T_STATUS_PRIV, "encrypted as a private message"));
        _itemDisplay.setText(registry.getText(T_DISPLAY, "Filter display options"));
        _displayThreaded.setText(registry.getText(T_DISPLAY_THREADED, "organize in threads"));
        
        _root.layout(true, true);
        
        _itemContent.setHeight(_content.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
        _itemAge.setHeight(_age.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
        _itemScope.setHeight(_scope.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
        _itemTags.setHeight(_tags.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
        _itemStatus.setHeight(_status.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
        _itemDisplay.setHeight(_display.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
    }
}
