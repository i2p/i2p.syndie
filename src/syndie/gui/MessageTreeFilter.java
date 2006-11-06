package syndie.gui;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ExpandEvent;
import org.eclipse.swt.events.ExpandListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
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
import syndie.db.DBClient;
import syndie.db.NullUI;
import syndie.db.ThreadAccumulator;

/**
 *
 */
public class MessageTreeFilter {
    private DBClient _client;
    private Composite _parent;
    private MessageTree _tree;
    private Set _channels;
    private Group _root;
    private Text _filterText;
    private ExpandBar _bar;
    // filter by scope
    private ExpandItem _itemScope;
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
    private Label _tagsInclude;
    private Text _tagsRequireText;
    private Label _tagsRequire;
    private Text _tagsIncludeText;
    private Label _tagsExclude;
    private Text _tagsExcludeText;
    private Button _tagsApplyToMessages;
    // filter by content
    private ExpandItem _itemContent;
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
    private Button _statusDecrypted;
    private Button _statusPBE;
    private Button _statusPrivate;
    // display options
    private ExpandItem _itemDisplay;
    private Button _displayThreaded;
    
    public MessageTreeFilter(DBClient client, Composite parent, MessageTree tree) {
        _client = client;
        _parent = parent;
        _tree = tree;
        _channels = new HashSet();
        initComponents();
    }

    public Control getControl() { return _root; }
    public void setChannels(Set channels) { 
        _channels.clear(); 
        if (channels != null) 
            _channels.addAll(channels); 
    }
    public void setFilter(String filter) { 
        _filterText.setText(filter); 
        parseFilter(); 
    }
    
    private void initComponents() {
        _root = new Group(_parent, SWT.SHADOW_ETCHED_IN);
        _root.setText("Message filter");
        GridLayout gl = new GridLayout(1, true);
        //gl.marginLeft = 10;
        //gl.marginRight = 0;
        _root.setLayout(gl);
        
        _filterText = new Text(_root, SWT.SINGLE | SWT.BORDER);
        _filterText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _bar = new ExpandBar(_root, SWT.V_SCROLL);
        _bar.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        buildScopeFilter();
        buildAgeFilter();
        buildTagsFilter();
        buildContentFilter();
        buildStatusFilter();
        buildDisplayFilter();
        
        Composite actions = new Composite(_root, SWT.NONE);
        actions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        Button ok = new Button(actions, SWT.PUSH);
        ok.setText("Apply");
        ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { apply(); }
            public void widgetSelected(SelectionEvent selectionEvent) { apply(); }
        });
        
        Button cancel = new Button(actions, SWT.PUSH);
        cancel.setText("Cancel");
        cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { cancel(); }
            public void widgetSelected(SelectionEvent selectionEvent) { cancel(); }
        });
        
        //_bar.addExpandListener(new ExpandListener() {
        //    public void itemCollapsed(ExpandEvent expandEvent) { _parent.pack(); }
        //    public void itemExpanded(ExpandEvent expandEvent) { _parent.pack(); }
        //});
    }
    
    /** interpret the _filterText and update the gui components to display the correct state */
    private void parseFilter() {}
    
    private Set getTags(Text component) {
        HashSet rv = new HashSet();
        String str = component.getText();
        String tags[] = Constants.split(" \t;,", str);
        if (tags != null)
            for (int i = 0; i < tags.length; i++)
                rv.add(tags[i].trim());
        return rv;
    }
    /**
     * actually generate the sorted list of matches
     *
     * @return list of ReferenceNode for the root of each thread, or if it isn't
     *         threaded, simply one per message
     */
    private List calculateMatches() {
        System.out.println("getting threads in channels: " + _channels);
        Set required = getTags(_tagsRequireText);
        Set include = getTags(_tagsIncludeText);
        Set exclude = getTags(_tagsExcludeText);
        ThreadAccumulator acc = new ThreadAccumulator(_client, new NullUI());
        acc.gatherThreads((_channels.size() == 0 ? null : _channels), required, include, exclude);
        List threads = new ArrayList();
        for (int i = 0; i < acc.getThreadCount(); i++)
            threads.add(acc.getRootThread(i));
        return threads;
    }
    
    private void apply() {
        // actually gather the messages
        _tree.setMessages(calculateMatches());
        _tree.setFilter(_filterText.getText());
        hide();
    }
    private void cancel() {
        hide();
    }
    private void hide() { 
        //
    }
    
    private void buildScopeFilter() {
        _itemScope = new ExpandItem(_bar, SWT.NONE);
        _itemScope.setText("Filter scope");
        Composite scope = new Composite(_bar, SWT.BORDER);
        scope.setLayout(new GridLayout(3, false));
        _forumLabel = new Label(scope, SWT.NONE);
        _forumLabel.setText("Forum: ");
        _forumLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _forumName = new Text(scope, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        _forumName.setText("(forum name)");
        _forumName.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _forumSelect = new Button(scope, SWT.PUSH);
        _forumSelect.setText("Select...");
        _forumSelect.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _authorLabel = new Label(scope, SWT.NONE);
        _authorLabel.setText("Author: ");
        _authorLabel.setLayoutData(new GridData(GridData.END, GridData.BEGINNING, false, false));
        _authorGroup = new Group(scope, SWT.NONE);
        _authorGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
        _authorGroup.setLayout(new FillLayout(SWT.VERTICAL));
        _authorAuthorized = new Button(_authorGroup, SWT.RADIO);
        _authorAuthorized.setText("any authorized user");
        _authorManager = new Button(_authorGroup, SWT.RADIO);
        _authorManager.setText("forum managers only");
        _authorOwner = new Button(_authorGroup, SWT.RADIO);
        _authorOwner.setText("forum owner only");
        _authorAny = new Button(_authorGroup, SWT.RADIO);
        _authorAny.setText("anyone, even unauthorized users");
        _authorAuthorized.setSelection(true);
        
        _itemScope.setControl(scope);
        _itemScope.setHeight(scope.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
    }
    
    private void buildAgeFilter() {
        _itemAge = new ExpandItem(_bar, SWT.NONE);
        _itemAge.setText("Filter age");
        Group age = new Group(_bar, SWT.BORDER);
        age.setLayout(new GridLayout(2, false));
        _ageDay = new Button(age, SWT.RADIO);
        _ageDay.setText("posted today");
        _ageDay.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 1, 1));
        _age2Days = new Button(age, SWT.RADIO);
        _age2Days.setText("posted in the last 2 days");
        _age2Days.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 1, 1));
        _ageWeek = new Button(age, SWT.RADIO);
        _ageWeek.setText("posted in the last week");
        _ageWeek.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 1, 1));
        _age2Weeks = new Button(age, SWT.RADIO);
        _age2Weeks.setText("posted in the last 2 weeks");
        _age2Weeks.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 1, 1));
        _ageMonth = new Button(age, SWT.RADIO);
        _ageMonth.setText("posted in the last month");
        _ageMonth.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 1, 1));
        _age2Months = new Button(age, SWT.RADIO);
        _age2Months.setText("posted in the last 2 months");
        _age2Months.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 1, 1));
        _ageCustom = new Button(age, SWT.RADIO);
        _ageCustom.setText("posted since: ");
        _ageCustom.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _ageCustomText = new Text(age, SWT.BORDER | SWT.SINGLE);
        _ageCustomText.setText("yyyy/MM/dd");
        _ageCustomText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _ageDay.setSelection(true);
        _itemAge.setControl(age);
        _itemAge.setHeight(age.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
    }
    private void buildTagsFilter() {
        _itemTags = new ExpandItem(_bar, SWT.NONE);
        _itemTags.setText("Filter tags");
        
        Composite tags = new Composite(_bar, SWT.BORDER);
        tags.setLayout(new GridLayout(2, false));
        _tagsInclude = new Label(tags, SWT.NONE);
        _tagsInclude.setText("include tags: ");
        _tagsInclude.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        _tagsIncludeText = new Text(tags, SWT.SINGLE | SWT.BORDER);
        _tagsIncludeText.setText("");
        _tagsIncludeText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _tagsRequire = new Label(tags, SWT.NONE);
        _tagsRequire.setText("require tags: ");
        _tagsRequire.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        _tagsRequireText = new Text(tags, SWT.SINGLE | SWT.BORDER);
        _tagsRequireText.setText("");
        _tagsRequireText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _tagsExclude = new Label(tags, SWT.NONE);
        _tagsExclude.setText("exclude tags: ");
        _tagsExclude.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        _tagsExcludeText = new Text(tags, SWT.SINGLE | SWT.BORDER);
        _tagsExcludeText.setText("");
        _tagsExcludeText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _tagsApplyToMessages = new Button(tags, SWT.CHECK);
        _tagsApplyToMessages.setText("apply to messages individually, not just to threads");
        _tagsApplyToMessages.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, true, false, 2, 1));
        
        _itemTags.setControl(tags);
        _itemTags.setHeight(tags.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
    }
    private void buildContentFilter() {
        _itemContent = new ExpandItem(_bar, SWT.NONE);
        _itemContent.setText("Filter content");
        
        Composite content = new Composite(_bar, SWT.NONE);
        content.setLayout(new FillLayout(SWT.VERTICAL));
        
        _contentPageGroup = new Group(content, SWT.HORIZONTAL);
        _contentPageGroup.setLayout(new FillLayout(SWT.HORIZONTAL));
        _contentPageGroup.setText("Pages:");
        _contentPageYes = new Button(_contentPageGroup, SWT.RADIO);
        _contentPageYes.setText("included");
        _contentPageNo = new Button(_contentPageGroup, SWT.RADIO);
        _contentPageNo.setText("not included");
        _contentPageIgnore = new Button(_contentPageGroup, SWT.RADIO);
        _contentPageIgnore.setText("don't care");
        _contentPageIgnore.setSelection(true);
        
        _contentAttachGroup = new Group(content, SWT.HORIZONTAL);
        _contentAttachGroup.setLayout(new FillLayout(SWT.HORIZONTAL));
        _contentAttachGroup.setText("Attachments:");
        _contentAttachYes = new Button(_contentAttachGroup, SWT.RADIO);
        _contentAttachYes.setText("included");
        _contentAttachNo = new Button(_contentAttachGroup, SWT.RADIO);
        _contentAttachNo.setText("not included");
        _contentAttachIgnore = new Button(_contentAttachGroup, SWT.RADIO);
        _contentAttachIgnore.setText("don't care");
        _contentAttachIgnore.setSelection(true);
        
        _contentRefGroup = new Group(content, SWT.HORIZONTAL);
        _contentRefGroup.setLayout(new FillLayout(SWT.HORIZONTAL));
        _contentRefGroup.setText("References:");
        _contentRefYes = new Button(_contentRefGroup, SWT.RADIO);
        _contentRefYes.setText("included");
        _contentRefNo = new Button(_contentRefGroup, SWT.RADIO);
        _contentRefNo.setText("not included");
        _contentRefIgnore = new Button(_contentRefGroup, SWT.RADIO);
        _contentRefIgnore.setText("don't care");
        _contentRefIgnore.setSelection(true);
        
        _contentKeyGroup = new Group(content, SWT.HORIZONTAL);
        _contentKeyGroup.setLayout(new FillLayout(SWT.HORIZONTAL));
        _contentKeyGroup.setText("Forum keys:");
        _contentKeyYes = new Button(_contentKeyGroup, SWT.RADIO);
        _contentKeyYes.setText("included");
        _contentKeyNo = new Button(_contentKeyGroup, SWT.RADIO);
        _contentKeyNo.setText("not included");
        _contentKeyIgnore = new Button(_contentKeyGroup, SWT.RADIO);
        _contentKeyIgnore.setText("don't care");
        _contentKeyIgnore.setSelection(true);
        
        _itemContent.setControl(content);
        _itemContent.setHeight(content.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
    }
    private void buildStatusFilter() {
        _itemStatus = new ExpandItem(_bar, SWT.NONE);
        _itemStatus.setText("Filter status");
        
        Composite status = new Composite(_bar, SWT.BORDER);
        status.setLayout(new GridLayout(1, true));
        
        _statusDecrypted = new Button(status, SWT.CHECK);
        _statusDecrypted.setText("already decrypted");
        _statusDecrypted.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _statusPBE = new Button(status, SWT.CHECK);
        _statusPBE.setText("encrypted with a passphrase");
        _statusPBE.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _statusPrivate = new Button(status, SWT.CHECK);
        _statusPrivate.setText("encrypted as a private message");
        _statusPrivate.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _statusDecrypted.setSelection(true);
        _itemStatus.setControl(status);
        _itemStatus.setHeight(status.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
    }
    private void buildDisplayFilter() {
        _itemDisplay = new ExpandItem(_bar, SWT.NONE);
        _itemDisplay.setText("Filter display options");
        
        Composite display = new Composite(_bar, SWT.BORDER);
        display.setLayout(new GridLayout(1, true));
        
        _displayThreaded = new Button(display, SWT.CHECK);
        _displayThreaded.setText("organize in threads");
        _displayThreaded.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        /*
        _displaySort = new Group(display, SWT.SHADOW_ETCHED_IN);
        _displaySort.setLayout(...
         */
        
        _displayThreaded.setSelection(true);
        _itemDisplay.setControl(display);
        _itemDisplay.setHeight(display.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
    }
}
