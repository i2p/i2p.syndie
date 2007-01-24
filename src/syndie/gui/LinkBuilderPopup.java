package syndie.gui;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import net.i2p.data.Base64;
import net.i2p.data.DataHelper;
import net.i2p.data.Hash;
import net.i2p.data.PrivateKey;
import net.i2p.data.SessionKey;
import net.i2p.data.SigningPrivateKey;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
import syndie.data.NymKey;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.SyncManager;

/**
 *
 */
class LinkBuilderPopup implements ReferenceChooserTree.AcceptanceListener, MessageTree.MessageTreeListener {
    private BrowserControl _browser;
    private DBClient _client;
    private Shell _parentShell;
    private Shell _shell;
    private LinkBuilderSource _target;
    
    private Label _textLabel;
    private Text _text;
    
    private Group _linkTypeGroup;
    private Button _linkTypeWeb;
    private Text _linkTypeWebText;
    private Button _linkTypeSyndie;
    private Composite _syndieLines;
    private Text _linkTypeSyndieText;
    private Label _syndieForumLabel;
    private Text _syndieForum;
    private Button _syndieForumBrowse;
    private Label _syndieMessageLabel;
    private Text _syndieMessage;
    private Button _syndieMessageBrowse;

    private Composite _syndieMessageDetailGroup;
    private Button _syndieMessageDetailGeneral;

    private Button _syndieMessageDetailPage;
    private Combo _syndieMessageDetailPageNum;
    //private Button _syndieMessageDetailPageView;

    private Button _syndieMessageDetailAttachment;
    private Combo _syndieMessageDetailAttachmentNum;
    //private Button _syndieMessageDetailAttachmentView;

    private Composite _syndieReadKeyLine;
    private Button _syndieReadKey;
    private Combo _syndieReadKeyCombo;
    private Composite _syndiePostKeyLine;
    private Button _syndiePostKey;
    private Combo _syndiePostKeyCombo;
    private Composite _syndieReplyKeyLine;
    private Button _syndieReplyKey;
    private Combo _syndieReplyKeyCombo;
    private Composite _syndieManageKeyLine;
    private Button _syndieManageKey;
    private Combo _syndieManageKeyCombo;
    
    private Button _linkTypePage;
    private Combo _linkTypePageCombo;
    private Button _linkTypeAttachment;
    private Combo _linkTypeAttachmentCombo;
    
    private Button _linkTypeFreenet;
    private Text _linkTypeFreenetText;
    
    private Button _linkTypeI2P;
    private Composite _linkTypeI2PRow;
    private Label _linkTypeI2PNameLabel;
    private Text _linkTypeI2PName;
    private Label _linkTypeI2PDestinationLabel;
    private Text _linkTypeI2PDestination;
    
    private Button _linkTypeEepsite;
    private Composite _linkTypeEepsiteRow;
    private Label _linkTypeEepsiteNameLabel;
    private Text _linkTypeEepsiteName;
    private Label _linkTypeEepsiteDestinationLabel;
    private Text _linkTypeEepsiteDestination;
    
    private Button _linkTypeArchive;
    private Combo _linkTypeArchiveCombo;
    private Button _actionOk;
    private Button _actionCancel;

    private ReferenceChooserPopup _refChooser;
    private MessageChooserPopup _messageChooser;
    
    private SyndieURI _selectedURI;
    private long _forumId;
    private Hash _forum;
    
    /** list of NymKey instances shown in the syndieReadKeyCombo */
    private List _readKeys;
    /** list of NymKey instances shown in the syndiePostKeyCombo */
    private List _postKeys;
    /** list of NymKey instances shown in the syndieReplyKeyCombo */
    private List _replyKeys;
    /** list of NymKey instances shown in the syndieManageKeyCombo */
    private List _manageKeys;
    
    /** archives (SyndieURI) populating the archiveCombo */
    private List _archives;
    
    /** has limitOptions been called */
    private boolean _fieldsLimited;
    
    public LinkBuilderPopup(BrowserControl browser, Shell parent, LinkBuilderSource src) {
        _browser = browser;
        _client = browser.getClient();
        _parentShell = parent;
        _target = src;
        _archives = new ArrayList();
        _fieldsLimited = false;
        initComponents();
    }
    
    public interface LinkBuilderSource {
        public void uriBuilt(SyndieURI uri, String text);
        public int getPageCount();
        public List getAttachmentDescriptions();
    }
    
    private void initComponents() {
        _shell = new Shell(_parentShell, SWT.SHELL_TRIM | SWT.PRIMARY_MODAL);
        _shell.setText("Link to...");
        _shell.setLayout(new GridLayout(1, true));
        _linkTypeGroup = new Group(_shell, SWT.NONE);
        _linkTypeGroup.setLayout(new GridLayout(2, false));
        GridData gd = new GridData(GridData.FILL_BOTH);
        //gd.widthHint = 400;
        gd.grabExcessVerticalSpace = true;
        gd.grabExcessHorizontalSpace = true;
        _linkTypeGroup.setLayoutData(gd);
        
        _textLabel = new Label(_linkTypeGroup, SWT.NONE);
        _textLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        _textLabel.setText("Link text:");
        _text = new Text(_linkTypeGroup, SWT.SINGLE | SWT.BORDER);
        _text.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _text.setText("TEXT");
        
        _linkTypeWeb = new Button(_linkTypeGroup, SWT.RADIO);
        _linkTypeWeb.setText("Website:");
        gd = new GridData();
        _linkTypeWeb.setLayoutData(gd);
        
        _linkTypeWebText = new Text(_linkTypeGroup, SWT.SINGLE | SWT.BORDER);
        _linkTypeWebText.setToolTipText("URL to the site, such as 'http://syndie.i2p.net/manual.html'");
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        _linkTypeWebText.setLayoutData(gd);
        _linkTypeWebText.addTraverseListener(new GroupPickListener(_linkTypeWeb));
        _linkTypeWebText.addSelectionListener(new GroupPickListener(_linkTypeWeb));
        
        _linkTypePage = new Button(_linkTypeGroup, SWT.RADIO);
        _linkTypePage.setText("Page:");
        _linkTypePage.setLayoutData(new GridData());
        _linkTypePageCombo = new Combo(_linkTypeGroup, SWT.DROP_DOWN);
        _linkTypePageCombo.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
        _linkTypePageCombo.addTraverseListener(new GroupPickListener(_linkTypePage));
        _linkTypePageCombo.addSelectionListener(new GroupPickListener(_linkTypePage));
        
        _linkTypeAttachment = new Button(_linkTypeGroup, SWT.RADIO);
        _linkTypeAttachment.setText("Attachment:");
        _linkTypeAttachment.setLayoutData(new GridData());
        _linkTypeAttachmentCombo = new Combo(_linkTypeGroup, SWT.DROP_DOWN);
        _linkTypeAttachmentCombo.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
        _linkTypeAttachmentCombo.addTraverseListener(new GroupPickListener(_linkTypeAttachment));
        _linkTypeAttachmentCombo.addSelectionListener(new GroupPickListener(_linkTypeAttachment));

        if (_target == null) {
            ((GridData)_linkTypePage.getLayoutData()).exclude = true;
            ((GridData)_linkTypePageCombo.getLayoutData()).exclude = true;
            ((GridData)_linkTypeAttachment.getLayoutData()).exclude = true;
            ((GridData)_linkTypeAttachmentCombo.getLayoutData()).exclude = true;
        }
        
        _linkTypeSyndie = new Button(_linkTypeGroup, SWT.RADIO);
        _linkTypeSyndie.setText("Syndie:");
        _linkTypeSyndie.setLayoutData(new GridData(SWT.BEGINNING, SWT.BEGINNING, false, false));
        
        _syndieLines = new Composite(_linkTypeGroup, SWT.NONE);
        _syndieLines.setLayout(new GridLayout(3, false));
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 300;
        _syndieLines.setLayoutData(gd);
        
        _linkTypeSyndieText = new Text(_syndieLines, SWT.SINGLE | SWT.BORDER);
        _linkTypeSyndieText.setToolTipText("Full syndie URL");
        _linkTypeSyndieText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        
        _syndieForumLabel = new Label(_syndieLines, SWT.NONE);
        _syndieForumLabel.setText("Forum: ");
        _syndieForumLabel.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        _syndieForum = new Text(_syndieLines, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        _syndieForum.setText("");
        _syndieForum.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));
        
        _syndieForumBrowse = new Button(_syndieLines, SWT.PUSH);
        _syndieForumBrowse.setText("Browse...");
        _syndieForumBrowse.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickForum(); }
        });
        _syndieForumBrowse.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _syndieMessageLabel = new Label(_syndieLines, SWT.NONE);
        _syndieMessageLabel.setText("Message: ");
        _syndieMessage = new Text(_syndieLines, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        _syndieMessage.setText("");
        _syndieMessage.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _syndieMessageBrowse = new Button(_syndieLines, SWT.PUSH);
        _syndieMessageBrowse.setText("Browse...");
        _syndieMessageBrowse.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickMessage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickMessage(); }
        });
        _syndieMessageBrowse.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _syndieMessageDetailGroup = new Composite(_syndieLines, SWT.NONE);
        _syndieMessageDetailGroup.setLayout(new GridLayout(2, false));
        _syndieMessageDetailGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        
        _syndieMessageDetailGeneral = new Button(_syndieMessageDetailGroup, SWT.RADIO);
        _syndieMessageDetailGeneral.setText("Link to the message as a whole");
        _syndieMessageDetailGeneral.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false, 2, 1));
        _syndieMessageDetailGeneral.addSelectionListener(new UpdateURIListener());
        
        _syndieMessageDetailPage = new Button(_syndieMessageDetailGroup, SWT.RADIO);
        _syndieMessageDetailPage.setText("Page: ");
        _syndieMessageDetailPage.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _syndieMessageDetailPage.addSelectionListener(new UpdateURIListener());
        
        _syndieMessageDetailPageNum = new Combo(_syndieMessageDetailGroup, SWT.DROP_DOWN);
        _syndieMessageDetailPageNum.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _syndieMessageDetailPageNum.addSelectionListener(new GroupPickListener(_syndieMessageDetailPage));
        
        //_syndieMessageDetailPageView = new Button(syndieMessageDetailGroup, SWT.PUSH);
        //_syndieMessageDetailPageView.setText("View...");
        //_syndieMessageDetailPageView.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _syndieMessageDetailAttachment = new Button(_syndieMessageDetailGroup, SWT.RADIO);
        _syndieMessageDetailAttachment.setText("Attachment: ");
        _syndieMessageDetailAttachment.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _syndieMessageDetailAttachment.addSelectionListener(new UpdateURIListener());
        
        _syndieMessageDetailAttachmentNum = new Combo(_syndieMessageDetailGroup, SWT.DROP_DOWN);
        _syndieMessageDetailAttachmentNum.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _syndieMessageDetailAttachmentNum.addSelectionListener(new GroupPickListener(_syndieMessageDetailAttachment));
        
        //_syndieMessageDetailAttachmentView = new Button(syndieMessageDetailGroup, SWT.PUSH);
        //_syndieMessageDetailAttachmentView.setText("View...");
        //_syndieMessageDetailAttachmentView.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _syndieReadKeyLine = new Composite(_syndieLines, SWT.NONE);
        _syndieReadKeyLine.setLayout(new GridLayout(2, false));
        _syndieReadKeyLine.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        _syndieReadKey = new Button(_syndieReadKeyLine, SWT.CHECK);
        _syndieReadKey.setText("include read key");
        _syndieReadKey.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _syndieReadKey.addSelectionListener(new UpdateURIListener());
        _syndieReadKeyCombo = new Combo(_syndieReadKeyLine, SWT.DROP_DOWN | SWT.READ_ONLY);
        _syndieReadKeyCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _syndieReadKeyCombo.addSelectionListener(new GroupPickListener(_syndieReadKey));
        
        _syndiePostKeyLine = new Composite(_syndieLines, SWT.NONE);
        _syndiePostKeyLine.setLayout(new GridLayout(2, false));
        _syndiePostKeyLine.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        _syndiePostKey = new Button(_syndiePostKeyLine, SWT.CHECK);
        _syndiePostKey.setText("include post key");
        _syndiePostKey.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _syndiePostKey.addSelectionListener(new UpdateURIListener());
        _syndiePostKeyCombo = new Combo(_syndiePostKeyLine, SWT.DROP_DOWN | SWT.READ_ONLY);
        _syndiePostKeyCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _syndiePostKeyCombo.addSelectionListener(new GroupPickListener(_syndiePostKey));
        
        _syndieReplyKeyLine = new Composite(_syndieLines, SWT.NONE);
        _syndieReplyKeyLine.setLayout(new GridLayout(2, false));
        _syndieReplyKeyLine.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        _syndieReplyKey = new Button(_syndieReplyKeyLine, SWT.CHECK);
        _syndieReplyKey.setText("include reply key");
        _syndieReplyKey.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _syndieReplyKey.addSelectionListener(new UpdateURIListener());
        _syndieReplyKeyCombo = new Combo(_syndieReplyKeyLine, SWT.DROP_DOWN | SWT.READ_ONLY);
        _syndieReplyKeyCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _syndieReplyKeyCombo.addSelectionListener(new GroupPickListener(_syndieReplyKey));
        
        _syndieManageKeyLine = new Composite(_syndieLines, SWT.NONE);
        _syndieManageKeyLine.setLayout(new GridLayout(2, false));
        _syndieManageKeyLine.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        _syndieManageKey = new Button(_syndieManageKeyLine, SWT.CHECK);
        _syndieManageKey.setText("include manage key");
        _syndieManageKey.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _syndieManageKey.addSelectionListener(new UpdateURIListener());
        _syndieManageKeyCombo = new Combo(_syndieManageKeyLine, SWT.DROP_DOWN | SWT.READ_ONLY);
        _syndieManageKeyCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _syndieManageKeyCombo.addSelectionListener(new GroupPickListener(_syndieManageKey));
        
        _linkTypeI2P = new Button(_linkTypeGroup, SWT.RADIO);
        _linkTypeI2P.setText("I2P:");
        gd = new GridData();
        _linkTypeI2P.setLayoutData(gd);
        
        _linkTypeI2PRow = new Composite(_linkTypeGroup, SWT.NONE);
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        _linkTypeI2PRow.setLayoutData(gd);
        _linkTypeI2PRow.setLayout(new GridLayout(4, false));
        
        _linkTypeI2PNameLabel = new Label(_linkTypeI2PRow, SWT.NONE);
        _linkTypeI2PNameLabel.setText("Name:");
        _linkTypeI2PNameLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _linkTypeI2PName = new Text(_linkTypeI2PRow,  SWT.SINGLE | SWT.BORDER);
        _linkTypeI2PName.setToolTipText("Nickname for the I2P destination");
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        _linkTypeI2PName.setLayoutData(gd);
        _linkTypeI2PName.addTraverseListener(new GroupPickListener(_linkTypeI2P));
        _linkTypeI2PName.addSelectionListener(new GroupPickListener(_linkTypeI2P));
    
        _linkTypeI2PDestinationLabel = new Label(_linkTypeI2PRow, SWT.NONE);
        _linkTypeI2PDestinationLabel.setText("Destination:");
        _linkTypeI2PDestinationLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _linkTypeI2PDestination = new Text(_linkTypeI2PRow, SWT.SINGLE | SWT.BORDER);
        _linkTypeI2PDestination.setToolTipText("Base64 I2P destination");
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        _linkTypeI2PDestination.setLayoutData(gd);
        _linkTypeI2PDestination.addTraverseListener(new GroupPickListener(_linkTypeI2P));
        _linkTypeI2PDestination.addSelectionListener(new GroupPickListener(_linkTypeI2P));
        
        _linkTypeEepsite = new Button(_linkTypeGroup, SWT.RADIO);
        _linkTypeEepsite.setText("I2P eepsite:");
        gd = new GridData();
        _linkTypeEepsite.setLayoutData(gd);
        
        _linkTypeEepsiteRow = new Composite(_linkTypeGroup, SWT.NONE);
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        _linkTypeEepsiteRow.setLayoutData(gd);
        _linkTypeEepsiteRow.setLayout(new GridLayout(4, false));
        
        _linkTypeEepsiteNameLabel = new Label(_linkTypeEepsiteRow, SWT.NONE);
        _linkTypeEepsiteNameLabel.setText("Name:");
        _linkTypeEepsiteNameLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _linkTypeEepsiteName = new Text(_linkTypeEepsiteRow,  SWT.SINGLE | SWT.BORDER);
        _linkTypeEepsiteName.setToolTipText("Nickname for the eepsite");
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        _linkTypeEepsiteName.setLayoutData(gd);
        _linkTypeEepsiteName.addTraverseListener(new GroupPickListener(_linkTypeEepsite));
        _linkTypeEepsiteName.addSelectionListener(new GroupPickListener(_linkTypeEepsite));
        
        _linkTypeEepsiteDestinationLabel = new Label(_linkTypeEepsiteRow, SWT.NONE);
        _linkTypeEepsiteDestinationLabel.setText("Destination:");
        _linkTypeEepsiteDestinationLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _linkTypeEepsiteDestination = new Text(_linkTypeEepsiteRow, SWT.SINGLE | SWT.BORDER);
        _linkTypeEepsiteDestination.setToolTipText("Base64 I2P destination");
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        _linkTypeEepsiteDestination.setLayoutData(gd);
        _linkTypeEepsiteDestination.addTraverseListener(new GroupPickListener(_linkTypeEepsite));
        _linkTypeEepsiteDestination.addSelectionListener(new GroupPickListener(_linkTypeEepsite));
    
        
        _linkTypeFreenet = new Button(_linkTypeGroup, SWT.RADIO);
        _linkTypeFreenet.setText("Freenet:");
        gd = new GridData();
        _linkTypeFreenet.setLayoutData(gd);
        
        _linkTypeFreenetText = new Text(_linkTypeGroup, SWT.SINGLE | SWT.BORDER);
        _linkTypeFreenetText.setToolTipText("Freenet key (USK@key, SSK@key, CHK@key, etc)");
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        _linkTypeFreenetText.setLayoutData(gd);
        _linkTypeFreenetText.addTraverseListener(new GroupPickListener(_linkTypeFreenet));
        _linkTypeFreenetText.addSelectionListener(new GroupPickListener(_linkTypeFreenet));
        
        
        _linkTypeArchive = new Button(_linkTypeGroup, SWT.RADIO);
        _linkTypeArchive.setText("Archive:");
        gd = new GridData();
        _linkTypeArchive.setLayoutData(gd);
        
        _linkTypeArchiveCombo = new Combo(_linkTypeGroup, SWT.DROP_DOWN);
        _linkTypeArchiveCombo.setToolTipText("Full archive URL");
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        _linkTypeArchiveCombo.setLayoutData(gd);
        _linkTypeArchiveCombo.addTraverseListener(new GroupPickListener(_linkTypeArchive));
        _linkTypeArchiveCombo.addSelectionListener(new GroupPickListener(_linkTypeArchive));
        
        Composite actionRow = new Composite(_shell, SWT.NONE);
        actionRow.setLayout(new FillLayout(SWT.HORIZONTAL));
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessVerticalSpace = false;
        gd.grabExcessHorizontalSpace = true;
        actionRow.setLayoutData(gd);
        
        _actionOk = new Button(actionRow, SWT.PUSH);
        _actionOk.setText("OK");
        _actionOk.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { onOk(); }
            public void widgetSelected(SelectionEvent selectionEvent) { onOk(); }
        });
        _actionCancel = new Button(actionRow, SWT.PUSH);
        _actionCancel.setText("Cancel");
        _actionCancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { onCancel(); }
            public void widgetSelected(SelectionEvent selectionEvent) { onCancel(); }
        });
        
        _refChooser = new ReferenceChooserPopup(_shell, _browser, this);
        _messageChooser = new MessageChooserPopup(_shell, _browser, this);
        
        // intercept the shell closing, since that'd cause the shell to be disposed rather than just hidden
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; onCancel(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        //_linkTypeGroup.setSize(_linkTypeGroup.computeSize(400, SWT.DEFAULT));
        //_shell.setSize(_shell.computeSize(400, SWT.DEFAULT));
        _shell.pack();
    }
    
    public void dispose() {}
    
    private void setURI(SyndieURI uri, String linkText) {
        _browser.getUI().debugMessage("setURI(" + uri + ", " + linkText + ")");
        blankSettings();
        
        if (uri != null) {
            if (uri.isArchive()) {
                _browser.getUI().debugMessage("uri is an archive: " + uri);
                _linkTypeArchive.setSelection(true);
                _linkTypeArchiveCombo.setText(uri.toString());
            } else if (uri.isChannel()) {
                _browser.getUI().debugMessage("uri is a channel: " + uri);
                displaySyndieURI(uri);
                //_linkTypeSyndie.setSelection(true);
                //_linkTypeSyndieText.setText(uri.toString());
            } else if (uri.isURL()) {
                String url = uri.getURL();
                _browser.getUI().debugMessage("uri is a url: " + url);
                if (url == null) {
                    _linkTypeWeb.setSelection(true);
                    _linkTypeWebText.setText("");
                } else {
                    if (url.startsWith("SSK@") || (url.startsWith("USK@")) || (url.startsWith("CHK@"))) {
                        _linkTypeFreenet.setSelection(true);
                        _linkTypeFreenetText.setText(url);
                    } else {
                        _linkTypeWeb.setSelection(true);
                        _linkTypeWebText.setText(url);
                    }
                }
            } else {
                _browser.getUI().debugMessage("uri is something else: " + uri);
                displaySyndieURI(uri);
                //_linkTypeSyndie.setSelection(true);
                //_linkTypeSyndieText.setText(uri.toString());
            }
        } else {
            _browser.getUI().debugMessage("uri is null: " + uri);
        }
        
        _text.setText(linkText);
    }

    private class UpdateURIListener implements SelectionListener {
        public void widgetSelected(SelectionEvent selectionEvent) { updateSyndieURI(); }
        public void widgetDefaultSelected(SelectionEvent selectionEvent) { updateSyndieURI(); }
    }
    private void updateSyndieURI() {
        //System.out.println("firing the update uri selection event");
        try {
            SyndieURI uri = new SyndieURI(_linkTypeSyndieText.getText());
            uri = updateURIWithOptions(uri);
            _linkTypeSyndieText.setText(uri.toString());
        } catch (URISyntaxException use) {}
    }
    
    private class GroupPickListener implements SelectionListener, TraverseListener {
        private Button _selectOnAction;
        public GroupPickListener(Button selectOnAction) {
            _selectOnAction = selectOnAction;
        }
        public void widgetDefaultSelected(SelectionEvent selectionEvent) {
            _selectOnAction.setSelection(true);
            updateSyndieURI();
        }
        public void widgetSelected(SelectionEvent selectionEvent) { 
            _selectOnAction.setSelection(true);
            updateSyndieURI();
        }

        public void keyTraversed(TraverseEvent evt) {
            if ( (evt.detail == SWT.TRAVERSE_RETURN) || (evt.detail == SWT.TRAVERSE_TAB_NEXT) ) {
                _selectOnAction.setSelection(true);
                unselectOthers();
                updateSyndieURI();
            }
        }
        
        private void unselectOthers() {
            if (_selectOnAction != _linkTypeWeb)
                _linkTypeWeb.setSelection(false);
            if (_selectOnAction != _linkTypeSyndie)
                _linkTypeSyndie.setSelection(false);
            if (_selectOnAction != _linkTypePage)
                _linkTypePage.setSelection(false);
            if (_selectOnAction != _linkTypeAttachment)
                _linkTypeAttachment.setSelection(false);
            if (_selectOnAction != _linkTypeFreenet)
                _linkTypeFreenet.setSelection(false);
            if (_selectOnAction != _linkTypeArchive)
                _linkTypeArchive.setSelection(false);
            if (_selectOnAction != _linkTypeEepsite)
                _linkTypeEepsite.setSelection(false);
            if (_selectOnAction != _linkTypeI2P)
                _linkTypeI2P.setSelection(false);
        }
    }
    
    private void pickForum() { _refChooser.show(); }
    private void pickMessage() {
        SyndieURI searchURI = SyndieURI.DEFAULT_SEARCH_URI;
        if (_forum != null)
            searchURI = SyndieURI.createSearch(_forum);
        _messageChooser.setFilter(searchURI);
        _messageChooser.show();
    }
    
    private void onOk() {
        if (_linkTypeWeb.getSelection()) {
            _selectedURI = SyndieURI.createURL(_linkTypeWebText.getText());
        } else if (_linkTypeSyndie.getSelection()) {
            try {
                _selectedURI = new SyndieURI(_linkTypeSyndieText.getText());
            } catch (URISyntaxException use) {
                _selectedURI = null;
            }
        } else if (_linkTypeArchive.getSelection()) {
            int selected = _linkTypeArchiveCombo.getSelectionIndex();
            SyndieURI archiveURI = null;
            if (selected >= 0) {
                archiveURI = (SyndieURI)_archives.get(selected);
            } else {
                String txt = _linkTypeArchiveCombo.getText();
                try {
                    archiveURI = new SyndieURI(txt);
                } catch (URISyntaxException use) {
                    if (txt.startsWith("http://"))
                        archiveURI = SyndieURI.createURL(txt);
                    if (txt.startsWith("https://"))
                        archiveURI = SyndieURI.createURL(txt);
                    else if (txt.startsWith("file://"))
                        archiveURI = SyndieURI.createURL(txt);
                    else if (txt.startsWith("SSK@") || txt.startsWith("CHK@") || txt.startsWith("USK@"))
                        archiveURI = SyndieURI.createURL(txt);
                    else
                        archiveURI = null;
                }
                if (archiveURI == null) {
                    MessageBox box = new MessageBox(_shell, SWT.ICON_ERROR | SWT.OK);
                    box.setText("Error");
                    box.setMessage("Location is not valid (use a Syndie URI, an http/https/file URL, or a freenet key)");
                    box.open();
                    return;
                }
            }
            _selectedURI = SyndieURI.createArchive(archiveURI.getURL(), null);
        } else if (_linkTypePage.getSelection()) {
            int idx = _linkTypePageCombo.getSelectionIndex();
            if (idx >= 0)
                _selectedURI = SyndieURI.createRelativePage(idx+1);
        } else if (_linkTypeFreenet.getSelection()) {
            _selectedURI = SyndieURI.createURL(_linkTypeFreenetText.getText().trim());
        } else if (_linkTypeEepsite.getSelection()) {
            StringBuffer buf = new StringBuffer();
            String name = _linkTypeEepsiteName.getText();
            if (!name.startsWith("http://"))
                name = "http://" + name;
            buf.append(name);
            // add a trailing / to http://foo.i2p
            if (name.lastIndexOf('/') <= 6)
                buf.append("/");
            String dest = _linkTypeEepsiteDestination.getText().trim();
            if (dest.length() > 0)
                buf.append("?i2paddresshelper=").append(dest);
            _selectedURI = SyndieURI.createURL(buf.toString());
        } else if (_linkTypeI2P.getSelection()) {
            StringBuffer buf = new StringBuffer();
            String name = _linkTypeI2PName.getText();
            String dest = _linkTypeI2PDestination.getText().trim();
            SyndieURI uri = SyndieURI.createURL(name);
            if (dest.length() > 0) {
                Map attributes = uri.getAttributes();
                attributes.put("destination", dest);
                _selectedURI = new SyndieURI(uri.getType(), attributes);
            } else {
                _selectedURI = uri;
            }
        } else if (_linkTypeAttachment.getSelection()) {
            int idx = _linkTypeAttachmentCombo.getSelectionIndex();
            if (idx >= 0)
                _selectedURI = SyndieURI.createRelativeAttachment(idx+1);
        } else {
            MessageBox box = new MessageBox(_shell, SWT.ICON_ERROR | SWT.OK);
            box.setText("Error");
            box.setMessage("No link selected");
            box.open();
            return;
        }
        
        uriBuilt(_selectedURI);
        _refChooser.hide();
        _shell.setVisible(false);
    }
    
    protected void uriBuilt(SyndieURI uri) {
        if (_target != null)
            _target.uriBuilt(uri, _text.getText());
    }
    
    private void onCancel() {
        _refChooser.hide();
        _shell.setVisible(false);
        _selectedURI = null;
    }
    
    public SyndieURI getURI() { return _selectedURI; }

    public void setShowText(boolean show) {
        if (!show) {
            _text.setVisible(false);
            ((GridData)_text.getLayoutData()).exclude = true;
            _textLabel.setVisible(false);
            ((GridData)_textLabel.getLayoutData()).exclude = true;
        }
    }
    
    /** limit what type of link can be built */
    public void limitOptions(boolean web, boolean page, boolean attach, boolean forum, boolean message, boolean submessage, boolean eepsite, boolean i2p, boolean freenet, boolean archive) { 
        _fieldsLimited = true;
        _shell.setRedraw(false);
        // web
        ((GridData)_linkTypeWeb.getLayoutData()).exclude = !web;
        ((GridData)_linkTypeWebText.getLayoutData()).exclude = !web;
        
        // relative page link
        ((GridData)_linkTypePage.getLayoutData()).exclude = !page;
        ((GridData)_linkTypePageCombo.getLayoutData()).exclude = !page;
        // relative attachment link
        ((GridData)_linkTypeAttachment.getLayoutData()).exclude = !attach;
        ((GridData)_linkTypeAttachmentCombo.getLayoutData()).exclude = !attach;
        
        // syndie links
        ((GridData)_syndieLines.getLayoutData()).exclude = !forum && !message && !submessage;
        ((GridData)_linkTypeSyndie.getLayoutData()).exclude = !forum && !message && !submessage;
        ((GridData)_linkTypeSyndieText.getLayoutData()).exclude = !forum && !message && !submessage;
        // - forum
        ((GridData)_syndieForumLabel.getLayoutData()).exclude = !forum && !message && !submessage;
        ((GridData)_syndieForum.getLayoutData()).exclude = !forum && !message && !submessage;
        ((GridData)_syndieForumBrowse.getLayoutData()).exclude = !forum && !message && !submessage;
        // - message
        ((GridData)_syndieMessageLabel.getLayoutData()).exclude = !message && !submessage;
        ((GridData)_syndieMessage.getLayoutData()).exclude = !message && !submessage;
        ((GridData)_syndieMessageBrowse.getLayoutData()).exclude = !message && !submessage;
        
        ((GridData)_syndieMessageDetailGroup.getLayoutData()).exclude = !submessage;
        // - message as a whole
        ((GridData)_syndieMessageDetailGeneral.getLayoutData()).exclude = !submessage;
        // - specific page
        ((GridData)_syndieMessageDetailPage.getLayoutData()).exclude = !submessage;
        ((GridData)_syndieMessageDetailPageNum.getLayoutData()).exclude = !submessage;
        // - specific attachment
        ((GridData)_syndieMessageDetailAttachment.getLayoutData()).exclude = !submessage;
        ((GridData)_syndieMessageDetailAttachmentNum.getLayoutData()).exclude = !submessage;
        
        // syndie bundled keys
        // - read keys
        ((GridData)_syndieReadKeyLine.getLayoutData()).exclude = !forum && !message && !submessage;
        ((GridData)_syndieReadKey.getLayoutData()).exclude = !forum && !message && !submessage;
        ((GridData)_syndieReadKeyCombo.getLayoutData()).exclude = !forum && !message && !submessage;
        // - post keys
        ((GridData)_syndiePostKeyLine.getLayoutData()).exclude = !forum && !message && !submessage;
        ((GridData)_syndiePostKey.getLayoutData()).exclude = !forum && !message && !submessage;
        ((GridData)_syndiePostKeyCombo.getLayoutData()).exclude = !forum && !message && !submessage;
        // - reply keys
        ((GridData)_syndieReplyKeyLine.getLayoutData()).exclude = !forum && !message && !submessage;
        ((GridData)_syndieReplyKey.getLayoutData()).exclude = !forum && !message && !submessage;
        ((GridData)_syndieReplyKeyCombo.getLayoutData()).exclude = !forum && !message && !submessage;
        // - manage keys
        ((GridData)_syndieManageKeyLine.getLayoutData()).exclude = !forum && !message && !submessage;
        ((GridData)_syndieManageKey.getLayoutData()).exclude = !forum && !message && !submessage;
        ((GridData)_syndieManageKeyCombo.getLayoutData()).exclude = !forum && !message && !submessage;
        
        // freenet links
        ((GridData)_linkTypeFreenet.getLayoutData()).exclude = !freenet;
        ((GridData)_linkTypeFreenetText.getLayoutData()).exclude = !freenet;
        
        // eepsite links
        ((GridData)_linkTypeEepsite.getLayoutData()).exclude = !eepsite;
        ((GridData)_linkTypeEepsiteDestination.getLayoutData()).exclude = !eepsite;
        ((GridData)_linkTypeEepsiteDestinationLabel.getLayoutData()).exclude = !eepsite;
        ((GridData)_linkTypeEepsiteName.getLayoutData()).exclude = !eepsite;
        ((GridData)_linkTypeEepsiteNameLabel.getLayoutData()).exclude = !eepsite;
        ((GridData)_linkTypeEepsiteRow.getLayoutData()).exclude = !eepsite;
        
        // i2p links
        ((GridData)_linkTypeI2P.getLayoutData()).exclude = !i2p;
        ((GridData)_linkTypeI2PDestination.getLayoutData()).exclude = !i2p;
        ((GridData)_linkTypeI2PDestinationLabel.getLayoutData()).exclude = !i2p;
        ((GridData)_linkTypeI2PName.getLayoutData()).exclude = !i2p;
        ((GridData)_linkTypeI2PNameLabel.getLayoutData()).exclude = !i2p;
        ((GridData)_linkTypeI2PRow.getLayoutData()).exclude = !i2p;
        
        // syndie archive links
        ((GridData)_linkTypeArchive.getLayoutData()).exclude = !archive;
        ((GridData)_linkTypeArchiveCombo.getLayoutData()).exclude = !archive;
        
        // now...
        adjustVisibility(_linkTypeWeb);
        adjustVisibility(_linkTypeWebText);
        adjustVisibility(_linkTypeSyndie);
        adjustVisibility(_syndieLines);
        adjustVisibility(_linkTypeSyndieText);
        adjustVisibility(_syndieForumLabel);
        adjustVisibility(_syndieForum);
        adjustVisibility(_syndieForumBrowse);
        adjustVisibility(_syndieMessageLabel);
        adjustVisibility(_syndieMessage);
        adjustVisibility(_syndieMessageBrowse);
        adjustVisibility(_syndieMessageDetailGroup);
        adjustVisibility(_syndieMessageDetailGeneral);
        adjustVisibility(_syndieMessageDetailPage);
        adjustVisibility(_syndieMessageDetailPageNum);
        adjustVisibility(_syndieMessageDetailAttachment);
        adjustVisibility(_syndieMessageDetailAttachmentNum);
        adjustVisibility(_syndieReadKeyLine);
        adjustVisibility(_syndieReadKey);
        adjustVisibility(_syndieReadKeyCombo);
        adjustVisibility(_syndiePostKeyLine);
        adjustVisibility(_syndiePostKey);
        adjustVisibility(_syndiePostKeyCombo);
        adjustVisibility(_syndieReplyKeyLine);
        adjustVisibility(_syndieReplyKey);
        adjustVisibility(_syndieReplyKeyCombo);
        adjustVisibility(_syndieManageKeyLine);
        adjustVisibility(_syndieManageKey);
        adjustVisibility(_syndieManageKeyCombo);
        adjustVisibility(_linkTypePage);
        adjustVisibility(_linkTypePageCombo);
        adjustVisibility(_linkTypeAttachment);
        adjustVisibility(_linkTypeAttachmentCombo);
        adjustVisibility(_linkTypeFreenet);
        adjustVisibility(_linkTypeFreenetText);
        adjustVisibility(_linkTypeEepsite);
        adjustVisibility(_linkTypeEepsiteDestination);
        adjustVisibility(_linkTypeEepsiteDestinationLabel);
        adjustVisibility(_linkTypeEepsiteName);
        adjustVisibility(_linkTypeEepsiteNameLabel);
        adjustVisibility(_linkTypeEepsiteRow);
        adjustVisibility(_linkTypeI2P);
        adjustVisibility(_linkTypeI2PDestination);
        adjustVisibility(_linkTypeI2PDestinationLabel);
        adjustVisibility(_linkTypeI2PName);
        adjustVisibility(_linkTypeI2PNameLabel);
        adjustVisibility(_linkTypeI2PRow);
        adjustVisibility(_linkTypeArchive);
        adjustVisibility(_linkTypeArchiveCombo);
    
        _linkTypeWeb.setSelection(false);
        _linkTypePage.setSelection(false);
        _linkTypeAttachment.setSelection(false);
        _linkTypeSyndie.setSelection(false);
        _linkTypeFreenet.setSelection(false);
        _linkTypeEepsite.setSelection(false);
        _linkTypeI2P.setSelection(false);
        _linkTypeArchive.setSelection(false);
    
        if (web) _linkTypeWeb.setSelection(true);
        else if (page) _linkTypePage.setSelection(true);
        else if (attach) _linkTypeAttachment.setSelection(true);
        else if (forum||message||submessage) _linkTypeSyndie.setSelection(true);
        else if (freenet) _linkTypeFreenet.setSelection(true);
        else if (eepsite) _linkTypeEepsite.setSelection(true);
        else if (i2p) _linkTypeI2P.setSelection(true);
        else if (archive) _linkTypeArchive.setSelection(true);
        
        _shell.setRedraw(true);
        _shell.layout(true, true);
        _shell.setSize(_shell.computeSize(500, SWT.DEFAULT));
    }
    
    private void adjustVisibility(Control ctl) { ctl.setVisible(!((GridData)ctl.getLayoutData()).exclude); }
    
    public void showPopup() { showPopup(null); }
    public void showPopup(SyndieURI uri) { showPopup(uri, _text.getText()); }
    public void showPopup(SyndieURI uri, String text) {
        if (uri != null) {
            setURI(uri, text);
        } else {
            blankSettings();
            if (_target == null) {
                _linkTypePage.setEnabled(false);
                _linkTypePageCombo.setEnabled(false);
                _linkTypeAttachment.setEnabled(false);
                _linkTypeAttachmentCombo.setEnabled(false);
            } else {
                int pages = _target.getPageCount();
                List attachments = _target.getAttachmentDescriptions();
                if (pages < 1) {
                    _linkTypePage.setEnabled(false);
                    _linkTypePageCombo.setEnabled(false);
                    _linkTypePageCombo.removeAll();
                } else {
                    _linkTypePage.setEnabled(true);
                    _linkTypePageCombo.setEnabled(true);
                    _linkTypePageCombo.removeAll();
                    for (int i = 1; i <= pages; i++)
                        _linkTypePageCombo.add(i+"");
                }
                if ( (attachments == null) || (attachments.size() <= 0) ) {
                    _linkTypeAttachment.setEnabled(false);
                    _linkTypeAttachmentCombo.setEnabled(false);
                    _linkTypeAttachmentCombo.removeAll();
                } else {
                    _linkTypeAttachment.setEnabled(true);
                    _linkTypeAttachmentCombo.setEnabled(true);
                    _linkTypeAttachmentCombo.removeAll();
                    for (int i = 0; i < attachments.size(); i++)
                        _linkTypeAttachmentCombo.add((String)attachments.get(i));
                }
            }
        }

        _shell.open();
    }
    
    private void blankSettings() { blankSettings(!_fieldsLimited); }
    private void blankSettings(boolean unselectType) {
        _selectedURI = null;
        _readKeys = null;
        _replyKeys = null;
        _postKeys = null;
        _manageKeys = null;
        
        _text.setText("TEXT");
        if (unselectType) _linkTypeWeb.setSelection(false);
        _linkTypeWebText.setText("");
        if (unselectType) _linkTypeFreenet.setSelection(false);
        _linkTypeFreenetText.setText("");
        if (unselectType) _linkTypePage.setSelection(false);
        _linkTypeAttachment.setSelection(false);
        if (unselectType) _linkTypeSyndie.setSelection(false);
        _linkTypeSyndieText.setText("");
        if (unselectType) _linkTypeArchive.setSelection(false);
        
        _linkTypeArchiveCombo.setRedraw(false);
        _linkTypeArchiveCombo.setText("");
        _linkTypeArchiveCombo.removeAll();
        _archives.clear();
        SyncManager mgr = SyncManager.getInstance(_browser.getClient(), _browser.getUI());
        int archives = mgr.getArchiveCount();
        for (int i = 0; i < archives; i++) {
            SyndieURI uri = mgr.getArchive(i).getArchiveURI();
            String name = mgr.getArchive(i).getName();
            if (name != null)
                _linkTypeArchiveCombo.add(name + ": " + getLocation(uri));
            else
                _linkTypeArchiveCombo.add(getLocation(uri));
            _archives.add(uri);
        }
        _linkTypeArchiveCombo.setRedraw(true);
        
        _syndieForum.setText("");
        _syndieMessageBrowse.setEnabled(false);
        _syndieMessage.setText("");
        _syndieMessage.setEnabled(false);
        _syndieMessageDetailAttachment.setEnabled(false);
        _syndieMessageDetailAttachment.setSelection(false);
        _syndieMessageDetailAttachmentNum.setEnabled(false);
        //_syndieMessageDetailAttachmentView.setEnabled(false);
        _syndieMessageDetailGeneral.setEnabled(false);
        _syndieMessageDetailGeneral.setSelection(true);
        _syndieMessageDetailPage.setEnabled(false);
        _syndieMessageDetailPage.setSelection(false);
        _syndieMessageDetailPageNum.setEnabled(false);
        //_syndieMessageDetailPageView.setEnabled(false);
        
        _syndiePostKey.setEnabled(false);
        _syndiePostKeyCombo.setEnabled(false);
        _syndiePostKeyCombo.removeAll();
        _syndiePostKey.setSelection(false);
        _syndieReadKey.setEnabled(false);
        _syndieReadKeyCombo.setEnabled(false);
        _syndieReadKeyCombo.removeAll();
        _syndieReadKey.setSelection(false);
        _syndieReplyKey.setEnabled(false);
        _syndieReplyKeyCombo.setEnabled(false);
        _syndieReplyKeyCombo.removeAll();
        _syndieReplyKey.setSelection(false);
        _syndieManageKey.setEnabled(false);
        _syndieManageKeyCombo.setEnabled(false);
        _syndieManageKeyCombo.removeAll();
        _syndieManageKey.setSelection(false);
    }
    
    private static final String getLocation(SyndieURI uri) {
        if (uri != null) {
            String url = uri.getURL();
            if (url == null) {
                return uri.toString();
            } else if ( (url.indexOf("SSK@") >= 0) || (url.indexOf("CHK@") >= 0) || (url.indexOf("USK@") >= 0) ) {
                int idx = url.indexOf("CHK@");
                if (idx < 0)
                    idx = url.indexOf("SSK@");
                if (idx < 0)
                    idx = url.indexOf("USK@");
                int end = url.indexOf('?', idx);
                if (end > 0)
                    return url.substring(idx, end);
                else
                    return url.substring(idx);
            } else if ( url.startsWith("/") || 
                        ((url.length() > 2) && (url.charAt(1) == ':') && (url.charAt(2) == '\\')) || 
                        (url.startsWith("file://")) ) {
                return url;
            } else {
                return url;
            }
        } else {
            return "";
        }
    }
    
    /** attach any of the keys specified to the given uri, as well as update the page/attachment */
    private SyndieURI updateURIWithOptions(SyndieURI orig) {
        TreeMap attributes = new TreeMap();
        attributes.putAll(orig.getAttributes());
        if (orig.getReadKey() != null) {
            attributes.put("readKey", SyndieURI.encodeKey(orig.getReadKey().getData()));
        } else if (_syndieReadKey.getSelection() && (_readKeys != null) && (_readKeys.size() > 0) ) {
            NymKey key = (NymKey)_readKeys.get(_syndieReadKeyCombo.getSelectionIndex());
            attributes.put("readKey", SyndieURI.encodeKey(key.getData()));
        } else {
            attributes.remove("readKey");
        }
        
        if (orig.getPostKey() != null) {
            attributes.put("postKey", SyndieURI.encodeKey(orig.getPostKey().getData()));
        } else if (_syndiePostKey.getSelection() && (_postKeys != null) && (_postKeys.size() > 0) ) {
            NymKey key = (NymKey)_postKeys.get(_syndiePostKeyCombo.getSelectionIndex());
            attributes.put("postKey", SyndieURI.encodeKey(key.getData()));
        } else {
            attributes.remove("postKey");
        }
        
        if (orig.getReplyKey() != null) {
            attributes.put("replyKey", SyndieURI.encodeKey(orig.getReplyKey().getData()));
        } else if (_syndieReplyKey.getSelection() && (_replyKeys != null) && (_replyKeys.size() > 0) ) {
            NymKey key = (NymKey)_replyKeys.get(_syndieReplyKeyCombo.getSelectionIndex());
            attributes.put("replyKey", SyndieURI.encodeKey(key.getData()));
        } else {
            attributes.remove("replyKey");
        }
        
        if (orig.getManageKey() != null) {
            attributes.put("manageKey", SyndieURI.encodeKey(orig.getManageKey().getData()));
        } else if (_syndieManageKey.getSelection() && (_manageKeys != null) && (_manageKeys.size() > 0) ) {
            NymKey key = (NymKey)_manageKeys.get(_syndieManageKeyCombo.getSelectionIndex());
            attributes.put("manageKey", SyndieURI.encodeKey(key.getData()));
        } else {
            attributes.remove("manageKey");
        }
        
        if (_syndieMessageDetailAttachment.getSelection() && (_syndieMessageDetailAttachmentNum.getItemCount() > 0) )
            attributes.put("attachment", new Long(_syndieMessageDetailAttachmentNum.getSelectionIndex()));
        else
            attributes.remove("attachment");
        
        if (_syndieMessageDetailPage.getSelection() && (_syndieMessageDetailPageNum.getItemCount() > 0) )
            attributes.put("page", new Long(_syndieMessageDetailPageNum.getSelectionIndex()));
        else
            attributes.remove("page");
        
        SyndieURI rv = new SyndieURI(orig.getType(), attributes);
        _browser.getUI().debugMessage("rewritten uri attributes: " + attributes);
        return rv;
    }

    public void referenceAccepted(SyndieURI uri) {
        displaySyndieURI(uri);
    }
    private void displaySyndieURI(SyndieURI uri) {
        _browser.getUI().debugMessage("displaying syndieURI: " + uri);
        _linkTypeSyndie.setSelection(true);
        uri = updateURIWithOptions(uri);
        _browser.getUI().debugMessage("displaying syndieURI, updated uri is: " + uri);
        _linkTypeSyndieText.setText(uri.toString());
        _forumId = _client.getChannelId(uri.getScope());
        _forum = null;
        MessageInfo msg = null;
        if (_forumId >= 0) {
            ChannelInfo chan = null;
            if (uri.getMessageId() != null) {
                msg = _client.getMessage(_forumId, uri.getMessageId());
                if (msg != null) {
                    _forumId = msg.getTargetChannelId();
                    _forum = msg.getTargetChannel();
                    chan = _client.getChannel(_forumId);
                }
            }
            if (chan == null)
                chan = _client.getChannel(_forumId);
            if (chan != null) {
                _forum = chan.getChannelHash();
                _syndieForum.setText(chan.getName());
                _syndieMessageBrowse.setEnabled(true);
                
                _syndiePostKey.setEnabled(false);
                _syndiePostKeyCombo.setEnabled(false);
                _syndieReadKey.setEnabled(false);
                _syndieReadKeyCombo.setEnabled(false);
                _syndieReplyKey.setEnabled(false);
                _syndieReplyKeyCombo.setEnabled(false);
                _syndieManageKey.setEnabled(false);
                _syndieManageKeyCombo.setEnabled(false);
                
                _syndiePostKey.setSelection(false);
                _syndieReadKey.setSelection(false);
                _syndieReplyKey.setSelection(false);
                _syndieManageKey.setSelection(false);
                
                _readKeys = new ArrayList();
                _postKeys = new ArrayList();
                _manageKeys = new ArrayList();
                _replyKeys = new ArrayList();
                
                _syndiePostKeyCombo.removeAll();
                _syndieReadKeyCombo.removeAll();
                _syndieReplyKeyCombo.removeAll();
                _syndieManageKeyCombo.removeAll();
                
                List nymKeys = _client.getNymKeys(uri.getScope(), null);
                for (int i = 0; i < nymKeys.size(); i++) {
                    NymKey key = (NymKey)nymKeys.get(i);
                    if (Constants.KEY_FUNCTION_POST.equalsIgnoreCase(key.getFunction())) {
                        _syndiePostKey.setEnabled(true);
                        _syndiePostKeyCombo.setEnabled(true);
                        _postKeys.add(key);
                        _syndiePostKeyCombo.add(key.getType() + " - " + key.getFunction());
                        SigningPrivateKey priv = uri.getPostKey();
                        if ( (_syndiePostKeyCombo.getItemCount() == 1) || ( (priv != null) && (DataHelper.eq(priv.getData(), key.getData())) ) )
                            _syndiePostKeyCombo.select(_syndiePostKeyCombo.getItemCount()-1);
                    } else if (Constants.KEY_FUNCTION_MANAGE.equalsIgnoreCase(key.getFunction())) {
                        _syndieManageKey.setEnabled(true);
                        _syndieManageKeyCombo.setEnabled(true);
                        _manageKeys.add(key);
                        _syndieManageKeyCombo.add(key.getType() + " - " + key.getFunction());
                        SigningPrivateKey priv = uri.getManageKey();
                        if ( (_syndieManageKeyCombo.getItemCount() == 1) || ( (priv != null) && (DataHelper.eq(priv.getData(), key.getData())) ) )
                            _syndieManageKeyCombo.select(_syndieManageKeyCombo.getItemCount()-1);
                    } else if (Constants.KEY_FUNCTION_READ.equalsIgnoreCase(key.getFunction())) {
                        _syndieReadKey.setEnabled(true);
                        _syndieReadKeyCombo.setEnabled(true);
                        _readKeys.add(key);
                        _syndieReadKeyCombo.add(key.getType() + " - " + key.getFunction());
                        SessionKey priv = uri.getReadKey();
                        if ( (_syndieReadKeyCombo.getItemCount() == 1) || ( (priv != null) && (DataHelper.eq(priv.getData(), key.getData())) ) )
                            _syndieReadKeyCombo.select(_syndieReadKeyCombo.getItemCount()-1);
                    } else if (Constants.KEY_FUNCTION_REPLY.equalsIgnoreCase(key.getFunction())) {
                        _syndieReplyKey.setEnabled(true);
                        _syndieReplyKeyCombo.setEnabled(true);
                        _replyKeys.add(key);
                        _syndieReplyKeyCombo.add(key.getType() + " - " + key.getFunction());
                        PrivateKey priv = uri.getReplyKey();
                        if ( (_syndieReplyKeyCombo.getItemCount() == 1) || ( (priv != null) && (DataHelper.eq(priv.getData(), key.getData())) ) )
                            _syndieReplyKeyCombo.select(_syndieReplyKeyCombo.getItemCount()-1);
                    }
                }
                

                if (uri.getReadKey() != null) {
                    _syndieReadKey.setSelection(true);
                    if (_readKeys == null)
                        _readKeys = new ArrayList();
                    int foundIndex = -1;
                    for (int i = 0; i < _readKeys.size(); i++) {
                        NymKey key = (NymKey)_readKeys.get(i);
                        if (DataHelper.eq(key.getData(), uri.getReadKey().getData())) {
                            foundIndex = i;
                            break;
                        }
                    }
                    if (foundIndex >= 0) {
                        _syndieReadKeyCombo.select(foundIndex);
                    } else {
                        NymKey nk = new NymKey(Constants.KEY_TYPE_AES256, uri.getReadKey().getData(), true, Constants.KEY_FUNCTION_READ, _client.getLoggedInNymId(), uri.getScope());
                        _readKeys.add(nk);
                        _syndieReadKeyCombo.add(Constants.KEY_TYPE_AES256 + " - " + Constants.KEY_FUNCTION_READ);
                    }
                    _syndieReadKey.setEnabled(true);
                    _syndieReadKeyCombo.setEnabled(true);
                }
                
                if (uri.getPostKey() != null) {
                    _syndiePostKey.setSelection(true);
                    if (_postKeys == null)
                        _postKeys = new ArrayList();
                    int foundIndex = -1;
                    for (int i = 0; i < _postKeys.size(); i++) {
                        NymKey key = (NymKey)_postKeys.get(i);
                        if (DataHelper.eq(key.getData(), uri.getPostKey().getData())) {
                            foundIndex = i;
                            break;
                        }
                    }
                    if (foundIndex >= 0) {
                        _syndiePostKeyCombo.select(foundIndex);
                    } else {
                        NymKey nk = new NymKey(Constants.KEY_TYPE_DSA, uri.getPostKey().getData(), true, Constants.KEY_FUNCTION_POST, _client.getLoggedInNymId(), uri.getScope());
                        _postKeys.add(nk);
                        _syndiePostKeyCombo.add(Constants.KEY_TYPE_DSA + " - " + Constants.KEY_FUNCTION_POST);
                    }
                    _syndiePostKey.setEnabled(true);
                    _syndiePostKeyCombo.setEnabled(true);
                }
                
                if (uri.getReplyKey() != null) {
                    _syndieReplyKey.setSelection(true);
                    if (_replyKeys == null)
                        _replyKeys = new ArrayList();
                    int foundIndex = -1;
                    for (int i = 0; i < _replyKeys.size(); i++) {
                        NymKey key = (NymKey)_replyKeys.get(i);
                        if (DataHelper.eq(key.getData(), uri.getReplyKey().getData())) {
                            foundIndex = i;
                            break;
                        }
                    }
                    if (foundIndex >= 0) {
                        _syndieReplyKeyCombo.select(foundIndex);
                    } else {
                        NymKey nk = new NymKey(Constants.KEY_TYPE_ELGAMAL2048, uri.getReplyKey().getData(), true, Constants.KEY_FUNCTION_REPLY, _client.getLoggedInNymId(), uri.getScope());
                        _replyKeys.add(nk);
                        _syndieReplyKeyCombo.add(Constants.KEY_TYPE_ELGAMAL2048 + " - " + Constants.KEY_FUNCTION_REPLY);
                    }
                    _syndieReplyKey.setEnabled(true);
                    _syndieReplyKeyCombo.setEnabled(true);
                }
                
                if (uri.getManageKey() != null) {
                    _syndieManageKey.setSelection(true);
                    if (_manageKeys == null)
                        _manageKeys = new ArrayList();
                    int foundIndex = -1;
                    for (int i = 0; i < _manageKeys.size(); i++) {
                        NymKey key = (NymKey)_manageKeys.get(i);
                        if (DataHelper.eq(key.getData(), uri.getManageKey().getData())) {
                            foundIndex = i;
                            break;
                        }
                    }
                    if (foundIndex >= 0) {
                        _syndieManageKeyCombo.select(foundIndex);
                    } else {
                        NymKey nk = new NymKey(Constants.KEY_TYPE_DSA, uri.getManageKey().getData(), true, Constants.KEY_FUNCTION_MANAGE, _client.getLoggedInNymId(), uri.getScope());
                        _manageKeys.add(nk);
                        _syndieManageKeyCombo.add(Constants.KEY_TYPE_DSA + " - " + Constants.KEY_FUNCTION_MANAGE);
                    }
                    _syndieManageKey.setEnabled(true);
                    _syndieManageKeyCombo.setEnabled(true);
                }
            } else {
                _syndieForum.setText(uri.getScope().toBase64().substring(0,6));
                _syndieMessageBrowse.setEnabled(false);
                _syndieMessageDetailAttachment.setEnabled(false);
                _syndieMessageDetailAttachmentNum.setEnabled(false);
                //_syndieMessageDetailAttachmentView.setEnabled(false);
                _syndieMessageDetailGeneral.setEnabled(false);
                _syndieMessageDetailGeneral.setSelection(true);
                _syndieMessageDetailPage.setEnabled(false);
                _syndieMessageDetailPageNum.setEnabled(false);
                //_syndieMessageDetailPageView.setEnabled(false);
                _syndiePostKey.setEnabled(false);
                _syndiePostKeyCombo.setEnabled(false);
                _syndieReadKey.setEnabled(false);
                _syndieReadKeyCombo.setEnabled(false);
                _syndieReplyKey.setEnabled(false);
                _syndieReplyKeyCombo.setEnabled(false);
                _syndieManageKey.setEnabled(false);
                _syndieManageKeyCombo.setEnabled(false);
                
                _syndiePostKey.setSelection(false);
                _syndieReadKey.setSelection(false);
                _syndieReplyKey.setSelection(false);
                _syndieManageKey.setSelection(false);
            }
        } else {
            if (uri.isChannel() && (uri.getScope() != null))
                _syndieForum.setText(uri.getScope().toBase64().substring(0,6));
            else
                _syndieForum.setText("");
            _syndieMessageBrowse.setEnabled(false);
            _syndieMessageDetailAttachment.setEnabled(false);
            _syndieMessageDetailAttachmentNum.setEnabled(false);
            //_syndieMessageDetailAttachmentView.setEnabled(false);
            _syndieMessageDetailGeneral.setEnabled(false);
            _syndieMessageDetailGeneral.setSelection(true);
            _syndieMessageDetailPage.setEnabled(false);
            _syndieMessageDetailPageNum.setEnabled(false);
            //_syndieMessageDetailPageView.setEnabled(false);
            _syndiePostKey.setEnabled(false);
            _syndiePostKeyCombo.setEnabled(false);
            _syndieReadKey.setEnabled(false);
            _syndieReadKeyCombo.setEnabled(false);
            _syndieReplyKey.setEnabled(false);
            _syndieReplyKeyCombo.setEnabled(false);
            _syndieManageKey.setEnabled(false);
            _syndieManageKeyCombo.setEnabled(false);
            
            _syndiePostKey.setSelection(false);
            _syndieReadKey.setSelection(false);
            _syndieReplyKey.setSelection(false);
            _syndieManageKey.setSelection(false);
        }
        
        if (msg != null) {
            _syndieMessageBrowse.setEnabled(true);
            
            _syndieMessage.setText(msg.getSubject());
            _syndieMessage.setEnabled(true);
            
            _syndieMessageDetailAttachment.setSelection(false);
            _syndieMessageDetailAttachmentNum.removeAll();
            int attachments = msg.getAttachmentCount();
            for (int i = 0; i < attachments; i++)
                _syndieMessageDetailAttachmentNum.add(""+i);
            if (attachments > 0) {
                _syndieMessageDetailAttachment.setEnabled(true);
                _syndieMessageDetailAttachmentNum.setEnabled(true);
                //_syndieMessageDetailAttachmentView.setEnabled(true);
                Long num = uri.getAttachment();
                if ( (num != null) && (num.longValue() >= 0) && (num.longValue() < attachments) )
                    _syndieMessageDetailAttachmentNum.select(num.intValue());
                else
                    _syndieMessageDetailAttachmentNum.select(0);
            } else {
                _syndieMessageDetailAttachment.setEnabled(false);
                _syndieMessageDetailAttachmentNum.setEnabled(false);
                //_syndieMessageDetailAttachmentView.setEnabled(false);
            }
            
            _syndieMessageDetailGeneral.setEnabled(true);
            _syndieMessageDetailGeneral.setSelection(true);
            
            _syndieMessageDetailPage.setSelection(false);
            _syndieMessageDetailPageNum.removeAll();
            int pages = msg.getPageCount();
            for (int i = 0; i < pages; i++)
                _syndieMessageDetailPageNum.add(""+i);
            if (pages > 0) {
                _syndieMessageDetailPage.setEnabled(true);
                _syndieMessageDetailPageNum.setEnabled(true);
                //_syndieMessageDetailPageView.setEnabled(true);
                _syndieMessageDetailPageNum.select(0);
                Long num = uri.getPage();
                if ( (num != null) && (num.longValue() >= 0) && (num.longValue() < pages) )
                    _syndieMessageDetailPageNum.select(num.intValue());
                else
                    _syndieMessageDetailPageNum.select(0);
            } else {
                _syndieMessageDetailPage.setEnabled(false);
                _syndieMessageDetailPageNum.setEnabled(false);
                //_syndieMessageDetailPageView.setEnabled(false);
            }
        } else {
            _syndieMessageDetailAttachment.setEnabled(false);
            _syndieMessageDetailAttachmentNum.setEnabled(false);
            //_syndieMessageDetailAttachmentView.setEnabled(false);
            _syndieMessageDetailPage.setEnabled(false);
            _syndieMessageDetailPageNum.setEnabled(false);
            //_syndieMessageDetailPageView.setEnabled(false);
            
            _syndieMessage.setEnabled(false);
            _syndieMessage.setText("");
        }
        
        _shell.pack();
    }

    public void referenceChoiceAborted() {}

    public void messageSelected(MessageTree tree, SyndieURI uri, boolean toView) {
        if (toView)
            displaySyndieURI(uri);
    }
    public void filterApplied(MessageTree tree, SyndieURI searchURI) {}
}
