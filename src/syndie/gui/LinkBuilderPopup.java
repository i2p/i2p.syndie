package syndie.gui;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
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
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
import syndie.data.NymKey;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

/**
 *
 */
class LinkBuilderPopup implements ReferenceChooserTree.AcceptanceListener, MessageTree.MessageTreeListener {
    private BrowserControl _browser;
    private DBClient _client;
    private Shell _parentShell;
    private Shell _shell;
    private PageEditor _target;
    
    private Group _linkTypeGroup;
    private Button _linkTypeWeb;
    private Text _linkTypeWebText;
    private Button _linkTypeSyndie;
    private Text _linkTypeSyndieText;
    private Text _syndieForum;
    private Button _syndieForumBrowse;
    private Text _syndieMessage;
    private Button _syndieMessageBrowse;

    private Button _syndieMessageDetailGeneral;

    private Button _syndieMessageDetailPage;
    private Combo _syndieMessageDetailPageNum;
    //private Button _syndieMessageDetailPageView;

    private Button _syndieMessageDetailAttachment;
    private Combo _syndieMessageDetailAttachmentNum;
    //private Button _syndieMessageDetailAttachmentView;

    private Button _syndieReadKey;
    private Combo _syndieReadKeyCombo;
    private Button _syndiePostKey;
    private Combo _syndiePostKeyCombo;
    private Button _syndieReplyKey;
    private Combo _syndieReplyKeyCombo;
    private Button _syndieManageKey;
    private Combo _syndieManageKeyCombo;
    
    private Button _linkTypePage;
    private Combo _linkTypePageCombo;
    private Button _linkTypeAttachment;
    private Combo _linkTypeAttachmentCombo;
    
    private Button _linkTypeArchive;
    private Text _linkTypeArchiveText;
    private Button _actionOk;
    private Button _actionCancel;

    private ReferenceChooserPopup _refChooser;
    private MessageChooserPopup _messageChooser;
    
    private SyndieURI _selectedURI;
    private long _forumId;
    private Hash _forum;
    private long _internalMsgId;
    
    /** list of NymKey instances shown in the syndieReadKeyCombo */
    private List _readKeys;
    /** list of NymKey instances shown in the syndiePostKeyCombo */
    private List _postKeys;
    /** list of NymKey instances shown in the syndieReplyKeyCombo */
    private List _replyKeys;
    /** list of NymKey instances shown in the syndieManageKeyCombo */
    private List _manageKeys;
    
    public LinkBuilderPopup(BrowserControl browser, Shell parent, PageEditor target) {
        _browser = browser;
        _client = browser.getClient();
        _parentShell = parent;
        _target = target;
        initComponents();
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
        
        _linkTypeSyndie = new Button(_linkTypeGroup, SWT.RADIO);
        _linkTypeSyndie.setText("Syndie:");
        _linkTypeSyndie.setLayoutData(new GridData(SWT.BEGINNING, SWT.BEGINNING, false, false));
        
        Composite syndieLines = new Composite(_linkTypeGroup, SWT.NONE);
        syndieLines.setLayout(new GridLayout(3, false));
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 300;
        syndieLines.setLayoutData(gd);
        
        _linkTypeSyndieText = new Text(syndieLines, SWT.SINGLE | SWT.BORDER);
        _linkTypeSyndieText.setToolTipText("Full syndie URL");
        _linkTypeSyndieText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        
        Label l = new Label(syndieLines, SWT.NONE);
        l.setText("Forum: ");
        l.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        _syndieForum = new Text(syndieLines, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        _syndieForum.setText("");
        _syndieForum.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));
        
        _syndieForumBrowse = new Button(syndieLines, SWT.PUSH);
        _syndieForumBrowse.setText("Browse...");
        _syndieForumBrowse.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickForum(); }
        });
        _syndieForumBrowse.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        l = new Label(syndieLines, SWT.NONE);
        l.setText("Message: ");
        _syndieMessage = new Text(syndieLines, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        _syndieMessage.setText("");
        _syndieMessage.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _syndieMessageBrowse = new Button(syndieLines, SWT.PUSH);
        _syndieMessageBrowse.setText("Browse...");
        _syndieMessageBrowse.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickMessage(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickMessage(); }
        });
        _syndieMessageBrowse.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        Composite syndieMessageDetailGroup = new Composite(syndieLines, SWT.NONE);
        syndieMessageDetailGroup.setLayout(new GridLayout(2, false));
        syndieMessageDetailGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        
        _syndieMessageDetailGeneral = new Button(syndieMessageDetailGroup, SWT.RADIO);
        _syndieMessageDetailGeneral.setText("Link to the message as a whole");
        _syndieMessageDetailGeneral.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false, 2, 1));
        _syndieMessageDetailGeneral.addSelectionListener(new UpdateURIListener());
        
        _syndieMessageDetailPage = new Button(syndieMessageDetailGroup, SWT.RADIO);
        _syndieMessageDetailPage.setText("Page: ");
        _syndieMessageDetailPage.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _syndieMessageDetailPage.addSelectionListener(new UpdateURIListener());
        
        _syndieMessageDetailPageNum = new Combo(syndieMessageDetailGroup, SWT.DROP_DOWN);
        _syndieMessageDetailPageNum.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _syndieMessageDetailPageNum.addSelectionListener(new GroupPickListener(_syndieMessageDetailPage));
        
        //_syndieMessageDetailPageView = new Button(syndieMessageDetailGroup, SWT.PUSH);
        //_syndieMessageDetailPageView.setText("View...");
        //_syndieMessageDetailPageView.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        _syndieMessageDetailAttachment = new Button(syndieMessageDetailGroup, SWT.RADIO);
        _syndieMessageDetailAttachment.setText("Attachment: ");
        _syndieMessageDetailAttachment.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _syndieMessageDetailAttachment.addSelectionListener(new UpdateURIListener());
        
        _syndieMessageDetailAttachmentNum = new Combo(syndieMessageDetailGroup, SWT.DROP_DOWN);
        _syndieMessageDetailAttachmentNum.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _syndieMessageDetailAttachmentNum.addSelectionListener(new GroupPickListener(_syndieMessageDetailAttachment));
        
        //_syndieMessageDetailAttachmentView = new Button(syndieMessageDetailGroup, SWT.PUSH);
        //_syndieMessageDetailAttachmentView.setText("View...");
        //_syndieMessageDetailAttachmentView.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        
        Composite syndieReadKeyLine = new Composite(syndieLines, SWT.NONE);
        syndieReadKeyLine.setLayout(new GridLayout(2, false));
        syndieReadKeyLine.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        _syndieReadKey = new Button(syndieReadKeyLine, SWT.CHECK);
        _syndieReadKey.setText("include read key");
        _syndieReadKey.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _syndieReadKey.addSelectionListener(new UpdateURIListener());
        _syndieReadKeyCombo = new Combo(syndieReadKeyLine, SWT.DROP_DOWN | SWT.READ_ONLY);
        _syndieReadKeyCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _syndieReadKeyCombo.addSelectionListener(new GroupPickListener(_syndieReadKey));
        
        Composite syndiePostKeyLine = new Composite(syndieLines, SWT.NONE);
        syndiePostKeyLine.setLayout(new GridLayout(2, false));
        syndiePostKeyLine.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        _syndiePostKey = new Button(syndiePostKeyLine, SWT.CHECK);
        _syndiePostKey.setText("include post key");
        _syndiePostKey.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _syndiePostKey.addSelectionListener(new UpdateURIListener());
        _syndiePostKeyCombo = new Combo(syndiePostKeyLine, SWT.DROP_DOWN | SWT.READ_ONLY);
        _syndiePostKeyCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _syndiePostKeyCombo.addSelectionListener(new GroupPickListener(_syndiePostKey));
        
        Composite syndieReplyKeyLine = new Composite(syndieLines, SWT.NONE);
        syndieReplyKeyLine.setLayout(new GridLayout(2, false));
        syndieReplyKeyLine.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        _syndieReplyKey = new Button(syndieReplyKeyLine, SWT.CHECK);
        _syndieReplyKey.setText("include reply key");
        _syndieReplyKey.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _syndieReplyKey.addSelectionListener(new UpdateURIListener());
        _syndieReplyKeyCombo = new Combo(syndieReplyKeyLine, SWT.DROP_DOWN | SWT.READ_ONLY);
        _syndieReplyKeyCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _syndieReplyKeyCombo.addSelectionListener(new GroupPickListener(_syndieReplyKey));
        
        Composite syndieManageKeyLine = new Composite(syndieLines, SWT.NONE);
        syndieManageKeyLine.setLayout(new GridLayout(2, false));
        syndieManageKeyLine.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        _syndieManageKey = new Button(syndieManageKeyLine, SWT.CHECK);
        _syndieManageKey.setText("include manage key");
        _syndieManageKey.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        _syndieManageKey.addSelectionListener(new UpdateURIListener());
        _syndieManageKeyCombo = new Combo(syndieManageKeyLine, SWT.DROP_DOWN | SWT.READ_ONLY);
        _syndieManageKeyCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _syndieManageKeyCombo.addSelectionListener(new GroupPickListener(_syndieManageKey));
        
        _linkTypeArchive = new Button(_linkTypeGroup, SWT.RADIO);
        _linkTypeArchive.setText("Archive:");
        gd = new GridData();
        _linkTypeArchive.setLayoutData(gd);
        
        _linkTypeArchiveText = new Text(_linkTypeGroup, SWT.SINGLE | SWT.BORDER);
        _linkTypeArchiveText.setToolTipText("Full archive URL");
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        _linkTypeArchiveText.setLayoutData(gd);
        _linkTypeArchiveText.addTraverseListener(new GroupPickListener(_linkTypeArchive));
        _linkTypeArchiveText.addSelectionListener(new GroupPickListener(_linkTypeArchive));
        
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
        
        _refChooser = new ReferenceChooserPopup(_shell, _client, this);
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

    private class UpdateURIListener implements SelectionListener {
        public void widgetSelected(SelectionEvent selectionEvent) { updateSyndieURI(); }
        public void widgetDefaultSelected(SelectionEvent selectionEvent) { updateSyndieURI(); }
    }
    private void updateSyndieURI() {
        System.out.println("firing the update uri selection event");
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
            if (evt.detail == SWT.TRAVERSE_RETURN) {
                _selectOnAction.setSelection(true);
                updateSyndieURI();
            }
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
            _selectedURI = SyndieURI.createArchive(_linkTypeArchiveText.getText(), null);
        } else if (_linkTypePage.getSelection()) {
            int idx = _linkTypePageCombo.getSelectionIndex();
            if (idx >= 0)
                _selectedURI = SyndieURI.createRelativePage(idx);
        } else if (_linkTypeAttachment.getSelection()) {
            int idx = _linkTypeAttachmentCombo.getSelectionIndex();
            if (idx >= 0)
                _selectedURI = SyndieURI.createRelativeAttachment(idx);
        }
        
        if (_selectedURI != null)
            _target.insertAtCaret("<a href=\"" + _selectedURI.toString() + "\">link target</a>");
        _refChooser.hide();
        _shell.setVisible(false);
    }
    private void onCancel() {
        _refChooser.hide();
        _shell.setVisible(false);
        _selectedURI = null;
    }
    
    public SyndieURI getURI() { return _selectedURI; }

    public void showPopup() {
        _selectedURI = null;
        _readKeys = null;
        _replyKeys = null;
        _postKeys = null;
        _manageKeys = null;
        
        _linkTypeWeb.setSelection(false);
        _linkTypeWebText.setText("");
        _linkTypePage.setSelection(false);
        _linkTypeAttachment.setSelection(false);
        _linkTypeSyndie.setSelection(false);
        _linkTypeSyndieText.setText("");
        _linkTypeArchive.setSelection(false);
        _linkTypeArchiveText.setText("");
        
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
        
        int pages = _target.getPageCount();
        List attachments = _target.getAttachmentDescriptions();
        if (pages <= 1) {
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

        _shell.open();
    }
    
    /** attach any of the keys specified to the given uri, as well as update the page/attachment */
    private SyndieURI updateURIWithOptions(SyndieURI orig) {
        TreeMap attributes = new TreeMap();
        attributes.putAll(orig.getAttributes());
        if (_syndieReadKey.getSelection() && (_readKeys != null) && (_readKeys.size() > 0) ) {
            NymKey key = (NymKey)_readKeys.get(_syndieReadKeyCombo.getSelectionIndex());
            attributes.put("readKey", SyndieURI.encodeKey(key.getData()));
        } else {
            attributes.remove("readKey");
        }
        if (_syndiePostKey.getSelection() && (_postKeys != null) && (_postKeys.size() > 0) ) {
            NymKey key = (NymKey)_postKeys.get(_syndiePostKeyCombo.getSelectionIndex());
            attributes.put("postKey", SyndieURI.encodeKey(key.getData()));
        } else {
            attributes.remove("postKey");
        }
        if (_syndieReplyKey.getSelection() && (_replyKeys != null) && (_replyKeys.size() > 0) ) {
            NymKey key = (NymKey)_replyKeys.get(_syndieReplyKeyCombo.getSelectionIndex());
            attributes.put("replyKey", SyndieURI.encodeKey(key.getData()));
        } else {
            attributes.remove("replyKey");
        }
        if (_syndieManageKey.getSelection() && (_manageKeys != null) && (_manageKeys.size() > 0) ) {
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
        System.out.println("rewritten uri attributes: " + attributes);
        return rv;
    }

    public void referenceAccepted(SyndieURI uri) {
        displaySyndieURI(uri);
    }
    private void displaySyndieURI(SyndieURI uri) {
        _linkTypeSyndie.setSelection(true);
        uri = updateURIWithOptions(uri);
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
