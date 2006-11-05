package syndie.gui;

import java.net.URISyntaxException;
import java.util.List;
import net.i2p.data.Base64;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
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
import syndie.data.NymKey;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

/**
 *
 */
class LinkBuilderPopup implements ReferenceChooserTree.AcceptanceListener {
    private DBClient _client;
    private Shell _parentShell;
    private Shell _shell;
    private PageEditor _target;
    
    private Group _linkTypeGroup;
    private Button _linkTypeWeb;
    private Text _linkTypeWebText;
    private Button _linkTypeSyndie;
    private Text _linkTypeSyndieText;
    private Label _syndieForum;
    private Button _syndieForumBrowse;
    private Label _syndieMessage;
    private Button _syndieMessageBrowse;

    private Button _syndieMessageDetailPage;
    private Combo _syndieMessageDetailPageNum;
    private Button _syndieMessageDetailPageView;

    private Button _syndieMessageDetailAttachment;
    private Combo _syndieMessageDetailAttachmentNum;
    private Button _syndieMessageDetailAttachmentView;

    private Button _syndieReadKey;
    private Combo _syndieReadKeyCombo;
    private Button _syndiePostKey;
    private Combo _syndiePostKeyCombo;
    private Button _syndieReplyKey;
    private Combo _syndieReplyKeyCombo;
    private Button _syndieManageKey;
    private Combo _syndieManageKeyCombo;
    
    private Button _linkTypeArchive;
    private Text _linkTypeArchiveText;
    private Button _actionOk;
    private Button _actionCancel;

    private ReferenceChooserPopup _refChooser;
    
    private SyndieURI _selectedURI;
    private long _forumId;
    private long _internalMsgId;
    
    public LinkBuilderPopup(DBClient client, Shell parent, PageEditor target) {
        _client = client;
        _parentShell = parent;
        _target = target;
        initComponents();
    }
    private void initComponents() {
        _shell = new Shell(_parentShell, SWT.SHELL_TRIM);
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
        
        _linkTypeSyndie = new Button(_linkTypeGroup, SWT.RADIO);
        _linkTypeSyndie.setText("Syndie:");
        gd = new GridData();
        gd.verticalAlignment = GridData.BEGINNING;
        gd.grabExcessVerticalSpace = false;
        _linkTypeSyndie.setLayoutData(gd);
        
        Composite syndieLines = new Composite(_linkTypeGroup, SWT.NONE);
        syndieLines.setLayout(new GridLayout(1, true));
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        gd.widthHint = 300;
        syndieLines.setLayoutData(gd);
        _linkTypeSyndieText = new Text(syndieLines, SWT.SINGLE | SWT.BORDER);
        _linkTypeSyndieText.setToolTipText("Full syndie URL");
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        _linkTypeSyndieText.setLayoutData(gd);
        
        Composite syndieForumLine = new Composite(syndieLines, SWT.NONE);
        syndieForumLine.setLayout(new RowLayout(SWT.HORIZONTAL));
        Label l = new Label(syndieForumLine, SWT.NONE);
        l.setText("Forum: ");
        _syndieForum = new Label(syndieForumLine, SWT.NONE);
        _syndieForum.setText("");
        _syndieForum.setLayoutData(new RowData(50, SWT.DEFAULT));
        
        _syndieForumBrowse = new Button(syndieForumLine, SWT.PUSH);
        _syndieForumBrowse.setText("Browse...");
        _syndieForumBrowse.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickForum(); }
            public void widgetSelected(SelectionEvent selectionEvent) { pickForum(); }
        });
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        syndieForumLine.setLayoutData(gd);
        
        Composite syndieMessageLine = new Composite(syndieLines, SWT.NONE);
        syndieMessageLine.setLayout(new RowLayout(SWT.HORIZONTAL));
        l = new Label(syndieMessageLine, SWT.NONE);
        l.setText("Message: ");
        _syndieMessage = new Label(syndieMessageLine, SWT.NONE);
        _syndieMessage.setText("");
        _syndieMessage.setLayoutData(new RowData(50, SWT.DEFAULT));
        
        _syndieMessageBrowse = new Button(syndieMessageLine, SWT.PUSH);
        _syndieMessageBrowse.setText("Browse...");
        
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        syndieMessageLine.setLayoutData(gd);
        
        Composite syndieMessageDetailGroup = new Composite(syndieLines, SWT.NONE);
        syndieMessageDetailGroup.setLayout(new GridLayout(3, false));
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        syndieMessageDetailGroup.setLayoutData(gd);
        
        _syndieMessageDetailPage = new Button(syndieMessageDetailGroup, SWT.RADIO);
        _syndieMessageDetailPage.setText("Page: ");
        
        _syndieMessageDetailPageNum = new Combo(syndieMessageDetailGroup, SWT.SIMPLE);
        
        _syndieMessageDetailPageView = new Button(syndieMessageDetailGroup, SWT.PUSH);
        _syndieMessageDetailPageView.setText("View...");
        
        _syndieMessageDetailAttachment = new Button(syndieMessageDetailGroup, SWT.RADIO);
        _syndieMessageDetailAttachment.setText("Attachment: ");
        
        _syndieMessageDetailAttachmentNum = new Combo(syndieMessageDetailGroup, SWT.SIMPLE);
        
        _syndieMessageDetailAttachmentView = new Button(syndieMessageDetailGroup, SWT.PUSH);
        _syndieMessageDetailAttachmentView.setText("View...");
        
        Composite syndieReadKeyLine = new Composite(syndieLines, SWT.NONE);
        syndieReadKeyLine.setLayout(new RowLayout(SWT.HORIZONTAL));
        _syndieReadKey = new Button(syndieReadKeyLine, SWT.CHECK);
        _syndieReadKey.setText("include read key");
        _syndieReadKeyCombo = new Combo(syndieReadKeyLine, SWT.SIMPLE);
        _syndieReadKeyCombo.setLayoutData(new RowData(50, SWT.DEFAULT));
        
        Composite syndiePostKeyLine = new Composite(syndieLines, SWT.NONE);
        syndiePostKeyLine.setLayout(new RowLayout(SWT.HORIZONTAL));
        _syndiePostKey = new Button(syndiePostKeyLine, SWT.CHECK);
        _syndiePostKey.setText("include post key");
        _syndiePostKeyCombo = new Combo(syndiePostKeyLine, SWT.SIMPLE);
        _syndiePostKeyCombo.setLayoutData(new RowData(50, SWT.DEFAULT));
        
        Composite syndieReplyKeyLine = new Composite(syndieLines, SWT.NONE);
        syndieReplyKeyLine.setLayout(new RowLayout(SWT.HORIZONTAL));
        _syndieReplyKey = new Button(syndieReplyKeyLine, SWT.CHECK);
        _syndieReplyKey.setText("include reply key");
        _syndieReplyKeyCombo = new Combo(syndieReplyKeyLine, SWT.SIMPLE);
        _syndieReplyKeyCombo.setLayoutData(new RowData(50, SWT.DEFAULT));
        
        Composite syndieManageKeyLine = new Composite(syndieLines, SWT.NONE);
        syndieManageKeyLine.setLayout(new RowLayout(SWT.HORIZONTAL));
        _syndieManageKey = new Button(syndieManageKeyLine, SWT.CHECK);
        _syndieManageKey.setText("include manage key");
        _syndieManageKeyCombo = new Combo(syndieManageKeyLine, SWT.SIMPLE);
        _syndieManageKeyCombo.setLayoutData(new RowData(50, SWT.DEFAULT));
        
        _linkTypeArchive = new Button(_linkTypeGroup, SWT.RADIO);
        _linkTypeArchive.setText("Archive:");
        gd = new GridData();
        _linkTypeArchive.setLayoutData(gd);
        
        _linkTypeArchiveText = new Text(_linkTypeGroup, SWT.SINGLE | SWT.BORDER);
        _linkTypeArchiveText.setToolTipText("Full archive URL");
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        _linkTypeArchiveText.setLayoutData(gd);
        
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
        
        //_linkTypeGroup.setSize(_linkTypeGroup.computeSize(400, SWT.DEFAULT));
        //_shell.setSize(_shell.computeSize(400, SWT.DEFAULT));
        _shell.pack();
    }
    
    private void pickForum() { _refChooser.show(); }
    
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
        _linkTypeWeb.setSelection(false);
        _linkTypeWebText.setText("");
        _linkTypeSyndie.setSelection(false);
        _linkTypeSyndieText.setText("");
        _linkTypeArchive.setSelection(false);
        _linkTypeArchiveText.setText("");
        
        _syndieForum.setText("");
        _syndieMessageBrowse.setEnabled(false);
        _syndieMessageDetailAttachment.setEnabled(false);
        _syndieMessageDetailAttachmentNum.setEnabled(false);
        _syndieMessageDetailAttachmentView.setEnabled(false);
        _syndieMessageDetailPage.setEnabled(false);
        _syndieMessageDetailPageNum.setEnabled(false);
        _syndieMessageDetailPageView.setEnabled(false);

        _shell.setVisible(true);
    }

    public void referenceAccepted(SyndieURI uri) {
        _linkTypeSyndie.setSelection(true);
        _linkTypeSyndieText.setText(uri.toString());
        _forumId = _client.getChannelId(uri.getScope());
        if (_forumId >= 0) {
            ChannelInfo chan = _client.getChannel(_forumId);
            if (chan != null) {
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
                        _syndiePostKeyCombo.add(Base64.encode(key.getData()));
                        if (_syndiePostKeyCombo.getItemCount() == 1)
                            _syndiePostKeyCombo.select(0);
                    } else if (Constants.KEY_FUNCTION_MANAGE.equalsIgnoreCase(key.getFunction())) {
                        _syndieManageKey.setEnabled(true);
                        _syndieManageKeyCombo.setEnabled(true);
                        _syndieManageKeyCombo.add(Base64.encode(key.getData()));
                        if (_syndieManageKeyCombo.getItemCount() == 1)
                            _syndieManageKeyCombo.select(0);
                    } else if (Constants.KEY_FUNCTION_READ.equalsIgnoreCase(key.getFunction())) {
                        _syndieReadKey.setEnabled(true);
                        _syndieReadKeyCombo.setEnabled(true);
                        _syndieReadKeyCombo.add(Base64.encode(key.getData()));
                        if (_syndieReadKeyCombo.getItemCount() == 1)
                            _syndieReadKeyCombo.select(0);
                    } else if (Constants.KEY_FUNCTION_REPLY.equalsIgnoreCase(key.getFunction())) {
                        _syndieReplyKey.setEnabled(true);
                        _syndieReplyKeyCombo.setEnabled(true);
                        _syndieReplyKeyCombo.add(Base64.encode(key.getData()));
                        if (_syndieReplyKeyCombo.getItemCount() == 1)
                            _syndieReplyKeyCombo.select(0);
                    }
                }
            } else {
                _syndieForum.setText(uri.getScope().toBase64().substring(0,6));
                _syndieMessageBrowse.setEnabled(false);
                _syndieMessageDetailAttachment.setEnabled(false);
                _syndieMessageDetailAttachmentNum.setEnabled(false);
                _syndieMessageDetailAttachmentView.setEnabled(false);
                _syndieMessageDetailPage.setEnabled(false);
                _syndieMessageDetailPageNum.setEnabled(false);
                _syndieMessageDetailPageView.setEnabled(false);
                _syndiePostKey.setEnabled(false);
                _syndiePostKeyCombo.setEnabled(false);
                _syndieReadKey.setEnabled(false);
                _syndieReadKeyCombo.setEnabled(false);
                _syndieReplyKey.setEnabled(false);
                _syndieReplyKeyCombo.setEnabled(false);
                _syndieManageKey.setEnabled(false);
                _syndieManageKeyCombo.setEnabled(false);
            }
        } else {
            _syndieForum.setText(uri.getScope().toBase64().substring(0,6));
            _syndieMessageBrowse.setEnabled(false);
            _syndieMessageDetailAttachment.setEnabled(false);
            _syndieMessageDetailAttachmentNum.setEnabled(false);
            _syndieMessageDetailAttachmentView.setEnabled(false);
            _syndieMessageDetailPage.setEnabled(false);
            _syndieMessageDetailPageNum.setEnabled(false);
            _syndieMessageDetailPageView.setEnabled(false);
            _syndiePostKey.setEnabled(false);
            _syndiePostKeyCombo.setEnabled(false);
            _syndieReadKey.setEnabled(false);
            _syndieReadKeyCombo.setEnabled(false);
            _syndieReplyKey.setEnabled(false);
            _syndieReplyKeyCombo.setEnabled(false);
            _syndieManageKey.setEnabled(false);
            _syndieManageKeyCombo.setEnabled(false);
        }
        _shell.pack();
    }

    public void referenceChoiceAborted() {}
}
