package syndie.gui;

import java.net.URISyntaxException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import syndie.data.SyndieURI;

/**
 *
 */
class LinkBuilderPopup {
    private Shell _parentShell;
    private Shell _shell;
    private Group _linkTypeGroup;
    private Button _linkTypeWeb;
    private Text _linkTypeWebText;
    private Button _linkTypeSyndie;
    private Text _linkTypeSyndieText;
    private Combo _syndieForumCombo;
    private Button _syndieForumBrowse;
    private Combo _syndieMessageCombo;
    private Button _syndieMessageBrowse;
    private Button _syndieMessageView;

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
    
    private SyndieURI _selectedURI;
    
    public LinkBuilderPopup(Shell parent) {
        _parentShell = parent;
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
        _syndieForumCombo = new Combo(syndieForumLine, SWT.SIMPLE);
        _syndieForumCombo.add("My forum [bF9afg]");
        
        _syndieForumBrowse = new Button(syndieForumLine, SWT.PUSH);
        _syndieForumBrowse.setText("Browse...");
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        syndieForumLine.setLayoutData(gd);
        
        Composite syndieMessageLine = new Composite(syndieLines, SWT.NONE);
        syndieMessageLine.setLayout(new RowLayout(SWT.HORIZONTAL));
        l = new Label(syndieMessageLine, SWT.NONE);
        l.setText("Message: ");
        _syndieMessageCombo = new Combo(syndieMessageLine, SWT.SIMPLE);
        _syndieMessageCombo.add("hi how are you? [2006/11/15]");
        
        _syndieMessageBrowse = new Button(syndieMessageLine, SWT.PUSH);
        _syndieMessageBrowse.setText("Browse...");
        
        _syndieMessageView = new Button(syndieMessageLine, SWT.PUSH);
        _syndieMessageView.setText("View...");
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
        _syndieMessageDetailPageNum.add("1");
        _syndieMessageDetailPageNum.add("2");
        _syndieMessageDetailPageNum.add("3");
        
        _syndieMessageDetailPageView = new Button(syndieMessageDetailGroup, SWT.PUSH);
        _syndieMessageDetailPageView.setText("View...");
        
        _syndieMessageDetailAttachment = new Button(syndieMessageDetailGroup, SWT.RADIO);
        _syndieMessageDetailAttachment.setText("Attachment: ");
        
        _syndieMessageDetailAttachmentNum = new Combo(syndieMessageDetailGroup, SWT.SIMPLE);
        _syndieMessageDetailAttachmentNum.add("1");
        _syndieMessageDetailAttachmentNum.add("2");
        _syndieMessageDetailAttachmentNum.add("3");
        
        _syndieMessageDetailAttachmentView = new Button(syndieMessageDetailGroup, SWT.PUSH);
        _syndieMessageDetailAttachmentView.setText("View...");
        
        Composite syndieReadKeyLine = new Composite(syndieLines, SWT.NONE);
        syndieReadKeyLine.setLayout(new RowLayout(SWT.HORIZONTAL));
        _syndieReadKey = new Button(syndieReadKeyLine, SWT.CHECK);
        _syndieReadKey.setText("include read key");
        _syndieReadKeyCombo = new Combo(syndieReadKeyLine, SWT.SIMPLE);
        _syndieReadKeyCombo.add("key xcxcvasfd");
        
        Composite syndiePostKeyLine = new Composite(syndieLines, SWT.NONE);
        syndiePostKeyLine.setLayout(new RowLayout(SWT.HORIZONTAL));
        _syndiePostKey = new Button(syndiePostKeyLine, SWT.CHECK);
        _syndiePostKey.setText("include post key");
        _syndiePostKeyCombo = new Combo(syndiePostKeyLine, SWT.SIMPLE);
        _syndiePostKeyCombo.add("key xcxcvasfd");
        
        Composite syndieReplyKeyLine = new Composite(syndieLines, SWT.NONE);
        syndieReplyKeyLine.setLayout(new RowLayout(SWT.HORIZONTAL));
        _syndieReplyKey = new Button(syndieReplyKeyLine, SWT.CHECK);
        _syndieReplyKey.setText("include reply key");
        _syndieReplyKeyCombo = new Combo(syndieReplyKeyLine, SWT.SIMPLE);
        _syndieReplyKeyCombo.add("key xcxcvasfd");
        
        Composite syndieManageKeyLine = new Composite(syndieLines, SWT.NONE);
        syndieManageKeyLine.setLayout(new RowLayout(SWT.HORIZONTAL));
        _syndieManageKey = new Button(syndieManageKeyLine, SWT.CHECK);
        _syndieManageKey.setText("include manage key");
        _syndieManageKeyCombo = new Combo(syndieManageKeyLine, SWT.SIMPLE);
        _syndieManageKeyCombo.add("key xcxcvasfd");
        
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
        
        //_linkTypeGroup.setSize(_linkTypeGroup.computeSize(400, SWT.DEFAULT));
        //_shell.setSize(_shell.computeSize(400, SWT.DEFAULT));
        _shell.pack();
    }
    
    private void onOk() {
        _shell.setVisible(false);
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
    }
    private void onCancel() {
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
        _shell.setVisible(true);
    }
}
