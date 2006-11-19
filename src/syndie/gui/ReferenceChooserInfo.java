package syndie.gui;

import java.text.SimpleDateFormat;
import java.util.Date;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

/**
 * summarize the currently selected reference
 *
 */
public class ReferenceChooserInfo implements ReferenceChooserTree.ChoiceListener {
    private Composite _parent;
    private ReferenceChooserTree _chooser;
    private ReferenceChooserTree.AcceptanceListener _listener;
    private SyndieURI _curReference;
    private Composite _root;
    private StackLayout _rootLayout;
    
    /** shown in the stack layout when rendering a channel reference */
    private Group _rootChannel;
    /** shown in the stack layout when rendering a message reference */
    private Group _rootMessage;
    /** shown in the stack layout when rendering an archive reference */
    private Group _rootArchive;
    /** shown in the stack layout when rendering a URI reference */
    private Group _rootURI;
    
    // fields for displaying channel references
    private Text _channelHash;
    private Text _channelName;
    private Text _channelDesc;
    private Button _channelPubPost;
    private Button _channelPubReply;
    
    // fields for displaying message references
    private Text _messageChannelHash;
    private Text _messageChannelName;
    private Text _messageChannelDesc;
    private Text _messageSubject;
    private Text _messageAuthor;
    private Text _messageDate;
    
    // fields for displaying archives
    private Text _archiveURI;
    
    // fields for displaying uris
    private Text _uri;
    
    /** Creates a new instance of ReferenceChooserInfo */
    public ReferenceChooserInfo(Composite parent, ReferenceChooserTree chooser, ReferenceChooserTree.AcceptanceListener lsnr) {
        _parent = parent;
        _chooser = chooser;
        _listener = lsnr;
        initComponents();
    }
    
    public Control getControl() { return _root; }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _rootLayout = new StackLayout();
        _root.setLayout(_rootLayout);
        
        initChannelComponents();
        initMessageComponents();
        initArchiveComponents();
        initURIComponents();
        
        updateChannel(null, null);
        _rootLayout.topControl = _rootChannel;
        _chooser.setListener(this);
    }
    
    private void initChannelComponents() {
        _rootChannel = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _rootChannel.setText("Forum summary");
        
        Label name = new Label(_rootChannel, SWT.NONE);
        name.setText("Name: ");
        _channelName = new Text(_rootChannel, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        
        Label desc = new Label(_rootChannel, SWT.NONE);
        desc.setText("Description: ");
        ScrolledComposite channelDescScroll = new ScrolledComposite(_rootChannel, SWT.BORDER);
        //_channelDesc = new Text(_rootChannel, SWT.MULTI | SWT.BORDER | SWT.READ_ONLY | SWT.WRAP);
        _channelDesc = new Text(channelDescScroll, SWT.MULTI | SWT.READ_ONLY | SWT.WRAP);
        channelDescScroll.setContent(_channelDesc);
        channelDescScroll.setExpandHorizontal(true);
        channelDescScroll.setExpandVertical(true);
        
        Label hash = new Label(_rootChannel, SWT.NONE);
        hash.setText("Hash: ");
        _channelHash = new Text(_rootChannel, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        
        _channelPubPost = new Button(_rootChannel, SWT.CHECK);
        _channelPubPost.setText("anyone can post");
        _channelPubPost.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { _channelPubPost.setSelection(!_channelPubPost.getSelection()); }
            public void widgetSelected(SelectionEvent evt) { _channelPubPost.setSelection(!_channelPubPost.getSelection()); }
        });

        _channelPubReply = new Button(_rootChannel, SWT.CHECK);
        _channelPubReply.setText("anyone can reply to posts");
        _channelPubReply.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent evt) { _channelPubReply.setSelection(!_channelPubReply.getSelection()); }
            public void widgetSelected(SelectionEvent evt) { _channelPubReply.setSelection(!_channelPubReply.getSelection()); }
        });
        
        Composite actions = addActions(_rootChannel);
        
        GridLayout gl = new GridLayout(2, false);
        _rootChannel.setLayout(gl);
        
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = false;
        gd.grabExcessVerticalSpace = false;
        //gd.horizontalAlignment = GridData.HORIZONTAL_ALIGN_END;
        //gd.verticalAlignment = GridData.VERTICAL_ALIGN_BEGINNING;
        gd.horizontalSpan = 1;
        name.setLayoutData(gd);
        
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = false;
        //gd.horizontalAlignment = GridData.HORIZONTAL_ALIGN_END;
        //gd.verticalAlignment = GridData.VERTICAL_ALIGN_BEGINNING;
        gd.horizontalSpan = 1;
        gd.widthHint = 100;
        _channelName.setLayoutData(gd);
        
        gd = new GridData(GridData.FILL_BOTH);
        //gd.grabExcessHorizontalSpace = true;
        //gd.grabExcessVerticalSpace = false;
        gd.horizontalAlignment = GridData.HORIZONTAL_ALIGN_END;
        //gd.verticalAlignment = GridData.VERTICAL_ALIGN_BEGINNING;
        gd.horizontalSpan = 1;
        desc.setLayoutData(gd);
        
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        //gd.horizontalAlignment = GridData.HORIZONTAL_ALIGN_END;
        //gd.verticalAlignment = GridData.VERTICAL_ALIGN_BEGINNING;
        gd.horizontalSpan = 1;
        gd.widthHint = 100;
        //_channelDesc.setLayoutData(gd);
        channelDescScroll.setLayoutData(gd);
        
        gd = new GridData();
        gd.grabExcessHorizontalSpace = false;
        gd.grabExcessVerticalSpace = false;
        gd.horizontalAlignment = GridData.HORIZONTAL_ALIGN_END;
        gd.verticalAlignment = GridData.VERTICAL_ALIGN_BEGINNING;
        gd.horizontalSpan = 1;
        hash.setLayoutData(gd);
        
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = false;
        //gd.horizontalAlignment = GridData.HORIZONTAL_ALIGN_END;
        //gd.verticalAlignment = GridData.VERTICAL_ALIGN_BEGINNING;
        gd.horizontalSpan = 1;
        gd.widthHint = 100;
        _channelHash.setLayoutData(gd);
        
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = false;
        //gd.horizontalAlignment = GridData.HORIZONTAL_ALIGN_BEGINNING;
        //gd.verticalAlignment = GridData.VERTICAL_ALIGN_BEGINNING;
        gd.horizontalSpan = 2;
        _channelPubPost.setLayoutData(gd);
        
        gd = new GridData(GridData.FILL_BOTH);
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = false;
        //gd.horizontalAlignment = GridData.HORIZONTAL_ALIGN_BEGINNING;
        //gd.verticalAlignment = GridData.VERTICAL_ALIGN_BEGINNING;
        gd.horizontalSpan = 2;
        _channelPubReply.setLayoutData(gd);
        
        if (actions != null) {
            gd = new GridData(GridData.FILL_BOTH);
            gd.grabExcessHorizontalSpace = true;
            gd.grabExcessVerticalSpace = false;
            //gd.horizontalAlignment = GridData.HORIZONTAL_ALIGN_BEGINNING;
            //gd.verticalAlignment = GridData.VERTICAL_ALIGN_BEGINNING;
            gd.horizontalSpan = 2;
            actions.setLayoutData(gd);
        }
    }
    
    private void initMessageComponents() {
        _rootMessage = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _rootMessage.setText("Message summary");
        _rootMessage.setLayout(new GridLayout(2, false));
        
        Label name = new Label(_rootMessage, SWT.NONE);
        //title.setFont(bold)
        name.setText("Name: ");
        name.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_CENTER | GridData.HORIZONTAL_ALIGN_END));
        _messageChannelName = new Text(_rootMessage, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        _messageChannelName.setLayoutData(new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL));
        
        Label desc = new Label(_rootMessage, SWT.NONE);
        //title.setFont(bold)
        desc.setText("Description: ");
        desc.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_CENTER | GridData.HORIZONTAL_ALIGN_END));
        _messageChannelDesc = new Text(_rootMessage, SWT.MULTI | SWT.BORDER | SWT.READ_ONLY);
        _messageChannelDesc.setLayoutData(new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL));
        
        Label title = new Label(_rootMessage, SWT.NONE);
        //title.setFont(bold)
        title.setText("Hash: ");
        title.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_CENTER | GridData.HORIZONTAL_ALIGN_END));
        _messageChannelHash = new Text(_rootMessage, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        _messageChannelHash.setLayoutData(new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL));
        
        Label subject = new Label(_rootMessage, SWT.NONE);
        //title.setFont(bold)
        subject.setText("Subject: ");
        subject.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_CENTER | GridData.HORIZONTAL_ALIGN_END));
        _messageSubject = new Text(_rootMessage, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        _messageSubject.setLayoutData(new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL));
        
        Label author = new Label(_rootMessage, SWT.NONE);
        //title.setFont(bold)
        author.setText("Author: ");
        author.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_CENTER | GridData.HORIZONTAL_ALIGN_END));
        _messageAuthor = new Text(_rootMessage, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        _messageAuthor.setLayoutData(new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL));
        
        Label date = new Label(_rootMessage, SWT.NONE);
        //title.setFont(bold)
        date.setText("Date: ");
        date.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_CENTER | GridData.HORIZONTAL_ALIGN_END));
        _messageDate = new Text(_rootMessage, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        _messageDate.setLayoutData(new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL));
        
        Composite actions = addActions(_rootMessage);
        if (actions != null) {
            GridData gd = new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL);
            gd.horizontalSpan = 2;
            actions.setLayoutData(gd);
        }
    }
    
    private void initArchiveComponents() {
        _rootArchive = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _rootArchive.setText("Archive summary");
        _rootArchive.setLayout(new GridLayout(2, false));
        
        Label title = new Label(_rootArchive, SWT.NONE);
        //title.setFont(bold)
        title.setText("Archive: ");
        title.setLayoutData(new GridData(GridData.FILL_BOTH));
        _archiveURI = new Text(_rootArchive, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        _archiveURI.setLayoutData(new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL));
        
        Composite actions = addActions(_rootArchive);
        if (actions != null) {
            GridData gd = new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL);
            gd.horizontalSpan = 2;
            actions.setLayoutData(gd);
        }
    }
    
    private void initURIComponents() {
        _rootURI = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _rootURI.setText("URI summary");
        _rootURI.setLayout(new GridLayout(2, false));
        
        Label title = new Label(_rootURI, SWT.NONE);
        //title.setFont(bold)
        title.setText("URI: ");
        title.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _uri = new Text(_rootURI, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        _uri.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        Composite actions = addActions(_rootURI);
        if (actions != null)
            actions.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, true, 2, 1));
    }
    
    private Composite addActions(Composite parent) {
        if (_listener != null) {
            Composite row = new Composite(parent, SWT.BORDER);
            row.setLayout(new FillLayout(SWT.HORIZONTAL));
            Button accept = new Button(row, SWT.PUSH);
            accept.setText("Accept");
            accept.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { if (_curReference != null) _listener.referenceAccepted(_curReference); }
                public void widgetSelected(SelectionEvent selectionEvent) { if (_curReference != null) _listener.referenceAccepted(_curReference); }
            });
            Button cancel = new Button(row, SWT.PUSH);
            cancel.setText("Cancel");
            cancel.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { _listener.referenceChoiceAborted(); }
                public void widgetSelected(SelectionEvent selectionEvent) { _listener.referenceChoiceAborted(); }
            });
            return row;
        } else {
            return null;
        }
    }
    
    public void bookmarkSelected(TreeItem item, NymReferenceNode node) {
        //System.out.println("bookmark selected [" + item.getText() + "]: " + node);
        _curReference = node.getURI();
        refSelected();
        // _root.pack();
        _root.layout();
    }
    private void refSelected() {
        if (_curReference == null) return;
        if (_curReference.isArchive()) {
            updateArchive(_curReference);
            _rootLayout.topControl = _rootArchive;
        } else if (_curReference.isChannel()) {
            Hash scope = _curReference.getScope();
            Long msgId = _curReference.getMessageId();
            if (scope != null) {
                if (msgId != null) {
                    long chanId = _chooser.getClient().getChannelId(scope);
                    if (chanId >= 0) {
                        ChannelInfo chan = _chooser.getClient().getChannel(chanId);
                        MessageInfo msg = _chooser.getClient().getMessage(chanId, msgId);
                        updateMessage(chan, msg, msgId.longValue());
                        _rootLayout.topControl = _rootMessage;
                    } else {
                        // ??
                        updateChannel(_curReference.getScope(), null);
                        _rootLayout.topControl = _rootChannel;
                    }
                } else {
                    long chanId = _chooser.getClient().getChannelId(scope);
                    if (chanId >= 0) {
                        ChannelInfo chan = _chooser.getClient().getChannel(chanId);
                        updateChannel(_curReference.getScope(), chan);
                        _rootLayout.topControl = _rootChannel;
                    } else {
                        updateChannel(_curReference.getScope(), null);
                        _rootLayout.topControl = _rootChannel;
                    }
                }
            } else {
                // channel URI but no scope?  noop
            }
        } else { //if (_curReference.isURL()) {
            updateURI(_curReference);
            _rootLayout.topControl = _rootURI;
        }
    }

    private void updateArchive(SyndieURI archiveURI) {
        _archiveURI.setText(archiveURI.toString());
    }
    private static final SimpleDateFormat _dayFmt = new SimpleDateFormat("yyyy/MM/dd");
    private static final String getDate(long msgId) {
        synchronized (_dayFmt) { return _dayFmt.format(new Date(msgId)); }
    }
    private void updateMessage(ChannelInfo chan, MessageInfo msg, long msgId) {
        _messageChannelHash.setText(chan.getChannelHash().toBase64());
        _messageChannelName.setText(chan.getName() + "");
        _messageChannelDesc.setText(chan.getDescription() + "");
        _messageDate.setText(getDate(msgId));
        if (msg == null) {
            _messageAuthor.setText("");
            _messageSubject.setText("");
            _messageAuthor.setEnabled(false);
            _messageSubject.setEnabled(false);
        } else {
            long authorChanId = msg.getAuthorChannelId();
            if (authorChanId == chan.getChannelId()) {
                _messageAuthor.setText(chan.getName() + "");
            } else {
                ChannelInfo author = _chooser.getClient().getChannel(authorChanId);
                if (author == null) { // implicit author
                    _messageAuthor.setText(chan.getName() + " [" + chan.getChannelHash().toBase64().substring(0,6) + "]");
                } else {
                    _messageAuthor.setText(author.getName() + " [" + author.getChannelHash().toBase64().substring(0,6) + "]");
                }
            }
            _messageSubject.setText(msg.getSubject() + "");
            _messageAuthor.setEnabled(true);
            _messageSubject.setEnabled(true);
        }
    }
    private void updateChannel(Hash scope, ChannelInfo chan) {
        if (scope == null) {
            _channelHash.setText("");
            _channelHash.setEnabled(false);
        } else {
            _channelHash.setText(scope.toBase64());
            _channelHash.setEnabled(true);
        }
        if (chan == null) {
            _channelDesc.setText("");
            _channelName.setText("");
            _channelPubPost.setSelection(false);
            _channelPubReply.setSelection(false);
            _channelDesc.setEnabled(false);
            _channelName.setEnabled(false);
            _channelPubPost.setEnabled(false);
            _channelPubReply.setEnabled(false);
        } else {
            _channelDesc.setText(chan.getDescription() + "");
            _channelName.setText(chan.getName() + "");
            _channelPubPost.setSelection(chan.getAllowPublicPosts());
            _channelPubReply.setSelection(chan.getAllowPublicReplies() || chan.getAllowPublicPosts());
            _channelDesc.setEnabled(true);
            _channelName.setEnabled(true);
            _channelPubPost.setEnabled(true);
            _channelPubReply.setEnabled(true);
        }
    }
    private void updateURI(SyndieURI uri) {
        _uri.setText(uri.toString());
    }
    
    public void manageChannelSelected(TreeItem item, ChannelInfo channel) {
        System.out.println("manage channel selected [" + item.getText() + "]: " + channel.getChannelHash().toBase64());
        _curReference = SyndieURI.createScope(channel.getChannelHash());
        updateChannel(channel.getChannelHash(), channel);
        _rootChannel.layout(true, true);
        _rootLayout.topControl = _rootChannel;
        _root.layout();
    }

    public void postChannelSelected(TreeItem item, ChannelInfo channel) {
        System.out.println("post channel selected [" + item.getText() + "]: " + channel.getChannelHash().toBase64());
        _curReference = SyndieURI.createScope(channel.getChannelHash());
        updateChannel(channel.getChannelHash(), channel);
        _rootLayout.topControl = _rootChannel;
        _root.layout();
    }

    public void searchResultSelected(TreeItem item, ReferenceNode node) {
        System.out.println("search result selected [" + item.getText() + "]: " + node.getURI().toString());
        _curReference = node.getURI();
        refSelected();
        _root.layout();
    }

    public void otherSelected(TreeItem item) {
        System.out.println("other item selected [" + item.getText() + "]");
    }
}
