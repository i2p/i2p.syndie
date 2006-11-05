package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.ChannelInfo;
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
    private AcceptanceListener _listener;
    private SyndieURI _curReference;
    private Composite _root;
    private StackLayout _rootLayout;
    /** shown in the stack layout when rendering a bookmark (nym reference node) */
    private Composite _rootNymRef;
    /** shown in the stack layout when rendering a search result value (reference node) */
    private Composite _rootRef;
    private Text _rootRefText;
    private Label _rootRefURI;
    
    /** shown in the stack layout when rendering a channel */
    private Composite _rootChannel;
    private Text _rootChannelText;
    
    /** Creates a new instance of ReferenceChooserInfo */
    public ReferenceChooserInfo(Composite parent, ReferenceChooserTree chooser, AcceptanceListener lsnr) {
        _parent = parent;
        _chooser = chooser;
        _listener = lsnr;
        initComponents();
    }
    
    public interface AcceptanceListener {
        public void referenceAccepted(SyndieURI uri);
    }
    
    public Control getControl() { return _root; }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(1, true));
        
        Composite info = new Composite(_root, SWT.NONE);
        _rootLayout = new StackLayout();
        info.setLayout(_rootLayout);
        
        _rootRef = new Composite(info, SWT.BORDER);
        _rootRef.setLayout(new GridLayout(1, true));
        _rootRefText = new Text(_rootRef, SWT.SIMPLE | SWT.BORDER);
        _rootRefText.setLayoutData(new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL));
        _rootRefURI = new Label(_rootRef, SWT.BORDER);
        _rootRefURI.setLayoutData(new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL));
        
        _rootChannel = new Composite(info, SWT.BORDER);
        _rootChannel.setLayout(new GridLayout(1, true));
        _rootChannelText = new Text(_rootChannel, SWT.SIMPLE | SWT.BORDER);
        _rootChannelText.setLayoutData(new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL));
        
        _rootNymRef = new Composite(info, SWT.BORDER);
        _rootNymRef.setLayout(new GridLayout(1, true));
        
        if (_listener != null) {
            Button accept = new Button(_root, SWT.PUSH);
            accept.setText("Accept");
            accept.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { _listener.referenceAccepted(_curReference); }
                public void widgetSelected(SelectionEvent selectionEvent) { _listener.referenceAccepted(_curReference); }
            });
        }
        
        
        _chooser.setListener(this);
    }

    public void bookmarkSelected(TreeItem item, NymReferenceNode node) {
        System.out.println("bookmark selected [" + item.getText() + "]: " + node);
        _curReference = node.getURI();
        _rootLayout.topControl = _rootNymRef;
        _root.layout();
    }

    public void manageChannelSelected(TreeItem item, ChannelInfo channel) {
        System.out.println("manage channel selected [" + item.getText() + "]: " + channel.getChannelHash().toBase64());
        _curReference = SyndieURI.createScope(channel.getChannelHash());
        _rootChannelText.setText(channel.toString());
        _rootLayout.topControl = _rootChannel;
        _root.layout();
    }

    public void postChannelSelected(TreeItem item, ChannelInfo channel) {
        System.out.println("post channel selected [" + item.getText() + "]: " + channel.getChannelHash().toBase64());
        _curReference = SyndieURI.createScope(channel.getChannelHash());
        _rootChannelText.setText(channel.toString());
        _rootLayout.topControl = _rootChannel;
        _root.layout();
    }

    public void searchResultSelected(TreeItem item, ReferenceNode node) {
        System.out.println("search result selected [" + item.getText() + "]: " + node.getURI().toString());
        _curReference = node.getURI();
        _rootRefText.setText(node.toString());
        _rootRefURI.setText(node.getURI().toString());
        _rootLayout.topControl = _rootRef;
        _root.layout();
    }

    public void otherSelected(TreeItem item) {
        System.out.println("other item selected [" + item.getText() + "]");
    }
}
