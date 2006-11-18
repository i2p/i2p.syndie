package syndie.gui;

import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
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
public class BrowseForum implements MessageTree.MessageTreeListener {
    private DBClient _client;
    private Composite _parent;
    private SashForm _root;
    private Composite _top;
    private Composite _meta;
    private ImageCanvas _metaAvatar;
    private Link _metaName;
    private Menu _metaNameMenu;
    private Button _metaIconManageable;
    private Button _metaIconPostable;
    private Button _metaIconReferences;
    private Button _metaIconAdmins;
    private MessageTree _tree;
    private MessageTree.MessageTreeListener _listener;
    private MessagePreview _preview;
    private Hash _scope;
    private UI _ui;
    private BrowserControl _browser;
    
    public BrowseForum(Composite parent, BrowserControl browser, MessageTree.MessageTreeListener lsnr) {
        _browser = browser;
        _client = browser.getClient();
        _parent = parent;
        _listener = lsnr;
        _ui = browser.getUI();
        _ui.debugMessage("initializing browse");
        initComponents();
        _ui.debugMessage("browse initialized");
    }
    
    public Control getControl() { return _root; }
    
    private void initComponents() {
        _root = new SashForm(_parent, SWT.VERTICAL);
        _root.SASH_WIDTH = 3;
        _root.setBackground(ColorUtil.getColor("gray", null));
        
        _top = new Composite(_root, SWT.NONE);
        _top.setLayout(new GridLayout(1, true));
        _meta = new Composite(_top, SWT.NONE);
        _meta.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _meta.setLayout(new GridLayout(6, false));

        _metaAvatar = new ImageCanvas(_meta, false);
        _metaAvatar.forceSize(20, 20);
        _metaAvatar.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false));
        _metaName = new Link(_meta, SWT.NONE);
        _metaName.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, true, false));
        _metaName.setText("");
        _metaIconManageable = new Button(_meta, SWT.PUSH);
        _metaIconPostable = new Button(_meta, SWT.PUSH);
        _metaIconReferences = new Button(_meta, SWT.PUSH);
        _metaIconAdmins = new Button(_meta, SWT.PUSH);
        _metaIconManageable.setLayoutData(new GridData(20, 20));
        _metaIconPostable.setLayoutData(new GridData(20, 20));
        _metaIconReferences.setLayoutData(new GridData(20, 20));
        _metaIconAdmins.setLayoutData(new GridData(20, 20));
        _metaIconManageable.setText("m");
        _metaIconPostable.setText("p");
        _metaIconReferences.setText("r");
        _metaIconAdmins.setText("a");
        _metaIconManageable.setToolTipText("You can manage this forum");
        _metaIconPostable.setToolTipText("You can post in this forum");
        _metaIconReferences.setToolTipText("This forum has published references");
        _metaIconAdmins.setToolTipText("This forum has specific admins");
        _metaIconManageable.setEnabled(false);
        _metaIconPostable.setEnabled(false);
        _metaIconReferences.setEnabled(false);
        _metaIconAdmins.setEnabled(false);
        
        _metaNameMenu = new Menu(_metaName);
        new MenuItem(_metaNameMenu, SWT.PUSH).setText("Bookmark this forum");
        new MenuItem(_metaNameMenu, SWT.PUSH).setText("Mark all messages read");
        new MenuItem(_metaNameMenu, SWT.PUSH).setText("Delete read messages");
        new MenuItem(_metaNameMenu, SWT.PUSH).setText("Copy forum URI");
        new MenuItem(_metaNameMenu, SWT.SEPARATOR);
        new MenuItem(_metaNameMenu, SWT.PUSH).setText("Delete all messages");
        new MenuItem(_metaNameMenu, SWT.PUSH).setText("Ban this forum");
        
        _metaName.setMenu(_metaNameMenu);
        _metaName.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _metaNameMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _metaNameMenu.setVisible(true); }
        });
        
        _metaIconPostable.addSelectionListener(new SelectionListener() {
            public void widgetSelected(SelectionEvent selectionEvent) { post(); }
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { post(); }
        });
        
        _tree = new MessageTree(_client, _top, this);
        _tree.getControl().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _preview = new MessagePreview(_browser, _root);
        _root.setWeights(new int[] { 80, 20 });
        _root.setMaximizedControl(_top);
    }

    private void updateMetadata(SyndieURI uri) {
        // if the target channel has changed, update the metadata fields
        Hash scope = null;
        if (uri.isChannel())
            scope = uri.getScope();
        else if (uri.isSearch())
            scope = uri.getHash("scope");
        
        if ( ( (scope == null) && (_scope == null) ) || ( (scope != null) && (scope.equals(_scope)) ) )
            return; // same as before
        
        ChannelInfo info = null;
        if (scope != null) {
            long chanId = _client.getChannelId(scope);
            info = _client.getChannel(chanId);
        }
        
        if (info != null) {
            String name = info.getName();
            if (name == null) name = scope.toBase64().substring(0,6);
            StringBuffer buf = new StringBuffer();
            buf.append("<a>");
            buf.append(CommandImpl.strip(name, "\n\r\t<>", ' '));
            buf.append("</a>: ");
            String desc = info.getDescription();
            if (desc == null) desc = scope.toBase64();
            buf.append(desc);
            _metaName.setText(buf.toString());
            boolean manage = (_client.getNymKeys(scope, Constants.KEY_FUNCTION_MANAGE).size() > 0);
            _metaIconManageable.setEnabled(manage);
            if (manage)
                _metaIconPostable.setEnabled(true);
            else
                _metaIconPostable.setEnabled(_client.getNymKeys(scope, Constants.KEY_FUNCTION_POST).size() > 0);
            List refs = info.getReferences();
            if ( (refs != null) && (refs.size() > 0) )
                _metaIconReferences.setEnabled(true);
            else
                _metaIconReferences.setEnabled(false);
            if ( (info.getAuthorizedManagers().size() > 0) || (info.getAuthorizedPosters().size() > 0) )
                _metaIconAdmins.setEnabled(true);
            else
                _metaIconAdmins.setEnabled(false);
        } else {
            _metaName.setText("multiple forums selected");
            _metaIconManageable.setEnabled(false);
            _metaIconPostable.setEnabled(false);
            _metaIconReferences.setEnabled(false);
            _metaIconAdmins.setEnabled(false);
        }
        _scope = scope;
        _meta.layout(true, true);
    }

    public void setFilter(SyndieURI filter) { 
        _ui.debugMessage("setting filter...");
        _tree.setFilter(filter);
        _ui.debugMessage("applying filter...");
        _tree.applyFilter();
        _ui.debugMessage("filter applied");
    }
    
    public void messageSelected(MessageTree tree, SyndieURI uri, boolean toView) {
        //if (toView)
        //    _shell.setVisible(false);
        _ui.debugMessage("message selected: " + uri);
        preview(uri);
        if (_listener != null)
            _listener.messageSelected(tree, uri, toView);
    }

    public void filterApplied(MessageTree tree, SyndieURI searchURI) {
        // update the metadata line if the scope has changed
        updateMetadata(searchURI);
        if (_listener != null)
            _listener.filterApplied(tree, searchURI);
    }
    
    void preview(SyndieURI uri) { 
        _tree.select(uri);
        _root.setMaximizedControl(null);
        _ui.debugMessage("updating metadata in preview...");
        updateMetadata(uri);
        _ui.debugMessage("previewing...");
        _preview.preview(uri);
        _ui.debugMessage("preview complete");
    }
    
    private void post() {
        if (_browser != null) {
            _browser.view(_browser.createPostURI(_scope, null));
        }
    }
}
