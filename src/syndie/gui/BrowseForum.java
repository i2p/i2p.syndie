package syndie.gui;

import java.util.ArrayList;
import java.util.List;
import net.i2p.data.Hash;
import net.i2p.data.SigningPrivateKey;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
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
import syndie.data.MessageInfo;
import syndie.data.NymKey;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.CommandImpl;
import syndie.db.DBClient;
import syndie.db.UI;


/**
 *
 */
public class BrowseForum extends BaseComponent implements MessageTree.MessageTreeListener, Translatable, Themeable {
    private NavigationControl _navControl;
    private BookmarkControl _bookmarkControl;
    private URIControl _uriControl;
    private DataCallback _dataCallback;
    private BanControl _banControl;

    private Composite _parent;
    private Composite _root;
    private SashForm _sash;
    private Composite _top;
    private Composite _meta;
    //private ImageCanvas _metaAvatar;
    private Label _metaAvatar;
    private Label _metaName;
    private Menu _metaNameMenu;
    private Button _metaAction;
    private Label _metaDesc;
    private MenuItem _metaNameMenuView;
    private MenuItem _metaNameMenuBookmark;
    private MenuItem _metaNameMenuMarkRead;
    private MenuItem _metaNameMenuDeleteRead;
    private MenuItem _metaNameMenuCopyURI;
    private MenuItem _metaNameMenuReply;
    private MenuItem _metaNameMenuDeleteAll;
    private MenuItem _metaNameMenuBan;
    private Button _metaIconManageable;
    private Button _metaIconPostable;
    private Label _metaIconArchives;
    private Label _metaIconReferences;
    private Label _metaIconAdmins;
    private Combo _metaRefCombo;
    private MessageTree _tree;
    private MessageTree.MessageTreeListener _listener;
    //private MessagePreview _preview;
    private Composite _filterRow;
    private Hash _scope;
    private boolean _viewOnly;
    //private boolean _shouldPreview;
    
    private boolean _byForum;
    
    public BrowseForum(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite parent, NavigationControl navControl, BookmarkControl bookmarkControl, URIControl uriControl, DataCallback callback, BanControl ban, MessageTree.MessageTreeListener lsnr, boolean byForum) {
        this(client, ui, themes, trans, parent, navControl, bookmarkControl, uriControl, callback, ban, lsnr, false, byForum);
    }
    public BrowseForum(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite parent, NavigationControl navControl, BookmarkControl bookmarkControl, URIControl uriControl, DataCallback callback, BanControl ban, MessageTree.MessageTreeListener lsnr, boolean viewOnly, boolean byForum) {
        super(client, ui, themes, trans);
        _navControl = navControl;
        _bookmarkControl = bookmarkControl;
        _uriControl = uriControl;
        _dataCallback = callback;
        _banControl = ban;
        _parent = parent;
        _listener = lsnr;
        _viewOnly = viewOnly;
        _byForum = byForum;
        //_shouldPreview = MessageTree.shouldShowPreview(client);
        _ui.debugMessage("initializing browse");
        initComponents();
        _ui.debugMessage("browse initialized");
    }
    
    public Control getControl() { return _sash; }
    public SyndieURI getURI() { return (_tree != null ? _tree.getCurrentFilter() : null); }
    
    void refresh() { if (_tree != null) _tree.applyFilter(); }
    public void toggleMaxView() {
        //if (_preview != null)
        //    _preview.toggleMaxView();
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(1, true));
        _sash = new SashForm(_root, SWT.VERTICAL);// | SWT.BORDER);
        _sash.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        //_sash.SASH_WIDTH = 3;
        //_sash.setBackground(ColorUtil.getColor("gray", null));
        
        _top = new Composite(_sash, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.verticalSpacing = 0;
        _top.setLayout(gl);
        _meta = new Composite(_top, SWT.NONE);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        gd.exclude = true;
        _meta.setLayoutData(gd);
        gl = new GridLayout(9, false);
        gl.horizontalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.verticalSpacing = 0;
        _meta.setLayout(gl);

        _metaAvatar = new Label(_meta, SWT.NONE);
        gd = new GridData(GridData.BEGINNING, GridData.CENTER, false, false);
        gd.exclude = true;
        _metaAvatar.setLayoutData(gd);
        _metaName = new Label(_meta, SWT.WRAP);
        _metaName.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false, false));
        _metaName.setText("");
        _metaAction = new Button(_meta, SWT.ARROW | SWT.DOWN);
        _metaAction.setLayoutData(new GridData(GridData.CENTER, GridData.CENTER, false, false));
        _metaDesc = new Label(_meta, SWT.WRAP);
        _metaDesc.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, true, false));
        _metaDesc.setText("");
        _metaIconManageable = new Button(_meta, SWT.PUSH);
        _metaIconPostable = new Button(_meta, SWT.PUSH);
        _metaIconArchives = new Label(_meta, SWT.NONE);
        _metaIconReferences = new Label(_meta, SWT.NONE);
        _metaIconAdmins = new Label(_meta, SWT.NONE);
        //_metaIconManageable.setLayoutData(new GridData(20, 20));
        //_metaIconPostable.setLayoutData(new GridData(20, 20));
        //_metaIconArchives.setLayoutData(new GridData(20, 20));
        //_metaIconReferences.setLayoutData(new GridData(20, 20));
        //_metaIconAdmins.setLayoutData(new GridData(20, 20));
        _metaIconManageable.setLayoutData(new GridData(GridData.END, GridData.FILL, false, false));
        _metaIconPostable.setLayoutData(new GridData(GridData.END, GridData.FILL, false, false));
        _metaIconArchives.setLayoutData(new GridData(GridData.END, GridData.FILL, false, false));
        _metaIconReferences.setLayoutData(new GridData(GridData.END, GridData.FILL, false, false));
        _metaIconAdmins.setLayoutData(new GridData(GridData.END, GridData.FILL, false, false));
        
        _metaIconManageable.setVisible(false);
        _metaIconPostable.setVisible(false);
        _metaIconArchives.setVisible(false);
        _metaIconReferences.setVisible(false);
        _metaIconAdmins.setVisible(false);
        _metaIconManageable.setImage(ImageUtil.ICON_BROWSE_MANAGEABLE);
        _metaIconPostable.setImage(ImageUtil.ICON_BROWSE_POSTABLE);
        _metaIconArchives.setImage(ImageUtil.ICON_BROWSE_ARCHIVES);
        _metaIconReferences.setImage(ImageUtil.ICON_BROWSE_REFS);
        _metaIconAdmins.setImage(ImageUtil.ICON_BROWSE_ADMINS);
        
        ((GridData)_metaIconManageable.getLayoutData()).exclude = true;
        ((GridData)_metaIconPostable.getLayoutData()).exclude = true;
        ((GridData)_metaIconArchives.getLayoutData()).exclude = true;
        ((GridData)_metaIconReferences.getLayoutData()).exclude = true;
        ((GridData)_metaIconAdmins.getLayoutData()).exclude = true;
        
        _metaNameMenu = new Menu(_metaName);
        _metaNameMenuView = new MenuItem(_metaNameMenu, SWT.PUSH);
        _metaNameMenuBookmark = new MenuItem(_metaNameMenu, SWT.PUSH);
        _metaNameMenuMarkRead = new MenuItem(_metaNameMenu, SWT.PUSH);
        _metaNameMenuDeleteRead = new MenuItem(_metaNameMenu, SWT.PUSH);
        _metaNameMenuCopyURI = new MenuItem(_metaNameMenu, SWT.PUSH);
        _metaNameMenuReply = new MenuItem(_metaNameMenu, SWT.PUSH);
        _metaNameMenuDeleteAll = new MenuItem(_metaNameMenu, SWT.PUSH);
        _metaNameMenuBan = new MenuItem(_metaNameMenu, SWT.PUSH);
        
        _metaNameMenuDeleteRead.setEnabled(false);
        _metaNameMenuDeleteAll.setEnabled(false);
        
        _metaAction.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _metaNameMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { _metaNameMenu.setVisible(true); }
        });
        
        _metaNameMenuView.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _navControl.view(_uriControl.createMetaURI(_scope)); }
            public void widgetSelected(SelectionEvent selectionEvent) { _navControl.view(_uriControl.createMetaURI(_scope)); }
        });
        
        _metaNameMenuBookmark.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _bookmarkControl.bookmark(SyndieURI.createScope(_scope)); }
            public void widgetSelected(SelectionEvent selectionEvent) { _bookmarkControl.bookmark(SyndieURI.createScope(_scope)); }
        });
        
        _metaNameMenuMarkRead.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { markAllRead(); }
            public void widgetSelected(SelectionEvent selectionEvent) { markAllRead(); }
            private void markAllRead() {
                Hash scope = _scope;
                if (scope == null) return;
                long scopeId = _client.getChannelId(scope);
                if (scopeId >= 0) {
                    _client.markChannelRead(scopeId);
                    // the filter may want to exclude 'read' messages (or it may just want
                    // to redraw them differently)
                    _tree.applyFilter();
                    _dataCallback.readStatusUpdated();
                }
            }
        });
        
        _metaNameMenuBan.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { fire(); }
            public void widgetSelected(SelectionEvent selectionEvent) { fire(); }
            private void fire() {
                if (_banControl.ban(_scope))
                    _navControl.unview(getURI());
                    
            }
        });
        
        _metaNameMenuCopyURI.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { copyURI(); }
            public void widgetSelected(SelectionEvent selectionEvent) { copyURI(); }
            private void copyURI() {
                if (_scope != null) {
                    SyndieURI uri = SyndieURI.createScope(_scope);
                    TextTransfer tt = TextTransfer.getInstance();
                    Clipboard clip = new Clipboard(_root.getDisplay());
                    Transfer xf[] = new Transfer[] { tt };
                    Object data[] = new Object[] { uri.toString() };
                    clip.setContents(data, xf);
                    clip.dispose();
                }
            }
        });
        
        _metaNameMenuReply.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { reply(); }
            public void widgetSelected(SelectionEvent selectionEvent) { reply(); }
            private void reply() {
                if (_scope != null)
                    _navControl.view(_uriControl.createPostURI(_scope, null, true));
            }
        });
        
        _metaName.setMenu(_metaNameMenu);
        _metaName.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) {}
            public void mouseDown(MouseEvent mouseEvent) { _metaNameMenu.setVisible(true); }
            public void mouseUp(MouseEvent mouseEvent) {}
        });
        _metaName.addKeyListener(new KeyListener() {
            public void keyPressed(KeyEvent keyEvent) {}
            public void keyReleased(KeyEvent keyEvent) { _metaNameMenu.setVisible(true); }
        });
        
        _metaIconManageable.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) {}
            public void mouseDown(MouseEvent mouseEvent) { manage(); }
            public void mouseUp(MouseEvent mouseEvent) {}
        });
        _metaIconManageable.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) manage();
            }
        });
        
        _metaIconPostable.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) {}
            public void mouseDown(MouseEvent mouseEvent) { post(); }
            public void mouseUp(MouseEvent mouseEvent) {}
        });
        _metaIconPostable.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) post();
            }
        });
        
        _metaIconArchives.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) {}
            public void mouseDown(MouseEvent mouseEvent) { viewArchives(); }
            public void mouseUp(MouseEvent mouseEvent) {}
        });
        _metaIconArchives.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) viewArchives();
            }
        });
        
        _metaIconReferences.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) {}
            public void mouseDown(MouseEvent mouseEvent) { viewRefs(); }
            public void mouseUp(MouseEvent mouseEvent) {}
        });
        _metaIconReferences.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) viewRefs();
            }
        });
        
        _metaIconAdmins.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) {}
            public void mouseDown(MouseEvent mouseEvent) { viewAdmins(); }
            public void mouseUp(MouseEvent mouseEvent) {}
        });
        _metaIconAdmins.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) viewAdmins();
            }
        });

        _metaRefCombo = new Combo(_meta, SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
        _metaRefCombo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 9, 1));
        ((GridData)_metaRefCombo.getLayoutData()).exclude = true;
        _metaRefCombo.setVisible(false);
        _metaRefCombo.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                int idx = _metaRefCombo.getSelectionIndex();
                if ( (idx >= 0) && (idx < _refs.size()) ) {
                    SyndieURI uri = (SyndieURI)_refs.get(idx);
                    _navControl.view(uri);
                }
            }
        });
        //_metaRefCombo.add("Refs go here...");
        //_metaRefCombo.add("abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz");

        _ui.debugMessage("browseForum.initialize: creating tree");
        if (_byForum)
            _tree = ComponentBuilder.instance().createWatchedMessageTree(_top, this, true);
        else
            _tree = ComponentBuilder.instance().createMessageTree(_top, this, true);
        _tree.getControl().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        //_ui.debugMessage("browseForum.initialize: creating preview");
        //_preview = ComponentBuilder.instance().createMessagePreview(_sash);
        _ui.debugMessage("browseForum.initialize: preview created");
        //_sash.setWeights(new int[] { 50, 50 });
        
        //if (_viewOnly) // erm, lets not waste all this stuff on the Messagetree if we don't need it
        //    _sash.setMaximizedControl(_preview.getControl());
        //else
            _sash.setMaximizedControl(_top);
        
        _filterRow = new Composite(_root, SWT.BORDER);
        _filterRow.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _filterRow.setLayout(new GridLayout(8, false));
        
        // the tree keeps track of the components, updating 'em, etc, and disposing on tree disposal
        _tree.createFilterBar(_filterRow, null);
        /*new MessageTree.PreviewControlListener() {
            public void togglePreview(boolean shouldShow) {
                //_shouldPreview = shouldShow;
                if (shouldShow) {
                    SyndieURI uri = _tree.getSelected();
                    preview(uri, false);
                    //_sash.setMaximizedControl(null);
                } else {
                    _sash.setMaximizedControl(_top);
                }
            }
        });
         */
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }

    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        //_preview.dispose();
        _tree.dispose();
        ImageUtil.dispose(_metaAvatar.getImage());
    }
    
    private void updateMetadata(SyndieURI uri) {
        // if the target channel has changed, update the metadata fields
        Hash scope = null;
        if (uri.isChannel())
            scope = uri.getScope();
        else if (uri.isSearch()) {
            String scopes[] = uri.getStringArray("scope");
            if ( (scopes != null) && (scopes.length == 1) )
                scope = uri.getHash("scope");
            else
                scope = null;
        }
        
        _ui.debugMessage("Update metadata for " + scope + " / " + uri);
        
        if ( ( (scope == null) && (_scope == null) ) || ( (scope != null) && (scope.equals(_scope)) ) )
            return; // same as before
        
        ChannelInfo info = null;
        if (scope != null) {
            long chanId = _client.getChannelId(scope);
            info = _client.getChannel(chanId);
            
            if ( (chanId >= 0) && (uri.getMessageId() != null) ) {
                MessageInfo msg = _client.getMessage(chanId, uri.getMessageId());
                if (msg != null) {
                    scope = msg.getTargetChannel();
                    info = _client.getChannel(msg.getTargetChannelId());
                }
            }
        }
        
        if (info != null) {
            GridData gd = (GridData)_meta.getLayoutData();
            gd.exclude = false;
            _ui.debugMessage("update metadata: forum: " + info.getChannelHash().toBase64());
            String name = info.getName();
            if (name == null) name = scope.toBase64().substring(0,6);
            _metaName.setText(name);
            
            byte avatar[] = _client.getChannelAvatar(info.getChannelId());
            Image img = null;
            if (avatar != null) {
                _ui.debugMessage("avatar found for channel " + info.getChannelHash().toBase64() + "/" + info.getChannelId());
                img = ImageUtil.createImage(avatar);
                if (img != null) {
                    Rectangle bounds = img.getBounds();
                    int width = bounds.width;
                    int height = bounds.height;
                    if ((width > Constants.MAX_AVATAR_WIDTH) || (height > Constants.MAX_AVATAR_HEIGHT))
                        img = ImageUtil.resize(img, Math.min(width, Constants.MAX_AVATAR_WIDTH), Math.min(height, Constants.MAX_AVATAR_HEIGHT), true);
                }
            } else {
                _ui.debugMessage("no avatar found for channel " + info.getChannelHash().toBase64() + "/" + info.getChannelId());
            }
            _metaAvatar.setImage(img);
            gd = (GridData)_metaAvatar.getLayoutData();
            gd.exclude = (img == null);
            _metaAvatar.setVisible(img != null);
            
            String desc = info.getDescription();
            if (desc == null) desc = scope.toBase64();
            _metaDesc.setText(desc);
            boolean manage = false;
            boolean post = info.getAllowPublicPosts();
            
            List nymKeys = _client.getNymKeys(null, null);
            for (int i = 0; i < nymKeys.size(); i++) {
                NymKey key = (NymKey)nymKeys.get(i);
                if (info.getChannelHash().equals(key.getChannel())) {
                    manage = true;
                    post = true;
                    break;
                } else if (Constants.KEY_TYPE_DSA.equals(key.getType())) {
                    SigningPrivateKey priv = new SigningPrivateKey(key.getData());
                    Hash pub = priv.toPublic().calculateHash();
                    if (info.getAuthorizedManagerHashes().contains(pub)) {
                        manage = true;
                        post = true;
                        break;
                    } else if (info.getAuthorizedPosterHashes().contains(pub)) {
                        post = true;
                        break;
                    }
                }
            }

            _metaIconManageable.setVisible(manage);
            _metaIconPostable.setVisible(post);
            boolean hasArchives = info.getPublicArchives().size() > 0;
            if (!hasArchives && (info.getPrivateArchives().size() > 0))
                hasArchives = true;
            _metaIconArchives.setVisible(hasArchives);
            List refs = info.getReferences();
            boolean inclRefs = (refs != null) && (refs.size() > 0);
            _metaIconReferences.setVisible(inclRefs);
            
            boolean admins = (info.getAuthorizedManagers().size() > 0) || (info.getAuthorizedPosters().size() > 0);
            _metaIconAdmins.setVisible(admins);
            
            ((GridData)_metaIconManageable.getLayoutData()).exclude = !manage;
            ((GridData)_metaIconPostable.getLayoutData()).exclude = !post;
            ((GridData)_metaIconArchives.getLayoutData()).exclude = !hasArchives;
            ((GridData)_metaIconReferences.getLayoutData()).exclude = !inclRefs;
            ((GridData)_metaIconAdmins.getLayoutData()).exclude = !admins;
        
            if (inclRefs) {
                addRefs(refs);
                _metaRefCombo.setVisible(true);
                ((GridData)_metaRefCombo.getLayoutData()).exclude = false;
            } else {
                _metaRefCombo.setVisible(false);
                ((GridData)_metaRefCombo.getLayoutData()).exclude = true;
            }
            
            _top.layout(true, true);
        } else {
            GridData gd = (GridData)_meta.getLayoutData();
            gd.exclude = false;
            _ui.debugMessage("update metadata: no forum");
            _metaName.setText("");
            _metaDesc.setText(_translationRegistry.getText(T_META_NAME_MULTIPLE, "multiple forums selected"));
            _metaIconManageable.setVisible(false);
            _metaIconPostable.setVisible(false);
            _metaIconArchives.setVisible(false);
            _metaIconReferences.setVisible(false);
            _metaIconAdmins.setVisible(false);
                
            ((GridData)_metaIconManageable.getLayoutData()).exclude = true;
            ((GridData)_metaIconPostable.getLayoutData()).exclude = true;
            ((GridData)_metaIconArchives.getLayoutData()).exclude = true;
            ((GridData)_metaIconReferences.getLayoutData()).exclude = true;
            ((GridData)_metaIconAdmins.getLayoutData()).exclude = true;
            
            _metaRefCombo.setVisible(false);
            ((GridData)_metaRefCombo.getLayoutData()).exclude = true;
            
            _ui.debugMessage("no avatar found for no channel: " + uri);
            gd = (GridData)_metaAvatar.getLayoutData();
            gd.exclude = true;
            _metaAvatar.setVisible(false);
            _top.layout(true, true);
        }
        _scope = scope;
        // need to layout the root, since the image size can change, thereby adjusting the meta hight
        // and thereby the message table height
        //_meta.layout(true, true);
        _sash.layout(true, true);
    }

    private List _refs;
    private void addRefs(List refRoots) {
        _refs = new ArrayList();
        _metaRefCombo.removeAll();
        ReferenceNode.walk(refRoots, new ReferenceNode.Visitor() {
            public void visit(ReferenceNode node, int depth, int siblingOrder) {
                SyndieURI uri = node.getURI();
                if (uri == null) return;
                String name = node.getName();
                String desc = node.getDescription();
                String val = null;
                if ( (name != null) && (desc != null) )
                    val = name + ": " + desc;
                else if (name != null)
                    val = name;
                else if (desc != null)
                    val = desc;
                else
                    val = "";
                if (uri.getURL() != null)
                    val = val + " - " + uri.getURL();
                else
                    val = val + " - " + uri.toString();
                
                _metaRefCombo.add(val);
                _refs.add(uri);
            }
        });
    }
    
    private static final String T_META_NAME_MULTIPLE = "syndie.gui.browseforum.meta.name.multiple";
    
    public void setFilter(SyndieURI filter) { 
        if (!_viewOnly) {
            _ui.debugMessage("setting filter...");
            _tree.setFilter(filter);
            _ui.debugMessage("applying filter...: " + filter);
            _tree.applyFilter();
            _ui.debugMessage("filter applied: " + filter);
        }
    }
    
    public void messageSelected(MessageTree tree, SyndieURI uri, boolean toView, boolean nodelay) {
        //if (toView)
        //    _shell.setVisible(false);
        //_ui.debugMessage("message selected: " + uri);
        if (toView)
            _navControl.view(uri);
        //else
        //    preview(uri, false, nodelay);
        if (_listener != null)
            _listener.messageSelected(tree, uri, toView, nodelay);
    }

    public void filterApplied(MessageTree tree, SyndieURI searchURI) {
        // update the metadata line if the scope has changed
        updateMetadata(searchURI);
        if (_listener != null)
            _listener.filterApplied(tree, searchURI);
    }
    
    void preview(SyndieURI uri) { _tree.select(uri); }

    /*
    private SyndieURI _toPreview;
    private static final int PREVIEW_DELAY = 500;
    // only actually preview if 500ms passes w/out trying to preview something else
    void preview(final SyndieURI uri, final boolean fullscreen) { preview(uri, fullscreen, false); }
    void preview(final SyndieURI uri, final boolean fullscreen, boolean nodelay) {
        if ( (uri == null) || (uri.getMessageId() == null) ) return;
        _ui.debugMessage("preview request for " + uri);
        _toPreview = uri;
        Runnable r = new Runnable() { 
            public void run() { 
                if (uri == _toPreview)
                    doPreview(uri, fullscreen); 
            } 
        };
        
        if (nodelay)
            _root.getDisplay().asyncExec(r);
        else
            _root.getDisplay().timerExec(PREVIEW_DELAY, r);
    }
    
    // actually preview
    void doPreview(SyndieURI uri, boolean fullscreen) {
        if (!_shouldPreview) return;
        _ui.debugMessage("previewing " + uri);
        if (fullscreen && uri.isChannel() && (uri.getScope() != null) && (uri.getMessageId() != null) )
            _sash.setMaximizedControl(_preview.getControl());
        else
            _sash.setMaximizedControl(null);
        _tree.select(uri); // do this after the preview pane is shown, so it doesn't hide lower selections
        // dont update the metadata, since a message may be selected that isn't strictly
        // in this forum (eg its in another forum, but uses something in the filtered messages
        // as a parent).  when viewing a forum, the forum metadata at the top stays the same
        // (unless you edit the filter to view another forum)
        //_ui.debugMessage("updating metadata in preview...");
        //updateMetadata(uri);
        _ui.debugMessage("previewing...");
        _preview.preview(uri);
        _ui.debugMessage("preview complete");
    }
     */
    
    private void post() {
        _ui.debugMessage("posting...");
        _navControl.view(_uriControl.createPostURI(_scope, null));
    }
    
    private void manage() { _navControl.view(_uriControl.createManageURI(_scope)); }
    private void viewRefs() { _navControl.view(_uriControl.createMetaRefsURI(_scope)); }
    private void viewArchives() { _navControl.view(_uriControl.createMetaArchivesURI(_scope)); }
    private void viewAdmins() { _navControl.view(_uriControl.createMetaManagersURI(_scope)); }

    private static final String T_MANAGEABLE_TOOLTIP = "syndie.gui.browseforum.manageable";
    private static final String T_POSTABLE_TOOLTIP = "syndie.gui.browseforum.postable";
    private static final String T_ARCHIVES_TOOLTIP = "syndie.gui.browseforum.archives";
    private static final String T_REFS_TOOLTIP = "syndie.gui.browseforum.refs";
    private static final String T_ADMINS_TOOLTIP = "syndie.gui.browseforum.admins";

    private static final String T_BOOKMARK = "syndie.gui.browseforum.bookmark";
    private static final String T_VIEW = "syndie.gui.browseforum.view";
    private static final String T_MARKALLREAD = "syndie.gui.browseforum.markallread";
    private static final String T_DELETEREAD = "syndie.gui.browseforum.deleteread";
    private static final String T_COPYURI = "syndie.gui.browseforum.copyuri";
    private static final String T_DELETEALL = "syndie.gui.browseforum.deleteall";
    private static final String T_BAN = "syndie.gui.browseforum.ban";
    
    private static final String T_MANAGE = "syndie.gui.browseforum.manage";
    private static final String T_POST = "syndie.gui.browseforum.post";
    
    private static final String T_REPLY = "syndie.gui.browseforum.reply";
    
    public void translate(TranslationRegistry registry) {
        _metaIconManageable.setToolTipText(registry.getText(T_MANAGEABLE_TOOLTIP, "You can manage this forum"));
        _metaIconPostable.setToolTipText(registry.getText(T_POSTABLE_TOOLTIP, "You can post in this forum"));
        _metaIconArchives.setToolTipText(registry.getText(T_ARCHIVES_TOOLTIP, "This forum has published archives"));
        _metaIconReferences.setToolTipText(registry.getText(T_REFS_TOOLTIP, "This forum has published references"));
        _metaIconAdmins.setToolTipText(registry.getText(T_ADMINS_TOOLTIP, "This forum has specific admins"));

        _metaIconManageable.setText(registry.getText(T_MANAGE, "Manage"));
        _metaIconPostable.setText(registry.getText(T_POST, "Post"));
        
        _metaNameMenuView.setText(registry.getText(T_VIEW, "View the forum profile"));
        _metaNameMenuView.setImage(ImageUtil.ICON_VIEW);

        _metaNameMenuBookmark.setText(registry.getText(T_BOOKMARK, "Bookmark this forum"));
        _metaNameMenuBookmark.setImage(ImageUtil.ICON_ADDBOOKMARK);

        _metaNameMenuMarkRead.setText(registry.getText(T_MARKALLREAD, "Mark all messages read"));
        _metaNameMenuDeleteRead.setText(registry.getText(T_DELETEREAD, "Delete read messages"));
        _metaNameMenuCopyURI.setText(registry.getText(T_COPYURI, "Copy forum URI"));
        _metaNameMenuReply.setText(registry.getText(T_REPLY, "Send the forum administrators a private message"));
        _metaNameMenuDeleteAll.setText(registry.getText(T_DELETEALL, "Delete all messages"));
        _metaNameMenuBan.setText(registry.getText(T_BAN, "Ban this forum"));
        _meta.layout(true, true);
    }
    
    public void applyTheme(Theme theme) {
        _metaName.setFont(theme.LINK_FONT);
        _metaDesc.setFont(theme.DEFAULT_FONT);
        
        _metaIconManageable.setFont(theme.BUTTON_FONT);
        _metaIconPostable.setFont(theme.BUTTON_FONT);
        _metaRefCombo.setFont(theme.DEFAULT_FONT);
        
        _ui.debugMessage("meta name size: " + _metaName.getFont().getFontData()[0].getHeight() + "/" + _metaName.getText());
        //_root.layout(true, true);
    }
}
