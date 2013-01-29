package syndie.gui;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import net.i2p.data.Hash;
import net.i2p.data.PrivateKey;
import net.i2p.data.SessionKey;
import net.i2p.data.SigningPrivateKey;
import net.i2p.data.SigningPublicKey;

import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import syndie.Constants;
import syndie.data.ArchiveInfo;
import syndie.data.ChannelInfo;
import syndie.data.NymKey;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;
import syndie.util.DateTime;

/**
 *
 */
public class ViewForum extends BaseComponent implements Translatable, Themeable {
    private BanControl _banControl;
    private NavigationControl _navControl;
    private URIControl _uriControl;
    private Composite _parent;
    private SyndieURI _uri;
    private Hash _scope;
    private long _scopeId;
    
    private Composite _root;
    private Label _avatar;
    private Image _avatarImgOrig;
    private Image _avatarImg;
    private ArrayList _avatarImgStandard;
    private Label _nameLabel;
    private Text _name;
    private Label _tagsLabel;
    private Text _tags;
    private Label _descriptionLabel;
    private Text _description;
    private Menu _avatarMenu;
    private Label _authorizationLabel;
    private Label _authorization;
    private Label _expirationLabel;
    private Text _expiration;
    private Button _expirationManager;
    private Group _refGroup;
    private Tree _refTree;
    private ArrayList _refRoots;
    private Map _refItemToNode;
    private Group _banGroup;
    private List _banList;
    private Menu _banMenu;
    private MenuItem _banView;
    private MenuItem _banImport;
    private ArrayList _banScopes;
    private Group _userGroup;
    private Table _users;
    private TableColumn _userName;
    private TableColumn _userPriv;
    private Map _userItemToHash;
    private Group _archiveGroup;
    private Table _archives;
    private TableColumn _archiveURL;
    private TableColumn _archiveIsPub;
    private Map _archiveItemToURI;

    private boolean _initialized;

    private ArrayList _managerHashes;
    private ArrayList _posterHashes;
    private ArrayList _pubArchiveURIs;
    private ArrayList _privArchiveURIs;
    
    private int _auth = 1;
    
    public ViewForum(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, BanControl banControl, NavigationControl navControl, URIControl uriControl, Composite parent, SyndieURI uri) {
        super(client, ui, themes, trans);
        _banControl = banControl;
        _navControl = navControl;
        _uriControl = uriControl;
        _parent = parent;
        _uri = uri;
        _scope = null;
        _scopeId = -1;
        _initialized = false;
        _avatarImgStandard = new ArrayList();
        _archiveItemToURI = new HashMap();
        _userItemToHash = new HashMap();
        _privArchiveURIs = new ArrayList();
        _pubArchiveURIs = new ArrayList();
        _managerHashes = new ArrayList();
        _posterHashes = new ArrayList();
        _refItemToNode = new HashMap();
        _banScopes = new ArrayList();
        _refRoots = new ArrayList();
        Hash scope = uri.getScope();
        if (scope == null)
            scope = uri.getHash("scope");
        if (scope != null) {
            _scope = scope;
            _scopeId = _client.getChannelId(scope);
        } else {
            _ui.debugMessage("no scope!  creating a new one");
        }
        initComponents();
    }
    public boolean getEditable() { return false; }
    public boolean canShow(Hash scope) {
        return (scope != null) && (_scope != null) && _scope.equals(scope);
    }
    
    public void resized() {
        _ui.debugMessage("resizing forum parent");
        applyTheme(_themeRegistry.getTheme()); 
        _parent.layout(true, true);
    }
    
    Composite getRoot() { return _root; }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(7, false));
        
        loadOrigAvatar();
        
        _avatar = new Label(_root, SWT.NONE);
        _avatar.setLayoutData(new GridData(GridData.CENTER, GridData.CENTER, false, false, 1, 3));
        
        _nameLabel = new Label(_root, SWT.NONE);
        _nameLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _name = new Text(_root, SWT.BORDER | SWT.SINGLE | SWT.READ_ONLY);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        //gd.widthHint = 100;
        _name.setLayoutData(gd);
        
        _tagsLabel = new Label(_root, SWT.NONE);
        _tagsLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _tags = new Text(_root, SWT.BORDER | SWT.SINGLE | SWT.READ_ONLY);
        _tags.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
        
        _descriptionLabel = new Label(_root, SWT.NONE);
        _descriptionLabel.setLayoutData(new GridData(GridData.END, GridData.BEGINNING, false, false));
        
        int descWidth = 3;
        _description = new Text(_root, SWT.BORDER | SWT.SINGLE | SWT.READ_ONLY);
        _description.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, descWidth, 1));
        
        Composite expiration = new Composite(_root, SWT.NONE);
        expiration.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false, 2, 1));
        expiration.setLayout(new GridLayout(3, false));
        
        _expirationLabel = new Label(expiration, SWT.NONE);
        _expirationLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
        
        _expiration = new Text(expiration, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        gd.widthHint = 100;
        _expiration.setLayoutData(gd);
        
        _expirationManager = new Button(expiration, SWT.PUSH);
        _expirationManager.addSelectionListener(new FireSelectionListener() { public void fire() { _navControl.view(URIHelper.instance().createExpirationURI(_scope)); } });
        _expirationManager.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _expirationManager.setImage(ImageUtil.ICON_MANAGEABLEFORUM);
        
        _authorizationLabel = new Label(_root, SWT.NONE);
        _authorizationLabel.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));

        _authorization = new Label(_root, SWT.NONE);
        _authorization.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 5, 1));
        
        Composite refRow = new Composite(_root, SWT.NONE);
        refRow.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 7, 1));
        refRow.setLayout(new FillLayout(SWT.HORIZONTAL));
        
        _refGroup = new Group(refRow, SWT.SHADOW_ETCHED_IN);
        _refGroup.setLayout(new FillLayout());
        _refTree = new Tree(_refGroup, SWT.MULTI | SWT.BORDER);
        _refTree.addMouseTrackListener(new MouseTrackListener() {
            public void mouseEnter(MouseEvent mouseEvent) {}
            public void mouseExit(MouseEvent mouseEvent) {
                _refTree.setToolTipText("");
            }
            public void mouseHover(MouseEvent evt) {
                TreeItem item = _refTree.getItem(new Point(evt.x, evt.y));
                if (item != null) {
                    ReferenceNode node = (ReferenceNode)_refItemToNode.get(item);
                    if (node != null) {
                        if (node.getDescription() != null)
                            _refTree.setToolTipText(node.getDescription());
                        else if (node.getURI() != null)
                            _refTree.setToolTipText(node.getURI().toString());
                        else
                            _refTree.setToolTipText("");
                    } else {
                        _refTree.setToolTipText("");
                    }
                }
            }
        });
        SyndieTreeListener lsnr = new SyndieTreeListener(_refTree) {
            /** the user doubleclicked on the selected row */
            public void doubleclick() {
                TreeItem items[] = _refTree.getSelection();
                for (int i = 0; i < items.length; i++) {
                    ReferenceNode node = (ReferenceNode)_refItemToNode.get(items[i]);
                    if ( (node != null) && (node.getURI() != null) )
                        _navControl.view(node.getURI());
                }
            }
            /** the user hit return on the selected row */
            public void returnHit() {
                TreeItem items[] = _refTree.getSelection();
                for (int i = 0; i < items.length; i++) {
                    ReferenceNode node = (ReferenceNode)_refItemToNode.get(items[i]);
                    if ( (node != null) && (node.getURI() != null) )
                        _navControl.view(node.getURI());
                }
            }
        };
        _refTree.addKeyListener(lsnr);
        _refTree.addMouseListener(lsnr);
        _refTree.addSelectionListener(lsnr);
        _refTree.addTraverseListener(lsnr);
        
        _banGroup = new Group(refRow, SWT.SHADOW_ETCHED_IN);
        _banGroup.setLayout(new FillLayout());
        _banList = new List(_banGroup, SWT.MULTI | SWT.BORDER);
        _banMenu = new Menu(_banList);
        _banList.setMenu(_banMenu);
        
        _banView = new MenuItem(_banMenu, SWT.PUSH);
        _banView.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                int indexes[] = _banList.getSelectionIndices();
                for (int i = 0; i < indexes.length; i++) {
                    Hash scope = (Hash)_banScopes.get(indexes[i]);
                    if (_client.getChannelId(scope) >= 0)
                        _navControl.view(SyndieURI.createScope(scope));
                }
            }
        });
        _banImport = new MenuItem(_banMenu, SWT.PUSH);
        _banImport.addSelectionListener(new FireSelectionListener() {
             public void fire() {
                int indexes[] = _banList.getSelectionIndices();
                for (int i = 0; i < indexes.length; i++) {
                    Hash scope = (Hash)_banScopes.get(indexes[i]);
                    _banControl.ban(scope);
                }
            }
        });
        
        initDnD();
        
        _userGroup = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _userGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 7, 1));
        _userGroup.setLayout(new GridLayout(1, false));

        _users = new Table(_userGroup, SWT.SINGLE | SWT.FULL_SELECTION | SWT.BORDER);
        _users.setHeaderVisible(false);
        _users.setLinesVisible(true);
        gd = new GridData(GridData.FILL, GridData.FILL, true, true, 1, 2);
        _users.setLayoutData(gd);

        _userName = new TableColumn(_users, SWT.LEFT);
        _userPriv = new TableColumn(_users, SWT.RIGHT);

        final Menu userMenu = new Menu(_users);
        _users.setMenu(userMenu);
        MenuItem viewForum = new MenuItem(userMenu, SWT.PUSH);
        viewForum.setImage(ImageUtil.ICON_VIEW);
        viewForum.setText(getText("View forum"));
        viewForum.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewUser(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewUser(); }
            private void viewUser() {
                TableItem items[] = _users.getSelection();
                for (int i = 0; i < items.length; i++) {
                    Hash scope = (Hash)_userItemToHash.get(items[i]);
                    _navControl.view(SyndieURI.createScope(scope));
                }
            }
        });
        MenuItem viewMeta = new MenuItem(userMenu, SWT.PUSH);
        viewMeta.setImage(ImageUtil.ICON_HM_ABOUT);
        viewMeta.setText(getText("View forum profile"));
        viewMeta.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewMeta(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewMeta(); }
            private void viewMeta() {
                TableItem items[] = _users.getSelection();
                for (int i = 0; i < items.length; i++) {
                    Hash scope = (Hash)_userItemToHash.get(items[i]);
                    _navControl.view(_uriControl.createMetaURI(scope));
                }
            }
        });

        _archiveGroup = new Group(_root, SWT.SHADOW_ETCHED_IN);
        _archiveGroup.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 7, 1));
        _archiveGroup.setLayout(new GridLayout(1, false));
        
        _archives = new Table(_archiveGroup, SWT.SINGLE | SWT.FULL_SELECTION | SWT.BORDER);
        _archives.setLinesVisible(true);
        _archives.setHeaderVisible(false);
        gd = new GridData(GridData.FILL, GridData.FILL, true, true);
        _archives.setLayoutData(gd);
        
        _archiveURL = new TableColumn(_archives, SWT.LEFT);
        _archiveIsPub = new TableColumn(_archives, SWT.RIGHT);

        final Menu archiveMenu = new Menu(_archives);
        _archives.setMenu(archiveMenu);

        MenuItem view = new MenuItem(archiveMenu, SWT.PUSH);
        view.setImage(ImageUtil.ICON_VIEW);
        view.setText(getText("View"));
        view.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { viewArchive(); }
            public void widgetSelected(SelectionEvent selectionEvent) { viewArchive(); }
            private void viewArchive() {
                TableItem items[] = _archives.getSelection();
                for (int i = 0; i < items.length; i++) {
                    SyndieURI uri = (SyndieURI)_archiveItemToURI.get(items[i]);
                    if (uri != null) {
                        if (uri.isArchive())
                            _navControl.view(uri);
                        else
                            _navControl.view(SyndieURI.createArchive(uri.getURL(), null));
                    }
                }
            }
        });
        
        ChannelInfo info = _client.getChannel(_scopeId);
        loadData();
        
        if (info != null) {
            if (info.getAllowPublicPosts())
                _auth = 1;
            else if (info.getAllowPublicReplies())
                _auth = 2;
            else
                _auth = 3;
        }
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
        
    }
    
    ChannelInfo getChannelInfo() { return _client.getChannel(_scopeId); }
    
    public void dispose() {
        ImageUtil.dispose(_avatarImgOrig);
        for (int i = 0; i < _avatarImgStandard.size(); i++)
            ImageUtil.dispose((Image)_avatarImgStandard.get(i));
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
    }
    
    /* called when the tab is closed */
    public boolean confirmClose() { return true; }
    
    private void initDnD() {
        initDnDBan();
        initDnDRefs();
    }
    
    private void initDnDBan() {
        Transfer transfer[] = new Transfer[] { TextTransfer.getInstance() };
        int ops = DND.DROP_COPY;
        DragSource source = new DragSource(_banList, ops);
        source.setTransfer(transfer);
        source.addDragListener(new DragSourceListener() {
            public void dragFinished(DragSourceEvent evt) {}
            public void dragSetData(DragSourceEvent evt) {
                Hash scope = null;
                int sel[] = _banList.getSelectionIndices();
                
                if ( (sel != null) && (sel.length == 1) )
                    scope = (Hash)_banScopes.get(sel[0]);
                
                if (scope == null) {
                    // maybe we should find a way to transfer trees instead of just individual bookmarks?
                    evt.doit = false;
                    _ui.debugMessage("dragSetData to null");
                    return;
                }

                BookmarkDnD src = null;
                src = new BookmarkDnD();
                src.desc = "";
                String name = scope.toBase64().substring(0,6);
                src.name = name;
                src.uri = SyndieURI.createScope(scope);
                
                evt.data = src.toString();
                evt.doit = true;
            }
            public void dragStart(DragSourceEvent evt) { evt.doit = _banList.getSelectionCount() > 0; }
        });
    }
    private void initDnDRefs() {
        Transfer transfer[] = new Transfer[] { TextTransfer.getInstance() };
        int ops = DND.DROP_COPY;
        DragSource source = new DragSource(_refTree, ops);
        source.setTransfer(transfer);
        source.addDragListener(new DragSourceListener() {
            public void dragFinished(DragSourceEvent evt) {}
            public void dragSetData(DragSourceEvent evt) {
                ReferenceNode node = null;
                TreeItem sel[] = _refTree.getSelection();
                
                if ( (sel != null) && (sel.length == 1) )
                    node = (ReferenceNode)_refItemToNode.get(sel[0]);
                
                if (node == null) {
                    // maybe we should find a way to transfer trees instead of just individual bookmarks?
                    evt.doit = false;
                    _ui.debugMessage("dragSetData to null");
                    return;
                }

                BookmarkDnD src = null;
                src = new BookmarkDnD();
                src.desc = node.getDescription();
                String name = node.getName();
                src.name = name;
                src.uri = node.getURI();
                
                evt.data = src.toString();
                evt.doit = true;
            }
            public void dragStart(DragSourceEvent evt) { 
                ReferenceNode node = null;
                TreeItem sel[] = _refTree.getSelection();
                
                if ( (sel != null) && (sel.length == 1) )
                    node = (ReferenceNode)_refItemToNode.get(sel[0]);
                
                if ( (node != null) && (node.getURI() != null) )
                    evt.doit = true;
                else
                    evt.doit = false;
            }
        });
    }
    
    private void loadOrigAvatar() {
        byte avatar[] = _client.getChannelAvatar(_scopeId);
        if (avatar != null) {
            Image img = ImageUtil.createImage(avatar);
            if (img != null) {
                Rectangle rect = img.getBounds();
                int width = rect.width;
                int height = rect.height;
                boolean mod = false;
                if (width > Constants.MAX_AVATAR_WIDTH) {
                    width = Constants.MAX_AVATAR_WIDTH;
                    mod = true;
                }
                if (height > Constants.MAX_AVATAR_HEIGHT) {
                    height = Constants.MAX_AVATAR_HEIGHT;
                    mod = true;
                }
                if (mod)
                    img = ImageUtil.resize(img, width, height, true);
                _avatarImgOrig = img;
            } else {
                _avatarImgOrig = null;
            }
        }
    }
    
    private void pickAvatar(Image img) {
        Image old = _avatarImg;
        if (!_avatarImgStandard.contains(old) && (old != _avatarImgOrig) ) {
            ImageUtil.dispose(old);
        }
        _avatarImg = img;
        _avatar.setImage(img);
        _avatar.setVisible(true);
        _avatar.redraw();
        _avatar.getParent().layout(new Control[] { _avatar });
    }
    
    private void loadData() { loadData(_client.getChannel(_scopeId)); }
    private void loadData(ChannelInfo info) {
        if (info != null) {
            _description.setText(str(info.getDescription()));
            
            if (info.getExpiration() > 0)
                _expiration.setText(DateTime.getDate(info.getExpiration()));
            
            _name.setText(str(info.getName()));
            
            loadReferences(info);
            
            StringBuilder buf = new StringBuilder();
            for (Iterator iter = info.getPublicTags().iterator(); iter.hasNext(); )
                buf.append((String)iter.next()).append(" ");
            for (Iterator iter = info.getPrivateTags().iterator(); iter.hasNext(); )
                buf.append((String)iter.next()).append(" ");
            _tags.setText(buf.toString());
            
            if (_avatarImgOrig != null)
                pickAvatar(_avatarImgOrig);
            
            loadArchives(info);
            loadUsers(info);
            _root.layout(true, true);
        }
        _initialized = true;
    }
    private static final String str(String orig) { return (orig != null ? orig : ""); }
    private void loadReferences(ChannelInfo info) {
        final ArrayList refs = new ArrayList(info.getReferences());
        final ArrayList banned = new ArrayList();
        final ArrayList unbannedRoots = new ArrayList();
        ReferenceNode.walk(refs, new ReferenceNode.Visitor() {
            public void visit(ReferenceNode node, int depth, int siblingOrder) {
                String type = node.getReferenceType();
                if ( (type != null) && (Constants.REF_TYPE_BANNED.equals(type)) ) {
                    SyndieURI uri = node.getURI();
                    if (uri != null) {
                        if (uri.isSearch()) {
                            Hash scopes[] = uri.getSearchScopes();
                            if (scopes != null) {
                                for (int i = 0; i < scopes.length; i++) {
                                    if (!banned.contains(scopes[i]))
                                        banned.add(scopes[i]);
                                }
                            }
                        } else if (uri.isChannel()) {
                            Hash scope = uri.getScope();
                            if ( (scope != null) && (!banned.contains(scope)) )
                                banned.add(scope);
                        }
                    }
                } else if (depth == 0) {
                    unbannedRoots.add(node);
                }
            }
        });
        _banScopes.clear();
        _banScopes.addAll(banned);
        _refRoots.addAll(unbannedRoots);
        redrawRefRow();
    }
    private void redrawRefRow() {
        redrawBanned();
        redrawReferences();
        boolean hide = (_banScopes.size() <= 0) && (_refRoots.size() <= 0);
        ((GridData)_refGroup.getParent().getLayoutData()).exclude = hide;
        _refGroup.getParent().setVisible(!hide);
        _root.layout(true, true);
    }
    private void redrawBanned() {
        _banList.setRedraw(false);
        _banList.removeAll();
        for (int i = 0; i < _banScopes.size(); i++) {
            Hash scope = (Hash)_banScopes.get(i);
            String name = _client.getChannelName(scope);
            if (name != null)
                _banList.add(name + " [" + scope.toBase64() + "]");
            else
                _banList.add(scope.toBase64());
        }
        _banList.setRedraw(true);
    }
    private void addRef(ReferenceNode node, TreeItem parent) {
        TreeItem item = null;
        if (parent == null)
            item = new TreeItem(_refTree, SWT.NONE);
        else
            item = new TreeItem(parent, SWT.NONE);
        if (node.getName() != null)
            item.setText(node.getName());
        else if (node.getDescription() != null)
            item.setText(node.getDescription());
        else if (node.getURI() != null)
            item.setText(node.getURI().toString());
        else
            item.setText("");
        
        if (node.getURI() != null)
            item.setImage(ImageUtil.getTypeIcon(node.getURI()));
        
        _refItemToNode.put(item, node);
        
        for (int i = 0; i < node.getChildCount(); i++)
            addRef(node.getChild(i), item);
    }
    private void redrawReferences() {
        _refTree.setRedraw(false);
        for (int i = 0; i < _refRoots.size(); i++) {
            ReferenceNode node = (ReferenceNode)_refRoots.get(i);
            addRef(node, null);
        }
        _refTree.setRedraw(true);
        /*
        _references.setRedraw(false);
        _references.removeAll();
        // place a depth first walk of the references into _references
        final List dfsNodes = new ArrayList();
        ReferenceNode.Visitor walker = new ReferenceNode.Visitor() {
            public void visit(ReferenceNode node, int depth, int siblingOrder) {
                dfsNodes.add(node);
                ReferenceNode parent = node.getParent();
                String parentString = null;
                if (parent != null)
                    parentString = _references.getItem(dfsNodes.indexOf(parent));
                String name = node.getName();
                String desc = node.getDescription();
                SyndieURI uri = node.getURI();
                StringBuilder buf = new StringBuilder();
                if (parentString != null) 
                    buf.append(parentString).append(" > ");
                if (name != null)
                    buf.append(name);
                else if (desc != null)
                    buf.append(desc);
                else if (uri != null)
                    buf.append(uri.toString());
                _references.add(buf.toString());
            }
        };
        ReferenceNode.walk(_referenceNodeRoots, walker);
        _referenceNodes = dfsNodes;
        if (_referenceNodes.size() <= 0) {
            ((GridData)_references.getLayoutData()).exclude = true;
            ((GridData)_referencesLabel.getLayoutData()).exclude = true;
            //((GridData)_referencesAdd.getParent().getLayoutData()).exclude = true;
            //_referencesAdd.getParent().setVisible(false);
            _references.setVisible(false);
            _referencesLabel.setVisible(false);
        }
        _references.setRedraw(true);
        */
    }
    private void loadArchives(ChannelInfo info) {
        // add buttons w/ menus for the archives in _archiveGroup
        _privArchiveURIs.clear();
        for (Iterator iter = info.getPrivateArchives().iterator(); iter.hasNext(); ) {
            ArchiveInfo archive = (ArchiveInfo)iter.next();
            _privArchiveURIs.add(archive.getURI());
        }
        _pubArchiveURIs.clear();
        for (Iterator iter = info.getPublicArchives().iterator(); iter.hasNext(); ) {
            ArchiveInfo archive = (ArchiveInfo)iter.next();
            _pubArchiveURIs.add(archive.getURI());
        }
        
        redrawArchives();
    }
    private void redrawArchives() {
        _archives.setRedraw(false);
        _archives.removeAll();
        _archiveItemToURI.clear();
        
        ArrayList all = new ArrayList();
        all.addAll(_pubArchiveURIs);
        all.addAll(_privArchiveURIs);
        _ui.debugMessage("redrawArchives: all=" + all.size() + " pub=" + _pubArchiveURIs.size() + " priv=" + _privArchiveURIs.size());
        for (int i = 0; i < all.size(); i++) {
            final SyndieURI uri = (SyndieURI)all.get(i);
            if (uri == null) continue;
            String url = uri.getURL();
            if ( (url == null) || (url.trim().length() <= 0) ) continue;
            TableItem item = new TableItem(_archives, SWT.NONE);
            item.setText(0, url.trim());
            if (_pubArchiveURIs.contains(uri))
                item.setText(1, getText("Public"));
            else
                item.setText(1, getText("Authorized readers only"));
            
            _archiveItemToURI.put(item, uri);
        }
        
        if (all.size() <= 0) {
            _archiveGroup.setVisible(false);
            ((GridData)_archiveGroup.getLayoutData()).exclude = true;
        }
        
        _archiveURL.pack();
        _archiveIsPub.pack();
    
        _archives.setRedraw(true);
    }

    
    private void loadUsers(ChannelInfo info) {
        // add buttons w/ menus for the authorized managers and posters in _userGroup
        _managerHashes.clear();
        _posterHashes.clear();
        _managerHashes.addAll(info.getAuthorizedManagerHashes());
        _posterHashes.addAll(info.getAuthorizedPosterHashes());
        
        redrawUsers();
    }
    
    private void redrawUsers() {
        _users.setRedraw(false);
        _users.removeAll();
        _userItemToHash.clear();
        
        ArrayList all = new ArrayList();
        all.addAll(_managerHashes);
        all.addAll(_posterHashes);
        
        for (int i = 0; i < all.size(); i++) {
            final Hash scope = (Hash)all.get(i);
            String name = _client.getChannelName(scope);
            
            TableItem item = new TableItem(_users, SWT.NONE);
            if (name != null)
                item.setText(0, scope.toBase64().substring(0,6) + ": " + name);
            else
                item.setText(0, scope.toBase64().substring(0,6));
            
            if (_managerHashes.contains(scope))
                item.setText(1, getText("Manager"));
            else
                item.setText(1, getText("Authorized poster"));
            
            _userItemToHash.put(item, scope);
        }
        
        if (all.size() <= 0) {
            _userGroup.setVisible(false);
            ((GridData)_userGroup.getLayoutData()).exclude = true;
        }
        
        _userName.pack();
        _userPriv.pack();
    
        _users.setRedraw(true);
    }
    private void toggleUserManagement(Hash scope) {
        if (_managerHashes.contains(scope)) {
            _managerHashes.remove(scope);
            if (!_posterHashes.contains(scope))
                _posterHashes.add(scope);
        } else {
            _posterHashes.remove(scope);
            if (!_managerHashes.contains(scope))
                _managerHashes.add(scope);
        }
        redrawUsers();
        _root.layout(true, true);
    }
    
    public void applyTheme(Theme theme) {
        _nameLabel.setFont(theme.DEFAULT_FONT);
        _name.setFont(theme.DEFAULT_FONT);
        _tagsLabel.setFont(theme.DEFAULT_FONT);
        _tags.setFont(theme.DEFAULT_FONT);
        _descriptionLabel.setFont(theme.DEFAULT_FONT);
        _description.setFont(theme.DEFAULT_FONT);
        _expirationLabel.setFont(theme.DEFAULT_FONT);
        _expiration.setFont(theme.DEFAULT_FONT);
        _expirationManager.setFont(theme.BUTTON_FONT);
        _archiveGroup.setFont(theme.DEFAULT_FONT);
        _archives.setFont(theme.TABLE_FONT);
        
        _userGroup.setFont(theme.DEFAULT_FONT);
        _users.setFont(theme.TABLE_FONT);
        _authorizationLabel.setFont(theme.DEFAULT_FONT);
        _authorization.setFont(theme.DEFAULT_FONT);
        
        _banGroup.setFont(theme.DEFAULT_FONT);
        _banList.setFont(theme.DEFAULT_FONT);
        _refGroup.setFont(theme.DEFAULT_FONT);
        _refTree.setFont(theme.TREE_FONT);
        
        redrawArchives();
        redrawUsers();

        _root.pack(true);
    }

    

    
    

    public void translate(TranslationRegistry registry) {
        _nameLabel.setText(registry.getText("Name") + ':');
        _tagsLabel.setText(registry.getText("Tags") + ':');
        _descriptionLabel.setText(registry.getText("Description") + ':');
        _expirationLabel.setText(registry.getText("Expiration") + ':');
        _expirationManager.setText(registry.getText("Manage expiration"));
        _archiveGroup.setText(registry.getText("Advertised archives") + ':');
        _userGroup.setText(registry.getText("Authorized managers and posters") + ':');
        _banGroup.setText(registry.getText("Banned forums/authors") + ':');
        _refGroup.setText(registry.getText("Advertised references") + ':');
        _authorizationLabel.setText(registry.getText("Authorization") + ':');

        _banView.setText(registry.getText("View the selected forum"));
        _banImport.setText(registry.getText("Ban the selected forum"));
        
        // order correlates w/ AUTH_*
        if (_auth == 1)
            _authorization.setText(registry.getText("Allow anyone to post to the forum"));
        else if (_auth == 2)
            _authorization.setText(registry.getText("Allow anyone to reply to authorized posts"));
        else if (_auth == 3)
            _authorization.setText(registry.getText("Only allow authorized posters to post"));
    }
    
    private static final int AUTH_PUBLIC = 0;
    private static final int AUTH_PUBREPLY = 1;
    private static final int AUTH_AUTH = 2;
}
