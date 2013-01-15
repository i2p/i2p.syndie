package syndie.gui;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import net.i2p.data.Base64;
import net.i2p.data.Hash;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.TreeEditor;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.DropTargetListener;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.ScrollBar;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;

import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.NymReferenceNode;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.WatchedChannel;
import syndie.db.DBClient;
import syndie.db.UI;

class ManageForumBans extends BaseComponent implements Themeable, Translatable {
    private final ManageForum _manage;
    private Shell _shell;
    private SashForm _sash;
    private RefTree _refTree;
    private Group _banGroup;
    private List _localBans;
    private final ArrayList<Hash> _localBanHashes;
    private List _targetBans;
    private final ArrayList<Hash> _targetBanHashes;
    private Button _ok;
    private Button _cancel;
    
    public ManageForumBans(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, ManageForum manage) {
        super(client, ui, themes, trans);
        _manage = manage;
        _localBanHashes = new ArrayList();
        _targetBanHashes = new ArrayList();
        initComponents();
    }
    
    private void initComponents() {
        _shell = new Shell(_manage.getRoot().getShell(), SWT.SHELL_TRIM | SWT.PRIMARY_MODAL);
        _shell.setLayout(new GridLayout(1, true));
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) {
                evt.doit = false;
                dispose();
            }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        _sash = new SashForm(_shell, SWT.HORIZONTAL);
        _sash.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        Composite lhs = new Composite(_sash, SWT.NONE);
        lhs.setLayout(new FillLayout(SWT.VERTICAL));
        _banGroup = new Group(lhs, SWT.SHADOW_ETCHED_IN);
        _banGroup.setLayout(new FillLayout());
        _localBans = new List(_banGroup, SWT.BORDER | SWT.MULTI);
        _refTree = ComponentBuilder.instance().createRefTree(lhs);
        _targetBans = new List(_sash, SWT.BORDER | SWT.MULTI);
        
        Composite actions = new Composite(_shell, SWT.NONE);
        actions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        _ok = new Button(actions, SWT.PUSH);
        _ok.addSelectionListener(new FireSelectionListener() {
            public void fire() { save(); }
        });
        _cancel = new Button(actions, SWT.PUSH);
        _cancel.addSelectionListener(new FireSelectionListener() {
            public void fire() { dispose(); }
        });
        
        initDnDBanListSrc();
        initDnDTarget();
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
        
        loadData();
        
        _shell.open();
    }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        _refTree.dispose();
        if (!_shell.isDisposed())
            _shell.dispose();
    }
    
    private void loadData() {
        _targetBanHashes.clear();
        ArrayList scopes = _manage.getBanned();
        if (scopes != null)
            _targetBanHashes.addAll(scopes);
        redrawTarget();
        
        _localBans.setRedraw(false);
        _localBanHashes.clear();
        ArrayList<Hash> banned = _client.getBannedChannels();
        for (int i = 0; i < banned.size(); i++) {
            _localBanHashes.add(banned.get(i));
            _localBans.add(((Hash)banned.get(i)).toBase64());
        }
        _localBans.setRedraw(true);
    }
    
    private void save() {
        _manage.setBanned(_targetBanHashes);
        dispose();
    }
    
    private void redrawTarget() {
        _targetBans.setRedraw(false);
        _targetBans.removeAll();
        for (int i = 0; i < _targetBanHashes.size(); i++)
            _targetBans.add(((Hash)_targetBanHashes.get(i)).toBase64());
        _targetBans.setRedraw(true);
    }
    
    private void initDnDTarget() {
        int ops = DND.DROP_COPY | DND.DROP_LINK;
        Transfer transfer[] = new Transfer[] { TextTransfer.getInstance() };
        DropTarget target = new DropTarget(_targetBans, ops);
        target.setTransfer(transfer);
        target.addDropListener(new DropTargetListener() {
            public void dragEnter(DropTargetEvent evt) {
                evt.detail = DND.DROP_COPY;
                evt.feedback = DND.FEEDBACK_SELECT | DND.FEEDBACK_EXPAND | DND.FEEDBACK_SCROLL;
            }
            public void dragLeave(DropTargetEvent evt) {}
            public void dragOperationChanged(DropTargetEvent evt) {}
            public void dragOver(DropTargetEvent evt) {
                evt.detail = DND.DROP_COPY;
                evt.feedback = DND.FEEDBACK_SELECT | DND.FEEDBACK_EXPAND | DND.FEEDBACK_SCROLL;
            }
            public void drop(DropTargetEvent evt) {
                if (evt.data == null) {
                    evt.detail = DND.DROP_NONE;
                    return;
                }
                
                _ui.debugMessage("drop: " + evt);
                
                ArrayList scopes = getToAdd(evt.data.toString());
                for (int i = 0; i < scopes.size(); i++) {
                    Hash scope = (Hash)scopes.get(i);
                    if (!_targetBanHashes.contains(scope))
                        _targetBanHashes.add(scope);
                }
                redrawTarget();
            }
            public void dropAccept(DropTargetEvent evt) {}
        });
    }
    
    private ArrayList getToAdd(String data) {
        if (_isDragging) {
            // from the ban list
            ArrayList rv = new ArrayList();
            int indexes[] = _localBans.getSelectionIndices();
            for (int i = 0; i < indexes.length; i++) {
                rv.add((Hash)_localBanHashes.get(indexes[i]));
            }
            return rv;
        }
        ReferenceNode rv = _refTree.getDragged();
        if (rv == null) {
            BookmarkDnD bookmark = null;
            SyndieURI uri = null;

            bookmark = new BookmarkDnD();
            bookmark.fromString(data);
            if (bookmark.uri == null) { // parse fail
                String str = data;
                try {
                    uri = new SyndieURI(str);
                } catch (URISyntaxException use) {
                    _ui.debugMessage("invalid uri: " + str, use);
                    byte val[] = Base64.decode(str);
                    if ( (val != null) && (val.length == Hash.HASH_LENGTH) ) {
                        uri = SyndieURI.createScope(Hash.create(val));
                    }
                }
            }
            if ( (uri == null) && (bookmark == null) ) {
                rv = null;
            } else if (bookmark != null) {
                rv = new ReferenceNode(bookmark.name, bookmark.uri, bookmark.desc, null);
            } else {
                rv = new ReferenceNode(System.currentTimeMillis()+"", uri, "", null);
            }
        }

        Scopes scopes = new Scopes();
        ArrayList pre = new ArrayList();
        pre.add(rv);
        ReferenceNode.walk(pre, scopes);
        return scopes.getScopes();
    }
    
    private class Scopes implements ReferenceNode.Visitor {
        private ArrayList _scopes;
        public Scopes() { _scopes = new ArrayList(); }
        public ArrayList getScopes() { return _scopes; }
        public void visit(ReferenceNode node, int depth, int siblingOrder) {
            SyndieURI uri = node.getURI();
            if (uri == null) return;
            if (uri.isChannel()) {
                if (uri.getMessageId() != null)
                    return;
                Hash scope = uri.getScope();
                if ((scope != null) && (!_scopes.contains(scope)))
                    _scopes.add(scope);
            } else if (uri.isSearch()) {
                Hash scopes[] = uri.getSearchScopes();
                if (scopes != null) {
                    for (int i = 0; i < scopes.length; i++) {
                        if (!_scopes.contains(scopes[i]))
                            _scopes.add(scopes[i]);
                    }
                }
            }
        }
    }
    
    private boolean _isDragging;
    private void initDnDBanListSrc() {
        _isDragging = false;
        Transfer transfer[] = new Transfer[] { TextTransfer.getInstance() };
        int ops = DND.DROP_COPY;
        DragSource source = new DragSource(_localBans, ops);
        source.setTransfer(transfer);
        source.addDragListener(new DragSourceListener() {
            public void dragFinished(DragSourceEvent evt) {
                _isDragging = false;
            }
            public void dragSetData(DragSourceEvent evt) {
                int indexes[] = _localBans.getSelectionIndices();
                StringBuilder buf = new StringBuilder();
                for (int i = 0; i < indexes.length; i++) {
                    Hash scope = (Hash)_localBanHashes.get(indexes[i]);
                    buf.append(scope.toBase64()).append("\n");
                }
                evt.data = buf.toString();
                evt.doit = true;
                return;
            }
            public void dragStart(DragSourceEvent evt) {
                evt.doit = _localBans.getSelectionCount() > 0;
                _isDragging = true;
            }
        });
    }
    
    public void applyTheme(Theme theme) {
        _banGroup.setFont(theme.DEFAULT_FONT);
        _localBans.setFont(theme.DEFAULT_FONT);
        _targetBans.setFont(theme.DEFAULT_FONT);
        _ok.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
    }
    
    
    public void translate(TranslationRegistry registry) {
        _banGroup.setText(registry.getText("Locally banned scopes"));
        _shell.setText(registry.getText("Banned scopes"));
        _ok.setText(registry.getText("OK"));
        _cancel.setText(registry.getText("Cancel"));
    }
}
