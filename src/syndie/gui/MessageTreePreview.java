package syndie.gui;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Map;
import java.util.TreeSet;
import net.i2p.data.Hash;
import net.i2p.data.PrivateKey;
import net.i2p.data.SessionKey;
import net.i2p.data.SigningPrivateKey;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.MenuListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
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
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 *
 */
public class MessageTreePreview extends BaseComponent implements Themeable, Translatable {
    private NavigationControl _navControl;
    private BookmarkControl _bookmarkControl;
    private URIControl _uriControl;
    private MessageTree _tree;
    private Shell _shell;
    
    //private Composite _header;
    private Label _headerSubject;
    private Button _headerClose;
    
    private PageRenderer _body;
    private Button _view;
    private SyndieURI _uri;
    private int _page;
    private Hash _author;
    private Hash _target;
    
    public MessageTreePreview(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl navControl, BookmarkControl bookmarkControl, URIControl uriControl, MessageTree tree) {
        super(client, ui, themes, trans);
        _navControl = navControl;
        _bookmarkControl = bookmarkControl;
        _uriControl = uriControl;
        _tree = tree;
        initComponents();
    }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        if ( (_shell != null) && (!_shell.isDisposed()) )
            _shell.dispose();
        if (_body != null)
            _body.dispose();
    }
    
    private long _msgId;
    private SyndieURI _msgURI;
    
    private boolean updatePreview() {
        if ( (_uri == null) || (_uri.getScope() == null) )
            return false;
        if ( (_shell == null) || (_shell.isDisposed()) ) 
            initComponents(); // in case hide() == dispose()
        long chanId = _client.getChannelId(_uri.getScope());
        MessageInfo msg = _client.getMessage(chanId, _uri.getMessageId());
        if (msg != null) {
            _msgId = msg.getInternalId();
            _msgURI = msg.getURI();
            String subj = MessageView.calculateSubject(_client, _translationRegistry, msg);
            _headerSubject.setText(subj);
            _target = msg.getTargetChannel();
            _author = _client.getChannelHash(msg.getAuthorChannelId());
            _body.renderPage(new PageRendererSource(_client, _themeRegistry), _uri);
            _shell.layout(true);
            if (MessageTree.shouldMarkReadOnPreview(_client))
                _client.markMessageRead(_msgId);
            return true;
        } else {
            _msgId = -1;
            _msgURI = null;
            _target = null;
            _author = null;
            return false;
        }
    }
    
    private void hide() { 
        _shell.setVisible(false);
        _tree.focusOnMessages();
        //dispose();
    }
    
    private void initComponents() {
        _shell = new Shell(_tree.getControl().getShell(), SWT.NO_TRIM);
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) {
                evt.doit = false;
                hide();
            }
            public void shellDeactivated(ShellEvent evt) {
                evt.doit = false;
                hide();
            }
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        _shell.setLayout(gl);
        
        _headerClose = new Button(_shell, SWT.PUSH);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, false, false);
        _headerClose.setLayoutData(gd);
        _headerClose.setText("X");
        _headerClose.addSelectionListener(new FireSelectionListener() { public void fire() { hide(); } });
        _headerClose.addKeyListener(new KeyListener() {
            public void keyPressed(KeyEvent evt) {
                if ( (evt.keyCode == SWT.ARROW_DOWN) || (evt.keyCode == SWT.ARROW_RIGHT) ) {
                    hide();
                    //_tree.getTree().forceFocus();
                    //_tree.selectNext();
                } else if ( (evt.keyCode == SWT.ARROW_UP) || (evt.keyCode == SWT.ARROW_LEFT) ) {
                    hide();
                    //_tree.getTree().forceFocus();
                    //_tree.selectPrev();
                }
            }
            public void keyReleased(KeyEvent keyEvent) {}
        });
        
        _headerSubject = new Label(_shell, SWT.BORDER | SWT.SINGLE | SWT.WRAP);
        gd = new GridData(GridData.FILL, GridData.FILL, true, false);
        _headerSubject.setLayoutData(gd);
        _headerSubject.setText("");
        _headerSubject.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) {
                hide();
                _navControl.view(_uri);
            }
            public void mouseDown(MouseEvent mouseEvent) {}
            public void mouseUp(MouseEvent mouseEvent) {}
        });
        
        _body = ComponentBuilder.instance().createPageRenderer(_shell, true);
        _body.getComposite().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
        _body.setListener(new PageRenderer.PageActionListener() {
            public void viewScopeMessages(PageRenderer renderer, Hash scope) { _navControl.view(SyndieURI.createScope(scope)); }
            public void viewScopeMetadata(PageRenderer renderer, Hash scope) { _navControl.view(_uriControl.createManageURI(scope)); }
            public void view(PageRenderer renderer, SyndieURI uri) { _navControl.view(SyndieURI.resolveRelative(_uri, uri)); }
            public void bookmark(PageRenderer renderer, SyndieURI uri) { _bookmarkControl.bookmark(uri); }
            public void banScope(PageRenderer renderer, Hash scope) {}
            public void deleteMessage(PageRenderer renderer, SyndieURI msg) {}
            public void viewImage(PageRenderer renderer, Image img) {}
            public void ignoreImageScope(PageRenderer renderer, Hash scope) {}
            public void importReadKey(PageRenderer renderer, Hash referencedBy, Hash keyScope, SessionKey key) {}
            public void importPostKey(PageRenderer renderer, Hash referencedBy, Hash keyScope, SigningPrivateKey key) {}
            public void importManageKey(PageRenderer renderer, Hash referencedBy, Hash keyScope, SigningPrivateKey key) {}
            public void importReplyKey(PageRenderer renderer, Hash referencedBy, Hash keyScope, PrivateKey key) {}
            public void importArchiveKey(PageRenderer renderer, Hash referencedBy, SyndieURI archiveURI, SessionKey key) {}
            public void saveAllImages(PageRenderer renderer, Map images) {}
            public void saveImage(PageRenderer renderer, String suggestedName, Image img) {}
            public void privateReply(PageRenderer renderer, Hash author, SyndieURI msg) { _navControl.view(_uriControl.createPostURI(author, msg, true)); }
            public void replyToForum(PageRenderer renderer, Hash forum, SyndieURI msg) { _navControl.view(_uriControl.createPostURI(forum, msg)); }
            public void nextPage() {}
            public void prevPage() {}
        });
        
        _view = new Button(_shell, SWT.PUSH);
        _view.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        _view.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                hide();
                _navControl.view(_uri);
            }
        });
        
        PopupListener lsnr = new PopupListener();
        _tree.getTree().addMouseMoveListener(lsnr);
        _tree.getTree().addSelectionListener(lsnr);
        _ui.debugMessage("popup listener registered");
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    private static final String T_VIEW = "syndie.gui.messagetreepreview.view";
    public void translate(TranslationRegistry registry) {
        _view.setText(registry.getText(T_VIEW, "View"));
    }
    public void applyTheme(Theme theme) {
        _headerSubject.setFont(theme.DEFAULT_FONT);
        _headerClose.setFont(theme.BUTTON_FONT);
        _view.setFont(theme.BUTTON_FONT);
    }

    private static final int PREVIEW_DELAY = 1000;
    
    private class PopupListener implements MouseMoveListener, SelectionListener, MouseListener {
        private long _lastMove;
        private boolean _scheduled;
        private boolean _lastEventWasMouse;
        public PopupListener() {
            _lastMove = System.currentTimeMillis();
            _scheduled = false;
            _lastEventWasMouse = false;
        }
        public void mouseMove(MouseEvent evt) {
            _lastEventWasMouse = true;
            fireEvent();
        }
        
        private void fireEvent() {
            _lastMove = System.currentTimeMillis();
            boolean schedule = false;
            synchronized (PopupListener.this) {
                if (!_scheduled)
                    schedule = true;
                _scheduled = true;
            }
            if (schedule)
                Display.getDefault().timerExec(PREVIEW_DELAY+10, new Runnable() { public void run() { popup(); } });
        }
        private void popup() {
            if (Display.getDefault().getCursorControl() != _tree.getTree()) {
                synchronized (PopupListener.this) {
                    _scheduled = false;
                }
                return; // we left the tree's area
            }
            int delay = (int)(System.currentTimeMillis() - _lastMove);
            if (delay >= PREVIEW_DELAY) {
                synchronized (PopupListener.this) {
                    _scheduled = false;
                }
                showPreview();
            } else {
                Display.getDefault().timerExec(PREVIEW_DELAY-delay, new Runnable() { public void run() { popup(); } });
            }
        }
        private void showPreview() {
            if (!MessageTree.shouldShowPreview(_client))
                return;
            if (_lastEventWasMouse)
                _uri = _tree.getMouseoverURI();
            else
                _uri = _tree.getSelected();
            _ui.debugMessage("showPreview: " + _uri);
            boolean ok = updatePreview();
            if (ok) {
                Point loc = null;
                if (_lastEventWasMouse)
                    loc = Display.getDefault().getCursorLocation();
                else
                    loc = _tree.getSelectedLocation();

                int width = (_tree.getTree().getClientArea().width - loc.x)/1;
                if (width < 300)
                    width = 300;
                _shell.setBounds(loc.x+10, loc.y, width, 200);
                _shell.open();
            }
        }

        public void widgetSelected(SelectionEvent selectionEvent) { sel(); }
        public void widgetDefaultSelected(SelectionEvent selectionEvent) { sel(); }
        private void sel() {
            _lastEventWasMouse = false;
            fireEvent();
        }

        public void mouseDoubleClick(MouseEvent mouseEvent) { _lastMove = System.currentTimeMillis(); }
        public void mouseDown(MouseEvent mouseEvent) { _lastMove = System.currentTimeMillis(); }
        public void mouseUp(MouseEvent mouseEvent) { _lastMove = System.currentTimeMillis(); }
    }
}
