package syndie.gui;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import net.i2p.data.Hash;
import net.i2p.data.PrivateKey;
import net.i2p.data.SessionKey;
import net.i2p.data.SigningPrivateKey;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import syndie.data.MessageInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.Timer;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.ThreadBuilder;
import syndie.db.ThreadMsgId;
import syndie.db.UI;

public class MessageViewBody extends BaseComponent implements Themeable, Translatable {
    private Composite _root;
    private NavigationControl _navControl;
    private URIControl _uriControl;
    private BookmarkControl _bookmarkControl;
    private BanControl _banControl;
    
    /** the tabFolder exists if there are multiple pages, refs, attachments, or threads */
    private CTabFolder _tabFolder;
    /** the tabs exist only if there are multiple pages, refs, attachments, or threads */
    private CTabItem _tabs[];
    /** the tabRoots are the composites for each tab */
    private Composite _tabRoots[];
    private PageRenderer _body[];
    
    private MessageTree _threadTree;
    private ManageReferenceChooser _refTree;
    private AttachmentPreview _attachmentPreviews[];
    
    private MessageInfo _msg;
    private int _page;

    private List _threadLoadedListeners;
    
    private MaxView _maxView;
    
    public interface ThreadLoadedListener {
        public void threadLoaded(List threadReferenceNodes, ThreadMsgId curMsg, int threadSize);
    }
    
    public MessageViewBody(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl navControl, URIControl uriControl, BookmarkControl bookmarkControl, BanControl ban, Composite root) {
        super(client, ui, themes, trans);
        _navControl = navControl;
        _uriControl = uriControl;
        _bookmarkControl = bookmarkControl;
        _banControl = ban;
        _root = root;
        _threadLoadedListeners = new ArrayList();
        initComponents();
    }
    
    public void addThreadLoadedListener(ThreadLoadedListener lsnr) { _threadLoadedListeners.add(lsnr); }
    public void removeThreadLoadedListener(ThreadLoadedListener lsnr) { _threadLoadedListeners.remove(lsnr); }
    
    private void initComponents() {
        _tabFolder = new CTabFolder(_root, SWT.BORDER | SWT.MULTI);
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        disposeDetails();
        if (_maxView != null)
            _maxView.dispose();
    }
    private void disposeDetails() {
        if (_tabRoots != null) {
            for (int i = 0; i < _tabRoots.length; i++)
                _tabRoots[i].dispose();
            _tabRoots = null;
        }
        if (_tabs != null) {
            for (int i = 0; i < _tabs.length; i++)
                _tabs[i].dispose();
            _tabs = null;
        }
        if (_attachmentPreviews != null) {
            for (int i = 0; i < _attachmentPreviews.length; i++)
                _attachmentPreviews[i].dispose();
            _attachmentPreviews = null;
        }
        if (_body != null) {
            for (int i = 0; i < _body.length; i++)
                _body[i].dispose();
            _body = null;
        }
        if (_threadTree != null) {
            _threadTree.dispose();
            _threadTree = null;
        }
        if (_refTree != null) {
            _refTree.dispose();
            _refTree = null;
        }
    }

    public void switchPage(int page) { _tabFolder.setSelection(_tabs[page-1]); }
    public void switchAttachment(int attachment) {
        if (_tabFolder != null) {
            int attachments = _msg.getAttachmentCount();
            int tabs = _tabs.length;
            // attachment tabs are at the end
            int tab = tabs - attachments + attachment - 1;
            if ( (tab >= 0) && (tab < tabs) )
                _tabFolder.setSelection(_tabs[tab]);
        }
    }
    
    private static final boolean DEFERRED_ATTACHMENT_PREVIEW = false;
    
    public void viewMessage(final MessageInfo msg, int startPage, Timer timer) {
        _root.setRedraw(false);
        disposeDetails();
        if (msg == null) {
            _root.setRedraw(true);
            return;
        }
        _msg = msg;
        _page = startPage;
        timer.addEvent("initBody");
        int pageCount = msg.getPageCount();
        List refs = msg.getReferences();
        timer.addEvent("initBody pages and references loaded");

        int threadSize = 2; // assume a reply so we build the tabs

        int attachments = msg.getAttachmentCount();
        int tabs = pageCount + attachments;
        if ( (refs != null) && (refs.size() > 0) ) tabs++;
        if (threadSize > 1) tabs++;

        _ui.debugMessage("tabs: " + tabs + " pages: " + pageCount + " attach: " + attachments + " refs? " + (refs != null ? refs.size() : 0) + " threadSize: " + threadSize);
        _tabs = new CTabItem[tabs];
        _tabRoots = new Composite[tabs];
        _body = new PageRenderer[pageCount];
        _tabFolder.setFont(_themeRegistry.getTheme().TAB_FONT);
        PageListener lsnr = new PageListener();
        for (int i = 0; i < pageCount; i++) {
            _tabs[i] = new CTabItem(_tabFolder, SWT.NONE);
            _tabRoots[i] = new Composite(_tabFolder, SWT.NONE);
            _tabs[i].setControl(_tabRoots[i]);
            _tabs[i].setText(_translationRegistry.getText(T_PAGE_PREFIX, "Page ") + (i+1));
            _tabRoots[i].setLayout(new FillLayout());
            _body[i] = ComponentBuilder.instance().createPageRenderer(_tabRoots[i], true);
            timer.addEvent("initBody n-page renderer constructed");
            _body[i].setListener(lsnr);
            SyndieURI uri = SyndieURI.createMessage(msg.getScopeChannel(), msg.getMessageId(), i+1);
            _body[i].renderPage(new PageRendererSource(_client, _themeRegistry), uri);
            timer.addEvent("initBody n-page renderer constructed");
            //_body[i].addKeyListener(new MaxViewListener(uri));
        }
        int off = pageCount;
        if (threadSize > 1) {
            timer.addEvent("initBody building the thread subtab");
            _tabs[off] = new CTabItem(_tabFolder, SWT.NONE);
            _tabRoots[off] = new Composite(_tabFolder, SWT.NONE);
            _tabs[off].setControl(_tabRoots[off]);
            _tabs[off].setText(_translationRegistry.getText(T_TAB_THREAD, "Thread"));
            _tabRoots[off].setLayout(new FillLayout());

            // no preview on the thread tree
            _threadTree = ComponentBuilder.instance().createMessageTree(_tabRoots[off], new MessageTree.MessageTreeListener() {
                    public void messageSelected(MessageTree tree, SyndieURI uri, boolean toView, boolean nodelay) {
                        if (toView)
                            _navControl.view(uri);
                    }
                    public void filterApplied(MessageTree tree, SyndieURI searchURI) {}
            }, true);
            //_threadTree.setMessages(msgs);
            //_threadTree.select(_uri);
            //_threadTree.setFilterable(false); // no sorting/refiltering/etc.  just a thread tree

            // deferred thread display
            final int toff = off;
            _tabFolder.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    _ui.debugMessage("tab folder selection fired");
                    if (_tabFolder.getSelection() == _tabs[toff]) {
                        _ui.debugMessage("tab folder: thread tab selected");
                        if ( (_threadTree.getMessages() == null) || (_threadTree.getMessages().size() == 0) ) {
                            loadThread(msg);
                        } else {
                            _ui.debugMessage("tab folder: thread tab already populated");
                        }
                    }
                }
            });
            _threadTree.setFilterable(false); // no sorting/refiltering/etc.  just a thread tree

            off++;
        }
        if ( (refs != null) && (refs.size() > 0) ) {
            _tabs[off] = new CTabItem(_tabFolder, SWT.NONE);
            _tabRoots[off] = new Composite(_tabFolder, SWT.NONE);
            _tabs[off].setControl(_tabRoots[off]);
            _tabs[off].setText(_translationRegistry.getText(T_TAB_REFS, "References"));
            _tabRoots[off].setLayout(new FillLayout());
            _refTree = ComponentBuilder.instance().createManageReferenceChooser(_tabRoots[off], false);
            _refTree.setReferences(refs);
            off++;
        }
        if (attachments > 0)
            _attachmentPreviews = new AttachmentPreview[attachments];

        Hash scope = msg.getScopeChannel();
        long messageId = msg.getMessageId();

        for (int i = 0; i < attachments; i++) {
            _tabs[off+i] = new CTabItem(_tabFolder, SWT.NONE);
            _tabRoots[off+i] = new Composite(_tabFolder, SWT.NONE);
            _tabs[off+i].setControl(_tabRoots[off+i]);
            _tabs[off+i].setText(_translationRegistry.getText(T_ATTACH_PREFIX, "Attachment ") + (i+1));
            _tabRoots[off+i].setLayout(new FillLayout());

            final SyndieURI uri = SyndieURI.createAttachment(scope, messageId, i+1);
            timer.addEvent("initBody attachment preview tab created");
            _attachmentPreviews[i] = new AttachmentPreview(_client, _ui, _themeRegistry, _translationRegistry, _tabRoots[off+i]);
            timer.addEvent("initBody attachment preview instantiated");

            final int toff = off;
            final int preview = i;
            if (DEFERRED_ATTACHMENT_PREVIEW) {
                _tabFolder.addSelectionListener(new FireSelectionListener() {
                    public void fire() {
                        if (_tabFolder.getSelection() == _tabs[toff]) {
                            _attachmentPreviews[preview].showURI(uri);
                        }
                    }
                });
            } else {
                _attachmentPreviews[preview].showURI(uri);
            }
        }
        timer.addEvent("initBody go to configured");
        
        if (_tabFolder != null) {
            if (_page > 0) {
                _tabFolder.setSelection(_tabs[_page-1]);
            } else {
                _tabFolder.setSelection(_tabs[0]);
            }
        }
        
        //_root.layout(true, true);
        _root.getDisplay().timerExec(500, new Runnable() {
            public void run() {
                if (_root.isDisposed()) return;
                if ( (_threadTree.getMessages() == null) || (_threadTree.getMessages().size() == 0) )
                    loadThread(msg);
            }
        });
        timer.addEvent("initBody root laid out");
        _root.setRedraw(true);
    }
    
    public void toggleMaxView() {
        synchronized (this) {
            if (_maxView != null) {
                _maxView.dispose();
                _maxView = null;
            } else {
                int tab = 0;
                int tabs = 0;
                if (_tabFolder != null) {
                    tab = _tabFolder.getSelectionIndex();
                    tabs = _tabs.length;
                }
                int pages = (_body == null ? 1 : _body.length);
                int attachments = (_attachmentPreviews == null ? 0 : _attachmentPreviews.length);
                if (tab < pages) {
                    SyndieURI uri = SyndieURI.createMessage(_msg.getScopeChannel(), _msg.getMessageId(), tab+1);
                    _maxView = new MaxView(_client, _ui, _themeRegistry, _translationRegistry, _root.getShell(), uri, new MaxView.MaxListener() {
                        public void unmax(MaxView view) {
                            synchronized (MessageViewBody.this) {
                                _maxView = null;
                            }
                            view.dispose();
                        }
                    });
                } else if (tab >= tabs - attachments) {
                    int attachStart = tabs - attachments;
                    _attachmentPreviews[tab-attachStart].maximize();
                    //_browser.getUI().debugMessage("no pages?");
                }
            }
        }
    }
    
    private static final String T_PAGE_PREFIX = "syndie.gui.messageview.pageprefix";
    private static final String T_ATTACH_PREFIX = "syndie.gui.messageview.attachprefix";
    private static final String T_TAB_THREAD = "syndie.gui.messageview.tabthread";
    private static final String T_TAB_REFS = "syndie.gui.messageview.tabrefs";

    private void loadThread(MessageInfo msg) {
        _ui.debugMessage("tab folder: populate thread tab");
        ThreadBuilder builder = new ThreadBuilder(_client, _ui);
        List msgs = new ArrayList(1);
        ThreadMsgId id = new ThreadMsgId(msg.getInternalId());
        id.messageId = msg.getMessageId();
        id.scope = msg.getScopeChannel();
        id.authorScopeId = msg.getAuthorChannelId();
        //timer.addEvent("initBody thread build prepared");
        msgs.add(builder.buildThread(id));
        //timer.addEvent("initBody thread built");
        int threadSize = countMessages(msgs);
        //timer.addEvent("initBody thread size counted");
        //_browser.getUI().debugMessage("thread for " + _uri + ":\n" + msgs);

        _threadTree.setMessages(msgs);
        //timer.addEvent("initBody thread tree messages set");
        _threadTree.expandAll();
        _threadTree.select(_msg.getURI());
    
        for (int i = 0; i < _threadLoadedListeners.size(); i++)
            ((ThreadLoadedListener)_threadLoadedListeners.get(i)).threadLoaded(msgs, id, threadSize);
    }
    
    private int countMessages(List nodes) {
        NodeCounter counter = new NodeCounter();
        ReferenceNode.walk(nodes, counter);
        return counter.getCount();
    }
    
    private static class NodeCounter implements ReferenceNode.Visitor {
        private int _count = 0;
        public void visit(ReferenceNode node, int depth, int siblingOrder) {
            _count++;
        }
        public int getCount() { return _count; }
    }

    public void translate(TranslationRegistry registry) {}
    public void applyTheme(Theme theme) {
        _tabFolder.setFont(theme.TAB_FONT);
    }
        
    private class PageListener implements PageRenderer.PageActionListener {
        public void viewScopeMessages(PageRenderer renderer, Hash scope) { _navControl.view(SyndieURI.createScope(scope)); }
        public void viewScopeMetadata(PageRenderer renderer, Hash scope) { _navControl.view(_uriControl.createManageURI(scope)); }
        public void view(PageRenderer renderer, SyndieURI uri) { _navControl.view(SyndieURI.resolveRelative(_msg.getURI(), uri)); }
        public void bookmark(PageRenderer renderer, SyndieURI uri) { _bookmarkControl.bookmark(uri); }
        public void banScope(PageRenderer renderer, Hash scope) {
            if (_banControl.ban(scope))
                _navControl.unview(_msg.getURI());
        }
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
        public void prevPage() {
            if (_tabFolder != null) {
                int idx = _tabFolder.getSelectionIndex();
                if (idx > 0) {
                    _tabFolder.setSelection(idx-1);
                    if (idx-1 < _body.length)
                        _body[idx-1].getComposite().forceFocus();
                }
            }
        }
        public void nextPage() {
            if (_tabFolder != null) {
                int idx = _tabFolder.getSelectionIndex();
                if (idx + 1 < _tabs.length) {
                    _tabFolder.setSelection(idx+1);
                    if (idx+1 < _body.length)
                        _body[idx+1].getComposite().forceFocus();
                }
            }
        }
    }
    
}
