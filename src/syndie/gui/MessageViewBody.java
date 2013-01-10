package syndie.gui;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
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
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import syndie.Constants;
import syndie.data.MessageInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.Timer;
import syndie.db.CommandImpl;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.ThreadBuilder;
import syndie.db.ThreadMsgId;
import syndie.db.ThreadReferenceNode;
import syndie.db.UI;

public class MessageViewBody extends BaseComponent implements Themeable, Translatable {
    private Composite _root;
    private NavigationControl _navControl;
    private URIControl _uriControl;
    private BookmarkControl _bookmarkControl;
    private BanControl _banControl;
    private DataCallback _dataCallback;
    
    /** the tabFolder exists if there are multiple pages, refs, attachments, or threads */
    private CTabFolder _tabFolder;
    /** the tabs exist only if there are multiple pages, refs, attachments, or threads */
    private CTabItem _tabs[];
    /** the tabRoots are the composites for each tab */
    private Composite _tabRoots[];
    private PageRenderer _body[];
    
    private MessageTree _threadTree;
    private List _threadTreeMsgs;
    private ThreadMsgId _threadTreeMsgId;
    private int _threadTreeSize;
    private ManageReferenceChooser _refTree;
    private AttachmentPreview _attachmentPreviews[];
    
    private MessageInfo _msg;
    private int _page;

    private List _threadLoadedListeners;
    
    private MaxView _maxView;
    private DetailState _viewState;
    
    private ThreadReferenceNode _messageThread;
    
    private boolean _hideThreadTab;
    private boolean _hideAttachmentTabs;
    
    public interface ThreadLoadedListener {
        public void threadLoaded(List threadReferenceNodes, ThreadMsgId curMsg, int threadSize);
    }
    
    public MessageViewBody(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl navControl, URIControl uriControl, BookmarkControl bookmarkControl, BanControl ban, Composite root, DataCallback dataCallback) {
        super(client, ui, themes, trans);
        _dataCallback = dataCallback;
        _navControl = navControl;
        _uriControl = uriControl;
        _bookmarkControl = bookmarkControl;
        _banControl = ban;
        _root = root;
        _hideThreadTab = false;
        _hideAttachmentTabs = false;
        _threadLoadedListeners = new ArrayList();
        _viewState = new DetailState();
        initComponents();
    }
    
    public void addThreadLoadedListener(ThreadLoadedListener lsnr) { _threadLoadedListeners.add(lsnr); }
    public void removeThreadLoadedListener(ThreadLoadedListener lsnr) { _threadLoadedListeners.remove(lsnr); }
    
    private void initComponents() {
        _tabFolder = new CTabFolder(_root, SWT.BORDER | SWT.MULTI);
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    private class DetailState {
        public boolean disposed;
        public DetailState() { disposed = false; }
    }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        disposeDetails();
        if (_maxView != null)
            _maxView.dispose();
    }
    private void disposeDetails() {
        _viewState.disposed = true;
        //if (!_root.isDisposed()) _root.setRedraw(false);
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
    
    private static final boolean DEFERRED_ATTACHMENT_PREVIEW = true;
    
    public void viewMessage(final MessageInfo msg, int startPage, Timer timer) { viewMessage(msg, startPage, timer, null); }
    public void viewMessage(final MessageInfo msg, int startPage, Timer timer, ThreadReferenceNode messageThread) {
        _messageThread = messageThread;
        _root.setVisible(false);
        disposeDetails();
        _viewState = new DetailState();
        if (msg == null) {
            _root.setVisible(true);
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
        int tabs = pageCount;
        if (!_hideAttachmentTabs) tabs += attachments;
        if ( (refs != null) && (refs.size() > 0) ) tabs++;
        if ( (threadSize > 1) && (!_hideThreadTab) ) tabs++;

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
            
            String cfg = _client.getMessagePageConfig(_msg.getInternalId(), i);
            Properties props = new Properties();
            CommandImpl.parseProps(cfg, props);
            String title = props.getProperty(Constants.MSG_PAGE_TITLE, "");
            if ( (title != null) && (title.trim().length() > 0) ) 
                _tabs[i].setText(title);
            else
                _tabs[i].setText(_translationRegistry.getText("Page ") + (i+1));
            
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
        if ( (threadSize > 1) && (!_hideThreadTab) ) {
            timer.addEvent("initBody building the thread subtab");
            _tabs[off] = new CTabItem(_tabFolder, SWT.NONE);
            _tabRoots[off] = new Composite(_tabFolder, SWT.NONE);
            _tabs[off].setControl(_tabRoots[off]);
            _tabs[off].setText(_translationRegistry.getText("Thread"));
            _tabRoots[off].setLayout(new FillLayout());

            // no preview on the thread tree

            // deferred thread display
            final Composite root = _tabRoots[off];
            final CTabItem threadItem = _tabs[off];
            //final int toff = off;
            _tabFolder.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    if (_viewState.disposed) return;
                    if (_tabFolder.getSelection() == threadItem) {
                        _ui.debugMessage("tab folder: thread tab selected");
                        if (_threadTree == null) {
                            _ui.debugMessage("tab folder: creating a new thread tree");
                            _threadTree = ComponentBuilder.instance().createMessageTree(root, new MessageTree.MessageTreeListener() {
                                    public void messageSelected(MessageTree tree, SyndieURI uri, boolean toView, boolean nodelay) {
                                        if (toView)
                                            _navControl.view(uri);
                                    }
                                    public void filterApplied(MessageTree tree, SyndieURI searchURI) {}
                            }, true);
                            _threadTree.setFilterable(false); // no sorting/refiltering/etc.  just a thread tree
                            if (_threadTreeMsgs != null) {
                                _ui.debugMessage("tab folder: messages already fetched");
                                _threadTree.setMessages(_threadTreeMsgs);
                                _threadTree.expandAll();
                                _threadTree.select(_msg.getURI());
                                root.layout(new Control[] { _threadTree.getControl() });
                            } else {
                                _ui.debugMessage("tab folder: messages not already fetched");
                                JobRunner.instance().enqueue(new Runnable() {
                                    public void run() {
                                        asyncLoadThread(msg, root);
                                    }
                                });
                            }
                        } else {
                            _ui.debugMessage("thread tree is not null, noop");
                        }
                    }
                }
            });
            
            off++;
        }
        if ( (refs != null) && (refs.size() > 0) ) {
            _tabs[off] = new CTabItem(_tabFolder, SWT.NONE);
            _tabRoots[off] = new Composite(_tabFolder, SWT.NONE);
            _tabs[off].setControl(_tabRoots[off]);
            _tabs[off].setText(_translationRegistry.getText("References"));
            _tabRoots[off].setLayout(new FillLayout());
            _refTree = ComponentBuilder.instance().createManageReferenceChooser(_tabRoots[off], false);
            _refTree.setReferences(refs);
            off++;
        }

        if (!_hideAttachmentTabs) {
            if (attachments > 0)
                _attachmentPreviews = new AttachmentPreview[attachments];

            Hash scope = msg.getScopeChannel();
            long messageId = msg.getMessageId();

            for (int i = 0; i < attachments; i++) {
                _tabs[off+i] = new CTabItem(_tabFolder, SWT.NONE);
                _tabRoots[off+i] = new Composite(_tabFolder, SWT.NONE);
                _tabs[off+i].setControl(_tabRoots[off+i]);
                _tabs[off+i].setText(_translationRegistry.getText("Attachment ") + (i+1));
                _tabRoots[off+i].setLayout(new FillLayout());
    
                final SyndieURI uri = SyndieURI.createAttachment(scope, messageId, i+1);
                timer.addEvent("initBody attachment preview tab created");
                _attachmentPreviews[i] = new AttachmentPreview(_client, _ui, _themeRegistry, _translationRegistry, _tabRoots[off+i]);
                timer.addEvent("initBody attachment preview instantiated");
    
                final int preview = i;
                final CTabItem item = _tabs[off];
                final AttachmentPreview attachPreview = _attachmentPreviews[i];
                if (DEFERRED_ATTACHMENT_PREVIEW) {
                    _tabFolder.addSelectionListener(new FireSelectionListener() {
                        public void fire() {
                            if (_viewState.disposed) return;
                            if (_tabFolder.getSelection() == item) {
                                attachPreview.showURI(new URIAttachmentSource(uri), uri);
                            }
                        }
                    });
                } else {
                    _attachmentPreviews[preview].showURI(new URIAttachmentSource(uri), uri);
                }
            }
        }
        timer.addEvent("initBody go to configured");
        
        if (_tabFolder != null) {
            if (_page > 0) {
                if (_page > _tabs.length)
                    _page = _tabs.length;
                _tabFolder.setSelection(_tabs[_page-1]);
            } else {
                _tabFolder.setSelection(_tabs[0]);
            }
        }
        
        //_root.layout(true, true);
        _root.getDisplay().timerExec(100, new Runnable() {
            public void run() {
                if (_root.isDisposed()) return;
                if (_threadTreeMsgs == null) {
                    JobRunner.instance().enqueue(new Runnable() {
                        public void run() {
                            asyncLoadThread(msg, null);
                        }
                    });
                }
            }
        });
        timer.addEvent("initBody root laid out");
        _root.setVisible(true);
    }
    
    public void hideThreadTab() { _hideThreadTab = true; }
    public void hideAttachmentTabs() { _hideAttachmentTabs = true; }
    
    private class URIAttachmentSource implements AttachmentPreview.AttachmentSource {
        private SyndieURI _attachURI;
        public URIAttachmentSource(SyndieURI uri) { _attachURI = uri; }

        public Properties getAttachmentConfig(int attachmentNum) {
            long msgId = _client.getMessageId(_attachURI.getScope(), _attachURI.getMessageId());
            return _client.getMessageAttachmentConfig(msgId, attachmentNum);
        }

        public byte[] getAttachmentData(int attachmentNum) {
            long msgId = _client.getMessageId(_attachURI.getScope(), _attachURI.getMessageId());
            return _client.getMessageAttachmentData(msgId, attachmentNum);
        }
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
    

    private void asyncLoadThread(MessageInfo msg, final Composite parent) {
        if (_viewState.disposed) return;
        _ui.debugMessage("tab folder: populate thread tab");
        ThreadBuilder builder = new ThreadBuilder(_client, _ui);
        final List msgs = new ArrayList(1);
        final ThreadMsgId id = new ThreadMsgId(msg.getInternalId());
        id.messageId = msg.getMessageId();
        id.scope = msg.getScopeChannel();
        id.authorScopeId = msg.getAuthorChannelId();
        //timer.addEvent("initBody thread build prepared");
        if (_messageThread != null)
            msgs.add(_messageThread);
        else
            msgs.add(builder.buildThread(id));
        //timer.addEvent("initBody thread built");
        final int threadSize = countMessages(msgs);
        //timer.addEvent("initBody thread size counted");
        //_browser.getUI().debugMessage("thread for " + _uri + ":\n" + msgs);

        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                if (!_root.isDisposed())
                    syncLoadThread(msgs, id, threadSize, parent);
            }
        });
    }
    private void syncLoadThread(List msgs, ThreadMsgId id, int threadSize, Composite parent) {
        _threadTreeMsgs = msgs;
        _threadTreeMsgId = id;
        _threadTreeSize = threadSize;
    
        if (_threadTree != null) {
            _ui.debugMessage("sync load thread: tree exists, so populate it");
            _threadTree.setMessages(_threadTreeMsgs);
            //timer.addEvent("initBody thread tree messages set");
            _threadTree.expandAll();
            _threadTree.select(_msg.getURI());
            if (parent != null)
                parent.layout(new Control[] { _threadTree.getControl() });
        }
        
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
        public void viewScopeMessages(PageRenderer renderer, Hash scope) { 
            _ui.debugMessage("viewScopeMessages");
            _navControl.view(SyndieURI.createScope(scope)); 
        }
        public void viewScopeMetadata(PageRenderer renderer, Hash scope) { _navControl.view(_uriControl.createManageURI(scope)); }
        public void view(PageRenderer renderer, SyndieURI uri) {
            SyndieURI rel = SyndieURI.resolveRelative(_msg.getURI(), uri);
            _navControl.view(rel);
        }
        public void bookmark(PageRenderer renderer, SyndieURI uri) { _bookmarkControl.bookmark(uri); }
        public void banScope(PageRenderer renderer, Hash scope) {
            if (_banControl.ban(scope)) {
                _navControl.unview(_msg.getURI());
                _dataCallback.readStatusUpdated();
            }
        }
        public void cancelMessage(PageRenderer renderer, SyndieURI msg) {
            if (msg != null) {
                if (_banControl.cancelMessage(msg)) { //_client.cancelMessage(msg, _ui);
                    _navControl.unview(_msg.getURI());
                    _dataCallback.readStatusUpdated();
                }
            }
        }
        public void deleteMessage(PageRenderer renderer, SyndieURI msg) {
            if (msg != null) {
                if (_banControl.deleteMessage(msg)) { //_client.deleteMessage(msg, _ui, true);
                    _navControl.unview(_msg.getURI());
                    _dataCallback.readStatusUpdated();
                }
            }
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
