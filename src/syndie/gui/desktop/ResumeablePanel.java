package syndie.gui.desktop;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;
import syndie.gui.Browser;
import syndie.gui.FireSelectionListener;
import syndie.gui.ImageUtil;
import syndie.gui.MessageEditor;
import syndie.gui.SyndieTreeListener;
import syndie.gui.Theme;
import syndie.gui.ThemeRegistry;
import syndie.gui.Themeable;
import syndie.gui.Translatable;
import syndie.gui.TranslationRegistry;
import syndie.gui.URIHelper;

/**
 * show the list of previously postponed messages, allowing the user to resume one of
 * them, cancel one or more of them, or to create a brand new message
 */
class ResumeablePanel extends DesktopPanel implements Themeable, Translatable {
    private Button _resumeExisting;
    private Button _abortExisting;
    private Tree _existingTree;
    private TreeColumn _colForum;
    private TreeColumn _colDate;
    private TreeColumn _colSubject;
    
    public ResumeablePanel(Desktop desktop, DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite center, SyndieURI origURI) {
        super(desktop, client, themes, trans, center, ui, origURI);
        initComponents();
    }
    
    private void initComponents() {
        getRoot().setLayout(new FillLayout());
        
        _existingTree = new Tree(getRoot(), SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.FULL_SELECTION);
        _existingTree.setHeaderVisible(true);
        _existingTree.setLinesVisible(true);
        
        SyndieTreeListener lsnr = new SyndieTreeListener(_existingTree) {
            public boolean collapseOnReturn() { return false; }
            public void doubleclick() { resumeSelected(); }
            public void returnHit() { resumeSelected(); }
        };
        _existingTree.addSelectionListener(lsnr);
        _existingTree.addMouseListener(lsnr);
        _existingTree.addTraverseListener(lsnr);
        
        _colForum = new TreeColumn(_existingTree, SWT.LEFT);
        _colDate = new TreeColumn(_existingTree, SWT.LEFT);
        _colSubject = new TreeColumn(_existingTree, SWT.RIGHT);
        
        _colDate.pack();
        _colForum.pack();
        _colSubject.pack();
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);

        populateTree();
    }
    
    protected void buildEast(Composite edge) { 
        if (_edgeEast == null) _edgeEast = new EastEdge(edge, _ui); 
    }
    protected void buildSouth(Composite edge) { 
        if (_edgeSouth == null) _edgeSouth = new SouthEdge(edge, _ui); 
    }
    
    private void resumeSelected() {
        TreeItem sel[] = _existingTree.getSelection();
        boolean viewing = false;
        for (int i = 0; i < sel.length; i++) {
            ResumeableMeta meta = (ResumeableMeta)sel[i].getData("meta");
            if (meta != null) {
                _desktop.getNavControl().view(URIHelper.instance().createPostURI(meta.postponeId, meta.version));
                viewing = true;
            }
        }
        if (viewing)
            close();
    }
    
    private void abortSelected() {
        TreeItem sel[] = _existingTree.getSelection();
        ArrayList toDrop = new ArrayList();
        ArrayList items = new ArrayList();
        for (int i = 0; i < sel.length; i++) {
            ResumeableMeta meta = (ResumeableMeta)sel[i].getData("meta");
            if (meta != null) {
                toDrop.add(meta);
                items.add(sel[i]);
            }
        }
        if (toDrop.size() > 0) {
            MessageBox confirm = new MessageBox(getRoot().getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO);
            if (toDrop.size() == 1) {
                confirm.setMessage(_translationRegistry.getText("Are you sure you want to completely drop the selected message?"));
                confirm.setText(_translationRegistry.getText("Confirm"));
            } else {
                confirm.setMessage(_translationRegistry.getText("Are you sure you want to completely drop the selected messages?"));
                confirm.setText(_translationRegistry.getText("Confirm"));                
            }

            int rc = confirm.open();
            if (rc == SWT.YES) {
                for (int i = 0; i < toDrop.size(); i++) {
                    MessageEditor.dropSavedState(_client, _ui, ((ResumeableMeta)toDrop.get(i)).postponeId);
                    ((TreeItem)items.get(i)).dispose();
                }
            }
        }
    }
    
    
    private void populateTree() {
        TreeMap postponeIdToVersion = _client.getResumeable();
        TreeMap forumToResumeableMeta = new TreeMap();
        
        for (Iterator iter = postponeIdToVersion.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry)iter.next();
            Long postponeId = (Long)entry.getKey();
            Integer version = (Integer)entry.getValue();
            
            ResumeableMeta meta = new ResumeableMeta(postponeId.longValue(), version.intValue());
            if (meta.isValid()) {
                String forumKey = meta.forumName + " - " + meta.forum.toBase64();
                
                TreeMap metaMsgs = (TreeMap)forumToResumeableMeta.get(forumKey);
                if (metaMsgs == null) {
                    metaMsgs = new TreeMap();
                    forumToResumeableMeta.put(forumKey, metaMsgs);
                }
                
                metaMsgs.put(meta.postDate + " - " + meta.postponeId, meta);
            } else {
                _ui.errorMessage("meta is not valid: " + postponeId + "." + version);
            }
        }
        
        int maxSubjectWidth = 0;
        int maxForumWidth = 0;
        
        int forums = forumToResumeableMeta.size();
        for (Iterator iter = forumToResumeableMeta.values().iterator(); iter.hasNext(); ) {
            TreeMap metaMap = (TreeMap)iter.next();
            TreeItem forumItem = new TreeItem(_existingTree, SWT.NONE);
            boolean forumSet = false;
            for (Iterator metaIter = metaMap.values().iterator(); metaIter.hasNext(); ) {
                ResumeableMeta meta = (ResumeableMeta)metaIter.next();
                if (!forumSet) {
                    forumItem.setText(0, meta.forumName);
                    int width = ImageUtil.getWidth(meta.forumName, _existingTree);
                    if (width > maxForumWidth)
                        maxForumWidth = width;
                    forumSet = true;
                }
                TreeItem msgItem = new TreeItem(forumItem, SWT.NONE);
                msgItem.setText(1, meta.postDate);
                msgItem.setText(2, meta.subject);
                int width = ImageUtil.getWidth(meta.subject, _existingTree);
                if (width > maxSubjectWidth)
                    maxSubjectWidth = width;
                msgItem.setData("meta", meta);
                msgItem.setExpanded(true);
            }
            forumItem.setExpanded(true);
        }
        
        int totalWidth = getRoot().getParent().getClientArea().width;
        int dateWidth = ImageUtil.getWidth("YYYY/MM/dd HH:mm:ss", _existingTree) + 16;
        int subjectWidth = maxSubjectWidth;
        int forumWidth = maxForumWidth + 32; // for the expandable arrow
        
        if (forumWidth > totalWidth / 3)
            forumWidth = totalWidth / 3;
            
        _colForum.setWidth(forumWidth);
        _colDate.setWidth(dateWidth);
        int maxSubject = totalWidth - forumWidth - dateWidth - 3*_existingTree.getBorderWidth();
        if (subjectWidth> maxSubject)
            _colSubject.setWidth(maxSubject);
        else
            _colSubject.setWidth(subjectWidth);
    }
    
    private class ResumeableMeta {
        long postponeId;
        int version;
        String postDate;
        Hash forum;
        String forumName;
        String subject;
        
        public ResumeableMeta(long id, int ver) {
            MessageEditor.MessageSummary summary = MessageEditor.loadSummary(_client, _ui, _translationRegistry, id, ver);
            if (summary != null) {
                postponeId = id;
                version = ver;
                postDate = Browser.getVersionTime(id);
                forum = summary.forum;
                forumName = _client.getChannelName(forum);
                subject = summary.subject;
            }
        }
        
        public boolean isValid() { return forum != null; }
    }
    
    public String getPanelName() { return _translationRegistry.getText("Resume"); }
    public String getPanelDescription() { return _translationRegistry.getText("Resume postponed messages"); }
    
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        super.dispose();
    }

    public void applyTheme(Theme theme) {
        _existingTree.setFont(theme.TREE_FONT);
    }

    
    public void translate(TranslationRegistry registry) {
        _colForum.setText(registry.getText("Forum"));
        _colDate.setText(registry.getText("Date"));
        _colSubject.setText(registry.getText("Subject"));
    }
    
    private class EastEdge extends DesktopEdge implements Themeable, Translatable {
        private Button _resumeExisting;
        private Button _abortExisting;
        
        public EastEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        private void initComponents() {
            Composite root = getEdgeRoot();
            root.setLayout(new FillLayout(SWT.VERTICAL));
            
            _resumeExisting = new Button(root, SWT.PUSH);
            _resumeExisting.addSelectionListener(new FireSelectionListener() {
                public void fire() { resumeSelected(); }
            });
            _resumeExisting.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _resumeExisting, _themeRegistry.getTheme().BUTTON_FONT, _translationRegistry.getText("Resume"));
                }
            });

            _abortExisting = new Button(root, SWT.PUSH);
            _abortExisting.addSelectionListener(new FireSelectionListener() {
                public void fire() { abortSelected(); }
            });
            _abortExisting.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent evt) {
                    ImageUtil.drawDescending(evt.gc, _abortExisting, _themeRegistry.getTheme().BUTTON_FONT, _translationRegistry.getText("Abort"));
                }
            });

            _translationRegistry.register(EastEdge.this);
            _themeRegistry.register(EastEdge.this);
        }
        
        public void dispose() {
            _translationRegistry.unregister(EastEdge.this);
            _themeRegistry.unregister(EastEdge.this);
            super.dispose();
        }
        
        public void translate(TranslationRegistry registry) {
            _resumeExisting.redraw();
            _abortExisting.redraw();
        }
        public void applyTheme(Theme theme) {
            _resumeExisting.setFont(theme.BUTTON_FONT);
            _abortExisting.setFont(theme.BUTTON_FONT);
        }
    }
    
    private class SouthEdge extends DesktopEdge implements Themeable, Translatable {
        private Button _createNew;
        private Button _cancel;
        
        public SouthEdge(Composite edge, UI ui) {
            super(edge, ui);
            initComponents();
        }
        private void initComponents() {
            Composite root = getEdgeRoot();
            root.setLayout(new FillLayout(SWT.HORIZONTAL));
            _createNew = new Button(root, SWT.PUSH);
            _createNew.addSelectionListener(new FireSelectionListener() {
                public void fire() {
                    _desktop.getNavControl().view(URIHelper.instance().createPostURI(null, null));
                    close();
                }
            });

            _cancel = new Button(root, SWT.PUSH);
            _cancel.addSelectionListener(new FireSelectionListener() {
                public void fire() { close(); }
            });

            _translationRegistry.register(SouthEdge.this);
            _themeRegistry.register(SouthEdge.this);
        }
        
        public void dispose() {
            _translationRegistry.unregister(SouthEdge.this);
            _themeRegistry.unregister(SouthEdge.this);
            super.dispose();
        }
        
        public void translate(TranslationRegistry registry) {
            _createNew.setText(registry.getText("Create a new message"));
            _cancel.setText(registry.getText("Don't create a message at this time"));
        }
        public void applyTheme(Theme theme) {
            _createNew.setFont(theme.BUTTON_FONT);
            _cancel.setFont(theme.BUTTON_FONT);
        }
    }
}
