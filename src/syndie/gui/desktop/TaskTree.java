package syndie.gui.desktop;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.SyndieURI;
import syndie.db.JobRunner;
import syndie.gui.*;

public class TaskTree extends BaseComponent implements Themeable, Translatable {
    private Desktop _desktop;
    private Composite _parent;
    
    private Composite _root;
    private Tree _tree;
    private Button _cancel;
    private Button _exit;
    private List _listeners;
    
    private Map _itemToPanel;
    private Map _itemToImage;
    
    public interface TaskTreeListener {
        public void cancelSelected();
        public void exitSelected();
        public void viewSelected(DesktopPanel panel);
        public void closeSelected(DesktopPanel panel);
        public void closeSelected(List panels);
    }
    
    public TaskTree(Desktop desktop, Composite parent) {
        super(desktop.getDBClient(), desktop.getUI(), desktop.getThemeRegistry(), desktop.getTranslationRegistry());
        _desktop = desktop;
        _parent = parent;
        _itemToPanel = new HashMap();
        _itemToImage = new HashMap();
        _listeners = new ArrayList();
        initComponents();
    }
    
    public void addListener(TaskTreeListener lsnr) { _listeners.add(lsnr); }
    public void removeListener(TaskTreeListener lsnr) { _listeners.remove(lsnr); }
    public Control getRoot() { return _root; }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        _root.setLayout(gl);
        
        _cancel = new Button(_root, SWT.PUSH);
        GridData gd = new GridData(GridData.FILL, GridData.FILL, false, true);
        gd.widthHint = 32;
        _cancel.setLayoutData(gd);
        _cancel.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                for (int i = 0; i < _listeners.size(); i++)
                    ((TaskTreeListener)_listeners.get(i)).cancelSelected();
            }
        });
        
        _tree = new Tree(_root, SWT.SINGLE | SWT.BORDER | SWT.FULL_SELECTION | SWT.V_SCROLL);
        _tree.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _tree.setHeaderVisible(false);
        
        SyndieTreeListener lsnr = new SyndieTreeListener(_tree) {
            public void doubleclick() { switchToSelected(); }
            public void returnHit() { switchToSelected(); }
        };
        _tree.addKeyListener(lsnr);
        _tree.addSelectionListener(lsnr);
        _tree.addMouseListener(lsnr);
        _tree.addTraverseListener(lsnr);
        
        _exit = new Button(_root, SWT.PUSH);
        gd = new GridData(GridData.FILL, GridData.FILL, false, true);
        gd.widthHint = 32;
        _exit.setLayoutData(gd);
        _exit.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                for (int i = 0; i < _listeners.size(); i++)
                    ((TaskTreeListener)_listeners.get(i)).exitSelected();
            }
        });
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        disposeDetails();
    }
    private void disposeDetails() {
        if (!_tree.isDisposed())
            _tree.removeAll();
        
        for (Iterator iter = _itemToImage.values().iterator(); iter.hasNext(); )
            ImageUtil.dispose((Image)iter.next());
        
        _itemToImage.clear();
        _itemToPanel.clear();
    }
    
    public void show() {
        _root.setRedraw(false);
        disposeDetails();
        buildTree(_desktop.getPanels());
        _tree.setFocus();
        _root.layout(true, true);
        _root.setRedraw(true);
    }
    
    private void buildTree(final List panels) {
        JobRunner.instance().enqueue(new Runnable() {
            public void run() {
                final Map forumNameToPanelList = new TreeMap();
                final Map forumNameToImageData = new HashMap();
                final List otherPanelList = new ArrayList();
                final Map panelToImageData = new HashMap();
                buildTreeInfo(panels, forumNameToPanelList, otherPanelList, forumNameToImageData, panelToImageData);
                
                Display.getDefault().asyncExec(new Runnable() {
                    public void run() {
                        renderTreeInfo(forumNameToPanelList, otherPanelList, forumNameToImageData, panelToImageData);
                    }
                });
            }
        });
    }
    
    private void buildTreeInfo(List panels, Map forumNameToPanelList, List otherPanels, Map forumNameToImageData, Map panelToImageData) {
        Map scopeToName = new HashMap();
        for (int i = 0; i < panels.size(); i++) {
            DesktopPanel panel = (DesktopPanel)panels.get(i);
            SyndieURI uri = panel.getOriginalURI();
            Hash scope = null;
            if (uri != null) {
                if (uri.isSearch()) {
                    Hash scopes[] = uri.getSearchScopes();
                    if ( (scopes != null) && (scopes.length == 1) )
                        scope = scopes[0];
                } else if (uri.isChannel()) {
                    if (uri.getMessageId() == null) {
                        scope = uri.getScope();
                    } else {
                        long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
                        long chanId = _client.getMessageTarget(msgId);
                        scope = _client.getChannelHash(chanId);
                    }
                }
                
                if (scope != null) {
                    String name = (String)scopeToName.get(scope);
                    if (name == null) {
                        long chanId = _client.getChannelId(scope);
                        String chan = _client.getChannelName(chanId);
                        byte avatar[] = _client.getChannelAvatar(chanId);
                        String chanName = chan;
                        if (chan == null) chanName = "";
                        name = chanName + " [" + scope.toBase64().substring(0,6) + "]";
                        scopeToName.put(scope, name);
                        forumNameToImageData.put(name, avatar);
                    }
                    
                    List forumPanels = (List)forumNameToPanelList.get(name);
                    if (forumPanels == null) {
                        forumPanels = new ArrayList();
                        forumNameToPanelList.put(name, forumPanels);
                    }
                    if (panel instanceof MessageTreePanel) // put these up first
                        forumPanels.add(0, panel);
                    else
                        forumPanels.add(panel);
                } else {
                    otherPanels.add(panel);
                }
            } else {
                otherPanels.add(panel);
            }
            
            /*
            TreeItem item = new TreeItem(_tree, SWT.NONE);
            item.setImage(ImageUtil.ICON_EDITOR_LINK);
            item.setText(panel.getPanelName());
            _itemToImage.put(item, item.getImage());
            _itemToPanel.put(item, panel);
             */
        }
    }
    
    private void renderTreeInfo(Map forumNameToPanelList, List otherPanels, Map forumNameToImageData, Map panelToImageData) {
        for (Iterator iter = forumNameToPanelList.keySet().iterator(); iter.hasNext(); ) {
            String name = (String)iter.next();
            List panels = (List)forumNameToPanelList.get(name);
            if (panels.size() == 0)
                continue;
            
            byte data[] = (byte[])forumNameToImageData.get(name);
            Image img = null;
            if (data != null)
                img = ImageUtil.createImage(data);
            else
                img = ImageUtil.ICON_TAB_BROWSE;
            
            TreeItem forumItem = new TreeItem(_tree, SWT.NONE);
            forumItem.setImage(img);
            forumItem.setText(name);

            _itemToImage.put(forumItem, img);
            
            DesktopPanel first = (DesktopPanel)panels.get(0);
            _itemToPanel.put(forumItem, first);
            
            for (int i = 1; i < panels.size(); i++) {
                DesktopPanel panel = (DesktopPanel)panels.get(i);
                TreeItem item = new TreeItem(forumItem, SWT.NONE);
                data = (byte[])panelToImageData.get(panel);
                img = null;
                if (data != null)
                    img = ImageUtil.createImage(data);
                else
                    img = ImageUtil.ICON_TAB_MSG;
                item.setImage(img);
                item.setText(panel.getPanelName());

                _itemToImage.put(item, img);
                _itemToPanel.put(item, panel);
            }
            
            
            forumItem.setExpanded(true);
        }
        
        for (int i = 0; i < otherPanels.size(); i++) {
            DesktopPanel panel = (DesktopPanel)otherPanels.get(i);
            TreeItem item = new TreeItem(_tree, SWT.NONE);
            byte[] data = (byte[])panelToImageData.get(panel);
            Image img = null;
            if (data != null)
                img = ImageUtil.createImage(data);
            else
                img = ImageUtil.ICON_TAB_MSG;
            item.setImage(img);
            item.setText(panel.getPanelName());

            _itemToImage.put(item, img);
            _itemToPanel.put(item, panel);
        }
    }
    
    private void switchToSelected() {
        TreeItem items[] = _tree.getSelection();
        if ( (items == null) || (items.length == 1) ) {
            DesktopPanel panel = (DesktopPanel)_itemToPanel.get(items[0]);
            for (int i = 0; i < _listeners.size(); i++)
                ((TaskTreeListener)_listeners.get(i)).viewSelected(panel);
        }
    }
    
    public void applyTheme(Theme theme) {
        _cancel.setFont(theme.BUTTON_FONT);
        _tree.setFont(theme.TREE_FONT);
        _exit.setFont(theme.BUTTON_FONT);
    }
    
    private static final String T_CANCEL = "syndie.gui.tasktree.cancel";
    private static final String T_EXIT = "syndie.gui.tasktree.exit";
    public void translate(TranslationRegistry trans) {
        _cancel.setToolTipText(trans.getText(T_CANCEL, "Cancel the task switch"));
        _exit.setToolTipText(trans.getText(T_EXIT, "Exit Syndie"));
    }
}
