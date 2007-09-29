package syndie.gui.desktop;

import java.util.ArrayList;
import java.util.List;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.NymReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;
import syndie.gui.FireSelectionListener;
import syndie.gui.LinkBuilderPopup;
import syndie.gui.ThemeRegistry;
import syndie.gui.TranslationRegistry;

class AddReferenceSource implements LinkBuilderPopup.LinkBuilderSource {
    private DBClient _client;
    private UI _ui;
    private TranslationRegistry _translationRegistry;
    private ThemeRegistry _themeRegistry;
    private Composite _root;
    private LinkBuilderPopup _popup;
    
    public AddReferenceSource(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Composite root) { 
        _client = client;
        _ui = ui;
        _themeRegistry = themes;
        _translationRegistry = trans;
        _root = root; 
    }
    
    private Composite getRoot() { return _root; }

    private static final String T_REFLOC_SHELL = "syndie.gui.desktop.forumselectionpanel.refloc.shell";
    private static final String T_REFLOC_DESC = "syndie.gui.desktop.forumselectionpanel.refloc.desc";
    private static final String T_REFLOC_OK = "syndie.gui.desktop.forumselectionpanel.refloc.ok";
    private static final String T_REFLOC_NEWFOLDER = "syndie.gui.desktop.forumselectionpanel.refloc.newfolder";
    private static final String T_REFLOC_ROOT = "syndie.gui.desktop.forumselectionpanel.refloc.root";

    public void setPopup(LinkBuilderPopup popup) { _popup = popup; }

    public void uriBuildingCancelled() { if (!_popup.isDisposed()) _popup.dispose(); }
    public void uriBuilt(final SyndieURI uri, final String text) {
        // now see /where/ they want to store the ref
        final Shell shell = new Shell(getRoot().getShell(), SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        shell.setText(_translationRegistry.getText(T_REFLOC_SHELL, "Reference location"));
        shell.setFont(_themeRegistry.getTheme().SHELL_FONT);
        GridLayout gl = new GridLayout(3, false);
        gl.verticalSpacing = 0;
        gl.horizontalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        shell.setLayout(gl);
        shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent shellEvent) {
                if (!_popup.isDisposed()) _popup.dispose();
            }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });

        Label l = new Label(shell, SWT.NONE);
        l.setText(_translationRegistry.getText(T_REFLOC_DESC, "Please specify where you want to store this reference"));
        l.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        l.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, false, false, 3, 1));

        final Tree tree = new Tree(shell, SWT.BORDER | SWT.SINGLE | SWT.FULL_SELECTION);
        tree.setFont(_themeRegistry.getTheme().TREE_FONT);
        tree.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 1));
        tree.setLinesVisible(true);
        tree.setHeaderVisible(false);

        Button ok = new Button(shell, SWT.PUSH);
        ok.setFont(_themeRegistry.getTheme().BUTTON_FONT);
        ok.setText(_translationRegistry.getText(T_REFLOC_OK, "Store"));
        ok.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));
        ok.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                storeRef(uri, text, tree);
                shell.dispose();
                if (!_popup.isDisposed()) _popup.dispose();
            }
        });

        Button newFolder = new Button(shell, SWT.PUSH);
        newFolder.setFont(_themeRegistry.getTheme().BUTTON_FONT);
        newFolder.setText(_translationRegistry.getText(T_REFLOC_NEWFOLDER, "Create folder"));
        newFolder.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false));

        final Text folderName = new Text(shell, SWT.SINGLE | SWT.BORDER);
        folderName.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
        folderName.setText(_translationRegistry.getText(T_REFLOC_NEWFOLDER, "New folder"));
        folderName.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));

        newFolder.addSelectionListener(new FireSelectionListener() {
            public void fire() {
                createRefFolder(tree, folderName.getText());
            }
        });

        populateRefFolderTree(tree);

        shell.pack();
        shell.open();
    }

    public int getPageCount() { return 0; }
    public List getAttachmentDescriptions() { return new ArrayList(); }
    
    private void populateRefFolderTree(Tree tree) {
        TreeItem root = new TreeItem(tree, SWT.NONE);
        root.setText(_translationRegistry.getText(T_REFLOC_ROOT, "Top level"));

        List refs = _client.getNymReferences();
        root.setData("nodes", refs);

        for (int i = 0; i < refs.size(); i++) {
            NymReferenceNode node = (NymReferenceNode)refs.get(i);
            populateRefFolderTree(root, node);
        }
        root.setExpanded(true);
        tree.setSelection(root);
    }
    private void populateRefFolderTree(TreeItem parent, NymReferenceNode node) {
        if (node == null) return;
        int kids = node.getChildCount();
        if (kids > 0) {
            TreeItem cur = new TreeItem(parent, SWT.NONE);
            cur.setText(node.getName());
            cur.setData("node", node);

            for (int i = 0; i < kids; i++)
                populateRefFolderTree(cur, (NymReferenceNode)node.getChild(i));
            cur.setExpanded(true);
        }
        parent.setExpanded(true);
    }

    private void createRefFolder(Tree tree, String name) {
        if ( (name == null) || (name.trim().length() == 0) ) {
            _ui.debugMessage("folder name is blank, aborting new folder");
            return;
        }
        TreeItem sel[] = tree.getSelection();
        if ( (sel == null) || (sel.length != 1) ) {
            _ui.debugMessage("no parent is selected, aborting new folder");
            return;
        }

        NymReferenceNode node = (NymReferenceNode)sel[0].getData("node");
        NymReferenceNode newNode = null;
        if (node == null) {
            List refs = (List)sel[0].getData("nodes");
            if (refs == null) {
                _ui.debugMessage("root has no nodes, aborting new folder");
                return;
            }
            int maxSeq = 0;
            for (int i = 0; i < refs.size(); i++) {
                NymReferenceNode cur = (NymReferenceNode)refs.get(i);
                if (cur.getSiblingOrder() > maxSeq) 
                    maxSeq = cur.getSiblingOrder();
                if (name.equalsIgnoreCase(cur.getName())) {
                    _ui.debugMessage("root already has an entry by that name, aborting new folder");
                    return;
                }
            }

            // ok, add the category at the top level
            newNode = new NymReferenceNode(name, null, null, -1, -1, -1, maxSeq+1, false, false, false);
            _client.addNymReference(_client.getLoggedInNymId(), newNode);
            _ui.debugMessage("new folder created: " + newNode.getGroupId() + ": " + newNode);
        } else {
            int kids = node.getChildCount();
            int maxSeq = 0;
            for (int i = 0; i < kids; i++) {
                NymReferenceNode cur = (NymReferenceNode)node.getChild(i);
                if (cur.getSiblingOrder() > maxSeq) 
                    maxSeq = cur.getSiblingOrder();
                if (name.equalsIgnoreCase(cur.getName())) {
                    _ui.debugMessage("parent already has an entry by that name, aborting new folder");
                    return;
                }
            }

            // ok, add the category at the specified level
            newNode = new NymReferenceNode(name, null, null, -1, -1, node.getGroupId(), maxSeq+1, false, false, false);
            _client.addNymReference(_client.getLoggedInNymId(), newNode);
            _ui.debugMessage("new folder created: " + newNode.getGroupId() + ": " + newNode);
        }

        TreeItem newItem = new TreeItem(sel[0], SWT.NONE);
        newItem.setText(name);
        newItem.setData("node", newNode);
        tree.setSelection(newItem);
    }

    private void storeRef(SyndieURI uri, String title, Tree tree) {
        if (uri == null) {
            _ui.debugMessage("uri is null, not adding");
            return;
        }

        TreeItem sel[] = tree.getSelection();
        if ( (sel == null) || (sel.length != 1) ) {
            _ui.debugMessage("no folder selected, not adding");
            return;
        }

        NymReferenceNode node = (NymReferenceNode)sel[0].getData("node");
        if (node == null) {
            List refs = (List)sel[0].getData("nodes");
            if (refs == null) {
                _ui.debugMessage("root has no nodes, not adding");
                return;
            }
            int maxSeq = 0;
            for (int i = 0; i < refs.size(); i++) {
                NymReferenceNode cur = (NymReferenceNode)refs.get(i);
                if (cur.getSiblingOrder() > maxSeq) 
                    maxSeq = cur.getSiblingOrder();
                if (uri.equals(cur.getURI())) {
                    _ui.debugMessage("root already has that URI, not adding");
                    return;
                }
            }

            // ok, add the ref at the top level
            NymReferenceNode newNode = new NymReferenceNode(title, uri, null, -1, -1, -1, maxSeq+1, false, false, false);
            _client.addNymReference(_client.getLoggedInNymId(), newNode);
            _ui.debugMessage("reference added: " + newNode.getGroupId() + ": " + newNode);
        } else {
            int kids = node.getChildCount();
            int maxSeq = 0;
            for (int i = 0; i < kids; i++) {
                NymReferenceNode cur = (NymReferenceNode)node.getChild(i);
                if (cur.getSiblingOrder() > maxSeq) 
                    maxSeq = cur.getSiblingOrder();
                if (uri.equals(cur.getURI())) {
                    _ui.debugMessage("folder already has that URI, not adding");
                    return;
                }
            }

            // ok, add the ref at the specified level
            NymReferenceNode newNode = new NymReferenceNode(title, uri, null, -1, -1, node.getGroupId(), maxSeq+1, false, false, false);    
            _client.addNymReference(_client.getLoggedInNymId(), newNode);
            _ui.debugMessage("reference added: " + newNode.getGroupId() + ": " + newNode);
        }
    }

}
