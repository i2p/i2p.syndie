package syndie.gui;

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
import org.eclipse.swt.widgets.Shell;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

/**
 *
 */
public class BrowseForum implements MessageTree.MessageTreeListener {
    private DBClient _client;
    private Composite _parent;
    private SashForm _root;
    private MessageTree _tree;
    private MessageTree.MessageTreeListener _listener;
    private MessagePreview _preview;
    
    public BrowseForum(Composite parent, DBClient client, MessageTree.MessageTreeListener lsnr) {
        _client = client;
        _parent = parent;
        _listener = lsnr;
        initComponents();
    }
    
    public Control getControl() { return _root; }
    
    private void initComponents() {
        _root = new SashForm(_parent, SWT.VERTICAL);
        _root.SASH_WIDTH = 3;
        _root.setBackground(ColorUtil.getColor("gray", null));
        
        _tree = new MessageTree(_client, _root, this);
        _preview = new MessagePreview(_client, _root);
        _root.setWeights(new int[] { 80, 20 });
        _root.setMaximizedControl(_tree.getControl());
    }
    
    public void setFilter(SyndieURI filter) { 
        _tree.setFilter(filter);
        _tree.applyFilter();
    }
    
    public void messageSelected(MessageTree tree, SyndieURI uri, boolean toView) {
        //if (toView)
        //    _shell.setVisible(false);
        System.out.println("message selected: " + uri);
        preview(uri);
        if (_listener != null)
            _listener.messageSelected(tree, uri, toView);
    }

    public void filterApplied(MessageTree tree, SyndieURI searchURI) {
        if (_listener != null)
            _listener.filterApplied(tree, searchURI);
    }
    
    private void preview(SyndieURI uri) { 
        _root.setMaximizedControl(null);
        _preview.preview(uri);
    }
}
