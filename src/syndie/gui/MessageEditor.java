package syndie.gui;

import java.util.ArrayList;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import syndie.db.DBClient;

/**
 *
 */
public class MessageEditor {
    private DBClient _client;
    /** list of (byte[]) instances */
    private ArrayList _attachments;
    /** list of Properties associated with each attachment */
    private ArrayList _attachmentConfig;
    /** list of PageEditor instances */
    private ArrayList _pages;
    
    private Composite _parent;
    private Composite _root;
    private Composite _msgControl;
    private Composite _pageRoot;
    private StackLayout _pageRootLayout;
    private Composite _actions;
    
    /** Creates a new instance of MessageEditor */
    public MessageEditor(DBClient client, Composite parent) {
        _client = client;
        _parent = parent;
        _pages = new ArrayList();
        _attachments = new ArrayList();
        _attachmentConfig = new ArrayList();
        initComponents();
    }
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(1, true));
        _msgControl = new Composite(_root, SWT.NONE);
        _msgControl.setLayout(new FillLayout(SWT.HORIZONTAL));
        Text cfg = new Text(_msgControl, SWT.SINGLE | SWT.READ_ONLY | SWT.BORDER);
        cfg.setText("the message-wide config goes here");
        _msgControl.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _pageRoot = new Composite(_root, SWT.NONE);
        _pageRootLayout = new StackLayout();
        _pageRoot.setLayout(_pageRootLayout);
        _pageRoot.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _actions = new Composite(_root, SWT.NONE);
        _actions.setLayout(new FillLayout(SWT.HORIZONTAL));
        _actions.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        new Button(_actions, SWT.PUSH).setText("Commit!");
        new Button(_actions, SWT.PUSH).setText("Postpone");
        new Button(_actions, SWT.PUSH).setText("Cancel");
    }
    
    public Control getControl() { return _root; }
    
    public void showPage(int pageNum) {
        PageEditor editor = (PageEditor)_pages.get(pageNum);
        if (editor != null) {
            _pageRootLayout.topControl = editor.getControl();
            _pageRoot.layout();
        } else {
            _pageRootLayout.topControl = null;
            _pageRoot.layout();
        }
    }
    public void addPage() {
        _pages.add(new PageEditor(_client, _pageRoot, this, "text/html"));
        showPage(_pages.size()-1);
    }
}
