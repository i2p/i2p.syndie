package syndie.gui;

import java.util.ArrayList;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 *
 */
public class MessageEditor {
    /** list of (byte[]) instances */
    private ArrayList _attachments;
    /** list of Properties associated with each attachment */
    private ArrayList _attachmentConfig;
    /** list of PageEditor instances */
    private ArrayList _pages;
    
    private Composite _parent;
    private Composite _root;
    private StackLayout _rootLayout;
    
    /** Creates a new instance of MessageEditor */
    public MessageEditor(Composite parent) {
        _parent = parent;
        _root = new Composite(parent, SWT.NONE);
        _rootLayout = new StackLayout();
        _root.setLayout(_rootLayout);
        _pages = new ArrayList();
        _attachments = new ArrayList();
        _attachmentConfig = new ArrayList();
    }
    
    public Control getControl() { return _root; }
    
    public void showPage(int pageNum) {
        PageEditor editor = (PageEditor)_pages.get(pageNum);
        if (editor != null) {
            _rootLayout.topControl = editor.getControl();
            _root.layout();
        } else {
            _rootLayout.topControl = null;
            _root.layout();
        }
    }
    public void addPage() {
        _pages.add(new PageEditor(_root, this, "text/html"));
        showPage(_pages.size()-1);
    }
}
