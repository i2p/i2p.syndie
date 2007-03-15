package syndie.gui.desktop;

import java.util.HashMap;
import java.util.Map;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import syndie.db.UI;
import syndie.gui.FireSelectionListener;

/**
 * desktop edge containing the watched forums, bookmarked references, and the
 * active panel list
 */
public class LinkEdge extends DesktopEdge {
    private Desktop _desktop;
    private Button _watchedButton;
    private Composite _watchedAvatars;
    private Composite _panels;
    private Composite _bookmarkIcons;
    private Button _bookmarkButton;
    
    private Map _panelButtons;

    /** each new panel gets a different color, rotating through this list */
    private int _panelCounter;
    private static final int PANEL_COLORS[] = new int[] { SWT.COLOR_RED, SWT.COLOR_GREEN, SWT.COLOR_BLUE, SWT.COLOR_CYAN, SWT.COLOR_MAGENTA, SWT.COLOR_DARK_YELLOW, SWT.COLOR_DARK_RED, SWT.COLOR_DARK_GREEN, SWT.COLOR_DARK_BLUE, SWT.COLOR_DARK_CYAN, SWT.COLOR_DARK_MAGENTA };
    
    public LinkEdge(Composite parent, UI ui, Desktop desktop) {
        super(parent, ui);
        _desktop = desktop;
        _panelButtons = new HashMap(8);
        _panelCounter = 0;
        initComponents();
    }
    
    private void initComponents() {
        Composite root = getRoot();
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        root.setLayout(gl);
        
        _watchedButton = new Button(root, SWT.PUSH);
        _watchedButton.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _watchedButton.setText("Watched");
        
        _watchedAvatars = new Composite(root, SWT.NONE);
        _watchedAvatars.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _panels = new Composite(root, SWT.NONE);
        _panels.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _panels.setLayout(new GridLayout(1, true));
        
        _bookmarkIcons = new Composite(root, SWT.NONE);
        _bookmarkIcons.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        
        _bookmarkButton = new Button(root, SWT.PUSH);
        _bookmarkButton.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _bookmarkButton.setText("Bookmarks");
        
        _desktop.addListener(new Desktop.DesktopListener() {
            public void panelShown(DesktopPanel panel) { addPanel(panel); }
            public void destroyed(DesktopPanel panel) { removePanel(panel); }
        });
    }
    
    private void addPanel(final DesktopPanel panel) {
        if (_panelButtons.containsKey(panel)) return;
        String name = panel.getPanelName();
        String desc = panel.getPanelDescription();
        Button b = new Button(_panels, SWT.PUSH);
        b.addSelectionListener(new FireSelectionListener() {
            public void fire() { _desktop.show(panel); }
        });
        StringBuffer tooltip = new StringBuffer();
        if (name != null) {
            b.setText(name);
            tooltip.append(name);
            if (desc != null)
                tooltip.append(": ").append(desc);
        } else if (desc != null) {
            b.setText(desc);
            tooltip.append(desc);
        } else {
            b.setText("panel");
            tooltip.append(panel.getClass().getName());
        }
        b.setToolTipText(tooltip.toString());
        b.setBackground(_panels.getDisplay().getSystemColor(PANEL_COLORS[_panelCounter % PANEL_COLORS.length]));
        b.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _panelButtons.put(panel, b);
        getRoot().layout(true, true);
        _panelCounter++;
    }
    
    private void removePanel(DesktopPanel panel) {
        Button b = (Button)_panelButtons.remove(panel);
        if (b != null)
            b.dispose();
    }
}
