package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.CoolBar;
import org.eclipse.swt.widgets.CoolItem;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;

public class ToolbarGroup {
    CoolBar _coolBar;
    CoolItem _coolItem;
    Composite _toolBar;
    Menu _chevronMenu = null;
    
    public ToolbarGroup(CoolBar coolBar) {
        _coolBar = coolBar;
        
        RowLayout layout = new RowLayout(SWT.HORIZONTAL);
        layout.wrap = false;
        
        _toolBar = new Composite(_coolBar, SWT.NONE);
        _toolBar.setLayout(layout);
        
        _coolItem = new CoolItem(_coolBar, SWT.DROP_DOWN);
        _coolItem.setControl(_toolBar);
        
        _coolItem.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (event.detail == SWT.ARROW) {
                    Rectangle toolBarBounds = _toolBar.getBounds();
                    Point pt = _coolBar.toDisplay(new Point(toolBarBounds.x, toolBarBounds.y));
                    toolBarBounds.x = pt.x;
                    toolBarBounds.y = pt.y;
                    
                    Control[] controls = _toolBar.getChildren();
                    for (int i = 0; i < controls.length; i++) {
                        Rectangle toolBounds = controls[i].getBounds();
                        pt = _toolBar.toDisplay(new Point(toolBounds.x, toolBounds.y));
                        toolBounds.x = pt.x;
                        toolBounds.y = pt.y;
                        
                        // check if this tool is completely visible
                        Rectangle intersection = toolBarBounds.intersection(toolBounds);
                        if (!intersection.equals(toolBounds)) {
                            if (_chevronMenu != null)
                                _chevronMenu.dispose();
                            
                            // build menu
                            _chevronMenu = new Menu(_coolBar);
                            for (int j = i; j < controls.length; j++)
                                createMenuItem(_chevronMenu, controls[j]);
                            
                            pt = _coolBar.toDisplay(new Point(event.x, event.y));
                            _chevronMenu.setLocation(pt.x, pt.y);
                            _chevronMenu.setVisible(true);
                            
                            break;
                        }
                    }
                }
            }
        });
    }
    
    public Button newButton(int style) { return new Button(_toolBar, style); }
    
    public void pack() {
        Point size = _toolBar.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        if (SWT.getPlatform().equals("win32")) size.x += 16;
        _coolItem.setPreferredSize(_coolItem.computeSize(size.x, size.y));
    }
    
    private MenuItem createMenuItem(Menu menu, Control control) {
        MenuItem menuItem = null;
        
        if (control instanceof Button) {
            final Button button = (Button) control;
            menuItem = new MenuItem(menu, button.getStyle());
            menuItem.setText(button.getText());
            menuItem.setImage(button.getImage());
            menuItem.addListener(SWT.Selection, new Listener() {
                public void handleEvent(Event e) {
                    button.notifyListeners(SWT.Selection, e);
                }
            });
        }
        
        return menuItem;
    }
}
