package syndie.gui;

import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;

public class FireSelectionListener implements SelectionListener {
    public void widgetSelected(SelectionEvent evt) { fire(evt); }
    public void widgetDefaultSelected(SelectionEvent evt) { fire(evt); }
    public void fire(SelectionEvent evt) { fire(); }
    public void fire() {}
}
