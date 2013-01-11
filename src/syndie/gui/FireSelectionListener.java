package syndie.gui;

import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;

/**
 *  For ease of writing SelectionListeners.
 *  Calls fire() for both.
 */
public abstract class FireSelectionListener implements SelectionListener {

    public void widgetSelected(SelectionEvent evt) { fire(); }

    public void widgetDefaultSelected(SelectionEvent evt) { fire(); }

    public abstract void fire();
}
