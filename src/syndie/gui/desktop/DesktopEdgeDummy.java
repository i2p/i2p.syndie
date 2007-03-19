package syndie.gui.desktop;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import syndie.db.UI;

class DesktopEdgeDummy extends DesktopEdge {
    public DesktopEdgeDummy(int sysColor, Composite parent, UI ui) {
        super(parent, ui);
        Canvas c = new Canvas(getEdgeRoot(), SWT.NONE);
        c.setBackground(getEdgeRoot().getDisplay().getSystemColor(sysColor));
    }
}