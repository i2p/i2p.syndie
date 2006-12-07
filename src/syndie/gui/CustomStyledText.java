package syndie.gui;

import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import syndie.db.UI;

public class CustomStyledText extends StyledText {
    /**
     * when retheming, the whole browser is relaid out, but this styled text
     * is also redrawn, as is the containing PageRenderer.  so, when thats going
     * to occur, the ThemeRegistry tells all instances of this sucker to use the
     * last cached size, rather than spend a lot of time recomputing the new size
     * (which it just did when redrawing the styled text anyway).
     *
     * we probably should put this in a Layout rather than in the component
     * though.
     */
    public static boolean IGNORE_FORCE = false;
    private UI _ui;
    private Point _lastSize;
    
    public CustomStyledText(UI ui, Composite composite, int attr) {
        super(composite, attr);
        _ui = ui;
    }

    public Rectangle getClientArea() {
        long before = System.currentTimeMillis();
        Rectangle rv = super.getClientArea();
        long after = System.currentTimeMillis();
        if (after-before > 10)
            trace("cst: getClientArea(): " + (after-before));
        return rv;
    }
    
    public Control[] getChildren() {
        long before = System.currentTimeMillis();
        Control rv[] = super.getChildren();
        long after = System.currentTimeMillis();
        trace("cst: getChildren(): " + (after-before));
        if (rv != null)
            for (int i = 0; i < rv.length; i++)
                trace("cst: getChildren(): child " + i + ": " + rv[i].getClass().getName());
        return rv;
    }

    public void layout() {
        long before = System.currentTimeMillis();
        super.layout();
        long after = System.currentTimeMillis();
        trace("cst: layout(): " + (after-before));
    }
    public void layout(Control changed[]) {
        long before = System.currentTimeMillis();
        super.layout(changed);
        long after = System.currentTimeMillis();
        trace("cst: layout(Control[]): " + (after-before));
    }
    public void layout(boolean changed) {
        long before = System.currentTimeMillis();
        super.layout(changed && !IGNORE_FORCE);
        long after = System.currentTimeMillis();
        trace("cst: layout(" + changed + "): " + (after-before));
    }
    public void layout(boolean changed, boolean all) {
        long before = System.currentTimeMillis();
        super.layout(changed && !IGNORE_FORCE, all && !IGNORE_FORCE);
        long after = System.currentTimeMillis();
        trace("cst: layout(" + changed + ", " + all + "): " + (after-before));
    }
    public Point computeSize(int wHint, int hHint) {
        long before = System.currentTimeMillis();
        Point rv = super.computeSize(wHint, hHint);
        long after = System.currentTimeMillis();
        trace("cst: computeSize(" + wHint + ", " + hHint+ "): " + (after-before));
        return rv;
    }
    public Point computeSize(int wHint, int hHint, boolean changed) {
        long before = System.currentTimeMillis();
        Point rv = null;
        boolean ignored = false;
        if (IGNORE_FORCE && (_lastSize != null)) {
            rv = _lastSize;
            ignored = true;
        } else {
            rv = super.computeSize(wHint, hHint, changed);
        }
        _lastSize = rv;
        long after = System.currentTimeMillis();
        trace("cst: computeSize(" + wHint + ", " + hHint+ ", " + changed + "): " + (after-before) + " [" + rv + "] [ignored? " + ignored + "]");
        return rv;
    }
    public Point getSize() {
        long before = System.currentTimeMillis();
        Point rv = super.getSize();
        long after = System.currentTimeMillis();
        trace("cst: getSize(): " + (after-before));
        return rv;
    }
    public Rectangle getBounds() {
        long before = System.currentTimeMillis();
        Rectangle rv = super.getBounds();
        long after = System.currentTimeMillis();
        trace("cst: getBounds(): " + (after-before));
        return rv;
    }
    public Rectangle computeTrim(int x, int y, int width, int height) {
        long before = System.currentTimeMillis();
        Rectangle rv = super.computeTrim(x, y, width, height);
        long after = System.currentTimeMillis();
        trace("cst: computeTrim(...): " + (after-before));
        return rv;
    }
    
    private void trace(String str) {
        UI ui = _ui;
        if (ui != null)
            ui.debugMessage(str);
        else
            System.err.println(str);
    }

    // lets us cheat
    protected void checkSubclass() {}
}
