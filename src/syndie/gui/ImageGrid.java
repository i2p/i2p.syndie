package syndie.gui;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import syndie.data.Timer;
import syndie.db.NullUI;

/**
 * create a grid of square images of the specified size and fill the grid from
 * either the top down or the bottom up.
 *
 */
public class ImageGrid extends Composite {
    private int _imgPx;
    private boolean _bottomsUp;
    private List _ids;
    private List _images;
    private List _tooltips;
    private List _actions;
    private List _controls;
    
    /**
     * @param imgPx image size (in pixels).  images are not rescaled, so must be the right size
     */
    public ImageGrid(Composite parent, int imgPx, boolean bottomUp) {
        super(parent, SWT.NONE);
        _imgPx = imgPx;
        _bottomsUp = bottomUp;
        _ids = new ArrayList();
        _images = new ArrayList();
        _tooltips = new ArrayList();
        _actions = new ArrayList();
        _controls = new ArrayList();
        
        // layout in a grid with no spacing/margin, either from the top down or the bottom up
        final Point size = new Point(_imgPx, _imgPx);
        setLayout(new Layout() {
            protected Point computeSize(Composite composite, int i, int i0, boolean b) { return size; }
            protected void layout(Composite grid, boolean flush) {
                Rectangle area = grid.getClientArea();
                int cols = (area.width)/ _imgPx;
                Control buttons[] = grid.getChildren();
                for (int i = 0; i < buttons.length; i++) {
                    if (_bottomsUp) {
                        buttons[i].setBounds((i%cols) * _imgPx, area.height - _imgPx - (i/cols) * _imgPx, _imgPx, _imgPx);
                    } else {
                        buttons[i].setBounds((i%cols) * _imgPx, (i/cols) * _imgPx, _imgPx, _imgPx);
                    }
                }
            }
        });
    }
    
    private int cols() {
        Rectangle area = getParent().getClientArea();
        return (area.width)/ _imgPx;
    }
    
    public int getImageSize() { return _imgPx; }
    public List getIds() { return new ArrayList(_ids); }
    
    /** 
     * @param id identifier for the indexed element (usable in select(id) and remove(id))
     * @param img image to render in the grid
     * @param tooltip text to use when hovering over the image
     * @param onFire task to run (in the SWT thread) when the item is selected
     * @return the control displaying the image if it was added, null if the grid was full
     */
    public Control add(Object id, final Image img, String tooltip, final Runnable onFire) {
        int max = (getParent().getClientArea().height / _imgPx) * cols();
        if (_ids.size() >= max) return null;
        
        _ids.add(id);
        _images.add(img);
        _tooltips.add(tooltip);
        _actions.add(onFire);
        final Button c = new Button(this, SWT.NONE);
        final Rectangle bounds = img.getBounds();
        _controls.add(c);
        c.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent evt) {
                int x = (_imgPx - bounds.width)/2;
                int y = (_imgPx - bounds.height)/2;
                evt.gc.drawImage(img, x, y);
                if (c.isFocusControl()) {
                    evt.gc.setLineWidth(2);
                    evt.gc.drawRectangle(1, 1, _imgPx-2, _imgPx-2);
                }
            }
        });
        c.addFocusListener(new FocusListener() {
            public void focusGained(FocusEvent evt) { c.redraw(); }
            public void focusLost(FocusEvent evt) { c.redraw(); }
        });
        c.setToolTipText(tooltip);
        c.addSelectionListener(new FireSelectionListener() { public void fire() { onFire.run(); } });
        redraw();
        return c;
    }
    
    /** highlight the indexed element */
    public void select(Object id) {
        int idx = _ids.indexOf(id);
        if (idx >= 0) {
            Button b = (Button)_controls.get(idx);
            b.forceFocus();
            b.redraw();
        }
    }
    
    /**
     * remove the indexed element from the grid, returning the image that was being
     * used to render it (dispose this as necessary)
     */
    public Image remove(Object id) {
        Image img = null;
        int idx = _ids.indexOf(id);
        if (idx >= 0) {
            _ids.remove(idx);
            img = (Image)_images.remove(idx);
            _tooltips.remove(idx);
            _actions.remove(idx);
            Button b = (Button)_controls.remove(idx);
            b.dispose();
        }
        layout(true);
        return img;
    }
    
    public static void main(String args[]) {
        Display d = Display.getDefault();
        ImageUtil.init(new File("."), new Timer("image init", new NullUI()));
        final int internalSize = 32;
        Image imgs[] = new Image[] {
            ImageUtil.resize(ImageUtil.createImageFromResource("iconAvatar" + 0 + ".png"), internalSize, internalSize, true),
            ImageUtil.resize(ImageUtil.createImageFromResource("iconAvatar" + 1 + ".png"), internalSize, internalSize, true),
            ImageUtil.resize(ImageUtil.createImageFromResource("iconAvatar" + 2 + ".png"), internalSize, internalSize, true),
            ImageUtil.resize(ImageUtil.createImageFromResource("iconAvatar" + 3 + ".png"), internalSize, internalSize, true),
            ImageUtil.resize(ImageUtil.createImageFromResource("iconAvatar" + 4 + ".png"), internalSize, internalSize, true),
            ImageUtil.resize(ImageUtil.createImageFromResource("iconAvatar" + 5 + ".png"), internalSize, internalSize, true)
        };
        Shell s = new Shell(d, SWT.NO_TRIM);
        s.setLayout(new FillLayout(SWT.HORIZONTAL));
        final ImageGrid gridDown = new ImageGrid(s, internalSize, true);
        for (int i = 0; i < 15; i++) {
            final int imgNum = i;
            if (null == gridDown.add("" + i, imgs[i%imgs.length], "image " + i, new Runnable() { public void run() { System.out.println("image " + imgNum + " selected"); } }))
                System.err.println("failed to add " + i);
        }
        new Button(s, SWT.PUSH); // just to test focus off the grid
        s.setSize(gridDown.computeSize(128, 300));
        s.open();
        s.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent shellEvent) { System.exit(0); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        d.timerExec(10000, new Runnable() { public void run() { gridDown.select("3"); } });
        d.timerExec(30000, new Runnable() { public void run() { gridDown.remove("5"); gridDown.remove("6"); gridDown.remove("7"); } });

        while (!d.isDisposed())
            try { if (!d.readAndDispatch()) d.sleep(); } catch (RuntimeException e) { e.printStackTrace(); }
    }
}
