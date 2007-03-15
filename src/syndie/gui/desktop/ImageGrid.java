package syndie.gui.desktop;

import java.util.ArrayList;
import java.util.List;
import org.eclipse.swt.SWT;
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
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 */
public class ImageGrid extends Canvas implements PaintListener, MouseListener, MouseTrackListener {
    private int _cols;
    private int _imgPx;
    private boolean _bottomsUp;
    private List _ids;
    private List _images;
    private List _tooltips;
    private List _actions;
    
    public ImageGrid(Composite parent, int style, int numCols, int imgPx, boolean bottomUp) {
        super(parent, style);
        _cols = numCols;
        _imgPx = imgPx;
        _bottomsUp = bottomUp;
        _ids = new ArrayList();
        _images = new ArrayList();
        _tooltips = new ArrayList();
        _actions = new ArrayList();
        addPaintListener(this);
        addMouseListener(this);
        addMouseTrackListener(this);
    }
    
    /** return true if the image was added, false if the grid was full */
    public boolean add(Object id, Image img, String tooltip, Runnable onFire) {
        int max = (getParent().getClientArea().height / _imgPx) * _cols;
        if (_ids.size() >= max) return false;
        
        _ids.add(id);
        _images.add(img);
        _tooltips.add(tooltip);
        _actions.add(onFire);
        redraw();
        return true;
    }

    public void paintControl(PaintEvent evt) {
        for (int i = 0; i < _ids.size(); i++) {
            Image img = (Image)_images.get(i);
            if (_bottomsUp) {
                evt.gc.drawImage(img, (i%_cols) * _imgPx, evt.height - _imgPx - (i/_cols) * _imgPx);
            } else {
                evt.gc.drawImage(img, (i%_cols) * _imgPx, (i/_cols) * _imgPx);
            }
        }
    }
    

    public void mouseDoubleClick(MouseEvent evt) {}
    public void mouseUp(MouseEvent evt) { redraw(); }
    public void mouseDown(MouseEvent evt) {
        int idx = getImageIndex(evt.x, evt.y);
        if (idx < _ids.size()) {
            Runnable r = (Runnable)_actions.get(idx);
            r.run();
        }
        redraw();
    }
    

    public void mouseEnter(MouseEvent mouseEvent) {}
    public void mouseExit(MouseEvent mouseEvent) {
        setToolTipText("");
        redraw();
    }
    public void mouseHover(MouseEvent evt) {
        int idx = getImageIndex(evt.x, evt.y);
        if (idx < _ids.size()) {
            String tooltip = (String)_tooltips.get(idx);
            setToolTipText(tooltip);
            redraw();
        }
    }
    
    private int getImageIndex(int x, int y) {
        int column = (x / _imgPx);
        int row = 0;
        if (_bottomsUp)
            row = (getClientArea().height - y) / _imgPx;
        else
            row = (y / _imgPx);
        
        return (row * _cols) + column;
    }

    public Point computeSize(int wHint, int hHint, boolean changed) { return computeSize(wHint, hHint); }
    public Point computeSize(int wHint, int hHint) {
        int width = _cols * _imgPx;
        if (wHint > 0) width = wHint;
        int height = ((_ids.size() + _cols - 1)/ _cols) * _imgPx;
        if (hHint > 0) height = hHint;
        Point rv = new Point(width, height);
        System.out.println("computing size: " + rv + " (" + wHint + "/" + hHint + ")");
        return rv;
    }
    
    public static void main(String args[]) {
        Display d = Display.getDefault();
        Image img = new Image(d, "/home/jrandom/data/dev/syndie/src/syndie/gui/iconSynOk.png");
        Shell s = new Shell(d, SWT.SHELL_TRIM);
        s.setLayout(new FillLayout(SWT.VERTICAL));
        ImageGrid gridUp = new ImageGrid(s, SWT.NONE, 4, 16, true);
        for (int i = 0; i < 25; i++) {
            final int imgNum = i;
            gridUp.add("" + i, img, "image " + i, new Runnable() { public void run() { System.out.println("image " + imgNum + " selected"); } });
        }
        s.open();
        s.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent shellEvent) { System.exit(0); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        while (!d.isDisposed())
            try { if (!d.readAndDispatch()) d.sleep(); } catch (RuntimeException e) { e.printStackTrace(); }
    }
}
