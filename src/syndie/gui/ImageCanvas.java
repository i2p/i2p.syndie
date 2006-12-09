package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.ScrollBar;

/**
 *
 */
public class ImageCanvas extends Canvas {
    private int _previewX;
    private int _previewY;
    private Image _imageCurrent;
    
    private int _forcedWidth;
    private int _forcedHeight;
    private boolean _scroll;
    
    public ImageCanvas(Composite parent) { this(parent, true); }
    public ImageCanvas(Composite parent, boolean scroll) {
        super(parent, SWT.BORDER | (scroll ? SWT.H_SCROLL | SWT.V_SCROLL : 0));// | SWT.NO_BACKGROUND);
        _scroll = scroll;
        _previewX = 0;
        _previewY = 0;
        _imageCurrent = null;
        addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent evt) {
                drawPreview(evt.gc);
            }
        });
        if (scroll) {
            getHorizontalBar().addSelectionListener(new ScrollListener(true));
            getVerticalBar().addSelectionListener(new ScrollListener(false));
            addControlListener(new ControlListener() {
                public void controlMoved(ControlEvent controlEvent) { syncScrollbars(false); }
                public void controlResized(ControlEvent controlEvent) { syncScrollbars(true); }
            });
        }
        setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        
        _forcedWidth = 0;
        _forcedHeight = 0;
    }
    
    public void forceSize(int width, int height) {
        _forcedWidth = width;
        _forcedHeight = height;
    }
    public void unforceSize() {
        _forcedWidth = 0;
        _forcedHeight = 0;
    }
    public Point computeSize(int wHint, int hHint) {
        if ( (_forcedWidth > 0) && (_forcedHeight > 0) )
            return new Point(_forcedWidth, _forcedHeight);
        else
            return super.computeSize(wHint, hHint);
    }
    public Point computeSize(int wHint, int hHint, boolean changed) {
        if ( (_forcedWidth > 0) && (_forcedHeight > 0) )
            return new Point(_forcedWidth, _forcedHeight);
        else
            return super.computeSize(wHint, hHint, changed);
    }
    public Rectangle getBounds() {
        if ( (_forcedWidth > 0) && (_forcedHeight > 0) )
            return new Rectangle(0, 0, _forcedWidth, _forcedHeight);
        else
            return super.getBounds();
    }
    public Point getSize() { 
        if ( (_forcedWidth > 0) && (_forcedHeight > 0) )
            return new Point(_forcedWidth, _forcedHeight);
        else
            return super.getSize();
    }
    public Rectangle getClientArea() {
        if ( (_forcedWidth > 0) && (_forcedHeight > 0) )
            return new Rectangle(0, 0, _forcedWidth, _forcedHeight);
        else
            return super.getClientArea();
    }
    
    public void setImage(Image img) {
        _imageCurrent = img;
        initPreviewScrollbars();
    }
    public Image getImage() { return _imageCurrent; }
    public void disposeImage() {
        ImageUtil.dispose(_imageCurrent);
        _imageCurrent = null;
    }
    
    private class ScrollListener implements SelectionListener {
        private boolean _horizontal;
        public ScrollListener(boolean horizontal) { _horizontal = horizontal; }
        public void widgetSelected(SelectionEvent selectionEvent) { scrollPreview(); }
        public void widgetDefaultSelected(SelectionEvent selectionEvent) { scrollPreview(); }
        private void scrollPreview() {
            if (_horizontal)
                _previewX = getHorizontalBar().getSelection();
            else
                _previewY = getVerticalBar().getSelection();
            //System.out.println("scrolling preview to [" + _previewX + "," + _previewY + "]");
            redraw();
        }
    }
    private void syncScrollbars(boolean rescaleScrollbars) {
        _previewX = getHorizontalBar().getSelection();
        _previewY = getVerticalBar().getSelection();
        if (rescaleScrollbars)
            rescaleScrollbars(_previewX, _previewY);
        //System.out.println("syncing preview scrollbars to [" + _previewX + "," + _previewY + "]");
        //_configPreviewCanvas.layout(true);
        //GC gc = new GC(_configPreviewCanvas);
        //drawPreview(gc);
        //gc.dispose();
        //_configPreviewCanvas.addPaintListener
        redraw();
    }
    private void initPreviewScrollbars() {
        _previewX = 0;
        _previewY = 0;
        
        rescaleScrollbars(0, 0);
    }
    private void rescaleScrollbars(int x, int y) {
        Rectangle target = getClientArea();
        Rectangle size = null;
        if ( (_imageCurrent == null) || (_imageCurrent.isDisposed()) )
            size = new Rectangle(0, 0, 0, 0);
        else
            size = _imageCurrent.getBounds();
        
        ScrollBar hb = getHorizontalBar();
        ScrollBar vb = getVerticalBar();
        
        if (!_scroll)
            return;
        
        //System.out.println("rescale " + size.width + "x" + size.height + " into " + target.width + "x" + target.height + " (" + hb.getSize().x + " and " + vb.getSize().y + ")");
        
        int excessWidth = size.width - (target.width);
        int excessHeight = size.height - (target.height);
        
        if (excessWidth > 0) {
            hb.setMaximum(excessWidth);
            hb.setMinimum(0);
            hb.setIncrement((int)Math.ceil(excessWidth/100));
            hb.setEnabled(true);
            hb.setSelection(x);
            hb.setVisible(true);
        } else {
            hb.setEnabled(false);
            hb.setVisible(false);
        }
        if (excessHeight > 0) {
            vb.setMaximum(excessHeight);
            vb.setMinimum(0);
            vb.setIncrement((int)Math.ceil(excessHeight/100));
            vb.setEnabled(true);
            vb.setSelection(y);
            vb.setVisible(true);
        } else {
            vb.setEnabled(false);
            vb.setVisible(false);
        }
    }
    private void drawPreview(GC gc) {
        if ( (_imageCurrent != null) && (!_imageCurrent.isDisposed()) ) {
            int x = _previewX;
            int y = _previewY;
            Rectangle bounds = _imageCurrent.getBounds();
            int width = bounds.width;
            int height = bounds.height;
            Rectangle pane = getClientArea();
            //Point pane = computeSize(SWT.DEFAULT, SWT.DEFAULT, true);
            width = Math.min(width, pane.width);
            height = Math.min(height, pane.height);
            if (bounds.width <= pane.width)
                x = 0;
            if (bounds.height <= pane.height)
                y = 0;
            if (x + width > bounds.width) {
                x = bounds.width-width;
                //System.out.println("(too wide, x=" + x +")");
            }
            if (y + height > bounds.height) {
                y = bounds.height-height;
                //System.out.println("(too tall, y=" + y +")");
            }
        
            //System.out.println("drawing the image onto " + pane.width +"x" + pane.height + " from " + bounds.width + "x" + bounds.height);
            
            // center the image if its smaller than the pane
            int xOff = 0;
            int yOff = 0;

            if (pane.x > bounds.width)
                xOff = (pane.width - bounds.width)/2;
            if (pane.y > bounds.height)
                yOff = (pane.height - bounds.height)/2;

            if (_scroll) {
                gc.drawImage(_imageCurrent, x, y, width, height, xOff, yOff, width, height);
            } else {
                //System.out.println("x=" + x + " y=" + y + " img.w=" + bounds.width + " img.h=" + bounds.height + " xoff=" + xOff + " yoff=" + yOff + " p.w=" + pane.width + " p.h=" + pane.height);
                gc.drawImage(_imageCurrent, x, y, bounds.width, bounds.height, xOff, yOff, pane.width, pane.height);
            }
        }
    }
}
