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
    public ImageCanvas(Composite parent) {
        super(parent, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);// | SWT.NO_BACKGROUND);
        _previewX = 0;
        _previewY = 0;
        _imageCurrent = null;
        addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent evt) {
                drawPreview(evt.gc);
            }
        });
        getHorizontalBar().addSelectionListener(new ScrollListener(true));
        getVerticalBar().addSelectionListener(new ScrollListener(false));
        addControlListener(new ControlListener() {
            public void controlMoved(ControlEvent controlEvent) { syncScrollbars(false); }
            public void controlResized(ControlEvent controlEvent) { syncScrollbars(true); }
        });
        setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
    }
    
    public void setImage(Image img) {
        _imageCurrent = img;
        initPreviewScrollbars();
    }
    public Image getImage() { return _imageCurrent; }
    public void disposeImage() {
        if ( (_imageCurrent != null) && (!_imageCurrent.isDisposed()) )
            _imageCurrent.dispose();
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
            
            gc.drawImage(_imageCurrent, x, y, width, height, 0, 0, width, height);
        }
    }
}
