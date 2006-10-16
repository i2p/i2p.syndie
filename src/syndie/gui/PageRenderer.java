package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.*;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.GlyphMetrics;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.*;

/**
 *
 */
public class PageRenderer {
    private MessageTab _tab;
    private Composite _parent;
    private StyledText _text;
    private Image _images[];
    private int _indexes[];
    public PageRenderer(MessageTab tab, Composite parent) {
        _tab = tab;
        _parent = parent;
        _text = new StyledText(parent, SWT.H_SCROLL | SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.WRAP | SWT.READ_ONLY);
        /*
        _text.addPaintObjectListener(new PaintObjectListener() {
            public void paintObject(PaintObjectEvent e) {
                GC gc = e.gc;
                StyleRange style = e.style;
                int start = style.start;
                for (int i = 0; i < _indexes.length; i++) {
                    if (start == _indexes[i])
                        gc.drawImage(_images[i], e.x, e.y + e.ascent - style.metrics.ascent);
                }
            }
        });
         */
        _text.setDoubleClickEnabled(true);
        _text.addSelectionListener(new SelectionListener() {
            public void widgetSelected(SelectionEvent selectionEvent) {
                System.out.println("Selected [" + selectionEvent.text + "] ["+selectionEvent.x + " to " + selectionEvent.y + "]");
            }

            public void widgetDefaultSelected(SelectionEvent selectionEvent) {}
        });
        _text.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) {
            }
            public void mouseDown(MouseEvent mouseEvent) {
                System.out.println("down [" + mouseEvent.x + " to " + mouseEvent.y + "]");
            }
            public void mouseUp(MouseEvent mouseEvent) {
                System.out.println("up [" + mouseEvent.x + " to " + mouseEvent.y + "]");
            }
        });
        _text.addMouseTrackListener(new MouseTrackListener() {
            public void mouseEnter(MouseEvent mouseEvent) {
            }

            public void mouseExit(MouseEvent mouseEvent) {
            }

            public void mouseHover(MouseEvent mouseEvent) {
                Point p = new Point(mouseEvent.x, mouseEvent.y);
                int off = -1;
                try {
                    off = _text.getOffsetAtLocation(p);
                } catch (IllegalArgumentException iae) {
                    // no char at that point (why doesn't swt just return -1?)
                }
                System.out.println("hoover [" + mouseEvent.x + " to " + mouseEvent.y + "] / " + off);
            }
        });
    }
    
    public void updateText() { updateText("Ponies are ggggreat!  X likes ponies too!\nWow!", "text/plain"); }
    public void updateText(String str, String mimeType) {
        _text.setText(str);
        int imgIndex = str.indexOf("X");
        //_images = new Image[] { _tab.getBrowser().getIconManager().getFlagPrivateMessageIcon() };
        //_indexes = new int[] { imgIndex };
        //StyleRange range = new StyleRange();
        //range.start = imgIndex;
        //range.length = 1;
        //Rectangle rect = _images[0].getBounds();
        //range.metrics = new GlyphMetrics(rect.height, 0, rect.width);
        //_text.setStyleRange(range);
    }
    public void setLayoutData(Object data) { _text.setLayoutData(data); }
}
