package syndie.gui;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Properties;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.*;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.GlyphMetrics;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.*;
import syndie.Constants;
import syndie.data.MessageInfo;
import syndie.data.SyndieURI;
import syndie.db.CommandImpl;
import syndie.db.DBClient;

/**
 * Creates a new StyledText component for rendering pages.  Supports plain
 * text pages as well as html, offering a simple listener interface to receive
 * hover/menu/selection events for html elements
 *
 */
public class PageRenderer {
    private Composite _parent;
    private StyledText _text;
    private DBClient _client;
    private MessageInfo _msg;
    private int _page;
    private Menu _menu;
    private PageActionListener _listener;
    private ArrayList _fonts;
    private ArrayList _imageIndexes;
    private ArrayList _images;
    private ArrayList _liIndexes;
    
    public PageRenderer(Composite parent) {
        _parent = parent;
        _text = new StyledText(parent, SWT.H_SCROLL | SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.WRAP | SWT.READ_ONLY);
        _menu = new Menu(_text);
        _fonts = null;

        _text.setDoubleClickEnabled(true);
        _text.addSelectionListener(new SelectionListener() {
            public void widgetSelected(SelectionEvent selectionEvent) {
                //System.out.println("Selected [" + selectionEvent.text + "] ["+selectionEvent.x + " to " + selectionEvent.y + "]");
            }
            public void widgetDefaultSelected(SelectionEvent selectionEvent) {}
        });
        _text.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) {}
            public void mouseDown(MouseEvent mouseEvent) {
                //System.out.println("down [" + mouseEvent.x + " to " + mouseEvent.y + "]");
            }
            public void mouseUp(MouseEvent mouseEvent) {
                //System.out.println("up [" + mouseEvent.x + " to " + mouseEvent.y + "]");
            }
        });
        _text.addMouseTrackListener(new MouseTrackListener() {
            public void mouseEnter(MouseEvent mouseEvent) {}
            public void mouseExit(MouseEvent mouseEvent) {}
            public void mouseHover(MouseEvent mouseEvent) {
                Point p = new Point(mouseEvent.x, mouseEvent.y);
                int off = -1;
                try {
                    off = _text.getOffsetAtLocation(p);
                } catch (IllegalArgumentException iae) {
                    // no char at that point (why doesn't swt just return -1?)
                }
                //System.out.println("hoover [" + mouseEvent.x + " to " + mouseEvent.y + "] / " + off);
            }
        });
        // draw the current image or bullet on the pane
        _text.addPaintObjectListener(new PaintObjectListener() {
            public void paintObject(PaintObjectEvent evt) {
                GC gc = evt.gc;
                StyleRange range = evt.style;
                int start = range.start;
                if (_imageIndexes != null) {
                    for (int i = 0; i < _imageIndexes.size(); i++) {
                        int offset = ((Integer)_imageIndexes.get(i)).intValue();
                        if (start == offset) {
                            Image img = (Image)_images.get(i);
                            int x = evt.x;
                            int y = evt.y + evt.ascent - range.metrics.ascent;
                            gc.drawImage(img, x, y);
                            return;
                        }
                    }
                }
                // not an image.. check the li indexes
                if (_liIndexes != null) {
                    for (int i = 0; i < _liIndexes.size(); i++) {
                        int offset = ((Integer)_liIndexes.get(i)).intValue();
                        if (start == offset) {
                            // render the line's bullet
                            // ...
                            return;
                        }
                    }
                }
            }
        });
    }
    public void setLayoutData(Object data) { _text.setLayoutData(data); }
    public void setListener(PageActionListener lsnr) { _listener = lsnr; }
    
    public void renderPage(DBClient client, MessageInfo msg, int pageNum) {
        _client = client;
        _msg = msg;
        _page = pageNum;
        String cfg = client.getMessagePageConfig(msg.getInternalId(), pageNum);
        String body = client.getMessagePageData(msg.getInternalId(), pageNum);
        Properties props = new Properties();
        CommandImpl.parseProps(cfg, props);
        String mimeType = props.getProperty(Constants.MSG_PAGE_CONTENT_TYPE, "text/plain");
        if ("text/html".equalsIgnoreCase(mimeType) || "text/xhtml".equalsIgnoreCase(mimeType)) {
            renderHTML(body);
        } else {
            renderText(body);
        }
    }
    private void renderText(String body) {
        disposeFonts();
        _text.setText(body);
        _text.setStyleRanges(null);
    }
    private void renderHTML(String html) {
        disposeFonts();
        HTMLStateBuilder builder = new HTMLStateBuilder(html, _msg);
        builder.buildState();
        String text = builder.getAsText();
        _text.setText(text);
        HTMLStyleBuilder sbuilder = new HTMLStyleBuilder(_client, builder.getTags(), text, _msg);
        sbuilder.buildStyles();
        _fonts = sbuilder.getFonts();
        _text.setStyleRanges(sbuilder.getStyleRanges());
        // also need to get the ranges for images/internal page links/internal attachments/links/etc
        // so that the listeners registered in the constructor can do their thing
        _imageIndexes = sbuilder.getImageIndexes();
        _liIndexes = sbuilder.getListItemIndexes();
        _images = sbuilder.getImages();
        Collection linkEndIndexes = sbuilder.getLinkEndIndexes();
    }
    
    private void disposeFonts() {
        if (_fonts != null) {
            for (int i = 0; i < _fonts.size(); i++) {
                Font f = (Font)_fonts.get(i);
                if (!f.isDisposed())
                    f.dispose();
            }
            _fonts = null;
        }
    }
    
    public MessageInfo getCurrentMessage() { return _msg; }
    public int getCurrentPage() { return _page; }
    public DBClient getCurrentClient() { return _client; }
    public Menu getMenu() { return _menu; }
    
    public interface PageActionListener {
        // hover events are item selection without clicking (mouseover / tab to)
        public void imageHover(PageRenderer renderer, int attachmentNum);
        public void internalPageLinkHover(PageRenderer renderer, int targetPage);
        public void internalAttachmentLinkHover(PageRenderer renderer, int targetAttachment);
        public void syndieLinkHover(PageRenderer renderer, SyndieURI uri);
        public void externalLinkHover(PageRenderer renderer, String url);
        
        // menu events are triggered by e.g. right mouse click on an item
        // the listener is responsible for populating the popup menu with actions
        // appropriate for the target
        public void imageMenu(PageRenderer renderer, Menu menu, int attachmentNum);
        public void internalPageLinkMenu(PageRenderer renderer, Menu menu, int targetPage);
        public void internalAttachmentLinkMenu(PageRenderer renderer, Menu menu, int targetAttachment);
        public void syndieLinkMenu(PageRenderer renderer, Menu menu, SyndieURI uri);
        public void externalLinkMenu(PageRenderer renderer, Menu menu, String url);
        
        // follow events are triggered by e.g. left mouse click on an item
        public void imageFollow(PageRenderer renderer, int attachmentNum);
        public void internalPageLinkFollow(PageRenderer renderer, int targetPage);
        public void internalAttachmentLinkFollow(PageRenderer renderer, int targetAttachment);
        public void syndieLinkFollow(PageRenderer renderer, SyndieURI uri);
        public void externalLinkFollow(PageRenderer renderer, String url);
    }
}
