package syndie.gui;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.*;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
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
    private ArrayList _colors;
    private ArrayList _imageIndexes;
    private ArrayList _images;
    private ArrayList _liIndexes;
    
    private ArrayList _imageTags;
    private ArrayList _linkTags;
    
    public PageRenderer(Composite parent) {
        _parent = parent;
        _text = new StyledText(parent, /*SWT.H_SCROLL | SWT.V_SCROLL |*/ SWT.MULTI | SWT.WRAP | SWT.READ_ONLY);
        _menu = new Menu(_text);
        _fonts = null;
        _colors = null;
        _imageTags = new ArrayList();
        _linkTags = new ArrayList();
    
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
            public void mouseExit(MouseEvent mouseEvent) {
            }
            public void mouseHover(MouseEvent mouseEvent) {
                Point p = new Point(mouseEvent.x, mouseEvent.y);
                int off = -1;
                try {
                    off = _text.getOffsetAtLocation(p);
                    for (int i = 0; i < _linkTags.size(); i++) {
                        HTMLTag tag = (HTMLTag)_linkTags.get(i);
                        if ( (off >= tag.getStartIndex()) && (off <= tag.getEndIndex()) ) {
                            StyleRange range = _text.getStyleRangeAtOffset(off);
                            System.out.println("Hover over " + tag + " (" + off + ": " + range + ")");
                            break;
                        }
                    }
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
                            //System.out.println("Paint x=" + x + " y=" + y + " offset=" + offset + " image: " + img);
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
    public Composite getComposite() { return _text; }
    
    public void renderPage(DBClient client, SyndieURI uri) {
        Hash chan = uri.getScope();
        if (chan == null) return;
        long chanId = client.getChannelId(chan);
        if (chanId < 0) return;
        MessageInfo msg = client.getMessage(chanId, uri.getMessageId());
        if (msg == null) return;
        Long page = null;
        page = uri.getLong("page");
        if (page != null) {
            if ( (page.longValue() > msg.getPageCount()) || (page.longValue() < 0) )
                page = new Long(0);
        } else {
            page = new Long(0);
        }
        renderPage(client, msg, page.intValue());
    }
    public void renderPage(DBClient client, MessageInfo msg, int pageNum) {
        _client = client;
        _msg = msg;
        _page = pageNum;
        if (msg == null) {
            renderText("");
            return;
        }
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
        disposeColors();
        _text.setText(body);
        _text.setStyleRanges(null);
    }
    private void renderHTML(String html) {
        disposeFonts();
        disposeColors();

        int charsPerLine = -1;
        if (true) {
            // have the HTMLStateBuilder inject fake line wrapping, even though
            // the wrapping won't be right all of the time.  this lets wrapped
            // lines have the right indentation.  however, it can cause problems
            // for bullet points, as each line is given a bullet
            GC gc = new GC(_text);
            FontMetrics metrics = gc.getFontMetrics();
            int charWidth = metrics.getAverageCharWidth();
            int paneWidth = _text.getBounds().width;
            //if (paneWidth > 800) paneWidth = 800;
            //else if (paneWidth < 100) paneWidth = 100;
            charsPerLine = paneWidth / (charWidth == 0 ? 12 : charWidth);
            System.out.println("max chars per line: " + charsPerLine + " pane width: " + paneWidth + " charWidth: " + charWidth);
        }
        
        HTMLStateBuilder builder = new HTMLStateBuilder(html, _msg, charsPerLine);
        builder.buildState();
        String text = builder.getAsText();
        _text.setText(text);
        HTMLStyleBuilder sbuilder = new HTMLStyleBuilder(_client, builder.getTags(), text, _msg);
        sbuilder.buildStyles();
        _fonts = sbuilder.getFonts();
        _colors = sbuilder.getCustomColors();
        _text.setStyleRanges(sbuilder.getStyleRanges());
        // also need to get the ranges for images/internal page links/internal attachments/links/etc
        // so that the listeners registered in the constructor can do their thing
        _imageIndexes = sbuilder.getImageIndexes();
        _liIndexes = sbuilder.getListItemIndexes();
        _images = sbuilder.getImages();
        if (_images.size() != _imageIndexes.size()) {
            throw new RuntimeException("images: " + _images + " imageIndexes: " + _imageIndexes);
        }
        // the _imageIndexes/_images contain the image for the linkEnd values, but
        // we may want to keep track of them separately for menu handling
        Collection linkEndIndexes = sbuilder.getLinkEndIndexes();
        
        setLineProperties(builder, sbuilder);
        _linkTags = sbuilder.getLinkTags();
        _imageTags = sbuilder.getImageTags();
    }
    
    /**
     * markup on a char by char level is done, but now handle the markup on a line-by-line
     * level, with indents, coloring, bullets, etc
     */
    private void setLineProperties(HTMLStateBuilder stateBuilder, HTMLStyleBuilder styleBuilder) {
        int lines = _text.getLineCount();
        int bodySize = _text.getCharCount();
        Map bulletLists = new HashMap();
        for (int line = 0; line < lines; line++) {
            int lineStart = _text.getOffsetAtLine(line);
            int lineEnd = -1;
            if (line + 1 == lines)
                lineEnd = bodySize;
            else
                lineEnd = _text.getOffsetAtLine(line+1)-1;
            
            int alignment = SWT.LEFT;
            
            // now get the tags applicable to [lineStart,lineEnd]
            ArrayList tags = getTags(stateBuilder, styleBuilder, lineStart, lineEnd);
            if (HTMLStyleBuilder.containsTag(tags, "pre")) {
                // if they have pre, do no formatting
            } else {
                // look for alignment attributes
                for (int i = 0; i < tags.size(); i++) {
                    HTMLTag tag = (HTMLTag)tags.get(i);
                    String align = tag.getAttribValue("align");
                    if (align != null) {
                        if ("left".equalsIgnoreCase(align))
                            alignment = SWT.LEFT;
                        else if ("center".equalsIgnoreCase(align))
                            alignment = SWT.CENTER;
                        else if ("right".equalsIgnoreCase(align))
                            alignment = SWT.RIGHT;
                        else
                            continue;
                        break; // left|center|right
                    }
                }
                // look for center tags
                if (HTMLStyleBuilder.containsTag(tags, "center"))
                    alignment = SWT.CENTER;
            }
            
            boolean bulletOrdered = false;
            int olLevel = 0;
            int ulLevel = 0;
            
            Bullet bullet = null;
            boolean liFound = false;
            int indentLevel = 0;
            // look for li tags, and indent $x times the nesting layer
            for (int i = 0; i < tags.size(); i++) {
                HTMLTag tag = (HTMLTag)tags.get(i);
                if ("li".equals(tag.getName())) {
                    indentLevel++;
                    // we only want to put a bullet point on the first line of
                    // a potentially multiline list item
                    if (!tag.wasConsumed()) {
                        liFound = true;
                        tag.consume();
                    }
                } else if ("ol".equals(tag.getName()) && liFound) {
                    if ( (olLevel == 0) && (ulLevel == 0) ) {
                        bulletOrdered = true;
                        bullet = (Bullet)bulletLists.get(tag);
                        if (bullet == null) {
                            StyleRange bulletRange = new StyleRange();
                            bulletRange.metrics = new GlyphMetrics(0, 0, 0);
                            bullet = new Bullet(ST.BULLET_NUMBER | ST.BULLET_TEXT, bulletRange);
                            bullet.text = ")";
                            bulletLists.put(tag, bullet);
                        }
                    }
                    olLevel++;
                } else if ("ul".equals(tag.getName()) && liFound) {
                    if ( (olLevel == 0) && (ulLevel == 0) ) {
                        bulletOrdered = false;
                        bullet = (Bullet)bulletLists.get(tag);
                        if (bullet == null) {
                            StyleRange bulletRange = new StyleRange();
                            bulletRange.metrics = new GlyphMetrics(0, 0, 0);
                            bullet = new Bullet(ST.BULLET_DOT, bulletRange);
                            bulletLists.put(tag, bullet);
                        }
                    }
                    ulLevel++;
                }
            }
            
            //if (indentLevel > 0)
            //    System.out.println("indent level: " + indentLevel + " bullet: " + bullet + " ulLevel: " + ulLevel + " olLevel: " + olLevel);
            
            boolean quoteFound = false;
            // look for <quote> tags, and indent $x times the nesting layer
            for (int i = 0; i < tags.size(); i++) {
                HTMLTag tag = (HTMLTag)tags.get(i);
                if ("quote".equals(tag.getName())) {
                    indentLevel++;
                    quoteFound = true;
                }
            }
            
            //System.out.println("line " + line + " [" + lineStart + ":" + lineEnd + "]: quote? " + quoteFound + " tags: " + tags 
            //                   + " (align: " + (alignment==SWT.LEFT ? "left" : alignment == SWT.CENTER ? "center" : "right")
            //                   + " indent: " + indentLevel + ")");
            
            int charWidth = -1;
            if (indentLevel > 0) {
                GC gc = new GC(_text);
                FontMetrics metrics = gc.getFontMetrics();
                charWidth = metrics.getAverageCharWidth();
            }
            
            _text.setLineAlignment(line, 1, alignment);
            
            if (bullet != null) {
                bullet.style.metrics.width = indentLevel * 4 * charWidth;
                _text.setLineBullet(line, 1, bullet);
            } else {
                _text.setLineIndent(line, 1, indentLevel * 4 * charWidth);
            }
        }
    }
    
    private ArrayList getTags(HTMLStateBuilder stateBuilder, HTMLStyleBuilder styleBuilder, int start, int end) {
        ArrayList rv = new ArrayList();
        for (Iterator iter = stateBuilder.getTags().iterator(); iter.hasNext(); ) {
            HTMLTag tag = (HTMLTag)iter.next();
            int tStart = tag.getStartIndex();
            int tEnd = tag.getEndIndex();
            if ( ( (tStart <= start) && (tEnd > start) ) ||
                 ( (tStart >= start) && (tStart < end) ) )
                rv.add(tag);
        }
        return rv;
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
    private void disposeColors() {
        if (_colors != null) {
            for (int i = 0; i < _colors.size(); i++) {
                Color c = (Color)_colors.get(i);
                if (!c.isDisposed())
                    c.dispose();
            }
            _colors = null;
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
