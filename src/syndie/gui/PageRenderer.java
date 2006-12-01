package syndie.gui;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import net.i2p.data.Hash;
import net.i2p.data.PrivateKey;
import net.i2p.data.SessionKey;
import net.i2p.data.SigningPrivateKey;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.*;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
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
    private PageRendererSource _source;
    private MessageInfo _msg;
    private int _page;
    private PageActionListener _listener;
    private ArrayList _fonts;
    private ArrayList _colors;
    private ArrayList _imageIndexes;
    private ArrayList _images;
    private ArrayList _liIndexes;
    private Color _bgColor;
    private Image _bgImage;
    
    private ArrayList _imageTags;
    private ArrayList _linkTags;
    
    private Menu _bodyMenu;
    private Menu _imageMenu;
    private Menu _linkMenu;
    private Menu _imageLinkMenu;
    
    private MenuItem _bodyViewForum;
    private MenuItem _bodyViewForumMetadata;
    private MenuItem _bodyBookmarkForum;
    private MenuItem _bodyViewAuthorForum;
    private MenuItem _bodyViewAuthorMetadata;
    private MenuItem _bodyBookmarkAuthor;
    private MenuItem _bodyReplyToForum;
    private MenuItem _bodyReplyToAuthor;
    private MenuItem _bodyBanForum;
    private MenuItem _bodyBanAuthor;
    private MenuItem _bodyEnable;
    private MenuItem _bodyDisable;
    private MenuItem _bodySaveAll;
    
    private MenuItem _imgView;
    private MenuItem _imgSave;
    private MenuItem _imgSaveAll;
    private MenuItem _imgDisable;
    private MenuItem _imgEnable;
    private MenuItem _imgIgnoreAuthor;
    private MenuItem _imgIgnoreForum;
    
    private MenuItem _linkView;
    private MenuItem _linkBookmark;
    private MenuItem _linkImportReadKey;
    private MenuItem _linkImportPostKey;
    private MenuItem _linkImportManageKey;
    private MenuItem _linkImportReplyKey;
    private MenuItem _linkImportArchiveKey;
    
    private MenuItem _imgLinkViewLink;
    private MenuItem _imgLinkViewImg;
    private MenuItem _imgLinkSave;
    private MenuItem _imgLinkSaveAll;
    private MenuItem _imgLinkDisable;
    private MenuItem _imgLinkEnable;
    private MenuItem _imgLinkIgnoreAuthor;
    private MenuItem _imgLinkIgnoreForum;
    private MenuItem _imgLinkBookmarkLink;
    private MenuItem _imgLinkImportReadKey;
    private MenuItem _imgLinkImportPostKey;
    private MenuItem _imgLinkImportManageKey;
    private MenuItem _imgLinkImportReplyKey;
    private MenuItem _imgLinkImportArchiveKey;

    private boolean _enableImages;
    private boolean _enableRender;
    
    private SyndieURI _currentEventURI;
    private HTMLTag _currentEventLinkTag;
    private Image _currentEventImage;
    private HTMLTag _currentEventImageTag;
    
    private int _viewSizeModifier;
    private int _charsPerLine;
    
    public PageRenderer(Composite parent) { this(parent, false); }
    public PageRenderer(Composite parent, boolean scrollbars) {
        _parent = parent;
        if (scrollbars)
            _text = new StyledText(parent, SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER | SWT.MULTI | SWT.WRAP | SWT.READ_ONLY);
        else
            _text = new StyledText(parent, /*SWT.H_SCROLL | SWT.V_SCROLL |*/ SWT.MULTI | SWT.WRAP | SWT.READ_ONLY);
        _fonts = null;
        _colors = null;
        _imageTags = new ArrayList();
        _linkTags = new ArrayList();
        
        _enableImages = true;
        _enableRender = true;
        _viewSizeModifier = 0;
    
        buildMenus();
        pickBodyMenu();
        
        _text.setDoubleClickEnabled(true);
        _text.addSelectionListener(new SelectionListener() {
            public void widgetSelected(SelectionEvent selectionEvent) { _text.copy(); }
            public void widgetDefaultSelected(SelectionEvent selectionEvent) {}
        });
        _text.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) {}
            public void mouseDown(MouseEvent mouseEvent) { pickMenu(mouseEvent.x, mouseEvent.y, true); }
            public void mouseUp(MouseEvent mouseEvent) {}
        });
        _text.addMouseTrackListener(new MouseTrackListener() {
            public void mouseEnter(MouseEvent mouseEvent) { pickMenu(mouseEvent.x, mouseEvent.y, false); }
            public void mouseExit(MouseEvent mouseEvent) { pickMenu(mouseEvent.x, mouseEvent.y, false); }
            public void mouseHover(MouseEvent mouseEvent) {
                pickMenu(mouseEvent.x, mouseEvent.y, false);
                _text.setToolTipText("");
                Point p = new Point(mouseEvent.x, mouseEvent.y);
                int off = -1;
                try {
                    off = _text.getOffsetAtLocation(p);
                    HTMLTag linkTag = null;
                    StyleRange linkRange = null;
                    HTMLTag imgTag = null;
                    StyleRange imgRange = null;
                    for (int i = 0; i < _linkTags.size(); i++) {
                        HTMLTag tag = (HTMLTag)_linkTags.get(i);
                        if ( (off >= tag.getStartIndex()) && (off <= tag.getEndIndex()) ) {
                            StyleRange range = _text.getStyleRangeAtOffset(off);
                            linkTag = tag;
                            linkRange = range;
                            break;
                        }
                    }
                    for (int i = 0; i < _imageTags.size(); i++) {
                        HTMLTag tag = (HTMLTag)_imageTags.get(i);
                        if ( (off >= tag.getStartIndex()) && (off <= tag.getEndIndex()) ) {
                            StyleRange range = _text.getStyleRangeAtOffset(off);
                            imgRange = range;
                            imgTag = tag;
                            break;
                        }
                    }
                    if ( (imgTag != null) && (linkTag != null) ) {
                        hoverImageLink(imgRange, imgTag, linkRange, linkTag, off);
                    } else if (imgTag != null) {
                        hoverImage(imgRange, off, imgTag);
                    } else if (linkTag != null) {
                        hoverLink(linkRange, off, linkTag);
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
            }
        });
        _text.addControlListener(new ControlListener() {
            public void controlMoved(ControlEvent controlEvent) {}
            public void controlResized(ControlEvent controlEvent) {
                //if ( (_msg != null) && (_enableRender) ) rerender();
            }
        });
        _text.addKeyListener(new KeyListener() {
            public void keyReleased(KeyEvent evt) { }
            public void keyPressed(KeyEvent evt) {
                //System.out.println("character pressed: " + (int)evt.character + " state: " + (int)evt.stateMask + " keycode: " + evt.keyCode);
                switch (evt.character) {
                    case '=': // ^=
                    case '+': // ^+
                        if ( (evt.stateMask & SWT.MOD1) != 0) {
                            _viewSizeModifier += 2;
                            rerender();
                        }
                        break;
                    case '_': // ^_
                    case '-': // ^-
                        if ( (evt.stateMask & SWT.MOD1) != 0) {
                            _viewSizeModifier -= 2;
                            rerender();
                        }
                        break;
                }
            }
        });
        
    }
    public void setLayoutData(Object data) { _text.setLayoutData(data); }
    public void setListener(PageActionListener lsnr) { _listener = lsnr; }
    public Composite getComposite() { return _text; }
    public void setRender(boolean render) { _enableRender = render; }
    
    private void showNoPage() {
        _text.setVisible(false);
        _text.setText("");
        _text.setStyleRanges(null, null);
    }
    
    public void renderPage(PageRendererSource src, SyndieURI uri) {
        Hash chan = uri.getScope();
        if (chan == null) {
            showNoPage();
            return;
        }
        long chanId = src.getChannelId(chan);
        if (chanId < 0) {
            showNoPage();
            return;
        }
        MessageInfo msg = src.getMessage(chanId, uri.getMessageId());
        if (msg == null) {
            showNoPage();
            return;
        }
        Long page = null;
        page = uri.getLong("page");
        if (page != null) {
            if ( (page.longValue() > msg.getPageCount()) || (page.longValue() <= 0) )
                page = new Long(1);
        } else {
            page = new Long(1);
        }
        renderPage(src, msg, page.intValue());
    }
    private void renderPage(PageRendererSource src, MessageInfo msg, int pageNum) {
        _source = src;
        _msg = msg;
        _page = pageNum;
        //System.out.println("rendering "+ msg + ": " + pageNum);
        Cursor cursor = _parent.getDisplay().getSystemCursor(SWT.CURSOR_WAIT);
        _parent.setCursor(cursor);
        _text.setVisible(false);
        PageRendererThread.enqueue(this);
    }
    /** called from the PageRendererThread - note that this thread cannot update SWT components! */
    void threadedRender() {
        if (_msg == null) {
            renderText(null);
            return;
        }
        String cfg = _source.getMessagePageConfig(_msg.getInternalId(), _page);
        String body = _source.getMessagePageData(_msg.getInternalId(), _page);
        if ( (cfg == null) || (body == null) ) {
            //System.out.println("threaded render had no body or config: " + _msg.getInternalId() + ", page " + _page + ", body? " + (body != null) + " cfg? " + (cfg != null));
            renderText(null);
            return;
        }
        Properties props = new Properties();
        CommandImpl.parseProps(cfg, props);
        String mimeType = props.getProperty(Constants.MSG_PAGE_CONTENT_TYPE, "text/plain");
        if ("text/html".equalsIgnoreCase(mimeType) || "text/xhtml".equalsIgnoreCase(mimeType)) {
            renderHTML(body);
        } else {
            renderText(body);
        }
    }
    private void renderText(final String body) {
        _text.getDisplay().asyncExec(new Runnable() {
            public void run() {
                disposeFonts();
                disposeColors();
                disposeImages();
                if (body != null) {
                    _text.setText(body);
                } else {
                    _text.setText("");
                }
                _text.setStyleRanges(null, null);
                _text.setVisible(true);
                _parent.setCursor(null);
                if (body == null)
                    _text.setEnabled(false);
                else
                    _text.setEnabled(true);
            }
        });
    }
    private int getCharsPerLine() {
        if (true) {
            // have the HTMLStateBuilder inject fake line wrapping, even though
            // the wrapping won't be right all of the time.  this lets wrapped
            // lines have the right indentation.  however, it can cause problems
            // for bullet points, as each line is given a bullet
            _text.getDisplay().syncExec(new Runnable() {
                public void run() {
                    GC gc = new GC(_text);
                    FontMetrics metrics = gc.getFontMetrics();
                    int charWidth = metrics.getAverageCharWidth();
                    gc.dispose();
                    int paneWidth = _text.getBounds().width;
                    int w = _text.getClientArea().width;
                    int ww = _parent.getClientArea().width;
                    //if (paneWidth > 800) paneWidth = 800;
                    //else if (paneWidth < 100) paneWidth = 100;
                    _charsPerLine = paneWidth / (charWidth == 0 ? 12 : charWidth);
                    System.out.println("max chars per line: " + _charsPerLine + " pane width: " + paneWidth + "/" + ww + "/" + w + " charWidth: " + charWidth);
                }
            });
        }
        return _charsPerLine;
    }
    private void renderHTML(String html) {
        _text.getDisplay().syncExec(new Runnable() {
            public void run() {
                disposeFonts();
                disposeColors();
                disposeImages();
            }
        });

        _charsPerLine = getCharsPerLine();
        
        final HTMLStateBuilder builder = new HTMLStateBuilder(html, _msg, _charsPerLine);
        builder.buildState();
        final String text = builder.getAsText();
        final HTMLStyleBuilder sbuilder = new HTMLStyleBuilder(_source, builder.getTags(), text, _msg, _enableImages);
        //todo: do this in two parts, once in the current thread, another in the swt thread
        sbuilder.buildStyles(_viewSizeModifier);
        sbuilder.ts("styles completely built");
        _fonts = sbuilder.getFonts();
        _colors = sbuilder.getCustomColors();
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
        
        _linkTags = sbuilder.getLinkTags();
        _imageTags = sbuilder.getImageTags();
        
        Display.getDefault().syncExec(new Runnable() {
            public void run() {
                _text.setEnabled(true);
                _text.setText(text);
                _text.setStyleRanges(sbuilder.getStyleRanges());
                setLineProperties(builder, sbuilder);

                _bgImage = sbuilder.getBackgroundImage();
                if (_bgImage != null) {
                    _text.setBackgroundImage(_bgImage);
                } else {
                    _text.setBackgroundImage(null);
                    _text.setBackgroundMode(SWT.INHERIT_DEFAULT); // use the container's background
                }

                _bgColor = sbuilder.getBackgroundColor();
                if (_bgColor != null)
                    _text.setBackground(_bgColor);
                _text.setVisible(true);
                _parent.setCursor(null);
            }
        });
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
            
            // look for <dd/dt> tags, and indent $x times the nesting layer
            if (HTMLStyleBuilder.containsTag(tags, "dd"))
                indentLevel += 2;
            if (HTMLStyleBuilder.containsTag(tags, "dt"))
                indentLevel++;
            
            //System.out.println("line " + line + " [" + lineStart + ":" + lineEnd + "]: quote? " + quoteFound + " tags: " + tags 
            //                   + " (align: " + (alignment==SWT.LEFT ? "left" : alignment == SWT.CENTER ? "center" : "right")
            //                   + " indent: " + indentLevel + ")");
            
            int charWidth = -1;
            if (indentLevel > 0) {
                GC gc = new GC(_text);
                FontMetrics metrics = gc.getFontMetrics();
                charWidth = metrics.getAverageCharWidth();
                gc.dispose();
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
    
    public void dispose() {
        disposeFonts();
        disposeColors();
        disposeImages();
        _text.dispose(); // should be unnecessary...
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
                if ( (!c.isDisposed()) && (!ColorUtil.isSystemColor(c)) )
                    c.dispose();
            }
            _colors = null;
        }
        if ( (_bgColor != null) && (!_bgColor.isDisposed()) && (!ColorUtil.isSystemColor(_bgColor)))
            _bgColor.dispose();
        _bgColor = null;
    }
    private void disposeImages() {
        if ( (_bgImage != null) && (!_bgImage.isDisposed()) )
            _bgImage.dispose();
        _bgImage = null;
        if (_images != null) {
            for (int i = 0; i < _images.size(); i++) {
                Image img = (Image)_images.get(i);
                ImageUtil.dispose(img);
                //if (img == ImageUtil.ICON_IMAGE_UNKNOWN) continue;
                //if (img == ImageUtil.ICON_LINK_END) continue;
                //if (ColorUtil.isSystemColorSwatch(img)) continue;
            }
            _images.clear();
        }
    }
    
    public MessageInfo getCurrentMessage() { return _msg; }
    public int getCurrentPage() { return _page; }
    //public DBClient getCurrentClient() { return _client; }

    private void pickMenu(int x, int y, boolean showMenu) {
        Point p = new Point(x, y);
        int off = -1;
        try {
            off = _text.getOffsetAtLocation(p);
            HTMLTag linkTag = null;
            HTMLTag imgTag = null;
            for (int i = 0; i < _linkTags.size(); i++) {
                HTMLTag tag = (HTMLTag)_linkTags.get(i);
                if ( (off >= tag.getStartIndex()) && (off <= tag.getEndIndex()) ) {
                    linkTag = tag;
                    break;
                }
            }
            for (int i = 0; i < _imageTags.size(); i++) {
                HTMLTag tag = (HTMLTag)_imageTags.get(i);
                if ( (off >= tag.getStartIndex()) && (off <= tag.getEndIndex()) ) {
                    imgTag = tag;
                    break;
                }
            }
            if ( (imgTag != null) && (linkTag != null) ) {
                pickImageLinkMenu(linkTag, imgTag);
                if (showMenu) _text.getMenu().setVisible(true);
                return;
            } else if (linkTag != null) {
                pickLinkMenu(linkTag);
                if (showMenu) _text.getMenu().setVisible(true);
                return;
            } else if (imgTag != null) {
                pickImageMenu(imgTag);
                if (showMenu) _text.getMenu().setVisible(true);
                return;
            }
        } catch (IllegalArgumentException iae) {
            // no char at that point (why doesn't swt just return -1?)
        }
        pickBodyMenu();
        //_text.getMenu().setVisible(true);
    }
    
    private void pickImageLinkMenu(HTMLTag linkTag, HTMLTag imgTag) {
        _text.setMenu(_imageLinkMenu);
        SyndieURI uri = null;
        if (linkTag != null)
            uri = HTMLStyleBuilder.getURI(linkTag.getAttribValue("href"), _msg);
        if ( (_msg == null) || (linkTag == null) || (uri == null) ) {
            _imgLinkBookmarkLink.setEnabled(false);
            _imgLinkDisable.setEnabled(false);
            _imgLinkEnable.setEnabled(false);
            _imgLinkIgnoreAuthor.setEnabled(false);
            _imgLinkIgnoreForum.setEnabled(false);
            _imgLinkImportArchiveKey.setEnabled(false);
            _imgLinkImportManageKey.setEnabled(false);
            _imgLinkImportPostKey.setEnabled(false);
            _imgLinkImportReadKey.setEnabled(false);
            _imgLinkImportReplyKey.setEnabled(false);
            _imgLinkSave.setEnabled(false);
            _imgLinkSaveAll.setEnabled(false);
            _imgLinkViewImg.setEnabled(false);
            _imgLinkViewLink.setEnabled(false);
            _currentEventURI = null;
            _currentEventLinkTag = null;
            _currentEventImage = null;
        } else {
            if (uri.isChannel()) {
                _imgLinkViewLink.setEnabled(true);
                _imgLinkBookmarkLink.setEnabled(true);
            } else if (uri.isArchive()) {
                _imgLinkViewLink.setEnabled(true);
                _imgLinkBookmarkLink.setEnabled(true);
            } else {
                _imgLinkViewLink.setEnabled(true);
                _imgLinkBookmarkLink.setEnabled(false);
            }
            
            _currentEventURI = uri;
            _currentEventLinkTag = linkTag;
            _currentEventImage = null;
            if (imgTag != null) {
                for (int i = 0; i < _imageIndexes.size(); i++) {
                    Integer idx = (Integer)_imageIndexes.get(i);
                    if (idx.intValue() == imgTag.getStartIndex()) {
                        _currentEventImage = (Image)_images.get(i);
                        _currentEventImageTag = imgTag;
                        break;
                    }
                }
            }
            
            _imgLinkDisable.setEnabled(_enableImages);
            _imgLinkEnable.setEnabled(!_enableImages);
            
            long targetId = _msg.getTargetChannelId();
            long authorId = _msg.getAuthorChannelId();
            if ( (targetId == authorId) || (authorId < 0) ) {
                _imgLinkIgnoreAuthor.setEnabled(false);
            } else {
                _imgLinkIgnoreAuthor.setEnabled(true);
            }
            
            _imgLinkIgnoreForum.setEnabled(true);
            _imgLinkImportArchiveKey.setEnabled(uri.getArchiveKey() != null);
            _imgLinkImportManageKey.setEnabled(uri.getManageKey() != null);
            _imgLinkImportPostKey.setEnabled(uri.getPostKey() != null);
            _imgLinkImportReadKey.setEnabled(uri.getReadKey() != null);
            _imgLinkImportReplyKey.setEnabled(uri.getReplyKey() != null);
            _imgLinkSave.setEnabled(true);
            _imgLinkSaveAll.setEnabled(true);
            _imgLinkViewImg.setEnabled(true);
        }
    }
    
    private void pickLinkMenu(HTMLTag linkTag) {
        _text.setMenu(_linkMenu);
        SyndieURI uri = null;
        if (linkTag != null)
            uri = HTMLStyleBuilder.getURI(linkTag.getAttribValue("href"), _msg);
        if ( (_msg == null) || (linkTag == null) || (uri == null) ) {
            _linkView.setEnabled(false);
            _linkBookmark.setEnabled(false);
            _linkImportArchiveKey.setEnabled(false);
            _linkImportManageKey.setEnabled(false);
            _linkImportPostKey.setEnabled(false);
            _linkImportReadKey.setEnabled(false);
            _linkImportReplyKey.setEnabled(false);
            _currentEventURI = null;
            _currentEventLinkTag = null;
            _currentEventImage = null;
        } else {
            if (uri.isChannel()) {
                _linkView.setEnabled(true);
                _linkBookmark.setEnabled(true);
            } else if (uri.isArchive()) {
                _linkView.setEnabled(true);
                _linkBookmark.setEnabled(true);
            } else {
                _linkView.setEnabled(true);
                _linkBookmark.setEnabled(false);
            }
            
            _currentEventURI = uri;
            _currentEventLinkTag = linkTag;
            _currentEventImage = null;
            
            _linkImportManageKey.setEnabled(uri.getManageKey() != null);
            _linkImportPostKey.setEnabled(uri.getPostKey() != null);
            _linkImportReadKey.setEnabled(uri.getReadKey() != null);
            _linkImportReplyKey.setEnabled(uri.getReplyKey() != null);
            
            _linkImportArchiveKey.setEnabled(uri.getArchiveKey() != null);
        }
    }
    
    private void pickBodyMenu() {
        _text.setMenu(_bodyMenu);
            
        _currentEventURI = null;
        _currentEventLinkTag = null;
        _currentEventImage = null;
            
        if (_msg == null) {
            _bodyBanAuthor.setEnabled(false);
            _bodyBanForum.setEnabled(false);
            _bodyBookmarkAuthor.setEnabled(false);
            _bodyBookmarkForum.setEnabled(false);
            _bodyReplyToForum.setEnabled(false);
            _bodyReplyToAuthor.setEnabled(false);
            _bodyViewAuthorForum.setEnabled(false);
            _bodyViewAuthorMetadata.setEnabled(false);
            _bodyViewForum.setEnabled(false);
            _bodyViewForumMetadata.setEnabled(false);
            _bodyEnable.setEnabled(false);
            _bodyDisable.setEnabled(false);
            _bodySaveAll.setEnabled(false);
        } else {
            _bodyDisable.setEnabled(_enableImages);
            _bodyEnable.setEnabled(!_enableImages);
            _bodySaveAll.setEnabled(true);
            long targetId = _msg.getTargetChannelId();
            long authorId = _msg.getAuthorChannelId();
            if ( (targetId == authorId) || (authorId < 0) ) {
                // author == target, so no need for a separate set of author commands
                _bodyBanAuthor.setEnabled(false);
                _bodyBookmarkAuthor.setEnabled(false);
                _bodyViewAuthorForum.setEnabled(false);
                _bodyViewAuthorMetadata.setEnabled(false);
                _bodyReplyToForum.setEnabled(true);
                _bodyReplyToAuthor.setEnabled(false);
                _bodyBanForum.setEnabled(true);
                _bodyBookmarkForum.setEnabled(true);
                _bodyViewForum.setEnabled(true);
                _bodyViewForumMetadata.setEnabled(true);
            } else {
                // author != target
                _bodyBanAuthor.setEnabled(true);
                _bodyBookmarkAuthor.setEnabled(true);
                _bodyViewAuthorForum.setEnabled(true);
                _bodyViewAuthorMetadata.setEnabled(true);
                _bodyReplyToForum.setEnabled(true);
                _bodyReplyToAuthor.setEnabled(true);
                _bodyBanForum.setEnabled(true);
                _bodyBookmarkForum.setEnabled(true);
                _bodyViewForum.setEnabled(true);
                _bodyViewForumMetadata.setEnabled(true);
            }
        }
    }
    
    private void pickImageMenu(HTMLTag imgTag) {
        _text.setMenu(_imageMenu);
            
        _currentEventURI = null;
        _currentEventLinkTag = null;
        
        _currentEventImage = null;
        if (imgTag != null) {
            for (int i = 0; i < _imageIndexes.size(); i++) {
                Integer idx = (Integer)_imageIndexes.get(i);
                if (idx.intValue() == imgTag.getStartIndex()) {
                    _currentEventImage = (Image)_images.get(i);
                    _currentEventImageTag = imgTag;
                    break;
                }
            }
        }
        
        if (_msg == null) {
            _imgDisable.setEnabled(false);
            _imgEnable.setEnabled(false);
            _imgIgnoreAuthor.setEnabled(false);
            _imgIgnoreForum.setEnabled(false);
            _imgSave.setEnabled(false);
            _imgSaveAll.setEnabled(false);
            _imgView.setEnabled(false);
        } else {
            _imgDisable.setEnabled(_enableImages);
            _imgEnable.setEnabled(!_enableImages);
            _imgSave.setEnabled(true);
            _imgSaveAll.setEnabled(true);
            _imgView.setEnabled(true);
            _imgIgnoreForum.setEnabled(true);
            
            long targetId = _msg.getTargetChannelId();
            long authorId = _msg.getAuthorChannelId();
            if ( (targetId == authorId) || (authorId < 0) ) {
                _imgIgnoreAuthor.setEnabled(false);
            } else {
                _imgIgnoreAuthor.setEnabled(true);
            }
        }
    }
    
    private void hoverLink(StyleRange range, int offset, HTMLTag tag) {
        System.out.println("Hover over link @ " + offset + ": " + tag);
        String href = tag.getAttribValue("href");
        String title = tag.getAttribValue("title");
        StringBuffer buf = new StringBuffer();
        if (title != null) {
            buf.append(CommandImpl.strip(title));
            if (href != null)
                buf.append('\n');
        }
        if (href != null)
            buf.append('(').append(CommandImpl.strip(href)).append(')');
        if (buf.length() > 0)
            _text.setToolTipText(buf.toString());
    }
    private void hoverImage(StyleRange range, int offset, HTMLTag tag) {
        //System.out.println("Hover over image @ " + offset + ": " + tag);
        String alt = tag.getAttribValue("alt");
        String src = tag.getAttribValue("src");
        StringBuffer buf = new StringBuffer();
        if (alt != null) {
            buf.append(CommandImpl.strip(alt));
            if (src != null)
                buf.append('\n');
        }
        if (src != null)
            buf.append('(').append(CommandImpl.strip(src)).append(')');
        if (buf.length() > 0)
            _text.setToolTipText(buf.toString());
    }
    private void hoverImageLink(StyleRange imgRange, HTMLTag imgTag, StyleRange linkRange, HTMLTag linkTag, int off) {
        StringBuffer buf = new StringBuffer();
        String alt = imgTag.getAttribValue("alt");
        String src = imgTag.getAttribValue("src");
        String href = linkTag.getAttribValue("href");
        String title = linkTag.getAttribValue("title");
        
        if (alt != null) {
            buf.append(CommandImpl.strip(alt));
            if (src != null)
                buf.append('\n');
        }
        if (src != null)
            buf.append('(').append(CommandImpl.strip(src)).append(')');
        
        if ( (alt != null) || (src != null))
            buf.append('\n');
        
        if (title != null) {
            buf.append(CommandImpl.strip(title));
            if (href != null)
                buf.append('\n');
        }
        if (href != null)
            buf.append('(').append(CommandImpl.strip(href)).append(')');

        if (buf.length() > 0)
            _text.setToolTipText(buf.toString());        
    }
    
    
    private void buildMenus() {
        buildBodyMenu();
        buildLinkMenu();
        buildImageMenu();
        buildImageLinkMenu();
    }

    private void toggleImages() {
        boolean old = _enableImages;
        _enableImages = !old;
        _imgLinkDisable.setEnabled(_enableImages);
        _imgLinkEnable.setEnabled(!_enableImages);
        _imgDisable.setEnabled(_enableImages);
        _imgEnable.setEnabled(!_enableImages);
        _bodyDisable.setEnabled(_enableImages);
        _bodyEnable.setEnabled(!_enableImages);
        rerender();
    }
    
    private abstract class FireEventListener implements SelectionListener {
        public void widgetSelected(SelectionEvent selectionEvent) { fireEvent(); }
        public void widgetDefaultSelected(SelectionEvent selectionEvent) { fireEvent(); }
        public abstract void fireEvent();
    }
    private abstract class FireLinkEventListener extends FireEventListener {
        public void fireEvent() { fireEvent(_currentEventLinkTag, _currentEventURI); }
        public abstract void fireEvent(HTMLTag tag, SyndieURI uri);
    }
    
    /** menu shown when right clicking on anything that isn't a link or image */
    private void buildBodyMenu() {
        _bodyMenu = new Menu(_text);
        _bodyMenu.setEnabled(true);

        _bodyViewForum = new MenuItem(_bodyMenu, SWT.PUSH);
        _bodyViewForum.setText("View forum");
        _bodyViewForum.addSelectionListener(new FireEventListener() { 
            public void fireEvent() { 
                if ( (_listener != null) && (_msg != null) )
                    _listener.viewScopeMessages(PageRenderer.this, _msg.getTargetChannel()); 
            }
        });
        _bodyViewForumMetadata = new MenuItem(_bodyMenu, SWT.PUSH);
        _bodyViewForumMetadata.setText("View forum metadata");
        _bodyViewForumMetadata.addSelectionListener(new FireEventListener() { 
            public void fireEvent() { 
                if ( (_listener != null) && (_msg != null) )
                    _listener.viewScopeMetadata(PageRenderer.this, _msg.getTargetChannel()); 
            }
        });
        new MenuItem(_bodyMenu, SWT.SEPARATOR);
        _bodyViewAuthorForum = new MenuItem(_bodyMenu, SWT.PUSH);
        _bodyViewAuthorForum.setText("View author");
        _bodyViewAuthorForum.addSelectionListener(new FireEventListener() { 
            public void fireEvent() { 
                if ( (_listener != null) && (_msg != null) )
                    _listener.viewScopeMessages(PageRenderer.this, _msg.getScopeChannel()); 
            }
        });
        _bodyViewAuthorMetadata = new MenuItem(_bodyMenu, SWT.PUSH);
        _bodyViewAuthorMetadata.setText("View author metadata");
        _bodyViewAuthorMetadata.addSelectionListener(new FireEventListener() { 
            public void fireEvent() { 
                if ( (_listener != null) && (_msg != null) )
                    _listener.viewScopeMetadata(PageRenderer.this, _msg.getScopeChannel()); 
            }
        });
        new MenuItem(_bodyMenu, SWT.SEPARATOR);
        _bodyBookmarkForum = new MenuItem(_bodyMenu, SWT.PUSH);
        _bodyBookmarkForum.setText("Bookmark forum");
        _bodyBookmarkForum.addSelectionListener(new FireEventListener() { 
            public void fireEvent() { 
                if ( (_listener != null) && (_msg != null) )
                    _listener.bookmark(PageRenderer.this, SyndieURI.createScope(_msg.getTargetChannel()));
            }
        });
        _bodyBookmarkAuthor = new MenuItem(_bodyMenu, SWT.PUSH);
        _bodyBookmarkAuthor.setText("Bookmark author");
        _bodyBookmarkAuthor.addSelectionListener(new FireEventListener() { 
            public void fireEvent() { 
                if ( (_listener != null) && (_msg != null) )
                    _listener.bookmark(PageRenderer.this, SyndieURI.createScope(_msg.getScopeChannel()));
            }
        });
        
        new MenuItem(_bodyMenu, SWT.SEPARATOR);
        
        _bodyReplyToForum = new MenuItem(_bodyMenu, SWT.PUSH);
        _bodyReplyToForum.setText("Reply to forum");
        _bodyReplyToForum.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                if ( (_listener != null) && (_msg != null) )
                    _listener.replyToForum(PageRenderer.this, _msg.getTargetChannel(), _msg.getURI());
            }
        });
        _bodyReplyToAuthor = new MenuItem(_bodyMenu, SWT.PUSH);
        _bodyReplyToAuthor.setText("Private reply to author");
        _bodyReplyToAuthor.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                if ( (_listener != null) && (_msg != null) )
                    _listener.privateReply(PageRenderer.this, getAuthorHash(), _msg.getURI());
            }
        });
        
        new MenuItem(_bodyMenu, SWT.SEPARATOR);
        
        _bodySaveAll = new MenuItem(_bodyMenu, SWT.PUSH);
        _bodySaveAll.setText("Save all images");
        _bodySaveAll.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                saveAllImages();
            }
        });
        _bodyDisable = new MenuItem(_bodyMenu, SWT.PUSH);
        _bodyDisable.setText("Disable images");
        _bodyDisable.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                toggleImages();
            }
        });
        _bodyEnable = new MenuItem(_bodyMenu, SWT.PUSH);
        _bodyEnable.setText("Enable images");
        _bodyEnable.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                toggleImages();
            }
        });
        
        new MenuItem(_bodyMenu, SWT.SEPARATOR);
        _bodyBanForum = new MenuItem(_bodyMenu, SWT.PUSH);
        _bodyBanForum.setText("Ban forum");
        _bodyBanForum.addSelectionListener(new FireEventListener() { 
            public void fireEvent() { 
                if ( (_listener != null) && (_msg != null) )
                    _listener.banScope(PageRenderer.this, _msg.getTargetChannel());
            }
        });
        _bodyBanAuthor = new MenuItem(_bodyMenu, SWT.PUSH);
        _bodyBanAuthor.setText("Ban author");
        _bodyBanAuthor.addSelectionListener(new FireEventListener() { 
            public void fireEvent() { 
                if ( (_listener != null) && (_msg != null) )
                    _listener.banScope(PageRenderer.this, _msg.getScopeChannel());
            }
        });
    }
    
    /** menu shown when right clicking on a link */
    private void buildLinkMenu() {
        _linkMenu = new Menu(_text);
        _linkMenu.setEnabled(true);
        
        _linkView = new MenuItem(_linkMenu, SWT.PUSH);
        _linkView.setText("View link");
        _linkView.addSelectionListener(new FireLinkEventListener() { 
            public void fireEvent(HTMLTag tag, SyndieURI uri) { 
                if ( (_listener != null) && (_msg != null) && (uri != null) ) {
                    _listener.view(PageRenderer.this, uri);
                }
            }
        });
        new MenuItem(_linkMenu, SWT.SEPARATOR);
        _linkBookmark = new MenuItem(_linkMenu, SWT.PUSH);
        _linkBookmark.setText("Bookmark link");
        _linkBookmark.addSelectionListener(new FireLinkEventListener() { 
            public void fireEvent(HTMLTag tag, SyndieURI uri) { 
                if ( (_listener != null) && (_msg != null) && (uri != null) ) {
                    _listener.bookmark(PageRenderer.this, uri);
                }
            }
        });
        new MenuItem(_linkMenu, SWT.SEPARATOR);
        _linkImportReadKey = new MenuItem(_linkMenu, SWT.PUSH);
        _linkImportReadKey.setText("Import read key");
        _linkImportReadKey.addSelectionListener(new FireLinkEventListener() { 
            public void fireEvent(HTMLTag tag, SyndieURI uri) { 
                if ( (_listener != null) && (_msg != null) && (uri != null) ) {
                    _listener.importReadKey(PageRenderer.this, getAuthorHash(), uri.getScope(), uri.getReadKey());
                }
            }
        });
        _linkImportPostKey = new MenuItem(_linkMenu, SWT.PUSH);
        _linkImportPostKey.setText("Import post key");
        _linkImportPostKey.addSelectionListener(new FireLinkEventListener() { 
            public void fireEvent(HTMLTag tag, SyndieURI uri) { 
                if ( (_listener != null) && (_msg != null) && (uri != null) ) {
                    _listener.importPostKey(PageRenderer.this, getAuthorHash(), uri.getScope(), uri.getPostKey());
                }
            }
        });
        _linkImportManageKey = new MenuItem(_linkMenu, SWT.PUSH);
        _linkImportManageKey.setText("Import manage key");
        _linkImportManageKey.addSelectionListener(new FireLinkEventListener() { 
            public void fireEvent(HTMLTag tag, SyndieURI uri) { 
                if ( (_listener != null) && (_msg != null) && (uri != null) ) {
                    _listener.importManageKey(PageRenderer.this, getAuthorHash(), uri.getScope(), uri.getManageKey());
                }
            }
        });
        _linkImportReplyKey = new MenuItem(_linkMenu, SWT.PUSH);
        _linkImportReplyKey.setText("Import reply key");
        _linkImportReplyKey.addSelectionListener(new FireLinkEventListener() { 
            public void fireEvent(HTMLTag tag, SyndieURI uri) { 
                if ( (_listener != null) && (_msg != null) && (uri != null) ) {
                    _listener.importReplyKey(PageRenderer.this, getAuthorHash(), uri.getScope(), uri.getReplyKey());
                }
            }
        });
        _linkImportArchiveKey = new MenuItem(_linkMenu, SWT.PUSH);
        _linkImportArchiveKey.setText("Import archive key");
        _linkImportArchiveKey.addSelectionListener(new FireLinkEventListener() { 
            public void fireEvent(HTMLTag tag, SyndieURI uri) { 
                if ( (_listener != null) && (_msg != null) && (uri != null) ) {
                    _listener.importArchiveKey(PageRenderer.this, getAuthorHash(), uri, uri.getArchiveKey());
                }
            }
        });
    }

    private Hash getAuthorHash() {
        long authorId = _msg.getAuthorChannelId();
        if (authorId != _msg.getScopeChannelId()) {
            return _source.getChannelHash(authorId);
        } else {
            return _msg.getScopeChannel();
        }
    }
    
    private void saveImage() {
        // prompt the user for where the image should be saved
        String name = getSuggestedName(_currentEventImageTag);
        if (_listener != null)
            _listener.saveImage(PageRenderer.this, name, _currentEventImage);
    }
    private void saveAllImages() {
        // prompt the user for where the images should be saved
        Map images = new HashMap();
        for (int i = 0; i < _imageTags.size(); i++) {
            HTMLTag tag = (HTMLTag)_imageTags.get(i);
            String suggestedName = getSuggestedName(tag);
            if (images.containsKey(suggestedName)) {
                int j = 1;
                while (images.containsKey(suggestedName + "." + j))
                    j++;
                suggestedName = suggestedName + "." + j;
            }
            for (int j = 0; j < _imageIndexes.size(); j++) {
                Integer idx = (Integer)_imageIndexes.get(j);
                if (idx.intValue() == tag.getStartIndex()) {
                    images.put(suggestedName, _images.get(j));
                    break;
                }
            }
        }
        if (_listener != null)
            _listener.saveAllImages(PageRenderer.this, images);
    }
    
    private String getSuggestedName(HTMLTag tag) {
        SyndieURI imgURI = HTMLStyleBuilder.getURI(tag.getAttribValue("src"), _msg);
        if (imgURI != null) {
            Long attachmentId = imgURI.getLong("attachment");
            if (attachmentId != null) {
                // the attachment may not be from this message...
                Hash scope = imgURI.getScope();
                long scopeId = -1;
                Long msgId = imgURI.getMessageId();
                if ( (scope == null) || (msgId == null) ) {
                    // ok, yes, its implicitly from this message
                    scope = _msg.getScopeChannel();
                    scopeId = _msg.getScopeChannelId();
                    msgId = new Long(_msg.getMessageId());
                } else {
                    scopeId = _source.getChannelId(scope);
                }
        
                long internalMsgId = _source.getMessageId(scopeId, msgId.longValue());
                Properties props = _source.getMessageAttachmentConfig(internalMsgId, attachmentId.intValue());
                String name = props.getProperty(Constants.MSG_ATTACH_NAME);
                if (name != null)
                    return Constants.stripFilename(name, false);
                else
                    return "attachment" + attachmentId.intValue() + ".png";
            }
        }
        return tag.hashCode() + ".png";
    }
    
    private void rerender() {
        // reparse/render/layout the text area, since the image/ban/etc changed
        System.out.println("rerender");
        renderPage(_source, _msg, _page);
    }
    
    /** menu shown when right clicking on an image*/
    private void buildImageMenu() {
        _imageMenu = new Menu(_text);
        _imageMenu.setEnabled(true);
        
        _imgView = new MenuItem(_imageMenu, SWT.PUSH);
        _imgView.setText("View image");
        _imgView.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                if ( (_listener != null) && (_msg != null) && (_currentEventImage != null) )
                    _listener.viewImage(PageRenderer.this, _currentEventImage);
            }
        });
        _imgSave = new MenuItem(_imageMenu, SWT.PUSH);
        _imgSave.setText("Save image");
        _imgSave.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                if (_currentEventImage != null)
                    saveImage();
            }
        });
        _imgSaveAll = new MenuItem(_imageMenu, SWT.PUSH);
        _imgSaveAll.setText("Save all images");
        _imgSaveAll.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                saveAllImages();
            }
        });
        new MenuItem(_imageMenu, SWT.SEPARATOR);
        _imgDisable = new MenuItem(_imageMenu, SWT.PUSH);
        _imgDisable.setText("Disable images");
        _imgDisable.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                toggleImages();
            }
        });
        _imgEnable = new MenuItem(_imageMenu, SWT.PUSH);
        _imgEnable.setText("Enable images");
        _imgEnable.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                toggleImages();
            }
        });
        new MenuItem(_imageMenu, SWT.SEPARATOR);
        _imgIgnoreForum= new MenuItem(_imageMenu, SWT.PUSH);
        _imgIgnoreForum.setText("Ignore images in this forum");
        _imgIgnoreForum.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                if ( (_listener != null) && (_msg != null) )
                    _listener.ignoreImageScope(PageRenderer.this, _msg.getTargetChannel());
                rerender();
            }
        });
        _imgIgnoreAuthor= new MenuItem(_imageMenu, SWT.PUSH);
        _imgIgnoreAuthor.setText("Ignore images from this author");
        _imgIgnoreAuthor.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                if ( (_listener != null) && (_msg != null) )
                    _listener.ignoreImageScope(PageRenderer.this, _msg.getScopeChannel());
                rerender();
            }
        });
    }
    
    /** menu shown when right clicking on an image that is inside a hyperlink */
    private void buildImageLinkMenu() {
        _imageLinkMenu = new Menu(_text);
        _imageLinkMenu.setEnabled(true);
        
        _imgLinkViewLink = new MenuItem(_imageLinkMenu, SWT.PUSH);
        _imgLinkViewLink.setText("View link");
        _imgLinkViewLink.addSelectionListener(new FireLinkEventListener() { 
            public void fireEvent(HTMLTag tag, SyndieURI uri) { 
                if ( (_listener != null) && (_msg != null) && (uri != null) ) {
                    _listener.view(PageRenderer.this, uri);
                }
            }
        });
        _imgLinkViewImg = new MenuItem(_imageLinkMenu, SWT.PUSH);
        _imgLinkViewImg.setText("View image");
        _imgLinkViewImg.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                if ( (_listener != null) && (_msg != null) && (_currentEventImage != null) )
                    _listener.viewImage(PageRenderer.this, _currentEventImage);
            }
        });
        _imgLinkBookmarkLink = new MenuItem(_imageLinkMenu, SWT.PUSH);
        _imgLinkBookmarkLink.setText("Bookmark link");
        _imgLinkBookmarkLink.addSelectionListener(new FireLinkEventListener() { 
            public void fireEvent(HTMLTag tag, SyndieURI uri) { 
                if ( (_listener != null) && (_msg != null) && (uri != null) ) {
                    _listener.bookmark(PageRenderer.this, uri);
                }
            }
        });
        new MenuItem(_imageLinkMenu, SWT.SEPARATOR);
        
        _imgLinkSave = new MenuItem(_imageLinkMenu, SWT.PUSH);
        _imgLinkSave.setText("Save image");
        _imgLinkSave.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                if (_currentEventImage != null)
                    saveImage();
            }
        });
        _imgLinkSaveAll = new MenuItem(_imageLinkMenu, SWT.PUSH);
        _imgLinkSaveAll.setText("Save all images");
        _imgLinkSaveAll.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                saveAllImages();
            }
        });
        _imgLinkDisable = new MenuItem(_imageLinkMenu, SWT.PUSH);
        _imgLinkDisable.setText("Disable images");
        _imgLinkDisable.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                toggleImages();
            }
        });
        _imgLinkEnable = new MenuItem(_imageLinkMenu, SWT.PUSH);
        _imgLinkEnable.setText("Enable images");
        _imgLinkEnable.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                toggleImages();
            }
        });
        new MenuItem(_imageLinkMenu, SWT.SEPARATOR);
        
        _imgLinkIgnoreForum = new MenuItem(_imageLinkMenu, SWT.PUSH);
        _imgLinkIgnoreForum.setText("Ignore images in this forum");
        _imgLinkIgnoreForum.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                if ( (_listener != null) && (_msg != null) )
                    _listener.ignoreImageScope(PageRenderer.this, _msg.getTargetChannel());
                rerender();
            }
        });
        _imgLinkIgnoreAuthor = new MenuItem(_imageLinkMenu, SWT.PUSH);
        _imgLinkIgnoreAuthor.setText("Ignore images from this author");
        _imgLinkIgnoreAuthor.addSelectionListener(new FireEventListener() {
            public void fireEvent() {
                if ( (_listener != null) && (_msg != null) )
                    _listener.ignoreImageScope(PageRenderer.this, _msg.getScopeChannel());
                rerender();
            }
        });
        new MenuItem(_imageLinkMenu, SWT.SEPARATOR);
        
        _imgLinkImportReadKey = new MenuItem(_imageLinkMenu, SWT.PUSH);
        _imgLinkImportReadKey.setText("Import read key");
        _imgLinkImportReadKey.addSelectionListener(new FireLinkEventListener() { 
            public void fireEvent(HTMLTag tag, SyndieURI uri) { 
                if ( (_listener != null) && (_msg != null) && (uri != null) ) {
                    _listener.importReadKey(PageRenderer.this, getAuthorHash(), uri.getScope(), uri.getReadKey());
                }
            }
        });
        _imgLinkImportPostKey = new MenuItem(_imageLinkMenu, SWT.PUSH);
        _imgLinkImportPostKey.setText("Import post key");
        _imgLinkImportPostKey.addSelectionListener(new FireLinkEventListener() { 
            public void fireEvent(HTMLTag tag, SyndieURI uri) { 
                if ( (_listener != null) && (_msg != null) && (uri != null) ) {
                    _listener.importPostKey(PageRenderer.this, getAuthorHash(), uri.getScope(), uri.getPostKey());
                }
            }
        });
        _imgLinkImportManageKey = new MenuItem(_imageLinkMenu, SWT.PUSH);
        _imgLinkImportManageKey.setText("Import manage key");
        _imgLinkImportManageKey.addSelectionListener(new FireLinkEventListener() { 
            public void fireEvent(HTMLTag tag, SyndieURI uri) { 
                if ( (_listener != null) && (_msg != null) && (uri != null) ) {
                    _listener.importManageKey(PageRenderer.this, getAuthorHash(), uri.getScope(), uri.getManageKey());
                }
            }
        });
        _imgLinkImportReplyKey = new MenuItem(_imageLinkMenu, SWT.PUSH);
        _imgLinkImportReplyKey.setText("Import reply key");
        _imgLinkImportReplyKey.addSelectionListener(new FireLinkEventListener() { 
            public void fireEvent(HTMLTag tag, SyndieURI uri) { 
                if ( (_listener != null) && (_msg != null) && (uri != null) ) {
                    _listener.importReplyKey(PageRenderer.this, getAuthorHash(), uri.getScope(), uri.getReplyKey());
                }
            }
        });
        _imgLinkImportArchiveKey = new MenuItem(_imageLinkMenu, SWT.PUSH);
        _imgLinkImportArchiveKey.setText("Import archive key");
        _imgLinkImportArchiveKey.addSelectionListener(new FireLinkEventListener() { 
            public void fireEvent(HTMLTag tag, SyndieURI uri) { 
                if ( (_listener != null) && (_msg != null) && (uri != null) ) {
                    _listener.importArchiveKey(PageRenderer.this, getAuthorHash(), uri, uri.getArchiveKey());
                }
            }
        });
    }
    
    public interface PageActionListener {
        /**
         * The user wants to view the list of messages in the given scope (forum/blog)
         */
        public void viewScopeMessages(PageRenderer renderer, Hash scope);
        /**
         * The user wants to view the description for the given scope (forum/blog)
         */
        public void viewScopeMetadata(PageRenderer renderer, Hash scope);
        /**
         * The user wants to view the given uri (may refer to a syndie location, archive, external url, etc) 
         */
        public void view(PageRenderer renderer, SyndieURI uri);
        /**
         * The user wants to bookmark the given uri (perhaps prompt them where they want to bookmark it, and what they want to call it?)
         */
        public void bookmark(PageRenderer renderer, SyndieURI uri);
        /**
         * The user never wants to see the given scope again
         */
        public void banScope(PageRenderer renderer, Hash scope);
        /**
         * Display the image
         */
        public void viewImage(PageRenderer renderer, Image img);
        /**
         * The user never wants to see images from the given author (or in the given forum)
         */
        public void ignoreImageScope(PageRenderer renderer, Hash scope);
        
        /**
         * Import the read key for posts on the given scope
         * @param referencedBy who gave us the key
         * @param keyScope what forum/blog the key is valid for
         */
        public void importReadKey(PageRenderer renderer, Hash referencedBy, Hash keyScope, SessionKey key);
        /**
         * Import the post key to post on the given scope
         * @param referencedBy who gave us the key
         * @param keyScope what forum/blog the key is valid for
         */
        public void importPostKey(PageRenderer renderer, Hash referencedBy, Hash keyScope, SigningPrivateKey key);
        /**
         * Import the manage key to manage the given scope
         * @param referencedBy who gave us the key
         * @param keyScope what forum/blog the key is valid for
         */
        public void importManageKey(PageRenderer renderer, Hash referencedBy, Hash keyScope, SigningPrivateKey key);
        /**
         * Import the reply key to decrypt posts in the given scope
         * @param referencedBy who gave us the key
         * @param keyScope what forum/blog the key is valid for
         */
        public void importReplyKey(PageRenderer renderer, Hash referencedBy, Hash keyScope, PrivateKey key);
        /**
         * Import the archive key to contact the given archive
         * @param referencedBy who gave us the key
         * @param archiveURI what archive the key is valid for
         */
        public void importArchiveKey(PageRenderer renderer, Hash referencedBy, SyndieURI archiveURI, SessionKey key);
        
        /**
         * The user wants to save the given images
         * @param images map of suggested filename (without any path) to the actual swt Image
         */
        public void saveAllImages(PageRenderer renderer, Map images);
        /**
         * The user wants to save the given image
         */
        public void saveImage(PageRenderer renderer, String suggestedName, Image img);
        
        /**
         * The user wants to create a reply that is readable only by the target author
         */
        public void privateReply(PageRenderer renderer, Hash author, SyndieURI msg);

        /**
         * The user wants to post up a reply to the given forum
         */
        public void replyToForum(PageRenderer renderer, Hash forum, SyndieURI msg);
    }
}
