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
import org.eclipse.swt.events.MouseMoveListener;
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
import syndie.data.HTMLStateBuilder;
import syndie.data.HTMLTag;
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
public class PageRenderer implements Themeable {
    private BrowserControl _browser;
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
    private MenuItem _bodyViewAsText;
    private MenuItem _bodyViewStyled;
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
    private boolean _viewAsText;
    
    private boolean _styled;
    
    private SyndieURI _currentEventURI;
    private HTMLTag _currentEventLinkTag;
    private Image _currentEventImage;
    private HTMLTag _currentEventImageTag;
    
    private int _viewSizeModifier;
    private int _charsPerLine;
    
    public PageRenderer(Composite parent, BrowserControl browser) { this(parent, false, browser); }
    public PageRenderer(Composite parent, boolean scrollbars, BrowserControl browser) {
        _parent = parent;
        _browser = browser;
        if (scrollbars)
            _text = new CustomStyledText(browser.getUI(), parent, SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER | SWT.MULTI | SWT.WRAP | SWT.READ_ONLY);
        else
            _text = new CustomStyledText(browser.getUI(), parent, /*SWT.H_SCROLL | SWT.V_SCROLL |*/ SWT.MULTI | SWT.WRAP | SWT.READ_ONLY);
        _fonts = null;
        _colors = null;
        _imageTags = new ArrayList();
        _linkTags = new ArrayList();
        
        _enableImages = true;
        _enableRender = true;
        _viewAsText = false;
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
            public void mouseExit(MouseEvent mouseEvent) { 
                pickMenu(mouseEvent.x, mouseEvent.y, false);
            }
            public void mouseHover(MouseEvent mouseEvent) {
                pickMenu(mouseEvent.x, mouseEvent.y, false);
                _text.setToolTipText("");
                Point p = new Point(mouseEvent.x, mouseEvent.y);
                int off = -1;
                boolean link = false;
                try {
                    off = _text.getOffsetAtLocation(p);
                    HTMLTag linkTag = null;
                    StyleRange linkRange = null;
                    HTMLTag imgTag = null;
                    StyleRange imgRange = null;
                    for (int i = 0; i < _linkTags.size(); i++) {
                        HTMLTag tag = (HTMLTag)_linkTags.get(i);
                        if ( (off >= tag.startIndex) && (off <= tag.endIndex) ) {
                            StyleRange range = _text.getStyleRangeAtOffset(off);
                            linkTag = tag;
                            linkRange = range;
                            break;
                        }
                    }
                    for (int i = 0; i < _imageTags.size(); i++) {
                        HTMLTag tag = (HTMLTag)_imageTags.get(i);
                        if ( (off >= tag.startIndex) && (off <= tag.endIndex) ) {
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
                        link = true;
                    }
                } catch (IllegalArgumentException iae) {
                    // no char at that point (why doesn't swt just return -1?)
                }
                //System.out.println("hoover [" + mouseEvent.x + " to " + mouseEvent.y + "] / " + off);
                if (!link)
                    _text.setCursor(null);
            }
        });
        
        _text.addMouseMoveListener(new MouseMoveListener() {
            public void mouseMove(MouseEvent mouseEvent) {
                Point p = new Point(mouseEvent.x, mouseEvent.y);
                int off = -1;
                boolean link = false;
                try {
                    off = _text.getOffsetAtLocation(p);
                    HTMLTag linkTag = null;
                    StyleRange linkRange = null;
                    HTMLTag imgTag = null;
                    StyleRange imgRange = null;
                    for (int i = 0; i < _linkTags.size(); i++) {
                        HTMLTag tag = (HTMLTag)_linkTags.get(i);
                        if ( (off >= tag.startIndex) && (off <= tag.endIndex) ) {
                            StyleRange range = _text.getStyleRangeAtOffset(off);
                            linkTag = tag;
                            linkRange = range;
                            break;
                        }
                    }
                    if (linkTag != null)
                        link = true;
                } catch (IllegalArgumentException iae) {
                    // no char at that point (why doesn't swt just return -1?)
                }
                //System.out.println("hoover [" + mouseEvent.x + " to " + mouseEvent.y + "] / " + off);
                if (!link)
                    _text.setCursor(null);
                else
                    _text.setCursor(_text.getDisplay().getSystemCursor(SWT.CURSOR_HAND));
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
                    case ' ':
                        pageDown(true);
                        break;
                    case 0x01: // ^A
                        _text.selectAll();
                        evt.doit = false;
                        break;
                    case 0x03: // ^C
                        if ( (evt.stateMask & SWT.MOD1) != 0) {
                            _text.copy();
                            evt.doit = false;
                        }
                        break;
                    case 0x18: // ^X for cut doesn't make sense in a page renderer, so just copy
                        if ( (evt.stateMask & SWT.MOD1) != 0) {
                            _text.copy();
                            evt.doit = false;
                        }
                        break;
                }
                _browser.getUI().debugMessage("keyCode: " + evt.keyCode + " char=" + (int)evt.character + " state=" + evt.stateMask + " pgDown=" + SWT.PAGE_DOWN + "/" + ST.PAGE_DOWN + " pgUp=" + SWT.PAGE_UP + "/" + ST.PAGE_UP);
                if (evt.keyCode == SWT.PAGE_DOWN)
                    pageDown(false);
                else if (evt.keyCode == SWT.PAGE_UP)
                    pageUp(false);
            }
        });
        _browser.getThemeRegistry().register(this);
    }
    private void pageDown(boolean fake) {
        ScrollBar bar = _text.getVerticalBar();
        if (bar != null) {
            int incr = bar.getPageIncrement();
            if (bar.getSelection() + 1 + incr >= bar.getMaximum()) {
                _browser.getUI().debugMessage("pageDown(" + fake + "): bar=" + bar + " sel=" + bar.getSelection() + " max=" + bar.getMaximum() + " min=" + bar.getMinimum() + " incr=" + bar.getIncrement() + "/" + bar.getPageIncrement());
                if (_listener != null)
                    _listener.nextPage();
            } else {
                _browser.getUI().debugMessage("pageDown(" + fake + "): bar=" + bar + " sel=" + bar.getSelection() + " max=" + bar.getMaximum() + " min=" + bar.getMinimum() + " incr=" + bar.getIncrement() + "/" + bar.getPageIncrement());
            }
        } else {
            _browser.getUI().debugMessage("pageDown(" + fake + "): bar=" + bar);
        }
        if (fake)
            _text.invokeAction(ST.PAGE_DOWN);
    }
    private void pageUp(boolean fake) {
        ScrollBar bar = _text.getVerticalBar();
        if (bar != null) {
            int incr = bar.getPageIncrement();
            if (bar.getSelection() - 1 - incr <= bar.getMinimum()) {
                _browser.getUI().debugMessage("pageUp(" + fake + "): bar=" + bar + " sel=" + bar.getSelection() + " max=" + bar.getMaximum() + " min=" + bar.getMinimum() + " incr=" + bar.getIncrement() + "/" + bar.getPageIncrement());
                if (_listener != null)
                    _listener.prevPage();
            } else {
                _browser.getUI().debugMessage("pageUp(" + fake + "): bar=" + bar + " sel=" + bar.getSelection() + " max=" + bar.getMaximum() + " min=" + bar.getMinimum() + " incr=" + bar.getIncrement() + "/" + bar.getPageIncrement());
            }
        } else {
            _browser.getUI().debugMessage("pageUp(" + fake + "): bar=" + bar);
        }
        if (fake)
            _text.invokeAction(ST.PAGE_UP);
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
        //if (msg.getPassphrasePrompt() != null) {
        //    showNoPage();
        //    return;
        //}
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
        _styled = _bodyViewStyled.getSelection();
        //System.out.println("rendering "+ msg + ": " + pageNum);
        _text.setCursor(_parent.getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        //_text.setRedraw(false);
        _browser.getUI().debugMessage("Enqueue html render");
        PageRendererThread.enqueue(this);
    }
    /** called from the PageRendererThread - note that this thread cannot update SWT components! */
    void threadedRender() {
        long before = System.currentTimeMillis();
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
        if (!_viewAsText && ("text/html".equalsIgnoreCase(mimeType) || "text/xhtml".equalsIgnoreCase(mimeType))) {
            renderHTML(body);
        } else {
            renderText(body);
        }
        long after = System.currentTimeMillis();
        _browser.getUI().debugMessage("threaded page render took " + (after-before));
    }
    private void renderText(final String body) {
        if (_text.isDisposed()) {
            _browser.getUI().errorMessage("render after dispose?", new Exception("source"));
            return;
        }
        _text.getDisplay().asyncExec(new Runnable() {
            public void run() {
                _text.setRedraw(false);
                disposeFonts();
                disposeColors();
                disposeImages();
        
                _text.setStyleRanges(null, null);
                if (body != null) {
                    _text.setText(body);
                    StyleRange range = new StyleRange(0, body.length(), null, null);
                    range.font = _browser.getThemeRegistry().getTheme().CONTENT_FONT;
                    _text.setStyleRange(range);
                } else {
                    _text.setText("");
                }
                _text.setVisible(true);
                _text.setRedraw(true);
                _text.setCursor(null);
                if (body == null)
                    _text.setEnabled(false);
                else
                    _text.setEnabled(true);
            }
        });
    }
    private int getCharsPerLine() {
        if (false) {
            // have the HTMLStateBuilder inject fake line wrapping, even though
            // the wrapping won't be right all of the time.  this lets wrapped
            // lines have the right indentation.  however, it can cause problems
            // for bullet points, as each line is given a bullet
            _text.getDisplay().syncExec(new Runnable() {
                public void run() {
                    // problem: this uses the default font, not the themed font.  can we get around this?
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
                    _browser.getUI().debugMessage("max chars per line: " + _charsPerLine + " pane width: " + paneWidth + "/" + ww + "/" + w + " charWidth: " + charWidth);
                }
            });
        }
        return _charsPerLine;
    }
    private void renderHTML(String html) {
        _browser.getUI().debugMessage("Beginning renderHTML");
        _text.getDisplay().syncExec(new Runnable() {
            public void run() {
                disposeFonts();
                disposeColors();
                disposeImages();
            }
        });
        _browser.getUI().debugMessage("renderHTML: old stuff disposed");

        _charsPerLine = getCharsPerLine();
        
        final HTMLStateBuilder builder = new HTMLStateBuilder(_browser.getUI(), html, _charsPerLine);
        builder.buildState();
        _browser.getUI().debugMessage("renderHTML: state built");
        final String rawText = builder.getAsText();
        final HTMLStyleBuilder sbuilder = new HTMLStyleBuilder(_browser.getUI(), _source, builder.getTags(), rawText, _msg, _enableImages, _styled);
        final String text = builder.stripPlaceholders(rawText);
        
        _browser.getUI().debugMessage("renderHTML: building styles");
        //todo: do this in two parts, once in the current thread, another in the swt thread
        sbuilder.buildStyles(_viewSizeModifier);
        _browser.getUI().debugMessage("renderHTML: styles built");
        final ArrayList fonts = sbuilder.getFonts();
        final ArrayList colors = sbuilder.getCustomColors();
        // also need to get the ranges for images/internal page links/internal attachments/links/etc
        // so that the listeners registered in the constructor can do their thing
        final ArrayList imageIndexes = sbuilder.getImageIndexes();
        final ArrayList liIndexes = sbuilder.getListItemIndexes();
        final ArrayList images = sbuilder.getImages();
        if (images.size() != imageIndexes.size()) {
            throw new RuntimeException("images: " + images + " imageIndexes: " + imageIndexes);
        }
        // the _imageIndexes/_images contain the image for the linkEnd values, but
        // we may want to keep track of them separately for menu handling
        //Collection linkEndIndexes = sbuilder.getLinkEndIndexes();
        
        final ArrayList linkTags = sbuilder.getLinkTags();
        final ArrayList imageTags = sbuilder.getImageTags();
        
        _browser.getUI().debugMessage("before syncExec to write on the styledText");
        Display.getDefault().syncExec(new Runnable() {
            public void run() {
                _fonts = fonts;
                _colors = colors;
                _imageIndexes = imageIndexes;
                _liIndexes = liIndexes;
                _images = images;
                _linkTags = linkTags;
                _imageTags = imageTags;
        
                _text.setRedraw(false);
                _text.setEnabled(true);
                _text.setText(text);
                _browser.getUI().debugMessage("syncExec to write on the styledText: text written: list indexes: " + _liIndexes);
                _text.setStyleRanges(sbuilder.getStyleRanges());
                _browser.getUI().debugMessage("syncExec to write on the styledText: ranges set");
                long before = System.currentTimeMillis();
                setLineProperties(builder, sbuilder);
                long after = System.currentTimeMillis();
                _browser.getUI().debugMessage("syncExec to write on the styledText: line props set after " + (after-before));

                _bgImage = sbuilder.getBackgroundImage();
                if (_styled && _bgImage != null) {
                    _text.setBackgroundImage(_bgImage);
                } else {
                    _text.setBackgroundImage(null);
                    _text.setBackgroundMode(SWT.INHERIT_DEFAULT); // use the container's background
                }

                _bgColor = sbuilder.getBackgroundColor();
                if (_styled && _bgColor != null)
                    _text.setBackground(_bgColor);
                else
                    _text.setBackground(null);
                _text.setVisible(true);
                _text.setRedraw(true);
                _text.setCursor(null);
                _browser.getUI().debugMessage("syncExec to write on the styledText: visible, redraw, cursor configured");
            }
        });
    }
    
    /**
     * markup on a char by char level is done, but now handle the markup on a line-by-line
     * level, with indents, coloring, bullets, etc
     */
    private void setLineProperties(HTMLStateBuilder stateBuilder, HTMLStyleBuilder styleBuilder) {
        long prep = System.currentTimeMillis();
        int lines = _text.getLineCount();
        _text.setLineAlignment(0, lines, SWT.LEFT);
        _text.setLineBackground(0, lines, null);
        _text.setLineBullet(0, lines, null);
        _text.setLineIndent(0, lines, 0);
        _text.setLineJustify(0, lines, false);
        
        // this is only an estimate used for indentation, so the fact that it doesn't
        // actually take into account the actual fonts used can probably be overlooked
        int charWidth = -1;
        GC gc = new GC(_text);
        FontMetrics metrics = gc.getFontMetrics();
        charWidth = metrics.getAverageCharWidth();
        gc.dispose();
        
        long bulletTime = 0;
        long indentTime = 0;
        long alignmentTime = 0;
        int alignmentMods = 0;
        int prevAlignment = -1;
        int sequentialAligned = 0;

        ArrayList lineTags = new ArrayList(16);
        HTMLTag stateTags[] = (HTMLTag[])stateBuilder.getTags().toArray(new HTMLTag[0]);
        int stateTagCount = stateTags.length;
        
        /*
        for (int i = 0; i < stateTags.length; i++) {
            if ("li".equals(stateTags[i].name)) {
                _browser.getUI().debugMessage("li tag: " + stateTags[i].toString());
                _browser.getUI().debugMessage("li tag body: " + _text.getText(stateTags[i].startIndex, stateTags[i].endIndex) + "\n\n");
            }
        }
         */
        
        int bodySize = _text.getCharCount();
        Map bulletLists = new HashMap();
        long times[] = new long[lines];
        long timesOff[] = new long[lines];
        long timesGetTags[] = new long[lines];
        long timesFindAlign[] = new long[lines];
        long timesFindList[] = new long[lines];
        long timesPrepare[] = new long[lines];
        long endPrep = System.currentTimeMillis();
        for (int line = 0; line < lines; line++) {
            times[line] = System.currentTimeMillis();
            int lineStart = _text.getOffsetAtLine(line);
            int lineEnd = -1;
            if (line + 1 == lines)
                lineEnd = bodySize;
            else
                lineEnd = _text.getOffsetAtLine(line+1)-1;
            timesOff[line] = System.currentTimeMillis();
            //_browser.getUI().debugMessage("line " + line + " goes from " + lineStart + " to " + lineEnd);
            
            int alignment = SWT.LEFT;
            
            // now get the tags applicable to [lineStart,lineEnd]
            //boolean liRowFound = false;
            for (int i = 0; i < stateTagCount; i++) {
                HTMLTag tag = stateTags[i];
                if ( (tag.startIndex <= lineEnd) && (tag.endIndex >= lineStart) ) {
                    //if (tag.name.equals("li") && (tag.startIndex >= lineStart) )
                    //    liRowFound = true;
                    lineTags.add(tag);
                }
                //else if (tag.endIndex > lineStart)
                //    break; // the stateTags are ordered with earliest end first
                //!! which means that you can't break there jrandom.
            }
            //if (liRowFound) {
            //    _browser.getUI().debugMessage("tag for line " + line + ": " + lineTags);
            //    _browser.getUI().debugMessage("content on that line: " + _text.getText(lineStart, lineEnd));
            //}
            //ArrayList tags = getTags(stateBuilder, styleBuilder, lineStart, lineEnd);
            timesGetTags[line] = System.currentTimeMillis();
            if (HTMLStyleBuilder.containsTag(lineTags, "pre")) {
                // if they have pre, do no formatting
            } else {
                // look for alignment attributes
                for (int i = 0; i < lineTags.size(); i++) {
                    HTMLTag tag = (HTMLTag)lineTags.get(i);
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
                if (HTMLStyleBuilder.containsTag(lineTags, "center"))
                    alignment = SWT.CENTER;
            }
            
            timesFindAlign[line] = System.currentTimeMillis();
            
            boolean bulletOrdered = false;
            int olLevel = 0;
            int ulLevel = 0;
            
            Bullet bullet = null;
            boolean liFound = false;
            int indentLevel = 0;
            // look for li tags, and indent $x times the nesting layer
            for (int i = 0; i < lineTags.size(); i++) {
                HTMLTag tag = (HTMLTag)lineTags.get(i);
                if ("li".equals(tag.name)) {
                    indentLevel++;
                    // we only want to put a bullet point on the first line of
                    // a potentially multiline list item
                    if (!tag.wasConsumed()) {
                        liFound = true;
                        tag.consume();
                    }
                } else if ("ol".equals(tag.name) && liFound) {
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
                } else if ("ul".equals(tag.name) && liFound) {
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
            
            timesFindList[line] = System.currentTimeMillis();
            
            //if (indentLevel > 0)
            //    System.out.println("indent level: " + indentLevel + " bullet: " + bullet + " ulLevel: " + ulLevel + " olLevel: " + olLevel);
            
            boolean quoteFound = false;
            // look for <quote> tags, and indent $x times the nesting layer
            for (int i = 0; i < lineTags.size(); i++) {
                HTMLTag tag = (HTMLTag)lineTags.get(i);
                if ("quote".equals(tag.name)) {
                    indentLevel++;
                    quoteFound = true;
                }
            }
            
            // look for <dd/dt> tags, and indent $x times the nesting layer
            /*
            if (HTMLStyleBuilder.containsTag(lineTags, "dd"))
                indentLevel += 2;
            if (HTMLStyleBuilder.containsTag(lineTags, "dt"))
                indentLevel++;
             */
            //boolean defFound = false;
            for (int i = 0; i < lineTags.size(); i++) {
                HTMLTag tag = (HTMLTag)lineTags.get(i);
                if ("dd".equals(tag.name)) {
                    indentLevel += 2;
                    //defFound = true;
                }
                if ("dt".equals(tag.name)) {
                    indentLevel++;
                    //defFound = true;
                }
            }
            //if (defFound)
            //    _browser.getUI().debugMessage("def found on line " + line + ", indentLevel: " + indentLevel + " tags: " + lineTags + "\n content: " + _text.getText(lineStart, lineEnd));

            timesPrepare[line] = System.currentTimeMillis();
            
            // we could optimize the line settings to deal with sequential lines w/ == settings,
            // but its not worth it atm
            long t1 = System.currentTimeMillis();
            if (alignment != SWT.LEFT)
                _text.setLineAlignment(line, 1, alignment);
            long t2 = System.currentTimeMillis();
            alignmentTime += (t2-t1);
            if (prevAlignment != alignment) {
                prevAlignment = alignment;
                //sequentialAligned = 0;
            } else {
                sequentialAligned++;
            }

            if (bullet != null) {
                int width = bullet.style.metrics.width;
                if (width <= 0)
                    bullet.style.metrics.width = indentLevel * 4 * charWidth;
                _text.setLineBullet(line, 1, bullet);
                //_text.setLineIndent(line, 1, indentLevel * 4 * charWidth);
                long t3 = System.currentTimeMillis();
                bulletTime += (t3-t2);
            } else if (indentLevel > 0) {
                _text.setLineIndent(line, 1, indentLevel * 4 * charWidth);
                long t3 = System.currentTimeMillis();
                indentTime += (t3-t2);
            }
            
            lineTags.clear();
        }

        long timesOffTot = 0;
        long timesGetTagsTot = 0;
        long timesFindAlignTot = 0;
        long timesFindListTot = 0;
        for (int i = 0; i < lines; i++) {
            timesOffTot += timesOff[i]-times[i];
            timesGetTagsTot += timesGetTags[i]-timesOff[i];
            timesFindAlignTot += timesFindAlign[i]-timesGetTags[i];
            timesFindListTot += timesFindList[i]-timesFindAlign[i];
        }
        /*
        _browser.getUI().debugMessage("line style: alignment: " + alignmentTime + ", bullets: " + bulletTime 
                                      + " indent: " + indentTime 
                                      //+ " sequential: " + sequentialAligned 
                                      + " prep: " + (endPrep-prep) + " timesOff: " + timesOffTot
                                      + " timesGetTags: " + timesGetTagsTot
                                      + " timesAlign: " + timesFindAlignTot 
                                      + " timesList: " + timesFindListTot);
         */
    }
    
    public void dispose() {
        disposeFonts();
        disposeColors();
        disposeImages();
        _browser.getThemeRegistry().unregister(this);
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
        _browser.getUI().debugMessage("menu is visible? " + _text.getMenu().isVisible());
        Point p = new Point(x, y);
        int off = -1;
        try {
            off = _text.getOffsetAtLocation(p);
            HTMLTag linkTag = null;
            HTMLTag imgTag = null;
            for (int i = 0; i < _linkTags.size(); i++) {
                HTMLTag tag = (HTMLTag)_linkTags.get(i);
                if ( (off >= tag.startIndex) && (off <= tag.endIndex) ) {
                    linkTag = tag;
                    break;
                }
            }
            for (int i = 0; i < _imageTags.size(); i++) {
                HTMLTag tag = (HTMLTag)_imageTags.get(i);
                if ( (off >= tag.startIndex) && (off <= tag.endIndex) ) {
                    imgTag = tag;
                    break;
                }
            }
            if ( (imgTag != null) && (linkTag != null) ) {
                _text.getMenu().setVisible(false);
                pickImageLinkMenu(linkTag, imgTag);
                if (showMenu) _text.getMenu().setVisible(true);
                return;
            } else if (linkTag != null) {
                _text.getMenu().setVisible(false);
                pickLinkMenu(linkTag);
                if (showMenu) _text.getMenu().setVisible(true);
                return;
            } else if (imgTag != null) {
                _text.getMenu().setVisible(false);
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
        _browser.getUI().debugMessage("pickImageLinkMenu: " + imgTag);
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
                    if (idx.intValue() == imgTag.startIndex) {
                        if (_images.size() <= 0) return; // disposing
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
        _browser.getUI().debugMessage("pickLinkMenu: " + linkTag);
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
        _browser.getUI().debugMessage("pickBodyMenu");
            
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
            _bodyViewAsText.setEnabled(false);
            _bodyViewStyled.setEnabled(false);
            _bodySaveAll.setEnabled(false);
        } else {
            _bodyDisable.setEnabled(_enableImages);
            _bodyEnable.setEnabled(!_enableImages);
            _bodyViewAsText.setEnabled(!_viewAsText);
            _bodySaveAll.setEnabled(true);
            _bodyViewStyled.setEnabled(true);
            long targetId = _msg.getTargetChannelId();
            long authorId = _msg.getAuthorChannelId();
            if ( (targetId == authorId) || (authorId < 0) ) {
                // author == target, so no need for a separate set of author commands
                _bodyBanAuthor.setEnabled(false);
                _bodyBookmarkAuthor.setEnabled(false);
                _bodyViewAuthorForum.setEnabled(false);
                _bodyViewAuthorMetadata.setEnabled(false);
                _bodyReplyToForum.setEnabled(true);
                _bodyReplyToAuthor.setEnabled(true);
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
        _browser.getUI().debugMessage("pickImageMenu: " + imgTag);
        _text.setMenu(_imageMenu);
            
        _currentEventURI = null;
        _currentEventLinkTag = null;
        
        _currentEventImage = null;
        if (imgTag != null) {
            for (int i = 0; i < _imageIndexes.size(); i++) {
                Integer idx = (Integer)_imageIndexes.get(i);
                if (idx.intValue() == imgTag.startIndex) {
                    if (_images.size() <= 0) return; // disposing
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
            /*_imgSave.setEnabled(false);
            _imgSaveAll.setEnabled(false);
            _imgView.setEnabled(false);
             */
        } else {
            _imgDisable.setEnabled(_enableImages);
            _imgEnable.setEnabled(!_enableImages);
            /*_imgSave.setEnabled(true);
            _imgSaveAll.setEnabled(true);
            _imgView.setEnabled(true);
             */
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
        //System.out.println("Hover over link @ " + offset + ": " + tag);
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
        if (buf.length() > 0) {
            _text.setToolTipText(buf.toString());
        }
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
    
    private void toggleViewAsText() {
        _viewAsText = !_viewAsText;
        rerender();
    }
    private void toggleViewStyled() { rerender(); }
    
    private abstract class FireEventListener implements SelectionListener {
        public void widgetSelected(SelectionEvent selectionEvent) { fireEvent(); }
        public void widgetDefaultSelected(SelectionEvent selectionEvent) { fireEvent(); }
        public abstract void fireEvent();
    }
    private abstract class FireLinkEventListener extends FireEventListener {
        public void fireEvent() { 
            _browser.getUI().debugMessage("fireLinkEvent for uri: " + _currentEventURI + " tag: " + _currentEventLinkTag);
            fireEvent(_currentEventLinkTag, _currentEventURI);
        }
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
        _bodyViewForumMetadata.setText("View forum profile");
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
        _bodyViewAuthorMetadata.setText("View author profile");
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
        _bodyViewAsText = new MenuItem(_bodyMenu, SWT.PUSH);
        _bodyViewAsText.setText("View as text");
        _bodyViewAsText.addSelectionListener(new FireEventListener() {
            public void fireEvent() { toggleViewAsText(); }
        });
        _bodyViewStyled = new MenuItem(_bodyMenu, SWT.CHECK);
        _bodyViewStyled.setText("View styled");
        _bodyViewStyled.addSelectionListener(new FireEventListener() {
            public void fireEvent() { toggleViewStyled(); }
        });
        _bodyViewStyled.setSelection(true);
        
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
                if (idx.intValue() == tag.startIndex) {
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
                    //scope = _msg.getScopeChannel();
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
        //System.out.println("rerender");
        renderPage(_source, _msg, _page);
    }
    
    /** menu shown when right clicking on an image*/
    private void buildImageMenu() {
        _imageMenu = new Menu(_text);
        _imageMenu.setEnabled(true);
        
        /*
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
         */
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
        
        public void nextPage();
        public void prevPage();
    }
    
    public void applyTheme(Theme theme) {
        if (_msg == null) return;
        // old fonts are disposed and new ones created in the HTMLStyleBuilder
        long before = System.currentTimeMillis();
        rerender();
        long after = System.currentTimeMillis();
        _browser.getUI().debugMessage("applyTheme to pageRenderer: render took " + (after-before));
    }
}
