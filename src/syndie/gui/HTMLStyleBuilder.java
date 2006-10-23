package syndie.gui;

import java.io.ByteArrayInputStream;
import java.net.URISyntaxException;
import java.util.*;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.GlyphMetrics;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import syndie.Constants;
import syndie.data.MessageInfo;
import syndie.data.SyndieURI;
import syndie.db.CommandImpl;
import syndie.db.DBClient;

/**
 *
 */
class HTMLStyleBuilder {
    private DBClient _client;
    private List _htmlTags;
    private String _msgText;
    private MessageInfo _msg;
    private List _tagRanges;
    private StyleRange[] _styleRanges;
    private ArrayList _imageIndexes;
    /** Image instances loaded up at the _imageIndexes location */
    private ArrayList _images;
    private ArrayList _linkIndexes;
    private ArrayList _listItemIndexes;
    
    // default fonts that can be shared across many ranges.  this does not cover
    // every scenario though, so some fonts may need to be built dynamically
    private Font _fontDefault;
    private Font _fontP;
    private Font _fontLI;
    private Font _fontH1;
    private Font _fontH2;
    private Font _fontH3;
    private Font _fontH4;
    private Font _fontH5;
    private Font _fontPRE;
    private Font _fontCODE;
    private Font _fontA;
    /** fonts built for ranges that did not match up with the defaults */
    private List _customFonts;

    private static Image _linkEndIcon;
    static { buildDefaultIcons(); }
    
    public HTMLStyleBuilder(DBClient client, List htmlTags, String msgText, MessageInfo msg) {
        _client = client;
        _htmlTags = htmlTags;
        _msgText = msgText;
        _msg = msg;
        _imageIndexes = new ArrayList();
        _linkIndexes = new ArrayList();
        _listItemIndexes = new ArrayList();
        _images = new ArrayList();
        _customFonts = new ArrayList();
        buildFonts(getFontConfig(client));
    }
    
    public void buildStyles() {
        // get a list of points where any tag starts or ends
        TreeMap breakPointTags = new TreeMap();
        for (int i = 0; i < _htmlTags.size(); i++) {
            HTMLTag tag = (HTMLTag)_htmlTags.get(i);
            List tags = (List)breakPointTags.get(new Integer(tag.getStartIndex()));
            if (tags == null) {
                tags = new ArrayList();
                breakPointTags.put(new Integer(tag.getStartIndex()), tags);
            }
            tags.add(tag);
            
            // now for ends
            tags = (List)breakPointTags.get(new Integer(tag.getEndIndex()));
            if (tags == null) {
                tags = new ArrayList();
                breakPointTags.put(new Integer(tag.getEndIndex()), tags);
            }
            tags.add(tag);
        }
        
        // include special characters
        insertCharBreakpoints(HTMLStateBuilder.PLACEHOLDER_IMAGE, breakPointTags, _imageIndexes);
        insertCharBreakpoints(HTMLStateBuilder.PLACEHOLDER_LINK_END, breakPointTags, _linkIndexes);
        insertCharBreakpoints(HTMLStateBuilder.PLACEHOLDER_LISTITEM, breakPointTags, _listItemIndexes);
        
        // make sure it covers the whole schebang
        List startTags = (List)breakPointTags.get(new Integer(0));
        if (startTags == null)
            breakPointTags.put(new Integer(0), new ArrayList());
        
        // dont need the end anymore
        breakPointTags.remove(new Integer(_msgText.length()));
        
        // now go through the breakpoints and check what other tags are applicable there.
        // the list of tags will contain all tags that are in that set, but it can contain
        // tags that should not be
        for (Iterator iter = breakPointTags.keySet().iterator(); iter.hasNext(); ) {
            Integer bp = (Integer)iter.next();
            List tags = (List)breakPointTags.get(bp);
            for (int i = 0; i < tags.size(); i++) {
                HTMLTag tag = (HTMLTag)tags.get(i);
                while (tag.getParent() != null) {
                    if (!tags.contains(tag.getParent()))
                        tags.add(tag.getParent());
                    tag = tag.getParent();
                }
            }
        }
        
        // iterate across those points, building a new StyleRange out of all tags applicable there
        _styleRanges = new StyleRange[breakPointTags.size()];
        int rangeIndex = 0;
        Iterator iter = breakPointTags.keySet().iterator();
        Integer nextIndex = null;
        for (;;) {
            Integer curBreakPoint = nextIndex;
            if (nextIndex == null) {
                if (iter.hasNext())
                    curBreakPoint = (Integer)iter.next();
                else
                    break;
            }
            nextIndex = null;
            int start = -1;
            int length = 0;
            if (rangeIndex == 0) {
                start = 0;
            } else {
                start = _styleRanges[rangeIndex-1].start + _styleRanges[rangeIndex-1].length;
            }
            if (iter.hasNext()) {
                nextIndex = (Integer)iter.next();
                length = nextIndex.intValue() - start;
            } else {
                length = _msgText.length() - start;
            }
            
            // now trim the set of applicable tags
            List tags = (List)breakPointTags.get(curBreakPoint);
            for (int i = 0; i < tags.size(); i++) {
                HTMLTag tag = (HTMLTag)tags.get(i);
                if ( (tag.getStartIndex() <= start) && (tag.getEndIndex() >= start+length) ) {
                    // ok, applicable
                } else {
                    tags.remove(i);
                    i--;
                }
            }
            
            _styleRanges[rangeIndex] = buildStyle((List)breakPointTags.get(curBreakPoint), start, length);
            rangeIndex++;
        }
    }
    
    public ArrayList getImageIndexes() { return _imageIndexes; }
    public ArrayList getImages() { return _images; }
    public ArrayList getLinkEndIndexes() { return _linkIndexes; }
    public ArrayList getListItemIndexes() { return _listItemIndexes; }
    public ArrayList getFonts() {
        ArrayList rv = new ArrayList(_customFonts);
        rv.add(_fontA);
        rv.add(_fontCODE);
        rv.add(_fontDefault);
        rv.add(_fontH1);
        rv.add(_fontH2);
        rv.add(_fontH3);
        rv.add(_fontH4);
        rv.add(_fontH5);
        rv.add(_fontLI);
        rv.add(_fontP);
        rv.add(_fontPRE);
        return rv;
    }
    
    private void insertCharBreakpoints(char placeholder, Map breakpoints, List indexes) {
        int start = 0;
        for (;;) {
            int index = _msgText.indexOf(placeholder, start);
            if (index == -1) {
                break;
            } else {
                Integer idx = new Integer(index);
                indexes.add(idx);
                List indexTags = (List)breakpoints.get(idx);
                if (indexTags == null) 
                    breakpoints.put(idx, new ArrayList());
                start = index+1;
            }
        }
    }
    
    private StyleRange buildStyle(List tags, int start, int length) {
        System.out.print("building style for [" + start + " through " + (start+length) + "]: ");
        for (int i = 0; i < tags.size(); i++)
            System.out.print(((HTMLTag)tags.get(i)).getName() + " ");
        if (length > 0)
            System.out.println("\n\t" + _msgText.substring(start, start+length));
        else
            System.out.println();
    
        return getStyle(start, length, tags);
    }
    
    public StyleRange[] getStyleRanges() { return _styleRanges; }
    
    private boolean containsTag(List tags, String tagName) {
        for (int i = 0; i < tags.size(); i++)
            if (((HTMLTag)tags.get(i)).getName().equalsIgnoreCase(tagName))
                return true;
        return false;
    }
    
    private StyleRange getStyle(int start, int length, List tags) {
        // turn the given tags into a style
        StyleRange style = new StyleRange();
        style.start = start;
        style.length = length;
        if (containsTag(tags, "h1")) {
            style.font = _fontH1;
        } else if (containsTag(tags, "h2")) {
            style.font = _fontH2;
        } else if (containsTag(tags, "h3")) {
            style.font = _fontH3;
        } else if (containsTag(tags, "h4")) {
            style.font = _fontH4;
        } else if (containsTag(tags, "h5")) {
            style.font = _fontH5;
        } else if (containsTag(tags, "code")) {
            style.font = _fontCODE;
        } else if (containsTag(tags, "pre")) {
            style.font = _fontPRE;
        } else if (containsTag(tags, "li")) {
            style.font = _fontLI;
        } else if (containsTag(tags, "a")) {
            style.font = _fontA;
            style.underline = true;
        } else if (containsTag(tags, "p")) {
            style.font = _fontP;
        }
        
        // these two props (U and SO) are separate from the font, so no trouble
        if (containsTag(tags, "u"))
            style.underline = true;
        if (containsTag(tags, "so"))
            style.strikeout = true;
        
        if (style.font == null)
            style.font = _fontDefault;
        
        int customStyle = 0;
        if (containsTag(tags, "i"))
            customStyle |= SWT.ITALIC;
        if (containsTag(tags, "b") || containsTag(tags, "em"))
            customStyle |= SWT.BOLD;
        
        int sizeModifier = 0;
        // innermost font size is used
        for (int i = 0; i < tags.size(); i++) {
            HTMLTag tag = (HTMLTag)tags.get(i);
            if (tag.getName().equalsIgnoreCase("font")) {
                String sz = tag.getAttribValue("size");
                if (sz != null) {
                    sz = sz.trim();
                    if (sz.startsWith("+") && (sz.length() > 1))
                        sz = sz.substring(1).trim();
                    try {
                        int offset = Integer.parseInt(sz);
                        sizeModifier += offset;
                        break;
                    } catch (NumberFormatException nfe) {
                        // ignore
                    }
                }
            }
        }
        
        if ( (customStyle != 0) || (sizeModifier != 0) ) {
            // ok, we can't use a default font, so lets construct a new one (or use a cached one)
            style.font = buildFont(style.font, customStyle, sizeModifier);
        }
        
        if (containsTag(tags, "img")) {
            System.out.println("Image w/ tags at index " + start + " : " +  tags);
            includeImage(style, tags);
        } else if (_linkIndexes.contains(new Integer(start))) {
            includeLinkEnd(style, tags);
        } else if (_imageIndexes.contains(new Integer(start))) {
            System.out.println("Image at index " + start + " : " +  tags);
            includeImage(style, tags);
        }
        return style;
    }
    
    private void includeImage(StyleRange style, List tags) {
        HTMLTag imgTag = null;
        for (int i = 0; i < tags.size(); i++) {
            HTMLTag tag = (HTMLTag)tags.get(i);
            if (tag.getName().equalsIgnoreCase("img")) {
                imgTag = tag;
                break;
            }
        }
        if (imgTag == null) return;
        SyndieURI imgURI = getURI(imgTag.getAttribValue("src"));
        if (imgURI == null) return;
        
        Long attachmentId = imgURI.getLong("attachment");
        if (attachmentId == null) return;
        
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
            scopeId = _client.getChannelId(scope);
        }
        
        long internalMsgId = _client.getMessageId(scopeId, msgId.longValue());
        byte imgData[] = _client.getMessageAttachmentData(internalMsgId, attachmentId.intValue());
        Image img = new Image(Display.getDefault(), new ByteArrayInputStream(imgData));
        int width = img.getBounds().width;
        int ascent = img.getBounds().height;
        _images.add(img);
        
        int descent = 0;
        style.metrics = new GlyphMetrics(ascent, 0, width);
        //style.background = _imgBGColor;
    }
    
    private void includeLinkEnd(StyleRange style, List tags) {
        HTMLTag aTag = null;
        for (int i = 0; i < tags.size(); i++) {
            HTMLTag tag = (HTMLTag)tags.get(i);
            if (tag.getName().equalsIgnoreCase("a")) {
                aTag = tag;
                break;
            }
        }
        if (aTag == null) return;
        SyndieURI targetURI = getURI(aTag.getAttribValue("href"));
        if (targetURI == null) return;
        
        // perhaps use different icons depending upon who or what is being targetted?
        // e.g. the channel icon for channels or messages, a special archive icon for
        // archives, a special external link icon for generic URLs, a special icon
        // for links with keys on them, etc
        
        Image img = _linkEndIcon;
        int width = img.getBounds().width;
        int ascent = img.getBounds().height;
        _images.add(img);
        
        int descent = 0;
        style.metrics = new GlyphMetrics(ascent, 0, width);
        //style.background = _imgBGColor;
    }
    
    private SyndieURI getURI(String src) {
        if (src == null) return null;
        
        SyndieURI uri = null;
        try {
            return new SyndieURI(src);
        } catch (URISyntaxException use) {
        }
        // not a syndie uri directly, lets try for some variants...
        if (src.startsWith("attachment") && (src.length() > "attachment".length())) {
            String attachmentNum = src.substring("attachment".length()).trim();
            try {
                int num = Integer.parseInt(attachmentNum);
                SyndieURI msgURI = _msg.getURI();
                return SyndieURI.createAttachment(msgURI.getScope(), msgURI.getMessageId().longValue(), num);
            } catch (NumberFormatException nfe) {
                return null;
            }
        }
        if (src.startsWith("page") && (src.length() > "page".length())) {
            String pageNum = src.substring("page".length()).trim();
            try {
                int num = Integer.parseInt(pageNum);
                SyndieURI msgURI = _msg.getURI();
                return SyndieURI.createMessage(msgURI.getScope(), msgURI.getMessageId().longValue(), num);
            } catch (NumberFormatException nfe) {
                return null;
            }
        }
        // not a valid syndie URI, nor is it a relative URI to a page or attachment.
        // treat 'er like a URL then
        return SyndieURI.createURL(src);
    }
    
    /**
     * get the client's font preferences. values should be in the form of
     * "fontname;sizeInPoints;(NORMAL|BOLD|ITALIC|BOLDITALIC)".
     */
    private Properties getFontConfig(DBClient client) {
        Properties rv = new Properties();
        Properties defaultCfg = getDefaultConfig();
        rv.putAll(defaultCfg);
        // now override it with the nym's preferences
        // (aka create a table to store the gui prefs in, and pull 'em out)
        return rv;
    }
    
    private Properties getDefaultConfig() {
        Properties rv = new Properties();
        rv.setProperty("default", "Times;12;NORMAL");
        rv.setProperty("p", "Times;12;NORMAL");
        rv.setProperty("li", "Times;12;NORMAL");
        rv.setProperty("h1", "Times;24;BOLD");
        rv.setProperty("h2", "Times;18;BOLD");
        rv.setProperty("h3", "Times;12;BOLD");
        rv.setProperty("h4", "Times;8;BOLD");
        rv.setProperty("h5", "Times;6;BOLD");
        rv.setProperty("pre", "Courier;12;NORMAL");
        rv.setProperty("code", "Courier;12;NORMAL");
        rv.setProperty("a", "Times;12;NORMAL");
        return rv;
    }
    
    private void buildFonts(Properties fontConfig) {
        String defaultConfig = fontConfig.getProperty("default", "Times;12;NORMAL");
        _fontDefault = buildFont(_fontDefault, fontConfig.getProperty("default"), null);
        _fontP = buildFont(_fontP, fontConfig.getProperty("p"), _fontDefault);
        _fontLI = buildFont(_fontLI, fontConfig.getProperty("li"), _fontDefault);
        _fontH1 = buildFont(_fontH1, fontConfig.getProperty("h1"), _fontDefault);
        _fontH2 = buildFont(_fontH2, fontConfig.getProperty("h2"), _fontDefault);
        _fontH3 = buildFont(_fontH3, fontConfig.getProperty("h3"), _fontDefault);
        _fontH4 = buildFont(_fontH4, fontConfig.getProperty("h4"), _fontDefault);
        _fontH5 = buildFont(_fontH5, fontConfig.getProperty("h5"), _fontDefault);
        _fontPRE = buildFont(_fontPRE, fontConfig.getProperty("pre"), _fontDefault);
        _fontCODE = buildFont(_fontCODE, fontConfig.getProperty("code"), _fontDefault);
        _fontA = buildFont(_fontA, fontConfig.getProperty("a"), _fontDefault);
    }
    
    private Font buildFont(Font oldFont, String config, Font defaultFont) {
        if ( (oldFont != null) && (!oldFont.isDisposed()) ) oldFont.dispose();
        if (config == null) return defaultFont;
        String cfg[] = Constants.split(';', config);
        if ( (cfg == null) || (cfg.length != 3) ) return defaultFont;
        String fontName = cfg[0].trim();
        int fontHeight = 12;
        try {
            fontHeight = Integer.parseInt(cfg[1]);
        } catch (NumberFormatException nfe) {
            // ignore
        }
        int fontStyle = SWT.NORMAL;
        String styleLower = cfg[2].toLowerCase();
        if (styleLower.indexOf("bold") >= 0)
            fontStyle |= SWT.BOLD;
        if (styleLower.indexOf("italic") >= 0)
            fontStyle |= SWT.ITALIC;
        return new Font(Display.getDefault(), fontName, fontHeight, fontStyle);
    }
    
    /**
     * Build a font by adding the specified swt attributes to the given old font.
     * This does not adjust the old font in any way, and may return a cached value
     * from _customFonts
     */
    private Font buildFont(Font oldFont, int swtAttribs, int sizeModifier) {
        FontData oldData[] = null;
        if (!oldFont.isDisposed()) {
            oldData = oldFont.getFontData();
        } else {
            throw new RuntimeException("old font is disposed: " + oldFont.toString());
        }
        FontData newData[] = new FontData[oldData.length];
        for (int i = 0; i < oldData.length; i++)
            newData[i] = new FontData(oldData[i].getName(), getSize(oldData[i].getHeight(), sizeModifier), oldData[i].getStyle() | swtAttribs);
        Font rv = null;
        // search through _customFonts to see if we are already using this combination
        for (int i = 0; i < _customFonts.size(); i++) {
            Font f = (Font)_customFonts.get(i);
            FontData curData[] = f.getFontData();
            if (curData.length == newData.length) {
                boolean diffFound = false;;
                for (int j = 0; !diffFound && j < newData.length; j++) {
                    if (!curData[j].equals(newData[j]))
                        diffFound = true;
                }
                if (!diffFound) {
                    // w00t.  (font reuse)++
                    rv = f;
                    break;
                }
            }
        }
        if (rv == null) {
            rv = new Font(Display.getDefault(), newData);
            _customFonts.add(rv);
        }
        return rv;
    }
    
    private int getSize(int baseSize, int sizeModifier) {
        int rv = baseSize + 2*sizeModifier;
        if (rv <= 4)
            rv = 4;
        if (rv >= 64)
            rv = 64;
        System.out.println("new size: " + rv + " baseSize: " + baseSize + " mod: " + sizeModifier);
        return rv;
    }

    private static void buildDefaultIcons() {
        _linkEndIcon = new Image(Display.getDefault(), 10, 10);
        _linkEndIcon.setBackground(new Color(Display.getDefault(), 255, 0, 0));
    }
    
    public static void main(String args[]) {
        test("<html><body>hi<br />how are you?</body></html>");
        test("<html><body>hi<br />how are you?</html>");
        test("hi<br />how are you?");
        test("a b  c   d\n\n\t\re");
        test("<!-- you can't see me -->now you can<br /><!-- -->la la la");
        test(HTMLStateBuilder.COMPREHENSIVE_TEST);
    }
    
    private static void test(String body) {
        HTMLStateBuilder b = new HTMLStateBuilder(body, null);
        b.buildState();
        String text = b.getAsText();
        System.out.println("parsed: [" + body + "]");
        System.out.println("text: [" + text + "]");
        HTMLStyleBuilder sb = new HTMLStyleBuilder(null, b.getTags(), text, null);
        try {
            sb.buildStyles();
        } catch (Exception e) {
            e.printStackTrace();
            try { Thread.sleep(3000); } catch (InterruptedException ie) {}
            throw new RuntimeException();
        }
        System.out.println("Image indexes: " + sb.getImageIndexes());
        System.out.println("Link end indexes: " + sb.getLinkEndIndexes());
        System.out.println("List item indexes: " + sb.getListItemIndexes());
        StyleRange ranges[] = sb.getStyleRanges();
        for (int i = 0; i < ranges.length; i++)
            System.out.println("range: " + ranges[i]);
    }
}
