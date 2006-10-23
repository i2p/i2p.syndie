package syndie.gui;

import java.util.*;
import org.eclipse.swt.custom.StyleRange;
import syndie.data.MessageInfo;

/**
 *
 */
class HTMLStateBuilder {
    private String _html;
    private MessageInfo _msg;
    private String _plainText;
    /** used during state building */
    private boolean _isInComment;
    /** used during state building */
    private boolean _prevWasWhitespace;
    /** list of Tag instances, used during state building */
    private List _activeTags;
    /** parsed Tag instances with start/stop ranges */
    private List _closedTags;
    
    private static final Map _charMap = new HashMap();
    private static final Set _noBodyTags = new HashSet();
    static {
        _charMap.put("amp", "&");
        _charMap.put("nbsp", " ");
        _charMap.put("lt", "<");
        _charMap.put("le", "\u8804");
        _charMap.put("ge", "\u8805");
        _charMap.put("eq", "=");
        _charMap.put("gt", ">");
        // a whole lot more... check http://www.w3.org/TR/html401/sgml/entities.html
        
        // tags that don't allow bodies (and should be implicitly closed if not done so explicitly)
        _noBodyTags.add("img");
        _noBodyTags.add("br");
        _noBodyTags.add("hr");
    }
    
    public HTMLStateBuilder(String html, MessageInfo msg) {
        _html = html;
        _msg = msg;
    }

    public void buildState() {
        _activeTags = new ArrayList();
        _closedTags = new ArrayList();
        int off = 0;
        int len = _html.length();
        
        // heuristic to check for html formatted pages that assume <html><body>
        // this isn't perfect, since the body could be in a comment, but, well...
        String lowerHTML = _html.toLowerCase();
        boolean assumeBody = ( (lowerHTML.indexOf("<body>") == -1) && (lowerHTML.indexOf("<body ") == -1));
        
        System.out.println("unparsed html:\n" + _html);
        
        _plainText = parse(off, len, assumeBody);
    }
    
    private String parse(int off, int len, boolean assumeBody) {
        boolean tagStarted = false;
        _prevWasWhitespace = false;
        _isInComment = false;
        StringBuffer tagContent = new StringBuffer();
        StringBuffer body = new StringBuffer();
        for (int i = 0; i < len; i++) {
            char c = _html.charAt(i);
            switch (c) {
                case '<':
                    if (_isInComment)
                        break;
                    if (tagStarted) { // hmm, not really valid (<a href="a<b">), but, well...
                        tagContent.append(c);
                    } else {
                        tagStarted = true;
                        tagContent.setLength(0);
                    }
                    break;
                case '&':
                    if (_isInComment)
                        break;
                    if (tagStarted) {
                        tagContent.append(c);
                        break;
                    } else {
                        // lookahead to see if this is an html character code
                        // (how can we tell?)
                        StringBuffer escapeCode = new StringBuffer();
                        boolean escapeFound = false;
                        for (int j = i+1; j < len; j++) {
                            char ce = _html.charAt(j);
                            if (ce == ';') {
                                body.append(getCharacter(escapeCode.toString()));
                                i = j+1;
                                escapeFound = true;
                                break;
                            } else if (Character.isWhitespace(ce)) { // invalid html, so lets treat it as an &
                                body.append(getCharacter("amp"));
                                _prevWasWhitespace = false;
                                escapeFound = true;
                                break;
                            } else {
                                // we probably want to handle broken html (e.g. terminate with whitespace)
                                escapeCode.append(ce);
                            }
                        }
                        if (!escapeFound) {
                            // if the entire page had no more ';' characters, so definitely invalid
                            body.append(getCharacter("amp"));
                            _prevWasWhitespace = false;
                        }
                        break;
                    }
                case '>':
                    if (_isInComment) {
                        // look to see if we just hit "-->"
                        if ( (i >= 2) && (_html.charAt(i-1) == '-') && (_html.charAt(i-2) == '-') ) {
                            _isInComment = false;
                        }
                        break;
                    }
                    if (tagStarted) {
                        tagStarted = false;
                        if (tagContent.length() == 0) { // <> ?!
                            break;
                        } else if (tagContent.charAt(0) == '/') { // e.g. </p>
                            receiveTagEnd(tagContent, body.length(), body);
                        } else {
                            receiveTagBegin(tagContent, body.length(), body);
                            if (tagContent.charAt(tagContent.length()-1) == '/') { // e.g. <br />
                                receiveTagEnd(tagContent, body.length(), body);
                            }
                        }
                        tagContent.setLength(0);
                        break;
                    } else {
                        // syntax error, skip the char (or should we interpret it as &gt; ?)
                        break;
                    }
                default:
                    if (tagStarted) {
                        tagContent.append(c);
                    } else {
                        // should we make sure that we are in a <body> tag?
                        if ( (assumeBody || tagIsActive("body")) && !tagIsActive("script")) {
                            if (Character.isWhitespace(c)) {
                                boolean pre = tagIsActive("pre");
                                if (_prevWasWhitespace && !pre) {
                                    // ignore dup whitespace
                                } else {
                                    if (pre)
                                        body.append(c);
                                    else
                                        body.append(' ');
                                    _prevWasWhitespace = true;
                                }
                            } else {
                                body.append(c);
                                _prevWasWhitespace = false;
                            }
                        }
                    }
            } // end switch(c)
        } // end loop over (html chars)
        
        // close any unclosed tags
        for (int i = _activeTags.size()-1; i >= 0; i--) {
            HTMLTag tag = (HTMLTag)_activeTags.get(i);
            receiveTagEnd(tag.getName(), body.length(), body);
        }
        
        return body.toString();
    }
    
    private boolean tagIsActive(String tagName) {
        for (int i = 0; i < _activeTags.size(); i++) {
            HTMLTag tag = (HTMLTag)_activeTags.get(i);
            if (tag.getName().equals(tagName))
                return true;
        }
        return false;
    }
    
    private void receiveTagBegin(StringBuffer content, int bodyIndex, StringBuffer body) {
        //System.out.println("Receive tag begin: " + content.toString() + " [applies to " + bodyIndex + "]");
        String tagContent = content.toString();
        if (tagContent.startsWith("!--")) {
            if (tagContent.endsWith("--")) {
                // <!-- -->
                _isInComment = false;
            } else {
                // <!-- <asdf> --> would get receiveTag("-- <asdf")
                _isInComment = true;
            }
            return;
        }
        HTMLTag parent = null;
        if (_activeTags.size() > 0)
            parent = (HTMLTag)_activeTags.get(_activeTags.size()-1);
        HTMLTag tag = new HTMLTag(tagContent, bodyIndex, parent);
        _activeTags.add(tag);
        System.out.println("tag parsed: " + tag.toString());
        String tagName = tag.getName();
        // some tags insert data into the document as soon as they begin (br, img, p, pre, li, etc),
        // while others only insert data when they end or not at all
        if ("br".equals(tagName)) {
            body.append("\n");
            _prevWasWhitespace = true;
        } else if ("img".equals(tagName)) {
            body.append(PLACEHOLDER_IMAGE);
            _prevWasWhitespace = false;
        } else if ("p".equals(tagName)) {
            // make sure the <p>foo</p> starts off with a blank line before it
            if ( (bodyIndex > 1) && (body.charAt(bodyIndex-1) != '\n') && (body.charAt(bodyIndex-2) != '\n') )
                body.append('\n');
        } else if ("pre".equals(tagName)) {
            // make sure the <pre>foo</pre> starts off on a new line
            if ( (bodyIndex > 0) && (body.charAt(bodyIndex-1) != '\n') )
                body.append('\n');
        } else if ("li".equals(tagName)) {
            if ( (bodyIndex > 0) && (body.charAt(bodyIndex-1) != '\n') )
                body.append('\n');
            body.append(PLACEHOLDER_LISTITEM);
            _prevWasWhitespace = false;
        }
        
        if (_noBodyTags.contains(tagName))
            receiveTagEnd(content, body.length(), body);
    }
    
    /** the following character is inserted into the document whenever there should be an image */
    static final char PLACEHOLDER_IMAGE = '\u0001';
    /** the following character is inserted into the document after all links */
    static final char PLACEHOLDER_LINK_END = '\u0002';
    /** the following character is inserted into the document before any list items */
    static final char PLACEHOLDER_LISTITEM = '\u0003';
    
    private void receiveTagEnd(StringBuffer content, int bodyIndex, StringBuffer body) {
        receiveTagEnd(content.toString(), bodyIndex, body);
    }
    private void receiveTagEnd(String content, int bodyIndex, StringBuffer body) {
        //System.out.println("Receive tag end: " + content + " [applies to " + bodyIndex + "]");
        HTMLTag parent = null;
        if (_activeTags.size() > 0)
            parent = (HTMLTag)_activeTags.get(_activeTags.size()-1);
        HTMLTag tag = new HTMLTag(content, bodyIndex, parent);
        for (int i = _activeTags.size()-1; i >= 0; i--) {
            HTMLTag open = (HTMLTag)_activeTags.get(i);
            if (tag.getName().equals(open.getName())) {
                open.setEndIndex(bodyIndex);
                System.out.println("Closing tag " + open.toString());
                _activeTags.remove(i);
                if ("p".equals(tag.getName())) {
                    body.append("\n\n");
                    _prevWasWhitespace = true;
                } else if ("pre".equals(tag.getName())) {
                    body.append("\n");
                    _prevWasWhitespace = true;
                } else if ("h1".equals(tag.getName()) || 
                           "h2".equals(tag.getName()) || 
                           "h3".equals(tag.getName()) || 
                           "h4".equals(tag.getName()) || 
                           "h5".equals(tag.getName())) {
                    body.append("\n\n");
                    _prevWasWhitespace = true;
                } else if ("li".equals(tag.getName())) {
                    body.append("\n");
                    _prevWasWhitespace = true;
                } else if ("a".equals(tag.getName())) {
                    body.append(PLACEHOLDER_LINK_END);
                }
                _closedTags.add(open);
                // should we remove all of the child tags too? 
                // no.  think about: <b><i>bold italic</b>italic not bold</i>
                return;
            }
        }
        System.out.println("tag closed that was never opened: " + tag);
        System.out.println("tags: " + _activeTags);
    }

    /**
     * interpret html character entities (&amp;, &nbsp;, &#0001;, etc)
     * @return string containing the referenced character, or if the reference could not be
     *         decoded, the escape body itself (so the typo &am; would return "&am;")
     */
    private String getCharacter(String escapeBody) {
        String rv = (String)_charMap.get(escapeBody.toLowerCase());
        if (rv != null)
            return rv;
        if (escapeBody.charAt(0) == '#') {
            try {
                int i = Integer.parseInt(escapeBody.substring(1));
                if (i > 0)
                    return Character.toString((char)i);
                // else, fallthrough
            } catch (NumberFormatException nfe) {
                // &#asdf;
                // fallthrough
            }
        } else {
            // not a unicode escape, and not one of the known chars in charMap
            // fallthrough
        }
        return "&" + escapeBody + ";";
    }
    
    public String getAsText() { return _plainText; }
    /** list of parsed HTMLTag instances, specifying their start and end index in the body */
    public List getTags() { return _closedTags; }
    
    public static void main(String args[]) {
        test("<html><body>hi<br />how are you?</body></html>");
        test("<html><body>hi<br />how are you?</html>");
        test("hi<br />how are you?");
        test("a b  c   d\n\n\t\re");
        test("<!-- you can't see me -->now you can<br /><!-- -->la la la");
        test(COMPREHENSIVE_TEST);
    }
    
    private static void test(String body) {
        HTMLStateBuilder b = new HTMLStateBuilder(body, null);
        b.buildState();
        String text = b.getAsText();
        System.out.println("Text: " + text);
    }
    /** try to flex the supported html... */
    static final String COMPREHENSIVE_TEST = 
"<html><body>\n" +
"<p>html test page (links to syndieURIs are invalid, as are pages/images)</p>\n" +
"<b>i am bold</b><br />\n" +
"<i>i am italicized</i><br />\n" +
"<u>i am underlined</u><br />\n" +
"<p>paragraph</p><p>another paragraph</p>\n" +
"<ul>\n" +
" <li>unordered list item</li>\n" +
" <li>another unordered list item</li>\n" +
" <li>unordered list item with a nested list:<ul>\n" +
"  <li>nested list item</li>\n" +
"  <li>another nested list item</li></ul></li>\n" +
" <li>fourth list item</li>\n" +
"</ul>\n" +
"<ol>\n" +
" <li>ordered list item</li>\n" +
" <li>another ordered list item</li>\n" +
" <li>ordered list item with a nested list:<ol>\n" +
"  <li>nested list item</li>\n" +
"  <li>another nested list item</li></ol></li>\n" +
" <li>fourth list item</li>\n" +
"</ol>\n" +
"<h1>heading 1</h1>\n" +
"<h2>heading 2</h2>\n" +
"<h3>heading 3</h3>\n" +
"<h4>heading 4</h4>\n" +
"<h5>heading 5</h5>\n" +
"<center>centered text</center><br />\n" +
"attached image: <img src=\"attachment1\" alt=\"alt text\" /><br />\n" +
"remote image: <img src=\"http://some/uri\" /><br />\n" +
"uri image: <img src=\"syndie:channel:d7:channel44:base64data...\" /><br />\n" +
"<pre>preformatted text\n" +
"la   la   la   \n" +
"   (whee!)</pre>\n" +
"<code>this is code!</code><br />\n" +
"<a href=\"page1\">page 1</a><br />\n" +
"<a href=\"http://some/uri\">some uri</a><br />\n" +
"<a href=\"syndie:channel:d7:channel44:base64data...\">syndie uri</a><br/>\n" +
"<a href=\"attachment1\"><img src=\"attachment1\" /></a><br/>\n" +
"<font size=\"0\">size0</font><br />\n" +
"<font size=\"1\">size+1</font><br />\n" +
"<font size=\"2\">size+2</font><br />\n" +
"<font size=\"3\">size+3</font><br />\n" +
"<font size=\"4\">size+4</font><br />\n" +
"<font size=\"5\">size+5</font><br />\n" +
"<font size=\"-1\">size-1</font><br />\n" +
"<font size=\"-2\">size-2</font><br />\n" +
"<font size=\"-3\">size-3</font><br />\n" +
"<font size=\"-4\">size-4</font><br />\n" +
"<font size=\"-5\">size-5</font><br />\n" +
"</body></html>";
}
