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
    private List _styleRanges;
    /** used during state building */
    private boolean _isInComment;
    /** used during state building */
    private boolean _prevWasWhitespace;
    /** list of Tag instances, used during state building */
    private List _activeTags;
    
    private static final Map _charMap = new HashMap();
    static {
        _charMap.put("amp", "&");
        _charMap.put("nbsp", " ");
        _charMap.put("lt", "<");
        _charMap.put("le", "\u1230");
        _charMap.put("eq", "=");
        _charMap.put("ge", "\u2345");
        _charMap.put("gt", ">");
    }
    
    public HTMLStateBuilder(String html, MessageInfo msg) {
        _html = html;
        _msg = msg;
    }

    public void buildState() {
        _activeTags = new ArrayList();
        _styleRanges = new ArrayList();
        int off = 0;
        int len = _html.length();
        
        // heuristic to check for html formatted pages that assume <html><body>
        // this isn't perfect, since the body could be in a comment, but, well...
        String lowerHTML = _html.toLowerCase();
        boolean assumeBody = ( (lowerHTML.indexOf("<body>") == -1) && (lowerHTML.indexOf("<body ") == -1));
        
        System.out.println("unparsed html:\n" + _html);
        
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
            Tag tag = (Tag)_activeTags.get(i);
            receiveTagEnd(tag.getName(), len, body);
        }
        
        _plainText = body.toString();
    }
    
    private boolean tagIsActive(String tagName) {
        for (int i = 0; i < _activeTags.size(); i++) {
            Tag tag = (Tag)_activeTags.get(i);
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
        Tag tag = new Tag(tagContent, bodyIndex);
        _activeTags.add(tag);
        System.out.println("tag parsed: " + tag.toString());
        String tagName = tag.getName();
        if ("br".equals(tagName)) {
            body.append("\n");
            _prevWasWhitespace = true;
        }
    }
    private void receiveTagEnd(StringBuffer content, int bodyIndex, StringBuffer body) {
        receiveTagEnd(content.toString(), bodyIndex, body);
    }
    private void receiveTagEnd(String content, int bodyIndex, StringBuffer body) {
        //System.out.println("Receive tag end: " + content + " [applies to " + bodyIndex + "]");
        Tag tag = new Tag(content, bodyIndex);
        for (int i = _activeTags.size()-1; i >= 0; i--) {
            Tag open = (Tag)_activeTags.get(i);
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
                }
                // should we remove all of the child tags too? 
                // no.  think about: <b><i>bold italic</b>italic not bold</i>
                return;
            }
        }
        System.out.println("tag closed that was never opened: " + tag);
        System.out.println("tags: " + _activeTags);
    }
    
    private class Tag {
        /** tag name, lower case */
        private String _name;
        /** attributes on the tag */
        private Properties _attributes;
        /** start index for the body text that the tag is applicable to */
        private int _startIndex;
        private int _endIndex;
        
        public Tag(String tagBody, int startIndex) {
            _startIndex = startIndex;
            _endIndex = -1;
            _attributes = new Properties();
            int attribNameStart = -1;
            int attribNameEnd = -1;
            int attribValueStart = -1;
            int quoteChar = -1;

            if (tagBody.charAt(0) == '/') // endTag
                tagBody = tagBody.substring(1);
            
            int len = tagBody.length();
            for (int i = 0; i < len; i++) {
                char c = tagBody.charAt(i);
                if (Character.isWhitespace(c)) {
                    if (_name == null) {
                        if (i == 0)
                            _name = "";
                        _name = tagBody.substring(0, i).toLowerCase();
                    } else {
                        if (quoteChar != -1) {
                            // keep going, we are inside a quote
                        } else {
                            if (attribNameStart == -1) {
                                // whitespace outside an attribute.. ignore
                            } else if (attribNameEnd == -1) {
                                // whitespace does terminate an attribute ("href = 'foo'")
                                attribNameEnd = i;
                            } else if (attribValueStart == -1) {
                                // whitespace doesn't start an attribute 
                            } else {
                                // whitespace does terminate an unquoted attribute value though
                                quoteChar = -1;
                                String name = tagBody.substring(attribNameStart, attribNameEnd);
                                String val = tagBody.substring(attribValueStart, i);
                                _attributes.setProperty(name, val);
                                attribNameStart = -1;
                                attribNameEnd = -1;
                                attribValueStart = -1;
                            }
                        }
                    }
                } else if ( (quoteChar != -1) && (quoteChar == c) && (attribValueStart != -1) ) {
                    quoteChar = -1;
                    String name = tagBody.substring(attribNameStart, attribNameEnd);
                    String val = tagBody.substring(attribValueStart, i);
                    _attributes.setProperty(name, val);
                    attribNameStart = -1;
                    attribNameEnd = -1;
                    attribValueStart = -1;
                } else if (_name != null) {
                    // already have our name, so we are parsing attributes
                    if (attribNameStart == -1) {
                        attribNameStart = i;
                    } else if (attribNameEnd == -1) {
                        if (c == '=') {
                            attribNameEnd = i;
                        }
                    } else if (attribValueStart == -1) {
                        if (c == '\'') {
                            quoteChar = c;
                            attribValueStart = i+1;
                        } else if (c == '\"') {
                            quoteChar = c;
                            attribValueStart = i+1;
                        }
                    }
                } else {
                    // name not known, and we haven't reached whitespace yet.  keep going
                }
            } // end looping over the tag body
            if (_name == null)
                _name = tagBody.toLowerCase();
        }
        
        /** lower case tag name */
        public String getName() { return _name; }
        public String getAttribValue(String name) { return _attributes.getProperty(name); }
        public int getStartIndex() { return _startIndex; }
        /** the tag was closed at the given body index */
        public void setEndIndex(int index) { _endIndex = index; }
        public int getEndIndex() { return _endIndex; }
        
        public String toString() {
            StringBuffer rv = new StringBuffer();
            rv.append('<');
            rv.append(_name);
            rv.append(' ');
            for (Iterator iter = _attributes.keySet().iterator(); iter.hasNext(); ) {
                String name = (String)iter.next();
                String val = _attributes.getProperty(name);
                rv.append(name).append('=').append('\'').append(val).append('\'').append(' ');
            }
            rv.append('>');
            rv.append(" applies to [" + _startIndex + (_endIndex >= 0 ? ":" + _endIndex : ":?") + "]");
            return rv.toString();
        }
    }

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
    public StyleRange[] getStyleRanges() {
        StyleRange rv[] = new StyleRange[_styleRanges.size()];
        return (StyleRange[])_styleRanges.toArray(rv);
    }
    
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
    private static final String COMPREHENSIVE_TEST = 
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
