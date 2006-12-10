package syndie.data;

import java.util.*;
import syndie.Constants;
import syndie.db.NullUI;
import syndie.db.UI;
//import syndie.gui.*;

/**
 *
 */
public class HTMLStateBuilder {
    private UI _ui;
    private String _html;
    private String _plainText;
    /** used during state building */
    private boolean _isInComment;
    /** used during state building */
    private boolean _prevWasWhitespace;
    /** list of Tag instances, used during state building */
    private List _activeTags;
    /** parsed Tag instances with start/stop ranges */
    private List _closedTags;
    /** rough attempt to figure out our own line wrapping so we can indent appropriately */
    private int _charsPerLine;
    
    private int _curLine;
    
    private static final Map _charMap = new HashMap();
    private static final Set _noBodyTags = new HashSet();
    private static final Set _noNestTags = new HashSet();
    private static final Set _closeNestedTags = new HashSet();
    static {
        buildEntities(); // populates _charMap
        // tags that don't allow bodies (and should be implicitly closed if not done so explicitly)
        _noBodyTags.add("img");
        _noBodyTags.add("br");
        _noBodyTags.add("hr");
        _noBodyTags.add("meta");
        _noBodyTags.add("link");
        // tags that can't nest (and should be closed implicitly when a new occurence of that same tag is reached)
        _noNestTags.add("p");
        //_noNestTags.add("dd");
        //_noNestTags.add("dt");
        //_noNestTags.add("dl");
        // tags that can't have overlapping elements, so children should all be closed when the tag is closed
        _closeNestedTags.add("dl");
        _closeNestedTags.add("ol");
        _closeNestedTags.add("ul");
        _closeNestedTags.add("td");
        _closeNestedTags.add("tr");
        _closeNestedTags.add("th");
        _closeNestedTags.add("table");
    }
    
    public HTMLStateBuilder(String html) { this(new NullUI(), html, -1); }
    public HTMLStateBuilder(UI ui, String html) { this(ui, html, -1); }
    public HTMLStateBuilder(String html, int charsPerLine) { this(new NullUI(), html, -1); }
    public HTMLStateBuilder(UI ui, String html, int charsPerLine) {
        _ui = ui;
        _html = html;
        _charsPerLine = charsPerLine;
        _curLine = 1;
    }

    public void buildState() {
        _activeTags = new ArrayList();
        _closedTags = new ArrayList();
        int off = 0;
        int len = _html.length();
        
        // heuristic to check for html formatted pages that assume <html><body>
        // this isn't perfect, since the body could be in a comment, but, well...
        String lowerHTML = Constants.lowercase(_html);
        boolean assumeBody = ( (lowerHTML.indexOf("<body>") == -1) && (lowerHTML.indexOf("<body ") == -1));
        
        //System.out.println("unparsed html:\n" + _html);
        
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
            if (c == '\n')
                _curLine++;
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
                                appendBody(body, getCharacter(escapeCode.toString()));
                                i = j;
                                escapeFound = true;
                                break;
                            } else if (Character.isWhitespace(ce)) { // invalid html, so lets treat it as an &
                                appendBody(body, getCharacter("amp"));
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
                            appendBody(body, getCharacter("amp"));
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
                                        appendBody(body, c, true);
                                    else
                                        appendBody(body, ' ', true);
                                    _prevWasWhitespace = true;
                                }
                            } else {
                                appendBody(body, c);
                                _prevWasWhitespace = false;
                            }
                        }
                    }
            } // end switch(c)
        } // end loop over (html chars)
        
        // close any unclosed tags
        for (int i = _activeTags.size()-1; i >= 0; i--) {
            HTMLTag tag = (HTMLTag)_activeTags.get(i);
            receiveTagEnd(tag.name, body.length(), body);
        }
        
        return body.toString();
    }

    /**
     * add the char to the body buffer, inserting an extra newline if the resulting
     * line would be too long
     */
    private void appendBody(StringBuffer body, char c) { appendBody(body, Character.toString(c)); }
    private void appendBody(StringBuffer body, char c, boolean isWhitespace) { appendBody(body, Character.toString(c), isWhitespace); }
    private void appendBody(StringBuffer body, String str) { appendBody(body, str, false); }
    private void appendBody(StringBuffer body, String str, boolean isWhitespace) {
        if ( (_charsPerLine > 0) && (str.indexOf('\n') == -1) && (isWhitespace) ) {
            int lineLen = 0;
            for (int i = body.length()-1; i >= 0; i--) {
                if (body.charAt(i) == '\n')
                    break;
                lineLen++;
            }
            if (lineLen > _charsPerLine) {
                body.append('\n');
                _prevWasWhitespace = true;
                return;
            }
        }
        body.append(str);
    }
    
    private boolean tagIsActive(String tagName) {
        for (int i = 0; i < _activeTags.size(); i++) {
            HTMLTag tag = (HTMLTag)_activeTags.get(i);
            if (tag.name.equals(tagName))
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
        HTMLTag tag = new HTMLTag(tagContent, bodyIndex, parent, _curLine);
        
        String tagName = tag.name;
        // implicitly close any open paragraphs / dds / dts
        if (_noNestTags.contains(tagName)) {
            for (int i = _activeTags.size()-1; i >= 0; i--) {
                HTMLTag c = (HTMLTag)_activeTags.get(i);
                if (tagName.equals(c.name)) {
                    // ok, implicitly close everything in that block
                    // e.g. with "<dl><dt>hi<dd>ho<dt>new def</dl>", the
                    // second <dt> closes both the previous <dt> and the <dd>
                    // this is, shall we say, questionable.
                    if (!"p".equals(tagName)) {
                        //System.out.print("tags underneath " + c + " @ " + bodyIndex + "/" + _curLine + ": ");
                        ArrayList toRemove = new ArrayList();
                        for (int j = _activeTags.size()-1; j > i; j--) 
                            toRemove.add(_activeTags.remove(j));
                        //System.out.println(toRemove.toString());
                        //System.out.println(body.substring(c.getStartIndex(), bodyIndex));
                        for (int j = 0; j < toRemove.size(); j++) {
                            HTMLTag r = (HTMLTag)toRemove.get(j);
                            r.endIndex = bodyIndex;
                            _closedTags.add(r);
                            //System.out.println("implicitly closing nested tag: " + r + " when receiving a new " + tag);
                        }
                        break;
                    } else {
                        _activeTags.remove(i);
                        c.endIndex = bodyIndex;
                        _closedTags.add(c);
                        break;
                    }
                }
            }
        }
        
        _activeTags.add(tag);
        //System.out.println("tag parsed: " + tag.toString());
        // some tags insert data into the document as soon as they begin (br, img, p, pre, li, etc),
        // while others only insert data when they end or not at all
        if ("br".equals(tagName)) {
            appendBody(body, '\n');
            _prevWasWhitespace = true;
        } else if ("img".equals(tagName)) {
            appendBody(body, PLACEHOLDER_IMAGE);
            _prevWasWhitespace = false;
        } else if ("p".equals(tagName)) {
            // make sure the <p>foo</p> starts off with a blank line before it
            if (!isStartOfLine(body, bodyIndex, 1)) {
                appendBody(body, '\n');
                _prevWasWhitespace = true;
            }
        } else if ("tr".equals(tagName)) {
            appendBody(body, '\n');
            _prevWasWhitespace = true;
        //} else if ("td".equals(tagName)) {
        //    appendBody(body, '\n');
        //    _prevWasWhitespace = true;
        } else if ("h1".equals(tagName) || "h2".equals(tagName) || "h3".equals(tagName) || "h4".equals(tagName)) {
            // make sure the <h*>foo</h*> starts off with a blank line before it
            if (!isStartOfLine(body, bodyIndex, 1)) {
                appendBody(body, '\n');
                _prevWasWhitespace = true;
            }
        } else if ("quote".equals(tagName)) {
            // make sure the <quote>foo</quote> starts off with a blank line before it
            if (!isStartOfLine(body, bodyIndex, 1)) {
                appendBody(body, '\n');
                _prevWasWhitespace = true;
            }
        } else if ("pre".equals(tagName)) {
            // make sure the <pre>foo</pre> starts off on a new line
            if (!isStartOfLine(body, bodyIndex)) {
                appendBody(body, '\n');
                _prevWasWhitespace = true;
            }
        } else if ("li".equals(tagName)) {
            if (!isStartOfLine(body, bodyIndex))
                appendBody(body, '\n');
            appendBody(body, PLACEHOLDER_LISTITEM);
            _prevWasWhitespace = true;
        } else if ("dd".equals(tagName)) {
            if (!isStartOfLine(body, bodyIndex))
                appendBody(body, '\n');
            _prevWasWhitespace = true;
        } else if ("dt".equals(tagName)) {
            if (!isStartOfLine(body, bodyIndex))
                appendBody(body, '\n');
            _prevWasWhitespace = true;
        }
        
        // if the html doesn't explicitly close <br>, <img>, <hr>, etc, then do so implicitly
        if ( (_noBodyTags.contains(tagName) || (tagName.startsWith("!"))) && 
             (content.charAt(content.length()-1) != '/') )
            receiveTagEnd(content, body.length(), body);
    }
    
    private boolean isStartOfLine(StringBuffer body, int bodyIndex) { return isStartOfLine(body, bodyIndex, 0); }
    private boolean isStartOfLine(StringBuffer body, int bodyIndex, int numPrecedingBlankLines) {
        int blank = 0;
        for (int i = bodyIndex - 1; i >= 0; i--) {
            char c = body.charAt(i);
            if ( (c == '\n') || (c == '\r') ) {
                if (blank >= numPrecedingBlankLines)
                    return true;
                else
                    blank++;
            } else if (!Character.isWhitespace(c)) {
                return false;
            }
        }
        // beginning of body reached
        return true;
    }
    
    /** the following character is inserted into the document whenever there should be an image */
    public static final char PLACEHOLDER_IMAGE = '\u0001';
    /** the following character is inserted into the document after all links */
    public static final char PLACEHOLDER_LINK_END = '\u0002';
    /** the following character is inserted into the document before any list items */
    public static final char PLACEHOLDER_LISTITEM = '\u0003';
    
    private void receiveTagEnd(StringBuffer content, int bodyIndex, StringBuffer body) {
        receiveTagEnd(content.toString(), bodyIndex, body);
    }
    private void receiveTagEnd(String content, int bodyIndex, StringBuffer body) {
        //System.out.println("Receive tag end: " + content + " [applies to " + bodyIndex + "]");
        HTMLTag parent = null;
        if (_activeTags.size() > 0)
            parent = (HTMLTag)_activeTags.get(_activeTags.size()-1);
        HTMLTag tag = new HTMLTag(content, bodyIndex, parent, _curLine);
        for (int i = _activeTags.size()-1; i >= 0; i--) {
            HTMLTag open = (HTMLTag)_activeTags.get(i);
            if (tag.name.equals(open.name)) {
                open.endIndex = bodyIndex;
                _activeTags.remove(i);
                if ("p".equals(tag.name)) {
                    appendBody(body, '\n');
                    appendBody(body, '\n');
                    _prevWasWhitespace = true;
                } else if ("pre".equals(tag.name)) {
                    appendBody(body, '\n');
                    _prevWasWhitespace = true;
                } else if ("tr".equals(tag.name)) {
                    appendBody(body, '\n');
                    _prevWasWhitespace = true;
                } else if ("td".equals(tag.name)) {
                    appendBody(body, '\n');
                    _prevWasWhitespace = true;
                } else if ("quote".equals(tag.name)) {
                    appendBody(body, '\n');
                    appendBody(body, '\n');
                    _prevWasWhitespace = true;
                } else if ("h1".equals(tag.name) || 
                           "h2".equals(tag.name) || 
                           "h3".equals(tag.name) || 
                           "h4".equals(tag.name) || 
                           "h5".equals(tag.name)) {
                    appendBody(body, '\n');
                    appendBody(body, '\n');
                    _prevWasWhitespace = true;
                } else if ("li".equals(tag.name)) {
                    //body.append("\n");
                    //_prevWasWhitespace = true;
                } else if ("dl".equals(tag.name) ||
                           "ul".equals(tag.name) ||
                           "ol".equals(tag.name)) {
                    appendBody(body, '\n');
                    _prevWasWhitespace = true;
                } else if ("a".equals(tag.name)) {
                    appendBody(body, ' ');
                    appendBody(body, PLACEHOLDER_LINK_END);
                }
                
                if (_closeNestedTags.contains(tag.name)) {
                    for (int j = _activeTags.size()-1; j > i; j--) {
                        HTMLTag nested = (HTMLTag)_activeTags.remove(j);
                        nested.endIndex = bodyIndex;
                        _closedTags.add(nested);
                        _ui.debugMessage("closing nested tag: " + nested + " when receiving tag end of " + open);
                        //_ui.debugMessage("------------------------");
                        //_ui.debugMessage(body.substring(open.startIndex, bodyIndex));
                        //_ui.debugMessage("------------------------");
                    }
                }
                
                _closedTags.add(open);
                //System.out.println("Closing tag " + open.toString());
                // should we remove all of the child tags too? 
                // no.  think about: <b><i>bold italic</b>italic not bold</i>
                return;
            }
        }
        //System.out.println("tag closed that was never opened: " + tag);
        //System.out.println(body.substring(tag.getStartIndex(), bodyIndex));
        //System.out.println("tags: " + _activeTags);
    }

    /**
     * interpret html character entities (&amp;, &nbsp;, &#0001;, etc)
     * @return string containing the referenced character, or if the reference could not be
     *         decoded, the escape body itself (so the typo &am; would return "&am;")
     */
    private String getCharacter(String escapeBody) {
        // note: escapes are case sensitive, so no lowercasing here
        Character rv = (Character)_charMap.get(escapeBody);
        if (rv != null) {
            //System.out.println("Escaping [" + escapeBody + "] and replacing it with [" + rv + "]");
            return rv.toString();
        }
        if (escapeBody.charAt(0) == '#') {
            try {
                int i = Integer.parseInt(escapeBody.substring(1));
                if (i > 0) {
                    //System.out.println("Escaping [" + escapeBody + "] and replacing it with [" + (char)i + "]");
                    return Character.toString((char)i);
                }
                // else, fallthrough
            } catch (NumberFormatException nfe) {
                // &#asdf;
                // fallthrough
                //System.out.println("Escape [" + escapeBody + "] is a bad number");
            }
        } else {
            // not a unicode escape, and not one of the known chars in charMap
            // fallthrough
            //System.out.println("Escape [" + escapeBody + "] is not a number");
        }
        return "&" + escapeBody + ";";
    }
    
    public String getAsText() { return _plainText; }
    /** list of parsed HTMLTag instances, specifying their start and end index in the body */
    public List getTags() { return _closedTags; }

    private static void buildEntities() {

        // from http://www.w3.org/TR/html401/sgml/entities.html
        _charMap.put("AElig", getDecimalChar("0198"));
        _charMap.put("Aacute", getDecimalChar("0193"));
        _charMap.put("Acirc", getDecimalChar("0194"));
        _charMap.put("Agrave", getDecimalChar("0192"));
        _charMap.put("Alpha", getDecimalChar("0913"));
        _charMap.put("Aring", getDecimalChar("0197"));
        _charMap.put("Atilde", getDecimalChar("0195"));
        _charMap.put("Auml", getDecimalChar("0196"));
        _charMap.put("Beta", getDecimalChar("0914"));
        _charMap.put("Ccedil", getDecimalChar("0199"));
        _charMap.put("Chi", getDecimalChar("0935"));
        _charMap.put("Dagger", getDecimalChar("8225"));
        _charMap.put("Delta", getDecimalChar("0916"));
        _charMap.put("ETH", getDecimalChar("0208"));
        _charMap.put("Eacute", getDecimalChar("0201"));
        _charMap.put("Ecirc", getDecimalChar("0202"));
        _charMap.put("Egrave", getDecimalChar("0200"));
        _charMap.put("Epsilon", getDecimalChar("0917"));
        _charMap.put("Eta", getDecimalChar("0919"));
        _charMap.put("Euml", getDecimalChar("0203"));
        _charMap.put("Gamma", getDecimalChar("0915"));
        _charMap.put("Iacute", getDecimalChar("0205"));
        _charMap.put("Icirc", getDecimalChar("0206"));
        _charMap.put("Igrave", getDecimalChar("0204"));
        _charMap.put("Iota", getDecimalChar("0921"));
        _charMap.put("Iuml", getDecimalChar("0207"));
        _charMap.put("Kappa", getDecimalChar("0922"));
        _charMap.put("Lambda", getDecimalChar("0923"));
        _charMap.put("Ntilde", getDecimalChar("0209"));
        _charMap.put("OElig", getDecimalChar("0338"));
        _charMap.put("Oacute", getDecimalChar("0211"));
        _charMap.put("Ocirc", getDecimalChar("0212"));
        _charMap.put("Ograve", getDecimalChar("0210"));
        _charMap.put("Omega", getDecimalChar("0937"));
        _charMap.put("Omicron", getDecimalChar("0927"));
        _charMap.put("Oslash", getDecimalChar("0216"));
        _charMap.put("Otilde", getDecimalChar("0213"));
        _charMap.put("Ouml", getDecimalChar("0214"));
        _charMap.put("Phi", getDecimalChar("0934"));
        _charMap.put("Prime", getDecimalChar("8243"));
        _charMap.put("Psi", getDecimalChar("0936"));
        _charMap.put("Rho", getDecimalChar("0929"));
        _charMap.put("Scaron", getDecimalChar("0352"));
        _charMap.put("Sigma", getDecimalChar("0931"));
        _charMap.put("THORN", getDecimalChar("0222"));
        _charMap.put("Tau", getDecimalChar("0932"));
        _charMap.put("Theta", getDecimalChar("0920"));
        _charMap.put("Uacute", getDecimalChar("0218"));
        _charMap.put("Ucirc", getDecimalChar("0219"));
        _charMap.put("Ugrave", getDecimalChar("0217"));
        _charMap.put("Upsilon", getDecimalChar("0933"));
        _charMap.put("Uuml", getDecimalChar("0220"));
        _charMap.put("Yacute", getDecimalChar("0221"));
        _charMap.put("Yuml", getDecimalChar("0376"));
        _charMap.put("Zeta", getDecimalChar("0918"));
        _charMap.put("aacute", getDecimalChar("0225"));
        _charMap.put("acirc", getDecimalChar("0226"));
        _charMap.put("acute", getDecimalChar("0180"));
        _charMap.put("aelig", getDecimalChar("0230"));
        _charMap.put("agrave", getDecimalChar("0224"));
        _charMap.put("alefsym", getDecimalChar("8501"));
        _charMap.put("alpha", getDecimalChar("0945"));
        _charMap.put("amp", getDecimalChar("0038"));
        _charMap.put("and", getDecimalChar("8743"));
        _charMap.put("ang", getDecimalChar("8736"));
        _charMap.put("aring", getDecimalChar("0229"));
        _charMap.put("asymp", getDecimalChar("8776"));
        _charMap.put("atilde", getDecimalChar("0227"));
        _charMap.put("auml", getDecimalChar("0228"));
        _charMap.put("bdquo", getDecimalChar("8222"));
        _charMap.put("beta", getDecimalChar("0946"));
        _charMap.put("brvbar", getDecimalChar("0166"));
        _charMap.put("bull", getDecimalChar("8226"));
        _charMap.put("cap", getDecimalChar("8745"));
        _charMap.put("ccedil", getDecimalChar("0231"));
        _charMap.put("cedil", getDecimalChar("0184"));
        _charMap.put("cent", getDecimalChar("0162"));
        _charMap.put("chi", getDecimalChar("0967"));
        _charMap.put("circ", getDecimalChar("0710"));
        _charMap.put("clubs", getDecimalChar("9827"));
        _charMap.put("cong", getDecimalChar("8773"));
        _charMap.put("copy", getDecimalChar("0169"));
        _charMap.put("crarr", getDecimalChar("8629"));
        _charMap.put("cup", getDecimalChar("8746"));
        _charMap.put("curren", getDecimalChar("0164"));
        _charMap.put("dArr", getDecimalChar("8659"));
        _charMap.put("dagger", getDecimalChar("8224"));
        _charMap.put("darr", getDecimalChar("8595"));
        _charMap.put("deg", getDecimalChar("0176"));
        _charMap.put("delta", getDecimalChar("0948"));
        _charMap.put("diams", getDecimalChar("9830"));
        _charMap.put("divide", getDecimalChar("0247"));
        _charMap.put("eacute", getDecimalChar("0233"));
        _charMap.put("ecirc", getDecimalChar("0234"));
        _charMap.put("egrave", getDecimalChar("0232"));
        _charMap.put("empty", getDecimalChar("8709"));
        _charMap.put("emsp", getDecimalChar("8195"));
        _charMap.put("ensp", getDecimalChar("8194"));
        _charMap.put("epsilon", getDecimalChar("0949"));
        _charMap.put("equiv", getDecimalChar("8801"));
        _charMap.put("eta", getDecimalChar("0951"));
        _charMap.put("eth", getDecimalChar("0240"));
        _charMap.put("euml", getDecimalChar("0235"));
        _charMap.put("euro", getDecimalChar("8364"));
        _charMap.put("exist", getDecimalChar("8707"));
        _charMap.put("fnof", getDecimalChar("0402"));
        _charMap.put("forall", getDecimalChar("8704"));
        _charMap.put("frac12", getDecimalChar("0189"));
        _charMap.put("frac14", getDecimalChar("0188"));
        _charMap.put("frac34", getDecimalChar("0190"));
        _charMap.put("frasl", getDecimalChar("8260"));
        _charMap.put("gamma", getDecimalChar("0947"));
        _charMap.put("gt", getDecimalChar("0062"));
        _charMap.put("hArr", getDecimalChar("8660"));
        _charMap.put("harr", getDecimalChar("8596"));
        _charMap.put("hearts", getDecimalChar("9829"));
        _charMap.put("hellip", getDecimalChar("8230"));
        _charMap.put("iacute", getDecimalChar("0237"));
        _charMap.put("icirc", getDecimalChar("0238"));
        _charMap.put("iexcl", getDecimalChar("0161"));
        _charMap.put("igrave", getDecimalChar("0236"));
        _charMap.put("image", getDecimalChar("8465"));
        _charMap.put("infin", getDecimalChar("8734"));
        _charMap.put("int", getDecimalChar("8747"));
        _charMap.put("iota", getDecimalChar("0953"));
        _charMap.put("iquest", getDecimalChar("0191"));
        _charMap.put("isin", getDecimalChar("8712"));
        _charMap.put("iuml", getDecimalChar("0239"));
        _charMap.put("kappa", getDecimalChar("0954"));
        _charMap.put("lArr", getDecimalChar("8656"));
        _charMap.put("lambda", getDecimalChar("0955"));
        _charMap.put("lang", getDecimalChar("9001"));
        _charMap.put("laquo", getDecimalChar("0171"));
        _charMap.put("larr", getDecimalChar("8592"));
        _charMap.put("lceil", getDecimalChar("8968"));
        _charMap.put("ldquo", getDecimalChar("8220"));
        _charMap.put("lfloor", getDecimalChar("8970"));
        _charMap.put("lowast", getDecimalChar("8727"));
        _charMap.put("loz", getDecimalChar("9674"));
        _charMap.put("lrm", getDecimalChar("8206"));
        _charMap.put("lsaquo", getDecimalChar("8249"));
        _charMap.put("lsquo", getDecimalChar("8216"));
        _charMap.put("lt", getDecimalChar("0060"));
        _charMap.put("macr", getDecimalChar("0175"));
        _charMap.put("mdash", getDecimalChar("8212"));
        _charMap.put("micro", getDecimalChar("0181"));
        _charMap.put("middot", getDecimalChar("0183"));
        _charMap.put("minus", getDecimalChar("8722"));
        _charMap.put("nabla", getDecimalChar("8711"));
        _charMap.put("nbsp", getDecimalChar("0160"));
        _charMap.put("ndash", getDecimalChar("8211"));
        _charMap.put("not", getDecimalChar("0172"));
        _charMap.put("notin", getDecimalChar("8713"));
        _charMap.put("nsub", getDecimalChar("8836"));
        _charMap.put("ntilde", getDecimalChar("0241"));
        _charMap.put("oacute", getDecimalChar("0243"));
        _charMap.put("ocirc", getDecimalChar("0244"));
        _charMap.put("oelig", getDecimalChar("0339"));
        _charMap.put("ograve", getDecimalChar("0242"));
        _charMap.put("oline", getDecimalChar("8254"));
        _charMap.put("omega", getDecimalChar("0969"));
        _charMap.put("omicron", getDecimalChar("0959"));
        _charMap.put("oplus", getDecimalChar("8853"));
        _charMap.put("ordf", getDecimalChar("0170"));
        _charMap.put("ordm", getDecimalChar("0186"));
        _charMap.put("oslash", getDecimalChar("0248"));
        _charMap.put("otilde", getDecimalChar("0245"));
        _charMap.put("otimes", getDecimalChar("8855"));
        _charMap.put("ouml", getDecimalChar("0246"));
        _charMap.put("para", getDecimalChar("0182"));
        _charMap.put("part", getDecimalChar("8706"));
        _charMap.put("permil", getDecimalChar("8240"));
        _charMap.put("perp", getDecimalChar("8869"));
        _charMap.put("phi", getDecimalChar("0966"));
        _charMap.put("piv", getDecimalChar("0982"));
        _charMap.put("plusmn", getDecimalChar("0177"));
        _charMap.put("pound", getDecimalChar("0163"));
        _charMap.put("prime", getDecimalChar("8242"));
        _charMap.put("prod", getDecimalChar("8719"));
        _charMap.put("prop", getDecimalChar("8733"));
        _charMap.put("psi", getDecimalChar("0968"));
        _charMap.put("quot", getDecimalChar("0034"));
        _charMap.put("rArr", getDecimalChar("8658"));
        _charMap.put("radic", getDecimalChar("8730"));
        _charMap.put("rang", getDecimalChar("9002"));
        _charMap.put("raquo", getDecimalChar("0187"));
        _charMap.put("rarr", getDecimalChar("8594"));
        _charMap.put("rceil", getDecimalChar("8969"));
        _charMap.put("rdquo", getDecimalChar("8221"));
        _charMap.put("real", getDecimalChar("8476"));
        _charMap.put("reg", getDecimalChar("0174"));
        _charMap.put("rfloor", getDecimalChar("8971"));
        _charMap.put("rho", getDecimalChar("0961"));
        _charMap.put("rlm", getDecimalChar("8207"));
        _charMap.put("rsaquo", getDecimalChar("8250"));
        _charMap.put("rsquo", getDecimalChar("8217"));
        _charMap.put("sbquo", getDecimalChar("8218"));
        _charMap.put("scaron", getDecimalChar("0353"));
        _charMap.put("sdot", getDecimalChar("8901"));
        _charMap.put("sect", getDecimalChar("0167"));
        _charMap.put("shy", getDecimalChar("0173"));
        _charMap.put("sigma", getDecimalChar("0963"));
        _charMap.put("sigmaf", getDecimalChar("0962"));
        _charMap.put("sim", getDecimalChar("8764"));
        _charMap.put("spades", getDecimalChar("9824"));
        _charMap.put("sub", getDecimalChar("8834"));
        _charMap.put("sube", getDecimalChar("8838"));
        _charMap.put("sum", getDecimalChar("8721"));
        _charMap.put("sup", getDecimalChar("8835"));
        _charMap.put("sup1", getDecimalChar("0185"));
        _charMap.put("sup2", getDecimalChar("0178"));
        _charMap.put("sup3", getDecimalChar("0179"));
        _charMap.put("supe", getDecimalChar("8839"));
        _charMap.put("szlig", getDecimalChar("0223"));
        _charMap.put("tau", getDecimalChar("0964"));
        _charMap.put("there4", getDecimalChar("8756"));
        _charMap.put("theta", getDecimalChar("0952"));
        _charMap.put("thetasym", getDecimalChar("0977"));
        _charMap.put("thinsp", getDecimalChar("8201"));
        _charMap.put("thorn", getDecimalChar("0254"));
        _charMap.put("tilde", getDecimalChar("0732"));
        _charMap.put("times", getDecimalChar("0215"));
        _charMap.put("trade", getDecimalChar("8482"));
        _charMap.put("uArr", getDecimalChar("8657"));
        _charMap.put("uacute", getDecimalChar("0250"));
        _charMap.put("uarr", getDecimalChar("8593"));
        _charMap.put("ucirc", getDecimalChar("0251"));
        _charMap.put("ugrave", getDecimalChar("0249"));
        _charMap.put("uml", getDecimalChar("0168"));
        _charMap.put("upsih", getDecimalChar("0978"));
        _charMap.put("upsilon", getDecimalChar("0965"));
        _charMap.put("uuml", getDecimalChar("0252"));
        _charMap.put("weierp", getDecimalChar("8472"));
        _charMap.put("yacute", getDecimalChar("0253"));
        _charMap.put("yen", getDecimalChar("0165"));
        _charMap.put("yuml", getDecimalChar("0255"));
        _charMap.put("zeta", getDecimalChar("0950"));
        _charMap.put("zwj", getDecimalChar("8205"));
        _charMap.put("zwnj", getDecimalChar("8204"));
        _charMap.put("Mu", getDecimalChar("0924"));
        _charMap.put("Mu", getDecimalChar("0924"));
        _charMap.put("Nu", getDecimalChar("0925"));
        _charMap.put("Pi", getDecimalChar("0928"));
        _charMap.put("Xi", getDecimalChar("0926"));
        _charMap.put("ge", getDecimalChar("8805"));
        _charMap.put("le", getDecimalChar("8804"));
        _charMap.put("ne", getDecimalChar("8800"));
        _charMap.put("nu", getDecimalChar("0957"));
        _charMap.put("pi", getDecimalChar("0960"));
        _charMap.put("xi", getDecimalChar("0958"));
        _charMap.put("or", getDecimalChar("8744"));
        _charMap.put("ni", getDecimalChar("8715"));
        _charMap.put("mu", getDecimalChar("0956"));        
    }
    
    private static final Character getDecimalChar(String decimal) {
        try {
            int i = Integer.parseInt(decimal, 10);
            return new Character((char)i);
        } catch (NumberFormatException nfe) {
            throw new RuntimeException("wtf: " + decimal);
        }
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
        HTMLStateBuilder b = new HTMLStateBuilder(body);
        b.buildState();
        String text = b.getAsText();
        System.out.println("Text: " + text);
    }
    /** try to flex the supported html... */
    public static final String COMPREHENSIVE_TEST = 
"<html><body bgimage=\"attachment0\">\n" +
"<p>html test page (links to syndieURIs are invalid, as are pages/images)</p>\n" +
"<b>i am bold</b><br />\n" +
"<i>i am italicized</i><br />\n" +
"<u>i am underlined</u><br />\n" +
"<so>i am struck out</so><br />\n" +
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
"<p bgcolor=\"red\" color=\"blue\" font=\"Times\">times in blue on a red background</p>\n" +
"<quote>this is a quote</quote>\n" +
"</body></html>";
}
