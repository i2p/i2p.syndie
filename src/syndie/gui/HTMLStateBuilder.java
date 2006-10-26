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
    /** rough attempt to figure out our own line wrapping so we can indent appropriately */
    private int _charsPerLine;
    
    private static final Map _charMap = new HashMap();
    private static final Set _noBodyTags = new HashSet();
    static {
        buildEntities(); // populates _charMap
        // tags that don't allow bodies (and should be implicitly closed if not done so explicitly)
        _noBodyTags.add("img");
        _noBodyTags.add("br");
        _noBodyTags.add("hr");
    }
    
    public HTMLStateBuilder(String html, MessageInfo msg) { this(html, msg, -1); }
    public HTMLStateBuilder(String html, MessageInfo msg, int charsPerLine) {
        _html = html;
        _msg = msg;
        _charsPerLine = charsPerLine;
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
                                appendBody(body, getCharacter(escapeCode.toString()));
                                i = j+1;
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
            receiveTagEnd(tag.getName(), body.length(), body);
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
        //System.out.println("tag parsed: " + tag.toString());
        String tagName = tag.getName();
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
            if ( (bodyIndex > 1) && (body.charAt(bodyIndex-1) != '\n') && (body.charAt(bodyIndex-2) != '\n') ) {
                appendBody(body, '\n');
                _prevWasWhitespace = true;
            }
        } else if ("quote".equals(tagName)) {
            // make sure the <quote>foo</quote> starts off with a blank line before it
            if ( (bodyIndex > 1) && (body.charAt(bodyIndex-1) != '\n') && (body.charAt(bodyIndex-2) != '\n') ) {
                appendBody(body, '\n');
                _prevWasWhitespace = true;
            }
        } else if ("pre".equals(tagName)) {
            // make sure the <pre>foo</pre> starts off on a new line
            if ( (bodyIndex > 0) && (body.charAt(bodyIndex-1) != '\n') ) {
                appendBody(body, '\n');
                _prevWasWhitespace = true;
            }
        } else if ("li".equals(tagName)) {
            if ( (bodyIndex > 0) && (body.charAt(bodyIndex-1) != '\n') )
                appendBody(body, '\n');
            appendBody(body, PLACEHOLDER_LISTITEM);
            _prevWasWhitespace = true;
        }
        
        // if the html doesn't explicitly close <br>, <img>, <hr>, etc, then do so implicitly
        if (_noBodyTags.contains(tagName) && (content.charAt(content.length()-1) != '/'))
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
                _activeTags.remove(i);
                if ("p".equals(tag.getName())) {
                    appendBody(body, '\n');
                    appendBody(body, '\n');
                    _prevWasWhitespace = true;
                } else if ("pre".equals(tag.getName())) {
                    appendBody(body, '\n');
                    _prevWasWhitespace = true;
                } else if ("quote".equals(tag.getName())) {
                    appendBody(body, '\n');
                    appendBody(body, '\n');
                    _prevWasWhitespace = true;
                } else if ("h1".equals(tag.getName()) || 
                           "h2".equals(tag.getName()) || 
                           "h3".equals(tag.getName()) || 
                           "h4".equals(tag.getName()) || 
                           "h5".equals(tag.getName())) {
                    appendBody(body, '\n');
                    appendBody(body, '\n');
                    _prevWasWhitespace = true;
                } else if ("li".equals(tag.getName())) {
                    //body.append("\n");
                    //_prevWasWhitespace = true;
                } else if ("ul".equals(tag.getName())) {
                    appendBody(body, '\n');
                    _prevWasWhitespace = true;
                } else if ("ol".equals(tag.getName())) {
                    appendBody(body, '\n');
                    _prevWasWhitespace = true;
                } else if ("a".equals(tag.getName())) {
                    appendBody(body, ' ');
                    appendBody(body, PLACEHOLDER_LINK_END);
                }
                _closedTags.add(open);
                System.out.println("Closing tag " + open.toString());
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

    private static void buildEntities() {

        // from http://www.w3.org/TR/html401/sgml/entities.html
        _charMap.put("AElig", "\u0198");
        _charMap.put("Aacute", "\u0193");
        _charMap.put("Acirc", "\u0194");
        _charMap.put("Agrave", "\u0192");
        _charMap.put("Alpha", "\u0913");
        _charMap.put("Aring", "\u0197");
        _charMap.put("Atilde", "\u0195");
        _charMap.put("Auml", "\u0196");
        _charMap.put("Beta", "\u0914");
        _charMap.put("Ccedil", "\u0199");
        _charMap.put("Chi", "\u0935");
        _charMap.put("Dagger", "\u8225");
        _charMap.put("Delta", "\u0916");
        _charMap.put("ETH", "\u0208");
        _charMap.put("Eacute", "\u0201");
        _charMap.put("Ecirc", "\u0202");
        _charMap.put("Egrave", "\u0200");
        _charMap.put("Epsilon", "\u0917");
        _charMap.put("Eta", "\u0919");
        _charMap.put("Euml", "\u0203");
        _charMap.put("Gamma", "\u0915");
        _charMap.put("Iacute", "\u0205");
        _charMap.put("Icirc", "\u0206");
        _charMap.put("Igrave", "\u0204");
        _charMap.put("Iota", "\u0921");
        _charMap.put("Iuml", "\u0207");
        _charMap.put("Kappa", "\u0922");
        _charMap.put("Lambda", "\u0923");
        _charMap.put("Ntilde", "\u0209");
        _charMap.put("OElig", "\u0338");
        _charMap.put("Oacute", "\u0211");
        _charMap.put("Ocirc", "\u0212");
        _charMap.put("Ograve", "\u0210");
        _charMap.put("Omega", "\u0937");
        _charMap.put("Omicron", "\u0927");
        _charMap.put("Oslash", "\u0216");
        _charMap.put("Otilde", "\u0213");
        _charMap.put("Ouml", "\u0214");
        _charMap.put("Phi", "\u0934");
        _charMap.put("Prime", "\u8243");
        _charMap.put("Psi", "\u0936");
        _charMap.put("Rho", "\u0929");
        _charMap.put("Scaron", "\u0352");
        _charMap.put("Sigma", "\u0931");
        _charMap.put("THORN", "\u0222");
        _charMap.put("Tau", "\u0932");
        _charMap.put("Theta", "\u0920");
        _charMap.put("Uacute", "\u0218");
        _charMap.put("Ucirc", "\u0219");
        _charMap.put("Ugrave", "\u0217");
        _charMap.put("Upsilon", "\u0933");
        _charMap.put("Uuml", "\u0220");
        _charMap.put("Yacute", "\u0221");
        _charMap.put("Yuml", "\u0376");
        _charMap.put("Zeta", "\u0918");
        _charMap.put("aacute", "\u0225");
        _charMap.put("acirc", "\u0226");
        _charMap.put("acute", "\u0180");
        _charMap.put("aelig", "\u0230");
        _charMap.put("agrave", "\u0224");
        _charMap.put("alefsym", "\u8501");
        _charMap.put("alpha", "\u0945");
        _charMap.put("amp", "\u0038");
        _charMap.put("and", "\u8743");
        _charMap.put("ang", "\u8736");
        _charMap.put("aring", "\u0229");
        _charMap.put("asymp", "\u8776");
        _charMap.put("atilde", "\u0227");
        _charMap.put("auml", "\u0228");
        _charMap.put("bdquo", "\u8222");
        _charMap.put("beta", "\u0946");
        _charMap.put("brvbar", "\u0166");
        _charMap.put("bull", "\u8226");
        _charMap.put("cap", "\u8745");
        _charMap.put("ccedil", "\u0231");
        _charMap.put("cedil", "\u0184");
        _charMap.put("cent", "\u0162");
        _charMap.put("chi", "\u0967");
        _charMap.put("circ", "\u0710");
        _charMap.put("clubs", "\u9827");
        _charMap.put("cong", "\u8773");
        _charMap.put("copy", "\u0169");
        _charMap.put("crarr", "\u8629");
        _charMap.put("cup", "\u8746");
        _charMap.put("curren", "\u0164");
        _charMap.put("dArr", "\u8659");
        _charMap.put("dagger", "\u8224");
        _charMap.put("darr", "\u8595");
        _charMap.put("deg", "\u0176");
        _charMap.put("delta", "\u0948");
        _charMap.put("diams", "\u9830");
        _charMap.put("divide", "\u0247");
        _charMap.put("eacute", "\u0233");
        _charMap.put("ecirc", "\u0234");
        _charMap.put("egrave", "\u0232");
        _charMap.put("empty", "\u8709");
        _charMap.put("emsp", "\u8195");
        _charMap.put("ensp", "\u8194");
        _charMap.put("epsilon", "\u0949");
        _charMap.put("equiv", "\u8801");
        _charMap.put("eta", "\u0951");
        _charMap.put("eth", "\u0240");
        _charMap.put("euml", "\u0235");
        _charMap.put("euro", "\u8364");
        _charMap.put("exist", "\u8707");
        _charMap.put("fnof", "\u0402");
        _charMap.put("forall", "\u8704");
        _charMap.put("frac12", "\u0189");
        _charMap.put("frac14", "\u0188");
        _charMap.put("frac34", "\u0190");
        _charMap.put("frasl", "\u8260");
        _charMap.put("gamma", "\u0947");
        _charMap.put("gt", "\u0062");
        _charMap.put("hArr", "\u8660");
        _charMap.put("harr", "\u8596");
        _charMap.put("hearts", "\u9829");
        _charMap.put("hellip", "\u8230");
        _charMap.put("iacute", "\u0237");
        _charMap.put("icirc", "\u0238");
        _charMap.put("iexcl", "\u0161");
        _charMap.put("igrave", "\u0236");
        _charMap.put("image", "\u8465");
        _charMap.put("infin", "\u8734");
        _charMap.put("int", "\u8747");
        _charMap.put("iota", "\u0953");
        _charMap.put("iquest", "\u0191");
        _charMap.put("isin", "\u8712");
        _charMap.put("iuml", "\u0239");
        _charMap.put("kappa", "\u0954");
        _charMap.put("lArr", "\u8656");
        _charMap.put("lambda", "\u0955");
        _charMap.put("lang", "\u9001");
        _charMap.put("laquo", "\u0171");
        _charMap.put("larr", "\u8592");
        _charMap.put("lceil", "\u8968");
        _charMap.put("ldquo", "\u8220");
        _charMap.put("lfloor", "\u8970");
        _charMap.put("lowast", "\u8727");
        _charMap.put("loz", "\u9674");
        _charMap.put("lrm", "\u8206");
        _charMap.put("lsaquo", "\u8249");
        _charMap.put("lsquo", "\u8216");
        _charMap.put("lt", "\u0060");
        _charMap.put("macr", "\u0175");
        _charMap.put("mdash", "\u8212");
        _charMap.put("micro", "\u0181");
        _charMap.put("middot", "\u0183");
        _charMap.put("minus", "\u8722");
        _charMap.put("nabla", "\u8711");
        _charMap.put("nbsp", "\u0160");
        _charMap.put("ndash", "\u8211");
        _charMap.put("not", "\u0172");
        _charMap.put("notin", "\u8713");
        _charMap.put("nsub", "\u8836");
        _charMap.put("ntilde", "\u0241");
        _charMap.put("oacute", "\u0243");
        _charMap.put("ocirc", "\u0244");
        _charMap.put("oelig", "\u0339");
        _charMap.put("ograve", "\u0242");
        _charMap.put("oline", "\u8254");
        _charMap.put("omega", "\u0969");
        _charMap.put("omicron", "\u0959");
        _charMap.put("oplus", "\u8853");
        _charMap.put("ordf", "\u0170");
        _charMap.put("ordm", "\u0186");
        _charMap.put("oslash", "\u0248");
        _charMap.put("otilde", "\u0245");
        _charMap.put("otimes", "\u8855");
        _charMap.put("ouml", "\u0246");
        _charMap.put("para", "\u0182");
        _charMap.put("part", "\u8706");
        _charMap.put("permil", "\u8240");
        _charMap.put("perp", "\u8869");
        _charMap.put("phi", "\u0966");
        _charMap.put("piv", "\u0982");
        _charMap.put("plusmn", "\u0177");
        _charMap.put("pound", "\u0163");
        _charMap.put("prime", "\u8242");
        _charMap.put("prod", "\u8719");
        _charMap.put("prop", "\u8733");
        _charMap.put("psi", "\u0968");
        _charMap.put("quot", "\u0034");
        _charMap.put("rArr", "\u8658");
        _charMap.put("radic", "\u8730");
        _charMap.put("rang", "\u9002");
        _charMap.put("raquo", "\u0187");
        _charMap.put("rarr", "\u8594");
        _charMap.put("rceil", "\u8969");
        _charMap.put("rdquo", "\u8221");
        _charMap.put("real", "\u8476");
        _charMap.put("reg", "\u0174");
        _charMap.put("rfloor", "\u8971");
        _charMap.put("rho", "\u0961");
        _charMap.put("rlm", "\u8207");
        _charMap.put("rsaquo", "\u8250");
        _charMap.put("rsquo", "\u8217");
        _charMap.put("sbquo", "\u8218");
        _charMap.put("scaron", "\u0353");
        _charMap.put("sdot", "\u8901");
        _charMap.put("sect", "\u0167");
        _charMap.put("shy", "\u0173");
        _charMap.put("sigma", "\u0963");
        _charMap.put("sigmaf", "\u0962");
        _charMap.put("sim", "\u8764");
        _charMap.put("spades", "\u9824");
        _charMap.put("sub", "\u8834");
        _charMap.put("sube", "\u8838");
        _charMap.put("sum", "\u8721");
        _charMap.put("sup", "\u8835");
        _charMap.put("sup1", "\u0185");
        _charMap.put("sup2", "\u0178");
        _charMap.put("sup3", "\u0179");
        _charMap.put("supe", "\u8839");
        _charMap.put("szlig", "\u0223");
        _charMap.put("tau", "\u0964");
        _charMap.put("there4", "\u8756");
        _charMap.put("theta", "\u0952");
        _charMap.put("thetasym", "\u0977");
        _charMap.put("thinsp", "\u8201");
        _charMap.put("thorn", "\u0254");
        _charMap.put("tilde", "\u0732");
        _charMap.put("times", "\u0215");
        _charMap.put("trade", "\u8482");
        _charMap.put("uArr", "\u8657");
        _charMap.put("uacute", "\u0250");
        _charMap.put("uarr", "\u8593");
        _charMap.put("ucirc", "\u0251");
        _charMap.put("ugrave", "\u0249");
        _charMap.put("uml", "\u0168");
        _charMap.put("upsih", "\u0978");
        _charMap.put("upsilon", "\u0965");
        _charMap.put("uuml", "\u0252");
        _charMap.put("weierp", "\u8472");
        _charMap.put("yacute", "\u0253");
        _charMap.put("yen", "\u0165");
        _charMap.put("yuml", "\u0255");
        _charMap.put("zeta", "\u0950");
        _charMap.put("zwj", "\u8205");
        _charMap.put("zwnj", "\u8204");
        _charMap.put("Mu", "\u0924");
        _charMap.put("Mu", "\u0924");
        _charMap.put("Nu", "\u0925");
        _charMap.put("Pi", "\u0928");
        _charMap.put("Xi", "\u0926");
        _charMap.put("ge", "\u8805");
        _charMap.put("le", "\u8804");
        _charMap.put("ne", "\u8800");
        _charMap.put("nu", "\u0957");
        _charMap.put("pi", "\u0960");
        _charMap.put("xi", "\u0958");
        _charMap.put("or", "\u8744");
        _charMap.put("ni", "\u8715");
        _charMap.put("mu", "\u0956");        
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
