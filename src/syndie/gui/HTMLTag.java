package syndie.gui;

import java.util.Iterator;
import java.util.Properties;


class HTMLTag {
    /** tag name, lower case */
    private String _name;
    /** attributes on the tag */
    private Properties _attributes;
    /** start index for the body text that the tag is applicable to */
    private int _startIndex;
    private int _endIndex;
    private HTMLTag _parent;
    private boolean _consumed;
    
    public HTMLTag(String tagBody, int startIndex, HTMLTag parent) {
        _startIndex = startIndex;
        _endIndex = -1;
        _parent = parent;
        _consumed = false;
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
                        } else                            if (attribNameEnd == -1) {
                                // whitespace does terminate an attribute ("href = 'foo'")
                                attribNameEnd = i;
                            } else                                if (attribValueStart == -1) {
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
            }  else if ((quoteChar != -1) && (quoteChar == c) && (attribValueStart != -1)) {
                    quoteChar = -1;
                    String name = tagBody.substring(attribNameStart, attribNameEnd);
                    String val = tagBody.substring(attribValueStart, i);
                    _attributes.setProperty(name, val);
                    attribNameStart = -1;
                    attribNameEnd = -1;
                    attribValueStart = -1;
            }  else if (_name != null) {
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
    /** 
     * the tag is at least partly within the parent: <a><b/></a> and <a><b></a><c/></b> both use 'a' as
     * the parent for 'b'
     */
    public HTMLTag getParent() { return _parent; }
    public boolean wasConsumed() { return _consumed; }
    public void consume() { _consumed = true; }
    
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
        rv.append("[" + _startIndex + (_endIndex >= 0 ? ":" + _endIndex : ":?") + "]");
        return rv.toString();
    }
}