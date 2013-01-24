package syndie.data;

import java.util.Iterator;
import java.util.Locale;
import java.util.Properties;

import syndie.util.StringUtil;

public class HTMLTag {
    /** tag name, lower case */
    public String name;
    /** attributes on the tag */
    public Properties attributes;
    /** start index for the body text that the tag is applicable to */
    public int startIndex;
    /** the tag was closed at the given body index */
    public int endIndex;
    public int srcLine;
    /** 
     * the tag is at least partly within the parent: <a><b/></a> and <a><b></a><c/></b> both use 'a' as
     * the parent for 'b'
     */
    public HTMLTag parent;
    public boolean consumed;
    
    public HTMLTag(String tagBody, int startIndex, HTMLTag parent, int srcLine) {
        this.startIndex = startIndex;
        endIndex = -1;
        this.parent = parent;
        this.srcLine = srcLine;
        attributes = new Properties();
        int attribNameStart = -1;
        int attribNameEnd = -1;
        int attribValueStart = -1;
        int quoteChar = -1;

        if (tagBody.charAt(0) == '/') // endTag
            tagBody = tagBody.substring(1);
        
        int len = tagBody.length();
        for (int i = 0; i < len; i++) {
            char c = tagBody.charAt(i);
            if (Character.isWhitespace(c)) {// || (c == '/')) {
                if (this.name == null) {
                    if (i == 0)
                        this.name = "";
                    this.name = StringUtil.lowercase(tagBody.substring(0, i));
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
                            String lname = StringUtil.lowercase(tagBody.substring(attribNameStart, attribNameEnd));
                            String val = tagBody.substring(attribValueStart, i);
                            attributes.setProperty(lname, val);
                            attribNameStart = -1;
                            attribNameEnd = -1;
                            attribValueStart = -1;
                        }
                    }
                }
            }  else if ((quoteChar != -1) && (quoteChar == c) && (attribValueStart != -1)) {
                quoteChar = -1;
                String lname = StringUtil.lowercase(tagBody.substring(attribNameStart, attribNameEnd));
                String val = tagBody.substring(attribValueStart, i);
                attributes.setProperty(lname, val);
                attribNameStart = -1;
                attribNameEnd = -1;
                attribValueStart = -1;
            } else if (this.name != null) {
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
                    } else {
                        attribValueStart = i;
                    }
                }
            } else {
                // name not known, and we haven't reached whitespace yet.  keep going
            }
        } // end looping over the tag body
        
        if (attribValueStart > 0) {
            // eg <a href=foo>
            String lname = StringUtil.lowercase(tagBody.substring(attribNameStart, attribNameEnd));
            String val = tagBody.substring(attribValueStart).trim();
            attributes.setProperty(lname, val);
            attribNameStart = -1;
            attribNameEnd = -1;
            attribValueStart = -1;            
        }
        
        if ((name == null) || (name.trim().length() <= 0)) {
            //System.out.println("name is empty for tag [" + tagBody + "] @ " + startIndex);
            name = StringUtil.lowercase(tagBody);
        }
        if ( (name != null) && name.endsWith("/") && (name.length() > 1) )
            name = name.substring(0, name.length()-1);
    }
    
    public String getAttribValue(String name) { return attributes.getProperty(StringUtil.lowercase(name)); }
    public String getAttribValueLC(String name) { return attributes.getProperty(name); }
    public void setAttribValue(String name, String value) { attributes.setProperty(StringUtil.lowercase(name), value); }
    public void removeAttribValue(String name) { attributes.remove(StringUtil.lowercase(name)); }
    public boolean wasConsumed() { return consumed; }
    public void consume() { consumed = true; }
    
    public String toString() {
        StringBuilder rv = new StringBuilder();
        rv.append(toHTML());
        rv.append("[" + this.startIndex + (endIndex >= 0 ? ":" + endIndex : ":?") + ":" + this.srcLine + "]");
        return rv.toString();
    }
    public String toHTML() {
        StringBuilder rv = new StringBuilder();
        rv.append('<');
        rv.append(this.name);
        rv.append(' ');
        for (Iterator iter = attributes.keySet().iterator(); iter.hasNext(); ) {
            String lname = (String)iter.next();
            String val = attributes.getProperty(lname);
            rv.append(lname).append('=').append('\'').append(val).append('\'').append(' ');
        }
        rv.append('>');
        return rv.toString();
    }
    
    public int hashCode() {
        int rv = 0;
        if (name != null) rv += name.hashCode();
        rv += startIndex;
        rv += endIndex;
        rv += srcLine;
        return rv;
    }
    public boolean equals(Object o) {
        HTMLTag tag = (HTMLTag)o;
        return ( (tag.name.equals(name)) &&
                 (tag.startIndex == startIndex) &&
                 (tag.endIndex == endIndex) &&
                 (tag.srcLine == srcLine) &&
                 ((tag.attributes != null) && (tag.attributes.equals(attributes)))
                 );
    }
}
