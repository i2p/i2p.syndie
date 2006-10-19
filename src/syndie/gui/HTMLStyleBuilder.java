package syndie.gui;

import java.util.*;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GlyphMetrics;
import syndie.data.MessageInfo;
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
    private List _imageIndexes;
    private List _linkIndexes;
    private List _listItemIndexes;
    
    public HTMLStyleBuilder(DBClient client, List htmlTags, String msgText, MessageInfo msg) {
        _client = client;
        _htmlTags = htmlTags;
        _msgText = msgText;
        _msg = msg;
        _imageIndexes = new ArrayList();
        _linkIndexes = new ArrayList();
        _listItemIndexes = new ArrayList();
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
    
    public List getImageIndexes() { return _imageIndexes; }
    public List getLinkEndIndexes() { return _linkIndexes; }
    public List getListItemIndexes() { return _listItemIndexes; }
    
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
        
        StyleRange range = new StyleRange();
        range.start = start;
        range.length = length;
        
        return range;
    }
    
    public StyleRange[] getStyleRanges() { return _styleRanges; }
    
    private StyleRange getStyle(TagRange range) {
        // turn the given tags into a style
        StyleRange style = new StyleRange();
        style.start = range.getStart();
        style.length = range.getEnd() - range.getStart();
        style.underline = range.containsTag("a");
        //style.font = _linkFont;
        int ascent = 0;
        int descent = 0;
        int width = 1;
        style.metrics = new GlyphMetrics(ascent, descent, width);
        return style;
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
    
    private class TagRange {
        private int _start;
        private int _end;
        private List _applicableTags;
        public TagRange(HTMLTag tag) { this(tag.getStartIndex(), tag.getEndIndex(), tag); }
        public TagRange(int start, int end, HTMLTag tag) {
            _start = start;
            _end = end;
            _applicableTags = new ArrayList();
            _applicableTags.add(tag);
        }
        
        public int getStart() { return _start; }
        public int getEnd() { return _end; }
        public List getApplicableTags() { return _applicableTags; }
        
        public boolean containsTag(String tagName) { 
            for (int i = 0; i < _applicableTags.size(); i++) {
                HTMLTag tag = (HTMLTag)_applicableTags.get(i);
                if (tagName.equals(tag.getName()))
                    return true;
            }
            return false;
        }
        
        /** 
         * remove any tags that do not share the same start and end
         * points, giving them their own TagRange
         */
        public List splitRanges() {
            if (_applicableTags.size() > 1) {
                List rv = new ArrayList();
                // sort them by end index
                TreeMap tagRanges = new TreeMap();
                for (int i = 0; i < _htmlTags.size(); i++) {
                    HTMLTag tag = (HTMLTag)_htmlTags.get(i);
                    TagRange range = (TagRange)tagRanges.get(new Integer(tag.getEndIndex()));
                    if (range == null) {
                        range = new TagRange(tag);
                        tagRanges.put(new Integer(tag.getEndIndex()), range);
                    } else {
                        range.getApplicableTags().add(tag);
                    }
                }
                
                for (Iterator iter = tagRanges.keySet().iterator(); iter.hasNext(); ) {
                    TagRange range = (TagRange)iter.next();
                }
                
                // keep the earliest ending one in the current object, and create
                // new small ranges starting where the last one finishes
                return rv;
            } else {
                return Collections.EMPTY_LIST;
            }
        }
    }
}
