package syndie.gui;

import java.util.ArrayList;
import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;
import syndie.data.HTMLTag;
import syndie.data.MessageInfo;
import syndie.data.SyndieURI;

/**
 *
 */
public class PageEditorNew {
    private BrowserControl _browser;
    private MessageEditorNew _editor;
    private Composite _root;
    private SashForm _sash;
    private StyledText _text;
    private PageRenderer _preview;
    private boolean _isPreviewable;
    /** current search match is highlighted */
    private StyleRange _findHighlight;
    /** has the current search wrapped the end at least once yet? */
    private boolean _findWrapped;
    
    /** Creates a new instance of PageEditorNew */
    public PageEditorNew(BrowserControl browser, MessageEditorNew editor, boolean previewable) {
        _browser = browser;
        _editor = editor;
        _isPreviewable = previewable;
        initComponents();
    }
    
    public Control getControl() { return _root; }
    public String getContent() { return _text.getText(); }
    public String getContentType() { return _isPreviewable ? "text/html" : "text/plain"; }
    
    public void setContent(String body) { _text.setText(body); }
    
    public void dispose() {
        if (_preview != null)
            _preview.dispose();
        if (!_root.isDisposed())
            _root.dispose();
    }
    
    private void initComponents() {
        Composite parent = _editor.getPageRoot();
        _root = new Composite(parent, SWT.NONE);
        _root.setLayout(new FillLayout());
        
        _sash = new SashForm(_root, SWT.HORIZONTAL);
        _text = new StyledText(_sash, SWT.MULTI | SWT.BORDER | SWT.WRAP | SWT.V_SCROLL | SWT.H_SCROLL);
        configText();
        
        if (_isPreviewable) {
            _preview = new PageRenderer(_sash, true, _browser);
            _text.addControlListener(new ControlListener() {
                public void controlMoved(ControlEvent controlEvent) {}
                public void controlResized(ControlEvent evt) {
                    //_browser.getUI().debugMessage("text resized: "+ evt);
                    if (_lastResizeWidth != _text.getBounds().width) {
                        _lastResized = System.currentTimeMillis();
                        evt.display.timerExec(100, _timedPreview);
                    }
                }
            });

            _sash.setWeights(new int[] { 80, 20 });
        } else {
            _sash.setWeights(new int[] { 100 });
            _sash.setMaximizedControl(_text);
        }
        
        _findHighlight = new StyleRange();
        _findHighlight.background = ColorUtil.getColor("yellow", null);
    }
    
    public void updated() { preview(); }
    
    private static final SyndieURI _dummyURI = SyndieURI.createMessage(new Hash(new byte[Hash.HASH_LENGTH]), Long.MAX_VALUE, 0);
    private void preview() {
        if (!_isPreviewable) return;
        MessageInfo msgInfo = new MessageInfo();
        msgInfo.setURI(_dummyURI);
        msgInfo.setTargetChannel(_dummyURI.getScope());
        msgInfo.setTargetChannelId(Long.MAX_VALUE);
        msgInfo.setScopeChannelId(Long.MAX_VALUE);
        msgInfo.setAuthorChannelId(Long.MAX_VALUE);
        msgInfo.setInternalId(Long.MAX_VALUE);
        msgInfo.setMessageId(_dummyURI.getMessageId().longValue());
        msgInfo.setPageCount(1);
        ArrayList pageData = new ArrayList();
        pageData.add(_text.getText());
        ArrayList attachments = new ArrayList();
        ArrayList attachmentOrder = new ArrayList();
        List names = _editor.getAttachmentNames();
        for (int i = 0; i < names.size(); i++) {
            String name = (String)names.get(i);
            byte data[] = _editor.getAttachmentData(i+1);
            attachmentOrder.add(name);
            attachments.add(data);
        }
        PageRendererSourceMem src = new PageRendererSourceMem(_browser, null, msgInfo, pageData, attachments, attachmentOrder);
        _preview.setRender(true);
        long before = System.currentTimeMillis();
        _preview.renderPage(src, _dummyURI);
        long renderTime = System.currentTimeMillis()-before;
        _browser.getUI().debugMessage("** render time: " + renderTime);
        _preview.setRender(false);
        _lastModified = -1;
        _lastResized = -1;
        _lastResizeWidth = _text.getBounds().width;
    }
    
    private long _lastModified;
    private long _lastResized;
    private int _lastResizeWidth;
    private Runnable _timedPreview = new Runnable() {
        public void run() {
            if ( (_lastModified > 0) || (_lastResized > 0) ) {
                long idle = -1;
                if (_lastModified > 0)
                    idle = System.currentTimeMillis() - _lastModified;
                else
                    idle = System.currentTimeMillis() - _lastResized;
                if (idle > 1000) {
                    _browser.getUI().debugMessage("idle for " + idle + "ms, previewing");
                    preview();
                } else {
                    //System.out.println("idle for " + idle + "ms, NOT previewing");
                    Display.getCurrent().timerExec(500, _timedPreview);
                    _preview.setRender(false);
                }
            }
        }
    };
    private void configText() {
        _text.setDoubleClickEnabled(true);
        _text.setEditable(true);
        
        _text.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent evt) {
                _lastModified = System.currentTimeMillis();
                _editor.modified();
                if (_isPreviewable)
                    evt.display.timerExec(500, _timedPreview);
            }
        });
        _text.addKeyListener(new KeyListener() {
            public void keyReleased(KeyEvent evt) { }
            public void keyPressed(KeyEvent evt) {
                switch (evt.character) {
                    case 0x01: // ^A
                        _text.selectAll();
                        evt.doit = false;
                        break;
                    case 0x02: // ^B
                        if ( (evt.stateMask & SWT.MOD1) != 0) {
                            insertAtCaret("<b></b>");
                            int newOffset = _text.getCaretOffset();
                            newOffset -= "</b>".length();
                            _text.setCaretOffset(newOffset);
                            evt.doit = false;
                        }
                        break;
                        /*
                    case 0x05: // ^E
                        if ( (evt.stateMask & SWT.MOD1) != 0) {
                            String term = _editor.getSearchTerm();
                            if ( (term != null) && (term.length() > 0) ) {
                                boolean wasHighlighted = _findHighlight.length > 0;
                                findNext();
                                if (!wasHighlighted && (_findHighlight.length <= 0))
                                    find();
                            } else {
                                find();
                            }
                        }
                        break;
                         */
                    case 0x06: // ^F
                        if ( (evt.stateMask & SWT.MOD1) != 0) {
                            String term = _editor.getSearchTerm();
                            if ( (term != null) && (term.length() > 0) ) {
                                boolean wasHighlighted = _findHighlight.length > 0;
                                findNext();
                                if (!wasHighlighted && (_findHighlight.length <= 0))
                                    find();
                            } else {
                                find();
                            }
                        }
                        break;
                    case 0x09: // ^I
                        if ( (evt.stateMask & SWT.MOD1) != 0) {
                            int off = _text.getCaretOffset();
                            _text.replaceTextRange(off-1, 1, ""); // remove the (^I-inserted) tab
                            insertAtCaret("<i></i>");
                            int newOffset = _text.getCaretOffset();
                            newOffset -= "</i>".length();
                            _text.setCaretOffset(newOffset);
                            _editor.modified();
                            evt.doit = false;
                        }
                        break;
                    case 0x12: // ^R
                        if ( (evt.stateMask & SWT.MOD1) != 0) {
                            String term = _editor.getSearchTerm();
                            if ( (term != null) && (term.length() > 0) )
                                findReplace();
                            else
                                find();
                        }
                        break;
                    case 0x13: // ^S
                        if ( (evt.stateMask & SWT.MOD1) != 0)
                            _editor.saveState();
                        break;
                    case 0x15: // ^U
                        if ( (evt.stateMask & SWT.MOD1) != 0) {
                            insertAtCaret("<u></u>");
                            int newOffset = _text.getCaretOffset();
                            newOffset -= "</u>".length();
                            _text.setCaretOffset(newOffset);
                            evt.doit = false;
                        }
                        break;
                    case 0x03: // ^C
                        if ( (evt.stateMask & SWT.MOD1) != 0) {
                            _text.copy();
                            evt.doit = false;
                        }
                        break;
                    //this is done automatically
                    //case 0x16: // ^V
                    //    if ( (evt.stateMask & SWT.MOD1) != 0) {
                    //        _text.paste();
                    //        evt.doit = false;
                    //        break;
                    //    }
                    case 0x18: // ^X
                        if ( (evt.stateMask & SWT.MOD1) != 0) {
                            _text.cut();
                            _editor.modified();
                            evt.doit = false;
                        }
                        break;
                }
            }
        });
    }
    
    /** helper to shove text in, adjusting the caret to the first character after the new text */
    void insertAtCaret(String text) {
        if (text != null) {
            _editor.modified();
            // rather than replacing everything selected, just insert at the caret
            _text.replaceTextRange(_text.getCaretOffset(), 0, text);
            //_text.insert(buf.toString());
            _text.setCaretOffset(_text.getCaretOffset()+text.length());
        }
        boolean focused = _text.forceFocus();
    }
    
    private boolean isAtBeginningOfLine(int offset) {
        if (offset == 0) return true;
        int len = _text.getCharCount();
        String txt = null;
        if (offset >= len)
            txt = _text.getText(0, offset-2);
        else
            txt = _text.getText(0, offset-1);
        boolean isInTag = false;
        for (int i = txt.length()-1; i >= 0; i--) {
            char c = txt.charAt(i);
            if ('\n' == c) {
                return true;
            } else if ('>' == c) {
                isInTag = true;
            } else if ('<' == c) {
                isInTag = false;
            } else if (!isInTag) {
                return false;
            }
        }
        // no non-tagged content, so this is the beginning of the page
        return true;
    }
    
    public void insert(String toInsert, boolean onNewline) {
        if (onNewline && !isAtBeginningOfLine(_text.getCaretOffset()))
            insertAtCaret('\n' + toInsert);
        else
            insertAtCaret(toInsert);
    }
    
    // find utils - called either from within the page editor or by the message editor's finder
    void find() {
        _findWrapped = false;
        _findHighlight.length = 0;
        _editor.search();
    }
    void findReplace() {
        if (_findHighlight.length <= 0)
            findNext();
        if (_findHighlight.length > 0) {
            String replaceWith = _editor.getSearchReplacement();
            _text.replaceTextRange(_findHighlight.start, _findHighlight.length, replaceWith);
            _text.setCaretOffset(_findHighlight.start + replaceWith.length());
            _findHighlight.length = 0;
            _text.setStyleRanges(null, null);
            _editor.modified();
            findNext();
        }
    }
    void findReplaceAll() {
        if (_findHighlight.length <= 0)
            findNext(false);
        while (_findHighlight.length > 0) {
            String replaceWith = _editor.getSearchReplacement();
            _text.replaceTextRange(_findHighlight.start, _findHighlight.length, replaceWith);
            _text.setCaretOffset(_findHighlight.start + replaceWith.length());
            _findHighlight.length = 0;
            _text.setStyleRanges(null, null);
            findNext(false);
        }
        _editor.modified();
    }
    void findNext() { findNext(true); }
    private void findNext(boolean wrapForever) {
        String searchFor = _editor.getSearchTerm();
        if ( (searchFor == null) || (searchFor.length() <= 0) ) return;
        String txt = _text.getText();
        boolean caseSensitive = _editor.getSearchCaseSensitive();
        boolean backwards = _editor.getSearchBackwards();
        if (!caseSensitive) { // note: locale-sensitive
            searchFor = searchFor.toLowerCase();
            txt = txt.toLowerCase();
        }
        int caret = _text.getCaretOffset();
        int nextStart = -1;
        if (backwards) {
            nextStart = txt.lastIndexOf(searchFor, caret);
        } else {
            nextStart = txt.indexOf(searchFor, caret);
        }
        if (nextStart == caret) {
            if (backwards) {
                if (caret > 0)
                    nextStart = txt.lastIndexOf(searchFor, caret-1);
                else
                    nextStart = -1;
            } else {
                if (caret >= _text.getCharCount())
                    nextStart = -1;
                else
                    nextStart = txt.indexOf(searchFor, caret+1);
            }
        }
        if (nextStart == -1) {
            if (_editor.getSearchWrap() && (!_findWrapped || wrapForever)) {
                if (backwards)
                    nextStart = txt.lastIndexOf(searchFor);
                else
                    nextStart = txt.indexOf(searchFor);
                _findWrapped = true;
            }
        }
        _browser.getUI().debugMessage("findNext @ " + nextStart + " (started @ " + caret + ")");
        if (nextStart != -1) {
            _text.setCaretOffset(nextStart);
            _text.setStyleRanges(null, null);
            _findHighlight.start = nextStart;
            _findHighlight.length = searchFor.length();
            _text.setStyleRange(_findHighlight);
            int line = _text.getLineAtOffset(nextStart);
            _text.setTopIndex(line);
        } else {
            _findHighlight.length = 0;
            _text.setStyleRanges(null, null);
            int line = _text.getLineAtOffset(caret);
            _text.setTopIndex(line);
        }
    }
    void cancelFind() {
        _text.setStyleRanges(null, null);
    }
    
    
    
    
    
    
    
    
    /** line in the text buffer we are spellchecking */
    private int _spellLine;
    /** word in the line that we are spellchecking */
    private int _spellWordIndex;
    /** offset in the entire text to the current word being spellchecked */
    private int _spellWordStart;
    /** offset in the entire text to the current word being spellchecked (end is inclusive) */
    private int _spellWordEnd;
    
    void spellIgnore() {
        _spellWordIndex++;
        spellNext();
    }
    void resetSpellcheck() {
        _spellLine = 0;
        _spellWordIndex = 0;
    }
    void spellReplaceWord(boolean replaceAll) {
        String old = _editor.getSpellWordOrig();
        String newText = _editor.getSpellWordSuggestion();
        int len = _spellWordEnd-_spellWordStart+1;
        if (_spellWordStart + len >= _text.getCharCount())
            len = _text.getCharCount() - _spellWordStart;
        String oldFound = _text.getTextRange(_spellWordStart, len);
        _browser.getUI().debugMessage("replacing [" + old + "]/[" + oldFound + "] with [" + newText + "]");
        _text.replaceTextRange(_spellWordStart, len, newText);
        _editor.modified();
        _spellWordIndex++;
        if (replaceAll) {
            int line = _spellLine;
            int word = _spellWordIndex;
            spellNext(newText, old);
            _spellLine = line;
            _spellWordIndex = word;
        }
    }
    void spellNext() { spellNext(null, null); }
    private void spellNext(String forceReplacement, String replaceFor) {
        // iterate over the lines
        //  iterate over the words (ignoring html)
        //   if (!spelledCorrectly)
        //    update spellcheck display
        //    _spellShell.setVisible(true);
        //    return;
        // if end reached
        //  display success dialog
              
        _spellWordStart = -1;
        _spellWordEnd = -1;
        boolean inTag = false;
        while (_spellLine < _text.getLineCount()) {
            int lineStart = _text.getOffsetAtLine(_spellLine);
            int lineEnd = -1;
            if (_spellLine + 1 >= _text.getLineCount())
                lineEnd = _text.getCharCount()-1;
            else
                lineEnd = _text.getOffsetAtLine(_spellLine+1)-1;
            
            String lineText = "";
            if (lineEnd > lineStart)
                lineText = _text.getText(lineStart, lineEnd).trim();
            //System.out.println("line " + _spellLine + ": [" + lineText + "]");
            
            int off = 0;
            if (inTag) {
                int endTag = lineText.indexOf('>');
                if (endTag == -1) {
                    // entire line is within a tag.  skip to the next line
                    _spellLine++;
                    continue;
                } else {
                    off = endTag+1;
                }
            }
            
            /** wordStart is part of the word */
            int wordStart = -1;
            /** wordEnd is part of the word */
            int wordEnd = -1;
            int curWord = 0;
            int cur = off;
            int len = lineText.length();
            while (true) {
                if ( (cur >= len) && (wordStart == -1) )
                    break;
                char c = 0;
                if (cur < len)
                    c = lineText.charAt(cur);
                if (c == '<') inTag = true;
                else if (c == '>') inTag = false;
                if ( (cur < len) && (Character.isLetterOrDigit(c)) ) {
                    if ( (!inTag) && (wordStart < 0) ) {
                        wordStart = cur;
                        //System.out.println("wordStart reached @ " + cur);
                    }
                } else if (wordStart != -1) {
                    if (cur >= len) {
                        wordEnd = cur;
                        //System.out.println("wordEnd reached: [" + lineText.substring(wordStart) + "] [" + wordStart + "," + wordEnd + "]");
                    } else {
                        wordEnd = cur-1;
                        //System.out.println("wordEnd reached: [" + lineText.substring(wordStart, wordEnd+1) + "] [" + wordStart + "," + wordEnd + "]");
                    }
                    if (curWord == _spellWordIndex) {
                        String word = null;
                        if (cur >= len)
                            word = lineText.substring(wordStart);
                        else
                            word = lineText.substring(wordStart, wordEnd+1);
                        String lower = word.toLowerCase();
                        if (forceReplacement != null) {
                            if (replaceFor.equals(lower)) {
                                _spellWordStart = lineStart + wordStart;
                                if (cur >= len)
                                    _spellWordEnd = lineStart + wordEnd - 1;
                                else
                                    _spellWordEnd = lineStart + wordEnd;
                                int wordLen = _spellWordEnd-_spellWordStart+1;
                                if (_spellWordStart + wordLen >= _text.getCharCount())
                                    wordLen = _text.getCharCount() - _spellWordStart;
                                String oldFound = _text.getTextRange(_spellWordStart, wordLen);
                                //System.out.println("force replacing [" + lower + "]/[" + oldFound + "] with [" + forceReplacement + "]");
                                _text.replaceTextRange(_spellWordStart, wordLen, forceReplacement);
                                // does not break.. keeps on iterating through the whole doc
                            } else {
                                // ok, this word may be misspelled, but we are doing a replaceAll
                            }
                        } else {
                            if (!_editor.getSpellIgnoreAllList().contains(lower)) {
                                ArrayList suggestions = _editor.getSuggestions(word, lower, lineText);
                                if (suggestions != null) {
                                    _spellWordStart = lineStart + wordStart;
                                    if (cur >= len)
                                        _spellWordEnd = lineStart + wordEnd - 1;
                                    else
                                        _spellWordEnd = lineStart + wordEnd;
                                    // underline the word
                                    _editor.showSpell(true);
                                    return;
                                }
                            }
                        }
                        _spellWordIndex++;
                    }
                    wordStart = -1;
                    wordEnd = -1;
                    curWord++;
                }
                cur++;
            }
            // end of line reached
            if (lineText.lastIndexOf('<') > lineText.lastIndexOf('>'))
                inTag = true;
            _spellWordIndex = 0;
            _spellLine++;
        }
        
        // end reached.  show success (or bailout if we just want to nested replaceAll)
        if (forceReplacement != null)
            return;
        _editor.showSpell(false);
        return;
    }
    
    // callback from the styler
    void cancelStyle() { _text.forceFocus(); }
    String getSelectedText() { return _text.getSelectionText(); }
    void insertStyle(String str, boolean insert, int begin, int end) {
        if (insert) {
            insertAtCaret(str);
            _text.setCaretOffset(_text.getCaretOffset() - (str.length()-begin));
            _text.setSelectionRange(_text.getCaretOffset(), (end-begin));
        } else { // replace selection
            _editor.modified();
            Point range = _text.getSelectionRange();
            _text.replaceTextRange(range.x, range.y, str);
            _text.setCaretOffset(range.x+begin);
            _text.setSelectionRange(_text.getCaretOffset(), (end-begin));
        }
    }
    
    // callbacks from pageBGColor/pageBGImage
    void setBodyTags() { setBodyTags(null, null); }
    void setBodyTags(String bgImageURL, String bodyColor) {
        if ( (bodyColor != null) || (bgImageURL != null) ) {
            String txt = _text.getText();
            int body = txt.indexOf("<body");
            if (body == -1) {
                // ok, this assumes that if they don't have a <body> tag, they don't have an <html>
                // tag either
                StringBuffer buf = new StringBuffer();
                buf.append("<html>\n<body ");
                if (bodyColor != null)
                    buf.append("bgcolor=\"").append(bodyColor).append("\" ");
                if (bgImageURL != null)
                    buf.append("bgimage=\"").append(bgImageURL).append("\" ");
                buf.append(">\n");
                _text.replaceTextRange(0, 0, buf.toString());
                int sz = _text.getCharCount();
                _text.replaceTextRange(sz, 0, "\n</body>\n</html>\n");
            } else {
                int bodyEnd = txt.indexOf('>', body);
                String attributes = txt.substring(body+1, bodyEnd);
                HTMLTag bodyTag = new HTMLTag(attributes, 0, null, -1);
                if (bodyColor != null)
                    bodyTag.setAttribValue("bgcolor", bodyColor);
                else
                    bodyTag.removeAttribValue("bgcolor");
                if (bgImageURL != null)
                    bodyTag.setAttribValue("bgimage", bgImageURL);
                else
                    bodyTag.removeAttribValue("bgimage");
                _text.replaceTextRange(body, bodyEnd-body+1, bodyTag.toHTML());
            }
        }
    }
}
