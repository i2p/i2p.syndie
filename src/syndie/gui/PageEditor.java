package syndie.gui;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.HTMLStateBuilder;
import syndie.data.HTMLTag;
import syndie.data.MessageInfo;
import syndie.data.SyndieURI;
import syndie.db.CommandImpl;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 *
 */
public class PageEditor extends BaseComponent implements Themeable {
    private MessageEditor _editor;
    private CTabItem _item;
    private Composite _root;
    private SashForm _sash;
    private Text _text;
    private TextChangeManager _textManager;
    private MaxEditor _maxEditor;
    private PageRenderer _preview;
    private MaxPreview _maxPreview;
    private boolean _isPreviewable;
    private boolean _isHTML;
    /** current search match (x=start, y=end) */
    private Point _findHighlight;
    /** has the current search wrapped the end at least once yet? */
    private boolean _findWrapped;
    private int _pageNum;
    
    /** Creates a new instance of PageEditorNew */
    public PageEditor(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, MessageEditor editor, boolean previewable, boolean isHTML, int pageNum) {
        super(client, ui, themes, trans);
        _editor = editor;
        _isPreviewable = previewable;
        _isHTML = isHTML;
        _pageNum = pageNum;
        initComponents();
    }
    
    public Control getControl() { return _root; }
    public String getContent() { 
        if (_maxText != null)
            return _maxText.getText();
        else
            return _text.getText(); 
    }
    public String getContentType() { return _isHTML ? MessageEditor.TYPE_HTML : MessageEditor.TYPE_TEXT; }
    public CTabItem getItem() { return _item; }
    
    public void setContent(String body) { _text.setText(body); }
    public void setContentType(String type) {
        _isHTML = MessageEditor.TYPE_HTML.equals(type);
        //_isPreviewable = MessageEditor.TYPE_HTML.equals(type);
        preview();
    }
    
    public long getUndoBufferSize() {
        long rv = 0;
        if (_maxTextManager != null) rv += _maxTextManager.getBufferSize();
        if (_textManager != null) rv += _textManager.getBufferSize();
        return rv;
    }
    
    public void dispose() {
        _themeRegistry.unregister(this);
        if (_preview != null)
            _preview.dispose();
        if (!_root.isDisposed())
            _root.dispose();
        if (_item != null)
            _item.dispose();
        if (_maxEditor != null)
            _maxEditor.dispose();
        if (_maxPreview != null)
            _maxPreview.dispose();
    }
    
    private void initComponents() {
        CTabFolder parent = _editor.getPageRoot();
        _item = new CTabItem(parent, SWT.NONE, _pageNum);
        _root = new Composite(parent, SWT.NONE);
        _root.setLayout(new FillLayout());
        _item.setControl(_root);
        
        _sash = new SashForm(_root, SWT.HORIZONTAL);
        _text = new Text(_sash, SWT.MULTI | SWT.BORDER | SWT.WRAP | SWT.V_SCROLL | SWT.H_SCROLL);
        configText();
        
        _preview = ComponentBuilder.instance().createPageRenderer(_sash, true);
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
        if (_isPreviewable && _isHTML) {
            _sash.setMaximizedControl(null);
        } else {
            _sash.setMaximizedControl(_text);
        }
        
        _findHighlight = new Point(0, 0);
        
        _themeRegistry.register(this);
    }
    
    public void updated() { preview(); }
    
    private static final SyndieURI _dummyURI = SyndieURI.createMessage(new Hash(new byte[Hash.HASH_LENGTH]), Long.MAX_VALUE, 0);
    private void preview() {
        if (_root.isDisposed()) return; // called after a delay, so may have been disposed in the meantime
        if (_editor != null) _editor.saveState();
        if (!_isPreviewable || !_isHTML) {
            _sash.setMaximizedControl(_text);
            return;
        }
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
        PageRendererSourceMem src = new PageRendererSourceMem(_client, _themeRegistry, msgInfo, pageData, attachments, attachmentOrder, null);
        _preview.setRender(true);
        _sash.setMaximizedControl(null);
        long before = System.currentTimeMillis();
        _preview.renderPage(src, _dummyURI);
        long renderTime = System.currentTimeMillis()-before;
        _ui.debugMessage("** render time: " + renderTime);
        _preview.setRender(false);
        _lastModified = -1;
        _lastResized = -1;
        _lastResizeWidth = _text.getBounds().width;
    }
    
    public boolean isPreviewShowing() {
        if (!_isHTML)
            return true; // editor == preview
        
        Composite previewControl = _preview.getComposite();
        return previewControl == _sash.getMaximizedControl();
    }
    
    private Object _previewLock = new Object();
    private Runnable _afterPreviewJob;
    public void toggleFullPreview(Runnable job) { 
        synchronized (_previewLock) {
            _afterPreviewJob = job;
        }
        toggleFullPreview();
    }
    private void previewComplete() {
        Runnable job = null;
        synchronized (_previewLock) {
            job = _afterPreviewJob;
            _afterPreviewJob = null;
        }
        _ui.debugMessage("preview complete: " + job, new Exception("completed by"));
        if (job != null)
            job.run();
    }
    public void toggleFullPreview() {
        if (_root.isDisposed()) {
            previewComplete();
            return; // called after a delay, so may have been disposed in the meantime
        }
        if (_editor != null) {
            _editor.modified();
            _editor.saveState();
        }
        _ui.debugMessage("toggle full preview: editor? " + _editor + " text: [" + _text.getText() + "] isHTML? " + _isHTML);
        
        if (!_isHTML) {
            _sash.setMaximizedControl(_text);
            previewComplete();
            return;
        }
        Composite previewControl = _preview.getComposite();
        if (previewControl == _sash.getMaximizedControl()) {
            if (_isPreviewable) {
                _text.forceFocus();
                _sash.setMaximizedControl(null);
            } else {
                _text.forceFocus();
                _sash.setMaximizedControl(_text);
            }
            previewComplete();
            return;
        }
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
        PageRendererSourceMem src = new PageRendererSourceMem(_client, _themeRegistry, msgInfo, pageData, attachments, attachmentOrder, new PageRendererSource.RenderListener() {
            public void renderComplete() {
                previewComplete();
            }
        });
        _preview.setRender(true);
        _sash.setMaximizedControl(_preview.getComposite());
        long before = System.currentTimeMillis();
        _preview.renderPage(src, _dummyURI);
        long renderTime = System.currentTimeMillis()-before;
        _ui.debugMessage("** render time: " + renderTime);
        _preview.setRender(false);
        _lastModified = -1;
        _lastResized = -1;
        _lastResizeWidth = _text.getBounds().width;
        _preview.forceFocus();
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
                    _ui.debugMessage("idle for " + idle + "ms, previewing");
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
                if (_isPreviewable && _isHTML)
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
                            wrapSelection("<b>", "</b>");
                            _editor.modified();
                            evt.doit = false;
                        }
                        break;
                    // case 0x05: // ^E //? why was this the same as ^F?
                    case 0x06: // ^F
                        if ( (evt.stateMask & SWT.MOD1) != 0) {
                            String term = _editor.getSearchTerm();
                            if ( (term != null) && (term.length() > 0) ) {
                                boolean wasHighlighted = _findHighlight.x < _findHighlight.y;
                                findNext(false);
                                if (!wasHighlighted && (_findHighlight.x >= _findHighlight.y))
                                    find();
                            } else {
                                find();
                            }
                        }
                        break;
                    case 0x09: // ^I
                        if ( (evt.stateMask & SWT.MOD1) != 0) {
                            wrapSelection("<i>", "</i>");
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
                            wrapSelection("<u>", "</u>");
                            _editor.modified();
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
        
        _textManager = new TextChangeManager(_text, _ui);
    }
    
    private void wrapSelection(String before, String after) {
        Point sel = _text.getSelection();
        _text.setSelection(sel.x, sel.x);
        _text.insert(before);
        _text.setSelection(sel.y+before.length());
        _text.insert(after);
        _text.setSelection(sel.x+before.length(), sel.y+before.length());
    }
    
    /** helper to shove text in, adjusting the caret to the first character after the new text */
    void insertAtCaret(String text) {
        if (text != null) {
            _editor.modified();
            // rather than replacing everything selected, just insert at the caret
            Point sel = _text.getSelection();
            _text.setSelection(sel.x); // 0-length selection
            _text.insert(text);
            _text.setSelection(sel.x + text.length(), sel.y + text.length());
            //StringBuilder buf = new StringBuilder(_text.getText());
            //buf.insert(off, text);
            //_text.setText(buf.toString());
            //_text.setSelection(off+text.length());
            ////_text.replaceTextRange(_text.getCaretOffset(), 0, text);
            //_text.insert(buf.toString());
            ////_text.setCaretOffset(_text.getCaretOffset()+text.length());
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
        if (onNewline && !isAtBeginningOfLine(_text.getCaretPosition()))
            insertAtCaret('\n' + toInsert);
        else
            insertAtCaret(toInsert);
    }
    
    // find utils - called either from within the page editor or by the message editor's finder
    void find() {
        _findWrapped = false;
        _findHighlight.x = 0;
        _findHighlight.y = 0;
        _editor.search();
    }
    void findReplace() {
        if (_findHighlight.x >= _findHighlight.y)
            findNext();
        if (_findHighlight.x < _findHighlight.y) {
            String replaceWith = _editor.getSearchReplacement();
            _text.setSelection(_findHighlight);
            _text.insert(replaceWith);
            _text.setSelection(_findHighlight.x+replaceWith.length());
            _findHighlight.x = 0;
            _findHighlight.y = 0;
            _editor.modified();
            findNext();
        }
    }
    void findReplaceAll() {
        if (_findHighlight.x >= _findHighlight.y)
            findNext(false);
        while (_findHighlight.x < _findHighlight.y) {
            String replaceWith = _editor.getSearchReplacement();
            _text.setSelection(_findHighlight);
            _text.insert(replaceWith);
            _text.setSelection(_findHighlight.x+replaceWith.length());
            _findHighlight.x = 0;
            _findHighlight.y = 0;
            findNext(false);
        }
        _editor.modified();
    }
    void findNext() { findNext(true); }
    private void findNext(boolean wrapForever) {
        _ui.debugMessage("findNext called", new Exception("source"));
        String searchFor = _editor.getSearchTerm();
        if ( (searchFor == null) || (searchFor.length() <= 0) ) return;
        String txt = _text.getText();
        boolean caseSensitive = _editor.getSearchCaseSensitive();
        boolean backwards = _editor.getSearchBackwards();
        if (!caseSensitive) { // note: locale-sensitive
            searchFor = searchFor.toLowerCase();
            txt = txt.toLowerCase();
        }
        int caret = _text.getCaretPosition();
        int nextStart = -1;
        if (backwards) {
            nextStart = txt.lastIndexOf(searchFor, caret);
        } else {
            nextStart = txt.indexOf(searchFor, caret);
        }
        _ui.debugMessage("findNext " + (backwards ? "lastIndex=" : "nextindex=") + nextStart + " wrapped? " + _findWrapped + " wrapForever? " + wrapForever);
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
        _ui.debugMessage("findNext @ " + nextStart + " (started @ " + caret + ")");
        if (nextStart != -1) {
            _findHighlight.x = nextStart;
            _findHighlight.y = nextStart + searchFor.length();
            _text.setSelection(_findHighlight);
            _text.showSelection();
        } else {
            _findHighlight.x = 0;
            _findHighlight.y = 0;
        }
    }
    void cancelFind() {
        _findHighlight.x = 0;
        _findHighlight.y = 0;
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
        _text.setSelection(_spellWordStart, _spellWordStart+len+1);
        _text.insert(newText);
        /*
        StringBuilder buf = new StringBuilder(_text.getText());
        String oldFound = buf.substring(_spellWordStart, _spellWordStart+len);
        //String oldFound = _text.getText(_spellWordStart, _spellWordStart+len); //_text.getTextRange(_spellWordStart, len);
        _ui.debugMessage("replacing [" + old + "]/[" + oldFound + "] with [" + newText + "]");
        //_text.replaceTextRange(_spellWordStart, len, newText);
        buf.replace(_spellWordStart, _spellWordStart+len, newText);
         */
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
        if (true) return;
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
            int lineStart = 0; //_text.getOffsetAtLine(_spellLine);
            int lineEnd = -1;
            if (_spellLine + 1 >= _text.getLineCount())
                lineEnd = _text.getCharCount()-1;
            else
                lineEnd = 0; //_text.getOffsetAtLine(_spellLine+1)-1;
            
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
            while (!_root.isDisposed()) {
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
                                String oldFound = ""; //_text.getTextRange(_spellWordStart, wordLen);
                                //System.out.println("force replacing [" + lower + "]/[" + oldFound + "] with [" + forceReplacement + "]");
                                ////_text.replaceTextRange(_spellWordStart, wordLen, forceReplacement);
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
            //_text.setSelection(_text.getCaretOffset() - (str.length()-begin));
            //_text.setSelectionRange(_text.getCaretOffset(), (end-begin));
        } else { // replace selection
            _editor.modified();
            _text.insert(str);
            //Point range = _text.getSelectionRange();
            //_text.replaceTextRange(range.x, range.y, str);
            //_text.setCaretOffset(range.x+begin);
            //_text.setSelectionRange(_text.getCaretOffset(), (end-begin));
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
                StringBuilder buf = new StringBuilder();
                buf.append("<html>\n<body ");
                if (bodyColor != null)
                    buf.append("bgcolor=\"").append(bodyColor).append("\" ");
                if (bgImageURL != null)
                    buf.append("bgimage=\"").append(bgImageURL).append("\" ");
                buf.append(">\n");
                
                Point sel = _text.getSelection();
                _text.setSelection(0);
                _text.insert(buf.toString());
                _text.append("\n</body>\n</html>\n");
                _text.setSelection(sel.x+buf.length(), sel.y+buf.length());
                /*
                _text.replaceTextRange(0, 0, buf.toString());
                int sz = _text.getCharCount();
                 _text.replaceTextRange(sz, 0, "\n</body>\n</html>\n");
                 */
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
                
                Point sel = _text.getSelection();
                _text.setSelection(body, bodyEnd+1);
                String newBody = bodyTag.toHTML();
                _text.insert(bodyTag.toHTML());
                _text.setSelection(sel.x + newBody.length()-bodyEnd, sel.y + newBody.length()-bodyEnd);
                /*
                _text.replaceTextRange(body, bodyEnd-body+1, bodyTag.toHTML());
                 */
            }
        }
    }

    public void toggleMaxView() {
        _ui.debugMessage("toggleMaxView of a page editor");
        if (_maxPreview != null) {
            _maxPreview.unmax();
            _maxPreview = null;
        } else if (_maxEditor != null) {
            _maxEditor.unmax();
            _maxEditor = null;
            _maxPreview = new MaxPreview();
        } else {
            _maxPreview = new MaxPreview();
        }
    }
    public void toggleMaxEditor() { 
        _ui.debugMessage("toggleMaxEditor of a page editor");
        if (_maxEditor != null) {
            _maxEditor.unmax();
            _maxEditor = null;
        } else if (_maxPreview != null) {
            _maxPreview.unmax();
            _maxPreview = null;
            _maxEditor = new MaxEditor();
        } else {
            _maxEditor = new MaxEditor();
        }
    }
    
    private Text _maxText;
    private TextChangeManager _maxTextManager;
    private class MaxEditor {
        private Shell _shell;
        
        public MaxEditor() {
            _shell = new Shell(_root.getShell(), SWT.NO_TRIM | SWT.PRIMARY_MODAL);
            _shell.setLayout(new GridLayout(2, true));
            Button unmax = new Button(_shell, SWT.PUSH);
            unmax.setText(_translationRegistry.getText("Restore normal editor size"));
            unmax.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            Button preview = new Button(_shell, SWT.PUSH);
            preview.setText(_translationRegistry.getText("Show maximized preview"));
            preview.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            _maxText = new Text(_shell, SWT.MULTI | SWT.BORDER | SWT.WRAP | SWT.V_SCROLL | SWT.H_SCROLL);
            _maxText.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
            
            _maxText.setText(_text.getText());

            _maxText.setDoubleClickEnabled(true);
            _maxText.setEditable(true);
            
            unmax.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { unmax(); }
                public void widgetSelected(SelectionEvent selectionEvent) { unmax(); }
            });
            
            preview.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { maxpreview(); }
                public void widgetSelected(SelectionEvent selectionEvent) { maxpreview(); }
            });
            
            Monitor mon[] = _root.getDisplay().getMonitors();
            Rectangle rect = null;
            if ( (mon != null) && (mon.length > 1) )
                rect = mon[0].getClientArea();
            else
                rect = _root.getDisplay().getClientArea();
            _shell.setSize(rect.width, rect.height);
            _shell.setMaximized(true);
            
            _shell.addShellListener(new ShellListener() {
                public void shellActivated(ShellEvent shellEvent) {}
                public void shellClosed(ShellEvent evt) {
                    evt.doit = false;
                    unmax();
                }
                public void shellDeactivated(ShellEvent shellEvent) {}
                public void shellDeiconified(ShellEvent shellEvent) {}
                public void shellIconified(ShellEvent shellEvent) {}
            });
            
            _maxTextManager = new TextChangeManager(_maxText, _ui);
            
            _maxText.setFont(_themeRegistry.getTheme().DEFAULT_FONT);
            _shell.open();
            _maxText.forceFocus();
        }
        public void unmax() {
            if (_maxText != null) {
                String val = _maxText.getText();
                _maxText = null;
                _text.setText(val);
            }
            _editor.modified();
            _editor.saveState();
            _shell.dispose();
        }
        private void maxpreview() {
            if (_maxText != null) {
                String val = _maxText.getText();
                _maxText = null;
                _text.setText(val);
            }
            _editor.modified();
            _editor.saveState();
            _shell.dispose();
            if (_maxPreview != null)
                _maxPreview.dispose();
            _maxPreview = new MaxPreview();
        }
        public void dispose() { _shell.dispose(); }
    }
    
    private class MaxPreview {
        private Shell _shell;
        private PageRenderer _maxRenderer;
        
        public MaxPreview() {
            _shell = new Shell(_root.getShell(), SWT.NO_TRIM | SWT.PRIMARY_MODAL);
            _shell.setLayout(new GridLayout(2, true));
            Button unmax = new Button(_shell, SWT.PUSH);
            unmax.setText(_translationRegistry.getText("Restore normal preview size"));
            unmax.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            Button edit = new Button(_shell, SWT.PUSH);
            edit.setText(_translationRegistry.getText("Show maximized editor"));
            edit.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
            
            _maxRenderer = ComponentBuilder.instance().createPageRenderer(_shell, true);
            _maxRenderer.getComposite().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
            
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
            pageData.add(getContent());
            ArrayList attachments = new ArrayList();
            ArrayList attachmentOrder = new ArrayList();
            List names = _editor.getAttachmentNames();
            for (int i = 0; i < names.size(); i++) {
                String name = (String)names.get(i);
                byte data[] = _editor.getAttachmentData(i+1);
                attachmentOrder.add(name);
                attachments.add(data);
            }
            PageRendererSourceMem src = new PageRendererSourceMem(_client, _themeRegistry, msgInfo, pageData, attachments, attachmentOrder, null);
            _maxRenderer.renderPage(src, _dummyURI);
            
            unmax.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { unmax(); }
                public void widgetSelected(SelectionEvent selectionEvent) { unmax(); }
                private void fire() { unmax(); }
            });
            edit.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent selectionEvent) { maxedit(); }
                public void widgetSelected(SelectionEvent selectionEvent) { maxedit(); }
            });
            
            Monitor mon[] = _root.getDisplay().getMonitors();
            Rectangle rect = null;
            if ( (mon != null) && (mon.length > 1) )
                rect = mon[0].getClientArea();
            else
                rect = _root.getDisplay().getClientArea();
            _shell.setSize(rect.width, rect.height);
            _shell.setMaximized(true);
            
            _shell.addShellListener(new ShellListener() {
                public void shellActivated(ShellEvent shellEvent) {}
                public void shellClosed(ShellEvent evt) {
                    evt.doit = false;
                    unmax();
                }
                public void shellDeactivated(ShellEvent shellEvent) {}
                public void shellDeiconified(ShellEvent shellEvent) {}
                public void shellIconified(ShellEvent shellEvent) {}
            });
            
            _shell.open();
            _maxRenderer.forceFocus();
        }
        
        public void unmax() {
            MaxPreview pv = _maxPreview;
            _maxPreview = null;
            pv.dispose();
        }
        private void maxedit() {
            MaxPreview pv = _maxPreview;
            _maxPreview = null;
            pv.dispose();
            if (_maxEditor != null)
                _maxEditor.dispose();
            _maxEditor = new MaxEditor();
        }
        public void dispose() { 
            _shell.dispose();
            _maxRenderer.dispose();
        }
    }
    
    void quote(SyndieURI parent) {
        long msgId = _client.getMessageId(parent.getScope(), parent.getMessageId());
        if (msgId >= 0) {
            int pageNum = 0;
            String page = _client.getMessagePageData(msgId, pageNum);
            if (page != null) {
                String cfg = _client.getMessagePageConfig(msgId, pageNum);
                boolean html = false;
                Properties props = new Properties();
                CommandImpl.parseProps(cfg, props);
                String mimeType = props.getProperty(Constants.MSG_PAGE_CONTENT_TYPE, "text/plain");
                if ("text/html".equalsIgnoreCase(mimeType) || "text/xhtml".equalsIgnoreCase(mimeType))
                    html = true;
                long authorId = _client.getMessageAuthor(msgId);
                Hash authorHash = _client.getChannelHash(authorId);
                String authorName = _client.getChannelName(authorId);
                quote(page, html, parent, authorHash, authorName);
            }
        }
    }
    void quote(String content, boolean contentIsHTML, SyndieURI source, Hash authorHash, String authorName) {
        boolean quoteAsHTML = _isHTML; //_isPreviewable;
        insert(getQuotable(content, contentIsHTML, quoteAsHTML, source, authorHash, authorName), true);
    }
    private String getQuotable(String src, boolean srcIsHTML, boolean quoteAsHTML, SyndieURI source, Hash authorHash, String authorName) {
        if (src == null) return "";
        boolean autoBR = true;
        String plainQuote = src;
        if (srcIsHTML) {
            HTMLStateBuilder sb = new HTMLStateBuilder(src);
            sb.buildState();
            plainQuote = getSourceToQuote(sb, quoteAsHTML);
            autoBR = false;
        } else if (quoteAsHTML) {
            autoBR = false;
            plainQuote = getSourceToQuoteFromText(src);
        }
        StringReader in = new StringReader(plainQuote);
        StringBuilder buf = new StringBuilder(plainQuote.length() + 64);
        String quoteAuthor = "";
        if (authorName != null)
            quoteAuthor = authorName;
        if (authorHash != null) {
            if (authorName != null)
                quoteAuthor = quoteAuthor + " ";
            quoteAuthor = quoteAuthor + "(" + authorHash.toBase64().substring(0,6) + ")";
        }
        if (quoteAsHTML)
            buf.append("<a href=\"").append(source.toString()).append("\"><b>").append(quoteAuthor).append("</b></a>:\n");
        else
            buf.append(quoteAuthor).append(":\n");
        if (quoteAsHTML)
            buf.append("<quote>");
        
        try {
            BufferedReader br = new BufferedReader(in);
            int lines = 0;
            String line = null;
            while ( (line = br.readLine()) != null) {
                if (quoteAsHTML) {
                    if (lines > 0) {
		        if (autoBR) buf.append("<br />");
                        buf.append("\n");
                    }
                    if (line.trim().length() > 0)
                        buf.append(line.trim());
                } else {
                    buf.append("> ").append(line.trim()).append("\n");
                    //buf.append(line.trim()).append("\n");
                }
                lines++;
            }
            if (!quoteAsHTML) buf.append("\n");
        } catch (IOException ioe) {
            // wtf?
        }
        
        if (quoteAsHTML)
            buf.append("</quote>");
        
        return buf.toString();
    }
    
    private String getSourceToQuote(HTMLStateBuilder sb, boolean quoteAsHTML) {
        List tags = sb.getTags();
        String txt = sb.getAsText();
        txt = HTMLStateBuilder.stripPlaceholders(txt);
        String formatted = injectTags(txt, tags, quoteAsHTML);
        return formatted;
    }
    private String injectTags(String asText, List tags, boolean quoteAsHTML) {
        StringBuilder buf = new StringBuilder();
        char str[] = asText.toCharArray();
        int textIndent = 0;
        //if (!quoteAsHTML) buf.append("> ");
        for (int i = 0; i < str.length; i++) {
            boolean quoteFound = false;
            for (int j = 0; j < tags.size(); j++) {
                HTMLTag tag = (HTMLTag)tags.get(j);
                if (tag.startIndex == i) {
                    if (quoteAsHTML) {
                        injectTagStart(buf, tag, tag.endIndex == i);
                    } else if (tag.name.equals("quote")) {
                        quoteFound = true;
                        textIndent++;
                    }
                } else if (tag.endIndex == i) {
                    if (quoteAsHTML) {
                        buf.append("</").append(tag.name).append(">");
                    } else if (tag.name.equals("quote")) {
                        textIndent--;
                    }
                }
            }
            if (!quoteAsHTML && quoteFound) {
                // inject the indentation at the site of the <quote> tag before the
                // actual quoted data
                for (int j = 0; j < textIndent; j++)
                    buf.append("> ");
            }
            buf.append(str[i]);
            if (!quoteAsHTML && (str[i] == '\n') ) {
                // inject all appropriate indentation after each newline
                for (int j = 0; j < textIndent; j++)
                    buf.append("> ");
            }
        }
        String rv = buf.toString();
        return rv;
    }
    private void injectTagStart(StringBuilder buf, HTMLTag tag, boolean closedTag) {
        if (tag.name.equals("img")) return;

        buf.append("<").append(tag.name);
        if ( (tag.attributes != null) && (tag.attributes.size() > 0) ) {
            buf.append(" ");
            for (Iterator iter = tag.attributes.keySet().iterator(); iter.hasNext(); ) {
                String attr = (String)iter.next();
                String val = tag.attributes.getProperty(attr);
                buf.append(attr).append("=\"").append(val).append("\" ");
            }
        }
        if (closedTag)
            buf.append(" /");
        buf.append(">");
    }
    private String getSourceToQuoteFromText(String src) {
        src = Constants.replace(src, "&", "&amp;");
        src = Constants.replace(src, ">", "&gt;");
        src = Constants.replace(src, "<", "&lt;");
        return "<pre>" + src + "</pre>";
    }
    
    public void applyTheme(Theme theme) {
        _text.setFont(theme.MONOSPACE_FONT);
    }
}
