package syndie.gui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ExtendedModifyEvent;
import org.eclipse.swt.custom.ExtendedModifyListener;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.CoolBar;
import org.eclipse.swt.widgets.CoolItem;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Item;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import syndie.data.MessageInfo;
import syndie.data.SyndieURI;

/**
 * wysiwyg editor for text or html pages in a message
 */
public class PageEditor {
    private MessageEditor _messageEditor;
    private String _contentType;
    private Composite _parent;
    private Composite _root;
    private Composite _toolbars;
    private StyledText _text;
    private PageRenderer _preview;
    
    // html toolbar
    private Button _htmlStyleChooser;
    private Button _htmlLink;
    private Button _htmlImg;
    private Button _htmlSymbol;
    private Combo _htmlHeader;
    private Button _htmlQuote;
    private Button _htmlPre;
    // page toolbar
    private Button _pageBGImage;
    private Button _pageBGColor;
    // list toolbar
    private Button _listOrdered;
    private Button _listUnordered;
    // meta toolbar
    private Button _metaCopy;
    private Button _metaPaste;
    private Button _metaCut;
    private Button _metaSpell;
    private Button _metaFind;
    private Button _metaReplace;
    private Button _metaPreview;
    
    // text style chooser dialog
    private Shell _txtShell;
    private StyledText _sampleText;
    private Button _txtBold;
    private Button _txtItalic;
    private Button _txtUnderline;
    private Button _txtStrikeout;
    private Combo _txtFont;
    private Combo _txtFontSize;
    private Button _txtFGColor;
    private Button _txtBGColor;
    
    public PageEditor(Composite parent, MessageEditor msg, String type) {
        _parent = parent;
        _messageEditor = msg;
        _contentType = type;
        buildControls();
    }
    
    public Control getControl() { return _root; }
    
    private Color getControlFGColor() { return ifDiff(_txtFGColor.getBackground(), _txtFGColor.getParent().getBackground()); }
    private Color getControlBGColor() { return ifDiff(_txtBGColor.getBackground(), _txtBGColor.getParent().getBackground()); }
    private Image getControlPageBGImage() { return ifDiff(_text.getBackgroundImage(), _text.getParent().getBackgroundImage()); }
    private Color getControlPageBGColor() { return ifDiff(_pageBGColor.getBackground(), _pageBGColor.getParent().getBackground()); }
    private boolean getControlBold() { return _txtBold.getSelection(); }
    private boolean getControlItalic() { return _txtItalic.getSelection(); }
    private boolean getControlUnderline() { return _txtUnderline.getSelection(); }
    private boolean getControlStrikeout() { return _txtStrikeout.getSelection(); }
    private String getControlFontName() { return _txtFont.getItem(_txtFont.getSelectionIndex()); }
    private String getControlFontSize() { return _txtFontSize.getItem(_txtFontSize.getSelectionIndex()); }
    private Color ifDiff(Color child, Color parent) {
        if (!child.equals(parent)) {
            //System.out.println("diff: " + child + " / " + parent);
            return child;
        } else {
            return null; 
        }
    }
    private Image ifDiff(Image child, Image parent) { if (!child.equals(parent)) return child; else return null; }
    
    public String getControlState() {
        StringBuffer buf = new StringBuffer();
        Color c = getControlBGColor();
        if (c != null) buf.append("bgcolor ").append(c.getRGB().toString()).append(" ");
        c = getControlFGColor();
        if (c != null) buf.append("fgcolor ").append(c.getRGB().toString()).append(" ");
        c = getControlPageBGColor();
        if (c != null) buf.append("pagebgcolor ").append(c.getRGB().toString()).append(" ");
        if (getControlBold()) buf.append("bold ");
        if (getControlItalic()) buf.append("italic ");
        if (getControlStrikeout()) buf.append("strikeout ");
        if (getControlUnderline()) buf.append("underline ");
        if (getControlFontName() != null) buf.append("font ").append(getControlFontName()).append(" ");
        if (getControlFontSize() != null) buf.append("fontsize ").append(getControlFontSize()).append(" ");
        return buf.toString();
    }

    private void buildControls() {
        _root = new Composite(_parent, SWT.BORDER);
        if ("text/html".equals(_contentType)) {
            createHTMLToolbar();
        } else {
            createTextToolbar();
        }
        _text = new StyledText(_root, SWT.MULTI | SWT.WRAP | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        
        createStyleChooser();
        preparePreview();
        
        GridLayout gl = new GridLayout(1, true);
        _root.setLayout(gl);
        GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = GridData.FILL;
        gd.verticalAlignment = GridData.FILL;
        _root.setLayoutData(gd);
        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = false;
        gd.horizontalAlignment = GridData.FILL;
        gd.verticalAlignment = GridData.FILL;
        _toolbars.setLayoutData(gd);
        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = GridData.FILL;
        gd.verticalAlignment = GridData.FILL;
        _text.setLayoutData(gd);
        _text.setEditable(true);
        _text.setDoubleClickEnabled(true);
    }
    
    private void createHTMLToolbar() { createToolbar(true); }
    private void createTextToolbar() { createToolbar(false); }
    private void createToolbar(boolean enable) {
        _toolbars = new Composite(_root, SWT.NONE);
        
        Group grpHTML = new Group(_toolbars, SWT.SHADOW_ETCHED_IN);
        grpHTML.setText("HTML");
        _htmlStyleChooser = new Button(grpHTML, SWT.PUSH);
        _htmlStyleChooser.setText("text style");
        _htmlStyleChooser.setToolTipText("Build the text styling");
        _htmlStyleChooser.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showStyleChooser(); }
            public void widgetSelected(SelectionEvent selectionEvent) { showStyleChooser(); }
        });
        _htmlLink = new Button(grpHTML, SWT.PUSH);
        _htmlLink.setText("link");
        _htmlLink.setToolTipText("Add a new hyperlink");
        _htmlImg = new Button(grpHTML, SWT.PUSH);
        _htmlImg.setText("img");
        _htmlImg.setToolTipText("Add a new image");
        _htmlSymbol = new Button(grpHTML, SWT.PUSH);
        _htmlSymbol.setText("sym");
        _htmlSymbol.setToolTipText("Add a new symbol");
        _htmlHeader = new Combo(grpHTML, SWT.DROP_DOWN | SWT.READ_ONLY);
        _htmlHeader.add("none");
        _htmlHeader.add("H1");
        _htmlHeader.add("H2");
        _htmlHeader.add("H3");
        _htmlHeader.add("H4");
        _htmlHeader.add("H5");
        _htmlHeader.select(0);
        _htmlHeader.setToolTipText("Use a header");
        _htmlHeader.setEnabled(enable); // needs to be done explicitly for some reason...
        _htmlHeader.addSelectionListener(new HeaderListener());
        _htmlPre = new Button(grpHTML, SWT.PUSH);
        _htmlPre.setText("pre");
        _htmlPre.setToolTipText("Add preformatted text");
        _htmlPre.addSelectionListener(new InsertListener("<pre>first line\n\tindented line</pre>", true));
        _htmlQuote = new Button(grpHTML, SWT.PUSH);
        _htmlQuote.setText("Q");
        
        Group grpPage = new Group(_toolbars, SWT.SHADOW_ETCHED_IN);
        grpPage.setText("Page");
        _pageBGImage = new Button(grpPage, SWT.PUSH);
        _pageBGImage.setText("bgimg");
        _pageBGImage.setToolTipText("Set the background image");
        _pageBGColor = buildColorCombo(grpPage, "pagebg", "Adjust the page bg color", "white", enable);
        
        Group grpList = new Group(_toolbars, SWT.SHADOW_ETCHED_IN);
        grpList.setText("List");
        _listOrdered = new Button(grpList, SWT.PUSH);
        _listOrdered.setText("1)");
        _listOrdered.setToolTipText("Add a new ordered list");
        _listOrdered.addSelectionListener(new InsertListener("<ol>\n\t<li>first list item</li>\n</ol>\n", true));
        _listUnordered = new Button(grpList, SWT.PUSH);
        _listUnordered.setText("*)");
        _listUnordered.setToolTipText("Add a new unordered list");
        _listUnordered.addSelectionListener(new InsertListener("<ul>\n\t<li>first list item</li>\n</ul>\n", true));

        Group grpMeta = new Group(_toolbars, SWT.HORIZONTAL);
        grpMeta.setText("Meta");
        _metaCopy = new Button(grpMeta, SWT.PUSH);
        _metaCopy.setText("^C");
        _metaCopy.setToolTipText("copy");
        _metaCopy.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _text.copy(); _text.forceFocus(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _text.copy(); _text.forceFocus(); }
        });
        _metaPaste = new Button(grpMeta, SWT.PUSH);
        _metaPaste.setText("^V");
        _metaPaste.setToolTipText("paste");
        _metaPaste.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _text.paste(); _text.forceFocus(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _text.paste(); _text.forceFocus(); }
        });
        _metaCut = new Button(grpMeta, SWT.PUSH);
        _metaCut.setText("^X");
        _metaCut.setToolTipText("cut");
        _metaCut.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _text.cut(); _text.forceFocus(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _text.cut(); _text.forceFocus(); }
        });
        _metaSpell = new Button(grpMeta, SWT.PUSH);
        _metaSpell.setText("sp");
        _metaSpell.setToolTipText("spellcheck");
        _metaFind = new Button(grpMeta, SWT.PUSH);
        _metaFind.setText("^F");
        _metaFind.setToolTipText("find");
        _metaReplace = new Button(grpMeta, SWT.PUSH);
        _metaReplace.setText("^R");
        _metaReplace.setToolTipText("replace");
        _metaPreview = new Button(grpMeta, SWT.PUSH);
        _metaPreview.setText("preview");
        _metaPreview.setToolTipText("preview");
        _metaPreview.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { preview(); }
            public void widgetSelected(SelectionEvent selectionEvent) { preview(); }
        });
        
        Group grpDebug = new Group(_toolbars, SWT.HORIZONTAL);
        grpDebug.setText("Debug");
        Button dumpStyle = new Button(grpDebug, SWT.PUSH);
        dumpStyle.setText("styles");
        dumpStyle.setToolTipText("walk styles");
        dumpStyle.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { dumpStyles(); }
            public void widgetSelected(SelectionEvent selectionEvent) { dumpStyles(); }
        });
        Button dumpStatus = new Button(grpDebug, SWT.PUSH);
        dumpStatus.setText("status");
        dumpStatus.setToolTipText("display all status info");
        dumpStatus.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { dumpStatus(); }
            public void widgetSelected(SelectionEvent selectionEvent) { dumpStatus(); }
        });
        
        grpDebug.setEnabled(true);
        RowLayout rl = new RowLayout(SWT.HORIZONTAL);
        grpDebug.setLayout(rl);
        grpMeta.setEnabled(enable);
        rl = new RowLayout(SWT.HORIZONTAL);
        grpMeta.setLayout(rl);
        grpList.setEnabled(enable);
        rl = new RowLayout(SWT.HORIZONTAL);
        grpList.setLayout(rl);
        grpPage.setEnabled(enable);
        rl = new RowLayout(SWT.HORIZONTAL);
        grpPage.setLayout(rl);
        grpHTML.setEnabled(enable);
        rl = new RowLayout(SWT.HORIZONTAL);
        grpHTML.setLayout(rl);
        
        rl = new RowLayout(SWT.HORIZONTAL);
        rl.fill = false;
        rl.wrap = true;
        _toolbars.setLayout(rl);
    }
    
    private void createStyleChooser() {
        _txtShell = new Shell(_parent.getShell(), SWT.DIALOG_TRIM);
        _txtShell.setText("Text style chooser");
        RowLayout rl = new RowLayout(SWT.VERTICAL);
        rl.fill = true;
        rl.wrap = false;
        _txtShell.setLayout(rl);
        
        Group grpText = new Group(_txtShell, SWT.SHADOW_ETCHED_IN);
        grpText.setLayout(new RowLayout(SWT.HORIZONTAL));
        grpText.setText("Styling");
        _txtBold = new Button(grpText, SWT.TOGGLE);
        _txtBold.setText("B");
        _txtItalic = new Button(grpText, SWT.TOGGLE);
        _txtItalic.setText("I");
        _txtUnderline = new Button(grpText, SWT.TOGGLE);
        _txtUnderline.setText("U");
        _txtStrikeout = new Button(grpText, SWT.TOGGLE);
        _txtStrikeout.setText("SO");
        _txtFont = new Combo(grpText, SWT.DROP_DOWN | SWT.READ_ONLY);
        _txtFont.add("Times");
        _txtFont.add("Courier");
        _txtFont.add("Helvetica");
        _txtFont.select(0);
        _txtFont.setToolTipText("Adjust the font");
        _txtFontSize = new Combo(grpText, SWT.DROP_DOWN | SWT.READ_ONLY);
        _txtFontSize.add("-5");
        _txtFontSize.add("-4");
        _txtFontSize.add("-3");
        _txtFontSize.add("-2");
        _txtFontSize.add("-1");
        _txtFontSize.add("+0");
        _txtFontSize.add("+1");
        _txtFontSize.add("+2");
        _txtFontSize.add("+3");
        _txtFontSize.add("+4");
        _txtFontSize.add("+5");
        _txtFontSize.select(5);
        _txtFontSize.setToolTipText("Adjust the font size");
        _txtFGColor = buildColorCombo(grpText, "color", "Adjust the fg color", "black", true);
        _txtBGColor = buildColorCombo(grpText, "bgcolor", "Adjust the bg color", "white", true);
        
        _sampleText = new StyledText(_txtShell, SWT.MULTI | SWT.WRAP | SWT.BORDER | SWT.READ_ONLY);
        _sampleText.setText("This is the sample text");
        
        Composite actionRow = new Composite(_txtShell, SWT.NONE);
        actionRow.setLayout(new FillLayout(SWT.HORIZONTAL));
        Button ok = new Button(actionRow, SWT.PUSH);
        ok.setText("ok");
        ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { insertTextStyle(); _txtShell.setVisible(false); }
            public void widgetSelected(SelectionEvent selectionEvent) { insertTextStyle(); _txtShell.setVisible(false); }
        });
        Button cancel = new Button(actionRow, SWT.PUSH);
        cancel.setText("cancel");
        cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { 
                resetTextStyle(); 
                _text.forceFocus();
                _txtShell.setVisible(false);
            }
            public void widgetSelected(SelectionEvent selectionEvent) {
                resetTextStyle(); 
                _text.forceFocus();
                _txtShell.setVisible(false);
            }
        });
        
        _txtShell.pack();
        
        PreviewStyle lsnr = new PreviewStyle();
        _txtBold.addSelectionListener(lsnr);
        _txtItalic.addSelectionListener(lsnr);
        _txtUnderline.addSelectionListener(lsnr);
        _txtStrikeout.addSelectionListener(lsnr);
        _txtFont.addSelectionListener(lsnr);
        _txtFontSize.addSelectionListener(lsnr);
        addListeners(lsnr, _txtFGColor);
        addListeners(lsnr, _txtBGColor);
    }
    
    private void addListeners(PreviewStyle lsnr, Button button) {
        Menu menu = button.getMenu();
        if (menu != null) {
            MenuItem items[] = menu.getItems();
            if (items != null) {
                for (int i = 0; i < items.length; i++)
                    items[i].addSelectionListener(lsnr);
            }
        }
        button.addSelectionListener(lsnr);
    }
    
    private class PreviewStyle implements SelectionListener, TraverseListener {
        private Font _sampleFont;
        public PreviewStyle() {
            _sampleFont = null;
        }
        public void widgetDefaultSelected(SelectionEvent selectionEvent) { redrawSample(); }
        public void widgetSelected(SelectionEvent selectionEvent) { redrawSample(); }
        public void keyTraversed(TraverseEvent traverseEvent) { redrawSample(); }
        private void redrawSample() {
            if ( (_sampleFont != null) && (!_sampleFont.isDisposed()) ) _sampleFont.dispose();
            _sampleFont = null;
            boolean bold = getControlBold();
            boolean italic = getControlItalic();
            boolean underline = getControlUnderline();
            boolean strikeout = getControlStrikeout();
            String font = getControlFontName();
            String sz = getControlFontSize();
            Color bgColor = getControlBGColor();
            Color fgColor = getControlFGColor();
            
            _sampleText.setStyleRanges(null, null);
            StyleRange range = new StyleRange(0, _sampleText.getCharCount(), fgColor, bgColor);
            range.strikeout = strikeout;
            range.underline = underline;
            range.font = getSampleFont(bold, italic, font, sz);
            _sampleText.setStyleRange(range);
            _txtShell.layout();
            _txtShell.pack(true);
        }
        
        private Font getSampleFont(boolean bold, boolean italic, String style, String sz) {
            int fontHeight = 12;
            try {
                if (sz.startsWith("+"))
                    sz = sz.substring(1);
                fontHeight += 2*Integer.parseInt(sz);
            } catch (NumberFormatException nfe) {
                // ignore
            }
            int fontStyle = SWT.NORMAL;
            if (bold) fontStyle |= SWT.BOLD;
            if (italic) fontStyle |= SWT.ITALIC;
            return new Font(Display.getDefault(), style, fontHeight, fontStyle);
        }
    }
    
    private void resetTextStyle() {
        _sampleText.setStyleRanges(null, null);
        _txtBold.setSelection(false);
        _txtItalic.setSelection(false);
        _txtUnderline.setSelection(false);
        _txtStrikeout.setSelection(false);
        _txtFont.select(0);
        _txtFontSize.select(5); // +0
        _txtFGColor.setBackground(null);
        _txtBGColor.setBackground(null);
    }
    
    private void insertTextStyle() {
        boolean bold = _txtBold.getSelection();
        boolean italic = _txtItalic.getSelection();
        boolean underline = _txtUnderline.getSelection();
        boolean strikeout = _txtStrikeout.getSelection();
        String font = _txtFont.getItem(_txtFont.getSelectionIndex());
        String sz = _txtFontSize.getItem(_txtFontSize.getSelectionIndex());
        String fg = ColorUtil.getSystemColorName(_txtFGColor.getBackground());
        String bg = ColorUtil.getSystemColorName(_txtBGColor.getBackground());
        
        StringBuffer buf = new StringBuffer();
        if (bold) buf.append("<b>");
        if (italic) buf.append("<i>");
        if (underline) buf.append("<u>");
        if (strikeout) buf.append("<so>");
        
        boolean fontSet = false;
        if ( (!"Times".equals(font)) || (!"+0".equals(sz)) || (fg != null) || (bg != null) )
            fontSet = true;
        
        if (fontSet) {
            buf.append("<font ");
            if (!"Times".equals(font)) buf.append("name=\"").append(font).append("\" ");
            if (!"+0".equals(sz)) buf.append(" size=\"").append(sz).append("\" ");
            if (fg != null) buf.append(" color=\"").append(fg).append("\" ");
            if (bg != null) buf.append(" bgcolor=\"").append(bg).append("\" ");
            buf.append(">");
        }
        
        buf.append("CONTENT GOES HERE");
        
        if (fontSet) buf.append("</font>");
        if (strikeout) buf.append("</so>");
        if (underline) buf.append("</u>");
        if (italic) buf.append("</i>");
        if (bold) buf.append("</b>");
        
        insertAtCaret(buf.toString());
    }
    
    private void insertAtCaret(String text) {
        // rather than replacing everything selected, just insert at the caret
        _text.replaceTextRange(_text.getCaretOffset(), 0, text);
        //_text.insert(buf.toString());
        _text.setCaretOffset(_text.getCaretOffset()+text.length());
        boolean focused = _text.forceFocus();
        System.out.println("forced focus: " + focused);
    }
    
    private boolean isAtBeginningOfLine(int offset) {
        if (offset == 0) return true;
        int len = _text.getCharCount();
        String txt = null;
        if (offset + 1 >= len)
            txt = _text.getText(0, offset);
        else
            txt = _text.getText(0, offset+1);
        boolean isInTag = false;
        for (int i = offset; i >= 0; i--) {
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
    
    private class HeaderListener implements SelectionListener {
        public void widgetSelected(SelectionEvent selectionEvent) { insert(); }
        public void widgetDefaultSelected(SelectionEvent selectionEvent) { insert(); }
        private void insert() {
            String header = _htmlHeader.getItem(_htmlHeader.getSelectionIndex());
            String toInsert = null;
            if ("h1".equals(header))
                toInsert = "<h1>HEADER</h1>\n";
            else if ("h2".equals(header))
                toInsert = "<h2>HEADER</h2>\n";
            if ("h3".equals(header))
                toInsert = "<h3>HEADER</h3>\n";
            if ("h4".equals(header))
                toInsert = "<h4>HEADER</h4>\n";
            if ("h5".equals(header))
                toInsert = "<h5>HEADER</h5>\n";
            
            if (!isAtBeginningOfLine(_text.getCaretOffset()))
                toInsert = "\n" + toInsert;
            
            insertAtCaret(toInsert);
        }
    }
    
    private class InsertListener implements SelectionListener {
        private boolean _onNewline;
        private String _toInsert;
        public InsertListener(String toInsert, boolean onNewline) {
            _onNewline = onNewline;
            _toInsert = toInsert;
        }
        public void widgetSelected(SelectionEvent selectionEvent) { insert(); }
        public void widgetDefaultSelected(SelectionEvent selectionEvent) { insert(); }
        private void insert() {
            if (_onNewline && !isAtBeginningOfLine(_text.getCaretOffset()))
                insertAtCaret('\n' + _toInsert);
            else
                insertAtCaret(_toInsert);
        }
    }
    
    private void showStyleChooser() { resetTextStyle(); _txtShell.setVisible(true); }
    
    private Button buildColorCombo(Group parent, String name, String tooltip, String defaultColor, boolean enable) {
        final Button rv = new Button(parent, SWT.PUSH);
        rv.setText(name);
        final Menu colorMenu = new Menu(rv);
        MenuItem none = new MenuItem(colorMenu, SWT.PUSH);
        none.setText("default");
        if ( (defaultColor == null) || ("default".equalsIgnoreCase(defaultColor)))
            none.setSelection(true);
        none.addSelectionListener(new ColorMenuItemListener(rv, null));
        ArrayList names = ColorUtil.getSystemColorNames();
        for (int i = 0; i < names.size(); i++) {
            String colorName = (String)names.get(i);
            Color color = (Color)ColorUtil.getColor(colorName, null);
            MenuItem item = new MenuItem(colorMenu, SWT.PUSH);
            item.setText(colorName);
            item.setImage(ColorUtil.getSystemColorSwatch(color));
            if (colorName.equalsIgnoreCase(defaultColor))
                item.setSelection(true);
            item.addSelectionListener(new ColorMenuItemListener(rv, colorName));
        }
        rv.setMenu(colorMenu);
        rv.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { colorMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { colorMenu.setVisible(true); }
        });
        rv.setToolTipText(tooltip);
        rv.setEnabled(enable);
        return rv;
    }

    private class ColorMenuItemListener implements SelectionListener {
        private Button _button;
        private String _color;
        public ColorMenuItemListener(Button button, String color) {
            _button = button;
            _color = color;
        }
        public void widgetDefaultSelected(SelectionEvent selectionEvent) {
            _button.setBackground(ColorUtil.getColor(_color, null));
        }
        public void widgetSelected(SelectionEvent selectionEvent) {
            _button.setBackground(ColorUtil.getColor(_color, null));
        }
    }

    private void preparePreview() {
        ScrolledComposite scroll = new ScrolledComposite(_root, SWT.H_SCROLL | SWT.V_SCROLL);
        scroll.setExpandHorizontal(true);
        scroll.setExpandVertical(true);
        _preview = new PageRenderer(scroll);
        scroll.setContent(_preview.getComposite());
        scroll.setLayout(new FillLayout());
        GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = false;
        gd.horizontalAlignment = GridData.FILL;
        gd.verticalAlignment = GridData.FILL;
        scroll.setLayoutData(gd);
    }
    private static final SyndieURI _dummyURI = SyndieURI.createMessage(new Hash(new byte[Hash.HASH_LENGTH]), Long.MAX_VALUE, 0);
    private void preview() {
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
        HashMap attachments = new HashMap();
        ArrayList attachmentOrder = new ArrayList();
        PageRendererSourceMem src = new PageRendererSourceMem(null, msgInfo, pageData, attachments, attachmentOrder);
        _preview.renderPage(src, _dummyURI);
    }
    
    private void dumpStyles() {
        System.out.println("dumping styles: text=[" + _text.getText() + "]");
        StyleRange ranges[] = _text.getStyleRanges();
        if ( (ranges != null) && (ranges.length > 0) ) {
            for (int i = 0; i < ranges.length; i++)
                System.out.println("range " + i + ": " + ranges[i].toString());
        } else {
            System.out.println("no styles");
        }
    }
    private void dumpSelection() {
        Point range = _text.getSelectionRange();
        if (range == null) {
            System.out.println("No content selected");
            int offset = _text.getCaretOffset();
            System.out.println("Caret offset: " + offset);
            return;
        }
        System.out.println("Selection range: " + range.x + " through " + (range.x+range.y));
        System.out.println("[" + _text.getTextRange(range.x, range.y) + "]");
        int offset = _text.getCaretOffset();
        System.out.println("Caret offset: " + offset);
    }
    private void dumpStatus() {
        dumpStyles();
        dumpSelection();
        System.out.println("Control: " + getControlState());
    }
}
