package syndie.gui;

import java.util.ArrayList;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ExtendedModifyEvent;
import org.eclipse.swt.custom.ExtendedModifyListener;
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
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.CoolBar;
import org.eclipse.swt.widgets.CoolItem;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Item;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

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
    
    // text toolbar
    private Button _txtBold;
    private Button _txtItalic;
    private Button _txtUnderline;
    private Button _txtStrikeout;
    private Combo _txtFont;
    private Combo _txtFontSize;
    private Button _txtFGColor;
    private Button _txtBGColor;
    // html toolbar
    private Button _htmlLink;
    private Button _htmlImg;
    private Button _htmlSymbol;
    private Combo _htmlHeader;
    private Button _htmlQuote;
    // page toolbar
    private Button _pageBGImage;
    private Button _pageBGColor;
    // list toolbar
    private Button _listOrdered;
    private Button _listUnordered;
    private Button _listIndentMore;
    private Button _listIndentLess;
    // meta toolbar
    private Button _metaCopy;
    private Button _metaPaste;
    private Button _metaCut;
    private Button _metaSpell;
    private Button _metaFind;
    private Button _metaReplace;
    
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
    private boolean getControlQuote() { return _htmlQuote.getSelection(); }
    private Color ifDiff(Color child, Color parent) { if (!child.equals(parent)) { System.out.println("diff: " + child + " / " + parent); return child; } else return null; }
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
        if (getControlQuote()) buf.append("quote ");
        if (getControlStrikeout()) buf.append("strikeout ");
        if (getControlUnderline()) buf.append("underline ");
        if (getControlFontName() != null) buf.append("font " + getControlFontName());
        if (getControlFontSize() != null) buf.append("fontsize " + getControlFontSize());
        return buf.toString();
    }

    private void buildControls() {
        _root = new Composite(_parent, SWT.BORDER);
        if ("text/html".equals(_contentType)) {
            createHTMLToolbar();
        } else {
            createTextToolbar();
        }
        _text = new StyledText(_root, SWT.MULTI | SWT.WRAP | SWT.BORDER);
        // lots of situations can cause us to want to update the text style controls, so
        // listen to 'em all
        _text.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent traverseEvent) { updateControls(); }
        });
        _text.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) { updateControls(); }
            public void mouseDown(MouseEvent mouseEvent) {}
            public void mouseUp(MouseEvent mouseEvent) { updateControls(); }
        });
        _text.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { updateControls(); }
            public void widgetSelected(SelectionEvent selectionEvent) { updateControls(); }
        });
        _text.addVerifyListener(new VerifyListener() {
            public void verifyText(VerifyEvent evt) {
                if (evt.start != evt.end) {
                    // some old styles are going to be adjusted (in the extendedModifyListener), but
                    // we may need to do some cleanup here before the change is made
                    System.out.println("modifyStyles(" + evt.start + ", " + evt.end + ", " + evt.text + ")");
                    modifyStyles(evt.start, evt.end, evt.text);
                }
            }
        });
        _text.addExtendedModifyListener(new ExtendedModifyListener() {
            public void modifyText(ExtendedModifyEvent evt) {
                if (evt.length == 0)
                    applyStyleDelete(evt.start, evt.replacedText);
                else if ( (evt.replacedText == null) || (evt.replacedText.length() <= 0) )
                    applyStyleAdd(evt.start, evt.length);
                else
                    applyStyleChange(evt.start, evt.length, evt.replacedText);
            }
        });
        
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
        
        Group grpText = new Group(_toolbars, SWT.SHADOW_ETCHED_IN);
        grpText.setText("Text");
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
        _txtFont.setEnabled(enable); // needs to be done explicitly for some reason...
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
        _txtFontSize.setEnabled(enable); // needs to be done explicitly for some reason...
        _txtFGColor = buildColorCombo(grpText, "color", "Adjust the fg color", "black", enable);
        _txtBGColor = buildColorCombo(grpText, "bgcolor", "Adjust the bg color", "white", enable);
        
        Group grpHTML = new Group(_toolbars, SWT.SHADOW_ETCHED_IN);
        grpHTML.setText("HTML");
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
        _htmlQuote = new Button(grpHTML, SWT.TOGGLE);
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
        _listUnordered = new Button(grpList, SWT.PUSH);
        _listUnordered.setText("*)");
        _listUnordered.setToolTipText("Add a new unordered list");
        _listIndentMore = new Button(grpList, SWT.PUSH);
        _listIndentMore.setText("->");
        _listIndentMore.setToolTipText("indent more");
        _listIndentLess = new Button(grpList, SWT.PUSH);
        _listIndentLess.setText("<-");
        _listIndentLess.setToolTipText("indent less");

        Group grpMeta = new Group(_toolbars, SWT.HORIZONTAL);
        grpMeta.setText("Meta");
        _metaCopy = new Button(grpMeta, SWT.PUSH);
        _metaCopy.setText("^C");
        _metaCopy.setToolTipText("copy");
        _metaPaste = new Button(grpMeta, SWT.PUSH);
        _metaPaste.setText("^V");
        _metaPaste.setToolTipText("paste");
        _metaCut = new Button(grpMeta, SWT.PUSH);
        _metaCut.setText("^X");
        _metaCut.setToolTipText("cut");
        _metaSpell = new Button(grpMeta, SWT.PUSH);
        _metaSpell.setText("sp");
        _metaSpell.setToolTipText("spellcheck");
        _metaFind = new Button(grpMeta, SWT.PUSH);
        _metaFind.setText("^F");
        _metaFind.setToolTipText("find");
        _metaReplace = new Button(grpMeta, SWT.PUSH);
        _metaReplace.setText("^R");
        _metaReplace.setToolTipText("replace");
        
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
        grpText.setEnabled(enable);
        rl = new RowLayout(SWT.HORIZONTAL);
        grpText.setLayout(rl);
        
        rl = new RowLayout(SWT.HORIZONTAL);
        rl.fill = false;
        rl.wrap = true;
        _toolbars.setLayout(rl);
    }
    
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

    private void updateControls() {
        // update the control state based on the current selection range
    }

    /** text has been inserted, so style it accordingly */
    private void applyStyleAdd(int start, int length) {
        String newText = _text.getTextRange(start, length);
        System.out.println("added [" + newText + "]");
        
        boolean isInList = false; // true if start is inside a list
        if (isInList && ( (newText.indexOf('\r') >= 0) || (newText.indexOf('\n') >= 0) ) ) {
            // we are in a list and the user hit return, so we probably need to create a new
            // bullet, etc
        } else {
            // try to merge in with the previous (or subsequent) style
            StyleRange range = null;
            if (start > 0) {
                // pull in the previous style
                range = _text.getStyleRangeAtOffset(start-1);
                if (range != null)
                    range.length = range.length + length;
            } else if (length < _text.getCharCount()) {
                // we are at the start, so pull in the next style
                range = _text.getStyleRangeAtOffset(start+length+1);
                if (range != null) {
                    range.start = start;
                    range.length = range.length + length;
                }
            } else {
                // the page is blank, don't import any style
            }
            if (range == null)
                range = buildStyleRange(start, length);
            _text.setStyleRange(range);
        }
    }
    /** create a new style range based on the current control states*/
    private StyleRange buildStyleRange(int start, int length) {
        StyleRange rv = new StyleRange();
        rv.start = start;
        rv.length = length;
        rv.background = getControlBGColor();
        rv.foreground = getControlFGColor();
        rv.strikeout = getControlStrikeout();
        rv.underline = getControlUnderline();
        if (getControlBold()) rv.fontStyle |= SWT.BOLD;
        if (getControlItalic()) rv.fontStyle |= SWT.ITALIC;
        // todo: deal with fonts...
        System.out.println("new style range: " + rv);
        return rv;
    }
    
    /** text has been changed, so style it accordingly */
    private void applyStyleChange(int start, int length, String oldText) {
        System.out.println("changed [" + _text.getTextRange(start, length) + "] was [" + oldText + "]");
    }
    /** text has been deleted, so adjust the styles accordingly */
    private void applyStyleDelete(int start, String oldText) {
        System.out.println("deleted [" + oldText + "]");
        // delete any bullets, images, links
    }
    private void modifyStyles(int start, int end, String newText) {
        System.out.println("changing [" + _text.getText(start, end-1) + "] to [" + newText + "]");
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
