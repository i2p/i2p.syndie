package syndie.gui;

import java.util.ArrayList;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

class MessageEditorStyler implements Themeable, Translatable {
    private MessageEditor _editor;
    
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
    private Button _txtAlignLeft;
    private Button _txtAlignCenter;
    private Button _txtAlignRight;
    private Button _styleOk;
    private Button _styleCancel;
    private Font _sampleFont;
    
    private Group _grpText;
    private Group _grpAlign;
    
    public MessageEditorStyler(MessageEditor editor) {
        _editor = editor;
        initComponents();
    }
    
    public void open() {
        resetTextStyle();
        _txtShell.open();
    }
    public void dispose() {
        _editor.getDataControl().getThemeRegistry().unregister(this);
        _editor.getDataControl().getTranslationRegistry().unregister(this);
        if (!_txtShell.isDisposed())
            _txtShell.dispose();
        if ( (_sampleFont != null) && (!_sampleFont.isDisposed()) )
            _sampleFont.dispose();
        _sampleFont = null;
    }
    
    public Color getControlFGColor() { return ifDiff(_txtFGColor.getBackground(), _txtFGColor.getParent().getBackground()); }
    public Color getControlBGColor() { return ifDiff(_txtBGColor.getBackground(), _txtBGColor.getParent().getBackground()); }
    public boolean getControlBold() { return _txtBold.getSelection(); }
    public boolean getControlItalic() { return _txtItalic.getSelection(); }
    public boolean getControlUnderline() { return _txtUnderline.getSelection(); }
    public boolean getControlStrikeout() { return _txtStrikeout.getSelection(); }
    public String getControlFontName() { return _txtFont.getItem(_txtFont.getSelectionIndex()); }
    public String getControlFontSize() { return _txtFontSize.getItem(_txtFontSize.getSelectionIndex()); }
    
    private Color ifDiff(Color child, Color parent) {
        if (!child.equals(parent)) {
            //System.out.println("diff: " + child + " / " + parent);
            return child;
        } else {
            return null; 
        }
    }
    private Image ifDiff(Image child, Image parent) { if (!child.equals(parent)) return child; else return null; }

    private void initComponents() {
        _txtShell = new Shell(_editor.getPageRoot().getShell(), SWT.DIALOG_TRIM);
        _txtShell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; cancelStyle(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        RowLayout rl = new RowLayout(SWT.VERTICAL);
        rl.fill = true;
        rl.wrap = false;
        _txtShell.setLayout(rl);
        
        Composite controlBar = new Composite(_txtShell, SWT.NONE);
        controlBar.setLayout(new RowLayout(SWT.HORIZONTAL));
        _grpText = new Group(controlBar, SWT.SHADOW_ETCHED_IN);
        _grpText.setLayout(new RowLayout(SWT.HORIZONTAL));
        _txtBold = new Button(_grpText, SWT.TOGGLE);
        _txtItalic = new Button(_grpText, SWT.TOGGLE);
        _txtUnderline = new Button(_grpText, SWT.TOGGLE);
        _txtStrikeout = new Button(_grpText, SWT.TOGGLE);
        _txtFont = new Combo(_grpText, SWT.DROP_DOWN | SWT.READ_ONLY);
        _txtFont.add("Times");
        _txtFont.add("Courier");
        _txtFont.add("Helvetica");
        _txtFont.select(0);
        _txtFontSize = new Combo(_grpText, SWT.DROP_DOWN | SWT.READ_ONLY);
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
        _txtFGColor = buildColorCombo(_grpText, "color", "Adjust the fg color", "black", true);
        _txtBGColor = buildColorCombo(_grpText, "bgcolor", "Adjust the bg color", "white", true);
        
        _grpAlign = new Group(controlBar, SWT.SHADOW_ETCHED_IN);
        _grpAlign.setLayout(new RowLayout(SWT.HORIZONTAL));
        _txtAlignLeft = new Button(_grpAlign, SWT.RADIO);
        _txtAlignCenter = new Button(_grpAlign, SWT.RADIO);
        _txtAlignRight = new Button(_grpAlign, SWT.RADIO);
        
        _sampleText = new StyledText(_txtShell, SWT.MULTI | SWT.WRAP | SWT.BORDER | SWT.READ_ONLY);
        
        Composite actionRow = new Composite(_txtShell, SWT.NONE);
        actionRow.setLayout(new FillLayout(SWT.HORIZONTAL));
        _styleOk = new Button(actionRow, SWT.PUSH);
        _styleOk.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { insertTextStyle(); dispose(); }
            public void widgetSelected(SelectionEvent selectionEvent) { insertTextStyle(); dispose(); }
        });
        _styleCancel = new Button(actionRow, SWT.PUSH);
        _styleCancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { cancelStyle(); }
            public void widgetSelected(SelectionEvent selectionEvent) { cancelStyle(); }
        });
        
        PreviewStyle lsnr = new PreviewStyle();
        _txtBold.addSelectionListener(lsnr);
        _txtItalic.addSelectionListener(lsnr);
        _txtUnderline.addSelectionListener(lsnr);
        _txtStrikeout.addSelectionListener(lsnr);
        _txtFont.addSelectionListener(lsnr);
        _txtFontSize.addSelectionListener(lsnr);
        _txtAlignLeft.addSelectionListener(lsnr);
        _txtAlignCenter.addSelectionListener(lsnr);
        _txtAlignRight.addSelectionListener(lsnr);
        addListeners(lsnr, _txtFGColor);
        addListeners(lsnr, _txtBGColor);
        
        lsnr.redrawSample();
    
        _editor.getDataControl().getTranslationRegistry().register(this);
        _editor.getDataControl().getThemeRegistry().register(this);
    }
    
    private void cancelStyle() {
        _txtShell.setVisible(false);
        resetTextStyle();
        _editor.cancelStyle();
        dispose();
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
        public void widgetDefaultSelected(SelectionEvent selectionEvent) { redrawSample(); }
        public void widgetSelected(SelectionEvent selectionEvent) { redrawSample(); }
        public void keyTraversed(TraverseEvent traverseEvent) { redrawSample(); }
        public void redrawSample() {
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
            _sampleFont = getSampleFont(bold, italic, font, sz);
            range.font = _sampleFont;
            int align = SWT.LEFT;
            if (_txtAlignCenter.getSelection()) align = SWT.CENTER;
            else if (_txtAlignRight.getSelection()) align = SWT.RIGHT;
            _sampleText.setStyleRange(range);
            _txtShell.pack(true);
            // align has to be after pack, otherwise it gets lost for some reason
            _sampleText.setLineAlignment(0, 1, align);
        }
        
        private Font getSampleFont(boolean bold, boolean italic, String style, String sz) {
            int fontHeight = Theme.getSize(_editor.getDataControl().getThemeRegistry().getTheme().CONTENT_FONT);
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
        if ( (_sampleFont != null) && (!_sampleFont.isDisposed()) ) _sampleFont.dispose();
        _sampleFont = null;
        _sampleText.setStyleRanges(null, null);
        _sampleText.setLineAlignment(0, 1, SWT.LEFT);
        _txtBold.setSelection(false);
        _txtItalic.setSelection(false);
        _txtUnderline.setSelection(false);
        _txtStrikeout.setSelection(false);
        _txtFont.select(0);
        _txtFontSize.select(5); // +0
        _txtFGColor.setBackground(null);
        _txtBGColor.setBackground(null);
        _txtAlignLeft.setSelection(true);
        _txtAlignCenter.setSelection(false);
        _txtAlignRight.setSelection(false);
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
        
        String align = null;
        if (_txtAlignCenter.getSelection())
            align = " align=\"center\"";
        else if (_txtAlignRight.getSelection())
            align = " align=\"right\"";
        
        StringBuffer buf = new StringBuffer();
        if (bold) {
            buf.append("<b");
            if (align != null)
                buf.append(align);
            align = null;
            buf.append(">");
        }
        if (italic) {
            buf.append("<i");
            if (align != null)
                buf.append(align);
            align = null;
            buf.append(">");
        }
        if (underline) {
            buf.append("<u");
            if (align != null)
                buf.append(align);
            align = null;
            buf.append(">");
        }
        if (strikeout) {
            buf.append("<so");
            if (align != null)
                buf.append(align);
            align = null;
            buf.append(">");
        }
        
        boolean fontSet = false;
        if ( (!"Times".equals(font)) || (!"+0".equals(sz)) || (fg != null) || (bg != null) || (align != null) )
            fontSet = true;
        
        if (fontSet) {
            buf.append("<font ");
            if (!"Times".equals(font)) buf.append(" name=\"").append(font).append("\"");
            if (!"+0".equals(sz)) buf.append(" size=\"").append(sz).append("\"");
            if (fg != null) buf.append(" color=\"").append(fg).append("\"");
            if (bg != null) buf.append(" bgcolor=\"").append(bg).append("\"");
            if (align != null) buf.append(align);
            buf.append(">");
        }
        
        int begin = buf.length();
        String sel = _editor.getSelectedText();
        if ( (sel == null) || (sel.trim().length() <= 0) )
            buf.append("CONTENT GOES HERE");
        else
            buf.append(sel);
        int end = buf.length();
        
        if (fontSet) buf.append("</font>");
        if (strikeout) buf.append("</so>");
        if (underline) buf.append("</u>");
        if (italic) buf.append("</i>");
        if (bold) buf.append("</b>");
        
        String str = buf.toString();
        boolean insert = ( (sel == null) || (sel.trim().length() == 0) );
        _editor.insertStyle(str, insert, begin, end);
    }

    private Button buildColorCombo(Group parent, String name, String tooltip, String defaultColor, boolean enable) {
        return buildColorCombo(parent, name, tooltip, defaultColor, enable, null);
    }
    private Button buildColorCombo(Group parent, String name, String tooltip, String defaultColor, boolean enable, Runnable onSelect) {
        final Button rv = new Button(parent, SWT.PUSH);
        ArrayList names = ColorUtil.getSystemColorNames();
        rv.setText(name);
        final Menu colorMenu = new Menu(rv);
        MenuItem none = new MenuItem(colorMenu, SWT.PUSH);
        none.setText(_editor.getDataControl().getTranslationRegistry().getText(T_COLOR_DEFAULT, "default"));
        if ( (defaultColor == null) || (!names.contains(defaultColor)) )
            none.setSelection(true);
        none.addSelectionListener(new ColorMenuItemListener(rv, null, onSelect));
        for (int i = 0; i < names.size(); i++) {
            String colorName = (String)names.get(i);
            Color color = (Color)ColorUtil.getColor(colorName, null);
            MenuItem item = new MenuItem(colorMenu, SWT.PUSH);
            item.setText(colorName);
            item.setImage(ColorUtil.getSystemColorSwatch(color));
            if (colorName.equalsIgnoreCase(defaultColor))
                item.setSelection(true);
            item.addSelectionListener(new ColorMenuItemListener(rv, colorName, onSelect));
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
        private Runnable _onSelect;
        public ColorMenuItemListener(Button button, String color, Runnable onSelect) {
            _button = button;
            _color = color;
            _onSelect = onSelect;
        }
        public void widgetDefaultSelected(SelectionEvent selectionEvent) { pickColor(); }
        public void widgetSelected(SelectionEvent selectionEvent) { pickColor(); }
        private void pickColor() {
            Color color = ColorUtil.getColor(_color, null);
            _button.setBackground(color);
            if (color == null) {
                _button.setForeground(ColorUtil.getColor("black", null));
            } else {
                // not the best heuristic, but it seems to work ok
                if ( (color.getRed() <= 128) && (color.getGreen() <= 128) && (color.getBlue() <= 128) )
                    _button.setForeground(ColorUtil.getColor("white", null));
                else
                    _button.setForeground(ColorUtil.getColor("black", null));
            }
            _editor.modified();
            if (_onSelect != null) _onSelect.run();
        }
    }
    
    private static final String T_COLOR_DEFAULT = "syndie.gui.messageeditorstyler.color.default";
    private static final String T_STYLE = "syndie.gui.messageeditorstyler.style";
    private static final String T_TEXT = "syndie.gui.messageeditorstyler.text";
    private static final String T_TEXT_B = "syndie.gui.messageeditorstyler.text.bold";
    private static final String T_TEXT_I = "syndie.gui.messageeditorstyler.text.italic";
    private static final String T_TEXT_U = "syndie.gui.messageeditorstyler.text.underline";
    private static final String T_TEXT_SO = "syndie.gui.messageeditorstyler.text.strikeout";
    private static final String T_TEXT_FONT = "syndie.gui.messageeditorstyler.text.font";
    private static final String T_TEXT_FONTSIZE = "syndie.gui.messageeditorstyler.text.fontsize";
    private static final String T_TEXT_ALIGN = "syndie.gui.messageeditorstyler.text.align";
    private static final String T_TEXT_ALIGN_LEFT = "syndie.gui.messageeditorstyler.text.align.left";
    private static final String T_TEXT_ALIGN_LEFT_TOOLTIP = "syndie.gui.messageeditorstyler.text.align.lefttooltip";
    private static final String T_TEXT_ALIGN_CENTER = "syndie.gui.messageeditorstyler.text.align.center";
    private static final String T_TEXT_ALIGN_CENTER_TOOLTIP = "syndie.gui.messageeditorstyler.text.align.centertooltip";
    private static final String T_TEXT_ALIGN_RIGHT = "syndie.gui.messageeditorstyler.text.align.right";
    private static final String T_TEXT_ALIGN_RIGHT_TOOLTIP = "syndie.gui.messageeditorstyler.text.align.righttooltip";
    private static final String T_TEXT_SAMPLE = "syndie.gui.messageeditorstyler.text.sample";
    private static final String T_TEXT_OK = "syndie.gui.messageeditorstyler.text.ok";
    private static final String T_TEXT_CANCEL = "syndie.gui.messageeditorstyler.text.cancel";
    
    public void translate(TranslationRegistry registry) {
        // todo: translate
        _txtShell.setText(registry.getText(T_STYLE, "Text style chooser"));        
        _grpText.setText(registry.getText(T_TEXT, "Styling"));
        _txtBold.setText(registry.getText(T_TEXT_B, "B"));
        _txtItalic.setText(registry.getText(T_TEXT_I, "I"));
        _txtUnderline.setText(registry.getText(T_TEXT_U, "U"));
        _txtStrikeout.setText(registry.getText(T_TEXT_SO, "SO"));
        _txtFont.setToolTipText(registry.getText(T_TEXT_FONT, "Adjust the font"));
        _txtFontSize.setToolTipText(registry.getText(T_TEXT_FONTSIZE, "Adjust the font size"));
        _grpAlign.setText(registry.getText(T_TEXT_ALIGN, "Alignment"));
        _txtAlignLeft.setText(registry.getText(T_TEXT_ALIGN_LEFT, "left"));
        _txtAlignLeft.setToolTipText(registry.getText(T_TEXT_ALIGN_LEFT_TOOLTIP, "Align the text to the left"));
        _txtAlignCenter.setText(registry.getText(T_TEXT_ALIGN_CENTER, "center"));
        _txtAlignCenter.setToolTipText(registry.getText(T_TEXT_ALIGN_CENTER_TOOLTIP, "Align the text to the center"));
        _txtAlignRight.setText(registry.getText(T_TEXT_ALIGN_RIGHT, "right"));
        _txtAlignRight.setToolTipText(registry.getText(T_TEXT_ALIGN_RIGHT_TOOLTIP, "Align the text to the right"));
        _sampleText.setText(registry.getText(T_TEXT_SAMPLE, "This is the sample text"));
        _styleOk.setText(registry.getText(T_TEXT_OK, "ok"));
        _styleCancel.setText(registry.getText(T_TEXT_CANCEL, "cancel"));
    }
    
    public void applyTheme(Theme theme) {
        _txtShell.setFont(theme.SHELL_FONT);
        _txtBold.setFont(theme.BUTTON_FONT);
        _txtItalic.setFont(theme.BUTTON_FONT);
        _txtUnderline.setFont(theme.BUTTON_FONT);
        _txtStrikeout.setFont(theme.BUTTON_FONT);
        _txtFont.setFont(theme.DEFAULT_FONT);
        _txtFontSize.setFont(theme.DEFAULT_FONT);
        _txtFGColor.setFont(theme.BUTTON_FONT);
        _txtBGColor.setFont(theme.BUTTON_FONT);
        _txtAlignLeft.setFont(theme.BUTTON_FONT);
        _txtAlignCenter.setFont(theme.BUTTON_FONT);
        _txtAlignRight.setFont(theme.BUTTON_FONT);
        _styleOk.setFont(theme.BUTTON_FONT);
        _styleCancel.setFont(theme.BUTTON_FONT);
        _grpText.setFont(theme.DEFAULT_FONT);
        _grpAlign.setFont(theme.DEFAULT_FONT);
        _txtShell.pack();
    }
}