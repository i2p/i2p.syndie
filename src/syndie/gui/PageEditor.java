package syndie.gui;

import com.swabunga.spell.engine.SpellDictionaryHashMap;
import com.swabunga.spell.engine.Word;
import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import net.i2p.data.Hash;
import net.i2p.util.SimpleTimer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ExtendedModifyEvent;
import org.eclipse.swt.custom.ExtendedModifyListener;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.custom.VerifyKeyListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
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
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import syndie.data.MessageInfo;
import syndie.data.SyndieURI;

import com.swabunga.spell.engine.SpellDictionary;
import syndie.db.DBClient;

/**
 * wysiwyg editor for text or html pages in a message
 */
public class PageEditor {
    private DBClient _client;
    private MessageEditor _messageEditor;
    private String _contentType;
    private Composite _parent;
    private Composite _root;
    private Composite _toolbars;
    private SashForm _sash;
    private StyledText _text;
    private PageRenderer _preview;
    
    private long _lastModified;
    private long _lastPreviewed;
    
    // html toolbar
    private Button _htmlStyleChooser;
    private Button _htmlLink;
    private Button _htmlImg;
    private Button _htmlSymbol;
    private Button _htmlHeader;
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
    private Button _txtAlignLeft;
    private Button _txtAlignCenter;
    private Button _txtAlignRight;
    
    // spell checker dialog
    private Shell _spellShell;
    private StyledText _spellContext;
    /** line in the text buffer we are spellchecking */
    private int _spellLine;
    /** word in the line that we are spellchecking */
    private int _spellWordIndex;
    /** offset in the entire text to the current word being spellchecked */
    private int _spellWordStart;
    /** offset in the entire text to the current word being spellchecked (end is inclusive) */
    private int _spellWordEnd;
    private Text _spellWord;
    private Combo _spellSuggestions;
    private Button _spellReplace;
    private Button _spellReplaceAll;
    private Button _spellIgnore;
    private Button _spellIgnoreAll;
    private Button _spellAdd;
    private Button _spellCancel;
    /** list of words we are ignoring for the current spellcheck iteration */
    private ArrayList _spellIgnoreAllList;
    private static SpellDictionary _spellDictionary;

    // search and replace dialog
    private Shell _findShell;
    private Text _findText;
    private Text _findReplace;
    private Button _findMatchCase;
    private Button _findWrapAround;
    private Button _findBackwards;
    private StyleRange _findHighlight;
    /** true if the find already wrapped around */
    private boolean _findWrapped;
    
    private LinkBuilderPopup _linkPopup;
    private ImageBuilderPopup _imagePopup;
    
    public PageEditor(DBClient client, Composite parent, MessageEditor msg, String type) {
        _client = client;
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
        boolean html = "text/html".equals(_contentType);
        if (html)
            createHTMLToolbar();
        else
            createTextToolbar();
        _sash = new SashForm(_root, SWT.VERTICAL);
        _text = new StyledText(_sash, SWT.MULTI | SWT.WRAP | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        _text.setEditable(true);
        _text.setDoubleClickEnabled(true);
        addEditListeners();
        
        _preview = new PageRenderer(_sash, true);
        if (html) {
            _sash.setMaximizedControl(null);
            _sash.setWeights(new int[] { 80, 20 });
        } else {
            _sash.setMaximizedControl(_text);
        }
        
        createStyleChooser();
        createSpellchecker();
        createFind();
        
        _linkPopup = new LinkBuilderPopup(_client, _parent.getShell(), this);
        _imagePopup = new ImageBuilderPopup(this);
        
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
        _sash.setLayoutData(gd);
        _root.setTabList(new Control[] { _sash }); //_text, _preview.getComposite() });
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
        _htmlLink.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showLinkPopup(); }
            public void widgetSelected(SelectionEvent selectionEvent) { showLinkPopup(); }
        });
        _htmlImg = new Button(grpHTML, SWT.PUSH);
        _htmlImg.setText("img");
        _htmlImg.setToolTipText("Add a new image");
        _htmlImg.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showImagePopup(false); }
            public void widgetSelected(SelectionEvent selectionEvent) { showImagePopup(false); }
        });
        _htmlSymbol = new Button(grpHTML, SWT.PUSH);
        _htmlSymbol.setText("sym");
        _htmlSymbol.setToolTipText("Add a new symbol");
        _htmlHeader = new Button(grpHTML, SWT.PUSH);
        prepareHeader();
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
        _pageBGImage.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showImagePopup(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { showImagePopup(true); }
        });
        _pageBGColor = buildColorCombo(grpPage, "pagebg", "Adjust the page bg color", "white", enable, new Runnable() {
            public void run() { setBodyTags(); }
        });
        
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
        _metaSpell.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { spellcheck(); }
            public void widgetSelected(SelectionEvent selectionEvent) { spellcheck(); }
        });
        _metaFind = new Button(grpMeta, SWT.PUSH);
        _metaFind.setText("^F");
        _metaFind.setToolTipText("find");
        _metaFind.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { find(); }
            public void widgetSelected(SelectionEvent selectionEvent) { find(); }
        });
        _metaPreview = new Button(grpMeta, SWT.CHECK);
        _metaPreview.setText("preview");
        _metaPreview.setToolTipText("Show the preview pane");
        _metaPreview.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { togglePreview(); }
            public void widgetSelected(SelectionEvent selectionEvent) { togglePreview(); }
        });
        _metaPreview.setSelection(true);
        
        enableActions(enable, grpMeta, grpList, grpPage, grpHTML);
        
        RowLayout rl = new RowLayout(SWT.HORIZONTAL);
        grpMeta.setLayout(rl);
        rl = new RowLayout(SWT.HORIZONTAL);
        grpList.setLayout(rl);
        rl = new RowLayout(SWT.HORIZONTAL);
        grpPage.setLayout(rl);
        rl = new RowLayout(SWT.HORIZONTAL);
        grpHTML.setLayout(rl);
        
        rl = new RowLayout(SWT.HORIZONTAL);
        rl.fill = false;
        rl.wrap = true;
        _toolbars.setLayout(rl);
    }
    
    private void enableActions(boolean enable, Group meta, Group list, Group page, Group html) {
        meta.setEnabled(enable);
        list.setEnabled(enable);
        page.setEnabled(enable);
        html.setEnabled(enable);
        
        _htmlImg.setEnabled(enable);
        _htmlHeader.setEnabled(enable);
        _htmlLink.setEnabled(enable);
        _htmlPre.setEnabled(enable);
        _htmlQuote.setEnabled(enable);
        _htmlStyleChooser.setEnabled(enable);
        _htmlSymbol.setEnabled(enable);
        
        _metaCopy.setEnabled(enable);
        _metaCut.setEnabled(enable);
        _metaFind.setEnabled(enable);
        _metaPaste.setEnabled(enable);
        _metaPreview.setEnabled(enable);
        _metaSpell.setEnabled(enable);
        
        _pageBGColor.setEnabled(enable);
        _pageBGImage.setEnabled(enable);
        
        _listOrdered.setEnabled(enable);
        _listUnordered.setEnabled(enable);
    }
    
    void setBodyTags() { setBodyTags(null); }
    void setBodyTags(String bgImageURL) {
        String bodyColor = ColorUtil.getSystemColorName(_pageBGColor.getBackground());
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
    
    private Properties getAttributes(String attributes) {
        Properties rv = new Properties();
        return rv;
    }
    
    private void prepareHeader() {
        _htmlHeader.setText("H*");
        final Menu headerMenu = new Menu(_htmlHeader);
        newHeader(headerMenu, "H1");
        newHeader(headerMenu, "H2");
        newHeader(headerMenu, "H3");
        newHeader(headerMenu, "H4");
        newHeader(headerMenu, "H5");
        _htmlHeader.setMenu(headerMenu);
        _htmlHeader.setToolTipText("Use a header");
        _htmlHeader.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { headerMenu.setVisible(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { headerMenu.setVisible(true); }
        });
    }
    
    private void newHeader(final Menu menu, String header) {
        MenuItem item = new MenuItem(menu, SWT.PUSH);
        item.setText(header);
        final String text = "<" + header + ">TEXT</" + header + ">";
        item.addSelectionListener(new InsertListener(text, true));
    }

    private void createStyleChooser() {
        _txtShell = new Shell(_parent.getShell(), SWT.DIALOG_TRIM);
        _txtShell.setText("Text style chooser");
        RowLayout rl = new RowLayout(SWT.VERTICAL);
        rl.fill = true;
        rl.wrap = false;
        _txtShell.setLayout(rl);
        
        Composite controlBar = new Composite(_txtShell, SWT.NONE);
        controlBar.setLayout(new RowLayout(SWT.HORIZONTAL));
        Group grpText = new Group(controlBar, SWT.SHADOW_ETCHED_IN);
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
        
        Group grpAlign = new Group(controlBar, SWT.SHADOW_ETCHED_IN);
        grpAlign.setLayout(new RowLayout(SWT.HORIZONTAL));
        grpAlign.setText("Alignment");
        _txtAlignLeft = new Button(grpAlign, SWT.RADIO);
        _txtAlignLeft.setText("left");
        _txtAlignLeft.setToolTipText("Align the text to the left");
        _txtAlignCenter = new Button(grpAlign, SWT.RADIO);
        _txtAlignCenter.setText("center");
        _txtAlignCenter.setToolTipText("Align the text to the center");
        _txtAlignRight = new Button(grpAlign, SWT.RADIO);
        _txtAlignRight.setText("right");
        _txtAlignRight.setToolTipText("Align the text to the right");
        
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
        _txtAlignLeft.addSelectionListener(lsnr);
        _txtAlignCenter.addSelectionListener(lsnr);
        _txtAlignRight.addSelectionListener(lsnr);
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
            int align = SWT.LEFT;
            if (_txtAlignCenter.getSelection()) align = SWT.CENTER;
            else if (_txtAlignRight.getSelection()) align = SWT.RIGHT;
            _sampleText.setStyleRange(range);
            _txtShell.pack(true);
            // align has to be after pack, otherwise it gets lost for some reason
            _sampleText.setLineAlignment(0, 1, align);
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
        
        buf.append("CONTENT GOES HERE");
        
        if (fontSet) buf.append("</font>");
        if (strikeout) buf.append("</so>");
        if (underline) buf.append("</u>");
        if (italic) buf.append("</i>");
        if (bold) buf.append("</b>");
        
        insertAtCaret(buf.toString());
    }
    
    void insertAtCaret(String text) {
        if (text != null) {
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
    
    /** simple hook to inert a buffer at the caret */
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
    private void showLinkPopup() { _linkPopup.showPopup(); }
    private void showImagePopup(boolean forBodyBackground) { _imagePopup.showPopup(forBodyBackground); }
    
    private Button buildColorCombo(Group parent, String name, String tooltip, String defaultColor, boolean enable) { return buildColorCombo(parent, name, tooltip, defaultColor, enable, null); }
    private Button buildColorCombo(Group parent, String name, String tooltip, String defaultColor, boolean enable, Runnable onSelect) {
        final Button rv = new Button(parent, SWT.PUSH);
        rv.setText(name);
        final Menu colorMenu = new Menu(rv);
        MenuItem none = new MenuItem(colorMenu, SWT.PUSH);
        none.setText("default");
        if ( (defaultColor == null) || ("default".equalsIgnoreCase(defaultColor)))
            none.setSelection(true);
        none.addSelectionListener(new ColorMenuItemListener(rv, null, onSelect));
        ArrayList names = ColorUtil.getSystemColorNames();
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
            if (_onSelect != null) _onSelect.run();
        }
    }

    private static final SyndieURI _dummyURI = SyndieURI.createMessage(new Hash(new byte[Hash.HASH_LENGTH]), Long.MAX_VALUE, 0);
    private void preview() {
        if (!_metaPreview.getSelection()) return;
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
        List names = _messageEditor.getAttachmentNames();
        for (int i = 0; i < names.size(); i++) {
            String name = (String)names.get(i);
            byte data[] = _messageEditor.getAttachmentData(i+1);
            attachmentOrder.add(name);
            attachments.add(data);
        }
        PageRendererSourceMem src = new PageRendererSourceMem(null, msgInfo, pageData, attachments, attachmentOrder);
        _preview.setRender(true);
        long before = System.currentTimeMillis();
        _preview.renderPage(src, _dummyURI);
        long renderTime = System.currentTimeMillis()-before;
        System.out.println("** render time: " + renderTime);
        _preview.setRender(false);
        _lastPreviewed = System.currentTimeMillis();
        _lastModified = -1;
    }
    
    private void addEditListeners() {
        _text.addExtendedModifyListener(new ExtendedModifyListener() {
            public void modifyText(ExtendedModifyEvent evt) {
                _lastModified = System.currentTimeMillis();
                SimpleTimer.getInstance().addEvent(_timedPreview, 500);
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
                    case 0x06: // ^F
                        if ( (evt.stateMask & SWT.MOD1) != 0) {
                            if (_findText.getText().length() > 0) {
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
                            evt.doit = false;
                        }
                        break;
                    case 0x12: // ^R
                        if ( (evt.stateMask & SWT.MOD1) != 0) {
                            if (_findText.getText().length() > 0)
                                findReplace();
                            else
                                find();
                        }
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
                            evt.doit = false;
                        }
                        break;
                }
                /*
                StringBuffer buf = new StringBuffer();
                if ( (evt.stateMask & SWT.ALT) != 0)
                    buf.append("ALT-");
                if ( (evt.stateMask & SWT.CONTROL) != 0)
                    buf.append("CNTR-");
                if ( (evt.stateMask & SWT.COMMAND) != 0)
                    buf.append("CMD-");
                if ( (evt.stateMask & SWT.MOD1) != 0) // control
                    buf.append("M1-");
                if ( (evt.stateMask & SWT.MOD2) != 0) // shift
                    buf.append("M2-");
                if ( (evt.stateMask & SWT.MOD3) != 0) // alt
                    buf.append("M3-");
                if ( (evt.stateMask & SWT.MOD4) != 0)
                    buf.append("M4-");
                buf.append(evt.character).append(" (").append((int)evt.character).append(") ");
                buf.append(" [").append(evt.character & ~SWT.ALT & ~SWT.CONTROL & ~SWT.COMMAND).append("]");
                System.out.println("Key press: " + buf.toString());
                 */
            }
        });
    }
    private SimpleTimer.TimedEvent _timedPreview = new SimpleTimer.TimedEvent() {
        public void timeReached() {
            if (_lastModified > 0) {
                long idle = System.currentTimeMillis() - _lastModified;
                if (idle > 1000) {
                    System.out.println("idle for " + idle + "ms, previewing");
                    Display.getDefault().asyncExec(new Runnable() { public void run() { preview(); } });
                } else {
                    //System.out.println("idle for " + idle + "ms, NOT previewing");
                    SimpleTimer.getInstance().addEvent(_timedPreview, 100);
                    _preview.setRender(false);
                }
            }
        }
    };
    
    private void createSpellchecker() {
        _spellShell = new Shell(_parent.getShell(), SWT.DIALOG_TRIM);
        _spellShell.setText("Spell checker");
        GridLayout gl = new GridLayout(2, false);
        _spellShell.setLayout(gl);
        
        _spellIgnoreAllList = new ArrayList();
        
        _spellContext = new StyledText(_spellShell, SWT.MULTI | SWT.WRAP | SWT.BORDER | SWT.READ_ONLY);
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalSpan = 2;
        _spellContext.setLayoutData(gd);
        
        Label l = new Label(_spellShell, SWT.NONE);
        l.setText("Word: ");
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = false;
        l.setLayoutData(gd);
        _spellWord = new Text(_spellShell, SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE | SWT.LEFT);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        _spellWord.setLayoutData(gd);

        l = new Label(_spellShell, SWT.NONE);
        l.setText("Suggestions: ");
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = false;
        l.setLayoutData(gd);
        _spellSuggestions = new Combo(_spellShell, SWT.SIMPLE);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        _spellSuggestions.setLayoutData(gd);
        
        Composite actionLine = new Composite(_spellShell, SWT.NONE);
        actionLine.setLayout(new FillLayout(SWT.HORIZONTAL));
        _spellReplace = new Button(actionLine, SWT.PUSH);
        _spellReplace.setText("replace");
        _spellReplaceAll = new Button(actionLine, SWT.PUSH);
        _spellReplaceAll.setText("replace all");
        _spellIgnore = new Button(actionLine, SWT.PUSH);
        _spellIgnore.setText("ignore");
        _spellIgnoreAll = new Button(actionLine, SWT.PUSH);
        _spellIgnoreAll.setText("ignore all");
        _spellAdd = new Button(actionLine, SWT.PUSH);
        _spellAdd.setText("add");
        _spellCancel = new Button(actionLine, SWT.PUSH);
        _spellCancel.setText("cancel");
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalSpan = 2;
        actionLine.setLayoutData(gd);
        
        _spellIgnore.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _spellWordIndex++; spellNext(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _spellWordIndex++; spellNext(); }
        });
        _spellIgnoreAll.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _spellWordIndex++; _spellIgnoreAllList.add(_spellWord.getText().trim().toLowerCase()); spellNext(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _spellWordIndex++; _spellIgnoreAllList.add(_spellWord.getText().trim().toLowerCase()); spellNext(); }
        });
        _spellReplace.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { spellReplaceWord(false); spellNext(); }
            public void widgetSelected(SelectionEvent selectionEvent) { spellReplaceWord(false); spellNext(); }
        });
        _spellReplaceAll.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { spellReplaceWord(true); spellNext(); }
            public void widgetSelected(SelectionEvent selectionEvent) { spellReplaceWord(true); spellNext(); }
        });

        _spellCancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { resetSpellcheck(); _spellShell.setVisible(false); }
            public void widgetSelected(SelectionEvent selectionEvent) { resetSpellcheck(); _spellShell.setVisible(false); }
        });
        
        _spellShell.pack();
    }
    private void spellReplaceWord(boolean replaceAll) {
        String old = _spellWord.getText().trim();
        String newText = _spellSuggestions.getText().trim();
        int len = _spellWordEnd-_spellWordStart+1;
        if (_spellWordStart + len >= _text.getCharCount())
            len = _text.getCharCount() - _spellWordStart;
        String oldFound = _text.getTextRange(_spellWordStart, len);
        System.out.println("replacing [" + old + "]/[" + oldFound + "] with [" + newText + "]");
        _text.replaceTextRange(_spellWordStart, len, newText);
        _spellWordIndex++;
        if (replaceAll) {
            int line = _spellLine;
            int word = _spellWordIndex;
            spellNext(newText, old);
            _spellLine = line;
            _spellWordIndex = word;
        }
    }
    private void spellNext() { spellNext(null, null); }
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
                            if (!_spellIgnoreAllList.contains(lower)) {
                                ArrayList suggestions = getSuggestions(lower);
                                if (suggestions != null) {
                                    _spellWordStart = lineStart + wordStart;
                                    if (cur >= len)
                                        _spellWordEnd = lineStart + wordEnd - 1;
                                    else
                                        _spellWordEnd = lineStart + wordEnd;
                                    _spellWord.setText(word);
                                    _spellSuggestions.removeAll();
                                    for (int i = 0; i < suggestions.size(); i++)
                                        _spellSuggestions.add((String)suggestions.get(i));
                                    _spellSuggestions.select(0);
                                    _spellContext.setText(lineText);
                                    // underline the word
                                    showSpell(true);
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
        showSpell(false);
        return;
    }
    
    /**
     * returns the list of suggested words to replace the given word, or null if
     * the word is spelled correctly
     */
    private ArrayList getSuggestions(String word) {
        if (!_spellDictionary.isCorrect(word)) {
            java.util.List suggestions = _spellDictionary.getSuggestions(word, 5); // 5?!  why?
            ArrayList rv = new ArrayList(suggestions.size());
            for (int i = 0; i < suggestions.size(); i++) {
                Word suggestedWord = (Word)suggestions.get(i);
                rv.add(suggestedWord.getWord());
            }
            return rv;
        }
        return null;
    }
    
    private void showSpell(boolean wordSet) {
        if (wordSet) {
            _spellContext.setLineBackground(0, 1, null);
            _spellWord.setEnabled(true);
            _spellSuggestions.setEnabled(true);
            _spellAdd.setEnabled(true);
            _spellCancel.setEnabled(true);
            _spellCancel.setText("cancel");
            _spellIgnore.setEnabled(true);
            _spellIgnoreAll.setEnabled(true);
            _spellReplace.setEnabled(true);
            _spellReplaceAll.setEnabled(true);
        } else {
            _spellContext.setText("End of content reached");
            _spellContext.setLineBackground(0, 1, ColorUtil.getColor("red", null));
            _spellWord.setText("");
            _spellWord.setEnabled(false);
            _spellSuggestions.removeAll();
            _spellSuggestions.setEnabled(false);
            _spellAdd.setEnabled(false);
            _spellCancel.setEnabled(true);
            _spellCancel.setText("ok");
            _spellIgnore.setEnabled(false);
            _spellIgnoreAll.setEnabled(false);
            _spellReplace.setEnabled(false);
            _spellReplaceAll.setEnabled(false);
        }
        _spellShell.setVisible(true);
    }
    
    private void spellcheck() { resetSpellcheck(); spellNext(); }
    private void resetSpellcheck() {
        _spellLine = 0;
        _spellWordIndex = 0;
        _spellIgnoreAllList.clear();
    }

    /*
     * initialize the dictionary, shared across all page editors
     */
    static {
        try {
            _spellDictionary = new SpellDictionaryHashMap(getDictionaryReader());
        } catch (IOException ioe) {
            // use an empty one
            try { _spellDictionary = new SpellDictionaryHashMap(); } catch (IOException ioe2) {}
        }
    }
    private static Reader getDictionaryReader() {
        // read from the db/etc
        try {
            return new InputStreamReader(new FileInputStream("/usr/share/dict/words"), "UTF-8");
        } catch (IOException ioe) {}
        return new InputStreamReader(new ByteArrayInputStream(new byte[0]));
    }

    private void createFind() {
        _findShell = new Shell(_parent.getShell(), SWT.DIALOG_TRIM);
        _findShell.setText("Search and replace");
        GridLayout gl = new GridLayout(2, false);
        _findShell.setLayout(gl);
    
        Label l = new Label(_findShell, SWT.NONE);
        l.setText("Find what: ");
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = false;
        l.setLayoutData(gd);
        _findText = new Text(_findShell, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        _findText.setLayoutData(gd);
        
        l = new Label(_findShell, SWT.NONE);
        l.setText("Replace with: ");
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = false;
        l.setLayoutData(gd);
        _findReplace = new Text(_findShell, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        _findReplace.setLayoutData(gd);
        
        _findMatchCase = new Button(_findShell, SWT.CHECK | SWT.LEFT);
        _findMatchCase.setText("match case");
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalSpan = 2;
        _findMatchCase.setLayoutData(gd);
        
        _findWrapAround = new Button(_findShell, SWT.CHECK | SWT.LEFT);
        _findWrapAround.setText("wrap around");
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalSpan = 2;
        _findWrapAround.setLayoutData(gd);
        
        _findBackwards = new Button(_findShell, SWT.CHECK | SWT.LEFT);
        _findBackwards.setText("backwards");
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalSpan = 2;
        _findBackwards.setLayoutData(gd);
        
        Composite actionRow = new Composite(_findShell, SWT.NONE);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalSpan = 2;
        actionRow.setLayoutData(gd);
        actionRow.setLayout(new FillLayout(SWT.HORIZONTAL));
        
        final Button findNext = new Button(actionRow, SWT.PUSH);
        findNext.setText("Find next");
        findNext.setToolTipText("Find the next occurrence of the word");
        findNext.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { findNext(); }
            public void widgetSelected(SelectionEvent selectionEvent) { findNext(); }
        });
        
        final Button close = new Button(actionRow, SWT.PUSH);
        close.setText("Close");
        close.setToolTipText("Finish searching");
        close.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _text.setStyleRanges(null, null); _findShell.setVisible(false); }
            public void widgetSelected(SelectionEvent selectionEvent) { _text.setStyleRanges(null, null); _findShell.setVisible(false); }
        });
        
        final Button replace = new Button(actionRow, SWT.PUSH);
        replace.setText("Replace");
        replace.setToolTipText("Replace the current occurrence of the word");
        replace.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { findReplace(); }
            public void widgetSelected(SelectionEvent selectionEvent) { findReplace(); }
        });
        
        final Button replaceAll = new Button(actionRow, SWT.PUSH);
        replaceAll.setText("Replace all");
        replaceAll.setToolTipText("Replace all remaining occurrences of the word");
        replaceAll.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { findReplaceAll(); }
            public void widgetSelected(SelectionEvent selectionEvent) { findReplaceAll(); }
        });

        _findHighlight = new StyleRange();
        _findHighlight.background = ColorUtil.getColor("yellow", null);
        
        _findText.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) {
                    findNext.forceFocus();
                    findNext();
                }
            }
        });
        _findReplace.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) {
                    replace.forceFocus();
                    findReplace();
                }
            }
        });
        
        _findShell.pack();
    }
    private void find() {
        _findWrapped = false;
        _findText.setText("");
        _findReplace.setText("");
        _findBackwards.setSelection(false);
        _findMatchCase.setSelection(false);
        _findWrapAround.setSelection(false);
        _findHighlight.length = 0;
        _findShell.setVisible(true);
        _findText.forceFocus();
    }
    private void findReplace() {
        if (_findHighlight.length <= 0)
            findNext();
        if (_findHighlight.length > 0) {
            String replaceWith = _findReplace.getText();
            _text.replaceTextRange(_findHighlight.start, _findHighlight.length, replaceWith);
            _text.setCaretOffset(_findHighlight.start + replaceWith.length());
            _findHighlight.length = 0;
            _text.setStyleRanges(null, null);
            findNext();
        }
    }
    private void findReplaceAll() {
        if (_findHighlight.length <= 0)
            findNext(false);
        while (_findHighlight.length > 0) {
            String replaceWith = _findReplace.getText();
            _text.replaceTextRange(_findHighlight.start, _findHighlight.length, replaceWith);
            _text.setCaretOffset(_findHighlight.start + replaceWith.length());
            _findHighlight.length = 0;
            _text.setStyleRanges(null, null);
            findNext(false);
        }
    }
    private void findNext() { findNext(true); }
    private void findNext(boolean wrapForever) {
        String searchFor = _findText.getText();
        if ( (searchFor == null) || (searchFor.length() <= 0) ) return;
        String txt = _text.getText();
        boolean caseSensitive = _findMatchCase.getSelection();
        boolean backwards = _findBackwards.getSelection();
        if (!caseSensitive) {
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
            if (_findWrapAround.getSelection() && (!_findWrapped || wrapForever)) {
                if (backwards)
                    nextStart = txt.lastIndexOf(searchFor);
                else
                    nextStart = txt.indexOf(searchFor);
                _findWrapped = true;
            }
        }
        System.out.println("findNext @ " + nextStart + " (started @ " + caret + ")");
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
    
    private void togglePreview() {
        if (_metaPreview.getSelection()) {
            _sash.setMaximizedControl(null);
            _preview.setRender(true);
            preview();
        } else {
            _sash.setMaximizedControl(_text);
            _preview.setRender(false);
        }
    }

    int getPageCount() { return _messageEditor.getPageCount(); }
    List getAttachmentDescriptions() { return _messageEditor.getAttachmentDescriptions(); }
    List getAttachmentDescriptions(boolean imagesOnly) { return _messageEditor.getAttachmentDescriptions(imagesOnly); }

    byte[] getImageAttachment(int idx) { return _messageEditor.getImageAttachment(idx); }
    int getImageAttachmentNum(int imageNum) { return _messageEditor.getImageAttachmentNum(imageNum); }
    void updateImageAttachment(int imageNum, String contentType, byte data[]) { _messageEditor.updateImageAttachment(imageNum, contentType, data); }
    int addAttachment(String contentType, String name, byte[] data) { return _messageEditor.addAttachment(contentType, name, data); }
}
