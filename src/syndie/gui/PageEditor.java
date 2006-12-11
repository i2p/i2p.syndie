package syndie.gui;

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
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
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
import syndie.data.HTMLTag;
import syndie.data.MessageInfo;
import syndie.data.SyndieURI;

import syndie.db.DBClient;
import syndie.db.UI;

/**
 * wysiwyg editor for text or html pages in a message
 */
public class PageEditor implements Translatable, Themeable {
    private BrowserControl _browser;
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
    private Button _metaSave;
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
    private Button _styleOk;
    private Button _styleCancel;
    private Font _sampleFont;
    
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

    private Group _grpHTML;
    private Group _grpMeta;
    private Group _grpPage;
    private Group _grpList;
    private Group _grpText;
    private Group _grpAlign;

    private Label _spellWordLabel;
    private Label _spellSuggestionsLabel;
    private Label _findTextLabel;
    private Label _findReplaceLabel;
    private Button _findNext;
    private Button _close;
    private Button _replace;
    private Button _replaceAll;
    
    public PageEditor(DBClient client, Composite parent, MessageEditor msg, String type, BrowserControl browser) {
        _client = client;
        _parent = parent;
        _messageEditor = msg;
        _contentType = type;
        _browser = browser;
        buildControls();
        browser.getTranslationRegistry().register(this);
        browser.getThemeRegistry().register(this);
    }
    
    public Control getControl() { return _root; }
    
    private Color getControlFGColor() { return ifDiff(_txtFGColor.getBackground(), _txtFGColor.getParent().getBackground()); }
    private Color getControlBGColor() { return ifDiff(_txtBGColor.getBackground(), _txtBGColor.getParent().getBackground()); }
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

    private UI getUI() { return _messageEditor.getBrowser().getUI(); }
    
    private void buildControls() {
        getUI().debugMessage("pageEditor.buildControls started");
        _root = new Composite(_parent, SWT.BORDER);
        boolean html = "text/html".equals(_contentType);
        if (html)
            createHTMLToolbar();
        else
            createTextToolbar();
        getUI().debugMessage("pageEditor.buildControls toolbars created");
        _sash = new SashForm(_root, SWT.VERTICAL);
        _text = new StyledText(_sash, SWT.MULTI | SWT.WRAP | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        _text.setEditable(true);
        _text.setDoubleClickEnabled(true);
        getUI().debugMessage("pageEditor.buildControls styledText built");
        addEditListeners();
        
        getUI().debugMessage("pageEditor.buildControls building page renderer");
        _preview = new PageRenderer(_sash, true, _browser);
        getUI().debugMessage("pageEditor.buildControls page renderer built");
        if (html) {
            _sash.setMaximizedControl(null);
            _sash.setWeights(new int[] { 80, 20 });
        } else {
            _sash.setMaximizedControl(_text);
        }
        
        getUI().debugMessage("pageEditor.buildControls creating style chooser");
        createStyleChooser();
        getUI().debugMessage("pageEditor.buildControls creating spell checker");
        createSpellchecker();
        getUI().debugMessage("pageEditor.buildControls creating find");
        createFind();
        
        getUI().debugMessage("pageEditor.buildControls creating link popup");
        _linkPopup = new LinkBuilderPopup(_messageEditor.getBrowser(), _parent.getShell(), this);
        getUI().debugMessage("pageEditor.buildControls creating image popup");
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
        
        _grpHTML = new Group(_toolbars, SWT.SHADOW_ETCHED_IN);
        _htmlStyleChooser = new Button(_grpHTML, SWT.PUSH);
        _htmlStyleChooser.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showStyleChooser(); }
            public void widgetSelected(SelectionEvent selectionEvent) { showStyleChooser(); }
        });
        _htmlLink = new Button(_grpHTML, SWT.PUSH);
        _htmlLink.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showLinkPopup(); }
            public void widgetSelected(SelectionEvent selectionEvent) { showLinkPopup(); }
        });
        _htmlImg = new Button(_grpHTML, SWT.PUSH);
        _htmlImg.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showImagePopup(false); }
            public void widgetSelected(SelectionEvent selectionEvent) { showImagePopup(false); }
        });
        _htmlSymbol = new Button(_grpHTML, SWT.PUSH);
        _htmlHeader = new Button(_grpHTML, SWT.PUSH);
        prepareHeader();
        _htmlPre = new Button(_grpHTML, SWT.PUSH);
        _htmlPre.addSelectionListener(new InsertListener("<pre>first line\n\tindented line</pre>", true));
        _htmlQuote = new Button(_grpHTML, SWT.PUSH);
        
        _grpPage = new Group(_toolbars, SWT.SHADOW_ETCHED_IN);
        _pageBGImage = new Button(_grpPage, SWT.PUSH);
        _pageBGImage.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { showImagePopup(true); }
            public void widgetSelected(SelectionEvent selectionEvent) { showImagePopup(true); }
        });
        _pageBGColor = buildColorCombo(_grpPage, "pagebg", "Adjust the page bg color", "white", enable, new Runnable() {
            public void run() { setBodyTags(); }
        });
        
        _grpList = new Group(_toolbars, SWT.SHADOW_ETCHED_IN);
        _listOrdered = new Button(_grpList, SWT.PUSH);
        _listOrdered.addSelectionListener(new InsertListener("<ol>\n\t<li>first list item</li>\n</ol>\n", true));
        _listUnordered = new Button(_grpList, SWT.PUSH);
        _listUnordered.addSelectionListener(new InsertListener("<ul>\n\t<li>first list item</li>\n</ul>\n", true));

        _grpMeta = new Group(_toolbars, SWT.HORIZONTAL);
        _metaCopy = new Button(_grpMeta, SWT.PUSH);
        _metaCopy.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _text.copy(); _text.forceFocus(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _text.copy(); _text.forceFocus(); }
        });
        _metaPaste = new Button(_grpMeta, SWT.PUSH);
        _metaPaste.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _text.paste(); _text.forceFocus(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _text.paste(); _text.forceFocus(); }
        });
        _metaCut = new Button(_grpMeta, SWT.PUSH);
        _metaCut.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _text.cut(); _text.forceFocus(); }
            public void widgetSelected(SelectionEvent selectionEvent) { _text.cut(); _text.forceFocus(); }
        });
        _metaSpell = new Button(_grpMeta, SWT.PUSH);
        _metaSpell.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { spellcheck(); }
            public void widgetSelected(SelectionEvent selectionEvent) { spellcheck(); }
        });
        _metaFind = new Button(_grpMeta, SWT.PUSH);
        _metaFind.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { find(); }
            public void widgetSelected(SelectionEvent selectionEvent) { find(); }
        });
        _metaSave = new Button(_grpMeta, SWT.PUSH);
        _metaSave.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { save(); }
            public void widgetSelected(SelectionEvent selectionEvent) { save(); }
        });
        _metaPreview = new Button(_grpMeta, SWT.CHECK);
        _metaPreview.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { togglePreview(); }
            public void widgetSelected(SelectionEvent selectionEvent) { togglePreview(); }
        });
        _metaPreview.setSelection(true);
        
        enableActions(enable, _grpMeta, _grpList, _grpPage, _grpHTML);
        
        RowLayout rl = new RowLayout(SWT.HORIZONTAL);
        _grpMeta.setLayout(rl);
        rl = new RowLayout(SWT.HORIZONTAL);
        _grpList.setLayout(rl);
        rl = new RowLayout(SWT.HORIZONTAL);
        _grpPage.setLayout(rl);
        rl = new RowLayout(SWT.HORIZONTAL);
        _grpHTML.setLayout(rl);
        
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
        _metaSave.setEnabled(enable);
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
    
    private void prepareHeader() {
        final Menu headerMenu = new Menu(_htmlHeader);
        newHeader(headerMenu, "H1");
        newHeader(headerMenu, "H2");
        newHeader(headerMenu, "H3");
        newHeader(headerMenu, "H4");
        newHeader(headerMenu, "H5");
        _htmlHeader.setMenu(headerMenu);
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
        _txtShell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; _txtShell.setVisible(false); }
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
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { insertTextStyle(); _txtShell.setVisible(false); }
            public void widgetSelected(SelectionEvent selectionEvent) { insertTextStyle(); _txtShell.setVisible(false); }
        });
        _styleCancel = new Button(actionRow, SWT.PUSH);
        _styleCancel.addSelectionListener(new SelectionListener() {
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
        
        lsnr.redrawSample();
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
            int fontHeight = Theme.getSize(_browser.getThemeRegistry().getTheme().CONTENT_FONT);
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
        
        int begin = buf.length();
        String sel = _text.getSelectionText();
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
        if ( (sel == null) || (sel.trim().length() == 0) ) {
            insertAtCaret(str);
            _text.setCaretOffset(_text.getCaretOffset() - (str.length()-begin));
            _text.setSelectionRange(_text.getCaretOffset(), (end-begin));
        } else {
            _messageEditor.modified();
            Point range = _text.getSelectionRange();
            _text.replaceTextRange(range.x, range.y, str);
            _text.setCaretOffset(range.x+begin);
            _text.setSelectionRange(_text.getCaretOffset(), (end-begin));
        }
    }
    
    void insertAtCaret(String text) {
        if (text != null) {
            _messageEditor.modified();
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
    
    private void showStyleChooser() { resetTextStyle(); _txtShell.open(); }
    private void showLinkPopup() { _linkPopup.showPopup(); }
    private void showImagePopup(boolean forBodyBackground) { _imagePopup.showPopup(forBodyBackground); }
    
    private Button buildColorCombo(Group parent, String name, String tooltip, String defaultColor, boolean enable) { return buildColorCombo(parent, name, tooltip, defaultColor, enable, null); }
    private Button buildColorCombo(Group parent, String name, String tooltip, String defaultColor, boolean enable, Runnable onSelect) {
        final Button rv = new Button(parent, SWT.PUSH);
        ArrayList names = ColorUtil.getSystemColorNames();
        rv.setText(name);
        final Menu colorMenu = new Menu(rv);
        MenuItem none = new MenuItem(colorMenu, SWT.PUSH);
        none.setText(_browser.getTranslationRegistry().getText(T_COLOR_DEFAULT, "default"));
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
            _messageEditor.modified();
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
        PageRendererSourceMem src = new PageRendererSourceMem(_browser, null, msgInfo, pageData, attachments, attachmentOrder);
        _preview.setRender(true);
        long before = System.currentTimeMillis();
        _preview.renderPage(src, _dummyURI);
        long renderTime = System.currentTimeMillis()-before;
        _browser.getUI().debugMessage("** render time: " + renderTime);
        _preview.setRender(false);
        _lastPreviewed = System.currentTimeMillis();
        _lastModified = -1;
    }
    
    private void addEditListeners() {
        _text.addExtendedModifyListener(new ExtendedModifyListener() {
            public void modifyText(ExtendedModifyEvent evt) {
                _lastModified = System.currentTimeMillis();
                _messageEditor.modified();
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
                            _messageEditor.modified();
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
                    case 0x13: // ^S
                        if ( (evt.stateMask & SWT.MOD1) != 0) {
                            save();
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
                            _messageEditor.modified();
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
                    _browser.getUI().debugMessage("idle for " + idle + "ms, previewing");
                    Display.getDefault().asyncExec(new Runnable() { public void run() { preview(); } });
                } else {
                    //System.out.println("idle for " + idle + "ms, NOT previewing");
                    SimpleTimer.getInstance().addEvent(_timedPreview, 100);
                    _preview.setRender(false);
                }
            }
        }
    };
    
    private void save() { _messageEditor.saveState(); }
    
    private void createSpellchecker() {
        _spellShell = new Shell(_parent.getShell(), SWT.DIALOG_TRIM);
        GridLayout gl = new GridLayout(2, false);
        _spellShell.setLayout(gl);
        
        _spellIgnoreAllList = new ArrayList();
        
        _spellContext = new StyledText(_spellShell, SWT.MULTI | SWT.WRAP | SWT.BORDER | SWT.READ_ONLY);
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalSpan = 2;
        _spellContext.setLayoutData(gd);
        
        _spellWordLabel = new Label(_spellShell, SWT.NONE);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = false;
        _spellWordLabel.setLayoutData(gd);
        _spellWord = new Text(_spellShell, SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE | SWT.LEFT);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        _spellWord.setLayoutData(gd);

        _spellSuggestionsLabel = new Label(_spellShell, SWT.NONE);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = false;
        _spellSuggestionsLabel.setLayoutData(gd);
        _spellSuggestions = new Combo(_spellShell, SWT.DROP_DOWN);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        _spellSuggestions.setLayoutData(gd);
        
        Composite actionLine = new Composite(_spellShell, SWT.NONE);
        actionLine.setLayout(new FillLayout(SWT.HORIZONTAL));
        _spellReplace = new Button(actionLine, SWT.PUSH);
        _spellReplaceAll = new Button(actionLine, SWT.PUSH);
        _spellIgnore = new Button(actionLine, SWT.PUSH);
        _spellIgnoreAll = new Button(actionLine, SWT.PUSH);
        _spellAdd = new Button(actionLine, SWT.PUSH);
        _spellCancel = new Button(actionLine, SWT.PUSH);
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
        _browser.getUI().debugMessage("replacing [" + old + "]/[" + oldFound + "] with [" + newText + "]");
        _text.replaceTextRange(_spellWordStart, len, newText);
        _messageEditor.modified();
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
        if (!SpellUtil.getDictionary().isCorrect(word)) {
            java.util.List suggestions = SpellUtil.getDictionary().getSuggestions(word, 5); // 5?!  why?
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
            _spellCancel.setText(_browser.getTranslationRegistry().getText(T_SPELL_CANCEL, "cancel"));
            _spellIgnore.setEnabled(true);
            _spellIgnoreAll.setEnabled(true);
            _spellReplace.setEnabled(true);
            _spellReplaceAll.setEnabled(true);
        } else {
            _spellContext.setText(_browser.getTranslationRegistry().getText(T_SPELL_END, "End of content reached"));
            _spellContext.setLineBackground(0, 1, ColorUtil.getColor("red", null));
            _spellWord.setText("");
            _spellWord.setEnabled(false);
            _spellSuggestions.removeAll();
            _spellSuggestions.setEnabled(false);
            _spellAdd.setEnabled(false);
            _spellCancel.setEnabled(true);
            _spellCancel.setText(_browser.getTranslationRegistry().getText(T_SPELL_END_OK, "ok"));
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

    private void createFind() {
        _findShell = new Shell(_parent.getShell(), SWT.DIALOG_TRIM);
        GridLayout gl = new GridLayout(2, false);
        _findShell.setLayout(gl);
    
        _findTextLabel = new Label(_findShell, SWT.NONE);
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = false;
        _findTextLabel.setLayoutData(gd);
        _findText = new Text(_findShell, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        _findText.setLayoutData(gd);
        
        _findReplaceLabel = new Label(_findShell, SWT.NONE);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = false;
        _findReplaceLabel.setLayoutData(gd);
        _findReplace = new Text(_findShell, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        _findReplace.setLayoutData(gd);
        
        _findMatchCase = new Button(_findShell, SWT.CHECK | SWT.LEFT);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalSpan = 2;
        _findMatchCase.setLayoutData(gd);
        
        _findWrapAround = new Button(_findShell, SWT.CHECK | SWT.LEFT);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalSpan = 2;
        _findWrapAround.setLayoutData(gd);
        
        _findBackwards = new Button(_findShell, SWT.CHECK | SWT.LEFT);
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
        
        _findNext = new Button(actionRow, SWT.PUSH);
        _findNext.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { findNext(); }
            public void widgetSelected(SelectionEvent selectionEvent) { findNext(); }
        });
        
        _close = new Button(actionRow, SWT.PUSH);
        _close.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _text.setStyleRanges(null, null); _findShell.setVisible(false); }
            public void widgetSelected(SelectionEvent selectionEvent) { _text.setStyleRanges(null, null); _findShell.setVisible(false); }
        });
        
        _replace = new Button(actionRow, SWT.PUSH);
        _replace.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { findReplace(); }
            public void widgetSelected(SelectionEvent selectionEvent) { findReplace(); }
        });
        
        _replaceAll = new Button(actionRow, SWT.PUSH);
        _replaceAll.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { findReplaceAll(); }
            public void widgetSelected(SelectionEvent selectionEvent) { findReplaceAll(); }
        });

        _findHighlight = new StyleRange();
        _findHighlight.background = ColorUtil.getColor("yellow", null);
        
        _findText.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) {
                    _findNext.forceFocus();
                    findNext();
                }
            }
        });
        _findReplace.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN) {
                    _replace.forceFocus();
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
            _messageEditor.modified();
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
        _messageEditor.modified();
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
    
    void dispose() {
        // destroy all the data.  the page has been dropped or cancelled
        if (!_root.isDisposed())
            _root.dispose();
        if ( (_sampleFont != null) && (!_sampleFont.isDisposed()) )
            _sampleFont.dispose();
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
    }
    
    String getContent() { return _text.getText(); }
    void setContent(String body) { _text.setText(body); }
    String getContentType() { return _contentType; }

    private static final String T_STYLE_BUTTON = "syndie.gui.pageeditor.stylebutton";
    private static final String T_STYLE_BUTTON_TOOLTIP = "syndie.gui.pageeditor.stylebuttontooltip";
    private static final String T_LINK = "syndie.gui.pageeditor.link";
    private static final String T_LINK_TOOLTIP = "syndie.gui.pageeditor.linktooltip";
    private static final String T_IMG = "syndie.gui.pageeditor.img";
    private static final String T_IMG_TOOLTIP = "syndie.gui.pageeditor.imgtooltip";
    private static final String T_SYMBOL = "syndie.gui.pageeditor.symbol";
    private static final String T_SYMBOL_TOOLTIP = "syndie.gui.pageeditor.symboltooltip";
    private static final String T_PRE = "syndie.gui.pageeditor.pre";
    private static final String T_PRE_TOOLTIP = "syndie.gui.pageeditor.pretooltip";
    private static final String T_QUOTE = "syndie.gui.pageeditor.quote";

    private static final String T_HTML = "syndie.gui.pageeditor.html";
    private static final String T_STYLE = "syndie.gui.pageeditor.style";
    private static final String T_PAGE = "syndie.gui.pageeditor.page";
    private static final String T_BGIMAGE = "syndie.gui.pageeditor.bgimage";
    private static final String T_BGIMAGE_TOOLTIP = "syndie.gui.pageeditor.bgimagetooltip";
    private static final String T_LIST = "syndie.gui.pageeditor.list";
    private static final String T_ORDERED = "syndie.gui.pageeditor.ordered";
    private static final String T_ORDERED_TOOLTIP = "syndie.gui.pageeditor.orderedtooltip";
    private static final String T_UNORDERED = "syndie.gui.pageeditor.unordered";
    private static final String T_UNORDERED_TOOLTIP = "syndie.gui.pageeditor.unorderedtooltip";
    private static final String T_META = "syndie.gui.pageeditor.meta";
    private static final String T_COPY = "syndie.gui.pageeditor.copy";
    private static final String T_COPY_TOOLTIP = "syndie.gui.pageeditor.copytooltip";
    private static final String T_PASTE = "syndie.gui.pageeditor.paste";
    private static final String T_PASTE_TOOLTIP = "syndie.gui.pageeditor.pastetooltip";
    private static final String T_CUT = "syndie.gui.pageeditor.cut";
    private static final String T_CUT_TOOLTIP = "syndie.gui.pageeditor.cuttooltip";
    private static final String T_SPELL = "syndie.gui.pageeditor.spell";
    private static final String T_SPELL_TOOLTIP = "syndie.gui.pageeditor.spelltooltip";
    private static final String T_FIND = "syndie.gui.pageeditor.find";
    private static final String T_FIND_TOOLTIP = "syndie.gui.pageeditor.findtooltip";
    private static final String T_SAVE = "syndie.gui.pageeditor.save";
    private static final String T_SAVE_TOOLTIP = "syndie.gui.pageeditor.savetooltip";
    private static final String T_PREVIEW = "syndie.gui.pageeditor.preview";
    private static final String T_PREVIEW_TOOLTIP = "syndie.gui.pageeditor.previewtooltip";
    private static final String T_HEADER = "syndie.gui.pageeditor.header";
    private static final String T_HEADER_TOOLTIP = "syndie.gui.pageeditor.headertooltip";
    private static final String T_TEXT = "syndie.gui.pageeditor.text";
    private static final String T_TEXT_B = "syndie.gui.pageeditor.text.bold";
    private static final String T_TEXT_I = "syndie.gui.pageeditor.text.italic";
    private static final String T_TEXT_U = "syndie.gui.pageeditor.text.underline";
    private static final String T_TEXT_SO = "syndie.gui.pageeditor.text.strikeout";
    private static final String T_TEXT_FONT = "syndie.gui.pageeditor.text.font";
    private static final String T_TEXT_FONTSIZE = "syndie.gui.pageeditor.text.fontsize";
    private static final String T_TEXT_ALIGN = "syndie.gui.pageeditor.text.align";
    private static final String T_TEXT_ALIGN_LEFT = "syndie.gui.pageeditor.text.align.left";
    private static final String T_TEXT_ALIGN_LEFT_TOOLTIP = "syndie.gui.pageeditor.text.align.lefttooltip";
    private static final String T_TEXT_ALIGN_CENTER = "syndie.gui.pageeditor.text.align.center";
    private static final String T_TEXT_ALIGN_CENTER_TOOLTIP = "syndie.gui.pageeditor.text.align.centertooltip";
    private static final String T_TEXT_ALIGN_RIGHT = "syndie.gui.pageeditor.text.align.right";
    private static final String T_TEXT_ALIGN_RIGHT_TOOLTIP = "syndie.gui.pageeditor.text.align.righttooltip";
    private static final String T_TEXT_SAMPLE = "syndie.gui.pageeditor.text.sample";
    private static final String T_TEXT_OK = "syndie.gui.pageeditor.text.ok";
    private static final String T_TEXT_CANCEL = "syndie.gui.pageeditor.text.cancel";
    
    private static final String T_SPELL_ROOT = "syndie.gui.pageeditor.spell.root";
    private static final String T_SPELL_WORD = "syndie.gui.pageeditor.spell.word";
    private static final String T_SPELL_SUGGESTION = "syndie.gui.pageeditor.spell.suggestion";
    private static final String T_SPELL_REPLACE = "syndie.gui.pageeditor.spell.replace";
    private static final String T_SPELL_REPLACE_ALL = "syndie.gui.pageeditor.spell.replaceall";
    private static final String T_SPELL_IGNORE = "syndie.gui.pageeditor.spell.ignore";
    private static final String T_SPELL_IGNORE_ALL = "syndie.gui.pageeditor.spell.ignoreall";
    private static final String T_SPELL_ADD = "syndie.gui.pageeditor.spell.add";
    private static final String T_SPELL_CANCEL = "syndie.gui.pageeditor.spell.cancel";
        
    private static final String T_FIND_ROOT = "syndie.gui.pageeditor.find.root";
    private static final String T_FIND_TEXT = "syndie.gui.pageeditor.find.text";
    private static final String T_FIND_REPLACE = "syndie.gui.pageeditor.find.replace";
    private static final String T_FIND_MATCH = "syndie.gui.pageeditor.find.match";
    private static final String T_FIND_WRAP = "syndie.gui.pageeditor.find.wrap";
    private static final String T_FIND_BACKWARDS = "syndie.gui.pageeditor.find.backwards";
    private static final String T_FIND_NEXT = "syndie.gui.pageeditor.find.next";
    private static final String T_FIND_NEXT_TOOLTIP = "syndie.gui.pageeditor.find.nexttooltip";
    private static final String T_FIND_CLOSE = "syndie.gui.pageeditor.find.close";
    private static final String T_FIND_CLOSE_TOOLTIP = "syndie.gui.pageeditor.find.closetooltip";
    private static final String T_FIND_REPLACE_ACTION = "syndie.gui.pageeditor.find.replace.action";
    private static final String T_FIND_REPLACE_ACTION_TOOLTIP = "syndie.gui.pageeditor.find.replace.actiontooltip";
    private static final String T_FIND_REPLACE_ALL_ACTION = "syndie.gui.pageeditor.find.replaceall.action";
    private static final String T_FIND_REPLACE_ALL_ACTION_TOOLTIP = "syndie.gui.pageeditor.find.replaceall.actiontooltip";
    
    private static final String T_FGCOLOR = "syndie.gui.pageeditor.text.fgcolor";
    private static final String T_FGCOLOR_TOOLTIP = "syndie.gui.pageeditor.text.fgcolortooltip";
    private static final String T_BGCOLOR = "syndie.gui.pageeditor.text.bgcolor";
    private static final String T_BGCOLOR_TOOLTIP = "syndie.gui.pageeditor.text.bgcolortooltip";
    private static final String T_PAGEBGCOLOR = "syndie.gui.pageeditor.pagebgcolor";
    private static final String T_PAGEBGCOLOR_TOOLTIP = "syndie.gui.pageeditor.pagebgcolortooltip";
    
    private static final String T_SPELL_END = "syndie.gui.pageeditor.spell.end";
    private static final String T_SPELL_END_OK = "syndie.gui.pageeditor.spell.end.ok";
    private static final String T_COLOR_DEFAULT = "syndie.gui.pageeditor.color.default";
    
    public void translate(TranslationRegistry registry) {
        _htmlStyleChooser.setText(registry.getText(T_STYLE_BUTTON, "text style"));
        _htmlStyleChooser.setToolTipText(registry.getText(T_STYLE_BUTTON_TOOLTIP, "Build the text styling"));
        _htmlLink.setText(registry.getText(T_LINK, "link"));
        _htmlLink.setToolTipText(registry.getText(T_LINK_TOOLTIP, "Add a new hyperlink"));
        _htmlImg.setText(registry.getText(T_IMG, "img"));
        _htmlImg.setToolTipText(registry.getText(T_IMG_TOOLTIP, "Add a new image"));
        _htmlSymbol.setText(registry.getText(T_SYMBOL, "sym"));
        _htmlSymbol.setToolTipText(registry.getText(T_SYMBOL_TOOLTIP, "Add a new symbol"));
        _htmlPre.setText(registry.getText(T_PRE, "pre"));
        _htmlPre.setToolTipText(registry.getText(T_PRE_TOOLTIP, "Add preformatted text"));
        _htmlQuote.setText(registry.getText(T_QUOTE, "Q"));

        _grpHTML.setText(registry.getText(T_HTML, "HTML"));
        _txtShell.setText(registry.getText(T_STYLE, "Text style chooser"));
        _grpPage.setText(registry.getText(T_PAGE, "Page"));
        _pageBGImage.setText(registry.getText(T_BGIMAGE, "bgimg"));
        _pageBGImage.setToolTipText(registry.getText(T_BGIMAGE_TOOLTIP, "Set the background image"));
        _grpList.setText(registry.getText(T_LIST, "List"));
        _listOrdered.setText(registry.getText(T_ORDERED, "1)"));
        _listOrdered.setToolTipText(registry.getText(T_ORDERED_TOOLTIP, "Add a new ordered list"));
        _listUnordered.setText(registry.getText(T_UNORDERED, "*)"));
        _listUnordered.setToolTipText(registry.getText(T_UNORDERED_TOOLTIP, "Add a new unordered list"));
        _grpMeta.setText(registry.getText(T_META, "Meta"));
        _metaCopy.setText(registry.getText(T_COPY, "^C"));
        _metaCopy.setToolTipText(registry.getText(T_COPY_TOOLTIP, "copy"));
        _metaPaste.setText(registry.getText(T_PASTE, "^V"));
        _metaPaste.setToolTipText(registry.getText(T_PASTE_TOOLTIP, "paste"));
        _metaCut.setText(registry.getText(T_CUT, "^X"));
        _metaCut.setToolTipText(registry.getText(T_CUT_TOOLTIP, "cut"));
        _metaSpell.setText(registry.getText(T_SPELL, "sp"));
        _metaSpell.setToolTipText(registry.getText(T_SPELL_TOOLTIP, "spellcheck"));
        _metaFind.setText(registry.getText(T_FIND, "^F"));
        _metaFind.setToolTipText(registry.getText(T_FIND_TOOLTIP, "find"));
        _metaSave.setText(registry.getText(T_SAVE, "^S"));
        _metaSave.setToolTipText(registry.getText(T_SAVE_TOOLTIP, "save the current state"));
        _metaPreview.setText(registry.getText(T_PREVIEW, "preview"));
        _metaPreview.setToolTipText(registry.getText(T_PREVIEW_TOOLTIP, "Show the preview pane"));
        _htmlHeader.setText(registry.getText(T_HEADER, "H*"));
        _htmlHeader.setToolTipText(registry.getText(T_HEADER_TOOLTIP, "Use a header"));
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
        
        _spellShell.setText(registry.getText(T_SPELL_ROOT, "Spell checker"));
        _spellWordLabel.setText(registry.getText(T_SPELL_WORD, "Word: "));
        _spellSuggestionsLabel.setText(registry.getText(T_SPELL_SUGGESTION, "Suggestions: "));
        _spellReplace.setText(registry.getText(T_SPELL_REPLACE, "replace"));
        _spellReplaceAll.setText(registry.getText(T_SPELL_REPLACE_ALL, "replace all"));
        _spellIgnore.setText(registry.getText(T_SPELL_IGNORE, "ignore"));
        _spellIgnoreAll.setText(registry.getText(T_SPELL_IGNORE_ALL, "ignore all"));
        _spellAdd.setText(registry.getText(T_SPELL_ADD, "add"));
        _spellCancel.setText(registry.getText(T_SPELL_CANCEL, "cancel"));
        
        _findShell.setText(registry.getText(T_FIND_ROOT, "Search and replace"));
        _findTextLabel.setText(registry.getText(T_FIND_TEXT, "Find what: "));
        _findReplaceLabel.setText(registry.getText(T_FIND_REPLACE, "Replace with: "));
        _findMatchCase.setText(registry.getText(T_FIND_MATCH, "match case"));
        _findWrapAround.setText(registry.getText(T_FIND_WRAP, "wrap around"));
        _findBackwards.setText(registry.getText(T_FIND_BACKWARDS, "backwards"));
        _findNext.setText(registry.getText(T_FIND_NEXT, "Find next"));
        _findNext.setToolTipText(registry.getText(T_FIND_NEXT_TOOLTIP, "Find the next occurrence of the word"));
        _close.setText(registry.getText(T_FIND_CLOSE, "Close"));
        _close.setToolTipText(registry.getText(T_FIND_CLOSE_TOOLTIP, "Finish searching"));
        _replace.setText(registry.getText(T_FIND_REPLACE_ACTION, "Replace"));
        _replace.setToolTipText(registry.getText(T_FIND_REPLACE_ACTION_TOOLTIP, "Replace the current occurrence of the word"));
        _replaceAll.setText(registry.getText(T_FIND_REPLACE_ALL_ACTION, "Replace all"));
        _replaceAll.setToolTipText(registry.getText(T_FIND_REPLACE_ALL_ACTION_TOOLTIP, "Replace all remaining occurrences of the word"));

        _txtFGColor.setText(registry.getText(T_FGCOLOR, "color"));
        _txtFGColor.setToolTipText(registry.getText(T_FGCOLOR_TOOLTIP, "Adjust the fg color"));
        _txtBGColor.setText(registry.getText(T_BGCOLOR, "bgcolor"));
        _txtBGColor.setToolTipText(registry.getText(T_BGCOLOR_TOOLTIP, "Adjust the bg color"));
        _pageBGColor.setText(registry.getText(T_PAGEBGCOLOR, "pagebg"));
        _pageBGColor.setToolTipText(registry.getText(T_PAGEBGCOLOR_TOOLTIP, "Adjust the page bg color"));
        
        // rebuild color combos to translate the color names
    }
    
    public void applyTheme(Theme theme) {
        _root.setRedraw(false);
        // styledText doesnt like this... seems it needs stylerange instead
        //_text.setFont(theme.CONTENT_FONT);

        if (_txtShell != null) {
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
        }
        
        if (_spellShell != null) {
            _spellShell.setFont(theme.SHELL_FONT);
            //see above
            //_spellContext.setFont(theme.CONTENT_FONT);
            _spellWord.setFont(theme.CONTENT_FONT);
            _spellSuggestions.setFont(theme.DEFAULT_FONT);
            _spellReplace.setFont(theme.BUTTON_FONT);
            _spellReplaceAll.setFont(theme.BUTTON_FONT);
            _spellIgnore.setFont(theme.BUTTON_FONT);
            _spellIgnoreAll.setFont(theme.BUTTON_FONT);
            _spellAdd.setFont(theme.BUTTON_FONT);
            _spellCancel.setFont(theme.BUTTON_FONT);
    
            _spellWordLabel.setFont(theme.DEFAULT_FONT);
            _spellSuggestionsLabel.setFont(theme.DEFAULT_FONT);
        }
        
        if (_findShell != null) {
            _findShell.setFont(theme.SHELL_FONT);
            _findText.setFont(theme.CONTENT_FONT);
            _findReplace.setFont(theme.CONTENT_FONT);
            _findMatchCase.setFont(theme.BUTTON_FONT);
            _findWrapAround.setFont(theme.BUTTON_FONT);
            _findBackwards.setFont(theme.BUTTON_FONT);
            
            _findTextLabel.setFont(theme.DEFAULT_FONT);
            _findReplaceLabel.setFont(theme.DEFAULT_FONT);
            _findNext.setFont(theme.BUTTON_FONT);
            _close.setFont(theme.BUTTON_FONT);
            _replace.setFont(theme.BUTTON_FONT);
            _replaceAll.setFont(theme.BUTTON_FONT);
        }
        
        _grpHTML.setFont(theme.DEFAULT_FONT);
        _grpMeta.setFont(theme.DEFAULT_FONT);
        _grpPage.setFont(theme.DEFAULT_FONT);
        _grpList.setFont(theme.DEFAULT_FONT);
        _grpText.setFont(theme.DEFAULT_FONT);
        _grpAlign.setFont(theme.DEFAULT_FONT);

        _txtShell.pack(true);
        _root.setRedraw(true);
    }
}
