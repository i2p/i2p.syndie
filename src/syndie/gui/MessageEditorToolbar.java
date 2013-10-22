package syndie.gui;

import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.CoolBar;
import org.eclipse.swt.widgets.CoolItem;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

class MessageEditorToolbar implements MessageEditor.EditorStatusListener {
    private final Composite _parent;
    private final MessageEditor _editor;
    private final BookmarkControl _bookmarkControl;
    private final DBClient _client;
    private final TranslationRegistry _translationRegistry;
    
    private int buttonWidth = 32;
    private int buttonHeight = 32;
    
    private final CoolBar _toolbar;
    // forum control
    private Button _forumButton;
    private Image _forumAvatar;
    private Menu _forumMenu;
    // author control
    private Button _authorButton;
    private Image _authorAvatar;
    private Menu _authorMenu;
    private MenuItem _authorMenuOther;
    // privacy control
    private Button _privButton;
    private Menu _privMenu;
    private MenuItem _privPublic;
    private MenuItem _privAuthorized;
    private MenuItem _privPBE;
    private MenuItem _privReply;
    // page add control
    private Button _pageAddButton;
    // page remove control
    private Button _pageRemoveButton;
    // web rip control
    private Button _webRipButton;
    // add image control
    private Button _addImageAttachmentButton;
    // attachment control
    private Button _addAttachmentButton;
    // attachment remove control
    private Button _removeAttachmentButton;
    // page type control
    private Button _pageTypeButton;
    // link control
    private Button _linkButton;
    private Menu _linkMenu;
    private MenuItem _linkWeb;
    private MenuItem _linkPage;
    private MenuItem _linkAttach;
    private MenuItem _linkForum;
    private MenuItem _linkMsg;
    private MenuItem _linkEepsite;
    private MenuItem _linkI2P;
    private MenuItem _linkFreenet;
    private MenuItem _linkArchive;
    private MenuItem _linkOther;
    // style control
    private Button _styleButton;
    private Menu _styleMenu;
    private MenuItem _styleText;
    private MenuItem _styleImage;
    private MenuItem _styleBGColor;
    private Menu _styleBGColorMenu;
    private MenuItem _styleBGColorDefault;
    private MenuItem _styleBGImage;
    private MenuItem _styleListOrdered;
    private MenuItem _styleListUnordered;
    private MenuItem _styleHeading;
    private Menu _styleHeadingMenu;
    private MenuItem _styleHeading1;
    private MenuItem _styleHeading2;
    private MenuItem _styleHeading3;
    private MenuItem _styleHeading4;
    private MenuItem _styleHeading5;
    private MenuItem _stylePre;
    // spellcheck control
    // needs revamp (line breaking, plurals, caps)
    // May be null if spellcheck disabled
    private Button _spellButton;
    // search control
    private Button _searchButton;
    // quote control
    private Button _quoteButton;
    
    public MessageEditorToolbar(Composite parent, MessageEditor editor, DBClient client, BookmarkControl bookmark, TranslationRegistry trans) {
        _parent = parent;
        _editor = editor;
        _client = client;
        _bookmarkControl = bookmark;
        _translationRegistry = trans;
        
        _toolbar = new CoolBar(parent, SWT.NONE);
        
        ToolbarGroup forumGroup = new ToolbarGroup(_toolbar);
        initForumControl(forumGroup);
        initAuthorControl(forumGroup);
        initPrivacyControl(forumGroup);
        forumGroup.pack();
        
        ToolbarGroup pageGroup = new ToolbarGroup(_toolbar);
        initPageTypeControl(pageGroup);
        initPageAddControl(pageGroup);
        initPageRemoveControl(pageGroup);
        initWebRipControl(pageGroup);
        pageGroup.pack();
        
        ToolbarGroup attachGroup = new ToolbarGroup(_toolbar);
        initAddImageAttachmentControl(attachGroup);
        initAddAttachmentControl(attachGroup);
        initRemoveAttachmentControl(attachGroup);
        attachGroup.pack();
        
        ToolbarGroup styleGroup = new ToolbarGroup(_toolbar);
        initLinkControl(styleGroup);
        initStyleControl(styleGroup);
        styleGroup.pack();
        
        ToolbarGroup textGroup = new ToolbarGroup(_toolbar);
        initQuoteControl(textGroup);
        initSearchControl(textGroup);
        if (SpellUtil.isEnabled())
            initSpellControl(textGroup);
        textGroup.pack();
    }

    public void dispose() {
        ImageUtil.dispose(_forumAvatar);
        ImageUtil.dispose(_authorAvatar);
        _toolbar.dispose();
    }
    
    public void setLayoutData(Object layoutData) {
        _toolbar.setLayoutData(layoutData);
    }
    
    public void pickPrivacyPublic() {
        _privButton.setImage(ImageUtil.resize(ImageUtil.ICON_EDITOR_PRIVACY_PUBLIC, buttonWidth, buttonHeight, false));
        _privPublic.setSelection(true);
    }
    public void pickPrivacyPBE() {
        _privButton.setImage(ImageUtil.resize(ImageUtil.ICON_EDITOR_PRIVACY_PBE, buttonWidth, buttonHeight, false));
        _privPBE.setSelection(true);
    }
    public void pickPrivacyPrivate() {
        _privButton.setImage(ImageUtil.resize(ImageUtil.ICON_EDITOR_PRIVACY_REPLY, buttonWidth, buttonHeight, false));
        _privReply.setSelection(true);
    }
    public void pickPrivacyAuthorized() {
        _privButton.setImage(ImageUtil.resize(ImageUtil.ICON_EDITOR_PRIVACY_AUTHORIZED, buttonWidth, buttonHeight, false));
        _privAuthorized.setSelection(true);
    }
    
    /**
     *  @param page current page or -1
     *  @param attachment current attachment or -1
     */
    public void statusUpdated(int page, int pages, int attachment, int attachments, String type, boolean pageLoaded, boolean isHTML, boolean hasAncestors) {
        _addImageAttachmentButton.setEnabled(isHTML);
        _webRipButton.setEnabled(isHTML);
        _linkMenu.setEnabled(isHTML);
        _linkArchive.setEnabled(isHTML);
        _linkAttach.setEnabled(isHTML);
        _linkButton.setEnabled(isHTML);
        _linkEepsite.setEnabled(isHTML);
        _linkForum.setEnabled(isHTML);
        _linkFreenet.setEnabled(isHTML);
        _linkI2P.setEnabled(isHTML);
        _linkMsg.setEnabled(isHTML);
        _linkOther.setEnabled(isHTML);
        _linkPage.setEnabled(isHTML);
        _linkWeb.setEnabled(isHTML);
        _styleMenu.setEnabled(isHTML);
        _styleBGColor.setEnabled(isHTML);
        _styleBGColorDefault.setEnabled(isHTML);
        _styleBGColorMenu.setEnabled(isHTML);
        _styleBGImage.setEnabled(isHTML);
        _styleButton.setEnabled(isHTML);
        _styleHeading.setEnabled(isHTML);
        _styleHeading1.setEnabled(isHTML);
        _styleHeading2.setEnabled(isHTML);
        _styleHeading3.setEnabled(isHTML);
        _styleHeading4.setEnabled(isHTML);
        _styleHeading5.setEnabled(isHTML);
        _styleHeadingMenu.setEnabled(isHTML);
        _styleImage.setEnabled(isHTML);
        _styleListOrdered.setEnabled(isHTML);
        _styleListUnordered.setEnabled(isHTML);
        _stylePre.setEnabled(isHTML);
        _styleText.setEnabled(isHTML);

        if (isHTML) {
            _linkPage.setEnabled(pages > 0);
            _linkAttach.setEnabled(attachments > 0);
        }
        
        _pageTypeButton.setSelection(isHTML);
       
        _pageTypeButton.setEnabled(page >= 0);
        _pageRemoveButton.setEnabled(page >= 0);
        _removeAttachmentButton.setEnabled(attachment >= 0);
        
        if (_spellButton != null)
            _spellButton.setEnabled(pageLoaded);
        
        _searchButton.setEnabled(pageLoaded);
        _quoteButton.setEnabled(hasAncestors);
    }
    
    public void forumSelected(Hash forum, long channelId, String summary, boolean isManaged) {
        //_forumButton.setRedraw(false);
        ImageUtil.dispose(_forumAvatar);
        _forumButton.setImage(null);
        if (channelId >= 0) {
            // don't show the forum avatar unless the forum is bookmarked or we own the channel -
            // this should help fight phishing attacks (to prevent spoofing w/ same 
            // icon & link <a href=...>send me your password</a>)
            if (isManaged || _bookmarkControl.isBookmarked(SyndieURI.createScope(forum))) {
                byte avatar[] = _client.getChannelAvatar(channelId);
                if (avatar != null) {
                    _forumAvatar = ImageUtil.createImage(avatar);
                    _forumButton.setImage(ImageUtil.resize(_forumAvatar, buttonWidth, buttonHeight, false));
                } else {
                    _forumButton.setImage(ImageUtil.resize(ImageUtil.ICON_EDITOR_BOOKMARKED_NOAVATAR, buttonWidth, buttonHeight, false));
                }
            } else {
                _forumButton.setImage(ImageUtil.resize(ImageUtil.ICON_EDITOR_NOT_BOOKMARKED, buttonWidth, buttonHeight, false));
            }
            _forumButton.setToolTipText(summary);
        } else {
            _forumButton.setImage(ImageUtil.resize(ImageUtil.ICON_EDITOR_NOT_BOOKMARKED, buttonWidth, buttonHeight, false));
            _forumButton.setToolTipText("");
        }
        //_forumButton.setRedraw(true);
    }
    
    public void authorSelected(Hash author, long channelId, String summary) {
        ImageUtil.dispose(_authorAvatar);
        _authorButton.setImage(null);
        byte avatar[] = _client.getChannelAvatar(channelId);
        if (avatar != null) {
            _authorAvatar = ImageUtil.createImage(avatar);
            _authorButton.setImage(ImageUtil.resize(_authorAvatar, buttonWidth, buttonHeight, false));
        } else {
            _authorButton.setImage(ImageUtil.resize(ImageUtil.ICON_EDITOR_BOOKMARKED_NOAVATAR, buttonWidth, buttonHeight, false));
        }
        _authorButton.setToolTipText(summary);
    }
    
    private Button addButton(ToolbarGroup tg, int style, String tooltip, Image image, SelectionListener listener) {
        Button button = tg.newButton(style);
        
        button.setToolTipText(tooltip);
        button.setImage(ImageUtil.resize(image, buttonWidth, buttonHeight, false));
        button.addSelectionListener(listener);
        
        return button;
    }
    
    private Button addFlatButton(ToolbarGroup tg, String tooltip, Image image, SelectionListener listener) {
        return addButton(tg, SWT.FLAT, tooltip, image, listener);
    }
    
    private Button addToggleButton(ToolbarGroup tg, String tooltip, Image image, SelectionListener listener) {
        return addButton(tg, SWT.TOGGLE, tooltip, image, listener);
    }
    
    private String getText(String s) {
        return _translationRegistry.getText(s);
    }

    public void initForumControl(ToolbarGroup tg) {
        _forumMenu = new Menu(_parent);
        SelectionListener listener = new FireSelectionListener() {
            public void fire() { _editor.pickForum(); }
        };
        _forumButton = addFlatButton(tg, getText("Select the forum to post in"), ImageUtil.ICON_EDITOR_NOT_BOOKMARKED, listener);
    }
    
    public void initAuthorControl(ToolbarGroup tg) {
        _authorMenu = new Menu(_parent);
        SelectionListener listener = new FireSelectionListener() {
            public void fire() { _editor.pickAuthor(); }
        };
        _authorButton = addFlatButton(tg, getText("Who do you want to sign the post as?"), ImageUtil.ICON_EDITOR_BOOKMARKED_NOAVATAR, listener);
    }
    
    public void initPrivacyControl(ToolbarGroup tg) {        
        _privMenu = new Menu(_parent);
        SelectionListener listener = new FireSelectionListener() {
            public void fire() { _privMenu.setVisible(true); }
        };
        _privButton = addFlatButton(tg,getText("Who is allowed to read the post?"), ImageUtil.ICON_EDITOR_PRIVACY_AUTHORIZED, listener);
        
        _privPublic = new MenuItem(_privMenu, SWT.PUSH);
        _privAuthorized = new MenuItem(_privMenu, SWT.PUSH);
        _privPBE = new MenuItem(_privMenu, SWT.PUSH);
        _privReply = new MenuItem(_privMenu, SWT.PUSH);
        
        _privPublic.setImage(ImageUtil.ICON_EDITOR_PRIVACY_PUBLIC);
        _privAuthorized.setImage(ImageUtil.ICON_EDITOR_PRIVACY_AUTHORIZED);
        _privPBE.setImage(ImageUtil.ICON_EDITOR_PRIVACY_PBE);
        _privReply.setImage(ImageUtil.ICON_EDITOR_PRIVACY_REPLY);
        
        _privPublic.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.pickPrivacy(0); }
        });
        _privAuthorized.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.pickPrivacy(1);}
        });
        _privPBE.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.pickPrivacy(2); }
        });
        _privReply.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.pickPrivacy(3);}
        });
        
        _privAuthorized.setSelection(true);
        
        _privPublic.setText(getText("Anyone can read the post"));
        _privAuthorized.setText(getText("Authorized readers of the forum can read the post"));
        _privPBE.setText(getText("Passphrase required to read the post") + "...");
        _privReply.setText(getText("Only forum administrators can read the post"));
        _privAuthorized.setSelection(true);
    }
    
    public void initPageTypeControl(ToolbarGroup tg) {
        SelectionListener listener = new FireSelectionListener() {
            public void fire() { _editor.togglePageType(); }
        };
        
        _pageTypeButton = addToggleButton(tg, getText("Toggle the type of the current page"), ImageUtil.ICON_EDITOR_TOGGLETYPE, listener);
    }
    
    public void initPageAddControl(ToolbarGroup tg) {
        SelectionListener listener = new FireSelectionListener() {
            public void fire() { _editor.addPage(); }
        };
        
        _pageAddButton = addFlatButton(tg, getText("Add new page"), ImageUtil.ICON_EDITOR_ADDPAGE, listener);
    }
    
    public void initPageRemoveControl(ToolbarGroup tg) {        
        SelectionListener listener = new FireSelectionListener() {
            public void fire() { _editor.removePage(); }
        };
        
        _pageRemoveButton = addFlatButton(tg, getText("Remove this page"), ImageUtil.ICON_EDITOR_REMOVEPAGE, listener);
    }
    
    public void initWebRipControl(ToolbarGroup tg) {
        SelectionListener listener = new FireSelectionListener() {
            public void fire() { _editor.addWebRip(); }
        };
        
        _webRipButton = addFlatButton(tg, getText("Rip a web page and add it as a new page in the message"), ImageUtil.ICON_EDITOR_WEBRIP, listener);
    }
    
    public void initAddImageAttachmentControl(ToolbarGroup tg) {
        SelectionListener listener = new FireSelectionListener() {
            public void fire() { _editor.showImagePopup(false); }
        };
        
        _addImageAttachmentButton = addFlatButton(tg, getText("Insert a new image into the current page"), ImageUtil.ICON_EDITOR_ADDIMAGE, listener);
    }
    
    public void initAddAttachmentControl(ToolbarGroup tg) {
        SelectionListener listener = new FireSelectionListener() {
            public void fire() { _editor.addAttachment(); }
        };
        _addAttachmentButton = addFlatButton(tg, getText("Add attachment"), ImageUtil.ICON_EDITOR_ADDFILE, listener);
    }
    
    public void initRemoveAttachmentControl(ToolbarGroup tg) {
        SelectionListener listener = new FireSelectionListener() {
            public void fire() { _editor.removeAttachment(); }
        };
        _removeAttachmentButton = addFlatButton(tg, getText("Remove this attachment"), ImageUtil.ICON_EDITOR_REMOVEFILE, listener);
    }
    
    public void initLinkControl(ToolbarGroup tg) {
        _linkMenu = new Menu(_parent);
        SelectionListener listener = new FireSelectionListener() {
            public void fire() { _linkMenu.setVisible(true); }
        };
        
        _linkButton = addFlatButton(tg, getText("Add a new link"), ImageUtil.ICON_EDITOR_LINK, listener);
        
        _linkWeb = new MenuItem(_linkMenu, SWT.PUSH);
        new MenuItem(_linkMenu, SWT.SEPARATOR);
        _linkPage = new MenuItem(_linkMenu, SWT.PUSH);
        _linkAttach = new MenuItem(_linkMenu, SWT.PUSH);
        _linkForum = new MenuItem(_linkMenu, SWT.PUSH);
        _linkMsg = new MenuItem(_linkMenu, SWT.PUSH);
        _linkArchive = new MenuItem(_linkMenu, SWT.PUSH);
        new MenuItem(_linkMenu, SWT.SEPARATOR);
        _linkEepsite = new MenuItem(_linkMenu, SWT.PUSH);
        _linkI2P = new MenuItem(_linkMenu, SWT.PUSH);
        new MenuItem(_linkMenu, SWT.SEPARATOR);
        _linkFreenet = new MenuItem(_linkMenu, SWT.PUSH);
        new MenuItem(_linkMenu, SWT.SEPARATOR);
        _linkOther = new MenuItem(_linkMenu, SWT.PUSH);
        
        _linkPage.setEnabled(false);
        _linkAttach.setEnabled(false);
        
        _linkWeb.setImage(ImageUtil.ICON_REF_URL);
        _linkWeb.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.showLinkPopup(true, false, false, false, false, false, false, false, false, false); }
        });
        _linkPage.setImage(ImageUtil.ICON_REF_SYNDIE);
        _linkPage.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.showLinkPopup(false, true, false, false, false, false, false, false, false, false); }
        });
        _linkAttach.setImage(ImageUtil.ICON_REF_SYNDIE);
        _linkAttach.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.showLinkPopup(false, false, true, false, false, false, false, false, false, false); }
        });
        _linkForum.setImage(ImageUtil.ICON_REF_FORUM);
        _linkForum.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.showLinkPopup(false, false, false, true, false, false, false, false, false, false); }
        });
        _linkMsg.setImage(ImageUtil.ICON_REF_MSG);
        _linkMsg.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.showLinkPopup(false, false, false, false, true, false, false, false, false, false); }
        });
        _linkEepsite.setImage(ImageUtil.ICON_REF_SYNDIE);
        _linkEepsite.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.showLinkPopup(false, false, false, false, false, false, true, false, false, false); }
        });
        _linkI2P.setImage(ImageUtil.ICON_REF_SYNDIE);
        _linkI2P.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.showLinkPopup(false, false, false, false, false, false, false, true, false, false); }
        });
        _linkFreenet.setImage(ImageUtil.ICON_REF_FREENET);
        _linkFreenet.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.showLinkPopup(false, false, false, false, false, false, false, false, true, false); }
        });
        _linkArchive.setImage(ImageUtil.ICON_REF_ARCHIVE);
        _linkArchive.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.showLinkPopup(false, false, false, false, false, false, false, false, false, true); }
        });
        _linkOther.setImage(ImageUtil.ICON_REF_SYNDIE);
        _linkOther.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.showLinkPopup(false, false, false, true, false, false, false, false, false, false); }
        });
        
        _linkWeb.setText(getText("Link to a website"));
        _linkPage.setText(getText("Link to a page in this message"));
        _linkAttach.setText(getText("Link to an attachment in this message"));
        _linkForum.setText(getText("Link to a forum"));
        _linkMsg.setText(getText("Link to a particular Syndie message"));
        _linkEepsite.setText(getText("Link to an I2P eepsite"));
        _linkI2P.setText(getText("Link to an I2P destination"));
        _linkFreenet.setText(getText("Link to a Freenet freesite"));
        _linkArchive.setText(getText("Link to a Syndie archive"));
        _linkOther.setText(getText("Link to another Syndie URI"));
    }
    
    public void initStyleControl(ToolbarGroup tg) {
        _styleMenu = new Menu(_parent);
        SelectionListener listener = new FireSelectionListener() {
            public void fire() { _styleMenu.setVisible(true); }
        };
        
        _styleButton = addFlatButton(tg, getText("Insert style elements"), ImageUtil.ICON_EDITOR_STYLE, listener);
        
        _styleText = new MenuItem(_styleMenu, SWT.PUSH);
        _styleText.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.styleText(); }
        });
        
        _styleImage = new MenuItem(_styleMenu, SWT.PUSH);
        _styleImage.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.showImagePopup(false); }
        });
        new MenuItem(_styleMenu, SWT.SEPARATOR);
        
        _styleBGColor = new MenuItem(_styleMenu, SWT.CASCADE);
        _styleBGColorMenu = new Menu(_styleBGColor);
        _styleBGColor.setMenu(_styleBGColorMenu);
        _styleBGColorDefault = new MenuItem(_styleBGColorMenu, SWT.PUSH);
        _styleBGColorDefault .addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.setPageBGColor(null); }
        });
        ColorUtil.init();
        List names = ColorUtil.getSystemColorNames();
        //_ui.debugMessage("color names: " + names);
        for (int i = 0; i < names.size(); i++) {
            final String name = (String)names.get(i);
            Color color = ColorUtil.getColor(name);
            MenuItem item = new MenuItem(_styleBGColorMenu, SWT.PUSH);
            item.setImage(ColorUtil.getSystemColorSwatch(color));
            item.setText(name);
            item.addSelectionListener(new FireSelectionListener() {
                public void fire() { _editor.setPageBGColor(name); }
            });
        }
        _styleBGImage = new MenuItem(_styleMenu, SWT.PUSH);
        _styleBGImage.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.showImagePopup(true); }
        });
        new MenuItem(_styleMenu, SWT.SEPARATOR);
        
        _styleListOrdered = new MenuItem(_styleMenu, SWT.PUSH);
        _styleListOrdered.addSelectionListener(new InsertListener("<ol>\n\t<li>first list item</li>\n</ol>\n", true));
        _styleListUnordered = new MenuItem(_styleMenu, SWT.PUSH);
        _styleListUnordered.addSelectionListener(new InsertListener("<ul>\n\t<li>first list item</li>\n</ul>\n", true));
        
        new MenuItem(_styleMenu, SWT.SEPARATOR);
        
        _styleHeading = new MenuItem(_styleMenu, SWT.CASCADE);
        _styleHeadingMenu = new Menu(_styleHeading);
        _styleHeading.setMenu(_styleHeadingMenu);
        _styleHeading1 = new MenuItem(_styleHeadingMenu, SWT.PUSH);
        _styleHeading1.addSelectionListener(new InsertListener("<h1>TEXT</h1>", true));
        _styleHeading2 = new MenuItem(_styleHeadingMenu, SWT.PUSH);
        _styleHeading2.addSelectionListener(new InsertListener("<h2>TEXT</h2>", true));
        _styleHeading3 = new MenuItem(_styleHeadingMenu, SWT.PUSH);
        _styleHeading3.addSelectionListener(new InsertListener("<h3>TEXT</h3>", true));
        _styleHeading4 = new MenuItem(_styleHeadingMenu, SWT.PUSH);
        _styleHeading4.addSelectionListener(new InsertListener("<h4>TEXT</h4>", true));
        _styleHeading5 = new MenuItem(_styleHeadingMenu, SWT.PUSH);
        _styleHeading5.addSelectionListener(new InsertListener("<h5>TEXT</h5>", true));
        _stylePre = new MenuItem(_styleMenu, SWT.PUSH);
        _stylePre.addSelectionListener(new InsertListener("<pre>first line\n\tindented line</pre>", true));
        
        _styleText.setText(getText("Styled text") + "...");
        _styleImage.setText(getText("Image") + "...");
        _styleBGColor.setText(getText("Page background color"));
        _styleBGColorDefault.setText(getText("standard"));
        _styleBGImage.setText(getText("Page background image") + "...");
        _styleListOrdered.setText(getText("List (ordered)"));
        _styleListUnordered.setText(getText("List (unordered)"));
        _styleHeading.setText(getText("Heading"));
        _styleHeading1.setText(getText("Heading 1 (largest)"));
        _styleHeading2.setText(getText("Heading 2"));
        _styleHeading3.setText(getText("Heading 3"));
        _styleHeading4.setText(getText("Heading 4"));
        _styleHeading5.setText(getText("Heading 5 (smallest)"));
        _stylePre.setText(getText("Preformatted text"));
    }

    /** simple hook to inert a buffer at the caret */
    private class InsertListener extends FireSelectionListener {
        private boolean _onNewline;
        private String _toInsert;
        public InsertListener(String toInsert, boolean onNewline) {
            _onNewline = onNewline;
            _toInsert = toInsert;
        }
        public void fire() {
            PageEditor editor = _editor.getPageEditor();
            if (editor != null)
                editor.insert(_toInsert, _onNewline);
        }
    }
    
    public void initSpellControl(ToolbarGroup tg) {
        SelectionListener listener = new FireSelectionListener() {
            public void fire() { _editor.spellNext(); }
        };
        
        _spellButton = addFlatButton(tg, getText("Check the spelling in the current page"), ImageUtil.ICON_EDITOR_SPELL, listener);
    }
    
    public void initSearchControl(ToolbarGroup tg) {
        SelectionListener listener = new FireSelectionListener() {
            public void fire() { _editor.search(); }
        };
        
        _searchButton = addFlatButton(tg, getText("Find or replace text in the current page"), ImageUtil.ICON_EDITOR_SEARCH, listener);
    }
    
    public void initQuoteControl(ToolbarGroup tg) {
        SelectionListener listener = new FireSelectionListener() {
            public void fire() { _editor.quote(); }
        };
        
        _quoteButton = addFlatButton(tg, getText("Quote a section of the previous message"), ImageUtil.ICON_EDITOR_QUOTE, listener);
    }
    
    public void applyTheme(Theme theme) {
        _forumButton.setFont(theme.DEFAULT_FONT);
        _authorButton.setFont(theme.DEFAULT_FONT);
        _privButton.setFont(theme.DEFAULT_FONT);
        _pageAddButton.setFont(theme.DEFAULT_FONT);
        _pageRemoveButton.setFont(theme.DEFAULT_FONT);
        _webRipButton.setFont(theme.DEFAULT_FONT);
        _pageTypeButton.setFont(theme.DEFAULT_FONT);
        _addAttachmentButton.setFont(theme.DEFAULT_FONT);
        _addImageAttachmentButton.setFont(theme.DEFAULT_FONT);
        _removeAttachmentButton.setFont(theme.DEFAULT_FONT);
        _linkButton.setFont(theme.DEFAULT_FONT);
        _styleButton.setFont(theme.DEFAULT_FONT);
        if (_spellButton != null)
            _spellButton.setFont(theme.DEFAULT_FONT);
        _searchButton.setFont(theme.DEFAULT_FONT);
        _quoteButton.setFont(theme.DEFAULT_FONT);
    }
    
    public void pickPageTypeHTML(boolean isHTML) {
        // required by MessageEditor.EditorStatusListener interface
        return;
    }
}
