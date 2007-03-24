package syndie.gui;

import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

public class MessageEditorToolbar implements MessageEditor.EditorStatusListener {
    private MessageEditor _editor;
    private BookmarkControl _bookmarkControl;
    private DBClient _client;
    private TranslationRegistry _translationRegistry;
    
    // forum control
    private Group _forumGroup;
    private Button _forumButton;
    private Image _forumAvatar;
    private Menu _forumMenu;
    // author control
    private Group _authorGroup;
    private Button _authorButton;
    private Image _authorAvatar;
    private Menu _authorMenu;
    private MenuItem _authorMenuOther;
    // privacy control
    private Group _privGroup;
    private Button _privButton;
    private Menu _privMenu;
    private MenuItem _privPublic;
    private MenuItem _privAuthorized;
    private MenuItem _privPBE;
    private MenuItem _privReply;
    // page control
    private Group _pageGroup;
    private Button _pageButton;
    private Menu _pageMenu;
    private MenuItem _pageAdd;
    private MenuItem _pageAddWebRip;
    private MenuItem _pageRemove;
    // page type control
    private Group _pageTypeGroup;
    private Button _pageType;
    // attachment control
    private Group _attachGroup;
    private Button _attachButton;
    private Menu _attachMenu;
    private MenuItem _attachAdd;
    private MenuItem _attachAddImage;
    // link control
    private Group _linkGroup;
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
    private Group _styleGroup;
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
    private Group _spellGroup;
    private Button _spellButton;
    // search control
    private Group _searchGroup;
    private Button _searchButton;
    // quote control
    private Group _quoteGroup;
    private Button _quoteButton;
    
    public MessageEditorToolbar(MessageEditor editor, DBClient client, BookmarkControl bookmark, TranslationRegistry trans) {
        _editor = editor;
        _client = client;
        _bookmarkControl = bookmark;
        _translationRegistry = trans;
    }

    public void dispose() {
        ImageUtil.dispose(_forumAvatar);
        ImageUtil.dispose(_authorAvatar);
    }
    
    public void pickPrivacyPublic() {
        _privButton.setImage(ImageUtil.ICON_EDITOR_PRIVACY_PUBLIC);
        _privPublic.setSelection(true);
    }
    public void pickPrivacyPBE() {
        _privButton.setImage(ImageUtil.ICON_EDITOR_PRIVACY_PBE); 
        _privPBE.setSelection(true);
    }
    public void pickPrivacyPrivate() {
        _privButton.setImage(ImageUtil.ICON_EDITOR_PRIVACY_REPLY);
        _privReply.setSelection(true);
    }
    public void pickPrivacyAuthorized() {
        _privButton.setImage(ImageUtil.ICON_EDITOR_PRIVACY_AUTHORIZED);
        _privAuthorized.setSelection(true);
    }
    
    public void pickPageTypeHTML(boolean isHTML) {
        if (isHTML)
            _pageType.setImage(ImageUtil.ICON_EDITOR_PAGETYPE_HTML);
        else
            _pageType.setImage(ImageUtil.ICON_EDITOR_PAGETYPE_TEXT);
    }
    
    public void statusUpdated(int page, int pages, int attachment, int attachments, String type, boolean pageLoaded, boolean isHTML, boolean hasAncestors) {
        _attachAddImage.setEnabled(isHTML);
        _linkMenu.setEnabled(isHTML);
        _linkArchive.setEnabled(isHTML);
        _linkAttach.setEnabled(isHTML);
        _linkButton.setEnabled(isHTML);
        _linkEepsite.setEnabled(isHTML);
        _linkForum.setEnabled(isHTML);
        _linkFreenet.setEnabled(isHTML);
        _linkGroup.setEnabled(isHTML);
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
        _styleGroup.setEnabled(isHTML);
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
        
        if (page >= 0) {
            if (isHTML)
                _pageType.setImage(ImageUtil.ICON_EDITOR_PAGETYPE_HTML);
            else
                _pageType.setImage(ImageUtil.ICON_EDITOR_PAGETYPE_TEXT);
            _pageType.setEnabled(true);
            _pageTypeGroup.setEnabled(true);
        } else {
            _pageType.setEnabled(false);
            _pageTypeGroup.setEnabled(false);
        }
        
        _spellButton.setEnabled(pageLoaded && false); // disabled for the moment,
        _spellGroup.setEnabled(pageLoaded && false); // pending revamp (line breaking, plurals, caps)
        
        _searchButton.setEnabled(pageLoaded);
        _searchGroup.setEnabled(pageLoaded);
        _quoteButton.setEnabled(hasAncestors);
        _quoteGroup.setEnabled(hasAncestors);
    }
    
    public void forumSelected(Hash forum, long channelId, String summary, boolean isManaged) {
        _forumButton.setRedraw(false);
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
                    _forumButton.setImage(_forumAvatar);
                } else {
                    _forumButton.setImage(ImageUtil.ICON_EDITOR_BOOKMARKED_NOAVATAR);
                }
            } else {
                _forumButton.setImage(ImageUtil.ICON_EDITOR_NOT_BOOKMARKED);
            }
            _forumButton.setToolTipText(summary);
        } else {
            _forumButton.setImage(ImageUtil.ICON_EDITOR_NOT_BOOKMARKED);
            _forumButton.setToolTipText("");
        }
        _forumButton.setRedraw(true);
    }
    
    public void authorSelected(Hash author, long channelId, String summary) {
        _authorButton.setRedraw(false);
        ImageUtil.dispose(_authorAvatar);
        _authorButton.setImage(null);
        byte avatar[] = _client.getChannelAvatar(channelId);
        if (avatar != null) {
            _authorAvatar = ImageUtil.createImage(avatar);
            _authorButton.setImage(_authorAvatar);
        } else {
            _authorButton.setImage(ImageUtil.ICON_EDITOR_BOOKMARKED_NOAVATAR);
        }
        _authorButton.setToolTipText(summary);
        _authorButton.setRedraw(true);
    }
    
    public void attachmentsRebuilt(List attachmentData, List attachmentSummary) {
        MenuItem items[] = _attachMenu.getItems();
        for (int i = 0; i < items.length; i++)
            if ( (items[i] != _attachAdd) && (items[i] != _attachAddImage) )
                items[i].dispose();
        for (int i = 0; i < attachmentData.size(); i++) {
            MenuItem attachItem = new MenuItem(_attachMenu, SWT.CASCADE);
            attachItem.setText((String)attachmentSummary.get(i));
            Menu sub = new Menu(attachItem);
            attachItem.setMenu(sub);
            MenuItem view = new MenuItem(sub, SWT.PUSH);
            view.setEnabled(false);
            view.setText(_translationRegistry.getText(T_ATTACHMENT_VIEW, "View"));
            MenuItem delete = new MenuItem(sub, SWT.PUSH);
            delete.setText(_translationRegistry.getText(T_ATTACHMENT_DELETE, "Delete"));
            final int attachNum = i;
            delete.addSelectionListener(new FireSelectionListener() {
                public void fire() { _editor.removeAttachment(attachNum); }
            });
        }
    }
    
    private static final String T_ATTACHMENT_VIEW = "syndie.gui.messageeditortoolbar.attachview";
    private static final String T_ATTACHMENT_DELETE = "syndie.gui.messageeditortoolbar.attachdelete";
    
    public void clearForumMenu() {
        MenuItem items[] = _forumMenu.getItems();
        for (int i = 0; i < items.length; i++)
            items[i].dispose();
    }
    
    public void addForumMenuItem(final Hash channel, final long channelId, boolean managed, final String summary) {
        MenuItem item = new MenuItem(_forumMenu, SWT.PUSH);
        item.setText(summary);
        //item.setData("channel.hash", channel);
        //item.setData("channel.managed", managed ? Boolean.TRUE : Boolean.FALSE);
        item.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.pickForum(channel, channelId, summary, true); }
        });
    }
    
    public void addForumMenuItem() {
        new MenuItem(_forumMenu, SWT.SEPARATOR);
    }
    
    public void addForumMenuItemOther() {
        MenuItem item = new MenuItem(_forumMenu, SWT.PUSH);
        item.setText("other...");
        item.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.pickOtherForum(); }
        });
    }

    public void initForumControl(Composite toolbar) {
        _forumGroup = new Group(toolbar, SWT.SHADOW_ETCHED_IN);
        //ctl.setLayoutData(new RowData(48, 48));
        _forumGroup.setLayoutData(new RowData(50, 50));
        _forumGroup.setLayout(new FillLayout());
        
        _forumButton = new Button(_forumGroup, SWT.PUSH);
        
        _forumMenu = new Menu(_forumButton);
        _forumGroup.setMenu(_forumMenu);
        _forumButton.addSelectionListener(new FireSelectionListener() {
            public void fire() { _forumMenu.setVisible(true); }
        });
        
        _forumGroup.setText("Post to:");
        _forumGroup.setToolTipText("Select the forum to post in");
    }
    
    public void clearAuthorMenu() {
        MenuItem items[] = _authorMenu.getItems();
        for (int i = 0; i < items.length; i++)
            items[i].dispose();
    }

    public void addAuthorMenuItem(final Hash channel, final long channelId, final String summary) {
        MenuItem item = new MenuItem(_authorMenu, SWT.PUSH);
        item.setText(summary);
        //item.setData("channel.hash", channel);
        item.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.pickAuthor(channel, channelId, summary); }
        });
    }
    
    public void addAuthorMenuItem() { new MenuItem(_authorMenu, SWT.SEPARATOR); }

    public void initAuthorControl(Composite toolbar) {
        _authorGroup = new Group(toolbar, SWT.SHADOW_ETCHED_IN);
        _authorGroup.setLayoutData(new RowData(50, 50));
        _authorGroup.setLayout(new FillLayout());
        
        _authorButton = new Button(_authorGroup, SWT.PUSH);
        _authorButton.setSize(48, 48);
        //_authorButton.setImage(ImageUtil.resize(ImageUtil.ICON_QUESTION, 48, 48, false));
        
        _authorMenu = new Menu(_authorButton);
        _authorGroup.setMenu(_authorMenu);
        _authorButton.addSelectionListener(new FireSelectionListener() {
            public void fire() { _authorMenu.setVisible(true); }
        });
        
        _authorGroup.setText("Author:");
        _authorGroup.setToolTipText("Who do you want to sign the post as?");
    }
    
    public void initPrivacyControl(Composite toolbar) {
        _privGroup = new Group(toolbar, SWT.SHADOW_ETCHED_IN);
        _privGroup.setLayout(new FillLayout());
        
        _privButton = new Button(_privGroup, SWT.PUSH);
        
        _privMenu = new Menu(_privButton);
        _privGroup.setMenu(_privMenu);
        _privButton.addSelectionListener(new FireSelectionListener() {
            public void fire() { _privMenu.setVisible(true); }
        });
        
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
        
        _privPublic.setText("Anyone can read the post");
        _privAuthorized.setText("Authorized readers of the forum can read the post");
        _privPBE.setText("Passphrase required to read the post...");
        _privReply.setText("Only forum administrators can read the post");
        _privAuthorized.setSelection(true);
        
        _privGroup.setText("Privacy:");
        _privGroup.setToolTipText("Who is allowed to read the post?");
    }
    
    public void initPageControl(Composite toolbar) {   
        _pageGroup = new Group(toolbar, SWT.SHADOW_ETCHED_IN);
        _pageGroup.setLayout(new FillLayout());
        
        _pageButton = new Button(_pageGroup, SWT.PUSH);
        _pageButton.setImage(ImageUtil.ICON_EDITOR_PAGEADD);
        
        _pageMenu = new Menu(_pageButton);
        _pageGroup.setMenu(_pageMenu);
        _pageButton.addSelectionListener(new FireSelectionListener() {
            public void fire() { _pageMenu.setVisible(true); }
        });
        
        _pageAdd = new MenuItem(_pageMenu, SWT.PUSH);
        _pageAddWebRip = new MenuItem(_pageMenu, SWT.PUSH);
        
        _pageAdd.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.addPage(); }
        });
        _pageAddWebRip.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.addWebRip(); }
        });
        
        _pageRemove = new MenuItem(_pageMenu, SWT.PUSH);
        _pageRemove.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.removePage(); }
        });
        
        _pageGroup.setText("Page:");
        _pageGroup.setToolTipText("Manage pages in this post");
        _pageAdd.setText("Add a new page");
        _pageAddWebRip.setText("Add a new web rip");
        _pageRemove.setText("Remove the current page");
        
        _pageTypeGroup = new Group(toolbar, SWT.SHADOW_ETCHED_IN);
        _pageTypeGroup.setLayout(new FillLayout());
        _pageType = new Button(_pageTypeGroup, SWT.PUSH);
        _pageType.setImage(ImageUtil.ICON_EDITOR_PAGETYPE_HTML);
        _pageType.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.togglePageType(); }
        });
        
        _pageTypeGroup.setText("Type:");
    }
    
    public void initAttachControl(Composite toolbar) {   
        _attachGroup = new Group(toolbar, SWT.SHADOW_ETCHED_IN);
        _attachGroup.setLayout(new FillLayout());
        
        _attachButton = new Button(_attachGroup, SWT.PUSH);
        _attachButton.setImage(ImageUtil.ICON_EDITOR_ATTACH);
        
        _attachMenu = new Menu(_attachButton);
        _attachGroup.setMenu(_attachMenu);
        _attachButton.addSelectionListener(new FireSelectionListener() {
            public void fire() { _attachMenu.setVisible(true); }
        });
        
        _attachAddImage = new MenuItem(_attachMenu, SWT.PUSH);
        _attachAddImage.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.showImagePopup(false); }
        });
        _attachAdd = new MenuItem(_attachMenu, SWT.PUSH);
        _attachAdd.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.addAttachment(); }
        });
        
        _attachGroup.setText("Attach:");
        _attachGroup.setToolTipText("Manage attachments to this post");
        _attachAddImage.setText("Insert a new image");
        _attachAdd.setText("Add a new attachment");
    }
    
    public void initLinkControl(Composite toolbar) {
        _linkGroup = new Group(toolbar, SWT.SHADOW_ETCHED_IN);
        _linkGroup.setLayout(new FillLayout());
        
        _linkButton = new Button(_linkGroup, SWT.PUSH);
        _linkButton.setImage(ImageUtil.ICON_EDITOR_LINK);
        
        _linkMenu = new Menu(_linkButton);
        _linkGroup.setMenu(_linkMenu);
        _linkButton.addSelectionListener(new FireSelectionListener() {
            public void fire() { _linkMenu.setVisible(true); }
        });
        
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
        
        _linkWeb.setText("Link to a website");
        _linkPage.setText("Link to a page in this message");
        _linkAttach.setText("Link to an attachment in this message");
        _linkForum.setText("Link to a forum");
        _linkMsg.setText("Link to a particular Syndie message");
        _linkEepsite.setText("Link to an I2P eepsite");
        _linkI2P.setText("Link to an I2P destination");
        _linkFreenet.setText("Link to a Freenet freesite");
        _linkArchive.setText("Link to a Syndie archive");
        _linkOther.setText("Link to another Syndie URI");
        
        _linkGroup.setText("Link:");
        _linkGroup.setToolTipText("Add a new link");
    }
    
    public void initStyleControl(Composite toolbar) {
        _styleGroup = new Group(toolbar, SWT.SHADOW_ETCHED_IN);
        _styleGroup.setLayout(new FillLayout());
        
        _styleButton = new Button(_styleGroup, SWT.PUSH);
        _styleButton.setImage(ImageUtil.ICON_EDITOR_STYLE);
        
        _styleMenu = new Menu(_styleButton);
        _styleGroup.setMenu(_styleMenu);
        _styleButton.addSelectionListener(new FireSelectionListener() {
            public void fire() { _styleMenu.setVisible(true); }
        });
        
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
        
        _styleGroup.setText("Style:");
        _styleButton.setToolTipText("Insert style elements");
        
        _styleText.setText("Styled text...");
        _styleImage.setText("Image...");
        _styleBGColor.setText("Page background color");
        _styleBGColorDefault.setText("standard");
        _styleBGImage.setText("Page background image...");
        _styleListOrdered.setText("List (ordered)");
        _styleListUnordered.setText("List (unordered)");
        _styleHeading.setText("Heading");
        _styleHeading1.setText("Heading 1 (largest)");
        _styleHeading2.setText("Heading 2");
        _styleHeading3.setText("Heading 3");
        _styleHeading4.setText("Heading 4");
        _styleHeading5.setText("Heading 5 (smallest)");
        _stylePre.setText("Preformatted text");
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
    
    public void initSpellControl(Composite toolbar) {
        _spellGroup = new Group(toolbar, SWT.SHADOW_ETCHED_IN);
        _spellGroup.setLayout(new FillLayout());
        
        _spellButton = new Button(_spellGroup, SWT.PUSH);
        _spellButton.setImage(ImageUtil.ICON_EDITOR_SPELL);
        _spellButton.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.spellNext(); }
        });
        
        _spellGroup.setText("Spell:");
        _spellButton.setToolTipText("Check the spelling in the current page");
    }
    
    public void initSearchControl(Composite toolbar) {
        _searchGroup = new Group(toolbar, SWT.SHADOW_ETCHED_IN);
        _searchGroup.setLayout(new FillLayout());
        
        _searchButton = new Button(_searchGroup, SWT.PUSH);
        _searchButton.setImage(ImageUtil.ICON_EDITOR_SEARCH);
        _searchButton.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.search(); }
        });
        
        _searchGroup.setText("Find:");
        _searchButton.setToolTipText("Find or replace text in the current page");
    }
    
    public void initQuoteControl(Composite toolbar) {
        _quoteGroup = new Group(toolbar, SWT.SHADOW_ETCHED_IN);
        _quoteGroup.setLayout(new FillLayout());
        
        _quoteButton = new Button(_quoteGroup, SWT.PUSH);
        _quoteButton.setImage(ImageUtil.ICON_EDITOR_SEARCH);
        _quoteButton.addSelectionListener(new FireSelectionListener() {
            public void fire() { _editor.quote(); }
        });
        
        _quoteGroup.setText("Quote:");
        _quoteButton.setToolTipText("Quote a section of the previous message");
    }
    
    public void applyTheme(Theme theme) {
        _forumGroup.setFont(theme.DEFAULT_FONT);
        _authorGroup.setFont(theme.DEFAULT_FONT);
        _privGroup.setFont(theme.DEFAULT_FONT);
        _pageGroup.setFont(theme.DEFAULT_FONT);
        _pageTypeGroup.setFont(theme.DEFAULT_FONT);
        _attachGroup.setFont(theme.DEFAULT_FONT);
        _linkGroup.setFont(theme.DEFAULT_FONT);
        _styleGroup.setFont(theme.DEFAULT_FONT);
        _spellGroup.setFont(theme.DEFAULT_FONT);
        _searchGroup.setFont(theme.DEFAULT_FONT);
        _quoteGroup.setFont(theme.DEFAULT_FONT);
    }
    
}
