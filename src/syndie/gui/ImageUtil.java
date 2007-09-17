package syndie.gui;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.Map;
import java.util.HashMap;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Drawable;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Transform;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import syndie.data.SyndieURI;
import syndie.data.Timer;
import syndie.db.NullUI;

/**
 *
 */
public class ImageUtil {
    private static final Set _indisposableImages = Collections.synchronizedSet(new HashSet());
    /** resource name to Image */
    private static final Map _loadedResources = new HashMap();
    private static File _tmpDir;
    
    private static Timer _timer;
    
    public static boolean dispose(Image img) {
        if ( (img == null) || (img.isDisposed()) || (_indisposableImages.contains(img)) )
            return false;
        img.dispose();
        return true;
    }

    /*
    public static Image ICON_ERROR;
    public static Image ICON_INFORMATION;
    public static Image ICON_QUESTION;
    public static Image ICON_WARNING;
    public static Image ICON_WORKING;
     */
    
    public static Image ICON_SHELL;
    
    /** shown after a link when rendering */
    public static Image ICON_LINK_END;
    /** shown in place of a nonexistant image when rendering */
    public static Image ICON_IMAGE_UNKNOWN;
    
    public static Image ICON_FM_EXIT;
    public static Image ICON_FM_EXPORT;
    public static Image ICON_FM_IMPORT;
    
    public static Image ICON_VM_BOOKMARK;
    public static Image ICON_VM_LANGUAGE;
    public static Image ICON_VM_STYLE;
    
    public static Image ICON_HM_ABOUT;
    public static Image ICON_HM_BUG;
    
    public static Image ICON_PM_NEWPOST;
    
    public static Image ICON_T_EDIT;
    public static Image ICON_T_MSG;
    public static Image ICON_T_LOGS;
    public static Image ICON_T_TEXT;
    public static Image ICON_T_BUG;
    
    public static Image ICON_VIEW;
    public static Image ICON_SETTINGS;
    public static Image ICON_EDIT;
    public static Image ICON_DELETE;
    public static Image ICON_CANCEL;
    public static Image ICON_CLEAR;
    public static Image ICON_CONTROLSYNDICATION;
    
    public static Image ICON_FOLDER;
    public static Image ICON_ADDFOLDER;
    public static Image ICON_ADDBOOKMARK;
    
    public static Image ICON_VIEWFORUM;
    public static Image ICON_CREATEFORUM;
    public static Image ICON_WRITEABLEFORUM;
    public static Image ICON_MANAGEABLEFORUM;
    public static Image ICON_WATCHEDFORUM;
    public static Image ICON_IMPORTEDRESOURCES;
    
    public static Image ICON_STARTARCHIVESERVER;
    public static Image ICON_STOPARCHIVESERVER;
    public static Image ICON_CONFIGUREARCHIVESERVER;
    public static Image ICON_ADDARCHIVE;
    public static Image ICON_DELETEARCHIVE;
    public static Image ICON_MANAGEARCHIVE;
    public static Image ICON_CANCELSYNDICATIONS;
    
    public static Image ICON_MSG;
    public static Image ICON_SYNC;
    public static Image ICON_VIEWMESSAGE;
    public static Image ICON_REPLYMESSAGE;
    
    public static Image ICON_FORUMMESSAGES;
    public static Image ICON_FORUMPROFILE;
    public static Image ICON_EXPAND;
    public static Image ICON_COLLAPSE;
    public static Image ICON_UNREADMESSAGE;
    
    public static Image ICON_SYNDICATE_TYPE_DIRECT;
    public static Image ICON_SYNDICATE_TYPE_INDIRECT;
    public static Image ICON_SYNDICATE_TYPE_FREENET;
    public static Image ICON_SYNDICATE_STATUS_INPROGRESS;
    public static Image ICON_SYNDICATE_STATUS_SCHEDULED;
    public static Image ICON_SYNDICATE_STATUS_NOKEY;
    public static Image ICON_SYNDICATE_STATUS_ERROR;
    public static Image ICON_SYNDICATE_STATUS_PBE;
    public static Image ICON_SYNDICATE_STATUS_OK;
    public static Image ICON_SYNDICATE_PUSH;
    
    public static Image ICON_ARCHIVE_TYPE_FILE;
    public static Image ICON_ARCHIVE_TYPE_FREENET;
    public static Image ICON_ARCHIVE_TYPE_URL;
    public static Image ICON_ARCHIVE_TYPE_SYNDIE;
    
    public static Image ICON_MSG_TYPE_NORMAL;
    public static Image ICON_MSG_TYPE_META;
    public static Image ICON_MSG_TYPE_PRIVATE;

    // used for the MessageFlagBar to describe a message
    public static Image ICON_MSG_FLAG_READKEYUNKNOWN;
    public static Image ICON_MSG_FLAG_REPLYKEYUNKNOWN;
    public static Image ICON_MSG_FLAG_PBE;
    public static Image ICON_MSG_FLAG_UNREADABLE;
    public static Image ICON_MSG_FLAG_PUBLIC;
    public static Image ICON_MSG_FLAG_AUTHENTICATED;
    public static Image ICON_MSG_FLAG_AUTHORIZED;
    public static Image ICON_MSG_FLAG_BANNED;
    public static Image ICON_MSG_FLAG_BOOKMARKED_AUTHOR;
    public static Image ICON_MSG_FLAG_BOOKMARKED_FORUM;
    public static Image ICON_MSG_FLAG_SCHEDULEDFOREXPIRE;
    public static Image ICON_MSG_FLAG_HASKEYS;
    public static Image ICON_MSG_FLAG_HASARCHIVES;
    public static Image ICON_MSG_FLAG_HASREFS;
    public static Image ICON_MSG_FLAG_HASATTACHMENTS;
    public static Image ICON_MSG_FLAG_ISNEW;
    
    public static Image ICON_REF_MSG;
    public static Image ICON_REF_FORUM;
    public static Image ICON_REF_ARCHIVE;
    public static Image ICON_REF_URL;
    public static Image ICON_REF_SYNDIE;
    public static Image ICON_REF_FREENET;
    
    public static Image ICON_BROWSE_ADMINS;
    public static Image ICON_BROWSE_MANAGEABLE;
    public static Image ICON_BROWSE_POSTABLE;
    public static Image ICON_BROWSE_ARCHIVES;
    public static Image ICON_BROWSE_REFS;
    
    static final int TAB_ICON_SIZE = 24;
    
    public static Image ICON_TAB_EDIT;
    public static Image ICON_TAB_TEXTUI;
    public static Image ICON_TAB_LOGS;
    public static Image ICON_TAB_SYNDICATE;
    public static Image ICON_TAB_SQL;
    public static Image ICON_TAB_PAGE;
    public static Image ICON_TAB_HIGHLIGHTS;
    public static Image ICON_TAB_MSG;
    public static Image ICON_TAB_ARCHIVE;
    public static Image ICON_TAB_BROWSE;

    public static Image ICON_EDITOR_PRIVACY_PUBLIC;
    public static Image ICON_EDITOR_PRIVACY_PBE;
    public static Image ICON_EDITOR_PRIVACY_AUTHORIZED;
    public static Image ICON_EDITOR_PRIVACY_REPLY;
    public static Image ICON_EDITOR_BOOKMARKED_NOAVATAR;
    public static Image ICON_EDITOR_NOT_BOOKMARKED;
    public static Image ICON_EDITOR_PAGEADD;
    public static Image ICON_EDITOR_PAGETYPE_TEXT;
    public static Image ICON_EDITOR_PAGETYPE_HTML;
    public static Image ICON_EDITOR_ATTACH;
    public static Image ICON_EDITOR_LINK;
    public static Image ICON_EDITOR_STYLE;
    public static Image ICON_EDITOR_SPELL;
    public static Image ICON_EDITOR_SEARCH;
    
    public static Image ICON_EDITOR_ADDPAGE;
    public static Image ICON_EDITOR_REMOVEPAGE;
    public static Image ICON_EDITOR_WEBRIP;
    public static Image ICON_EDITOR_TOGGLETYPE;
    public static Image ICON_EDITOR_ADDIMAGE;
    public static Image ICON_EDITOR_ADDFILE;
    public static Image ICON_EDITOR_REMOVEFILE;
    public static Image ICON_EDITOR_QUOTE;
    
    public static Image ICON_ONLINE;
    public static Image ICON_OFFLINE;
    
    public static Image ICON_TASKTREE_CLOSE_SELF;
    public static Image ICON_TASKTREE_CLOSE_GROUP;
    
    public static Image ICON_MSGNAV_FORUM;
    public static Image ICON_MSGNAV_NEXTINTHREAD;
    public static Image ICON_MSGNAV_PREVINTHREAD;
    public static Image ICON_MSGNAV_NEXTVIATHREAD;
    public static Image ICON_MSGNAV_PREVVIATHREAD;
    public static Image ICON_MSGNAV_NEXTTHREAD;
    public static Image ICON_MSGNAV_PREVTHREAD;
    public static Image ICON_MSGNAV_NEXTNEW;
    public static Image ICON_MSGNAV_PREVNEW;
    
    public static Image ICON_CLOSE;
    public static Image ICON_HELP;
    
    public static final Cursor CURSOR_WAIT = Display.getDefault().getSystemCursor(SWT.CURSOR_WAIT);
    
    private static boolean _initialized = false;
    public static void init(File tmpDir, Timer timer) {
        _timer = timer;
        _timer.addEvent("init begin");
        _tmpDir = tmpDir;
        synchronized (ImageUtil.class) {
            if (_initialized) return;
            initImages();
            _initialized = true;
        }
        /*
        _indisposableImages.add(ICON_ERROR);
        _indisposableImages.add(ICON_INFORMATION);
        _indisposableImages.add(ICON_QUESTION);
        _indisposableImages.add(ICON_WARNING);
        _indisposableImages.add(ICON_WORKING);
         */
        
        /* File Menu*/ 
        _indisposableImages.add(ICON_FM_EXIT);
        _indisposableImages.add(ICON_FM_EXPORT);
        _indisposableImages.add(ICON_FM_IMPORT);

        /* View Menu*/ 
        _indisposableImages.add(ICON_VM_BOOKMARK);
        _indisposableImages.add(ICON_VM_LANGUAGE);
        _indisposableImages.add(ICON_VM_STYLE);
        
        /* Post Menu */
        _indisposableImages.add(ICON_PM_NEWPOST);
        
        /* Help Menu*/ 
        _indisposableImages.add(ICON_HM_ABOUT);
        _indisposableImages.add(ICON_HM_BUG);
        
        /* Tab Icons */
        _indisposableImages.add(ICON_T_EDIT);
        _indisposableImages.add(ICON_T_MSG);
        _indisposableImages.add(ICON_T_LOGS);
        _indisposableImages.add(ICON_T_TEXT);
        _indisposableImages.add(ICON_T_BUG);
        
        /* Context Menu Icons */
        _indisposableImages.add(ICON_VIEW);
        _indisposableImages.add(ICON_EDIT);
        _indisposableImages.add(ICON_DELETE);
        _indisposableImages.add(ICON_CANCEL);
        _indisposableImages.add(ICON_CLEAR);
        
        _indisposableImages.add(ICON_FOLDER);
        _indisposableImages.add(ICON_ADDFOLDER);
        _indisposableImages.add(ICON_ADDBOOKMARK);
        
        _indisposableImages.add(ICON_VIEWFORUM);
        _indisposableImages.add(ICON_CREATEFORUM);
        _indisposableImages.add(ICON_WRITEABLEFORUM);
        _indisposableImages.add(ICON_MANAGEABLEFORUM);
        _indisposableImages.add(ICON_SETTINGS);
        _indisposableImages.add(ICON_SYNC);
        _indisposableImages.add(ICON_CONTROLSYNDICATION);
        
        _indisposableImages.add(ICON_STARTARCHIVESERVER);
        _indisposableImages.add(ICON_STOPARCHIVESERVER);
        _indisposableImages.add(ICON_CONFIGUREARCHIVESERVER);
        _indisposableImages.add(ICON_ADDARCHIVE);
        _indisposableImages.add(ICON_DELETEARCHIVE);
        _indisposableImages.add(ICON_MANAGEARCHIVE);
        _indisposableImages.add(ICON_CANCELSYNDICATIONS);
        
        _indisposableImages.add(ICON_MSG);
        _indisposableImages.add(ICON_VIEWMESSAGE);
        _indisposableImages.add(ICON_REPLYMESSAGE);
        _indisposableImages.add(ICON_UNREADMESSAGE);
        
        _indisposableImages.add(ICON_FORUMMESSAGES);
        _indisposableImages.add(ICON_FORUMPROFILE);
        _indisposableImages.add(ICON_EXPAND);
        _indisposableImages.add(ICON_COLLAPSE);
        
        _indisposableImages.add(ICON_SHELL);
        
        _indisposableImages.add(ICON_LINK_END);
        _indisposableImages.add(ICON_IMAGE_UNKNOWN);
        
        _indisposableImages.add(ICON_SYNDICATE_TYPE_DIRECT);
        _indisposableImages.add(ICON_SYNDICATE_TYPE_INDIRECT);
        _indisposableImages.add(ICON_SYNDICATE_TYPE_FREENET);
        
        _indisposableImages.add(ICON_SYNDICATE_STATUS_ERROR);
        _indisposableImages.add(ICON_SYNDICATE_STATUS_INPROGRESS);
        _indisposableImages.add(ICON_SYNDICATE_STATUS_SCHEDULED);
        _indisposableImages.add(ICON_SYNDICATE_STATUS_NOKEY);
        _indisposableImages.add(ICON_SYNDICATE_STATUS_PBE);
        _indisposableImages.add(ICON_SYNDICATE_STATUS_OK);
        _indisposableImages.add(ICON_SYNDICATE_PUSH);
        
        _indisposableImages.add(ICON_MSG_TYPE_NORMAL);
        _indisposableImages.add(ICON_MSG_TYPE_META);
        _indisposableImages.add(ICON_MSG_TYPE_PRIVATE);
        
        _indisposableImages.add(ICON_MSG_FLAG_PBE);
        _indisposableImages.add(ICON_MSG_FLAG_READKEYUNKNOWN);
        _indisposableImages.add(ICON_MSG_FLAG_REPLYKEYUNKNOWN);
        _indisposableImages.add(ICON_MSG_FLAG_UNREADABLE);
        _indisposableImages.add(ICON_MSG_FLAG_PUBLIC);
        _indisposableImages.add(ICON_MSG_FLAG_AUTHENTICATED);
        _indisposableImages.add(ICON_MSG_FLAG_AUTHORIZED);
        _indisposableImages.add(ICON_MSG_FLAG_BANNED);
        _indisposableImages.add(ICON_MSG_FLAG_BOOKMARKED_AUTHOR);
        _indisposableImages.add(ICON_MSG_FLAG_BOOKMARKED_FORUM);
        _indisposableImages.add(ICON_MSG_FLAG_SCHEDULEDFOREXPIRE);
        _indisposableImages.add(ICON_MSG_FLAG_HASKEYS);
        _indisposableImages.add(ICON_MSG_FLAG_HASARCHIVES);
        _indisposableImages.add(ICON_MSG_FLAG_HASREFS);
        _indisposableImages.add(ICON_MSG_FLAG_HASATTACHMENTS);
        _indisposableImages.add(ICON_MSG_FLAG_ISNEW);
        
        _indisposableImages.add(ICON_REF_MSG);
        _indisposableImages.add(ICON_REF_FORUM);
        _indisposableImages.add(ICON_REF_ARCHIVE);
        _indisposableImages.add(ICON_REF_URL);
        _indisposableImages.add(ICON_REF_SYNDIE);
        _indisposableImages.add(ICON_REF_FREENET);
        
        _indisposableImages.add(ICON_BROWSE_ADMINS);
        _indisposableImages.add(ICON_BROWSE_MANAGEABLE);
        _indisposableImages.add(ICON_BROWSE_ARCHIVES);
        _indisposableImages.add(ICON_BROWSE_POSTABLE);
        _indisposableImages.add(ICON_BROWSE_REFS);
        
        _indisposableImages.add(ICON_TAB_EDIT);
        _indisposableImages.add(ICON_TAB_TEXTUI);
        _indisposableImages.add(ICON_TAB_LOGS);
        _indisposableImages.add(ICON_TAB_SYNDICATE);
        _indisposableImages.add(ICON_TAB_SQL);
        _indisposableImages.add(ICON_TAB_PAGE);
        _indisposableImages.add(ICON_TAB_HIGHLIGHTS);
        _indisposableImages.add(ICON_TAB_MSG);
        _indisposableImages.add(ICON_TAB_ARCHIVE);
        _indisposableImages.add(ICON_TAB_BROWSE);
        
        _indisposableImages.add(ICON_EDITOR_PRIVACY_PUBLIC);
        _indisposableImages.add(ICON_EDITOR_PRIVACY_PBE);
        _indisposableImages.add(ICON_EDITOR_PRIVACY_AUTHORIZED);
        _indisposableImages.add(ICON_EDITOR_PRIVACY_REPLY);
        _indisposableImages.add(ICON_EDITOR_BOOKMARKED_NOAVATAR);
        _indisposableImages.add(ICON_EDITOR_NOT_BOOKMARKED);
        _indisposableImages.add(ICON_EDITOR_PAGEADD);
        _indisposableImages.add(ICON_EDITOR_PAGETYPE_TEXT);
        _indisposableImages.add(ICON_EDITOR_PAGETYPE_HTML);
        _indisposableImages.add(ICON_EDITOR_ATTACH);
        _indisposableImages.add(ICON_EDITOR_LINK);
        _indisposableImages.add(ICON_EDITOR_STYLE);
        _indisposableImages.add(ICON_EDITOR_SPELL);
        _indisposableImages.add(ICON_EDITOR_SEARCH);
    
        _indisposableImages.add(ICON_EDITOR_ADDPAGE);
        _indisposableImages.add(ICON_EDITOR_REMOVEPAGE);
        _indisposableImages.add(ICON_EDITOR_WEBRIP);
        _indisposableImages.add(ICON_EDITOR_TOGGLETYPE);
        _indisposableImages.add(ICON_EDITOR_ADDIMAGE);
        _indisposableImages.add(ICON_EDITOR_ADDFILE);
        _indisposableImages.add(ICON_EDITOR_REMOVEFILE);
        _indisposableImages.add(ICON_EDITOR_QUOTE);
        
        _indisposableImages.add(ICON_ONLINE);
        _indisposableImages.add(ICON_OFFLINE);
        
        _indisposableImages.add(ICON_TASKTREE_CLOSE_GROUP);
        _indisposableImages.add(ICON_TASKTREE_CLOSE_SELF);
        
        _indisposableImages.add(ICON_MSGNAV_FORUM);
        _indisposableImages.add(ICON_MSGNAV_NEXTINTHREAD);
        _indisposableImages.add(ICON_MSGNAV_NEXTNEW);
        _indisposableImages.add(ICON_MSGNAV_NEXTTHREAD);
        _indisposableImages.add(ICON_MSGNAV_NEXTVIATHREAD);
        _indisposableImages.add(ICON_MSGNAV_PREVINTHREAD);
        _indisposableImages.add(ICON_MSGNAV_PREVNEW);
        _indisposableImages.add(ICON_MSGNAV_PREVTHREAD);
        _indisposableImages.add(ICON_MSGNAV_PREVVIATHREAD);
        
        _indisposableImages.add(ICON_CLOSE);
        _indisposableImages.add(ICON_HELP);
        
        _timer.addEvent("image init add complete");
        //_timer.complete();
    }
    
    private static void initImages() {
        /*
        ICON_ERROR = Display.getDefault().getSystemImage(SWT.ICON_ERROR);
        ICON_INFORMATION = Display.getDefault().getSystemImage(SWT.ICON_INFORMATION);
        ICON_QUESTION = Display.getDefault().getSystemImage(SWT.ICON_QUESTION);
        ICON_WARNING = Display.getDefault().getSystemImage(SWT.ICON_WARNING);
        ICON_WORKING = Display.getDefault().getSystemImage(SWT.ICON_WORKING);
         */
        
        _timer.addEvent("image init: system images fetched");
        
        ICON_SHELL = createImageFromResource("iconShell.png");
        ICON_LINK_END = createImageFromResource("iconLink.png");
        ICON_IMAGE_UNKNOWN = createImageFromResource("iconUnknown.png");
        
        /* File Menu */
        ICON_FM_EXIT = createImageFromResource("iconExit.png");
        ICON_FM_EXPORT = createImageFromResource("iconExport.png");
        ICON_FM_IMPORT = createImageFromResource("iconImport.png");
        
        /* View Menu */
        ICON_VM_BOOKMARK = createImageFromResource("iconBookmark.png");
        ICON_VM_LANGUAGE = createImageFromResource("iconLanguage.png");
        ICON_VM_STYLE = createImageFromResource("iconStyle.png");
        
        /* Post Menu */
        ICON_PM_NEWPOST = createImageFromResource("iconNewPost.png");
                
        /* About Menu */
        ICON_HM_ABOUT = createImageFromResource("iconInfo.png");
        ICON_HM_BUG = createImageFromResource("iconBug.png");
        
        /* Context menu icons */
        ICON_VIEW = createImageFromResource("iconView.png");
        ICON_EDIT = createImageFromResource("iconEdit.png");
        ICON_DELETE = createImageFromResource("iconDelete.png");
        ICON_CLEAR = createImageFromResource("iconClear.png");
        ICON_CANCEL = createImageFromResource("iconCancel.png");
        
        ICON_FOLDER = createImageFromResource("iconFolder.png");
        ICON_ADDBOOKMARK = createImageFromResource("iconAddBookmark.png");
        ICON_ADDFOLDER = createImageFromResource("iconAddfolder.png");
        
        ICON_VIEWFORUM = createImageFromResource("iconViewForum.png");
        ICON_CREATEFORUM = createImageFromResource("iconCreateForum.png");
        ICON_SETTINGS = createImageFromResource("iconSettings.png");
        ICON_SYNC = createImageFromResource("iconSync.png");
        
        ICON_VIEWMESSAGE = createImageFromResource("iconViewMessage.png");
        ICON_REPLYMESSAGE = createImageFromResource("iconReplyMessage.png");
        ICON_FORUMMESSAGES = createImageFromResource("iconForumMessages.png");
        ICON_FORUMPROFILE = createImageFromResource("iconForumProfile.png");
        ICON_EXPAND = createImageFromResource("iconExpand.png");
        ICON_COLLAPSE = createImageFromResource("iconCollapse.png");
        
        ICON_WRITEABLEFORUM = createImageFromResource("iconWriteableForum.png");
        ICON_MANAGEABLEFORUM = createImageFromResource("iconManageableForum.png");
        ICON_WATCHEDFORUM = createImageFromResource("iconWatchedForum.png");
        ICON_IMPORTEDRESOURCES = createImageFromResource("iconImportedResources.png");

        ICON_STARTARCHIVESERVER = createImageFromResource("iconStartArchiveServer.png");
        ICON_STOPARCHIVESERVER = createImageFromResource("iconStopArchiveServer.png");
        ICON_CONFIGUREARCHIVESERVER = createImageFromResource("iconConfigureArchiveServer.png");
        ICON_ADDARCHIVE = createImageFromResource("iconAddArchive.png");
        ICON_DELETEARCHIVE = createImageFromResource("iconDeleteArchive.png");
        ICON_MANAGEARCHIVE = createImageFromResource("iconManageArchive.png");
        ICON_CANCELSYNDICATIONS = createImageFromResource("iconCancelSyndications.png");
        
        ICON_MSG = createImageFromResource("iconMessage.png");
        ICON_UNREADMESSAGE = createImageFromResource("iconUnreadMessage.png");
        ICON_CONTROLSYNDICATION = createImageFromResource("iconControlSyndiation.png");

    
        ICON_SYNDICATE_TYPE_DIRECT = createImageFromResource("iconDirect.png");
        ICON_SYNDICATE_TYPE_INDIRECT = createImageFromResource("iconIndirect.png");
        ICON_SYNDICATE_TYPE_FREENET = createImageFromResource("iconFreenet.png");
        ICON_SYNDICATE_STATUS_INPROGRESS = createImageFromResource("iconSynInProgress.png");
        ICON_SYNDICATE_STATUS_SCHEDULED = createImageFromResource("iconSynScheduled.png");
        ICON_SYNDICATE_STATUS_NOKEY = createImageFromResource("iconSynNoKey.png");
        ICON_SYNDICATE_STATUS_ERROR = createImageFromResource("iconSynError.png");
        ICON_SYNDICATE_STATUS_PBE = createImageFromResource("iconSynPBE.png");
        ICON_SYNDICATE_STATUS_OK = createImageFromResource("iconSynOk.png");
        ICON_SYNDICATE_PUSH = createImageFromResource("iconSynPush.png");
    
        ICON_ARCHIVE_TYPE_FILE = createImageFromResource("iconArchiveFile.png");
        ICON_ARCHIVE_TYPE_FREENET = createImageFromResource("iconArchiveFreenet.png");
        ICON_ARCHIVE_TYPE_URL = createImageFromResource("iconArchiveURL.png");
        ICON_ARCHIVE_TYPE_SYNDIE = createImageFromResource("iconArchiveSyndie.png");
    
        ICON_MSG_TYPE_NORMAL = createImageFromResource("iconMsgNormal.png");
        ICON_MSG_TYPE_META = createImageFromResource("iconMsgMeta.png");
        ICON_MSG_TYPE_PRIVATE = createImageFromResource("iconMsgPrivate.png");

        ICON_MSG_FLAG_READKEYUNKNOWN = createImageFromResource("iconMsgFlagReadKeyUnknown.png");
        ICON_MSG_FLAG_REPLYKEYUNKNOWN = createImageFromResource("iconMsgFlagReplyKeyUnknown.png");
        ICON_MSG_FLAG_PBE = createImageFromResource("iconMsgFlagPBE.png");
        ICON_MSG_FLAG_UNREADABLE = createImageFromResource("iconMsgFlagUnreadable.png");
        ICON_MSG_FLAG_PUBLIC = createImageFromResource("iconMsgFlagPublic.png");
        ICON_MSG_FLAG_AUTHENTICATED = createImageFromResource("iconMsgFlagAuthenticated.png");
        ICON_MSG_FLAG_AUTHORIZED = createImageFromResource("iconMsgFlagAuthorized.png");
        ICON_MSG_FLAG_BANNED = createImageFromResource("iconMsgFlagBanned.png");
        ICON_MSG_FLAG_BOOKMARKED_AUTHOR = createImageFromResource("iconMsgFlagBookmarked.png", false); // use two different images, so we can
        ICON_MSG_FLAG_BOOKMARKED_FORUM = createImageFromResource("iconMsgFlagBookmarked.png", false); // differentiate them in the message flag bar
        ICON_MSG_FLAG_SCHEDULEDFOREXPIRE = createImageFromResource("iconMsgFlagScheduledForExpire.png");
        ICON_MSG_FLAG_HASKEYS = createImageFromResource("iconMsgFlagHasKeys.png");
        ICON_MSG_FLAG_HASARCHIVES = createImageFromResource("iconMsgFlagHasArchives.png");
        ICON_MSG_FLAG_HASREFS = createImageFromResource("iconMsgFlagHasRefs.png");
        ICON_MSG_FLAG_HASATTACHMENTS = createImageFromResource("iconMsgFlagHasAttachments.png");
        ICON_MSG_FLAG_ISNEW = createImageFromResource("iconMsgFlagIsNew.png");
    
        ICON_REF_MSG = createImageFromResource("iconRefMsg.png");
        ICON_REF_FORUM = createImageFromResource("iconRefForum.png");
        ICON_REF_ARCHIVE = createImageFromResource("iconRefArchive.png");
        ICON_REF_URL = createImageFromResource("iconRefURL.png");
        ICON_REF_SYNDIE = createImageFromResource("iconRefSyndie.png");
        ICON_REF_FREENET = createImageFromResource("iconRefFreenet.png");
    
        ICON_BROWSE_ADMINS = createImageFromResource("iconBrowseAdmins.png");
        ICON_BROWSE_MANAGEABLE = createImageFromResource("iconBrowseManage.png");
        ICON_BROWSE_POSTABLE = createImageFromResource("iconBrowsePost.png");
        ICON_BROWSE_ARCHIVES = createImageFromResource("iconBrowseArchives.png");
        ICON_BROWSE_REFS = createImageFromResource("iconBrowseRefs.png");
        
        ICON_CLOSE = createImageFromResource("iconClose.png");
        ICON_HELP = createImageFromResource("iconHelp.png");

        _timer.addEvent("image init: bulk icons created");
        
        
        ICON_TAB_EDIT = resize(ImageUtil.ICON_PM_NEWPOST, TAB_ICON_SIZE, TAB_ICON_SIZE, false);
        ICON_TAB_TEXTUI = resize(ImageUtil.ICON_REF_SYNDIE, TAB_ICON_SIZE, TAB_ICON_SIZE, false);
        ICON_TAB_LOGS = resize(ImageUtil.ICON_REF_SYNDIE, TAB_ICON_SIZE, TAB_ICON_SIZE, false);
        ICON_TAB_SYNDICATE = resize(ImageUtil.ICON_REF_SYNDIE, TAB_ICON_SIZE, TAB_ICON_SIZE, false);
        ICON_TAB_SQL = resize(ImageUtil.ICON_REF_SYNDIE, TAB_ICON_SIZE, TAB_ICON_SIZE, false);
        ICON_TAB_PAGE = resize(ImageUtil.ICON_PM_NEWPOST, TAB_ICON_SIZE, TAB_ICON_SIZE, false);
        ICON_TAB_HIGHLIGHTS = resize(ImageUtil.ICON_PM_NEWPOST, TAB_ICON_SIZE, TAB_ICON_SIZE, false);
        ICON_TAB_MSG = resize(ImageUtil.ICON_MSG, TAB_ICON_SIZE, TAB_ICON_SIZE, false);
        ICON_TAB_ARCHIVE = resize(ImageUtil.ICON_REF_ARCHIVE, TAB_ICON_SIZE, TAB_ICON_SIZE, false);
        ICON_TAB_BROWSE = resize(ImageUtil.ICON_FOLDER, TAB_ICON_SIZE, TAB_ICON_SIZE, false);

        _timer.addEvent("image init: tab icons resized");
        
        ICON_EDITOR_PRIVACY_PUBLIC = createImageFromResource("iconPrivPublic.png");
        ICON_EDITOR_PRIVACY_PBE = createImageFromResource("iconPrivPBE.png");
        ICON_EDITOR_PRIVACY_AUTHORIZED = createImageFromResource("iconPrivAuthorized.png");
        ICON_EDITOR_PRIVACY_REPLY = createImageFromResource("iconPrivReply.png");
        ICON_EDITOR_BOOKMARKED_NOAVATAR = createImageFromResource("iconEdBookmarkedNoAvatar.png");
        ICON_EDITOR_NOT_BOOKMARKED = createImageFromResource("iconEdNotBookmarked.png");
        ICON_EDITOR_PAGEADD = createImageFromResource("iconEdPageAdd.png");
        ICON_EDITOR_PAGETYPE_TEXT = createImageFromResource("iconEdPageTypeText.png");
        ICON_EDITOR_PAGETYPE_HTML = createImageFromResource("iconEdPageTypeHTML.png");
        ICON_EDITOR_ATTACH = createImageFromResource("iconEdAttach.png");
        ICON_EDITOR_LINK = createImageFromResource("iconEdLink.png");
        
        ICON_EDITOR_STYLE = createImageFromResource("iconEdStyle.png");
        ICON_EDITOR_SPELL = createImageFromResource("iconEdSpell.png");
        ICON_EDITOR_SEARCH = createImageFromResource("iconEdSearch.png");
    
        ICON_EDITOR_ADDPAGE = createImageFromResource("iconEdAddPage.png");
        ICON_EDITOR_REMOVEPAGE = createImageFromResource("iconEdRemovePage.png");
        ICON_EDITOR_WEBRIP = createImageFromResource("iconEdWebRip.png");
        ICON_EDITOR_TOGGLETYPE = createImageFromResource("iconEdToggleType.png");
        ICON_EDITOR_ADDIMAGE = createImageFromResource("iconEdAddImage.png");
        ICON_EDITOR_ADDFILE = createImageFromResource("iconEdAddFile.png");
        ICON_EDITOR_REMOVEFILE = createImageFromResource("iconEdRemoveFile.png");
        ICON_EDITOR_QUOTE = createImageFromResource("iconEdQuote.png");
    
        ICON_ONLINE = createImageFromResource("iconOnline.png");
        ICON_OFFLINE = createImageFromResource("iconOffline.png");
        
        ICON_TASKTREE_CLOSE_GROUP = createImageFromResource("iconTaskCloseGroup.png");
        ICON_TASKTREE_CLOSE_SELF = createImageFromResource("iconTaskCloseSelf.png");
        
        ICON_MSGNAV_FORUM = createImageFromResource("iconMsgNavForum.png");
        ICON_MSGNAV_NEXTINTHREAD = createImageFromResource("iconMsgNavNextInThread.png");
        ICON_MSGNAV_NEXTNEW = createImageFromResource("iconMsgNavNextNew.png");
        ICON_MSGNAV_NEXTTHREAD = createImageFromResource("iconMsgNavNextThread.png");
        ICON_MSGNAV_NEXTVIATHREAD = createImageFromResource("iconMsgNavNextViaThread.png");
        ICON_MSGNAV_PREVINTHREAD = createImageFromResource("iconMsgNavPrevInThread.png");
        ICON_MSGNAV_PREVNEW = createImageFromResource("iconMsgNavPrevNew.png");
        ICON_MSGNAV_PREVTHREAD = createImageFromResource("iconMsgNavPrevThread.png");
        ICON_MSGNAV_PREVVIATHREAD = createImageFromResource("iconMsgNavPrevViaThread.png");
        
        _timer.addEvent("image init: remaining images created");
    }
    
    public static Image resize(Image orig, int width, int height, boolean dispose) {
        if ( (orig == null) || (orig.isDisposed()) ) return null;
        Image scaled = new Image(Display.getDefault(), width, height);
        GC gc = new GC(scaled);
        Rectangle origBounds = orig.getBounds();
        gc.drawImage(orig, 0, 0, origBounds.width, origBounds.height, 0, 0, width, height);
        gc.dispose();
        if (dispose)
            dispose(orig);
        return scaled;
    }
    
    public static Image createImage(byte data[]) { return createImage(data, _tmpDir); }
    public static Image createMemoryImage(byte data[]) {
        if (data == null) return null;
        try {
            return new Image(Display.getDefault(), new ByteArrayInputStream(data));
        } catch (IllegalArgumentException iae) {
            return null;
        } catch (SWTException se) {
            return null;
        }
    }
    
    /**
     * swt is optimized for loading images from disk, so lets exploit that by
     * writing to disk, loading it from there, and then deleting the temp file.
     * this falls back on loading in-memory if it can't write to the disk
     */
    public static Image createImage(byte data[], File tmpDir) {
        if (data == null) return null;
        try {
            File tmp = null;
            try {
                tmp = File.createTempFile("img", ".png", tmpDir);
                FileOutputStream fos = new FileOutputStream(tmp);
                fos.write(data);
                fos.close();
            } catch (IOException ioe) { 
                tmp.delete();
                return createMemoryImage(data);
            }
            
            Image img = new Image(Display.getDefault(), tmp.getAbsolutePath()); //new ByteArrayInputStream(data));
            tmp.delete();
            return img;
        } catch (IllegalArgumentException iae) {
            return null;
        } catch (SWTException se) {
            return null;
        }
    }
    
    public static Image createImage(int width, int height, Color color, boolean indisposable) {
        Image img = new Image(Display.getDefault(), width, height);
        GC gc = new GC(img);
        gc.setForeground(color);
        gc.setBackground(color);
        //gc.drawRectangle(0, 0, 16, 16);
        gc.fillRectangle(0, 0, 16, 16);
        gc.dispose();
        if (indisposable) _indisposableImages.add(img);
        return img;
    }
    
    public static Image createImageFromResource(String resource) { return createImageFromResource(resource, true); }
    public static Image createImageFromResource(String resource, boolean cache) {
        if (false) return Display.getDefault().getSystemImage(SWT.ICON_WARNING);
        synchronized (_loadedResources) {
            Image img = (Image)_loadedResources.get(resource);
            if (cache && (img != null))
                return img;
            //_timer.addEvent("before getResource("+resource+")");
            InputStream in = ImageUtil.class.getResourceAsStream(resource);
            //_timer.addEvent("after getResource("+resource+")");
            if (in != null) {
                if (_tmpDir != null) {
                    try {
                        File tmp = null;
                        try {
                            tmp = File.createTempFile("img", ".png", _tmpDir);
                            FileOutputStream fos = new FileOutputStream(tmp);
                            byte buf[] = new byte[4096];
                            int read = -1;
                            while ( (read = in.read(buf)) != -1)
                                fos.write(buf, 0, read);
                            fos.close();

                            img = new Image(Display.getDefault(), tmp.getAbsolutePath());
                            tmp.delete();
                        } catch (IOException ioe) { 
                            tmp.delete();
                            in = ImageUtil.class.getResourceAsStream(resource);
                            img = new Image(Display.getDefault(), in);
                        }

                    } catch (IllegalArgumentException iae) {
                        return null;
                    } catch (SWTException se) {
                        return null;
                    }
                } else { // no tmpDir yet
                    try {
                        img = new Image(Display.getDefault(), in);
                    } catch (IllegalArgumentException iae) {
                        return null;
                    } catch (SWTException se) {
                        return null;
                    }                    
                }
                //_timer.addEvent("after image instantiation ("+resource+")");
                _indisposableImages.add(img);
                _loadedResources.put(resource, img);
                return img;

            } else {
                return null;
            }
        }
    }
    
    public static Image createImageFromFile(String filename) throws SWTException {
        return new Image(Display.getDefault(), filename);
    }
    
    public static byte[] serializeImage(Image img) throws SWTException {
        ImageLoader loader = new ImageLoader();
        ByteArrayOutputStream outBuf = new ByteArrayOutputStream();
        loader.data = new ImageData[] { img.getImageData() };
        // foo. png not supported on early SWT (though newer swt revs do)
        //loader.save(outBuf, SWT.IMAGE_PNG);
        loader.save(outBuf, SWT.IMAGE_JPEG);
        return outBuf.toByteArray();
    }
    
    public static int getWidth(String text, Drawable target) {
        GC gc = new GC(target);
        FontMetrics fm = gc.getFontMetrics();
        int per = fm.getAverageCharWidth();
        gc.dispose();
        int rv = per * (text == null ? 1 : text.length());
        return rv;
    }
    
    public static Image getTypeIcon(SyndieURI uri) {
        int type = getType(uri);
        if (type == TYPE_MSG) return ICON_REF_MSG;
        else if (type == TYPE_FORUM) return ICON_REF_FORUM;
        else if (type == TYPE_ARCHIVE) return ICON_REF_ARCHIVE;
        else if (type == TYPE_URL) return ICON_REF_URL;
        else if (type == TYPE_SYNDIE) {
            String str = uri.getType();
            if (str != null) {
                if (BrowserTab.TYPE_LOGS.equals(str))
                    return ICON_TAB_LOGS;
                else if (BrowserTab.TYPE_SYNDICATE_ARCHIVES.equals(str))
                    return ICON_TAB_SYNDICATE;
                else if (BrowserTab.TYPE_SYNDICATE_CONFIG.equals(str))
                    return ICON_TAB_SYNDICATE;
                else if (BrowserTab.TYPE_SYNDICATE_STATUS.equals(str))
                    return ICON_TAB_SYNDICATE;
                else if (BrowserTab.TYPE_POST.equals(str))
                    return ICON_TAB_EDIT;
            }
            return ICON_REF_SYNDIE;
        }
        else if (type == TYPE_FREENET) return ICON_REF_FREENET;
        return null;
    }
    
    private static final int TYPE_MSG = 0;
    private static final int TYPE_FORUM = 1;
    private static final int TYPE_ARCHIVE = 2;
    private static final int TYPE_URL = 3;
    private static final int TYPE_SYNDIE = 4;
    private static final int TYPE_FREENET = 5;
    private static final int TYPE_OTHER = -1;
    
    private static int getType(SyndieURI uri) {
        if (uri == null) {
            return TYPE_OTHER;
        } else if (uri.isChannel()) {
            if (uri.getScope() != null) {
                if (uri.getMessageId() != null) 
                    return TYPE_MSG;
                else
                    return TYPE_FORUM;
            } else {
                return TYPE_SYNDIE;
            }
        } else if (uri.isArchive()) {
            return TYPE_ARCHIVE;
        } else if (uri.isURL()) {
            String url = uri.getURL();
            if ( (url != null) && (url.startsWith("SSK@") || url.startsWith("CHK@") || url.startsWith("USK@")) )
                return TYPE_FREENET;
            else
                return TYPE_URL;
        } else if (uri.isSearch()) {
            Hash scope = uri.getHash("scope");
            if (scope == null)
                return TYPE_SYNDIE;
            else if (uri.getMessageId() == null)
                return TYPE_FORUM;
            else
                return TYPE_MSG;
        } else if (BrowserTab.TYPE_SYNDICATE_ARCHIVES.equals(uri.getType())) {
            return TYPE_ARCHIVE;
        } else if (BrowserTab.TYPE_MANAGE.equals(uri.getType())) {
            return TYPE_FORUM;
        } else if (BrowserTab.TYPE_META.equals(uri.getType())) {
            return TYPE_FORUM;
        } else {
            return TYPE_SYNDIE;
        }
    }
    
    private static final boolean BUTTONS_HAVE_BG = System.getProperty("os.name").toLowerCase().indexOf("win") == -1;
    
    /**
     * vertically draw the given text on the control in the specified font so that the
     * bottom of the words are on the right hand side
     */
    public static void drawAscending(GC gc, Control ctl, Font font, String text) {
        drawRotated(gc, ctl, font, text, -90);
    }
    
    /**
     * vertically draw the given text on the control in the specified font so that the
     * bottom of the words are on the left hand side
     */
    public static void drawDescending(GC gc, Control ctl, Font font, String text) {
        drawRotated(gc, ctl, font, text, 90);
    }
    
    /**
     * create a new image containing the rotated text, and draw that image onto the control
     */
    private static void drawRotated(GC gc, Control ctl, Font font, String text, int angle) {
        gc.setFont(font);
        Point size = gc.stringExtent(text);

        Image img = new Image(ctl.getDisplay(), size.y, size.x);
        GC imgGC = new GC(img);
        
        // windows doesn't support background colors on buttons (at least not in swt), so
        // on windows, use the system gray as the bgcolor
        if (BUTTONS_HAVE_BG) {
            imgGC.setForeground(gc.getBackground());
            imgGC.setBackground(gc.getBackground());
        } else {
            imgGC.setForeground(ColorUtil.getColor("#D4D0C8")); // ms xp gray
            imgGC.setBackground(ColorUtil.getColor("#D4D0C8"));
        }
        // the drawString leaves some white spots around the text, so fill 'er with the bgcolor
        imgGC.fillRectangle(0, 0, size.y+20, size.x+20);
        imgGC.setFont(font);
        imgGC.setAntialias(SWT.ON);

        Transform transform = new Transform(ctl.getDisplay());
        transform.rotate(angle);
        imgGC.setTransform(transform);

        imgGC.setForeground(ColorUtil.getColor("black"));
        if (angle > 0)
            imgGC.drawString(text, 0, -size.y);
        else
            imgGC.drawString(text, -size.x, 0);
        imgGC.dispose();
        transform.dispose();

        Rectangle ctlSize = ctl.getBounds();
        Rectangle textSize = img.getBounds();

        gc.drawImage(img, (ctlSize.width - textSize.width) / 2, (ctlSize.height - textSize.height) / 2);
    }
}
