package syndie.gui;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.Map;
import java.util.HashMap;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Drawable;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import syndie.data.SyndieURI;

/**
 *
 */
public class ImageUtil {
    private static final Set _indisposableImages = Collections.synchronizedSet(new HashSet());
    /** resource name to Image */
    private static final Map _loadedResources = new HashMap();
    
    public static void dispose(Image img) {
        if ( (img == null) || (img.isDisposed()) || (_indisposableImages.contains(img)) )
            return;
        img.dispose();
    }
    
    public static final Image ICON_ERROR = Display.getDefault().getSystemImage(SWT.ICON_ERROR);
    public static final Image ICON_INFORMATION = Display.getDefault().getSystemImage(SWT.ICON_INFORMATION);
    public static final Image ICON_QUESTION = Display.getDefault().getSystemImage(SWT.ICON_QUESTION);
    public static final Image ICON_WARNING = Display.getDefault().getSystemImage(SWT.ICON_WARNING);
    public static final Image ICON_WORKING = Display.getDefault().getSystemImage(SWT.ICON_WORKING);
    
    /** shown after a link when rendering */
    public static final Image ICON_LINK_END = createImageFromResource("iconLink.png");
    /** shown in place of a nonexistant image when rendering */
    public static final Image ICON_IMAGE_UNKNOWN = createImageFromResource("iconUnknown.png");
    
    public static final Image ICON_SYNDICATE_TYPE_DIRECT = createImageFromResource("iconDirect.png");
    public static final Image ICON_SYNDICATE_TYPE_INDIRECT = createImageFromResource("iconIndirect.png");
    public static final Image ICON_SYNDICATE_TYPE_FREENET = createImageFromResource("iconFreenet.png");
    public static final Image ICON_SYNDICATE_STATUS_INPROGRESS = createImageFromResource("iconSynInProgress.png");
    public static final Image ICON_SYNDICATE_STATUS_SCHEDULED = createImageFromResource("iconSynScheduled.png");
    public static final Image ICON_SYNDICATE_STATUS_NOKEY = createImageFromResource("iconSynNoKey.png");
    public static final Image ICON_SYNDICATE_STATUS_ERROR = createImageFromResource("iconSynError.png");
    public static final Image ICON_SYNDICATE_STATUS_PBE = createImageFromResource("iconSynPBE.png");
    public static final Image ICON_SYNDICATE_STATUS_OK = createImageFromResource("iconSynOk.png");

    public static final Image ICON_ARCHIVE_TYPE_FILE = createImageFromResource("iconArchiveFile.png");
    public static final Image ICON_ARCHIVE_TYPE_FREENET = createImageFromResource("iconArchiveFreenet.png");
    public static final Image ICON_ARCHIVE_TYPE_URL = createImageFromResource("iconArchiveURL.png");
    public static final Image ICON_ARCHIVE_TYPE_SYNDIE = createImageFromResource("iconArchiveSyndie.png");
    
    public static final Image ICON_MSG_TYPE_NORMAL = createImageFromResource("iconMsgNormal.png");
    public static final Image ICON_MSG_TYPE_META = createImageFromResource("iconMsgMeta.png");
    public static final Image ICON_MSG_TYPE_PRIVATE = createImageFromResource("iconMsgPrivate.png");
    
    public static final Image ICON_REF_MSG = createImageFromResource("iconRefMsg.png");
    public static final Image ICON_REF_FORUM = createImageFromResource("iconRefForum.png");
    public static final Image ICON_REF_ARCHIVE = createImageFromResource("iconRefArchive.png");
    public static final Image ICON_REF_URL = createImageFromResource("iconRefURL.png");
    public static final Image ICON_REF_SYNDIE = createImageFromResource("iconRefSyndie.png");
    public static final Image ICON_REF_FREENET = createImageFromResource("iconRefFreenet.png");
    
    public static final Image ICON_BROWSE_ADMINS = createImageFromResource("iconBrowseAdmins.png");
    public static final Image ICON_BROWSE_MANAGEABLE = createImageFromResource("iconBrowseManage.png");
    public static final Image ICON_BROWSE_POSTABLE = createImageFromResource("iconBrowsePost.png");
    public static final Image ICON_BROWSE_ARCHIVES = createImageFromResource("iconBrowseArchives.png");
    public static final Image ICON_BROWSE_REFS = createImageFromResource("iconBrowseRefs.png");
    
    public static final Image ICON_TAB_EDIT = resize(ImageUtil.ICON_WARNING, 16, 16, false);
    public static final Image ICON_TAB_TEXTUI = resize(ImageUtil.ICON_WARNING, 16, 16, false);
    public static final Image ICON_TAB_LOGS = resize(ImageUtil.ICON_WARNING, 16, 16, false);
    public static final Image ICON_TAB_SYNDICATE = resize(ImageUtil.ICON_WARNING, 16, 16, false);

    public static final Cursor CURSOR_WAIT = Display.getDefault().getSystemCursor(SWT.CURSOR_WAIT);
    
    public static void init() {
        _indisposableImages.add(ICON_ERROR);
        _indisposableImages.add(ICON_INFORMATION);
        _indisposableImages.add(ICON_QUESTION);
        _indisposableImages.add(ICON_WARNING);
        _indisposableImages.add(ICON_WORKING);
        
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
        
        _indisposableImages.add(ICON_MSG_TYPE_NORMAL);
        _indisposableImages.add(ICON_MSG_TYPE_META);
        _indisposableImages.add(ICON_MSG_TYPE_PRIVATE);
    
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
    
    public static Image createImage(byte data[]) {
        try {
            return new Image(Display.getDefault(), new ByteArrayInputStream(data));
        } catch (IllegalArgumentException iae) {
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
    
    public static Image createImageFromResource(String resource) {
        synchronized (_loadedResources) {
            Image img = (Image)_loadedResources.get(resource);
            if (img != null)
                return img;
            InputStream in = ImageUtil.class.getResourceAsStream(resource);
            if (in != null) {
                try {
                    img = new Image(Display.getDefault(), in);
                    _indisposableImages.add(img);
                    _loadedResources.put(resource, img);
                    return img;
                } catch (IllegalArgumentException iae) {
                    return null;
                }
            } else {
                return null;
            }
        }
    }
    
    public static Image createImageFromFile(String filename) {
        return new Image(Display.getDefault(), filename);
    }
    
    public static byte[] serializeImage(Image img) {
        ImageLoader loader = new ImageLoader();
        ByteArrayOutputStream outBuf = new ByteArrayOutputStream();
        loader.data = new ImageData[] { img.getImageData() };
        // foo. png not supported on early SWT (though newer swt revs do)
        loader.save(outBuf, SWT.IMAGE_PNG);
        //loader.save(outBuf, SWT.IMAGE_JPEG);
        return outBuf.toByteArray();
    }
    
    public static int getWidth(String text, Drawable target) {
        GC gc = new GC(target);
        FontMetrics fm = gc.getFontMetrics();
        int per = fm.getAverageCharWidth();
        gc.dispose();
        int rv = per * text.length();
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
                else if (BrowserTab.TYPE_SYNDICATE.equals(str))
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
        } else if (BrowserTab.TYPE_SYNDICATE.equals(uri.getType())) {
            return TYPE_ARCHIVE;
        } else if (BrowserTab.TYPE_MANAGE.equals(uri.getType())) {
            return TYPE_FORUM;
        } else if (BrowserTab.TYPE_META.equals(uri.getType())) {
            return TYPE_FORUM;
        } else {
            return TYPE_SYNDIE;
        }
    }
}
