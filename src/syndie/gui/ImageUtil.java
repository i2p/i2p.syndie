package syndie.gui;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.Map;
import java.util.HashMap;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;

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
    
    public static final Image ICON_TAB_EDIT = resize(ImageUtil.ICON_WARNING, 16, 16, false);
    public static final Image ICON_TAB_TEXTUI = resize(ImageUtil.ICON_WARNING, 16, 16, false);
    public static final Image ICON_TAB_LOGS = resize(ImageUtil.ICON_WARNING, 16, 16, false);

    public static final Cursor CURSOR_WAIT = Display.getDefault().getSystemCursor(SWT.CURSOR_WAIT);
    
    public static void init() {
        _indisposableImages.add(ICON_ERROR);
        _indisposableImages.add(ICON_INFORMATION);
        _indisposableImages.add(ICON_QUESTION);
        _indisposableImages.add(ICON_WARNING);
        _indisposableImages.add(ICON_WORKING);
        
        _indisposableImages.add(ICON_LINK_END);
        _indisposableImages.add(ICON_IMAGE_UNKNOWN);
        
        _indisposableImages.add(ICON_TAB_EDIT);
        _indisposableImages.add(ICON_TAB_TEXTUI);
        _indisposableImages.add(ICON_TAB_LOGS);
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
}
