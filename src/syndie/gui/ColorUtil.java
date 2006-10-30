package syndie.gui;

import java.util.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;

public class ColorUtil {
    private static final Map _colorNameToRGB = new HashMap();
    private static final Map _colorNameToSystem = new HashMap();
    private static final Map _colorRGBToSystem = new HashMap();
    private static final ArrayList _systemColorNames = new ArrayList();
    private static void buildColorNameToSystemColor() {
        Device dev = Display.getDefault();
        _colorNameToSystem.put("darkblue", dev.getSystemColor(SWT.COLOR_DARK_BLUE));
        _colorNameToSystem.put("darkcyan", dev.getSystemColor(SWT.COLOR_DARK_CYAN));
        _colorNameToSystem.put("darkgray", dev.getSystemColor(SWT.COLOR_DARK_GRAY));
        _colorNameToSystem.put("darkgreen", dev.getSystemColor(SWT.COLOR_DARK_GREEN));
        _colorNameToSystem.put("darkmagenta", dev.getSystemColor(SWT.COLOR_DARK_MAGENTA));
        _colorNameToSystem.put("darkred", dev.getSystemColor(SWT.COLOR_DARK_RED));
        _colorNameToSystem.put("darkyellow", dev.getSystemColor(SWT.COLOR_DARK_YELLOW));
        _colorNameToSystem.put("blue", dev.getSystemColor(SWT.COLOR_BLUE));
        _colorNameToSystem.put("cyan", dev.getSystemColor(SWT.COLOR_CYAN));
        _colorNameToSystem.put("gray", dev.getSystemColor(SWT.COLOR_GRAY));
        _colorNameToSystem.put("green", dev.getSystemColor(SWT.COLOR_GREEN));
        _colorNameToSystem.put("magenta", dev.getSystemColor(SWT.COLOR_MAGENTA));
        _colorNameToSystem.put("red", dev.getSystemColor(SWT.COLOR_RED));
        _colorNameToSystem.put("yellow", dev.getSystemColor(SWT.COLOR_YELLOW));
        _colorNameToSystem.put("white", dev.getSystemColor(SWT.COLOR_WHITE));
        _colorNameToSystem.put("black", dev.getSystemColor(SWT.COLOR_BLACK));
        _systemColorNames.addAll(new TreeSet(_colorNameToSystem.keySet()));
        for (Iterator iter = _colorNameToSystem.keySet().iterator(); iter.hasNext(); ) {
            String name = (String)iter.next();
            Color color = (Color)_colorNameToSystem.get(name);
            String rgb = "#" + toHex(color.getRed()) + toHex(color.getGreen()) + toHex(color.getBlue());
            _colorNameToRGB.put(name, rgb);
            _colorRGBToSystem.put(rgb, color);
        }
    }
    private static String toHex(int color) {
        String rv = Integer.toHexString(color);
        if (color <= 0x0F)
            return "0" + rv;
        else
            return rv;
    }
    private static Map _systemColorSwatches = new HashMap();
    private static void buildSystemColorSwatches() {
        Device dev = Display.getDefault();
        for (Iterator iter = _colorNameToSystem.values().iterator(); iter.hasNext(); ) {
            Color color = (Color)iter.next();
            Image img = new Image(dev, 16, 16);
            GC gc = new GC(img);
            gc.setForeground(color);
            gc.setBackground(color);
            //gc.drawRectangle(0, 0, 16, 16);
            gc.fillRectangle(0, 0, 16, 16);
            gc.dispose();
            _systemColorSwatches.put(color, img);
        }
    }
    static {
        buildColorNameToSystemColor();
        buildSystemColorSwatches();
    }
    
    /** alphabetically ordered list of system colors */
    public static ArrayList getSystemColorNames() { return _systemColorNames; }
    
    /**
     * get the given color, pulling it from the set of system colors or the cache,
     * if possible.
     */
    public static Color getColor(String color, Map cache) {
        Color rv = null;
        if (color != null) {
            color = color.trim();
            String rgb = (String)_colorNameToRGB.get(color);
            if (rgb != null)
                color = rgb;
            //System.out.println("color: " + color);
            if (color.startsWith("#") && (color.length() == 7)) {
                Color cached = (Color)_colorRGBToSystem.get(color);
                if ( (cache != null) && (cached == null) )
                    cached = (Color)cache.get(color);
                if (cached == null) {
                    try {
                        int r = Integer.parseInt(color.substring(1, 3), 16);
                        int g = Integer.parseInt(color.substring(3, 5), 16);
                        int b = Integer.parseInt(color.substring(5, 7), 16);
                        cached = new Color(Display.getDefault(), r, g, b);
                        if (cache != null)
                            cache.put(color, cached);
                        //System.out.println("rgb: " + cached + " [" + r + "/" + g + "/" + b + "]");
                    } catch (NumberFormatException nfe) {
                        // invalid rgb
                        System.out.println("invalid rgb");
                        nfe.printStackTrace();
                    }
                }
                if (cached != null)
                    rv = cached;
            } else {
                // invalid rgb
                //System.out.println("rgb is not valid [" + color + "]");
            }
        }
        return rv;
    }

    /**
     * 16x16 pixel image of the given system color (do not dispose!)
     */
    public static Image getSystemColorSwatch(Color color) { return (Image)_systemColorSwatches.get(color); }
}
