package syndie.gui;

import java.util.Properties;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.SWT;

/**
 * define the colors/fonts/icons to use
 */
public class Theme {
    private Theme() {
        TAB_FONT = adjustHeight(SYSFONT, 0);
        MENU_FONT = adjustHeight(SYSFONT, 0);
        SHELL_FONT = adjustHeight(SYSFONT, 0);
        TREE_FONT = adjustHeight(SYSFONT, 0);
        TABLE_FONT = adjustHeight(SYSFONT, 0);
        BUTTON_FONT = adjustHeight(SYSFONT, 0);
        CONTENT_FONT = adjustHeight(SYSFONT, 0);
        LINK_FONT = adjustHeight(SYSFONT, 0, Boolean.TRUE, null);
        LOG_FONT = adjustHeight(SYSFONT, 0, null, null, "Courier");
        DEFAULT_FONT = adjustHeight(SYSFONT, 0);
    }
    
    private static final Font SYSFONT = Display.getDefault().getSystemFont();
    private static final Theme _default = new Theme();
    
    /** used as the tab headers */
    public Font TAB_FONT;
    /** used for the menubar and popup menus */
    public Font MENU_FONT;
    /** used for shell and dialog titles */
    public Font SHELL_FONT;
    /** used for tree items and columns */
    public Font TREE_FONT;
    /** used for table items and columns */
    public Font TABLE_FONT;
    /** used for text buttons */
    public Font BUTTON_FONT;
    /** used for text links */
    public Font LINK_FONT;
    /** used for rendering messages */
    public Font CONTENT_FONT;
    /** used for log messages */
    public Font LOG_FONT;
    /** used for anything else */
    public Font DEFAULT_FONT;
    
    public static Theme getDefault() { return _default; }
    public static Theme getTheme(Properties props) {
        Theme rv = new Theme();
        rv.load(props);
        return rv;
    }
    
    public String validate() { 
        if (true) return null;
        
        StringBuffer rv = new StringBuffer();
        if (TAB_FONT == null)
            rv.append("tabFont is null ");
        else if (TAB_FONT.isDisposed())
            rv.append("tabFont is disposed ");
        
        if (MENU_FONT == null)
            rv.append("menuFont is null ");
        else if (MENU_FONT.isDisposed())
            rv.append("menuFont is disposed ");
        
        if (SHELL_FONT == null)
            rv.append("shellFont is null ");
        else if (SHELL_FONT.isDisposed())
            rv.append("shellFont is disposed ");
        
        if (TREE_FONT == null)
            rv.append("treeFont is null ");
        else if (TREE_FONT.isDisposed())
            rv.append("treeFont is disposed ");
        
        if (TABLE_FONT == null)
            rv.append("tableFont is null ");
        else if (TABLE_FONT.isDisposed())
            rv.append("tableFont is disposed ");
        
        if (BUTTON_FONT == null)
            rv.append("buttonFont is null ");
        else if (BUTTON_FONT.isDisposed())
            rv.append("buttonFont is disposed ");
        
        if (LINK_FONT == null)
            rv.append("linkFont is null ");
        else if (LINK_FONT.isDisposed())
            rv.append("linkFont is disposed ");
        
        if (CONTENT_FONT == null)
            rv.append("contentFont is null ");
        else if (CONTENT_FONT.isDisposed())
            rv.append("contentFont is disposed ");
        
        if (DEFAULT_FONT == null)
            rv.append("defaultFont is null ");
        else if (DEFAULT_FONT.isDisposed())
            rv.append("defaultFont is disposed ");
        return (rv.length() > 0 ? rv.toString() : null);
    }
    
    public void increaseFont() {
        TAB_FONT = increaseFont(TAB_FONT);
        MENU_FONT = increaseFont(MENU_FONT);
        SHELL_FONT = increaseFont(SHELL_FONT);
        TREE_FONT = increaseFont(TREE_FONT);
        TABLE_FONT = increaseFont(TABLE_FONT);
        BUTTON_FONT = increaseFont(BUTTON_FONT);
        CONTENT_FONT = increaseFont(CONTENT_FONT);
        LOG_FONT = increaseFont(LOG_FONT);
        LINK_FONT = increaseFont(LINK_FONT);
        DEFAULT_FONT = increaseFont(DEFAULT_FONT);
    }
    public void decreaseFont() {
        TAB_FONT = decreaseFont(TAB_FONT);
        MENU_FONT = decreaseFont(MENU_FONT);
        SHELL_FONT = decreaseFont(SHELL_FONT);
        TREE_FONT = decreaseFont(TREE_FONT);
        TABLE_FONT = decreaseFont(TABLE_FONT);
        BUTTON_FONT = decreaseFont(BUTTON_FONT);
        CONTENT_FONT = decreaseFont(CONTENT_FONT);
        LOG_FONT = decreaseFont(LOG_FONT);
        LINK_FONT = decreaseFont(LINK_FONT);
        DEFAULT_FONT = decreaseFont(DEFAULT_FONT);
    }
    
    public void store(Properties props) {
        Font sys = Display.getDefault().getSystemFont();
        String face = getFace(sys);
        int size = getSize(sys);
        store(props, TAB_FONT, face, size, "theme.tabfont");
        store(props, MENU_FONT, face, size, "theme.menufont");
        store(props, SHELL_FONT, face, size, "theme.shellfont");
        store(props, TREE_FONT, face, size, "theme.treefont");
        store(props, TABLE_FONT, face, size, "theme.tablefont");
        store(props, BUTTON_FONT, face, size, "theme.buttonfont");
        store(props, CONTENT_FONT, face, size, "theme.contentfont");
        store(props, LOG_FONT, face, size, "theme.logfont");
        store(props, LINK_FONT, face, size, "theme.linkfont");
        store(props, DEFAULT_FONT, face, size, "theme.defaultfont");
    }
    private void store(Properties props, Font font, String defaultFace, int baselineSize, String prefPrefix) {
        if (props == null) return;
        String face = getFace(font);
        int size = getSize(font);
        if (defaultFace.equals(face))
            face = null;
        int sizeMod = size - baselineSize;
        if (face != null)
            props.setProperty(prefPrefix + ".face", face);
        else
            props.remove(prefPrefix + ".face");
        if (sizeMod != 0)
            props.setProperty(prefPrefix + ".size", ""+sizeMod);
        else
            props.remove(prefPrefix + ".size");
        props.setProperty(prefPrefix + ".bold", ""+isBold(font));
        props.setProperty(prefPrefix + ".italic", ""+isItalic(font));
    }
    private void load(Properties props) {
        TAB_FONT = load(props, TAB_FONT, "theme.tabfont");
        MENU_FONT = load(props, MENU_FONT, "theme.menufont");
        SHELL_FONT = load(props, SHELL_FONT, "theme.shellfont");
        TREE_FONT = load(props, TREE_FONT, "theme.treefont");
        TABLE_FONT = load(props, TABLE_FONT, "theme.tablefont");
        BUTTON_FONT = load(props, BUTTON_FONT, "theme.buttonfont");
        CONTENT_FONT = load(props, CONTENT_FONT, "theme.contentfont");
        LOG_FONT = load(props, LOG_FONT, "theme.logfont");
        LINK_FONT = load(props, LINK_FONT, "theme.linkfont");
        DEFAULT_FONT = load(props, DEFAULT_FONT, "theme.defaultfont");
    }
    private Font load(Properties props, Font old, String prefPrefix) {
        try {
            if (props == null) return old;

            String defaultFace = getFace(old);
            int baselineSize = getSize(old);

            String face = props.getProperty(prefPrefix + ".face");
            String szModStr = props.getProperty(prefPrefix + ".size");
            int szMod = baselineSize;
            if (szModStr != null) {
                try {
                    szMod = Integer.parseInt(szModStr);
                } catch (NumberFormatException nfe) {}
            }

            Boolean bold = new Boolean(props.getProperty(prefPrefix + ".bold"));
            Boolean italic = new Boolean(props.getProperty(prefPrefix + ".italic"));

            return adjustHeight(old, szMod, bold, italic, face);
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("Error loading " + prefPrefix + ", props: " + props);
            return SYSFONT;
        }
    }
    
    private Font increaseFont(Font old) { return adjustHeight(old, 2); }
    private Font decreaseFont(Font old) { return adjustHeight(old, -2); }
    private Font adjustHeight(Font old, int mod) { return adjustHeight(old, mod, null, null); }
    private Font adjustHeight(Font old, int mod, Boolean bold, Boolean italic) {
        return adjustHeight(old, mod, bold, italic, null);
    }
    private Font adjustHeight(Font old, int mod, Boolean bold, Boolean italic, String newFace) {
        FontData oldData[] = old.getFontData();
        if ( (old != SYSFONT) && (!old.equals(SYSFONT)) ) {
            //System.out.println("disposing: " + old);
            old.dispose();
        }
        FontData newData[] = new FontData[oldData.length];
        for (int i = 0; i < newData.length; i++) {
            newData[i] = new FontData(oldData[i].toString());
            int height = newData[i].getHeight() + mod;
            if (height < 6)
                height = 6;
            newData[i].setHeight(height);
            int style = newData[i].getStyle();
            if (bold != null)
                if (bold.booleanValue())
                    style |= SWT.BOLD;
                else
                    style &= ~SWT.BOLD;
            if (italic != null)
                if (italic.booleanValue())
                    style |= SWT.ITALIC;
                else
                    style &= ~SWT.ITALIC;
            
            newData[i].setStyle(style);
            if (newFace != null)
                newData[i].setName(newFace);
        }
        Font rv = new Font(Display.getDefault(), newData);
        //System.out.println("creating: " + rv);
        return rv;
    }
    
    public static String getFace(Font font) {
        if ( (font == null) || (font.isDisposed()) )
            return getFace(Display.getDefault().getSystemFont());
        FontData fd[] = font.getFontData();
        for (int i = 0; i < fd.length; i++) {
            String name = fd[i].getName();
            if (name != null)
                return name;
        }
        return null;
    }
    public static int getSize(Font font) {
        if ( (font == null) || (font.isDisposed()) )
            return getSize(Display.getDefault().getSystemFont());
        FontData fd[] = font.getFontData();
        for (int i = 0; i < fd.length; i++) {
            int height = fd[i].getHeight();
            if (height > 0)
                return height;
        }
        return -1;
    }
    public static boolean isBold(Font font) {
        if ( (font == null) || (font.isDisposed()) )
            return false;
        FontData fd[] = font.getFontData();
        int style = 0;
        for (int i = 0; i < fd.length; i++)
            style |= fd[i].getStyle();
        return (style & SWT.BOLD) != 0;
    }
    public static boolean isItalic(Font font) {
        if ( (font == null) || (font.isDisposed()) )
            return false;
        FontData fd[] = font.getFontData();
        int style = 0;
        for (int i = 0; i < fd.length; i++)
            style |= fd[i].getStyle();
        return (style & SWT.ITALIC) != 0;
    }
}
