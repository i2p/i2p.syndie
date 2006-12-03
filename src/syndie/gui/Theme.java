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
        TAB_FONT = adjustHeight("inittab", SYSFONT, 0);
        MENU_FONT = adjustHeight("initmenu", SYSFONT, 0);
        SHELL_FONT = adjustHeight("initshell", SYSFONT, 0);
        TREE_FONT = adjustHeight("inittree", SYSFONT, 0);
        TABLE_FONT = adjustHeight("inittable", SYSFONT, 0);
        BUTTON_FONT = adjustHeight("initbutton", SYSFONT, 0);
        CONTENT_FONT = adjustHeight("initcontent", SYSFONT, 0);
        LINK_FONT = adjustHeight("initlink", SYSFONT, 0, Boolean.TRUE, null);
        LOG_FONT = adjustHeight("initlog", SYSFONT, 0, null, null, "Courier");
        MSG_OLD_FONT = adjustHeight("initmsgold", SYSFONT, 0, null, Boolean.TRUE);
        MSG_NEW_READ_FONT = adjustHeight("initmsgnewread", SYSFONT, 0, null, null);
        MSG_NEW_UNREAD_FONT = adjustHeight("initmsgnewunread", SYSFONT, 0, Boolean.TRUE, null);
        DEFAULT_FONT = adjustHeight("initdefault", SYSFONT, 0);
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
    /** messages in a message tree that were marked read in bulk */
    public Font MSG_OLD_FONT;
    /** messages in a message tree that were read after the last mark read in bulk */
    public Font MSG_NEW_READ_FONT;
    /** messages in a message tree that have not yet been read */
    public Font MSG_NEW_UNREAD_FONT;
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
        TAB_FONT = increaseFont("tab", TAB_FONT);
        MENU_FONT = increaseFont("menu", MENU_FONT);
        SHELL_FONT = increaseFont("shell", SHELL_FONT);
        TREE_FONT = increaseFont("tree", TREE_FONT);
        TABLE_FONT = increaseFont("table", TABLE_FONT);
        BUTTON_FONT = increaseFont("button", BUTTON_FONT);
        CONTENT_FONT = increaseFont("content", CONTENT_FONT);
        LOG_FONT = increaseFont("log", LOG_FONT);
        LINK_FONT = increaseFont("link", LINK_FONT);
        DEFAULT_FONT = increaseFont("default", DEFAULT_FONT);
        MSG_OLD_FONT = increaseFont("msgold", MSG_OLD_FONT);
        MSG_NEW_READ_FONT = increaseFont("msgnewread", MSG_NEW_READ_FONT);
        MSG_NEW_UNREAD_FONT = increaseFont("msgnewunread", MSG_NEW_UNREAD_FONT);
    }
    public void decreaseFont() {
        TAB_FONT = decreaseFont("tab", TAB_FONT);
        MENU_FONT = decreaseFont("menu", MENU_FONT);
        SHELL_FONT = decreaseFont("shell", SHELL_FONT);
        TREE_FONT = decreaseFont("tree", TREE_FONT);
        TABLE_FONT = decreaseFont("table", TABLE_FONT);
        BUTTON_FONT = decreaseFont("button", BUTTON_FONT);
        CONTENT_FONT = decreaseFont("content", CONTENT_FONT);
        LOG_FONT = decreaseFont("log", LOG_FONT);
        LINK_FONT = decreaseFont("link", LINK_FONT);
        DEFAULT_FONT = decreaseFont("default", DEFAULT_FONT);
        MSG_OLD_FONT = decreaseFont("msgold", MSG_OLD_FONT);
        MSG_NEW_READ_FONT = decreaseFont("msgnewread", MSG_NEW_READ_FONT);
        MSG_NEW_UNREAD_FONT = decreaseFont("msgnewunread", MSG_NEW_UNREAD_FONT);
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
        store(props, MSG_OLD_FONT, face, size, "theme.msgoldfont");
        store(props, MSG_NEW_READ_FONT, face, size, "theme.msgnewreadfont");
        store(props, MSG_NEW_UNREAD_FONT, face, size, "theme.msgnewunreadfont");
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
        MSG_OLD_FONT = load(props, MSG_OLD_FONT, "theme.msgoldfont");
        MSG_NEW_READ_FONT = load(props, MSG_NEW_READ_FONT, "theme.msgnewreadfont");
        MSG_NEW_UNREAD_FONT = load(props, MSG_NEW_UNREAD_FONT, "theme.msgnewunreadfont");
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

            String str = props.getProperty(prefPrefix + ".bold");
            Boolean bold = str != null ? new Boolean(str) : null;
            str = props.getProperty(prefPrefix + ".italic");
            Boolean italic = str != null ? new Boolean(str) : null;

            return adjustHeight(prefPrefix, old, szMod, bold, italic, face);
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("Error loading " + prefPrefix + ", props: " + props);
            return SYSFONT;
        }
    }
    
    private Font increaseFont(String src, Font old) { return adjustHeight(src, old, 2); }
    private Font decreaseFont(String src, Font old) { return adjustHeight(src, old, -2); }
    private Font adjustHeight(String src, Font old, int mod) { return adjustHeight(src, old, mod, null, null); }
    private Font adjustHeight(String src, Font old, int mod, Boolean bold, Boolean italic) {
        return adjustHeight(src, old, mod, bold, italic, null);
    }
    private Font adjustHeight(String src, Font old, int mod, Boolean bold, Boolean italic, String newFace) {
        FontData oldData[] = old.getFontData();
        if ( (old != SYSFONT) && (!old.equals(SYSFONT)) ) {
            //System.out.println("disposing: " + old);
            old.dispose();
        }
        FontData newData[] = new FontData[oldData.length];
        for (int i = 0; i < newData.length; i++) {
            newData[i] = new FontData(oldData[i].toString());
            int height = oldData[i].getHeight() + mod;
            if (height < 6)
                height = 6;
            newData[i].setHeight(height);
            int style = oldData[i].getStyle();
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
        //System.out.println("creating [" + src + "]: " + rv + " (bold? " + isBold(rv) + " italic? " + isItalic(rv) + " face: " + getFace(rv) + " size: " + getSize(rv) + ")");
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
