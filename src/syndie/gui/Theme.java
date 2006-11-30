package syndie.gui;

import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Display;

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
    /** used for anything else */
    public Font DEFAULT_FONT;
    
    public static Theme getDefault() { return _default; }
    
    public void increaseFont() {
        TAB_FONT = increaseFont(TAB_FONT);
        MENU_FONT = increaseFont(MENU_FONT);
        SHELL_FONT = increaseFont(SHELL_FONT);
        TREE_FONT = increaseFont(TREE_FONT);
        TABLE_FONT = increaseFont(TABLE_FONT);
        BUTTON_FONT = increaseFont(BUTTON_FONT);
        DEFAULT_FONT = increaseFont(DEFAULT_FONT);
    }
    public void decreaseFont() {
        TAB_FONT = decreaseFont(TAB_FONT);
        MENU_FONT = decreaseFont(MENU_FONT);
        SHELL_FONT = decreaseFont(SHELL_FONT);
        TREE_FONT = decreaseFont(TREE_FONT);
        TABLE_FONT = decreaseFont(TABLE_FONT);
        BUTTON_FONT = decreaseFont(BUTTON_FONT);
        DEFAULT_FONT = decreaseFont(DEFAULT_FONT);
    }
    
    private Font increaseFont(Font old) { return adjustHeight(old, 2); }
    private Font decreaseFont(Font old) { return adjustHeight(old, -2); }
    private Font adjustHeight(Font old, int mod) {
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
        }
        Font rv = new Font(Display.getDefault(), newData);
        //System.out.println("creating: " + rv);
        return rv;
    }
}
