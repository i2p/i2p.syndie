package syndie.gui;

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
    /** used for log messages */
    public Font LOG_FONT;
    /** used for anything else */
    public Font DEFAULT_FONT;
    
    public static Theme getDefault() { return _default; }
    
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
        LOG_FONT = decreaseFont(LOG_FONT);
        LINK_FONT = decreaseFont(LINK_FONT);
        DEFAULT_FONT = decreaseFont(DEFAULT_FONT);
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
}
