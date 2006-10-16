package syndie.gui;

import java.io.File;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Drawable;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.custom.*;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.*;

/**
 * Base class for the syndie tabs
 */
public abstract class Component {
    protected void initComponents() {
        instantiateComponents();
        addActions();
        updateFields();
        updateResources();
        arrangeComponents();
    }
    
    /**
     * Create new widgets/etc, to be released on dispose()
     */
    protected abstract void instantiateComponents();
    /**
     * Attach any actions and listeners to the components
     */
    protected abstract void addActions();
    /**
     * Populate any fields with values
     */
    protected abstract void updateFields();
    
    /** Place the components in the layout */
    protected abstract void arrangeComponents();
    
    /** Set internationalized text and images */
    protected void updateResources() {}
    
    /**
     * Release all creted components
     */
    public void dispose() {}

    /** 
     * get a point whose x and y values equal the width and height of text 
     * drawn on the given surface 
     */
    protected final Point getCharSize(Drawable d) {
        GC gc = new GC(d);
        FontMetrics fm = gc.getFontMetrics();
        int width = fm.getAverageCharWidth();
        int height = fm.getHeight();
        gc.dispose();
        return new Point(width, height);
    }
}
