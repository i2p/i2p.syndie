package syndie.gui;

import java.util.Map;
import java.util.Properties;

public interface Translatable {
    /**
     * translate the current component
     */
    public void translate(TranslationRegistry registry);
}