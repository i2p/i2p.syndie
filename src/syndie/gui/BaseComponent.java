package syndie.gui;

import syndie.db.DBClient;
import syndie.db.UI;

public class BaseComponent {
    protected final DBClient _client;
    protected final UI _ui;
    protected final ThemeRegistry _themeRegistry;
    protected final TranslationRegistry _translationRegistry;
    
    public BaseComponent(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans) {
        _client = client;
        _ui = ui;
        _themeRegistry = themes;
        _translationRegistry = trans;
    }

    /**
     *  Tagging for static initializers. Does not translate!
     *  @return s
     *  @since 1.102b-5
     */
    protected static final String _x(String s) {
        return s;
    }
}
