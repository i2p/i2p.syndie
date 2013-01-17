package syndie.gui;

import syndie.db.DBClient;
import syndie.db.UI;

/**
 *  Base for almost all high-level gui elements
 */
public abstract class BaseComponent {
    protected final DBClient _client;
    protected final UI _ui;
    protected final ThemeRegistry _themeRegistry;
    protected final TranslationRegistry _translationRegistry;
    
    protected BaseComponent(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans) {
        _client = client;
        _ui = ui;
        _themeRegistry = themes;
        _translationRegistry = trans;
    }

    /**
     *  Helper
     *  @since 1.102b-9
     */
    protected Theme getTheme() { return _themeRegistry.getTheme(); }

    /**
     *  Helper
     *  @since 1.102b-9
     */
    protected String getText(String text) { return _translationRegistry.getText(text); }

    /**
     *  Following all moved from BrowserTab for ease of use
     *  @since 1.102b-9
     */
    protected void debug(String msg) { _ui.debugMessage(msg); }
    protected void debug(String msg, Exception e) { _ui.debugMessage(msg, e); }
    protected void status(String msg) { _ui.statusMessage(msg); }
    protected void error(String msg) { _ui.errorMessage(msg); }
    protected void error(String msg, Exception e) { _ui.errorMessage(msg, e); }
    
    /**
     *  Tagging for static initializers. Does not translate!
     *  @return s
     *  @since 1.102b-5
     */
    protected static final String _x(String s) {
        return s;
    }
}
