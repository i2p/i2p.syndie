package syndie.gui;

import syndie.db.DBClient;
import syndie.db.UI;

public class BaseComponent {
    protected DBClient _client;
    protected UI _ui;
    protected ThemeRegistry _themeRegistry;
    protected TranslationRegistry _translationRegistry;
    
    public BaseComponent(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans) {
        _client = client;
        _ui = ui;
        _themeRegistry = themes;
        _translationRegistry = trans;
    }
}
