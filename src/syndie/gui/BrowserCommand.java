package syndie.gui;

import syndie.db.CLI;
import syndie.db.DBClient;
import syndie.db.Opts;
import syndie.db.UI;
import syndie.util.Timer;

/**
 *  Unused, unreferenced, to be deleted
 */
class BrowserCommand implements CLI.Command {
    public BrowserCommand() {}
    public DBClient runCommand(Opts opts, UI ui, DBClient client) {
        Browser browser = new Browser(client);
        browser.startup(new Timer("BrowserCommand", ui));
        return client;
    }
}
