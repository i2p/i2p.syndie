package syndie.gui;

import java.net.URISyntaxException;
import org.eclipse.swt.widgets.Display;
import syndie.data.SyndieURI;
import syndie.db.*;

/**
 *
 */
public class ViewMessageCommand implements CLI.Command {
    static MessageTab messageTab;
    static Display display;
    public ViewMessageCommand() {}
    public DBClient runCommand(Opts opts, UI ui, DBClient client) {
        String uri = opts.getOptValue("uri");
        if (uri != null) {
            try {
                final SyndieURI msg = new SyndieURI(uri);
                display.asyncExec(new Runnable() { public void run() { messageTab.viewMessage(msg); } });
            } catch (URISyntaxException use) {
                final SyndieURI msg = SyndieURI.createSearch(uri);
                display.asyncExec(new Runnable() { public void run() { messageTab.viewMessage(msg); } });
            }
        }
        
        return client;
    }
}
