package syndie.gui;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.net.URISyntaxException;
import syndie.data.SyndieURI;

/**
 * coordinate drag&drop of bookmarks.  a half-hearted attempt to make this use
 * a subclass of the swt.dnd.ByteArrayTransfer was able to make some progress,
 * but on winxp it seemed to do some funky things, so just fall back on using
 * strings for now
 */
class BookmarkDnD {
    public SyndieURI uri;
    public String name;
    public String desc;    
    
    public String toString() {
        StringBuffer buf = new StringBuffer();
        buf.append(uri.toString()).append('\n');
        buf.append(name).append('\n');
        buf.append(desc);
        return buf.toString();
    }
    public void fromString(String str) {
        try {
            BufferedReader r = new BufferedReader(new StringReader(str));
            String l1 = r.readLine();
            String l2 = r.readLine();
            String l3 = r.readLine();

            uri = new SyndieURI(l1);
            name = l2;
            desc = l3;
        } catch (URISyntaxException use) {
        } catch (IOException ioe) {
        }
    }
}
