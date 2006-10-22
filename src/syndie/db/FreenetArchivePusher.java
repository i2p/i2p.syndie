package syndie.db;

import java.io.*;
import java.net.Socket;
import java.util.*;
import net.i2p.data.DataHelper;
import net.i2p.util.SimpleTimer;
import syndie.data.SyndieURI;

/**
 * this class posts the archive under a freenet 0.7 SSK, creating a new 
 * one if necessary.  it uses a persistent put so that syndie doesn't have to
 * wait around for freenet to fully insert the data, and it sends the data to
 * freenet directly so that freenet can be on a remote machine or otherwise
 * unable to read the archive's files, and to make sure a slow insert process
 * doesn't get confused by files later updated by syndie.
 */
public class FreenetArchivePusher {
    private UI _ui;
    private int _fcpPort;
    private String _fcpHost;
    private String _privateSSK;
    private String _publicSSK;
    
    public FreenetArchivePusher(UI ui, String fcpHost, int fcpPort) {
        _ui = ui;
        _fcpHost = fcpHost;
        _fcpPort = fcpPort;
    }
    
    public String getPublicSSK() { return _publicSSK; }
    public String getPrivateSSK() { return _privateSSK; }
    public void setPublicSSK(String uri) { _publicSSK = uri; }
    public void setPrivateSSK(String uri) { _privateSSK = uri; }
    
    public void generateSSK() {
        try {
            Socket s = new Socket(_fcpHost, _fcpPort);
            OutputStream out = s.getOutputStream();
            long msgId = System.currentTimeMillis();
            out.write(DataHelper.getUTF8("ClientHello\r\n" +
                       "Name=syndie" + msgId + "\r\n" +
                       "ExpectedVersion=2.0\r\n" +
                       "Identifier=" + msgId + "\r\n" +
                       "EndMessage\r\n" +
                       "GenerateSSK\r\n" +
                       "Identifier=" + (msgId+1) + "\r\n" +
                       "EndMessage\r\n"));
            Map rv = readResults(s.getInputStream());
            if (rv == null) {
                _ui.errorMessage("Error communicating with the Freenet server");
                //_ui.commandComplete(-1, null);
            } else {
                _publicSSK = (String)rv.get("RequestURI");
                _privateSSK = (String)rv.get("InsertURI");
                if ( (_publicSSK != null) && (_privateSSK != null) ) {
                    _ui.statusMessage("New Freenet keypair generated");
                    _ui.statusMessage("public URI: " + _publicSSK);
                    _ui.statusMessage("private URI: " + _privateSSK);
                    _ui.statusMessage("(both will be stored in the current nym's profile)");
                    //_ui.commandComplete(0, null);
                } else {
                    _ui.errorMessage("Unable to generate a Freenet keypair");
                    _ui.debugMessage("FCP response: " + rv);
                    //_ui.commandComplete(-1, null);
                }
            }
            s.close();
        } catch (IOException ioe) {
            _ui.errorMessage("Error generating a new Freenet keypair", ioe);
            //_ui.commandComplete(-1, null);
        }
    }
    
    public void putArchive(File archiveDir) {
        List files = listArchive(archiveDir);
        try {
            Socket s = new Socket(_fcpHost, _fcpPort);
            OutputStream out = s.getOutputStream();
            long msgId = System.currentTimeMillis();
            out.write(DataHelper.getUTF8("ClientHello\r\n" +
                       "Name=syndie" + msgId + "\r\n" +
                       "ExpectedVersion=2.0\r\n" +
                       "Identifier=" + msgId + "\r\n" +
                       "EndMessage\r\n"));
            
            String target = getTarget(_privateSSK);
            
            _ui.debugMessage("Posting to " + target);
            
            out.write(("ClientPutComplexDir\r\n" +
                       "Identifier=" + (msgId+1) + "\r\n" +
                       "URI=" + target + "\r\n" +
                       "Verbosity=1023\r\n" + // we don't care about anyting
                       "Global=true\r\n" + // let it be seen on fproxy's /queue/
                       "MaxRetries=10\r\n" +
                       "PriorityClass=3\r\n" + // 3 is lower than interactive
                       "Persistence=reboot\r\n" // runs until success or the freenet instance restarts
                       ).getBytes());
            
            // dont use UploadFrom=disk, because that breaks if the fcpHost != localhost,
            // or if the freenet instance doesn't have read permissions on the archive
            for (int i = 0; i < files.size(); i++) {
                File f = (File)files.get(i);
                String path = getArchivePath(f, archiveDir);
                _ui.debugMessage("including path: " + path);
                out.write(("Files." + i + ".Name=" + path + "\r\n" +
                           "Files." + i + ".UploadFrom=direct\r\n" +
                           "Files." + i + ".Metadata.ContentType=" + getContentType(f) + "\r\n" +
                           "Files." + i + ".DataLength=" + f.length() + "\r\n").getBytes());
            }
            out.write(DataHelper.getUTF8("EndMessage\r\n"));
            int bytes = 0;
            for (int i = 0; i < files.size(); i++) {
                File f = (File)files.get(i);
                byte buf[] = new byte[4096];
                FileInputStream fin = new FileInputStream(f);
                int read = -1;
                while ( (read = fin.read(buf)) != -1) {
                    out.write(buf, 0, read);
                    bytes += read;
                }
                fin.close();
            }
            
            _ui.debugMessage("FCP message written, now reading the response");
            
            // since we are doing a persistent put, this does not block waiting
            // for the actual full posting, merely schedules the posting through
            // freenet
            Map rv = readResults(s.getInputStream());
            if (rv == null) {
                _ui.errorMessage("Error communicating with the Freenet server");
                _ui.commandComplete(-1, null);
            } else {
                String code = (String)rv.get("Code");
                if ( (code != null) && !("0".equals(code))) {
                    _ui.errorMessage("Error posting the archive");
                    _ui.debugMessage("FCP response: " + rv);
                    _ui.commandComplete(-1, null);
                } else {
                    _ui.statusMessage("Freenet archive publishing queued on the freenet server");
                    _ui.statusMessage("Total size queued: " + (bytes+1023)/1024 + "KBytes");
                    _ui.statusMessage("It may take a long time for the archive to be visible for others");
                    _ui.statusMessage("Please see the Freenet fproxy for status information.");
                    if (_publicSSK != null) {
                        _ui.statusMessage("Archive URL: ");
                        _ui.statusMessage(getTarget(_publicSSK));
                        _ui.statusMessage(" aka ");
                        _ui.statusMessage(SyndieURI.createArchive(getTarget(_publicSSK), null).toString());
                    }
                    _ui.commandComplete(0, null);
                }
            }
            s.close();
        } catch (IOException ioe) {
            _ui.errorMessage("Error posting the archive to Freenet", ioe);
            _ui.commandComplete(-1, null);
        }
    }
    
    /**
     * recursive list of files in the archive, excluding directories
     */
    private List listArchive(File root) {
        List rv = new ArrayList();
        listArchive(rv, root);
        _ui.debugMessage("Archive contains: " + rv);
        return rv;
    }
    
    /** depth first search - fills in an entire channel, then goes to the next, etc */
    private void listArchive(List rv, File root) {
        File files[] = root.listFiles();
        for (int i = 0; i < files.length; i++) {
            if (files[i].isDirectory()) {
                listArchive(rv, files[i]);
            } else if (files[i].getName().endsWith(".syndie")) {
                rv.add(files[i]);
            } else if (files[i].getName().endsWith(".dat")) {
                rv.add(files[i]);
            } else if (files[i].getName().endsWith(".html")) {
                rv.add(files[i]);
            }
        }
    }

    private String getTarget(String privateSSK) {
        int index = privateSSK.indexOf("SSK@");
        String key = privateSSK.substring(index+4);
        while (key.endsWith("/"))
            key = key.substring(0, key.length()-1);
        return "USK@" + key + "/archive/0/";
    }
    
    private String getArchivePath(File f, File archiveDir) {
        StringBuffer buf = new StringBuffer();
        while (f.compareTo(archiveDir) != 0) {
            buf.insert(0, f.getName());
            buf.insert(0, "/");
            if (f.compareTo(f.getParentFile()) == 0)
                break; // root of the filesystem... file is not in the archive, or the archive is the root
            f = f.getParentFile();
        }
        while ( (buf.length() > 0) && (buf.charAt(0) == '/') )
            buf.deleteCharAt(0);
        return buf.toString();
    }
    
    private String getContentType(File f) {
        if (f.getName().endsWith(".syndie"))
            return "application/x-syndie";
        else if (f.getName().endsWith(".dat"))
            return "application/x-syndie-index";
        else if (f.getName().endsWith(".html"))
            return "text/html";
        else
            return "application/octet-stream";
    }
    
    private Map readResults(InputStream in) throws IOException {
        // read the result map, ignoring the NodeHello message
        BufferedReader bin = new BufferedReader(new InputStreamReader(in, "UTF-8"));
        String line = null;
        Map rv = new HashMap();
        String cmd = null;
        while ( (line = bin.readLine()) != null) {
            _ui.debugMessage("Line read: " + line);
            if (cmd == null) {
                cmd = line;
            } else if (line.startsWith("EndMessage")) {
                if ("NodeHello".equals(cmd)) {
                    // ignore this message
                    rv.clear();
                    cmd = null;
                } else {
                    // return this message
                    return rv;
                }
            } else {
                int split = line.indexOf('=');
                if (split < 0) throw new IOException("Invalid format of a line [" + line + "]");
                String name = line.substring(0, split);
                String val = line.substring(split+1);
                rv.put(name, val);
            }
        }
        return rv;
    }
}
