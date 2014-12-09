package syndie.db;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.Socket;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import net.i2p.data.DataFormatException;
import net.i2p.data.DataHelper;
import net.i2p.util.SimpleTimer2;

import syndie.Constants;
import syndie.data.SyndieURI;

/**
 *  Pusher threads
 */
public class SyncOutboundPusher {
    private final SyncManager _manager;
    private static final Map _runnerToArchive = new HashMap();
    private volatile boolean _die;
    
    /** pushes are bundled so we don't need as many threads as for pulls */
    private static final int THREADS = 2;
    
    public SyncOutboundPusher(SyncManager mgr) {
        _manager = mgr;
    }
    
    public void start() {
        for (int i = 0; i < THREADS; i++) {
            Thread t = new Thread(new Runner(), "OutboundPusher" + (i+1) + '/' + THREADS);
            t.setDaemon(true);
            t.start();
        }
    }
    
    public void wakeUp() { synchronized (this) { notifyAll(); } }

    public void kill() { _die = true; wakeUp(); }
    
    private class Runner implements Runnable {
        public void run() {
            while (true) {
                if (_die) return;
                while (!_manager.isOnline()) {
                    try {
                        synchronized (SyncOutboundPusher.this) {
                            SyncOutboundPusher.this.wait(60*1000);
                        } 
                    } catch (InterruptedException ie) {}
                    if (_die) return;
                }
                
                SyncArchive archive = getNextToPush(Runner.this);
                if (archive != null) {
                    try {
                        push(Runner.this, archive);
                    } catch (Exception e) {
                        synchronized (_runnerToArchive) { _runnerToArchive.remove(Runner.this); }
                        archive.indexFetchFail("Internal error pushing", e, true);
                    }
                } else {
                    try {
                        synchronized (SyncOutboundPusher.this) {
                            SyncOutboundPusher.this.wait(60*1000);
                        } 
                    } catch (InterruptedException ie) {}
                }
            }
        }
    }
    
    private SyncArchive getNextToPush(Runner runner) {
        int count = _manager.getArchiveCount();
        // shuffle the archives so we aren't always syncing with the first on the list
        List<SyncArchive> archives = new ArrayList(count);
        for (int i = 0; i < count; i++) {
            archives.add(_manager.getArchive(i));
        }
        Collections.shuffle(archives);
        long now = System.currentTimeMillis();
        for (SyncArchive archive : archives) {
            synchronized (_runnerToArchive) {
                _runnerToArchive.remove(runner);
                if (archive.getIncompleteOutgoingActionCount() > 0) {
                    // there's stuff to be done
                    if (_runnerToArchive.containsValue(archive)) {
                        // but someone else is doing it
                        continue;
                    } else {
                        _runnerToArchive.put(runner, archive);
                        return archive;
                    }
                } else {
                    // not scheduled, or scheduled for the future
                }
            }
        }
        return null;
    }

    private void push(Runner runner, SyncArchive archive) {
        String url = archive.getURL();
        if ( (url == null) || (url.length() == 0) ) {
            archive.indexFetchFail("No URL", null, false);
            synchronized (_runnerToArchive) { _runnerToArchive.remove(runner); }
            return;
        }
        if ( (url.indexOf("USK@") >= 0) || (url.indexOf("SSK@") >= 0) || (url.indexOf("KSK@") >= 0) ) {
            pushFreenet(archive);
        } else if (url.startsWith("/") || url.startsWith("file://") || url.startsWith("C:\\")) {
            pushFile(archive);
        } else { // use http as the fallthrough, for "http://foo/" as well as "foo/"
            pushHTTP(archive);
        }
        archive.pushActionComplete();
        synchronized (_runnerToArchive) { _runnerToArchive.remove(runner); }
    }
    
    private void pushFreenet(SyncArchive archive) {
        int actions = archive.getOutgoingActionCount();
        List uris = new ArrayList();
        List actionsPushed = new ArrayList();
        for (int i = 0; i < actions; i++) {
            SyncArchive.OutgoingAction action = archive.getOutgoingAction(i);
            if (action.getCompletionTime() > 0) continue; // already complete
            if (action.isPaused()) continue; // dont wanna do it
            if (!action.setIsExecuting(true)) continue; // someone else is doing it
            SyndieURI uri = action.getURI();
            uris.add(uri);
            actionsPushed.add(action);
        }
        String err = null;
        if (uris.size() > 0)
            err = pushFreenet(archive, uris);
        if (err == null) {
            for (int i = 0; i < actionsPushed.size(); i++) {
            SyncArchive.OutgoingAction action = (SyncArchive.OutgoingAction)actionsPushed.get(i);
            action.pushOK();
        }
        } else {
            for (int i = 0; i < actionsPushed.size(); i++) {
                SyncArchive.OutgoingAction action = (SyncArchive.OutgoingAction)actionsPushed.get(i);
                action.pushFailed(err, null);
            }
        }
    }
    
    private String pushFreenet(SyncArchive archive, List uris) {
        String error = null;
        
        String host = archive.getFCPHost();
        int port = archive.getFCPPort();
        if (host != null) host = host.trim();
        if ( (host == null) || (port <= 0) )
            return "FCP Host Failed, check your settings.";
        if (archive.getPostKey() == null)
            return "You lack a post key";
        
        
        try {
            Socket s = new Socket(host, port);
            OutputStream out = s.getOutputStream();
            long msgId = System.currentTimeMillis();
            
            // say hello to FCP
            _manager.getUI().debugMessage("Saying hello to freenet (fcp host " + host + " port " + port + ")");
            out.write(DataHelper.getUTF8("ClientHello\r\n" +
                       "Name=syndie" + msgId + "\r\n" +
                       "ExpectedVersion=2.0\r\n" +
                       "Identifier=" + msgId + "\r\n" +
                       "EndMessage\r\n"));
            
            Map rv = readResults(s.getInputStream());
            //check for clienthello success here
            //CloseConnectionDuplicateClientName or lack of response
            if (rv.get("cmd") == null) {
                error = "Error communicating with the Freenet server";
                _manager.getUI().errorMessage(error);
                s.close();
                return error;
            }else{
                String cmd = (String) rv.get("cmd");
                if(cmd.startsWith("CloseConnectionDuplicateClientName")){
                    error = "Error communicating with the Freenet server";
                    _manager.getUI().errorMessage(error);
                    s.close(); 
                    return error;
                }
            }
            
            
            String target = getTarget(archive.getPostKey());
            
            _manager.getUI().debugMessage("Posting to " + target);
            
            out.write(("ClientPutComplexDir\r\n" +
                       "URI=" + target + "\r\n" + 
                       "Identifier=" + (msgId+1) + "\r\n" +
                       "ClientToken=" + (msgId+3) + "\r\n" +
                       "Verbosity=1111111\r\n" + // we want everything
                       "Persistence=forever\r\n" //seems to ensure you recieve PutSuccessfull
                       ).getBytes());
            
            
            
            // dont use UploadFrom=disk, because that breaks if the fcpHost != localhost,
            // or if the freenet instance doesn't have read permissions on the archive
            for (int i = 0; i < uris.size(); i++) {
                SyndieURI uri = (SyndieURI)uris.get(i);
                File f = null;
                if (uri.getMessageId() == null)
                    f = new File(new File(_manager.getClient().getOutboundDir(), uri.getScope().toBase64()), "meta" + Constants.FILENAME_SUFFIX);
                else
                    f = new File(new File(_manager.getClient().getOutboundDir(), uri.getScope().toBase64()), uri.getMessageId().toString() + Constants.FILENAME_SUFFIX);
                if (!f.exists()) {
                    if (uri.getMessageId() == null)
                        f = new File(new File(_manager.getClient().getArchiveDir(), uri.getScope().toBase64()), "meta" + Constants.FILENAME_SUFFIX);
                    else
                        f = new File(new File(_manager.getClient().getArchiveDir(), uri.getScope().toBase64()), uri.getMessageId().toString() + Constants.FILENAME_SUFFIX);
                }
                
                String path = uri.getScope().toBase64() + "/";
                if (uri.getMessageId() == null)
                    path = path + "meta" + Constants.FILENAME_SUFFIX;
                else
                    path = path + uri.getMessageId().toString() + Constants.FILENAME_SUFFIX;
                
                _manager.getUI().debugMessage("including path: " + path);
                out.write(("Files." + i + ".Name=" + path + "\r\n" +
                           "Files." + i + ".UploadFrom=direct\r\n" +
                           "Files." + i + ".Metadata.ContentType=application/x-syndie\r\n" +
                           "Files." + i + ".DataLength=" + f.length() + "\r\n").getBytes());
            }
            // now include an index.html, if it exists
            File htmlIndex = new File(_manager.getClient().getWebDir(), "index.html");
            if (htmlIndex.exists()) {
                _manager.getUI().debugMessage("including HTML index");
                out.write(("Files." + uris.size() + ".Name=index.html\r\n" +
                           "Files." + uris.size() + ".UploadFrom=direct\r\n" +
                           "Files." + uris.size() + ".Metadata.ContentType=text/html\r\n" +
                           "Files." + uris.size() + ".DataLength=" + htmlIndex.length() + "\r\n").getBytes());
            }
            // don't forget the sharedIndex
            File sharedIndex = new File(_manager.getClient().getWebDir(), LocalArchiveManager.SHARED_INDEX_FILE);
            _manager.getUI().debugMessage("******"+sharedIndex.getAbsoluteFile());
            if (sharedIndex.exists()) {
                _manager.getUI().debugMessage("including shared index");
                out.write(("Files." + (uris.size()+1) + ".Name=" + LocalArchiveManager.SHARED_INDEX_FILE + "\r\n" +
                           "Files." + (uris.size()+1) + ".UploadFrom=direct\r\n" +
                           "Files." + (uris.size()+1) + ".Metadata.ContentType=application/x-syndie-index\r\n" +
                           "Files." + (uris.size()+1) + ".DataLength=" + sharedIndex.length() + "\r\n").getBytes());
            }
            out.write(DataHelper.getUTF8("EndMessage\r\n"));
            
            int bytes = 0;
            for (int i = 0; i < uris.size(); i++) {
                SyndieURI uri = (SyndieURI)uris.get(i);
                File f = null;
                if (uri.getMessageId() == null)
                    f = new File(new File(_manager.getClient().getOutboundDir(), uri.getScope().toBase64()), "meta" + Constants.FILENAME_SUFFIX);
                else
                    f = new File(new File(_manager.getClient().getOutboundDir(), uri.getScope().toBase64()), uri.getMessageId().toString() + Constants.FILENAME_SUFFIX);
                if (!f.exists()) {
                    if (uri.getMessageId() == null)
                        f = new File(new File(_manager.getClient().getArchiveDir(), uri.getScope().toBase64()), "meta" + Constants.FILENAME_SUFFIX);
                    else
                        f = new File(new File(_manager.getClient().getArchiveDir(), uri.getScope().toBase64()), uri.getMessageId().toString() + Constants.FILENAME_SUFFIX);
                }
            
                byte buf[] = new byte[4096];
                FileInputStream fin = null;
                try {
                    fin = new FileInputStream(f);
                    int read = -1;
                    while ( (read = fin.read(buf)) != -1) {
                        out.write(buf, 0, read);
                        bytes += read;
                    }
                    fin.close();
                    fin = null;
                } finally {
                    if (fin != null) try { fin.close(); } catch (IOException ioe) {}
                }
            }
            
            if (htmlIndex.exists()) {
                byte buf[] = new byte[4096];
                FileInputStream fin = null;
                try {
                    fin = new FileInputStream(htmlIndex);
                    int read = -1;
                    while ( (read = fin.read(buf)) != -1) {
                        out.write(buf, 0, read);
                        bytes += read;
                    }
                    fin.close();
                    fin = null;
                } finally {
                    if (fin != null) try { fin.close(); } catch (IOException ioe) {}
                }
            }
            
            if (sharedIndex.exists()) {
                byte buf[] = new byte[4096];
                FileInputStream fin = null;
                try {
                    fin = new FileInputStream(sharedIndex);
                    int read = -1;
                    while ( (read = fin.read(buf)) != -1) {
                        out.write(buf, 0, read);
                        bytes += read;
                    }
                    fin.close();
                    fin = null;
                } finally {
                    if (fin != null) try { fin.close(); } catch (IOException ioe) {}
                }
            }
            
            _manager.getUI().debugMessage("FCP message written, now reading the response");
            
            // since we are doing a persistent put, this does not block waiting
            // for the actual full posting, merely schedules the posting through
            // freenet
            rv = readResults(s.getInputStream());
            
            if (rv.get("cmd") == null) {
                error = "Error communicating with the Freenet server";
                _manager.getUI().errorMessage(error);
                s.close();
                //return error;
                
            }else {
                //_manager.getUI().debugMessage("Freenet archive publishing queued on the freenet server");
                _manager.getUI().debugMessage("Total size queued: " + (bytes+1023)/1024 + "KBytes");
                //maybe use debugs to give rough estimates of precentage left/done 
               
                //while loop to adjust error to null if sucess or error if put failed
                String cmd = (String) rv.get("cmd");
                while(!(cmd.startsWith("PutFailed")) && !(cmd.startsWith("PutSuccessful"))){
                    _manager.getUI().debugMessage(cmd);
                    rv = readResults(s.getInputStream());
                    out.write(("Void\r\nEndMessage\r\n").getBytes());
                    cmd = (String)rv.get("cmd");
                }if(cmd.startsWith("PutFailed")){
                    error = "Pushing failed try again later";
                    _manager.getUI().errorMessage(error);
                    s.close();
                    return error;
                }else{
                    s.close();
                    return null;
                }
                
            }
        }
        catch (IOException ioe) {
            error = "Error posting the archive to Freenet (fcp " + host + ":"+ port + ")";
            _manager.getUI().errorMessage(error, ioe);
        }
        return error;
    }
    
    private String getTarget(String privateSSK) {
        String key = privateSSK;
        if ( privateSSK.indexOf("USK@")!=0 && privateSSK.indexOf("USK@")!=-1 ) {
            key = key.substring(privateSSK.indexOf("USK@"));
        }
        else if ( privateSSK.indexOf("SSK@")!=0 && privateSSK.indexOf("SSK@")!=-1 ){
            key = key.substring(privateSSK.indexOf("SSK@"));
        }
        else if ( privateSSK.indexOf("KSK@")!=0 && privateSSK.indexOf("KSK@")!=-1 ){
            key = key.substring(privateSSK.indexOf("KSK@"));
        }
        else if ( privateSSK.indexOf("CHK@")!=0 && privateSSK.indexOf("CHK@")!=-1 ){
            key = key.substring(privateSSK.indexOf("CHK@"));
        }
        
        return key;
    }
    
    private Map readResults(InputStream in) throws IOException { return readResults(in, _manager.getUI()); }
    public static Map readResults(InputStream in, UI ui) throws IOException {
        // I dont think the node hello message should be ignored... check for the unlikely 
        // CloseConnectionDuplicateClientName
        BufferedReader bin = new BufferedReader(new InputStreamReader(in, "UTF-8"));
        String line = null;
        Map rv = new HashMap();
        String cmd = null;
        while ( (line = bin.readLine()) != null) {
            ui.debugMessage("Line read: " + line);
            if (cmd == null) {
                cmd = line;
            } else if (line.startsWith("EndMessage")) {
                // Save message types now for polling
                rv.put("cmd",cmd);
                return rv;
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
    
    private void pushFile(SyncArchive archive) {
        int actions = archive.getOutgoingActionCount();
        for (int i = 0; i < actions; i++) {
            SyncArchive.OutgoingAction action = archive.getOutgoingAction(i);
            if (action.getCompletionTime() > 0) continue; // already complete
            if (action.isPaused()) continue; // dont wanna do it
            
            action.pushFailed("Push to files not supported", null);
        }
    }
    
    /** send HTTP posts in batches of 100KB */
    private static final int HTTP_SEND_BATCH_SIZE = 100*1024;
    private void pushHTTP(SyncArchive archive) {
        while (true) {
            int actions = archive.getOutgoingActionCount();
            if (actions <= 0) return;
            
            List<SyncArchive.OutgoingAction> actionsPushed = new ArrayList();
            int len = 0;
            for (int i = 0; i < actions; i++) {
                SyncArchive.OutgoingAction action = archive.getOutgoingAction(i);
                if (action.getCompletionTime() > 0) continue; // already complete
                if (action.isPaused()) continue; // dont wanna do it
                if (!action.setIsExecuting(true)) continue; // someone else is doing it
                
                len += action.getSize();

                actionsPushed.add(action);
                
                if (len > HTTP_SEND_BATCH_SIZE)
                    break;
            }
            String err = null;
            if (!actionsPushed.isEmpty())
                err = pushHTTP(archive, actionsPushed);
            else
                break; // all paused/complete/in flight/etc

            if (err == null) {
                for (int i = 0; i < actionsPushed.size(); i++) {
                    SyncArchive.OutgoingAction action = (SyncArchive.OutgoingAction)actionsPushed.get(i);
                    action.pushOK();
                }
            } else {
                for (int i = 0; i < actionsPushed.size(); i++) {
                    SyncArchive.OutgoingAction action = (SyncArchive.OutgoingAction)actionsPushed.get(i);
                    action.pushFailed(err, null);
                }
            }
        }
    }
    
    /**
     * Copied from net.i2p.util.EepGet.handleStatus
     *
     * parse the first status line and grab the response code.
     * e.g. "HTTP/1.1 206 OK" vs "HTTP/1.1 200 OK" vs 
     * "HTTP/1.1 404 NOT FOUND", etc.  
     *
     * @return HTTP response code (200, 206, other)
     */
    private int getHTTPResponseCode(String initialLine) {
        if (initialLine == null) return -1;
        StringTokenizer tok = new StringTokenizer(initialLine, " ");
        if (!tok.hasMoreTokens())
            return -1;
        String protocol = tok.nextToken(); // ignored
        if (!tok.hasMoreTokens())
            return -1;
        String rc = tok.nextToken();
        try {
            return Integer.parseInt(rc);
        } catch (NumberFormatException nfe) {
            return -1;
        }
    }
    
    private String pushHTTP(SyncArchive archive, List<SyncArchive.OutgoingAction> actions) {
        String error = null;
        long len = 0;
        List<File> metaFiles = new ArrayList();
        List<File> msgFiles = new ArrayList();
        for (SyncArchive.OutgoingAction action: actions) {
            SyndieURI uri = action.getURI();
            File chanDir = new File(_manager.getClient().getArchiveDir(), uri.getScope().toBase64());
            File f = null;
            if (uri.getMessageId() == null) {
                f = new File(chanDir, "meta" + Constants.FILENAME_SUFFIX);
                len += f.length();
                metaFiles.add(f);
            } else {
                f = new File(chanDir, uri.getMessageId().longValue() + Constants.FILENAME_SUFFIX);
                len += f.length();
                msgFiles.add(f);
            }
            
            len += 5; // attribs
            
            _manager.getUI().debugMessage("Posting " + f.getPath());
        }
        _manager.getUI().debugMessage("Posting " + metaFiles.size() + " metadata messages and " + msgFiles.size() + " posts, totalling " + len);
        
        String url = archive.getURL();
        int q = url.indexOf('?');
        String query = null;
        if (q > 0) {
            query = url.substring(q);
            url = url.substring(0, q);
        }
        if (url.endsWith("/"))
            url = url + "import.cgi";
        else
            url = url + "/import.cgi";
        if (q > 0)
            url = url + query;
        
        
        if ( (archive.getHTTPProxyHost() != null) && (archive.getHTTPProxyHost().length() > 0) )
            _manager.getUI().debugMessage("Pushing to [" + url + "] proxy " + archive.getHTTPProxyHost() + ":" + archive.getHTTPProxyPort());
        else
            _manager.getUI().debugMessage("Pushing to [" + url + "]");
        Socket s = null;
        TimeoutEvent timeout = null;
        try {
            if ( (archive.getHTTPProxyHost() != null) && (archive.getHTTPProxyHost().length() > 0) && (archive.getHTTPProxyPort() > 0) ) {
                s = new Socket(archive.getHTTPProxyHost(), archive.getHTTPProxyPort());
            } else {
                try {
                    URI uri = new URI(url);
                    String host = uri.getHost();
                    int port = uri.getPort();
                    s = new Socket(host, port);
                } catch (URISyntaxException use) {
                    throw new IOException("invalid uri: " + use.getMessage());
                }
            }
            
            final Socket toClose = s;
            final String sentURL = url;
            timeout = new TimeoutEvent(toClose,sentURL);
            timeout.schedule(5*60*1000); // if it can't send the post in 5 minutes, its not going anywhere
            
            len += 2; // header size=0
            
            StringBuilder buf = new StringBuilder();
            buf.append("POST " + url + " HTTP/1.0\r\nConnection: close\r\nContent-length: ");
            buf.append(len).append("\r\n\r\n");
            OutputStream out = s.getOutputStream();
            out.write(DataHelper.getUTF8(buf.toString()));
            DataHelper.writeLong(out, 2, 0);
            int idx = 0;
            for (int i = 0; i < metaFiles.size(); i++) {
                actions.get(i).setPushingMeta();
                send(++idx, out, metaFiles.get(i), 0x1);
            }
            for (int i = 0; i < msgFiles.size(); i++) {
                actions.get(i).setPushingBody();
                send(++idx, out, msgFiles.get(i), 0x0);
            }
            out.flush();
            
            String line = DataHelper.readLine(s.getInputStream());
            _manager.getUI().debugMessage("result from http post: " + line);
            switch (getHTTPResponseCode(line)) {
                case 200: // OK
                    break;
                case 403: // Not authorized
                    error = "access denied (archive may not be accepting pushes)";
                    break;
                default: // invalid initial response line (code -1) or unrecognized code
                    error = "post failed";
                    break;
            }
            out.close();
            s.close();

            timeout.cancel();
            
            _manager.getUI().debugMessage("Files posted");
            _manager.getUI().commandComplete(0, null);
        } catch (DataFormatException dfe) {
        	timeout.cancel();
            error = "Internal error: " + dfe.getMessage();
            _manager.getUI().debugMessage("Error posting", dfe);
            _manager.getUI().commandComplete(-1, null);
        } catch (IOException ioe) {
        	timeout.cancel();
            error = ioe.getMessage();
            _manager.getUI().debugMessage("Error posting", ioe);
            _manager.getUI().commandComplete(-1, null);
        }
        return error;
    }
    
    private class TimeoutEvent extends SimpleTimer2.TimedEvent {
    	private final Socket toClose;
    	private final String sentURL;
    	TimeoutEvent(Socket toClose, String sentURL) {
    		super(SimpleTimer2.getInstance());
    		this.toClose = toClose;
    		this.sentURL = sentURL;
    	}
    	public void timeReached() {
            try {
                if (!toClose.isClosed()) {
                    _manager.getUI().debugMessage("Push to " + sentURL + " timed out");
                    toClose.close();
                }
            } catch (IOException ioe) {}
        }
    }
    
    private void send(int idx, OutputStream out, File file, int flag) throws IOException, DataFormatException {
        _manager.getUI().debugMessage(idx + ": Sending" + file.getPath() + "/" + file.length() + "/" + flag);
        DataHelper.writeLong(out, 1, flag);
        DataHelper.writeLong(out, 4, file.length());
        byte buf[] = new byte[4096];
        FileInputStream fin = null;
        try {
            fin = new FileInputStream(file);
            int read = -1;
            while ( (read = fin.read(buf)) != -1)
                out.write(buf, 0, read);
            fin.close();
            fin = null;
        } finally {
            if (fin != null) fin.close();
        }
    }
}
