package syndie.db;

import gnu.crypto.hash.Sha256Standalone;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import net.i2p.data.Base64;
import net.i2p.data.DataFormatException;
import net.i2p.data.DataHelper;
import net.i2p.data.SessionKey;

/**
 * CLI parameters: ([--port $num] [--listeners $num] [--writable true] | [--kill true])
 */
public class HTTPServ implements CLI.Command {
    private static ServerSocket _ssocket;
    private List _runners;
    /** accepted Socket instances that haven't run yet */
    private static List _pendingSockets = new ArrayList();
    private DBClient _client;
    private static UI _ui;
    private static boolean _alive;
    private boolean _allowPost;
    
    public HTTPServ() {
        _runners = new ArrayList();
        _client = null;
        _allowPost = false;
    }
    
    public static void killAll() { 
        _alive = false; 
        _ui.debugMessage("Marking server as dead");
        synchronized (_pendingSockets) { 
            _pendingSockets.notifyAll(); 
        }
        try {
            if (_ssocket != null) {
                _ui.debugMessage("Closing server socket");
                _ssocket.close();
            }
        } catch (IOException ioe) {
            _ui.debugMessage("Problem closing server socket", ioe);
        }
        _ssocket = null;
    }
    public static boolean isAlive() { return _alive; }

    public DBClient runCommand(Opts opts, UI ui, DBClient client) {
        _client = client;
        _ui = ui;
        if (opts.getOptBoolean("kill", false)) {
            killAll();
            _ui.statusMessage("HTTP server killed");
            _ui.commandComplete(0, null);
            return client;
        }
        
        int port = (int)opts.getOptLong("port", 8080);
        int listeners = (int)opts.getOptLong("listeners", 5);
        _allowPost = opts.getOptBoolean("writable", true);
        try {
            _ssocket = new ServerSocket(port);
            _alive = true;
            _ui.debugMessage("Set server socket to " + _ssocket);
            for (int i = 0; i < listeners; i++) {
                Thread t = new Thread(new Runner(), "HTTPServ run " + i);
                t.setDaemon(true);
                t.start();
            }
            Thread t = new Thread(new AcceptRunner(), "HTTPServ accept");
            t.setDaemon(true);
            t.start();
            ui.statusMessage("HTTP archive server listening on " + port);
            ui.commandComplete(0, null);
        } catch (IOException ioe) {
            ui.errorMessage("error listening to " + port, ioe);
            ui.commandComplete(-1, null);
        }
        return client;
    }
    
    private static final int MAX_PENDING = 10;
    
    private class AcceptRunner implements Runnable {
        public void run() {
            while (_alive && _ssocket != null) {
                try {
                    Socket socket = _ssocket.accept();
                    _ui.debugMessage("Connection accepted");
                    boolean added = false;
                    synchronized (_pendingSockets) {
                        if (_pendingSockets.size() < MAX_PENDING) {
                            _pendingSockets.add(socket);
                            added = true;
                        }
                        _pendingSockets.notifyAll();
                    }
                    if (!added)
                        tooBusy(socket);
                } catch (IOException ioe) {
                    if (_alive)
                        _ui.debugMessage("Error accepting", ioe);
                }
            }
            _ui.debugMessage("Accept runner terminated");
        }
    }
    
    private class Runner implements Runnable {
        public void run() {
            while (_alive) {
                try {
                    Socket socket = null;
                    synchronized (_pendingSockets) {
                        if (_pendingSockets.size() <= 0)
                            _pendingSockets.wait();
                        else
                            socket = (Socket)_pendingSockets.remove(0);
                    }
                    if (socket != null)
                        handle(socket);
                } catch (IOException ioe) {
                    _ui.debugMessage("Error handing socket", ioe);
                } catch (InterruptedException ie) {}
            }
            _ui.debugMessage("Terminating runner " + Thread.currentThread().getName());
        }
    }
    
    private void handle(Socket socket) throws IOException {
        _ui.debugMessage("handling a client");
        InputStream in = socket.getInputStream();
        OutputStream out = socket.getOutputStream();
        String line = null;
        line = DataHelper.readLine(in);
        if (line == null)
            fail(socket, in, out);
        
        try {
            if (line.startsWith("GET "))
                handleGet(socket, in, out, getPath(line));
            else if (line.startsWith("POST "))
                handlePost(socket, in, out);
            else
                fail(socket, in, out);
        } catch (RuntimeException re) {
            _ui.errorMessage("Error handling", re);
            fail(socket, in, out);
        }
    }
    
    private static final String getPath(String line) {
        int urlBegin = line.indexOf(' ') + 1;
        if (urlBegin > 0) {
            int urlEnd = line.indexOf(' ', urlBegin+1);
            if (urlEnd > 0) {
                String url = line.substring(urlBegin, urlEnd);
                if (url.startsWith("/")) {
                    return url.trim();
                } else {
                    try {
                        URI uri = new URI(url);
                        return uri.getPath();
                    } catch (URISyntaxException use) {
                        return null;
                    }
                }
            } else {
                return null;
            }
        } else {
            return null;
        }
    }
    private static final String getChannel(String path) {
        int idx = path.lastIndexOf('/');
        if (idx < 0) return null;
        if ( (path.charAt(0) == '/') && (idx > 1) )
            return path.substring(1, idx);
        else
            return path.substring(0, idx);
    }
    private static final String getChannelSub(String path) {
        int idx = path.lastIndexOf('/');
        if ( (idx < 0) || (idx + 1 >= path.length()) ) return null;
        return path.substring(idx+1);
    }
    private void handleGet(Socket socket, InputStream in, OutputStream out, String path) throws IOException {
        if (path == null) path = "/index.html";
        _ui.debugMessage("GET " + path);
        String chan = getChannel(path);
        if (chan == null) chan = "";
        String sub = getChannelSub(path);
        _ui.debugMessage("GET of [" + chan + "]  [" + sub + "]");
        if ( (chan.length() <= 0) && (sub == null) )
            sub = "index.html";
        
        // overzealous file read control - pick out of known ok files, rather than parse the requested dir/file
        File chans[] = _client.getArchiveDir().listFiles();
        for (int i = 0; i < chans.length; i++) {
            _ui.debugMessage("chans[" + i + "]: " + chans[i].getName());
            if (chan.length() <= 0) {
                if ( (chans[i].isFile()) && (chans[i].getName().equals(sub)) ) {
                    // index file
                    _ui.debugMessage("index file selected: " + chans[i].getName());
                    send(socket, in, out, chans[i]);
                    return;
                }
            } else {
                if (chans[i].getName().equals(chan)) {
                    File subs[] = chans[i].listFiles();
                    for (int j = 0; j < subs.length; j++) {
                        if (subs[j].getName().equals(sub)) {
                            _ui.debugMessage("data file selected: " + chans[i].getName() + "/" + subs[j].getName());
                            send(socket, in, out, subs[j]);
                            return;
                        }
                    }
                    fail404(socket, in, out);
                }
            }
        }
        fail404(socket, in, out);
    }
    
    
    private void send(Socket socket, InputStream in, OutputStream out, File file) throws IOException {
        String type = "application/octet-stream";
        String name = file.getName();
        if (name.endsWith(".html"))
            type = "text/html";
        else if (name.endsWith(".syndie"))
            type = "application/x-syndie";
        else if (name.endsWith(".dat"))
            type = "application/x-syndie-index";
        StringBuffer buf = new StringBuffer();
        buf.append("HTTP/1.0 200 OK\r\n");
        buf.append("Content-type: ").append(type).append("\r\n");
        buf.append("Content-length: ").append(file.length()).append("\r\n");
        buf.append("Connection: close\r\n");
        buf.append("\r\n");
        out.write(DataHelper.getUTF8(buf.toString()));
        
        int len = 0;
        Sha256Standalone hash = new Sha256Standalone();
        FileInputStream fin = null;
        try {
            fin = new FileInputStream(file);
            byte dbuf[] = new byte[4096];
            int read = 0;
            while ( (read = fin.read(dbuf)) != -1) {
                out.write(dbuf, 0, read);
                hash.update(dbuf, 0, read);
                len += read;
            }
            
            fin.close();
            fin = null;
            
            out.flush();
            
            _ui.debugMessage("Sent " + file.getPath() + ": " + len + "/" + file.length() +", sha256 = " + Base64.encode(hash.digest()));
        } finally {
            if (fin != null) try { fin.close(); } catch (IOException ioe) {}
            close(socket, in, out);
        }
    }
    
    private static void close(Socket socket, InputStream in, OutputStream out) throws IOException {
        try {
            socket.setSoTimeout(5000);
            try {
                in.read(); // block until the other side gets the data
            } catch (IOException ioe) {}
            in.close();
            in = null;
            out.close();
            out = null;
            socket.close();
            socket = null;
        } finally {
            if (in != null) try { in.close(); } catch (IOException ioe) {}
            if (out != null) try { in.close(); } catch (IOException ioe) {}
            if (out != null) try { out.close(); } catch (IOException ioe) {}
            if (socket != null) try { socket.close(); } catch (IOException ioe) {}
        }
    }
    
    private void handlePost(Socket socket, InputStream in, OutputStream out) throws IOException {
        if (!_allowPost) {
            fail403(socket, in, out);
            return;
        }
        _ui.debugMessage("handlePost");
        StringBuffer buf = new StringBuffer();
        long contentLength = -1;
        long remaining = 0;
        while (DataHelper.readLine(in, buf)) {
            String str = buf.toString().trim();
            buf.setLength(0);
            _ui.debugMessage("handle post line: " + str);
            if (str.length() == 0)
                break; // end of the headers
            if (str.startsWith("Content-length:")) {
                try {
                    contentLength = Long.parseLong(str.substring("content-length:".length()).trim());
                } catch (NumberFormatException nfe) {
                    fail(socket, in, out);
                    return;
                }
            }
        }
        
        remaining = contentLength;
        
        File importDir = new File(_client.getTempDir(), System.currentTimeMillis() + "." + Thread.currentThread().hashCode() + ".imp");
        importDir.mkdirs();

        try {
            int headerSize = (int)DataHelper.readLong(in, 2);
            byte header[] = new byte[headerSize];
            int read = DataHelper.read(in, header);
            if (read != headerSize) {
                fail(socket, in, out);
                return;
            }

            remaining -= headerSize + 2;
            
            _ui.debugMessage("handlePost: header read, remaining: " + remaining);
            
            SessionKey authKey = getAuthorizationKey(header);
            SessionKey encKey = getEncryptionKey(header);
            if (!authorized(authKey)) {
                fail403(socket, in, out);
            }
            
            int msgNum = 0;
            while (remaining > 0) {
                msgNum++;
                int flags = (int)DataHelper.readLong(in, 1);
                long sz = DataHelper.readLong(in, 4);
                remaining -= 5;
                //if ( (sz > ArchiveIndex.DEFAULT_MAX_SIZE) || (sz > remaining{
                if (sz > remaining) {
                    _ui.debugMessage(msgNum + ": invalid size: " + sz + " remaining: " + remaining);
                    fail(socket, in, out);
                    return;
                }
                 
                remaining -= sz;
                
                byte msg[] = new byte[(int)sz];
                read = DataHelper.read(in, msg);
                if (read != (int)sz) {
                    fail(socket, in, out);
                    return;
                }
                
                if (sz > ArchiveIndex.DEFAULT_MAX_SIZE) {
                    _ui.debugMessage(msgNum + ": message size is too large: " + sz);
                    // ignore it
                } else {
                    if (remaining >= 0) {
                        // import it now?  queue it up for later?  see if its a dup?
                        FileOutputStream fos = new FileOutputStream(new File(importDir, msgNum + ".syndie"));
                        _ui.debugMessage(msgNum + ": handlePost: read message of size " + read + ", remaining: " + remaining);

                        fos.write(msg);
                        fos.close();
                    }
                }
            } 
            
            _ui.debugMessage(msgNum + ": handlePost: read complete " + contentLength + " to " + importDir.getPath());
            out.write(DataHelper.getUTF8("HTTP/1.0 200 OK\r\nConnection: close\r\n\r\n"));
            close(socket, in, out);
            if (msgNum > 0) {
                _ui.statusMessage("HTTP server received " + msgNum + " messages, scheduling bulk import");
                _ui.insertCommand("menu syndicate");
                _ui.insertCommand("bulkimport --dir '" + importDir.getPath() + "' --delete true");
                _ui.insertCommand("buildindex");
            }
        } catch (DataFormatException dfe) {
            delete(importDir);
            throw new IOException(dfe.getMessage());
        } catch (IOException ioe) {
            delete(importDir);
            throw ioe;
        }
    }
    
    /** key used to verify they're authorized to post */
    private SessionKey getAuthorizationKey(byte header[]) { return null; }
    /** key used to decrypt the content (to extract the .syndie files) */
    private SessionKey getEncryptionKey(byte header[]) { return null; }
    /** true if they're authorized to post */
    private boolean authorized(SessionKey authorizationKey) { return true; }
    
    private void delete(File dir) {
        File files[] = dir.listFiles();
        for (int i = 0; i < files.length; i++) {
            if (!files[i].getName().startsWith("."))
                files[i].delete();
        }
        dir.delete();
    }
    
    private void fail404(Socket socket, InputStream in, OutputStream out) throws IOException {
        out.write(ERR_404);
        fail(socket, in, out);
    }
    private void fail403(Socket socket, InputStream in, OutputStream out) throws IOException {
        out.write(ERR_403);
        fail(socket, in, out);
    }
    private void fail(Socket socket, InputStream in, OutputStream out) throws IOException {
        _ui.debugMessage("failing socket", new Exception("source"));
        close(socket, in, out);
    }
    
    private static final byte[] TOO_BUSY = DataHelper.getUTF8("HTTP/1.0 401 TOO BUSY\r\nConnection: close\r\n");
    private static final byte[] ERR_404 = DataHelper.getUTF8("HTTP/1.0 404 File not found\r\nConnection: close\r\n");
    private static final byte[] ERR_403 = DataHelper.getUTF8("HTTP/1.0 403 Not authorized\r\nConnection: close\r\n");
    
    private static final void tooBusy(Socket socket) throws IOException {
        OutputStream out = socket.getOutputStream();
        out.write(TOO_BUSY);
        close(socket, socket.getInputStream(), out);
    }
}
