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
 * See http://trac.i2p2.i2p/ticket/1424
 * for background and limitations of Freenet support
 *
 * @since 1.106 code moved from SyncOutboundPusher
 */
public class FreenetHelper {
  
  public static String pushFreenetArchive(SyncArchive archive, List<SyndieURI> uris, Socket s,
                                          String global, UI ui, DBClient client) {
        String error = null;
        String host = archive.getFCPHost();
        int port = archive.getFCPPort();
        
        try {
            OutputStream out = s.getOutputStream();
            long msgId = System.currentTimeMillis();
            // say hello to FCP
            ui.debugMessage("Saying hello to freenet (fcp host " + host + " port " + port + ")");
            out.write(DataHelper.getUTF8("ClientHello\r\n" +
                       "Name=syndie" + msgId + "\r\n" +
                       "ExpectedVersion=2.0\r\n" +
                       "Identifier=" + msgId + "\r\n" +
                       "EndMessage\r\n"));
            
            Map<String, String> rv = readResults(s.getInputStream(),ui);
            //check for clienthello success here
            //CloseConnectionDuplicateClientName or lack of response
            if (rv.get("cmd") == null) {
                error = "Error communicating with the Freenet server";
                ui.errorMessage(error);
                s.close();
                return error;
            } else {
                String cmd = rv.get("cmd");
                if (cmd.startsWith("CloseConnectionDuplicateClientName")) {
                    error = "Error communicating with the Freenet server";
                    ui.errorMessage(error);
                    s.close(); 
                    return error;
                }
            }
            String target = getTarget(archive.getPostKey());
            
            ui.debugMessage("Posting to " + target);
            
            out.write(("ClientPutComplexDir\r\n" +
                       "Identifier=" + (msgId+1) + "\r\n" +
                       "URI=" + target + "\r\n" +  
                       "ClientToken=" + (msgId+3) + "\r\n" +
                       "Verbosity=1111111\r\n" + // we want everything
                       "Global="+global+"\r\n" +
                       "Persistence=forever\r\n" //seems to ensure you recieve PutSuccessfull
                       ).getBytes());
            
            // dont use UploadFrom=disk, because that breaks if the fcpHost != localhost,
            // or if the freenet instance doesn't have read permissions on the archive
            for (int i = 0; i < uris.size(); i++) {
                SyndieURI uri = uris.get(i);
                File f = null;
                if (uri.getMessageId() == null)
                    f = new File(new File(client.getOutboundDir(), uri.getScope().toBase64()), "meta" + Constants.FILENAME_SUFFIX);
                else
                    f = new File(new File(client.getOutboundDir(), uri.getScope().toBase64()), uri.getMessageId().toString() + Constants.FILENAME_SUFFIX);
                if (!f.exists()) {
                    if (uri.getMessageId() == null)
                        f = new File(new File(client.getArchiveDir(), uri.getScope().toBase64()), "meta" + Constants.FILENAME_SUFFIX);
                    else
                        f = new File(new File(client.getArchiveDir(), uri.getScope().toBase64()), uri.getMessageId().toString() + Constants.FILENAME_SUFFIX);
                }
                
                String path = uri.getScope().toBase64() + "/";
                if (uri.getMessageId() == null)
                    path = path + "meta" + Constants.FILENAME_SUFFIX;
                else
                    path = path + uri.getMessageId().toString() + Constants.FILENAME_SUFFIX;
                
                ui.debugMessage("including path: " + path);
                out.write(("Files." + i + ".Name=" + path + "\r\n" +
                           "Files." + i + ".UploadFrom=direct\r\n" +
                           "Files." + i + ".Metadata.ContentType=application/x-syndie\r\n" +
                           "Files." + i + ".DataLength=" + f.length() + "\r\n").getBytes());
            }
            // now include an index.html, if it exists
            File htmlIndex = new File(client.getWebDir(), "index.html");
            if (htmlIndex.exists()) {
                ui.debugMessage("including HTML index");
                out.write(("Files." + uris.size() + ".Name=index.html\r\n" +
                           "Files." + uris.size() + ".UploadFrom=direct\r\n" +
                           "Files." + uris.size() + ".Metadata.ContentType=text/html\r\n" +
                           "Files." + uris.size() + ".DataLength=" + htmlIndex.length() + "\r\n").getBytes());
            }
            // don't forget the sharedIndex
            File sharedIndex = new File(client.getWebDir(), LocalArchiveManager.SHARED_INDEX_FILE);
            if(!(sharedIndex.exists())) sharedIndex = new File(client.getArchiveDir(), LocalArchiveManager.SHARED_INDEX_FILE);
            if (sharedIndex.exists()) {
                ui.debugMessage("including shared index");
                out.write(("Files." + (uris.size()+1) + ".Name=" + LocalArchiveManager.SHARED_INDEX_FILE + "\r\n" +
                           "Files." + (uris.size()+1) + ".UploadFrom=direct\r\n" +
                           "Files." + (uris.size()+1) + ".Metadata.ContentType=application/x-syndie-index\r\n" +
                           "Files." + (uris.size()+1) + ".DataLength=" + sharedIndex.length() + "\r\n").getBytes());
            }
            out.write(DataHelper.getUTF8("EndMessage\r\n"));
            
            int bytes = 0;
            for (int i = 0; i < uris.size(); i++) {
                SyndieURI uri = uris.get(i);
                File f = null;
                if (uri.getMessageId() == null)
                    f = new File(new File(client.getOutboundDir(), uri.getScope().toBase64()), "meta" + Constants.FILENAME_SUFFIX);
                else
                    f = new File(new File(client.getOutboundDir(), uri.getScope().toBase64()), uri.getMessageId().toString() + Constants.FILENAME_SUFFIX);
                if (!f.exists()) {
                    if (uri.getMessageId() == null)
                        f = new File(new File(client.getArchiveDir(), uri.getScope().toBase64()), "meta" + Constants.FILENAME_SUFFIX);
                    else
                        f = new File(new File(client.getArchiveDir(), uri.getScope().toBase64()), uri.getMessageId().toString() + Constants.FILENAME_SUFFIX);
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
            
            ui.debugMessage("FCP message written, now reading the response");
            return null;
        } catch (IOException ioe) {
            error = "Error posting the archive to Freenet (fcp " + host + ":"+ port + ")";
            ui.errorMessage(error, ioe);
            return error;
        }
    }
    
    private static String getTarget(String privateSSK) {
        String key = privateSSK;
        if (privateSSK.indexOf("USK@")!=0 && privateSSK.indexOf("USK@") != -1 ) {
            key = key.substring(privateSSK.indexOf("USK@"));
        } else if (privateSSK.indexOf("SSK@")!=0 && privateSSK.indexOf("SSK@") != -1 ) {
            key = key.substring(privateSSK.indexOf("SSK@"));
        } else if (privateSSK.indexOf("KSK@")!=0 && privateSSK.indexOf("KSK@") != -1 ) {
            key = key.substring(privateSSK.indexOf("KSK@"));
        } else if (privateSSK.indexOf("CHK@")!=0 && privateSSK.indexOf("CHK@") != -1 ) {
            key = key.substring(privateSSK.indexOf("CHK@"));
        }
        
        return key;
    }
    
    public static Map<String, String> readResults(InputStream in, UI ui) throws IOException {
        //Buffered reader was possibly tossing results
        // I dont think the node hello message should be ignored... check for the unlikely 
        // CloseConnectionDuplicateClientName
        int c;
        InputStreamReader isr = new InputStreamReader(in, "UTF-8");
        StringBuilder sb = new StringBuilder();
        for (c = isr.read(); c != '\n' && c != -1 ; c = isr.read()) {
            sb.append((char)c);
        }
        if (c == -1 && sb.length() == 0) return null;
        String line = sb.toString();
        Map<String, String> rv = new HashMap<String, String>();
        // Save message types now for polling
        rv.put("cmd",line);
        while (line != null) {
            sb = new StringBuilder();
            for (c = isr.read(); c != '\n' && c != -1 ; c = isr.read()) {
                sb.append((char)c);
            }
            if (c == -1 && sb.length() == 0) return null;
            line = sb.toString();
            
            ui.debugMessage("Line read: " + line);
            if (line.startsWith("EndMessage")) {
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
    
}
