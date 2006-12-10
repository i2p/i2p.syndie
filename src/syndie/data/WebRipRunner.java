package syndie.data;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.i2p.I2PAppContext;
import net.i2p.data.DataHelper;
import net.i2p.util.EepGet;
import syndie.Constants;
import syndie.db.JobRunner;
import syndie.db.Opts;
import syndie.db.TextUI;
import syndie.db.UI;

/**
 *
 */
class WebRipRunner implements EepGet.StatusListener {
    private String _location;
    private String _proxyHost;
    private int _proxyPort;
    private File _tmpDir;
    private File _htmlFile;
    /** file the attachment is stored in (File) */
    private List _attachmentFiles;
    /** absolute url used to fetch (String) */
    private List _attachmentURLs;
    /** url found in the html (String) */
    private List _attachmentURLRefs;
    /** relative url from the html */
    private List _otherURLRefs;
    
    private List _errorMessages;
    private List _exceptions;
    
    private int _maxAttachKB;
    private int _maxTotalKB;
    private boolean _allowFileURLs;
    /** number of attachments on the message before ripping any more */
    private int _existingAttachments;
    
    private int _totalSize;
    private int _pendingAttachments;
    private int _state;
    
    private Set _allowedSuffixes;
    private static final Set IMAGE_SUFFIXES = new HashSet();
    private static final Set TORRENT_SUFFIXES = new HashSet();
    static {
        IMAGE_SUFFIXES.add(".png");
        IMAGE_SUFFIXES.add(".jpg");
        IMAGE_SUFFIXES.add(".jpeg");
        IMAGE_SUFFIXES.add(".gif");
        IMAGE_SUFFIXES.add(".ico");
        TORRENT_SUFFIXES.add(".torrent");
    }
    
    /** the rip has not yet been configured */
    public static final int STATE_INIT = 0;
    /** the rip has been configured */
    public static final int STATE_CONFIGURED = 1;
    /** the fetch process has begun */
    public static final int STATE_STARTED_FETCH_HTML = 2;
    /** the html has been fetched from the remote site */
    public static final int STATE_FETCH_HTML_COMPLETE = 3;
    /** the fetch of the attachments has begun */
    public static final int STATE_STARTED_FETCH_ATTACHMENTS = 5;
    /** all of the attachments have been fetched */
    public static final int STATE_FETCH_ATTACHMENTS_COMPLETE = 6;
    /** the page was fully rewritten to refer to the attachments */
    public static final int STATE_REWRITE_HTML_COMPLETE = 7;
    /** an error occurred during the rip */
    public static final int STATE_ERROR = -1;
    
    public WebRipRunner(String location, String proxy, int proxyPort, File tmpDir) {
        _location = location;
        _proxyHost = proxy;
        _proxyPort = proxyPort;
        _tmpDir = tmpDir;
        _totalSize = 0;
        _maxAttachKB = 64;
        _maxTotalKB = 4*1024;
        _pendingAttachments = 0;
        _state = STATE_INIT;
        _existingAttachments = 0;
        _attachmentFiles = new ArrayList();
        _attachmentURLRefs = new ArrayList();
        _attachmentURLs = new ArrayList();
        _otherURLRefs = new ArrayList();
        _errorMessages = new ArrayList();
        _exceptions = new ArrayList();
        _allowedSuffixes = new HashSet();
    }
    
    /**
     * @param images if true, fetch attachments that look like images (*.png, *.gif, etc)
     * @param torrents if true, fetch attachments ending in .torrent
     * @param maxAttachKB maximum file size to attach, in kilobytes
     * @param maxTotalKB maximum size of the html and all attachments, in kilobytes
     * @param allowFileURLs if true, fetch file attachments
     */
    public void configure(boolean images, boolean torrents, int maxAttachKB, int maxTotalKB, boolean allowFileURLs, int existingAttachments) {
        if (images) _allowedSuffixes.addAll(IMAGE_SUFFIXES);
        if (torrents) _allowedSuffixes.addAll(TORRENT_SUFFIXES);
        _maxAttachKB = maxAttachKB;
        _maxTotalKB = maxTotalKB;
        _allowFileURLs = allowFileURLs;
        setState(STATE_CONFIGURED);
    }

    /** fire up a new thread to run blockingRip() */
    public void nonblockingRip() {
        Thread t = new Thread(new Runnable() {
            public void run() { blockingRip(); }
        }, "WebRip");
        t.setDaemon(true);
        t.start();
    }
    
    /** blocking call to rip.  check getState from another thread for async progress info, or call
     * abortRip() from another thread to cancel it asap (though it won't abort immediately)
     */
    public void blockingRip() {
        try {
            _htmlFile = File.createTempFile("webrip", ".html", _tmpDir);
        } catch (IOException ioe) {
            fatal("Unable to create a temporary file", ioe);
            return;
        }
        setState(STATE_STARTED_FETCH_HTML);
        if (get(_htmlFile, _location)) {
            setState(STATE_FETCH_HTML_COMPLETE);
            _totalSize = (int)_htmlFile.length();
            String html = parse(_htmlFile);
            setState(STATE_STARTED_FETCH_ATTACHMENTS);
            fetchAttachments();
            setState(STATE_FETCH_ATTACHMENTS_COMPLETE);
            rewriteHTML(html);
            setState(STATE_REWRITE_HTML_COMPLETE);
        } else {
            fatal("Unable to retrieve the location [" + _location + "]", null);
        }
    }
    
    public List getErrorMessages() { return _errorMessages; }
    public List getExceptions() { return _exceptions; }
    public File getRewrittenFile() { return _htmlFile; }
    public List getAttachmentFiles() { return _attachmentFiles; }
    public List getAttachmentURLs() { return _attachmentURLs; }
    public int getState() { return _state; }
    /** how many attachments are pending fetch (only relevent during STATE_STARTED_FETCH_ATTACHMENTS) */
    public int getPendingAttachments() { return _pendingAttachments; }
    
    public void abortRip() {
        fatal("aborted by the user");
    }
    
    private void setState(int state) { if (_state != STATE_ERROR) _state = state; }
    
    /** delete all temporary files, including the rewritten html and attachments */
    public void cleanupData() {
        _htmlFile.delete();
        for (int i = 0; i < _attachmentFiles.size(); i++)
            ((File)_attachmentFiles.get(i)).delete();
    }
    
    private void rewriteHTML(String origHTML) {
        if (died()) return;
        String html = origHTML;
        // files that are bundled as attachments should have relative links
        for (int i = 0; i < _attachmentURLRefs.size(); i++) {
            String ref = (String)_attachmentURLRefs.get(i);
            SyndieURI uri = SyndieURI.createRelativeAttachment(_existingAttachments + 1 + i);
            String uriStr = uri.toString();
            // replace all instances of $ref with $uriStr
            html = Constants.replace(html, ref, uriStr);
        }
        // rewrite all refs that aren't bundled as attachments as absolute links
        for (int i = 0; i < _otherURLRefs.size(); i++) {
            String ref = (String)_otherURLRefs.get(i);
            SyndieURI uri = SyndieURI.createURL(getAbsolute(ref));
            String uriStr = uri.toString();
            // replace all instances of $ref with $uriStr
            html = Constants.replace(html, ref, uriStr);
        }
        FileOutputStream fos = null;
        try {
            fos = new FileOutputStream(_htmlFile);
            fos.write(DataHelper.getUTF8(html));
            fos.close();
            fos = null;
        } catch (IOException ioe) {
            fatal("error writing the rewritten html", ioe);
        } finally {
            if (fos != null) try { fos.close(); } catch (IOException ioe) {}
        }
    }
    
    private void fetchAttachments() {
        for (int i = 0; !died() && i < _attachmentURLs.size(); i++) {
            String url = (String)_attachmentURLs.get(i);
            String refURL = (String)_attachmentURLRefs.get(i);
            File file = (File)_attachmentFiles.get(i);
            if (!get(file, url)) {
                fatal("Unable to retrieve an attachment: [" + refURL + "] from [" + url + "]");
            } else {
                int size = (int)file.length();
                if ( (size > _maxAttachKB*1024) || (_totalSize + size > _maxTotalKB*1024) ) {
                    file.delete();
                    _attachmentFiles.remove(i);
                    _attachmentURLRefs.remove(i);
                    _attachmentURLs.remove(i);
                    _otherURLRefs.add(refURL);
                    i--;
                } else {
                    _totalSize += size;
                }
            }
            _pendingAttachments--;
        }
    }
    
    private boolean get(File file, String url) {
        if (_allowFileURLs && url.startsWith("file://")) {
            String f = url.substring("file://".length());
            FileInputStream fis = null;
            FileOutputStream fos = null;
            try {
                fis = new FileInputStream(f);
                fos = new FileOutputStream(file);
                byte buf[] = new byte[1024];
                int read = -1;
                while ( (read = fis.read(buf)) != -1)
                    fos.write(buf, 0, read);
                fis.close();
                fos.close();
                fis = null;
                fos = null;
                return true;
            } catch (IOException ioe) {
                fatal("error fetching from the file [" + f + "]", ioe);
                return false;
            } finally {
                if (fis != null) try { fis.close(); } catch (IOException ioe) {}
                if (fos != null) try { fos.close(); } catch (IOException ioe) {}
            }
        }
        EepGet get = new EepGet(I2PAppContext.getGlobalContext(), _proxyHost, _proxyPort, 0, file.getPath(), url, false);
        get.addStatusListener(this);
        boolean fetched = get.fetch(30*1000);
        if (fetched) {
            return true;
        } else {
            file.delete(); // delete partial fetch
            return false;
        }
    }
    
    private String parse(File htmlFile) {
        if (!htmlFile.exists()) {
            fatal("cannot parse " + htmlFile.toString());
            return "";
        }
        String html = read(htmlFile);
        if (html == null) {
            return null;
        } else {
            HTMLStateBuilder builder = new HTMLStateBuilder(new StdoutUI(), html);
            builder.buildState();
            List tags = builder.getTags();
            parseTags(tags);
            return html;
        }
    }
    
    private void parseTags(List tags) {
        if (tags != null) {
            for (int i = 0; i < tags.size(); i++) {
                HTMLTag tag = (HTMLTag)tags.get(i);
                if ("a".equals(tag.name))
                    parseLink(tag.attributes.getProperty("href"));
                else if ("img".equals(tag.name))
                    parseRef(tag.attributes.getProperty("src"));
                if (died())
                    break;
            }
        }
    }
    
    private void parseLink(String ref) {
        if (ref == null) return;
        if (shouldFetchLink(ref))
            parseRef(ref);
        else if (!_otherURLRefs.contains(ref))
            _otherURLRefs.add(ref);
    }
    private void parseRef(String ref) {
        if (ref == null) return;
        String absoluteRef = getAbsolute(ref);
        scheduleFetch(absoluteRef, ref);
    }
    private boolean shouldFetchLink(String ref) {
        String lc = Constants.lowercase(ref);
        String suffix = null;
        int point = lc.lastIndexOf('.');
        if ( (point > 0) && (point + 1 < lc.length()) )
            suffix = lc.substring(point+1);
        return (_allowedSuffixes.contains(suffix));
    }
    
    private String getAbsolute(String ref) {
        if (ref.startsWith("http://") || ref.startsWith("https://")) {
            return ref;
        } else if (ref.startsWith("/")) {
            return getAbsoluteHost() + ref;
        } else if (ref.startsWith("#")) {
            return null;
        } else {
            return getAbsoluteHost() + getRelativeParent() + ref;
        }
    }
    
    private String getAbsoluteHost() { 
        try {
            URI uri = new URI(_location);
            String authority = uri.getAuthority();
            if (authority == null)
                authority = "";
            String scheme = uri.getScheme();
            //return "http://some.host.com:1234"; 
            return scheme + "://" + authority;
        } catch (URISyntaxException use) {
            return null;
        }
    }
    
    private String getRelativeParent() { 
        try {
            URI uri = new URI(_location);
            String path = uri.getPath();
            //String path = "/my/path/foo.html";
            if (path.endsWith("/")) {
                return path;
            } else {
                int end = path.lastIndexOf('/');
                if (end <= 0) {
                    return "/";
                } else {
                    String str = path.substring(0, end);
                    if (str.endsWith("/"))
                        return str;
                    else
                        return str + "/";
                }
            }
        } catch (URISyntaxException use) {
            return null;
        }
    }

    private void scheduleFetch(String absoluteURL, String relative) {
        if ( (absoluteURL != null) && (!_attachmentURLRefs.contains(relative)) ) {
            try {
                File f = File.createTempFile("webrip", ".attach", _tmpDir);
                _attachmentFiles.add(f);
                _attachmentURLs.add(absoluteURL);
                _attachmentURLRefs.add(relative);
                _pendingAttachments++;
            } catch (IOException ioe) {
                fatal("error creating temporary file for attachment", ioe);
            }
        }
    }
    
    private boolean died() { return _errorMessages.size() > 0; }
    private void fatal(String msg) { fatal(msg, null); }
    private void fatal(String msg, Exception e) {
        if (msg != null)
            _errorMessages.add(msg);
        if (e != null)
            _exceptions.add(e);
    }
    
    public void bytesTransferred(long alreadyTransferred, int currentWrite, long bytesTransferred, long bytesRemaining, String url) {}
    public void transferComplete(long alreadyTransferred, long bytesTransferred, long bytesRemaining, String url, String outputFile, boolean notModified) {}
    public void attemptFailed(String url, long bytesTransferred, long bytesRemaining, int currentAttempt, int numRetries, Exception cause) {}
    public void transferFailed(String url, long bytesTransferred, long bytesRemaining, int currentAttempt) {}
    public void headerReceived(String url, int currentAttempt, String key, String val) {}
    public void attempting(String url) {}
    
    public static void main(String args[]) {
        WebRipRunner runner = new WebRipRunner("file:///tmp/webrip/src/index.html", null, -1, new File("/tmp/webrip/tmp"));
        runner.configure(true, true, 16, 64, true, 0);
        runner.blockingRip();
        List err = runner.getErrorMessages();
        List exc = runner.getExceptions();
        List urls= runner.getAttachmentURLs();
        String rewritten = runner.read(runner.getRewrittenFile());
        System.out.println("Errors: " + err);
        for (int i = 0; i < exc.size(); i++) {
            Exception e = (Exception)exc.get(i);
            e.printStackTrace();
        }
        System.out.println("URLs: " + urls);
        System.out.println("rewritten html: " + rewritten);
        System.out.println("attachments fetched for: " + runner.getAttachmentURLs());
        runner.cleanupData();
    }
    
    private String read(File f) {
        BufferedReader in = null;
        try {
            in = new BufferedReader(new InputStreamReader(new FileInputStream(f), "UTF-8"));
            String line = null;
            StringBuffer buf = new StringBuffer((int)f.length());
            while ( (line = in.readLine()) != null)
                buf.append(line).append('\n');
            in.close();
            in = null;
            return buf.toString();
        } catch (IOException ioe) {
            fatal("error reading " + f.getPath(), ioe);
            return null;
        } finally {
            if (in != null) try { in.close(); } catch (IOException ioe) {}
        }
    }
    
    private class StdoutUI implements UI {
        public Opts readCommand() { return null; }
        public Opts readCommand(boolean displayPrompt) { return null; }
        public void errorMessage(String msg) {
            System.err.println(msg);
        }

        public void errorMessage(String msg, Exception cause) {
            System.err.println(msg);
            if (cause != null)
                cause.printStackTrace();
        }

        public void statusMessage(String msg) {
            System.out.println(msg);
        }

        public void debugMessage(String msg) {
            System.out.println(msg);
        }

        public void debugMessage(String msg, Exception cause) {
            System.out.println(msg);
            if (cause != null)
                cause.printStackTrace();
        }

        public void commandComplete(int status, List location) {}
        public boolean toggleDebug() { return true; }
        public boolean togglePaginate() { return true; }
        public void insertCommand(String commandline) {}
        public String readStdIn() { return ""; }
    }
}
