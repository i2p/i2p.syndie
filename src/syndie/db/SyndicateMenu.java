package syndie.db;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import net.i2p.data.*;
import net.i2p.util.EepGet;
import syndie.Constants;
import syndie.data.SyndieURI;

/**
 *
 */
class SyndicateMenu implements TextEngine.Menu {
    private TextEngine _engine;
    private SharedArchive _currentIndex;
    //private ArchiveDiff _diff;
    //private HTTPSyndicator _syndicator;
    private String _baseUrl;
    private String _proxyHost;
    private int _proxyPort;
    private boolean _shouldProxy;
    private boolean _archiveWasRemote;
    private int _curPBEIndex;
    
    public SyndicateMenu(TextEngine engine) {
        _engine = engine;
    }
    
    public static final String NAME = "syndicate";
    public String getName() { return NAME; }
    public String getDescription() { return "syndication menu"; }
    public boolean requireLoggedIn() { return true; }
    public void listCommands(UI ui) {
        ui.statusMessage(" buildindex         : create or update the current archive's index");
        ui.statusMessage(" getindex --archive $url [--proxyHost $host --proxyPort $port] [--pass $pass]");
        ui.statusMessage("          [--scope (all|new|meta|unauth)]");
        ui.statusMessage("                    : fetch the appropriate index from the archive");
        ui.statusMessage(" diff [--maxSize $numBytes]");
        ui.statusMessage("                    : summarize the differences between the fetched index and the local db");
        ui.statusMessage(" fetch [--style (diff|known|metaonly|pir|unauth)] [--includeReplies $boolean] [--maxSize $numBytes]");
        ui.statusMessage("                    : actually fetch the posts/replies/metadata");
        ui.statusMessage(" nextpbe [--lines $num]");
        ui.statusMessage(" prevpbe [--lines $num]");
        ui.statusMessage("                    : paginate through the messages using passphrase based encryption");
        ui.statusMessage(" resolvepbe --index $num --passphrase $passphrase");
        ui.statusMessage("                    : import the indexed message by using the specified passphrase");
        ui.statusMessage(" schedule --put (outbound|outboundmeta|archive|archivemeta) [--deleteOutbound $boolean] [--knownChanOnly $boolean]");
        ui.statusMessage("                    : schedule a set of messages to be posted");
        ui.statusMessage(" put                : send up the scheduled posts/replies/metadata to the archive");
        ui.statusMessage(" bulkimport --dir $directory --delete $boolean --rmdir $boolean");
        ui.statusMessage("                    : import all of the " + Constants.FILENAME_SUFFIX + " files in the given directory, deleting them on completion");
        ui.statusMessage(" freenetpost --privateSSK ($key|new) [--fcpHost localhost] [--fcpPort 9481]");
        ui.statusMessage("                    : post the entire local archive into Freenet, storing the data either in the");
        ui.statusMessage("                    : given SSK or in a brand new SSK, as requested.");
        ui.statusMessage(" listban            : list the channels currently banned in the local archive");
        ui.statusMessage(" unban [--scope $index|$chanHash]");
    }
    public boolean processCommands(DBClient client, UI ui, Opts opts) {
        String cmd = opts.getCommand();
        if ("buildindex".equalsIgnoreCase(cmd)) {
            //_mgr = SyndicationManager.getInstance(client, ui);
            //_mgr.loadArchives();
            SyncManager mgr = SyncManager.getInstance(client, ui);
            processBuildIndex(client, ui, opts, mgr.getDefaultPullStrategy());
        } else if ("getindex".equalsIgnoreCase(cmd)) {
            processGetIndex(client, ui, opts);
        } else if ("diff".equalsIgnoreCase(cmd)) {
            processDiff(client, ui, opts);
        } else if ("fetch".equalsIgnoreCase(cmd)) {
            processFetch(client, ui, opts);
        } else if ("nextpbe".equalsIgnoreCase(cmd)) {
            processNextPBE(client, ui, opts);
        } else if ("prevpbe".equalsIgnoreCase(cmd)) {
            processPrevPBE(client, ui, opts);
        } else if ("resolvepbe".equalsIgnoreCase(cmd)) {
            processResolvePBE(client, ui, opts);
        } else if ("schedule".equalsIgnoreCase(cmd)) {
            processSchedule(client, ui, opts);
        } else if ("put".equalsIgnoreCase(cmd)) {
            processPut(client, ui, opts);
        } else if ("bulkimport".equalsIgnoreCase(cmd)) {
            processBulkImport(client, ui, opts);
        } else if ("freenetpost".equalsIgnoreCase(cmd)) {
            processFreenetPost(client, ui, opts);
        } else if ("listban".equalsIgnoreCase(cmd)) {
            processListBan(client, ui, opts);
        } else if ("unban".equalsIgnoreCase(cmd)) {
            processUnban(client, ui, opts);
        } else {
            return false;
        }
        return true;
    }
    public List getMenuLocation(DBClient client, UI ui) {
        List rv = new ArrayList();
        rv.add("syndicate");
        return rv;
    }
    
    /**
     * getindex --archive $url [--proxyHost $host --proxyPort $port] [--pass $pass]
     *          [--scope (all|new|meta)]
     */
    private void processGetIndex(DBClient client, UI ui, Opts opts) {
        if (true) {
            ui.errorMessage("Currently disabled in the text interface");
            ui.commandComplete(-1, null);
            return;
        }
        //_diff = null;
        //_syndicator = null; // delete files?
        _baseUrl = opts.getOptValue("archive");
        if (_baseUrl == null)
            _baseUrl = client.getDefaultHTTPArchive();
        if (_baseUrl == null) {
            ui.errorMessage("The archive url is required.  Usage: ");
            ui.errorMessage("getindex --archive $url [--proxyHost $host --proxyPort $port] [--pass $pass] [--scope (all|new|meta|unauth)] [--channel $chan]");
            ui.commandComplete(-1, null);
            return;
        }
        _proxyHost = opts.getOptValue("proxyHost");
        _proxyPort = (int)opts.getOptLong("proxyPort", -1);
        if ( ( (_proxyHost == null) || (_proxyPort <= 0) ) &&
             ( (client.getDefaultHTTPProxyHost() != null) && (client.getDefaultHTTPProxyPort() > 0) ) ) {
            _proxyHost = client.getDefaultHTTPProxyHost();
            _proxyPort = client.getDefaultHTTPProxyPort();
        }
        
        int keyStart = -1;
        keyStart = _baseUrl.indexOf("SSK@");
        if (keyStart < 0) {
            keyStart = _baseUrl.indexOf("USK@");
            if (keyStart < 0) {
                keyStart = _baseUrl.indexOf("CHK@");
            }
        }
	boolean includeForceDownload = false;
        if (keyStart >= 0) {
            String fproxyHost = _proxyHost;
            int fproxyPort = _proxyPort;
            if (fproxyHost == null)
                fproxyHost = "127.0.0.1";
            if (fproxyPort <= 0)
                fproxyPort = 8888;
            _proxyHost = null;
            _proxyPort = -1;
            _baseUrl = "http://" + fproxyHost + ":" + fproxyPort + "/" + _baseUrl.substring(keyStart);
	    includeForceDownload = true;
        }
        
        
        boolean unauth = false;
        String scope = opts.getOptValue("scope");
        String url = null;
        if (scope == null)
            scope = "all";
        if (!_baseUrl.endsWith("/"))
            _baseUrl = _baseUrl + "/";
        /*
        if ("new".equalsIgnoreCase(scope)) {
            url = _baseUrl + "index-new.dat";
        } else if ("meta".equalsIgnoreCase(scope)) {
            url = _baseUrl + "index-meta.dat";
        } else if ("unauth".equalsIgnoreCase(scope)) {
            unauth = true;
            String chan = opts.getOptValue("channel");
            if (chan != null) {
                url = _baseUrl + chan + "/index-unauthorized.dat";
            } else {
                url = _baseUrl + "index-unauthorized.dat";
            }
        } else { //if ("all".equalsIgnoreCase(scope))
        */
        
        // deal with baseurls like http://foo.i2p/?i2paddresshelper=blah
        int q = _baseUrl.indexOf('?');
        if (q > 0)
            url = _baseUrl.substring(0, q) + LocalArchiveManager.SHARED_INDEX_FILE + _baseUrl.substring(q);
        else
            url = _baseUrl + LocalArchiveManager.SHARED_INDEX_FILE;
        //url = _baseUrl + LocalArchiveManager.SHARED_INDEX_FILE; //"index-all.dat";
	if (includeForceDownload) {
            if (q > 0)
                url = url + "&forcedownload";
            else
                url = url + "?forcedownload";
        }

        _shouldProxy = (_proxyHost != null) && (_proxyPort > 0);
        _archiveWasRemote = true;
        File out = null;
        if (_baseUrl.startsWith("/")) {
            out = new File(url);
            _archiveWasRemote = false;
        } else if (_baseUrl.startsWith("file://")) {
            out = new File(_baseUrl.substring("file://".length()));
            _archiveWasRemote = false;
        } else {
            try {
                out = File.createTempFile("syndicate", ".index", client.getTempDir());
                EepGet get = new EepGet(client.ctx(), _shouldProxy, _proxyHost, (int)_proxyPort, 0, out.getPath(), url, false, null, null);
                get.addStatusListener(new UIStatusListener(ui));
                boolean fetched = get.fetch();
                if (!fetched) {
                    ui.errorMessage("Fetch failed of " + url);
                    ui.commandComplete(-1, null);
                    return;
                }
                ui.statusMessage("Fetch complete");
            } catch (IOException ioe) {
                ui.errorMessage("Error pulling the index", ioe);
                ui.commandComplete(-1, null);
            }
        }
        try {
            SharedArchive index = new SharedArchive();
            FileInputStream fin = new FileInputStream(out);
            index.read(fin);
            fin.close();
            ui.statusMessage("Fetched shared archive index");
            _currentIndex = index;
            /*
            ArchiveIndex index = ArchiveIndex.loadIndex(out, ui, unauth);
            if (index != null) {
                ui.statusMessage("Fetched archive loaded with " + index.getChannelCount() + " channels");
                _currentIndex = index;
                _syndicator = new HTTPSyndicator(_baseUrl, _proxyHost, _proxyPort, client, ui, _currentIndex, opts.getOptBoolean("reimport", false));
                processDiff(client, ui, opts);
            } else {
                ui.errorMessage("Unable to load the fetched archive");
            }
             */
            ui.commandComplete(0, null);
        } catch (IOException ioe) {
            ui.errorMessage("Error loading the index", ioe);
            ui.commandComplete(-1, null);
        }
        if (_archiveWasRemote && out != null)
            out.delete();
    }
    
    private static class UIStatusListener implements EepGet.StatusListener {
        private UI _ui;
        public UIStatusListener(UI ui) { _ui = ui; }
        public void bytesTransferred(long alreadyTransferred, int currentWrite, long bytesTransferred, long bytesRemaining, String url) {
            _ui.debugMessage("Transferred: " + bytesTransferred);
        }
        public void transferComplete(long alreadyTransferred, long bytesTransferred, long bytesRemaining, String url, String outputFile, boolean notModified) {
            _ui.debugMessage("Transfer complete: " + bytesTransferred);
        }
        public void attemptFailed(String url, long bytesTransferred, long bytesRemaining, int currentAttempt, int numRetries, Exception cause) {
            _ui.debugMessage("Transfer attempt failed: " + bytesTransferred, cause);
        }
        public void transferFailed(String url, long bytesTransferred, long bytesRemaining, int currentAttempt)  {
            _ui.statusMessage("Transfer totally failed of " + url);
        }
        public void headerReceived(String url, int currentAttempt, String key, String val)  {
            _ui.debugMessage("Header received: " + key + "=" + val);
        }
        public void attempting(String url) {
            _ui.statusMessage("Fetching " + url + "...");
        }
    }
    
    private void processDiff(DBClient client, UI ui, Opts opts) {
        if (true) {
            ui.errorMessage("Currently disabled in the text interface");
            ui.commandComplete(-1, null);
            return;
        }
        if (_currentIndex == null) {
            ui.errorMessage("No index loaded");
            ui.commandComplete(-1, null);
            return;
        }
        SharedArchiveEngine.PullStrategy strategy = new SharedArchiveEngine.PullStrategy();
        strategy.includeDupForPIR = false;
        strategy.includePBEMessages = true;
        strategy.includePrivateMessages = true;
        strategy.includeRecentMessagesOnly = true;
        strategy.maxKBPerMessage = 512;
        strategy.maxKBTotal = 4096;
        SharedArchiveEngine engine = new SharedArchiveEngine();
        List uris = engine.selectURIsToPull(client, ui, _currentIndex, strategy, -1);
        //List uris = _currentIndex.selectURIsToPull(client, ui, strategy);
        int msgs = 0;
        int meta = 0;
        for (int i = 0; i < uris.size(); i++) {
            if (((SyndieURI)uris.get(i)).getMessageId() == null)
                meta++;
            else
                msgs++;
        }
        ui.statusMessage("New messages in the remote archive: " + msgs);
        ui.statusMessage("New or updated forums in the remote archive: " + meta);
        /*
        long maxSize = opts.getOptLong("maxSize", ArchiveIndex.DEFAULT_MAX_SIZE);
        if ( (_diff == null) || (maxSize != _diff.maxSizeUsed) ) {
            _diff = _currentIndex.diff(client, ui, opts);
        }
        StringBuffer buf = new StringBuffer();
        if (_diff != null) {
            if (_diff.fetchNewUnauthorizedBytes > 0) {
                buf.append("Unauthorized posts the remote archive has that we do not:\n");
                buf.append("- ").append(_diff.fetchNewUnauthorizedMetadata.size()).append(" new channels\n");
                buf.append("- ").append(_diff.fetchNewUnauthorizedPosts.size()).append(" new posts\n");
                buf.append("- ").append(_diff.fetchNewUnauthorizedReplies.size()).append(" new replies\n");
                buf.append("To fetch all new unauthorized data, syndie would download:\n");
                buf.append("- ").append((_diff.fetchNewUnauthorizedBytes+1023)/1024).append(" kilobytes\n");
            } else {
                buf.append("Things the remote archive has that we do not:\n");
            
                buf.append("- ").append(_diff.totalNewChannels).append(" new channels including ");
                buf.append(_diff.totalNewMessages).append(" new messages\n");
                
                buf.append("- ").append(_diff.totalNewMessagesOnKnownChannels).append(" new messages on ");
                buf.append(_diff.totalKnownChannelsWithNewMessages).append(" channels we already know\n");
                
                buf.append("- ").append(_diff.totalUpdatedChannels).append(" updated channels\n");
                
                buf.append("To fetch all new posts and metadata, syndie would download:\n");
                buf.append("- ").append((_diff.fetchNewBytes+1023)/1024).append(" kilobytes in ");
                buf.append(_diff.fetchNewMetadata.size()).append(" metadata messages, ");
                buf.append(_diff.fetchNewPosts.size()).append(" posts, and ");
                buf.append(_diff.fetchNewReplies.size()).append(" private replies\n");
                
                buf.append("To fetch all new posts and metadata for locally known channels, syndie would download:\n");
                buf.append("- ").append((_diff.fetchKnownBytes+1023)/1024).append(" kilobytes in ");
                buf.append(_diff.fetchKnownMetadata.size()).append(" metadata messages, ");
                buf.append(_diff.fetchKnownPosts.size()).append(" posts, and ");
                buf.append(_diff.fetchKnownReplies.size()).append(" private replies\n");
                
                buf.append("To fetch only the updated metadata, syndie would download:\n");
                buf.append("- ").append((_diff.fetchMetaBytes+1023)/1024).append(" kilobytes in ");
                buf.append(_diff.fetchMetaMessages.size()).append(" metadata messages\n");
                
                buf.append("To avoid certain types of profiling, syndie would download:\n");
                buf.append("- ").append((_diff.fetchPIRBytes+1023)/1024).append(" kilobytes in ");
                buf.append(_diff.fetchPIRMetadata.size()).append(" metadata messages, ");
                buf.append(_diff.fetchPIRPosts.size()).append(" posts, and ");
                buf.append(_diff.fetchPIRReplies.size()).append(" private replies\n");
            }
        }
        ui.statusMessage(buf.toString());
         */
        ui.commandComplete(0, null);
    }
    
    private void processFetch(DBClient client, UI ui, Opts opts) {
        if (true) {
            ui.errorMessage("Currently disabled in the text interface");
            ui.commandComplete(-1, null);
            return;
        }
        if (_currentIndex == null) {
            ui.errorMessage("No archive fetched");
            ui.commandComplete(-1, null);
            return;
        }
        
        boolean includeReplies = opts.getOptBoolean("includeReplies", true);
        String style = opts.getOptValue("style");
        if (style == null)
            style = "diff";
        SharedArchiveEngine.PullStrategy strategy = new SharedArchiveEngine.PullStrategy();
        List uris = null;
        if ("known".equalsIgnoreCase(style)) {
            strategy.includePrivateMessages = includeReplies;
            strategy.includeRecentMessagesOnly = true;
            strategy.includePBEMessages = true;
            strategy.knownChannelsOnly = true;
        } else if ("metaonly".equalsIgnoreCase(style)) {
            strategy.includePrivateMessages = includeReplies;
            strategy.includeRecentMessagesOnly = true;
            strategy.includePBEMessages = false;
            strategy.knownChannelsOnly = false;
        } else if ("pir".equalsIgnoreCase(style)) {
            strategy.includeDupForPIR = true;
        //} else if ("unauth".equalsIgnoreCase(style)) {
        //    uris = _diff.getFetchNewUnauthorizedURIs(includeReplies);
        } else { // "diff" as the default
            strategy.includePrivateMessages = includeReplies;
            strategy.includeRecentMessagesOnly = true;
            strategy.includePBEMessages = true;
            strategy.knownChannelsOnly = false;
        }
        
        SharedArchiveEngine engine = new SharedArchiveEngine();
        
        uris = engine.selectURIsToPull(client, ui, _currentIndex, strategy, -1);
        //uris = _currentIndex.selectURIsToPull(client, ui, strategy);
        ui.debugMessage("Fetching " + uris.size() + " entries: " + uris);
        
        boolean ok = false; //_syndicator.fetch(uris);
        if (ok) {
            ui.debugMessage("Messages fetched.  Importing...");
            int imported = 0; //_syndicator.importFetched();
            int missing = 0; //_syndicator.countMissingPassphrases();
            if (missing > 0) {
                ui.statusMessage("Some messages could not be imported as they require a passphrase to read.");
                ui.statusMessage("To import these " + missing + " messages, please review them with");
                ui.statusMessage("the 'nextpbe' command and import them with the 'resolvepbe' command");
            }
            ui.commandComplete(0, null);
        } else {
            ui.statusMessage("Fetch failed");
            ui.commandComplete(-1, null);
        }
    }
    
    private void processNextPBE(DBClient client, UI ui, Opts opts) {
        if (true) {
            ui.errorMessage("Currently disabled in the text interface");
            ui.commandComplete(-1, null);
            return;
        }
        /*
        if (_syndicator == null) {
            ui.errorMessage("No syndication in progress");
            ui.commandComplete(0, null);
            return;
        }
        int total = _syndicator.countMissingPassphrases();
        int pass = 10;
        if (_curPBEIndex + pass > total)
            pass = total - _curPBEIndex;
        for (int i = 0; i < pass; i++) {
            String prompt = _syndicator.getMissingPrompt(_curPBEIndex+i);
            SyndieURI uri = _syndicator.getMissingURI(_curPBEIndex+i);
            if (uri.getMessageId() == null)
                ui.statusMessage((i + _curPBEIndex) + ": Metadata for " + uri.getScope().toBase64() + " requires: ");
            else
                ui.statusMessage((i + _curPBEIndex) + ": Message " + uri.getMessageId().longValue() + " in " + uri.getScope().toBase64() + " requires: ");
            ui.statusMessage("\t" + CommandImpl.strip(prompt));
        }
        ui.commandComplete(0, null);
         */
    }
    private void processPrevPBE(DBClient client, UI ui, Opts opts) {
        if (true) {
            ui.errorMessage("Currently disabled in the text interface");
            ui.commandComplete(-1, null);
            return;
        }
        _curPBEIndex -= 10;
        if (_curPBEIndex < 0)
            _curPBEIndex = 0;
        processNextPBE(client, ui, opts);
    }
    private void processResolvePBE(DBClient client, UI ui, Opts opts) {
        if (true) {
            ui.errorMessage("Currently disabled in the text interface");
            ui.commandComplete(-1, null);
            return;
        }
        int index = (int)opts.getOptLong("index", 0);
        String pass = opts.getOptValue("passphrase");
        //_syndicator.importPBE(index, pass);
    }
    
    private void processSchedule(DBClient client, UI ui, Opts opts) {
        if (true) {
            ui.errorMessage("Currently disabled in the text interface");
            ui.commandComplete(-1, null);
            return;
        }
        /*
        String style = opts.getOptValue("put");
        if (style == null) {
            ui.errorMessage("Usage: schedule --put (outbound|outboundmeta|archive|archivemeta) [--deleteOutbound $boolean]");
            ui.commandComplete(-1, null);
            return;
        } else if (_syndicator == null) {
            ui.errorMessage("An archive's index must be fetched before scheduling updates");
            ui.commandComplete(-1, null);
            return;
        }
        boolean deleteOutbound = opts.getOptBoolean("deleteOutbound", true);
        boolean knownChanOnly = opts.getOptBoolean("knownChanOnly", false);
        //_syndicator.setDeleteOutboundAfterSend(deleteOutbound);
        //_syndicator.schedulePut(style, knownChanOnly);
        ui.statusMessage("Posting scheduled");
        ui.commandComplete(0, null);
         */
    }

    private void processPut(DBClient client, UI ui, Opts opts) {
        if (true) {
            ui.errorMessage("Currently disabled in the text interface");
            ui.commandComplete(-1, null);
            return;
        }
        /*
        String url = opts.getOptValue("postURL");
        if (url != null)
            _syndicator.setPostURLOverride(url);
        String pass = opts.getOptValue("passphrase");
        if (pass != null)
            _syndicator.setPostPassphrase(pass);
        _syndicator.post();
        _syndicator = null;
         */
    }
    
    /** bulkimport --dir $directory --delete $boolean --rmdir $boolean*/
    private void processBulkImport(DBClient client, UI ui, Opts opts) {
        String dir = opts.getOptValue("dir");
        boolean del = opts.getOptBoolean("delete", true);
        boolean rmdir = opts.getOptBoolean("rmdir", false);
        
        if (dir == null) {
            ui.errorMessage("Usage: bulkimport --dir $directory --delete $boolean --rmdir $boolean");
            ui.commandComplete(-1, null);
            return;
        }
        
        int metaImported = 0;
        int postImported = 0;
        
        File f = new File(dir);
        File files[] = f.listFiles(_metafilter);
        SharedArchiveBuilder.sortFiles(files);
        for (int i = 0; files != null && i < files.length; i++) {
            importMsg(client, ui, files[i], opts.getOptBoolean("reimport", false));
            if (del) {
                boolean deleted = files[i].delete();
                if (!deleted)
                    ui.statusMessage("Unable to delete " + files[i].getPath());
                else
                    ui.statusMessage("Metadata deleted from " + files[i].getPath());
            }
            metaImported++;
        }
        
        files = f.listFiles(_postfilter);
        SharedArchiveBuilder.sortFiles(files);
        for (int i = 0; files != null && i < files.length; i++) {
            importMsg(client, ui, files[i], opts.getOptBoolean("reimport", false));
            if (del) {
                boolean deleted = files[i].delete();
                if (!deleted)
                    ui.statusMessage("Unable to delete " + files[i].getPath());
                else
                    ui.statusMessage("Post deleted from " + files[i].getPath());
            }
            postImported++;
        }
        
        if (del && rmdir)
            f.delete();
        ui.statusMessage("Imported " + metaImported + " metadata and " + postImported + " posts");
        ui.commandComplete(0, null);
    }
    
    private void importMsg(DBClient client, UI ui, File f, boolean forceReimport) {
        Importer imp = new Importer(client, client.getPass());
        ui.debugMessage("Importing from " + f.getPath());
        boolean ok;
        try {
            NestedUI nested = new NestedUI(ui);
            ok = imp.processMessage(nested, new FileInputStream(f), client.getLoggedInNymId(), client.getPass(), null, forceReimport, null, null);
            if (ok && (nested.getExitCode() >= 0) ) {
                if (nested.getExitCode() == 1) {
                    ui.errorMessage("Imported but could not decrypt " + f.getPath());
                } else {
                    ui.debugMessage("Import successful for " + f.getPath());
                }
            } else {
                ui.debugMessage("Could not import " + f.getPath());
            }
        } catch (IOException ioe) {
            ui.errorMessage("Error importing the message from " + f.getPath(), ioe);
        }
    }
    
    private static MetaFilter _metafilter = new MetaFilter();
    private static class MetaFilter implements FilenameFilter {
        public boolean accept(File dir, String name) {
            return name.startsWith("meta") && name.endsWith(Constants.FILENAME_SUFFIX);
        }
    }
    private static PostFilter _postfilter = new PostFilter();
    private static class PostFilter implements FilenameFilter {
        public boolean accept(File dir, String name) {
            return (!name.startsWith("post")) && name.endsWith(Constants.FILENAME_SUFFIX);
        }
    }

    /** freenetpost --privateSSK ($key|new) [--fcpHost localhost] [--fcpPort 9481] */
    private void processFreenetPost(DBClient client, UI ui, Opts opts) {
        String fcpHost = opts.getOptValue("fcpHost");
        if (fcpHost == null)
            fcpHost = client.getDefaultFreenetHost();
        if (fcpHost == null)
            fcpHost = "localhost";
        int fcpPort = (int)opts.getOptLong("fcpPort", -1);
        if (fcpPort <= 0)
            fcpPort = client.getDefaultFreenetPort();
        if (fcpPort <= 0)
            fcpPort = 9481;
        String ssk = opts.getOptValue("privateSSK");
        if (ssk == null)
            ssk = client.getDefaultFreenetPrivateKey();
        if (ssk == null)
            ssk = "new";

        
        SyncManager mgr = SyncManager.getInstance(client, ui);
        processBuildIndex(client, new NestedUI(ui), new Opts(), mgr.getDefaultPullStrategy());
        // ignore the results

        //FreenetArchivePusher pusher = new FreenetArchivePusher(ui, fcpHost, fcpPort);
        if ("new".equalsIgnoreCase(ssk)) {
            //pusher.generateSSK();
            ssk = null; //pusher.getPrivateSSK();
            if (ssk != null) {
                String pubSSK = null; //pusher.getPublicSSK();
                ui.statusMessage("Published results will be visible under " + pubSSK);
                // save the ssk to the prefs at the next opportunity
                ui.insertCommand("prefs --freenetPublicKey " + CommandImpl.strip(pubSSK) +
                                 " --freenetPrivateKey " + CommandImpl.strip(ssk) + 
                                 " --fcpHost " + CommandImpl.strip(fcpHost) + 
                                 " --fcpPort " + fcpPort);
            } else {
                ui.errorMessage("Unable to post the archive to Freenet");
                ui.commandComplete(-1, null);
                return;
            }
        } else {
            //pusher.setPrivateSSK(ssk);
            //pusher.setPublicSSK(client.getDefaultFreenetPublicKey());
        }
        //pusher.putArchive(client.getArchiveDir());
    }
    
    
    private void processListBan(DBClient client, UI ui, Opts opts) {
        List chans = client.getBannedChannels();
        ui.statusMessage("Total of " + chans.size() + " banned channels");
        for (int i = 0; i < chans.size(); i++) {
            Hash chan = (Hash)chans.get(i);
            ui.statusMessage(i + ": banned channel " + chan.toBase64());
        }
        ui.commandComplete(0, null);
    }
    private void processUnban(DBClient client, UI ui, Opts opts) {
        String scope = opts.getOptValue("scope");
        if (scope == null) {
            ui.errorMessage("Usage: unban [--scope $index|$chanHash]");
            ui.commandComplete(0, null);
            return;
        }
        int index = (int)opts.getOptLong("scope", -1);
        if (index >= 0) {
            List chans = client.getBannedChannels();
            if (index >= chans.size()) {
                ui.errorMessage("Channel out of range - only " + chans.size() + " banned channels");
                ui.commandComplete(-1, null);
                return;
            } else {
                Hash chan = (Hash)chans.get(index);
                client.unban(chan);
                ui.statusMessage("Channel " + chan.toBase64() + " unbanned");
                ui.commandComplete(0, null);
                return;
            }
        } else {
            byte chan[] = Base64.decode(scope);
            if ( (chan != null) && (chan.length == Hash.HASH_LENGTH) ) {
                client.unban(new Hash(chan));
                ui.statusMessage("Channel " + scope + " unbanned");
                ui.commandComplete(0, null);
            } else {
                ui.errorMessage("Channel specified is not valid [" + scope + "]");
                ui.commandComplete(-1, null);
            }
        }
    }
    
    private void processBuildIndex(DBClient client, UI ui, Opts opts, SharedArchiveEngine.PullStrategy pullStrategy) {
        //long maxSize = opts.getOptLong("maxSize", ArchiveIndex.DEFAULT_MAX_SIZE);
        LocalArchiveManager.buildIndex(client, ui, pullStrategy); //, maxSize);
        ui.commandComplete(0, null);
    }
}
