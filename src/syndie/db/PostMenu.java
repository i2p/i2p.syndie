package syndie.db;

import java.io.*;
import java.net.URISyntaxException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.text.ParseException;
import java.util.*;

import net.i2p.crypto.KeyGenerator;
import net.i2p.data.Base64;
import net.i2p.data.DataHelper;
import net.i2p.data.SessionKey;
import net.i2p.data.PrivateKey;
import net.i2p.data.PublicKey;
import net.i2p.data.SigningPrivateKey;
import net.i2p.data.SigningPublicKey;
import net.i2p.data.Signature;
import net.i2p.data.Hash;
import net.i2p.util.SecureFile;
import net.i2p.util.SecureFileOutputStream;

import syndie.Constants;
import syndie.data.*;

/**
 *
 */
class PostMenu implements TextEngine.Menu {
    private TextEngine _engine;
    /** text description of each indexed channel */
    private List _itemText;
    /** internal channel id (Long) for each indexed item */
    private List _itemKeys;
    /** if true, the items refer to a list of channels matching the requested criteria */
    private boolean _itemIsChannelList;
    /** refers to the next index into the item lists that the user should be shown */
    private int _itemIteratorIndex;
    /** current message the user is working on (if any) */
    private MessageInfo _currentMessage;
    /** current list of file names to use as pages */
    private List _pageFiles;
    /** current list of config (Properties) for each page */
    private List _pageConfig;
    /** current list of file names to use as attachments */
    private List _attachmentFiles;
    /** current list of config (Properties) for each attachment */
    private List _attachmentConfig;
    /** filename to pull the channel avatar from */
    private String _avatarFile;
    /** nym keys being listed */
    private List _listedNymKeys;
    /** how we should prove who we are */
    private NymKey _authenticationKey;
    /** how we should prove we are allowed to post in the target channel */
    private NymKey _authorizationKey;
    /** list of references (ReferenceNode) to be delivered with the message */
    private List _referenceNodes;
    /** list of parents (SyndieURI) of this message, with the most recent parent at index 0 */
    private List _parents;
    /** use a publicly visible encryption key for the post so anyone can read it */
    private Boolean _publiclyReadable;
    /** private read key to use when encrypting the post */
    private SessionKey _readKey;
    /** pbe key root */
    private String _passphrase;
    /** pbe key prompt */
    private String _passphrasePrompt;
    /**
     * files to delete after post creation or cancellation.  this contains
     * temp files built from stdin, etc.
     */
    private List _toDelete;
        
    public PostMenu(TextEngine engine) {
        _engine = engine;
        _itemText = new ArrayList();
        _itemKeys = new ArrayList();
        _toDelete = new ArrayList();
        _itemIsChannelList = false;
        _itemIteratorIndex = 0;
        resetContent();
    }
    private void resetContent() {
        _currentMessage = null;
        _pageFiles = new ArrayList();
        _pageConfig = new ArrayList();
        _attachmentFiles = new ArrayList();
        _attachmentConfig = new ArrayList();
        _listedNymKeys = new ArrayList();
        _authenticationKey = null;
        _authorizationKey = null;
        _avatarFile = null;
        _referenceNodes = new ArrayList();
        _parents = new ArrayList();
        _publiclyReadable = null;
        _passphrase = null;
        _passphrasePrompt = null;
        _readKey = null;
        while (_toDelete.size() > 0) {
            String filename = (String)_toDelete.remove(0);
            File f = new File(filename);
            f.delete();
        }
    }
    
    public static final String NAME = "post";
    public String getName() { return NAME; }
    public String getDescription() { return "posting menu"; }
    public boolean requireLoggedIn() { return true; }
    public void listCommands(UI ui) {
        ui.statusMessage(" channels           : display a list of channels the current nym can post to");
        if (_itemIsChannelList) {
            ui.statusMessage(" next [--lines $num]: paginate through the channels, 10 or $num at a time");
            ui.statusMessage(" prev [--lines $num]: paginate through the channels, 10 or $num at a time");
        }
        ui.statusMessage(" meta [--channel ($index|$hash)] : display the current channel's metadata");
        if (_currentMessage == null) {
            ui.statusMessage(" create --channel ($index|$hash): begin the process of creating a new post");
        } else {
            ui.statusMessage(" addPage [--page $num] --in ($filename|stdin) [--type $contentType]");
            ui.statusMessage(" listpages          : display a list of pages currently sloted for posting");
            ui.statusMessage(" delpage $num       : delete the given page");
            ui.statusMessage(" addattachment [--attachment $num] --in $filename [--type $contentType]");
            ui.statusMessage("               [--name $name] [--description $desc]");
            ui.statusMessage(" listattachments    : display a list of attachments currently sloted for posting");
            ui.statusMessage(" delattachment $num");
            ui.statusMessage(" addref --in $file  : load in references from the given file");
            ui.statusMessage(" listkeys [--scope $scope] [--type $type]");
            ui.statusMessage(" addref [--name $name] --uri $uri [--reftype $type] [--description $desc]");
            ui.statusMessage("                    : add a single reference.  the reftype can be 'recommend', 'ignore', etc");
            ui.statusMessage(" addref --readkey $keyHash --scope $scope [--name $name] [--description $desc]");
            ui.statusMessage("                    : add a reference that includes the given channel read key (AES256)");
            ui.statusMessage(" addref --postkey $keyHash --scope $scope [--name $name] [--description $desc]");
            ui.statusMessage("                    : add a reference that includes the given channel post key (DSA private)");
            ui.statusMessage(" addref --managekey $keyHash --scope $scope [--name $name] [--description $desc]");
            ui.statusMessage("                    : add a reference that includes the given channel manage key (DSA private)");
            ui.statusMessage(" addref --replykey $keyHash --scope $scope [--name $name] [--description $desc]");
            ui.statusMessage("                    : add a reference that includes the given channel's reply key (ElGamal private)");
            ui.statusMessage(" listrefs           : display an indexed list of references already added");
            ui.statusMessage(" delref $index      : delete the specified reference");
            ui.statusMessage(" addparent --uri $uri [--order $num]");
            ui.statusMessage("                    : add the given syndie URI as a threaded parent to the new message");
            ui.statusMessage(" listparents        : display a list of URIs this new post will be marked as");
            ui.statusMessage("                    : replying to (most recent parent at index 0)");
            ui.statusMessage(" delparent $index");
            ui.statusMessage(" listauthkeys [--authorizedOnly $boolean]");
            ui.statusMessage("                    : display an indexed list of signing keys that the nym has");
            ui.statusMessage("                    : access to.  if requested, only includes those keys which have");
            ui.statusMessage("                    : been marked as authorized to post in the channel (or");
            ui.statusMessage("                    : authorized to manage the channel)");
            ui.statusMessage(" authenticate $index: use the specified key to authenticate the post");
            ui.statusMessage(" authorize $index   : use the specified key to authorize the post");
            ui.statusMessage(" listreadkeys       : display a list of known channel read keys that we can use to");
            ui.statusMessage("                    : encrypt the message");
            ui.statusMessage(" set --readkey (public|$index|pbe --passphrase $passphrase --prompt $prompt)");
            ui.statusMessage("                    : if public, create a random key and publicize it in the public");
            ui.statusMessage("                    : headers.  if pbe, then derive a read key from the passphrase,");
            ui.statusMessage("                    : publicizing the prompt in the public headers.  Otherwise use the");
            ui.statusMessage("                    : indexed read key for the channel");
            //ui.statusMessage(" set --cancel $uri  : state that the given URI should be cancelled (ignored unless authorized)");
            ui.statusMessage(" set --messageId ($id|date) : specify the message Id, or if 'date', generate one based on the date");
            ui.statusMessage(" set --subject $subject : specify the message subject");
            ui.statusMessage(" set --avatar $filename : specify a message-specific avatar to use");
            ui.statusMessage(" set --encryptToReply $boolean");
            ui.statusMessage("                    : if true, the message should be encrypted to the channel's reply key");
            ui.statusMessage("                    : so that only the channel's owner (or designee) can read it, and the");
            ui.statusMessage("                    : channel is included in the public header (if not authorized)");
            ui.statusMessage(" set --overwrite $uri : mark this message as a replacement for the given URI");
            ui.statusMessage(" set --expiration ($yyyyMMdd|none) : suggest a date on which the message can be discarded");
            ui.statusMessage(" set --forceNewThread $boolean : if true, branch off this message into a new thread");
            ui.statusMessage(" set --refuseReplies $boolean : if true, only the author can reply to this message in the same thread");
            ui.statusMessage(" set --publicTags [$tag[,$tag]*]: list of tags visible by anyone");
            ui.statusMessage(" set --privateTags [$tag[,$tag]*]: list of tags visible only by those authorized to read the message");
            ui.statusMessage(" preview [--page $n]: view the post as it will be seen");
            ui.statusMessage(" execute [--out $filename]");
            ui.statusMessage("                    : actually generate the post, exporting it to the given file, and then");
            ui.statusMessage("                    : importing it into the local database");
            ui.statusMessage(" cancel             : clear the current create state without updating anything");
        }
    }
    public boolean processCommands(DBClient client, UI ui, Opts opts) {
        String cmd = opts.getCommand();
        if ("channels".equalsIgnoreCase(cmd)) {
            processChannels(client, ui, opts);
        } else if ("next".equalsIgnoreCase(cmd)) {
            processNext(client, ui, opts);
        } else if ("prev".equalsIgnoreCase(cmd)) {
            processPrev(client, ui, opts);
        } else if ("meta".equalsIgnoreCase(cmd)) {
            processMeta(client, ui, opts);
        } else if ("cancel".equalsIgnoreCase(cmd)) {
            resetContent();
            ui.statusMessage("Posting cancelled");
            ui.commandComplete(-1, null);
        } else if ("create".equalsIgnoreCase(cmd)) {
            processCreate(client, ui, opts);
        } else if ("addpage".equalsIgnoreCase(cmd)) {
            processAddPage(client, ui, opts);
        } else if ("listpages".equalsIgnoreCase(cmd)) {
            processListPages(client, ui, opts);
        } else if ("delpage".equalsIgnoreCase(cmd)) {
            processDelPage(client, ui, opts);
        } else if ("addattachment".equalsIgnoreCase(cmd)) {
            processAddAttachment(client, ui, opts);
        } else if ("listattachments".equalsIgnoreCase(cmd)) {
            processListAttachments(client, ui, opts);
        } else if ("delattachment".equalsIgnoreCase(cmd)) {
            processDelAttachment(client, ui, opts);
        } else if ("listauthkeys".equalsIgnoreCase(cmd)) {
            processListAuthKeys(client, ui, opts);
        } else if ("authenticate".equalsIgnoreCase(cmd)) {
            processAuthenticate(client, ui, opts);
        } else if ("authorize".equalsIgnoreCase(cmd)) {
            processAuthorize(client, ui, opts);
        } else if ("listkeys".equalsIgnoreCase(cmd)) {
            processListKeys(client, ui, opts);
        } else if ("addref".equalsIgnoreCase(cmd)) {
            processAddRef(client, ui, opts);
        } else if ("listrefs".equalsIgnoreCase(cmd)) {
            processListRefs(client, ui, opts);
        } else if ("delref".equalsIgnoreCase(cmd)) {
            processDelRef(client, ui, opts);
        } else if ("addparent".equalsIgnoreCase(cmd)) {
            processAddParent(client, ui, opts);
        } else if ("listparents".equalsIgnoreCase(cmd)) {
            processListParents(client, ui, opts);
        } else if ("delparent".equalsIgnoreCase(cmd)) {
            processDelParent(client, ui, opts);
        } else if ("preview".equalsIgnoreCase(cmd)) {
            processPreview(client, ui, opts);
        } else if ("execute".equalsIgnoreCase(cmd)) {
            processExecute(client, ui, opts);
        } else if ("listreadkeys".equalsIgnoreCase(cmd)) {
            processListReadKeys(client, ui, opts);
        } else if ("set".equalsIgnoreCase(cmd)) {
            processSet(client, ui, opts);
        } else {
            return false;
        }
        return true;
    }
    public List getMenuLocation(DBClient client, UI ui) {
        List rv = new ArrayList();
        rv.add("post");
        if (_currentMessage != null)
            rv.add("create");
        return rv;
    }
    
    private static final SimpleDateFormat _dayFmt = new SimpleDateFormat("yyyy/MM/dd");
    /** channels */
    private void processChannels(DBClient client, UI ui, Opts opts) {
        _itemIteratorIndex = 0;
        _itemIsChannelList = true;
        _itemKeys.clear();
        _itemText.clear();
        
        boolean manageOnly = false;
        String cap = opts.getOptValue("capability");
        if ( (cap != null) && ("manage".equalsIgnoreCase(cap)) ) {
            // if we want capability=manage, then include ident+manage chans.
            // if we want capability=post, include ident+manage+post+publicPost chans
            // (since we can post on channels we have the identity key for or can manage)
            manageOnly = true;
        }
        
        boolean includeManage = true;
        boolean includeIdent = true;
        boolean includePost = !manageOnly;
        boolean includePublicPost = !manageOnly;
        DBClient.ChannelCollector channels = client.getChannels(includeManage, includeIdent, includePost, includePublicPost);
        
        // first, go through and find all the 'identity' channels - those that we have
        // the actual channel signing key for
        for (int i = 0; i < channels.getIdentityChannelCount(); i++) {
            ChannelInfo info = channels.getIdentityChannel(i);
            ui.debugMessage("nym has the identity key for " + info.getChannelHash().toBase64());
            _itemKeys.add(new Long(info.getChannelId()));
            _itemText.add("Identity channel " + CommandImpl.strip(info.getName()) + " (" + info.getChannelHash().toBase64().substring(0,6) + "): " + CommandImpl.strip(info.getDescription()));
        }
        
        // now, go through and see what other channels our management keys are
        // authorized to manage (beyond their identity channels)
        for (int i = 0; i < channels.getManagedChannelCount(); i++) {
            ChannelInfo info = channels.getManagedChannel(i);
            ui.debugMessage("nym has a key that is an explicit management key for " + info.getChannelHash().toBase64());
            _itemKeys.add(new Long(info.getChannelId()));
            _itemText.add("Managed channel " + CommandImpl.strip(info.getName()) + " (" + info.getChannelHash().toBase64().substring(0,6) + "): " + CommandImpl.strip(info.getDescription()));
        }

        // continue on to see what channels our management keys are
        // authorized to post in (beyond their identity and manageable channels)
        for (int i = 0; i < channels.getPostChannelCount(); i++) {
            ChannelInfo info = channels.getPostChannel(i);
            ui.debugMessage("nym has a key that is an explicit post key for " + info.getChannelHash().toBase64());
            _itemKeys.add(new Long(info.getChannelId()));
            _itemText.add("Authorized channel " + CommandImpl.strip(info.getName()) + " (" + info.getChannelHash().toBase64().substring(0,6) + "): " + CommandImpl.strip(info.getDescription()));
        }

        // now for channels anyone can post to
        for (int i = 0; i < channels.getPublicPostChannelCount(); i++) {
            ChannelInfo info = channels.getPublicPostChannel(i);
            _itemKeys.add(new Long(info.getChannelId()));
            _itemText.add("Public channel " + CommandImpl.strip(info.getName()) + " (" + info.getChannelHash().toBase64().substring(0,6) + "): " + CommandImpl.strip(info.getDescription()));
        }
        
        ui.statusMessage(_itemKeys.size() + " channels matched - use 'next' to view them");
        ui.commandComplete(0, null);
    }

    /** next [--lines $num]  : iterate through the channels */
    private void processNext(DBClient client, UI ui, Opts opts) {
        int num = (int)opts.getOptLong("lines", 10);
        String name = "channels";
        if (_itemIsChannelList) {
            if (_itemIteratorIndex >= _itemKeys.size()) {
                ui.statusMessage("No more " + name + " - use 'prev' to review earlier " + name);
                ui.commandComplete(0, null);
            } else {
                int end = Math.min(_itemIteratorIndex+num, _itemKeys.size());
                ui.statusMessage(name + " " + _itemIteratorIndex + " through " + (end-1) + " of " + (_itemKeys.size()-1));
                while (_itemIteratorIndex < end) {
                    String desc = (String)_itemText.get(_itemIteratorIndex);
                    ui.statusMessage(_itemIteratorIndex + ": " + desc);
                    _itemIteratorIndex++;
                }
                int remaining = _itemKeys.size() - _itemIteratorIndex;
                if (remaining > 0)
                    ui.statusMessage(remaining + " " + name + " remaining");
                else
                    ui.statusMessage("No more " + name + " - use 'prev' to review earlier " + name);
                ui.commandComplete(0, null);
            }
        } else {
            ui.statusMessage("Cannot iterate through the list, as no channels have been selected");
            ui.commandComplete(-1, null);
        }
    }

    /** prev [--lines $num]  : iterate through the channels */
    private void processPrev(DBClient client, UI ui, Opts opts) {
        int num = (int)opts.getOptLong("lines", 10);
        _itemIteratorIndex -= num;
        if (_itemIteratorIndex < 0)
            _itemIteratorIndex = 0;
        processNext(client, ui, opts);
    }

    /* create --channel ($index|$hash): begin the process of creating a new post */
    private void processCreate(DBClient client, UI ui, Opts opts) {
        if (_currentMessage != null) {
            ui.errorMessage("Cannot create a new message - an existing create process is already in progress");
            ui.errorMessage("Cancel or complete that process before continuing (with the cancel or execute commands)");
            ui.commandComplete(-1, null);
            return;
        }
     
        ChannelInfo channel = null;
        String chan = opts.getOptValue("channel");
        if (chan != null) {
            try {
                int val = Integer.parseInt(chan);
                if ( (val < 0) || (val >= _itemKeys.size()) ) {
                    ui.errorMessage("Channel index out of bounds");
                    ui.commandComplete(-1, null);
                    return;
                }
                Long chanId = (Long)_itemKeys.get(val);
                channel = client.getChannel(chanId.longValue());
            } catch (NumberFormatException nfe) {
                ui.debugMessage("channel requested is not an index (" + chan + ")");
                // ok, not an integer, maybe its a full channel hash?
                byte val[] = Base64.decode(chan);
                if ( (val != null) && (val.length == Hash.HASH_LENGTH) ) {
                    long id = client.getChannelId(Hash.create(val));
                    if (id >= 0) {
                        channel = client.getChannel(id);
                    } else {
                        ui.errorMessage("Channel is not locally known: " + chan);
                        ui.commandComplete(-1, null);
                        return;
                    }
                } else {
                    ui.errorMessage("Channel requested is not valid - either specify --channel $index or --channel $base64(channelHash)");
                    ui.commandComplete(-1, null);
                    return;
                }
            }
        }
        if (channel == null) {
            ui.errorMessage("Target channel must be specified");
            ui.commandComplete(-1, null);
            return;
        }

        resetContent();
        _currentMessage = new MessageInfo();
        _currentMessage.setTargetChannel(channel.getChannelHash());
        _currentMessage.setTargetChannelId(channel.getChannelId());
        // set the scope to the target (if we are authorized), or to the first
        // channel we are authorized to post on
        List priv = client.getNymKeys(client.getLoggedInNymId(), client.getPass(), null, null);
        for (int i = 0; i < priv.size(); i++) {
            NymKey curKey = (NymKey)priv.get(i);
            if (Constants.KEY_FUNCTION_MANAGE.equals(curKey.getFunction()) ||
                Constants.KEY_FUNCTION_POST.equals(curKey.getFunction())) {
                SigningPrivateKey privKey = new SigningPrivateKey(curKey.getData());
                SigningPublicKey pub = KeyGenerator.getSigningPublicKey(privKey);
                if (channel.getAuthorizedManagers().contains(pub)) {
                    _currentMessage.setScopeChannelId(channel.getChannelId());
                    break;
                } else if (channel.getAuthorizedPosters().contains(pub)) {
                    _currentMessage.setScopeChannelId(channel.getChannelId());
                    break;
                }
            }
        }
        // not authorized, so lets just set the default scope to our first one
        if (_currentMessage.getScopeChannelId() < 0) {
            for (int i = 0; i < priv.size(); i++) {
                NymKey curKey = (NymKey)priv.get(i);
                if (Constants.KEY_FUNCTION_MANAGE.equals(curKey.getFunction()) ||
                    Constants.KEY_FUNCTION_POST.equals(curKey.getFunction())) {
                    SigningPrivateKey privKey = new SigningPrivateKey(curKey.getData());
                    SigningPublicKey pub = KeyGenerator.getSigningPublicKey(privKey);
                    long chanId = client.getChannelId(pub.calculateHash());
                    if (chanId >= 0) {
                        _currentMessage.setScopeChannelId(chanId);
                        break;
                    }
                }
            }
        }
        _currentMessage.setMessageId(createEdition(client));
        ui.statusMessage("Posting to '" + CommandImpl.strip(channel.getName()) + "' (" + channel.getChannelHash().toBase64().substring(0,6) + ")");
        
        SigningPublicKey pub = getNymPublicKey(client);
        if ( (pub != null) && (!channel.getChannelHash().equals(pub.calculateHash())) ) {
            long id = client.getChannelId(pub.calculateHash());
            if (id >= 0) {
                ChannelInfo author = client.getChannel(id);
                _currentMessage.setAuthorChannelId(id);//pub.calculateHash());
                ui.statusMessage("Defaulting identity channel " + CommandImpl.strip(author.getName()) + " (" + pub.calculateHash().toBase64().substring(0,6) + ") as the author");
            }
        }
        
        ui.statusMessage("Post creation process initiated");
        ui.statusMessage("Please specify fields as, and complete the post creation");
        ui.statusMessage("process with 'execute', or cancel the process with 'cancel'");
        ui.commandComplete(0, null);
    }
    
    private SigningPublicKey getNymPublicKey(DBClient client) {
        List manageKeys = client.getNymKeys(client.getLoggedInNymId(), client.getPass(), null, Constants.KEY_FUNCTION_MANAGE);
        List pubKeys = new ArrayList();
        // find all the 'identity' channels - those that we have
        // the actual channel signing key for
        for (int i = 0; i < manageKeys.size(); i++) {
            NymKey key = (NymKey)manageKeys.get(i);
            if (key.getAuthenticated()) {
                SigningPrivateKey priv = new SigningPrivateKey(key.getData());
                SigningPublicKey pub = KeyGenerator.getSigningPublicKey(priv);
                Hash chan = pub.calculateHash();
                long chanId = client.getChannelId(chan);
                if (chanId >= 0)
                    pubKeys.add(pub);
            }
        }
        if (pubKeys.size() == 1) {
            return (SigningPublicKey)pubKeys.get(0);
        } else {
            return null;
        }
    }
    
    /** today's date, but with a randomized hhmmss.SSS component */
    private long createEdition(DBClient client) {
        long now = System.currentTimeMillis();
        now -= (now % 24*60*60*1000);
        now += client.ctx().random().nextLong(24*60*60*1000);
        return now;
    }
    
    private void processPreview(DBClient client, UI ui, Opts opts) {
        if (_currentMessage == null) {
            ui.errorMessage("No creation or update process in progress");
            ui.commandComplete(-1, null);
            return;
        }
        ui.statusMessage(_currentMessage.toString());
        if (_avatarFile != null)
            ui.statusMessage("Loading the message avatar from: " + _avatarFile);
        
        ui.statusMessage("Pages: " + _pageFiles.size());
        for (int i = 0; i < _pageFiles.size(); i++) {
            String filename = (String)_pageFiles.get(i);
            String type = ((Properties)_pageConfig.get(i)).getProperty(Constants.MSG_PAGE_CONTENT_TYPE);
            ui.statusMessage("Page " + i + ": loaded from " + CommandImpl.strip(filename) + " (type: " + CommandImpl.strip(type) + ")");
        }
        
        ui.statusMessage("Attachments: " + _attachmentFiles.size());
        for (int i = 0; i < _attachmentFiles.size(); i++) {
            String filename = (String)_attachmentFiles.get(i);
            Properties cfg = (Properties)_attachmentConfig.get(i);
            String type = cfg.getProperty(Constants.MSG_PAGE_CONTENT_TYPE);
            String name = cfg.getProperty(Constants.MSG_ATTACH_NAME);
            String desc = cfg.getProperty(Constants.MSG_ATTACH_DESCRIPTION);
            ui.statusMessage("Attachment " + i + ": loaded from " + CommandImpl.strip(filename) + " (type: " + CommandImpl.strip(type) + ")");
            ui.statusMessage("            : suggested name: '" + CommandImpl.strip(name) + "', description: '" + CommandImpl.strip(desc) + "'");
        }
        
        if (_authenticationKey != null) {
            SigningPrivateKey priv = new SigningPrivateKey(_authenticationKey.getData());
            SigningPublicKey pub = KeyGenerator.getSigningPublicKey(priv);
            ui.statusMessage("Authenticating with the private key for " + pub.calculateHash().toBase64().substring(0,6));
        }
        if (_authorizationKey != null) {
            SigningPrivateKey priv = new SigningPrivateKey(_authorizationKey.getData());
            SigningPublicKey pub = KeyGenerator.getSigningPublicKey(priv);
            ui.statusMessage("Authorizing with the private key for " + pub.calculateHash().toBase64().substring(0,6));
        }
        
        if (_referenceNodes.size() > 0) {
            ui.statusMessage("References: ");
            ListWalker w = new ListWalker(ui);
            ReferenceNode.walk(_referenceNodes, w);
        }
        
        ui.statusMessage("Parents (most recent first):");
        for (int i = 0; i < _parents.size(); i++) {
            SyndieURI uri = (SyndieURI)_parents.get(i);
            long id = client.getChannelId(uri.getScope());
            MessageInfo msg = null;
            if (id >= 0) {
                msg = client.getMessage(id, uri.getMessageId());
                if (msg != null) {
                    ui.statusMessage(i + ": " + msg.getTargetChannel().toBase64().substring(0,6)
                                     + " - '" + CommandImpl.strip(msg.getSubject()) + "' (" + msg.getMessageId() + ")");
                }
            }
            if (msg == null)
                ui.statusMessage(i + ": " + uri.getScope().toBase64().substring(0,6) + " (" + uri.getMessageId().longValue() + ")");
        }
        
        int page = (int)opts.getOptLong("page", -1);
        if ( (page >= 0) && (page < _pageFiles.size()) ) {
            String filename = (String)_pageFiles.get(page);
            String type = ((Properties)_pageConfig.get(page)).getProperty(Constants.MSG_PAGE_CONTENT_TYPE);
            ui.statusMessage("Page " + page + " (loaded from " + CommandImpl.strip(filename) + " (type: " + CommandImpl.strip(type) + ")");
            
            File f = new File(filename);
            BufferedReader in = null;
            try {
                in = new BufferedReader(new InputStreamReader(new FileInputStream(f), "UTF-8"));
                String line = null;
                while ( (line = in.readLine()) != null)
                    ui.statusMessage(line);
                in.close();
                in = null;
            } catch (IOException ioe) {
                ui.errorMessage("Error previewing the page", ioe);
            } finally {
                if (in != null) try { in.close(); } catch (IOException ioe) {}
            }
        }
        
        ui.commandComplete(0, null);
    }
    
    private void processMeta(DBClient client, UI ui, Opts opts) {
        long channelIndex = -1;
        Hash channel = null;
        String chan = opts.getOptValue("channel");
        if (chan != null) {
            try {
                long val = Long.parseLong(chan);
                channelIndex = val;
            } catch (NumberFormatException nfe) {
                ui.debugMessage("channel requested is not an index (" + chan + ")");
                // ok, not an integer, maybe its a full channel hash?
                byte val[] = Base64.decode(chan);
                if ( (val != null) && (val.length == Hash.HASH_LENGTH) ) {
                    channel = Hash.create(val);
                    ui.debugMessage("channel requested is a hash (" + channel.toBase64() + ")");
                } else {
                    ui.errorMessage("Channel requested is not valid - either specify --channel $index or --channel $base64(channelHash)");
                    ui.commandComplete(-1, null);
                    return;
                }
            }
        }
        
        ChannelInfo info = null;
        
        if (_currentMessage != null)
            info = client.getChannel(_currentMessage.getTargetChannelId());
        
        long channelId = -1;
        if ( (channelIndex >= 0) && (channelIndex < _itemKeys.size()) ) {
            channelId = ((Long)_itemKeys.get((int)channelIndex)).longValue();
            info = client.getChannel(channelId);
        } else if (channel != null) {
            channelId = client.getChannelId(channel);
            info = client.getChannel(channelId);
        }
     
        if (info == null) {
            ui.debugMessage("channelIndex=" + channelIndex + " itemKeySize: " + _itemKeys.size());
            ui.debugMessage("channel=" + channelIndex);
            ui.errorMessage("Invalid or unknown channel requested");
            ui.commandComplete(-1, null);
            return;
        }
        
        ui.statusMessage(info.toString());
    }
    
    /** addPage [--page $num] --in ($filename|stdin) [--type $contentType] */
    private void processAddPage(DBClient client, UI ui, Opts opts) {
        if (_currentMessage == null) {
            ui.errorMessage("No posting in progress");
            ui.commandComplete(-1, null);
            return;
        }
        String in = opts.getOptValue("in");
        if (in != null) {
            int index = _pageFiles.indexOf(in);
            if (index >= 0) {
                ui.errorMessage("The file " + in + " is already slotted as page " + index);
                ui.commandComplete(-1, null);
                return;
            }
        } else {
            ui.errorMessage("The file must be specified with --in $filename");
            ui.commandComplete(-1, null);
            return;
        }
        String type = opts.getOptValue("type");
        if (type == null)
            type = "text/plain";
        int page = (int)opts.getOptLong("page", _pageFiles.size());
        boolean deleteAfterPost = false;
        File f = null;
        if ("stdin".equalsIgnoreCase(in)) {
            String content = ui.readStdIn();
            FileWriter out = null;
            try {
                f = SecureFile.createTempFile("stdin", ".txt", client.getTempDir());
                out = new FileWriter(f);
                out.write(content);
                out.close();
                out = null;
                in = f.getPath();
                deleteAfterPost = true;
            } catch (IOException ioe) {
                ui.errorMessage("Error buffering the new page", ioe);
                ui.commandComplete(-1, null);
                return;
            } finally {
                if (out != null) try { out.close(); } catch (IOException ioe) {}
            }
        }
        f = new File(in);
        if (!f.exists()) {
            ui.errorMessage("Page file does not exist");
            ui.commandComplete(-1, null);
        } else if (!f.canRead()) {
            ui.errorMessage("Page file is not readable");
            ui.commandComplete(-1, null);
        } else if (!f.isFile()) {
            ui.errorMessage("Page file is not a normal file");
            ui.commandComplete(-1, null);
        } else if ( (page < 0) || (page > _pageFiles.size()) ) {
            ui.errorMessage("Page index is out of range");
            ui.commandComplete(-1, null);
        } else {
            _pageFiles.add(page, in);
            Properties cfg = new Properties();
            cfg.setProperty(Constants.MSG_PAGE_CONTENT_TYPE, CommandImpl.strip(type));
            _pageConfig.add(page, cfg);
            if (deleteAfterPost) {
                _toDelete.add(in);
                ui.statusMessage("Page " + page + " read from standard input (size: " + f.length() + " bytes, type: " + CommandImpl.strip(type) + ")");
            } else {
                ui.statusMessage("Page " + page + " configured to use " + CommandImpl.strip(in) + " (size: " + f.length() + " bytes, type: " + CommandImpl.strip(type) + ")");
            }
            ui.commandComplete(0, null);
        }
    }
    /** listpages          : display a list of pages currently sloted for posting */
    private void processListPages(DBClient client, UI ui, Opts opts) {
        if (_currentMessage == null) {
            ui.errorMessage("No posting in progress");
            ui.commandComplete(-1, null);
            return;
        }
        ui.statusMessage("Pages: " + _pageFiles.size());
        for (int i = 0; i < _pageFiles.size(); i++) {
            String filename = (String)_pageFiles.get(i);
            String type = ((Properties)_pageConfig.get(i)).getProperty(Constants.MSG_PAGE_CONTENT_TYPE);
            ui.statusMessage("Page " + i + ": loaded from " + CommandImpl.strip(filename) + " (type: " + CommandImpl.strip(type) + ")");
        }
        ui.commandComplete(-1, null);
    }
    /** delpage $num       : delete the given page */
    private void processDelPage(DBClient client, UI ui, Opts opts) {
        if (_currentMessage == null) {
            ui.errorMessage("No posting in progress");
            ui.commandComplete(-1, null);
            return;
        }
    
        String arg = opts.getArg(0);
        if (arg == null) {
            ui.errorMessage("Usage: delpage $pageNumber");
            ui.commandComplete(-1, null);
            return;
        }
        try {
            int page = Integer.parseInt(arg);
            if ( (page >= 0) && (page < _pageFiles.size()) ) {
                _pageFiles.remove(page);
                _pageConfig.remove(page);
                ui.statusMessage("Not including page " + page);
                ui.commandComplete(0, null);
            } else {
                ui.statusMessage("Page " + page + " out of range");
                ui.commandComplete(-1, null);
            }
        } catch (NumberFormatException nfe) {
            ui.statusMessage("Invalid page requested");
            ui.commandComplete(-1, null);
        }
    }
    
    /** 
     * addattachment [--attachment $num] --in $filename [--type $contentType] [--name $name] [--description $desc]
     */
    private void processAddAttachment(DBClient client, UI ui, Opts opts) {
        if (_currentMessage == null) {
            ui.errorMessage("No posting in progress");
            ui.commandComplete(-1, null);
            return;
        }
        String in = opts.getOptValue("in");
        if (in != null) {
            int index = _attachmentFiles.indexOf(in);
            if (index >= 0) {
                ui.errorMessage("The file " + in + " is already slotted as attachment " + index);
                ui.commandComplete(-1, null);
                return;
            }
        }
        String type = opts.getOptValue("type");
        if (type == null)
            type = "application/octet-stream";
        int num = (int)opts.getOptLong("attachment", _attachmentFiles.size());
        File f = new File(in);
        if (!f.exists()) {
            ui.errorMessage("Attachment file does not exist");
            ui.commandComplete(-1, null);
        } else if (!f.canRead()) {
            ui.errorMessage("Attachment file is not readable");
            ui.commandComplete(-1, null);
        } else if (!f.isFile()) {
            ui.errorMessage("Attachment file is not a normal file");
            ui.commandComplete(-1, null);
        } else if ( (num < 0) || (num > _attachmentFiles.size()) ) {
            ui.errorMessage("Attachment index is out of range");
            ui.commandComplete(-1, null);
        } else {
            _attachmentFiles.add(num, in);
            String desc = opts.getOptValue("description");
            if (desc == null) desc = "";
            String name = opts.getOptValue("name");
            if (name == null) name = f.getName();
            ui.debugMessage("Options: " + opts.getOptNames());
            Properties cfg = new Properties();
            cfg.setProperty(Constants.MSG_ATTACH_CONTENT_TYPE, CommandImpl.strip(type));
            cfg.setProperty(Constants.MSG_ATTACH_DESCRIPTION, CommandImpl.strip(desc));
            cfg.setProperty(Constants.MSG_ATTACH_NAME, CommandImpl.strip(name));
            _attachmentConfig.add(num, cfg);
            ui.statusMessage("Attachment " + num 
                             + " (" + CommandImpl.strip(name) + " - '" + CommandImpl.strip(desc) 
                             + "') configured to use " + CommandImpl.strip(in)
                             + " (type: " + CommandImpl.strip(type) + ")");
            ui.commandComplete(0, null);
        }
    }
    /** listattachments    : display a list of attachments currently sloted for posting */
    private void processListAttachments(DBClient client, UI ui, Opts opts) {
        if (_currentMessage == null) {
            ui.errorMessage("No posting in progress");
            ui.commandComplete(-1, null);
            return;
        }
        ui.statusMessage("Attachments: " + _attachmentFiles.size());
        for (int i = 0; i < _attachmentFiles.size(); i++) {
            String filename = (String)_attachmentFiles.get(i);
            Properties cfg = (Properties)_attachmentConfig.get(i);
            String type = cfg.getProperty(Constants.MSG_PAGE_CONTENT_TYPE);
            String name = cfg.getProperty(Constants.MSG_ATTACH_NAME);
            String desc = cfg.getProperty(Constants.MSG_ATTACH_DESCRIPTION);
            ui.statusMessage("Attachment " + i + ": loaded from " + CommandImpl.strip(filename) + " (type: " + CommandImpl.strip(type) + ")");
            ui.statusMessage("            : suggested name: '" + CommandImpl.strip(name) + "', description: '" + CommandImpl.strip(desc) + "'");
        }
        ui.commandComplete(-1, null);
    }
    /** delattachment $num */
    private void processDelAttachment(DBClient client, UI ui, Opts opts) {
        if (_currentMessage == null) {
            ui.errorMessage("No posting in progress");
            ui.commandComplete(-1, null);
            return;
        }
    
        String arg = opts.getArg(0);
        if (arg == null) {
            ui.errorMessage("Usage: delattachment $attachmentNumber");
            ui.commandComplete(-1, null);
            return;
        }
        try {
            int num = Integer.parseInt(arg);
            if ( (num >= 0) && (num < _attachmentFiles.size()) ) {
                _attachmentFiles.remove(num);
                _attachmentConfig.remove(num);
                ui.statusMessage("Not including attachment " + num);
                ui.commandComplete(0, null);
            } else {
                ui.statusMessage("Attachment " + num + " out of range");
                ui.commandComplete(-1, null);
            }
        } catch (NumberFormatException nfe) {
            ui.statusMessage("Invalid attachment requested");
            ui.commandComplete(-1, null);
        }
    }
    
    /** 
     * listauthkeys [--authorizedOnly $boolean]
     * display an indexed list of signing keys that the nym has access to.  if
     * requested, only includes those keys which have been marked as authorized to
     * post in the channel (or authorized to manage the channel)
     */
    private void processListAuthKeys(DBClient client, UI ui, Opts opts) {
        if ( (_currentMessage == null) || (_currentMessage.getTargetChannel() == null) ) {
            ui.errorMessage("Can only list keys once a target channel has been selected");
            ui.commandComplete(-1, null);
            return;
        }
        _listedNymKeys.clear();
        boolean auth = opts.getOptBoolean("authorizedOnly", true);
        Hash scope = _currentMessage.getTargetChannel();
        if (!auth)
            scope = null;
        List keys = client.getNymKeys(client.getLoggedInNymId(), client.getPass(), scope, Constants.KEY_FUNCTION_MANAGE);
        for (int i = 0; i < keys.size(); i++) {
            NymKey key = (NymKey)keys.get(i);
            ui.statusMessage("key " + _listedNymKeys.size() + ": " + key.getType() + " for " + key.getChannel().toBase64().substring(0,6) + " (authenticated? " + key.getAuthenticated() + ")");
            _listedNymKeys.add(key);
        }
        keys = client.getNymKeys(client.getLoggedInNymId(), client.getPass(), scope, Constants.KEY_FUNCTION_POST);
        for (int i = 0; i < keys.size(); i++) {
            NymKey key = (NymKey)keys.get(i);
            ui.statusMessage("key " + _listedNymKeys.size() + ": " + key.getType() + " for " + key.getChannel().toBase64().substring(0,6) + " (authenticated? " + key.getAuthenticated() + ")");
            _listedNymKeys.add(key);
        }
        // now offer the manage keys for authentication only
        keys = client.getNymKeys(client.getLoggedInNymId(), client.getPass(), null, Constants.KEY_FUNCTION_MANAGE);
        for (int i = 0; i < keys.size(); i++) {
            NymKey key = (NymKey)keys.get(i);
            SigningPrivateKey priv = new SigningPrivateKey(key.getData());
            SigningPublicKey pub = KeyGenerator.getSigningPublicKey(priv);
            if (key.getChannel().equals(pub.calculateHash())) {
                ui.statusMessage("identity key " + _listedNymKeys.size() + ": " + key.getChannel().toBase64().substring(0,6) + " (for authentication only)");
                _listedNymKeys.add(key);
            }
        }
        ui.commandComplete(0, null);
    }
    
    /** authenticate $index */
    private void processAuthenticate(DBClient client, UI ui, Opts opts) {
        if (_listedNymKeys.size() <= 0) {
            ui.errorMessage("No keys listed (list them through 'listauthkeys')");
            ui.commandComplete(-1, null);
            return;
        }
        String arg = opts.getArg(0);
        if (arg == null) {
            ui.errorMessage("Usage: authenticate $num");
            ui.commandComplete(-1, null);
            return;
        }
        try {
            int num = Integer.parseInt(arg);
            if ( (num >= 0) && (num < _listedNymKeys.size()) ) {
                _authenticationKey = (NymKey)_listedNymKeys.get(num);
                SigningPrivateKey priv = new SigningPrivateKey(_authenticationKey.getData());
                SigningPublicKey pub = KeyGenerator.getSigningPublicKey(priv);
                long authenticationId = client.getChannelId(pub.calculateHash());
                _currentMessage.setScopeChannelId(authenticationId);
                ui.statusMessage("Authenticating with the private key for " + pub.calculateHash().toBase64().substring(0,6));
                ui.commandComplete(0, null);
            } else {
                ui.errorMessage("Authentication index out of range");
                ui.commandComplete(-1, null);
            }
        } catch (NumberFormatException nfe) {
            ui.errorMessage("Invalid authentication index");
            ui.commandComplete(-1, null);
        }
    }
    /** authorize $index */
    private void processAuthorize(DBClient client, UI ui, Opts opts) {
        if (_listedNymKeys.size() <= 0) {
            ui.errorMessage("No keys listed (list them through 'listauthkeys')");
            ui.commandComplete(-1, null);
            return;
        }
        String arg = opts.getArg(0);
        if (arg == null) {
            ui.errorMessage("Usage: authorize $num");
            ui.commandComplete(-1, null);
            return;
        }
        try {
            int num = Integer.parseInt(arg);
            if ( (num >= 0) && (num < _listedNymKeys.size()) ) {
                _authorizationKey = (NymKey)_listedNymKeys.get(num);
                SigningPrivateKey priv = new SigningPrivateKey(_authorizationKey.getData());
                SigningPublicKey pub = KeyGenerator.getSigningPublicKey(priv);
                ui.statusMessage("Authorizing with the private key for " + pub.calculateHash().toBase64().substring(0,6));
                ui.commandComplete(0, null);
            } else {
                ui.errorMessage("Authorization index out of range");
                ui.commandComplete(-1, null);
            }
        } catch (NumberFormatException nfe) {
            ui.errorMessage("Invalid authorization index");
            ui.commandComplete(-1, null);
        }
    }
    
    /** listkeys [--scope $scope] [--type $type] */
    private void processListKeys(DBClient client, UI ui, Opts opts) {
        byte chan[] = opts.getOptBytes("scope");
        Hash scope = null;
        if (chan != null)
            scope = Hash.create(chan);
        String type = opts.getOptValue("type");
        if (type != null) {
            if (!Constants.KEY_FUNCTION_MANAGE.equalsIgnoreCase(type) &&
                !Constants.KEY_FUNCTION_POST.equalsIgnoreCase(type) &&
                !Constants.KEY_FUNCTION_READ.equalsIgnoreCase(type) &&
                !Constants.KEY_FUNCTION_REPLY.equalsIgnoreCase(type)) {
                ui.errorMessage("Key type must be one of the following:");
                ui.errorMessage(Constants.KEY_FUNCTION_MANAGE + " (for channel management)");
                ui.errorMessage(Constants.KEY_FUNCTION_POST + " (for posting to a channel)");
                ui.errorMessage(Constants.KEY_FUNCTION_READ + " (for reading a channel)");
                ui.errorMessage(Constants.KEY_FUNCTION_REPLY+ " (for decrypting private replies on a channel)");
                ui.commandComplete(-1, null);
                return;
            }
        }
        List keys = client.getNymKeys(client.getLoggedInNymId(), client.getPass(), scope, type);
        TreeMap keysByScope = new TreeMap();
        for (int i = 0; i < keys.size(); i++) {
            NymKey key = (NymKey)keys.get(i);
            List scopeKeys = (List)keysByScope.get(key.getChannel().toBase64());
            if (scopeKeys == null) {
                scopeKeys = new ArrayList();
                keysByScope.put(key.getChannel().toBase64(), scopeKeys);
            }
            scopeKeys.add(key);
        }
        for (Iterator iter = keysByScope.values().iterator(); iter.hasNext(); ) {
            List scopeKeys = (List)iter.next();
            if (scopeKeys.size() <= 0) continue;
            Hash chanHash = ((NymKey)scopeKeys.get(0)).getChannel();
            long chanId = client.getChannelId(chanHash);
            ChannelInfo info = null;
            if (chanId >= 0)
                info = client.getChannel(chanId);
            if (info != null)
                ui.statusMessage("Private keys for '" + CommandImpl.strip(info.getName()) + "' (" + chanHash.toBase64() + ")");
            else
                ui.statusMessage("Private keys for unknown (" + chanHash.toBase64() + ")");
            for (int i = 0; i < scopeKeys.size(); i++) {
                NymKey key = (NymKey)scopeKeys.get(i);
                if (Constants.KEY_FUNCTION_MANAGE.equalsIgnoreCase(key.getFunction())) {
                    SigningPrivateKey priv = new SigningPrivateKey(key.getData());
                    SigningPublicKey pub = KeyGenerator.getSigningPublicKey(priv);
                    if (pub.calculateHash().equals(chanHash)) {
                        ui.statusMessage("- identity key: " + client.ctx().sha().calculateHash(key.getData()).toBase64());
                    } else {
                        ui.statusMessage("- manage key (" + 
                                         (key.getAuthenticated()?"authenticated":"not authenticated") +
                                         "): " + client.ctx().sha().calculateHash(key.getData()).toBase64());
                    }
                } else if (Constants.KEY_FUNCTION_POST.equalsIgnoreCase(key.getFunction())) {
                    ui.statusMessage("- post key (" + 
                                     (key.getAuthenticated()?"authenticated":"not authenticated") +
                                     "): " + client.ctx().sha().calculateHash(key.getData()).toBase64());
                } else if (Constants.KEY_FUNCTION_READ.equalsIgnoreCase(key.getFunction())) {
                    ui.statusMessage("- read key (" + 
                                     (key.getAuthenticated()?"authenticated":"not authenticated") +
                                     "): " + client.ctx().sha().calculateHash(key.getData()).toBase64());
                } else if (Constants.KEY_FUNCTION_REPLY.equalsIgnoreCase(key.getFunction())) {
                    ui.statusMessage("- reply key (" + 
                                     (key.getAuthenticated()?"authenticated":"not authenticated") +
                                     "): " + client.ctx().sha().calculateHash(key.getData()).toBase64());
                } else {
                    ui.statusMessage("Channel key of unknown type [" + key.getFunction() + "] (" +
                                     (key.getAuthenticated()?"authenticated":"not authenticated") +
                                     "): " + client.ctx().sha().calculateHash(key.getData()).toBase64());
                }
            }
        }
        ui.commandComplete(0, null);
    }
    
    /** 
     * addref (--filename | [--name $name] --uri $uri [--reftype $type] [--description $desc])
     *
     * addref --readkey $keyHash --scope $scope [--name $name] [--description $desc]
     *        add a reference that includes the given channel read key (AES256)
     * addref --postkey $keyHash --scope $scope [--name $name] [--description $desc]
     *        add a reference that includes the given channel post key (DSA private)
     * addref --managekey $keyHash --scope $scope [--name $name] [--description $desc]
     *        add a reference that includes the given channel manage key (DSA private)
     * addref --replykey $keyHash --scope $scope [--name $name] [--description $desc]
     *        add a reference that includes the given channel's reply key (ElGamal private)
     */
    private void processAddRef(DBClient client, UI ui, Opts opts) {
        String filename = opts.getOptValue("filename");
        if (filename != null) {
            FileInputStream in = null;
            try {
                in = new FileInputStream(filename);
                List roots = ReferenceNode.buildTree(in);
                _referenceNodes.addAll(roots);
                Walker w = new Walker();
                ReferenceNode.walk(roots, w);
                ui.statusMessage("Added " + w.getNodeCount() + " references");
                return;
            } catch (IOException ioe) {
                ui.errorMessage("Cannot add references from " + filename, ioe);
                ui.commandComplete(-1, null);
                return;
            }
        }
        
        String name = opts.getOptValue("name");
        String uriStr = opts.getOptValue("uri");
        String type = opts.getOptValue("reftype");
        String desc = opts.getOptValue("description");
            
        if (opts.getOptValue("readkey") != null) {
            type = "channel read key";
            byte channel[] = opts.getOptBytes("scope");
            byte keyHash[] = opts.getOptBytes("readkey");
            List keys = client.getReadKeys(Hash.create(channel), client.getLoggedInNymId(), client.getPass(), true);
            ui.debugMessage("read keys for scope " + Base64.encode(channel) + ": " + keys.size()
                            + " (looking for " + Base64.encode(keyHash) + ")");
            for (int i = 0; i < keys.size(); i++) {
                SessionKey key = (SessionKey)keys.get(i);
                Hash calcHash = key.calculateHash();
                ui.debugMessage("key " + i + " has hash: " + calcHash.toBase64() + " (data: " + Base64.encode(key.getData()) + ")");
                if (DataHelper.eq(calcHash.getData(), keyHash)) {
                    SyndieURI uri = SyndieURI.createKey(Hash.create(channel), key);
                    uriStr = uri.toString();
                    break;
                }
            }
        } else if (opts.getOptValue("postkey") != null) {
            type = "channel post key";
            byte channel[] = opts.getOptBytes("scope");
            byte keyHash[] = opts.getOptBytes("postkey");
            Hash chan = Hash.create(channel);
            List keys = client.getNymKeys(client.getLoggedInNymId(), client.getPass(), chan, Constants.KEY_FUNCTION_POST);
            ui.debugMessage("post keys for scope " + Base64.encode(channel) + ": " + keys.size()
                            + " (looking for " + Base64.encode(keyHash) + ")");
            for (int i = 0; i < keys.size(); i++) {
                NymKey key = (NymKey)keys.get(i);
                Hash calcHash = client.ctx().sha().calculateHash(key.getData());
                if (DataHelper.eq(calcHash.getData(), keyHash)) {
                    SigningPrivateKey priv = new SigningPrivateKey(key.getData());
                    SigningPublicKey expectedPub = KeyGenerator.getSigningPublicKey(priv);
                    long channelId = client.getChannelId(chan);
                    ChannelInfo info = client.getChannel(channelId);
                    
                    Set postKeys = info.getAuthorizedPosters();
                    boolean authorized = false;
                    for (Iterator iter = postKeys.iterator(); iter.hasNext(); ) {
                        SigningPublicKey pub = (SigningPublicKey)iter.next();
                        if (pub.equals(expectedPub)) {
                            authorized = true;
                            break;
                        }
                    }
                    if (!authorized) {
                        Set manageKeys = info.getAuthorizedManagers();
                        for (Iterator iter = manageKeys.iterator(); iter.hasNext(); ) {
                            SigningPublicKey pub = (SigningPublicKey)iter.next();
                            if (pub.equals(expectedPub)) {
                                authorized = true;
                                break;
                            }
                        }
                    }
                    if (!authorized) {
                        if (info.getIdentKey().equals(expectedPub)) {
                            authorized = true;
                        }
                    }
                    
                    if (!authorized) {
                        ui.errorMessage("The specified channel post key is not authorized to post to the channel");
                        return;
                    }
                    SyndieURI uri = SyndieURI.createKey(chan, Constants.KEY_FUNCTION_POST, priv);
                    uriStr = uri.toString();
                    break;
                }
            }
        }
        
        if ( (opts.getOptValue("managekey") != null) || 
             ( (opts.getOptValue("postkey") != null) && (uriStr == null) ) ) { // manage keys can be used to post
            byte keyHash[] = null;
            String keyType = null;
            if (opts.getOptValue("postkey") != null) {
                type = "channel post key";
                keyHash = opts.getOptBytes("postkey");
                keyType = Constants.KEY_FUNCTION_POST;
            } else {
                type = "channel manage key";
                keyHash = opts.getOptBytes("managekey");
                keyType = Constants.KEY_FUNCTION_MANAGE;
            }
            byte channel[] = opts.getOptBytes("scope");
            Hash chan = Hash.create(channel);
            List keys = client.getNymKeys(client.getLoggedInNymId(), client.getPass(), chan, Constants.KEY_FUNCTION_MANAGE);
            ui.debugMessage("manage keys for scope " + Base64.encode(channel) + ": " + keys.size()
                            + " (looking for " + Base64.encode(keyHash) + ")");
            for (int i = 0; i < keys.size(); i++) {
                NymKey key = (NymKey)keys.get(i);
                Hash calcHash = client.ctx().sha().calculateHash(key.getData());
                ui.debugMessage("key " + i + " has hash: " + calcHash.toBase64());
                if (DataHelper.eq(calcHash.getData(), keyHash)) {
                    SigningPrivateKey priv = new SigningPrivateKey(key.getData());
                    SigningPublicKey expectedPub = KeyGenerator.getSigningPublicKey(priv);
                    long channelId = client.getChannelId(chan);
                    ChannelInfo info = client.getChannel(channelId);

                    if (info == null) {
                        ui.errorMessage("We cannot verify the authorization of the key, as the channel is not known");
                        return;
                    }
                    
                    ui.debugMessage("channel found (" + channelId + "/" + info.getName() + ")");
                    boolean authorized = false;
                    Set manageKeys = info.getAuthorizedManagers();
                    for (Iterator iter = manageKeys.iterator(); iter.hasNext(); ) {
                        SigningPublicKey pub = (SigningPublicKey)iter.next();
                        if (pub.equals(expectedPub)) {
                            ui.debugMessage("Key is one of the authorized manager keys");
                            authorized = true;
                            break;
                        }
                    }
                    if (!authorized) {
                        if (info.getIdentKey().equals(expectedPub)) {
                            ui.debugMessage("Key is the identity key");
                            authorized = true;
                        }
                    }
                    
                    if (!authorized) {
                        ui.errorMessage("The specified channel manage key is not authorized to manage the channel");
                        return;
                    }
                    SyndieURI uri = SyndieURI.createKey(chan, keyType, priv);
                    uriStr = uri.toString();
                    ui.debugMessage("URI: " + uriStr);
                    break;
                }
            }
        } else if (opts.getOptValue("replykey") != null) {
            type = "channel reply key";
            byte channel[] = opts.getOptBytes("scope");
            byte keyHash[] = opts.getOptBytes("replykey");
            Hash chan = Hash.create(channel);
            List keys = client.getNymKeys(client.getLoggedInNymId(), client.getPass(), chan, Constants.KEY_FUNCTION_REPLY);
            for (int i = 0; i < keys.size(); i++) {
                NymKey key = (NymKey)keys.get(i);
                Hash calcHash = client.ctx().sha().calculateHash(key.getData());
                if (DataHelper.eq(calcHash.getData(), keyHash)) {
                    PrivateKey priv = new PrivateKey(key.getData());
                    PublicKey expectedPub = KeyGenerator.getPublicKey(priv);
                    long channelId = client.getChannelId(chan);
                    ChannelInfo info = client.getChannel(channelId);
                    
                    boolean authorized = false;
                    if (info.getEncryptKey().equals(expectedPub))
                        authorized = true;
                    
                    if (!authorized) {
                        ui.errorMessage("The specified channel reply key is not authorized to decrypt the channel's replies");
                        return;
                    }
                    SyndieURI uri = SyndieURI.createKey(chan, priv);
                    uriStr = uri.toString();
                    break;
                }
            }
        }
        
        if (uriStr == null) {
            ui.errorMessage("URI is required (--uri syndieURI)");
            ui.commandComplete(-1, null);
            return;
        }
        SyndieURI uri = null;
        try {
            if (uriStr.startsWith("http"))
                uri = SyndieURI.createURL(uriStr);
            else
                uri = new SyndieURI(uriStr);
        } catch (URISyntaxException use) {
            ui.errorMessage("URI is not valid (" + uriStr + ")", use);
            ui.commandComplete(-1, null);
            return;
        }

        if (name == null) name = type;
        _referenceNodes.add(new ReferenceNode(name, uri, desc, type));
        ui.statusMessage("Reference added");
    }
    
    private static class Walker implements ReferenceNode.Visitor {
        private int _nodes;
        public Walker() { _nodes = 0; }
        public void visit(ReferenceNode node, int depth, int siblingOrder) { _nodes++; }
        public int getNodeCount() { return _nodes; }
    }
    
    /** listrefs: display a list of references already added, prefixed by an index */
    private void processListRefs(DBClient client, UI ui, Opts opts) {
        if (_currentMessage == null) {
            ui.errorMessage("Can only list references once a target channel has been selected");
            ui.commandComplete(-1, null);
            return;
        }
        
        ui.statusMessage("References: ");
        ListWalker w = new ListWalker(ui);
        ReferenceNode.walk(_referenceNodes, w);
        ui.commandComplete(0, null);
    }
    
    private static class ListWalker implements ReferenceNode.Visitor {
        private UI _ui;
        private int _nodes;
        public ListWalker(UI ui) { _ui = ui; _nodes = 0; }
        public void visit(ReferenceNode node, int indent, int siblingOrder) {
            StringBuilder walked = new StringBuilder();
            walked.append(_nodes).append(": ");
            for (int i = 0; i < indent; i++)
                walked.append('\t');
            if (node.getName() != null)
                walked.append('\"').append(CommandImpl.strip(node.getName())).append("\" ");
            if (node.getURI() != null)
                walked.append(node.getURI().toString());
            _ui.statusMessage(walked.toString());
            walked.setLength(0);
            walked.append("   ");
            for (int i = 0; i < indent; i++)
                walked.append('\t');
            if (node.getDescription() != null)
                walked.append(CommandImpl.strip(node.getDescription())).append(" ");
            if (node.getReferenceType() != null)
                walked.append("(type: ").append(CommandImpl.strip(node.getReferenceType())).append(")");
            _ui.statusMessage(walked.toString());
            _nodes++;
        }
    }
    
    /** delref $index */
    private void processDelRef(DBClient client, UI ui, Opts opts) {
        if (_referenceNodes.size() <= 0) {
            ui.errorMessage("No references specified");
            ui.commandComplete(-1, null);
            return;
        }
        String arg = opts.getArg(0);
        if (arg == null) {
            ui.errorMessage("Usage: delref $num");
            ui.commandComplete(-1, null);
            return;
        }
        try {
            int num = Integer.parseInt(arg);
            DelWalker w = new DelWalker(ui, num);
            ReferenceNode.walk(_referenceNodes, w);
            if (w.refDeleted()) {
                ui.commandComplete(0, null);
            } else {
                ui.errorMessage("No reference at index " + num);
                ui.commandComplete(-1, null);
            }
        } catch (NumberFormatException nfe) {
            ui.errorMessage("Invalid reference number", nfe);
            ui.commandComplete(-1, null);
        }
    }
    
    private class DelWalker implements ReferenceNode.Visitor {
        private UI _ui;
        private int _nodes;
        private int _toDelete;
        private boolean _deleted;
        public DelWalker(UI ui, int toDelete) { 
            _ui = ui; 
            _nodes = 0; 
            _toDelete = toDelete; 
            _deleted = false; 
        }
        public boolean refDeleted() { return _deleted; }
        public void visit(ReferenceNode node, int indent, int siblingOrder) {
            if (_nodes > _toDelete) {
                return;
            } else if (_nodes == _toDelete) {
                _nodes++;
                if (node.getChildCount() == 0) {
                    _ui.statusMessage("Removing reference node " + _toDelete + " (" + node.getName() + ")");
                    ReferenceNode parent = node.getParent();
                    if (parent != null) {
                        parent.removeChild(node);
                    } else {
                        for (int i = 0; i < _referenceNodes.size(); i++) {
                            if (_referenceNodes.get(i) == node) {
                                _referenceNodes.remove(i);
                                break;
                            }
                        }
                    }
                    _deleted = true;
                    return;
                } else {
                    _ui.errorMessage("Not removing reference node " + _toDelete + " - please remove its children first");
                    return;
                }
            } else {
                _nodes++;
            }
        }
    }
    
    /** addparent --uri $uri [--order $num] */
    private void processAddParent(DBClient client, UI ui, Opts opts) {
        if (_currentMessage == null) {
            ui.errorMessage("Can only add parents once a target channel has been selected");
            ui.commandComplete(-1, null);
            return;
        }
        String uriStr = opts.getOptValue("uri");
        int index = (int)opts.getOptLong("order", _parents.size());
        SyndieURI uri = null;
        try {
            uri = new SyndieURI(uriStr);
            if ( (uri.getScope() != null) && (uri.getMessageId() != null) ) {
                if ( (index >= 0) && (index <= _parents.size()) ) {
                    _parents.add(index, uri);
                    ui.statusMessage("Parent URI added");
                    ui.commandComplete(0, null);
                } else {
                    ui.errorMessage("Order is out of range");
                    ui.commandComplete(-1, null);
                }
            } else {
                ui.errorMessage("URI is valid, but does not refer to a message");
                ui.commandComplete(-1, null);
            }
        } catch (URISyntaxException use) {
            ui.errorMessage("URI is not valid", use);
            ui.commandComplete(-1, null);
        }
    }
    
    /**  listparents        : display a list of URIs this new post will be marked as replying to (most recent parent at index 0) */
    private void processListParents(DBClient client, UI ui, Opts opts) {
        ui.statusMessage("Parents (most recent first):");
        for (int i = 0; i < _parents.size(); i++) {
            SyndieURI uri = (SyndieURI)_parents.get(i);
            long id = client.getChannelId(uri.getScope());
            MessageInfo msg = null;
            if (id >= 0) {
                msg = client.getMessage(id, uri.getMessageId());
                if (msg != null) {
                    ui.statusMessage(i + ": " + msg.getTargetChannel().toBase64().substring(0,6)
                                     + " - '" + CommandImpl.strip(msg.getSubject()) + "' (" + msg.getMessageId() + ")");
                }
            }
            if (msg == null)
                ui.statusMessage(i + ": " + uri.getScope().toBase64().substring(0,6) + " (" + uri.getMessageId().longValue() + ")");
        }
        ui.commandComplete(0, null);
    }
    /** delparent $index */
    private void processDelParent(DBClient client, UI ui, Opts opts) {
        if (_parents.size() <= 0) {
            ui.errorMessage("No parents specified");
            ui.commandComplete(-1, null);
            return;
        }
        String arg = opts.getArg(0);
        if (arg == null) {
            ui.errorMessage("Usage: delparent $num");
            ui.commandComplete(-1, null);
            return;
        }
        try {
            int num = Integer.parseInt(arg);
            if ( (num >= 0) && (num < _parents.size()) ) {
                SyndieURI uri = (SyndieURI)_parents.remove(num);
                ui.statusMessage("Parent removed: " + uri);
                ui.commandComplete(0, null);
            } else {
                ui.errorMessage("Index out of bounds");
                ui.commandComplete(-1, null);
            }
        } catch (NumberFormatException nfe) {
            ui.errorMessage("Invalid index", nfe);
            ui.commandComplete(-1, null);
        }
    }

    /**
     * execute [--out $filename] : actually generate the post, exporting it to 
     * the given file, and then importing it into the local database
     */
    private void processExecute(DBClient client, UI ui, Opts opts) {
        if (_currentMessage == null) {
            ui.errorMessage("No post in progress");
            ui.commandComplete(-1, null);
            return;
        }
        
        long scopeId = _currentMessage.getScopeChannelId();
        if (scopeId < 0) {
            ui.errorMessage("No scope specified?");
            ui.commandComplete(-1, null);
            return;
        }
        ChannelInfo scopeChan = client.getChannel(scopeId); // not necessarily == targetChannelId!
        
        String out = opts.getOptValue("out");
        if (out == null) {
            File chanDir = new SecureFile(client.getOutboundDir(), scopeChan.getChannelHash().toBase64());
            chanDir.mkdirs();
            File msgFile = new File(chanDir, _currentMessage.getMessageId() + Constants.FILENAME_SUFFIX);
            out = msgFile.getPath();
            //ui.errorMessage("Output file must be specified with --out $filename");
            //ui.commandComplete(-1, null);
            //return;
        }
        
        File tmpDir = client.getTempDir();
        tmpDir.mkdirs();
        
        List cfgFiles = new ArrayList();
        File refFile = null;
        
        MessageGen cmd = new MessageGen();
        Opts genOpts = new Opts();
        genOpts.setCommand("messagegen");
        if (_currentMessage.getTargetChannel() != null) {
            genOpts.setOptValue("targetChannel", _currentMessage.getTargetChannel().toBase64());
        }
        genOpts.addOptValue("scopeChannel", scopeChan.getChannelHash().toBase64());
        
        for (int i = 0; i < _pageFiles.size(); i++) {
            String filename = (String)_pageFiles.get(i);
            Properties cfg = (Properties)_pageConfig.get(i);
            FileOutputStream fos = null;
            try {
                File cfgFile = SecureFile.createTempFile("pageConfig", ""+ i, tmpDir);
                fos = new SecureFileOutputStream(cfgFile);
                for (Iterator iter = cfg.keySet().iterator(); iter.hasNext(); ) {
                    String name = (String)iter.next();
                    String val = cfg.getProperty(name);
                    fos.write(DataHelper.getUTF8(CommandImpl.strip(name) + "=" + CommandImpl.strip(val.trim()) + "\n"));
                }
                fos.close();
                fos = null;
                cfgFiles.add(cfgFile);
                genOpts.setOptValue("page" + i, filename);
                genOpts.setOptValue("page" + i + "-config", cfgFile.getPath());
            } catch (IOException ioe) {
                ui.errorMessage("Error writing out the configuration for page " + i, ioe);
                ui.commandComplete(-1, null);
                return;
            }
        }

        for (int i = 0; i < _attachmentFiles.size(); i++) {
            String filename = (String)_attachmentFiles.get(i);
            Properties cfg = (Properties)_attachmentConfig.get(i);
            FileOutputStream fos = null;
            try {
                File cfgFile = SecureFile.createTempFile("attachConfig", ""+ i, tmpDir);
                fos = new SecureFileOutputStream(cfgFile);
                for (Iterator iter = cfg.keySet().iterator(); iter.hasNext(); ) {
                    String name = (String)iter.next();
                    String val = cfg.getProperty(name);
                    fos.write(DataHelper.getUTF8(CommandImpl.strip(name) + "=" + CommandImpl.strip(val.trim()) + "\n"));
                }
                fos.close();
                fos = null;
                cfgFiles.add(cfgFile);
                genOpts.setOptValue("attach" + i, filename);
                genOpts.setOptValue("attach" + i + "-config", cfgFile.getPath());
            } catch (IOException ioe) {
                ui.errorMessage("Error writing out the configuration for attachment " + i, ioe);
                ui.commandComplete(-1, null);
                return;
            }
        }

        if (_authenticationKey != null)
            //genOpts.setOptValue("authenticationKey", Base64.encode(_authenticationKey.getData()));
            genOpts.setOptValue("authenticationKey", client.ctx().sha().calculateHash(_authenticationKey.getData()).toBase64());
        if (_authorizationKey != null) {
            //genOpts.setOptValue("authorizationKey", Base64.encode(_authorizationKey.getData()));
            genOpts.setOptValue("authorizationKey", client.ctx().sha().calculateHash(_authorizationKey.getData()).toBase64());
        } else {
            boolean noAuthRequired = false;
            if (_currentMessage.getTargetChannelId() >= 0) {
                ChannelInfo target = client.getChannel(_currentMessage.getTargetChannelId());
                if (target.getAllowPublicPosts()) {
                    noAuthRequired = true;
                } else if (target.getAllowPublicReplies()) {
                    List parents = _currentMessage.getHierarchy();
                    if (parents != null) {
                        for (int i = 0; i < parents.size(); i++) {
                            SyndieURI parent = (SyndieURI)parents.get(i);
                            Set allowed = new HashSet();
                            for (Iterator iter = target.getAuthorizedManagers().iterator(); iter.hasNext(); )
                                allowed.add(((SigningPublicKey)iter.next()).calculateHash());
                            for (Iterator iter = target.getAuthorizedPosters().iterator(); iter.hasNext(); )
                                allowed.add(((SigningPublicKey)iter.next()).calculateHash());
                            allowed.add(target.getChannelHash());
                            if (allowed.contains(parent.getScope())) {
                                noAuthRequired = true;
                                break;
                            }
                        }
                    }
                }
            }
            if (!noAuthRequired)
                genOpts.setOptValue("postAsUnauthorized", "true");
        }
        
        if (_currentMessage.getMessageId() >= 0)
            genOpts.setOptValue("messageId", Long.toString(_currentMessage.getMessageId()));
        if (_currentMessage.getSubject() != null)
            genOpts.setOptValue("subject", _currentMessage.getSubject());
 
        if ( (_passphrase != null) && (_passphrasePrompt != null) ) {
            genOpts.setOptValue("bodyPassphrase", CommandImpl.strip(_passphrase));
            genOpts.setOptValue("bodyPassphrasePrompt", CommandImpl.strip(_passphrasePrompt));
        } else if ( (_publiclyReadable != null) && (_publiclyReadable.booleanValue()) ) {
            genOpts.setOptValue("encryptContent", "false"); // if true, encrypt the content with a known read key for the channel
        }
        
        if (_avatarFile != null)
            genOpts.setOptValue("avatar", _avatarFile);
        
        if (_currentMessage.getWasPrivate())
            genOpts.setOptValue("postAsReply", "true"); // if true, the post should be encrypted to the channel's reply key
        
        if (_currentMessage.getPublicTags() != null) {
            for (Iterator iter = _currentMessage.getPublicTags().iterator(); iter.hasNext(); )
                genOpts.addOptValue("pubTag", (String)iter.next());
        }
        if (_currentMessage.getPrivateTags() != null) {
            for (Iterator iter = _currentMessage.getPrivateTags().iterator(); iter.hasNext(); )
                genOpts.addOptValue("privTag", (String)iter.next());
        }
        if (_referenceNodes.size() > 0) {
            String refs = ReferenceNode.walk(_referenceNodes);
            FileOutputStream fos = null;
            try {
                refFile = SecureFile.createTempFile("refs", "txt", tmpDir);
                fos = new SecureFileOutputStream(refFile);
                fos.write(DataHelper.getUTF8(refs));
                fos.close();
                genOpts.setOptValue("refs", refFile.getPath());
                ui.debugMessage("Pulling refs from " + refFile.getPath());
            } catch (IOException ioe) {
                ui.errorMessage("Error writing out the references", ioe);
                ui.commandComplete(-1, null);
                return;
            }
        }
        //* (--cancel $uri)*                // posts to be marked as cancelled (only honored if authorized to do so for those posts)
        
        // replace the $uri with the current post, if authorized to do so
        if ( (_currentMessage.getOverwriteChannel() != null) && (_currentMessage.getOverwriteMessage() >= 0) )
            genOpts.setOptValue("overwrite", SyndieURI.createMessage(_currentMessage.getOverwriteChannel(), _currentMessage.getOverwriteMessage()).toString());

        if ( (_parents != null) && (_parents.size() > 0) ) {
            StringBuilder buf = new StringBuilder();
            for (int i = 0; i < _parents.size(); i++) {
                SyndieURI uri = (SyndieURI)_parents.get(i);
                buf.append(uri.toString());
                if (i + 1 < _parents.size())
                    buf.append(",");
            }
            genOpts.setOptValue("references", buf.toString());
        }

        if (_currentMessage.getExpiration() > 0)
            genOpts.setOptValue("expiration", _dayFmt.format(new Date(_currentMessage.getExpiration())));
        
        genOpts.setOptValue("forceNewThread", ""+_currentMessage.getForceNewThread());
        genOpts.setOptValue("refuseReplies", ""+_currentMessage.getRefuseReplies());
        
        genOpts.setOptValue("out", out);
        
        NestedUI nestedUI = new NestedUI(ui);
        ui.debugMessage("generating with opts: " + genOpts);
        cmd.runCommand(genOpts, nestedUI, client);
        if (nestedUI.getExitCode() >= 0) {
            // generated fine, so lets import 'er
            ui.statusMessage("Message generated and written to " + out);
            
            Importer msgImp = new Importer();
            Opts msgImpOpts = new Opts();
            msgImpOpts.setOptValue("in", out);
            if (_passphrase != null)
                msgImpOpts.setOptValue("passphrase", CommandImpl.strip(_passphrase));
            msgImpOpts.setCommand("import");
            NestedUI dataNestedUI = new NestedUI(ui);
            msgImp.runCommand(msgImpOpts, dataNestedUI, client);
            if (dataNestedUI.getExitCode() < 0) {
                ui.debugMessage("Failed in the nested import command");
                ui.commandComplete(dataNestedUI.getExitCode(), null);
            } else {
                ui.statusMessage("Post imported");
                ui.commandComplete(0, null);
                resetContent();
            }
        } else {
            ui.errorMessage("Error generating the message");
            ui.commandComplete(nestedUI.getExitCode(), null);
        }
        
        for (int i = 0; i < cfgFiles.size(); i++)
            ((File)cfgFiles.get(i)).delete();
        if (refFile != null)
            refFile.delete();        
    }
    
    /** listreadkeys: display a list of known channel read keys that we can use to encrypt the message */
    private void processListReadKeys(DBClient client, UI ui, Opts opts) {
        if (_currentMessage == null) {
            ui.errorMessage("No post in progress");
            ui.commandComplete(-1, null);
            return;
        }
        Hash channel = _currentMessage.getTargetChannel();
        List keys = client.getNymKeys(client.getLoggedInNymId(), client.getPass(), channel, Constants.KEY_FUNCTION_READ);
        _listedNymKeys.clear();
        for (int i = 0; i < keys.size(); i++) {
            NymKey key = (NymKey)keys.get(i);
            ui.statusMessage("key " + _listedNymKeys.size() + ": " + key.getType() + " for " + key.getChannel().toBase64().substring(0,6) + " (authenticated? " + key.getAuthenticated() + ")");
            _listedNymKeys.add(key);
        }        
        ui.commandComplete(0, null);
    }
    
    private void processSet(DBClient client, UI ui, Opts opts) {
        for (Iterator iter = opts.getOptNames().iterator(); iter.hasNext(); ) {
            String opt = (String)iter.next();
            if ("readkey".equalsIgnoreCase(opt)) {
                //set --readkey (public|$index)
                if ("public".equalsIgnoreCase(opts.getOptValue(opt))) {
                    _publiclyReadable = Boolean.TRUE;
                    _currentMessage.setWasPassphraseProtected(false);
                    ui.statusMessage("Public read key selected");
                } else if ("pbe".equalsIgnoreCase(opts.getOptValue(opt))) {
                    _publiclyReadable = Boolean.FALSE;
                    _passphrase = opts.getOptValue("passphrase");
                    _passphrasePrompt = opts.getOptValue("prompt");
                    if ( (_passphrase == null) || (_passphrasePrompt == null) ) {
                        ui.errorMessage("You must specify a --passphrase and a --prompt to use the passphrase base encryption");
                        ui.commandComplete(-1, null);
                        return;
                    }
                    _publiclyReadable = Boolean.FALSE;
                    _currentMessage.setWasPassphraseProtected(true);
                    ui.statusMessage("Passphrase based read key generated");
                } else {
                    int index = (int)opts.getOptLong(opt, -1);
                    if ( (index >= 0) && (index < _listedNymKeys.size()) ) {
                        Object o = _listedNymKeys.get(index);
                        if (o instanceof NymKey) {
                            _readKey = new SessionKey(((NymKey)o).getData());
                            _publiclyReadable = Boolean.FALSE;
                            _currentMessage.setWasPassphraseProtected(false);
                            ui.statusMessage("Read key selected");
                        } else {
                            ui.errorMessage("Please call listreadkeys before using set --readkey");
                            ui.commandComplete(-1, null);
                            return;
                        }
                    } else {
                        ui.errorMessage("Read key index out of range - please use a valid number or 'public'");
                        ui.commandComplete(-1, null);
                        return;
                    }
                }
            } else if ("messageId".equalsIgnoreCase(opt)) {
                // set --messageId ($id|date) : specify the message Id, or if 'date', generate one based on the date
                if ("date".equalsIgnoreCase(opts.getOptValue(opt))) {
                    _currentMessage.setMessageId(createEdition(client));
                    ui.statusMessage("MessageId randomized based on the date and set to " + _currentMessage.getMessageId());
                } else {
                    long id = opts.getOptLong(opt, -1);
                    if (id >= 0) {
                        _currentMessage.setMessageId(id);
                        ui.statusMessage("MessageId set to " + id);
                    } else {
                        ui.errorMessage("Invalid message id requested - please specify a number or the value 'date'");
                        ui.commandComplete(-1, null);
                        return;
                    }
                }
            } else if ("subject".equalsIgnoreCase(opt)) {
                // set --subject $subject : specify the message subject
                _currentMessage.setSubject(CommandImpl.strip(opts.getOptValue(opt)));
                ui.statusMessage("Subject set to " + _currentMessage.getSubject());
            } else if ("avatar".equalsIgnoreCase(opt)) {
                // set --avatar $filename : specify a message-specific avatar to use
                _avatarFile = opts.getOptValue(opt);
                File f = new File(_avatarFile);
                if (f.exists()) {
                    if (f.length() > Constants.MAX_AVATAR_SIZE) {
                        ui.errorMessage("Avatar file requested is too large (" + f.length() + ", max size " + Constants.MAX_AVATAR_SIZE + ")");
                        ui.commandComplete(-1, null);
                        return;
                    }
                    ui.statusMessage("Message-specific avatar selected");
                } else {
                    ui.errorMessage("Avatar file requested does not exist (" + _avatarFile + ")");
                    ui.commandComplete(-1, null);
                    _avatarFile = null;
                    return;
                }
            } else if ("encryptToReply".equalsIgnoreCase(opt)) {
                // set --encryptToReply $boolean
                _currentMessage.setWasPrivate(opts.getOptBoolean(opt, _currentMessage.getWasPrivate()));
                if (_currentMessage.getWasPrivate())
                    ui.statusMessage("Message will be encrypted to the channel owner's reply key");
                else
                    ui.statusMessage("Message will be encrypted as a normal channel post");
            } else if ("overwrite".equalsIgnoreCase(opt)) {
                // set --overwrite $uri
                try {
                    SyndieURI uri = new SyndieURI(opts.getOptValue(opt));
                    if ( (uri.getScope() == null) || (uri.getMessageId() == null) ) {
                        ui.errorMessage("You can only overwrite syndie messages");
                        ui.commandComplete(-1, null);
                        return;
                    }
                    _currentMessage.setOverwriteChannel(uri.getScope());
                    _currentMessage.setOverwriteMessage(uri.getMessageId().longValue());
                    ui.statusMessage("Post set to overwrite " + uri.getScope().toBase64() + ":" + uri.getMessageId().longValue());
                } catch (URISyntaxException use) {
                    ui.errorMessage("Invalid syndie overwrite URI: " + opts.getOptValue(opt), use);
                    ui.commandComplete(-1, null);
                    return;
                }
            } else if ("expiration".equalsIgnoreCase(opt)) {
                // set --expiration ($yyyyMMdd|none) : suggest a date on which the message can be discarded
                String val = opts.getOptValue(opt);
                if ("none".equalsIgnoreCase(val)) {
                    _currentMessage.setExpiration(-1);
                    ui.statusMessage("Post configured to have no expiration");
                } else {
                    try {
                        Date when = _dayFmt.parse(val);
                        _currentMessage.setExpiration(when.getTime());
                        ui.statusMessage("Post configured with a suggested expiration of " + val);
                    } catch (ParseException pe) {
                        ui.errorMessage("Invalid expiration requested (please specify YYYYMMDD)", pe);
                        ui.commandComplete(-1, null);
                        return;
                    }
                }
            } else if ("forceNewThread".equalsIgnoreCase(opt)) {
                _currentMessage.setForceNewThread(opts.getOptBoolean(opt, _currentMessage.getForceNewThread()));
                ui.statusMessage("Post " + (_currentMessage.getForceNewThread() ? "will " : "will not") 
                                 + " force a new discussion thread to be started");
            } else if ("refuseReplies".equalsIgnoreCase(opt)) {
                _currentMessage.setRefuseReplies(opts.getOptBoolean(opt, _currentMessage.getRefuseReplies()));
                ui.statusMessage("Post " + (_currentMessage.getForceNewThread() ? "will " : "will not") 
                                 + " allow other people to reply to it directly");
            } else if ("publicTags".equalsIgnoreCase(opt)) {
                String tags = opts.getOptValue(opt);
                Set pubTags = new HashSet();
                while (tags != null) {
                    int split = tags.indexOf(',');
                    if (split < 0) {
                        pubTags.add(CommandImpl.strip(tags.trim()));
                        tags = null;
                    } else if (split == 0) {
                        tags = tags.substring(1);
                    } else {
                        String tag = CommandImpl.strip(tags.substring(0, split).trim());
                        if (tag.length() > 0)
                            pubTags.add(tag);
                        tags = tags.substring(split+1);
                    }
                }
                _currentMessage.setPublicTags(pubTags);
            } else if ("privateTags".equalsIgnoreCase(opt)) {
                String tags = opts.getOptValue(opt);
                Set privTags= new HashSet();
                while (tags != null) {
                    int split = tags.indexOf(',');
                    if (split < 0) {
                        privTags.add(CommandImpl.strip(tags.trim()));
                        tags = null;
                    } else if (split == 0) {
                        tags = tags.substring(1);
                    } else {
                        String tag = CommandImpl.strip(tags.substring(0, split).trim());
                        if (tag.length() > 0)
                            privTags.add(tag);
                        tags = tags.substring(split+1);
                    }
                }
                _currentMessage.setPrivateTags(privTags);
            }
        }
        ui.commandComplete(0, null);
    }
    
    public static void main(String args[]) {
        String rootDir = TextEngine.getRootPath();
        TextUI ui = new TextUI(true);
        TextEngine engine = new TextEngine(rootDir, ui);
        ui.insertCommand("login");
        ui.insertCommand("menu post");
        ui.insertCommand("channels");
        ui.insertCommand("create --channel 1");
        ui.insertCommand("listauthkeys");
        ui.insertCommand("authenticate 5");
        ui.insertCommand("authorize 0");
        ui.insertCommand("set --readkey pbe --passphrase 'you smell' --prompt 'do i smell?'");
        ui.insertCommand("execute");
        engine.run();
    }
}
