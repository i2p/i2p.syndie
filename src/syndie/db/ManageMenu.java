package syndie.db;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.text.ParseException;
import java.util.*;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.SimpleDateFormat;

import net.i2p.crypto.KeyGenerator;
import net.i2p.data.Base64;
import net.i2p.data.Hash;
import net.i2p.data.SigningPrivateKey;
import net.i2p.data.SigningPublicKey;
import net.i2p.util.SecureFile;
import net.i2p.util.SecureFileOutputStream;

import syndie.Constants;
import syndie.data.ArchiveInfo;
import syndie.data.ChannelInfo;
import syndie.data.Enclosure;
import syndie.data.NymKey;
import syndie.data.SyndieURI;

/**
 *
 */
class ManageMenu implements TextEngine.Menu {
    private TextEngine _engine;
    /** text description of each indexed channel */
    private List _itemText;
    /** internal channel id (Long) for each indexed item */
    private List _itemKeys;
    /** if true, the items refer to a list of channels matching the requested criteria */
    private boolean _itemIsChannelList;
    /** refers to the next index into the item lists that the user should be shown */
    private int _itemIteratorIndex;
    /** current channel the user is working on (if any) */
    private ChannelInfo _currentChannel;
    /** filename to pull the channel avatar from */
    private String _avatar;
    /** filename to pull the channel references from */
    private String _refs;
    /** if true, don't publicize the keys to decrypt the metadata content, and put a private read key in it */
    private Boolean _encryptContent;
    private String _bodyPassphrase;
    private String _bodyPassphrasePrompt;
    /** SigningPublicKey of listed nyms */
    private List _listedNymKeys;
    
    public ManageMenu(TextEngine engine) {
        _engine = engine;
        _itemText = new ArrayList();
        _itemKeys = new ArrayList();
        _listedNymKeys = new ArrayList();
        _itemIsChannelList = false;
        _itemIteratorIndex = 0;
        _currentChannel = null;
        _avatar = null;
        _refs = null;
        _encryptContent = null;
    }
    
    public static final String NAME = "manage";
    public String getName() { return NAME; }
    public String getDescription() { return "channel management menu"; }
    public boolean requireLoggedIn() { return true; }

    public void listCommands(UI ui) {
        // alphabetical please
        if (_currentChannel != null) {
            ui.statusMessage(" addnym (--nym $index | --key $base64(pubKey)) --action (manage|post)");
            ui.statusMessage(" cancel             : clear the current create|update state without updating anything");
        }
        ui.statusMessage(" channels           : display a list of channels the current nym can manage");
        if (_currentChannel == null) {
            ui.statusMessage(" create             : begin the process of creating a new channel");
            ui.statusMessage(" execute --out $outputDir: create/update the channel, generating the metadata and ");
            ui.statusMessage("                    : private keys in the given dir, and importing them into the current ");
            ui.statusMessage("                    : nym.  also clears the current create or update state");
        }
        if (_currentChannel != null) {
            ui.statusMessage(" listnyms [--name $namePrefix] [--channel $hashPrefix]");
            ui.statusMessage("                    : list locally known nyms matching the criteria");
        }
        ui.statusMessage(" meta [--channel ($index|$hash)] : display the channel's metadata");
        if (_itemIsChannelList) {
            ui.statusMessage(" next [--lines $num]: paginate through the channels, 10 or $num at a time");
            ui.statusMessage(" prev [--lines $num]: paginate through the channels, 10 or $num at a time");
        }
        if (_currentChannel != null) {
            ui.statusMessage(" preview            : summarize the channel configuration");
            ui.statusMessage(" removenym (--nym $index | --key $base64(pubKey)) --action (manage|post)");
        }
        if (_currentChannel != null) {
            ui.statusMessage(" set [$opts]        : set various options on the channel being created/updated,");
            ui.statusMessage("                    : using the options from the ChanGen command");
        }
        if (_currentChannel == null) {
            ui.statusMessage(" update --channel ($index|$hash): begin the process of updating an existing channel");
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
        } else if ("create".equalsIgnoreCase(cmd)) {
            processCreate(client, ui, opts);
        } else if ("update".equalsIgnoreCase(cmd)) {
            processUpdate(client, ui, opts);
        } else if ("cancel".equalsIgnoreCase(cmd)) {
            _currentChannel = null;
            _avatar = null;
            _refs = null;
            _encryptContent = null;
            _bodyPassphrase = null;
            _bodyPassphrasePrompt = null;
            ui.statusMessage("Process cancelled");
            ui.commandComplete(0, null);
        } else if ("set".equalsIgnoreCase(cmd)) {
            processSet(client, ui, opts);
        } else if ("listnyms".equalsIgnoreCase(cmd)) {
            processListNyms(client, ui, opts);
        } else if ("addnym".equalsIgnoreCase(cmd)) {
            processAddNym(client, ui, opts);
        } else if ("removenym".equalsIgnoreCase(cmd)) {
            processRemoveNym(client, ui, opts);
        } else if ("preview".equalsIgnoreCase(cmd)) {
            processPreview(client, ui, opts);
        } else if ("execute".equalsIgnoreCase(cmd)) {
            processExecute(client, ui, opts);
        } else {
            return false;
        }
        return true;
    }
    public List getMenuLocation(DBClient client, UI ui) {
        List rv = new ArrayList();
        rv.add("manage");
        if ( (_currentChannel != null) && (_currentChannel.getChannelHash() != null) ) {
            rv.add("update " + CommandImpl.strip(_currentChannel.getName()) + "/" + _currentChannel.getChannelHash().toBase64().substring(0,6));
        } else if (_currentChannel != null) {
            rv.add("create");
        }
        return rv;
    }
    
    private static final SimpleDateFormat _dayFmt = new SimpleDateFormat("yyyy/MM/dd");
    private static final String SQL_LIST_MANAGED_CHANNELS = "SELECT channelId FROM channelManageKey WHERE authPubKey = ?";
    /** channels */
    private void processChannels(DBClient client, UI ui, Opts opts) {
        _itemIteratorIndex = 0;
        _itemIsChannelList = true;
        _itemKeys.clear();
        _itemText.clear();
        
        List manageKeys = client.getNymKeys(client.getLoggedInNymId(), client.getPass(), null, Constants.KEY_FUNCTION_MANAGE);
        List pubKeys = new ArrayList();
        // first, go through and find all the 'identity' channels - those that we have
        // the actual channel signing key for
        for (int i = 0; i < manageKeys.size(); i++) {
            NymKey key = (NymKey)manageKeys.get(i);
            if (key.getAuthenticated()) {
                SigningPrivateKey priv = new SigningPrivateKey(key.getData());
                SigningPublicKey pub = KeyGenerator.getSigningPublicKey(priv);
                pubKeys.add(pub);
                Hash chan = pub.calculateHash();
                long chanId = client.getChannelId(chan);
                if (chanId >= 0) {
                    ChannelInfo info = client.getChannel(chanId);
                    _itemKeys.add(new Long(chanId));
                    _itemText.add("Identity channel " + CommandImpl.strip(info.getName()) + " (" + chan.toBase64().substring(0,6) + "): " + CommandImpl.strip(info.getDescription()));
                }
            } else {
                ui.debugMessage("Nym ky is not authenticated: " + key.getChannel().toBase64());
            }
        }
        
        // now, go through and see what other channels our management keys are
        // authorized to manage (beyond their identity channels)
        Connection con = client.con();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = con.prepareStatement(SQL_LIST_MANAGED_CHANNELS);
            for (int i = 0; i < pubKeys.size(); i++) {
                SigningPublicKey key = (SigningPublicKey)pubKeys.get(i);
                stmt.setBytes(1, key.getData());
                rs = stmt.executeQuery();
                while (rs.next()) {
                    // channelId
                    long chanId = rs.getLong(1);
                    if (!rs.wasNull()) {
                        Long id = new Long(chanId);
                        if (!_itemKeys.contains(id)) {
                            ChannelInfo info = client.getChannel(chanId);
                            if (info != null) {
                                _itemKeys.add(id);
                                _itemText.add("Authorized channel " + CommandImpl.strip(info.getName()) + " (" + info.getChannelHash().toBase64().substring(0,6) + "): " + CommandImpl.strip(info.getDescription()));
                            }
                        }
                    }
                }
                rs.close();
            }
        } catch (SQLException se) {
            ui.errorMessage("Internal error listing channels", se);
            ui.commandComplete(-1, null);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
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
                ui.statusMessage(name + " " + _itemIteratorIndex + " through " + (end-1) + " of " + _itemKeys.size());
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
    
    private void processCreate(DBClient client, UI ui, Opts opts) {
        if (_currentChannel != null) {
            ui.errorMessage("Cannot create a new channel - an existing create process is already in progress");
            ui.errorMessage("Cancel or complete that process before continuing (with the cancel or execute commands)");
            ui.commandComplete(-1, null);
            return;
        }
        _currentChannel = new ChannelInfo();
        _avatar = null;
        _refs = null;
        _encryptContent = null;
        
        // now populate it with some default values
        SigningPublicKey nymPub = getNymPublicKey(client);
        if (nymPub != null) {
            ui.debugMessage("Nym identity channel public key guessed, adding it as a manager to the new channel");
            Set managers = new HashSet();
            managers.add(nymPub);
            _currentChannel.setAuthorizedManagers(managers);
        } else {
            _currentChannel.setAuthorizedManagers(new HashSet());
        }
        
        _currentChannel.setAuthorizedPosters(new HashSet());
        _currentChannel.setPrivateArchives(new HashSet());
        _currentChannel.setPrivateHeaders(new Properties());
        _currentChannel.setPrivateTags(new HashSet());
        _currentChannel.setPublicArchives(new HashSet());
        _currentChannel.setPublicHeaders(new Properties());
        _currentChannel.setPublicTags(new HashSet());
        _currentChannel.setReadKeys(new HashSet());
        _currentChannel.setReferences(new ArrayList());
        
        _currentChannel.setAllowPublicPosts(false);
        _currentChannel.setAllowPublicReplies(false);
        _currentChannel.setEdition(createEdition(client));
        _currentChannel.setName("Default channel name");
        
        ui.statusMessage("Channel creation process initiated");
        ui.statusMessage("Please specify fields as necessary with 'set', and complete the");
        ui.statusMessage("channel creation process with 'execute', or cancel the process with 'cancel'");
        ui.commandComplete(0, null);
    }
    
    SigningPublicKey getNymPublicKey(DBClient client) {
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
    
    /** see DBClient.createEdition() */
    private long createEdition(DBClient client) { return client.createEdition(_currentChannel.getEdition()); }

    /** update --channel ($index|$hash): begin the process of updating an existing channel */
    private void processUpdate(DBClient client, UI ui, Opts opts) {
        if (_currentChannel != null) {
            ui.errorMessage("Cannot update an existing channel - an existing create process is already in progress");
            ui.errorMessage("Cancel or complete that process before continuing (with the cancel or execute commands)");
            ui.commandComplete(-1, null);
            return;
        }
        
        String chan = opts.getOptValue("channel");
        if (chan == null) {
            ui.errorMessage("Please specify the channel to update with --channel $index or --channel $hash");
            ui.commandComplete(-1, null);
            return;
        }
        
        try {
            int index = Integer.parseInt(chan);
            if ( (index >= 0) && (index < _itemKeys.size()) ) {
                long id = ((Long)_itemKeys.get(index)).longValue();
                _currentChannel = client.getChannel(id);
            } else {
                ui.errorMessage("Channel index out of range (channel count: " + _itemKeys.size() + ")");
                ui.commandComplete(-1, null);
                return;
            }
        } catch (NumberFormatException nfe) {
            byte h[] = Base64.decode(chan);
            if ( (h != null) && (h.length == Hash.HASH_LENGTH) ) {
                long id = client.getChannelId(Hash.create(h));
                if (id >= 0) {
                    _currentChannel = client.getChannel(id);
                } else {
                    ui.errorMessage("Channel " + chan + " is not known");
                    ui.commandComplete(-1, null);
                    return;
                }
            }
        }
        
        if (_currentChannel == null) {
            ui.errorMessage("Invalid channel requested: " + chan);
            ui.commandComplete(-1, null);
            return;
        }
        
        // now populate it with some default values
        long ed = createEdition(client);
        if (ed <= _currentChannel.getEdition())
            ed = _currentChannel.getEdition() + client.ctx().random().nextLong(1000);
        _currentChannel.setEdition(ed);
        _avatar = null;
        _refs = null;
        _encryptContent = null;
        
        ui.statusMessage("Channel update process initiated");
        ui.statusMessage("Please specify fields as necessary with 'set', and complete the");
        ui.statusMessage("channel update process with 'execute', or cancel the process with 'cancel'");
        ui.commandComplete(0, null);
    }
    
    private void processSet(DBClient client, UI ui, Opts opts) {
        if (_currentChannel == null) {
            ui.errorMessage("Create/update process not yet initiated");
            ui.commandComplete(-1, null);
            return;
        }
        ui.debugMessage("updating fields: " + opts.getOptNames());
        String name = opts.getOptValue("name");
        if (name != null) {
            _currentChannel.setName(CommandImpl.strip(name));
            ui.statusMessage("Updated channel name");
        }
        
        String desc = opts.getOptValue("description");
        if (desc != null) {
            _currentChannel.setDescription(CommandImpl.strip(desc));
            ui.statusMessage("Updated channel description");
        }
        
        String avatar = opts.getOptValue("avatar");
        if (avatar != null) {
            File f = new File(avatar);
            if (f.exists()) {
                if (f.length() > Constants.MAX_AVATAR_SIZE) {
                    ui.errorMessage("Avatar file is too large (" + f.length() + ", max " + Constants.MAX_AVATAR_SIZE + ")");
                } else {
                    _avatar = avatar;
                    ui.statusMessage("Updated channel avatar");
                }
            } else {
                ui.errorMessage("Avatar file does not exist");
                _avatar = null;
            }
        }
        
        String edVal = opts.getOptValue("edition");
        if (edVal != null) {
            long ed = opts.getOptLong("edition", _currentChannel.getEdition()+client.ctx().random().nextLong(1000));
            if (ed >= 0) {
                _currentChannel.setEdition(ed);
                ui.statusMessage("Updated channel edition");
            } else {
                ed = createEdition(client);
                if (ed <= _currentChannel.getEdition())
                    ed = _currentChannel.getEdition() + client.ctx().random().nextLong(1000);
                _currentChannel.setEdition(ed);
                ui.statusMessage("Updated channel edition randomly");
            }
        }
        
        String exp = opts.getOptValue("expiration");
        if (exp != null) {
            Date when = null;
            try {
                synchronized (_dayFmt) {
                    when = _dayFmt.parse(exp);
                }
            } catch (ParseException pe) {
                when = null;
            }
            if (when != null)
                _currentChannel.setExpiration(when.getTime());
            else
                _currentChannel.setExpiration(-1);
            ui.statusMessage("Updated channel expiration");
        }
        
        String val = opts.getOptValue("publicPosting");
        if (val != null) {
            boolean post = opts.getOptBoolean("publicPosting", _currentChannel.getAllowPublicPosts());
            _currentChannel.setAllowPublicPosts(post);
            ui.statusMessage("Updated channel public posting policy");
        }
        
        val = opts.getOptValue("publicReplies");
        if (val != null) {
            boolean reply = opts.getOptBoolean("publicReplies", _currentChannel.getAllowPublicReplies());
            _currentChannel.setAllowPublicReplies(reply);
            ui.statusMessage("Updated channel public replies policy");
        }
        
        List tags = opts.getOptValues("pubTag");
        if (tags != null) {
            _currentChannel.setPublicTags(new HashSet(tags));
            ui.statusMessage("Updated channel public tags");
        }
        tags = opts.getOptValues("privTag");
        if (tags != null) {
            _currentChannel.setPrivateTags(new HashSet(tags));
            ui.statusMessage("Updated channel private tags");
        }
        
        List manageKeys = opts.getOptValues("manageKey");
        if (manageKeys != null) {
            Set mkeys = new HashSet();
            for (int i = 0; i < manageKeys.size(); i++) {
                String mkey = (String)manageKeys.get(i);
                byte mkeyData[] = Base64.decode(mkey);
                if ( (mkeyData != null) && (mkeyData.length == SigningPublicKey.KEYSIZE_BYTES) )
                    mkeys.add(SigningPublicKey.create(mkeyData, 0));
            }
            _currentChannel.setAuthorizedManagers(mkeys);
            ui.statusMessage("Updated channel manager keys");
        }
        
        List postKeys = opts.getOptValues("postKey");
        if (postKeys != null) {
            Set pkeys = new HashSet();
            for (int i = 0; i < postKeys.size(); i++) {
                String pkey = (String)postKeys.get(i);
                byte pkeyData[] = Base64.decode(pkey);
                if ( (pkeyData != null) && (pkeyData.length == SigningPublicKey.KEYSIZE_BYTES) )
                    pkeys.add(SigningPublicKey.create(pkeyData, 0));
            }
            _currentChannel.setAuthorizedPosters(pkeys);
            ui.statusMessage("Updated channel post keys");
        }
        
        String refs = opts.getOptValue("refs");
        if (refs != null) {
            File f = new File(refs);
            if (f.exists()) {
                _refs = refs;
                ui.statusMessage("Updated channel references file");
            } else {
                ui.errorMessage("References file does not exist");
                _refs = null;
            }
        }

        List archives = opts.getOptValues("pubArchive");
        if (archives != null) {
            Set infos = new HashSet();
            for (int i = 0; i < archives.size(); i++) {
                String str = (String)archives.get(i);
                try {
                    SyndieURI uri = new SyndieURI(str);
                    ArchiveInfo info = new ArchiveInfo();
                    info.setArchiveId(-1);
                    info.setPostAllowed(false);
                    info.setReadAllowed(true);
                    info.setURI(uri);
                    infos.add(info);
                } catch (URISyntaxException use) {
                    ui.errorMessage("Archive URI is not valid [" + str + "]");
                }
            }
            _currentChannel.setPublicArchives(infos);
            ui.statusMessage("Updated channel public archives");
        }
        archives = opts.getOptValues("privArchive");
        if (archives != null) {
            Set infos = new HashSet();
            for (int i = 0; i < archives.size(); i++) {
                String str = (String)archives.get(i);
                try {
                    SyndieURI uri = new SyndieURI(str);
                    ArchiveInfo info = new ArchiveInfo();
                    info.setArchiveId(-1);
                    info.setPostAllowed(false);
                    info.setReadAllowed(true);
                    info.setURI(uri);
                    infos.add(info);
                } catch (URISyntaxException use) {
                    ui.errorMessage("Archive URI is not valid [" + str + "]");
                }
            }
            _currentChannel.setPrivateArchives(infos);
            ui.statusMessage("Updated channel private archives");
        }
        
        String enc = opts.getOptValue("encryptContent");
        if (enc != null) {
            _encryptContent = Boolean.valueOf(opts.getOptBoolean("encryptContent", false));
            ui.statusMessage("Updated channel encryption policy");
        }
        
        String passphrase = opts.getOptValue("bodyPassphrase");
        if (passphrase != null)
            _bodyPassphrase = passphrase;
        String prompt = opts.getOptValue("bodyPassphrasePrompt");
        if (prompt != null)
            _bodyPassphrasePrompt = prompt;
        
        ui.statusMessage("Channel settings updated");
        ui.commandComplete(0, null);
    }
    
    private static final String SQL_LIST_NYMS = "SELECT identKey, name FROM channel ORDER BY name ASC";

    /** listnyms [--name $namePrefix] [--channel $hashPrefix] */
    private void processListNyms(DBClient client, UI ui, Opts opts) {
        if (_currentChannel == null) {
            ui.errorMessage("No creation or update process in progress");
            ui.commandComplete(-1, null);
            return;
        }
        String namePrefix = opts.getOptValue("name");
        String chanPrefix = opts.getOptValue("channel");

        _listedNymKeys.clear();
        Connection con = client.con();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = con.prepareStatement(SQL_LIST_NYMS);
            rs = stmt.executeQuery();
            List banned = client.getBannedChannels();
            while (rs.next()) {
                byte pubKey[] = rs.getBytes(1);
                String name = rs.getString(2);
                if (pubKey != null) {
                    SigningPublicKey pk = SigningPublicKey.create(pubKey, 0);
                    Hash chan = pk.calculateHash();
                    if (banned.contains(chan))
                        continue;
                    if (namePrefix != null) {
                        if (name == null)
                            continue;
                        if (!name.startsWith(namePrefix))
                            continue;
                    }
                    if (chanPrefix != null) {
                        if (!chan.toBase64().startsWith(chanPrefix))
                            continue;
                    }
                    _listedNymKeys.add(pk);
                    ui.statusMessage(_listedNymKeys.size() + ": " +
                                     (name != null ? CommandImpl.strip(name) : "") +
                                     " (" + chan.toBase64() + ")");
                }
            }
            ui.commandComplete(0, null);
        } catch (SQLException se) {
            ui.errorMessage("Internal error listing nyms", se);
            ui.commandComplete(-1, null);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    /** addnym (--nym $index | --key $base64(pubKey)) --action (manage|post) */
    private void processAddNym(DBClient client, UI ui, Opts opts) {
        if (_currentChannel == null) {
            ui.errorMessage("No creation or update process in progress");
            ui.commandComplete(-1, null);
            return;
        }
        SigningPublicKey key = null;
        int index = (int)opts.getOptLong("nym", -1);
        if (index > 0) {
            if (_listedNymKeys.size() < index) {
                ui.errorMessage("Index is out of range (size=" + _listedNymKeys.size() + ")");
                ui.commandComplete(-1, null);
                return;
            } else {
                key = (SigningPublicKey)_listedNymKeys.get(index-1);
            }
        } else {
            byte data[] = opts.getOptBytes("key");
            if ( (data != null) && (data.length == SigningPublicKey.KEYSIZE_BYTES) ) {
                key = SigningPublicKey.create(data, 0);
            }
        }
        
        boolean manage = false;
        boolean post = false;
        String action = opts.getOptValue("action");
        if (action != null) {
            if ("manage".equalsIgnoreCase(action))
                manage = true;
            else if ("post".equalsIgnoreCase(action))
                post = true;
        }
        
        if ( (key == null) || (!manage && !post)) {
            ui.errorMessage("Usage: addnym (--nym $index | --key $base64(pubKey)) --action (manage|post)");
            ui.commandComplete(-1, null);
            return;
        }
        
        if (manage) {
            Set managers = _currentChannel.getAuthorizedManagers();
            if (managers == null)
                managers = new HashSet();
            managers.add(key);
            ui.statusMessage("Key " + key.calculateHash().toBase64() + " added to the managers list");
        } else {
            Set posters = _currentChannel.getAuthorizedPosters();
            if (posters == null)
                posters = new HashSet();
            posters.add(key);
            ui.statusMessage("Key " + key.calculateHash().toBase64() + " added to the posters list");
        }
        ui.commandComplete(0, null);
    }

    /** removenym (--nym $index | --key $base64(pubKey)) --action (manage|post) */
    private void processRemoveNym(DBClient client, UI ui, Opts opts) {
        
        if (_currentChannel == null) {
            ui.errorMessage("No creation or update process in progress");
            ui.commandComplete(-1, null);
            return;
        }
        SigningPublicKey key = null;
        int index = (int)opts.getOptLong("nym", -1);
        if (index > 0) {
            if (_listedNymKeys.size() < index) {
                ui.errorMessage("Index is out of range (size=" + _listedNymKeys.size() + ")");
                ui.commandComplete(-1, null);
                return;
            } else {
                key = (SigningPublicKey)_listedNymKeys.get(index-1);
            }
        } else {
            byte data[] = opts.getOptBytes("key");
            if ( (data != null) && (data.length == SigningPublicKey.KEYSIZE_BYTES) ) {
                key = SigningPublicKey.create(data, 0);
            }
        }
        
        boolean manage = false;
        boolean post = false;
        String action = opts.getOptValue("action");
        if (action != null) {
            if ("manage".equalsIgnoreCase(action))
                manage = true;
            else if ("post".equalsIgnoreCase(action))
                post = true;
        }
        
        if ( (key == null) || (!manage && !post)) {
            ui.errorMessage("Usage: removenym (--nym $index | --key $base64(pubKey)) --action (manage|post)");
            ui.commandComplete(-1, null);
            return;
        }
        
        if (manage) {
            Set managers = _currentChannel.getAuthorizedManagers();
            if (managers == null)
                managers = new HashSet();
            managers.remove(key);
            _currentChannel.setAuthorizedManagers(managers);
            ui.statusMessage("Key " + key.calculateHash().toBase64() + " removed from the managers list");
        } else {
            Set posters = _currentChannel.getAuthorizedPosters();
            if (posters == null)
                posters = new HashSet();
            posters.remove(key);
            _currentChannel.setAuthorizedPosters(posters);
            ui.statusMessage("Key " + key.calculateHash().toBase64() + " remove from the posters list");
        }
        ui.commandComplete(0, null);
    }
    
    private void processPreview(DBClient client, UI ui, Opts opts) {
        if (_currentChannel == null) {
            ui.errorMessage("No creation or update process in progress");
            ui.commandComplete(-1, null);
            return;
        }
        ui.statusMessage(_currentChannel.toString());
        if (_avatar != null)
            ui.statusMessage("Loading the channel avatar from: " + _avatar);
        else
            ui.statusMessage("Using the existing channel avatar");
        if (_encryptContent != null)
            ui.statusMessage("Encrypt all content for authorized users only? " + _encryptContent.booleanValue());
        if (_refs != null)
            ui.statusMessage("Loading the channel references from: " + _refs);
        else
            ui.statusMessage("No channel references source file defined");
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
    
        ChannelInfo info = _currentChannel;
        
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
    
    /**
     * execute [--out $outputDir]: create/update the channel, generating the metadata and
     * private keys in the given dir, and importing them into the current nym.  also
     * clears the current create or update state
     */
    private void processExecute(DBClient client, UI ui, Opts opts) {
        if (_currentChannel == null) {
            ui.errorMessage("No create or update process in progress");
            ui.commandComplete(-1, null);
            return;
        }
       
        String out = opts.getOptValue("out");
        //if (out == null) {
        //    ui.errorMessage("You must specify a file to write the signed metadata to (with --out $filename)");
        //    ui.commandComplete(-1, null);
        //    return;
        //}
        File tmpDir = client.getTempDir();
        tmpDir.mkdirs();
        File manageOut = null;
        File replyOut = null;
        File encPostOut = null;
        File encMetaOut = null;
        try {
            manageOut = SecureFile.createTempFile("syndieManage", "dat", tmpDir);
            replyOut = SecureFile.createTempFile("syndieReply", "dat", tmpDir);
            encPostOut = SecureFile.createTempFile("syndieEncPost", "dat", tmpDir);
            encMetaOut = SecureFile.createTempFile("syndieEncMeta", "dat", tmpDir);
            if (out == null) {
                out = SecureFile.createTempFile("syndieMetaOut", Constants.FILENAME_SUFFIX, tmpDir).getPath();
            }
        } catch (IOException ioe) {
            ui.errorMessage("Unable to create temporary files", ioe);
            ui.commandComplete(-1, null);
            return;
        }
        
        Opts chanGenOpts = new Opts();
        chanGenOpts.setCommand("changen");
        chanGenOpts.setOptValue("name", _currentChannel.getName());
        chanGenOpts.setOptValue("description", _currentChannel.getDescription());
        chanGenOpts.setOptValue("avatar", _avatar);
        chanGenOpts.setOptValue("edition", Long.toString(_currentChannel.getEdition()));
        chanGenOpts.setOptValue("publicPosting", (_currentChannel.getAllowPublicPosts() ? Boolean.TRUE.toString() : Boolean.FALSE.toString()));
        chanGenOpts.setOptValue("publicReplies", (_currentChannel.getAllowPublicReplies() ? Boolean.TRUE.toString() : Boolean.FALSE.toString()));
        Set tags = _currentChannel.getPublicTags();
        if (tags != null) {
            for (Iterator iter = tags.iterator(); iter.hasNext(); )
                chanGenOpts.addOptValue("pubTag", iter.next().toString());
        }
        tags = _currentChannel.getPrivateTags();
        if (tags != null) {
            for (Iterator iter = tags.iterator(); iter.hasNext(); )
                chanGenOpts.addOptValue("privTag", iter.next().toString());
        }
        
        SigningPublicKey us = getNymPublicKey(client);
        
        Set keys = _currentChannel.getAuthorizedPosters();
        if (keys != null) {
            for (Iterator iter = keys.iterator(); iter.hasNext(); ) {
                SigningPublicKey pub = (SigningPublicKey)iter.next();
                chanGenOpts.addOptValue("postKey", pub.toBase64());
            }
        }
        
        keys = _currentChannel.getAuthorizedManagers();
        if (keys != null) {
            for (Iterator iter = keys.iterator(); iter.hasNext(); ) {
                SigningPublicKey pub = (SigningPublicKey)iter.next();
                chanGenOpts.addOptValue("manageKey", pub.toBase64());
            }
        }
        
        chanGenOpts.setOptValue("refs", _refs);
        
        Set archives = _currentChannel.getPublicArchives();
        if (archives != null) {
            for (Iterator iter = archives.iterator(); iter.hasNext(); ) {
                ArchiveInfo archive = (ArchiveInfo)iter.next();
                chanGenOpts.addOptValue("pubArchive", archive.getURI().toString());
            }
        }
        archives = _currentChannel.getPrivateArchives();
        if (archives != null) {
            for (Iterator iter = archives.iterator(); iter.hasNext(); ) {
                ArchiveInfo archive = (ArchiveInfo)iter.next();
                chanGenOpts.addOptValue("privArchive", archive.getURI().toString());
            }
        }
        
        if (_encryptContent != null)
            chanGenOpts.setOptValue("encryptContent", _encryptContent.booleanValue() ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
        
        if (_currentChannel.getChannelId() >= 0)
            chanGenOpts.setOptValue("channelId", Long.toString(_currentChannel.getChannelId()));
        
        chanGenOpts.setOptValue("metaOut", out);
        chanGenOpts.setOptValue("keyManageOut", manageOut.getPath());
        chanGenOpts.setOptValue("keyReplyOut", replyOut.getPath());
        chanGenOpts.setOptValue("keyEncryptPostOut", encPostOut.getPath());
        chanGenOpts.setOptValue("keyEncryptMetaOut", encMetaOut.getPath());
        
        if ( (_bodyPassphrase != null) && (_bodyPassphrasePrompt != null) ) {
            chanGenOpts.setOptValue("bodyPassphrase", CommandImpl.strip(_bodyPassphrase));
            chanGenOpts.setOptValue("bodyPassphrasePrompt", CommandImpl.strip(_bodyPassphrasePrompt));
        }
        
        ChanGen cmd = new ChanGen();
        ui.debugMessage("Creating/updating channel with options " + chanGenOpts);
        NestedUI nestedUI = new NestedUI(ui);
        cmd.runCommand(chanGenOpts, nestedUI, client);
        
        if ( (nestedUI.getExitCode() >= 0) && (opts.getOptValue("metaOut") == null) ) {
            // ok, used the default dir - migrate it
            FileInputStream fis = null;
            FileOutputStream fos = null;
            try {
                fis = new FileInputStream(out);
                Enclosure enc = new Enclosure(fis);
                SigningPublicKey pub = enc.getHeaderSigningKey(Constants.MSG_META_HEADER_IDENTITY);
                if (pub == null) {
                    ui.errorMessage("Unable to pull the channel from the enclosure");
                    ui.commandComplete(-1, null);
                    return;
                } else {
                    ui.debugMessage("Channel identity: " +pub.calculateHash().toBase64());
                }
                File chanDir = new SecureFile(client.getOutboundDir(), pub.calculateHash().toBase64());
                chanDir.mkdirs();
                File mdFile = new File(chanDir, "meta" + Constants.FILENAME_SUFFIX);
                fos = new SecureFileOutputStream(mdFile);
                fis = new FileInputStream(out);
                byte buf[] = new byte[4096];
                int read = -1;
                while ( (read = fis.read(buf)) != -1)
                    fos.write(buf, 0, read);
                fis.close();
                fos.close();
                fis = null;
                fos = null;
                File outFile = new File(out);
                outFile.delete();
                out = mdFile.getPath();
                ui.statusMessage("Sharable channel metadata saved to " + mdFile.getPath());
            } catch (IOException ioe) {
                ui.errorMessage("Error migrating the channel metadata from " + out, ioe);
            } finally {
                if (fis != null) try { fis.close(); } catch (IOException ioe) {}
                if (fos != null) try { fos.close(); } catch (IOException ioe) {}
            }
        }
        
        File outFile = new File(out);
        if ( (nestedUI.getExitCode() >= 0) && (outFile.exists() && outFile.length() > 0) ) {
            // channel created successfully, now import the metadata and keys, and delete
            // the temporary files
            ui.statusMessage("Channel metadata created and stored in " + outFile.getPath());
            
            Importer msgImp = new Importer();
            Opts msgImpOpts = new Opts();
            msgImpOpts.setOptValue("in", out);
            if (_bodyPassphrase != null)
                msgImpOpts.setOptValue("passphrase", CommandImpl.strip(_bodyPassphrase));
            msgImpOpts.setCommand("import");
            NestedUI dataNestedUI = new NestedUI(ui);
            ui.debugMessage("Importing with options " + msgImpOpts);
            msgImp.runCommand(msgImpOpts, dataNestedUI, client);
            if (dataNestedUI.getExitCode() < 0) {
                ui.debugMessage("Failed in the nested import command");
                ui.commandComplete(dataNestedUI.getExitCode(), null);
                return;
            }
            ui.statusMessage("Channel metadata imported");

            KeyImport keyImp = new KeyImport();
            Opts keyOpts = new Opts();
            if (manageOut.length() > 0) {
                keyOpts.setOptValue("keyfile", manageOut.getPath());
                keyOpts.setOptValue("authentic", "true");
                dataNestedUI = new NestedUI(ui);
                keyImp.runCommand(keyOpts, dataNestedUI, client);
                if (dataNestedUI.getExitCode() < 0) {
                    ui.errorMessage("Failed in the nested key import command");
                    ui.commandComplete(dataNestedUI.getExitCode(), null);
                    return;
                }
                ui.statusMessage("Channel management key imported");
            }
            if (replyOut.length() > 0) {
                keyOpts = new Opts();
                keyOpts.setOptValue("keyfile", replyOut.getPath());
                keyOpts.setOptValue("authentic", "true");
                dataNestedUI = new NestedUI(ui);
                keyImp.runCommand(keyOpts, dataNestedUI, client);
                if (dataNestedUI.getExitCode() < 0) {
                    ui.errorMessage("Failed in the nested key import command");
                    ui.commandComplete(dataNestedUI.getExitCode(), null);
                    return;
                }
                ui.statusMessage("Channel reply key imported");
            }
            if (encPostOut.length() > 0) {
                keyOpts = new Opts();
                keyOpts.setOptValue("keyfile", encPostOut.getPath());
                keyOpts.setOptValue("authentic", "true");
                dataNestedUI = new NestedUI(ui);
                keyImp.runCommand(keyOpts, dataNestedUI, client);
                if (dataNestedUI.getExitCode() < 0) {
                    ui.errorMessage("Failed in the nested key import command");
                    ui.commandComplete(dataNestedUI.getExitCode(), null);
                    return;
                }
                ui.statusMessage("Channel post read key imported");
            }
            if (encMetaOut.length() > 0) {
                keyOpts = new Opts();
                keyOpts.setOptValue("keyfile", encMetaOut.getPath());
                keyOpts.setOptValue("authentic", "true");
                dataNestedUI = new NestedUI(ui);
                keyImp.runCommand(keyOpts, dataNestedUI, client);
                if (dataNestedUI.getExitCode() < 0) {
                    ui.errorMessage("Failed in the nested key import command");
                    ui.commandComplete(dataNestedUI.getExitCode(), null);
                    return;
                }
                ui.statusMessage("Channel metadata read key imported");
            }
            
            manageOut.delete();
            replyOut.delete();
            encPostOut.delete();
            encMetaOut.delete();
            
            _currentChannel = null;
            _avatar = null;
            _refs = null;
            _encryptContent = null;
        }
        ui.commandComplete(nestedUI.getExitCode(), null);
    }
}
