package syndie.db;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.Serializable;
import java.net.URISyntaxException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
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
import net.i2p.util.SecureFileOutputStream;

import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;

/**
 *
 */
class ReadMenu implements TextEngine.Menu {
    private TextEngine _engine;
    /** text description of each channel */
    private List _channelText;
    /** text description of each message */
    private List _messageText;
    /** internal channel ids (Long)  */
    private List _channelKeys;
    /** internal message ids (Long)  */
    private List _messageKeys;
    /** next channel the user should be shown */
    private int _channelIteratorIndex;
    /** next message the user should be shown */
    private int _messageIteratorIndex;
    /** current channel the user is in (if any) */
    private ChannelInfo _currentChannel;
    /** current message in the current channel that the user is reviewing (if any) */
    private MessageInfo _currentMessage;
    /** root of the current message's thread */
    private ReferenceNode _currentThreadRoot;
    /** SyndieURI for each of the threads matching the most recent 'threads' command */
    private List _threadRootURIs;
    /** text describing each of the _threadRootURIs */
    private List _threadText;
    
    public ReadMenu(TextEngine engine) {
        _engine = engine;
        _messageText = new ArrayList();
        _channelText = new ArrayList();
        _messageKeys = new ArrayList();
        _channelKeys = new ArrayList();
        _threadRootURIs = new ArrayList();
        _threadText = new ArrayList();
        _messageIteratorIndex = 0;
        _channelIteratorIndex = 0;
        _currentChannel = null;
        _currentMessage = null;
        _currentThreadRoot = null;
    }
    
    public static final String NAME = "read";
    public String getName() { return NAME; }
    public String getDescription() { return "read menu"; }
    public boolean requireLoggedIn() { return true; }
    public void listCommands(UI ui) {
        ui.statusMessage(" channels [--unreadOnly $boolean] [--name $name] [--hash $hashPrefix]");
        ui.statusMessage("          : lists channels matching the given criteria");
        if ( (_messageKeys.size() > 0) || (_channelKeys.size() > 0) ) {
            ui.statusMessage(" next [--lines $num]  : iterate through the channels/messages");
            ui.statusMessage(" prev [--lines $num]  : iterate through the channels/messages");
        }
        ui.statusMessage(" meta [--channel ($index|$hash)] : display the channel's metadata");
        ui.statusMessage(" messages [--channel ($index|$hash)] [--includeUnauthorized $boolean]");
        ui.statusMessage("          [--includeUnauthenticated $boolean]");
        ui.statusMessage("          : lists messages matching the given criteria");
        ui.statusMessage(" threads [--channel ($index|$hash|all)] [-tags [-]tag[,[-]tag]*]");
        ui.statusMessage("         [--includeUnauthorized $boolean] [--compact $boolean]");
        ui.statusMessage("          : Display a list of threads matching the given criteria. The ");
        ui.statusMessage("          : tags parameter picks threads where at least one message has");
        ui.statusMessage("          : each of the tags, and that none of the messages have any of the");
        ui.statusMessage("          : tags prefaced by -");
        ui.statusMessage(" view [(--message ($index|$uri)|--thread $index)] [--page $n]");
        ui.statusMessage("          : view a page in the given message");
        if (_currentMessage != null) {
            ui.statusMessage(" threadnext [--position $position]");
            ui.statusMessage("          : view the next message in the thread (or the given");
            ui.statusMessage("          : thread position)");
            ui.statusMessage(" threadprev [--position $position]");
            ui.statusMessage("          : view the previous message in the thread (or the given");
            ui.statusMessage("          : thread position)");
            ui.statusMessage(" importkey --position $position");
            ui.statusMessage("          : import the key included in the given message reference");
        }
        ui.statusMessage(" export [--message ($index|$uri)] --out $directory");
        ui.statusMessage("          : dump the full set of pages/attachments/status to the");
        ui.statusMessage("          : specified directory");
        ui.statusMessage(" save [--message ($index|$uri)] (--page $n|--attachment $n) --out $filename");
        ui.statusMessage("          : save just the specified page/attachment to the given file");
        if (_currentMessage != null) {
            ui.statusMessage(" reply    : jump to the post menu, prepopulating the --references field");
        }
        if ( (_currentChannel != null) || (_currentMessage != null) ) {
            ui.statusMessage(" ban [--scope (author|channel|$hash)] [--delete $boolean]");
            ui.statusMessage("          : ban the author or channel so that no more posts from that author");
            ui.statusMessage("          : or messages by any author in that channel will be allowed into the");
            ui.statusMessage("          : Syndie archive.  If --delete is specified, the messages themselves");
            ui.statusMessage("          : will be removed from the archive as well as the database");
            ui.statusMessage(" decrypt [(--message $msgId|--channel $channelId)] [--passphrase pass]");
            ui.statusMessage("          : attempt to decrypt the specified channel metadata or message for");
            ui.statusMessage("          : those that could not be decrypted earlier");
            ui.statusMessage(" watch (--author $true|--channel $true) [--nickname $name]");
            ui.statusMessage("       [--category $nameInWatchedTree]");
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
        } else if ("messages".equalsIgnoreCase(cmd)) {
            processMessages(client, ui, opts);
        } else if ("threads".equalsIgnoreCase(cmd)) {
            processThreads(client, ui, opts);
        } else if ("view".equalsIgnoreCase(cmd)) {
            processView(client, ui, opts);
        } else if ("threadnext".equalsIgnoreCase(cmd)) {
            processThreadNext(client, ui, opts);
        } else if ("threadprev".equalsIgnoreCase(cmd)) {
            processThreadPrev(client, ui, opts);
        } else if ("importkey".equalsIgnoreCase(cmd)) {
            processImportKey(client, ui, opts);
        } else if ("export".equalsIgnoreCase(cmd)) {
            processExport(client, ui, opts);
        } else if ("save".equalsIgnoreCase(cmd)) {
            processSave(client, ui, opts);
        } else if ("reply".equalsIgnoreCase(cmd)) {
            processReply(client, ui, opts);
        } else if ("ban".equalsIgnoreCase(cmd)) {
            processBan(client, ui, opts);
        } else if ("decrypt".equalsIgnoreCase(cmd)) {
            processDecrypt(client, ui, opts);
        } else if ("watch".equalsIgnoreCase(cmd)) {
            notImplementedYet(ui);
        } else {
            return false;
        }
        return true;
    }
    private void notImplementedYet(UI ui) {
        ui.statusMessage("Command not implemented yet");
    }
    public List getMenuLocation(DBClient client, UI ui) {
        ArrayList rv = new ArrayList();
        rv.add("read");
        
        if (_currentMessage != null) {
            long chanId = client.getChannelId(_currentMessage.getTargetChannel());
            // we refetch the channel so when we bounce around scopes within a single
            //thread, it looks less confusing
            ChannelInfo chan = client.getChannel(chanId);
            rv.add("chan '" + chan.getName() + "'/" + chan.getChannelHash().toBase64().substring(0,6));
            rv.add("msg " + _currentMessage.getMessageId());
        } else if (_currentChannel != null) {
            rv.add("chan '" + _currentChannel.getName() + "'/" + _currentChannel.getChannelHash().toBase64().substring(0,6));
            if (_messageKeys.size() > 0)
                rv.add("message list");
        } else if (_channelKeys.size() > 0) {
            rv.add("channel list");
        }
        return rv;
    }
    
    private static final SimpleDateFormat _dayFmt = new SimpleDateFormat("yyyy/MM/dd");
    private static final String SQL_LIST_CHANNELS = "SELECT channelId, channelHash, name, description, COUNT(msgId), MAX(messageId) FROM channel LEFT OUTER JOIN channelMessage ON channelId = targetChannelId GROUP BY channelId, name, description, channelHash";
    /** channels [--unreadOnly $boolean] [--name $name] [--hash $hashPrefix] */
    private void processChannels(DBClient client, UI ui, Opts opts) {
        _channelIteratorIndex = 0;
        _channelKeys.clear();
        _channelText.clear();
        _messageIteratorIndex = 0;
        _messageKeys.clear();
        _messageText.clear();
        _currentChannel = null;
        _currentMessage = null;
        _currentThreadRoot = null;
        
        boolean unreadOnly = opts.getOptBoolean("unreadOnly", false);
        if (unreadOnly) {
            ui.statusMessage("Ignoring the unreadOnly flag, as it is not yet supported");
            unreadOnly = false;
        }
        String name = opts.getOptValue("name");
        String prefix = opts.getOptValue("hash");
        
        Connection con = client.con();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = con.prepareStatement(SQL_LIST_CHANNELS);
            rs = stmt.executeQuery();
            while (rs.next()) {
                // "channelId, channelHash, name, description";
                long id = rs.getLong(1);
                if (rs.wasNull())
                    continue;
                byte hash[] = rs.getBytes(2);
                if (hash == null)
                    continue;
                String curName = rs.getString(3);
                String desc = rs.getString(4);
                long numMessages = rs.getLong(5);
                long mostRecentMsg = rs.getLong(6);
                String b64 = Base64.encode(hash);

                if (name != null) {
                    if (curName == null)
                        continue;
                    else if (!curName.startsWith(name))
                        continue;
                }
                
                if (prefix != null) {
                    if (!b64.startsWith(prefix))
                        continue;
                }
                
                // ok, matches criteria
                _channelKeys.add(new Long(id));
                StringBuilder buf = new StringBuilder();
                
                ChannelInfo chan = client.getChannel(id);
                if (chan.getReadKeyUnknown()) {
                    buf.append("(undecrypted metadata)\n\tuse 'decrypt --channel ");
                    buf.append(_channelKeys.size()-1).append("' to decrypt");
                } else if (chan.getPassphrasePrompt() != null) {
                    buf.append("(undecrypted metadata) - prompt: \"");
                    buf.append(CommandImpl.strip(chan.getPassphrasePrompt())).append("\"");
                    buf.append("\n\tuse 'decrypt --channel ");
                    buf.append(_channelKeys.size()-1).append(" --passphrase $passphrase' to decrypt");
                } else {
                    if (curName != null)
                        buf.append('\'').append(CommandImpl.strip(curName)).append("\' ");
                    buf.append("(").append(b64.substring(0,6)).append(") ");
                    if (desc != null)
                        buf.append("- ").append(CommandImpl.strip(desc));
                    buf.append(" messages: ").append(numMessages);
                    if (numMessages > 0) {
                        String when = null;
                        synchronized (_dayFmt) {
                            when = _dayFmt.format(new Date(mostRecentMsg));
                        }
                        buf.append(" last post on ").append(when);
                    }
                }
                _channelText.add(buf.toString());
            }
            ui.statusMessage(_channelKeys.size() + " channels matched - use 'next' to view them");
            ui.commandComplete(0, null);
        } catch (SQLException se) {
            ui.errorMessage("Internal error listing channels", se);
            ui.commandComplete(-1, null);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }

    /** next [--lines $num]  : iterate through the channels/messages */
    private void processNext(DBClient client, UI ui, Opts opts) {
        int num = (int)opts.getOptLong("lines", 10);
        if (_messageKeys.size() > 0) {
            // list messages
            if (_messageIteratorIndex >= _messageKeys.size()) {
                ui.statusMessage("No more messages - use 'prev' to review earlier messages");
                ui.commandComplete(0, null);
            } else {
                int end = Math.min(_messageIteratorIndex+num, _messageKeys.size());
                ui.statusMessage("message " + _messageIteratorIndex + " through " + (end-1) + " of " + (_messageKeys.size()-1));
                while (_messageIteratorIndex < end) {
                    String desc = (String)_messageText.get(_messageIteratorIndex);
                    ui.statusMessage(_messageIteratorIndex + ": " + desc);
                    _messageIteratorIndex++;
                }
                int remaining = _messageKeys.size() - _messageIteratorIndex;
                if (remaining > 0)
                    ui.statusMessage(remaining + " messages remaining");
                else
                    ui.statusMessage("No more messages - use 'prev' to review earlier messages");
                ui.commandComplete(0, null);
            }
        } else {
            // list channels
            if (_channelIteratorIndex >= _channelKeys.size()) {
                ui.statusMessage("No more channels - use 'prev' to review earlier channels");
                ui.commandComplete(0, null);
            } else {
                int end = Math.min(_channelIteratorIndex+num, _channelKeys.size());
                ui.statusMessage("channel " + _channelIteratorIndex + " through " + (end-1) + " of " + (_channelKeys.size()-1));
                while (_channelIteratorIndex < end) {
                    String desc = (String)_channelText.get(_channelIteratorIndex);
                    ui.statusMessage(_channelIteratorIndex + ": " + desc);
                    _channelIteratorIndex++;
                }
                int remaining = _channelKeys.size() - _channelIteratorIndex;
                if (remaining > 0)
                    ui.statusMessage(remaining + " channels remaining");
                else
                    ui.statusMessage("No more channels - use 'prev' to review earlier channels");
                ui.commandComplete(0, null);
            }
        }
    }

    /** prev [--lines $num]  : iterate through the channels/messages */
    private void processPrev(DBClient client, UI ui, Opts opts) {
        int num = (int)opts.getOptLong("lines", 10);
        int index = 0;
        if (_messageKeys.size() > 0) {
            _messageIteratorIndex -= num;
            if (_messageIteratorIndex < 0)
                _messageIteratorIndex = 0;
        } else {
            _channelIteratorIndex -= num;
            if (_channelIteratorIndex < 0)
                _channelIteratorIndex = 0;
        }
        processNext(client, ui, opts);
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
                    channel = new Hash(val);
                    ui.debugMessage("channel requested is a hash (" + channel.toBase64() + ")");
                } else {
                    ui.errorMessage("Channel requested is not valid - either specify --channel $index or --channel $base64(channelHash)");
                    ui.commandComplete(-1, null);
                    return;
                }
            }
        }
        
        long channelId = -1;
        if ( (channelIndex >= 0) && (channelIndex < _channelKeys.size()) ) {
            channelId = ((Long)_channelKeys.get((int)channelIndex)).longValue();
            _currentChannel = client.getChannel(channelId);
        } else if (channel != null) {
            channelId = client.getChannelId(channel);
            _currentChannel = client.getChannel(channelId);
        }
     
        if (_currentChannel == null) {
            ui.debugMessage("channelIndex=" + channelIndex + " channelKeySize: " + _channelKeys.size());
            ui.debugMessage("channel=" + channelIndex);
            ui.errorMessage("Invalid or unknown channel requested");
            ui.commandComplete(-1, null);
            return;
        }
        
        ui.statusMessage(_currentChannel.toString());
    }
    
    // $index\t$date\t$subject\t$author
    private static final String SQL_LIST_MESSAGES = "SELECT msgId, messageId, subject, authorChannelId FROM channelMessage WHERE targetChannelId = ? AND wasPrivate = FALSE AND isCancelled = FALSE AND deletionCause IS NULL";
    /** messages [--channel ($index|$hash)] [--includeUnauthorized $boolean] [--includeUnauthenticated $boolean] */
    private void processMessages(DBClient client, UI ui, Opts opts) {
        boolean unauthorized = opts.getOptBoolean("includeUnauthorized", false);
        //unauthenticated included by default, since undecrypted posts are
        //unauthenticated until successful decryption (and unauthenticated posts
        //are only imported if they are authorized)
        boolean unauthenticated = opts.getOptBoolean("includeUnauthenticated", true);
        long channelIndex = -1;
        Hash channel = null;
        String chan = opts.getOptValue("channel");
        if (chan == null) {
            if (_currentChannel != null)
                chan = _currentChannel.getChannelHash().toBase64();
        }
        try {
            long val = Long.parseLong(chan);
            channelIndex = val;
        } catch (NumberFormatException nfe) {
            ui.debugMessage("channel requested is not an index (" + chan + ")");
            // ok, not an integer, maybe its a full channel hash?
            byte val[] = Base64.decode(chan);
            if ( (val != null) && (val.length == Hash.HASH_LENGTH) ) {
                channel = new Hash(val);
                ui.debugMessage("channel requested is a hash (" + channel.toBase64() + ")");
            } else {
                ui.errorMessage("Channel requested is not valid - either specify --channel $index or --channel $base64(channelHash)");
                ui.commandComplete(-1, null);
                return;
            }
        }
        
        long channelId = -1;
        if ( (channelIndex >= 0) && (channelIndex < _channelKeys.size()) ) {
            channelId = ((Long)_channelKeys.get((int)channelIndex)).longValue();
            _currentChannel = client.getChannel(channelId);
        } else if (channel != null) {
            channelId = client.getChannelId(channel);
            _currentChannel = client.getChannel(channelId);
        }
        
        if ( (channelId < 0) || (_currentChannel == null) ) {
            ui.debugMessage("channelIndex=" + channelIndex + " itemKeySize: " + _channelKeys.size());
            ui.debugMessage("channel=" + channelIndex);
            ui.debugMessage("currentChannel=" + _currentChannel);
            ui.errorMessage("Invalid or unknown channel requested");
            ui.commandComplete(-1, null);
            return;
        }

        _messageIteratorIndex = 0;
        _messageKeys.clear();
        _messageText.clear();

        if (_currentChannel.getReadKeyUnknown()) {
            ui.errorMessage("Channel metadata could not be read, as you did not have the correct channel read key");
            ui.errorMessage("To try and decrypt the metadata, use 'decrypt'");
            // technically, we don't have to return, and can list the readable and unreadable messages in the
            // channel, but its probably best not to
            return;            
        } else if (_currentChannel.getPassphrasePrompt() != null) {
            ui.errorMessage("Channel metadata could not be read, as you have not specified the");
            ui.errorMessage("correct passphrase.  The passphrase prompt is " + CommandImpl.strip(_currentChannel.getPassphrasePrompt()));
            ui.errorMessage("To try and decrypt the metadata, use 'decrypt --passphrase \"the correct passphrase\"'");
            // technically, we don't have to return, and can list the readable and unreadable messages in the
            // channel, but its probably best not to
            return;
        }
        
        List privMsgIds = client.getMessageIdsPrivate(_currentChannel.getChannelHash());
        for (int i = 0; i < privMsgIds.size(); i++) {
            Long msgId = (Long)privMsgIds.get(i);
            _messageKeys.add(msgId);
            MessageInfo msg = client.getMessage(msgId.longValue());
            StringBuilder buf = new StringBuilder();
            String date = null;
            synchronized (_dayFmt) {
                date = _dayFmt.format(new Date(msg.getMessageId()));
            }
            if (msg.getReplyKeyUnknown() || msg.getReadKeyUnknown()) {
                buf.append("(undecrypted private message)\n\tuse 'decrypt --message ");
                buf.append(_messageKeys.size()-1).append("' to decrypt");
            } else if (msg.getPassphrasePrompt() != null) {
                buf.append("(undecrypted private message) - prompt: \"");
                buf.append(CommandImpl.strip(msg.getPassphrasePrompt()));
                buf.append("\"\n\tuse 'decrypt --message ");
                buf.append(_messageKeys.size()-1).append(" --passphrase $passphrase' to decrypt");
            } else {
                buf.append("(Private message) ");
                buf.append('[').append(date).append("] ");
                if (msg.getSubject() != null)
                    buf.append('\'').append(CommandImpl.strip(msg.getSubject())).append("\' ");
                else
                    buf.append("(no subject) ");
                if (msg.getAuthorChannelId() >= 0) {
                    ChannelInfo chanInfo = client.getChannel(msg.getAuthorChannelId());
                    buf.append(" written by ");
                    if (chanInfo != null) {
                        buf.append(chanInfo.getName()).append(" ");
                        buf.append("[").append(chanInfo.getChannelHash().toBase64().substring(0,6)).append("] ");
                    }
                }
            }
            _messageText.add(buf.toString());
        }
        
        Connection con = client.con();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            String sql = SQL_LIST_MESSAGES;
            if (!unauthorized)
                sql = sql + " AND wasAuthorized = TRUE";
            if (!unauthenticated)
                sql = sql + " AND wasAuthenticated = TRUE";
            stmt = con.prepareStatement(sql);
            stmt.setLong(1, channelId);
            ui.debugMessage("query: " + sql + " (channelId = " + channelId + ")");
            rs = stmt.executeQuery();
            while (rs.next()) {
                // msgId, messageId, subject, authorChannelHash
                long id = rs.getLong(1);
                if (rs.wasNull())
                    continue;
                Long messageId = new Long(rs.getLong(2));
                if (rs.wasNull())
                    messageId = null;
                String subject = rs.getString(3);
                long authorChannelId = rs.getLong(4);
                if (rs.wasNull()) authorChannelId = -1;
                //byte hash[] = rs.getBytes(4);
                
                // ok, matches criteria
                _messageKeys.add(new Long(id));
                StringBuilder buf = new StringBuilder();
                String date = null;
                if (messageId != null) {
                    synchronized (_dayFmt) {
                        date = _dayFmt.format(new Date(messageId.longValue()));
                    }
                }
                
                MessageInfo msg = client.getMessage(id);
                if (msg.getReplyKeyUnknown() || msg.getReadKeyUnknown()) {
                    buf.append("(undecrypted message)\n\tuse 'decrypt --message ");
                    buf.append(_messageKeys.size()-1).append("' to decrypt");
                } else if (msg.getPassphrasePrompt() != null) {
                    buf.append("(undecrypted message) - prompt: \"");
                    buf.append(CommandImpl.strip(msg.getPassphrasePrompt()));
                    buf.append("\"\n\tuse 'decrypt --message ");
                    buf.append(_messageKeys.size()-1).append(" --passphrase $passphrase' to decrypt");
                } else {
                    if (date == null)
                        buf.append("[????/??/??] ");
                    else
                        buf.append('[').append(date).append("] ");
                    if (subject != null)
                        buf.append('\'').append(CommandImpl.strip(subject)).append("\' ");
                    else
                        buf.append("(no subject) ");
                    if (authorChannelId >= 0) {
                        ChannelInfo info = client.getChannel(authorChannelId);
                        buf.append(" written by ");
                        if (info != null) {
                            buf.append(info.getName()).append(" ");
                            buf.append("[").append(info.getChannelHash().toBase64().substring(0,6)).append("] ");
                        }
                    }
                }
                _messageText.add(buf.toString());
            }
            ui.statusMessage(_messageKeys.size() + " messages matched - use 'next' to view them");
            ui.commandComplete(0, null);
        } catch (SQLException se) {
            ui.errorMessage("Internal error listing messages", se);
            ui.commandComplete(-1, null);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    /** threads [--channel ($index|$hash|all)] [--tags [-]tag[,[-]tag]*] [--includeUnauthorized $boolean] [--compact $boolean]*/
    private void processThreads(DBClient client, UI ui, Opts opts) {
        String chan = opts.getOptValue("channel");
        List tags = opts.getOptValues("tags");
        boolean includeUnauthorized = opts.getOptBoolean("includeUnauthorized", false);
        boolean compact = opts.getOptBoolean("compact", true);
        if ( (opts.getOptNames().size() <= 0) && (_threadText.size() > 0) ) {
            // just display the last result set
            for (int i = 0; i < _threadText.size(); i++) {
                String line = (String)_threadText.get(i);
                ui.statusMessage(line);
            }
            ui.statusMessage("Matching threads: " + _threadText.size());
        } else {
            // recalc the results
            _threadRootURIs.clear();
            _threadText.clear();

            Set channelHashes = new HashSet();
            if (chan == null) {
                if (_currentChannel != null) {
                    channelHashes.add(_currentChannel.getChannelHash());
                } else {
                    ui.errorMessage("To view threads in all channels, specify --channel all");
                    ui.commandComplete(-1, null);
                    return;
                }
            } else {
                byte chanHash[] = opts.getOptBytes("channel");
                if ( (chanHash != null) && (chanHash.length == Hash.HASH_LENGTH) ) {
                    channelHashes.add(new Hash(chanHash));
                } else if ("all".equalsIgnoreCase(chan)) {
                    channelHashes = null;
                } else {
                    try {
                        int index = Integer.parseInt(chan);
                        if ( (index >= 0) && (index < _channelKeys.size()) ) {
                            Long chanId = (Long)_channelKeys.get(index);
                            ChannelInfo info = client.getChannel(chanId.longValue());
                            channelHashes.add(info.getChannelHash());
                        } else {
                            ui.errorMessage("Index is out of range");
                            ui.commandComplete(-1, null);
                        }
                    } catch (NumberFormatException nfe) {
                        ui.errorMessage("Invalid channel index");
                        ui.commandComplete(-1, null);
                    }
                }
            }

            Set tagsRequired = new HashSet();
            Set tagsWanted = new HashSet();
            Set tagsRejected = new HashSet();
            if (tags != null) {
                for (int i = 0; i < tags.size(); i++) {
                    String tag = (String)tags.get(i);
                    if (tag.startsWith("-") && (tag.length() > 1))
                        tagsRejected.add(tag.substring(1));
                    else if (tag.startsWith("+") && (tag.length() > 1))
                        tagsRequired.add(tag.substring(1));
                    else
                        tagsWanted.add(tag);
                }
            }

            ui.debugMessage("Channels: " + (channelHashes == null ? "ALL" : channelHashes.toString()));
            ui.debugMessage("Required tags: " + tagsRequired.toString());
            ui.debugMessage("Wanted tags:   " + tagsWanted.toString());
            ui.debugMessage("Rejected tags: " + tagsRejected.toString());

            ThreadAccumulator accumulator = new ThreadAccumulator(client, ui);
            accumulator.setTags(tagsRequired, tagsWanted, tagsRejected);
            accumulator.setScope(channelHashes);
            accumulator.gatherThreads();
            Map order = new TreeMap(new HighestFirstComparator());
            for (int i = 0; i < accumulator.getThreadCount(); i++) {
                long mostRecentDate = accumulator.getMostRecentDate(i);
                Long when = new Long(mostRecentDate);
                while (order.containsKey(when))
                    when = new Long(when.longValue()+1);
                order.put(when, new Integer(i));
            }
            for (Iterator iter = order.values().iterator(); iter.hasNext(); ) {
                int i = ((Integer)iter.next()).intValue();
                SyndieURI rootURI = accumulator.getRootURI(i);
                _threadRootURIs.add(rootURI);
                Set threadTags = accumulator.getTags(i);
                int messages = accumulator.getMessages(i);
                String subject = accumulator.getSubject(i);
                long rootAuthorId = accumulator.getRootAuthor(i);
                long mostRecentAuthorId = accumulator.getMostRecentAuthor(i);
                long mostRecentDate = accumulator.getMostRecentDate(i);

                ChannelInfo rootAuthor = client.getChannel(rootAuthorId);
                ChannelInfo mostRecentAuthor = client.getChannel(mostRecentAuthorId);

                StringBuilder buf = new StringBuilder();
                if (compact) {
                    // 10: [2006/10/09 2 msgs] $subject (tag, tag, tag, tag)
                    buf.append(_threadText.size()).append(": [");
                    synchronized (_dayFmt) {
                        buf.append(_dayFmt.format(new Date(mostRecentDate)));
                    }
                    buf.append(" ").append(messages);
                    if (messages > 1)
                        buf.append(" msgs] ");
                    else
                        buf.append(" msg ] ");
                    buf.append(CommandImpl.strip(subject));
                    if (threadTags.size() > 0) {
                        buf.append(" [");
                        for (Iterator titer = threadTags.iterator(); titer.hasNext(); ) {
                            String tag = (String)titer.next();
                            buf.append(CommandImpl.strip(tag));
                            int count = accumulator.getTagCount(i, tag);
                            if (count > 1)
                                buf.append("#").append(count);
                            buf.append(" ");
                        }
                        buf.append("]");
                    }
                } else {
                    buf.append(_threadText.size()).append(": ").append(CommandImpl.strip(subject));
                    buf.append("\n\tOriginal author: ");
                    if (rootAuthor.getName() != null)
                        buf.append(CommandImpl.strip(rootAuthor.getName())).append(" ");
                    buf.append("(").append(rootAuthor.getChannelHash().toBase64().substring(0,6)).append(")");
                    if (messages > 1) {
                        buf.append("\n\tLast reply by ");
                        if (mostRecentAuthor.getName() != null)
                            buf.append(CommandImpl.strip(mostRecentAuthor.getName())).append(" ");
                        buf.append("(").append(mostRecentAuthor.getChannelHash().toBase64().substring(0,6)).append(")");
                    }
                    buf.append("\n\tPost date: ");
                    synchronized (_dayFmt) {
                        buf.append(_dayFmt.format(new Date(mostRecentDate)));
                    }
                    if (messages > 1)
                        buf.append("\n\t" + messages + " messages");
                    if (threadTags.size() > 0) {
                        buf.append("\n\tTags: ");
                        for (Iterator titer = threadTags.iterator(); titer.hasNext(); ) {
                            String tag = (String)titer.next();
                            buf.append(CommandImpl.strip(tag));
                            int count = accumulator.getTagCount(i, tag);
                            if (count > 1)
                                buf.append("#").append(count);
                            buf.append(" ");
                        }
                    }
                }
                String line = buf.toString();
                _threadText.add(line);
                ui.statusMessage(line);
            }
            ui.statusMessage("Matching threads: " + _threadText.size());
        }
        ui.commandComplete(0, null);
    }
    
    private static final class HighestFirstComparator implements Comparator, Serializable {
        public int compare(Object lhs, Object rhs) {
            if (lhs instanceof Long)
                return -1*((Long)lhs).compareTo((Long)rhs);
            else
                return -1*((Integer)lhs).compareTo((Integer)rhs);
        }
        
    }

    /** view [(--message ($index|$uri)|--thread $index)] [--page $n] : view a page in the given message */
    private void processView(DBClient client, UI ui, Opts opts) {
        boolean rebuildThread = opts.getOptBoolean("rebuildThread", true);
        String msg = opts.getOptValue("message");
        
        int threadIndex = (int)opts.getOptLong("thread", -1);
        if (threadIndex >= 0) {
            if (threadIndex >= _threadRootURIs.size()) {
                ui.errorMessage("Thread index is out of bounds");
                ui.commandComplete(-1, null);
                return;
            }
            SyndieURI uri = (SyndieURI)_threadRootURIs.get(threadIndex);
            msg = uri.toString();
        }
        
        if (msg != null) {
            int index = -1;
            try {
                index = Integer.parseInt(msg);
                if ( (index >= 0) && (index < _messageKeys.size()) ) {
                    long msgId = ((Long)_messageKeys.get(index)).longValue();
                    _currentMessage = client.getMessage(msgId);
                    if (rebuildThread)
                        _currentThreadRoot = null;
                } else {
                    ui.errorMessage("Requested message index is out of range");
                    ui.commandComplete(-1, null);
                }
            } catch (NumberFormatException nfe) {
                try {
                    SyndieURI uri = new SyndieURI(msg);
                    long chanId = client.getChannelId(uri.getScope());
                    if (chanId >= 0) {
                        _currentChannel = client.getChannel(chanId);
                        _currentMessage = client.getMessage(chanId, uri.getMessageId());
                        if (rebuildThread)
                            _currentThreadRoot = null;
                        if (_currentMessage != null) {
                            // ok, switched over
                        } else {
                            ui.statusMessage("The requested message is not known locally: " + uri);
                        }
                    } else {
                        ui.statusMessage("The requested message is not known locally: " + uri);
                    }
                } catch (URISyntaxException use) {
                    ui.errorMessage("The requested message is neither an index to the message list or a full syndie URI");
                    ui.commandComplete(-1, null);
                    return;
                }
            }
        }
        
        if (_currentMessage == null) {
            if (!rebuildThread) {
                displayThread(client, ui, rebuildThread);
                ui.commandComplete(0, null);
            } else {
                ui.errorMessage("Current message is null");
                ui.commandComplete(-1, null);
            }
        } else {
            displayMessage(client, ui, _currentMessage, (int)opts.getOptLong("page", 1));
            displayThread(client, ui, rebuildThread);
            ui.commandComplete(0, null);
        }
    }
    
    private static void displayMessage(DBClient client, UI ui, MessageInfo message, int page) {
        ChannelInfo scopeChan = client.getChannel(message.getScopeChannelId());
        if (scopeChan != null) {
            SyndieURI uri = SyndieURI.createMessage(scopeChan.getChannelHash(), message.getMessageId());
            ui.statusMessage("URI: " + uri.toString());
        } else {
            ui.errorMessage("Unable to find the channel info that the post was scoped under (" + message.getScopeChannelId() + ")");
        }
        
        if (message.getReplyKeyUnknown()) {
            ui.statusMessage("Message is an undecrypted private reply message");
            ui.statusMessage("You cannot read this message unless you have the channel's private reply key");
            ui.statusMessage("If you have the key, decrypt with 'decrypt'");
            // technically, we don't have to return, and can display the public tags/etc
            return;
        } else if (message.getReadKeyUnknown()) {
            ui.statusMessage("Message is an undecrypted post");
            ui.statusMessage("You cannot read this message unless you have the correct channel's read key");
            ui.statusMessage("If you have the key, decrypt with 'decrypt'");
            // technically, we don't have to return, and can display the public tags/etc
            return;
        } else if (message.getPassphrasePrompt() != null) {
            ui.statusMessage("Message is an undecrypted passphrase protected post");
            ui.statusMessage("You cannot read this message unless you know the correct passphrase");
            ui.statusMessage("The passphrase prompt is: " + CommandImpl.strip(message.getPassphrasePrompt()));
            ui.statusMessage("To try and decrypt the message, use 'decrypt --passphrase \"the correct passphrase\"'");
            // technically, we don't have to return, and can display the public tags/etc
            return;
        }
        
        if (page >= message.getPageCount())
            page = message.getPageCount();
        if (page <= 0)
            page = 1;
        if (message.getWasPrivate())
            ui.statusMessage("Message was privately encrypted to the channel reply key");
        if (message.getWasAuthenticated()) {
            long authorId = message.getAuthorChannelId();
            if (authorId >= 0) {
                if (message.getTargetChannelId() == authorId) {
                    // no need to mention that the channel's author posted in their own channel
                    ui.debugMessage("targetChannelId == authorChannelId");
                } else {
                    ChannelInfo info = client.getChannel(authorId);
                    if (info != null) {
                        StringBuilder buf = new StringBuilder();
                        buf.append("Author: ").append(CommandImpl.strip(info.getName()));
                        buf.append(" (").append(info.getChannelHash().toBase64().substring(0,6)).append(")");
                        ui.statusMessage(buf.toString());
                    }
                }
            } else {
                // author was the target channel itself, so no need to mention an Author
            }
        } else {
            ui.statusMessage("Author was not authenticated");
        }
        
        Hash chan = message.getTargetChannel();
        long chanId = message.getTargetChannelId();
        ChannelInfo targetChannel = client.getChannel(chanId);
        if (targetChannel != null) {
            StringBuilder buf = new StringBuilder();
            buf.append("Channel: ").append(CommandImpl.strip(targetChannel.getName()));
            buf.append(" (").append(targetChannel.getChannelHash().toBase64().substring(0,6)).append(") ");
            if (message.getWasAuthorized())
                buf.append("[post was authorized] ");
            else
                buf.append("[post was NOT authorized] ");
            if (message.getWasAuthenticated())
                buf.append("[post was authenticated] ");
            else
                buf.append("[post was NOT authenticated] ");
            ui.statusMessage(buf.toString());
        } else if (chan != null) {
            StringBuilder buf = new StringBuilder();
            buf.append("Channel: ");
            buf.append(" (").append(chan.toBase64().substring(0,6)).append(") ");
            if (message.getWasAuthorized())
                buf.append("[post was authorized] ");
            else
                buf.append("[post was NOT authorized] ");
            if (message.getWasAuthenticated())
                buf.append("[post was authenticated] ");
            else
                buf.append("[post was NOT authenticated] ");
            ui.statusMessage(buf.toString());
        }
        
        ui.statusMessage("MessageId: " + message.getMessageId());
        
        String when = null;
        synchronized (_dayFmt) { when = _dayFmt.format(new Date(message.getMessageId())); }
        ui.statusMessage("Date: " + when);
        
        
        if (message.getSubject() != null)
            ui.statusMessage("Subject: " + CommandImpl.strip(message.getSubject()));
        
        Set tags = new TreeSet();
        if (message.getPublicTags() != null)
            tags.addAll(message.getPublicTags());
        if (message.getPrivateTags() != null)
            tags.addAll(message.getPrivateTags());
        if ( (tags != null) && (tags.size() > 0) ) {
            StringBuilder buf = new StringBuilder();
            buf.append("Tags: ");
            for (Iterator iter = tags.iterator(); iter.hasNext(); ) {
                buf.append(CommandImpl.strip(iter.next().toString())).append(" ");
            }
            ui.statusMessage(buf.toString());
        }
        
        String content = client.getMessagePageData(message.getInternalId(), page-1);
        if (content == null) {
            ui.statusMessage("(content not available)");
        } else {
            ui.statusMessage("Page: " + page + " of " + message.getPageCount());
            ui.statusMessage("-----------------------------------------------------------------");
            ui.statusMessage(content);
            ui.statusMessage("-----------------------------------------------------------------");
            ui.statusMessage("Attachments: " + message.getAttachmentCount());
        }
        
        List refs = message.getReferences();
        if ( (refs != null) && (refs.size() > 0) ) {
            ui.statusMessage("References:");
            ReferenceNode.walk(refs, new RefWalker(ui));
        }
    }
    
    private static class RefWalker implements ReferenceNode.Visitor {
        private UI _ui;
        private int _nodes;
        public RefWalker(UI ui) { _ui = ui; _nodes = 0; }
        public void visit(ReferenceNode node, int indent, int siblingOrder) {
            SyndieURI uri = node.getURI();
            StringBuilder walked = new StringBuilder();
            
            walked.append(node.getTreeIndex()).append(": ");
            
            boolean wasKey = false;
            if (uri.getScope() != null) {
                if (uri.getString("readKey") != null) {
                    walked.append("Read key for " + uri.getScope().toBase64() + " included\n");
                    wasKey = true;
                } else if (uri.getString("postKey") != null) {
                    walked.append("Post key for " + uri.getScope().toBase64() + " included\n");
                    wasKey = true;
                } else if (uri.getString("manageKey") != null) {
                    walked.append("Manage key for " + uri.getScope().toBase64() + " included\n");
                    wasKey = true;
                } else if (uri.getString("replyKey") != null) {
                    walked.append("Reply key for " + uri.getScope().toBase64() + " included\n");
                    wasKey = true;
                }
            }

            if (!wasKey) {
                walked.append(CommandImpl.strip(node.getName()));
                if (node.getDescription() != null) {
                    walked.append(" - ");
                    walked.append(CommandImpl.strip(node.getDescription()));
                }
                walked.append(" [type: ").append(node.getReferenceType()).append("]\n");
                if (uri.isURL())
                    walked.append("\tURL: ").append(CommandImpl.strip(uri.getString("url")));
                else
                    walked.append("\tURI: ").append(uri.toString());
            }
            
            _ui.statusMessage(walked.toString());
            _nodes++;
        }
    }
    
    /**
     * importkey --position $position
     * import the key included in the given message reference
     */
    private void processImportKey(DBClient client, UI ui, Opts opts) {
        String position = opts.getOptValue("position");
        List refs = _currentMessage.getReferences();
        KeyRefWalker walker = new KeyRefWalker(ui, position);
        ReferenceNode.walk(refs, walker);
        ReferenceNode node = walker.getSelectedNode();
        if ( (node == null) || (node.getURI() == null) ) {
            ui.errorMessage("Invalid reference position");
            ui.commandComplete(-1, null);
            return;
        }
        SyndieURI uri = node.getURI();
        Hash scope = uri.getScope();
        ui.debugMessage("Selected reference: " + uri.toString() + " [for " + scope + "]");
        if (scope != null) {
            SessionKey readKey = uri.getReadKey();
            if (readKey != null) {
                // consider the read key authenticated if it was posted by the owner
                // or a manager of the channel it refers to
                boolean authenticated = false;
                long authorChan = _currentMessage.getAuthorChannelId();
                if (authorChan < 0)
                    authorChan = _currentMessage.getTargetChannelId();
                long scopeChan = client.getChannelId(scope);
                if (authorChan == scopeChan) {
                    authenticated = true;
                } else {
                    ChannelInfo info = client.getChannel(scopeChan);
                    Set managers = info.getAuthorizedManagers();
                    for (Iterator iter = managers.iterator(); iter.hasNext(); ) {
                        SigningPublicKey pub = (SigningPublicKey)iter.next();
                        long mgrChannel = client.getChannelId(pub.calculateHash());
                        if (mgrChannel == authorChan) {
                            authenticated = true;
                            break;
                        }
                    }
                }
                KeyImport.importKey(ui, client, Constants.KEY_FUNCTION_READ, scope, readKey.getData(), authenticated, false);
                ui.statusMessage("Read key for channel " + scope.toBase64() + " imported (authentic? " + authenticated + ")");
                ui.commandComplete(0, null);
                return;
            }

            SigningPrivateKey postKey = uri.getPostKey();
            if (postKey != null) {
                // consider the post key authentic if it is in the target channel's post or
                // manage list
                SigningPublicKey pub = KeyGenerator.getSigningPublicKey(postKey);
                boolean authenticated = false;
                if (pub.calculateHash().equals(scope))
                    authenticated = true;
                if (!authenticated) {
                    long scopeChan = client.getChannelId(scope);
                    if (scopeChan < 0) {
                        ui.debugMessage("Post key is for an unknown channel");
                    } else {
                        ChannelInfo info = client.getChannel(scopeChan);
                        if (info == null) {
                            ui.debugMessage("Post key is for an unloadable channel");
                        } else {
                            if (info.getAuthorizedPosters().contains(pub) ||
                                info.getAuthorizedManagers().contains(pub))
                                authenticated = true;
                        }
                    }
                }
                
                KeyImport.importKey(ui, client, Constants.KEY_FUNCTION_POST, scope, postKey.getData(), authenticated, false);
                ui.statusMessage("Post key for channel " + scope.toBase64() + " imported (authentic? " + authenticated + ")");
                ui.commandComplete(0, null);
                return;
            }
            
            SigningPrivateKey manageKey = uri.getManageKey();
            if (manageKey != null) {
                // consider the manage key authentic if it is in the target channel's manage list
                SigningPublicKey pub = KeyGenerator.getSigningPublicKey(manageKey);
                boolean authenticated = false;
                if (pub.calculateHash().equals(scope))
                    authenticated = true;
                if (!authenticated) {
                    long scopeChan = client.getChannelId(scope);
                    if (scopeChan < 0) {
                        ui.debugMessage("Manage key is for an unknown channel");
                    } else {
                        ChannelInfo info = client.getChannel(scopeChan);
                        if (info == null) {
                            ui.debugMessage("Manage key is for an unloadable channel");
                        } else {
                            if (info.getAuthorizedManagers().contains(pub))
                                authenticated = true;
                        }
                    }
                }
                
                KeyImport.importKey(ui, client, Constants.KEY_FUNCTION_MANAGE, scope, manageKey.getData(), authenticated, false);
                ui.statusMessage("Manage key for channel " + scope.toBase64() + " imported (authentic? " + authenticated + ")");
                ui.commandComplete(0, null);
                return;
            }
            
            PrivateKey replyKey = uri.getReplyKey();
            if (replyKey != null) {
                // consider the reply key authentic if it is in the target channel's reply key
                PublicKey pub = KeyGenerator.getPublicKey(replyKey);
                boolean authenticated = false;
                long scopeChan = client.getChannelId(scope);
                if (scopeChan < 0) {
                    ui.debugMessage("Reply key is for an unknown channel");
                } else {
                    ChannelInfo info = client.getChannel(scopeChan);
                    if (info == null) {
                        ui.debugMessage("Reply key is for an unloadable channel");
                    } else {
                        if (info.getEncryptKey().equals(pub))
                            authenticated = true;
                    }
                }
                
                KeyImport.importKey(ui, client, Constants.KEY_FUNCTION_REPLY, scope, replyKey.getData(), authenticated, false);
                ui.statusMessage("Reply key for channel " + scope.toBase64() + " imported (authentic? " + authenticated + ")");
                ui.commandComplete(0, null);
                return;
            }
        }
        ui.errorMessage("Reference does not have a key");
        ui.commandComplete(-1, null);
    }
    
    private static class KeyRefWalker implements ReferenceNode.Visitor {
        private UI _ui;
        private String _position;
        private ReferenceNode _selected;
        public KeyRefWalker(UI ui, String position) { _ui = ui; _position = position; }
        public ReferenceNode getSelectedNode() { return _selected; }
        public void visit(ReferenceNode node, int indent, int siblingOrder) {
            if (_selected != null) return;
            if (node.getTreeIndex().equalsIgnoreCase(_position))
                _selected = node;
        }
    }
    
    /**
                [Thread:
                $position: $channel $date $subject $author
                $position: $channel $date $subject $author
                $position: $channel $date $subject $author
                $position: $channel $date $subject $author]
                (thread display includes nesting and the current position,
                e.g. "1: $hash 2006/08/01 'I did stuff' me"
                     "1.1: $hash 2006/08/02 'Liar, you did not' you"
                     "2: $hash 2006/08/03 'No more stuff talk' foo"
                     "2.1: $hash 2006/08/03 'wah wah wah' you"
                     "2.1.1: $hash 2006/08/03 'what you said' me"
                     "* 2.2: $hash 2006/08/03 'message being displayed...' blah"
                     "2.2.1: $hash 2006/08/04 'you still talking?' moo")
     */
    private void displayThread(DBClient client, UI ui, boolean rebuildThread) {
        if (rebuildThread) {
            MessageThreadBuilder builder = new MessageThreadBuilder(client, ui);
            ui.debugMessage("building the thread from " + _currentMessage.getScopeChannel().toBase64().substring(0,6) + ":" + _currentMessage.getMessageId() 
                            + " (internalId: " + _currentMessage.getInternalId() + " channel: " + _currentMessage.getScopeChannelId() + ")");
            _currentThreadRoot = builder.buildThread(_currentMessage);
        } else {
            ui.debugMessage("Not rebuilding the thread");
        }
        if ( (_currentThreadRoot == null) || (_currentThreadRoot.getChildCount() == 0) ) {
            // only one message, no need to display a thread
        } else {
            List roots = new ArrayList(1);
            roots.add(_currentThreadRoot);
            ThreadWalker walker = new ThreadWalker(ui);
            ui.statusMessage("Thread: ");
            ReferenceNode.walk(roots, walker);
        }
    }
    
    private class ThreadWalker implements ReferenceNode.Visitor {
        private UI _ui;
        private int _nodes;
        public ThreadWalker(UI ui) { _ui = ui; _nodes = 0; }
        public void visit(ReferenceNode node, int indent, int siblingOrder) {
            SyndieURI uri = node.getURI();
            if (uri == null) return;
            Hash channel = uri.getScope();
            Long msgId = uri.getMessageId();
            if ( (channel == null) || (msgId == null) ) return;
            //_ui.debugMessage("Walking node " + _nodes + " - " + channel.toBase64() + ":" + msgId.longValue() + " [" + node.getTreeIndex() + "/" + node.getName() + "]");
            //if (node.getParent() == null)
            //    _ui.debugMessage("parent: none");
            //else
            //    _ui.debugMessage("parent: " + node.getParent().getURI());
            //_ui.debugMessage("Child count: " + node.getChildCount());
            
            StringBuilder walked = new StringBuilder();
            
            if ( (_currentMessage != null) && (_currentMessage.getScopeChannel().equals(channel)) && (msgId.longValue() == _currentMessage.getMessageId()) )
                walked.append("* ");
            
            walked.append(node.getTreeIndex()).append(": ");
            if (node.getDescription() == null) {
                // dummy element in the tree, representing a message we don't have locally
                walked.append(CommandImpl.strip(node.getName()));
                walked.append(" (").append(channel.toBase64().substring(0,6)).append(") ");
                String when = null;
                synchronized (_dayFmt) {
                    when = _dayFmt.format(new Date(msgId.longValue()));
                }
                walked.append(when).append(" ");
                walked.append("[message not locally known]");
            } else {
                walked.append(CommandImpl.strip(node.getName()));
                walked.append(" (").append(channel.toBase64().substring(0,6)).append(") ");
                String when = null;
                synchronized (_dayFmt) {
                    when = _dayFmt.format(new Date(msgId.longValue()));
                }
                walked.append(when).append(" ");
                walked.append(CommandImpl.strip(node.getDescription()));
            }
            _ui.statusMessage(walked.toString());
            _nodes++;
        }
    }

    /**
     * threadnext [--position $position]
     * view the next message in the thread (or the given thread position)
     */
    private void processThreadNext(DBClient client, UI ui, Opts opts) {
        if ( (_currentThreadRoot == null) || (_currentThreadRoot.getChildCount() == 0) ) {
            // only one message, there is no next
            ui.statusMessage("No remaining messages in the thread");
            ui.commandComplete(-1, null);
        } else {
            String position = opts.getOptValue("position");
            List roots = new ArrayList(1);
            roots.add(_currentThreadRoot);
            NextThreadWalker walker = new NextThreadWalker(ui, position);
            ReferenceNode.walk(roots, walker);
            SyndieURI uri = walker.getNextURI();
            if (uri != null) {
                Opts viewOpts = new Opts();
                viewOpts.setCommand("view");
                viewOpts.setOptValue("message", uri.toString());
                viewOpts.setOptValue("rebuildThread", "false");
                processView(client, ui, viewOpts);
            } else {
                ui.statusMessage("No remaining messages in the thread");
                ui.commandComplete(-1, null);
            }
        }
    }
    
    private class NextThreadWalker implements ReferenceNode.Visitor {
        private UI _ui;
        private String _wanted;
        private int _nodes;
        private SyndieURI _nextURI;
        private SyndieURI _prevURI;
        private boolean _prevWasCurrent;
        public NextThreadWalker(UI ui, String wanted) { _ui = ui; _nodes = 0; _wanted = wanted; }
        public SyndieURI getNextURI() { return _nextURI; }
        public SyndieURI getPrevURI() { return _prevURI; }
        public void visit(ReferenceNode node, int indent, int siblingOrder) {
            SyndieURI uri = node.getURI();
            if (uri == null) return;
            Hash channel = uri.getScope();
            Long msgId = uri.getMessageId();
            if ( (channel == null) || (msgId == null) ) return;
            
            _ui.debugMessage("Visiting " + node.getTreeIndex() + ": " + channel.toBase64().substring(0,6) + ":" + msgId);
            if (_nextURI != null) return; // done
            if (node.getDescription() == null) // not known locally
                return;
            if (_prevWasCurrent) {
                _prevWasCurrent = false;
                if (_wanted == null) { // pick next available
                    _nextURI = node.getURI();
                    _ui.debugMessage("no position specified and the previous was current.  setting next=" + node.getTreeIndex());
                    return;
                }
            }
            
            if ( (_currentMessage != null) && (_currentMessage.getScopeChannel().equals(channel)) && (msgId.longValue() == _currentMessage.getMessageId()) ) {
                _prevWasCurrent = true;
                _ui.debugMessage("current message is being viewed (" + node.getTreeIndex() + ")");
            } else {
                _prevURI = uri;
                _ui.debugMessage("current message is not being viewed, updating prevURI to " + node.getTreeIndex());
            }
            
            if ( (_wanted != null) && (_wanted.equalsIgnoreCase(node.getTreeIndex())) ) {
                if (node.getName() == null) {
                    // dummy element in the tree, representing a message we don't have locally
                    _ui.errorMessage("Requested thread message is not known locally: " + node.getURI().toString());
                } else {
                    _nextURI = uri;
                    _prevURI = uri;
                    _ui.debugMessage("explicit position is matched (treeIndex of " + node.getTreeIndex() + ")");
                }
            }
            _nodes++;
        }
    }
    /**
     * threadprev [--position $position]
     * view the previous message in the thread (or the given thread position)
     */
    private void processThreadPrev(DBClient client, UI ui, Opts opts) {
        if ( (_currentThreadRoot == null) || (_currentThreadRoot.getChildCount() == 0) ) {
            // only one message, there is no previous
            ui.statusMessage("No earlier messages in the thread");
            ui.commandComplete(-1, null);
        } else {
            String position = opts.getOptValue("position");
            List roots = new ArrayList(1);
            roots.add(_currentThreadRoot);
            NextThreadWalker walker = new NextThreadWalker(ui, position);
            ReferenceNode.walk(roots, walker);
            SyndieURI uri = walker.getPrevURI();
            if (uri != null) {
                Opts viewOpts = new Opts();
                viewOpts.setCommand("view");
                viewOpts.setOptValue("message", uri.toString());
                viewOpts.setOptValue("rebuildThread", "false");
                processView(client, ui, viewOpts);
            } else {
                ui.statusMessage("No earlier messages in the thread");
                ui.commandComplete(-1, null);
            }
        }
    }
    
    /** export [--message ($index|$uri)] --out $directory */
    private void processExport(DBClient client, UI ui, Opts opts) {
        String msg = opts.getOptValue("message");
        if (msg != null) {
            try {
                int index = Integer.parseInt(msg);
                if ( (index >= 0) && (index < _messageKeys.size()) ) {
                    _currentMessage = client.getMessage(((Long)_messageKeys.get(index)).longValue());
                    _currentThreadRoot = null;
                } else {
                    ui.errorMessage("Message index is out of range (highest value is " + _messageKeys.size() + ")");
                    ui.commandComplete(-1, null);
                    return;
                }
            } catch (NumberFormatException nfe) {
                // try it as a full URI
                try {
                    SyndieURI uri = new SyndieURI(msg);
                    long chanId = client.getChannelId(uri.getScope());
                    if (chanId >= 0) {
                        _currentChannel = client.getChannel(chanId);
                        _currentMessage = client.getMessage(chanId, uri.getMessageId());
                        _currentThreadRoot = null;
                        if (_currentMessage != null) {
                            // ok, switched over
                        } else {
                            ui.statusMessage("Switched over to the specified channel, but the requested message was not known (" + uri + ")");
                            ui.commandComplete(0, null);
                            return;
                        }
                    } else {
                        ui.statusMessage("The message requested is not in a locally known channel (" + uri.getScope() + ")");
                        ui.commandComplete(0, null);
                        return;
                    }
                } catch (URISyntaxException use) {
                    ui.errorMessage("The requested message is neither an index to the message list or a full syndie URI");
                    ui.commandComplete(-1, null);
                    return;
                }
            }
        }
        
        if (_currentMessage == null) {
            ui.errorMessage("No implicit message known, please specify one with --message $index or --message $syndieURI");
            ui.commandComplete(-1, null);
            return;
        }
        
        CLI.Command cmd = CLI.getCommand("viewmessage");
        if (cmd == null) {
            ui.errorMessage("Internal error extracting the message");
            ui.commandComplete(-1, null);
            return;
        }
        
        String out = opts.getOptValue("out");
        if (out == null) {
            ui.errorMessage("You must specify where the message should be extracted to with --out $outDir");
            ui.commandComplete(-1, null);
            return;
        }
        
        NestedUI nestedUI = new NestedUI(ui);
        Opts viewOpts = new Opts();
        viewOpts.setCommand("viewmessage");
        viewOpts.setOptValue("internalid", Long.toString(_currentMessage.getInternalId()));
        viewOpts.setOptValue("out", out);
        cmd.runCommand(viewOpts, nestedUI, client);
        ui.commandComplete(nestedUI.getExitCode(), null);
    }
    
    /** save [--message ($index|$uri)] (--page $n|--attachment $n) --out $filename */
    private void processSave(DBClient client, UI ui, Opts opts) {
        String msg = opts.getOptValue("message");
        if (msg != null) {
            try {
                int index = Integer.parseInt(msg);
                if ( (index >= 0) && (index < _messageKeys.size()) ) {
                    _currentMessage = client.getMessage(((Long)_messageKeys.get(index)).longValue());
                    _currentThreadRoot = null;
                } else {
                    ui.errorMessage("Message index is out of range (highest value is " + _messageKeys.size() + ")");
                    ui.commandComplete(-1, null);
                    return;
                }
            } catch (NumberFormatException nfe) {
                // try it as a full URI
                try {
                    SyndieURI uri = new SyndieURI(msg);
                    long chanId = client.getChannelId(uri.getScope());
                    if (chanId >= 0) {
                        _currentChannel = client.getChannel(chanId);
                        _currentMessage = client.getMessage(chanId, uri.getMessageId());
                        _currentThreadRoot = null;
                        if (_currentMessage != null) {
                            // ok, switched over
                        } else {
                            ui.statusMessage("Switched over to the specified channel, but the requested message was not known (" + uri + ")");
                            ui.commandComplete(0, null);
                            return;
                        }
                    } else {
                        ui.statusMessage("The message requested is not in a locally known channel (" + uri.getScope() + ")");
                        ui.commandComplete(0, null);
                        return;
                    }
                } catch (URISyntaxException use) {
                    ui.errorMessage("The requested message is neither an index to the message list or a full syndie URI");
                    ui.commandComplete(-1, null);
                    return;
                }
            }
        }
        
        if (_currentMessage == null) {
            ui.errorMessage("No implicit message known, please specify one with --message $index or --message $syndieURI");
            ui.commandComplete(-1, null);
            return;
        }
        
        int page = (int)opts.getOptLong("page", -1);
        int attach = (int)opts.getOptLong("attachment", -1);
        if ( (page < 0) && (attach < 0) ) {
            ui.errorMessage("Please specify a page or attachment to save with --page $num or --attachment $num");
            ui.commandComplete(-1, null);
            return;
        }
        if ( (page >= 0) && (page >= _currentMessage.getPageCount()) ) {
            ui.errorMessage("Page is out of range (number of pages: " + _currentMessage.getPageCount() + ")");
            ui.commandComplete(-1, null);
            return;
        }
        if ( (attach >= 0) && (attach >= _currentMessage.getAttachmentCount()) ) {
            ui.errorMessage("Attachment is out of range (number of attachments: " + _currentMessage.getAttachmentCount() + ")");
            ui.commandComplete(-1, null);
            return;
        }
        
        String filename = opts.getOptValue("out");
        if (filename == null) {
            ui.errorMessage("Please specify a file to save the content as with --out $filename");
            ui.commandComplete(-1, null);
            return;
        }
        
        FileOutputStream fos = null;
        try {
            fos = new SecureFileOutputStream(filename);
            if (page >= 0) {
                String data = client.getMessagePageData(_currentMessage.getInternalId(), page);
                fos.write(DataHelper.getUTF8(data));
            } else {
                fos.write(client.getMessageAttachmentData(_currentMessage.getInternalId(), attach));
            }
            fos.close();
            fos = null;
            ui.statusMessage("Content written to " + filename);
            ui.commandComplete(0, null);
        } catch (IOException ioe) {
            ui.errorMessage("Error writing the content to " + filename, ioe);
            ui.commandComplete(-1, null);
        } finally {
            if (fos != null) try { fos.close(); } catch (IOException ioe) {}
        }
    }

    /** reply */
    private void processReply(DBClient client, UI ui, Opts opts) {
        if (_currentMessage == null) {
            ui.errorMessage("Cannot reply - there is no current message");
            ui.commandComplete(-1, null);
            return;
        }
        Hash target = _currentMessage.getTargetChannel();
        ui.insertCommand("menu post");
        ui.insertCommand("create --channel " + target.toBase64());
        ui.insertCommand("addparent --uri " + _currentMessage.getURI().toString());
        for (int i = 0; i < _currentMessage.getHierarchy().size() && i < 5; i++) {
            SyndieURI uri = (SyndieURI)_currentMessage.getHierarchy().get(i);
            ui.insertCommand("addParent --uri " + uri.toString());
        }
    }
    
    /**
     * ban [--scope (author|channel|$hash)] [--delete $boolean]
     * ban the author or channel so that no more posts from that author
     * or messages by any author in that channel will be allowed into the
     * Syndie archive.  If --delete is specified, the messages themselves
     * will be removed from the archive as well as the database
     */
    private void processBan(DBClient client, UI ui, Opts opts) {
        String scope = opts.getOptValue("scope");
        Hash bannedChannel = null;
        if (scope == null) {
            if (_currentMessage != null) {
                // if the scope is not specified and we are viewing a message,
                // ban the author ofthe message (or the channel it is in if no author is specified)
                bannedChannel = getScopeToBan(client, _currentMessage, true);
            } else {
                // if the scope is not specified and we are not viewing a message,
                // ban the channel we are in (if any)
                if (_currentChannel != null) {
                    bannedChannel = _currentChannel.getChannelHash();
                }
            }
        } else {
            // scope is specified
            if ("author".equalsIgnoreCase(scope)) {
                bannedChannel = getScopeToBan(client, _currentMessage, true);
            } else if ("channel".equalsIgnoreCase(scope)) {
                bannedChannel = getScopeToBan(client, _currentMessage, false);
                if (bannedChannel == null)
                    bannedChannel = _currentChannel.getChannelHash();
            } else {
                byte scopeBytes[] = Base64.decode(scope);
                if ( (scopeBytes != null) && (scopeBytes.length == Hash.HASH_LENGTH) )
                    bannedChannel = new Hash(scopeBytes);
            }
        }
        
        if (bannedChannel != null) {
            boolean delete = opts.getOptBoolean("delete", true);
            client.ban(bannedChannel, ui, delete);
            ui.statusMessage("Scope banned: " + bannedChannel.toBase64() + " (all posts/metadata deleted? " + delete + ")");
            ui.commandComplete(0, null);
        } else {
            ui.errorMessage("Usage: ban [--scope (author|channel|$hash)] [--delete $boolean]");
            ui.commandComplete(-1, null);
        }
    }
    private Hash getScopeToBan(DBClient client, MessageInfo message, boolean banAuthor) {
        if (message == null) return null;
        Hash bannedChannel = null;
        if (banAuthor) {
            long authorId = message.getAuthorChannelId();
            if (authorId >= 0) {
                ChannelInfo author = client.getChannel(authorId);
                if (author != null) {
                    bannedChannel = author.getChannelHash();
                }
            }
            if (bannedChannel == null) {
                long scopeId = message.getScopeChannelId();
                if (scopeId >= 0) {
                    ChannelInfo scopeChan = client.getChannel(scopeId);
                    if (scopeChan != null) {
                        bannedChannel = scopeChan.getChannelHash();
                    }
                }
            }
        }
        if (bannedChannel == null)
            bannedChannel = message.getTargetChannel();
        return bannedChannel;
    }

    /**
     * decrypt [(--message $msgId|--channel $channelId)] [--passphrase pass]
     */
    private void processDecrypt(DBClient client, UI ui, Opts opts) {
        int messageIndex = (int)opts.getOptLong("message", -1);
        int channelIndex = (int)opts.getOptLong("channel", -1);
        String passphrase = opts.getOptValue("passphrase");
        
        File archivedFile = null;
        File archiveDir = client.getArchiveDir();
        if (messageIndex >= 0) {
            if (messageIndex < _messageKeys.size()) {
                Long msgId = (Long)_messageKeys.get(messageIndex);
                MessageInfo msg = client.getMessage(msgId.longValue());
                if (msg != null) {
                    Hash scope = msg.getScopeChannel();
                    File channelDir = new File(archiveDir, scope.toBase64());
                    archivedFile = new File(channelDir, msg.getMessageId() + Constants.FILENAME_SUFFIX);
                } else {
                    ui.errorMessage("The message specified could not be found");
                    ui.commandComplete(-1, null);
                    return;
                }
            } else {
                ui.errorMessage("The message index is out of bounds");
                ui.commandComplete(-1, null);
                return;
            }
        } else if (channelIndex >= 0) {
            if (channelIndex < _channelKeys.size()) {
                Long channelId = (Long)_channelKeys.get(channelIndex);
                ChannelInfo chan = client.getChannel(channelId.longValue());
                if (chan != null) {
                    File channelDir = new File(archiveDir, chan.getChannelHash().toBase64());
                    archivedFile = new File(channelDir, "meta" + Constants.FILENAME_SUFFIX);
                } else {
                    ui.errorMessage("The channel metadata specified could not be found");
                    ui.commandComplete(-1, null);
                    return;
                }
            } else {
                ui.errorMessage("The channel index is out of bounds");
                ui.commandComplete(-1, null);
                return;
            }
        } else {
            if (_currentMessage != null) {
                Hash scope = _currentMessage.getScopeChannel();
                File channelDir = new File(archiveDir, scope.toBase64());
                archivedFile = new File(channelDir, _currentMessage.getMessageId() + Constants.FILENAME_SUFFIX);
            } else if (_currentChannel != null) {
                File channelDir = new File(archiveDir, _currentChannel.getChannelHash().toBase64());
                archivedFile = new File(channelDir, "meta" + Constants.FILENAME_SUFFIX);
            } else {
                ui.errorMessage("No channel or message specified to decrypt");
                ui.commandComplete(-1, null);
                return;
            }
        }
        
        if ( (archivedFile != null) && (!archivedFile.exists()) ) {
            ui.errorMessage("The decryption could not be completed, because the signed archive file");
            ui.errorMessage("was not retained");
            ui.commandComplete(-1, null);
            return;
        }
        
        Importer imp = new Importer(client, client.getPass());
        NestedUI nestedUI = new NestedUI(ui);
        try {
            ui.debugMessage("Importing from " + archivedFile.getPath());
            boolean ok = imp.processMessage(nestedUI, new FileInputStream(archivedFile), client.getLoggedInNymId(), client.getPass(), passphrase, false, null, null);
            if (ok) {
                if (nestedUI.getExitCode() == 0) {
                    ui.statusMessage("Decrypted successfully");
                    ui.commandComplete(0, null);
                } else {
                    ui.errorMessage("Decryption failed");
                    ui.commandComplete(nestedUI.getExitCode(), null);
                }
            } else {
                ui.errorMessage("Decryption and import failed");
                ui.commandComplete(-1, null);
            }
        } catch (IOException ioe) {
            ui.errorMessage("Decryption failed");
            ui.commandComplete(-1, null);
        }
    }
}
