package syndie.db;

import java.io.*;
import java.util.*;
import net.i2p.data.*;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
import syndie.data.SyndieURI;

/**
 *
 */
public class ArchiveIndex {
    private List _channelEntries;

    /** default max file size to include in the index when filtering */
    static final long DEFAULT_MAX_SIZE = 32*1024;
    
    private ArchiveIndex() {
        _channelEntries = new ArrayList();
    }
    
    public int getChannelCount() { return _channelEntries.size(); }
    public ArchiveChannel getChannel(int index) { return (ArchiveChannel)_channelEntries.get(index); }
    private void addChannel(ArchiveChannel channel) { _channelEntries.add(channel); }

    public ArchiveMessage getMessage(SyndieURI uri) {
        ArchiveChannel chan = getChannel(uri);
        if (chan != null) {
            long msgId = uri.getMessageId().longValue();
            for (int j = 0; j < chan.getMessageCount(); j++) {
                ArchiveMessage me = chan.getMessage(j);
                if (me.getMessageId() == msgId)
                    return me;
            }
        }
        return null;
    }
    public ArchiveChannel getChannel(SyndieURI uri) {
        for (int i = 0; i < getChannelCount(); i++) {
            ArchiveChannel e = getChannel(i);
            if (DataHelper.eq(e.getScope(), uri.getScope().getData()))
                return e;
        }
        return null;
    }
    
    /** how new a message has to be to be considered 'new' */
    private static final int AGE_NEW_DAYS = 3;
    
    public static ArchiveIndex buildIndex(DBClient client, UI ui, File archiveDir, long maxSize) throws IOException {
        ArchiveIndex index = new ArchiveIndex();
        File channelDirs[] = archiveDir.listFiles();
        for (int i = 0; i < channelDirs.length; i++) {
            if (channelDirs[i].isDirectory())
                buildChannelIndex(client, ui, channelDirs[i], index, maxSize);
        }
        return index;
    }
    public static ArchiveIndex buildChannelIndex(DBClient client, UI ui, File channelDir, long maxSize) throws IOException {
        ArchiveIndex index = new ArchiveIndex();
        buildChannelIndex(client, ui, channelDir, index, maxSize);
        return index;
    }
    
    /**
     * rather than automatically including all of the unauthorized messages, 
     * only include up to 50 'new' messages
     */
    private static final int MAX_UNAUTHORIZED_INDEXED = 50;
    
    private static void buildChannelIndex(DBClient client, UI ui, File channelDir, ArchiveIndex index, long maxSize) throws IOException {
        byte chanHash[] = Base64.decode(channelDir.getName());
        long chanId = client.getChannelId(new Hash(chanHash));
        if (chanId < 0) {
            ui.errorMessage("Channel " + channelDir.getName() + " is invalid within the archive?");
            return;
        }

        ChannelInfo info = client.getChannel(chanId);
        if (info == null) {
            ui.errorMessage("Channel " + channelDir.getName() + " is in the archive, but not the database?");
            return;
        }
        
        ArchiveChannel chan = new ArchiveChannel(ui);
        
        List messages = new ArrayList();
        List pseudoAuthMessages = new ArrayList();
        List unauthorizedMessages = new ArrayList();
    
        // grab authorized messages
        List authorizedIds = client.getMessageIdsAuthorized(info.getChannelHash());
        for (int i = 0; i < authorizedIds.size(); i++) {
            ui.debugMessage("Authorized messageIds for " + info.getChannelHash().toBase64() + ": " + authorizedIds);
            Long msgId = (Long)authorizedIds.get(i);
            MessageInfo msgInfo = client.getMessage(msgId.longValue());
            if (msgInfo != null) {
                if ( (msgInfo.getExpiration() > 0) && (msgInfo.getExpiration() < System.currentTimeMillis()) )
                    continue;
                File msgFile = null;
                if (msgInfo.getScopeChannel().equals(info.getChannelHash())) {
                    msgFile = new File(channelDir, msgInfo.getMessageId() + Constants.FILENAME_SUFFIX);
                } else {
                    File dir = new File(channelDir.getParentFile(), msgInfo.getScopeChannel().toBase64());
                    msgFile = new File(dir, msgInfo.getMessageId() + Constants.FILENAME_SUFFIX);
                }
                if (msgFile.exists()) {
                    long size = msgFile.length();
            
                    String name = msgFile.getName();
                    long when = msgFile.lastModified();
                    when = when - (when % 24*60*60*1000); // ignore the time of day

                    if (size > maxSize)
                        continue;
            
                    ArchiveMessage entry = new ArchiveMessage();
                    entry.setMessageId(msgInfo.getMessageId());
                    entry.setReceiveDate(when);
                    entry.setEntrySize(size);
                    entry.setPrimaryScope(msgInfo.getScopeChannel());

                    boolean isNew = false;
                    if (when >= System.currentTimeMillis() - AGE_NEW_DAYS*24*60*60*1000)
                        isNew = true;

                    int flags = 0;
                    if (msgInfo.getWasPrivate())
                        flags |= ArchiveMessage.MASK_REPLY;
                    if (msgInfo.getWasAuthorized())
                        flags |= ArchiveMessage.MASK_AUTHORIZED;
                    if (msgInfo.getWasPassphraseProtected())
                        flags |= ArchiveMessage.MASK_PBE;
                    if (isNew)
                        flags |= ArchiveMessage.MASK_NEW;
                    entry.setFlags(flags);

                    if (info.getChannelHash().equals(entry.getPrimaryScope()))
                        messages.add(entry);
                    else
                        pseudoAuthMessages.add(entry);
                } else {
                    // ok, known authenticated post, but we don't have the original
                    // signed message anymore
                }
            }
        }
        
        // grab unauthorized yet authenticated messages
        List authIds = client.getMessageIdsAuthenticated(info.getChannelHash());
        // grab unauthenticated messages
        // ?!? why would we want to pass on unauthenticated?  no unauthenticated unauthorized
        // posts - just create a random identity and authenticate with that if you want to.
        //List unauthIds = client.getMessageIdsUnauthenticated(info.getChannelHash());
        //unauthIds.addAll(authIds);
        File archiveDir = client.getArchiveDir();
        for (int i = 0; i < authIds.size() && i < MAX_UNAUTHORIZED_INDEXED; i++) {
            Long msgId = (Long)authIds.get(i);
            MessageInfo msgInfo = client.getMessage(msgId.longValue());
            if (msgInfo != null) {
                if ( (msgInfo.getExpiration() > 0) && (msgInfo.getExpiration() < System.currentTimeMillis()) )
                    continue;
                long scopeChanId = msgInfo.getScopeChannelId();
                ChannelInfo scopeChan = client.getChannel(scopeChanId);
                if (scopeChan == null)
                    continue;
                File scopeChanDir = new File(archiveDir, scopeChan.getChannelHash().toBase64());
                if (!scopeChanDir.exists())
                    continue; // known in the db, not in the archive
                File msgFile = new File(scopeChanDir, msgInfo.getMessageId() + Constants.FILENAME_SUFFIX);
                if (msgFile.exists()) {
                    long size = msgFile.length();
            
                    String name = msgFile.getName();
                    long when = msgFile.lastModified();
                    when = when - (when % 24*60*60*1000); // ignore the time of day

                    if (size > maxSize)
                        continue;

                    boolean isNew = false;
                    if (when >= System.currentTimeMillis() - AGE_NEW_DAYS*24*60*60*1000)
                        isNew = true;

                    if (!isNew) {
                        // unauth only includes new posts
                        continue;
                    }
                    
                    ArchiveMessage entry = new ArchiveMessage();
                    entry.setMessageId(msgInfo.getMessageId());
                    entry.setReceiveDate(when);
                    entry.setEntrySize(size);
                    entry.setPrimaryScope(scopeChan.getChannelHash());

                    int flags = 0;
                    if (msgInfo.getWasPrivate())
                        flags |= ArchiveMessage.MASK_REPLY;
                    if (msgInfo.getWasAuthorized())
                        flags |= ArchiveMessage.MASK_AUTHORIZED;
                    if (msgInfo.getWasPassphraseProtected())
                        flags |= ArchiveMessage.MASK_PBE;
                    if (isNew)
                        flags |= ArchiveMessage.MASK_NEW;
                    entry.setFlags(flags);

                    unauthorizedMessages.add(entry);
                } else {
                    // ok, known unauthenticated post, but we don't have the original
                    // signed message anymore
                }
            }
        }

        // grab the metadata
        File mdFile = new File(channelDir, "meta" + Constants.FILENAME_SUFFIX);
        long mdSize = mdFile.length();
        long mdDate = mdFile.lastModified();
        mdDate = mdDate - (mdDate % 24*60*60*1000); // ignore the time of day
        chan.setReceiveDate(mdDate);
        chan.setEntrySize(mdSize);
        
        chan.setScope(chanHash);
        chan.setVersion(info.getEdition());
        chan.setMessages(messages);
        chan.setPseudoAuthorizedMessages(pseudoAuthMessages);
        chan.setUnauthorizedMessages(unauthorizedMessages);
        
        index.addChannel(chan);
        return;
    }

    public static ArchiveIndex loadIndex(File in, UI ui, boolean unauth) throws IOException {
        ArchiveIndex index = new ArchiveIndex();
        FileInputStream fin = null;
        try {
            fin = new FileInputStream(in);
            while (true) {
                ArchiveChannel entry = new ArchiveChannel(ui);
                boolean ok = entry.read(fin, unauth);
                if (ok) {
                    if (unauth) {
                        ui.debugMessage("Index contains the unauthorized channel data for " + Base64.encode(entry.getScope()));
                        for (int i = 0 ; i < entry.getUnauthorizedMessageCount(); i++) {
                            ArchiveMessage msg = entry.getUnauthorizedMessage(i);
                            ui.debugMessage(i + ": " + msg.getPrimaryScope().toBase64() + ":" + msg.getMessageId() + "/" + msg.getIsAuthorized() + "/"+msg.getIsNew() + "/" + msg.getIsPasswordProtected() + "/" + msg.getIsReply() + "/" + ((msg.getEntrySize()+1023)/1024) + "KB");
                        }
                    } else {
                        ui.debugMessage("Index contains the channel data for " + Base64.encode(entry.getScope()));
                        for (int i = 0 ; i < entry.getMessageCount(); i++) {
                            ArchiveMessage msg = entry.getMessage(i);
                            ui.debugMessage(i + ": " + msg.getMessageId() + "/" + msg.getIsAuthorized() + "/"+msg.getIsNew() + "/" + msg.getIsPasswordProtected() + "/" + msg.getIsReply() + "/" + ((msg.getEntrySize()+1023)/1024) + "KB");
                        }
                        ui.debugMessage("Pseudoauthorized messages: " + entry.getPseudoAuthorizedMessageCount());
                        for (int i = 0 ; i < entry.getPseudoAuthorizedMessageCount(); i++) {
                            ArchiveMessage msg = entry.getPseudoAuthorizedMessage(i);
                            ui.debugMessage(i + ": " + msg.getPrimaryScope().toBase64() +":" + msg.getMessageId() + "/" + msg.getIsAuthorized() + "/"+msg.getIsNew() + "/" + msg.getIsPasswordProtected() + "/" + msg.getIsReply() + "/" + ((msg.getEntrySize()+1023)/1024) + "KB");
                        }
                    }
                    index.addChannel(entry);
                } else {
                    break;
                }
            }
        } finally {
            if (fin != null) fin.close();
        }
        return index;
    }

    /**
     * compare the current index and the locally known messages, filtering out
     * banned/ignored/deleted posts/authors/channels, etc.
     */
    public ArchiveDiff diff(DBClient client, UI ui, Opts opts) {
        long maxSize = opts.getOptLong("maxSize", DEFAULT_MAX_SIZE);
        ArchiveDiff rv = new ArchiveDiff();
        List banned = client.getBannedChannels();
        for (int i = 0; i < _channelEntries.size(); i++) {
            ArchiveChannel chan = (ArchiveChannel)_channelEntries.get(i);            

            if (chan.getEntrySize() > maxSize) {
                ui.debugMessage("Indexed channel metadata is too large (" + chan.getEntrySize() + " bytes)");
                continue;
            }
            
            byte scope[] = chan.getScope();
            if (banned.contains(new Hash(scope))) {
                ui.debugMessage("Skipping banned channel " + Base64.encode(scope));
                continue;
            }
            
            if (chan.getUnauthorizedMessageCount() > 0) {
                diffUnauth(client, ui, opts, chan, banned, rv);
                continue;
            }
            
            long channelId = client.getChannelId(new Hash(scope));
            ChannelInfo chanInfo = null;
            if (channelId >= 0)
                chanInfo = client.getChannel(channelId);
            
            SyndieURI chanURI = SyndieURI.createScope(new Hash(scope));
            
            if (chanInfo == null) {
                rv.totalNewChannels++;
            } else if (chan.getVersion() > chanInfo.getEdition()) {
                rv.totalUpdatedChannels++;
                rv.fetchKnownBytes += chan.getEntrySize();
                rv.fetchKnownMetadata.add(chanURI);
            }
            
            if ( (chanInfo == null) || (chan.getVersion() > chanInfo.getEdition()) ) {
                rv.fetchNewMetadata.add(chanURI);
                rv.fetchNewBytes += chan.getEntrySize();
                rv.fetchMetaMessages.add(chanURI);
                rv.fetchMetaBytes += chan.getEntrySize();        
            }
            
            boolean newMsgFound = false;
            boolean newPIRMsgFound = false;
            List chanMsgs = new ArrayList();
            for (int j = 0; j < chan.getMessageCount(); j++)
                chanMsgs.add(chan.getMessage(j));
            for (int j = 0; j < chan.getPseudoAuthorizedMessageCount(); j++)
                chanMsgs.add(chan.getPseudoAuthorizedMessage(j));
            for (int j = 0; j < chanMsgs.size(); j++) {
                ArchiveMessage msg = (ArchiveMessage)chanMsgs.get(j);
                SyndieURI msgURI = SyndieURI.createMessage(msg.getPrimaryScope(), msg.getMessageId());
                if (!banned.contains(msg.getPrimaryScope())) {
                    long scopeId = client.getChannelId(msg.getPrimaryScope());
                    long msgId = client.getMessageId(scopeId, msg.getMessageId());
                    if ( (msgId < 0) && (msg.getEntrySize() <= maxSize) ) {
                        ui.debugMessage("new message: " + msg.getPrimaryScope().toBase64() + ":" + msg.getMessageId() + " (" + scopeId + "/" + msgId + ")");
                        rv.fetchNewBytes += msg.getEntrySize();
                        if (msg.getIsReply())
                            rv.fetchNewReplies.add(msgURI);
                        else
                            rv.fetchNewPosts.add(msgURI);
                        if (chanInfo != null) {
                            rv.totalNewMessagesOnKnownChannels++;
                            rv.fetchKnownBytes += msg.getEntrySize();
                            if (msg.getIsReply())
                                rv.fetchKnownReplies.add(msgURI);
                            else
                                rv.fetchKnownPosts.add(msgURI);
                        } else {
                            rv.totalNewMessages++;
                        }
                        newMsgFound = true;
                    }
                }
                // even if it is banned, PIR requires it to be fetched
                if (msg.getIsNew()) {
                    newPIRMsgFound = true;
                    rv.fetchPIRBytes += msg.getEntrySize();
                    if (msg.getIsReply())
                        rv.fetchPIRReplies.add(msgURI);
                    else
                        rv.fetchPIRPosts.add(msgURI);
                }
            }
            if (newMsgFound && (chanInfo != null))
                rv.totalKnownChannelsWithNewMessages++;
            if (newPIRMsgFound) {
                rv.fetchPIRMetadata.add(chanURI);
                rv.fetchPIRBytes += chan.getEntrySize();
            }
        }
        rv.maxSizeUsed = maxSize;
        return rv;
    }
    
    private void diffUnauth(DBClient client, UI ui, Opts opts, ArchiveChannel chan, List banned, ArchiveDiff rv) {
        //todo: the unauth diff logic is off, populating Diff in an odd way, since an
        //      index containing diffs will contain ONLY diffs
        // ?? is it still?
        for (int i = 0 ; i < chan.getUnauthorizedMessageCount(); i++) {
            ArchiveMessage msg = chan.getUnauthorizedMessage(i);
            Hash scope = msg.getPrimaryScope();
            if (banned.contains(scope)) {
                // banned author, but not banned target channel
                continue;
            }
            long localChanId = client.getChannelId(scope);
            if (localChanId >= 0) {
                long localInternalId = client.getMessageId(localChanId, msg.getMessageId());
                if (localInternalId >= 0) {
                    // the unauthorized post is already known
                    continue;
                } else {
                    // unauthorized post is not known
                }
            } else {
                // unauthorized post is by an unknown author, so try
                // to include the author's metadata in the to-fetch list
                SyndieURI scopeMeta = SyndieURI.createScope(scope);
                if (!rv.fetchNewUnauthorizedMetadata.contains(scopeMeta)) {
                    rv.fetchNewUnauthorizedMetadata.add(scopeMeta);
                    for (int j = 0; j < _channelEntries.size(); j++) {
                        ArchiveChannel curChan = (ArchiveChannel)_channelEntries.get(j);
                        if (curChan.getScope().equals(scope)) {
                            rv.fetchNewUnauthorizedBytes += curChan.getEntrySize();
                            break;
                        }
                    }
                }
            }
            
            Hash targetScope = new Hash(chan.getScope());
            long localTargetChanId = client.getChannelId(targetScope);
            if (localTargetChanId < 0) {
                // unauthorized post is targetting an unknown channel, so try
                // to include the target channel's metadata in the to-fetch list
                if (!rv.fetchNewUnauthorizedMetadata.contains(targetScope)) {
                    rv.fetchNewUnauthorizedMetadata.add(SyndieURI.createScope(targetScope));
                    for (int j = 0; j < _channelEntries.size(); j++) {
                        ArchiveChannel curChan = (ArchiveChannel)_channelEntries.get(j);
                        if (DataHelper.eq(curChan.getScope(),chan.getScope())) {
                            rv.fetchNewUnauthorizedBytes += curChan.getEntrySize();
                            break;
                        }
                    }
                }
            }
            
            rv.fetchNewUnauthorizedBytes += msg.getEntrySize();
            SyndieURI uri = SyndieURI.createMessage(scope, msg.getMessageId());
            if (msg.getIsReply())
                rv.fetchNewUnauthorizedReplies.add(uri);
            else
                rv.fetchNewUnauthorizedPosts.add(uri);
        }
    }
    
    public static void main(String args[]) {
        String path = "/home/jrandom/.syndie/archive/index-all.dat";
        if (args.length >= 1)
            path = args[0];
        boolean unauth = false;
        if ( (args.length >= 2) && ("unauth".equalsIgnoreCase(args[1])) )
            unauth = true;
        try {
            ArchiveIndex index = ArchiveIndex.loadIndex(new File(path), new TextUI(true), unauth);
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
}
