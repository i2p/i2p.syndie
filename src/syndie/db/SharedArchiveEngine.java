package syndie.db;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import net.i2p.data.Base64;
import net.i2p.data.DataFormatException;
import net.i2p.data.DataHelper;
import net.i2p.data.Hash;

import syndie.Constants;
import syndie.data.SyndieURI;

/**
 * coordinate the pull and push from a particular shared archive
 */
public class SharedArchiveEngine {

    //private static final long PERIOD_TOO_OLD = 7*24*60*60*1000;
    
    /** 
     * pick out what elements of the shared archive we want, according to the given
     * strategy.  the elements are ordered so as to include dependencies first, though
     * for parallel execution, all metadata URIs should be run prior to the messages.
     *
     */
    public List<SyndieURI> selectURIsToPull(DBClient client, UI ui, SharedArchive archive, PullStrategy strategy, long whitelistGroupId) {
        int totalAllocatedKB = 0;
        Set<SyndieURI> scopes = new HashSet();
        Set<SyndieURI> msgs = new TreeSet(new MsgURIComparator());
        if (strategy.pullNothing)
            return Collections.EMPTY_LIST;
        long begin = System.currentTimeMillis();
        
        List banned = client.getBannedChannels();
        
        SharedArchive.Channel channels[] = archive.getChannels();
        for (int i = 0; i < channels.length; i++) {
            Hash scope = Hash.create(channels[i].getScope());
            if (!channels[i].wantNewMsg() && !channels[i].wantNewMeta() && (channels[i].getVersion() == 0) ) {
                // the remote side has banned it, so they won't be able to give it to us, obviously
            } else if (channels[i].isNew() && strategy.includeDupForPIR) {
                scopes.add(SyndieURI.createScope(scope));
            } else {
                if (banned.contains(scope))
                    continue;
                long knownVersion = client.getChannelVersion(scope);
                if (strategy.knownChannelsOnly && (knownVersion < 0))
                    continue;
                if (strategy.requiredChannelsOnly && knownVersion < 0) {
                    // Don't fetch all channels. If we find a message for the scope below,
                    // we will add the scope then.
                    // In effect we won't fetch empty channels.
                    continue;
                }
                if (channels[i].getVersion() > knownVersion) {
                    ui.debugMessage("shared archive has a newer version than we do for " + scope.toBase64() + " [them: " + channels[i].getVersion() + ", us: " + knownVersion + "]");
                    scopes.add(SyndieURI.createScope(scope));
                } else {
                    // already known.  no need
                }
            }
        }
        
        Set whitelistScopes = client.getReferencedScopes(whitelistGroupId);
        
        SharedArchive.Message messages[] = archive.getMessages();
        for (int i = 0; i < messages.length; i++) {
            Hash scope = Hash.create(channels[messages[i].getScopeIndex()].getScope());
            Hash target = Hash.create(channels[messages[i].getTargetIndex()].getScope());
            if (messages[i].isNew() && strategy.includeDupForPIR) {
                SyndieURI scopeURI = SyndieURI.createScope(scope);
                SyndieURI targetURI = SyndieURI.createScope(target);
                scopes.add(scopeURI);
                scopes.add(targetURI);
                msgs.add(SyndieURI.createMessage(scope, messages[i].getMessageId()));
                totalAllocatedKB += messages[i].getMaxSizeKB();
            } else {
                if ( (strategy.maxKBPerMessage > 0) && (messages[i].getMaxSizeKB() > strategy.maxKBPerMessage) ) {
                    //ui.debugMessage("message size exceeds strategy max (" + strategy.maxKBPerMessage + "): " + messages[i].getMaxSizeKB() + ": " + messages[i].toString());
                    continue;
                }
                if (messages[i].isPBE() && !strategy.includePBEMessages)
                    continue;
                if (messages[i].isPrivate() && !strategy.includePrivateMessages)
                    continue;

                if (strategy.includeRecentMessagesOnly) {
                    // if the server hasn't marked it as 'new' and the author gave it a date earlier 
                    // than what we want, skip
                    if (!messages[i].isNew() && 
                        (messages[i].getMessageId() < System.currentTimeMillis()-strategy.newAgeDays*24L*60*60*1000L))
                        continue;
                }
                
                // already known
                if (!strategy.includeDupForPIR && (client.getMessageId(scope, messages[i].getMessageId()) >= 0)) {
                    //ui.debugMessage("message is already known: " + messages[i]);
                    continue;
                }
                
                if (banned.contains(scope) || banned.contains(target))
                    continue;
                
                if (strategy.pullWhitelistOnly) {
                    if (whitelistScopes.contains(target) || whitelistScopes.contains(scope)) {
                        // ok, passes the whitelist
                    } else {
                        continue;
                    }
                }
                        
                long targetChanId = client.getChannelId(target);
                long scopeChanId = client.getChannelId(scope);
                if ( ( (scopeChanId < 0) || (targetChanId < 0) ) && (strategy.knownChannelsOnly) )
                    continue;
                
                if ( (strategy.maxKBTotal > 0) && (totalAllocatedKB + messages[i].getMaxSizeKB() > strategy.maxKBTotal) )
                    continue;
                
                SyndieURI scopeURI = SyndieURI.createScope(scope);
                SyndieURI targetURI = SyndieURI.createScope(target);
                if (!scopes.contains(scopeURI) && (client.getChannelId(scope) < 0))
                    scopes.add(scopeURI);
                if (!scopes.contains(targetURI) && (client.getChannelId(target) < 0))
                    scopes.add(targetURI);
                totalAllocatedKB += messages[i].getMaxSizeKB();
                ui.debugMessage("message meets our criteria: " + scope.toBase64() + ":" + messages[i]);
                msgs.add(SyndieURI.createMessage(scope, messages[i].getMessageId()));
            }
        }
        List<SyndieURI> uris = new ArrayList(scopes.size() + msgs.size());
        uris.addAll(scopes);
        uris.addAll(msgs);
        ui.debugMessage("Selected to Pull: strategy=" + strategy + " Total allocated KB: " + totalAllocatedKB +
                        " URIs: " + uris.size() +
                        " total time = " + (System.currentTimeMillis() - begin));
        // makes debugging harder and the UI messier
        //Collections.shuffle(uris);
        return uris;
    }
    
    private static class MsgURIComparator implements Comparator<SyndieURI> {
        public int compare(SyndieURI l, SyndieURI r) {
            Long lid = l.getMessageId();
            Long rid = r.getMessageId();
            if (lid != null && rid != null) {
                // reverse
                int rv = rid.compareTo(lid);
                if (rv != 0)
                    return rv;
                int lhc = lid.hashCode();
                int rhc = rid.hashCode();
                if (lhc < rhc)
                    return -1;
                if (lhc > rhc)
                    return 1;
            }
            return 0;
        }
    }

    public List<SyndieURI> selectURIsToPush(DBClient client, UI ui, SharedArchive archive, PushStrategy strategy) {
        List<SyndieURI> rv = new ArrayList();
        if (strategy.sendNothing)
            return rv;
        long begin = System.currentTimeMillis();
        
        /** SyndieURI of a message to the SyndieURI of a scope it depends on */
        Map dependencies = new HashMap();
        if (strategy.sendLocalNewOnly) // local new == messages in our ./outbound/*/ directories
            scheduleNew(client, ui, archive, rv, dependencies, client.getOutboundDir(), strategy);
        else // otherwise, push new (etc) from our ./archive/*/ directories
            scheduleNew(client, ui, archive, rv, dependencies, client.getArchiveDir(), strategy);
        resolveDependencies(client, ui, archive, rv, dependencies);
        
        ui.debugMessage("Selected to Push: strategy=" + strategy + " URIs: " + rv.size() +
                        " total time = " + (System.currentTimeMillis() - begin));
        // makes debugging harder and the UI messier
        //Collections.shuffle(rv);
        return rv;
    }
    
    private void resolveDependencies(DBClient client, UI ui, SharedArchive archive,
                                     List<SyndieURI> rv, Map<SyndieURI, SyndieURI> dependencies) {
        for (Iterator iter = dependencies.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry)iter.next();
            SyndieURI msgURI = (SyndieURI)entry.getKey();
            SyndieURI chanURI = (SyndieURI)entry.getValue();
            // FIXME O(n**2)
            if (rv.contains(chanURI))
                continue; // ok, already scheduled
            
            // FIXME O(n**2)
            if (archive.getChannel(chanURI.getScope()) != null)
                continue; // ok, they already have it (or at least some version of it)

            if (archive.getAbout().wantKnownChannelsOnly()) {
                // boo.  dependency failed because they are no fun.
                //ui.debugMessage("not sending " + msgURI.toString() + " because it depends on " + chanURI.toString() + ", which they don't know, and they don't want new channels");
                rv.remove(msgURI);
                continue;
            }
            
            File meta = new File(new File(client.getArchiveDir(), chanURI.getScope().toBase64()), "meta" + Constants.FILENAME_SUFFIX);
            if (meta.exists()) {
                ui.debugMessage("Adding metadata for " + chanURI.getScope().toBase64() + " as dependency");
                rv.add(chanURI);
            } else {
                // dependency failed because we don't keep full archives
                ui.debugMessage("not sending " + msgURI.toString() + " because it depends on " + chanURI.toString() + ", which they don't know, and we don't have that channel's signed metadata anymore");
                rv.remove(msgURI);
            }
        }
    }
    
    /**
     *  What to push?
     */
    private void scheduleNew(DBClient client, UI ui, SharedArchive archive,
                             List<SyndieURI> rv, Map<SyndieURI, SyndieURI> dependencies, File dir, PushStrategy strategy) {
        long totalKB = 0;
        File dirs[] = dir.listFiles(new FileFilter() {
            public boolean accept(File pathname) {
                return pathname.isDirectory() && !pathname.getName().startsWith(".");
            }
        });
        if (dirs == null)
            return;
        
        // rather than sort to hide OS type, we shuffle later
        // no dont shuffle
        SharedArchiveBuilder.sortFiles(dirs);
        
        for (int i = 0; i < dirs.length; i++) {
            Hash scope = Hash.create(Base64.decode(dirs[i].getName()));
            long version = client.getChannelVersion(scope);
            //ui.debugMessage("Scheduling push from " + scope.toBase64());

            // FIXME O(n**2)
            SharedArchive.Channel remChan = archive.getChannel(scope);
            if (archive.getAbout().wantKnownChannelsOnly() && (remChan == null)) {
                ui.debugMessage("Remote archive doesn't know " + scope.toBase64().substring(0,6) + " and doesn't want any new chans.  skipping");
                continue;
            }

            boolean refuseMeta = (remChan != null) && (!remChan.wantNewMeta());
            boolean refuseMsg = (remChan != null) && (!remChan.wantNewMsg());
            
            boolean sendMeta = false;
            if ( (remChan == null) || (remChan.getVersion() < version) )
                sendMeta = true;

            SyndieURI metaURI = SyndieURI.createScope(scope);
            File metaFile = new File(dirs[i], "meta" + Constants.FILENAME_SUFFIX);
            if (sendMeta && !refuseMeta) {
                if (metaFile.exists()) {
                    // we send meta if we have newer, regardless of how old it is
                    // (there's no push policy for metadata age)
                    ui.debugMessage("sending metadata for " + scope.toBase64() + " (our version: " + version + " theirs: " + (remChan == null ? -1 : remChan.getVersion()) + ")");
                    rv.add(metaURI);
                } else {
                    ui.debugMessage("we want to send them the metadata for " + scope.toBase64() + ", but don't have it anymore");
                    continue;
                }
            }
            
            if (refuseMsg)
                continue;

            File files[] = dirs[i].listFiles(new FileFilter() {
                public boolean accept(File pathname) {
                    String name = pathname.getName();
                    return (name.endsWith(Constants.FILENAME_SUFFIX) && !name.startsWith("meta"));
                }
            });
            if (files == null)
                continue;

            // rather than sort to hide OS type, we shuffle later
            // no dont shuffle
            SharedArchiveBuilder.sortFiles(files);
            
            boolean added = false;
            for (int j = 0; j < files.length; j++) {
                long messageId = SharedArchiveBuilder.getMessageId(files[j]);
                if (messageId < 0) {                
                    ui.debugMessage("File is not relevant for a message: " + files[j].getName());
                    continue;
                }
                // FIXME O(n**2)
                boolean known = archive.isKnown(scope, messageId);
                if (known)
                    continue;

                long lenKB = (files[j].length()+1023)/1024;
                if (lenKB > archive.getAbout().maxMessageSize()) {
                    ui.debugMessage("Don't send them " + messageId + " because it is too large for them to receive (" + lenKB + "KB vs " + archive.getAbout().maxMessageSize() + ")");
                    continue;
                }
                if ( (strategy.maxKBPerMessage > 0) && (lenKB > strategy.maxKBPerMessage) ) {
                    ui.debugMessage("Don't send them " + messageId + " because it is too large for us to send (" + lenKB + "KB)");
                    continue;
                }

                // the internal ID
                long msgId = client.getMessageId(scope, messageId);
                int privacy = client.getMessagePrivacy(msgId);
                if (!archive.getAbout().wantPBE() && (privacy == DBClient.PRIVACY_PBE)) {
                    ui.debugMessage("Don't send them " + messageId + " because it they don't want PBE'd messages");
                    continue;
                }
                if (!archive.getAbout().wantPrivate() && (privacy == DBClient.PRIVACY_PRIVREPLY)) {
                    ui.debugMessage("Don't send them " + messageId + " because it they don't want private reply messages");
                    continue;
                }

                // No, let's not use the importDate, or a new user spends forever pushing
                // stuff he just imported. Let's not use our upstream bandwidth pushing ancient stuff.
                // If the other side wants it, they can pull it from some other archive.
                // But use the min in case they spoofed it to the future.
                // TODO maybe add an option?
                long importDate = client.getMessageImportDate(msgId);
                long msgDate = messageId;
                if (importDate > 0 && importDate < msgDate)
                    msgDate = importDate;
                if (archive.getAbout().wantRecentOnly()) {
                    if (msgDate + SharedArchiveBuilder.PERIOD_NEW < System.currentTimeMillis()) {
                        ui.debugMessage("Don't send them " + messageId + " because they only want recent messages");
                        continue;
                    }
                }
                
                if (strategy.sendMaxAge > 0) {
                    if (msgDate + 24*60*60*1000L*strategy.sendMaxAge < System.currentTimeMillis()) {
                        ui.debugMessage("Don't send them " + messageId + " because it is too old; import date " + importDate);
                        continue;
                    }
                }

                if ( (strategy.maxKBTotal > 0) && (lenKB + totalKB > strategy.maxKBTotal)) {
                    ui.debugMessage("Don't send them " + messageId + " because the total exceeds what they want");
                    continue;
                }

                long authorId = client.getMessageAuthor(msgId);
                long targetId = client.getMessageTarget(msgId);
                long scopeId = client.getChannelId(scope);

                totalKB += lenKB;
                SyndieURI msgURI = SyndieURI.createMessage(scope, messageId);
                rv.add(msgURI);
                ui.debugMessage("scheduling " + msgURI + ": size=" + lenKB + " privacy=" + privacy + " age=" + DataHelper.formatDuration(System.currentTimeMillis()-msgDate));

                if ( (targetId >= 0) && (scopeId != targetId) ) {
                    Hash target = client.getChannelHash(targetId);
                    SyndieURI uri = SyndieURI.createScope(target);
                    dependencies.put(msgURI, uri);
                } else if ( (authorId >= 0) && (scopeId != authorId) ) {
                    Hash author = client.getChannelHash(authorId);
                    SyndieURI uri = SyndieURI.createScope(author);
                    dependencies.put(msgURI, uri);
                }
                added = true;
            }
            if (!added) {
                if ( (remChan != null) && (remChan.getVersion() > version) ) {
                    // ok, they want this new version
                } else if (dependencies.containsValue(metaURI)) {
                    //ui.debugMessage("All of the messages in " + scope.toBase64() + " were rejected, but someone else depends on it, so send it");
                } else {
                    ui.debugMessage("All of the messages in " + scope.toBase64() + " were rejected, and no one else depends on it, so don't send it");
                    //ui.debugMessage("All of the messages in " + scope.toBase64() + " were rejected, so we don't need to send them the metadata");
                    rv.remove(metaURI);
                }
            }
        }
    }
}
