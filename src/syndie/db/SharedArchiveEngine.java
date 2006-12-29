package syndie.db;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
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
    public static class PullStrategy {
        public PullStrategy() {
            maxKBPerMessage = SharedArchive.DEFAULT_MAX_SIZE_KB;
            maxKBTotal = -1;
            knownChannelsOnly = false;
            includePrivateMessages = true;
            includePBEMessages = true;
            includeRecentMessagesOnly = true;
            includeDupForPIR = false;
            pullNothing = false;
            discoverArchives = true;
        }
        public PullStrategy(String serialized) {
            this();
            if (serialized != null) {
                includeDupForPIR = (serialized.indexOf("PIR") != -1);
                discoverArchives = (serialized.indexOf("DontDiscoverArchives") == -1);
                if (!includeDupForPIR) {
                    pullNothing = (serialized.indexOf("PullNothing") != -1);
                    includeRecentMessagesOnly = (serialized.indexOf("RecentMessagesOnly") != -1);
                    includePBEMessages = (serialized.indexOf("DontIncludePBE") == -1);
                    includePrivateMessages = (serialized.indexOf("DontIncludePrivate") == -1);
                    knownChannelsOnly = (serialized.indexOf("KnownChannelsOnly") != -1);
                    int maxPerIdx = serialized.indexOf("MaxPerMsg");
                    if (maxPerIdx >= 0) {
                        int end = serialized.indexOf(' ', maxPerIdx);
                        if (end > 0) {
                            try {
                                maxKBPerMessage = Integer.parseInt(serialized.substring(maxPerIdx + "MaxPerMsg".length(), end));
                            } catch (NumberFormatException nfe) {}
                        }
                    }
                    int maxTotIdx = serialized.indexOf("MaxTotal");
                    if (maxTotIdx >= 0) {
                        int end = serialized.indexOf(' ', maxTotIdx);
                        if (end > 0) {
                            try {
                                maxKBTotal = Integer.parseInt(serialized.substring(maxTotIdx + "MaxTotal".length(), end));
                            } catch (NumberFormatException nfe) {}
                        }
                    }
                }
            }
        }
        /**
         * if a message exceeds this size, do not pull it
         */
        public int maxKBPerMessage;
        /**
         * how much data we will pull across all of the messages (ignoring metadata size
         * and transmission overhead)
         */
        public int maxKBTotal;
        /** 
         * if true, only pull down messages and/or metadata for channels we already
         * know locally
         */
        public boolean knownChannelsOnly;
        /**
         * if true, pull down messages encrypted to forum owner(s) (even if that
         * isn't us)
         */
        public boolean includePrivateMessages;
        /**
         * if true, pull down messages that are encrypted with a passphrase 
         */
        public boolean includePBEMessages;
        /**
         * should we only fetch messages the archive advertizes as 'recent', even if they
         * have messages we would otherwise want but are flagged as 'old'?
         */
        public boolean includeRecentMessagesOnly;
        /**
         * if true, we want to use trivial single-database private information retrieval:
         * get everything the archive advertizes as "new", and their dependencies, even if
         * we already have some of them.  This way the archive can't tell what we have or
         * what we want (and hence will have a much harder time profiling the user).  If
         * this flag is set, all of the other flags in the strategy are ignored.
         */
        public boolean includeDupForPIR;
        
        /** noop strategy - dont pull anything */
        public boolean pullNothing;
        
        /** 
         * when we talk to an archive, they may tell us about other archives, and if this
         * flag is set, we will add those other archives to our list (though we will NOT
         * schedule them up for syndication, and WILL track what archive told us about it)
         */
        public boolean discoverArchives;
        
        public String toString() {
            StringBuffer buf = new StringBuffer();
            if (includeDupForPIR) {
                buf.append("PIR ");
            } else if (pullNothing) {
                buf.append("PullNothing ");
            } else {
                if (includeRecentMessagesOnly) buf.append("RecentMessagesOnly ");
                if (includePBEMessages)
                    buf.append("IncludePBE ");
                else
                    buf.append("DontIncludePBE ");
                if (includePrivateMessages)
                    buf.append("IncludePrivate ");
                else
                    buf.append("DontIncludePrivate ");
                if (knownChannelsOnly)
                    buf.append("KnownChannelsOnly ");
                else
                    buf.append("AllChannels ");
                if (maxKBPerMessage >= 0)
                    buf.append("MaxPerMsg").append(maxKBPerMessage).append(" ");
                if (maxKBTotal >= 0)
                    buf.append("MaxTotal").append(maxKBTotal).append(" ");
            }
            if (discoverArchives)
                buf.append("DiscoverArchives ");
            else
                buf.append("DontDiscoverArchives ");
            return buf.toString();
        }
        public String serialize() { return toString(); }
    }

    public static class PushStrategy {
        public PushStrategy() {
            maxKBPerMessage = SharedArchive.DEFAULT_MAX_SIZE_KB;
            maxKBTotal = -1;
            sendHashcashForLocal = false;
            sendHashcashForAll = false;
            sendLocalNewOnly = false;
            sendNothing = false;
        }
        public PushStrategy(String serialized) {
            this();
            if (serialized != null) {
                sendNothing = (serialized.indexOf("SendNothing") != -1);
                sendLocalNewOnly = !sendNothing && (serialized.indexOf("LocalNewOnly") != -1);
                sendHashcashForAll = !sendNothing && (serialized.indexOf("HCForAll") != -1);
                sendHashcashForLocal = !sendNothing && (serialized.indexOf("HCForLocal") != -1);
                
                int maxPerIdx = serialized.indexOf("MaxPerMsg");
                if (maxPerIdx >= 0) {
                    int end = serialized.indexOf(' ', maxPerIdx);
                    if (end > 0) {
                        try {
                            maxKBPerMessage = Integer.parseInt(serialized.substring(maxPerIdx + "MaxPerMsg".length(), end));
                        } catch (NumberFormatException nfe) {}
                    }
                }
                int maxTotIdx = serialized.indexOf("MaxTotal");
                if (maxTotIdx >= 0) {
                    int end = serialized.indexOf(' ', maxTotIdx);
                    if (end > 0) {
                        try {
                            maxKBTotal = Integer.parseInt(serialized.substring(maxTotIdx + "MaxTotal".length(), end));
                        } catch (NumberFormatException nfe) {}
                    }
                }
            }
        }
        /**
         * if a message exceeds this size, do not push it
         */
        public int maxKBPerMessage;
        /**
         * how much data we will push across all of the messages (ignoring metadata size
         * and transmission overhead)
         */
        public int maxKBTotal;
        /**
         * if true, generate hashcash certs for locally created messages that
         * the remote archive wants certs for
         */
        public boolean sendHashcashForLocal;
        /**
         * if true, generate hashcash certs for all messages that
         * the remote archive wants certs for
         */
        public boolean sendHashcashForAll;
        /**
         * only send messages we generated locally in the last few days that
         * the archive doesnt have yet.  this has obvious anonymity attributes
         */
        public boolean sendLocalNewOnly;
        /** noop strategy - dont send anything */
        public boolean sendNothing;
        
        public String toString() {
            StringBuffer buf = new StringBuffer();
            if (sendNothing) {
                buf.append("SendNothing ");
            } else {
                if (sendHashcashForLocal) buf.append("HCForLocal ");
                if (sendHashcashForAll) buf.append("HCForAll ");
                if (sendLocalNewOnly)
                    buf.append("LocalNewOnly ");
                else
                    buf.append("AllDiff ");
                if (maxKBPerMessage >= 0)
                        buf.append("MaxPerMsg").append(maxKBPerMessage).append(" ");
                if (maxKBTotal >= 0)
                    buf.append("MaxTotal").append(maxKBTotal).append(" ");
            }
            return buf.toString();
        }
        public String serialize() { return toString(); }
    }
    
    private static final long PERIOD_TOO_OLD = 7*24*60*60*1000;
    
    /** 
     * pick out what elements of the shared archive we want, according to the given
     * strategy.  the elements are ordered so as to include dependencies first, though
     * for parallel execution, all metadata URIs should be run prior to the messages.
     *
     */
    public List selectURIsToPull(DBClient client, UI ui, SharedArchive archive, PullStrategy strategy) {
        int totalAllocatedKB = 0;
        List uris = new ArrayList();
        SharedArchive.Channel channels[] = archive.getChannels();
        for (int i = 0; i < channels.length; i++) {
            Hash scope = new Hash(channels[i].getScope());
            if (channels[i].isNew() && strategy.includeDupForPIR) {
                uris.add(SyndieURI.createScope(scope));
            } else {
                long knownVersion = client.getChannelVersion(scope);
                if (strategy.knownChannelsOnly && (knownVersion < 0))
                    continue;
                if (channels[i].getVersion() > knownVersion) {
                    ui.debugMessage("shared archive has a newer version than we do for " + scope.toBase64() + " [them: " + channels[i].getVersion() + ", us: " + knownVersion + "]");
                    uris.add(SyndieURI.createScope(scope));
                } else {
                    // already known.  no need
                }
            }
        }
        SharedArchive.Message messages[] = archive.getMessages();
        for (int i = 0; i < messages.length; i++) {
            Hash scope = new Hash(channels[messages[i].getScopeIndex()].getScope());
            Hash target = new Hash(channels[messages[i].getTargetIndex()].getScope());
            if (messages[i].isNew() && strategy.includeDupForPIR) {
                SyndieURI scopeURI = SyndieURI.createScope(scope);
                SyndieURI targetURI = SyndieURI.createScope(target);
                if (!uris.contains(scopeURI))
                    uris.add(scopeURI);
                if (!uris.contains(targetURI))
                    uris.add(targetURI);
                uris.add(SyndieURI.createMessage(scope, messages[i].getMessageId()));
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
                if (!messages[i].isNew() && strategy.includeRecentMessagesOnly) {
                    //ui.debugMessage("message is old and we only want recent messages: " + messages[i]);
                    continue;
                }
                
                // already known
                if (!strategy.includeDupForPIR && (client.getMessageId(scope, messages[i].getMessageId()) >= 0)) {
                    //ui.debugMessage("message is already known: " + messages[i]);
                    continue;
                }
                
                long targetChanId = client.getChannelId(target);
                long scopeChanId = client.getChannelId(scope);
                if ( ( (scopeChanId < 0) || (targetChanId < 0) ) && (strategy.knownChannelsOnly) )
                    continue;
                
                if ( (strategy.maxKBTotal > 0) && (totalAllocatedKB + messages[i].getMaxSizeKB() > strategy.maxKBTotal) )
                    continue;
                
                SyndieURI scopeURI = SyndieURI.createScope(scope);
                SyndieURI targetURI = SyndieURI.createScope(target);
                if (!uris.contains(scopeURI) && (client.getChannelId(scope) < 0))
                    uris.add(scopeURI);
                if (!uris.contains(targetURI) && (client.getChannelId(target) < 0))
                    uris.add(targetURI);
                totalAllocatedKB += messages[i].getMaxSizeKB();
                ui.debugMessage("message meets our criteria: " + scope.toBase64() + ":" + messages[i]);
                uris.add(SyndieURI.createMessage(scope, messages[i].getMessageId()));
            }
        }
        ui.debugMessage("Pull: strategy=" + strategy + " Total allocated KB: " + totalAllocatedKB + " URIs: " + uris);
        return uris;
    }
    
    public List selectURIsToPush(DBClient client, UI ui, SharedArchive archive, PushStrategy strategy) {
        /** SyndieURI of a message to the SyndieURI of a scope it depends on */
        Map dependencies = new HashMap();
        List rv = new ArrayList();
        if (strategy.sendLocalNewOnly) // local new == messages in our ./outbound/*/ directories
            scheduleNew(client, ui, archive, rv, dependencies, client.getOutboundDir(), strategy);
        else // otherwise, push new (etc) from our ./archive/*/ directories
            scheduleNew(client, ui, archive, rv, dependencies, client.getArchiveDir(), strategy);
        resolveDependencies(client, ui, archive, rv, dependencies);
        
        ui.debugMessage("Push: strategy=" + strategy + " URIs: " + rv);
        return rv;
    }
    
    private void resolveDependencies(DBClient client, UI ui, SharedArchive archive, List rv, Map dependencies) {
        for (Iterator iter = dependencies.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry)iter.next();
            SyndieURI msgURI = (SyndieURI)entry.getKey();
            SyndieURI chanURI = (SyndieURI)entry.getValue();
            if (rv.contains(chanURI))
                continue; // ok, already scheduled
            
            if (archive.getChannel(chanURI.getScope()) == null)
                continue; // ok, they already have it (or at least some version of it)
            
            if (archive.getAbout().wantKnownChannelsOnly()) {
                // boo.  dependency failed because they are no fun.
                //ui.debugMessage("not sending " + msgURI.toString() + " because it depends on " + chanURI.toString() + ", which they don't know, and they don't want new channels");
                rv.remove(msgURI);
                continue;
            }
            
            File meta = new File(new File(client.getArchiveDir(), chanURI.getScope().toBase64()), "meta" + Constants.FILENAME_SUFFIX);
            if (meta.exists()) {
                rv.add(chanURI);
            } else {
                // dependency failed because we don't keep full archives
                //ui.debugMessage("not sending " + msgURI.toString() + " because it depends on " + chanURI.toString() + ", which they don't know, and we don't have that channel's signed metadata anymore");
                rv.remove(msgURI);
                continue;
            }
        }
    }
    
    private void scheduleNew(DBClient client, UI ui, SharedArchive archive, List rv, Map dependencies, File dir, PushStrategy strategy) {
        long totalKB = 0;
        File dirs[] = dir.listFiles(new FileFilter() {
            public boolean accept(File pathname) {
                return pathname.isDirectory() && !pathname.getName().startsWith(".");
            }
        });
        
        SharedArchiveBuilder.sortFiles(dirs);
        
        for (int i = 0; i < dirs.length; i++) {
            Hash scope = new Hash(Base64.decode(dirs[i].getName()));
            long version = client.getChannelVersion(scope);
            //ui.debugMessage("Scheduling push from " + scope.toBase64());

            SharedArchive.Channel remChan = archive.getChannel(scope);
            if (archive.getAbout().wantKnownChannelsOnly() && (remChan == null)) {
                ui.debugMessage("Remote archive doesn't know " + scope.toBase64().substring(0,6) + " and doesn't want any new chans.  skipping");
                continue;
            }

            boolean sendMeta = false;
            if ( (remChan == null) || (remChan.getVersion() < version) )
                sendMeta = true;

            SyndieURI metaURI = SyndieURI.createScope(scope);
            File metaFile = new File(dirs[i], "meta" + Constants.FILENAME_SUFFIX);
            if (sendMeta) {
                if (metaFile.exists()) {
                    ui.debugMessage("sending metadata for " + scope.toBase64() + " (our version: " + version + " theirs: " + (remChan == null ? -1 : remChan.getVersion()) + ")");
                    rv.add(metaURI);
                } else {
                    ui.debugMessage("we want to send them the metadata for " + scope.toBase64() + ", but don't have it anymore");
                    continue;
                }
            }

            File files[] = dirs[i].listFiles(new FileFilter() {
                public boolean accept(File pathname) {
                    String name = pathname.getName();
                    return (name.endsWith(Constants.FILENAME_SUFFIX) && !name.startsWith("meta"));
                }
            });

            SharedArchiveBuilder.sortFiles(files);
            
            boolean added = false;
            for (int j = 0; j < files.length; j++) {
                long messageId = SharedArchiveBuilder.getMessageId(files[j]);
                if (messageId < 0) {                
                    ui.debugMessage("File is not relevent for a message: " + files[j].getName());
                    continue;
                }
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

                long importDate = client.getMessageImportDate(msgId);
                if (archive.getAbout().wantRecentOnly()) {
                    if (importDate + SharedArchiveBuilder.PERIOD_NEW < System.currentTimeMillis()) {
                        ui.debugMessage("Don't send them " + messageId + " because they only want recent messages");
                        continue;
                    }
                }
                
                if (importDate + PERIOD_TOO_OLD < System.currentTimeMillis()) {
                    ui.debugMessage("Don't send them " + messageId + " because it is just too old, and if they wanted it, they'd have it already");
                    continue;
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
                ui.debugMessage("scheduling " + msgURI + ": size=" + lenKB + " privacy=" + privacy + " age=" + DataHelper.formatDuration(System.currentTimeMillis()-importDate));

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
