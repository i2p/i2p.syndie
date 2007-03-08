package syndie.db;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import net.i2p.data.Base64;
import net.i2p.data.DataHelper;
import net.i2p.data.Hash;
import syndie.Constants;
import syndie.data.Enclosure;

/**
 * load the local archive (disk and database) into a SharedArchive
 */
public class SharedArchiveBuilder {
    private DBClient _client;
    private UI _ui;
    private int _shareDelayHours;
    /** if a message was created locally, don't advertise it as being here for at least this long */
    private int _hideLocalHours;
    private boolean _shareBanned;
    private boolean _shareReceivedOnly;
    private SharedArchive.About _about;
    
    public SharedArchiveBuilder(DBClient client, UI ui, SharedArchive.About about) {
        _client = client;
        _ui = ui;
        _about = about;
        
        setHideLocalHours(1); // don't advertise things we created locally until at least an hour has passed
        setShareBanned(true); // just because we have banned something doesn't mean other people need to know that
        setShareDelayHours(_about.getPublishRebuildFrequencyHours());
        setShareReceivedOnly(false);
    }
    
    /** if it arrived in the last 3 days, its "new" */
    static final long PERIOD_NEW = 3*24*60*60*1000L;
    
    public void setShareDelayHours(int numHours) { _shareDelayHours = numHours; }
    public void setShareBanned(boolean share) { _shareBanned = share; }
    public void setShareReceivedOnly(boolean receivedOnly) { _shareReceivedOnly = receivedOnly; }
    public void setHideLocalHours(int numHours) { _hideLocalHours = numHours; }
    
    public SharedArchive buildSharedArchive() {
        _ui.statusMessage("Building shared archive rooted out of " + _client.getArchiveDir().getPath());
        SharedArchive archive = new SharedArchive();
        // get all updated channel metadata
        List channels = getRecentChannels();
        // get all matching messages
        List messages = getRecentMessages(channels);
        
        SharedArchive.About about = buildAbout();
        archive.setChannels(channels);
        archive.setMessages(messages);
        archive.setAbout(about);
        //_ui.debugMessage("no_admin_chan = " + SharedArchive.ABOUT_NO_ADMIN_CHANNEL);
        _ui.debugMessage("shared archive message count = " + messages.size() + " channel count = " + channels.size());
        
        return archive;
    }
    
    private SharedArchive.About buildAbout() {
        /*
        SharedArchive.About rv = new SharedArchive.About();
        rv.setAdminChannel(SharedArchive.ABOUT_NO_ADMIN_CHANNEL);
        rv.setAlternativeArchives(_about.getA);
        rv.setPublishRebuildFrequencyHours(_shareDelayHours);
        rv.setMaxMessageSize(4096); // 4MB messages!  craziness.
        rv.setMinMessageSizeKBRequiringHashcash(512); 
        rv.setPostingRequiresPassphrase(false); // we're nice
        rv.setWantKnownChannelsOnly(false); // give us new stuff too
        rv.setWantPBE(true); // we may not be able to read it, but someone pulling from us may
        rv.setWantPrivate(true); // ditto
        rv.setWantRecentOnly(true); // only things posted in the last 3 days
        return rv;
         */
        return _about;
    }
    
    /**
     * compare two filenames and order them numerically and then alphabetically -
     * eg: "1", "2.foo", "3.bar", "11.baz", "1003.asdf", "0100000.boing", "a", "b", "c"
     */
    private static final Comparator FILENAME_COMPARATOR = new Comparator() {
        public int compare(Object o1, Object o2) {
            String lhs = o1.toString();
            String rhs = o2.toString();
            
            long lhsNum = -1;
            if (Character.isDigit(lhs.charAt(0))) {
                int split = lhs.indexOf('.');
                if (split > 0) {
                    try {
                        lhsNum = Long.parseLong(lhs.substring(0, split));
                    } catch (NumberFormatException nfe) {}
                } else {
                    try {
                        lhsNum = Long.parseLong(lhs);
                    } catch (NumberFormatException nfe) {}
                }
            }
            
            long rhsNum = -1;
            if (Character.isDigit(rhs.charAt(0))) {
                int split = rhs.indexOf('.');
                if (split > 0) {
                    try {
                        rhsNum = Long.parseLong(rhs.substring(0, split));
                    } catch (NumberFormatException nfe) {}
                } else {
                    try {
                        rhsNum = Long.parseLong(rhs);
                    } catch (NumberFormatException nfe) {}
                }
            }
            
            int rv = 0;
            if (lhsNum >= 0) {
                if (rhsNum < 0)
                    return -1;
                if (lhsNum < rhsNum)
                    rv = -1;
                else if (lhsNum == rhsNum)
                    rv = lhs.compareTo(rhs);
                else
                    rv = 1;
            } else if (rhsNum >= 0) {
                rv = 1;
            } else {
                rv = lhs.compareTo(rhs);
            }
            
            //System.out.println("comparing [" + lhs + "/" + rhs + "]: " + rv + " ("+ lhsNum + "/" + rhsNum + ")");
            
            return rv;
        }
    };

    /**
     * sort them alphabetically, so any os-dependence on file.listFiles() is avoided 
     * (listFiles has no specified order)
     */
    static void sortFiles(File files[]) {        
        TreeMap sorted = new TreeMap(FILENAME_COMPARATOR);
        for (int i = 0; i < files.length; i++)
            sorted.put(files[i].getName(), files[i]);
        int i = 0;
        for (Iterator iter = sorted.values().iterator(); iter.hasNext(); i++)
            files[i] = (File)iter.next();
    }
    
    /**
     * return a list of SharedArchive.Message instances, including the index into the channels list.
     * the channels list itself will be updated to contain any additional channels not already in there
     */
    private List getRecentMessages(List channels) {
        List rv = new ArrayList();
        File dirs[] = _client.getArchiveDir().listFiles(new FileFilter() {
            public boolean accept(File pathname) {
                return (pathname.isDirectory() && !pathname.getName().startsWith("."));
            }
        });
        sortFiles(dirs);
        for (int i = 0; i < dirs.length; i++) {
            Hash scope = new Hash(Base64.decode(dirs[i].getName()));
            
            int scopeChannel = getChannelIndex(channels, scope);
            if (scopeChannel < 0) {
                _ui.debugMessage("Could not share the metadata for " + scope.toBase64() + ", so we cannot share any of its messages");
                continue;
            }
            File msgFiles[] = dirs[i].listFiles(new FileFilter() {
                public boolean accept(File pathname) {
                    String name = pathname.getName();
                    return name.endsWith(Constants.FILENAME_SUFFIX) && !name.startsWith("meta") && !name.startsWith(".");
                }
            });
            //_ui.debugMessage("# message files in " + scope.toBase64() + ": " + msgFiles.length);
            sortFiles(msgFiles);
            for (int j = 0; j < msgFiles.length; j++) {
                long messageId = getMessageId(msgFiles[j]);
                if (messageId < 0) {
                    _ui.errorMessage("Error parsing messageId out of " + msgFiles[j].getPath());
                    continue;
                }
                
                //_ui.debugMessage("parsing message " + messageId + " in " + scope.toBase64());
                
                FileInputStream fin = null;
                try {
                    fin = new FileInputStream(msgFiles[j]);
                    Enclosure enc = new Enclosure(fin);
                    
                    long size = -1;
                    boolean isPBE = false;
                    boolean isPublic = false;
                    boolean isNew = false;
                    boolean isPrivate = false;
                    int targetChannel = -1;
                    
                    byte target[] = enc.getHeaderBytes(Constants.MSG_HEADER_TARGET_CHANNEL);
                    if ( (target != null) && (target.length == Hash.HASH_LENGTH) ) {
                        targetChannel = getChannelIndex(channels, new Hash(target));
                        if (targetChannel == -1) {
                            _ui.debugMessage("cannot include message " + messageId + " because it depends on a channel we can't share: " + Base64.encode(target));
                            continue;
                        }
                    } else {
                        targetChannel = scopeChannel;
                    }
                    
                    long when = _client.getMessageImportDate(scope, messageId);
                    
                    if (when + PERIOD_NEW >= System.currentTimeMillis())
                        isNew = true;
                    
                    if (isLocal(scope, messageId) && (when + _hideLocalHours*60*1000L >= System.currentTimeMillis())) {
                        //_ui.debugMessage("Message is in a local channel and was imported very recently, not sharing it: " + scope.toBase64() + ":" + messageId);
                        continue;
                    }
                    
                    //if (!isNew)
                    //    continue;
                    
                    isPBE = null != enc.getHeaderString(Constants.MSG_HEADER_PBE_PROMPT);
                    isPublic = null != enc.getHeaderString(Constants.MSG_HEADER_BODYKEY);
                    
                    size = msgFiles[j].length();
                    String type = enc.getHeaderString(Constants.MSG_HEADER_TYPE);
                    if (Constants.MSG_TYPE_POST.equals(type))
                        isPrivate = false;
                    else if (Constants.MSG_TYPE_REPLY.equals(type))
                        isPrivate = true;
                    else {
                        _ui.debugMessage("message " + messageId + " in " + scope.toBase64() + ": invalid type: " + type);
                        continue;
                    }
                    
                    SharedArchive.Message msg = new SharedArchive.Message();
                    msg.setMessageId(messageId);
                    msg.setScope(scopeChannel);
                    msg.setTarget(targetChannel);
                    msg.setSize(size);
                    msg.setIsNew(isNew);
                    msg.setIsPBE(isPBE);
                    msg.setIsPrivate(isPrivate);
                    msg.setIsPublic(isPublic);
                    rv.add(msg);
                } catch (IOException ioe) {
                    _ui.errorMessage("Error parsing message file " + msgFiles[j].getPath(), ioe);
                } finally {
                    if (fin != null) try { fin.close(); } catch (IOException ioe) {}
                }
            }
        }
        return rv;
    }

    /**
     * true if the message was created locally
     */
    private boolean isLocal(Hash scope, long messageId) {
        File msg = new File(new File(_client.getOutboundDir(), scope.toBase64()), messageId + Constants.FILENAME_SUFFIX);
        return msg.exists();
    }
    
    static long getMessageId(File file) { return getMessageId(file.getName()); }
    static long getMessageId(String filename) {
        try {
            String idStr = filename;
            idStr = idStr.substring(0, idStr.length() - Constants.FILENAME_SUFFIX.length());
            return Long.parseLong(idStr);
        } catch (NumberFormatException nfe) {
            return -1;
        }
    }
    
    /**
     * get the index in the channels list for the Channel corresponding to the given scope,
     * adding a new one if necessary and possible, or returning -1 if not possible.
     */
    private int getChannelIndex(List channels, Hash scope) {
        for (int i = 0; i < channels.size(); i++) {
            SharedArchive.Channel chan = (SharedArchive.Channel)channels.get(i);
            if (DataHelper.eq(scope.getData(), chan.getScope()))
                return i;
        }
        // not found.  add if possible
        File metaFile = new File(new File(_client.getArchiveDir(), scope.toBase64()), "meta" + Constants.FILENAME_SUFFIX);
        if (!metaFile.exists())
            return -1;
        
        long version = _client.getChannelVersion(scope);
        boolean isNew = false; // if it isn't in the list already, its not new
        boolean isPBE = false;
        boolean isPublic = false;

        FileInputStream fin = null;
        try {
            fin = new FileInputStream(metaFile);
            Enclosure enc = new Enclosure(fin);

            isPBE = (null != enc.getHeaderString(Constants.MSG_HEADER_PBE_PROMPT));
            isPublic = (null != enc.getHeaderString(Constants.MSG_HEADER_BODYKEY));

            fin.close();
            fin = null;

            SharedArchive.Channel chan = new SharedArchive.Channel();
            chan.setScope(scope);
            chan.setVersion(version);
            chan.setIsNew(isNew);
            chan.setIsPBE(isPBE);
            chan.setIsPublic(isPublic);
            chan.setWantNewMeta(true); // todo: make this configurable
            chan.setWantNewMsgs(true); // todo: make this configurable

            channels.add(chan);
            return channels.size()-1;
        } catch (IOException ioe) {
            _ui.errorMessage("Error getting the channel metadata from " + metaFile.getPath(), ioe);
            return -1;
        } finally {
            if (fin != null) try { fin.close(); } catch (IOException ioe) {}
        }
    }
    
    /** list of SharedArchive.Channel of those whose version has recently been updated */
    private List getRecentChannels() {
        List rv = new ArrayList();
        File dirs[] = _client.getArchiveDir().listFiles(new FileFilter() {
            public boolean accept(File pathname) {
                return (pathname.isDirectory() && !pathname.getName().startsWith("."));
            }
        });
        sortFiles(dirs);
        for (int i = 0; i < dirs.length; i++) {
            File metaFile = new File(dirs[i], "meta" + Constants.FILENAME_SUFFIX);
            if (!metaFile.exists())
                continue;
            Hash scope = new Hash(Base64.decode(dirs[i].getName()));
            long version = _client.getChannelVersion(scope);
            boolean isNew = false;
            boolean isPBE = false;
            boolean isPublic = false;
            
            FileInputStream fin = null;
            try {
                fin = new FileInputStream(metaFile);
                Enclosure enc = new Enclosure(fin);
                
                isPBE = (null != enc.getHeaderString(Constants.MSG_HEADER_PBE_PROMPT));
                isPublic = (null != enc.getHeaderString(Constants.MSG_HEADER_BODYKEY));
                
                long when = _client.getChannelImportDate(scope);
                if (when + PERIOD_NEW < System.currentTimeMillis())
                    isNew = false;
                else
                    isNew = true;
                
                fin.close();
                fin = null;
                
                if (!isNew)
                    continue;
                
                // todo: keep track of old channel metadata messages that are managed
                // locally, so we can update that metadata and still be able to serve up
                // the old version during the _hideLocalHours period.
            
                SharedArchive.Channel chan = new SharedArchive.Channel();
                chan.setScope(scope);
                chan.setVersion(version);
                chan.setIsNew(isNew);
                chan.setIsPBE(isPBE);
                chan.setIsPublic(isPublic);
                chan.setWantNewMeta(true); // todo: make this configurable
                chan.setWantNewMsgs(true); // todo: make this configurable

                rv.add(chan);
            } catch (IOException ioe) {
                _ui.errorMessage("Error checking " + metaFile.getPath(), ioe);
            } finally {
                if (fin != null) try { fin.close(); } catch (IOException ioe) {}
            }
        }
        addBannedChannels(rv);
        return rv;
    }
       
    /**
     * add the SharedArchive.Channel instance relating to (recently?) banned channels so 
     * that people don't send it to us 
     */
    private void addBannedChannels(List channels) {
        List scopes = _client.getBannedChannels(false); // should this only list recent ones?
        for (int i = 0; i < scopes.size(); i++) {
            Hash scope = (Hash)scopes.get(i);
            SharedArchive.Channel chan = new SharedArchive.Channel();
            chan.setScope(scope);
            chan.setVersion(0);
            chan.setIsNew(false);
            chan.setIsPBE(false);
            chan.setIsPublic(false);
            chan.setWantNewMeta(false);
            chan.setWantNewMsgs(false);
            channels.add(chan);
        }
    }
}
