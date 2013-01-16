package syndie.gui;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.i2p.data.Hash;

import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.data.WatchedChannel;
import syndie.db.DBClient;

/**
 *  Generate SyndieURIs for internal use in the Browser
 */
public class URIHelper implements URIControl {
    private static final URIHelper _instance = new URIHelper();
    public static URIHelper instance() { return _instance; }
    
    public SyndieURI createPostURI(long postponeId, int postponeVersion) {
        Map attributes = new HashMap();
        attributes.put("postponeid", new Long(postponeId));
        attributes.put("postponever", new Long(postponeVersion));
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_POST, attributes);
        return uri;
    }
    public SyndieURI createManageURI(Hash forum) {
        Map attributes = new HashMap();
        if (forum != null)
            attributes.put("channel", forum.toBase64());
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_MANAGE, attributes);
        return uri;
    }
    public SyndieURI createMetaURI(Hash forum) {
        Map attributes = new HashMap();
        if (forum != null)
            attributes.put("channel", forum.toBase64());
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_META, attributes);
        return uri;
    }
    public SyndieURI createMetaRefsURI(Hash forum) {
        Map attributes = new HashMap();
        if (forum != null)
            attributes.put("channel", forum.toBase64());
        //attributes.put(ManageForum.DETAIL, ManageForum.DETAIL_REFS);
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_META, attributes);
        return uri;
    }
    public SyndieURI createMetaArchivesURI(Hash forum) {
        Map attributes = new HashMap();
        if (forum != null)
            attributes.put("channel", forum.toBase64());
        //attributes.put(ManageForum.DETAIL, ManageForum.DETAIL_ARCHIVES);
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_META, attributes);
        return uri;
    }
    public SyndieURI createMetaPostersURI(Hash forum) {
        Map attributes = new HashMap();
        if (forum != null)
            attributes.put("channel", forum.toBase64());
        //attributes.put(ManageForum.DETAIL, ManageForum.DETAIL_POSTERS);
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_META, attributes);
        return uri;
    }
    public SyndieURI createMetaManagersURI(Hash forum) {
        Map attributes = new HashMap();
        if (forum != null)
            attributes.put("channel", forum.toBase64());
        //attributes.put(ManageForum.DETAIL, ManageForum.DETAIL_MANAGER);
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_META, attributes);
        return uri;
    }
    
    public SyndieURI createTextUIURI() { return new SyndieURI(BrowserTab.TYPE_TEXTUI, new HashMap()); }
    public SyndieURI createLogsURI() { return new SyndieURI(BrowserTab.TYPE_LOGS, new HashMap()); }
    public SyndieURI createSQLURI() { return new SyndieURI(BrowserTab.TYPE_SQL, new HashMap()); }
    public SyndieURI createTranslateURI() { return new SyndieURI(BrowserTab.TYPE_TRANSLATE, new HashMap()); }
    public SyndieURI createSyndicationArchiveURI() { return new SyndieURI(BrowserTab.TYPE_SYNDICATE_ARCHIVES, new HashMap()); }
    public SyndieURI createSyndicationConfigURI() { return new SyndieURI(BrowserTab.TYPE_SYNDICATE_CONFIG, new HashMap()); }
    public SyndieURI createSyndicationDiffURI() { return createSyndicationConfigURI(); }
    public SyndieURI createSyndicationStatusURI() { return new SyndieURI(BrowserTab.TYPE_SYNDICATE_STATUS, new HashMap()); }

    public SyndieURI createBugReportURI() { 
        HashMap attr = new HashMap();
        // not really random, just (likely) different from other values, so multiple instances of the bug report
        // tab can come up (one per uri)
        attr.put("rand", System.currentTimeMillis()+""); 
        return new SyndieURI(BrowserTab.TYPE_BUGREPORT, attr);
    }
    public SyndieURI createBackupSecretsURI() { return new SyndieURI(BrowserTab.TYPE_BACKUPSECRETS, new HashMap()); }    

    public SyndieURI createResumeableURI() {
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_RESUMEABLE, new HashMap());
        return uri;
    }
    public SyndieURI createPostURI(Hash forum, SyndieURI parent) {
        return createPostURI(forum, parent, false);
    }
    public SyndieURI createPostURI(Hash forum, SyndieURI parent, boolean asPrivateReply) {
        return createPostURI(forum, parent, asPrivateReply, null, null);
    }
    
    public SyndieURI createPostURI(Hash forum, SyndieURI parent, boolean asPrivateReply, List references, File attachments[]) {
        Map attributes = new HashMap();
        if (forum != null)
            attributes.put("channel", forum.toBase64());
        else if (parent != null)
            attributes.put("channel", parent.getScope());
        if (parent != null)
            attributes.put("parent", parent.toString());
        attributes.put("reply", ""+asPrivateReply);
        if (references != null)
            attributes.put("refs", ReferenceNode.walk(references));
        if (attachments != null) {
            attributes.put("attachments", new Long(attachments.length));
            for (int i = 0; i < attachments.length; i++)
                attributes.put("attachment" + i, attachments[i].getAbsolutePath());
        }
        attributes.put("uniq", "" + System.currentTimeMillis() + attributes.hashCode()); // just a local uniq
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_POST, attributes);
        return uri;
    }
    public SyndieURI createPostURI(Hash forum, SyndieURI parent, String pbePass, String pbePrompt, List references, File attachments[]) {
        Map attributes = new HashMap();
        if (forum != null)
            attributes.put("channel", forum.toBase64());
        else if (parent != null)
            attributes.put("channel", parent.getScope());
        if (parent != null)
            attributes.put("parent", parent.toString());
        attributes.put("pbePass", pbePass);
        attributes.put("pbePrompt", pbePrompt);
        if (references != null)
            attributes.put("refs", ReferenceNode.walk(references));
        if (attachments != null) {
            attributes.put("attachments", new Long(attachments.length));
            for (int i = 0; i < attachments.length; i++)
                attributes.put("attachment" + i, attachments[i].getAbsolutePath());
        }
        attributes.put("uniq", "" + System.currentTimeMillis() + attributes.hashCode()); // just a local uniq
        SyndieURI uri = new SyndieURI(BrowserTab.TYPE_POST, attributes);
        return uri;
    }

    public SyndieURI createHighlightWatchedURI(DBClient client, boolean threaded, boolean unreadOnly, boolean useImportDate) {
        List scopes = new ArrayList();
        List watched = client.getWatchedChannels();
        for (int i = 0; i < watched.size(); i++) {
            WatchedChannel chan = (WatchedChannel)watched.get(i);
            if (chan.getHighlight())
                scopes.add(client.getChannelHash(chan.getChannelId()));
        }
        SyndieURI uri = SyndieURI.createBookmarked(scopes, threaded, unreadOnly, useImportDate);
        return uri;
    }

    public SyndieURI createExpirationURI(Hash scope) { 
        HashMap attr = new HashMap();
        if (scope != null)
            attr.put("scope", scope.toBase64());
        return new SyndieURI(BrowserTab.TYPE_EXPIRATION, attr);
    }
    
    public SyndieURI createCancelURI(Hash scope) { 
        HashMap attr = new HashMap();
        if (scope != null)
            attr.put("scope", scope.toBase64());
        return new SyndieURI(BrowserTab.TYPE_CANCEL, attr);
    }

    public SyndieURI createArchiveManagerURI() {
        return new SyndieURI(BrowserTab.TYPE_ARCHIVEMGR, new HashMap());
    }
}
