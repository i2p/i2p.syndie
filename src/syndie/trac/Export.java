package syndie.trac;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.StringReader;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import net.i2p.data.Hash;
import net.i2p.util.SecureFile;
import net.i2p.util.SecureFileOutputStream;

import syndie.Constants;
import syndie.html.HTMLStateBuilder;
import syndie.data.MessageInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.CommandImpl;
import syndie.db.DBClient;
import syndie.db.TextEngine;
import syndie.db.TextUI;
import syndie.thread.ThreadAccumulatorJWZ;
import syndie.util.StringUtil;

/**
 * Export the Syndie bug reports into a trac database.
 * Usage: export $syndieDataDir --tracdb $tracDbFile --attachmentdir $dir
 *
 * If $attachmentDir is not trac's ./attachments/ directory, be sure to copy its 
 * contents there recursively.  You will need to have sqlite's jdbc driver in the
 * classpath.  e.g.
 *  java -cp lib/syndie.jar:lib/hsqldb.jar:../sqlite/sqlitejdbc-v033-nested.jar \
 *           syndie.trac.Export \
 *           /tmp/newsyndieroot \
 *           --tracdb /tmp/newsyndieroot/trac.db \
 *           --attachmentdir /tmp/newsyndieroot/attachments/
 *
 * Logic:
 *  1) load up all threads in the syndie install that are tagged with "bugreport"
 *  2) foreach thread
 *   2.1) see if there is already an existing trac ticket for it (via the custom field "Syndie Bug URI")
 *    2.1.1) if there isn't, create one using the thread's root
 *   2.2) find all the URIs of messages in the thread
 *   2.3) find out which of those URIs are already inserted as replies to the current ticket
 *   2.4) foreach of those URIs that are NOT already replies
 *    2.4.1) insert a new reply to the current ticket with the URI's content
 *
 * Note that reply ordering may be off (we insert replies in a depth-first thread
 * traversal, but individual messages have random timestamps)
 *
 * This is a command line tool only, not referenced anywhere.
 * Don't worry about all the System.exit() calls.
 */
public class Export {
    private TextUI _ui;
    private DBClient _client;
    private Connection _tracCon;
    private File _attachmentDir;
    
    public Export(final String args[], final String dbFile, String attachmentDir) {
        _attachmentDir = new SecureFile(attachmentDir);
        if (!_attachmentDir.exists()) _attachmentDir.mkdirs();
        _ui = new TextUI(args, new TextEngine.ScriptListener() {
            public void alreadyRunning() {}
            public void loginFailed(Exception e) {}
            public void loginFailedBadPassphrase() {}
            public void loginFailedBadLogin() {}
            public void scriptComplete(String script) {
                if ("login".equals(script)) {
                    new Thread(new Runnable() { public void run() { export(dbFile); } }, "Exporter").start();
                }
            }
        }, false);
        _ui.insertCommand("togglepaginate");
        Thread t = new Thread(new Runnable() { 
            public void run() { _ui.run(); }
        }, "TextUI");
        t.start();
    }
    
    private void export(String dbfile) {
        _client = _ui.getEngine().getClient();
        connect(dbfile); // does a System.exit on failure
        ThreadAccumulatorJWZ acc = new ThreadAccumulatorJWZ(_client, _ui);
        acc.setFilter(getSearchFilter());
        acc.gatherThreads();
        _ui.debugMessage("Threads matching: " + acc.getThreadCount());
        for (int i = 0; i < acc.getThreadCount(); i++) {
            ReferenceNode threadRoot = acc.getRootThread(i);
            export(threadRoot);
        }
        //testTracCon();
        _ui.statusMessage("Done exporting " + acc.getThreadCount() + " threads");
        System.exit(0);
    }
    
    private SyndieURI getSearchFilter() {
        // search for threads tagged with bug report (not including PBE or privately encrypted messages)
        // only includes posts received in the last 7 days
        return SyndieURI.createSearch(null, null, null, Long.valueOf(7), null, new String[] { "bugreport" }, null, false, null, null, null, null, null, null, null, null, false, false, false, true, false);
    }
    
    /**
     *   2.1) see if there is already an existing trac ticket for it (via the custom field "Syndie Bug URI")
     *    2.1.1) if there isn't, create one using the thread's root
     *   2.2) find all the URIs of messages in the thread
     *   2.3) find out which of those URIs are already inserted as replies to the current ticket
     *   2.4) foreach of those URIs that are NOT already replies
     *    2.4.1) insert a new reply to the current ticket with the URI's content
     */
    private void export(ReferenceNode threadRoot) {
        _ui.statusMessage("Bug report thread rooted at " + threadRoot.getURI());
        int threadTicketId = getThreadTicketId(threadRoot);
        List uris = getURIs(threadRoot);
        uris.remove(threadRoot.getURI());
        List newReplies = getNewReplies(threadTicketId, uris);
        for (int i = 0; i < newReplies.size(); i++) {
            SyndieURI uri = (SyndieURI)newReplies.get(i);
            addReply(threadTicketId, uri);
        }
    }
    
    /** 2.2) find all the URIs of messages in the thread */
    private List getURIs(ReferenceNode root) {
        final List rv = new ArrayList();
        List roots = new ArrayList();
        roots.add(root);
        ReferenceNode.walk(roots, new ReferenceNode.Visitor() {
            public void visit(ReferenceNode node, int depth, int siblingOrder) {
                SyndieURI uri = node.getURI();
                if ( (uri != null) && (_client.getMessageId(uri.getScope(), uri.getMessageId()) >= 0) )
                    rv.add(uri);
            }
        });
        return rv;
    }
    
    /** 2.3) find out which of those URIs are already inserted as replies to the current ticket */
    private List getNewReplies(int threadTicketId, List uris) {
        List rv = new ArrayList();
        for (int i = 0; i < uris.size(); i++) {
            SyndieURI uri = (SyndieURI)uris.get(i);
            PreparedStatement stmt = null;
            ResultSet rs = null;
            try {
                stmt = _tracCon.prepareStatement("SELECT COUNT(*) FROM ticket_change WHERE ticket = ? AND newvalue LIKE '%" + uri.toString() + "%'");
                stmt.setInt(1, threadTicketId);
                rs = stmt.executeQuery();
                if (rs.next()) {
                    int count = rs.getInt(1);
                    if (count == 0)
                        rv.add(uri);
                }
            } catch (SQLException se) {
                _ui.errorMessage("Error retrieving the thread ticket reply count", se);
                System.exit(-1);
            } finally { 
                if (rs != null) try { rs.close(); } catch (SQLException se) {}
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }

        }
        return rv;
    }
    
    /**
     *   2.1) see if there is already an existing trac ticket for it (via the custom field "Syndie Bug URI")
     *    2.1.1) if there isn't, create one using the thread's root
     */
    private int getThreadTicketId(ReferenceNode threadRoot) {
        if ( (threadRoot == null) || (threadRoot.getURI() == null) ) return -1;
        String uri = threadRoot.getURI().toString();
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _tracCon.prepareStatement("SELECT ticket FROM ticket_custom WHERE name = 'syndie_buguri' AND value = ?");
            stmt.setString(1, uri);
            rs = stmt.executeQuery();
            if (rs.next()) {
                int ticketId = rs.getInt(1);
                if (rs.wasNull()) {
                    _ui.errorMessage("Null ticketId for " + uri);
                    return -1;
                } else {
                    _ui.statusMessage("TicketId " + ticketId + " already exists for " + uri);
                    return ticketId;
                }
            }
        } catch (SQLException se) {
            _ui.errorMessage("Error retrieving the thread ticket id", se);
            System.exit(-1);
        } finally { 
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        _ui.debugMessage("No ticket for " + uri + ", creating a new one");
        return createTicket(threadRoot);
    }
    
    private static final String SQL_INSERT_TICKET = "INSERT INTO ticket " +
            "(type, time, changetime, component, severity, priority, owner, " +
            "reporter, cc, version, milestone, status, resolution, summary, description, keywords" +
            ") VALUES (" +
            "?, ?, ?, ?, ?, ?, ?, " +
            "?, ?, ?, ?, ?, ?, ?, ?, ?" +
            ")";
    private int createTicket(ReferenceNode threadRoot) {
        SyndieURI uri = threadRoot.getURI();
        if (uri == null) {
            _ui.errorMessage("Cannot create a ticket for the thread - root is not known: " + threadRoot);
            return -1;
        }
        
        long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId());
        if (msgId == -1) {
            _ui.errorMessage("Cannot create a ticket for " + uri + ", as the message is not known locally");
            return -1;
        }
        
        MessageInfo msg = _client.getMessage(msgId);
        
        Set tags = new HashSet(msg.getPublicTags());
        tags.addAll(msg.getPrivateTags());
        
        // standard fields
        String type = getTicketType(tags); // "defect", "usability issue", "enhancement", "task"
        int creationTime = (int)(uri.getMessageId().longValue() / 1000L); // time in seconds since epoch
        int modifiedTime = (int)(uri.getMessageId().longValue() / 1000L); // time in seconds since epoch
        String component = getTicketComponent(tags); // "Overall GUI", "text interface", etc
        String severity = getTicketSeverity(tags);
        String priority = getTicketPriority(tags);
        String owner = "dev";
        String reporter = getTicketReporter(msg);
        String cc = "";
        String version = getTicketVersion(tags);
        String milestone = "";
        String status = "new";
        String resolution = "";
        String summary = getTicketSubject(msg); // subject
        String keywords = getTicketKeywords(tags); // other tags
        String description = null;
        
        // custom fields
        String author = reporter;
        String os = null;
        String jvm = null;
        String swt = null;
        
        // we need to get the page content an stash it in the description, but 
        // also parse the first page for the OS/JVM/SWT version
        
        if (msg.getPageCount() == 0) {
            os = "";
            jvm = "";
            swt = "";
            description = "";
        } else {
            StringBuilder buf = new StringBuilder();
            
            for (int i = 0; i < msg.getPageCount(); i++) {
                String page = _client.getMessagePageData(msg.getInternalId(), i);
                String cfg = _client.getMessagePageConfig(msg.getInternalId(), i);
                boolean html = false;
                Properties props = new Properties();
                CommandImpl.parseProps(cfg, props);
                String mimeType = props.getProperty(Constants.MSG_PAGE_CONTENT_TYPE, "text/plain");
                if ("text/html".equalsIgnoreCase(mimeType) || "text/xhtml".equalsIgnoreCase(mimeType))
                    html = true;

                if (html)
                    page = htmlToText(page);

                buf.append(page);

                if (i + 1 < msg.getPageCount())
                    buf.append("\n\n======================================\n\n");
            }
            
            StringBuilder desc = new StringBuilder();
            //desc.append("{{{\n");
            try {
                BufferedReader in = new BufferedReader(new StringReader(buf.toString()));
                String line = null;
                boolean blankLineFound = false;
                while ( (line = in.readLine()) != null) {
                    line = line.trim();
                    if (!blankLineFound) {
                        if (line.length() <= 0) {
                            blankLineFound = true;
                            continue;
                        }
                        if (line.startsWith("OS:")) {
                            String val = line.substring("OS:".length()).trim();
                            os = val;
                        } else if (line.startsWith("JVM:")) {
                            String val = line.substring("JVM:".length()).trim();
                            jvm = val;
                        } else if (line.startsWith("SWT:")) {
                            String val = line.substring("SWT:".length()).trim();
                            swt = val;
                        }
                    } else {
                        desc.append(line).append("\n\n"); // 2 line for wiki
                    }
                }
            } catch (IOException ioe) {
                _ui.errorMessage("Internal error parsing the page", ioe);
                System.exit(-1);
            }
        
            //desc.append("}}}");
            
            description = desc.toString();
        }
    
        PreparedStatement stmt = null;
        try {
            stmt = _tracCon.prepareStatement(SQL_INSERT_TICKET);
            stmt.setString(1, type);
            stmt.setInt(2, creationTime);
            stmt.setInt(3, modifiedTime);
            stmt.setString(4, component);
            stmt.setString(5, severity);
            stmt.setString(6, priority);
            stmt.setString(7, owner);
            stmt.setString(8, reporter);
            stmt.setString(9, cc);
            stmt.setString(10, version);
            stmt.setString(11, milestone);
            stmt.setString(12, status);
            stmt.setString(13, resolution);
            stmt.setString(14, summary);
            stmt.setString(15, description);
            stmt.setString(16, keywords);

            stmt.executeUpdate();
            
            stmt.close();
        } catch (SQLException se) {
            _ui.errorMessage("Error inserting the ticket for " + threadRoot, se);
            System.exit(-1);
        } finally { 
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        int ticketId = getLastTicketId();
        if (ticketId < 0) System.exit(-1);
        
        _ui.statusMessage("Inserted ticket " + ticketId);
        
        // now insert custom fields, attachments, etc
        insertTicketCustom(ticketId, "syndie_buguri", threadRoot.getURI().toString());
        insertTicketCustom(ticketId, "syndie_bugfrom", author);
        insertTicketCustom(ticketId, "syndie_os", os);
        insertTicketCustom(ticketId, "syndie_jvm", jvm);
        insertTicketCustom(ticketId, "syndie_swt", swt);
        
        for (int i = 0; i < msg.getAttachmentCount(); i++) {
            _ui.statusMessage("Exporting attachment " + i + " for " + msg.getURI());
            addAttachment(ticketId, msg, i, author, modifiedTime);
        }
        
        return ticketId;
    }
    
    private static final Map<String, String> TYPE_MAP = new HashMap<String, String>();
    private static final String TYPE_DEFAULT = "defect";
    static {
        TYPE_MAP.put("type.bug", "defect");
        TYPE_MAP.put("type.rfe", "enhancement");
        TYPE_MAP.put("type.usability", "usability issue");
    }
    
    private static final Map<String, String> COMPONENT_MAP = new HashMap<String, String>();
    private static final String COMPONENT_DEFAULT = "other";
    /* 
     * trac's components:
     *  Overall GUI, browse forums, build process, create message, docs, find forums,
     *  find messages, gui theming, installer, manage forum, other, syndication, 
     *  syndication : freenet, syndiction : http, text interface, translation, view message,
     *  view message : HTML rendering, web rip, website
     */
    static {

        COMPONENT_MAP.put("component.build", "build process");
        COMPONENT_MAP.put("component.docs", "docs");
        COMPONENT_MAP.put("component.installer", "installer");
        COMPONENT_MAP.put("component.other", "other");
        COMPONENT_MAP.put("component.syndie.gui.bookmarks", "Overall GUI");
        COMPONENT_MAP.put("component.syndie.gui.browse", "browse forums");
        COMPONENT_MAP.put("component.syndie.gui.browse.filter", "browse forums");
        COMPONENT_MAP.put("component.syndie.gui.browse.multi", "browse forums");
        COMPONENT_MAP.put("component.syndie.gui.browse.preview", "browse forums");
        COMPONENT_MAP.put("component.syndie.gui.browser", "Overall GUI");
        COMPONENT_MAP.put("component.syndie.gui.forumsearch", "find forums");
        COMPONENT_MAP.put("component.syndie.gui.highlight", "Overall GUI");
        COMPONENT_MAP.put("component.syndie.gui.manageforum", "manage forum");
        COMPONENT_MAP.put("component.syndie.gui.manageforum.archives", "manage forum");
        COMPONENT_MAP.put("component.syndie.gui.manageforum.auth", "manage forum");
        COMPONENT_MAP.put("component.syndie.gui.manageforum.avatar", "manage forum");
        COMPONENT_MAP.put("component.syndie.gui.manageforum.keys", "manage forum");
        COMPONENT_MAP.put("component.syndie.gui.manageforum.refs", "manage forum");
        COMPONENT_MAP.put("component.syndie.gui.messageeditor", "create message");
        COMPONENT_MAP.put("component.syndie.gui.messageeditor.webrip", "create message");
        COMPONENT_MAP.put("component.syndie.gui.messagepostpone", "create message");
        COMPONENT_MAP.put("component.syndie.gui.messageview", "view message");
        COMPONENT_MAP.put("component.syndie.gui.messageview.attachments", "view message");
        COMPONENT_MAP.put("component.syndie.gui.messageview.pages", "view message");
        COMPONENT_MAP.put("component.syndie.gui.messageview.refs", "view message");
        COMPONENT_MAP.put("component.syndie.gui.messageview.render", "view message : HTML rendering");
        COMPONENT_MAP.put("component.syndie.gui.messageview.threading", "view message");
        COMPONENT_MAP.put("component.syndie.gui.syndication", "syndication");
        COMPONENT_MAP.put("component.syndie.gui.syndication.archive", "syndication");
        COMPONENT_MAP.put("component.syndie.gui.syndication.fcp", "syndication : freenet");
        COMPONENT_MAP.put("component.syndie.gui.syndication.http", "syndication : http");
        COMPONENT_MAP.put("component.syndie.gui.syndication.httpserv", "syndication");
        COMPONENT_MAP.put("component.syndie.gui.syndication.schedule", "syndication");
        COMPONENT_MAP.put("component.syndie.gui.textui", "text interface");
        COMPONENT_MAP.put("component.syndie.gui.theming", "gui theming");
        COMPONENT_MAP.put("component.syndie.gui.translation", "translation");
        COMPONENT_MAP.put("component.syndie.gui.webrip", "web rip");
        COMPONENT_MAP.put("component.web", "website");
    }
    
    private static final Map<String, String> SEVERITY_MAP = new HashMap<String, String>();
    private static final String SEVERITY_DEFAULT = "normal";
    static {
        SEVERITY_MAP.put("severity.trivial", "very minor");
        SEVERITY_MAP.put("severity.minor", "minor annoyance");
        SEVERITY_MAP.put("severity.standard", "normal");
        SEVERITY_MAP.put("severity.elevated", "limits some useful functionality");
        SEVERITY_MAP.put("severity.major", "limits important functionality");
        SEVERITY_MAP.put("severity.critical", "prevents important functionality");
        SEVERITY_MAP.put("severity.blocker", "prevents anything from working");
    }

    private static final Map<String, String> PRIORITY_MAP = new HashMap<String, String>();
    private static final String PRIORITY_DEFAULT = "minor";
    static {
        // use the severity from syndie to populate the priority as well
        PRIORITY_MAP.put("severity.trivial", "trivial");
        PRIORITY_MAP.put("severity.minor", "trivial");
        PRIORITY_MAP.put("severity.standard", "minor");
        PRIORITY_MAP.put("severity.elevated", "major");
        PRIORITY_MAP.put("severity.major", "major");
        PRIORITY_MAP.put("severity.critical", "critical");
        PRIORITY_MAP.put("severity.blocker", "blocker");
    }
    
    private String getTicketType(Set tags) { return translateTagVal(tags, TYPE_MAP, TYPE_DEFAULT); }
    private String getTicketComponent(Set tags) { return translateTagVal(tags, COMPONENT_MAP, COMPONENT_DEFAULT); }
    private String getTicketSeverity(Set tags) { return translateTagVal(tags, SEVERITY_MAP, SEVERITY_DEFAULT); }
    private String getTicketPriority(Set tags) { return translateTagVal(tags, PRIORITY_MAP, PRIORITY_DEFAULT); }
    
    private String translateTagVal(Set tags, Map<String, String> map, String defValue) {
        for (Map.Entry<String, String> e : map.entrySet()) {
            String src = e.getKey();
            if (tags.contains(src)) {
                String val = e.getValue();
                //_ui.debugMessage("translating [" + src + "] into [" + val + "] (default=" + defValue + ")");
                if (val != null)
                    return val;
                else
                    return defValue;
            }
        }
        _ui.debugMessage("translating found no matching tags, using default value of " + defValue);
        return defValue;
    }
    private String getTicketVersion(Set tags) { 
        for (Iterator iter = tags.iterator(); iter.hasNext(); ) {
            String val = (String)iter.next();
            if (val.startsWith("syndie."))
                return val.substring("syndie.".length());
        }
        return "1.003a"; 
    }
    private String getTicketKeywords(Set tags) {
        StringBuilder buf = new StringBuilder();
        for (Iterator iter = tags.iterator(); iter.hasNext(); ) {
            String val = (String)iter.next();
            if (val.startsWith("syndie.") || val.startsWith("component.") || val.startsWith("bugreport") || val.startsWith("type.") || val.startsWith("severity")) {
                continue;
            } else {
                // only allows ascii alphanum + a few symbols
                val = StringUtil.stripFilename(val, false);
                if (val.length() > 0)
                    buf.append(val).append(" ");
            }
        }
        return buf.toString();
    }
    
    private String getTicketSubject(MessageInfo msg) {
        String subj = msg.getSubject();
        if (subj != null)
            return subj;
        else
            return "";
    }
    
    private String getTicketReporter(MessageInfo msg) { 
        long chanId = msg.getAuthorChannelId();
        String name = _client.getChannelName(chanId);
        Hash hash = _client.getChannelHash(chanId);
        if (hash == null) return "unknown";
        if (name == null)
            return hash.toBase64().substring(0,6);
        else 
            return name + " [" + hash.toBase64().substring(0,6) + "]";
    }
    
    private String htmlToText(String src) {
        HTMLStateBuilder sb = new HTMLStateBuilder(src);
        sb.buildState();
        return HTMLStateBuilder.stripPlaceholders(sb.getAsText());
    }
    
    private static final String SQL_INSERT_TICKET_CUSTOM = "INSERT INTO ticket_custom (ticket, name, value) VALUES (?, ?, ?)";
    private void insertTicketCustom(int ticketId, String name, String value) {
        PreparedStatement stmt = null;
        try {
            stmt = _tracCon.prepareStatement(SQL_INSERT_TICKET_CUSTOM);
            stmt.setInt(1, ticketId);
            stmt.setString(2, name);
            if (value == null) value = "";
            stmt.setString(3, value);
            stmt.executeUpdate();
            stmt.close();
        } catch (SQLException se) {
            _ui.errorMessage("Error inserting custom attribute for ticket " + ticketId + ": " + name, se);
            System.exit(-1);
        }
    }
    
    private int getLastTicketId() {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _tracCon.prepareStatement("SELECT MAX(id) FROM ticket");
            rs = stmt.executeQuery();
            if (rs.next()) {
                int ticketId = rs.getInt(1);
                if (rs.wasNull()) {
                    _ui.errorMessage("Null last ticketId?");
                } else {
                    return ticketId;
                }
            } else {
                _ui.errorMessage("No last ticketId?");
            }
        } catch (SQLException se) {
            _ui.errorMessage("Error retrieving the thread ticket id", se);
        } finally { 
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        return -1;
    }
    
    private static final String SQL_ADD_REPLY = "INSERT INTO ticket_change (ticket, time, author, field, oldvalue, newvalue) VALUES (?, ?, ?, ?, ?, ?)";
    private void addReply(int threadTicketId, SyndieURI uri) {
        long msgId = _client.getMessageId(uri.getScope(), uri.getMessageId().longValue());
        MessageInfo msg = _client.getMessage(msgId);
        if (msg == null) return;
        int replyTime = (int)(uri.getMessageId().longValue() / 1000L);
        String author = getTicketReporter(msg);
        String replyContent = getReplyContent(msg);
        
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _tracCon.prepareStatement(SQL_ADD_REPLY);
            stmt.setInt(1, threadTicketId);
            stmt.setInt(2, replyTime);
            stmt.setString(3, author);
            stmt.setString(4, "comment");
            stmt.setString(5, "");
            stmt.setString(6, replyContent);
            stmt.executeUpdate();
            _ui.statusMessage("Reply added to " + threadTicketId + ": " + uri.toString());
        } catch (SQLException se) {
            _ui.errorMessage("Error adding reply", se);
            System.exit(-1);
        } finally { 
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
        
        for (int i = 0; i < msg.getAttachmentCount(); i++) {
            _ui.statusMessage("Exporting attachment " + i + " for " + msg.getURI());
            addAttachment(threadTicketId, msg, i, author, replyTime);
        }
    }
    
    private static final String SQL_ADD_ATTACHMENT = "INSERT INTO attachment (type, id, filename, size, time, description, author, ipnr) VALUES (?, ?, ?, ?, ?, ?, ?, ?)";
    private void addAttachment(int threadTicketId, MessageInfo msg, int attachmentNum, String author, int time) {
        // export the file to the _attachmentDir with the 'right' filename extension, then insert
        File f = exportAttachment(threadTicketId, msg, attachmentNum);
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _tracCon.prepareStatement(SQL_ADD_ATTACHMENT);
            stmt.setString(1, "ticket");
            stmt.setInt(2, threadTicketId);
            stmt.setString(3, f.getName());
            stmt.setInt(4, (int)f.length());
            stmt.setInt(5, time);
            stmt.setString(6, "");
            stmt.setString(7, author);
            stmt.setString(8, "127.0.0.1");
            stmt.executeUpdate();
            _ui.statusMessage("Attachment added to " + threadTicketId + ": " + f.getAbsolutePath());
        } catch (SQLException se) {
            _ui.errorMessage("Error adding reply", se);
            System.exit(-1);
        } finally { 
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private File exportAttachment(int threadTicketId, MessageInfo msg, int attachmentNum) {
        Properties cfg = _client.getMessageAttachmentConfig(msg.getInternalId(), attachmentNum);
        byte data[] = _client.getMessageAttachmentData(msg.getInternalId(), attachmentNum);
       
        String suffix = null;
        if (cfg.containsKey(Constants.MSG_ATTACH_NAME))
            suffix = cfg.getProperty(Constants.MSG_ATTACH_NAME);
        else
            suffix = ".dat";
        
        String type = cfg.getProperty(Constants.MSG_ATTACH_CONTENT_TYPE);
        if (type == null)
            type = "application/octet-stream";
        if ("application/octet-stream".equals(type) && (!suffix.endsWith(".dat")))
            suffix = suffix + ".dat";

        File out = null;
        try {
            File ticketsDir = new File(_attachmentDir, "ticket");
            File ticketDir = new SecureFile(ticketsDir, threadTicketId + "");
            ticketDir.mkdirs();
            out = SecureFile.createTempFile("attach", "_" + suffix, ticketDir);
            FileOutputStream fos = new SecureFileOutputStream(out);
            fos.write(data);
            fos.close();
        } catch (IOException ioe) {
            _ui.errorMessage("Unable to export the attachment", ioe);
            System.exit(-1);
        }
        
        _ui.statusMessage("Attachment exported to " + out.getName() + ": " + msg.getURI() + " attachment " + attachmentNum);
        return out;
    }
    
    private String getReplyContent(MessageInfo msg) {
        StringBuilder rv = new StringBuilder();
        rv.append("Syndie URI: ").append(msg.getURI().toString()).append("\n");
        
        //rv.append("{{{\n");
        
        for (int i = 0; i < msg.getPageCount(); i++) {
            String page = _client.getMessagePageData(msg.getInternalId(), i);
            String cfg = _client.getMessagePageConfig(msg.getInternalId(), i);
            boolean html = false;
            Properties props = new Properties();
            CommandImpl.parseProps(cfg, props);
            String mimeType = props.getProperty(Constants.MSG_PAGE_CONTENT_TYPE, "text/plain");
            if ("text/html".equalsIgnoreCase(mimeType) || "text/xhtml".equalsIgnoreCase(mimeType))
                html = true;

            if (html)
                page = htmlToText(page);

            rv.append(page);

            if (i + 1 < msg.getPageCount())
                rv.append("\n======================================\n");
        }
        
        // now double up newlines for wiki
        
        //rv.append("}}}");
        for (int i = 0; i < rv.length(); i++) {
            char c = rv.charAt(i);
            switch (c) {
                case '\n':
                case '\r':
                    rv.insert(i, '\n');
                    i++;
            }
        }
    
        return rv.toString();
    }
    
    private void connect(String tracDbFile) {
        File tracDb = new File(tracDbFile);
        if (!tracDb.exists()) {
            _ui.errorMessage("Tracdb file " + tracDb.getAbsolutePath() + " does not exist");
            System.exit(-1);
        }
        
        String tracURL = getTracURL(tracDb);
        try {
            Class.forName("org.sqlite.JDBC");
            _tracCon = DriverManager.getConnection(tracURL);
            _ui.debugMessage("TracDB connection established: " + tracURL);
        } catch (ClassNotFoundException cnfe) {
            _ui.errorMessage("SQLite JDBC driver not found - make sure to have sqlitejdbc.jar in your classpath", cnfe);
            System.exit(-1);
        } catch (SQLException se) {
            _ui.errorMessage("Cannot establish tracdb connection", se);
            System.exit(-1);
        }
    }
    
    private static final String getTracURL(File dbfile) {
        return "jdbc:sqlite:" + dbfile.getAbsolutePath();
    }
    
    public static void main(String args[]) { 
        String tracDbFile = null;
        String attachmentDir = null;
        String nargs[] = null;
        if (args.length >= 5) {
            nargs = new String[args.length-4];
            int c = 0;
            for (int i = 0; i < args.length; i++) {
                if ("--tracdb".equals(args[i]) && (i+1 < args.length)) {
                    tracDbFile = args[i+1].trim();
                    i++;
                    continue;
                } else if ("--attachmentdir".equals(args[i]) && (i+1 < args.length)) {
                    attachmentDir = args[i+1].trim();
                    i++;
                    continue;
                } else {
                    nargs[c] = args[i];
                    c++;
                }
            }
            new Export(nargs, tracDbFile, attachmentDir);
        } else {
            System.err.println("Usage: export $syndieDataDir --tracdb $tracDbFile --attachmentdir $dir");
            System.exit(-1);
        }
    }
}
