package syndie.gui;

import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import syndie.Constants;
import syndie.data.NymKey;
import syndie.data.SyndieURI;
import syndie.data.Timer;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 * displays specified channels, including some basic stats, the avatar, and controls
 * to perform basic functions on the channels
 */
public class ChannelTable extends BaseComponent implements Themeable, Translatable {
    private NavigationControl _navControl;
    private URIControl _uriControl;
    
    private Composite _parent;
    
    private Composite _root;
    private Table _table;
    private TableColumn _colAvatar;
    private TableColumn _colName;
    private TableColumn _colMsgs;
    private TableColumn _colLastActivity;
    private TableColumn _colDescription;
    private TableColumn _colIsIdent;
    private TableColumn _colIsReplyReadable;
    private TableColumn _colIsManageable;
    private TableColumn _colIsPostable;
    private TableColumn _colIsPubliclyReplyable;
    private TableColumn _colIsPubliclyPostable;
    private TableColumn _colIsReadable;
    
    private Button _viewMsgs;
    private Button _viewProfile;
    private Button _manageProfile;
    private Button _post;
    private Button _close;
    
    private Runnable _onClose;
    
    private List _records;
    
    public ChannelTable(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl nav, URIControl uriControl, Composite parent, Runnable onClose) {
        super(client, ui, themes, trans);
        _navControl = nav;
        _uriControl = uriControl;
        _parent = parent;
        _onClose = onClose;
        _records = new ArrayList();
        initComponents();
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        GridLayout gl = new GridLayout(5, true);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        _root.setLayout(gl);
        
        _table = new Table(_root, SWT.BORDER | SWT.FULL_SELECTION | SWT.SINGLE);
        _table.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 5, 1));
        _table.setHeaderVisible(true);
        _table.setLinesVisible(true);
        
        _colAvatar = new TableColumn(_table, SWT.LEFT);
        _colName = new TableColumn(_table, SWT.LEFT);
        _colMsgs = new TableColumn(_table, SWT.LEFT);
        _colLastActivity = new TableColumn(_table, SWT.LEFT);
        _colDescription = new TableColumn(_table, SWT.LEFT);
        _colIsIdent = new TableColumn(_table, SWT.CENTER);
        _colIsReplyReadable = new TableColumn(_table, SWT.CENTER);
        _colIsManageable = new TableColumn(_table, SWT.CENTER);
        _colIsPostable = new TableColumn(_table, SWT.CENTER);
        _colIsPubliclyReplyable = new TableColumn(_table, SWT.CENTER);
        _colIsPubliclyPostable = new TableColumn(_table, SWT.CENTER);
        _colIsReadable = new TableColumn(_table, SWT.CENTER);
        
        _table.addSelectionListener(new FireSelectionListener() { public void fire() { selectionUpdated(); } });
        _table.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.keyCode == SWT.TRAVERSE_RETURN)
                    viewSelected();
            }
        });
        _table.addKeyListener(new KeyListener() {
            public void keyPressed(KeyEvent keyEvent) {}
            public void keyReleased(KeyEvent evt) {
                if (evt.keyCode == SWT.CR) viewSelected();
            }
        });
        _table.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) { viewSelected(); }
            public void mouseDown(MouseEvent mouseEvent) {}
            public void mouseUp(MouseEvent mouseEvent) {}
        });
           
        
        _viewMsgs = new Button(_root, SWT.PUSH);
        _viewMsgs.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _viewMsgs.addSelectionListener(new FireSelectionListener() { public void fire() { viewSelected(); } });
        _viewProfile = new Button(_root, SWT.PUSH);
        _viewProfile.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _viewProfile.addSelectionListener(new FireSelectionListener() { public void fire() { viewSelectedProfile(); } });
        _manageProfile = new Button(_root, SWT.PUSH);
        _manageProfile.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _manageProfile.addSelectionListener(new FireSelectionListener() { public void fire() { manageSelectedProfile(); } });
        _post = new Button(_root, SWT.PUSH);
        _post.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _post.addSelectionListener(new FireSelectionListener() { public void fire() { postSelected(); } });
        _close = new Button(_root, SWT.PUSH);
        _close.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _close.addSelectionListener(new FireSelectionListener() { public void fire() { close(); } });
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }
    
    /** 
     * tell the table to render the given channelIds.  this should NOT be run from the 
     * swt thread 
     */
    public void setChannels(List channelIds, final Runnable afterSet) {
        if ( (channelIds == null) || (channelIds.size() == 0) ) return;
        
        final Timer timer = new Timer("set channels", _ui);
        Display d = _root.getDisplay();
        // step 1 gets the basic data per channel
        // step 2 grabs the avatar data
        // step 3 checks what privs we have on each of the channels
        // step 4 gets the message basics
        // step 5 runs in the GUI thread, creating avatars and rows
        final List records = getRecordsBasic(channelIds); // returns channels ordered by name
        timer.addEvent("record basics fetched");
        getAvatarData(records);
        timer.addEvent("avatar data fetched");
        getRecordPrivs(records, timer);
        timer.addEvent("record privs fetched");
        getMessageInfo(records);
        timer.addEvent("message info fetched");
        d.asyncExec(new Runnable() { public void run() { setChannelData(records, afterSet, timer); } });
    }
    
    private void setChannelData(List records, Runnable afterSet, Timer timer) {
        if (_table.isDisposed()) return;
        timer.addEvent("async set records");
        _table.setRedraw(false);
        timer.addEvent("redraw disabled");
        disposeExisting();
        timer.addEvent("old records disposed");
        for (int i = 0; i < records.size(); i++) {
            Record r = (Record)records.get(i); // no need to sort - getRecordsBasic did for us
            if (r.avatarData != null) {
                r.avatar = ImageUtil.createImage(r.avatarData);
                r.avatarData = null;
            }
            if (r.avatar == null)
                r.avatar = ImageUtil.ICON_EDITOR_BOOKMARKED_NOAVATAR;
            TableItem item = new TableItem(_table, SWT.NONE);
            item.setImage(0, r.avatar);
            item.setText(1, r.name != null ? r.name + " [" + r.scope.toBase64().substring(0,6) + "]" : "[" + r.scope.toBase64().substring(0,6) + "]");
            item.setText(2, r.messageCount + "");
            if (r.messageCount > 0)
                item.setText(3, Constants.getDate(r.lastImportDate));
            else
                item.setText(3, "");
            item.setText(4, r.desc != null ? r.desc : "");
            item.setText(5, r.isIdent ? "X" : "");
            item.setText(6, r.isReplyKnown ? "X" : "");
            item.setText(7, r.isManageable ? "X" : "");
            item.setText(8, r.isPostable ? "X" : "");
            // publicly postable implies publicly repliable
            item.setText(9, r.isPubliclyRepliable || r.isPubliclyPostable ? "X" : "");
            item.setText(10, r.isPubliclyPostable ? "X" : "");
            item.setText(11, r.isReadable ? "X" : "");
            
            _records.add(r);
            
            timer.addEvent("record " + i + " rendered");
        }
        timer.addEvent("all records rendered");
        
        _colAvatar.pack();
        _colName.pack();
        _colMsgs.pack();
        _colLastActivity.pack();
        _colDescription.pack();
        _colIsIdent.setWidth(16);
        _colIsReplyReadable.setWidth(16);
        _colIsManageable.setWidth(16);
        _colIsPostable.setWidth(16);
        _colIsPubliclyReplyable.setWidth(16);
        _colIsPubliclyPostable.setWidth(16);
        _colIsReadable.setWidth(16);
        
        timer.addEvent("columns packed");
        _table.setRedraw(true);
        timer.addEvent("redraw reenabled");
        timer.complete();
        afterSet.run();
    }
    private void disposeExisting() {
        if (!_table.isDisposed()) {
            TableItem items[] = _table.getItems();
            for (int i = 0; i < items.length; i++)
                if (!items[i].isDisposed()) items[i].dispose();
        }
        while (_records.size() > 0)
            ImageUtil.dispose(((Record)_records.remove(0)).avatar);
        _records.clear();
    }
    
    private static final String SQL_GET_RECORD_BEGIN = "SELECT name, channelHash, description, allowPubPost, allowPubReply, channelId FROM channel WHERE channelId IN (";
    private List getRecordsBasic(List channelIds) {
        StringBuffer query = new StringBuffer(SQL_GET_RECORD_BEGIN);
        int numIds = channelIds.size();
        for (int i = 0; i < numIds; i++) {
            Long id = (Long)channelIds.get(i);
            query.append(id.longValue());
            if (i + 1 < numIds)
                query.append(", ");
        }
        query.append(") ORDER BY name");
        Statement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _client.con().createStatement();
            rs = stmt.executeQuery(query.toString());
            List rv = new ArrayList();
            while (rs.next()) {
                String name = rs.getString(1);
                byte hash[] = rs.getBytes(2);
                String desc = rs.getString(3);
                boolean pubPost = rs.getBoolean(4);
                if (rs.wasNull()) pubPost = false;
                boolean pubReply = rs.getBoolean(5);
                if (rs.wasNull()) pubReply = false;
                long channelId = rs.getLong(6);
                
                if ( (channelId < 0) || (hash == null) || (hash.length != Hash.HASH_LENGTH) )
                    continue;
                
                Record r = new Record();
                r.channelId = channelId;
                r.name = name;
                r.desc = desc;
                r.scope = new Hash(hash);
                r.isPubliclyPostable = pubPost;
                r.isPubliclyRepliable = pubReply;
                rv.add(r);
            }
            return rv;
        } catch (SQLException se) {
            _ui.errorMessage("Internal error loading the basic data", se);
            return new ArrayList();
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    private void getAvatarData(List records) {
        for (int i = 0; i < records.size(); i++) {
            Record r = (Record)records.get(i);
            byte avatar[] = _client.getChannelAvatar(r.channelId);
            r.avatarData = avatar;
            if (avatar == null)
                _ui.debugMessage("no avatar for channelId " + r.channelId + " [" + r.name + "]");
        }
    }
    
    private void getRecordPrivs(List records, Timer timer) {
        // the heavy (and important) bit of this getChannels call is that it checks what forums
        // explicitly list the keys we have as authorized managers or posters, as opposed to what
        // we have imported the keys as.  to do this, we need to decrypt the keys and query based on
        // the public version of its sha256 (and fetching the public version means a 1024bit modPow).
        // this could certainly be sped up though, by caching the sha256(public version), for instance
        DBClient.ChannelCollector channels = _client.getChannels(true, true, true, false, false);
        timer.addEvent("privs collected");
        for (int i = 0; i < records.size(); i++) {
            Record r = (Record)records.get(i);
            Long chanId = new Long(r.channelId);
            r.isIdent = channels.getIdentityChannelIds().contains(chanId);
            r.isManageable = channels.getManagedChannelIds().contains(chanId);
            r.isPostable = channels.getPostChannelIds().contains(chanId);
            r.isReplyKnown = _client.getNymKeys(r.scope, Constants.KEY_FUNCTION_REPLY).size() > 0;
            r.isReadable = _client.getReadKeys(r.scope, false).size() > 0 ||
                           _client.getNymKeys(r.scope, Constants.KEY_FUNCTION_READ).size() > 0;
        }
    }
    
    private static final String SQL_GET_MSGINFO_BEGIN = "SELECT targetChannelId, MAX(messageId), MAX(importDate) FROM channelMessage WHERE targetChannelId IN (";
    private static final String SQL_GET_MSGCOUNT_BEGIN = "SELECT targetChannelId, COUNT(msgId) FROM channelMessage WHERE targetChannelId IN (";
    private void getMessageInfo(List records) {
        Map idToRec = new HashMap();
        StringBuffer query1 = new StringBuffer(SQL_GET_MSGINFO_BEGIN);
        StringBuffer query2 = new StringBuffer(SQL_GET_MSGCOUNT_BEGIN);
        int numRecs = records.size();
        for (int i = 0; i < numRecs; i++) {
            Record r = (Record)records.get(i);
            idToRec.put(new Long(r.channelId), r);
            query1.append(r.channelId);
            query2.append(r.channelId);
            if (i + 1 < numRecs) {
                query1.append(", ");
                query2.append(", ");
            }
        }
        query1.append(") GROUP BY targetChannelId");
        query2.append(") GROUP BY targetChannelId");
        
        Statement stmt = null;
        ResultSet rs = null;
        try {
            stmt = _client.con().createStatement();
            String sql = query1.toString();
            rs = stmt.executeQuery(sql);
            while (rs.next()) {
                long chanId = rs.getLong(1);
                long maxMsgId = rs.getLong(2);
                Date maxImport = rs.getDate(3);
                Record r = (Record)idToRec.get(new Long(chanId));
                r.lastMessageId = maxMsgId;
                r.lastImportDate = (maxImport != null ? maxImport.getTime() : -1);
            }
        
            rs.close();
            stmt.close();
        
            stmt = _client.con().createStatement();
            sql = query2.toString();
            rs = stmt.executeQuery(sql);
            while (rs.next()) {
                long chanId = rs.getLong(1);
                long msgs = rs.getLong(2);
                Record r = (Record)idToRec.get(new Long(chanId));
                r.messageCount = msgs;
            }            
        } catch (SQLException se) {
            _ui.errorMessage("Internal error loading the message info", se);
        } finally {
            if (rs != null) try { rs.close(); } catch (SQLException se) {}
            if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
        }
    }
    
    /** load up as much as we can async before mucking with the gui thread */
    private class Record {
        long channelId;
        String name;
        Hash scope;
        String desc;
        byte avatarData[];
        Image avatar;
        long messageCount;
        long lastMessageId;
        long lastImportDate;
        boolean isIdent;
        boolean isReplyKnown;
        boolean isManageable;
        boolean isPostable;
        boolean isPubliclyRepliable;
        boolean isPubliclyPostable;
        boolean isReadable;
    }
    
    public void dispose() {
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        disposeExisting();
    }

    private void close() {
        _onClose.run();
        dispose();
    }
    private void viewSelected() {
        int idx = _table.getSelectionIndex();
        if (idx >= 0) {
            Record r = (Record)_records.get(idx);
            close();
            _navControl.view(SyndieURI.createScope(r.scope));
        }
    }
    private void viewSelectedProfile() {
        int idx = _table.getSelectionIndex();
        if (idx >= 0) {
            Record r = (Record)_records.get(idx);
            close();
            _navControl.view(_uriControl.createMetaURI(r.scope));
        }
    }
    private void manageSelectedProfile() {
        int idx = _table.getSelectionIndex();
        if (idx >= 0) {
            Record r = (Record)_records.get(idx);
            close();
            _navControl.view(_uriControl.createManageURI(r.scope));
        }
    }
    private void postSelected() {
        int idx = _table.getSelectionIndex();
        if (idx >= 0) {
            Record r = (Record)_records.get(idx);
            close();
            _navControl.view(_uriControl.createPostURI(r.scope, null));
        }
    }
    private void selectionUpdated() {
        int idx = _table.getSelectionIndex();
        if (idx >= 0) {
            Record r = (Record)_records.get(idx);
            _manageProfile.setEnabled(r.isManageable | r.isIdent);
            _viewMsgs.setEnabled(r.isReadable);
            _post.setEnabled(r.isPostable || r.isPubliclyPostable || r.isManageable || r.isIdent);
        }
    }
    
    private static final String T_COLNAME = "syndie.gui.channeltab.colname";
    private static final String T_COLMSGS = "syndie.gui.channeltab.colmsgs";
    private static final String T_COLMSGS_TT = "syndie.gui.channeltab.colmsgs.tt";
    private static final String T_COLLASTACTIVITY = "syndie.gui.channeltab.collastactivity";
    private static final String T_COLLASTACTIVITY_TT = "syndie.gui.channeltab.collastactivity.tt";
    private static final String T_COLDESC = "syndie.gui.channeltab.coldesc";
    private static final String T_COLISIDENT_TT = "syndie.gui.channeltab.colisident.tt";
    private static final String T_COLISREPLYREADABLE_TT = "syndie.gui.channeltab.colisreplyreadable.tt";
    private static final String T_COLISMANAGEABLE_TT = "syndie.gui.channeltab.colismanageable.tt";
    private static final String T_COLISPOSTABLE_TT = "syndie.gui.channeltab.colispostable.tt";
    private static final String T_COLISPUBLICLYREPLYABLE_TT = "syndie.gui.channeltab.colispubliclyreplyable.tt";
    private static final String T_COLISPUBLICLYPOSTABLE_TT = "syndie.gui.channeltab.colispubliclypostable.tt";
    private static final String T_COLISREADABLE_TT = "syndie.gui.channeltab.colisreadable.tt";
    private static final String T_VIEWMSGS = "syndie.gui.channeltab.viewmsgs";
    private static final String T_VIEWPROFILE = "syndie.gui.channeltab.viewprofile";
    private static final String T_MANAGEPROFILE = "syndie.gui.channeltab.manageprofile";
    private static final String T_POST = "syndie.gui.channeltab.post";
    private static final String T_CLOSE = "syndie.gui.channeltab.close";

    public void translate(TranslationRegistry registry) {
        _colName.setText(registry.getText(T_COLNAME, "Name"));
        _colMsgs.setText(registry.getText(T_COLMSGS, "# msgs"));
        _colMsgs.setToolTipText(registry.getText(T_COLMSGS_TT, "Total number of messages posted"));
        _colLastActivity.setText(registry.getText(T_COLLASTACTIVITY, "activity"));
        _colLastActivity.setToolTipText(registry.getText(T_COLLASTACTIVITY_TT, "Date of the most recent post"));
        _colDescription.setText(registry.getText(T_COLDESC, "Description"));
        _colIsIdent.setToolTipText(registry.getText(T_COLISIDENT_TT, "Private identity key known?"));
        _colIsReplyReadable.setToolTipText(registry.getText(T_COLISREPLYREADABLE_TT, "Private reply key known?"));
        _colIsManageable.setToolTipText(registry.getText(T_COLISMANAGEABLE_TT, "Private management key known?"));
        _colIsPostable.setToolTipText(registry.getText(T_COLISPOSTABLE_TT, "Private posting key known?"));
        _colIsPubliclyReplyable.setToolTipText(registry.getText(T_COLISPUBLICLYREPLYABLE_TT, "Anyone can reply?"));
        _colIsPubliclyPostable.setToolTipText(registry.getText(T_COLISPUBLICLYPOSTABLE_TT, "Anyone can post?"));
        _colIsReadable.setToolTipText(registry.getText(T_COLISREADABLE_TT, "Posts may be readable?"));
        
        _viewMsgs.setText(registry.getText(T_VIEWMSGS, "View messages"));
        _viewProfile.setText(registry.getText(T_VIEWPROFILE, "View profile"));
        _manageProfile.setText(registry.getText(T_MANAGEPROFILE, "Manage profile"));
        _post.setText(registry.getText(T_POST, "Post"));
        _close.setText(registry.getText(T_CLOSE, "Close"));
    }
    
    public void applyTheme(Theme theme) {
        _table.setFont(theme.TABLE_FONT);
        _viewMsgs.setFont(theme.BUTTON_FONT);
        _viewProfile.setFont(theme.BUTTON_FONT);
        _manageProfile.setFont(theme.BUTTON_FONT);
        _post.setFont(theme.BUTTON_FONT);
        _close.setFont(theme.BUTTON_FONT);
    }
}
