package syndie.gui;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

/**
 * 
 */
public class MessageTree {
    private DBClient _client;
    private Composite _parent;
    private Tree _tree;
    private TreeColumn _colSubject;
    private TreeColumn _colAuthor;
    private TreeColumn _colChannel;
    private TreeColumn _colDate;
    private TreeColumn _colTags;
    
    private boolean _showAuthor;
    private boolean _showChannel;
    private boolean _showDate;
    private boolean _showTags;

    private MessageTreeListener _listener;
    private Map _itemToURI;
    
    public MessageTree(DBClient client, Composite parent, MessageTreeListener lsnr) { this(client, parent, lsnr, true, true, true, true); }        
    public MessageTree(DBClient client, Composite parent, MessageTreeListener lsnr, boolean showAuthor, boolean showChannel, boolean showDate, boolean showTags) {
        _client = client;
        _parent = parent;
        _listener = lsnr;
        _showAuthor = showAuthor;
        _showChannel = showChannel;
        _showDate = showDate;
        _showTags = showTags;
        _itemToURI = new HashMap();
        initComponents();
    }
    
    public Control getControl() { return _tree; }

    public interface MessageTreeListener {
        /** 
         * @param toView if true, the user wants to fully select the message.
         *        if false, they are just traversing the tree, browsing through messages,
         *        though they'd probably be ok with some contextual info regarding the
         *        selected message
         */
        public void messageSelected(SyndieURI uri, boolean toView);
    }
    
    private void initComponents() {
        _tree = new Tree(_parent, SWT.BORDER | SWT.SINGLE);
        
        _colSubject = new TreeColumn(_tree, SWT.LEFT);
        _colSubject.setText("Subject");
        _colAuthor = new TreeColumn(_tree, SWT.LEFT);
        _colAuthor.setText("Author");
        _colChannel = new TreeColumn(_tree, SWT.LEFT);
        _colChannel.setText("Forum");
        _colDate = new TreeColumn(_tree, SWT.LEFT);
        _colDate.setText("Date");
        _colTags = new TreeColumn(_tree, SWT.LEFT);
        _colTags.setText("Tags");
        
        _tree.setHeaderVisible(true);
        _tree.setLinesVisible(true);
        
        SyndieTreeListener lsnr = new SyndieTreeListener(_tree) {
            public void resized() { resizeCols(); }
            public void selected() { fireSelected(false); }
            public void returnHit() { fireSelected(true); }
            public boolean collapseOnReturn() { return false; }
        };
        _tree.addSelectionListener(lsnr);
        _tree.addControlListener(lsnr);
        _tree.addTraverseListener(lsnr);
        _tree.addKeyListener(lsnr);
    }
    
    public void sortDate(boolean newestFirst) { _tree.setSortColumn(_colDate); _tree.setSortDirection(newestFirst ? SWT.DOWN : SWT.UP); }
    public void sortAuthor(boolean aToZ) { _tree.setSortColumn(_colAuthor); _tree.setSortDirection(aToZ ? SWT.UP : SWT.DOWN); }
    public void sortChannel(boolean aToZ) { _tree.setSortColumn(_colChannel); _tree.setSortDirection(aToZ ? SWT.UP : SWT.DOWN); }
    public void sortSubject(boolean aToZ) { _tree.setSortColumn(_colSubject); _tree.setSortDirection(aToZ ? SWT.UP : SWT.DOWN); }
    public void sortTags(boolean aToZ) { _tree.setSortColumn(_colTags); _tree.setSortDirection(aToZ ? SWT.UP : SWT.DOWN); }
    
    public void showChannel(boolean show) { _showChannel = show; }
    public void showDate(boolean show) { _showDate = show; }
    public void showAuthor(boolean show) { _showAuthor = show; }
    public void showTags(boolean show) { _showTags = show; }
    
    public void setMessages(List referenceNodes) {
        _tree.setRedraw(false);
        _tree.removeAll();
        _itemToURI.clear();
        for (int i = 0; i < referenceNodes.size(); i++) {
            ReferenceNode node = (ReferenceNode)referenceNodes.get(i);
            add(node, null);
        }
        _colSubject.pack();
        _colDate.pack();
        _colChannel.pack();
        _colAuthor.pack();
        _colTags.pack();
        
        resizeCols();
        _tree.setRedraw(true);
    }
    
    private void add(ReferenceNode node, TreeItem parent) {
        TreeItem item = null;
        SyndieURI uri = node.getURI();
        if ( (uri != null) && (uri.getScope() != null) && (uri.getMessageId() != null) ) {
            if (parent == null)
                item = new TreeItem(_tree, SWT.NONE);
            else
                item = new TreeItem(parent, SWT.NONE);

            _itemToURI.put(item, uri);
            
            String subj = "";
            String auth = "";
            String chan = "";
            String date = "";
            String tags = "";

            long chanId = _client.getChannelId(uri.getScope());
            ChannelInfo scopeInfo = _client.getChannel(chanId);
            MessageInfo msg = _client.getMessage(chanId, uri.getMessageId());
            if (msg != null) {
                if (msg.getSubject() != null)
                    subj = msg.getSubject();
                long authorId = msg.getAuthorChannelId();
                if (authorId != chanId) {
                    ChannelInfo authInfo = _client.getChannel(authorId);
                    if (authInfo != null) {
                        auth = authInfo.getName() + " [" + authInfo.getChannelHash().toBase64().substring(0,6) + "]";
                    } else {
                        auth = "[unknown]";
                    }
                    //System.out.println("author is NOT the scope chan for " + uri.toString() + ": " + auth);
                } else {
                    //System.out.println("author is the scope chan for " + uri.toString());
                    auth = scopeInfo.getName() + " [" + scopeInfo.getChannelHash().toBase64().substring(0,6) + "]";
                }
                ChannelInfo chanInfo = scopeInfo;
                if (msg.getTargetChannelId() != scopeInfo.getChannelId()) {
                    System.out.println("target chan != scope chan: " + msg.getTargetChannel().toBase64() + "/" + msg.getTargetChannelId() + " vs " + scopeInfo.getChannelHash().toBase64() + "/" + scopeInfo.getChannelId());
                    //System.out.println("msg: " + uri.toString());
                    chanInfo = _client.getChannel(msg.getTargetChannelId());
                    if (chanInfo == null) {
                        chan = "[" + msg.getTargetChannel().toBase64().substring(0,6) + "]";
                    } else {
                        chan = chanInfo.getName() + " [" + chanInfo.getChannelHash().toBase64().substring(0,6) + "]";
                    }
                } else {
                    System.out.println("target chan == scope chan: " + msg.getTargetChannel().toBase64() + "/" + msg.getTargetChannelId() + "/" + msg.getInternalId() + "/" + msg.getScopeChannelId() + "/" + msg.getAuthorChannelId());
                    //System.out.println("msg: " + uri.toString());
                    chan = chanInfo.getName() + " [" + chanInfo.getChannelHash().toBase64().substring(0,6) + "]";
                }
                
                if (auth.length() <= 0) {
                     auth = chan;
                }
                Set msgTags = new TreeSet();
                msgTags.addAll(msg.getPublicTags());
                msgTags.addAll(msg.getPrivateTags());
                StringBuffer buf = new StringBuffer();
                for (Iterator iter = msgTags.iterator(); iter.hasNext(); ) {
                    buf.append(((String)iter.next()).trim()).append(" ");
                }
                tags = buf.toString().trim();
                date = getDate(msg.getMessageId());
                item.setGrayed(false);
            } else {
                // message is not locally known
                subj = "[unknown]";
                if (scopeInfo != null)
                    auth = scopeInfo.getName() + " [" + scopeInfo.getChannelHash().toBase64().substring(0,6) + "]";
                else
                    auth = "[" + uri.getScope().toBase64().substring(0,6) + "]";
                chan = "[unknown]";
                date = getDate(uri.getMessageId().longValue());
                tags = "[unknown]";
            }
            item.setText(0, subj);
            item.setText(1, auth);
            item.setText(2, chan);
            item.setText(3, date);
            item.setText(4, tags);
        } else {
            // reference node does not point to a uri, so don't build a row
            item = parent;
        }
        for (int i = 0; i < node.getChildCount(); i++)
            add(node.getChild(i), item);
    }
    
    private static final SimpleDateFormat _dayFmt = new SimpleDateFormat("yyyy/MM/dd");
    private static final String getDate(long msgId) { synchronized (_dayFmt) { return _dayFmt.format(new Date(msgId)); } }
    
    private void resizeCols() {
        int total = _tree.getClientArea().width;
        int subjWidth = 100;
        
        int chanWidth = 100;
        if (!_showChannel) chanWidth = 0;
        int authWidth = 100;
        if (!_showAuthor) authWidth = 0;
        int dateWidth = 100;
        if (!_showDate) dateWidth = 0;
        int tagsWidth = 100;
        if (!_showTags) tagsWidth = 0;
        
        if (total > subjWidth+chanWidth+authWidth+dateWidth+tagsWidth)
            subjWidth = total-chanWidth-authWidth-dateWidth-tagsWidth;
        
        _colSubject.setWidth(subjWidth);
        _colChannel.setWidth(chanWidth);
        _colAuthor.setWidth(authWidth);
        _colDate.setWidth(dateWidth);
        _colTags.setWidth(tagsWidth);
        
        //System.out.println("resize w/ total=" + total + " colSubject=" + _colSubject.getWidth());
    }

    private void fireSelected(boolean toView) {
        TreeItem selected[] = _tree.getSelection();
        if (selected != null) {
            for (int i = 0; i < selected.length; i++) {
                SyndieURI uri = (SyndieURI)_itemToURI.get(selected[i]);
                if (_listener != null)
                    _listener.messageSelected(uri, toView);
            }
        }
    }
    
}
