package syndie.gui;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.StringTokenizer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import syndie.Constants;
import syndie.data.ChannelInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.CommandImpl;
import syndie.db.DBClient;

/**
 * search control for a ReferenceChooserTree
 */
public class ReferenceChooserSearch {
    private ReferenceChooserTree _chooser;
    private Composite _parent;
    private Group _root;
    private Text _name;
    private Text _tags;
    private Text _hash;
    private Button _publicPost;
    private Button _search;
    
    public ReferenceChooserSearch(Composite parent, ReferenceChooserTree chooser) {
        _parent = parent;
        _chooser = chooser;
        initComponents();
    }
    
    public Control getControl() { return _root; }
    
    private void initComponents() {
        _root = new Group(_parent, SWT.SHADOW_ETCHED_OUT);
        _root.setLayout(new GridLayout(2, false));
        _root.setText("Forum search criteria:");
        
        Label l = new Label(_root, SWT.NONE);
        l.setText("Name: ");
        GridData gd = new GridData();
        gd.horizontalAlignment = GridData.HORIZONTAL_ALIGN_END;
        gd.verticalAlignment = GridData.VERTICAL_ALIGN_CENTER;
        l.setLayoutData(gd);
        _name = new Text(_root, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
        _name.setToolTipText("the channel name must start with this string");
        _name.setLayoutData(new GridData(GridData.FILL_BOTH));

        l = new Label(_root, SWT.NONE);
        l.setText("Tags: ");
        gd = new GridData();
        gd.horizontalAlignment = GridData.HORIZONTAL_ALIGN_END;
        gd.verticalAlignment = GridData.VERTICAL_ALIGN_CENTER;
        l.setLayoutData(gd);
        _tags = new Text(_root, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
        _tags.setToolTipText("-tag excludes, +tag requires all, without a prefix requires one or more");
        _tags.setLayoutData(new GridData(GridData.FILL_BOTH));

        l = new Label(_root, SWT.NONE);
        l.setText("Hash: ");
        gd = new GridData();
        gd.horizontalAlignment = GridData.HORIZONTAL_ALIGN_END;
        gd.verticalAlignment = GridData.VERTICAL_ALIGN_CENTER;
        l.setLayoutData(gd);
        _hash = new Text(_root, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
        _hash.setToolTipText("the channel's hash must start with this string");
        _hash.setLayoutData(new GridData(GridData.FILL_BOTH));

        _publicPost = new Button(_root, SWT.CHECK);
        _publicPost.setText("anyone can post?");
        _publicPost.setToolTipText("if true, include channels that anyone is allowed to post to");
        gd = new GridData(GridData.FILL_BOTH);
        gd.horizontalSpan = 2;
        _publicPost.setLayoutData(gd);
        
        _search = new Button(_root, SWT.PUSH);
        _search.setText("Search");
        gd = new GridData(GridData.FILL_BOTH);
        gd.horizontalSpan = 2;
        _search.setLayoutData(gd);
        
        _name.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) { 
                if (evt.detail == SWT.TRAVERSE_RETURN) search(); 
            }
        });
        _hash.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) { 
                if (evt.detail == SWT.TRAVERSE_RETURN) search(); 
            }
        });
        _tags.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) { 
                if (evt.detail == SWT.TRAVERSE_RETURN) search(); 
            }
        });
        _search.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { search(); }
            public void widgetSelected(SelectionEvent selectionEvent) { search(); }
        });
    }
    
    private void search() {
        String name = _name.getText().trim();
        String tags = _tags.getText().trim();
        String hash = _hash.getText().trim();
        boolean publicPost = _publicPost.getSelection();
        
        DBClient client = _chooser.getClient();
        DBClient.ChannelSearchCriteria crit = new DBClient.ChannelSearchCriteria();
        crit.setHashPrefix(hash);
        crit.setName(name);
        StringTokenizer tok = new StringTokenizer(tags);
        while (tok.hasMoreTokens()) {
            String cur = tok.nextToken();
            if (cur.startsWith("-") && (cur.length() > 1))
                crit.excludeTag(cur.substring(1));
            else if (cur.startsWith("+") && (cur.length() > 1))
                crit.requireTag(cur.substring(1));
            else
                crit.includeTag(cur);
        }
        List channels = client.getChannels(crit);
        if (publicPost) {
            List pubMatch = new ArrayList();
            DBClient.ChannelCollector chans = client.getChannels(false, false, false, true);
            for (int i = 0; i < chans.getPublicPostChannelCount(); i++) {
                ChannelInfo pub = chans.getPublicPostChannel(i);
                if (channels.contains(pub)) {
                    pubMatch.add(pub);
                }
            }
            channels = pubMatch;
        }
        List matches = new ArrayList();
        for (int i = 0; i < channels.size(); i++) {
            ChannelInfo chan = (ChannelInfo)channels.get(i);
            String matchName = chan.getName();
            SyndieURI uri = SyndieURI.createScope(chan.getChannelHash());
            String description = chan.getDescription();
            String type = null;
            ReferenceNode match = new ReferenceNode(matchName, uri, description, type);
            matches.add(match);
        }
        _chooser.setSearchResults(matches);
    }
}
