package syndie.gui;

import java.net.URISyntaxException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import syndie.data.SyndieURI;

import org.eclipse.swt.dnd.*;
import syndie.db.DBClient;
import syndie.db.TextUI;
import syndie.db.TextEngine;
import java.io.*;

public class MessageTab extends Component {
    private static Color _encrypted;
    private DBClient _client;
    private CTabFolder _parent;
    private Composite _top;
    private Image _icon;
    private Image _avatar;
    private Label _aboutAvatar;
    private Label _aboutSubject;
    private Composite _aboutChannelRow;
    private Button _aboutChannel;
    private Label _aboutDate;
    private Link _aboutTags;
    private Button _aboutNym;
    private Composite _aboutFlags;
    private Combo _page;
    private PageRenderer _body;
    private CTabItem _tab;
    private SyndieURI _uri;
    private DragSource _dndSource;
    
    public MessageTab(DBClient client, CTabFolder parent, String uri) {
        try {
            SyndieURI u = new SyndieURI(uri);
            init(client, parent, u);
        } catch (URISyntaxException use) {
            init(client, parent, SyndieURI.createSearch(uri));
        }
    }
    public MessageTab(DBClient client, CTabFolder parent, SyndieURI uri) {
        init(client, parent, uri);
    }
    private void init(DBClient client, CTabFolder parent, SyndieURI uri) {
        _client = client;
        _parent = parent;
        _uri = uri;
        initComponents();
    }
    
    public SyndieURI getURI() { return _uri; }
    public String getName() { return "jrandom: 2006/04/26"; }
    public Image getIcon() { return _icon; }
    public String getDescription() { return "this message talks about my love of ponies"; }
    public Composite getTabRoot() { return _top; }
    public CTabItem getTab() { return _tab; }
    public void dispose() { 
        _icon.dispose();
        _tab.dispose();
        _top.dispose();
        if (_dndSource != null)
            _dndSource.dispose();
    }

    protected void instantiateComponents() {
        _tab = new CTabItem(_parent, SWT.NONE);
        
        _icon = new Image(_tab.getDisplay(), "resources/icon1.png");
        _avatar = new Image(_tab.getDisplay(), "resources/avatar.png");
        // now for the body
        
        _top = new Composite(_parent, SWT.NONE);
        _tab.setControl(_top);
        
        _aboutAvatar = new Label(_top, SWT.NONE);
        _aboutSubject = new Label(_top, SWT.NONE);
        _aboutChannelRow = new Composite(_top, SWT.BORDER);
        _aboutChannel = new Button(_aboutChannelRow, SWT.FLAT);
        _aboutDate = new Label(_aboutChannelRow, SWT.NONE);
        _aboutTags = new Link(_aboutChannelRow, SWT.NONE);
        _aboutNym = new Button(_top, SWT.FLAT);
        _aboutFlags = new Composite(_top, SWT.BORDER);
        _page = new Combo(_top, SWT.NONE);
        _body = new PageRenderer(this, _top);
    }
    protected void addActions() { 
        _aboutChannel.addMouseListener(new MouseListener() {
            public void mouseDoubleClick(MouseEvent mouseEvent) {}
            public void mouseDown(MouseEvent mouseEvent) {}
            public void mouseUp(MouseEvent mouseEvent) { showMessagesTab(); }
        });
    }
    private void showMessagesTab() {
        //_browser.getUser().view("urn:syndie:channel:d7:channel40:1234567890123456789012345678901234567890e");
    }
    protected void updateFields() {
        _aboutSubject.setText("This message talks about my love of ponies");
        _aboutAvatar.setImage(_avatar);
        _aboutChannelRow.setLayout(new RowLayout(SWT.HORIZONTAL));
        _aboutChannel.setText("$postedInChannel");
        _aboutChannel.setImage(_icon);
        _aboutDate.setText("2006/04/25");
        _aboutTags.setText("(<a>ponies</a>, <a>rainbows</a>, <a>kittens</a>)");
        _aboutNym.setImage(_icon);
        _aboutNym.setText("jrandom");
        _tab.setText(_uri.toString());
        _tab.setToolTipText(_aboutSubject.getText());
        
        updateFlags();
        
        _page.setItems(new String[] { "Page 1", "Page 2", "Page 3", "Page 4" });
        _page.select(0);
        _body.updateText();
    }
    
    private void updateFlags() {
        updateAttachments();
        updateKeys();
        updateReferences();
        updateChannelFlags();
    }
    private void updateAttachments() {
        Button flag = new Button(_aboutFlags, SWT.PUSH);
        flag.setImage(_icon);
        flag.setToolTipText("Attachments included");
        Menu menu = new Menu(flag);
        flag.setMenu(menu);
        flag.addSelectionListener(new ShowMenuOnSelect(menu));
        for (int i = 0; i < 10; i++) {
            MenuItem item = new MenuItem(menu, SWT.CASCADE);
            item.setText("attachment " + i);
            Menu action = new Menu(item);
            item.setMenu(action);
            new MenuItem(action, SWT.PUSH).setText("View");
            new MenuItem(action, SWT.PUSH).setText("Save");
            new MenuItem(action, SWT.SEPARATOR);
            new MenuItem(action, SWT.PUSH).setText("Type: image/png");
            new MenuItem(action, SWT.PUSH).setText("Size: 32KB");
            new MenuItem(action, SWT.PUSH).setText("Name: my_image_file.png");
            new MenuItem(action, SWT.PUSH).setText("Description: this is an image.  i think you'll like it");
        }
    }
    private void updateKeys() {
        Button flag = new Button(_aboutFlags, SWT.PUSH);
        flag.setImage(_icon);
        flag.setToolTipText("Keys included");
        Menu menu = new Menu(flag);
        flag.setMenu(menu);
        flag.addSelectionListener(new ShowMenuOnSelect(menu));
        
        MenuItem item = new MenuItem(menu, SWT.CASCADE);
        item.setText("Channel read key for $channelName");
        Menu action = new Menu(item);
        item.setMenu(action);
        new MenuItem(action, SWT.PUSH).setText("Import key");
        new MenuItem(action, SWT.PUSH).setText("View channel $channelName");
        
        item = new MenuItem(menu, SWT.CASCADE);
        item.setText("Channel post key for $channelName");
        action = new Menu(item);
        item.setMenu(action);
        new MenuItem(action, SWT.PUSH).setText("Import key");
        new MenuItem(action, SWT.PUSH).setText("View channel $channelName");
        
        item = new MenuItem(menu, SWT.CASCADE);
        item.setText("Channel manage key for $channelName");
        action = new Menu(item);
        item.setMenu(action);
        new MenuItem(action, SWT.PUSH).setText("Import key");
        new MenuItem(action, SWT.PUSH).setText("View channel $channelName");
        
        item = new MenuItem(menu, SWT.CASCADE);
        item.setText("Channel read reply key for $channelName");
        action = new Menu(item);
        item.setMenu(action);
        new MenuItem(action, SWT.PUSH).setText("Import key");
        new MenuItem(action, SWT.PUSH).setText("View channel $channelName");
    }
    private void updateReferences() {
        Button flag = new Button(_aboutFlags, SWT.PUSH);
        flag.setImage(_icon);
        flag.setToolTipText("References included");
        Menu menu = new Menu(flag);
        flag.setMenu(menu);
        flag.addSelectionListener(new ShowMenuOnSelect(menu));
        for (int i = 0; i < 10; i++) {
            MenuItem item = new MenuItem(menu, SWT.PUSH);
            item.setText("Reference to $uri");
        }
    }
    private void updateChannelFlags() {
        Button flag = new Button(_aboutFlags, SWT.PUSH);
        flag.setImage(_icon);
        flag.setToolTipText("You can manage this channel");
        Menu menu = new Menu(flag);
        flag.setMenu(menu);
        flag.addSelectionListener(new ShowMenuOnSelect(menu));
        new MenuItem(menu, SWT.PUSH).setText("Manage this channel");
        new MenuItem(menu, SWT.PUSH).setText("Flag this post for deletion");
        
        flag = new Button(_aboutFlags, SWT.PUSH);
        flag.setImage(_icon);
        flag.setToolTipText("This post has been cancelled by its author");
    }
    protected void arrangeComponents() {
        _top.setLayout(new GridLayout(3, false));
        _aboutFlags.setLayout(new RowLayout(SWT.HORIZONTAL));

        GridData data = new GridData();
        data.grabExcessHorizontalSpace = false;
        data.grabExcessVerticalSpace = false;
        data.horizontalAlignment = GridData.HORIZONTAL_ALIGN_BEGINNING;
        data.verticalAlignment = GridData.VERTICAL_ALIGN_BEGINNING;
        data.verticalSpan = 2;
        _aboutAvatar.setLayoutData(data);
        
        data = new GridData();
        data.grabExcessHorizontalSpace = true;
        data.grabExcessVerticalSpace = false;
        data.horizontalAlignment = GridData.FILL;
        data.verticalAlignment = GridData.FILL;
        data.horizontalSpan = 2;
        _aboutSubject.setLayoutData(data);
        
        data = new GridData();
        data.grabExcessHorizontalSpace = true;
        data.grabExcessVerticalSpace = false;
        data.horizontalAlignment = GridData.FILL;
        data.verticalAlignment = GridData.FILL;
        data.horizontalSpan = 2;
        _aboutChannelRow.setLayoutData(data);
        
        data = new GridData();
        data.grabExcessHorizontalSpace = false;
        data.grabExcessVerticalSpace = false;
        data.horizontalAlignment = GridData.FILL;
        data.verticalAlignment = GridData.FILL;
        _aboutNym.setLayoutData(data);
        
        data = new GridData();
        data.grabExcessHorizontalSpace = true;
        data.grabExcessVerticalSpace = false;
        data.horizontalAlignment = GridData.FILL;
        data.verticalAlignment = GridData.FILL;
        _aboutFlags.setLayoutData(data);
        
        data = new GridData();
        data.grabExcessHorizontalSpace = false;
        data.grabExcessVerticalSpace = false;
        data.horizontalAlignment = GridData.HORIZONTAL_ALIGN_END;
        data.verticalAlignment = GridData.FILL;
        _page.setLayoutData(data);
        
        data = new GridData();
        data.grabExcessHorizontalSpace = true;
        data.grabExcessVerticalSpace = true;
        data.horizontalAlignment = GridData.FILL;
        data.verticalAlignment = GridData.FILL;
        data.horizontalSpan = 3;
        data.heightHint = 50;
        _body.setLayoutData(data);
        
        //_scroll.setMinSize(_composite.computeSize(SWT.DEFAULT, SWT.DEFAULT));
    }

    public void setFocus() { _top.setFocus(); }
    
    private class ShowMenuOnSelect implements SelectionListener {
        private Menu _menu;
        public ShowMenuOnSelect(Menu menu) { _menu = menu; }
        public void widgetDefaultSelected(SelectionEvent selectionEvent) { _menu.setVisible(true); }
        public void widgetSelected(SelectionEvent selectionEvent) { _menu.setVisible(true); }
    }
    
    public static void main(String args[]) {
        System.out.println("running");
        System.setProperty("jbigi.dontLog", "true");
        System.setProperty("jcpuid.dontLog", "true");
        
        String rootDir = TextEngine.getRootPath();
        String script = null;
        for (int i = 0; i < args.length; i++) {
            if (args[i].startsWith("@"))
                script = args[i].substring(1);
            else
                rootDir = args[i];
        }
        TextUI ui = new TextUI(false);
        if (script != null) {
            try {
                BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(script), "UTF-8"));
                String line = null;
                while ( (line = in.readLine()) != null)
                    ui.insertCommand(line);
            } catch (UnsupportedEncodingException uee) {
                ui.errorMessage("internal error, your JVM doesn't support UTF-8?", uee);
            } catch (IOException ioe) {
                ui.errorMessage("Error running the script " + script, ioe);
            }
        }
        final TextEngine engine = new TextEngine(rootDir, ui);
        new Thread(new Runnable() { public void run() { engine.run(); } }).start();
        
        Display display = Display.getDefault();
        Shell shell = new Shell(display, SWT.SHELL_TRIM);
        Composite composite = new Composite(shell, SWT.NONE);
        composite.setLayout(new GridLayout(1, true));
        shell.setLayout(new FillLayout());
        CTabFolder parent = new CTabFolder(composite, SWT.BORDER | SWT.CLOSE | SWT.V_SCROLL | SWT.H_SCROLL);

        MessageTab tab = new MessageTab(engine.getClient(), parent, "");

        parent.setSelection(tab.getTab());
        tab.setFocus();
        parent.setLayoutData(new GridData(700, 500));
        shell.pack();
        shell.open();
        while (!shell.isDisposed()) {
            try {
                if (!display.readAndDispatch()) display.sleep();
            } catch (RuntimeException e) {
                e.printStackTrace();
            }
        }
        System.out.println("Done");
        System.exit(0);
    }
}
