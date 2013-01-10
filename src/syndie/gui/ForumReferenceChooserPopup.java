package syndie.gui;

import java.util.ArrayList;
import java.util.List;

import net.i2p.data.Hash;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 * TODO this popup is also used for "choose author" from MessageEditor, but
 * needs some small changes for that case.
 * and also has the "only include forums" text at the top
 */
public class ForumReferenceChooserPopup extends BaseComponent implements ReferenceChooserPopup, Themeable, Translatable {
    private Composite _parent;
    private Shell _shell;
    private NymChannelTree _channels;
    private boolean _preferRefs;
    private NymChannelTree.ChannelSource _channelSource;
    private ReferenceChooserTree.AcceptanceListener _acceptListener;
    
    private Button _cancel;
    
    private NavigationControl _navControl;
    private BanControl _banControl;
    private BookmarkControl _bookmarkControl;
    
    public ForumReferenceChooserPopup(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl navControl, BanControl banControl, BookmarkControl bookmarkControl, Composite parent, ReferenceChooserTree.AcceptanceListener acceptLsnr) {
        this(client, ui, themes, trans, navControl, banControl, bookmarkControl, parent, acceptLsnr, null);
    }
    public ForumReferenceChooserPopup(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, NavigationControl navControl, BanControl banControl, BookmarkControl bookmarkControl, Composite parent, ReferenceChooserTree.AcceptanceListener acceptLsnr, NymChannelTree.ChannelSource channelSource) {
        super(client, ui, themes, trans);
        _parent = parent;
        _preferRefs = false;
        _acceptListener = acceptLsnr;
        _navControl = navControl;
        _banControl = banControl;
        _bookmarkControl = bookmarkControl;
        _channelSource = channelSource;
        initComponents();
    }
    
    public void dispose() {
        _themeRegistry.unregister(this);
        _translationRegistry.unregister(this);
        _channels.dispose();
        if (!_shell.isDisposed()) _shell.dispose();
    }
    
    private void initComponents() {
        _shell = new Shell(_parent.getShell(), SWT.SHELL_TRIM | SWT.PRIMARY_MODAL);
        GridLayout gl = new GridLayout(5, true);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        _shell.setLayout(gl);
        
        Color color = ColorUtil.getColor("yellow");
        
        _channels = new NymChannelTree(_client, _ui, _themeRegistry, _translationRegistry, _navControl, _banControl, _bookmarkControl, _shell, new NymChannelTree.NymChannelTreeListener() {
            public void channelSelected(SyndieURI uri) { 
                _channels.dispose();
                _acceptListener.referenceAccepted(uri);
                dispose();
            }
            public void channelProfileSelected(SyndieURI uri) {
                _channels.dispose();
                _acceptListener.referenceAccepted(uri);
                dispose();
            }
            public void channelManageSelected(SyndieURI manageURI) {
                _channels.dispose();
                _acceptListener.referenceAccepted(manageURI);
                dispose();
            }
            public void channelPostSelected(SyndieURI postURI) {
                _channels.dispose();
                _acceptListener.referenceAccepted(postURI);
                dispose();
            }
            public void channelPreviewed(Hash scope, long chanId, String name, String desc, Image avatar) {}
        }, true, true);
        _channels.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 5, 1));
        
        _cancel = new Button(_shell, SWT.PUSH);
        _cancel.addSelectionListener(new FireSelectionListener() { public void fire() { forumSelectorCancelled(); } });
        _cancel.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 5, 1));
        
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) {
                evt.doit = false;
                forumSelectorCancelled();
            }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }

    public void setListener(ReferenceChooserTree.AcceptanceListener lsnr) { _acceptListener = lsnr; }
    public void show() { open(); }
    public void hide() { dispose(); }
    
    public void open() {
        if ( (_shell == null) || (_shell.isDisposed()) )
            initComponents(); // reopened

        if (_channelSource != null) {
            _channels.setChannelSource(_channelSource);
            _channels.loadData();
        } else if (_preferRefs) {
            _channels.showBookmarks();
        } else {
            _channels.showNymChannels();
        }
        
        _shell.pack(true);
        _shell.setSize(_shell.computeSize(SWT.DEFAULT, 500));
        _shell.open();
    }
    
    public void forumSelectorCancelled() {
        _ui.debugMessage("channel selector cancelled");
        _acceptListener.referenceChoiceAborted();
        dispose();
    }

    public void applyTheme(Theme theme) {
        _shell.setFont(theme.SHELL_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
    }
    
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText("Select forum"));
        _cancel.setText(registry.getText("Cancel"));
    }
}
