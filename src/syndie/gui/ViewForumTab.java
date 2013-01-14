package syndie.gui;

import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.SWT;
import syndie.Constants;
import syndie.data.SyndieURI;

/**
 *  Contains a ManageForum (RW) or ViewForum (RO)
 */
public class ViewForumTab extends BrowserTab {
    private ViewForum _view;
    private ManageForum _manage;
    private boolean _editable;
    
    public ViewForumTab(BrowserControl browser, SyndieURI uri) { super(browser, uri); }
    
    protected void initComponents() {
        getRoot().setLayout(new FillLayout());
        
        SyndieURI uri = getURI();
        _editable = false;
        Hash scope = uri.getScope();
        if (scope == null)
            scope = uri.getHash("scope");
        if (scope != null) {
            List keys = getBrowser().getClient().getNymKeys(scope, Constants.KEY_FUNCTION_MANAGE);
            if ( (keys != null) && (keys.size() > 0) )
                _editable = true;
            getBrowser().getUI().debugMessage("management nym keys for " + scope.toBase64() + ": " + keys);
            
            if (_editable) {
                Long val = uri.getLong("editable");
                if ( (val != null) && (val.longValue() == 0) )
                    _editable = false;
            }
        } else {
            getBrowser().getUI().debugMessage("no scope!  creating a new one");
            _editable = true;
        }
        
        if (_editable)
            _manage = new ManageForum(_client, _ui, _themeRegistry, _translationRegistry, getBrowser().getNavControl(), getBrowser(), getBrowser(), URIHelper.instance(), getBrowser(), getRoot(), uri, true);
        else
            _view = new ViewForum(_client, _ui, _themeRegistry, _translationRegistry, getBrowser(), getBrowser().getNavControl(), URIHelper.instance(), getRoot(), getURI());
        reconfigItem();
    }
    
    public Image getIcon() { return ImageUtil.ICON_TAB_ARCHIVE; }
    public String getName() { return _editable ? "Manage forum" : "View forum"; }
    public String getDescription() { return getName(); }
    
    protected void disposeDetails() { 
        if (_editable)
            _manage.dispose();
        else
            _view.dispose(); 
    }
    
    public boolean close() {
        if (allowClose()) 
            return super.close();
        else
            return false;
    }
    
    protected boolean allowClose() { 
        if (_editable)
            return _manage.confirmClose();
        else
            return true;
    }
    public void resized() { 
        if (_editable)
            _manage.resized();
        else
            _view.resized();
    }
    
    public boolean canShow(SyndieURI uri) { 
        if (uri == null) return false;
        String type = uri.getType();
        if (!type.equals(BrowserTab.TYPE_MANAGE) && !type.equals(BrowserTab.TYPE_META) && !type.equals("channel"))
            return false;
        
        Hash scope = uri.getScope();
        if (scope == null)
            scope = uri.getHash("scope");
        if (uri.getMessageId() != null)
            return false;
        if (uri.isSearch())
            return false;
        if (_editable)
            return _manage.canShow(scope);
        else
            return _view.canShow(scope);
    }
}
