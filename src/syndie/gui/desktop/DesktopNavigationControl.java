package syndie.gui.desktop;

import java.net.URISyntaxException;
import java.util.List;
import net.i2p.data.Hash;
import syndie.data.SyndieURI;
import syndie.gui.BrowserTab;
import syndie.gui.NavigationControl;
import syndie.gui.URIHelper;

public class DesktopNavigationControl implements NavigationControl {
    private Desktop _desktop;
    
    public DesktopNavigationControl(Desktop desktop) {
        _desktop = desktop;
    }

    public void view(SyndieURI uri) { view(uri, null, null); }
    public void view(SyndieURI uri, String suggestedName, String suggestedDescription) {
        _desktop.getUI().debugMessage("view: " + uri);
        DesktopPanel panel = getPanel(uri);
        if (panel != null) {
            _desktop.show(panel, uri, suggestedName, suggestedDescription);
        } else {
            panel = createPanel(uri, suggestedName, suggestedDescription);
            if (panel != null)
                _desktop.show(panel, uri, suggestedName, suggestedDescription);
        }
    }
    
    private DesktopPanel createPanel(SyndieURI uri, String name, String desc) {
        if (uri == null) return null;
        // we have already checked and none of the existing message tree panels can show
        // this uri
        if ( (uri.isChannel() && uri.getMessageId() == null) || (uri.isSearch()) ) {
            //System.out.println("creating a new message tree panel for " + uri);
            return new MessageTreePanel(_desktop, uri);
        } else if (uri.isChannel() && (uri.getMessageId() != null)) {
            return new MessagePanel(_desktop, _desktop.getDBClient(), _desktop.getThemeRegistry(), _desktop.getTranslationRegistry(), _desktop.getCenter(), _desktop.getUI(), _desktop.getNavControl());
        } else if (BrowserTab.TYPE_POST.equals(uri.getType())) {
            Long postponeId = uri.getLong("postponeid");
            Long postponeVer = uri.getLong("postponever");
            if ( (postponeId != null) && (postponeVer != null) ) {
                return new MessageEditorPanel(_desktop, _desktop.getDBClient(), _desktop.getUI(), _desktop.getThemeRegistry(), _desktop.getTranslationRegistry(), _desktop.getCenter(), postponeId.longValue(), postponeVer.intValue(), uri);
            } else {
                Hash targetForum = uri.getHash("channel");
                String parentStr = uri.getString("parent");  
                String asReplyStr = uri.getString("reply");
                SyndieURI parent = null;
                boolean asReply = false;
                if ( (asReplyStr != null) && (Boolean.valueOf(asReplyStr).booleanValue()) )
                    asReply = true;
                if (parentStr != null)
                    try { parent = new SyndieURI(parentStr); } catch (URISyntaxException use) {}
                
                return new MessageEditorPanel(_desktop, _desktop.getDBClient(), _desktop.getUI(), _desktop.getThemeRegistry(), _desktop.getTranslationRegistry(), _desktop.getCenter(), targetForum, parent, asReply, uri);
            }
        } else if (BrowserTab.TYPE_SYNDICATE_STATUS.equals(uri.getType()) || uri.isArchive()) {
            return new SyndicatorPanel(_desktop, _desktop.getDBClient(), _desktop.getThemeRegistry(), _desktop.getTranslationRegistry(), _desktop.getCenter(), _desktop.getUI(), uri);
        }
        _desktop.getUI().errorMessage("don't know how to view: " + uri + ", punting it to the tabs");
        return _desktop.getTabPanel(true);
    }
    
    public void unview(SyndieURI uri) {
        DesktopPanel panel = getPanel(uri);
        if (panel != null) {
            panel.close();
        } else if (uri != null) {
            TabPanel tabs = _desktop.getTabPanel(false);
            if (tabs != null)
                tabs.unview(uri);
        }
    }
    
    private DesktopPanel getPanel(SyndieURI uri) {
        List panels = _desktop.getPanels();
        for (int i = 0; i < panels.size(); i++) {
            DesktopPanel panel = (DesktopPanel)panels.get(i);
            if (panel.canShow(uri))
                return panel;
        }
        return null;
    }

    public void resumePost(long postponeId, int postponeVersion) {}
    public void showWaitCursor(boolean wait) {}
}
