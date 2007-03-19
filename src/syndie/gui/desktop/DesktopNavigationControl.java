package syndie.gui.desktop;

import java.util.List;
import syndie.data.SyndieURI;
import syndie.gui.NavigationControl;

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
            _desktop.show(panel, uri);
        } else {
            panel = createPanel(uri, suggestedName, suggestedDescription);
            if (panel != null)
                _desktop.show(panel, uri);
        }
    }
    
    private DesktopPanel createPanel(SyndieURI uri, String name, String desc) {
        if (uri == null) return null;
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
