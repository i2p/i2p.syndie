package syndie.gui;

import syndie.data.SyndieURI;

public interface NavigationControl {
    public void view(SyndieURI uri);
    public void view(SyndieURI uri, String suggestedName, String suggestedDescription);
    public void unview(SyndieURI uri);
    public void resumePost(long postponeId, int postponeVersion);
    
    public void showWaitCursor(boolean wait);
}
