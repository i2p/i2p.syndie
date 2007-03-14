package syndie.gui;

import syndie.data.SyndieURI;

public interface LocalMessageCallback {        
    /** the message was created locally */
    public void messageCreated(SyndieURI postedURI);
    /** the message was postponed */
    public void messagePostponed(long postponementId);
    /** the message creation was cancelled */
    public void messageCancelled();
}
