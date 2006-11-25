package syndie.db;

import net.i2p.data.DataHelper;

/**
 * all of the contained ArchiveChannel and ArchiveMessage include the source
 * from which the data can be found
 *
 */
public class MergedArchiveIndex extends ArchiveIndex {
    public MergedArchiveIndex() {
        super();
    }
    
    /** what is one of the archives with the given channel metadata */
    public String getSource(ArchiveChannel chan) {
        if (chan instanceof MergedChannel)
            return ((MergedChannel)chan).getSource();
        else
            return null;
    }
    /** what is one of the archives with the given message */
    public String getSource(ArchiveMessage msg) {
        if (msg instanceof MergedMessage)
            return ((MergedMessage)msg).getSource();
        else
            return null;
    }
    
    public void merge(ArchiveIndex toAdd, String sourceName) {
        for (int i = 0; i < toAdd.getChannelCount(); i++) {
            ArchiveChannel chan = toAdd.getChannel(i);
            MergedChannel merged = null;
            for (int j = 0; j < getChannelCount(); j++) {
                ArchiveChannel c = getChannel(j);
                if (DataHelper.eq(c.getScope(), chan.getScope())) {
                    merged = (MergedChannel)c;
                    break;
                }
            }
            if (merged == null) {
                addChannel(new MergedChannel(chan, sourceName));
            } else {
                merge(merged, chan, sourceName);
            }
        }
    }
    private void merge(MergedChannel merged, ArchiveChannel orig, String sourceName) {
        if (orig.getVersion() > merged.getVersion()) {
            merged.setVersion(orig.getVersion());
            merged.setEntrySize(orig.getEntrySize());
            merged.setSource(sourceName);
        }
        
        for (int j = 0; j < orig.getMessageCount(); j++) {
            ArchiveMessage msg = orig.getMessage(j);
            if (!merged.isKnownMessage(msg))
                merged.addMessage(new MergedMessage(msg, sourceName));
        }
        
        for (int j = 0; j < orig.getPseudoAuthorizedMessageCount(); j++) {
            ArchiveMessage msg = orig.getPseudoAuthorizedMessage(j);
            if (!merged.isKnownPseudoAuthorizedMessage(msg))
                merged.addPseudoAuthorizedMessage(new MergedMessage(msg, sourceName));
        }
        
        for (int j = 0; j < orig.getUnauthorizedMessageCount(); j++) {
            ArchiveMessage msg = orig.getUnauthorizedMessage(j);
            if (!merged.isKnownUnauthorizedMessage(msg))
                merged.addUnauthorizedMessage(new MergedMessage(msg, sourceName));
        }
    }
}
