package syndie.db;

import java.util.*;

/**
 * summarize the differences between the index and the local database
 */
public class ArchiveDiff {
    // class fields are being exposed directly contrary to good standards so that
    // the archive index and syndicators can simply rework the data.  it is
    // package scoped though, so the tight coupling isn't too bad
    
    /** how many new channels the index has that we do not */
    int totalNewChannels;
    /** how many new messages the index has that we do not */
    int totalNewMessages;
    /** how many new messages they have that we do not */
    int totalNewMessagesOnKnownChannels;
    /** hopefully pretty self-explanatory */
    int totalKnownChannelsWithNewMessages;
    /** channels that we know whose metadata has been updated remotely */
    int totalUpdatedChannels;
    /** if we wanted to only fetch things we did not already have, how much data would we fetch? */
    long fetchNewBytes;
    /** if we wanted to only fetch things we did not already have, how many metadata messages would we fetch? contains SyndieURIs*/
    List fetchNewMetadata;
    /** if we wanted to only fetch things we did not already have, how many posts would we fetch? contains SyndieURIs */
    List fetchNewPosts;
    /** if we wanted to only fetch things we did not already have, how many replies would we fetch?  contains SyndieURIs */
    List fetchNewReplies;
    /** if we wanted to only fetch posts on channels known locally, how much data would we fetch?  contains SyndieURIs */
    long fetchKnownBytes;
    /** if we wanted to only fetch posts on channels known locally, how many metadata messages would we fetch?  contains SyndieURIs */
    List fetchKnownMetadata;
    /** if we wanted to only fetch posts on channels known locally, how many posts would we fetch?  contains SyndieURIs */
    List fetchKnownPosts;
    /** if we wanted to only fetch posts on channels known locally, how many replies would we fetch?  contains SyndieURIs */
    List fetchKnownReplies;
    /** if we wanted to only fetch updated metatdata, how much data would we fetch?  */
    long fetchMetaBytes;
    /** if we wanted to only fetch updated metadata, how many metadata messages would we fetch?  contains SyndieURIs */
    List fetchMetaMessages;
    /**
     * if we wanted to fetch all of the information the archive marks as "new", even if
     * we already have it locally (as a crude form of information-theoretic anonymity via
     * private information retrieval), how much data would we need to download?
     */
    long fetchPIRBytes;
    /**
     * if we wanted to fetch all of the information the archive marks as "new", even if
     * we already have it locally (as a crude form of information-theoretic anonymity via
     * private information retrieval), how many metadata messages would we need to download?
     */
    List fetchPIRMetadata;
    /**
     * if we wanted to fetch all of the information the archive marks as "new", even if
     * we already have it locally (as a crude form of information-theoretic anonymity via
     * private information retrieval), how many posts would we need to download?
     */
    List fetchPIRPosts;
    /**
     * if we wanted to fetch all of the information the archive marks as "new", even if
     * we already have it locally (as a crude form of information-theoretic anonymity via
     * private information retrieval), how many replies would we need to download?
     */
    List fetchPIRReplies;
    /** if we wanted to only fetch new unauthorized posts, how much data would we fetch? */
    long fetchNewUnauthorizedBytes;
    /** if we wanted to only fetch new unauthorized posts, how many metadata messages would we fetch? */
    List fetchNewUnauthorizedMetadata;
    /** if we wanted to only fetch new unauthorized posts, how many posts would we fetch? */
    List fetchNewUnauthorizedPosts;
    /** if we wanted to only fetch new unauthorized posts, how many replies would we fetch? */
    List fetchNewUnauthorizedReplies;
    
    /** what was the max message size used when calculating the diff */
    long maxSizeUsed;

    public ArchiveDiff() {
        fetchNewMetadata = new ArrayList();
        fetchNewPosts = new ArrayList();
        fetchNewReplies = new ArrayList();
        fetchKnownMetadata = new ArrayList();
        fetchKnownPosts = new ArrayList();
        fetchKnownReplies = new ArrayList();
        fetchMetaMessages = new ArrayList();
        fetchPIRMetadata = new ArrayList();
        fetchPIRPosts = new ArrayList();
        fetchPIRReplies = new ArrayList();
        fetchNewUnauthorizedMetadata = new ArrayList();
        fetchNewUnauthorizedPosts = new ArrayList();
        fetchNewUnauthorizedReplies = new ArrayList();
        maxSizeUsed = -1;
    }

    /** SyndieURI instances of the URIs to fetch if only grabbing ones we don't have */
    public List getFetchNewURIs(boolean includeReplies) {
        List rv = new ArrayList();
        rv.addAll(fetchNewMetadata);
        rv.addAll(fetchNewPosts);
        if (includeReplies)
            rv.addAll(fetchNewReplies);
        return rv;
    }
    /** SyndieURI instances of the URIs to fetch if only grabbing ones on channels known locally */
    public List getFetchKnownURIs(boolean includeReplies) {
        List rv = new ArrayList();
        rv.addAll(fetchKnownMetadata);
        rv.addAll(fetchKnownPosts);
        if (includeReplies)
            rv.addAll(fetchKnownReplies);
        return rv;
    }
    /** SyndieURI instances of the URIs to fetch if only grabbing updated metadata */
    public List getFetchMetaURIs() {
        List rv = new ArrayList();
        rv.addAll(fetchMetaMessages);
        return rv;
    }
    /** SyndieURI instances of the URIs to fetch if only grabbing PIR style */
    public List getFetchPIRURIs() {
        List rv = new ArrayList();
        rv.addAll(fetchPIRMetadata);
        rv.addAll(fetchPIRPosts);
        rv.addAll(fetchPIRReplies);
        return rv;
    }
    /** SyndieURI instances of the URIs to fetch if only grabbing new unauthorized ones */
    public List getFetchNewUnauthorizedURIs(boolean includeReplies) {
        List rv = new ArrayList();
        rv.addAll(fetchNewUnauthorizedMetadata);
        rv.addAll(fetchNewUnauthorizedPosts);
        if (includeReplies)
            rv.addAll(fetchNewUnauthorizedReplies);
        return rv;
    }
}

