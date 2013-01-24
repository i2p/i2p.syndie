package syndie;

import net.i2p.data.Base64;
import net.i2p.data.Hash;

import syndie.data.SyndieURI;

/**
 * ugly centralized place to put shared constants.  who needs ooad?
 */
public class Constants {

    /** header line in the enclosure before the body specifying the body size */
    public static final String MSG_HEADER_SIZE = "Size";
    
    /** first line of the enclosure must start with this prefix for it to be supported */
    public static final String TYPE_PREFIX = "Syndie.Message.1.";
    /** the type line we use when we can choose */
    public static final String TYPE_CURRENT = TYPE_PREFIX + "0";
    
    /** what type of message is it? */
    public static final String MSG_HEADER_TYPE = "Syndie.MessageType";
    
    /** msg_header_type value for normal content-bearing posts */
    public static final String MSG_TYPE_POST = "post";
    /** msg_header_type value for posts updating channel metadata */
    public static final String MSG_TYPE_META = "meta";
    /** msg_header_type value for posts encrypted to the channel reply key */
    public static final String MSG_TYPE_REPLY = "reply";
    
    public static final String MSG_META_HEADER_IDENTITY = "Identity";
    public static final String MSG_META_HEADER_MANAGER_KEYS = "ManagerKeys";
    public static final String MSG_META_HEADER_POST_KEYS = "AuthorizedKeys";
    public static final String MSG_META_HEADER_EDITION = "Edition";
    public static final String MSG_META_HEADER_ENCRYPTKEY = "EncryptKey";
    public static final String MSG_META_HEADER_NAME = "Name";
    public static final String MSG_META_HEADER_DESCRIPTION = "Description";
    public static final String MSG_META_HEADER_PUBLICPOSTING = "PublicPosting";
    public static final String MSG_META_HEADER_PUBLICREPLY = "PublicReplies";
    public static final String MSG_META_HEADER_TAGS = "Tags";
    public static final String MSG_META_HEADER_ARCHIVES = "Archives";
    public static final String MSG_META_HEADER_READKEYS = "ChannelReadKeys";
    
    /** list of posts to be cancelled (if authorized) */
    public static final String MSG_META_HEADER_CANCEL = "Cancel";
    
    public static final String MSG_HEADER_BODYKEY = "BodyKey";
    /**
     * if specified, the answer to the given question is fed into the password-based-encryption
     * algorithm to derive the body's read key
     */
    public static final String MSG_HEADER_PBE_PROMPT = "BodyKeyPrompt";
    public static final String MSG_HEADER_PBE_PROMPT_SALT = "BodyKeyPromptSalt";
    
    /** URI the message is posted under */
    public static final String MSG_HEADER_POST_URI = "PostURI";
    /**
     * in case the channel in the postURI is not the channel that the post should
     * be displayed in (eg an unauthorized post, or a reply)
     */
    public static final String MSG_HEADER_TARGET_CHANNEL = "TargetChannel";
    /** tab delimited list of URIs the message is in reply to, most recent first */
    public static final String MSG_HEADER_REFERENCES = "References";
    /** URI the post is supposed to replace */
    public static final String MSG_HEADER_OVERWRITE = "Overwrite";
    /** If true, act as if this is the beginning of a new discussion thread */
    public static final String MSG_HEADER_FORCE_NEW_THREAD = "ForceNewThread";
    /** If true, only allow the poster to reply to the message */
    public static final String MSG_HEADER_REFUSE_REPLIES = "RefuseReplies";
    /** list of posts to be cancelled (if authorized) */
    public static final String MSG_HEADER_CANCEL = "Cancel";
    /** post subject */
    public static final String MSG_HEADER_SUBJECT = "Subject";
    /** suggested post expiration */
    public static final String MSG_HEADER_EXPIRATION = "Expiration";
    /** for multiauthor channels, we specify what nym we are authenticating ourselves with in the headers */
    public static final String MSG_HEADER_AUTHOR = "Author";
    /**
     * if we are hiding what nym posted the message inside the headers, xor the 
     * actual authentication signature with this random AuthenticationMask to prevent
     * confirmation attacks
     */
    public static final String MSG_HEADER_AUTHENTICATION_MASK = "AuthenticationMask";
    
    /** key can be used to read posts to a channel or its encrypted metadata */
    public static final String KEY_FUNCTION_READ = "read";
    /** key can be used to post metadata messages, etc */
    public static final String KEY_FUNCTION_MANAGE = "manage";
    /** key can be used to decrypt replies to a channel */
    public static final String KEY_FUNCTION_REPLY = "reply";
    /** key can be used to authorize normal posts without the poster necessarily authenticating themselves */
    public static final String KEY_FUNCTION_POST = "post";
    /** key can be used to insert under a freenet SSK keyspace.  the scope of the associated nymkeys is the hash of the SSK pubkey */
    public static final String KEY_FUNCTION_SSKPRIV = "sskpriv";
    public static final String KEY_TYPE_AES256 = "AES256";
    public static final String KEY_TYPE_DSA = "DSA";
    public static final String KEY_TYPE_ELGAMAL2048 = "ELGAMAL2048";

    public static final Boolean DEFAULT_ALLOW_PUBLIC_POSTS = Boolean.FALSE;
    public static final Boolean DEFAULT_ALLOW_PUBLIC_REPLIES = Boolean.FALSE;

    public static final String MSG_PAGE_CONTENT_TYPE = "Content-type";
    public static final String MSG_PAGE_TITLE = "Title";
    public static final String MSG_ATTACH_CONTENT_TYPE = "Content-type";
    public static final String MSG_ATTACH_NAME = "Name";
    public static final String MSG_ATTACH_DESCRIPTION = "Description";
    public static final String MSG_HEADER_TAGS = "Tags";

    /** max size in bytes */
    public static final int MAX_AVATAR_SIZE = 16*1024;
    /** max width in pixels */
    public static final int MAX_AVATAR_WIDTH = 64;
    /** max height in pixels */
    public static final int MAX_AVATAR_HEIGHT = 64;

    /** how many messages to cancel in each metadata (at most) */
    public static final int MAX_CANCELLED_PER_META = 64;
    /** how many days to keep cancel messages around (in case the message arrives after the cancel) */
    public static final int MAX_CANCELLED_HISTORY_DAYS = 31;
    
    public static final String FILENAME_SUFFIX = ".syndie";

    public static final String URI_ARCHIVE_PASSPHRASE = "passphrase";

    public static final long MAX_ATTACHMENT_SIZE = 4*1024*1024;
    
    public static final String REF_TYPE_BANNED = "banned";

    /** this refers to the tour uri - this url is just there for testing until the tour is written */
    public static final SyndieURI TOUR_MSG = SyndieURI.createMessage(new Hash(Base64.decode("Jx3gl5m8XzjIBcYVTNbVG1VlVb0nAXP41d-MbvlxPbY=")), 1187120476998l);
    /** this refers to the (context-sensitive) help uri - this url is just there for testing until the help is written */
    public static final SyndieURI HELP_MSG = SyndieURI.createMessage(new Hash(Base64.decode("bF2lursCrXhSECJAEILhtXYqQ6o-TwjlEUNJLA5Nu8o=")), 1187210851042l);
}
