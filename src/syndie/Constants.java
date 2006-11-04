package syndie;

import java.io.File;
import java.util.*;

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
    public static final String KEY_TYPE_AES256 = "AES256";
    public static final String KEY_TYPE_DSA = "DSA";
    public static final String KEY_TYPE_ELGAMAL2048 = "ELGAMAL2048";

    public static final Boolean DEFAULT_ALLOW_PUBLIC_POSTS = Boolean.FALSE;
    public static final Boolean DEFAULT_ALLOW_PUBLIC_REPLIES = Boolean.FALSE;

    public static final String MSG_PAGE_CONTENT_TYPE = "Content-type";
    public static final String MSG_ATTACH_CONTENT_TYPE = "Content-type";
    public static final String MSG_ATTACH_NAME = "Name";
    public static final String MSG_ATTACH_DESCRIPTION = "Description";
    public static final String MSG_HEADER_TAGS = "Tags";

    public static final int MAX_AVATAR_SIZE = 16*1024;

    public static final String FILENAME_SUFFIX = ".syndie";

    public static final String URI_ARCHIVE_PASSPHRASE = "passphrase";

    /** split on the given character, with the resulting tokens not including that character */
    public static final String[] split(char elem, String orig) { return split(""+elem, orig); }
    /** split on all of the characters in splitElements, with the resulting tokens not including that character */
    public static final String[] split(String splitElements, String orig) {
        List vals = new ArrayList();
        int off = 0;
        int start = 0;
        char str[] = orig.toCharArray();
        while (off < str.length) {
            if (splitElements.indexOf(str[off]) != -1) {
                if (off-start > 0) {
                    vals.add(new String(str, start, off-start));
                } else {
                    vals.add(new String(""));
                }
                start = off+1;
            }
            off++;
        }
        if (off-start > 0)
            vals.add(new String(str, start, off-start));
        else
            vals.add(new String(""));
        String rv[] = new String[vals.size()];
        for (int i = 0; i < rv.length; i++)
            rv[i] = (String)vals.get(i);
        return rv;
    }
    
    private static final String ALLOWED_FILENAME = "abcdefghijklmnopqrstuvwxyz" +
                                                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
                                                   "0123456789" +
                                                   "._-+";
    
    /** incredibly conservative filter, only allowing ascii alphanum and a few symbols */
    public static String stripFilename(String name, boolean allowPaths) {
        char rv[] = name.toCharArray();
        boolean mod = false;
        for (int i = 0; i < rv.length; i++) {
            if (allowPaths && ( (rv[i] == '/' || rv[i] == '\\') )) {
                rv[i] = File.separatorChar;
                mod = true;
            } else if (ALLOWED_FILENAME.indexOf(rv[i]) == -1) {
                rv[i] = '_';
                mod = true;
            }
        }
        if (!mod) return name;
        return new String(rv);
    }

    public static void main(String args[]) {
        String split[] = split('\n', "hi\nhow are you?\n\nw3wt\n\nthe above is a blank line");
        for (int i = 0; i < split.length; i++)
            System.out.println(split[i]);
    }
}
