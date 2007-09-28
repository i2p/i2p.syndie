package syndie.db;

import java.util.*;
import net.i2p.data.Base64;

/**
 */
public class Opts {
    private String _command;
    private Map _opts;
    private List _args;
    private int _size;
    private boolean _parseOk;
    private String _origLine;
    
    /**
     * Parse out a list of string[]s into a multivalued mapping of 0 or more (--name value)
     * options, followed by a list of 0 or more arguments.  the options end when an option
     * doesn't begin with "--" or when an option has no name (e.g. "--opt1 val1 -- arg1")
     */
    public Opts(String cmd, String args[]) {
        _command = cmd;
        _parseOk = parse(args);
    }
    public Opts(Opts old) {
        _command = old._command;
        _opts = new HashMap(old._opts);
        _args = new ArrayList(old._args);
        _size = old._size;
        _parseOk = old._parseOk;
    }
    public Opts() {
        _command = null;
        _opts = new HashMap();
        _args = new ArrayList();
        _size = 0;
        _parseOk = true;
    }
    /**
     * @param line unparsed command line (starting with the command to be run)
     */
    public Opts(String line) {
        this();
        _origLine = line;
        List elements = splitLine(line);
        
        if (elements.size() > 0) {
            _command = (String)elements.get(0);
            if (elements.size() > 1) {
                String elems[] = new String[elements.size()-1];
                for (int i = 0; i < elems.length; i++)
                    elems[i] = (String)elements.get(i+1);
                _parseOk = parse(elems);
            }
        }
    }
    public boolean parse(String args[]) {
        _opts = new HashMap();
        _args = new ArrayList();
        if (args == null) return false;
        int argBegin = args.length;
        try {
            for (int i = 0; i < argBegin; i+=2) {
                if (args[i].equals("--")) {
                    argBegin = i+1;
                    continue;
                } else if (args[i].startsWith("--")) {
                    String arg = args[i].substring("--".length());
                    if (i+1 >= args.length) {
                        _opts.clear();
                        _args.clear();
                        _size = 0;
                        return false;
                    }
                    String param = args[i+1];
                    List vals = (List)_opts.get(arg);
                    if (vals == null)
                        vals = new ArrayList();
                    vals.add(param);
                    _opts.put(arg, vals);
                    _size++;
                } else {
                    argBegin = i;
                }
            }
            for (int i = argBegin; i < args.length; i++) {
                _args.add(args[i]);
                _size++;
            }
            return true;
        } catch (ArrayIndexOutOfBoundsException e) {
            return false;
        }
    }
    public boolean getParseOk() { return _parseOk; }
    public String getCommand() { return _command; }
    public void setCommand(String cmd) { _command = cmd; }
    public String getOrigLine() { return _origLine; }
    public Set getOptNames() { return new HashSet(_opts.keySet()); }
    public String getOptValue(String name) {
        List vals = (List)_opts.get(name);
        if ( (vals != null) && (vals.size() > 0) )
            return (String)vals.get(0);
        else
            return null;
    }
    public List getOptValues(String name) { return (List)_opts.get(name); }
    public boolean getOptBoolean(String name, boolean defaultValue) {
        String val = getOptValue(name);
        if (val == null)
            return defaultValue;
        else
            return Boolean.valueOf(val).booleanValue();
    }
    public long getOptLong(String name, long defaultValue) {
        String val = getOptValue(name);
        if (val == null) {
            return defaultValue;
        } else {
            try {
                return Long.parseLong(val);
            } catch (NumberFormatException nfe) {
                return defaultValue;
            }
        }
    }
    public byte[] getOptBytes(String name) {
        String val = getOptValue(name);
        if (val == null) {
            return null;
        } else {
            return Base64.decode(val);
        }
    }
    public List getArgs() { return _args; }
    public String getArg(int index) {
        if ( (index >= 0) && (index < _args.size()) )
            return (String)_args.get(index);
        return null;
    }
    public int size() { return _size; }
    /** return list of missing options, or an empty list if we have all of the required options */
    public List requireOpts(String opts[]) {
        List missing = new ArrayList();
        for (int i = 0; i < opts.length; i++) {
            if (!_opts.containsKey(opts[i]))
                missing.add(opts[i]);
        }
        return missing;
    }

    public void setOptValue(String name, String val) { addOptValue(name, val); }
    public void addOptValue(String name, String val) {
        if ( (val == null) || (name == null) ) return;
        List vals = getOptValues(name);
        if (vals == null) {
            vals = new ArrayList();
            _opts.put(name, vals);
        }
        vals.add(val);
    }
    public void addArg(String val) {
        if (_args == null) _args = new ArrayList();
        _args.add(val);
        _size++;
    }
    public boolean dbOptsSpecified() {
        return ( (getOptValue("db") != null) &&
                 (getOptValue("login") != null) &&
                 (getOptValue("pass") != null));
    }
    
    public String toString() {
        StringBuffer buf = new StringBuffer();
        for (Iterator iter = _opts.keySet().iterator(); iter.hasNext(); ) {
            String name = (String)iter.next();
            String val = getOptValue(name);
            buf.append(name).append('=').append(val).append('\t');
        }
        return buf.toString();
    }
    
    public static void main(String args[]) {
        System.out.println("Starting tests");
        System.out.println(splitLine(" hi how are you?").toString());
        System.out.println(splitLine("I am fine, thanks! ").toString());
        System.out.println(splitLine("What you \"up to\" g?").toString());
        System.out.println(splitLine("\"y\'all had best answer\" me").toString());
        System.out.println(splitLine("a \"\" val \"\""));
        System.out.println(splitLine("\\\"you 'all had' \\\"best answer\\\" me").toString());
    }
    /**
     * split up the line into tokens, removing intertoken whitespace, grouping
     * quoted tokens, etc. 
     *
     * Works as an iterating state machine
     * */
    private static List splitLine(String line) {
        List rv = new ArrayList();
        if (line == null) return rv;
        char l[] = line.toCharArray();

        StringBuffer token = new StringBuffer();
        char quoteChar = 'Q';
        int cur = 0;

    	//0 = eat blank, 1 = parse word, 2 = parse quote
        int state = 0;

        while (cur < l.length) {
            if (l[cur]=='\\') {
                ++cur;
                // need goto
                token.append(l[cur]);
                ++cur;
                continue;
            }

       	    switch(state) {
	            case 0:
                    if (!isBlank(l[cur])) {
                        if (isQuote(l[cur])) {
                            quoteChar = l[cur];
                            state = 2;
                            //Do not include the quote character:
                            ++cur;
                            continue;
                        } else {
                            state = 1;
                            //Do include the non-blank word character:
                            break;
                        }
                   }
                   ++cur;
                   continue;
                case 1:
                    if (isBlank(l[cur])) {
                        state = 0;
                        rv.add(token.toString());
                        token = new StringBuffer();
                        //Can ignore the trailing blank
                        ++cur;
                        continue;
                    }
                    break;
                case 2:
                    if (quoteChar == l[cur]) {
                        /* See the closing quote character hasn't been
                         * added either yet */
                        state = 0;
                        rv.add(token.toString());
                        token = new StringBuffer();
                        ++cur;
                        continue;
                    }
                    break;
            }
            
            token.append(l[cur]);
            ++cur;
        }

        return rv;
    }

    private static boolean isBlank(char c) {
        switch (c) {
            case ' ':
            case '\t':
            case '\r':
            case '\n':
            case '\f':
                return true;
            default:
                return false;
        }
    }
    private static boolean isQuote(char c) {
        switch (c) {
            case '\'':
            case '\"':
                return true;
            default:
                return false;
        }
    }
}
