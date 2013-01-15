package syndie.db;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.util.List;
import net.i2p.data.DataHelper;

class ControlServer implements CLI.Command {
    private ServerSocket _listenSocket;
    
    private UI _ui;
    private DBClient _client;
    
    private Boolean _shutdown = Boolean.valueOf(false);
    
    public DBClient runCommand(Opts opts, final UI ui, DBClient client) {
        _client = client;
        _ui = ui;
        
        int port = (int) opts.getOptLong("port", 10111);
        
        try {
            _listenSocket = new ServerSocket(port, 0, InetAddress.getLocalHost());
            //_listenSocket.setSoTimeout(1000); // why?
            
            Thread listenThread = new Thread(new Runnable() {
                public void run() {
                    boolean shutdown = _shutdown.booleanValue();
                    while (!shutdown) {
                        try {
                            _ui.debugMessage("ControlServer: waiting for connection");
                            Thread clientThread = new ControlServerClientHandler(ui, _listenSocket.accept(), false);
                            clientThread.start();
                            _ui.debugMessage("ControlServer: connection accepted");
                        } catch (SocketTimeoutException e) {
                            synchronized(_shutdown) { shutdown = _shutdown.booleanValue(); }
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    }
                    
                    try { _listenSocket.close(); } catch (IOException e) {}
                }
            }, "ControlServerListener");
            listenThread.setDaemon(true);
            listenThread.start();
            
            _ui.debugMessage("Created ControlServerListener on port " + port);
            _ui.commandComplete(0, null);
        } catch (IOException e) {
            _ui.errorMessage("Failed to create ControlServerListener on port " + port, e);
            _ui.commandComplete(-1, null);
        }
        
        return client;
    }
    
    private class ControlServerClientHandler extends Thread implements UI {
        private UI _ui;
        
        private Socket _clientSocket;
        private InputStream _inputStream;
        private OutputStream _outputStream;
        
        private boolean _debug;
        
        private boolean _shutdown;
        
        ControlServerClientHandler(UI ui, Socket clientSocket, boolean debug) throws IOException {
            _ui = ui;
            _ui.addUI(this);
            
            _clientSocket = clientSocket;
            _inputStream = _clientSocket.getInputStream();
            _outputStream = _clientSocket.getOutputStream();
            
            _debug = debug;
            
            _shutdown = false;
        }
        
        public void run() {
            try {
                String command = DataHelper.readLine(_inputStream);
                
                while (!_shutdown && command != null) {
                    _ui.debugMessage("ControlServer recvd a command: " + command);
                    _ui.insertCommand(command);

                    command = DataHelper.readLine(_inputStream);
                }
            } catch (IOException e) {
                _shutdown = true;
            }
            
            try { _inputStream.close(); } catch (IOException e) {}
            try { _outputStream.close(); } catch (IOException e) {}
            try { _clientSocket.close(); } catch (IOException e) {}
            
            _ui.removeUI(this);
       }
        
        void send(String s, Exception e) {
            StringBuffer b = new StringBuffer();
            
            if (s != null) {
                b.append(s);
                b.append('\n');
            }
            
            if (e != null) {
                StringWriter out = new StringWriter();
                e.printStackTrace(new PrintWriter(out));
                b.append(out.toString());
                b.append('\n');
            }
            
            try { _outputStream.write(b.toString().getBytes()); } catch (IOException ioe) { _shutdown = true; }
        }
        
        public void errorMessage(String msg) { send(msg, null); }
        public void errorMessage(String msg, Exception cause) { send(msg, cause); }
        public void statusMessage(String msg) { send(msg, null); }
        public void debugMessage(String msg) { if (_debug) send(msg, null); }
        public void debugMessage(String msg, Exception cause) { if (_debug) send(msg, cause); }
        public void commandComplete(final int status, final List location) {
            StringBuffer buf = new StringBuffer();
            buf.append("* Command execution complete.\n");
            buf.append("* Status: ");
            buf.append(status);
            buf.append("\n* Location: ");
            if (location != null) {
                for (int i = 0; i < location.size(); i++) {
                    buf.append(location.get(i).toString());
                    buf.append("> ");
                }
            }
            send(buf.toString(), null);
        }

        /*
         * This class is based on TextUITab
         * That's how I know it's OK not to implement these UI methods
         */
        public Opts readCommand() { return null; }
        public Opts readCommand(boolean displayPrompt) { return null; }
        public boolean toggleDebug() { return false; }
        public boolean togglePaginate() { return false; }
        public void insertCommand(String commandline) {}
        public String readStdIn() { return null; }
        public void addUI(UI ui) {}
        public void removeUI(UI ui) {}
    }
}
