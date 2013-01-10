package syndie.gui;

import java.util.ArrayList;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 *
 */
abstract public class Wizard implements Themeable, Translatable {
    private Display _display;
    private Shell _shell;
    
    private Composite _stack;
    private StackLayout _stackLayout;
    private Label _separator;
    private Button _back;
    private GridData _backData;
    private Button _next;
    private GridData _nextData;
    private Button _finish;
    private GridData _finishData;
    private Button _cancel;
    private GridData _cancelData;
    
    private ArrayList _pages;
    private int currentPage = 0;
    
    private static final int _buttonWidth = 100;
    private static final int MAX_WIDTH = 900;
    private static final int MAX_HEIGHT = 450;
    
    public Wizard(Display display) {
        final int numColumns = 4;

        _display = display;
        
        _shell = new Shell(_display, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL | SWT.SHELL_TRIM);
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; close(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        _shell.setLayout(new GridLayout(numColumns, false));
        _shell.setImage(ImageUtil.ICON_SHELL);

        _stack = new Composite(_shell, SWT.NONE);
        _stack.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, numColumns, 1));
        _stackLayout = new StackLayout();
        _stack.setLayout(_stackLayout);
        
        _separator = new Label(_shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        _separator.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, numColumns, 1));
        
        _backData = new GridData(SWT.END, SWT.CENTER, true, false);
        _backData.widthHint = _buttonWidth;
        _back = new Button(_shell, SWT.PUSH);
        _back.setLayoutData(_backData);
        _back.addSelectionListener(new FireSelectionListener() { public void fire() { back(); } });
        
        _nextData = new GridData(SWT.END, SWT.CENTER, false, false);
        _nextData.widthHint = _buttonWidth;
        _next = new Button(_shell, SWT.PUSH);
        _next.setLayoutData(_nextData);
        _next.addSelectionListener(new FireSelectionListener() { public void fire() { next(); } });
        
        _finishData = new GridData(SWT.END, SWT.CENTER, false, false);
        _finishData.widthHint = _buttonWidth;
        _finish = new Button(_shell, SWT.PUSH);
        _finish.setLayoutData(_finishData);
        _finish.addSelectionListener(new FireSelectionListener() { public void fire() { finish(); } });
        
        _cancelData = new GridData(SWT.END, SWT.CENTER, false, false);
        _cancelData.widthHint = _buttonWidth;
        _cancel = new Button(_shell, SWT.PUSH);
        _cancel.setLayoutData(_cancelData);
        _cancel.addSelectionListener(new FireSelectionListener() { public void fire() { cancel(); } });
        
        _pages = new ArrayList();
    }
    
    void open() {
        _shell.pack();
        
        Rectangle screenBounds = _display.getPrimaryMonitor().getBounds();
        int width = Math.min(screenBounds.width, MAX_WIDTH);
        int height = Math.min(screenBounds.height, MAX_HEIGHT);
        int x = screenBounds.x + (screenBounds.width - width) / 2;
        int y = screenBounds.y + (screenBounds.height - height) / 2;
        _shell.setBounds(x, y, width, height);
        
        _shell.open();
    }
    
    abstract void save();
    
    void close() {
        _shell.dispose();
    }
    
    void next() {
        if (currentPage < (_pages.size() - 1))
            currentPage++;
        
        update();
    }
    
    void back() {
        if (currentPage > 0)
            currentPage--;
        
        update();
    }
    
    void finish() {
        save();
        close();
    }
    
    void cancel() {
        close();
    }
    
    void update() {
        boolean firstPageShown = (currentPage == 0);
        boolean lastPageShown = (currentPage == (_pages.size() - 1));
        
        _back.setEnabled(!firstPageShown);
        _next.setEnabled(!lastPageShown);
        _finish.setEnabled(lastPageShown);
        _cancel.setEnabled(false);
        
        Page pp = (Page) _stackLayout.topControl;
        Page cp = (Page) _pages.get(currentPage);
        
        if (pp != cp) {
            _stackLayout.topControl = cp;
            
            if (pp != null) pp.hidden();
            if (cp != null) cp.shown();
        }
        
        _stack.layout();
    }
    
    public Shell getShell() { return _shell; }
    
    public void applyTheme(Theme theme) {
        _back.setFont(theme.BUTTON_FONT);
        _next.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
    }
    
    
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText("Welcome to Syndie!"));
        _back.setText(registry.getText("< Back"));
        _next.setText(registry.getText("Next >"));
        _finish.setText(registry.getText("Finish"));
        _cancel.setText(registry.getText("Cancel"));
    }
    
    class Page extends Composite {
        private ArrayList _listeners;
        
        Page() {
            super(_stack, SWT.NONE);
            constructor(5, 5);
        }
        
        Page(int marginWidth, int marginHeight) {
            super(_stack, SWT.NONE);
            constructor(marginWidth, marginHeight);
        }
        
        private void constructor(int marginWidth, int marginHeight) {
            _listeners = new ArrayList();
            
            FillLayout pageLayout = new FillLayout();
            pageLayout.marginWidth = marginWidth;
            pageLayout.marginHeight = marginHeight;
            this.setLayout(pageLayout);
            
            _pages.add(this);
        }
        
        public void addPageListener(PageListener listener) {
            _listeners.add(listener);
        }
        
        public void removePageListener(PageListener listener) {
            _listeners.remove(_listeners.indexOf(listener));
        }
        
        public void shown() {
            int length = _listeners.size();
            for (int c = 0; c < length; c++)
                ((PageListener) _listeners.get(c)).shown();
        }
        
        public void hidden() {
            int length = _listeners.size();
            for (int c = 0; c < length; c++)
                ((PageListener) _listeners.get(c)).hidden();
        }
    }
    
    private interface PageListener {
        public void shown();
        public void hidden();
    }
}
