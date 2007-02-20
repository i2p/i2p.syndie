package syndie.data;

import syndie.db.UI;
import java.util.Arrays;

/**
 * utility to help time the operation of various components and processes
 */
public class Timer {
    private String _taskName;
    private UI _ui;
    private boolean _logImmediately;
    private long _created;
    private long _times[];
    private String _events[];
    private int _nextEventIndex;
    
    public Timer(String taskName, UI ui) { this(taskName, ui, false, -1); }
    /**
     * @param taskName identifies the timer in the logs
     * @param ui where to log (filed with debugMessage)
     * @param logImmediately if true, logs the events when they are generated, 
     *                       but if false, waits until complete is called (deferring string construction, etc)
     * @param expectedEvents estimated number of events expected, or -1 if unknown
     */
    public Timer(String taskName, UI ui, boolean logImmediately, int expectedEvents) {
        _taskName = taskName;
        _ui = ui;
        _logImmediately = logImmediately;
        _created = System.currentTimeMillis();
        if (expectedEvents > 0) {
            _times = new long[expectedEvents];
            _events = new String[expectedEvents];
        } else {
            _times = new long[8];
            _events = new String[8];
        }
        Arrays.fill(_times, -1);
        _nextEventIndex = 0;
    }
    
    public void addEvent(String eventName) {
        if (_nextEventIndex >= _times.length) {
            long times[] = new long[_times.length + 50];
            String events[] = new String[_times.length + 50];
            System.arraycopy(_times, 0, times, 0, _times.length);
            System.arraycopy(_events, 0, events, 0, _events.length);
            _times = times;
            _events = events;
        }
        _times[_nextEventIndex] = System.currentTimeMillis();
        _events[_nextEventIndex] = eventName;
        if (_logImmediately)
            log(_nextEventIndex);
        _nextEventIndex++;
    }
    
    public void complete() {
        if (!_logImmediately) {
            for (int i = 0; i < _nextEventIndex; i++)
                log(i);
        }
        //_events = null;
        //_times = null;
        //_nextEventIndex = -1;
    }
   
    private void log(int event) {
        long timeSinceStart = _times[event] - _created;
        StringBuffer buf = new StringBuffer(128);
        buf.append(_taskName).append(": ");
        if (timeSinceStart < 10*1000)
            buf.append(timeSinceStart).append("ms ");
        else
            buf.append(timeSinceStart/1000).append("s ");
        
        if (event == 0)
            buf.append("(").append(_times[event]-_created).append("): ");
        else
            buf.append("(").append(_times[event]-_times[event-1]).append("): ");
        buf.append(_events[event]);
        _ui.debugMessage(buf.toString());
    }
}
