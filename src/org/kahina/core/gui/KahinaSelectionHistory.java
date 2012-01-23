package org.kahina.core.gui;

import java.util.LinkedList;

import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.gui.event.KahinaSelectionEvent;

public class KahinaSelectionHistory implements KahinaListener
{
    public static boolean VERBOSE = false;
    
    // TODO move history from GUI to state?
    
    LinkedList<Integer> selectionHistory;
    int pointInHistory;
    
    private boolean selectionCausedByHistory;
    
    public KahinaSelectionHistory(KahinaController control)
    {
        selectionHistory = new LinkedList<Integer>();
        pointInHistory = -1;
        selectionCausedByHistory = false;
        control.registerListener(KahinaEventTypes.SELECTION, this);
    }
    
    public boolean canMoveToPrevious()
    {
        return !(pointInHistory <= 0 || pointInHistory > selectionHistory.size() - 1);
    }
    
    public boolean canMoveToNext()
    {
        return !(pointInHistory < 0 || pointInHistory >= selectionHistory.size() - 1);
    }
    
    public void moveToPrevious()
    {
        if (canMoveToPrevious())
        {
            int newSelection = selectionHistory.get(--pointInHistory);
            selectionCausedByHistory = true;
            KahinaRunner.processEvent(new KahinaSelectionEvent(newSelection));
            selectionCausedByHistory = false;
        }
    }
    
    public void moveToNext()
    {
        if (canMoveToNext())
        {
            int newSelection = selectionHistory.get(++pointInHistory);
            selectionCausedByHistory = true;
            KahinaRunner.processEvent(new KahinaSelectionEvent(newSelection));
            selectionCausedByHistory = false;
        }
    }
    
    public void processEvent(KahinaEvent e)
    {
        if (e instanceof KahinaSelectionEvent)
        {
            processEvent((KahinaSelectionEvent) e);
        }
    }
    
    private void processEvent(KahinaSelectionEvent e)
    {
    	if (VERBOSE)
    	{
    		System.err.println(this + ".processEvent(" + e + ")");
    	}
        if (!selectionCausedByHistory)
        {
            if (selectionHistory.size() > 0 && pointInHistory != -1)
            {
                selectionHistory.subList(pointInHistory + 1, selectionHistory.size()).clear();
            }
            selectionHistory.add(e.getSelectedStep());
            pointInHistory = selectionHistory.size() - 1;
            if (VERBOSE) System.err.println("Selection History: " + selectionHistory);
        }
    }
}
