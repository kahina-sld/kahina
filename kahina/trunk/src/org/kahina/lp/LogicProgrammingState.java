package org.kahina.lp;

import java.util.HashSet;
import java.util.Set;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaState;
import org.kahina.core.data.text.KahinaLineReference;
import org.kahina.core.event.KahinaMessageEvent;
import org.kahina.lp.data.text.LogicProgrammingLineReference;

public class LogicProgrammingState extends KahinaState
{
    public LogicProgrammingState(KahinaInstance<? extends LogicProgrammingState, ?, ?> kahina, int dataHandlingMethod)
    {
        super(kahina, dataHandlingMethod);
    }
    
    public void breakpointConsoleMessage(int stepID, String message)
    {
        int lineID = consoleMessages.text.addLine(message);
        KahinaLineReference ref = new LogicProgrammingLineReference(consoleMessages,lineID,stepID,-1, -1);
        KahinaRunner.processEvent(new KahinaMessageEvent(ref));
    }
    
    public void consoleMessage(int stepID, int extID, int port, String message)
    {
        int lineID = consoleMessages.text.addLine(message);
        KahinaLineReference ref = new LogicProgrammingLineReference(consoleMessages,lineID,stepID,extID,port);
        Set<KahinaLineReference> refs = consoleLines.get(stepID);
        if (refs == null)
        {
            refs = new HashSet<KahinaLineReference>();
            consoleLines.put(stepID, refs);
        }
        refs.add(ref);
        //ref.store();
        KahinaRunner.processEvent(new KahinaMessageEvent(ref));
    }
    
    public void consoleMessage(LogicProgrammingLineReference ref)
    {
        Set<KahinaLineReference> refs = consoleLines.get(ref.step);
        if (refs == null)
        {
            refs = new HashSet<KahinaLineReference>();
            consoleLines.put(ref.step, refs);
        }
        refs.add(ref);
        //ref.store();
        KahinaRunner.processEvent(new KahinaMessageEvent(ref));
    }
    
    public LogicProgrammingLineReference getConsoleLineRefForStep(int stepID)
    {
        Set<KahinaLineReference> refs = consoleLines.get(stepID);
        if (refs == null) return null;
        return (LogicProgrammingLineReference) refs.iterator().next();
    }
}
