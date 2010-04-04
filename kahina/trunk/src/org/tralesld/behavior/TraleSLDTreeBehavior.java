package org.tralesld.behavior;

import org.kahina.behavior.LogicProgrammingTreeBehavior;
import org.kahina.control.KahinaController;
import org.kahina.control.event.KahinaEvent;
import org.kahina.core.KahinaInstance;
import org.kahina.data.tree.KahinaTree;
import org.tralesld.control.event.TraleSLDBridgeEvent;
import org.tralesld.control.event.TraleSLDBridgeEventType;
import org.tralesld.core.TraleSLDStepStatus;

/**
 * This is supposed to contain the logic by which TraleSLD handles its step trees.
 * 
 * @author johannes
 *
 */

public class TraleSLDTreeBehavior extends LogicProgrammingTreeBehavior
{   
    public TraleSLDTreeBehavior(KahinaTree tree, KahinaController control, KahinaInstance kahina, KahinaTree secondaryTree)
    {
        super(tree, control, kahina, secondaryTree);  
        control.registerListener("traleSLD bridge", this);
    }
    
    public void processRuleApplication(int externalID, String ruleName)
    {
        int newID = object.addNode("rule(" + ruleName + ")", "", TraleSLDStepStatus.PROGRESS);   
        object.addChild(lastActiveID, newID);
        lastActiveID = newID;
    }
    
    public void processStepFail(int externalID)
    {
        
    }
    
    public void processEvent(KahinaEvent e)
    {
        super.processEvent(e);
        if (e instanceof TraleSLDBridgeEvent)
        {
            processEvent((TraleSLDBridgeEvent) e);
        }
    }
    
    public void processEvent(TraleSLDBridgeEvent e)
    {
        switch (e.getEventType())
        {
            case TraleSLDBridgeEventType.RULE_APP:
            {
                processRuleApplication(e.getExternalID(), e.getStrContent());
                break;
            }
        }
    }
}
