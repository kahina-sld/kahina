package org.kahina.tralesld.behavior;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.event.KahinaEvent;
import org.kahina.lp.behavior.LogicProgrammingTreeBehavior;
import org.kahina.tralesld.TraleSLDStepStatus;
import org.kahina.tralesld.control.event.TraleSLDBridgeEvent;
import org.kahina.tralesld.control.event.TraleSLDBridgeEventType;

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
    
    public void initializeParseTree(int stepID, String parsedSentence)
    {
        object.setRootID(stepID);
        object.addNode(stepID, "parsing " + parsedSentence, "", TraleSLDStepStatus.PROGRESS);
        secondaryTree.setRootID(stepID);
        lastActiveID = stepID;
    }
    
    public void processRuleApplication(int stepID, String ruleName)
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
            case TraleSLDBridgeEventType.INIT:
            {
                initializeParseTree(e.getExternalID(), e.getStrContent());
                break;
            }
        }
    }
}
