package org.kahina.tralesld.behavior;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.breakpoint.KahinaBreakpoint;
import org.kahina.core.breakpoint.KahinaBreakpointType;
import org.kahina.core.breakpoint.TreeNodePattern;
import org.kahina.core.breakpoint.TreePattern;
import org.kahina.core.breakpoint.TreePatternNode;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.event.KahinaEvent;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.behavior.LogicProgrammingTreeBehavior;
import org.kahina.tralesld.TraleSLDStepStatus;
import org.kahina.tralesld.TraleSLDStepType;
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
	private static final boolean verbose = false;
	
    public TraleSLDTreeBehavior(KahinaTree tree, KahinaInstance<?, ?, ?> kahina, KahinaTree secondaryTree)
    {
        super(tree, kahina, secondaryTree);  
        KahinaRunner.getControl().registerListener("traleSLD bridge", this);
    }
    
    public void initializeSkipPoints()
    {
        TreePattern pat = new TreePattern();
        TreePatternNode rootNode = new TreePatternNode();
        TreeNodePattern rootPattern = new TreeNodePattern(TreeNodePattern.CAPTION, TreeNodePattern.MATCHING, "[0-9]* (unify|featval|type).*");    
        rootNode.setPattern(rootPattern);
        pat.setRoot(rootNode);
        KahinaBreakpoint bp = new KahinaBreakpoint(KahinaBreakpointType.SKIP_POINT);
        bp.setName("unify/featval/type Skip");
        bp.setPattern(pat);
        ((LogicProgrammingState) kahina.getState()).getSkipPoints().add(bp); // TODO embeds interesting when's, what to do about it?
        //System.err.println(aut.toString());
        TreePattern pat2 = new TreePattern();
        TreePatternNode rootNode2 = new TreePatternNode();
        TreeNodePattern rootPattern2 = new TreeNodePattern(TreeNodePattern.CAPTION, TreeNodePattern.MATCHING, "[0-9]* lex\\(.*");    
        rootNode2.setPattern(rootPattern2);
        pat2.setRoot(rootNode2);
        KahinaBreakpoint bp2 = new KahinaBreakpoint(KahinaBreakpointType.SKIP_POINT);
        bp2.setName("Lex Detail Skip");
        bp2.setPattern(pat2);
        ((LogicProgrammingState) kahina.getState()).getSkipPoints().add(bp2);
    }
    
    public void initializeCreepPoints()
    {
        TreePattern pat = new TreePattern();
        TreePatternNode rootNode = new TreePatternNode();
        TreeNodePattern rootPattern = new TreeNodePattern(TreeNodePattern.CAPTION, TreeNodePattern.MATCHING, "[0-9]* lex\\(.*");    
        rootNode.setPattern(rootPattern);
        pat.setRoot(rootNode);
        KahinaBreakpoint bp = new KahinaBreakpoint(KahinaBreakpointType.CREEP_POINT);
        bp.setName("Failed Lex Creep");
        bp.setPattern(pat);
        ((LogicProgrammingState) kahina.getState()).getCreepPoints().add(bp);
        //System.err.println(aut.toString());
    }
    
    public void initializeParseTree(int stepID, String parsedSentence)
    {
        object.setRootID(stepID);
        object.addNode(stepID, "parsing " + parsedSentence, "", TraleSLDStepStatus.PROGRESS);
        secondaryTree.setRootID(stepID);
        secondaryTree.addNode(stepID, "parsing " + parsedSentence, "", TraleSLDStepStatus.PROGRESS);
        lastActiveID = stepID;
    }
    
    public void processRuleApplication(int stepID, int externalID, String ruleName)
    {
    	String caption = externalID + " rule(" + ruleName + ")";
        object.addNode(stepID, caption, "", TraleSLDStepStatus.PROGRESS);  
        secondaryTree.addNode(stepID, caption, "", TraleSLDStepStatus.PROGRESS);   
    }
    
    /**
     * registers and reacts to a finished step
     * @param externalID - the ID of the step that was finished in the monitored logic programming system
     */
    public void processStepFinished(int stepID)
    {
        if (verbose) System.err.println("TraleSLDTreeBehavior.processStepFinished(" + stepID + ")");
        stepBeingRedone = -1;
        deterministicallyExited.add(stepID);
        object.setNodeStatus(stepID,TraleSLDStepType.FINISHED);
        lastActiveID = object.getParent(stepID);
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
                processRuleApplication(e.getInternalID(), e.getIntContent(), e.getStrContent());
                break;
            }
            case TraleSLDBridgeEventType.INIT:
            {
                initializeParseTree(e.getInternalID(), e.getStrContent());
                break;
            }
            case TraleSLDBridgeEventType.STEP_FINISHED:
            {
                processStepFinished(e.getInternalID());
                break;
            }
        }
    }
}
