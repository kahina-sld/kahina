package org.tralesld.behavior;

import java.util.List;

import org.kahina.behavior.KahinaTreeBehavior;
import org.kahina.data.tree.KahinaMemTree;
import org.kahina.data.tree.KahinaTree;
import org.kahina.util.PrologUtilities;

/**
 * This is supposed to contain the logic by which TraleSLD handles its step trees.
 * 
 * @author johannes
 *
 */

public class TraleSLDTreeBehavior extends KahinaTreeBehavior
{
    KahinaTree secondaryTree;
    
    int lastActiveID;
    
    public TraleSLDTreeBehavior(KahinaMemTree tree, KahinaTree secondaryTree)
    {
        super(tree);
        this.secondaryTree = secondaryTree;
        this.lastActiveID = -1;
    }
    
    /**
     * contains the logic by which the tree is formed out of callstacks
     * will later be called by an event processing routine for a custom Kahina event "new step"
     */
    public void processNewCallstack(String callStack)
    {
        //TODO: should reimplement most of TraleSld.registerStepLocation()
        List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
        int extStepID = stack.get(0);
        int stepID = extStepID;
        lastActiveID = stepID;
        secondaryTree.addChild(lastActiveID, extStepID);
        //TODO: this is nonsense at the moment
        ((KahinaTree) object).addChild(extStepID, stepID);
    }
}
