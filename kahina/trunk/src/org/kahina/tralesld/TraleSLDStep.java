package org.kahina.tralesld;

import org.kahina.core.KahinaRunner;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.tralesld.data.fs.TraleSLDFS;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;

public class TraleSLDStep extends LogicProgrammingStep
{
	/**
	 * 
	 */
	private static final long serialVersionUID = -43108052730328464L;

    public TraleSLDFS startFeatStruct;
    public TraleSLDFS endFeatStruct;
    public TraleSLDVariableBindingSet startBindings = new TraleSLDVariableBindingSet();
    public TraleSLDVariableBindingSet endBindings = new TraleSLDVariableBindingSet();
    
    public TraleSLDStep copy()
    {
    	TraleSLDStep copy = new TraleSLDStep();
    	copy.goalDesc = goalDesc;
    	copy.externalID = externalID;
    	copy.codeLocation = codeLocation;
    	copy.startFeatStruct = startFeatStruct;
    	copy.endFeatStruct = endFeatStruct;
    	copy.startBindings = startBindings.copy();
    	copy.endBindings = endBindings.copy();
    	return copy;
    }
    
    public static TraleSLDStep get(int id)
    {
        return KahinaRunner.retrieve(TraleSLDStep.class, id);
    }
}
