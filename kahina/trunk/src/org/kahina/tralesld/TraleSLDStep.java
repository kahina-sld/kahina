package org.kahina.tralesld;

import org.kahina.core.KahinaRunner;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.tralesld.data.fs.StructureSharedString;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;

public class TraleSLDStep extends LogicProgrammingStep
{
	// TODO this does not support DB storage yet
    public StructureSharedString startFeatStruct;
    public StructureSharedString endFeatStruct;
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
        return KahinaRunner.getDataManager().retrieve(TraleSLDStep.class, id);
    }
}
