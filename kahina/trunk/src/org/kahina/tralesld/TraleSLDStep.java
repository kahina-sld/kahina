package org.kahina.tralesld;

import org.kahina.core.KahinaRunner;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.tralesld.data.fs.TraleSLDFeatureStructure;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;

public class TraleSLDStep extends LogicProgrammingStep
{
    public TraleSLDFeatureStructure startFeatStruct;
    public TraleSLDFeatureStructure endFeatStruct;
    public TraleSLDVariableBindingSet startBindings = new TraleSLDVariableBindingSet();
    public TraleSLDVariableBindingSet endBindings = new TraleSLDVariableBindingSet();
    
    public TraleSLDStep copy()
    {
    	TraleSLDStep copy = new TraleSLDStep();
    	copy.type = type;
    	copy.goalDesc = goalDesc;
    	copy.externalID = externalID;
    	copy.codeLocation = codeLocation;
    	copy.startFeatStruct = startFeatStruct;
    	copy.endFeatStruct = endFeatStruct;
    	copy.startBindings = startBindings.copy();
    	copy.endBindings = endBindings.copy();
    	return copy;
    }
    
    public TraleSLDFeatureStructure getStartFeatureStructure()
    {
        return startFeatStruct;
    }
    
    public void setStartFeatureStructure(TraleSLDFeatureStructure startFeatStruct)
    {
        this.startFeatStruct = startFeatStruct;
    }
    
    public TraleSLDFeatureStructure getEndFeatureStructure()
    {
        return endFeatStruct;
    }
    
    public void setEndFeatureStructure(TraleSLDFeatureStructure endFeatStruct)
    {
        this.endFeatStruct = endFeatStruct;
    }
    
    public static TraleSLDStep get(int id)
    {
        return KahinaRunner.getDataManager().retrieve(TraleSLDStep.class, id);
    }
}
