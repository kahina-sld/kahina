package org.kahina.tralesld;

import java.util.Collections;
import java.util.SortedSet;
import java.util.TreeSet;

import org.kahina.core.KahinaRunner;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.tralesld.data.fs.TraleSLDFeatureStructure;
import org.kahina.tralesld.data.fs.TraleSLDVariableBinding;

public class TraleSLDStep extends LogicProgrammingStep
{
    public TraleSLDFeatureStructure startFeatStruct;
    public TraleSLDFeatureStructure endFeatStruct;
    // declared as TreeSet, so lightweight store will construct it as such
	public TreeSet<TraleSLDVariableBinding> variableBindings = new TreeSet<TraleSLDVariableBinding>();
    
    public TraleSLDStep copy()
    {
    	TraleSLDStep copy = new TraleSLDStep();
    	copy.type = type;
    	copy.goalDesc = goalDesc;
    	copy.externalID = externalID;
    	copy.codeLocation = codeLocation;
    	copy.startFeatStruct = startFeatStruct;
    	copy.endFeatStruct = endFeatStruct;
    	copy.variableBindings.addAll(variableBindings);
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

	public void addVariableBinding(TraleSLDVariableBinding binding)
	{
		variableBindings.add(binding);
	}
	
	public SortedSet<TraleSLDVariableBinding> getVariableBindings()
	{
		return Collections.unmodifiableSortedSet(variableBindings);
	}
}
