package org.kahina.tralesld;

import org.kahina.core.KahinaRunner;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.tralesld.data.fs.TraleSLDFeatureStructure;

public class TraleSLDStep extends LogicProgrammingStep
{
    public TraleSLDFeatureStructure startFeatStruct;
    public TraleSLDFeatureStructure endFeatStruct;
    
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
