package org.kahina.tralesld;

import org.kahina.core.data.fs.KahinaFeatureStructure;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.lp.LogicProgrammingStep;

public class TraleSLDStep extends LogicProgrammingStep
{
    public KahinaFeatureStructure startFeatStruct;
    public KahinaFeatureStructure endFeatStruct;
    
    public KahinaFeatureStructure getStartFeatureStructure()
    {
        return startFeatStruct;
    }
    
    public void setStartFeatureStructure(KahinaFeatureStructure startFeatStruct)
    {
        this.startFeatStruct = startFeatStruct;
    }
    
    public KahinaFeatureStructure getEndFeatureStructure()
    {
        return endFeatStruct;
    }
    
    public void setEndFeatureStructure(KahinaFeatureStructure endFeatStruct)
    {
        this.endFeatStruct = endFeatStruct;
    }
}
