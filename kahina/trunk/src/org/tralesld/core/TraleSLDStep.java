package org.tralesld.core;

import org.kahina.core.LogicProgrammingStep;
import org.kahina.data.fs.KahinaFeatureStructure;
import org.kahina.data.source.KahinaSourceCodeLocation;

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
