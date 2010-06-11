package org.kahina.tulipa;

import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaStep;
import org.kahina.lp.LogicProgrammingStep;

public class TulipaStep extends KahinaStep
{
    //the item description associated with that step
    public String itemDesc; 
    //the item ID used by the surveyed TuLiPa instance
    public int externalID;
    
    public String getItemDesc()
    {
        return itemDesc;
    }
    
    public void setItemDesc(String itemDesc)
    {
        this.itemDesc = itemDesc;
    }
    
    public int getExternalID()
    {
        return externalID;
    }
    
    public void setExternalID(int externalID)
    {
        this.externalID = externalID;
    }
    
    public static TulipaStep get(int id)
    {
        return KahinaRunner.getDataManager().retrieve(TulipaStep.class, id);
    }
}
