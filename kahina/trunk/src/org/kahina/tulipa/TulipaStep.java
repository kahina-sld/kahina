package org.kahina.tulipa;

import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaStep;

public class TulipaStep extends KahinaStep
{
    /**
	 * 
	 */
	private static final long serialVersionUID = 3677954377223379901L;
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
        return KahinaRunner.retrieve(TulipaStep.class, id);
    }
}
