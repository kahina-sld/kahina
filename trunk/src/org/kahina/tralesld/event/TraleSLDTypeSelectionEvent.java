package org.kahina.tralesld.event;

import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.visual.KahinaViewPanel;

public class TraleSLDTypeSelectionEvent extends KahinaEvent 
{
	 String selectedType;
	    
    public TraleSLDTypeSelectionEvent(String selectedType)
    {
        super(TraleSLDEventTypes.TYPE_SELECTION);
        this.selectedType = selectedType;
    }
    
    public String getSelectedType()
    {
        return selectedType;
    }
    
    @Override
	public String toString()
    {
        return  "select type " + selectedType;
    }
}
