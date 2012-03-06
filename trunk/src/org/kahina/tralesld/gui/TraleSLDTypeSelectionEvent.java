package org.kahina.tralesld.gui;

import org.kahina.core.control.KahinaEvent;
import org.kahina.tralesld.control.TraleSLDEventTypes;

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
