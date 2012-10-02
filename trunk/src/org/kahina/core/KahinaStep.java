package org.kahina.core;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.source.KahinaSourceCodeLocation;

public class KahinaStep extends KahinaObject
{
    /**
	 * 
	 */
	private static final long serialVersionUID = 7289160962407655124L;
	
    //the source code location associated with this step
    public KahinaSourceCodeLocation codeLocation;

	public KahinaStep()
    {
    }
	
    public KahinaSourceCodeLocation getSourceCodeLocation()
    {
        return codeLocation;
    }
    
    public void setSourceCodeLocation(KahinaSourceCodeLocation codeLocation)
    {
        this.codeLocation = codeLocation;
    }
}
