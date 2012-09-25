package org.kahina.core.control;

import org.kahina.core.data.agent.KahinaControlAgent;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

public abstract class KahinaStepProperty
{   
    public abstract KahinaStepProperty copy();
    
    public String exportXML(boolean b)
    {
        return "";
    }

    public abstract Element exportXML(Document dom);

    public static KahinaStepProperty importXML(Element el)
    {
        return null;
    }
}
