package org.kahina.core.control;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

public class KahinaStepProperty
{
    public String exportXML(boolean b)
    {
        return "";
    }

    public Element exportXML(Document dom)
    {
        return dom.createElement("kahina:stepProperty");
    } 
    
    public static KahinaStepProperty importXML(Document dom)
    {
        return new KahinaStepProperty();
    }
}
