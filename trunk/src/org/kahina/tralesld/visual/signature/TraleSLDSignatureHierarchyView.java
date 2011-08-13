package org.kahina.tralesld.visual.signature;

import java.util.HashMap;
import java.util.Map;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.KahinaView;
import org.kahina.tralesld.data.signature.TraleSLDSignature;

public class TraleSLDSignatureHierarchyView extends KahinaView<TraleSLDSignature>
{
	private Map<String,String> htmlForType;
	
    public TraleSLDSignatureHierarchyView(KahinaController control)
    {
    	super(control);
        
        htmlForType = new HashMap<String,String>();
    }
    
    public TraleSLDSignatureHierarchyView(TraleSLDSignature signature, KahinaController control)
    {
        this(control);
        display(signature);
    }
    
    public void display(TraleSLDSignature signature)
    {
        this.model = signature;
        recalculate();
    }
    
    public void recalculate()
    {
    	//generate the HTML code for hierarchy display here
    	StringBuilder htmlBuilder;
    	for (String type : model.getTypes())
    	{
    		System.err.println("Producing hierarchy HTML for type: " + type);
    		htmlBuilder = new StringBuilder("<h1>" + type + "</h1><b>Immediate supertypes: </b>");
    		for (String supertype : model.getSupertypes(type))
    		{
    			htmlBuilder.append("<a href=\"type:" + supertype + "\">" + supertype + "</a>,");
    		}
    		htmlBuilder.append("<br/><b>Immediate subtypes: </b>");
    		for (String subtype : model.getSubtypes(type))
    		{
    			htmlBuilder.append("<a href=\"type:" + subtype + "\">" + subtype + "</a>,");
    		}
    		htmlForType.put(type, htmlBuilder.toString());
    	}
    }
    
    /**
     * Gets hierarchy information on a type in HTML.
     * If no information on the type is available, the HTML says so.
     * @param type: the ALE type we want the hierarchy information for
     * @return hierarchy information in HTML for display in the view panel
     */
    public String getHTML(String type)
    {
    	String usageHTML = htmlForType.get(type);
    	if (usageHTML == null)
    	{
    		usageHTML = "No usage info available for type <b>" + type + "</b>.";
    	}
    	return usageHTML;
    }

	@Override
	public JComponent wrapInPanel(KahinaController control) 
	{
        TraleSLDSignatureHierarchyViewPanel panel = new TraleSLDSignatureHierarchyViewPanel(control);
        control.registerListener("redraw", panel);
        panel.setView(this);
        return new JScrollPane(panel);
	}
}
