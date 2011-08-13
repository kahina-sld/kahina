package org.kahina.tralesld.visual.signature;

import java.util.HashMap;
import java.util.Map;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.KahinaView;
import org.kahina.tralesld.data.signature.TraleSLDSignature;

public class TraleSLDSignatureAppropriatenessView extends KahinaView<TraleSLDSignature>
{
	private Map<String,String> htmlForType;
	
    public TraleSLDSignatureAppropriatenessView(KahinaController control)
    {
    	super(control);
        
        htmlForType = new HashMap<String,String>();
    }
    
    public TraleSLDSignatureAppropriatenessView(TraleSLDSignature signature, KahinaController control)
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
    	//generate the HTML code for feature appropriateness display here
    	
    }
    
    /**
     * Gets feature appropriateness information on a type in HTML.
     * If no information on the type is available, the HTML says so.
     * @param type: the ALE type we want the feature appropriateness information for
     * @return feature appropriateness information in HTML for display in the view panel
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
        TraleSLDSignatureAppropriatenessViewPanel panel = new TraleSLDSignatureAppropriatenessViewPanel(control);
        control.registerListener("redraw", panel);
        panel.setView(this);
        return new JScrollPane(panel);
	}
}
