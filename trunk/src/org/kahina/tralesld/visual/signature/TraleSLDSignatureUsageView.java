package org.kahina.tralesld.visual.signature;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.visual.KahinaView;
import org.kahina.tralesld.data.signature.TraleSLDSignature;
import org.kahina.tralesld.gui.TraleSLDTypeSelectionEvent;

public class TraleSLDSignatureUsageView extends KahinaView<TraleSLDSignature>
{
	private Map<String,String> htmlForType;
	String currentType = "bot";
	
    public TraleSLDSignatureUsageView(KahinaInstance<?, ?, ?, ?> kahina)
    {
    	super(kahina);
        
        htmlForType = new HashMap<String,String>();
        
        kahina.registerInstanceListener("type selection", this);
    }
    
    public TraleSLDSignatureUsageView(TraleSLDSignature signature, KahinaInstance<?, ?, ?, ?> kahina)
    {
        this(kahina);
        display(signature);
    }
    
    public void display(TraleSLDSignature signature)
    {
        this.model = signature;
        recalculate();
    }
    
    public void recalculate()
    {
    	//generate the HTML code for usage information display here
    	StringBuilder htmlBuilder;
    	for (String type : model.getTypes())
    	{
    		//System.err.println("Producing feature appropriateness HTML for type: " + type);
    		htmlBuilder = new StringBuilder("<u>Type <b>" + type + "</b> is valid for:</u><br/><br/>");
    		List<String> usesList = new ArrayList<String>();
    		usesList.addAll(model.getUses(type));
    		Collections.sort(usesList);
    		for (String entry : usesList)
    		{
    			String[] tyAndF = entry.split(":");
    			htmlBuilder.append("<a href=\"type:" + tyAndF[0] + "\">" + tyAndF[0] + "</a>:" + tyAndF[1].toUpperCase() + ":" + type + "<br/> ");
    		}
    		htmlForType.put(type, htmlBuilder.toString());
    	}
    }
    
    public String getCurrentType()
    {
    	return currentType;
    }
    
    public void setCurrentType(String type)
    {
    	currentType = type;
    }
    
    /**
     * Gets usage information on a type in HTML.
     * If no information on the type is available, the HTML says so.
     * @param type: the ALE type we want the usage information for
     * @return usage information in HTML for display in the view panel
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
	public JComponent makePanel()
	{
        TraleSLDSignatureUsageViewPanel panel = new TraleSLDSignatureUsageViewPanel(kahina);
        kahina.registerInstanceListener("redraw", panel);
        panel.setView(this);
        return new JScrollPane(panel);
	}
	
	public void processEvent(KahinaEvent event) 
	{
		if (event instanceof TraleSLDTypeSelectionEvent)
        {
            processEvent((TraleSLDTypeSelectionEvent) event);
        }
    }
    
    protected void processEvent(TraleSLDTypeSelectionEvent e)
    {
        setCurrentType(e.getSelectedType());
    }
}
