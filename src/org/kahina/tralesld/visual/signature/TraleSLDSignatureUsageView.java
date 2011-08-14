package org.kahina.tralesld.visual.signature;

import java.awt.Color;
import java.awt.Font;
import java.awt.Stroke;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.chart.KahinaChartEdgeDisplayDecider;
import org.kahina.core.visual.chart.KahinaChartViewConfiguration;
import org.kahina.core.visual.chart.KahinaChartViewPanel;
import org.kahina.tralesld.data.signature.TraleSLDSignature;
import org.kahina.tralesld.event.TraleSLDTypeSelectionEvent;

public class TraleSLDSignatureUsageView extends KahinaView<TraleSLDSignature>
{
	private Map<String,String> htmlForType;
	String currentType = "bot";
	
    public TraleSLDSignatureUsageView(KahinaController control)
    {
    	super(control);
        
        htmlForType = new HashMap<String,String>();
        
        control.registerListener("type selection", this);
    }
    
    public TraleSLDSignatureUsageView(TraleSLDSignature signature, KahinaController control)
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
    	//generate the HTML code for usage information display here
    	StringBuilder htmlBuilder;
    	for (String type : model.getTypes())
    	{
    		//System.err.println("Producing feature appropriateness HTML for type: " + type);
    		htmlBuilder = new StringBuilder("<u>Type <b>" + type + "</b> is valid for:</u><br/><br/>");
    		for (String entry : model.getUses(type))
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
	public JComponent wrapInPanel(KahinaController control) 
	{
        TraleSLDSignatureUsageViewPanel panel = new TraleSLDSignatureUsageViewPanel(control);
        control.registerListener("redraw", panel);
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
