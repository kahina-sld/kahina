package org.kahina.tralesld.visual.signature;

import java.awt.Color;
import java.awt.Font;
import java.awt.Stroke;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.chart.KahinaChartEdgeDisplayDecider;
import org.kahina.core.visual.chart.KahinaChartViewConfiguration;
import org.kahina.core.visual.chart.KahinaChartViewPanel;
import org.kahina.tralesld.data.signature.TraleSLDSignature;

public class TraleSLDSignatureUsageView extends KahinaView<TraleSLDSignature>
{
	private Map<String,String> htmlForType;
	
    public TraleSLDSignatureUsageView(KahinaController control)
    {
    	super(control);
        
        htmlForType = new HashMap<String,String>();
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
}
