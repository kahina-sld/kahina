package org.kahina.tralesld.visual.signature;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.visual.KahinaView;
import org.kahina.tralesld.data.signature.TraleSLDSignature;
import org.kahina.tralesld.gui.TraleSLDTypeSelectionEvent;

public class TraleSLDSignatureAppropriatenessView extends KahinaView<TraleSLDSignature>
{
	private Map<String,String> htmlForType;
	String currentType = "bot";
	
    public TraleSLDSignatureAppropriatenessView(KahinaController control)
    {
    	super(control);
        
        htmlForType = new HashMap<String,String>();
        
        control.registerListener("type selection", this);
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
    	StringBuilder htmlBuilder;
    	for (String type : model.getTypes())
    	{
    		//System.err.println("Producing feature appropriateness HTML for type: " + type);
    		htmlBuilder = new StringBuilder("<u>Appropriate features for type <b>" + type + "</b>:</u><br/><br/>");
    		Map<String,String> approp = model.getAppropriateness(type);
    		List<String> featList = new ArrayList<String>();
    		featList.addAll(approp.keySet());
    		Collections.sort(featList);
    		for (String feat : featList)
    		{
    			htmlBuilder.append(feat.toUpperCase() + ": <a href=\"type:" + approp.get(feat) + "\">" + approp.get(feat) + "</a>");
    			String introducer = model.getIntroducer(type,feat);
    			if (introducer == null)
    			{
    				htmlBuilder.append(" (WARNING: introducer unknown!) <br/>");
    			}
    			else if (type.equals(introducer))
    			{
        			htmlBuilder.append(" (introduced here) <br/>");
    			}
    			else
    			{
    				htmlBuilder.append(" (introduced by <a href=\"type:" + introducer + "\">" + introducer +  "</a>) <br/>");
    			}
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
    		usageHTML = "No feature appropriateness information available for type <b>" + type + "</b>.";
    	}
    	return usageHTML;
    }

	@Override
	public JComponent makePanel(KahinaGUI gui)
	{
        TraleSLDSignatureAppropriatenessViewPanel panel = new TraleSLDSignatureAppropriatenessViewPanel(control);
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
