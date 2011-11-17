package org.kahina.tralesld.visual.signature;

import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.visual.KahinaView;
import org.kahina.tralesld.data.signature.TraleSLDSignature;
import org.kahina.tralesld.event.TraleSLDTypeSelectionEvent;

public class TraleSLDSignatureHierarchyView extends KahinaView<TraleSLDSignature>
{
	private Map<String,String> htmlForType;
	String currentType = "bot";
	
    public TraleSLDSignatureHierarchyView(KahinaController control)
    {
    	super(control);
        
        htmlForType = new HashMap<String,String>();
        
        control.registerListener("type selection", this);
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
    		//System.err.println("Producing hierarchy HTML for type: " + type);
    		Set<String> supertypes = model.getSupertypes(type);
    		Set<String> subtypes = model.getSubtypes(type);
    		htmlBuilder = new StringBuilder("<h1>" + type + "</h1>");
    		if (supertypes.size() == 0)
    		{
    			htmlBuilder.append("<b>Root type. </b>");
    		}
    		else
    		{
    			htmlBuilder.append("<b>Immediate supertypes: </b>");
    		}
    		List<String> supertypeList = new ArrayList<String>();
    		supertypeList.addAll(supertypes);
    		Collections.sort(supertypeList);
    		for (String supertype : supertypeList)
    		{
    			htmlBuilder.append("<a href=\"type:" + supertype + "\">" + supertype + "</a> ");
    		}
    		
    		if (subtypes.size() == 0)
    		{
    			htmlBuilder.append("<br/><b>Atomic type. </b>");
    		}
    		else
    		{
	    		htmlBuilder.append("<br/><b>Immediate subtypes: </b>");
	    		List<String> subtypeList = new ArrayList<String>();
	    		subtypeList.addAll(subtypes);
	    		Collections.sort(subtypeList);
	    		for (String subtype : subtypeList)
	    		{
	    			htmlBuilder.append("<a href=\"type:" + subtype + "\">" + subtype + "</a> ");
	    		}
    		}
    		
    		if (supertypes.size() > 0)
    		{
	    		htmlBuilder.append("<br/><br/><b>Sibling types: </b><br/>");	
	    		for (String supertype : supertypes)
	    		{
	    			Set<String> siblingTypes = model.getSubtypes(supertype);
	    			if (siblingTypes.size() == 1)
	    			{
	    				htmlBuilder.append("--none--");
	    			}
	        		List<String> siblingList = new ArrayList<String>();
	        		siblingList.addAll(siblingTypes);
	        		Collections.sort(siblingList);
	    			for (String siblingType : siblingList)
	    			{
	    				if (!type.equals(siblingType))
	    				{
	    					htmlBuilder.append("<a href=\"type:" + siblingType + "\">" + siblingType + "</a> ");
	    				}
	    			}
	    			htmlBuilder.append("(as subtypes of <a href=\"type:" + supertype + "\">" + supertype + "</a>) <br/>");
	    		}
    		}	
    		
    		htmlBuilder.append("<br/><b>Paths: </b><br/>");
    		List<List<String>> pathList = new ArrayList<List<String>>();
    		pathList.addAll(model.getPaths(type));
    		Collections.sort(pathList, new ListComparator());
    		for (List<String> path : pathList)
    		{
    			for (String pathElement : path)
    			{
					htmlBuilder.append(" -> <a href=\"type:" + pathElement + "\">" + pathElement + "</a>");
    			}
    			htmlBuilder.append("<br/>");
    		}
    		htmlForType.put(type, htmlBuilder.toString());
    	}
    }
    
    private class ListComparator implements Comparator<List<String>>
    {
		@Override
		public int compare(List<String> list0, List<String> list1) 
		{
			int i = 0;
			int list0size = list0.size();
			int list1size = list1.size();			
			while (i < list0.size() && i < list1.size())
			{
				int compare = list0.get(i).compareTo(list1.get(i));
				if (compare != 0) return compare;
				i++;
			}
			if (list0size < list1size) return -1;
			if (list0size > list1size) return 1;
			return 0;
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
    		usageHTML = "No type hierarchy available for type <b>" + type + "</b>.";
    	}
    	return usageHTML;
    }

	@Override
	public JComponent makePanel(KahinaGUI gui)
	{
        TraleSLDSignatureHierarchyViewPanel panel = new TraleSLDSignatureHierarchyViewPanel(control);
        control.registerListener("redraw", panel);
        panel.setView(this);
        //TODO: somehow prevent the editor pane from exceeding the viewport bounds instead of line wrapping
        JScrollPane scrollPane = new JScrollPane(panel);
        //scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.setPreferredSize(new Dimension(150,180));
        return scrollPane;
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
