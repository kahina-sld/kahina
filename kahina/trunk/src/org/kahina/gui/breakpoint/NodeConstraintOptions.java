package org.kahina.gui.breakpoint;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class controls the possible settings for elementary constraints in a node constraint editor.
 * 
 * It stores a set of constraint types as well as maps from such types to possible relations and values.
 * 
 * Implementations can either inherit from this method and reimplement addStandardOptions() 
 * or add their additional options to the standard set via addType(), addRelationForType() etc.
 * 
 * @author johannes
 *
 */

public class NodeConstraintOptions
{
    //stores the list of constraint types; will be the choices of the left combobox in the node constraint editor
    List<String> types;
    //maps constraint types to the relations which will be the choices of the middle combobox, depending on the selected type
    Map<String,List<String>> typesToRelations; 
    //maps constraint types to the relations which will be the choices of the right combobox, depending on the selected type
    //an empty string in a value list indicates that the node constraint editor should provide an editable combobox
    Map<String,List<String>> typesToValues;
    
    public NodeConstraintOptions()
    {
        types = new ArrayList<String>();
        typesToRelations = new HashMap<String,List<String>>();
        typesToValues = new HashMap<String,List<String>>();
        //the default empty option must always be there, new elementary constraints will be initialized with this
        types.add("--");
    }
    
    public void setStandardOptions()
    {
        addType("step label");
        //addType("step origin");
        addType("step id");
        //addType("step type");
        List<String> nodeLabelRelations = new ArrayList<String>();
        nodeLabelRelations.add("equals");
        nodeLabelRelations.add("matches");
        nodeLabelRelations.add("starts with");
        nodeLabelRelations.add("contains");
        nodeLabelRelations.add("ends with");
        typesToRelations.put("step label", nodeLabelRelations);
        List<String> edgeLabelRelations = new ArrayList<String>();
        edgeLabelRelations.addAll(nodeLabelRelations);
        typesToRelations.put("step origin", edgeLabelRelations);
        List<String> nodeIDRelations = new ArrayList<String>();
        nodeIDRelations.add("=");
        nodeIDRelations.add("<=");
        nodeIDRelations.add(">=");
        nodeIDRelations.add("<");
        nodeIDRelations.add(">");
        typesToRelations.put("step id", nodeIDRelations);
    }
    
    public List<String> getTypes()
    {
        return types;
    }
    
    public List<String> getRelationsForType(String typeID)
    {
        List<String> rels = typesToRelations.get(typeID);
        if (rels == null)
        {
            rels = new ArrayList<String>();
            rels.add("--");
        }
        return rels;
    }
    
    public List<String> getValuesForType(String typeID)
    {
        List<String> vals = typesToRelations.get(typeID);
        if (vals == null)
        {
            vals = new ArrayList<String>();
            vals.add("");
        }
        return vals;
    }
    
    public void addType(String type)
    {
        if (!types.add(type))
        {
            System.err.println("WARNING: node constraint type \"" + type + "\" was not registered twice!");
        }
    }
    
    public void addRelationToType(String type, String relation)
    {
        
    }
}
