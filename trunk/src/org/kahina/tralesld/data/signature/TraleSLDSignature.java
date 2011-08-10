package org.kahina.tralesld.data.signature;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kahina.core.data.KahinaObject;

public class TraleSLDSignature extends KahinaObject
{
	/**
	 * 
	 */
	private static final long serialVersionUID = 7411049956894696227L;

	//types can simply be stored as strings because they have unique names
	//this list defines the order in which types are displayed
	List<String> types;
	
	//the immediate subtypes and supertypes for each type are stored here
	//this also implicitly encodes the sibling relation used by the signature-enhanced editor
	Map<String,Set<String>> subtypes;
	Map<String,Set<String>> supertypes;
	
	//store the features introduced at each type, inherited features are not listed
	Map<String,Map<String,String>> introFeats;
	//inherited features are listed again when a type restriction is tightened
	Map<String,Map<String,String>> typeRestr;
	
	public TraleSLDSignature()
	{

		types = new LinkedList<String>();
		subtypes = new HashMap<String,Set<String>>();
		supertypes = new HashMap<String,Set<String>>();
		introFeats = new HashMap<String,Map<String,String>>();
		typeRestr = new HashMap<String,Map<String,String>>();
		
		//the empty signature contains the root type "bot" without features
		registerType("bot");
	}
	
	public void registerType(String type)
	{
		types.add(type);
		subtypes.put(type, new HashSet<String>());
		supertypes.put(type, new HashSet<String>());
		introFeats.put(type, new HashMap<String,String>());
		typeRestr.put(type, new HashMap<String,String>());
	}

	public void addSubtypeRelation(String type, String subtype) 
	{
		if (!types.contains(type))
		{
			registerType(type);
		}
		if (!types.contains(subtype))
		{
			registerType(subtype);
		}
		subtypes.get(type).add(subtype);
		supertypes.get(subtype).add(type);
	}
	
	public void addAppropriateFeature(String type, String feature, String valueRestr) 
	{
		if (!types.contains(type))
		{
			registerType(type);
		}
		if (!types.contains(valueRestr))
		{
			registerType(valueRestr);
		}
		typeRestr.get(type).put(feature,valueRestr);
	}
	
	/**
	 * Fills the introFeats map; TODO: precompute and cache more type information
	 * This needs to be called once for the visualization to work.
	 */
	public void inferCachedInformation()
	{
		
	}
}
