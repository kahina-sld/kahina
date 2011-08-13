package org.kahina.tralesld.data.signature;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

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
	//store the features whose value can be of a given type
	Map<String,Set<String>> usage;
	
	public TraleSLDSignature()
	{

		types = new LinkedList<String>();
		subtypes = new HashMap<String,Set<String>>();
		supertypes = new HashMap<String,Set<String>>();
		introFeats = new HashMap<String,Map<String,String>>();
		typeRestr = new HashMap<String,Map<String,String>>();
		usage = new HashMap<String,Set<String>>();
		
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
		usage.put(type, new HashSet<String>());
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
		usage.get(valueRestr).add(type + ":" + feature);
	}
	
	public List<String> getTypes()
	{
		return types;
	}
	
	public Set<String> getSubtypes(String type)
	{
		return subtypes.get(type);
	}
	
	public Set<String> getSupertypes(String type)
	{
		return supertypes.get(type);
	}
	
	public Set<String> getUses(String type)
	{
		return usage.get(type);
	}
	
	public Map<String,String> getAppropriateness(String type)
	{
		return typeRestr.get(type);
	}
	
	/**
	 * Fills the introFeats map. 
	 * This needs to be called once for the visualization to work.
	 */
	//TODO: precompute and cache more type information:
	//		- an indexing structure that directly encodes where features come from
	//		- another indexing structure that makes all type restrictions for a feature explicit
	//		- cache the usage data (i.e.: for which features at which types is a given type appropriate?)
	//		- perhaps even generate and cache the HTML code for the view components?
	//		  (probably not, this should logically be done by the TraleSLDSignatureView
	public void inferCachedInformation()
	{
		for (String type : types)
		{
			
		}
	}
}
