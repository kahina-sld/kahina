package org.kahina.tralesld.data.signature;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.kahina.core.data.KahinaObject;

public class TraleSLDSignature extends KahinaObject
{
	/**
	 * 
	 */
	private static final long serialVersionUID = 7411049956894696227L;

	//types can simply be stored as strings because they have unique names
	List<String> types;
	
	//the immediate subtypes and supertypes for each type are stored here
	//this also implicitly encodes the sibling relation used by the signature-enhanced editor
	Map<String,List<String>> subtypes;
	Map<String,List<String>> supertypes;
	
	//store the features introduced at each type
	//inherited features are not listed, except when a type restriction is tightened
	Map<String,Map<String,String>> introFeats;
	
	public TraleSLDSignature()
	{
		//the empty signature contains the root type "bot" without features
		types = new LinkedList<String>();
		types.add("bot");
		subtypes = new HashMap<String,List<String>>();
		subtypes.put("bot", new LinkedList<String>());
		supertypes = new HashMap<String,List<String>>();
		supertypes.put("bot", new LinkedList<String>());
		introFeats = new HashMap<String,Map<String,String>>();
		introFeats.put("bot", new HashMap<String,String>());
	}
	
	public static TraleSLDSignature loadFromTraleSignatureFile()
	{
		return new TraleSLDSignature();
	}
}
