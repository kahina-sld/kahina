package org.kahina.tralesld.visual.fs;

import java.io.ByteArrayInputStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.kahina.tralesld.data.signature.TraleSLDSignature;

import gralej.controller.StreamInfo;
import gralej.om.Entities;
import gralej.om.EntityFactory;
import gralej.om.IAny;
import gralej.om.IEntity;
import gralej.om.IFeatureValuePair;
import gralej.om.IList;
import gralej.om.ITag;
import gralej.om.IType;
import gralej.om.ITypedFeatureStructure;
import gralej.parsers.IDataPackage;
import gralej.parsers.ParseException;

public class GraleJUtility 
{
	static EntityFactory ent = EntityFactory.getInstance();
	static TraleSLDFeatureStructureEditor editor = null;
	
	public static void setFeatureStructureEditor(TraleSLDFeatureStructureEditor e)
	{
		editor = e;
	}
	
	public static IEntity specialize(IEntity e, List<String> path, String ty, TraleSLDSignature sig)
	{
		IEntity et = go(e,path);
		if (et == null)
		{
			failMsg("Specialize failed: Unable to evaluate address!");
			return e;
		}
		if (et instanceof ITag)
		{
			et = ((ITag) et).target();
		}
		String eType = getType(et);
		if (eType != null && sig.dominates(eType,ty))
		{
			if (eType.equals("list"))
			{
				IEntity parent = goUpToLast(e,path);
				if (parent instanceof ITag)
				{
					parent = ((ITag) parent).target();
				}
				IList list = ent.newList();
				if (ty.equals("ne_list"))
				{
					list.append(signatureMGS("bot", sig));
				}
				successMsg("List specialization successful!");
				if (parent == null) return list;
				replace(parent,et,list);
			}
			else
			{
				setType(et,ty);
				successMsg("Type specialization successful!");
			}
		}
		else
		{
			failMsg("Specialize failed: Dominance condition violated!");
		}
		return e;
	}
	
	public static IEntity generalize(IEntity e, List<String> path, String ty, TraleSLDSignature sig)
	{
		IEntity ent = go(e,path);
		if (ent == null)
		{
			failMsg("Generalize failed: Unable to evaluate address!");
			return e;
		}
		if (ent instanceof ITag)
		{
			ent = ((ITag) ent).target();
		}
		String eType = getType(ent);
		if (eType != null && sig.dominates(ty,eType))
		{
			setType(ent,ty);
			successMsg("Type generalization successful!");
		}
		else
		{
			failMsg("Specialize failed: Dominance condition violated!");
		}
		return e;
	}
	
	public static IEntity switchType(IEntity e, List<String> path, String ty, TraleSLDSignature sig)
	{
		IEntity ent = go(e,path);
		if (ent == null)
		{
			failMsg("Switching failed: Unable to evaluate address!");
			return e;
		}
		if (ent instanceof ITag)
		{
			ent = ((ITag) ent).target();
		}
		String oldType = getType(ent);
		String commonSupertype = null;
		//find common ancestor
		for (String supertype : sig.getSupertypes(getType(ent)))
		{
			if (sig.getSubtypes(supertype).contains(ty))
			{
				commonSupertype = supertype;
				break;
			}
		}
		if (commonSupertype == null)
		{
			failMsg("Switching failed: No common supertype found for types " + oldType + " and " + ty + "!");
		}
		else
		{
			//use sequence of generalization and specialization (inefficient, but clean)
			generalize(e,path,commonSupertype,sig);
			specialize(e,path,ty,sig);
			successMsg("Type switching successful!");
		}
		return e;
	}
	
	public static IEntity changeAtom(IEntity e, List<String> path, String newName, TraleSLDSignature sig)
	{
		IEntity parent = goUpToLast(e,path);
		IEntity et = goLast(parent,path);
		if (parent == null) et = e;	
		if (et == null)
		{
			failMsg("Atom change failed: Unable to evaluate address!");
			return e;
		}
		if (et instanceof ITag)
		{
			parent = et;
			et = ((ITag) et).target();
		}
		if (et instanceof ITypedFeatureStructure)
		{
			IAny replacement = ent.newAny(newName);
			successMsg("Atom change successful!");
			if (parent == null) return replacement;
			replace(parent,et,replacement);
			return e;
		}
		else if (et instanceof IAny)
		{
			IAny atom = (IAny) et;
			atom.setValue(newName);
			successMsg("Atom change successful!");
			return e;
		}
		failMsg("Atom change failed: not possible in context " + et + ".");
		return e;
	}
	
	public static IEntity generalizeAtom(IEntity e, List<String> path, TraleSLDSignature sig)
	{
		IEntity parent = goUpToLast(e,path);
		IEntity et = goLast(parent,path);
		if (parent == null) et = e;	
		if (et == null)
		{
			failMsg("Atom generalization failed: Unable to evaluate address!");
			return e;
		}
		if (et instanceof ITag)
		{
			parent = et;
			et = ((ITag) et).target();
		}
		if (et instanceof IAny)
		{
			ITypedFeatureStructure replacement = ent.newTFS("bot");
			successMsg("Atom generalization successful!");
			if (parent == null) return replacement;
			replace(parent,et,replacement);
			return e;
		}
		else if (et instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure fs = (ITypedFeatureStructure) et;
			fs.featureValuePairs().clear();
			fs.setType(ent.newType("bot"));
			successMsg("Atom generalization successful!");
			return e;
		}
		failMsg("Atom generalization failed: not possible in context " + et + ".");
		return e;
	}
	
	public static IEntity specializeTTF(IEntity e, List<String> path, String ty, TraleSLDSignature sig)
	{
		e = specialize(e,path,ty,sig);
		IEntity et = go(e,path);
		if (et == null)
		{
			failMsg("TTF specialization failed: Unable to evaluate address!");
			return e;
		}
		if (et instanceof ITag)
		{
			et = ((ITag) et).target();
		}
		if (et instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure fs = (ITypedFeatureStructure) et;
			enforceTTF(fs,sig);
		}
		else if (!(et instanceof IList))
		{
			failMsg("TTF specialization failed: not a typed feature structure!");
		}
		return e;
	}
	
	public static IEntity generalizeTTF(IEntity e, List<String> path, String ty, TraleSLDSignature sig)
	{
		e = generalize(e,path,ty,sig);
		IEntity et = go(e,path);
		if (et == null)
		{
			failMsg("TTF generalization failed: Unable to evaluate address!");
			return e;
		}
		if (et instanceof ITag)
		{
			et = ((ITag) et).target();
		}
		if (et instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure fs = (ITypedFeatureStructure) et;
			enforceTTF(fs,sig);
		}
		else
		{
			failMsg("TTF generalization failed: not a typed feature structure!");
		}
		return e;
	}
	
	public static IEntity switchTTF(IEntity e, List<String> path, String ty, TraleSLDSignature sig)
	{
		e = switchType(e,path,ty,sig);
		IEntity et = go(e,path);
		if (et == null)
		{
			failMsg("TTF type switch failed: Unable to evaluate address!");
			return e;
		}
		if (et instanceof ITag)
		{
			et = ((ITag) et).target();
		}
		if (et instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure fs = (ITypedFeatureStructure) et;
			enforceTTF(fs,sig);
		}
		else
		{
			failMsg("TTF type switch failed: not a typed feature structure!");
		}
		return e;
	}
	
	private static void enforceTTF(ITypedFeatureStructure fs, TraleSLDSignature sig)
	{
		Map<String,String> appropFeats = sig.getTypeRestrictions(getType(fs));
		List<String> appropFeatsList = new LinkedList<String>();
		appropFeatsList.addAll(appropFeats.keySet());
		Collections.sort(appropFeatsList);
		Map<String,IFeatureValuePair> featureMap = new HashMap<String,IFeatureValuePair>();
		for (IFeatureValuePair pair : fs.featureValuePairs())
		{
			featureMap.put(pair.text(), pair);
		}
		//TODO: subsumption check to determine whether feature values are specific enough
		for (String feat : appropFeatsList)
		{
			IFeatureValuePair fv = featureMap.remove(feat);
			if (fv == null)
			{
				fs.addFeatureValue(ent.newFeatVal(feat, signatureMGS(appropFeats.get(feat), sig)));
			}
		}
		//remove superfluous features
		for (IFeatureValuePair fv : featureMap.values())
		{
			fs.featureValuePairs().remove(fv);
		}
	}
	
	public static IEntity introFeat(IEntity e, List<String> path, String feat, IEntity val, TraleSLDSignature sig)
	{
		IEntity parent = goUpToLast(e,path);
		IEntity et = goLast(parent,path);
		if (parent == null) et = e;	
		if (et == null)
		{
			failMsg("Feature introduction failed: Unable to evaluate address!");
			return e;
		}
		if (et instanceof ITag)
		{
			parent = et;
			et = ((ITag) et).target();
		}
		IFeatureValuePair fv = ent.newFeatVal(feat, val); 
		if (et instanceof IType)
		{
			List<IFeatureValuePair> fvList = new LinkedList<IFeatureValuePair>();
			fvList.add(fv);
			IType type = (IType) et;
			ITypedFeatureStructure replacement = ent.newTFS(type, fvList);
			successMsg("Feature introduction successful!");
			if (parent == null) return replacement;
			replace(parent,et,replacement);
			return e;
		}
		else if (et instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure fs = (ITypedFeatureStructure) et;
			fs.addFeatureValue(fv);
			successMsg("Feature introduction successful!");
			return e;
		}
		failMsg("Feature introduction failed: not possible in context " + et + ".");
		return e;
	}
	
	public static IEntity remFeat(IEntity e, List<String> path, String feat)
	{
		IEntity et = goUpToLast(e,path);
		if (et == null)
		{
			failMsg("Feature removal failed: Unable to evaluate address!");
			return e;
		}
		if (et  instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure fs = (ITypedFeatureStructure) et;
			for (int i = 0; i < fs.featureValuePairs().size(); i++)
			{
				if (fs.featureValuePairs().get(i).feature().equals(feat))
				{
					fs.featureValuePairs().remove(i);
					successMsg("Feature removal successful!");
					return e;
				}
			}
		}
		else
		{
			failMsg("Feature removal failed: not a typed feature structure!");
		}
		return e;
	}
	
	public static IEntity resetFeat(IEntity e, List<String> path, String feat, TraleSLDSignature sig)
	{
		IEntity parent = goUpToLast(e,path);
		IEntity et = goLast(parent,path);
		if (et instanceof ITag)
		{
			parent = et;
			et = ((ITag) et).target();
		}
		if (et == null || parent == null)
		{
			failMsg("Feature reset failed: Unable to evaluate addresses!");
			return e;
		}
		IEntity replacement = signatureMGS(getType(et), sig);
		replace(parent,et,replacement);
		successMsg("Feature reset successful!");
		return e;
	}
	
	public static IEntity remIdent(IEntity e, List<String> path)
	{
		Map<Integer,List<List<String>>> identities = getIdentities(e);
		IEntity parent = goUpToLast(e,path);
		IEntity et = goLast(parent,path);
		if (et == null)
		{
			failMsg("Identity dissolval failed: Unable to evaluate address!");
			return e;
		}
		if (et instanceof ITag)
		{
			ITag tag = (ITag) et;
			identities.get(tag.number()).remove(path);
			e = removeTag(e, path);
			if (identities.get(tag.number()).size() == 1)
			{
				List<String> otherPath = identities.get(tag.number()).get(0);
				e = removeTag(e,otherPath);
				identities.remove(tag.number());
			}
			successMsg("Identity dissolval successful!");
		}
		else
		{
			failMsg("Identity dissolval failed: No reentrancy at this address!");
		}
		return e;
	}
	
	private static IEntity removeTag(IEntity e, List<String> path)
	{
		IEntity parent = goUpToLast(e,path);
		IEntity et = goLast(parent,path);
		if (et == null)
		{
			failMsg("Tag removal failed: Unable to evaluate address!");
			return e;
		}
		if (et instanceof ITag)
		{
			ITag tag = (ITag) et;
			IEntity replacement = copy(tag.target());
			replace(parent,et,replacement);
		}
		else
		{
			failMsg("Tag removal failed: No tag at address " + path);
		}
		return e;
	}
	
	public static IEntity addListElement(IEntity e, List<String> path, int listIndex, TraleSLDSignature sig)
	{
		IEntity ent = go(e,path);
		if (ent == null)
		{
			failMsg("List manipulation failed: Unable to evaluate address!");
			return e;
		}
		if (ent instanceof ITag)
		{
			ent = ((ITag) ent).target();
		}
		if (!(ent instanceof IList))
		{
			failMsg("List manipulation failed: No list at this address!");
			return e;
		}
		IEntity newElement = signatureMGS("bot",sig);
		IList list = (IList) ent;
		List<IEntity> listEl = new LinkedList<IEntity>();
		for (IEntity el : list.elements())
		{
			listEl.add(el);
		}
		listEl.add(listIndex, newElement);
		list.clear();
		for (IEntity el : listEl)
		{
			list.append(el);
		}
		successMsg("New list element successfully added.");
		return e;
	}
	
	public static IEntity removeListElement(IEntity e, List<String> path, int listIndex, TraleSLDSignature sig)
	{
		IEntity ent = go(e,path);
		if (ent == null)
		{
			failMsg("List manipulation failed: Unable to evaluate address!");
			return e;
		}
		if (ent instanceof ITag)
		{
			ent = ((ITag) ent).target();
		}
		if (!(ent instanceof IList))
		{
			failMsg("List manipulation failed: No list at this address!");
			return e;
		}
		IList list = (IList) ent;
		List<IEntity> listEl = new LinkedList<IEntity>();
		for (IEntity el : list.elements())
		{
			listEl.add(el);
		}
		listEl.remove(listIndex);
		list.clear();
		for (IEntity el : listEl)
		{
			list.append(el);
		}
		successMsg("List element successfully removed.");
		return e;
	}
	
	public static IEntity clearList(IEntity e, List<String> path, TraleSLDSignature sig)
	{
		IEntity ent = go(e,path);
		if (ent == null)
		{
			failMsg("List manipulation failed: Unable to evaluate address!");
			return e;
		}
		if (ent instanceof ITag)
		{
			ent = ((ITag) ent).target();
		}
		if (!(ent instanceof IList))
		{
			failMsg("List manipulation failed: No list at this address!");
			return e;
		}
		IList list = (IList) ent;
		list.clear();
		successMsg("List successfully cleared.");
		return e;
	}
	
	public static IEntity copy(IEntity e)
	{
		return e;
	}
	
	public static IEntity newAtom(String name)
	{
		return ent.newAny(name);
	}
	
	public static IEntity makeIdent(IEntity e, List<String> path1, List<String> path2, TraleSLDSignature sig)
	{
		IEntity ent1 = go(e,path1);
		IEntity ent2 = go(e,path2);
		if (ent1 == null || ent2 == null)
		{
			failMsg("Identity introduction failed: Unable to evaluate address!");
			return null;
		}
		IEntity parent1 = goUpToLast(e,path1);
		IEntity parent2 = goUpToLast(e,path2);
		if (ent1 instanceof ITag)
		{
			parent1 = ent1;
			ent1 = ((ITag) ent1).target();
		}
		if (ent2 instanceof ITag)
		{
			parent2 = ent2;
			ent2 = ((ITag) ent2).target();
		}
		IEntity mgu = unify(ent1,ent2,sig);
		if (mgu == null) return null;
		Map<Integer,List<List<String>>> identities = getIdentities(e);
		int number = getFreeTagID(identities);
		replace(parent1,ent1,ent.newTag(number,mgu));
		replace(parent2,ent2,ent.newTag(number,mgu));
		if (contractTags(e, parent1) && contractTags(e, parent2))
		{
			successMsg("Identity introduction successful!");
		}
		return e;
	}
	
	private static int getFreeTagID(Map<Integer,List<List<String>>> identities)
	{
		for (int i = 0; ; i++)
		{
			if (identities.get(i) == null) return i;
		}
	}
	
	private static boolean contractTags(IEntity e, IEntity ent)
	{
		Map<Integer,List<List<String>>> identities = getIdentities(e);
		if (ent instanceof ITag)
		{
			ITag tag1 = (ITag) ent;
			if (tag1.target() instanceof ITag)
			{
				ITag tag2 = (ITag) tag1.target();
				int lowerNumber = tag1.number();
				int higherNumber = tag2.number();
				if (lowerNumber > higherNumber)
				{
					lowerNumber = tag2.number();
					higherNumber = tag1.number();
				}
				replace(tag1,tag2,tag2.target());
				for (List<String> path : identities.get(higherNumber))
				{
					IEntity otherE = go(e,path);
					if (otherE instanceof ITag)
					{
						((ITag) otherE).setNumber(lowerNumber);
					}
					else
					{
						failMsg("Failed to rename tag at path: " + path);
						return false;
					}
				}
			}
			else
			{
				failMsg("Expected double tag was not encountered!");
				return false;
			}
		}
		return true;
	}
	
	private static boolean inOnePath(List<String> path1, List<String> path2)
	{
		int length = path1.size();
		if (path2.size() < length) length = path2.size();
		for (int i = 0; i < length; i++)
		{
			if (!path1.get(i).equals(path2.get(i))) return false;
		}
		return true;
	}
	
	public static IEntity unify(IEntity e1, IEntity e2, TraleSLDSignature sig)
	{
		if (e1 instanceof ITypedFeatureStructure && e2 instanceof ITypedFeatureStructure)
		{
			return unify((ITypedFeatureStructure) e1, (ITypedFeatureStructure) e2, sig);
		}
		else if (e1 instanceof IList && e2 instanceof IList)
		{
			return unify((IList) e1, (IList) e2, sig);
		}
		else if (e1 instanceof IList && e2 instanceof ITypedFeatureStructure)
		{
			if (getType(e2).equals("list")) return e1;
			failMsg("Unification failed: lists and other structures are incompatible.");
			return null;
		}
		else if (e2 instanceof IList && e1 instanceof IList)
		{
			if (getType(e1).equals("list")) return e2;
			failMsg("Unification failed: lists and other structures are incompatible.");
			return null;
		}
		else if (e1 instanceof ITag && e2 instanceof ITag)
		{
			return unify((ITag) e1, (ITag) e2, sig);
		}
		else if (e1 instanceof ITag)
		{
			IEntity res = unify(((ITag) e1).target(),e2, sig);
			if (res != null)
			{
				return ent.newTag(((ITag) e1).number(), res);
			}
			else
			{
				return null;
			}
		}
		else if (e2 instanceof ITag)
		{
			IEntity res = unify(((ITag) e2).target(),e1, sig);
			if (res != null)
			{
				return ent.newTag(((ITag) e2).number(), res);
			}
			else
			{
				return null;
			}
		}
		else
		{
			failMsg("Unification failed: lists and other structures are incompatible.");
			return null;
		}
	}
	
	private static IEntity unify(ITypedFeatureStructure tfs1, ITypedFeatureStructure tfs2, TraleSLDSignature sig)
	{
		String type1 = getType(tfs1);
		String type2 = getType(tfs2);
		String unifType = null;
		if (sig.dominates(type1,type2))
		{
			unifType = type2;
		}
		else if (sig.dominates(type2,type1))
		{
			unifType = type1;
		}
		else
		{
			failMsg("Unification failed: types " + type1 + " and " + type2 + " are incompatible.");
			return null;
		}
		Map<String,IEntity> featVals1 = featValMap(tfs1);
		Map<String,IEntity> featVals2 = featValMap(tfs2);
		Map<String,IEntity> unifFeatVals = new HashMap<String,IEntity>();
		for (String feat : sig.getAppropriateness(unifType).keySet())
		{
			IEntity ent1 = featVals1.get(feat);
			IEntity ent2 = featVals2.get(feat);
			if (ent1 == null && ent2 == null)
			{
				unifFeatVals.put(feat, signatureMGS(sig.getAppropriateValueType(unifType,feat), sig));
			}
			else if (ent2 == null)
			{
				//TODO: enforce type restriction if necessary (via TTF specialization)
				unifFeatVals.put(feat, ent1);
			}
			else if (ent1 == null)
			{
				//TODO: enforce type restriction if necessary (via TTF specialization)
				unifFeatVals.put(feat, ent2);
			}
			else
			{
				//unification of the two values
				IEntity res = unify(ent1,ent2,sig);
				if (res == null) return null;
				unifFeatVals.put(feat, res);
			}
		}
		List<String> featList = new LinkedList<String>();
		featList.addAll(unifFeatVals.keySet());
		Collections.sort(featList);
		List<IFeatureValuePair> unifFeatVal = new LinkedList<IFeatureValuePair>();
		for (String feat : featList)
		{
			unifFeatVal.add(ent.newFeatVal(feat, unifFeatVals.get(feat)));
		}
		return ent.newTFS(unifType, unifFeatVal);
	}
	
	private static IEntity unify(IList l1, IList l2, TraleSLDSignature sig)
	{
		IList resultList = ent.newList();
		List<IEntity> ents1 = new LinkedList<IEntity>();
		int i = 0;
		for (IEntity ent2 : l2.elements())
		{
			if (i >= ents1.size())
			{
				resultList.append(ent2);
			}
			else
			{
				IEntity res = unify(ents1.get(i), ent2, sig);
				if (res == null) return null;
				resultList.append(res);
			}
			i++;
		}
		while (i < ents1.size())
		{
			resultList.append(ents1.get(i));
			i++;
		}
		return resultList;
	}
	
	private static IEntity unify(ITag t1, ITag t2, TraleSLDSignature sig)
	{
		IEntity res = unify(((ITag) t1).target(), ((ITag) t2).target(), sig);
		if (res == null) return null;
		//TODO: equate the tag numbers throughout the structure, not only locally
		int number1 = ((ITag) t1).number();
		int number2 = ((ITag) t2).number();
		int newNumber = number1;
		if (number2 < number1) newNumber = number2;
		return ent.newTag(newNumber,res);
	}
	
	private static Map<String,IEntity> featValMap(ITypedFeatureStructure fs)
	{
		HashMap<String,IEntity> featValMap = new HashMap<String,IEntity>();
		for (IFeatureValuePair fval : fs.featureValuePairs())
		{
			featValMap.put(fval.feature(), fval.value());
		}
		return featValMap;
	}
	
	/**
	 * Retrieves the substructure of an IEntity at a given a path.
	 * @param e the entity to address into
	 * @param path - a list of strings encoding a path of features
	 * @return the IEntity at the path in e, null if no such structure was found
	 */
	public static IEntity go(IEntity e, List<String> path)
	{
		return goSubpath(e,path,0,path.size());
	}
	
	private static IEntity goSubpath(IEntity e, List<String> path, int start, int end)
	{
		//failMsg("goSubpath(" + e + "," + path + "," + start + "," + end + ")");
		if (start < 0 || end < start) return null;
		if (start == end)
		{
			//if (e instanceof ITag) return ((ITag) e).target();
			return e;
		}
		else
		{
			if (e  instanceof ITypedFeatureStructure)
			{
				String feat = path.get(start);
				ITypedFeatureStructure fs = (ITypedFeatureStructure) e;
				for (int i = 0; i < fs.featureValuePairs().size(); i++)
				{
					if (fs.featureValuePairs().get(i).feature().equals(feat))
					{
						return goSubpath(fs.featureValuePairs().get(i).value(), path, start+1, end);
					}
				}
			}
			else if (e instanceof IList)
			{
				int i = 0;
				Iterator<IEntity> entIt = ((IList) e).elements().iterator();
				IEntity selectedItem = entIt.next();
				while (path.get(start + i).equals("tl"))
				{
					selectedItem = entIt.next();
					i++;
				}
				if (!path.get(start + i).equals("hd")) return null;
				return goSubpath(selectedItem,path,start+i+1,end);
			}
			else if (e instanceof ITag)
			{
				return goSubpath(((ITag) e).target(),path,start,end);
			}
		}
		return null;
	}
	
	public static IEntity goUpToLast(IEntity e, List<String> path)
	{
		//special treatment for list case
		int lastBeforeList = path.size() - 1;
		if (path.get(lastBeforeList).equals("hd"))
		{
			lastBeforeList--;
			while (path.get(lastBeforeList).equals("tl"))
			{
				lastBeforeList--;
			}
			//System.err.println("upToLast: " + path.subList(0, lastBeforeList + 1));
			return goSubpath(e,path,0, lastBeforeList + 1);
		}
		else
		{
			return goSubpath(e,path,0, path.size() - 1);
		}
	}
	
	public static IEntity goLast(IEntity e, List<String> path)
	{
		//special treatment for list case
		int lastBeforeList = path.size() - 1;
		if (path.get(lastBeforeList).equals("hd"))
		{
			lastBeforeList--;
			while (path.get(lastBeforeList).equals("tl"))
			{
				lastBeforeList--;
			}
			//System.err.println("last: " + path.subList(lastBeforeList + 1, path.size()));
			return goSubpath(e,path,lastBeforeList + 1, path.size());
		}
		else
		{
			return goSubpath(e,path,path.size() - 1, path.size());
		}
	}
	
	private static boolean setType(IEntity e, String ty)
	{
		if (e instanceof IType)
		{
			IType type = (IType) e;
			type.setTypeName(ty);
			return true;
		}
		else if (e instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure fs = (ITypedFeatureStructure) e;
			fs.type().setTypeName(ty);
			return true;
		}
		return false;
	}
	
	public static IEntity replacePaste(IEntity e, List<String> path, IEntity paste, TraleSLDSignature sig)
	{
		//System.err.println("replacePaste(" + e + "," + path + "," + paste + "," + sig + ");");
		IEntity parent = goUpToLast(e,path);
		IEntity et = goLast(parent,path);
		if (parent == null) et = e;	
		if (et == null)
		{
			failMsg("Paste failed: Unable to evaluate address!");
			return e;
		}
		if (et instanceof ITag)
		{
			parent = et;
			et = ((ITag) et).target();
		}
		//System.err.println("parent = " + parent);
		//System.err.println("et = " + et);
		successMsg("Replacement paste successful.");
		if (parent == null) return paste;
		replace(parent,et,paste);
		return e;
	}
	
	private static void replace(IEntity parent, IEntity e, IEntity replacement)
	{
		//System.err.println("replace(" + parent + "," + e + "," + replacement + ");");
		if (parent instanceof ITypedFeatureStructure)
		{
			
			ITypedFeatureStructure fs = (ITypedFeatureStructure) parent;
			for (IFeatureValuePair fv : fs.featureValuePairs())
			{
				if (fv.value() == e)
				{
					fv.setValue(replacement);
					return;
				}
			}
		}
		else if (parent instanceof IList)
		{
			IList list = (IList) parent;
			List<IEntity> elList = new LinkedList<IEntity>();
			for (IEntity el : list.elements())
			{
				if (el == e)
				{
					elList.add(replacement);
				}
				else
				{
					elList.add(el);
				}
			}
			list.clear();
			for (IEntity el : elList)
			{
				list.append(el);
			}
		}
		else if (parent instanceof ITag)
		{
			((ITag) parent).setTarget(replacement);
		}
		else
		{
			System.err.println("Replace failed at " + parent);
		}
	}
	
	public static int listLength(IList list)
	{
		int i = 0;
		//the selected block is ">"; set index to end of list
		for (@SuppressWarnings("unused") IEntity e : list.elements())
		{
			i++;
		}
		return i;
	}
	
	public static String getType(IEntity e)
	{
		String type = "?";
		if (e instanceof IType)
		{
			IType ty = (IType) e;
			type = ty.typeName();
		}
		else if (e instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure fs = (ITypedFeatureStructure) e;
			type = fs.typeName();
		}
		else if (e instanceof IList)
		{
			type = "list";
		}
		//the way to deal with mgsat(Type) for the moment
		if (type.startsWith("mgsat("))
		{
			type = type.substring(6, type.length() - 1);
		}
		return type;
	}
	
	public static Map<Integer,List<List<String>>> getIdentities(IEntity e)
	{
		Map<Integer,List<List<String>>> identities = new HashMap<Integer,List<List<String>>>();
		List<String> currentPath = new LinkedList<String>();
		fillIdentities(e,currentPath,identities);
		/*for (Integer i : identities.keySet())
		{
			System.err.println(i + " -> " + identities.get(i));
		}*/
		return identities;
	}
	
	private static void fillIdentities(IEntity ent, List<String> path, Map<Integer,List<List<String>>> identities)
	{
		List<String> currentPath = new LinkedList<String>();
		currentPath.addAll(path);
		if (ent instanceof ITag)
		{
			ITag tag = (ITag) ent;
			int i = tag.number();
			List<List<String>> paths = identities.get(i);
			if (paths == null)
			{
				paths = new LinkedList<List<String>>();
				identities.put(i, paths);
			}
			paths.add(currentPath);
			fillIdentities(tag.target(),currentPath,identities);
		}
		else if (ent instanceof IAny)
		{

		}
		else if (ent instanceof IList)
		{
			IList list = (IList) ent;
			currentPath.add("hd");
			for (IEntity lEnt : list.elements())
			{
				fillIdentities(lEnt, currentPath, identities);
				currentPath.add(currentPath.size() - 1, "tl");
			}
		}
		else if (ent instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure tfs = (ITypedFeatureStructure) ent;
			for (IFeatureValuePair fv : tfs.featureValuePairs())
			{
				fillIdentities(fv, currentPath, identities);
			}
		}
		else if (ent instanceof IType)
		{

		}
		else if (ent instanceof IFeatureValuePair)
		{
			IFeatureValuePair fv = (IFeatureValuePair) ent;
			currentPath.add(fv.feature());
			fillIdentities(fv.value(), currentPath, identities);
		}
	}
	
	public static List<String> listFeatures(IEntity e)
	{
		List<String> features = new LinkedList<String>();
		if (e instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure fs = (ITypedFeatureStructure) e;
			for (IFeatureValuePair fv : fs.featureValuePairs())
			{
				features.add(fv.feature());
			}
		}
		return features;
	}
	
	public static IEntity signatureMGS(String type, TraleSLDSignature sig)
	{
		if (!sig.getTypes().contains(type))
		{
			return ent.newType("mgsat(" + type + ")");
		}
		IEntity struct;
		if (type.equals("e_list"))
		{
			struct = ent.newList();
		}
		else if (type.equals("ne_list"))
		{
			IList list = ent.newList();
			list.append(ent.newTFS("bot"));
			struct = list;
		}
		else
		{
			List<IFeatureValuePair> fvList = new LinkedList<IFeatureValuePair>();
			Map<String,String> appropFeats = sig.getTypeRestrictions(type);
			List<String> appropFeatsList = new LinkedList<String>();
			appropFeatsList.addAll(appropFeats.keySet());
			Collections.sort(appropFeatsList);
			for (String feat : appropFeatsList)
			{
				fvList.add(ent.newFeatVal(feat, signatureMGS(appropFeats.get(feat), sig)));
			}
			struct = ent.newTFS(type,fvList);
		}
		return struct;
	}
	
	public static IEntity grisuToGraleJ(String grisu)
	{
		IDataPackage data = null;
		try
		{
			data = VisualizationUtility.getDefault().parseGrisu(grisu);
		}
		catch (ParseException e)
		{
			failMsg("Parsing of GRISU string failed!" + e.getMessage());
			return null;
		}
		return (IEntity) data.getModel();
	}
	
	public static String convertGraleJToGrisu(IEntity ent)
	{
		int[] counter = {0};
		StringBuilder s = new StringBuilder("!newdata\"grisu\"");
		HashMap<Integer,IEntity> ref = new HashMap<Integer,IEntity>();
		graleJToGrisu(ent, s, counter, ref);
		while (ref.size() > 0)
		{
			resolveReferenced(s,counter,ref);
		}
		s.append("\n");
		return s.toString();
	}
	
	private static void resolveReferenced(StringBuilder s, int[] counter, Map<Integer,IEntity> ref)
	{
		int number = ref.keySet().iterator().next();
		IEntity target = ref.remove(number);
		s.append("(R");
		s.append(counter[0]++);
		s.append(" ");
		s.append(number);
		graleJToGrisu(target,s,counter,ref);
		s.append(")");
	}
	
	private static void graleJToGrisu(IEntity ent, StringBuilder s, int[] counter, Map<Integer,IEntity> ref)
	{
		if (ent instanceof IList)
		{
			IList list = (IList) ent;
			s.append("(L");
			s.append(counter[0]++);
			for (IEntity lEnt : list.elements())
			{
				graleJToGrisu(lEnt, s, counter, ref);
			}
			//TODO: correct treatment of the tail when processing lists
			s.append(")");
		}
		else if (ent instanceof IAny)
		{
			IAny atom = (IAny) ent;
			s.append("(S");
			s.append(counter[0]++);
			s.append("(");
			s.append(counter[0]++);
			s.append("\"");
			s.append(atom.value());
			s.append("\"");
			s.append("))");
		}
		else if (ent instanceof ITag)
		{
			ITag tag = (ITag) ent;
			s.append("(#");
			s.append(counter[0]++);
			s.append(" ");
			s.append(tag.number());
			ref.put(tag.number(), tag.target());
			s.append(")");
		}
		else if (ent instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure tfs = (ITypedFeatureStructure) ent;
			s.append("(S" + (counter[0] + 1));
			graleJToGrisu(tfs.type(), s, counter, ref);
			counter[0]++;
			for (IFeatureValuePair fv : tfs.featureValuePairs())
			{
				graleJToGrisu(fv, s, counter, ref);
			}
			s.append(")");
		}
		else if (ent instanceof IType)
		{
			IType type = (IType) ent;
			s.append("(");
			s.append(counter[0]++);
			s.append("\"");
			s.append(type.text());
			s.append("\"");
			s.append(")");
		}
		else if (ent instanceof IFeatureValuePair)
		{
			IFeatureValuePair fv = (IFeatureValuePair) ent;
			s.append("(V");
			s.append(counter[0]++);
			s.append("\"");
			s.append(fv.feature());
			s.append("\"");
			graleJToGrisu(fv.value(), s, counter, ref);
			s.append(")");
		}
	}
	
	private static void failMsg(String string)
	{
		if (editor != null)
		{
			editor.failureMessage(string);
		}
		else
		{
			System.err.println(string);
		}
	}
	
	private static void successMsg(String string)
	{
		if (editor != null)
		{
			editor.success(string);
		}
		else
		{
			failMsg(string);
		}
	}
}
