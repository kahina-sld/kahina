package org.kahina.tralesld.visual.fs;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.kahina.tralesld.data.signature.TraleSLDSignature;

import gralej.om.EntityFactory;
import gralej.om.IEntity;
import gralej.om.IFeatureValuePair;
import gralej.om.IList;
import gralej.om.ITag;
import gralej.om.IType;
import gralej.om.ITypedFeatureStructure;

public class GraleJUtility 
{
	static EntityFactory ent = EntityFactory.getInstance();
	
	public static IEntity specialize(IEntity e, List<String> path, String ty, TraleSLDSignature sig)
	{
		IEntity ent = go(e,path);
		if (ent == null)
		{
			System.err.println("Specialize failed: Unable to evaluate address!");
			return e;
		}
		String eType = getType(ent);
		if (eType != null && sig.dominates(eType,ty))
		{
			setType(ent,ty);
		}
		else
		{
			System.err.println("Specialize failed: Dominance condition violated!");
		}
		return e;
	}
	
	public static IEntity generalize(IEntity e, List<String> path, String ty, TraleSLDSignature sig)
	{
		IEntity ent = go(e,path);
		if (ent == null)
		{
			System.err.println("Generalize failed: Unable to evaluate address!");
			return e;
		}
		String eType = getType(ent);
		if (eType != null && sig.dominates(ty,eType))
		{
			setType(ent,ty);
		}
		else
		{
			System.err.println("Specialize failed: Dominance condition violated!");
		}
		return e;
	}
	
	public static IEntity introFeat(IEntity e, List<String> path, String feat, IEntity val, TraleSLDSignature sig)
	{
		IEntity parent = goUpToLast(e,path);
		IEntity et = goLast(parent,path);
		if (et == null)
		{
			System.err.println("Feature introduction failed: Unable to evaluate address!");
			return e;
		}
		IFeatureValuePair fv = ent.newFeatVal(feat, val); 
		if (et instanceof IType)
		{
			List<IFeatureValuePair> fvList = new LinkedList<IFeatureValuePair>();
			fvList.add(fv);
			IType type = (IType) et;
			ITypedFeatureStructure replacement = ent.newTFS(type, fvList);
			if (parent == null) return replacement;
			replace(parent,et,replacement);
		}
		else if (et instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure fs = (ITypedFeatureStructure) et;
			fs.addFeatureValue(fv);
			return fs;
		}
		System.err.println("Feature introduction failed: not possible in context.");
		return e;
	}
	
	public static IEntity remFeat(IEntity e, String feat)
	{
		if (e  instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure fs = (ITypedFeatureStructure) e;
			for (int i = 0; i < fs.featureValuePairs().size(); i++)
			{
				if (fs.featureValuePairs().get(i).feature().equals(feat))
				{
					fs.featureValuePairs().remove(i);
					return fs;
				}
			}
		}
		System.err.println("Operation failed: remFeat");
		return e;
	}
	
	//TODO: makes this functional
	public static IEntity introIdent(IEntity e, IEntity e1, IEntity e2, TraleSLDSignature sig)
	{
		ent.newTag(0, e2);
		return e;
	}
	
	//TODO: makes this functional
	public static IEntity remIdent(IEntity e, IEntity e1)
	{
		if (e1 instanceof ITag)
		{
			return ((ITag) e).target();
		}
		return e1;
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
		if (start < 0 || end < start) return null;
		if (start == end)
		{
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
		return goSubpath(e,path,0, path.size() - 1);
	}
	
	public static IEntity goLast(IEntity e, List<String> path)
	{
		return goSubpath(e,path,path.size() - 1, path.size());
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
	
	private static void replace(IEntity parent, IEntity ent, IEntity replacement)
	{
		//TODO: implement
	}
	
	public static String getType(IEntity e)
	{
		if (e instanceof IType)
		{
			IType type = (IType) e;
			return type.text();
		}
		else if (e instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure fs = (ITypedFeatureStructure) e;
			return fs.typeName();
		}
		return null;
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
			s.append(")");
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
}
