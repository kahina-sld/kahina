package org.kahina.tralesld.visual.fs;

import org.kahina.tralesld.data.signature.TraleSLDSignature;

import gralej.om.EntityFactory;
import gralej.om.IEntity;
import gralej.om.IFeatureValuePair;
import gralej.om.IList;
import gralej.om.IType;
import gralej.om.ITypedFeatureStructure;

public class GraleJUtility 
{
	
	public static boolean specialize(IEntity e, String ty, TraleSLDSignature sig)
	{
		String eType = getType(e);
		if (eType != null && sig.dominates(eType,ty))
		{
			return setType(e,ty);
		}
		return false;
	}
	
	public static boolean generalize(IEntity e, String ty, TraleSLDSignature sig)
	{
		String eType = getType(e);
		if (eType != null && sig.dominates(ty,eType))
		{
			return setType(e,ty);
		}
		return false;
	}
	
	/*public static boolean introFeat(IEntity e, String feat, TraleSLDSignature sig)
	{
		
	}
	
	public static boolean remFeat(IEntity e, String feat)
	{
		
	}*/
	
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
	
	public static String convertGraleJToGrisu(IEntity ent)
	{
		int[] counter = {0};
		StringBuilder s = new StringBuilder("!newdata\"grisu\"");
		graleJToGrisu(ent, s, counter);
		s.append("\n");
		return s.toString();
	}
	
	private static void graleJToGrisu(IEntity ent, StringBuilder s, int[] counter)
	{
		if (ent instanceof IList)
		{
			IList list = (IList) ent;
			s.append("(L");
			s.append(counter[0]++);
			for (IEntity lEnt : list.elements())
			{
				graleJToGrisu(lEnt, s, counter);
			}
			s.append(")");
		}
		/*else if (ent instanceof IAny)
		{
			IList list = (IList) ent;
			s.append("(L");
			s.append(counter[0]++);
			for (IEntity lEnt : list.elements())
			{
				graleJToGrisu(lEnt, s, counter);
			}
			s.append(")");
		}*/
		else if (ent instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure tfs = (ITypedFeatureStructure) ent;
			s.append("(S" + (counter[0] + 1));
			graleJToGrisu(tfs.type(), s, counter);
			counter[0]++;
			for (IFeatureValuePair fv : tfs.featureValuePairs())
			{
				graleJToGrisu(fv, s, counter);
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
			graleJToGrisu(fv.value(), s, counter);
			s.append(")");
		}
	}
}
