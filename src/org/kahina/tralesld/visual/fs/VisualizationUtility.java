package org.kahina.tralesld.visual.fs;

import gralej.Config;
import gralej.blocks.BlockPanel;
import gralej.controller.StreamInfo;
import gralej.om.EntityFactory;
import gralej.om.IEntity;
import gralej.om.IFeatureValuePair;
import gralej.om.IList;
import gralej.om.IType;
import gralej.om.ITypedFeatureStructure;
import gralej.parsers.GraleParserFactory;
import gralej.parsers.IDataPackage;
import gralej.parsers.IGraleParser;
import gralej.parsers.ParseException;
import gralej.parsers.UnsupportedProtocolException;

import java.io.ByteArrayInputStream;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * 
 * @author ke
 */
public class VisualizationUtility
{
	private static final boolean verbose = false;

	private static VisualizationUtility def;

	private IGraleParser parser;

	public static VisualizationUtility getDefault()
	{
		if (def == null)
		{
			def = new VisualizationUtility();
		}
		return def;
	}

	public VisualizationUtility()
	{
		try
		{
			parser = GraleParserFactory.createParser(StreamInfo.GRISU);
		} catch (UnsupportedProtocolException e)
		{
			throw new RuntimeException("could not create Grisu format parser", e);
		}

		Config config = gralej.Config.currentConfig();
		config.set("behavior.selectOnClick", true);
		config.set("block.panel.different.background.color", "0xffffaa");
		config.set("behavior.nodeContentInitiallyVisible", true);
		config.set("behavior.autoexpandtags", true);
		config.set("behavior.alwaysfitsize", false);
	}

	public IDataPackage parseGrisu(String grisuMessage) throws ParseException
	{
		IDataPackage dataPackage = null;
		try
		{
			if (verbose)
			{
				System.err.println(this + ".parseGrisu(" + grisuMessage + ")");
			}
			dataPackage = parser.parseAll(new ByteArrayInputStream(grisuMessage.getBytes()), StreamInfo.GRISU).get(0);
		} catch (ParseException e)
		{
			JPanel result = new JPanel();
			result.add(new JLabel("Parse error: \n" + e.getMessage() + "\nGrisu message was: \n" + grisuMessage));
			// TODO: handle error message
			System.err.println("GRISU parse error!");
			throw e;
		}
		return dataPackage;
	}

	/**
	 * 
	 * @param grisuMessage
	 *            A typed feature structure or tree in Grisu format.
	 * @return A GraleJ block panel, providing various methods to control
	 *         rendering, and a method called <code>getCanvas()</code> to obtain
	 *         the actual {@link JPanel}.
	 */
	public BlockPanel visualize(String grisuMessage)
	{
		BlockPanel blockPanel = null;
		try
		{
			if (verbose)
			{
				System.err.println(this + ".visualize(" + grisuMessage + ")");
			}
			blockPanel = parseGrisu(grisuMessage).createView();
		} catch (ParseException e)
		{
			// TODO: restore display of error messages; display empty list as
			// temporary solution
			blockPanel = new BlockPanel(EntityFactory.getInstance().newList());
		}
		return blockPanel;
	}

	public JPanel makeJPanel(String grisuMessage)
	{
		try
		{
			return parseGrisu(grisuMessage).createView().getCanvas();
		} catch (ParseException e)
		{
			JPanel errorPanel = new JPanel();
			errorPanel.add(new JLabel("Parse error: \n" + e.getMessage() + "\nGrisu message was: \n" + grisuMessage));
			return errorPanel;
		}
	}

	public JPanel createFSFrame(String varName, String grisuMessage)
	{
		JPanel result = new JPanel();
		result.add(makeJPanel(grisuMessage));
		result.setBorder(BorderFactory.createTitledBorder(varName));
		return result;
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
				s.append(lEnt);
			}
			s.append(")");
		}
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