package org.kahina.tralesld.visual.fs;

import gralej.Config;
import gralej.blocks.BlockPanel;
import gralej.controller.StreamInfo;
import gralej.om.EntityFactory;
import gralej.parsers.GraleParserFactory;
import gralej.parsers.IGraleParser;
import gralej.parsers.ParseException;
import gralej.parsers.UnsupportedProtocolException;

import java.io.ByteArrayInputStream;

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
		} 
		catch (UnsupportedProtocolException e)
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

	/**
	 * 
	 * @param grisuMessage
	 *            A typed feature structure or tree in Grisu format.
	 * @return A GraleJ block panel, providing various methods to control rendering, and a
	 *         method called <code>getCanvas()</code> to obtain the actual
	 *         {@link JPanel}.
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
			blockPanel = parser.parseAll(new ByteArrayInputStream(grisuMessage.getBytes()), StreamInfo.GRISU).get(0).createView();
		} 
		catch (ParseException e)
		{
			JPanel result = new JPanel();
			result.add(new JLabel("Parse error: \n" + e.getMessage() + "\nGrisu message was: \n" + grisuMessage));
			//TODO: restore display of error messages; display empty list as temporary solution
			blockPanel = new BlockPanel(EntityFactory.getInstance().newList());
		}
		return blockPanel;
	}
}