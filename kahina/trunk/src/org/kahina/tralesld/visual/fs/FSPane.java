package org.kahina.tralesld.visual.fs;

import gralej.parsers.ParseException;

import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;

import javax.swing.JLabel;
import javax.swing.JPanel;

public class FSPane extends JPanel implements ComponentListener
{
	
	private static final long serialVersionUID = -8884702142909964244L;

	private boolean fresh = false;
	
	private VisualizationUtility util;
	
	private String grisuMessage;

	public FSPane(String grisuMessage, VisualizationUtility util)
	{
		this.grisuMessage = grisuMessage;
		this.util = util;
		addComponentListener(this);
	}

	@Override
	public void componentHidden(ComponentEvent e)
	{
		// do nothing
		
	}

	@Override
	public void componentMoved(ComponentEvent e)
	{
		// do nothing
		
	}

	@Override
	public void componentResized(ComponentEvent e)
	{
		// do nothing
		
	}

	@Override
	public void componentShown(ComponentEvent e)
	{
		if (!fresh)
		{
			try
			{
				add(util.visualize(grisuMessage));
			} catch (ParseException e1)
			{
				add(new JLabel("Parse error: " + e1.getMessage()));
			}
		}
		fresh = true;
		repaint();
	}

}
