package org.kahina.core.gui.event;

import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.visual.KahinaViewPanel;

/**
 * Event to indicate a certain step has been "marked" internally in the main
 * tree view and all views should be updated accordingly.
 * 
 * @author ke
 * 
 */
public class KahinaSelectionEvent extends KahinaEvent
{
	private final int selectedStep;

	private final KahinaViewPanel<?> panel;

	private final int layer;

	public KahinaSelectionEvent(int selectedStep)
	{
		this(selectedStep, -1);
	}
	
	public KahinaSelectionEvent(int selectedStep, int layer)
	{
		this(selectedStep, layer, null);
	}

	public KahinaSelectionEvent(int selectedStep, int layer, KahinaViewPanel<?> panel)
	{
		super(KahinaEventTypes.SELECTION);
		this.selectedStep = selectedStep;
		this.layer = layer;
		this.panel = panel;
	}

	public int getSelectedStep()
	{
		return selectedStep;
	}

	/**
	 * @return the layer of the GUI element that fired this event, or -1 if it
	 *         was not fired by a GUI element associated with a specific layer
	 */
	public int getLayer()
	{
		return layer;
	}

	public KahinaViewPanel<?> getPanel()
	{
		return panel;
	}

	@Override
	public String toString()
	{
		return "select node " + selectedStep;
	}
}
