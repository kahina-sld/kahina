package org.kahina.core.visual;

import java.awt.Component;

import javax.swing.JComponent;

import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;

public abstract class KahinaView<T extends KahinaObject> implements KahinaListener
{
	private static final boolean verbose = false;

	// the title of the tab or window this view is displayed in
	private String title = "Unnamed View";

	protected KahinaViewConfiguration config;

	protected T model;
	
	public KahinaController control;

	public KahinaView(KahinaController control)
	{
		this.control = control;
		this.title = "New View";
	}

	public void processEvent(KahinaEvent e)
	{
		if (verbose)
		{
			System.err.println(this + " received " + e);
		}
		if (e instanceof KahinaUpdateEvent)
		{
			processEvent((KahinaUpdateEvent) e);
		} else if (e instanceof KahinaSelectionEvent)
		{
			processEvent((KahinaSelectionEvent) e);
		}
	}

	protected void processEvent(KahinaUpdateEvent e)
	{
		recalculate();
	}

	// only listens to this in absence of KahinaGUI; do not register as listener in KahinaGUI case!!!
	private void processEvent(KahinaSelectionEvent e)
	{
		control.processEvent(new KahinaUpdateEvent(e.getSelectedStep()));
		control.processEvent(new KahinaRedrawEvent());
	}

	/**
	 * Subclasses may override this method to take certain actions directly
	 * after {@link #model} has changed. This default implementation does
	 * nothing.
	 */
	protected void doDisplay()
	{
		// do nothing
	}

	@SuppressWarnings("unchecked")
	public final void display(KahinaObject model)
	{
		this.model = (T) model;
		doDisplay();
	}

	public T getModel()
	{
		return model;
	}

	/**
	 * Override this method to define necessary operations after changes to the
	 * model (coordinate recomputations etc.)
	 */
	protected void recalculate()
	{

	}

	/**
	 * Returns the panel that represents this view in the GUI. This method must
	 * be invoked from the event dispatch thread.
	 * @return
	 */
	public abstract JComponent makePanel();

	public String getTitle()
	{
		return title;
	}

	public void setTitle(String title)
	{
		this.title = title;
	}

	public void setConfig(KahinaViewConfiguration config)
	{
		this.config = config;
	}

	public KahinaViewConfiguration getConfig()
	{
		return config;
	}

	public Component makeEditorPanel(KahinaGUI gui) 
	{
		return makePanel();
	}
}
