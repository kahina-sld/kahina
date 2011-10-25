package org.kahina.core.visual;

import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import org.kahina.core.KahinaException;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.gui.event.KahinaRedrawEvent;

public abstract class KahinaViewPanel<T extends KahinaView<?>> extends JPanel implements KahinaListener
{
	private static final long serialVersionUID = 5677332450070203832L;

	private static final boolean VERBOSE = false;

	public T view;

	public void processEvent(KahinaEvent event)
	{
		if (VERBOSE)
		{
			System.err.println(this + " received " + event);
		}
		if (event instanceof KahinaRedrawEvent)
		{
			updateDisplayAndRepaintFromEventDispatchThread();
		}
	}

	public void setView(T view)
	{
		this.view = view;
		updateDisplayAndRepaintFromEventDispatchThread();
	}

	public void updateDisplayAndRepaintFromEventDispatchThread()
	{
		try
		{
			if (SwingUtilities.isEventDispatchThread()) 
			{
				updateDisplay();
				revalidate();
				repaint();
			} 
			else 
			{
			    SwingUtilities.invokeAndWait(new Runnable() 
			    {
			        @Override
			        public void run() 
			        {
						updateDisplay();
						revalidate();
						repaint();
			        }
			    });
			}
		} 
		catch (Exception e)
		{
			throw new KahinaException("Failed to update and repaint " + this + ".", e);
		}
	}

	/**
	 * This method must be called from the Swing event dispatch thread.
	 */
	public abstract void updateDisplay();
}