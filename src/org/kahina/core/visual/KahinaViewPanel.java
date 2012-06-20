package org.kahina.core.visual;

import java.awt.Component;
import java.lang.reflect.InvocationTargetException;

import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import org.kahina.core.KahinaException;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.gui.KahinaProgressBar;
import org.kahina.core.gui.event.KahinaRedrawEvent;

public abstract class KahinaViewPanel<T extends KahinaView<?>> extends JPanel implements KahinaListener
{
	private static final long serialVersionUID = 5677332450070203832L;

	private static final boolean VERBOSE = false;

	public T view;
    
    protected JScrollPane topScrollPane;
    protected JPanel topScrollPanel;
	
    //a standardized progress bar for views which require extensive computations
    protected KahinaProgressBar progressBar;
    JComponent progressBarParent;
    
    public KahinaViewPanel()
    {
       topScrollPanel = new JPanel();
       topScrollPanel.setLayout(new BoxLayout(topScrollPanel,BoxLayout.LINE_AXIS));
       topScrollPane = new JScrollPane(topScrollPanel);
       super.add(topScrollPane);
    }
    
    //TODO: overload other methods inherited from JPanel as well, precluding unexpected behavior
    
    public Component add(Component comp)
    {
        return topScrollPanel.add(comp);
    }
    
    public Component add(Component comp, int index)
    {
        return topScrollPanel.add(comp, index);
    }
    
    public void add(Component comp, Object constraint)
    {
        topScrollPanel.add(comp, constraint);
    }
    
    public void add(Component comp, Object constraint, int index)
    {
        topScrollPanel.add(comp, constraint);
    }
    
    public void removeAll()
    {
        topScrollPanel.removeAll();
    }
    
    public void revalidate()
    {
        super.revalidate();
        if (topScrollPanel != null) topScrollPanel.revalidate();
    }

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
        catch (InterruptedException e)
        {
            e.printStackTrace();
        }
        catch (InvocationTargetException e)
        {
            e.printStackTrace();
        }
	}

	/**
	 * This method must be called from the Swing event dispatch thread.
	 */
	public abstract void updateDisplay();
	
    public void showProgressBar()
    {
        if (progressBarParent != null)
        {
            progressBarParent.add(progressBar);
            progressBarParent.revalidate();
        }
    }
    
    public void hideProgressBar()
    {
        if (progressBarParent != null)
        {
            progressBarParent.remove(progressBar);
            progressBarParent.revalidate();
        }
    }

    public void setProgressBar(KahinaProgressBar progressBar)
    {
        if (progressBar != null)
        {
            this.progressBar = progressBar;  
            this.progressBarParent = (JComponent) progressBar.getParent();
            hideProgressBar();
        }
    }
}
