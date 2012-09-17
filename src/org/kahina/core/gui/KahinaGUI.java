package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import org.kahina.core.KahinaException;
import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaStep;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.control.KahinaWarnEvent;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.gui.windows.KahinaMainWindow;
import org.kahina.core.util.ProgressMonitorWrapper;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.text.KahinaTextView;

public class KahinaGUI implements KahinaListener
{
	private static final boolean VERBOSE = false;

	protected final KahinaInstance<?, ?, ?, ?> kahina;

	KahinaSelectionHistory selectionHistory;

	public Map<String,List<KahinaControlButton>> controlWindows;

	// FIXME get rid of this generics warning
	protected KahinaTextView messageConsoleView;

	protected List<KahinaView<?>> views;
	protected KahinaWindowManager windowManager;

	//TODO: overhaul the treatment of clones, which is currently inconsistent
	protected Set<KahinaView<?>> livingViews;

	private Map<Field, KahinaView<? extends KahinaObject>> fieldToView;
	protected Map<String, KahinaView<? extends KahinaObject>> varNameToView;

	Class<? extends KahinaStep> stepType;

	public KahinaGUI(Class<? extends KahinaStep> stepType, KahinaInstance<?, ?, ?, ?> kahina)
	{
		if (VERBOSE)
		{
			System.err.println("creating Kahina GUI...");
		}
		this.stepType = stepType;
		this.kahina = kahina;
		kahina.getGuiControl().registerListener(KahinaEventTypes.STEP_FOCUS, this);
		kahina.getGuiControl().registerListener(KahinaEventTypes.SELECTION, this);
		kahina.getGuiControl().registerListener(KahinaEventTypes.DIALOG, this);
		kahina.getGuiControl().registerListener(KahinaEventTypes.CONTROL, this);
		kahina.getGuiControl().registerListener(KahinaEventTypes.WARN, this);
	}
	
	protected void initialize()
	{
	    this.selectionHistory = new KahinaSelectionHistory(kahina);
	        
        this.windowManager = createWindowManager();

        this.controlWindows = new HashMap<String,List<KahinaControlButton>>();

        this.views = new ArrayList<KahinaView<?>>();

        this.livingViews = new HashSet<KahinaView<?>>();
        this.fieldToView = new HashMap<Field, KahinaView<? extends KahinaObject>>();
        this.varNameToView = new HashMap<String, KahinaView<? extends KahinaObject>>();
        fillFieldToView(stepType, kahina);

        messageConsoleView = new KahinaTextView(kahina);
        messageConsoleView.setTitle("Message console");
        kahina.getGuiControl().registerListener("message", messageConsoleView);
        kahina.getGuiControl().registerListener("console line", messageConsoleView);
        views.add(messageConsoleView);
        livingViews.add(messageConsoleView);
        varNameToView.put("messageConsole", messageConsoleView);
	}

	/**
	 * overwrite this to specify a non-standard mapping from step properties to views
	 * 
	 * @param stepType
	 */
	protected void fillFieldToView(Class<? extends KahinaStep> stepType, KahinaInstance<?, ?, ?, ?> kahina)
	{
		if (VERBOSE)
		{
			System.err.println("Generating views for step fields:");
		}
		for (Field field : stepType.getFields())
		{
			if (VERBOSE)
			{
				System.err.println("\tfield: " + field.getName() + "\n\t\tclass: " + field.getType());
			}
			if (KahinaObject.class.isAssignableFrom(field.getType()))
			{
				KahinaView<?> newView = KahinaViewRegistry.generateViewFor(field.getType(), kahina);
				kahina.getGuiControl().registerListener("update", newView);
				if (VERBOSE)
				{
					System.err.println("\t\tview: " + newView);
				}
				newView.setTitle(field.getName());
				fieldToView.put(field, newView);
				varNameToView.put(field.getName(), newView);
				views.add(newView);
				livingViews.add(newView);
			}
		}
	}
	
	public KahinaInstance<?,?,?,?> getKahinaInstance()
	{
		return kahina;
	}
    
    public KahinaPerspective getPerspective()
    {
        return windowManager.getPerspective();
    }
    
    public void setPerspective(KahinaPerspective perspective)
    {
        windowManager.setAndApplyPerspective(perspective);
    }
	
	public KahinaMainWindow getMainWindow()
	{
	    return windowManager.mainWindow;
	}
	
	public void addControlButton(String windowTitle, String iconFilePath, String command, String toolTipText, int mnemonic)
	{
		List<KahinaControlButton> list = controlWindows.get(windowTitle);
		if (list == null)
		{
			list = new LinkedList<KahinaControlButton>();
			controlWindows.put(windowTitle, list);
		}
		list.add(new KahinaControlButton(iconFilePath, command, toolTipText, mnemonic));
	}

	/*public KahinaWindow getWindowForVarName(String varName)
	{
		KahinaView<?> view = varNameToView.get(varName);
		if (view != null)
		{
			return windowManager.topLevelWindows.get(view);
		}
		return null;
	}*/

	public void integrateWindows(int integrationType, int window1ID, int window2ID, String newTitle, KahinaController control)
	{
		switch (integrationType)
		{
			case KahinaViewIntegrationType.VERTICAL:
			{
				windowManager.integrateInVerticallySplitWindow(window1ID, window2ID, newTitle, control);
				break;
			}
			case KahinaViewIntegrationType.HORIZONTAL:
			{
				windowManager.integrateInHorizontallySplitWindow(window1ID, window2ID, newTitle, control);
				break;
			}
			case KahinaViewIntegrationType.TABBED:
			{
				//TODO
			}
		}
	}

	public void prepare()
	{
		displayMainViews();
	    windowManager.createWindows();
	}	
	
	public KahinaWindowManager getWindowManager()
	{
	    return windowManager;
	}

	protected KahinaWindowManager createWindowManager()
	{
        System.err.println("MUCGUI.createWindowManager()");
		return new KahinaWindowManager(kahina);
	}

	public final void show()
	{
		windowManager.displayWindows();
	}

	public void displayMainViews()
	{
	    //System.err.println("WARNING: called stub version of KahinaGUI.displayMainViews()! This should not happen!");
	}

	public void displayStepContent(int stepID)
	{
		KahinaStep step = kahina.getState().retrieve(KahinaStep.class, stepID);
		for (Field field : fieldToView.keySet())
		{
			try
			{
				fieldToView.get(field).display((KahinaObject) field.get(step));
			} 
			catch (IllegalAccessException e)
			{

				throw new KahinaException("Problem in displaying step content!", e);
			}
		}
	}

	public void processEvent(KahinaEvent e)
	{
		if (e instanceof KahinaSelectionEvent)
		{
			processEvent((KahinaSelectionEvent) e);
		} 
		else if (e instanceof KahinaDialogEvent)
		{
			processDialogEvent((KahinaDialogEvent) e);
		} 
		else if (e instanceof KahinaControlEvent)
		{
			processEvent((KahinaControlEvent) e);
		} 
		else if (e instanceof KahinaWarnEvent)
		{
			processEvent((KahinaWarnEvent) e);
		}
	}

	private void processEvent(KahinaSelectionEvent e)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".processEvent(" + e + ")");
		}
		if (e.getPanel() == null || livingViews.contains(e.getPanel().view))
		{
			if (VERBOSE)
			{
				System.err.println("Updating and redrawing...");
			}
			// ignore selections that would lead to an empty current node
			int stepID = e.getSelectedStep();
			if (stepID != -1)
			{
				if (VERBOSE)
				{
					System.err.println("Updating selection to step " + stepID);
				}
				displayStepContent(stepID);
				kahina.dispatchEvent(new KahinaUpdateEvent(stepID, e.getLayer()));
				kahina.dispatchEvent(new KahinaRedrawEvent());
			}
		}
		// for special case of isolated view components: there is no coordinating KahinaGUI
		else
		{
			if (VERBOSE)
			{
				System.err.println("Updating and redrawing (isolated view component)...");
			}
			e.getPanel().view.processEvent(new KahinaUpdateEvent(e.getSelectedStep(), e.getLayer()));
			e.getPanel().processEvent(new KahinaRedrawEvent());
		}
	}
	
	public KahinaPerspective generateInitialPerspective()
	{
	    return KahinaPerspective.generateDefaultPerspective(varNameToView);
	}

	protected void processDialogEvent(KahinaDialogEvent e)
	{
		switch (e.getDialogEventType())
		{
			case KahinaDialogEvent.ABOUT:
			{
				new AboutDialog(windowManager.mainWindow).setVisible(true);
			}
		}
	}

	private void processEvent(KahinaControlEvent e)
	{
		String command = e.getCommand();
		if (command.equals("backInHistory"))
		{
			selectionHistory.moveToPrevious();
		} else if (command.equals("forwardInHistory"))
		{
			selectionHistory.moveToNext();
		}
	}

	private void processEvent(KahinaWarnEvent e)
	{
		JOptionPane.showMessageDialog(windowManager.mainWindow, "Warn point " + e.getBreakpoint() + " has matched " + e.getMatchCount() + " times.", "Warn point", JOptionPane.WARNING_MESSAGE);
	}

	private class AboutDialog extends JDialog
	{
		private static final long serialVersionUID = 1004459218135806816L;

		public AboutDialog(JFrame parent)
		{
			super(parent, "About Kahina", false);
			getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
			add(new JLabel("Kilian Evang, Johannes Dellert"), "West");
			add(new JLabel("Tuebingen University"), "West");
			add(new JLabel("(c) 2009-2011"), "West");

			JPanel p2 = new JPanel();
			JButton ok = new JButton("Ok");
			p2.add(ok);
			getContentPane().add(p2, "South");

			ok.addActionListener(new ActionListener()
			{
				public void actionPerformed(ActionEvent evt)
				{
					setVisible(false);
				}
			});
			setSize(280, 110);
			setLocation(parent.getX() + 200, parent.getY() + 200);
		}
	}

	public ProgressMonitorWrapper createProgressMonitorWrapper(String message, String note, int min, int max)
	{
		return new ProgressMonitorWrapper(windowManager.mainWindow, message, note, min, max);
	}

	public int showConfirmDialog(Object message, String title, int optionType)
	{
		return JOptionPane.showConfirmDialog(windowManager.mainWindow, message, title, optionType);
	}

	public void showMessageDialog(Object message, String title, int messageType)
	{
		JOptionPane.showMessageDialog(windowManager.mainWindow, message, title, messageType);
	}

}
