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
import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaStep;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.event.KahinaControlEvent;
import org.kahina.core.event.KahinaDialogEvent;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.event.KahinaWarnEvent;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.util.ProgressMonitorWrapper;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.text.KahinaTextView;

public class KahinaGUI implements KahinaListener
{
	private static final boolean VERBOSE = false;

	protected KahinaInstance<?, ?, ?> kahina;

	KahinaSelectionHistory selectionHistory;

	Map<String,List<KahinaControlButton>> controlWindows;

	protected KahinaTextView messageConsoleView;

	protected List<KahinaView<?>> views;
	protected KahinaWindowManager windowManager;

	//TODO: overhaul the treatment of clones that is currently inconsistent
	protected Set<KahinaView<?>> livingViews;

	Map<Field, KahinaView<? extends KahinaObject>> fieldToView;
	protected Map<String, KahinaView<? extends KahinaObject>> varNameToView;

	Class<? extends KahinaStep> stepType;

	public KahinaGUI(Class<? extends KahinaStep> stepType, KahinaInstance<?, ?, ?> kahina, KahinaController control)
	{
		if (VERBOSE)
		{
			System.err.println("creating Kahina GUI...");
		}
		this.stepType = stepType;
		this.kahina = kahina;
		control.registerListener(KahinaEventTypes.STEP_FOCUS, this);
		control.registerListener(KahinaEventTypes.SELECTION, this);
		control.registerListener(KahinaEventTypes.DIALOG, this);
		control.registerListener(KahinaEventTypes.CONTROL, this);
		control.registerListener(KahinaEventTypes.WARN, this);

		this.selectionHistory = new KahinaSelectionHistory(control);
		
		this.windowManager = createWindowManager(this, control);

		this.controlWindows = new HashMap<String,List<KahinaControlButton>>();

		this.views = new ArrayList<KahinaView<?>>();

		this.livingViews = new HashSet<KahinaView<?>>();
		this.fieldToView = new HashMap<Field, KahinaView<? extends KahinaObject>>();
		this.varNameToView = new HashMap<String, KahinaView<? extends KahinaObject>>();
		fillFieldToView(stepType, control);

		messageConsoleView = new KahinaTextView(control);
		messageConsoleView.setTitle("Message console");
		control.registerListener("message", messageConsoleView);
		control.registerListener("console line", messageConsoleView);
		views.add(messageConsoleView);
		livingViews.add(messageConsoleView);
		varNameToView.put("messageConsole", messageConsoleView);
	}

	/**
	 * overwrite this to specify a non-standard mapping from step properties to views
	 * 
	 * @param stepType
	 */
	protected void fillFieldToView(Class<? extends KahinaStep> stepType, KahinaController control)
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
				KahinaView<?> newView = KahinaViewRegistry.generateViewFor(field.getType(), control);
				control.registerListener("update", newView);
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
	
	public KahinaInstance<?,?,?> getKahinaInstance()
	{
		return kahina;
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

	public void prepare(KahinaController control)
	{
		windowManager.createWindows(KahinaPerspective.generateDefaultPerspective(varNameToView));
		displayMainViews();
	}	

	protected KahinaWindowManager createWindowManager(KahinaGUI kahinaGUI, KahinaController control)
	{
		return new KahinaWindowManager(this, control);
	}

	public final void show()
	{
		windowManager.displayWindows();
	}

	public void displayMainViews()
	{
		
	}

	public void displayStepContent(int stepID)
	{
		KahinaStep step = KahinaRunner.retrieve(KahinaStep.class, stepID);
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
			processEvent((KahinaDialogEvent) e);
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
				KahinaRunner.processEvent(new KahinaUpdateEvent(stepID));
				KahinaRunner.processEvent(new KahinaRedrawEvent());
			}
		}
		// for special case of isolated view components: there is no coordinating KahinaGUI
		else
		{
			if (VERBOSE)
			{
				System.err.println("Updating and redrawing (isolated view component)...");
			}
			e.getPanel().view.processEvent(new KahinaUpdateEvent(e.getSelectedStep()));
			e.getPanel().processEvent(new KahinaRedrawEvent());
		}
	}

	protected void processEvent(KahinaDialogEvent e)
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
