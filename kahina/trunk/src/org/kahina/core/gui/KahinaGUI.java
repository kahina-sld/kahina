package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.Box;
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
import org.kahina.core.event.KahinaDialogEvent;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.gui.breakpoint.BreakpointEditorWindow;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.text.KahinaTextView;
import org.kahina.core.visual.tree.KahinaLayeredTreeView;

public class KahinaGUI implements KahinaListener
{
	private static final boolean verbose = false;

	KahinaInstance<?, ?, ?> kahina;

	KahinaControlPanel controlPanel;

	protected KahinaLayeredTreeView mainTreeView;
    
    protected KahinaTextView messageConsoleView;

	protected List<KahinaView<?>> views;
	// values as defined in KahinaViewVisibility
	Map<KahinaView<?>, Integer> viewVisibility;
	KahinaWindow window;

	protected Set<KahinaView<?>> livingViews;

	Map<Field, KahinaView<? extends KahinaObject>> fieldToView;

	Class<? extends KahinaStep> stepType;

	public KahinaGUI(Class<? extends KahinaStep> stepType, KahinaInstance<?, ?, ?> kahina)
	{
		if (verbose)
		{
			System.err.println("creating Kahina GUI...");
		}
		this.stepType = stepType;
		this.kahina = kahina;
		KahinaRunner.getControl().registerListener(KahinaEventTypes.STEP_FOCUS, this);
		KahinaRunner.getControl().registerListener(KahinaEventTypes.SELECTION, this);
        KahinaRunner.getControl().registerListener("dialog", this);

		this.controlPanel = new KahinaControlPanel();

		this.views = new ArrayList<KahinaView<?>>();
		this.viewVisibility = new HashMap<KahinaView<?>, Integer>();

		this.livingViews = new HashSet<KahinaView<?>>();
		this.fieldToView = new HashMap<Field, KahinaView<? extends KahinaObject>>();
		fillFieldToView(stepType);

		mainTreeView = new KahinaLayeredTreeView(0, 1);
		mainTreeView.setTitle("Control flow tree");
		KahinaRunner.getControl().registerListener(KahinaEventTypes.UPDATE, mainTreeView);
		views.add(mainTreeView);
		livingViews.add(mainTreeView);
        
        messageConsoleView = new KahinaTextView();
        messageConsoleView.setTitle("Message console");
        KahinaRunner.getControl().registerListener("message", messageConsoleView);
        KahinaRunner.getControl().registerListener("console line", messageConsoleView);
        views.add(messageConsoleView);
        livingViews.add(messageConsoleView);
	}

	/**
	 * overwrite this to specify a non-standard mapping from step properties to
	 * views
	 * 
	 * @param stepType
	 */
	protected void fillFieldToView(Class<? extends KahinaStep> stepType)
	{
		if (verbose)
		{
			System.err.println("Generating views for step fields:");
		}
		for (Field field : stepType.getFields())
		{
			if (verbose)
			{
				System.err.println("\tfield: " + field.getName() + "\n\t\tclass: " + field.getType());
			}
			if (KahinaObject.class.isAssignableFrom(field.getType()))
			{
				KahinaView<?> newView = KahinaViewRegistry.generateViewFor(field.getType());
				KahinaRunner.getControl().registerListener("update", newView);
				if (verbose)
				{
					System.err.println("\t\tview: " + newView);
				}
				newView.setTitle(field.getName());
				fieldToView.put(field, newView);
				views.add(newView);
				livingViews.add(newView);
			}
		}
	}

	public KahinaControlPanel getControlPanel()
	{
		return controlPanel;
	}

	public final void buildAndShow()
	{
		displayMainViews();
		window = new KahinaWindow(this);
	}

	protected void displayMainViews()
	{
		mainTreeView.display(kahina.getState().getStepTree());
		mainTreeView.displaySecondaryTree(kahina.getState().getSecondaryStepTree());
        //Bad Software Design: KahinaTextViews do not really have models
        //messageConsoleView.display();
	}

	public void displayStepContent(int stepID)
	{
		KahinaStep step = KahinaRunner.getDataManager().retrieve(stepType, stepID);
		for (Field field : fieldToView.keySet())
		{
			try
			{
				fieldToView.get(field).display((KahinaObject) field.get(step));
			} catch (IllegalAccessException e)
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
	}

	private void processEvent(KahinaSelectionEvent e)
	{
		if (e.getPanel() == null || livingViews.contains(e.getPanel().view))
		{
            //ignore selections that would lead to an empty current node
            if (e.getSelectedStep() != -1)
            {
                int selectedStep = e.getSelectedStep();
                displayStepContent(selectedStep);
                KahinaRunner.processEvent(new KahinaUpdateEvent(selectedStep));
                KahinaRunner.processEvent(new KahinaRedrawEvent());
            }
		}
		// for special case of isolated view components: there is no
		// coordinating KahinaGUI
		else
		{
			e.getPanel().view.processEvent(new KahinaUpdateEvent(e.getSelectedStep()));
			e.getPanel().processEvent(new KahinaRedrawEvent());
		}
	}
    
    private void processEvent(KahinaDialogEvent e)
    {
        switch (e.getDialogEventType())
        {
            case KahinaDialogEvent.BREAKPOINTS:
            {
                BreakpointEditorWindow breakpointEditor = new BreakpointEditorWindow(new KahinaController());
                breakpointEditor.setVisible(true);
                break;
            }
            case KahinaDialogEvent.ABOUT:
            {
                new AboutDialog(window).setVisible(true);        
            }
        }
    }
    
    private class AboutDialog extends JDialog 
    {
        public AboutDialog(JFrame parent) 
        {
          super(parent, "About Kahina", false);
          getContentPane().setLayout(new BoxLayout(getContentPane(),BoxLayout.Y_AXIS));
          add(new JLabel("Kilian Evang, Johannes Dellert"));
          add(new JLabel("Tuebingen University"));
          add(new JLabel("(c) 2009-2010"));

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
          setSize(250, 150);
        }
    }
}
