package org.kahina.tralesld.gui;

import java.awt.Color;
import java.io.File;
import java.util.Arrays;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaStep;
import org.kahina.core.control.KahinaController;
import org.kahina.core.event.KahinaControlEvent;
import org.kahina.core.event.KahinaDialogEvent;
import org.kahina.core.gui.KahinaViewIntegrationType;
import org.kahina.core.profiler.ProfileEntry;
import org.kahina.core.util.Mapper;
import org.kahina.core.visual.chart.KahinaChartView;
import org.kahina.core.visual.tree.KahinaLayeredTreeView;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.tralesld.TraleSLDInstance;
import org.kahina.tralesld.TraleSLDStepType;
import org.kahina.tralesld.data.chart.TraleSLDChartEdgeStatus;
import org.kahina.tralesld.data.tree.TraleSLDLayerDecider;
import org.kahina.tralesld.profiler.TraleSLDProfileEntryMapper;
import org.kahina.tralesld.visual.chart.TraleSLDChartEdgeDisplayDecider;
import org.tralesld.core.event.TraleSLDControlEventCommands;

public class TraleSLDGUI extends LogicProgrammingGUI
{
	private TraleSLDInstance instance;

	protected KahinaChartView mainChartView;

	public TraleSLDGUI(Class<? extends KahinaStep> stepType, TraleSLDInstance instance, KahinaController control)
	{
		super(stepType, instance, control);
		this.instance = instance;

		mainChartView = new KahinaChartView(control);
		mainChartView.setTitle("Chart");
		views.add(mainChartView);
		livingViews.add(mainChartView);
		varNameToView.put("chart", mainChartView);

		mainTreeView.setStatusColorEncoding(TraleSLDStepType.FINISHED, new Color(102, 51, 153));
		mainTreeView.setStatusColorEncoding(TraleSLDStepType.BLOCKED, Color.BLACK);
		// TODO: build font color customization facilities into TreeView
		// mainTreeView.setStatusFontColorEncoding(TraleSLDStepType.BLOCKED,
		// Color.BLACK);

		mainChartView.setStatusColorEncoding(TraleSLDChartEdgeStatus.PROSPECTIVE, new Color(192, 192, 192));
		mainChartView.setStatusColorEncoding(TraleSLDChartEdgeStatus.SUCCESSFUL, new Color(102, 153, 102));
		mainChartView.setStatusColorEncoding(TraleSLDChartEdgeStatus.FAILED, new Color(183, 50, 50));
		mainChartView.setStatusHighlightColorEncoding(TraleSLDChartEdgeStatus.PROSPECTIVE, new Color(240, 240, 240));
		mainChartView.setStatusHighlightColorEncoding(TraleSLDChartEdgeStatus.SUCCESSFUL, Color.GREEN);
		mainChartView.setStatusHighlightColorEncoding(TraleSLDChartEdgeStatus.FAILED, Color.RED);
		mainChartView.setDisplayDecider(new TraleSLDChartEdgeDisplayDecider());
	}

	@Override
	protected KahinaLayeredTreeView generateTreeView(KahinaController control)
	{
		return new KahinaLayeredTreeView(control, 0, 1, 2);
	}

	@Override
	public void displayMainViews()
	{
		super.displayMainViews();
		// set deciders here because the trees are generated generically by the
		// KahinaState
		mainTreeView.getModel().setLayerDecider(new TraleSLDLayerDecider(2));
		mainTreeView.getSecondaryModel().setLayerDecider(new TraleSLDLayerDecider(2));
		mainChartView.display(instance.getState().getChart());
	}

	@Override
	public void prepare(KahinaController control)
	{
		super.prepare(control);
		integrateVariableDisplays(KahinaViewIntegrationType.VERTICAL, "startBindings", "endBindings", "Variable bindings", control);
		integrateVariableDisplays(KahinaViewIntegrationType.VERTICAL, "codeLocation", "messageConsole", "Source & Console", control);
		integrateVariableDisplays(KahinaViewIntegrationType.HORIZONTAL, "startFeatStruct", "endFeatStruct", "Feature Structures", control);
		// slightly hacky: allow direct manipulation of view components
		getWindowForVarName("codeLocation").setSize(400, 500);
		getWindowForVarName("codeLocation").setLocation(400, 0);
		getWindowForVarName("startFeatStruct").setSize(700, 300);
		getWindowForVarName("startFeatStruct").setLocation(0, 550);
		getWindowForVarName("startBindings").setSize(200, 300);
		getWindowForVarName("startBindings").setLocation(700, 550);
		getWindowForVarName("controlFlowTree").setSize(500, 800);
		getWindowForVarName("controlFlowTree").setLocation(800, 0);
		getWindowForVarName("chart").setSize(400, 400);
		getWindowForVarName("chart").setLocation(0, 150);
	}

	@Override
	protected Mapper<String, ProfileEntry> getProfileEntryMapper()
	{
		return new TraleSLDProfileEntryMapper();
	}

	@Override
	protected void processEvent(KahinaDialogEvent e)
	{
		super.processEvent(e);
		int type = e.getDialogEventType();

		if (type == KahinaDialogEvent.COMPILE)
		{
			// TODO custom dialog with fancy stuff like recently compiled
			// grammars
			// TODO add filter, just show .pl files per default
			File directory = new File(".");
			JFileChooser chooser = new JFileChooser(directory);
			String absolutePath = (String) e.getArguments()[0];

			if (absolutePath != null)
			{
				chooser.setSelectedFile(new File(absolutePath));
			} else
			{
				String[] files = directory.list();
				Arrays.sort(files);

				for (String file : files)
				{
					if (file.startsWith("theory") && file.endsWith(".pl"))
					{
						chooser.setSelectedFile(new File(directory, file));
						break;
					}
				}
			}

			chooser.setDialogTitle("Compile grammar");
			chooser.showOpenDialog(windowManager.mainWindow);
			File grammar = chooser.getSelectedFile();

			if (grammar != null)
			{
				KahinaRunner.processEvent(new KahinaControlEvent(TraleSLDControlEventCommands.COMPILE, new Object[] { grammar.getAbsolutePath() }));
			}
		} else if (type == KahinaDialogEvent.PARSE)
		{
			// TODO offer recent parses in a ComboBox
			String sentence = (String) JOptionPane.showInputDialog(windowManager.mainWindow, "Enter a sentence to parse, with the words separated by spaces.", "Parse sentence",
					JOptionPane.QUESTION_MESSAGE, null, null, e.getArguments()[0]);

			if (sentence != null)
			{
				KahinaRunner.processEvent(new KahinaControlEvent(TraleSLDControlEventCommands.PARSE, new Object[] { Arrays.asList(sentence.split(" +")) }));
			}
		}
	}
}
