package org.kahina.tralesld.gui;

import java.awt.Color;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.kahina.core.KahinaException;
import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaStep;
import org.kahina.core.control.KahinaController;
import org.kahina.core.event.KahinaControlEvent;
import org.kahina.core.event.KahinaDialogEvent;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.io.util.XMLUtilities;
import org.kahina.core.profiler.ProfileEntry;
import org.kahina.core.util.Mapper;
import org.kahina.core.visual.chart.KahinaChartView;
import org.kahina.core.visual.tree.KahinaLayeredTreeView;
import org.kahina.core.visual.tree.KahinaLayeredTreeViewPanel;
import org.kahina.core.visual.tree.KahinaListTreeView;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.tralesld.TraleSLDInstance;
import org.kahina.tralesld.TraleSLDStepType;
import org.kahina.tralesld.data.chart.TraleSLDChartEdgeStatus;
import org.kahina.tralesld.data.tree.TraleSLDLayerDecider;
import org.kahina.tralesld.event.TraleSLDControlEventCommands;
import org.kahina.tralesld.profiler.TraleSLDProfileEntryMapper;
import org.kahina.tralesld.visual.chart.TraleSLDChartEdgeDisplayDecider;
import org.kahina.tralesld.visual.signature.TraleSLDSignatureAppropriatenessView;
import org.kahina.tralesld.visual.signature.TraleSLDSignatureHierarchyView;
import org.kahina.tralesld.visual.signature.TraleSLDSignatureUsageView;

public class TraleSLDGUI extends LogicProgrammingGUI
{
	private TraleSLDInstance instance;

	protected KahinaChartView mainChartView;
	
	protected TraleSLDSignatureHierarchyView signatureHierarchyView;
	protected TraleSLDSignatureAppropriatenessView signatureAppropriatenessView;
	protected TraleSLDSignatureUsageView signatureUsageView;

	public TraleSLDGUI(Class<? extends KahinaStep> stepType, TraleSLDInstance instance, KahinaController control)
	{
		super(stepType, instance, control);
		this.instance = instance;

		mainChartView = new KahinaChartView(control);
		mainChartView.setTitle("Chart");
		views.add(mainChartView);
		livingViews.add(mainChartView);
		varNameToView.put("chart", mainChartView);
		
		signatureHierarchyView = new TraleSLDSignatureHierarchyView(control);
		signatureHierarchyView.setTitle("Type Hierarchy");
		views.add(signatureHierarchyView);
		livingViews.add(signatureHierarchyView);
		varNameToView.put("signature-hierarchy", signatureHierarchyView);
		
		signatureAppropriatenessView = new TraleSLDSignatureAppropriatenessView(control);
		signatureAppropriatenessView.setTitle("Appropriateness Conditions");
		views.add(signatureAppropriatenessView);
		livingViews.add(signatureAppropriatenessView);
		varNameToView.put("signature-appropriateness", signatureAppropriatenessView);
		
		signatureUsageView = new TraleSLDSignatureUsageView(control);
		signatureUsageView.setTitle("Type Usage");
		views.add(signatureUsageView);
		livingViews.add(signatureUsageView);
		varNameToView.put("signature-usage", signatureUsageView);

		mainTreeView.setStatusColorEncoding(TraleSLDStepType.FINISHED, new Color(102, 51, 153));

		mainChartView.setStatusColorEncoding(TraleSLDChartEdgeStatus.PROSPECTIVE, new Color(192, 192, 192));
		mainChartView.setStatusColorEncoding(TraleSLDChartEdgeStatus.SUCCESSFUL, new Color(102, 153, 102));
		mainChartView.setStatusColorEncoding(TraleSLDChartEdgeStatus.FAILED, new Color(183, 50, 50));
		mainChartView.setStatusHighlightColorEncoding(TraleSLDChartEdgeStatus.PROSPECTIVE, new Color(240, 240, 240));
		mainChartView.setStatusHighlightColorEncoding(TraleSLDChartEdgeStatus.SUCCESSFUL, Color.GREEN);
		mainChartView.setStatusHighlightColorEncoding(TraleSLDChartEdgeStatus.FAILED, Color.RED);
		mainChartView.setDisplayDecider(new TraleSLDChartEdgeDisplayDecider());
	}

	@Override
	protected KahinaWindowManager createWindowManager(KahinaGUI gui, KahinaController control)
	{
		return new TraleSLDWindowManager(gui, control);
	}

	/*
	 * @Override protected KahinaListTreeView generateTreeView(KahinaController
	 * control) { return new KahinaListTreeView(control, 0, 1, 2); }
	 */

	@Override
	protected KahinaLayeredTreeView generateTreeView(KahinaController control)
	{
		return new KahinaLayeredTreeView(KahinaLayeredTreeViewPanel.Orientation.VERTICAL, control, 0, 1, 2);
	}

	@Override
	public void displayMainViews()
	{
		super.displayMainViews();
		// set deciders here because the trees are generated generically by the KahinaState
		mainTreeView.getModel().setLayerDecider(new TraleSLDLayerDecider(2));
		mainTreeView.getSecondaryModel().setLayerDecider(new TraleSLDLayerDecider(2));
		mainChartView.display(instance.getState().getChart());
		signatureHierarchyView.display(instance.getState().getSignature());
		signatureAppropriatenessView.display(instance.getState().getSignature());
		signatureUsageView.display(instance.getState().getSignature());
	}

	@Override
	public void prepare(KahinaController control)
	{
		try
		{
			displayMainViews();

			// TODO: load last perspective instead of only default perspective
			// from XML
			InputStream xmlStream = new BufferedInputStream(TraleSLDGUI.class.getResourceAsStream("tralesld-manywindows.xml"));
			windowManager.createWindows(KahinaPerspective.importXML(XMLUtilities.parseXMLStream(xmlStream, false).getDocumentElement()));
			xmlStream.close();
		} catch (NullPointerException e)
		{
			e.printStackTrace();
		} catch (IOException e)
		{
			throw new KahinaException("Failed to prepare GUI", e);
		}
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

			if (chooser.showOpenDialog(windowManager.mainWindow) == JFileChooser.APPROVE_OPTION)
			{
				File grammar = chooser.getSelectedFile();

				if (grammar != null)
				{
					KahinaRunner.processEvent(new KahinaControlEvent(TraleSLDControlEventCommands.COMPILE, new Object[] { grammar.getAbsolutePath() }));
				}
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
