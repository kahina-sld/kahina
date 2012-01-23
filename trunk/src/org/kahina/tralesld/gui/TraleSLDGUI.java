package org.kahina.tralesld.gui;

import gralej.controller.Controller;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.kahina.core.KahinaException;
import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaStep;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaDialogEvent;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.gui.KahinaWindow;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
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
import org.kahina.tralesld.control.TraleSLDControlEventCommands;
import org.kahina.tralesld.data.chart.TraleSLDChartEdgeStatus;
import org.kahina.tralesld.data.fs.TraleSLDFS;
import org.kahina.tralesld.data.fs.TraleSLDPackedFSTerminal;
import org.kahina.tralesld.data.tree.TraleSLDLayerDecider;
import org.kahina.tralesld.data.workbench.FeatureWorkbench;
import org.kahina.tralesld.profiler.TraleSLDProfileEntryMapper;
import org.kahina.tralesld.visual.chart.TraleSLDChartEdgeDisplayDecider;
import org.kahina.tralesld.visual.signature.TraleSLDSignatureAppropriatenessView;
import org.kahina.tralesld.visual.signature.TraleSLDSignatureHierarchyView;
import org.kahina.tralesld.visual.signature.TraleSLDSignatureUsageView;
import org.kahina.tralesld.visual.workbench.FeatureWorkbenchView;

public class TraleSLDGUI extends LogicProgrammingGUI
{
	private TraleSLDInstance instance;

	protected KahinaChartView mainChartView;
	
	protected TraleSLDSignatureHierarchyView signatureHierarchyView;
	protected TraleSLDSignatureAppropriatenessView signatureAppropriatenessView;
	protected TraleSLDSignatureUsageView signatureUsageView;
	
	private boolean withWorkbench = false;
	protected FeatureWorkbenchView workbenchView;
	
	//needed to generate additional views (e.g. workbenches) after initialization
	protected KahinaController control;

	public TraleSLDGUI(Class<? extends KahinaStep> stepType, TraleSLDInstance instance, KahinaController control)
	{
		super(stepType, instance, control);
		this.instance = instance;
		this.control = control;

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
		
		if (withWorkbench)
		{
			workbenchView = new FeatureWorkbenchView(control,instance.getState().getTrale());
			workbenchView.setTitle("Feature Workbench");
			views.add(workbenchView);
			livingViews.add(workbenchView);
			varNameToView.put("workbench", workbenchView);
		}

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
		
		if (withWorkbench)
		{
			FeatureWorkbench workbench = new FeatureWorkbench();
			workbench.setSignature(instance.getState().getSignature());
			//add a few feature structures for testing purposes until dragging is possible
			workbench.storeStructure("complex", "!newdata\"Edge\"(S1(0\"word\")(V2\"phon\"(L3(S5(4\"cruel\"))))(V6\"qstore\"(S8(7\"list\")))(V9\"synsem\"(S11(10\"synsem\")(V12\"loc\"(S14(13\"loc\")(V15\"cat\"(S17(16\"cat\")(V18\"determined\"(S20(19\"boolean\")))(V21\"head\"(S23(22\"adj\")(V24\"mod\"(S26(25\"synsem\")(V27\"loc\"(S29(28\"loc\")(V30\"cat\"(S32(31\"cat\")(V33\"determined\"(S35(34\"minus\")))(V36\"head\"(S38(37\"noun\")(V39\"case\"(S41(40\"case\")))(V42\"mod\"(S44(43\"synsem_none\")))(V45\"pred\"(S47(46\"boolean\")))))(V48\"val\"(S50(49\"mgsat(val)\")))))(V51\"cont\"(S53(52\"nom_obj\")(V54\"index\"(#55 1))(V56\"restr\"(#57 2))))))(V58\"nonloc\"(S60(59\"mgsat(nonloc)\")))))(V61\"pred\"(S63(62\"minus\")))))(V64\"val\"(S66(65\"val\")(V67\"subj\"(L68))(V69\"comps\"(#70 0))))))(V71\"cont\"(S73(72\"nom_obj\")(V74\"index\"(#75 1))(V76\"restr\"(L77(S79(78\"psoa\")(V80\"nucleus\"(S82(81\"adjmod\")(V83\"inst\"(#84 1))(V85\"relationname\"(S87(86\"bot\")))(V88\"soa_arg\"(S90(89\"mgsat(psoa)\")))))(V91\"quants\"(L92)))(Z93(#94 2))))))))(V95\"nonloc\"(S97(96\"mgsat(nonloc)\")))))(V98\"arg_st\"(#99 0)))(R100 2(S2(101\"list\")))(R102 0(L0))(R103 1(S1(104\"index\")(V105\"gen\"(S107(106\"gen\")))(V108\"num\"(S110(109\"num\")))(V111\"pers\"(S113(112\"pers\")))(V114\"sort\"(S116(115\"bot\")))))\n");
			workbench.storeStructure("cruel", "!newdata \"cruel\" (S1(0\"mgsat\"))(T2 \"head_subject:cruel\" 1)\n");
			workbenchView.display(workbench);
		}
	}
	
	public void signatureUpdate()
	{
		signatureHierarchyView.display(instance.getState().getSignature());
		signatureAppropriatenessView.display(instance.getState().getSignature());
		signatureUsageView.display(instance.getState().getSignature());
		if (withWorkbench)
		{
			workbenchView.getModel().setSignature(instance.getState().getSignature());
		}
		KahinaRunner.processEvent(new TraleSLDTypeSelectionEvent("bot"));
		KahinaRunner.processEvent(new KahinaRedrawEvent());
	}

	@Override
	public void prepare(KahinaController control)
	{
		try
		{
			displayMainViews();

			// TODO: load last perspective instead of only default perspective from XML
			InputStream xmlStream = new BufferedInputStream(TraleSLDGUI.class.getResourceAsStream("tralesld-manywindows.xml"));
			windowManager.createWindows(KahinaPerspective.importXML(XMLUtilities.parseXMLStream(xmlStream, false).getDocumentElement()));
			if (withWorkbench)
			{
				KahinaWindow workbenchWindow = windowManager.integrateInDefaultWindow(workbenchView);
				windowManager.registerWindow(workbenchWindow);
				workbenchWindow.setSize(800, 500);
				workbenchWindow.setLocation(200, 200);
				workbenchWindow.setVisible(true);
			}
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
	
	/**
	 * initialises a new feature workbench and creates a window for it
	 */
	public final Action NEW_WORKBENCH_ACTION = new AbstractAction("New Feature Workbench")
	{
		private static final long serialVersionUID = -5054097578696268196L;

		@Override
		public void actionPerformed(ActionEvent e)
		{
			FeatureWorkbench workbench = new FeatureWorkbench();
			workbench.setSignature(instance.getState().getSignature());
			//add a few feature structures for testing purposes until dragging is possible
			workbench.storeStructure("complex", "!newdata\"Edge\"(S1(0\"word\")(V2\"phon\"(L3(S5(4\"cruel\"))))(V6\"qstore\"(S8(7\"list\")))(V9\"synsem\"(S11(10\"synsem\")(V12\"loc\"(S14(13\"loc\")(V15\"cat\"(S17(16\"cat\")(V18\"determined\"(S20(19\"boolean\")))(V21\"head\"(S23(22\"adj\")(V24\"mod\"(S26(25\"synsem\")(V27\"loc\"(S29(28\"loc\")(V30\"cat\"(S32(31\"cat\")(V33\"determined\"(S35(34\"minus\")))(V36\"head\"(S38(37\"noun\")(V39\"case\"(S41(40\"case\")))(V42\"mod\"(S44(43\"synsem_none\")))(V45\"pred\"(S47(46\"boolean\")))))(V48\"val\"(S50(49\"mgsat(val)\")))))(V51\"cont\"(S53(52\"nom_obj\")(V54\"index\"(#55 1))(V56\"restr\"(#57 2))))))(V58\"nonloc\"(S60(59\"mgsat(nonloc)\")))))(V61\"pred\"(S63(62\"minus\")))))(V64\"val\"(S66(65\"val\")(V67\"subj\"(L68))(V69\"comps\"(#70 0))))))(V71\"cont\"(S73(72\"nom_obj\")(V74\"index\"(#75 1))(V76\"restr\"(L77(S79(78\"psoa\")(V80\"nucleus\"(S82(81\"adjmod\")(V83\"inst\"(#84 1))(V85\"relationname\"(S87(86\"bot\")))(V88\"soa_arg\"(S90(89\"mgsat(psoa)\")))))(V91\"quants\"(L92)))(Z93(#94 2))))))))(V95\"nonloc\"(S97(96\"mgsat(nonloc)\")))))(V98\"arg_st\"(#99 0)))(R100 2(S2(101\"list\")))(R102 0(L0))(R103 1(S1(104\"index\")(V105\"gen\"(S107(106\"gen\")))(V108\"num\"(S110(109\"num\")))(V111\"pers\"(S113(112\"pers\")))(V114\"sort\"(S116(115\"bot\")))))\n");
			workbench.storeStructure("cruel", "!newdata \"cruel\" (S1(0\"mgsat\"))(T2 \"head_subject:cruel\" 1)\n");
			workbenchView = new FeatureWorkbenchView(control, instance.getState().getTrale());
			workbenchView.display(workbench);
			workbenchView.setTitle("Feature Workbench");
			views.add(workbenchView);
			//TODO: this should be done more systematically
			KahinaWindow workbenchWindow = windowManager.integrateInDefaultWindow(workbenchView);
			windowManager.registerWindow(workbenchWindow);
			workbenchWindow.setSize(300, 100);
			workbenchWindow.setLocation(200, 200);
			workbenchWindow.setVisible(true);
		}
	};

	@Override
	protected void processDialogEvent(KahinaDialogEvent e)
	{
		super.processDialogEvent(e);
		int type = e.getDialogEventType();

		if (type == KahinaDialogEvent.COMPILE)
		{
			// TODO custom dialog with fancy stuff like recently compiled grammars
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
