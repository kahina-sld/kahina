package org.kahina.tralesld;

import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaStep;
import org.kahina.core.data.DataManager;
import org.kahina.core.data.DbDataManager;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.data.source.KahinaSourceFileModel;
import org.kahina.core.data.text.KahinaConstantDbTextStore;
import org.kahina.core.data.text.KahinaText;
import org.kahina.core.data.text.KahinaTextModel;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.tralesld.bridge.TraleSLDBridge;
import org.kahina.tralesld.data.fs.TraleSLDPackedFS;
import org.kahina.tralesld.data.fs.TraleSLDPackedFSNonTerminal;
import org.kahina.tralesld.data.fs.TraleSLDPackedFSTerminal;
import org.kahina.tralesld.data.fs.TraleSLDVariableBinding;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;

public class TraleSLDRunner extends KahinaRunner
{
	public static void main(String[] args)
	{
		TraleSLDBridge bridge = runAndGetBridge();
		bridge.initializeParseTrace("[she,thinks,she,gives,her,milk]");
		bridge.registerChartEdge(0, 5, 6, "lexicon");
		bridge.registerStepInformation(1, "rule_close", "close chart edge under rule application");
		bridge.registerStepLocation(1, 0);
		bridge.registerRuleApplication(2, "subject_head_rule", 0, "apply rule, subject_head_rule"); // TODO
		bridge.registerStepSourceCodeLocation(2, "/home/johannes/pro/kahina/trale/test_gram/theory3.pl", 185);
		bridge.registerStepLocation(2, 1);
		bridge.registerEdgeRetrieval(0);
		bridge.registerStepInformation(3, "unify(Subj)", "unify edge with Subj");
		bridge.registerStepSourceCodeLocation(3, "/home/johannes/pro/kahina/trale/test_gram/theory3.pl", 191);
		bridge.registerStepLocation(3, 2);
		bridge.registerStepInformation(4, "type(edge,bot)", "add type, bot, to edge");
		bridge.registerStepLocation(4, 3);
		bridge.registerStepExit(4, true);
		bridge.registerStepExit(3, true);
		bridge.registerStepInformation(5, "featval(edge:phon)", "enforce description on phon value of edge");
		bridge.registerStepSourceCodeLocation(5, "/home/johannes/pro/kahina/trale/test_gram/theory3.pl", 191);
		bridge.registerStepLocation(5, 2);
		bridge.registerStepInformation(6, "unify(SubjPhon)", "unify value at phon with SubjPhon");
		bridge.registerStepSourceCodeLocation(6, "/home/johannes/pro/kahina/trale/test_gram/theory3.pl", 191);
		bridge.registerStepLocation(6, 5);
		bridge.registerStepInformation(7, "type(phon,ne_list)", "add type, ne_list, to value at phon");
		bridge.registerStepLocation(7, 6);
		bridge.registerStepExit(7, true);
		bridge.registerStepExit(6, true);
		bridge.registerStepExit(5, true);
		bridge.registerStepFailure(2);
		bridge.registerRuleApplication(8, "head_complement_rule", 0, "apply rule, head_complement_rule");
		bridge.registerStepSourceCodeLocation(8, "/home/johannes/pro/kahina/trale/test_gram/theory3.pl", 195);
		bridge.registerStepLocation(8, 1);
		bridge.registerEdgeRetrieval(0);
		bridge.registerStepInformation(9, "unify(Head)", "unify edge with Head");
		bridge.registerStepSourceCodeLocation(9, "/home/johannes/pro/kahina/trale/test_gram/theory3.pl", 201);
		bridge.registerStepLocation(9, 8);
		bridge.registerStepInformation(10, "type(edge,bot)", "add type, bot, to edge");
		bridge.registerStepLocation(10, 9);
	}

	public static void initialize(int dataHandlingType)
	{
		KahinaRunner.initialize(dataHandlingType);
		DataManager dm = KahinaRunner.getDataManager();
		dm.registerDataType(KahinaStep.class);
		dm.registerDataType(LogicProgrammingStep.class);
		dm.registerDataType(TraleSLDStep.class);
		dm.registerDataType(TraleSLDPackedFSNonTerminal.class);
		dm.registerDataType(TraleSLDPackedFSTerminal.class);
		dm.registerDataType(KahinaSourceCodeLocation.class);
		dm.registerDataType(KahinaSourceFileModel.class);
		dm.registerDataType(TraleSLDVariableBinding.class);
		dm.registerDataType(TraleSLDVariableBindingSet.class);
		dm.registerDataType(KahinaTextModel.class);
		// *slaps himself for the following code* TODO think of a more elegant
		// and general solution for such switches.
		if (dm instanceof DbDataManager)
		{
			dm.registerDataType(KahinaText.class, new KahinaConstantDbTextStore((DbDataManager) dm, getDatabaseHandler()));
		} else
		{
			dm.registerDataType(KahinaText.class);
		}
	}

	public static TraleSLDBridge runAndGetBridge()
	{
		try
		{
			System.err.println("Starting TraleSLD instance...");
			initialize(KahinaDataHandlingMethod.MEMORY);
			TraleSLDInstance kahina = new TraleSLDInstance();
			kahina.getGUI().prepare();
			kahina.getGUI().buildAndShow();
			return kahina.getBridge();
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(-1);
			return null;
		}
	}
}
