package org.kahina.tralesld;

import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaStep;
import org.kahina.core.data.DataManager;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.tralesld.bridge.TraleSLDBridge;
import org.kahina.tralesld.data.fs.TraleSLDFeatureStructure;
import org.kahina.tralesld.data.fs.TraleSLDVariableBinding;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;

public class TraleSLDRunner extends KahinaRunner
{
    public static void main(String[] args)
    {
        TraleSLDBridge bridge = runAndGetBridge();
        bridge.initializeParseTrace("[she,thinks,she,gives,her,milk]");
        bridge.registerChartEdge(0, 5, 6, "lexicon");
        bridge.registerStepInformation(1, "rule_close");
        bridge.registerStepLocation(1,0);
        bridge.registerRuleApplication(2,5,7,"subject_head_rule");
        bridge.registerStepSourceCodeLocation(2,"/home/johannes/pro/kahina/trale/test_gram/theory3.pl",185);
        bridge.registerStepLocation(2,1);
        bridge.registerActiveEdgeDependency(0);
        bridge.registerStepInformation(3,"unify(Subj)");
        bridge.registerStepSourceCodeLocation(3,"/home/johannes/pro/kahina/trale/test_gram/theory3.pl",191);
        bridge.registerStepLocation(3,2);
        bridge.registerStepInformation(4,"type(edge,bot)");
        bridge.registerStepLocation(4,3);
        bridge.registerStepExit(4,true);
        bridge.registerStepExit(3,true);  
        bridge.registerStepInformation(5,"featval(edge:phon)");
        bridge.registerStepSourceCodeLocation(5,"/home/johannes/pro/kahina/trale/test_gram/theory3.pl",191);
        bridge.registerStepLocation(5,2);
        bridge.registerStepInformation(6,"unify(SubjPhon)");
        bridge.registerStepSourceCodeLocation(6,"/home/johannes/pro/kahina/trale/test_gram/theory3.pl",191);
        bridge.registerStepLocation(6,5);
        bridge.registerStepInformation(7,"type(phon,ne_list)");
        bridge.registerStepLocation(7,6);
        bridge.registerStepExit(7,true);
        bridge.registerStepExit(6,true);
        bridge.registerStepExit(5,true);
        bridge.registerStepFailure(2);
        bridge.registerRuleApplication(8,5,7,"head_complement_rule");
        bridge.registerStepSourceCodeLocation(8,"/home/johannes/pro/kahina/trale/test_gram/theory3.pl",195);
        bridge.registerStepLocation(8,1);
        bridge.registerActiveEdgeDependency(0);
        bridge.registerStepInformation(9,"unify(Head)");
        bridge.registerStepSourceCodeLocation(9,"/home/johannes/pro/kahina/trale/test_gram/theory3.pl",201);
        bridge.registerStepLocation(9,8);
        bridge.registerStepInformation(10,"type(edge,bot)");
        bridge.registerStepLocation(10,9);
    }
    
    public static void initialize(int dataHandlingType)
    {
        KahinaRunner.initialize(dataHandlingType);
        DataManager dm = KahinaRunner.getDataManager();
        dm.registerDataType(KahinaStep.class);
        dm.registerDataType(LogicProgrammingStep.class);
        dm.registerDataType(TraleSLDStep.class);
        dm.registerDataType(TraleSLDFeatureStructure.class);
        dm.registerDataType(KahinaSourceCodeLocation.class);
        dm.registerDataType(TraleSLDVariableBinding.class);
        dm.registerDataType(TraleSLDVariableBindingSet.class);
    }
    
    public static TraleSLDBridge runAndGetBridge()
    {
        System.err.println("Starting TraleSLD instance...");
        initialize(KahinaDataHandlingMethod.DATABASE);
        TraleSLDInstance kahina = new TraleSLDInstance();
        kahina.getGUI().buildAndShow();
        return kahina.getBridge();
    }
}
