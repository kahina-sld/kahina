package org.kahina.tralesld;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.DataManager;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.tralesld.bridge.TraleSLDBridge;

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
        bridge.registerStepInformation(3,"unify(Subj)");
        bridge.registerStepLocation(3,2);
        bridge.registerStepInformation(4,"type(edge,bot)");
        bridge.registerStepLocation(4,3);
    }
    
    public static void initialize(int dataHandlingType)
    {
        KahinaRunner.initialize(dataHandlingType);
        DataManager dm = KahinaRunner.getDataManager();
        dm.registerDataType(LogicProgrammingStep.class);
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
