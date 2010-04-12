package org.kahina.tralesld;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.DataManager;
import org.kahina.core.data.DbDataManager;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.data.MemDataManager;
import org.kahina.core.io.database.DatabaseHandler;
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
        bridge.registerRuleApplication(2,5,7,"subject_head_rule");
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
