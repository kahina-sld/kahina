package org.kahina.tulipa;

import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaStep;
import org.kahina.core.data.DataManager;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.data.source.KahinaSourceFileModel;
import org.kahina.core.data.text.KahinaText;
import org.kahina.core.data.text.KahinaTextModel;
import org.kahina.tulipa.bridge.TulipaBridge;

public class TulipaRunner extends KahinaRunner
{
    public static void initialize(KahinaDataHandlingMethod dataHandlingType)
    {
        KahinaRunner.initialize(dataHandlingType);
        DataManager dm = KahinaRunner.getDataManager();
        dm.registerDataType(KahinaStep.class);
        dm.registerDataType(KahinaSourceCodeLocation.class);
        dm.registerDataType(KahinaSourceFileModel.class);
        dm.registerDataType(KahinaTextModel.class);
        dm.registerDataType(TulipaStep.class);
        dm.registerDataType(KahinaText.class);
        /*dm.registerDataType(KahinaLineReference.class);
        dm.registerDataType(LogicProgrammingLineReference.class);
        dm.registerDataType(KahinaText.class);*/
    }
    
    public static TulipaBridge runAndGetBridge()
    {
        System.err.println("Starting Kahina.TuLiPa instance...");
        initialize(KahinaDataHandlingMethod.MEMORY);
        TulipaInstance kahina = new TulipaInstance();
        kahina.getGUI().prepare();
        kahina.getGUI().buildAndShow();
        return kahina.getBridge();
    }
}
