package org.kahina.logic.sat.muc.test;

import java.io.File;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.MetaLearningMode;
import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.muc.io.MUCExtension;

public class UCReducerTest
{
    public static void main(String[] args)
    {
        //MUCBridge bridge = null;
        CnfSatInstance satInstance = null;
        MUCStatistics stat = null;
        MiniSATFiles files = null;
        MUCInstance kahinaInstance = null;
        if (args.length > 0)
        {
            satInstance = DimacsCnfParser.parseDimacsCnfFile(args[0]);
            System.err.println("Starting Kahina for MinUnsatCore on SAT instance at " + args[0]);
            System.err.println("  Instance Size: (" + satInstance.getNumClauses() + "," + satInstance.getNumVariables() + ")");
            
            stat = new MUCStatistics();
            stat.instanceName = args[0];
            
            files = new MiniSATFiles();
            files.sourceFile = new File(args[0]);
            files.createTargetFile("test-target");
            files.createExtendedFile("test-target");
            files.createTempFiles("test-target-seed");
            
            MUCExtension.extendCNFBySelVars(files.sourceFile, files.tmpFile, stat);
            
            kahinaInstance = new MUCInstance(MetaLearningMode.BLOCK_PARTITION, satInstance, stat, files);
            kahinaInstance.startNewSessionWithoutBridge();
            
            kahinaInstance.generateFirstUC();
        }
        else
        {
            kahinaInstance = new MUCInstance(MetaLearningMode.BLOCK_PARTITION);
            kahinaInstance.startNewSessionWithoutBridge();
        }
    }
    
}
