package org.kahina.logic.sat.muc.test;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.io.cnf.CfgToSatConverter;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.parse.data.cfg.ContextFreeGrammar;
import org.kahina.parse.io.cfg.ContextFreeGrammarParser;

public class CfgParseViaSat
{
    public static void main(String[] args)
    {
        //TODO: sentence file
        if (args.length < 2)
        {
            System.err.println("Usage: java CfgParseViaSat [CFG file] \"sentence\"");
            System.exit(0);
        }
        System.err.println("Loading grammar: " + args[0]);
        ContextFreeGrammar cfg = ContextFreeGrammarParser.parseCFGFile(args[0]);
        System.err.println("Parsing sentence: " + args[1]);
        CnfSatInstance instance = CfgToSatConverter.parsingToSat(cfg, args[1].split(" "));
        File tempCnfFile = new File("cfg-parsing-tmp.cnf");
        File tempResultFile = new File("cfg-parsing-tmp.res");
        try
        {
            tempResultFile.createNewFile();
            DimacsCnfOutput.writeDimacsCnfFile("cfg-parsing-tmp.cnf", instance);

            //TODO: display results symbolically (positive atoms for success, MUS for failure)
            boolean parseSuccess = MiniSAT.isSatisfiable(tempCnfFile, tempResultFile);
            if (parseSuccess)
            {
                System.err.println("Parse successful!");
            }
            else
            {
                System.err.println("Parse failed!");
            }
            tempCnfFile.delete();
            tempResultFile.delete();
        }
        catch (TimeoutException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (InterruptedException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (IOException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
