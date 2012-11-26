package org.kahina.logic.sat.io.cnf;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.parse.data.cfg.ContextFreeGrammar;
import org.kahina.parse.io.cfg.ContextFreeGrammarParser;
import org.kahina.parse.io.cfg.SentenceGenerator;

public class CfgInstanceGenerator
{
    public static void main(String[] args)
    {
        //TODO: sentence file
        if (args.length < 2)
        {
            System.err.println("Usage: java CfgInstanceGenerator [CFG file] [instance dir]");
            System.exit(0);
        }
        System.err.println("Loading grammar: " + args[0]);
        ContextFreeGrammar cfg = ContextFreeGrammarParser.parseCFGFile(args[0]);
        
        //systematically generating all terminal sequences up to length 10 over instance-source.cfg
        //   => would amount to |words|^10 = 11^10 = 25.937.424.601 sentences!
        
        //more interesting approach: generate sentences, delete or swap at one position
        //   => we do not get "the the man man", but things like "man sleeps", "the sleeps man" etc.
        
        SentenceGenerator gen = new SentenceGenerator(cfg);
        
        for (int i = 0; i < 100; i++)
        {
            //TODO: add errors to these sentences, write to file & check for satisfiability
            int[] iWrapper = new int[1];
            iWrapper[0] = i;
            System.err.print(i);
            System.err.println("\nGenerated sentence:" + gen.generateGrammaticalSentence("s", iWrapper));
        }
        
        /*System.err.println("Parsing sentence: " + args[1]);
        //TODO: optionally generate more interesting symbols (replace ranges by substrings!)
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
        }*/
    }
}
