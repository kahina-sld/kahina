package org.kahina.logic.sat.muc.test;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Scanner;
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
        if (args.length < 2)
        {
            System.err.println("Usage: java CfgParseViaSat [CFG file] [temp directory] ([sentence file])");
            System.exit(0);
        }
        String grammarFile = args[0];
        String tempDir = args[1];
        System.err.println("Loading grammar: " + grammarFile);
        ContextFreeGrammar cfg = ContextFreeGrammarParser.parseCFGFile(args[0]);
        
        Scanner in = new Scanner(System.in);
        if (args.length > 2)
        {
            try
            {
                in = new Scanner(new File(args[2]));
            }
            catch (FileNotFoundException e)
            {
                System.err.println("ERROR: file \"" + args[2] + "\" not found!");
                System.err.println("Entering interactive mode instead.");
            }
        }
        else
        {
            System.err.println("Parser is in interactive mode.");
        }
        while (in.hasNextLine())
        {
            String sentence = in.nextLine();
            System.err.println("Parsing sentence: " + sentence);
            String[] words = sentence.split(" ");
            String filename = sentence.replace(' ', '-');
            CnfSatInstance instance = CfgToSatConverter.parsingToSat(cfg, words);
            File tempCnfFile = new File(tempDir + "/" + filename + ".cnf");
            File tempResultFile = new File(tempDir + "/cfg-parsing-tmp.res");
            try
            {
                tempResultFile.createNewFile();
                DimacsCnfOutput.writeDimacsCnfFile(tempDir + "/" + filename + ".cnf", instance);

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
                //tempCnfFile.delete();
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
}
