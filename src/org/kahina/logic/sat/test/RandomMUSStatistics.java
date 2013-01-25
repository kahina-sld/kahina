package org.kahina.logic.sat.test;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;


import org.kahina.logic.sat.muc.test.MinUnsatCore;

public class RandomMUSStatistics
{
    public static void main(String[] args)
    {
        if (args.length != 1)
        {
            System.err.println("Usage: java RandomMUSStatistics [cnf file list]");
            System.exit(1);
        }
        List<String> fileList = new LinkedList<String>();
        Scanner fileListScanner;
        try
        {
            fileListScanner = new Scanner(new File(args[0]));
            while (fileListScanner.hasNextLine())
            {
                fileList.add(fileListScanner.nextLine());
            }
        }
        catch (FileNotFoundException e)
        {
            System.err.println("ERROR: file list \"" + args[0] + "\" not found!");
            System.exit(1);
        }
        for (String fileName : fileList)
        {
            System.err.print(fileName + ": ");
            
            List<Integer> sizes = MinUnsatCore.computeRandomMUCs(10, fileName, fileName + ".trg", false, 14400000);
             
            double sum = 0.0;
            int min = Integer.MAX_VALUE;
            int max = Integer.MIN_VALUE;
            
            for (int size : sizes)
            {
                sum += size;
                if (size < min)
                {
                    min = size;
                }
                if (size > max)
                {
                    max = size;
                }
            }   
            System.err.println(sizes + " (" + min + "," + max + "," + (sum / sizes.size()) + ")");
        }
    }
}
