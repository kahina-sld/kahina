package org.kahina.logic.sat.muc.test;

import java.io.File;

public class MUCTestRunner
{
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        if (args.length < 2)
        {
            System.err.println("Required Arguments:  Source Dir, Target Dir");
        }
        else
        {
            File sourceDir = new File(args[0]);
            File targetDir = new File(args[1]);
            if (sourceDir.isDirectory() && targetDir.isDirectory())
            {
                for (File file : sourceDir.listFiles())
                {
                    String targetFileName = targetDir.getAbsolutePath() + "/" + file.getName() + ".stat";
                    MinUnsatCore.computeMUC(file.getAbsolutePath(), targetFileName, false, 14400000);
                }
            }
            else
            {
                System.err.println("Both source and target need to be directories!");
            }
        }
        System.exit(0);
    }
}
