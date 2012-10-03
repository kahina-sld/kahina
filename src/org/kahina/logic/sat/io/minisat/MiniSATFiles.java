package org.kahina.logic.sat.io.minisat;

import java.io.File;
import java.io.IOException;

public class MiniSATFiles
{
    public File sourceFile;
    public File targetFile;
    public File tmpFile;
    public File tmpResultFile;
    public File tmpProofFile;
    public File tmpFreezeFile;
    
    public void createTargetFile(String targetFileName)
    {
        targetFile = new File(targetFileName);
        try
        {
            targetFile.createNewFile();
        }
        catch (IOException e)
        {
            e.printStackTrace();
            System.err.println("IO error: failed to create target file");
            System.exit(0);
        }
    }
    
    public void createExtendedFile(String targetFileName)
    {
        tmpFile = new File(targetFileName.concat("tmp"));
        try
        {
            tmpFile.createNewFile();
        }
        catch (IOException e)
        {
            e.printStackTrace();
            System.err.println("IO error: failed to create extended file");
            System.exit(0);
        }
    }
    
    public void createTempFiles(String targetFileName)
    {
        tmpResultFile = new File(targetFileName.concat("erg"));
        tmpProofFile = new File(targetFileName.concat("bw"));
        tmpFreezeFile = new File(targetFileName.concat("fr"));
        try
        {
            tmpResultFile.createNewFile();
            tmpProofFile.createNewFile();
            tmpFreezeFile.createNewFile();
        }
        catch (IOException e)
        {
            e.printStackTrace();
            System.err.println("IO error: failed to create temporary files");
            System.exit(0);
        }
    }
    
    public void deleteTempFiles()
    {
        tmpResultFile.delete();
        tmpProofFile.delete();
        tmpFreezeFile.delete();
    }
    
    public MiniSATFiles copyWithoutTmpFiles()
    {
        MiniSATFiles copy = new MiniSATFiles();
        copy.sourceFile = new File(sourceFile.getAbsolutePath());
        copy.targetFile = new File(targetFile.getAbsolutePath());
        copy.tmpFile = new File(tmpFile.getAbsolutePath());
        return copy;
    }
}
