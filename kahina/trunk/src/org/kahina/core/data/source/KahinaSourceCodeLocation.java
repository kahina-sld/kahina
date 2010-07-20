package org.kahina.core.data.source;

import java.util.HashMap;

import org.kahina.core.data.text.KahinaLineReference;

public class KahinaSourceCodeLocation extends KahinaLineReference
{    
    static HashMap<String,KahinaSourceFileModel> codeFiles = new HashMap<String,KahinaSourceFileModel>();
    
    public KahinaSourceCodeLocation()
    {
        super();
    }
    
    public KahinaSourceCodeLocation(String absolutePath, int lineNumber, int stepID)
    {
        super(lineNumber, stepID);
        KahinaSourceFileModel file = codeFiles.get(absolutePath);
        if (file == null)
        {
            file = new KahinaSourceFileModel(absolutePath);
            codeFiles.put(absolutePath, file);
        }
        this.text = file;
    }
    
    public KahinaSourceFileModel getText()
    {
        return (KahinaSourceFileModel) text;
    }
}
