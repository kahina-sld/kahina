package org.kahina.core.data.source;

import org.kahina.core.data.text.KahinaTextModel;

public class KahinaSourceFileModel extends KahinaTextModel
{
    public String absolutePathName;
    
    public KahinaSourceFileModel()
    {
        super();
    }
    
    public KahinaSourceFileModel(String absolutePathName)
    {
        super(absolutePathName);
        this.absolutePathName = absolutePathName;
    }
}
