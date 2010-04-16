package org.kahina.core.data.source;

import java.util.HashMap;
import java.util.Map;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.lightweight.LightweightKahinaObject;

public class KahinaSourceFileRegistry extends KahinaObject implements LightweightKahinaObject
{
    //map from full source file names to source file models
    public Map<String, KahinaSourceFileModel> models;
    
    public KahinaSourceFileRegistry()
    {
        models = new HashMap<String, KahinaSourceFileModel>();
    }
    
    public KahinaSourceFileModel getFileModel(String fileName)
    {
        KahinaSourceFileModel model = models.get(fileName);
        if (model == null)
        {
            model = new KahinaSourceFileModel(fileName);
            models.put(fileName, model);
        }
        return model;
    }
    
    
}
