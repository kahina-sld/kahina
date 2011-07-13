package org.kahina.core.data.source;

import java.util.HashMap;
import java.util.Map;

import org.kahina.core.data.KahinaObject;

public class KahinaSourceFileRegistry extends KahinaObject
{
    /**
	 * 
	 */
	private static final long serialVersionUID = 6178519108143817440L;
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
