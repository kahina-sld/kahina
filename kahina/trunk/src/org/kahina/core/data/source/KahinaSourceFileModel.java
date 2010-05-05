package org.kahina.core.data.source;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.lightweight.LightweightKahinaObject;
import org.kahina.core.data.text.KahinaText;
import org.kahina.core.data.text.KahinaTextWithMarking;

public class KahinaSourceFileModel extends KahinaText
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
