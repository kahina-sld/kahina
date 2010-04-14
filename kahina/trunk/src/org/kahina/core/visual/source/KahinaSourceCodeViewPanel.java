package org.kahina.core.visual.source;

import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaSourceCodeViewPanel extends KahinaViewPanel<KahinaSourceCodeLocation>
{
    KahinaSourceCodeView v;
    JTextArea codePane;
    JScrollPane codeScrollPane;
    
    public KahinaSourceCodeViewPanel()
    {
        v = new KahinaSourceCodeView();
        codePane = new JTextArea();
        codePane.setEditable(false);
        codePane.setLineWrap(false);
        codeScrollPane = new JScrollPane(codePane);
        this.add(codeScrollPane);
        //files = new HashMap<String, SourceFileModel>();
        //this.addComponentListener(this);
    }
    
    public void updateDisplay()
    {
        
    }
}
