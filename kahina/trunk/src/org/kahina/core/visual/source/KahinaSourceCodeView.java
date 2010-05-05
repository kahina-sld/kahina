package org.kahina.core.visual.source;

import java.util.HashMap;

import javax.swing.DefaultListModel;
import javax.swing.DefaultListSelectionModel;
import javax.swing.JComponent;
import javax.swing.ListSelectionModel;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.data.source.KahinaSourceFileModel;
import org.kahina.core.data.text.KahinaText;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.text.KahinaTextView;

public class KahinaSourceCodeView extends KahinaTextView<KahinaSourceCodeLocation>
{     
    HashMap<KahinaText, DefaultListModel> sourceCodeModels;
    
    public KahinaSourceCodeView()
    {
        super();
        sourceCodeModels = new HashMap<KahinaText, DefaultListModel>();
    }
    
    public void doDisplay()
    {
        if (model != null)
        {
            System.err.println("Displaying source code model with text " + model.getText());
            DefaultListModel newModel = sourceCodeModels.get(model.getText());
            if (newModel == null)
            {
                newModel = new DefaultListModel();
                int lineNum = model.getText().getLines().size();
                for (int i = 0; i < lineNum; i++)
                {          
                    newModel.addElement(new KahinaSourceCodeLocation(model.getText().absolutePathName,i,-1));
                }
                sourceCodeModels.put(model.getText(), newModel);
            }
            newModel.set(model.getLine(), model);
            this.listModel = newModel;
            selectionModel.clearSelection();
            selectionModel.setSelectionInterval(model.getLine(), model.getLine());
            selectionModel.setLeadSelectionIndex(model.getLine());
        }
        else
        {
            DefaultListModel defaultModel = new DefaultListModel();
            defaultModel.addElement("no source code location specified!");
            this.listModel = defaultModel;
        }
    }
    
    public JComponent wrapInPanel()
    {
        KahinaSourceCodeViewPanel panel = new KahinaSourceCodeViewPanel();
        KahinaRunner.getControl().registerListener("redraw", panel);
        panel.setView(this);
        return panel;
    }
}
