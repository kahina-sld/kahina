package org.kahina.core.visual.source;

import java.awt.Color;
import java.util.HashMap;

import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;

import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.data.source.KahinaSourceFileModel;
import org.kahina.core.data.text.KahinaTextWithMarking;
import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.core.visual.text.KahinaTextViewPanel;

public class KahinaSourceCodeViewPanel extends KahinaTextViewPanel
{
	private static final long serialVersionUID = 8239151024120580599L;
    
    public void updateDisplay()
    {
        //System.err.println("Updating source code view panel!");
        //System.err.println("Old list model length: " + list.getModel().getSize());
        if (list.getModel() != view.getListModel())
        {
            list.setModel(view.getListModel());
            list.setSelectionModel(view.getSelectionModel());
            //System.err.println("New list model length: " + list.getModel().getSize());
        }
        super.updateDisplay();
    }
}
