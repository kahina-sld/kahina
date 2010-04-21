package org.kahina.core.visual.source;

import java.util.HashMap;

import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;

import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.data.source.KahinaSourceFileModel;
import org.kahina.core.data.text.KahinaTextWithMarking;
import org.kahina.core.visual.KahinaDefaultView;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaSourceCodeViewPanel extends KahinaViewPanel<KahinaSourceCodeView>
{
    HashMap<String, KahinaSourceFileModel> files;
    
    KahinaSourceCodeView v;
    JTextArea codePane;
    JScrollPane codeScrollPane;
    
    public KahinaSourceCodeViewPanel()
    {
        files = new HashMap<String, KahinaSourceFileModel>();
        
        v = new KahinaSourceCodeView();
        codePane = new JTextArea();
        codePane.setEditable(false);
        codePane.setLineWrap(false);
        codePane.setColumns(50);
        codePane.setRows(15);
        codeScrollPane = new JScrollPane(codePane);
        this.add(codeScrollPane);
        //files = new HashMap<String, SourceFileModel>();
        //this.addComponentListener(this);
    }
    
    public void setView(KahinaSourceCodeView view)
    {
        this.v = view;
        updateDisplay();
        repaint();
    }
    
    public void updateDisplay()
    {
        if (v.getModel() == null)
        {
            System.err.println("No source code location found!");
            codePane.setText("-- no source code location specified --");
        }
        else
        {
            KahinaSourceCodeLocation m = v.getModel();
            KahinaSourceFileModel sourceModel = files.get(m.absolutePath);
            if (sourceModel == null)
            {
                sourceModel = new KahinaSourceFileModel(m.absolutePath);
                files.put(m.absolutePath, sourceModel);
            }
            System.err.println("Show code location: " + m.absolutePath + ", line " + m.lineNumber);
            KahinaTextWithMarking textWithMarking = sourceModel.getCompleteContentWithLineOffsets(m.lineNumber);
            codePane.setText(textWithMarking.text);
            try
            {
                codePane.getHighlighter().addHighlight(textWithMarking.beginIndex, textWithMarking.endIndex, DefaultHighlighter.DefaultPainter);
                codePane.setCaretPosition(textWithMarking.caretIndex);
            }
            catch(BadLocationException ble) 
            {
                System.err.println("Bad location during highlighting!");
            }
        }
    }
}
