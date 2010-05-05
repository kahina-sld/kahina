package org.kahina.core.visual.text;

import java.util.Set;

import javax.swing.DefaultListModel;
import javax.swing.DefaultListSelectionModel;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;

import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.text.KahinaLineReference;
import org.kahina.core.data.text.KahinaText;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaMessageEvent;
import org.kahina.core.gui.event.KahinaConsoleLineEvent;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.chart.KahinaChartViewPanel;

public class KahinaTextView extends KahinaView<KahinaText> implements KahinaListener
{
    DefaultListModel listModel;
    ListSelectionModel selectionModel;
    
    public KahinaTextView()
    {
        listModel = new DefaultListModel();
        selectionModel = new DefaultListSelectionModel();
    }
    
    public void registerNewLineReference(KahinaLineReference ref)
    {
        listModel.addElement(ref);
    }
    
    public ListModel getListModel()
    {
        return listModel;
    }
    
    public ListSelectionModel getSelectionModel()
    {
        return selectionModel;
    }
    
    @Override
    public JComponent wrapInPanel()
    {
        KahinaTextViewPanel panel = new KahinaTextViewPanel();
        KahinaRunner.getControl().registerListener("redraw", panel);
        panel.setView(this);
        return panel;
    }
    
    public void processEvent(KahinaEvent e)
    {
        if (e instanceof KahinaMessageEvent)
        {
            processEvent((KahinaMessageEvent) e);
        }
        else if (e instanceof KahinaConsoleLineEvent)
        {
            processEvent((KahinaConsoleLineEvent) e);
        }
    }
    
    public void processEvent(KahinaMessageEvent e)
    {
        registerNewLineReference(e.getLine());
    }
    
    public void processEvent(KahinaConsoleLineEvent e)
    {
        Set<Integer> consoleLines = e.getConsoleLines();
        int leadIndex = selectionModel.getLeadSelectionIndex();
        if (!consoleLines.contains(leadIndex))
        {
            leadIndex = e.getConsoleLines().iterator().next();
        }
        selectionModel.clearSelection();
        for (int consoleLine : e.getConsoleLines())
        {
            selectionModel.addSelectionInterval(consoleLine, consoleLine);
        }
        selectionModel.setAnchorSelectionIndex(leadIndex);
        selectionModel.setLeadSelectionIndex(leadIndex);
    }
}
