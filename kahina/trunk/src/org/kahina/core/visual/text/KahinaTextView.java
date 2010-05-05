package org.kahina.core.visual.text;

import javax.swing.DefaultListModel;
import javax.swing.DefaultListSelectionModel;
import javax.swing.JComponent;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;

import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.text.KahinaLineReference;
import org.kahina.core.data.text.KahinaText;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaMessageEvent;
import org.kahina.core.gui.event.KahinaConsoleLineEvent;
import org.kahina.core.visual.KahinaView;

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
        selectionModel.setSelectionInterval(e.getConsoleLine(), e.getConsoleLine());
    }
}
