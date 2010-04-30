package org.kahina.core.visual.text;

import javax.swing.DefaultListModel;
import javax.swing.JComponent;
import javax.swing.ListModel;

import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.text.KahinaLineReference;
import org.kahina.core.data.text.KahinaText;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaMessageEvent;
import org.kahina.core.visual.KahinaView;

public class KahinaTextView extends KahinaView<KahinaText> implements KahinaListener
{
    DefaultListModel listModel;
    
    public KahinaTextView()
    {
        listModel = new DefaultListModel();
    }
    
    public void registerNewLineReference(KahinaLineReference ref)
    {
        listModel.addElement(ref);
    }
    
    public ListModel getListModel()
    {
        return listModel;
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
    }
    
    public void processEvent(KahinaMessageEvent e)
    {
        registerNewLineReference(e.getLine());
    }
}
