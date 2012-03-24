package org.kahina.core.visual.text;

import java.util.Set;

import javax.swing.DefaultListModel;
import javax.swing.DefaultListSelectionModel;
import javax.swing.JComponent;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.text.KahinaLineReference;
import org.kahina.core.gui.event.KahinaConsoleLineEvent;
import org.kahina.core.gui.event.KahinaMessageEvent;
import org.kahina.core.visual.KahinaView;

public class KahinaTextView<T extends KahinaLineReference> extends KahinaView<T> implements KahinaListener
{
    protected DefaultListModel listModel;
    protected ListSelectionModel selectionModel;
    
    public KahinaTextView(KahinaInstance<?, ?, ?> kahina)
    {
    	super(kahina);
        listModel = new DefaultListModel();
        selectionModel = new DefaultListSelectionModel();
    }
    
    @Override
    public void doDisplay()
    {
    	// FIXME create listModel and selectionModel from existing model, e.g.
    	// when a stored session is loaded.
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
	public JComponent makePanel()
    {
        KahinaTextViewPanel panel = new KahinaTextViewPanel(kahina);
        kahina.getGuiControl().registerListener("redraw", panel);
        panel.setView(this);
        return panel;
    }
    
    @Override
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
        Set<KahinaLineReference> consoleLines = e.getConsoleLines();
        selectionModel.clearSelection();
        // TODO It would be nicer to add the selection interval for the line
        // that was just added/clicked on (if any) last so it becomes the
        // anchor and the lead. But at the moment we do not have that
        // information here.
        for (KahinaLineReference consoleLine : consoleLines)
        {
            int index =  listModel.indexOf(consoleLine);
            selectionModel.addSelectionInterval(index, index);
        }
    }
}
