package org.kahina.tralesld.gui;

import java.awt.event.ActionEvent;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.util.ListUtil;
import org.kahina.tralesld.control.TraleSLDControlEventCommands;

public class TraleSLDParseExampleMenu extends JMenu implements KahinaListener
{
    private static final long serialVersionUID = 5337584282998019751L;
    
    private final KahinaInstance<?, ?, ?, ?> kahina;

    public TraleSLDParseExampleMenu(KahinaInstance<?, ?, ?, ?> kahina)
    {
        super("Parse example");
        this.kahina = kahina;
        kahina.registerInstanceListener(KahinaEventTypes.CONTROL, this);
    }

    @Override
    public void processEvent(KahinaEvent event)
    {
        if (event instanceof KahinaControlEvent)
        {
            processControlEvent((KahinaControlEvent) event);
        }
    }

    protected void processControlEvent(KahinaControlEvent event)
    {
        if (event.getCommand().equals(TraleSLDControlEventCommands.UPDATE_EXAMPLES))
        {
            removeAll();
            List<?> examples = (List<?>) event.getArguments()[0];
            int size = examples.size();
            for (int i = 0; i < size; i++)
            {
                final List<String> example = ListUtil.castToStringList(examples.get(i));
                if (example != null)
                {
                    add(new JMenuItem(new AbstractAction(ListUtil.join(" ", example))
                    {
                        private static final long serialVersionUID = -637062118697209076L;

                        @Override
                        public void actionPerformed(ActionEvent e)
                        {
                            kahina.dispatchEvent(new KahinaControlEvent(TraleSLDControlEventCommands.PARSE, new Object[] { example }));
                        }

                    }));
                }
            }
        }
    }
}
