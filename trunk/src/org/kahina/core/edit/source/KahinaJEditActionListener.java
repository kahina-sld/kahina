package org.kahina.core.edit.source;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import org.kahina.core.control.KahinaCodeLineProperty;
import org.kahina.core.control.KahinaCodeLinePropertySensor;
import org.kahina.core.data.agent.KahinaControlAgent;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.control.NewControlAgentEvent;
import org.kahina.lp.control.ControlAgentType;

public class KahinaJEditActionListener implements ActionListener
{
    LogicProgrammingInstance instance;
    KahinaJEditPanel panel;
    
    public KahinaJEditActionListener(LogicProgrammingInstance instance, KahinaJEditPanel panel)
    {
        this.instance = instance;
        this.panel = panel;
    }
    
    public void actionPerformed(ActionEvent action)
    {
        String command = action.getActionCommand();
        KahinaControlAgent controlAgent = new KahinaControlAgent(instance.getControl());
        int line = panel.getTextArea().getLastPopupLine();
        File file = panel.getFile();
        controlAgent.setName(file.getName() + ":" + line);
        String absolutePath = file.getAbsolutePath();
        KahinaSourceCodeLocation codeLocation = new KahinaSourceCodeLocation(absolutePath, line);
        KahinaCodeLineProperty lineProperty = new KahinaCodeLineProperty(codeLocation);
        controlAgent.setSensor(new KahinaCodeLinePropertySensor(controlAgent, instance.getState(), lineProperty));
        if (command.equals("addLineBreakPoint"))
        {
            instance.dispatchEvent(new NewControlAgentEvent(controlAgent, ControlAgentType.BREAK_AGENT));
        }
        else if (command.equals("addLineCreepPoint"))
        {
            instance.dispatchEvent(new NewControlAgentEvent(controlAgent, ControlAgentType.CREEP_AGENT));
        }
        else if (command.equals("addLineCompletePoint"))
        {
            instance.dispatchEvent(new NewControlAgentEvent(controlAgent, ControlAgentType.COMPLETE_AGENT));
        }
        else if (command.equals("addLineSkipPoint"))
        {
            instance.dispatchEvent(new NewControlAgentEvent(controlAgent, ControlAgentType.SKIP_AGENT));
        }
        else if (command.equals("addLineFailPoint"))
        {
            instance.dispatchEvent(new NewControlAgentEvent(controlAgent, ControlAgentType.FAIL_AGENT));
        }
        else
        {
            System.err.println("WARNING: unknown JEdit action command " + command + ", ignoring it.");
            return;
        }
        //update event needed for the control point editor to be redrawn
        instance.dispatchGUIEvent(new KahinaRedrawEvent());
    }
    
}
