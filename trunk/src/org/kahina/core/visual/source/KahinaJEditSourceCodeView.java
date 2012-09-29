package org.kahina.core.visual.source;

import javax.swing.JComponent;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.visual.KahinaView;
import org.kahina.lp.LogicProgrammingInstance;

public class KahinaJEditSourceCodeView extends KahinaView<KahinaSourceCodeLocation>
{
    protected LogicProgrammingInstance kahina;

	public KahinaJEditSourceCodeView(LogicProgrammingInstance kahina)
	{
		super(kahina);
        this.kahina = kahina;
	}

	@Override
	public JComponent makePanel()
	{
		KahinaJEditSourceCodeViewPanel panel = createPanel();
		kahina.registerInstanceListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

	protected KahinaJEditSourceCodeViewPanel createPanel()
	{
		return new KahinaJEditSourceCodeViewPanel(kahina);
	}

}
