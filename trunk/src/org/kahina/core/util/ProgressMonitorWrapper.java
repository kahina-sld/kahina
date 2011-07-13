package org.kahina.core.util;

import java.awt.Component;

import javax.swing.ProgressMonitor;

import org.kahina.core.gui.KahinaControlPanel;

public class ProgressMonitorWrapper
{
	
	private ProgressMonitor monitor;
	
	private int progress = 0;
	
	private int max;

	public ProgressMonitorWrapper(Component parentComponent, String message, String note, int min, int max)
	{
		monitor = new ProgressMonitor(parentComponent, message, note, min, max);
		this.max = max;
	}
	
	public void increment()
	{
		progress++;
		if (progress >= max)
		{
			max++;
			monitor.setMaximum(max);
		}
		monitor.setProgress(progress);
	}
	
	public void close()
	{
		monitor.close();
	}
	
	public boolean isCanceled()
	{
		return monitor.isCanceled();
	}
}