package org.kahina.core;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.Set;

import javax.swing.JOptionPane;

import org.kahina.core.bridge.KahinaBridge;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.DataManager;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.data.text.KahinaLineReference;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaStateEvent;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.core.gui.event.KahinaConsoleLineEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.util.ProgressMonitorWrapper;
import org.kahina.core.util.Utilities;
import org.kahina.core.visual.KahinaDefaultView;
import org.kahina.core.visual.source.KahinaSourceCodeView;
import org.kahina.core.visual.tree.KahinaTreeView;

public abstract class KahinaInstance<S extends KahinaState, G extends KahinaGUI, B extends KahinaBridge> implements KahinaListener
{
	protected S state;
	protected G gui;
	protected B bridge;

	public KahinaInstance()
	{
		fillViewRegistry();
		state = createState();
		gui = createGUI();
		bridge = createBridge();
		KahinaRunner.getControl().registerListener("update", this);
		KahinaRunner.getControl().registerListener("state", this);
	}

	public KahinaInstance(S state)
	{
		fillViewRegistry();
		this.state = state;
		gui = createGUI();
		// TODO (re)create bridge (when adding support for resuming live
		// sessions)
		KahinaRunner.getControl().registerListener("update", this);
		KahinaRunner.getControl().registerListener("state", this);
	}

	protected abstract S createState();

	protected abstract G createGUI();

	protected abstract B createBridge();

	public G getGUI()
	{
		return gui;
	}

	public B getBridge()
	{
		return bridge;
	}

	public S getState()
	{
		return state;
	}

	/**
	 * overwrite this to register views for user-defined datatypes MUST register
	 * views for all data types use super.fillViewRegistry() in implementations
	 * to register most basic views
	 */
	protected void fillViewRegistry()
	{
		KahinaViewRegistry.registerMapping(KahinaObject.class, KahinaDefaultView.class);
		KahinaViewRegistry.registerMapping(KahinaTree.class, KahinaTreeView.class);
		KahinaViewRegistry.registerMapping(KahinaSourceCodeLocation.class, KahinaSourceCodeView.class);
	}

	@Override
	public void processEvent(KahinaEvent e)
	{
		if (e instanceof KahinaUpdateEvent)
		{
			processUpdateEvent((KahinaUpdateEvent) e);
		} else if (e instanceof KahinaStateEvent)
		{
			processStateEvent((KahinaStateEvent) e);
		}
	}

	private void processStateEvent(KahinaStateEvent e)
	{
		int type = e.getStateEventType();
		if (type == KahinaStateEvent.SAVE_STATE)
		{
			saveStateAs(e.getFile());
		}
	}

	private void saveStateAs(File folder)
	{
		if (folder.exists())
		{
			int answer = gui.showConfirmDialog("File " + folder + " exists. Overwrite it?", "Confirm overwrite", JOptionPane.YES_NO_OPTION);
			if (answer != JOptionPane.YES_OPTION)
			{
				return;
			}
			if (!Utilities.deleteRecursively(folder))
			{
				gui.showMessageDialog("Failed to overwrite " + folder + ". State not saved.", "Error", JOptionPane.ERROR_MESSAGE);
				return;
			}
		}
		if (!folder.mkdir())
		{
			gui.showMessageDialog("Failed to create directory " + folder + ". State not saved.", "Error", JOptionPane.ERROR_MESSAGE);
			return;
		}
		File stepFolder = new File(folder, "steps");
		DataManager dm = KahinaRunner.getDataManager();
		ProgressMonitorWrapper monitor = gui.createProgressMonitorWrapper("Saving state", null, 0, dm.persistSteps() + 1);
		ObjectOutputStream out = null;
		try
		{
			synchronized (dm)
			{
				dm.persist(stepFolder, monitor);
			}
			synchronized (state)
			{
				out = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(new File(folder, "state"))));
				out.writeObject(state);

			}
			monitor.close();
		} catch (Exception e)
		{
			monitor.close();
			gui.showMessageDialog("State could not be saved due to the following problem: " + e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
		} finally
		{
			if (out != null)
			{
				try
				{
					out.close();
				} catch (IOException e)
				{
					gui.showMessageDialog("State could not be saved due to the following problem: " + e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
				}
			}
		}
	}

	private void processUpdateEvent(KahinaUpdateEvent e)
	{
		Set<KahinaLineReference> refs = state.getLineReferencesForStep(e.getSelectedStep());
		if (refs != null)
		{
			KahinaRunner.processEvent(new KahinaConsoleLineEvent(refs));
		}
	}
}
