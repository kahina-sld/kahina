package org.kahina.core;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import javax.swing.JOptionPane;

import org.kahina.core.bridge.KahinaBridge;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.control.KahinaProjectEvent;
import org.kahina.core.control.KahinaSessionEvent;
import org.kahina.core.control.KahinaSystemEvent;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.data.project.KahinaProjectStatus;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.data.text.KahinaLineReference;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.core.gui.event.KahinaConsoleLineEvent;
import org.kahina.core.gui.event.KahinaPerspectiveEvent;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.io.magazine.ObjectMagazine;
import org.kahina.core.io.util.FileUtil;
import org.kahina.core.io.util.ResourceList;
import org.kahina.core.io.util.XMLUtil;
import org.kahina.core.profiler.KahinaWarner;
import org.kahina.core.util.ProgressMonitorWrapper;
import org.kahina.core.util.SwingUtil;
import org.kahina.core.visual.KahinaDefaultView;
import org.kahina.core.visual.source.KahinaJEditSourceCodeView;
import org.kahina.core.visual.tree.KahinaTreeView;
import org.kahina.tralesld.TraleSLDState;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

public abstract class KahinaInstance<S extends KahinaState, G extends KahinaGUI, B extends KahinaBridge, P extends KahinaProject> implements KahinaListener
{
	protected static final boolean VERBOSE = false;

	protected G gui;

	//TODO maybe group state, bridge, profiler etc. under a new Session type

	protected S state;

	protected B bridge;
    
    protected P project;
    
    // store recent perspectives and cache default perspectives
    public List<KahinaPerspective> recentPerspectives;
    public List<KahinaPerspective> defaultPerspectives;
    
    // store recent projects and cache default projects
    public List<P> recentProjects;
    public List<P> defaultProjects;

	protected ObjectMagazine<KahinaStep> steps;
    
    private KahinaProjectStatus projectStatus = KahinaProjectStatus.NO_OPEN_PROJECT;

	/**
	 * GUI and views listen to this controller. It never changes.
	 */
	protected final KahinaController instanceControl;
	
	/**
	 * Everything else (e.g. bridges) listens to this controller. It changes with every session.
	 */
	protected KahinaController sessionControl;

	private boolean guiStarted = false;

	public KahinaInstance()
	{
		instanceControl = new KahinaController();
        instanceControl.registerListener(KahinaEventTypes.PERSPECTIVE, this);
        instanceControl.registerListener(KahinaEventTypes.SYSTEM, this);
		try
		{
			fillViewRegistry();
			initializeNewSession(true); //avoid creating a dummy session so views have something (empty) to show
		} 
		catch (Exception e)
		{
			e.printStackTrace();
			System.exit(-1);
		}
	}
	
	protected abstract void preparePerspectiveLists();
	
	protected abstract void prepareProjectLists();

	/**
	 * This code used to live in the constructor, but that caused problems with
	 * subclass fields that need to be initialized before the GUI is created.
	 * Now the GUI is created lazily from startNewSession().
	 */
	private void startGUI()
	{
	    if (gui != null) gui.getWindowManager().disposeAllWindows();
		gui = createGUI();
		KahinaPerspective perspective = gui.generateInitialPerspective();
		gui.getWindowManager().setPerspective(perspective);
		registerRecentPerspective(perspective);
		gui.prepare();
		guiStarted = true;
        setProjectStatus(KahinaProjectStatus.NO_OPEN_PROJECT);
	}
	
    public void startNewSessionWithoutBridge()
    {
        try
        {       
            initializeNewSession(false);
            if (!guiStarted)
            {
                startGUI();
            }
            else
            {
                gui.displayMainViews();
            }
            gui.show();
            dispatchEvent(new KahinaSelectionEvent(-1));
        } 
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }
        if (VERBOSE)
        {
            System.err.println(this + ".startNewSessionWithoutBridge()");
        }
    }

	public B startNewSession()
	{
		try
		{	    
	        initializeNewSession(true);
			if (!guiStarted)
			{
				startGUI();
			}
            else
            {
                gui.displayMainViews();
            }
			gui.show();
			dispatchEvent(new KahinaSelectionEvent(-1));
		} 
		catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
		if (VERBOSE)
		{
			System.err.println(this + ".startNewSession()=" + bridge);
		}
		return bridge;
	}

	protected void initializeNewSession(boolean withBridge)
	{
        if (sessionControl == null)
        {
    		sessionControl = new KahinaController();
    		sessionControl.registerListener(KahinaEventTypes.UPDATE, this);
    		sessionControl.registerListener(KahinaEventTypes.SESSION, this);
            sessionControl.registerListener(KahinaEventTypes.PROJECT, this);
            sessionControl.registerListener("new agent", this);
        }
		if (state != null)
		{
			state.initialize();
		} 
		else
		{
			state = createState();
		}
		if (bridge != null) bridge.deregister();
		if (withBridge)
		{	    
		    bridge = createBridge();
		}
        prepareProjectLists();
        preparePerspectiveLists();
		createTreeBehavior();
		createWarner();
	}
	
	public void registerSessionListener(String type, KahinaListener listener)
	{
	    sessionControl.registerListener(type, listener);
	}
	
    public void registerInstanceListener(String type, KahinaListener listener)
    {
        instanceControl.registerListener(type, listener);
    }
    
    public void deregisterSessionListener(String type, KahinaListener listener)
    {
        sessionControl.removeListener(type, listener);
    }
    
    public void deregisterInstanceListener(String type, KahinaListener listener)
    {
        instanceControl.removeListener(type, listener);
    }

	protected abstract void createTreeBehavior();

	private KahinaWarner createWarner()
	{
		return new KahinaWarner(this);
	}

	protected abstract S createState();

	protected abstract G createGUI();

	protected abstract B createBridge();
    
    protected abstract P createNewProject();

	public S getState()
	{
		return state;
	}
    
    public void setProjectStatus(KahinaProjectStatus projectStatus)
    {
        //System.err.println("setProjectStatus(" + projectStatus + ")");
        this.projectStatus = projectStatus;
        gui.getMainWindow().processProjectStatus(projectStatus);
    }
    
    public KahinaProjectStatus getProjectStatus()
    {
        return projectStatus;
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
		KahinaViewRegistry.registerMapping(KahinaSourceCodeLocation.class, KahinaJEditSourceCodeView.class);
	}
	
    public void dispatchEvent(KahinaEvent e)
    {
    	if (VERBOSE)
    	{
    		System.err.println("KahinaInstance.dispatchEvent(" + e + ")");
    	}
        sessionControl.processEvent(e);
    	instanceControl.processEvent(e);
    }
    
    public void dispatchSessionEvent(KahinaEvent e)
    {
        if (VERBOSE)
        {
            System.err.println("KahinaInstance.dispatchSessionEvent(" + e + ")");
        }
        sessionControl.processEvent(e);
    }
    
    public void dispatchInstanceEvent(KahinaEvent e)
    {
    	if (VERBOSE)
    	{
    		System.err.println("KahinaInstance.dispatchInstanceEvent(" + e + ")");
    	}
    	instanceControl.processEvent(e);
    }

	//@Override
    public void processEvent(KahinaEvent e)
	{
		if (e instanceof KahinaUpdateEvent)
		{
			processUpdateEvent((KahinaUpdateEvent) e);
		} 
        else if (e instanceof KahinaSessionEvent)
		{
			processSessionEvent((KahinaSessionEvent) e);
		} 
        else if (e instanceof KahinaPerspectiveEvent)
        {
            processPerspectiveEvent((KahinaPerspectiveEvent) e);
        } 
        else if (e instanceof KahinaProjectEvent)
        {
            processProjectEvent((KahinaProjectEvent) e);
        } 
        else if (e instanceof KahinaSystemEvent)
		{
			processSystemEvent((KahinaSystemEvent) e);
		}
	}

	private void processSystemEvent(KahinaSystemEvent e)
	{
		if (e.getSystemEventType() == KahinaSystemEvent.QUIT)
		{
			if (steps != null)
			{
				steps.close();
				steps = null;
			}
			sessionControl = null;
		}
	}

	private void processSessionEvent(KahinaSessionEvent e)
	{
		int type = e.getSessionEventType();
		if (type == KahinaSessionEvent.SAVE_SESSION)
		{
			saveSessionAs(e.getFile());
		} else if (type == KahinaSessionEvent.LOAD_SESSION)
		{
			loadSession(e.getFile());
		}
	}
	
    private void processPerspectiveEvent(KahinaPerspectiveEvent e)
    {
        int type = e.getPerspectiveEventType();
        if (type == KahinaPerspectiveEvent.SAVE_PERSPECTIVE)
        {
            savePerspectiveAs(e.getFile());
        }
        else if (type == KahinaPerspectiveEvent.LOAD_PERSPECTIVE)
        {
            KahinaPerspective perspective;
            try
            {
                perspective = loadPerspective(new FileInputStream(e.getFile()));
                project.setPerspective(perspective);
                gui.setPerspective(perspective);
            }
            catch (FileNotFoundException e1)
            {
                System.err.println("ERROR: could not load perspective " + e.getFile());
                e1.printStackTrace();
            }
        }
        else if (type == KahinaPerspectiveEvent.LOAD_RECENT_PERSPECTIVE)
        {
            KahinaPerspective perspective =  recentPerspectives.get(e.getID()).copy();
            project.setPerspective(perspective);
            gui.setPerspective(perspective);
        }
        else if (type == KahinaPerspectiveEvent.LOAD_DEFAULT_PERSPECTIVE)
        {
            KahinaPerspective perspective =  defaultPerspectives.get(e.getID()).copy();
            project.setPerspective(perspective);
            gui.setPerspective(perspective);
        }
    }
    
    @SuppressWarnings("unchecked")
    protected void processProjectEvent(KahinaProjectEvent e)
    {
        switch (e.getProjectEventType())
        {
            case NEW_PROJECT:
            {
                if (project != null)
                {
                    dispatchEvent(new KahinaControlEvent("abort"));
                    project.deregister();
                }
                newProject(e.getFile(), e.getName());
                project.register();
                registerRecentProject(project);
                break;
            }
            case SAVE_PROJECT:
            {
                saveProjectAs(e.getFile());
                break;
            }
            case LOAD_PROJECT:
            {
                if (project != null)
                {
                    dispatchEvent(new KahinaControlEvent("abort"));
                    project.deregister();
                }
                try
                {
                    project = loadProject(new FileInputStream(e.getFile()));
                    project.register();
                    registerRecentProject(project);
                    gui.setPerspective(project.getPerspective());
                    gui.displayMainViews();
                    setProjectStatus(KahinaProjectStatus.PROGRAM_UNCOMPILED);
                    dispatchInstanceEvent(new KahinaRedrawEvent());
                }
                catch (FileNotFoundException e1)
                {
                    System.err.println("ERROR: Project file not found!");
                    e1.printStackTrace();
                }

                break;
            }
            case LOAD_RECENT_PROJECT:
            {
                if (project != null)
                {
                    dispatchEvent(new KahinaControlEvent("abort"));
                    project.deregister();
                }
                project = (P) recentProjects.get(e.getID()).copy();
                project.register();
                registerRecentProject(project);
                gui.setPerspective(project.getPerspective());
                gui.displayMainViews();
                setProjectStatus(KahinaProjectStatus.PROGRAM_UNCOMPILED);
                dispatchInstanceEvent(new KahinaRedrawEvent());
                break;
            }
            case LOAD_DEFAULT_PROJECT:
            {
                if (project != null)
                {
                    dispatchEvent(new KahinaControlEvent("abort"));
                    project.deregister();
                }
                project = (P) defaultProjects.get(e.getID()).copy();
                project.register();
                registerRecentProject(project);
                gui.setPerspective(project.getPerspective());
                gui.displayMainViews();
                setProjectStatus(KahinaProjectStatus.PROGRAM_UNCOMPILED);
                dispatchInstanceEvent(new KahinaRedrawEvent());
                break;
            }
        }
    }
    
    protected KahinaPerspective loadPerspective(InputStream stream)
    {
        try
        {
            KahinaPerspective result = KahinaPerspective.importXML(XMLUtil.parseXMLStream(stream, false).getDocumentElement());
            stream.close();
            return result;
        }
        catch (IOException e)
        {
            throw new KahinaException("Failed to load perspective.", e);
        }
    }

    // by default, the five most recent perspectives are kept in memory
    private void registerRecentPerspective(KahinaPerspective psp)
    {
        // move to the front, or add to the front
        recentPerspectives.remove(psp);
        recentPerspectives.add(0, psp);
        if (recentPerspectives.size() > 5)
        {
            recentPerspectives.remove(5);
        }
    }
    
    // by default, the five most recent projects are kept in memory
    private void registerRecentProject(P prj)
    {
        // move to the front, or add to the front
        recentProjects.remove(prj);
        recentProjects.add(0, prj);
        if (recentProjects.size() > 5)
        {
            recentProjects.remove(5);
        }
    }

    private void savePerspectiveAs(File file)
    {
        Node node = project.getPerspective().exportXML(XMLUtil.newEmptyDocument());
        XMLUtil.writeXML(node, file.getAbsolutePath());
    }

	private void loadSession(File file)
	{
		ZipFile zipFile = null;
		ProgressMonitorWrapper monitor = null;
		try
		{
			zipFile = new ZipFile(file);
			ZipEntry entry = zipFile.getEntry("state");
			if (VERBOSE)
			{
				System.err.println(entry);
			}
			ObjectInputStream in = new ObjectInputStream(zipFile.getInputStream(entry));
			state = castToStateType(in.readObject());
			if (VERBOSE)
			{
				System.err.println(((TraleSLDState) state).getStepTree().getLayerDecider());
			}
			in.close();
			File directory = FileUtil.createTemporaryDirectory();
			monitor = gui.createProgressMonitorWrapper("Loading session", null, 0, zipFile.size());
			FileUtil.unzipToDirectory(zipFile, directory, "steps/", monitor);
			state.loadSteps(directory);
			gui.displayMainViews();
			dispatchEvent(new KahinaSelectionEvent(state.getSelectedStepID()));
			dispatchEvent(new KahinaSystemEvent(KahinaSystemEvent.NODE_COUNT, state.getStepCount()));
		} catch (Exception e)
		{
			gui.showMessageDialog(SwingUtil.visualError("Session could not be loaded due to the following problem: ", e), "Error", JOptionPane.ERROR_MESSAGE);
			return;
		} finally
		{
			if (zipFile != null)
			{
				try
				{
					monitor.close();
					zipFile.close();
				} catch (IOException e)
				{
					gui.showMessageDialog(SwingUtil.visualError("Session could not be loaded due to the following problem: ", e), "Error", JOptionPane.ERROR_MESSAGE);
				}
			}
		}
	}

	@SuppressWarnings("unchecked")
	private S castToStateType(Object object)
	{
		return (S) object;
	}

	private void saveSessionAs(File zipFile)
	{
		// TODO deal with bridge, subclasses need to deal with profiler...
		File directory;
		try
		{
			directory = FileUtil.createTemporaryDirectory();
		} catch (IOException e)
		{
			gui.showMessageDialog(SwingUtil.visualError("Session could not be saved due to the following problem:", e), "Error", JOptionPane.ERROR_MESSAGE);
			return;
		}
		if (zipFile.exists())
		{
			int answer = gui.showConfirmDialog("File " + zipFile + " exists. Overwrite it?", "Confirm overwrite", JOptionPane.YES_NO_OPTION);
			if (answer != JOptionPane.YES_OPTION)
			{
				return;
			}
		}
		File stepFolder = new File(directory, "steps");
		if (!stepFolder.mkdir())
		{
			gui.showMessageDialog("Failed to create directory " + directory + ". Session not saved.", "Error", JOptionPane.ERROR_MESSAGE);
			return;
		}
		ObjectMagazine<KahinaStep> steps = state.getSteps();
		ProgressMonitorWrapper monitor = gui.createProgressMonitorWrapper("Saving session", null, 0, steps.persistSteps() * 2 + 2);
		ObjectOutputStream out = null;
		try
		{
			synchronized (steps)
			{
				steps.persist(stepFolder, monitor);
			}
			synchronized (state)
			{
				out = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(new File(directory, "state"))));
				out.writeObject(state);
				monitor.increment();
			}
			out.flush();
			FileUtil.zipDirectory(directory, zipFile, monitor);
			FileUtil.deleteRecursively(directory);
			monitor.close();
		} catch (Exception e)
		{
			monitor.close();
			gui.showMessageDialog(SwingUtil.visualError("Session could not be saved due to the following problem: ", e), "Error", JOptionPane.ERROR_MESSAGE);
		} finally
		{
			if (out != null)
			{
				try
				{
					out.close();
				} catch (IOException e)
				{
					gui.showMessageDialog(SwingUtil.visualError("Session could not be saved due to the following problem: ", e), "Error", JOptionPane.ERROR_MESSAGE);
				}
			}
		}
	}

	private void processUpdateEvent(KahinaUpdateEvent e)
	{
		Set<KahinaLineReference> refs = state.getLineReferencesForStep(e.getSelectedStep());
		if (VERBOSE)
		{
			System.err.println("Line references for step " + e.getSelectedStep() + ": " + refs);
		}
		if (refs != null)
		{
			dispatchEvent(new KahinaConsoleLineEvent(refs));
		}
	}

	/**
	 * Writing a main method for a Kahina-based debugging environment is simple:
	 * just create an instance of your KahinaInstance subclass and pass its
	 * start method the arguments.
	 * 
	 * @param args
	 */
	public void start(String[] args)
	{
		startNewSession();

		if (args.length > 0)
		{
			loadSession(new File(args[0]));
		}
	}

    public KahinaBridge getBridge()
    {
        return bridge;
    }

	public String getApplicationName()
	{
		return "Kahina";
	}
	
	public P getProject()
	{
	    return project;
	}
    
    public void newProject(File grammarFile, String name)
    {
        project = createNewProject();
        project.setName(name);
        project.setMainFile(grammarFile);
        project.setPerspective(gui.getPerspective());
        setProjectStatus(KahinaProjectStatus.PROGRAM_UNCOMPILED);
    }
    
    public void saveProjectAs(File projectFile)
    {
        Document dom = XMLUtil.newEmptyDocument();
        Element el = project.exportXML(dom);
        XMLUtil.writeXML(el, projectFile.getAbsolutePath());
    }
    
    public abstract P loadProject(InputStream stream);
    
    public G getGUI()
    {
        return gui;
    }
}
