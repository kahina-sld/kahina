package org.kahina.qtype.gui;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.InputStream;
import java.util.Arrays;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.gui.KahinaDialogEvent;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.io.util.XMLUtil;
import org.kahina.core.visual.tree.KahinaListTreeView;
import org.kahina.qtype.QTypeDebuggerInstance;
import org.kahina.qtype.QTypeStep;
import org.kahina.qtype.control.QTypeControlEventCommands;
import org.kahina.qtype.data.tree.QTypeLayerDecider;
import org.kahina.sicstus.gui.SICStusPrologGUI;

public class QTypeGUI extends SICStusPrologGUI
{

	public QTypeGUI(Class<? extends QTypeStep> stepType, QTypeDebuggerInstance instance)
	{
		super(stepType, instance);
	}

	@Override
	public void prepare()
	{
		try
		{
			displayMainViews();
			//TODO: load last perspective instead of only default perspective from XML
			InputStream xmlStream = new BufferedInputStream(QTypeGUI.class.getResourceAsStream("kahinaqtype-integrated2.xml"));
			windowManager.createWindows(KahinaPerspective.importXML(XMLUtil.parseXMLStream(xmlStream, false).getDocumentElement()));	
		}
		catch (NullPointerException e)
		{
			e.printStackTrace();
		}
	}

	@Override
	protected KahinaListTreeView generateTreeView()
	{
		return new KahinaListTreeView(kahina, 0, 1);
	}

	@Override
	public void displayMainViews()
	{
		super.displayMainViews();
		mainTreeView.getModel().setLayerDecider(new QTypeLayerDecider());
		mainTreeView.getSecondaryModel().setLayerDecider(new QTypeLayerDecider());
	}

	@Override
	protected KahinaWindowManager createWindowManager()
	{
		return new QTypeWindowManager((QTypeDebuggerInstance) kahina);
	}
	
	@Override
	// TODO factor this code out into helper class, the code used in TraleSLDGUI is almost identical.
	protected void processDialogEvent(KahinaDialogEvent e)
	{
		super.processDialogEvent(e);
		int type = e.getDialogEventType();

		if (type == KahinaDialogEvent.COMPILE)
		{
			// TODO custom dialog with fancy stuff like recently compiled grammars
			// TODO add filter, just show .qtg files by default
			File directory = new File(".");
			JFileChooser chooser = new JFileChooser(directory);
			String absolutePath = (String) e.getArguments()[0];

			if (absolutePath != null)
			{
				chooser.setSelectedFile(new File(absolutePath));
			} else
			{
				String[] files = directory.list();
				Arrays.sort(files);

				for (String file : files)
				{
					if (file.endsWith(".qtg"))
					{
						chooser.setSelectedFile(new File(directory, file));
						break;
					}
				}
			}

			chooser.setDialogTitle("Compile grammar");

			if (chooser.showOpenDialog(windowManager.mainWindow) == JFileChooser.APPROVE_OPTION)
			{
				File grammar = chooser.getSelectedFile();

				if (grammar != null)
				{
					kahina.dispatchEvent(new KahinaControlEvent(QTypeControlEventCommands.COMPILE, new Object[] { grammar.getAbsolutePath() }));
				}
			}
		} else if (type == KahinaDialogEvent.PARSE)
		{
			// TODO offer recent parses in a ComboBox
			String sentence = (String) JOptionPane.showInputDialog(windowManager.mainWindow, "Enter a sentence to parse, with the words separated by spaces.", "Parse sentence",
					JOptionPane.QUESTION_MESSAGE, null, null, e.getArguments()[0]);

			if (sentence != null)
			{
				kahina.dispatchEvent(new KahinaControlEvent(QTypeControlEventCommands.PARSE, new Object[] { Arrays.asList(sentence.split(" +")) }));
			}
		}
	}


}
