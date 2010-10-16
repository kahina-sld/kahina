package org.kahina.core.editor;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.AbstractAction;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.textarea.StandaloneTextArea;
import org.gjt.sp.jedit.textarea.TextArea;
import org.kahina.core.util.FileUtilities;
import org.kahina.core.util.KahinaSwingUtilities;

public class KahinaJEditPanel extends JPanel
{

	private static final long serialVersionUID = 2807203309357135993L;

	// jEdit thankfully provides its text area and buffer classes as
	// independently usable components. Things get more complicated if
	// one wants to use menus and toolbars. It should be possible by
	// running a headless JEdit instance (jEdit.main() with appropriate
	// arguments) and using the static methods in the jEdit class to
	// create views. A view is, roughly, a window with a menu bar, a
	// toolbar, and a tabbed pane with one text area per opened file.
	// Unfortunately, jEdit currently does not offer a possibility to
	// embed a view in another Swing window. We would need something
	// that is like View, but is not a window.

	private JButton saveButton;

	private JEditBuffer buffer;

	private File file;

	// TODO retain last modified date when opening/saving file and warn user if
	// it's modified by another application

	/**
	 * @param file
	 *            The text file to edit in this editor panel.
	 */
	public KahinaJEditPanel(File file)
	{
		this.file = file;
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		add(createControlPanel());
		try
		{
			add(createTextArea());
		} catch (IOException e)
		{
			add(createErrorPanel(e));
		}
	}

	private Component createControlPanel()
	{
		JPanel result = new JPanel();
		result.setLayout(new BoxLayout(result, BoxLayout.X_AXIS));
		result.add(createSaveButton());
		// TODO buttons for buffer.undo(), buffer.redo()
		return result;
	}

	private Component createTextArea() throws IOException
	{
		TextArea result = new StandaloneTextArea(new KahinaJEditPropertyManager());
		result.setBuffer(createBuffer());
		return result;
	}

	private Component createErrorPanel(Exception e)
	{
		return new JLabel(e.getMessage());
	}

	private JEditBuffer createBuffer() throws IOException
	{
		JEditBuffer result = new JEditBuffer();
		result.insert(0, FileUtilities.read(file)); // TODO proper character
													// encoding support
		return result;
	}

	private Component createSaveButton()
	{
		saveButton = new JButton();
		saveButton.setText("Save");
		final Component gui = this;
		saveButton.setAction(new AbstractAction()
		{

			private static final long serialVersionUID = 233207225613043398L;

			@Override
			public void actionPerformed(ActionEvent e)
			{
				try
				{
					save();
				} catch (IOException e1)
				{
					JOptionPane.showMessageDialog(gui, KahinaSwingUtilities.visualError("The file could not be saved.", e1));
				}
			}

		});
		saveButton.setEnabled(false);
		return saveButton;
	}

	public void save() throws IOException
	{
		FileUtilities.write(buffer.getText(), file);
		saveButton.setEnabled(false);
		// TODO how do we notice buffer has changed?
	}

}
