package org.kahina.core.edit.source;

import java.awt.Component;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextArea;

import org.gjt.sp.jedit.buffer.BufferAdapter;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.textarea.StandaloneTextArea;
import org.kahina.core.io.util.FileUtil;
import org.kahina.core.util.ListUtil;
import org.kahina.core.util.MsgUtil;
import org.kahina.core.util.SwingUtil;

public class KahinaJEditPanel extends JPanel
{

	// TODO fix layout

	// TODO add shortcut for saving

	private static final long serialVersionUID = 2807203309357135993L;

	private static final boolean VERBOSE = false;

	public static final String PROPERTY_DIRTY = "dirty";

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

	private static final Dimension SPACE = new Dimension(8, 8);

	private JButton saveButton;

	private StandaloneTextArea textArea;

	private JEditBuffer buffer;

	private File file;

	private boolean dirty = false;

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
		add(Box.createRigidArea(SPACE));
		add(createControlPanel());
		add(Box.createRigidArea(SPACE));
		try
		{
			add(createTextArea());
		} catch (Exception e)
		{
			add(createErrorPanel(e));
		}
	}

	private Component createControlPanel()
	{
		JPanel controlPanel = new JPanel();
		controlPanel.setLayout(new BoxLayout(controlPanel, BoxLayout.X_AXIS));
		// controlPanel.add(Box.createRigidArea(SPACE));
		controlPanel.add(createSaveButton());
		controlPanel.add(Box.createRigidArea(SPACE));
		controlPanel.add(createHelpButton());
		controlPanel.add(Box.createHorizontalGlue());
		controlPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		return controlPanel;
	}

	private Component createTextArea() throws Exception
	{
		textArea = new StandaloneTextArea(new KahinaJEditPropertyManager());
		textArea.setSize(100, 100);
		if (VERBOSE)
		{
			System.err.println("Created text area: " + textArea);
		}
		buffer = textArea.getBuffer();
		configureBuffer(buffer);		
		buffer.insert(0, FileUtil.read(file)); // TODO encoding support
		buffer.addBufferListener(new BufferAdapter()
		{

			@Override
			public void contentInserted(JEditBuffer buffer2, int startLine, int offset, int numLines, int length)
			{
				setDirty(true);
			}

			@Override
			public void contentRemoved(JEditBuffer buffer2, int startLine, int offset, int numLines, int length)
			{
				setDirty(true);
			}

			@Override
			public void transactionComplete(JEditBuffer buffer2)
			{
				// This method is called e.g. after an undo operation. An undo
				// operation may have cleaned the buffer rather than making it
				// dirty. UndoManager doesn't seem to respect this, though.
				setDirty(buffer.isDirty());
			}

		});
		textArea.setAlignmentX(Component.LEFT_ALIGNMENT);
		return textArea;
	}
	
	/**
	 * Override this method to configure the buffer, e.g. for code highlighting.
	 * This default implementation does nothing.
	 * @param buffer
	 * @throws Exception
	 */
	protected void configureBuffer(JEditBuffer buffer) throws Exception
	{
	}

	private void setDirty(boolean newValue)
	{
		boolean oldValue = dirty;

		if (oldValue != newValue)
		{
			dirty = newValue;
			firePropertyChange(PROPERTY_DIRTY, oldValue, newValue);
			saveButton.setEnabled(newValue);
		}
	}

	private Component createErrorPanel(Exception e)
	{
		return new JTextArea(ListUtil.join("\n", MsgUtil.portrayStackTrace(e)));
	}

	private Component createSaveButton()
	{
		final Component gui = this;
		saveButton = new JButton((new AbstractAction("Save")
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
					JOptionPane.showMessageDialog(gui, SwingUtil.visualError("The file could not be saved.", e1));
				}
			}

		}));
		saveButton.setEnabled(false);
		return saveButton;
	}

	private Component createHelpButton()
	{
		final Component gui = this;
		return new JButton(new AbstractAction("Help")
		{

			private static final long serialVersionUID = -6814197816082607385L;

			@Override
			public void actionPerformed(ActionEvent e)
			{
				try
				{
					showHelp();
				} catch (IOException e1)
				{
					JOptionPane.showMessageDialog(gui, SwingUtil.visualError(
							"An error occured while trying to show the jEdit keyboard shortcuts in your browser. You can find them at http://www.jedit.org/users-guide/shortcuts.html#id2573783", e1),
							"Error", JOptionPane.ERROR_MESSAGE);
				}
			}

		});
	}

	protected void showHelp() throws IOException
	{
		Desktop.getDesktop().browse(java.net.URI.create("http://www.jedit.org/users-guide/shortcuts.html#id2573783"));
	}

	public void save() throws IOException
	{
		FileUtil.write(buffer.getText(), file);
		setDirty(false);
	}

	public void showLine(int lineNumber)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".showLine(" + lineNumber + ")");
		}
		try
		{
			textArea.setCaretPosition(buffer.getLineStartOffset(lineNumber));
			textArea.scrollToCaret(true);
			textArea.selectLine();
		} catch (Exception e)
		{
			// This happens randomly. Hopefully harmless.
		}
	}

}
