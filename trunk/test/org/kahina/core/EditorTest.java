package org.kahina.core;

import java.io.File;

import javax.swing.JFrame;

import org.kahina.prolog.editor.PrologJEditPanel;

//import statements
//Check if window closes automatically. Otherwise add suitable code
public class EditorTest extends JFrame
{

	private static final long serialVersionUID = -3113000982062624482L;

	public static void main(String args[])
	{
		new EditorTest();
	}

	EditorTest()
	{
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		PrologJEditPanel editPanel = new PrologJEditPanel(new File("/home/ke/trale/test_gram/theory3.pl"));
		setContentPane(editPanel);
		this.setSize(800, 600);
		setVisible(true);
		editPanel.showLine(24);
	}
}