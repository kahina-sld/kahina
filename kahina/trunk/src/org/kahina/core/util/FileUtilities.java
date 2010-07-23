package org.kahina.core.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.kahina.core.KahinaException;

public class FileUtilities
{

	public static void copy(File source, File destination) throws IOException
	{
		OutputStream out = null;
		try
		{
			out = new BufferedOutputStream(new FileOutputStream(destination));
			copy(source, out);
		} catch (IOException e)
		{
			throw e;
		} finally
		{
			if (out != null)
			{
				out.close();
			}
		} // Java is absurd.
	}

	private static void copy(File file, OutputStream out) throws IOException
	{
		InputStream in = new BufferedInputStream(new FileInputStream(file));
		int length;
		byte[] buffer = new byte[4096];
		try
		{
			while ((length = in.read(buffer)) > 0)
			{
				out.write(buffer, 0, length);
			}
		} catch (IOException e)
		{
			throw e;
		} finally
		{
			in.close();
		}
	}

	public static boolean deleteRecursively(File file)
	{
		if (file.isDirectory())
		{
			for (File child : file.listFiles())
			{
				if (!deleteRecursively(child))
				{
					return false;
				}
			}
		}
		return file.delete();
	}

	public static void zipDirectory(File directory, File zipFile, ProgressMonitorWrapper monitor) throws IOException
	{
		ZipOutputStream out = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(zipFile)));
		int rootPathLength = directory.getCanonicalPath().length() + 1;
		try
		{
			for (File file : directory.listFiles())
			{
				zipRecursively(file, out, rootPathLength);
				monitor.increment();
			}
		} catch (IOException e)
		{
			throw e;
		} finally
		{
			out.close();
		}
	}

	private static void zipRecursively(File toZip, ZipOutputStream out, int rootPathLength) throws IOException
	{
		if (toZip.isDirectory())
		{
			for (File file : toZip.listFiles())
			{
				zipRecursively(file, out, rootPathLength);
			}
		} else
		{
			out.putNextEntry(new ZipEntry(toZip.getCanonicalPath().substring(rootPathLength)));
			copy(toZip, out);
		}
	}

	public static File createTemporaryDirectory() throws IOException
	{
		File directory = File.createTempFile("kahina", null);
		if (!(directory.delete() && directory.mkdir()))
		{
			throw new KahinaException("Directory " + directory + " could not be created.");
		}
		return directory;
	}

}
