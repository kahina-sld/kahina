package org.kahina.core.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class Utilities
{
	public static boolean equal(Object one, Object another)
	{
		if (one == null)
		{
			return another == null;
		}
		return one.equals(another);
	}

	public static int hashCode(Object... objects)
	{
		int result = 31 + hashCode(objects[0]);

		for (int i = 1; i < objects.length; i++)
		{
			result *= 31 + hashCode(objects[i]);
		}
		return result;
	}

	public static int hashCode(Object object)
	{
		if (object == null)
		{
			return 0;
		}

		return object.hashCode();
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

	public static void copy(File source, File destination) throws IOException
	{
		InputStream in = null;
		OutputStream out = null;
		byte[] buffer = new byte[4096];
		int length;
		try
		{
			try
			{
				in = new BufferedInputStream(new FileInputStream(source));
				out = new BufferedOutputStream(new FileOutputStream(destination));
				while ((length = in.read(buffer)) != 0)
				{
					out.write(buffer, 0, length);
				}
			} catch (IOException e)
			{
				throw e;
			} finally
			{
				if (in != null)
				{
					in.close();
				}
			}
		} finally
		{
			if (out != null)
			{
				out.close();
			}
		} // Java is absurd.
	}
}
