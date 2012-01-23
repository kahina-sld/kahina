package org.kahina.core.edit.source;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.gjt.sp.jedit.IPropertyManager;
import org.gjt.sp.jedit.textarea.TextArea;
import org.gjt.sp.util.IOUtilities;
import org.kahina.core.KahinaException;

public class KahinaJEditPropertyManager implements IPropertyManager
{
	
	private final Properties properties = new Properties();
	
	public KahinaJEditPropertyManager()
	{
		properties.putAll(loadProperties(TextArea.class, "/org/gjt/sp/jedit/jedit_keys.props"));
		properties.putAll(loadProperties(TextArea.class, "/org/gjt/sp/jedit/jedit.props"));
		properties.putAll(loadProperties(KahinaJEditPropertyManager.class, "/org/kahina/core/editor/kahina_jedit.properties"));
		// TODO load user-configurable properties
	}

	@Override
	public String getProperty(String key)
	{
		return properties.getProperty(key);
	}
	
	private static Properties loadProperties(Class<?> clazz, String resourceName)
	{
		Properties properties = new Properties();
		InputStream in = clazz.getResourceAsStream(resourceName);
		try
		{
			properties.load(in);
		} catch (IOException e)
		{
			throw new KahinaException("Error while configuring editor view.", e);
		} finally
		{
			IOUtilities.closeQuietly(in);
		}
		return properties;
	}

}
