package org.kahina.core.io.magazine;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStreamWriter;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Properties;

import org.kahina.core.KahinaException;
import org.kahina.core.util.FileUtilities;
import org.kahina.core.util.ProgressMonitorWrapper;

/**
 * Stores objects in memory by default, but serializes them away to disk in
 * blocks when memory usage exceeds a specified threshold. The idea is from
 * {@link http://forums.sun.com/thread.jspa?messageID=10949277#10949277}.
 * @author ke
 *
 * @param <S>
 */
public class ObjectMagazine<S>
{
	private static final boolean VERBOSE = false;
	
	private static final int MIN_BLOCKS = 1;

	private final File folder;

	private final int blockSize;

	private final float lowerBound;

	private final float upperBound;

	private final Map<Integer, Object[]> loadedBlocksByBlockNumber = new HashMap<Integer, Object[]>();

	private final Deque<Integer> blockNumbersUnloadQueue = new LinkedList<Integer>();

	private final Runtime runtime = Runtime.getRuntime();

	private int fileCount;

	private ObjectMagazine(File folder, int blockSize, float lowerBound, float upperBound, int fileCount)
	{
		this.folder = folder;
		this.blockSize = blockSize;
		this.lowerBound = lowerBound;
		this.upperBound = upperBound;
		this.fileCount = fileCount;
	}

	private Object[] loadOrCreateBlock(int blockNumber)
	{
		blockNumbersUnloadQueue.addLast(blockNumber);
		Object[] block;
		File file = new File(folder, Integer.toString(blockNumber));
		if (file.exists())
		{
			try
			{
				ObjectInputStream in = new ObjectInputStream(new BufferedInputStream(new FileInputStream(file)));
				block = (Object[]) in.readObject();
				in.close();
			} catch (IOException e)
			{
				throw new KahinaException("Error reading block " + blockNumber + " in folder " + folder + ".");
			} catch (ClassNotFoundException e)
			{
				throw new KahinaException("Error reading block " + blockNumber + " in folder " + folder + ".");
			}
		} else
		{
			block = new Object[blockSize];
			fileCount++;
		}
		loadedBlocksByBlockNumber.put(blockNumber, block);
		if (VERBOSE)
		{
			System.err.println("Loaded or created block " + blockNumber + ", loaded blocks: " + blockNumbersUnloadQueue);
		}
		return block; // mere convenience
	}

	private void unloadBlock(int blockNumber)
	{
		Object[] block = loadedBlocksByBlockNumber.remove(blockNumber);
		ObjectOutputStream out = null;
		try
		{
			out = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(new File(folder, Integer.toString(blockNumber)))));
			out.writeObject(block);
		} catch (IOException e)
		{
			throw new KahinaException("Error writing block " + blockNumber + " in folder " + folder + ".", e);
		} finally
		{
			if (out != null)
			{
				try
				{
					out.close();
					if (VERBOSE)
					{
						System.err.println("Wrote block " + blockNumber + " (" + block + "), length " + block.length + ").");
					}
				} catch (IOException e)
				{
					throw new KahinaException("Error writing block " + blockNumber + " in folder " + folder + ".", e);
				}
			}
		}
	}

	private void reduceMemoryUsage()
	{
		if (memoryRatio() > upperBound)
		{
			long ns;
			if (VERBOSE)
			{
				System.err.println("Reducing memory usage. Loaded blocks: " + blockNumbersUnloadQueue);
				ns = System.nanoTime();
			}
			// TODO Comparing memory usage to the lower bound doesn't really
			// make sense, the VM will keep all the garbage lying around. Maybe
			// just reduce to a fixed number of blocks (like 10) and explicitly
			// garbage-collect then.
			while (blockNumbersUnloadQueue.size() > MIN_BLOCKS && memoryRatio() > lowerBound)
			{
				unloadBlock(blockNumbersUnloadQueue.removeFirst());
			}
			if (VERBOSE)
			{
				ns = System.nanoTime() - ns;
				System.err.println("Reduced memory usage, took " + ns + " ns. Loaded blocks: " + blockNumbersUnloadQueue);
			}
		}
	}

	private float memoryRatio()
	{
		if (VERBOSE)
		{
			System.err.println("Total memory: " + runtime.totalMemory());
			System.err.println("Max memory:   " + runtime.maxMemory());
			System.err.println("Free memory:  " + runtime.freeMemory());
			System.err.println("Ratio: " + ((float) (runtime.totalMemory() - runtime.freeMemory())) / runtime.maxMemory());
		}
		return ((float) (runtime.totalMemory() - runtime.freeMemory())) / runtime.maxMemory();
	}

	public S retrieve(int index)
	{
		return cast(getBlockForIndex(index)[index % blockSize]);
	}

	public void store(int index, S object)
	{
		getBlockForIndex(index)[index % blockSize] = object;
	}

	private Object[] getBlockForIndex(int index)
	{
		int blockNumber = index / blockSize;
		Object[] block = loadedBlocksByBlockNumber.get(blockNumber);
		if (block == null)
		{
			reduceMemoryUsage();
			block = loadOrCreateBlock(blockNumber);
		}
		return block;
	}

	@SuppressWarnings("unchecked")
	private S cast(Object object)
	{
		return (S) object;
	}

	public static <S> ObjectMagazine<S> load(File folder, Class<S> objectType)
	{
		Properties properties = readPropertiesFile(folder);
		return new ObjectMagazine<S>(folder, Integer.parseInt(properties.getProperty("blockSize")), Float.parseFloat(properties.getProperty("lowerBound")), Float.parseFloat(properties
				.getProperty("upperBound")), folder.list().length);
	}

	public static <S> ObjectMagazine<S> create()
	{
		return create(1000, 0.2F, 0.9F);
	}

	private static <S> ObjectMagazine<S> create(int blockSize, float lowerBound, float upperBound)
	{
		File directory;
		try
		{
			directory = FileUtilities.createTemporaryDirectory();
		} catch (IOException e)
		{
			throw new KahinaException("Failed to create magazine.", e);
		}
		writePropertiesFile(directory, blockSize, lowerBound, upperBound);
		return new ObjectMagazine<S>(directory, blockSize, lowerBound, upperBound, 1);
	}

	private static Properties readPropertiesFile(File folder)
	{
		Properties properties = new Properties();
		try
		{
			properties.load(new BufferedReader(new InputStreamReader(new FileInputStream(new File(folder, "magazine.properties")), "UTF-8")));
		} catch (IOException e)
		{
			throw new KahinaException("I/O error reading magazine properties file.", e);
		}
		return properties;
	}

	private static void writePropertiesFile(File folder, int blockSize, float lowerBound, float upperBound)
	{
		Properties properties = new Properties();
		properties.setProperty("blockSize", Integer.toString(blockSize));
		properties.setProperty("lowerBound", Float.toString(lowerBound));
		properties.setProperty("upperBound", Float.toString(upperBound));
		try
		{
			properties.store(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(new File(folder, "magazine.properties")), "UTF-8")), null);
		} catch (IOException e)
		{
			throw new KahinaException("I/O error creating magazine properties file.", e);
		}
	}

	public int persistSteps()
	{
		return blockNumbersUnloadQueue.size() + fileCount;
	}

	// TODO canceling via progress monitor
	public void persist(File destinationFolder, ProgressMonitorWrapper monitor)
	{
		for (int blockNumber : blockNumbersUnloadQueue)
		{
			unloadBlock(blockNumber);
			monitor.increment();
		}
		blockNumbersUnloadQueue.clear();
		for (File file : folder.listFiles())
		{
			try
			{
				FileUtilities.copy(file, new File(destinationFolder, file.getName()));
				if (monitor != null)
				{
					monitor.increment();
				}
			} catch (IOException e)
			{
				throw new KahinaException("I/O error while saving object magazine.", e);
			}
		}
	}

	public void close()
	{
		FileUtilities.deleteRecursively(folder);
	}
}
