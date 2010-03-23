package org.kahina.data;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kahina.core.KahinaException;
import org.kahina.io.database.DatabaseHandler;

/**
 * A database data store suitable for {@link LightweightKahinaObject}s.
 * 
 * @author ke
 * 
 */
public class LightweightKahinaObjectDbDataStore extends DbDataStore
{
	private enum FieldType
	{
		OBJECT, OBJECT_LIST, STRING, STRING_LIST, INTEGER, INTEGER_LIST, INT, INT_LIST;
	}

	private static final String CLIENT_ID = LightweightKahinaObjectDbDataStore.class
			.getName();

	private static final String TABLE_NAME_PREFIX = LightweightKahinaObjectDbDataStore.class
			.getSimpleName()
			+ "_";

	private static final String OBJECT_TABLE_NAME = TABLE_NAME_PREFIX
			+ "object_values";

	private static final String STRING_TABLE_NAME = TABLE_NAME_PREFIX
			+ "string_values";

	private static final String INT_TABLE_NAME = TABLE_NAME_PREFIX
			+ "int_values";

	private PreparedStatement deleteObjectStatement;

	private PreparedStatement deleteStringStatement;

	private PreparedStatement deleteIntegerStatement;

	private PreparedStatement insertObjectStatement;

	private PreparedStatement insertStringStatement;

	private PreparedStatement insertIntegerStatement;

	private PreparedStatement selectObjectStatement;

	private PreparedStatement selectStringStatement;

	private PreparedStatement selectIntegerStatement;

	private List<Field> fields;

	private List<FieldType> fieldTypes;

	private int storeTypeID;

	private Constructor<? extends KahinaObject> constructor;

	private Set<Integer> currentlyBeingStored = new HashSet<Integer>();

	private Map<Integer, KahinaObject> currentlyBeingRetrieved = new HashMap<Integer, KahinaObject>();

	/**
	 * Creates a new {@link LightweightKahinaObjectDbDataStore}.
	 * 
	 * @param type
	 *            Must be a subclass of {@link LightweightKahinaObject}.
	 * @param manager
	 * @param db
	 */
	public LightweightKahinaObjectDbDataStore(
			Class<? extends KahinaObject> type, DbDataManager manager,
			DatabaseHandler db)
	{
		super(manager, db);
		if (!LightweightKahinaObject.class.isAssignableFrom(type))
		{
			throw new KahinaException(
					"Cannot deal with non-lightweight data type " + type + ".");
		}
		storeTypeID = manager.getTypeID(type);
		try
		{
			constructor = type.getConstructor(int.class);
		} catch (NoSuchMethodException e)
		{
			throw new KahinaException("Lightweight data type " + type
					+ "is missing (int) constructor.", e);
		}
		createTablesIfNecessary();
		prepareStatements();
		examineType(type);
	}

	private void createTablesIfNecessary()
	{
		if (db.isRegistered(CLIENT_ID))
		{
			return;
		}
		db.execute("CREATE TABLE " + OBJECT_TABLE_NAME + " ("
				+ "class_id INT, " + "object_id INT, " + "field_id INT, "
				+ "value_class_id INT, " + "value_object_id INT, "
				+ "INDEX id (class_id, object_id, field_id)" + "); "
				+ "CREATE TABLE KahinaObjectDbDataStore_string_values " + "("
				+ "class_id INT, " + "object_id INT, " + "field_id INT, "
				+ "value LONG VARCHAR, "
				+ "INDEX id (class_id, object_id, field_id)" + "); "
				+ "CREATE TABLE KahinaObjectDbDataStore_int_values " + "("
				+ "class_id INT, " + "object_id INT, " + "field_id INT, "
				+ "value INT, " + "INDEX id (class_id, object_id, field_id)"
				+ "); ");
		db.register(CLIENT_ID);
	}

	private void prepareStatements()
	{
		deleteObjectStatement = db.prepareStatement("DELETE FROM "
				+ OBJECT_TABLE_NAME + " WHERE class_id = " + storeTypeID
				+ " AND object_id = ?");
		insertObjectStatement = db
				.prepareStatement("INSERT INTO "
						+ OBJECT_TABLE_NAME
						+ " (class_id, object_id, field_id, value_class_id, value_object_id) VALUES ("
						+ storeTypeID + ", ?, ?, ?, ?");
		selectObjectStatement = db
				.prepareStatement("SELECT value_class_id, value_object_id FROM "
						+ OBJECT_TABLE_NAME
						+ " WHERE class_id = "
						+ storeTypeID + " AND object_id = ? AND field_id = ?");
		deleteStringStatement = db.prepareStatement("DELETE FROM "
				+ STRING_TABLE_NAME + " WHERE class_id = " + storeTypeID
				+ " AND object_id = ?");
		insertStringStatement = db.prepareStatement("INSERT INTO "
				+ STRING_TABLE_NAME
				+ " (class_id, object_id, field_id, value) VALUES ("
				+ storeTypeID + ", ?, ?, ?");
		selectStringStatement = db.prepareStatement("SELECT value FROM "
				+ STRING_TABLE_NAME + " WHERE class_id = " + storeTypeID
				+ " AND object_id = ? AND field_id = ?");
		deleteIntegerStatement = db.prepareStatement("DELETE FROM "
				+ INT_TABLE_NAME + " WHERE class_id = " + storeTypeID
				+ " AND object_id = ?");
		insertIntegerStatement = db.prepareStatement("INSERT INTO "
				+ INT_TABLE_NAME
				+ " (class_id, object_id, field_id, value) VALUES ("
				+ storeTypeID + ", ?, ?, ?");
		selectIntegerStatement = db.prepareStatement("SELECT value FROM "
				+ INT_TABLE_NAME + " WHERE class_id = " + storeTypeID
				+ " AND object_id = ? AND field_id = ?");
	}

	private void examineType(Class<? extends KahinaObject> type)
	{
		fields = new ArrayList<Field>();
		fieldTypes = new ArrayList<FieldType>();
		for (Field field : type.getFields())
		{
			if (Modifier.isPublic(field.getModifiers()))
			{
				Class<?> fieldType = field.getType();
				if (KahinaObject.class.isAssignableFrom(fieldType))
				{
					fieldTypes.add(FieldType.OBJECT);
				} else if (KahinaObject[].class.isAssignableFrom(fieldType))
				{
					fieldTypes.add(FieldType.OBJECT_LIST);
				} else if (String.class.isAssignableFrom(fieldType))
				{
					fieldTypes.add(FieldType.STRING);
				} else if (String[].class.isAssignableFrom(fieldType))
				{
					fieldTypes.add(FieldType.STRING_LIST);
				} else if (Integer.class.isAssignableFrom(fieldType))
				{
					fieldTypes.add(FieldType.INTEGER);
				} else if (Integer[].class.isAssignableFrom(fieldType))
				{
					fieldTypes.add(FieldType.INTEGER_LIST);
				} else if (int.class.isAssignableFrom(fieldType))
				{
					fieldTypes.add(FieldType.INT);
				} else if (int[].class.isAssignableFrom(fieldType))
				{
					fieldTypes.add(FieldType.INT_LIST);
				} else
				{
					continue;
				}
				fields.add(field);
			}
		}
	}

	@Override
	public KahinaObject retrieve(int id)
	{
		try
		{
			KahinaObject result = constructor.newInstance(id);
			currentlyBeingRetrieved.put(id, result);
			int numFields = fields.size();
			for (int i = 0; i < numFields; i++)
			{
				FieldType type = fieldTypes.get(i);
				if (type == FieldType.OBJECT)
				{
					ResultSet resultSet = selectObjectValue(id, i);
					resultSet.next();
					int typeID = resultSet.getInt(1);
					if (resultSet.wasNull())
					{
						fields.get(i).set(result, null);
					} else
					{
						fields.get(i).set(result,
								retrieve(typeID, resultSet.getInt(2)));
					}
				} else if (type == FieldType.OBJECT_LIST)
				{
					ResultSet resultSet = selectObjectValue(id, i);
					List<KahinaObject> values = new ArrayList<KahinaObject>();
					while (resultSet.next())
					{
						int typeID = resultSet.getInt(1);
						if (resultSet.wasNull())
						{
							values.add(null);
						} else
						{
							values.add(retrieve(typeID, resultSet.getInt(2)));
						}
					}
					fields.get(i).set(result,
							values.toArray(new KahinaObject[values.size()]));
				} else if (type == FieldType.STRING)
				{
					ResultSet resultSet = selectStringValue(id, i);
					resultSet.next();
					fields.get(i).set(result, resultSet.getString(1));
				} else if (type == FieldType.STRING_LIST)
				{
					ResultSet resultSet = selectStringValue(id, i);
					List<String> values = new ArrayList<String>();
					while (resultSet.next())
					{
						values.add(resultSet.getString(1));
					}
					fields.get(i).set(result,
							values.toArray(new String[values.size()]));
				} else if (type == FieldType.INTEGER)
				{
					ResultSet resultSet = selectIntegerValue(id, i);
					resultSet.next();
					fields.get(i).set(result, resultSet.getInt(1));
					if (resultSet.wasNull())
					{
						fields.get(i).set(result, null);
					}
				} else if (type == FieldType.INTEGER_LIST)
				{
					ResultSet resultSet = selectIntegerValue(id, i);
					List<Integer> values = new ArrayList<Integer>();
					while (resultSet.next())
					{
						int value = resultSet.getInt(1);
						if (resultSet.wasNull())
						{
							values.add(null);
						} else
						{
							values.add(value);
						}
					}
				} else if (type == FieldType.INT)
				{
					ResultSet resultSet = selectIntegerValue(id, i);
					resultSet.next();
					fields.get(i).set(result, resultSet.getInt(1));
				} else if (type == FieldType.INT_LIST)
				{
					ResultSet resultSet = selectIntegerValue(id, i);
					List<Integer> values = new ArrayList<Integer>();
					while (resultSet.next())
					{
						values.add(resultSet.getInt(1));
					}
					int[] array = new int[values.size()];
					for (int j = 0; j < array.length; j++)
					{
						array[j] = values.get(j);
					}
					fields.get(i).set(result, array);
				}
			}
			return result;
		} catch (IllegalArgumentException e)
		{
			throw new KahinaException("Could not retrieve object.", e);
		} catch (SecurityException e)
		{
			throw new KahinaException("Could not retrieve object.", e);
		} catch (InstantiationException e)
		{
			throw new KahinaException("Could not retrieve object.", e);
		} catch (IllegalAccessException e)
		{
			throw new KahinaException("Could not retrieve object.", e);
		} catch (InvocationTargetException e)
		{
			throw new KahinaException("Could not retrieve object.", e);
		} catch (SQLException e)
		{
			throw new KahinaException("Could not retrieve object.", e);
		} finally
		{
			currentlyBeingRetrieved.remove(id);
		}
	}

	@Override
	public void store(KahinaObject object)
	{
		int id = object.getID();
		currentlyBeingStored.add(id); // to protect from cycles
		try
		{
			// If the object is already there, delete it.
			deleteObjectStatement.setInt(1, id);
			deleteObjectStatement.execute();
			deleteStringStatement.setInt(1, id);
			deleteStringStatement.execute();
			deleteIntegerStatement.setInt(1, id);
			deleteIntegerStatement.execute();
			int numFields = fields.size();
			for (int i = 0; i < numFields; i++)
			{
				FieldType type = fieldTypes.get(i);
				if (type == FieldType.OBJECT)
				{
					insertObjectValue(id, i, (KahinaObject) fields.get(i).get(
							object));
				} else if (type == FieldType.OBJECT_LIST)
				{
					Object values = fields.get(i).get(object);
					if (values != null)
					{
						for (KahinaObject value : (KahinaObject[]) values)
						{
							insertObjectValue(id, i, value);
						}
					}
				} else if (type == FieldType.STRING)
				{
					insertStringValue(id, i, (String) fields.get(i).get(object));
				} else if (type == FieldType.STRING_LIST)
				{
					Object values = fields.get(i).get(object);
					if (values != null)
					{
						for (String value : (String[]) values)
						{
							insertStringValue(id, i, value);
						}
					}
				} else if (type == FieldType.INTEGER)
				{
					insertIntegerValue(id, i, (Integer) fields.get(i).get(
							object));
				} else if (type == FieldType.INTEGER_LIST)
				{
					Object values = fields.get(i).get(object);
					if (values != null)
					{
						for (Integer value : (Integer[]) values)
						{
							insertIntegerValue(id, i, value);
						}
					}
				} else if (type == FieldType.INT)
				{
					insertIntegerValue(id, i, fields.get(i).getInt(object));
				} else if (type == FieldType.INT_LIST)
				{
					Object values = fields.get(i).get(object);
					if (values != null)
					{
						for (int value : (int[]) values)
						{
							insertIntegerValue(id, i, value);
						}
					}
				}
			}
		} catch (SQLException e)
		{
			throw new KahinaException("Object could not be stored.", e);
		} catch (IllegalArgumentException e)
		{
			throw new KahinaException("Object could not be stored.", e);
		} catch (IllegalAccessException e)
		{
			throw new KahinaException("Object could not be stored.", e);
		} finally
		{
			currentlyBeingStored.remove(id);
		}
	}

	private ResultSet selectObjectValue(int objectID, int fieldID)
			throws SQLException
	{
		selectObjectStatement.setInt(1, objectID);
		selectObjectStatement.setInt(2, fieldID);
		return selectObjectStatement.executeQuery();
	}

	private ResultSet selectStringValue(int objectID, int fieldID)
			throws SQLException
	{
		selectStringStatement.setInt(1, objectID);
		selectStringStatement.setInt(2, fieldID);
		return selectStringStatement.executeQuery();
	}

	private ResultSet selectIntegerValue(int objectID, int fieldID)
			throws SQLException
	{
		selectIntegerStatement.setInt(1, objectID);
		selectIntegerStatement.setInt(2, fieldID);
		return selectIntegerStatement.executeQuery();
	}

	private void insertObjectValue(int objectID, int fieldID, KahinaObject value)
			throws SQLException
	{
		if (value != null && !currentlyBeingStored.contains(value))
		{
			manager.store(value);
		}
		insertObjectStatement.setInt(1, objectID);
		insertObjectStatement.setInt(2, fieldID);
		if (value == null)
		{
			insertObjectStatement.setNull(3, Types.INTEGER);
			insertObjectStatement.setNull(4, Types.INTEGER);
		} else
		{
			insertObjectStatement
					.setInt(3, manager.getTypeID(value.getClass()));
			insertObjectStatement.setInt(4, value.getID());
		}
		insertObjectStatement.execute();
	}

	private void insertStringValue(int objectID, int fieldID, String value)
			throws SQLException
	{
		insertStringStatement.setInt(1, objectID);
		insertStringStatement.setInt(2, fieldID);
		if (value == null)
		{
			insertStringStatement.setNull(3, Types.LONGVARCHAR);
		} else
		{
			insertStringStatement.setString(3, value);
		}
		insertStringStatement.execute();
	}

	private void insertIntegerValue(int objectID, int fieldID, Integer value)
			throws SQLException
	{
		insertIntegerStatement.setInt(1, objectID);
		insertIntegerStatement.setInt(2, fieldID);
		if (value == null)
		{
			insertIntegerStatement.setNull(3, Types.INTEGER);
		} else
		{
			insertIntegerStatement.setInt(3, value);
		}
		insertIntegerStatement.execute();
	}

	/*
	 * Alternative version without unboxing and null check.
	 */
	private void insertIntegerValue(int objectID, int fieldID, int value)
			throws SQLException
	{
		insertIntegerStatement.setInt(1, objectID);
		insertIntegerStatement.setInt(2, fieldID);
		insertIntegerStatement.setInt(3, value);
		insertIntegerStatement.execute();
	}

	private KahinaObject retrieve(int typeID, int objectID)
	{
		if (typeID == this.storeTypeID
				&& currentlyBeingRetrieved.containsKey(objectID))
		{
			return currentlyBeingRetrieved.get(objectID);
		}
		return manager.retrieve(typeID, objectID);
	}
}
