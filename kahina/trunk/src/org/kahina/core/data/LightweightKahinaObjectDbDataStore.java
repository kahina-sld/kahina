package org.kahina.core.data;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.kahina.core.KahinaException;
import org.kahina.io.database.DatabaseHandler;

/**
 * A database data store suitable for {@link LightweightKahinaObject}s.
 * @author ke
 *
 */
public class LightweightKahinaObjectDbDataStore extends DbDataStore
{
	// TODO allow null values
	
	private enum FieldType
	{
		OBJECT, OBJECT_LIST, STRING, STRING_LIST, INT, INT_LIST;
	}

	private static final String TABLE_NAME_PREFIX = "KahinaObjectDbDataStore_";

	private static final String OBJECT_TABLE_NAME = TABLE_NAME_PREFIX + "object_values";

	private static final String STRING_TABLE_NAME = TABLE_NAME_PREFIX + "string_values";

	private static final String INT_TABLE_NAME = TABLE_NAME_PREFIX + "int_values";

	private PreparedStatement deleteObjectStatement;

	private PreparedStatement deleteStringStatement;

	private PreparedStatement deleteIntStatement;

	private PreparedStatement insertObjectStatement;

	private PreparedStatement insertStringStatement;

	private PreparedStatement insertIntStatement;

	private PreparedStatement selectObjectStatement;

	private PreparedStatement selectStringStatement;

	private PreparedStatement selectIntStatement;

	private List<Field> fields;

	private List<FieldType> fieldTypes;

	private Class<? extends KahinaObject> type;

	/**
	 * Creates a new {@link LightweightKahinaObjectDbDataStore}.
	 * @param type Must be a subclass of {@link LightweightKahinaObject}.
	 * @param manager
	 * @param db
	 */
	public LightweightKahinaObjectDbDataStore(Class<? extends KahinaObject> type, DbDataManager manager, DatabaseHandler db)
	{
		super(manager, db);
		if (!LightweightKahinaObject.class.isAssignableFrom(type))
		{
			throw new KahinaException("Cannot deal with non-lightweight data type " + type + ".");
		}
		this.type = type;
		createTablesIfNecessary();
		prepareStatements();
		examineClass();
	}

	private void createTablesIfNecessary()
	{
		String clientID = LightweightKahinaObjectDbDataStore.class.getName();
		if (db.isRegistered(clientID))
		{
			return;
		}
		db.execute("CREATE TABLE " + OBJECT_TABLE_NAME + " (" + "class_id INT, " + "object_id INT, " + "field_id INT, " + "value_class_id INT, " + "value_object_id INT, "
				+ "INDEX id (class_id, object_id, field_id)" + "); " + "CREATE TABLE KahinaObjectDbDataStore_string_values " + "(" + "class_id INT, " + "object_id INT, " + "field_id INT, "
				+ "value BLOB, " + "INDEX id (class_id, object_id, field_id)" + "); " + "CREATE TABLE KahinaObjectDbDataStore_int_values " + "(" + "class_id INT, " + "object_id INT, "
				+ "field_id INT, " + "value INT, " + "INDEX id (class_id, object_id, field_id)" + "); ");
		db.register(clientID);
	}

	private void prepareStatements()
	{
		int classID = manager.getTypeID(type);
		deleteObjectStatement = db.prepareStatement("DELETE FROM " + OBJECT_TABLE_NAME + " WHERE class_id = " + classID + " AND object_id = ?");
		insertObjectStatement = db.prepareStatement("INSERT INTO " + OBJECT_TABLE_NAME + " (class_id, object_id, field_id, value_class_id, value_object_id) VALUES (" + classID + ", ?, ?, ?, ?");
		selectObjectStatement = db.prepareStatement("SELECT value_class_id, value_object_id FROM " + OBJECT_TABLE_NAME + " WHERE class_id = " + classID + " AND object_id = ? AND field_id = ?");
		deleteStringStatement = db.prepareStatement("DELETE FROM " + STRING_TABLE_NAME + " WHERE class_id = " + classID + " AND object_id = ?");
		insertStringStatement = db.prepareStatement("INSERT INTO " + STRING_TABLE_NAME + " (class_id, object_id, field_id, value) VALUES (" + classID + ", ?, ?, ?");
		selectStringStatement = db.prepareStatement("SELECT value FROM " + STRING_TABLE_NAME + " WHERE class_id = " + classID + " AND object_id = ? AND field_id = ?");
		deleteIntStatement = db.prepareStatement("DELETE FROM " + INT_TABLE_NAME + " WHERE class_id = " + classID + " AND object_id = ?");
		insertIntStatement = db.prepareStatement("INSERT INTO " + INT_TABLE_NAME + " (class_id, object_id, field_id, value) VALUES (" + classID + ", ?, ?, ?");
		selectIntStatement = db.prepareStatement("SELECT value FROM " + INT_TABLE_NAME + " WHERE class_id = " + classID + " AND object_id = ? AND field_id = ?");
	}

	private void examineClass()
	{
		fields = new ArrayList<Field>();
		fieldTypes = new ArrayList<FieldType>();
		for (Field field : type.getFields())
		{
			if (Modifier.isPublic(field.getModifiers()))
			{
				Class<?> type = field.getType();
				if (KahinaObject.class.isAssignableFrom(type))
				{
					fieldTypes.add(FieldType.OBJECT);
				} else if (KahinaObject[].class.isAssignableFrom(type))
				{
					fieldTypes.add(FieldType.OBJECT_LIST);
				} else if (String.class.isAssignableFrom(type))
				{
					fieldTypes.add(FieldType.STRING);
				} else if (String[].class.isAssignableFrom(type))
				{
					fieldTypes.add(FieldType.STRING_LIST);
				} else if (int.class.isAssignableFrom(type))
				{
					fieldTypes.add(FieldType.INT);
				} else if (int[].class.isAssignableFrom(type))
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
			KahinaObject result = type.getConstructor(int.class).newInstance(id);
			int numFields = fields.size();
			for (int i = 0; i < numFields; i++)
			{
				FieldType type = fieldTypes.get(i);
				if (type == FieldType.OBJECT)
				{
					ResultSet resultSet = selectObjectValue(id, i);
					resultSet.next();
					fields.get(i).set(result, manager.retrieve(resultSet.getInt(1), resultSet.getInt(2)));
				} else if (type == FieldType.OBJECT_LIST)
				{
					ResultSet resultSet = selectObjectValue(id, i);
					List<KahinaObject> values = new ArrayList<KahinaObject>();
					while (resultSet.next())
					{
						values.add(manager.retrieve(resultSet.getInt(1), resultSet.getInt(2)));
					}
					fields.get(i).set(result, values.toArray(new KahinaObject[values.size()]));
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
					fields.get(i).set(result, values.toArray(new String[values.size()]));
				} else if (type == FieldType.INT)
				{
					ResultSet resultSet = selectIntValue(id, i);
					resultSet.next();
					fields.get(i).set(result, resultSet.getInt(1));
				} else if (type == FieldType.INT_LIST)
				{
					ResultSet resultSet = selectIntValue(id, i);
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
		} catch (NoSuchMethodException e)
		{
			throw new KahinaException("Could not retrieve object.", e);
		} catch (SQLException e)
		{
			throw new KahinaException("Could not retrieve object.", e);
		}
	}

	@Override
	public void store(KahinaObject object)
	{
		int id = object.getID();
		try
		{
			deleteObjectStatement.setInt(1, id);
			deleteObjectStatement.execute();
			deleteStringStatement.setInt(1, id);
			deleteStringStatement.execute();
			deleteIntStatement.setInt(1, id);
			deleteIntStatement.execute();
			int numFields = fields.size();
			for (int i = 0; i < numFields; i++)
			{
				FieldType type = fieldTypes.get(i);
				if (type == FieldType.OBJECT)
				{
					insertObjectValue(id, i, (KahinaObject) fields.get(i).get(object));
				} else if (type == FieldType.OBJECT_LIST)
				{
					for (KahinaObject value : (KahinaObject[]) fields.get(i).get(object))
					{
						insertObjectValue(id, i, value);
					}
				} else if (type == FieldType.STRING)
				{
					insertStringValue(id, i, (String) fields.get(i).get(object));
				} else if (type == FieldType.STRING_LIST)
				{
					for (String value : (String[]) fields.get(i).get(object))
					{
						insertStringValue(id, i, value);
					}
				} else if (type == FieldType.INT)
				{
					insertIntValue(id, i, fields.get(i).getInt(object));
				} else if (type == FieldType.INT_LIST)
				{
					for (int value : (int[]) fields.get(i).get(object))
					{
						insertIntValue(id, i, value);
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
		}
	}

	private ResultSet selectObjectValue(int objectID, int fieldID) throws SQLException
	{
		selectObjectStatement.setInt(1, objectID);
		selectObjectStatement.setInt(2, fieldID);
		return selectObjectStatement.executeQuery();
	}

	private ResultSet selectStringValue(int objectID, int fieldID) throws SQLException
	{
		selectStringStatement.setInt(1, objectID);
		selectStringStatement.setInt(2, fieldID);
		return selectStringStatement.executeQuery();
	}

	private ResultSet selectIntValue(int objectID, int fieldID) throws SQLException
	{
		selectIntStatement.setInt(1, objectID);
		selectIntStatement.setInt(2, fieldID);
		return selectIntStatement.executeQuery();
	}

	private void insertObjectValue(int objectID, int fieldID, KahinaObject value) throws SQLException
	{
		manager.store(value);
		insertObjectStatement.setInt(1, objectID);
		insertObjectStatement.setInt(2, fieldID);
		insertObjectStatement.setInt(3, manager.getTypeID(value.getClass()));
		insertObjectStatement.setInt(4, value.getID());
		insertObjectStatement.execute();
	}

	private void insertStringValue(int objectID, int fieldID, String value) throws SQLException
	{
		insertStringStatement.setInt(1, objectID);
		insertStringStatement.setInt(2, fieldID);
		insertStringStatement.setString(3, value);
		insertStringStatement.execute();
	}

	private void insertIntValue(int objectID, int fieldID, int value) throws SQLException
	{
		insertIntStatement.setInt(1, objectID);
		insertIntStatement.setInt(2, fieldID);
		insertIntStatement.setInt(3, value);
		insertIntStatement.execute();
	}
}
