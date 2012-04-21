package org.geoscript.example.kmlrip

class Logger {
  val conn = {
    java.lang.Class.forName("org.h2.Driver")
    val cxn = 
      java.sql.DriverManager.getConnection("jdbc:h2:kmlrip", "kmlrip", "")

    val st = cxn.createStatement()

    st.execute("""
      CREATE TABLE Documents (
        url VARCHAR(1024),
        valid BOOLEAN,
        mimetype VARCHAR(64),
        length INTEGER
      );
    """)

    st.execute("""
      CREATE TABLE Links (
        source VARCHAR(1024),
        target VARCHAR(1024)
      );
    """)

    st.execute("""
      CREATE TABLE Features (
        source VARCHAR(1024),
        id VARCHAR(64)
      );
    """)

    st.close()

    cxn
  }

  val insertDocument = conn.prepareStatement("""
    INSERT INTO Documents (url, valid, mimetype, length) VALUES (?, ?, ?, ?);
  """)

  val insertFeature = conn.prepareStatement("""
    INSERT INTO Features (source, id) VALUES (?, ?);
  """)

  val insertLink = conn.prepareStatement("""
    INSERT INTO Links (source, target) VALUES (?, ?);
  """)

  def document(
    source: java.net.URL, 
    valid: Boolean, 
    mimetype: String, 
    length: Int) = {
    insertDocument.setString(1, source.toString)
    insertDocument.setBoolean(2, valid)
    insertDocument.setString(3, mimetype)
    insertDocument.setInt(4, length)
    insertDocument.execute()
  }

  def feature(source: java.net.URL, id: String) = {
    insertFeature.setString(1, source.toString)
    insertFeature.setString(2, id)
    insertFeature.execute()
  }

  def link(source: java.net.URL, target: java.net.URL) = {
    insertLink.setString(1, source.toString)
    insertLink.setString(2, target.toString)
    insertLink.execute()
  }

  def finish() = conn.close()
}
