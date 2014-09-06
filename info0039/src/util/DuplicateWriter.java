package util;

import java.io.*;

/** Writer which duplicates its inputs over two writers. */
public class DuplicateWriter extends Writer {

    private final Writer _out1;
    private final Writer _out2;

    public DuplicateWriter(Writer out1, Writer out2)
    {
        this._out1 = out1;
        this._out2 = out2;
    }

    public void close() throws IOException
    {
        this._out1.close();
        this._out2.close();
    }

    public void flush() throws IOException
    {
        this._out1.flush();
        this._out2.flush();
    }

    public void write(char[] cbuf, int off, int len) throws IOException
    {
        this._out1.write(cbuf, off, len);
        this._out2.write(cbuf, off, len);
    }
}
