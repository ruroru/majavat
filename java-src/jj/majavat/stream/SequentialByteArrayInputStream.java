package jj.majavat.stream;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;

final public class SequentialByteArrayInputStream extends InputStream {
    private final ArrayList<byte[]> arrays;
    private int arrayIndex;
    private int position;

    public SequentialByteArrayInputStream(ArrayList<byte[]> arrays) {
        this.arrays = arrays;
        this.arrayIndex = 0;
        this.position = 0;
    }

    private byte[] getCurrentArray() {
        while (arrayIndex < arrays.size()) {
            byte[] arr = arrays.get(arrayIndex);
            if (position < arr.length) {
                return arr;
            }
            arrayIndex++;
            position = 0;
        }
        return null;
    }

    @Override
    public int read() {
        byte[] current = getCurrentArray();
        if (current != null) {
            return current[position++] & 0xFF;
        }
        return -1;
    }

    @Override
    public int read(byte[] b, int off, int len) {
        if (b == null) throw new NullPointerException();
        if (off < 0 || len < 0 || len > b.length - off) throw new IndexOutOfBoundsException();
        if (len == 0) return 0;

        int totalRead = 0;
        byte[] current;

        while (totalRead < len && (current = getCurrentArray()) != null) {
            int available = current.length - position;
            int toRead = Math.min(available, len - totalRead);

            System.arraycopy(current, position, b, off + totalRead, toRead);
            position += toRead;
            totalRead += toRead;
        }

        return totalRead > 0 ? totalRead : -1;
    }

    @Override
    public long transferTo(OutputStream out) throws IOException {
        long transferred = 0;
        byte[] current;

        while ((current = getCurrentArray()) != null) {
            int remaining = current.length - position;
            out.write(current, position, remaining);
            transferred += remaining;

            position += remaining;
        }

        return transferred;
    }

    @Override
    public int available() {
        byte[] current = getCurrentArray();
        if (current == null) return 0;

        long total = current.length - position;
        for (int i = arrayIndex + 1; i < arrays.size(); i++) {
            total += arrays.get(i).length;
        }

        return (total > Integer.MAX_VALUE) ? Integer.MAX_VALUE : (int) total;
    }

    @Override
    public long skip(long n) {
        if (n <= 0) return 0;

        long totalSkipped = 0;
        byte[] current;

        while (totalSkipped < n && (current = getCurrentArray()) != null) {
            int available = current.length - position;
            long remaining = n - totalSkipped;

            if (remaining >= available) {
                totalSkipped += available;
                position += available;
            } else {
                position += (int) remaining;
                totalSkipped = n;
            }
        }

        return totalSkipped;
    }

    @Override
    public void close() throws IOException {
        arrayIndex = arrays.size();
        super.close();
        position = 0;
    }
}