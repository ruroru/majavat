package jj.majavat.stream;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

public class ByteArrayListInputStream extends InputStream {
    private final List<byte[]> arrays;
    private int arrayIndex;
    private byte[] currentArray;
    private int position;

    public ByteArrayListInputStream(List<byte[]> arrays) {
        this.arrays = arrays;
        this.arrayIndex = 0;
        this.position = 0;
        advanceToNextArray();
    }

    private void advanceToNextArray() {
        while (arrayIndex < arrays.size()) {
            currentArray = arrays.get(arrayIndex++);
            if (currentArray.length > 0) {
                position = 0;
                return;
            }
        }
        currentArray = null;
    }

    @Override
    public int read() {
        while (currentArray != null) {
            if (position < currentArray.length) {
                return currentArray[position++] & 0xFF;
            }
            advanceToNextArray();
        }
        return -1;
    }

    @Override
    public int read(byte[] b, int off, int len) {
        if (b == null) {
            throw new NullPointerException();
        }
        if (off < 0 || len < 0 || len > b.length - off) {
            throw new IndexOutOfBoundsException();
        }
        if (len == 0) {
            return 0;
        }

        int totalRead = 0;

        while (currentArray != null && totalRead < len) {
            int available = currentArray.length - position;
            int toRead = Math.min(available, len - totalRead);

            System.arraycopy(currentArray, position, b, off + totalRead, toRead);
            position += toRead;
            totalRead += toRead;

            if (position >= currentArray.length) {
                advanceToNextArray();
            }
        }

        return totalRead > 0 ? totalRead : -1;
    }

    @Override
    public long transferTo(OutputStream out) throws IOException {
        long transferred = 0;

        if (currentArray != null && position < currentArray.length) {
            int remaining = currentArray.length - position;
            out.write(currentArray, position, remaining);
            transferred += remaining;
        }

        while (arrayIndex < arrays.size()) {
            byte[] array = arrays.get(arrayIndex++);
            if (array.length > 0) {
                out.write(array, 0, array.length);
                transferred += array.length;
            }
        }

        currentArray = null;
        return transferred;
    }

    @Override
    public int available() {
        return (currentArray == null) ? 0 : currentArray.length - position;
    }

    @Override
    public void close() {
        currentArray = null;
        position = 0;
        arrayIndex = arrays.size();
    }

    @Override
    public long skip(long n) {
        if (n <= 0) {
            return 0;
        }

        long totalSkipped = 0;

        while (currentArray != null && totalSkipped < n) {
            int available = currentArray.length - position;
            long remaining = n - totalSkipped;

            if (remaining >= available) {
                totalSkipped += available;
                advanceToNextArray();
            } else {
                position += (int) remaining;
                totalSkipped = n;
            }
        }

        return totalSkipped;
    }
}