import java.io.*;
import java.security.*;
import java.util.stream.*;

public final class app {
    public static void main(String[] args) throws Exception {
        int N = 100;
        if (args.length >= 1) {
            N = Integer.parseInt(args[0]);
        }

        N = (N + 7) / 8 * 8;
        int chunkSize = N / 8;
        double inv = 2.0 / N;

        var xloc = new double[chunkSize][8];
        for (int i = 0; i < N; i++) {
            xloc[i / 8][i % 8] = inv * i - 1.5;
        }

        byte[] data = new byte[N * chunkSize];
        IntStream.range(0, N)
                .forEach(
                        y -> {
                            var ci = y * inv - 1.0;
                            IntStream.range(0, chunkSize).forEach(x -> {
                                data[y * chunkSize + x] = mbrot8(xloc[x], ci);
                            });
                        });
        System.out.println("P4\n" + N + " " + N);
        // System.out.println(toHexString(data));
        MessageDigest hasher = MessageDigest.getInstance("md5");
        hasher.update(data);
        String hash = toHexString(hasher.digest());
        System.out.println(hash);
    }

    static final byte mbrot8(final double[] cr, final double civ) {
        var ci = new double[] { civ, civ, civ, civ, civ, civ, civ, civ, };
        var zr = new double[8];
        var zi = new double[8];
        var tr = new double[8];
        var ti = new double[8];
        var absz = new double[8];
        var tmp = new double[8];
        for (var _i = 0; _i < 10; _i++) {
            IntStream.range(0, 5).forEach(_j -> {
                add(zr, zr, tmp);
                mul(tmp, zi, tmp);
                add(tmp, ci, zi);

                minus(tr, ti, tmp);
                add(tmp, cr, zr);

                mul(zr, zr, tr);
                mul(zi, zi, ti);
            });
            add(tr, ti, absz);
            var terminate = true;
            for (var i = 0; i < 8; i++) {
                if (absz[i] <= 4.0) {
                    terminate = false;
                    break;
                }
            }
            if (terminate) {
                return 0;
            }
        }

        var accu = (byte) 0;
        for (var i = 0; i < 8; i++) {
            if (absz[i] <= 4.0) {
                accu |= 0x80 >> i;
            }
        }
        return accu;
    }

    static final void add(final double[] a, final double[] b, double[] r) {
        IntStream.range(0, 8).forEach(i -> {
            r[i] = a[i] + b[i];
        });
    }

    static final void minus(final double[] a, final double[] b, double[] r) {
        IntStream.range(0, 8).forEach(i -> {
            r[i] = a[i] - b[i];
        });
    }

    static final void mul(final double[] a, final double[] b, double[] r) {
        IntStream.range(0, 8).forEach(i -> {
            r[i] = a[i] * b[i];
        });
    }

    static final String toHexString(byte[] a) {
        StringBuilder sb = new StringBuilder(a.length * 2);
        for (byte b : a)
            sb.append(String.format("%02x", b));
        return sb.toString();
    }
}
