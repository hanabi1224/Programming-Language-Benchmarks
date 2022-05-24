import java.io.*;
import java.security.*;
import java.util.Arrays;
import java.util.stream.*;

/**
 * Variantion of 1.java but with 10x less memory needs due to
 * reuse of the arrays instead of reallocation them over and over
 */
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
		for (int y = 0; y < N; y++) {
			var ci = y * inv - 1.0;

			var yc = y * chunkSize;
			for (int x = 0; x < chunkSize; x++) {
				data[yc + x] = mbrot8(xloc[x], ci);
			}
		}

		System.out.println("P4\n" + N + " " + N);
		// System.out.println(toHexString(data));
		MessageDigest hasher = MessageDigest.getInstance("md5");
		hasher.update(data);
		String hash = toHexString(hasher.digest());
		System.out.println(hash);
	}

	static double[] ci = new double[8];
	static double[] zr = new double[8];
	static double[] zi = new double[8];
	static double[] tr = new double[8];
	static double[] ti = new double[8];
	static double[] absz = new double[8];
	static double[] tmp = new double[8];
	
	static final byte mbrot8(final double[] cr, final double civ) {
		// don't allocate new memory, just zero or init it
		for ( int i = 0; i < 8; i++) {
			ci[i] = civ;
			zr[i] = zi[i] = tr[i] = ti[i] = absz[i] = tmp[i] = 0.0;			
		}
		
		for (var _i = 0; _i < 10; _i++) {
			for (var _j = 0; _j < 5; _j++) {
				add(zr, zr, tmp);
				mul(tmp, zi, tmp);
				add(tmp, ci, zi);

				minus(tr, ti, tmp);
				add(tmp, cr, zr);

				mul(zr, zr, tr);
				mul(zi, zi, ti);
			}
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
		for (int i = 0; i < 8; i++) {
			r[i] = a[i] + b[i];
		}
	}

	static final void minus(final double[] a, final double[] b, double[] r) {
		for (int i = 0; i < 8; i++) {
			r[i] = a[i] - b[i];
		}
	}

	static final void mul(final double[] a, final double[] b, double[] r) {
		for (int i = 0; i < 8; i++) {
			r[i] = a[i] * b[i];
		}
	}

	static final String toHexString(byte[] a) {
		final StringBuilder sb = new StringBuilder(a.length * 2);
		for (final byte b : a) {
			sb.append(String.format("%02x", b));
		}
		
		return sb.toString();
	}
}
