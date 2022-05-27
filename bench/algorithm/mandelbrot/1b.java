import java.io.*;
import java.security.*;
import java.util.Arrays;
import java.util.stream.*;

/**
 * Variantion of 1a.java with further saving in terms of memory
 * access and loop count
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

		// this brings the memory into a local context instead of a global
		// which makes it faster (compare 1a,java)
		double[] ci = new double[8];
		double[] zr = new double[8];
		double[] zi = new double[8];
		double[] tr = new double[8];
		double[] ti = new double[8];
		double[] absz = new double[8];
		
		byte[] data = new byte[N * chunkSize];
		for (int y = 0; y < N; y++) {
			var civ = y * inv - 1.0;

			var yc = y * chunkSize;
			for (int x = 0; x < chunkSize; x++) {
				data[yc + x] = mbrot8(ci, zr, zi, tr, ti, absz, xloc[x], civ);
			}
		}

		System.out.println("P4\n" + N + " " + N);
		// System.out.println(toHexString(data));
		
		MessageDigest hasher = MessageDigest.getInstance("md5");
		hasher.update(data);
		
		// this avoid to load a StringBuilder which we really don't need here
		// because this output is small
		for (final byte b : hasher.digest()) {
			System.out.print(String.format("%02x", b));
		}
		System.out.println();
	}

	static final byte mbrot8(
			double[] ci,
			double[] zr,
			double[] zi,
			double[] tr,
			double[] ti,
			double[] absz,
			final double[] cr, final double civ) {
		// don't allocate new memory, just zero or init it
		for ( int i = 0; i < 8; i++) {
			ci[i] = civ;
			zr[i] = zi[i] = tr[i] = ti[i] = absz[i] = 0.0;			
		}
		
		for (var _i = 0; _i < 10; _i++) {
			for (var _j = 0; _j < 5; _j++) {
				for (int i = 0; i < 8; i++) {
					zi[i] = ((zr[i] + zr[i]) * zi[i]) + ci[i];
					
					zr[i] = tr[i] - ti[i] + cr[i];
					
					tr[i] = zr[i] * zr[i];
					ti[i] = zi[i] * zi[i];
				}
			}
			for (int i = 0; i < 8; i++) {
				absz[i] = tr[i] + ti[i];
			}
			
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
}
