// translation from zig version. From https://github.com/cyrusmsk/lang_benchmark

import std;
import std.outbuffer: OutBuffer;

immutable maxLine = 60;
immutable im = 139_968;
immutable ia = 3_877;
immutable ic = 29_573;

uint seed = 42;

static struct AminoAcid {
    char l;
    double p;
}

double nextRandom(double max) {
    seed = (seed * ia + ic) % im;
    return max * seed/im;
}

void repeatAndWrap (OutBuffer b, immutable char[] seq, size_t count) {
    uint len = cast(uint) seq.length;
    char[] paddedSeq = new char[](len + maxLine);
    foreach (i, ref e; paddedSeq)
        e = seq[i % len];

    size_t off, idx;
    while (idx < count) {
        immutable rem = count - idx;
        immutable size_t lineLength = min(maxLine, rem);

        // speed up the writeln with lockWriter
        b.write(paddedSeq[off .. off + lineLength]);
        b.write("\n");
        
        off += lineLength;
        if (off > len)
            off -= len;
        idx += lineLength;
    }
}

void generateAndWrap (OutBuffer b, immutable AminoAcid[] nucleotides, size_t count) {
    double cumProb = 0.0;
    double[] cumProbTotal = new double[](nucleotides.length);
    foreach(i, e; nucleotides) {
        cumProb += e.p;
        cumProbTotal[i] = cumProb * im;
    }

    char[] line = new char[](maxLine+1);
    line[maxLine] = '\n';
    size_t idx;
    while (idx < count) {
        immutable rem = count - idx;
        immutable size_t lineLength = min(maxLine, rem);
        foreach (ref col; line[0 .. lineLength]) {
            immutable r = nextRandom(im);
            size_t c;
            foreach (n; cumProbTotal)
                if (n <= r)
                    c++;
            col = nucleotides[c].l;
        }
        line[lineLength] = '\n';
        b.write(line[0 .. lineLength + 1]);

        idx += lineLength;
    }
}

void main(string[] args) {
    immutable uint n = args.length > 1 ? args[1].to!uint : 100;
    OutBuffer b = new OutBuffer();
    
    static immutable char[72*3 + 71] homoSapiensAlu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";
    write(">ONE Homo sapiens alu\n");
    repeatAndWrap(b, homoSapiensAlu, 2 * n);
    write(b);
    b.clear();

    static immutable AminoAcid[15] iubNucleotideInfo = [
        { l:'a', p: 0.27 },
        { l:'c', p: 0.12 },
        { l:'g', p: 0.12 },
        { l:'t', p: 0.27 },
        { l:'B', p: 0.02 },
        { l:'D', p: 0.02 },
        { l:'H', p: 0.02 },
        { l:'K', p: 0.02 },
        { l:'M', p: 0.02 },
        { l:'N', p: 0.02 },
        { l:'R', p: 0.02 },
        { l:'S', p: 0.02 },
        { l:'V', p: 0.02 },
        { l:'W', p: 0.02 },
        { l:'Y', p: 0.02 },
    ];
    write(">TWO IUB ambiguity codes\n");
    generateAndWrap(b, iubNucleotideInfo, 3 * n);
    write(b);
    b.clear();

    static immutable AminoAcid[4] homoSapienNucleotideInfo = [
        { l:'a', p: 0.3029549426680 },
        { l:'c', p: 0.1979883004921 },
        { l:'g', p: 0.1975473066391 },
        { l:'t', p: 0.3015094502008 },
    ];
    write(">THREE Homo sapiens frequency\n");
    generateAndWrap(b, homoSapienNucleotideInfo, 5 * n);
    write(b);
}
