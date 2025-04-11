import std;

immutable maxLine = 60;
immutable im = 139_968;
immutable ia = 3_877;
immutable ic = 29_573;

uint seed = 42;

static struct AminoAcid {
    ubyte l;
    double p;
}

double nextRandom(double max) {
    seed = (seed * ia + ic) % im;
    return max * seed / im;
}

void repeatAndWrap (immutable ubyte[] seq, size_t count) {
    uint len = cast(uint) seq.length;
    ubyte[] paddedSeq = new ubyte[](len + maxLine);
    foreach (i, ref e; paddedSeq)
        e = seq[i % len];

    size_t off, idx;
    size_t rem, lineLength;
    while (idx < count) {
        rem = count - idx;
        lineLength = min(maxLine, rem);

        writeln(cast(string)paddedSeq[off .. off + lineLength]);
        
        off += lineLength;
        if (off > len)
            off -= len;
        idx += lineLength;
    }
}

void generateAndWrap (immutable AminoAcid[] nucleotides, size_t count) {
    double cumProb = 0.0;
    double[] cumProbTotal = new double[](nucleotides.length);
    foreach(i, e; nucleotides) {
        cumProb += e.p;
        cumProbTotal[i] = cumProb * im;
    }

    ubyte[maxLine+1] line; // was new before
    line[maxLine] = cast(ubyte)'\n';
    size_t idx, rem, lineLength;
    while (idx < count) {
        rem = count - idx;
        lineLength = min(maxLine, rem);
        foreach (ref col; line[0 .. lineLength]) {
            immutable r = nextRandom(im);
            size_t c;
            foreach (n; cumProbTotal)
                if (n <= r)
                    c++;
            col = nucleotides[c].l;
        }
        line[lineLength] = '\n';
        write(cast(string)line[0 .. lineLength + 1]);

        idx += lineLength;
    }
}

void main(string[] args) {
    immutable uint n = args.length > 1 ? args[1].to!uint : 100;
    
    static immutable(ubyte[72*3 + 71]) homoSapiensAlu = cast(immutable(ubyte[287]))"GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";
    write(">ONE Homo sapiens alu\n");
    repeatAndWrap(homoSapiensAlu, 2 * n);

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
    generateAndWrap(iubNucleotideInfo, 3 * n);

    static immutable AminoAcid[4] homoSapienNucleotideInfo = [
        { l:'a', p: 0.3029549426680 },
        { l:'c', p: 0.1979883004921 },
        { l:'g', p: 0.1975473066391 },
        { l:'t', p: 0.3015094502008 },
    ];
    write(">THREE Homo sapiens frequency\n");
    generateAndWrap(homoSapienNucleotideInfo, 5 * n);
}
