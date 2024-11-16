// contributed by Branimir Maksimovic

package main
import "core:bufio"
import "core:fmt"
import "core:io"
import "core:os"
import "core:slice"
import "core:strings"
import "core:thread"

FACTOR :: 16
main :: proc() {
	initTables()
	buf, err := read_lines_with_buffering()
	defer delete(buf)
	if err != io.Error.No_Progress {
		fmt.println("error:", err)
		return
	}
	Value :: union {
		u8,
		string,
	}
	data: []Value = {1, 2, "GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT"}
	for i in data {
		switch j in i {
		case u8:
			work(&buf, j)
		case string:
			work(&buf, j)
		}
	}
}
frequencies :: proc(buf: ^string, count: u8) {
	hash := calculate(buf, cast(u32)count)
	defer delete_table(hash)
	list := make([dynamic]Value, 0)
	defer {
		for i in list {
			delete(i.key)
		}
		delete(list)
	}
	Data :: struct {
		list:  ^[dynamic]Value,
		count: u8,
	}
	data := Data{&list, count}
	ForEach(hash, &data, proc(key: u64, value: ^int, data: ^Data) {
		append(data.list, Value{decode(key, u32(data.count)), value^})
	})
	slice.reverse_sort_by_key(list[:], proc(v: Value) -> int {return v.value})
	for entry in list {
		fmt.printf(
			"%s %.3f\n",
			entry.key,
			100.0 * f64(entry.value) / f64(len(buf) - int(count) + 1),
		)
	}
	fmt.println()
}
count :: proc(buf: ^string, entry: string) {
	hash := calculate(buf, cast(u32)len(entry))
	defer delete_table(hash)
	data, ok := Get(hash, encode(transmute([]u8)entry))
	fmt.printf("%d\t%s\n", data^, entry)
}
work :: proc {
	frequencies,
	count,
}
hash_u64 :: proc(i: u64) -> u64 {
	return i
}
equal_u64 :: proc(l, r: u64) -> bool {
	return l == r
}
calculate :: proc(poly: ^string, size: u32) -> (rc: ^HTable(u64, int)) {
	rc = makeTable(u64, int, hash_u64, equal_u64)
	frames: [FACTOR]A
	threads: [FACTOR]^thread.Thread
	A :: struct {
		poly: ^string,
		size: u32,
		i:    u32,
		hash: ^HTable(u64, int),
	}
	ex :: proc(self: ^A) {
		j := self.i
		for ; j < u32(len(self.poly)) + 1 - self.size; j += FACTOR {
			data, ok := Get(self.hash, encode(transmute([]u8)self.poly[j:j + self.size]))
			data^ += 1
		}
	}
	for frame, index in frames {
		frames[index] = {poly, size, u32(index), makeTable(u64, int, hash_u64, equal_u64)}
		threads[index] = thread.create_and_start_with_poly_data(&frames[index], ex)
	}
	for t, index in threads {
		thread.join(t)
		defer thread.destroy(t)
		ht := frames[index].hash
		defer delete_table(ht)
		ForEach(ht, rc, proc(key: u64, value: ^int, rc: ^HTable(u64, int)) {
			data, ok := Get(rc, key)
			data^ += value^
		})
	}
	return
}
read_lines_with_buffering :: proc() -> (string, io.Error) {
	r: bufio.Reader
	buffer: [1024]byte
	#no_bounds_check f, err := os.open(os.args[1])
	if err != os.ERROR_NONE {
		return "", io.Error.Unknown
	}
	defer os.close(f)
	bufio.reader_init_with_buf(&r, os.stream_from_handle(f), buffer[:])
	defer bufio.reader_destroy(&r)

	linerc: strings.Builder
	strings.builder_init_none(&linerc)
	to_read: bool
	for {
		line, err := bufio.reader_read_string(&r, '\n', context.allocator)
		defer delete(line, context.allocator)
		if err != nil {
			return strings.to_string(linerc), err
		}
		if !to_read && strings.has_prefix(line, `>THREE`) {
			to_read = true
			continue
		}
		if to_read {
			line = strings.trim_right(line, "\n")
			strings.write_string(&linerc, line)
		}
	}
}
Value :: struct {
	key:   string,
	value: int,
}
toChar: [256]rune
toNum: [256]u8
initTables :: proc() {
	toNum['A'] = 0
	toNum['C'] = 1
	toNum['T'] = 2
	toNum['G'] = 3
	toNum['a'] = 0
	toNum['c'] = 1
	toNum['t'] = 2
	toNum['g'] = 3

	toChar[0] = 'A'
	toChar[1] = 'C'
	toChar[2] = 'T'
	toChar[3] = 'G'
}

encode :: proc(i: []u8) -> u64 {
	rc: u64
	for j in i {
		rc <<= 2
		rc |= u64(toNum[j])
	}
	return rc
}
decode :: proc(i: u64, size: u32) -> string {
	rc: strings.Builder
	strings.builder_init_none(&rc)
	i := i
	for _ in 0 ..< size {
		strings.write_rune(&rc, toChar[i & 3])
		i >>= 2
	}
	s := strings.to_string(rc)
	slice.reverse(transmute([]u8)s)
	return s
}
SIZE :: 1 << 16
HTable :: struct(Key, Value: typeid) {
	table: [SIZE]^Node(Key, Value),
	hash:  proc(k: Key) -> u64,
	equal: proc(l, r: Key) -> bool,
}
Node :: struct(Key, Value: typeid) {
	key:   Key,
	value: Value,
	next:  ^Node(Key, Value),
}
makeTable :: proc(
	$Key, $Value: typeid,
	hash: proc(k: Key) -> u64,
	equal: proc(l, r: Key) -> bool,
) -> ^HTable(Key, Value) {
	rc := new(HTable(Key, Value))
	rc^ = {
		hash  = hash,
		equal = equal,
	}
	return rc
}
Get :: proc(input: ^HTable($Key, $Value), key: Key) -> (^Value, bool) {
	hash := input.hash(key)
	slot := hash & (SIZE - 1)
	n := input.table[slot]
	if n == nil {
		n = new(Node(Key, Value))
		n^ = {
			key = key,
		}
		input.table[slot] = n
		return &n.value, false
	}
	for ; n != nil; n = n.next {
		if input.equal(key, n.key) {
			return &n.value, true
		}
	}
	n = new(Node(Key, Value))
	n^ = {
		key  = key,
		next = input.table[slot],
	}
	input.table[slot] = n
	return &n.value, false
}
ForEach :: proc(input: ^HTable($Key, $Value), data: ^$T, f: proc(k: Key, v: ^Value, data: ^T)) {
	for &n in &input.table {
		for ; n != nil; n = n.next {
			f(n.key, &n.value, data)
		}
	}
}
delete_table :: proc(input: ^HTable($Key, $Value)) {
	for &n in &input.table {
		for n != nil {
			tmp := n
			n = n.next
			free(tmp)
		}
	}
	free(input)
}
