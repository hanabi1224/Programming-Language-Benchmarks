module main

import os
import regex

fn main() {
	file_name := os.args[1] or { '25000_in' }
	mut content := os.read_file(file_name)!
	ilen := content.len
	mut replace_re := regex.regex_opt('(>.*\n)|(\n)')!
	content = replace_re.replace(content, '')
	clen := content.len

	patterns := [
		'agggtaaa|tttaccct',
		'[cgt]gggtaaa|tttaccc[acg]',
		'a[act]ggtaaa|tttacc[agt]t',
		'ag[act]gtaaa|tttac[agt]ct',
		'agg[act]taaa|ttta[agt]cct',
		'aggg[acg]aaa|ttt[cgt]ccct',
		'agggt[cgt]aa|tt[acg]accct',
		'agggta[cgt]a|t[acg]taccct',
		'agggtaa[cgt]|[acg]ttaccct',
	]

	// The counts are independent scans of the same text; the substitutions
	// below are not, as each consumes the previous one's output.
	mut counts := []thread int{cap: patterns.len}
	for p in patterns {
		counts << spawn var_find(content, p)
	}
	for i, n in counts.wait() {
		println('${patterns[i]} ${n}')
	}

	for p, r in {
		'tHa[Nt]':                    '<4>'
		'(aND)|(caN)|(Ha[DS])|(WaS)': '<3>'
		'(a[NSt])|(BY)':              '<2>'
		'<[^>]*>':                    '|'
		'\\|[^|][^|]*\\|':            '-'
	} {
		mut re := regex.regex_opt(p)!
		content = re.replace(content, r)
	}
	println('\n${ilen}\n${clen}\n${content.len}')
}

fn var_find(content string, pattern string) int {
	mut re := regex.regex_opt(normalize_pattern(pattern)) or { panic(err) }
	matches := re.find_all(content)
	return matches.len / 2
}

// vlib's regex matches nothing for a bare top-level `a|b`.
fn normalize_pattern(pattern string) string {
	return pattern.split('|').map('(${it})').join('|')
}
