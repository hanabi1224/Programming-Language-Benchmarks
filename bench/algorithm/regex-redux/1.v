module main

import os
import regex

fn main() {
	file_name := os.args[1] or { '25000_in' }
	mut content := os.read_file(file_name)?
	ilen := content.len
	mut replace_re := regex.regex_opt('(>.*\n)|(\n)')?
	content = replace_re.replace(content, '')
	clen := content.len
	for p in [
		'agggtaaa|tttaccct',
		'[cgt]gggtaaa|tttaccc[acg]',
		'a[act]ggtaaa|tttacc[agt]t',
		'ag[act]gtaaa|tttac[agt]ct',
		'agg[act]taaa|ttta[agt]cct',
		'aggg[acg]aaa|ttt[cgt]ccct',
		'agggt[cgt]aa|tt[acg]accct',
		'agggta[cgt]a|t[acg]taccct',
		'agggtaa[cgt]|[acg]ttaccct',
	] {
		println('$p ${var_find(content, p)?}')
	}
	for p, r in {
		'tHa[Nt]':                    '<4>'
		'(aND)|(caN)|(Ha[DS])|(WaS)': '<3>'
		'(a[NSt])|(BY)':              '<2>'
		'<[^>]*>':                    '|'
		'\\|[^|][^|]*\\|':            '-'
	} {
		mut re := regex.regex_opt(p)?
		content = re.replace(content, r)
	}
	println('\n$ilen\n$clen\n$content.len')
}

fn var_find(content string, pattern string) ?int {
	mut re := regex.regex_opt(normalize_pattern(pattern))?
	matches := re.find_all(content)
	return matches.len / 2
}

fn normalize_pattern(pattern string) string {
	return pattern.split('|').map(fn (s string) string {
		return '($s)'
	}).join('|')
}
