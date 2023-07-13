package main

import (
	"crypto/md5"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strconv"
)

func main() {
	if err := run(); err != nil {
		fmt.Fprintln(os.Stderr, err)
	}
}

func run() error {
	fileName := "sample"
	n := 10

	if len(os.Args) > 1 {
		fileName = os.Args[1]
	}
	if len(os.Args) > 2 {
		var err error
		n, err = strconv.Atoi(os.Args[2])
		if err != nil {
			return err
		}
	}

	var data GeoData
	jsonStr, err := ioutil.ReadFile(fileName + ".json")
	if err != nil {
		return err
	}

	if err := json.Unmarshal([]byte(jsonStr), &data); err != nil {
		return err
	}
	if err := encodeHash(data); err != nil {
		return err
	}

	array := make([]GeoData, 0, n)
	for i := 0; i < n; i++ {
		var data GeoData
		if err := json.Unmarshal([]byte(jsonStr), &data); err != nil {
			return err
		}
		array = append(array, data)
	}

	if err := encodeHash(array); err != nil {
		return err
	}
	return nil
}

type lastNewlineIgnorerWriter struct {
	w io.Writer
}

func (w lastNewlineIgnorerWriter) Write(b []byte) (int, error) {
	if b[len(b)-1] != '\n' {
		return w.w.Write(b)
	}

	_, err := w.w.Write(b[:len(b)-1])
	if err != nil {
		return 0, err
	}
	return len(b), nil
}

func encodeHash(data any) error {
	hasher := md5.New()
	// Ignore the last byte if it is a newline character, streaming encoder
	// adds it to the end of the json.
	encoder := json.NewEncoder(lastNewlineIgnorerWriter{w: hasher})
	if err := encoder.Encode(data); err != nil {
		return err
	}
	fmt.Printf("%x\n", hasher.Sum(nil))
	return nil
}

type GeoData struct {
	Type     string    `json:"type"`
	Features []Feature `json:"features"`
}

type Feature struct {
	Type       string     `json:"type"`
	Properties Properties `json:"properties"`
	Geometry   Geometry   `json:"geometry"`
}

type Properties struct {
	Name string `json:"name"`
}

type Geometry struct {
	Type        string         `json:"type"`
	Coordinates [][][2]float64 `json:"coordinates"`
}
