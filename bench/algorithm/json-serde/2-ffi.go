package main

import (
	"crypto/md5"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"

	json "github.com/bytedance/sonic"
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

	jsonStr, err := ioutil.ReadFile(fileName + ".json")
	if err != nil {
		return err
	}

	var data GeoData
	if err := json.Unmarshal(jsonStr, &data); err != nil {
		return err
	}

	bytes, err := json.Marshal(data)
	if err != nil {
		return err
	}
	printHash(bytes)

	array := make([]GeoData, 0, n)
	for i := 0; i < n; i++ {
		if err := json.Unmarshal(jsonStr, &data); err != nil {
			return err
		}
		array = append(array, data)
	}

	bytes, err = json.Marshal(array)
	if err != nil {
		return err
	}
	printHash(bytes)

	return nil
}

func printHash(json []byte) {
	hasher := md5.New()
	hasher.Write(json)
	fmt.Printf("%x\n", hasher.Sum(nil))
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
