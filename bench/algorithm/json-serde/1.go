package main

import (
	"crypto/md5"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
)

func main() {
	fileName := "sample"
	n := 10
	if len(os.Args) > 1 {
		fileName = os.Args[1]
	}
	if len(os.Args) > 2 {
		n, _ = strconv.Atoi(os.Args[2])
	}
	jsonStr, _ := ioutil.ReadFile(fileName + ".json")
	indent := " "
	for i := 0; i < n; i++ {
		var data GeoData
		json.Unmarshal([]byte(jsonStr), &data)
		prettified := data.Prettify(indent)
		// fmt.Println(string(prettified))
		// break
		hasher := md5.New()
		hasher.Write(prettified)
		fmt.Printf("%x\n", hasher.Sum(nil))
		indent += " "
	}
}

type GeoData struct {
	Type     string    `json:"type"`
	Features []Feature `json:"features"`
}

func (data *GeoData) Prettify(indent string) []byte {
	if bytes, err := json.MarshalIndent(data, "", indent); err == nil {
		return bytes
	}
	return []byte{}
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
